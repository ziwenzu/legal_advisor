#!/usr/bin/env python3

from __future__ import annotations

import re
from pathlib import Path

import cpca
import numpy as np
import pandas as pd


ROOT = Path(__file__).resolve().parents[1]
DOC_FILE = ROOT / "data" / "output data" / "document_level_winner_vs_loser_clean.parquet"
DOC_CSV_FILE = ROOT / "data" / "output data" / "document_level_winner_vs_loser_clean.csv"
CASE_FILE = ROOT / "data" / "output data" / "case_level.parquet"
CITY_FILE = ROOT / "data" / "output data" / "city_year_panel.csv"
FIRM_FILE = ROOT / "data" / "output data" / "firm_level.csv"
MERGED_FILE = ROOT / "data" / "temp data" / "litigation_panels_full" / "law_firm_year_panel_merged.parquet"
MASTER_FILE = ROOT / "data" / "temp data" / "litigation_panels_full" / "law_firm_master.parquet"
SUMMARY_FILE = ROOT / "data" / "output data" / "analysis_panel_rebuild_summary_20260417.md"

YEAR_MIN = 2010
YEAR_MAX = 2020
MUNICIPALITIES = {"北京市", "上海市", "天津市", "重庆市"}
SPECIAL_PREFIX_MAP = {
    "北京": ("北京市", "北京市"),
    "上海": ("上海市", "上海市"),
    "天津": ("天津市", "天津市"),
    "重庆": ("重庆市", "重庆市"),
}
COURT_SUFFIX_RE = re.compile(r"(人民法院|知识产权法院|互联网法院|海事法院|铁路运输法院|金融法院).*$")
SPACE_RE = re.compile(r"\s+")
DOC_DERIVED_DROP_COLS = [
    "court_location_text",
    "court_province",
    "court_city",
    "court_district",
    "court_adcode",
    "court_city_matched",
    "court_province_matched",
    "raw_city_year_case_n",
    "sample_city_year_case_n",
    "city_year_quota_weight",
    "raw_province_year_case_n",
    "sample_province_year_case_n",
    "province_year_quota_weight",
    "raw_year_case_n",
    "sample_year_case_n",
    "year_quota_weight",
    "case_quota_weight",
    "case_quota_weight_source",
    "year_quota_normalizer",
]
DOC_OUTPUT_COLS = [
    "year",
    "case_uid",
    "court",
    "cause",
    "law_firm",
    "firm_id",
    "stack_id",
    "treated_firm",
    "event_year",
    "event_time",
    "post",
    "did_treatment",
    "side",
    "case_win_binary",
    "case_decisive",
    "opponent_has_lawyer",
    "plaintiff_party_is_entity",
    "defendant_party_is_entity",
    "case_win_rate_fee",
    "log_legal_reasoning_length_chars",
    "legal_reasoning_share",
    "lawyer_gender",
    "lawyer_practice_years",
    "lawyer_ccp",
    "lawyer_edu",
]
FIRM_OUTPUT_COLS = [
    "year",
    "law_firm",
    "firm_id",
    "stack_id",
    "province",
    "city",
    "event_year",
    "winner_firm",
    "treated_firm",
    "control_firm",
    "event_time",
    "did_treatment",
    "civil_case_n",
    "civil_win_n_binary",
    "civil_decisive_case_n",
    "civil_win_rate_mean",
    "avg_filing_to_hearing_days",
    "civil_fee_decisive_case_n",
    "civil_win_rate_fee_mean",
]
CITY_OUTPUT_COLS = [
    "province",
    "city",
    "year",
    "treatment",
    "government_win_rate",
    "appeal_rate",
    "admin_case_n",
    "log_population_10k",
    "log_gdp",
    "log_registered_lawyers",
    "log_court_caseload_n",
]


def clean_court_text(raw: object) -> str:
    text = "" if pd.isna(raw) else str(raw).strip()
    if not text:
        return ""
    text = text.replace("（", "(").replace("）", ")")
    text = text.replace("中华人民共和国", "")
    text = SPACE_RE.sub("", text)
    text = COURT_SUFFIX_RE.sub("", text)
    return text


def normalize_city_name(city: object, province: object | None = None) -> str | None:
    if pd.isna(city):
        return None
    text = str(city).strip()
    if not text:
        return None
    if text == "市辖区" and province in MUNICIPALITIES:
        return str(province)
    return text


def parse_court_locations(courts: pd.Series, known_cities: set[str]) -> pd.DataFrame:
    unique_courts = pd.Series(courts.dropna().unique(), name="court")
    cleaned = unique_courts.map(clean_court_text)
    parsed = cpca.transform(cleaned.tolist(), pos_sensitive=False)

    out = pd.DataFrame(
        {
            "court": unique_courts,
            "court_location_text": cleaned,
            "court_province": parsed["省"].replace("", pd.NA),
            "court_city": parsed["市"].replace("", pd.NA),
            "court_district": parsed["区"].replace("", pd.NA),
            "court_adcode": parsed["adcode"].replace("", pd.NA),
        }
    )

    out["court_city"] = [
        normalize_city_name(city, province)
        for city, province in zip(out["court_city"], out["court_province"])
    ]

    for prefix, (province, city) in SPECIAL_PREFIX_MAP.items():
        idx = out["court_location_text"].str.startswith(prefix) & out["court_city"].isna()
        out.loc[idx, "court_province"] = province
        out.loc[idx, "court_city"] = city

    # Use any known city name embedded in the court text as a fallback.
    known_city_roots = sorted(
        {
            city.replace("市", "").replace("自治州", "").replace("地区", "").replace("盟", "")
            for city in known_cities
            if isinstance(city, str) and city
        },
        key=len,
        reverse=True,
    )
    if known_city_roots:
        fallback_city = []
        for text, current_city in zip(out["court_location_text"], out["court_city"]):
            if current_city:
                fallback_city.append(current_city)
                continue
            matched_city = None
            for root in known_city_roots:
                if root and root in text:
                    candidates = [city for city in known_cities if city.startswith(root)]
                    if candidates:
                        matched_city = sorted(candidates, key=len)[0]
                        break
            fallback_city.append(matched_city)
        out["court_city"] = fallback_city

    out["court_city_matched"] = out["court_city"].notna().astype(np.int8)
    out["court_province_matched"] = out["court_province"].notna().astype(np.int8)
    return out


def build_case_quota_tables(
    case_df: pd.DataFrame,
    doc_df: pd.DataFrame,
) -> tuple[pd.DataFrame, pd.DataFrame, pd.DataFrame, pd.DataFrame]:
    raw_city = (
        case_df.dropna(subset=["court_province", "court_city"])
        .groupby(["year", "court_province", "court_city"], as_index=False)
        .agg(raw_city_year_case_n=("case_uid", "nunique"))
    )
    doc_city = (
        doc_df.dropna(subset=["court_province", "court_city"])
        .groupby(["year", "court_province", "court_city"], as_index=False)
        .agg(sample_city_year_case_n=("case_uid", "nunique"))
    )
    city_quota = raw_city.merge(doc_city, on=["year", "court_province", "court_city"], how="outer")
    city_quota["raw_city_year_case_n"] = city_quota["raw_city_year_case_n"].fillna(0)
    city_quota["sample_city_year_case_n"] = city_quota["sample_city_year_case_n"].fillna(0)
    city_quota["city_year_quota_weight"] = np.where(
        city_quota["sample_city_year_case_n"] > 0,
        city_quota["raw_city_year_case_n"] / city_quota["sample_city_year_case_n"],
        np.nan,
    )

    raw_province = (
        case_df.dropna(subset=["court_province"])
        .groupby(["year", "court_province"], as_index=False)
        .agg(raw_province_year_case_n=("case_uid", "nunique"))
    )
    doc_province = (
        doc_df.dropna(subset=["court_province"])
        .groupby(["year", "court_province"], as_index=False)
        .agg(sample_province_year_case_n=("case_uid", "nunique"))
    )
    province_quota = raw_province.merge(doc_province, on=["year", "court_province"], how="outer")
    province_quota["raw_province_year_case_n"] = province_quota["raw_province_year_case_n"].fillna(0)
    province_quota["sample_province_year_case_n"] = province_quota["sample_province_year_case_n"].fillna(0)
    province_quota["province_year_quota_weight"] = np.where(
        province_quota["sample_province_year_case_n"] > 0,
        province_quota["raw_province_year_case_n"] / province_quota["sample_province_year_case_n"],
        np.nan,
    )

    raw_year = case_df.groupby("year", as_index=False).agg(raw_year_case_n=("case_uid", "nunique"))
    doc_year = doc_df.groupby("year", as_index=False).agg(sample_year_case_n=("case_uid", "nunique"))
    year_quota = raw_year.merge(doc_year, on="year", how="outer")
    year_quota["raw_year_case_n"] = year_quota["raw_year_case_n"].fillna(0)
    year_quota["sample_year_case_n"] = year_quota["sample_year_case_n"].fillna(0)
    year_quota["year_quota_weight"] = np.where(
        year_quota["sample_year_case_n"] > 0,
        year_quota["raw_year_case_n"] / year_quota["sample_year_case_n"],
        1.0,
    )

    return city_quota, province_quota, year_quota, raw_city


def choose_quota_weight(doc_df: pd.DataFrame) -> pd.Series:
    return (
        doc_df["city_year_quota_weight"]
        .combine_first(doc_df["province_year_quota_weight"])
        .combine_first(doc_df["year_quota_weight"])
        .fillna(1.0)
    )


def normalize_quota_within_year(doc_df: pd.DataFrame) -> pd.DataFrame:
    year_totals = (
        doc_df.groupby("year", as_index=False)
        .agg(current_weight_total=("case_quota_weight", "sum"), raw_year_case_n=("raw_year_case_n", "first"))
    )
    year_totals["year_quota_normalizer"] = np.where(
        year_totals["current_weight_total"] > 0,
        year_totals["raw_year_case_n"] / year_totals["current_weight_total"],
        1.0,
    )
    doc_df = doc_df.merge(
        year_totals[["year", "year_quota_normalizer"]],
        on="year",
        how="left",
    )
    doc_df["case_quota_weight"] = doc_df["case_quota_weight"] * doc_df["year_quota_normalizer"].fillna(1.0)
    return doc_df


def load_static_stack_meta() -> pd.DataFrame:
    current = pd.read_csv(
        FIRM_FILE,
        usecols=[
            "law_firm",
            "firm_id",
            "stack_id",
            "province",
            "city",
            "event_year",
            "winner_firm",
            "treated_firm",
            "control_firm",
        ],
    )
    static_cols = [
        "law_firm",
        "firm_id",
        "stack_id",
        "province",
        "city",
        "event_year",
        "winner_firm",
        "treated_firm",
        "control_firm",
    ]
    return current[static_cols].drop_duplicates(subset=["stack_id", "firm_id"])


def load_raw_firm_year_attrs() -> pd.DataFrame:
    merged = pd.read_parquet(
        MERGED_FILE,
        columns=[
            "firm_id",
            "panel_year",
            "avg_duration_days_civil_defendant",
            "avg_duration_days_civil_plaintiff",
            "duration_obs_n_civil_defendant",
            "duration_obs_n_civil_plaintiff",
        ],
    )
    merged = merged.loc[merged["panel_year"].between(YEAR_MIN, YEAR_MAX)].copy()
    merged["duration_obs_total"] = (
        merged["duration_obs_n_civil_defendant"].fillna(0)
        + merged["duration_obs_n_civil_plaintiff"].fillna(0)
    )
    merged["duration_total"] = (
        merged["avg_duration_days_civil_defendant"].fillna(0)
        * merged["duration_obs_n_civil_defendant"].fillna(0)
        + merged["avg_duration_days_civil_plaintiff"].fillna(0)
        * merged["duration_obs_n_civil_plaintiff"].fillna(0)
    )
    merged["avg_filing_to_hearing_days"] = np.where(
        merged["duration_obs_total"] > 0,
        merged["duration_total"] / merged["duration_obs_total"],
        np.nan,
    )
    attrs = merged.rename(columns={"panel_year": "year"})[
        [
            "firm_id",
            "year",
            "avg_filing_to_hearing_days",
        ]
    ].drop_duplicates(subset=["firm_id", "year"])
    return attrs


def build_firm_level_panel(doc_df: pd.DataFrame, static_meta: pd.DataFrame, raw_attrs: pd.DataFrame) -> pd.DataFrame:
    work = doc_df.copy()
    work["case_decisive"] = pd.to_numeric(work["case_decisive"], errors="coerce").fillna(0).astype(int)
    work["case_win_binary"] = pd.to_numeric(work["case_win_binary"], errors="coerce")
    work["case_win_rate_fee"] = pd.to_numeric(work["case_win_rate_fee"], errors="coerce")
    work["raw_decisive_case_n"] = np.where(work["case_decisive"] == 1, 1.0, 0.0)
    work["raw_binary_win_n"] = np.where(
        (work["case_decisive"] == 1) & work["case_win_binary"].notna(),
        work["case_win_binary"],
        0.0,
    )
    work["raw_fee_case_n"] = np.where(
        (work["case_decisive"] == 1) & work["case_win_rate_fee"].notna(),
        1.0,
        0.0,
    )
    work["raw_fee_winrate_sum"] = np.where(
        (work["case_decisive"] == 1) & work["case_win_rate_fee"].notna(),
        work["case_win_rate_fee"],
        0.0,
    )

    firm_year = (
        work.groupby(["stack_id", "firm_id", "law_firm", "year"], as_index=False)
        .agg(
            civil_case_n=("case_uid", "size"),
            civil_decisive_case_n=("raw_decisive_case_n", "sum"),
            civil_win_n_binary=("raw_binary_win_n", "sum"),
            civil_fee_decisive_case_n=("raw_fee_case_n", "sum"),
            civil_win_rate_fee_sum=("raw_fee_winrate_sum", "sum"),
        )
    )
    firm_year["civil_win_rate_mean"] = np.where(
        firm_year["civil_decisive_case_n"] > 0,
        firm_year["civil_win_n_binary"] / firm_year["civil_decisive_case_n"],
        np.nan,
    )
    firm_year["civil_win_rate_fee_mean"] = np.where(
        firm_year["civil_fee_decisive_case_n"] > 0,
        firm_year["civil_win_rate_fee_sum"] / firm_year["civil_fee_decisive_case_n"],
        np.nan,
    )

    stack_pairs = static_meta[["stack_id", "firm_id", "law_firm"]].drop_duplicates()
    years = pd.DataFrame({"year": np.arange(YEAR_MIN, YEAR_MAX + 1, dtype=int)})
    balanced = stack_pairs.assign(_key=1).merge(years.assign(_key=1), on="_key").drop(columns="_key")
    balanced = balanced.merge(static_meta, on=["stack_id", "firm_id", "law_firm"], how="left")
    balanced = balanced.merge(firm_year, on=["stack_id", "firm_id", "law_firm", "year"], how="left")
    balanced = balanced.merge(raw_attrs, on=["firm_id", "year"], how="left")

    count_cols = [
        "civil_case_n",
        "civil_decisive_case_n",
        "civil_win_n_binary",
        "civil_fee_decisive_case_n",
        "civil_win_rate_fee_sum",
    ]
    for col in count_cols:
        balanced[col] = balanced[col].fillna(0.0)

    balanced["avg_filing_to_hearing_days"] = pd.to_numeric(
        balanced["avg_filing_to_hearing_days"], errors="coerce"
    )

    balanced["event_time"] = balanced["year"] - balanced["event_year"]
    balanced["did_treatment"] = (
        balanced["treated_firm"] * (balanced["year"] >= balanced["event_year"]).astype(int)
    )

    balanced["civil_case_n"] = balanced["civil_case_n"].astype(float)
    balanced["civil_decisive_case_n"] = balanced["civil_decisive_case_n"].astype(float)
    balanced["civil_win_n_binary"] = balanced["civil_win_n_binary"].astype(float)
    balanced["civil_fee_decisive_case_n"] = balanced["civil_fee_decisive_case_n"].astype(float)
    return balanced[FIRM_OUTPUT_COLS].sort_values(["stack_id", "firm_id", "year"]).reset_index(drop=True)


def augment_city_panel(
    city_panel: pd.DataFrame,
    raw_city: pd.DataFrame,
    doc_df: pd.DataFrame,
) -> pd.DataFrame:
    del raw_city, doc_df
    return city_panel[CITY_OUTPUT_COLS].copy()


def build_summary(
    court_map: pd.DataFrame,
    doc_df: pd.DataFrame,
    case_df: pd.DataFrame,
    city_panel: pd.DataFrame,
    firm_panel: pd.DataFrame,
) -> str:
    raw_unique_cases = case_df["case_uid"].nunique()
    sample_cases = doc_df["case_uid"].nunique()
    firm_total = firm_panel["civil_case_n"].sum()
    raw_matched_cases = case_df.loc[case_df["court_city"].notna(), "case_uid"].nunique()
    sample_matched_cases = doc_df.loc[doc_df["court_city"].notna(), "case_uid"].nunique()
    doc_city_match_rate = doc_df["court_city_matched"].mean()
    doc_province_match_rate = doc_df["court_province_matched"].mean()
    doc_decisive_total = pd.to_numeric(doc_df["case_decisive"], errors="coerce").fillna(0).sum()
    firm_decisive_total = firm_panel["civil_decisive_case_n"].sum()
    case_wvr_rows = int((pd.read_parquet(CASE_FILE, columns=["winner_vs_runnerup_case"])["winner_vs_runnerup_case"] == 1).sum())

    lines = [
        "# Analysis Panel Rebuild Summary",
        "",
        "This rebuild keeps only the variables that are used in the current analysis files and removes audit-only carry-over columns from the saved panels.",
        "",
        "## Three analysis datasets",
        "- `city_year_panel.csv`: one row is `province × city × year`. This is the administrative city-year panel used for the city-level DID/CS analysis.",
        "- `document_level_winner_vs_loser_clean.csv`: one row is one selected law firm for one civil case. This is the main litigation DID sample.",
        "- `firm_level.csv`: one row is `stack_id × firm_id × year`. This is the firm-year aggregation of the document-level litigation sample.",
        "",
        "## Court parsing used only for audit",
        f"- Unique courts parsed: `{len(court_map):,}`",
        f"- Document rows with province match: `{doc_province_match_rate:.4f}`",
        f"- Document rows with city match: `{doc_city_match_rate:.4f}`",
        "",
        "## Relationship to the broader civil-case source",
        f"- Raw unique civil cases in `case_level`: `{raw_unique_cases:,}`",
        f"- Raw `case_level` rows flagged `winner_vs_runnerup_case == 1`: `{case_wvr_rows:,}`",
        f"- One-document-one-firm sample cases: `{sample_cases:,}`",
        f"- Raw unique civil cases with matched city: `{raw_matched_cases:,}`",
        f"- Document-sample cases with matched city: `{sample_matched_cases:,}`",
        "",
        "## Exact identities across the three analysis files",
        f"- `sum(document_level rows)` = `{sample_cases:,.0f}`",
        f"- `sum(firm_level.civil_case_n)` = `{firm_total:,.0f}`",
        f"- Raw case-count identity holds: `{np.isclose(sample_cases, firm_total, rtol=0, atol=1e-6)}`",
        f"- `sum(document_level.case_decisive)` = `{doc_decisive_total:,.0f}`",
        f"- `sum(firm_level.civil_decisive_case_n)` = `{firm_decisive_total:,.0f}`",
        f"- Decisive-case identity holds: `{np.isclose(doc_decisive_total, firm_decisive_total, rtol=0, atol=1e-6)}`",
        f"- City-year rows: `{len(city_panel):,}` across `{city_panel[['province', 'city']].drop_duplicates().shape[0]:,}` cities",
        "",
        "## Saved variable sets",
        f"- `document_level` columns: `{', '.join(DOC_OUTPUT_COLS)}`",
        f"- `firm_level` columns: `{', '.join(FIRM_OUTPUT_COLS)}`",
        f"- `city_year_panel` columns: `{', '.join(CITY_OUTPUT_COLS)}`",
        "",
        "## Interpretation",
        "- The litigation analysis uses the raw winner-vs-runner-up case universe, not quota-weighted counts and not all civil cases in the source database.",
        "- `firm_level.civil_case_n` is therefore the correct firm-level quantity outcome: it is the exact stack-firm-year aggregation of the document-level analytical sample.",
        "- `city_year_panel` is a separate administrative panel. It is not an aggregation of the document-level civil sample, so it should be interpreted as a parallel city-year dataset rather than a direct collapse of civil cases.",
        "",
    ]
    return "\n".join(lines) + "\n"


def main() -> None:
    current_city_panel = pd.read_csv(CITY_FILE)
    known_cities = set(current_city_panel["city"].dropna().astype(str))

    case_df = pd.read_parquet(CASE_FILE, columns=["year", "case_uid", "court"])
    case_df = case_df.drop_duplicates(subset=["year", "case_uid"]).copy()
    doc_base = pd.read_parquet(DOC_FILE).copy()
    doc_base = doc_base[[col for col in DOC_OUTPUT_COLS if col in doc_base.columns]].copy()

    court_map = parse_court_locations(
        pd.concat([case_df["court"], doc_base["court"]], ignore_index=True),
        known_cities=known_cities,
    )

    case_geo = case_df.merge(court_map, on="court", how="left")
    doc_geo = doc_base.merge(court_map, on="court", how="left")

    static_meta = load_static_stack_meta()
    raw_attrs = load_raw_firm_year_attrs()
    firm_panel = build_firm_level_panel(doc_base, static_meta, raw_attrs)
    city_panel = augment_city_panel(current_city_panel, pd.DataFrame(), doc_base)

    doc_base.to_parquet(DOC_FILE, compression="zstd", index=False)
    doc_base.to_csv(DOC_CSV_FILE, index=False)
    firm_panel.to_csv(FIRM_FILE, index=False)
    city_panel.to_csv(CITY_FILE, index=False)
    SUMMARY_FILE.write_text(
        build_summary(court_map, doc_geo, case_geo, city_panel, firm_panel),
        encoding="utf-8",
    )

    print(f"Wrote {DOC_FILE}")
    print(f"Wrote {DOC_CSV_FILE}")
    print(f"Wrote {FIRM_FILE}")
    print(f"Wrote {CITY_FILE}")
    print(f"Wrote {SUMMARY_FILE}")


if __name__ == "__main__":
    main()
