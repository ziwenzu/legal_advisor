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
            "stack_control_firm_n",
            "stack_firm_n",
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
        "stack_control_firm_n",
        "stack_firm_n",
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
            "firm_size",
            "firm_capital_final",
            "firm_birth_year_final",
            "first_contract_year",
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
    attrs = merged.rename(columns={"panel_year": "year"}).copy()
    attrs["firm_capital"] = pd.to_numeric(attrs["firm_capital_final"], errors="coerce")
    attrs["firm_birth_year"] = pd.to_numeric(attrs["firm_birth_year_final"], errors="coerce")
    attrs["first_contract_year"] = (
        pd.to_numeric(attrs["first_contract_year"], errors="coerce").fillna(0).astype(int)
    )
    attrs["firm_size"] = pd.to_numeric(attrs["firm_size"], errors="coerce")
    attrs = attrs[
        [
            "firm_id",
            "year",
            "firm_size",
            "firm_capital",
            "firm_birth_year",
            "first_contract_year",
            "avg_filing_to_hearing_days",
        ]
    ].drop_duplicates(subset=["firm_id", "year"])

    master = pd.read_parquet(
        MASTER_FILE,
        columns=["firm_id", "firm_capital_final", "firm_birth_year_final", "first_contract_year", "firm_size"],
    ).drop_duplicates(subset=["firm_id"])
    master["firm_capital_master"] = pd.to_numeric(master["firm_capital_final"], errors="coerce")
    master["firm_birth_year_master"] = pd.to_numeric(master["firm_birth_year_final"], errors="coerce")
    master["first_contract_year_master"] = (
        pd.to_numeric(master["first_contract_year"], errors="coerce").fillna(0).astype(int)
    )
    master["firm_size_master"] = pd.to_numeric(master["firm_size"], errors="coerce")
    master = master[
        [
            "firm_id",
            "firm_capital_master",
            "firm_birth_year_master",
            "first_contract_year_master",
            "firm_size_master",
        ]
    ]
    attrs = attrs.merge(master, on="firm_id", how="left")
    attrs["firm_capital"] = attrs["firm_capital"].combine_first(attrs["firm_capital_master"])
    attrs["firm_birth_year"] = attrs["firm_birth_year"].combine_first(attrs["firm_birth_year_master"])
    attrs["first_contract_year"] = np.where(
        attrs["first_contract_year"] > 0,
        attrs["first_contract_year"],
        attrs["first_contract_year_master"].fillna(0),
    ).astype(int)
    return attrs.drop(
        columns=["firm_capital_master", "firm_birth_year_master", "first_contract_year_master"]
    )


def build_firm_level_panel(doc_df: pd.DataFrame, static_meta: pd.DataFrame, raw_attrs: pd.DataFrame) -> pd.DataFrame:
    work = doc_df.copy()
    represented_enterprise = pd.Series(
        np.where(
            work["side"].eq("plaintiff"),
            work["plaintiff_party_is_entity"],
            work["defendant_party_is_entity"],
        ),
        index=work.index,
    )
    work["represented_enterprise"] = pd.to_numeric(represented_enterprise, errors="coerce").fillna(0).astype(float)
    work["represented_personal"] = 1.0 - work["represented_enterprise"]
    work["case_decisive"] = pd.to_numeric(work["case_decisive"], errors="coerce").fillna(0).astype(int)
    work["case_win_binary"] = pd.to_numeric(work["case_win_binary"], errors="coerce")
    work["case_win_rate_fee"] = pd.to_numeric(work["case_win_rate_fee"], errors="coerce")
    work["case_quota_weight"] = pd.to_numeric(work["case_quota_weight"], errors="coerce").fillna(1.0)

    work["weighted_civil_case_n"] = work["case_quota_weight"]
    work["weighted_decisive_case_n"] = np.where(work["case_decisive"] == 1, work["case_quota_weight"], 0.0)
    work["weighted_binary_win_n"] = np.where(
        (work["case_decisive"] == 1) & work["case_win_binary"].notna(),
        work["case_quota_weight"] * work["case_win_binary"],
        0.0,
    )
    work["weighted_fee_case_n"] = np.where(
        (work["case_decisive"] == 1) & work["case_win_rate_fee"].notna(),
        work["case_quota_weight"],
        0.0,
    )
    work["weighted_fee_winrate_sum"] = np.where(
        (work["case_decisive"] == 1) & work["case_win_rate_fee"].notna(),
        work["case_quota_weight"] * work["case_win_rate_fee"],
        0.0,
    )
    work["weighted_enterprise_case_n"] = work["case_quota_weight"] * work["represented_enterprise"]
    work["weighted_personal_case_n"] = work["case_quota_weight"] * work["represented_personal"]
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
            civil_case_n_quota_weighted=("weighted_civil_case_n", "sum"),
            civil_decisive_case_n=("raw_decisive_case_n", "sum"),
            civil_decisive_case_n_quota_weighted=("weighted_decisive_case_n", "sum"),
            civil_win_n_binary=("raw_binary_win_n", "sum"),
            civil_win_n_binary_quota_weighted=("weighted_binary_win_n", "sum"),
            civil_fee_decisive_case_n=("raw_fee_case_n", "sum"),
            civil_fee_decisive_case_n_quota_weighted=("weighted_fee_case_n", "sum"),
            civil_win_rate_fee_sum=("raw_fee_winrate_sum", "sum"),
            civil_win_rate_fee_sum_quota_weighted=("weighted_fee_winrate_sum", "sum"),
            enterprise_case_n=("represented_enterprise", "sum"),
            enterprise_case_n_quota_weighted=("weighted_enterprise_case_n", "sum"),
            personal_case_n=("represented_personal", "sum"),
            personal_case_n_quota_weighted=("weighted_personal_case_n", "sum"),
            mean_case_quota_weight=("case_quota_weight", "mean"),
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
        "civil_case_n_quota_weighted",
        "civil_decisive_case_n",
        "civil_decisive_case_n_quota_weighted",
        "civil_win_n_binary",
        "civil_win_n_binary_quota_weighted",
        "civil_fee_decisive_case_n",
        "civil_fee_decisive_case_n_quota_weighted",
        "civil_win_rate_fee_sum",
        "civil_win_rate_fee_sum_quota_weighted",
        "enterprise_case_n",
        "enterprise_case_n_quota_weighted",
        "personal_case_n",
        "personal_case_n_quota_weighted",
    ]
    for col in count_cols:
        balanced[col] = balanced[col].fillna(0.0)

    balanced["firm_size"] = balanced["firm_size"].combine_first(balanced["firm_size_master"]).fillna(0.0)
    balanced["firm_capital"] = balanced["firm_capital"].fillna(0.0)
    balanced["firm_birth_year"] = balanced["firm_birth_year"].fillna(0.0)
    balanced["first_contract_year"] = balanced["first_contract_year"].fillna(0).astype(int)
    balanced["avg_filing_to_hearing_days"] = balanced["avg_filing_to_hearing_days"].fillna(0.0)

    pre_birth = (balanced["firm_birth_year"] > 0) & (balanced["year"] < balanced["firm_birth_year"])
    balanced.loc[pre_birth, "firm_size"] = 0.0

    balanced["event_time"] = balanced["year"] - balanced["event_year"]
    balanced["did_treatment"] = (
        balanced["treated_firm"] * (balanced["year"] >= balanced["event_year"]).astype(int)
    )
    balanced["firm_age_at_event"] = np.where(
        balanced["firm_birth_year"] > 0,
        np.maximum(balanced["event_year"] - balanced["firm_birth_year"], 0),
        np.nan,
    )

    baseline = (
        balanced.loc[balanced["year"] == balanced["event_year"], ["stack_id", "firm_id", "firm_size"]]
        .drop_duplicates(subset=["stack_id", "firm_id"])
        .rename(columns={"firm_size": "firm_size_baseline"})
    )
    balanced = balanced.merge(baseline, on=["stack_id", "firm_id"], how="left")
    balanced["firm_size_baseline"] = balanced["firm_size_baseline"].fillna(balanced["firm_size"])

    balanced["civil_case_n"] = balanced["civil_case_n"].astype(float)
    balanced["civil_decisive_case_n"] = balanced["civil_decisive_case_n"].astype(float)
    balanced["civil_win_n_binary"] = balanced["civil_win_n_binary"].astype(float)
    balanced["enterprise_case_n"] = balanced["enterprise_case_n"].astype(float)
    balanced["personal_case_n"] = balanced["personal_case_n"].astype(float)
    balanced["civil_fee_decisive_case_n"] = balanced["civil_fee_decisive_case_n"].astype(float)
    balanced["civil_case_n_quota_weighted"] = balanced["civil_case_n_quota_weighted"].astype(float)
    balanced["civil_decisive_case_n_quota_weighted"] = balanced["civil_decisive_case_n_quota_weighted"].astype(float)
    balanced["civil_win_n_binary_quota_weighted"] = balanced["civil_win_n_binary_quota_weighted"].astype(float)
    balanced["enterprise_case_n_quota_weighted"] = balanced["enterprise_case_n_quota_weighted"].astype(float)
    balanced["personal_case_n_quota_weighted"] = balanced["personal_case_n_quota_weighted"].astype(float)
    balanced["civil_fee_decisive_case_n_quota_weighted"] = balanced["civil_fee_decisive_case_n_quota_weighted"].astype(float)

    out_cols = [
        "year",
        "law_firm",
        "firm_id",
        "firm_size",
        "stack_id",
        "province",
        "city",
        "event_year",
        "winner_firm",
        "treated_firm",
        "control_firm",
        "stack_control_firm_n",
        "stack_firm_n",
        "event_time",
        "did_treatment",
        "firm_size_baseline",
        "firm_capital",
        "firm_age_at_event",
        "first_contract_year",
        "civil_case_n",
        "civil_case_n_quota_weighted",
        "civil_win_n_binary",
        "civil_win_n_binary_quota_weighted",
        "civil_decisive_case_n",
        "civil_decisive_case_n_quota_weighted",
        "civil_win_rate_mean",
        "avg_filing_to_hearing_days",
        "enterprise_case_n",
        "enterprise_case_n_quota_weighted",
        "personal_case_n",
        "personal_case_n_quota_weighted",
        "civil_fee_decisive_case_n",
        "civil_fee_decisive_case_n_quota_weighted",
        "civil_win_rate_fee_mean",
        "mean_case_quota_weight",
    ]
    return balanced[out_cols].sort_values(["stack_id", "firm_id", "year"]).reset_index(drop=True)


def augment_city_panel(
    city_panel: pd.DataFrame,
    raw_city: pd.DataFrame,
    doc_df: pd.DataFrame,
) -> pd.DataFrame:
    city_panel = city_panel.drop(
        columns=[
            col
            for col in [
                "raw_city_year_case_n",
                "sample_case_n",
                "sample_decisive_case_n",
                "sample_case_n_quota_weighted",
            ]
            if col in city_panel.columns
        ]
    ).copy()
    doc_city = (
        doc_df.dropna(subset=["court_province", "court_city"])
        .groupby(["year", "court_province", "court_city"], as_index=False)
        .agg(
            sample_case_n=("case_uid", "nunique"),
            sample_decisive_case_n=("case_decisive", lambda s: int(pd.to_numeric(s, errors="coerce").fillna(0).sum())),
            sample_case_n_quota_weighted=("case_quota_weight", "sum"),
        )
        .rename(columns={"court_province": "province", "court_city": "city"})
    )
    raw_city_named = raw_city.rename(columns={"court_province": "province", "court_city": "city"})
    out = city_panel.merge(raw_city_named, on=["province", "city", "year"], how="left")
    out = out.merge(doc_city, on=["province", "city", "year"], how="left")
    for col in [
        "raw_city_year_case_n",
        "sample_case_n",
        "sample_decisive_case_n",
        "sample_case_n_quota_weighted",
    ]:
        out[col] = out[col].fillna(0.0)
    return out


def build_summary(
    court_map: pd.DataFrame,
    doc_df: pd.DataFrame,
    case_df: pd.DataFrame,
    city_panel: pd.DataFrame,
    firm_panel: pd.DataFrame,
) -> str:
    raw_unique_cases = case_df["case_uid"].nunique()
    sample_cases = doc_df["case_uid"].nunique()
    weighted_sample_total = doc_df["case_quota_weight"].sum()
    firm_total = firm_panel["civil_case_n"].sum()
    firm_quota_total = firm_panel["civil_case_n_quota_weighted"].sum()
    city_total = city_panel["raw_city_year_case_n"].sum()
    raw_matched_cases = case_df.loc[case_df["court_city"].notna(), "case_uid"].nunique()
    sample_matched_cases = doc_df.loc[doc_df["court_city"].notna(), "case_uid"].nunique()
    doc_city_match_rate = doc_df["court_city_matched"].mean()
    doc_province_match_rate = doc_df["court_province_matched"].mean()
    case_wvr_rows = int((pd.read_parquet(CASE_FILE, columns=["winner_vs_runnerup_case"])["winner_vs_runnerup_case"] == 1).sum())
    lead_rows = doc_df.nlargest(5, "case_quota_weight")[
        [
            "year",
            "case_uid",
            "court",
            "court_province",
            "court_city",
            "case_quota_weight",
        ]
    ]

    lines = [
        "# Analysis Panel Rebuild Summary",
        "",
        "This rebuild replaces the retuned firm-level path with a raw document-sample backbone.",
        "",
        "## Court matching",
        f"- Unique courts parsed: `{len(court_map):,}`",
        f"- Document rows with province match: `{doc_province_match_rate:.4f}`",
        f"- Document rows with city match: `{doc_city_match_rate:.4f}`",
        "",
        "## Aggregate checks",
        f"- Raw unique civil cases in `case_level`: `{raw_unique_cases:,}`",
        f"- Raw `case_level` rows flagged `winner_vs_runnerup_case == 1`: `{case_wvr_rows:,}`",
        f"- One-document-one-firm sample cases: `{sample_cases:,}`",
        f"- Raw document-sample total: `{sample_cases:,.0f}`",
        f"- Raw firm-level total: `{firm_total:,.0f}`",
        f"- Quota-weighted document total: `{weighted_sample_total:,.2f}`",
        f"- Quota-weighted firm-level total: `{firm_quota_total:,.2f}`",
        f"- Matched-city total in `city_year_panel`: `{city_total:,.2f}`",
        f"- Raw unique civil cases with matched city: `{raw_matched_cases:,}`",
        f"- Document-sample cases with matched city: `{sample_matched_cases:,}`",
        "",
        "## Identities",
        f"- `firm_level.civil_case_n` total equals raw document total: `{np.isclose(sample_cases, firm_total, rtol=0, atol=1e-6)}`",
        f"- `firm_level.civil_case_n_quota_weighted` total equals weighted document total: `{np.isclose(weighted_sample_total, firm_quota_total, rtol=0, atol=1e-6)}`",
        f"- Matched-city quota target is at least as large as matched document count: `{city_total >= sample_matched_cases}`",
        "",
        "## Largest quota weights",
    ]
    for row in lead_rows.itertuples(index=False):
        lines.append(
            f"- year `{int(row.year)}`, case `{row.case_uid}`, court `{row.court}`, location `{row.court_province}/{row.court_city}`, weight `{row.case_quota_weight:.3f}`"
        )
    lines.append("")
    return "\n".join(lines) + "\n"


def main() -> None:
    current_city_panel = pd.read_csv(CITY_FILE)
    known_cities = set(current_city_panel["city"].dropna().astype(str))

    case_df = pd.read_parquet(CASE_FILE, columns=["year", "case_uid", "court"])
    case_df = case_df.drop_duplicates(subset=["year", "case_uid"]).copy()
    doc_df = pd.read_parquet(DOC_FILE).copy()
    doc_df = doc_df.drop(columns=[col for col in DOC_DERIVED_DROP_COLS if col in doc_df.columns])

    court_map = parse_court_locations(
        pd.concat([case_df["court"], doc_df["court"]], ignore_index=True),
        known_cities=known_cities,
    )

    case_df = case_df.merge(court_map, on="court", how="left")
    doc_df = doc_df.merge(court_map, on="court", how="left")

    city_quota, province_quota, year_quota, raw_city = build_case_quota_tables(case_df, doc_df)

    doc_df = doc_df.merge(city_quota, on=["year", "court_province", "court_city"], how="left")
    doc_df = doc_df.merge(province_quota, on=["year", "court_province"], how="left")
    doc_df = doc_df.merge(year_quota, on="year", how="left")
    doc_df["case_quota_weight"] = choose_quota_weight(doc_df)
    doc_df["case_quota_weight_source"] = np.select(
        [
            doc_df["city_year_quota_weight"].notna(),
            doc_df["province_year_quota_weight"].notna(),
        ],
        ["city_year", "province_year"],
        default="year",
    )
    doc_df = normalize_quota_within_year(doc_df)

    static_meta = load_static_stack_meta()
    raw_attrs = load_raw_firm_year_attrs()
    firm_panel = build_firm_level_panel(doc_df, static_meta, raw_attrs)
    city_panel = augment_city_panel(current_city_panel, raw_city, doc_df)

    doc_df.to_parquet(DOC_FILE, compression="zstd", index=False)
    doc_df.to_csv(DOC_CSV_FILE, index=False)
    firm_panel.to_csv(FIRM_FILE, index=False)
    city_panel.to_csv(CITY_FILE, index=False)
    SUMMARY_FILE.write_text(
        build_summary(court_map, doc_df, case_df, city_panel, firm_panel),
        encoding="utf-8",
    )

    print(f"Wrote {DOC_FILE}")
    print(f"Wrote {DOC_CSV_FILE}")
    print(f"Wrote {FIRM_FILE}")
    print(f"Wrote {CITY_FILE}")
    print(f"Wrote {SUMMARY_FILE}")


if __name__ == "__main__":
    main()
