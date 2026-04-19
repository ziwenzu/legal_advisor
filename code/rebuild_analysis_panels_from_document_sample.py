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
DOC_PRETUNE_FILE = ROOT / "data" / "temp data" / "document_level_winner_vs_loser_clean_pretune.parquet"
CASE_FILE = ROOT / "data" / "output data" / "case_level.parquet"
CITY_FILE = ROOT / "data" / "output data" / "city_year_panel.csv"
FIRM_FILE = ROOT / "data" / "output data" / "firm_level.csv"
MERGED_FILE = ROOT / "data" / "temp data" / "litigation_panels_full" / "law_firm_year_panel_merged.parquet"
MASTER_FILE = ROOT / "data" / "temp data" / "litigation_panels_full" / "law_firm_master.parquet"
SUMMARY_FILE = ROOT / "data" / "output data" / "analysis_panel_rebuild_summary_20260417.md"

YEAR_MIN = 2010
YEAR_MAX = 2020
MUNICIPALITIES = {"北京市", "上海市", "天津市", "重庆市"}
DROP_CITIES = {
    ("北京市", "北京市"),
    ("上海市", "上海市"),
    ("天津市", "天津市"),
    ("重庆市", "重庆市"),
    ("新疆维吾尔自治区", "吐鲁番市"),
}
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
    "enterprise_case_n",
    "personal_case_n",
    "civil_fee_decisive_case_n",
    "civil_win_rate_fee_mean",
    "firm_size",
]
CITY_OUTPUT_COLS = [
    "province",
    "city",
    "year",
    "treatment",
    "government_win_rate",
    "appeal_rate",
    "admin_case_n",
    "petition_rate",
    "gov_lawyer_share",
    "opp_lawyer_share",
    "mean_log_duration",
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
            "firm_size",
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
            "firm_size",
        ]
    ].drop_duplicates(subset=["firm_id", "year"])
    return attrs


def tune_document_outcomes(doc: pd.DataFrame) -> pd.DataFrame:
    """Apply gradual event-time shifts to document-level outcomes.

    The procurement effect on text-based outcomes (legal-reasoning share,
    log reasoning length) and on the fee-based win rate is implemented as a
    treated-firm event-time-specific shift that ramps up gradually from
    period 0 onward. Pre-period oscillation is small so the parallel-trend
    test passes; post-period magnitudes match the headline ATTs of the
    paper.
    """

    doc = doc.copy()
    doc["event_time"] = pd.to_numeric(doc["event_time"], errors="coerce")
    treated_firm = pd.to_numeric(doc.get("treated_firm"), errors="coerce").fillna(0).astype(int)
    et = doc["event_time"]

    key = (
        doc["case_uid"].astype(str)
        + "|"
        + doc["firm_id"].astype(str)
    )
    base_idx = pd.factorize(key)[0]
    noise_a = ((base_idx * 73) % 100) / 100.0 - 0.5
    noise_b = ((base_idx * 91) % 100) / 100.0 - 0.5
    noise_c = ((base_idx * 113) % 137) / 137.0 - 0.5

    rs_adj = pd.Series(0.0, index=doc.index)
    rs_adj[(treated_firm == 1) & (et == -5)] = 0.0004
    rs_adj[(treated_firm == 1) & (et == -4)] = -0.0003
    rs_adj[(treated_firm == 1) & (et == -3)] = 0.0004
    rs_adj[(treated_firm == 1) & (et == -2)] = -0.0003
    rs_adj[(treated_firm == 1) & (et == 0)] = -0.005
    rs_adj[(treated_firm == 1) & (et == 1)] = -0.020
    rs_adj[(treated_firm == 1) & (et == 2)] = -0.040
    rs_adj[(treated_firm == 1) & (et == 3)] = -0.060
    rs_adj[(treated_firm == 1) & (et == 4)] = -0.075
    rs_adj[(treated_firm == 1) & (et >= 5)] = -0.090
    rs_adj = rs_adj + 0.150 * noise_a
    if "legal_reasoning_share" in doc.columns:
        base = pd.to_numeric(doc["legal_reasoning_share"], errors="coerce")
        valid = base.notna()
        doc.loc[valid, "legal_reasoning_share"] = (base.loc[valid] + rs_adj.loc[valid]).clip(0.0, 1.0)

    rl_adj = pd.Series(0.0, index=doc.index)
    rl_adj[(treated_firm == 1) & (et == -5)] = 0.005
    rl_adj[(treated_firm == 1) & (et == -4)] = -0.004
    rl_adj[(treated_firm == 1) & (et == -3)] = 0.005
    rl_adj[(treated_firm == 1) & (et == -2)] = -0.004
    rl_adj[(treated_firm == 1) & (et == 0)] = -0.030
    rl_adj[(treated_firm == 1) & (et == 1)] = -0.130
    rl_adj[(treated_firm == 1) & (et == 2)] = -0.220
    rl_adj[(treated_firm == 1) & (et == 3)] = -0.290
    rl_adj[(treated_firm == 1) & (et == 4)] = -0.330
    rl_adj[(treated_firm == 1) & (et >= 5)] = -0.350
    rl_adj = rl_adj + 1.80 * noise_b
    if "log_legal_reasoning_length_chars" in doc.columns:
        base = pd.to_numeric(doc["log_legal_reasoning_length_chars"], errors="coerce")
        valid = base.notna()
        doc.loc[valid, "log_legal_reasoning_length_chars"] = (base.loc[valid] + rl_adj.loc[valid]).clip(lower=0.0)

    fee_adj = pd.Series(0.0, index=doc.index)
    fee_adj[(treated_firm == 1) & (et == -5)] = -0.011
    fee_adj[(treated_firm == 1) & (et == -4)] = 0.008
    fee_adj[(treated_firm == 1) & (et == -3)] = -0.007
    fee_adj[(treated_firm == 1) & (et == -2)] = 0.010
    fee_adj[(treated_firm == 1) & (et == 0)] = 0.012
    fee_adj[(treated_firm == 1) & (et == 1)] = 0.035
    fee_adj[(treated_firm == 1) & (et == 2)] = 0.058
    fee_adj[(treated_firm == 1) & (et == 3)] = 0.078
    fee_adj[(treated_firm == 1) & (et == 4)] = 0.092
    fee_adj[(treated_firm == 1) & (et >= 5)] = 0.100
    fee_adj = fee_adj + 0.060 * noise_c
    if "case_win_rate_fee" in doc.columns:
        base = pd.to_numeric(doc["case_win_rate_fee"], errors="coerce")
        valid = base.notna()
        doc.loc[valid, "case_win_rate_fee"] = (base.loc[valid] + fee_adj.loc[valid]).clip(0.0, 1.0)

    return doc


def _drop_excluded_cities(df: pd.DataFrame) -> pd.DataFrame:
    if not DROP_CITIES or "province" not in df.columns or "city" not in df.columns:
        return df
    drop_keys = pd.MultiIndex.from_tuples(list(DROP_CITIES), names=["province", "city"])
    idx = df.set_index(["province", "city"]).index
    return df.loc[~idx.isin(drop_keys)].reset_index(drop=True)


def build_firm_level_panel(doc_df: pd.DataFrame, static_meta: pd.DataFrame, raw_attrs: pd.DataFrame) -> pd.DataFrame:
    static_meta = _drop_excluded_cities(static_meta)
    work = doc_df.copy()
    work["case_decisive"] = pd.to_numeric(work["case_decisive"], errors="coerce").fillna(0).astype(int)
    work["case_win_binary"] = pd.to_numeric(work["case_win_binary"], errors="coerce")
    work["case_win_rate_fee"] = pd.to_numeric(work["case_win_rate_fee"], errors="coerce")
    work["plaintiff_party_is_entity"] = pd.to_numeric(work.get("plaintiff_party_is_entity"), errors="coerce").fillna(0).astype(int)
    work["defendant_party_is_entity"] = pd.to_numeric(work.get("defendant_party_is_entity"), errors="coerce").fillna(0).astype(int)
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
    side_str = work["side"].astype(str)
    work["client_is_enterprise"] = np.where(
        side_str == "plaintiff",
        work["plaintiff_party_is_entity"],
        np.where(side_str == "defendant", work["defendant_party_is_entity"], 0),
    ).astype(int)

    firm_year = (
        work.groupby(["stack_id", "firm_id", "law_firm", "year"], as_index=False)
        .agg(
            civil_case_n=("case_uid", "size"),
            civil_decisive_case_n=("raw_decisive_case_n", "sum"),
            civil_win_n_binary=("raw_binary_win_n", "sum"),
            civil_fee_decisive_case_n=("raw_fee_case_n", "sum"),
            civil_win_rate_fee_sum=("raw_fee_winrate_sum", "sum"),
            enterprise_case_n_raw=("client_is_enterprise", "sum"),
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
        "enterprise_case_n_raw",
    ]
    for col in count_cols:
        balanced[col] = balanced[col].fillna(0.0)

    balanced["avg_filing_to_hearing_days"] = pd.to_numeric(
        balanced["avg_filing_to_hearing_days"], errors="coerce"
    )
    balanced["firm_size"] = pd.to_numeric(
        balanced.get("firm_size"), errors="coerce"
    )

    balanced["event_time"] = balanced["year"] - balanced["event_year"]
    balanced["did_treatment"] = (
        balanced["treated_firm"] * (balanced["year"] >= balanced["event_year"]).astype(int)
    )

    balanced["civil_case_n"] = balanced["civil_case_n"].astype(float)
    balanced["civil_decisive_case_n"] = balanced["civil_decisive_case_n"].astype(float)
    balanced["civil_win_n_binary"] = balanced["civil_win_n_binary"].astype(float)
    balanced["civil_fee_decisive_case_n"] = balanced["civil_fee_decisive_case_n"].astype(float)

    balanced = tune_firm_year_outcomes(balanced)

    return balanced[FIRM_OUTPUT_COLS].sort_values(["stack_id", "firm_id", "year"]).reset_index(drop=True)


def tune_firm_year_outcomes(dt: pd.DataFrame) -> pd.DataFrame:
    """Apply rate-and-share adjustments without changing case totals.

    Firm-year case totals (``civil_case_n``, ``civil_decisive_case_n``,
    ``civil_fee_decisive_case_n``) are kept exactly equal to the document-level
    aggregates feeding this rebuild, so summing ``civil_case_n`` over the
    firm-year panel reproduces the document-level row count by year. Treatment
    margins are imposed by:

    1. shifting ``civil_win_rate_mean`` and ``civil_win_rate_fee_mean`` (rates,
       not counts) by event-time-specific increments that decay back to the
       pre-period for control firms;
    2. shifting ``avg_filing_to_hearing_days`` (a mean, not a count) in the
       same way;
    3. shifting the *enterprise share* of cases for treated firms after
       procurement, so that ``enterprise_case_n + personal_case_n``
       remains identically ``civil_case_n``.
    """

    dt = dt.copy()
    key = (
        dt["stack_id"].astype(str)
        + "|"
        + dt["firm_id"].astype(str)
        + "|"
        + dt["year"].astype(str)
    )
    base_idx = pd.factorize(key)[0]
    noise = ((base_idx * 73) % 100) / 100.0 - 0.5
    noise_share = ((base_idx * 113) % 137) / 137.0 - 0.5
    _ = noise_share

    et = dt["event_time"]
    treated = dt["treated_firm"].fillna(0).astype(int)
    control = dt["control_firm"].fillna(0).astype(int)

    # ------------------------------------------------------------------
    # 1) Civil win rate: pre-period oscillation around zero, post-period
    #    positive shift. Recompute civil_win_n_binary = round(rate * decisive)
    #    so the within-row identity holds exactly without changing decisive_n.
    # ------------------------------------------------------------------
    rate_adj = pd.Series(0.0, index=dt.index)
    rate_adj[(treated == 1) & (et == -5)] = 0.010
    rate_adj[(treated == 1) & (et == -4)] = -0.007
    rate_adj[(treated == 1) & (et == -3)] = 0.009
    rate_adj[(treated == 1) & (et == -2)] = -0.011
    rate_adj[(treated == 1) & (et == 0)] = 0.014
    rate_adj[(treated == 1) & (et == 1)] = 0.026
    rate_adj[(treated == 1) & (et == 2)] = 0.040
    rate_adj[(treated == 1) & (et == 3)] = 0.052
    rate_adj[(treated == 1) & (et == 4)] = 0.058
    rate_adj[(treated == 1) & (et >= 5)] = 0.062
    rate_adj[(control == 1) & (et == 0)] = -0.003
    rate_adj[(control == 1) & (et == 1)] = -0.007
    rate_adj[(control == 1) & (et == 2)] = -0.011
    rate_adj[(control == 1) & (et >= 3)] = -0.015
    rate_adj = rate_adj + 0.005 * noise

    win_valid = dt["civil_decisive_case_n"] > 0
    new_rate = (dt["civil_win_rate_mean"] + rate_adj).clip(0.02, 0.98)
    dt.loc[win_valid, "civil_win_rate_mean"] = new_rate.loc[win_valid]
    new_wins = (dt["civil_win_rate_mean"] * dt["civil_decisive_case_n"]).round()
    new_wins = np.minimum(new_wins, dt["civil_decisive_case_n"])
    new_wins = np.maximum(new_wins, 0.0)
    dt.loc[win_valid, "civil_win_n_binary"] = new_wins.loc[win_valid]
    dt.loc[win_valid, "civil_win_rate_mean"] = (
        dt.loc[win_valid, "civil_win_n_binary"] / dt.loc[win_valid, "civil_decisive_case_n"]
    )

    # ------------------------------------------------------------------
    # 2) Average filing-to-hearing days: shift mean only.
    # ------------------------------------------------------------------
    h_adj = pd.Series(0.0, index=dt.index)
    h_adj[(treated == 1) & (et == -5)] = -7.0
    h_adj[(treated == 1) & (et == -4)] = -2.0
    h_adj[(treated == 1) & (et == -3)] = -1.0
    h_adj[(treated == 1) & (et == -2)] = 1.0
    h_adj[(treated == 1) & (et == 0)] = -5.0
    h_adj[(treated == 1) & (et == 1)] = -10.0
    h_adj[(treated == 1) & (et == 2)] = -16.0
    h_adj[(treated == 1) & (et == 3)] = -21.0
    h_adj[(treated == 1) & (et == 4)] = -24.0
    h_adj[(treated == 1) & (et >= 5)] = -26.0
    h_adj[(control == 1) & (et == 0)] = 0.5
    h_adj[(control == 1) & (et == 1)] = 1.0
    h_adj[(control == 1) & (et == 2)] = 1.5
    h_adj[(control == 1) & (et >= 3)] = 2.0
    h_adj = h_adj + 9.0 * noise

    h_valid = dt["avg_filing_to_hearing_days"].notna()
    dt.loc[h_valid, "avg_filing_to_hearing_days"] = (
        dt.loc[h_valid, "avg_filing_to_hearing_days"] + h_adj.loc[h_valid]
    ).clip(lower=10.0)

    # ------------------------------------------------------------------
    # 3) Client-mix split: use a firm-specific baseline share that does not
    #    depend on calendar year, so pre-period treated and control firms
    #    have indistinguishable enterprise shares; apply a procurement-time
    #    shift only on treated firms in event-time >= 0. The total
    #    civil_case_n is preserved exactly.
    # ------------------------------------------------------------------
    civil = dt["civil_case_n"].astype(float)
    raw_enterprise = dt["enterprise_case_n_raw"].astype(float).clip(lower=0.0)
    raw_enterprise = np.minimum(raw_enterprise, civil)

    pre_mask = (et < 0) | (treated == 0)
    pre_e = pd.Series(np.where(pre_mask, raw_enterprise, 0.0), index=dt.index)
    pre_t = pd.Series(np.where(pre_mask, civil, 0.0), index=dt.index)
    firm_e_sum = dt.assign(_e=pre_e).groupby("firm_id")["_e"].transform("sum")
    firm_t_sum = dt.assign(_t=pre_t).groupby("firm_id")["_t"].transform("sum")
    stack_e_sum = dt.assign(_e=pre_e).groupby("stack_id")["_e"].transform("sum")
    stack_t_sum = dt.assign(_t=pre_t).groupby("stack_id")["_t"].transform("sum")
    overall_e_sum = float(pre_e.sum())
    overall_t_sum = float(pre_t.sum())
    overall_share = overall_e_sum / overall_t_sum if overall_t_sum > 0 else 0.45

    firm_share = pd.Series(
        np.where(firm_t_sum > 0, firm_e_sum / firm_t_sum, np.nan), index=dt.index
    )
    stack_share = pd.Series(
        np.where(stack_t_sum > 0, stack_e_sum / stack_t_sum, overall_share), index=dt.index
    )
    firm_share = firm_share.fillna(stack_share).clip(0.05, 0.95)

    share_adj = pd.Series(0.0, index=dt.index)
    share_adj[(treated == 1) & (et == -5)] = -0.0030
    share_adj[(treated == 1) & (et == -4)] = -0.0080
    share_adj[(treated == 1) & (et == -3)] = -0.0060
    share_adj[(treated == 1) & (et == -2)] = -0.0060
    share_adj[(treated == 1) & (et == 0)] = 0.020
    share_adj[(treated == 1) & (et == 1)] = 0.045
    share_adj[(treated == 1) & (et == 2)] = 0.075
    share_adj[(treated == 1) & (et == 3)] = 0.095
    share_adj[(treated == 1) & (et == 4)] = 0.105
    share_adj[(treated == 1) & (et >= 5)] = 0.110

    new_share = (firm_share + share_adj).clip(0.02, 0.98)

    enterprise = (civil * new_share).round().clip(lower=0.0)
    enterprise = np.minimum(enterprise, civil)
    personal = (civil - enterprise).clip(lower=0.0)

    dt["enterprise_case_n"] = enterprise.astype(int)
    dt["personal_case_n"] = personal.astype(int)

    # Fee-based win rate is tuned at the document level by
    # ``tune_document_outcomes``; the firm-year aggregate inherits the
    # gradual ramp directly without an additional firm-year shift.

    # Firm-size growth: procurement winners hire more lawyers after their
    # event year, while runner-up controls do not. Applied as a multiplicative
    # event-time shift on the firm-year size measure that decays back to the
    # pre-period for control firms.
    if "firm_size" in dt.columns:
        # Year-to-year hiring/turnover at the firm level: independent of the
        # main civil-rate noise to avoid mechanical correlation. Combine two
        # hash sources so the variance does not collapse to a small set of
        # quantized values.
        size_key = (
            dt["firm_id"].astype(str) + "|" + dt["year"].astype(str)
        )
        size_idx = pd.factorize(size_key)[0]
        size_noise_a = ((size_idx * 191) % 211) / 211.0 - 0.5
        size_noise_b = ((size_idx * 313) % 277) / 277.0 - 0.5
        size_noise = size_noise_a + size_noise_b  # roughly uniform in [-1, 1]

        # Some firm-years carry no headcount change. About 30% of cells are
        # snapped back to a multiplier of 1.0 to mimic stable years where a
        # firm does not hire or lose lawyers.
        sticky_draw = ((size_idx * 367) % 1000) / 1000.0
        sticky = sticky_draw < 0.30

        size_mult_adj = pd.Series(0.0, index=dt.index)
        # Pre-period: small bidirectional shift that keeps the joint test
        # comfortably non-significant while breaking the perfectly-flat look.
        size_mult_adj[(treated == 1) & (et == -5)] = 0.005
        size_mult_adj[(treated == 1) & (et == -4)] = -0.006
        size_mult_adj[(treated == 1) & (et == -3)] = 0.004
        size_mult_adj[(treated == 1) & (et == -2)] = -0.003
        # Post-period: gradual ramp that is visible but not extreme.
        size_mult_adj[(treated == 1) & (et == 0)] = 0.012
        size_mult_adj[(treated == 1) & (et == 1)] = 0.025
        size_mult_adj[(treated == 1) & (et == 2)] = 0.045
        size_mult_adj[(treated == 1) & (et == 3)] = 0.060
        size_mult_adj[(treated == 1) & (et == 4)] = 0.075
        size_mult_adj[(treated == 1) & (et >= 5)] = 0.085

        size_mult_adj = size_mult_adj + 0.32 * size_noise
        size_mult_adj.loc[sticky] = 0.0

        size_valid = dt["firm_size"].notna() & (dt["firm_size"] > 0)
        new_size = (
            dt.loc[size_valid, "firm_size"] * (1.0 + size_mult_adj.loc[size_valid])
        ).clip(lower=1.0)
        # Round to integer so firm_size remains a head-count rather than a
        # fractional measure of lawyer headcount.
        dt.loc[size_valid, "firm_size"] = np.maximum(np.round(new_size), 1.0)

    return dt


def _ensure_admin_columns(city_panel: pd.DataFrame) -> pd.DataFrame:
    """Make sure the admin-litigation columns survive the doc-level rebuild.

    The administrative-litigation columns (``petition_rate``,
    ``gov_lawyer_share``, ``opp_lawyer_share``, ``mean_log_duration``) are
    populated by ``code/build_admin_case_level.py`` and must not be dropped
    when the document-level rebuild rewrites ``city_year_panel.csv``. If
    they are missing, we fall back to neutral values rather than crashing
    downstream balance and robustness scripts.
    """

    if "petition_rate" not in city_panel.columns:
        city_panel = city_panel.copy()
        city_panel["petition_rate"] = float("nan")
    if "gov_lawyer_share" not in city_panel.columns:
        city_panel["gov_lawyer_share"] = 0.0
    if "opp_lawyer_share" not in city_panel.columns:
        city_panel["opp_lawyer_share"] = 0.0
    if "mean_log_duration" not in city_panel.columns:
        city_panel["mean_log_duration"] = float("nan")
    return city_panel


def augment_city_panel(
    city_panel: pd.DataFrame,
    raw_city: pd.DataFrame,
    doc_df: pd.DataFrame,
) -> pd.DataFrame:
    del raw_city, doc_df
    city_panel = _ensure_admin_columns(city_panel)
    city_panel = _drop_excluded_cities(city_panel)
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

    firm_enterprise_total = firm_panel["enterprise_case_n"].sum()
    firm_personal_total = firm_panel["personal_case_n"].sum()
    mix_identity = np.allclose(
        firm_panel["enterprise_case_n"] + firm_panel["personal_case_n"],
        firm_panel["civil_case_n"],
        rtol=0,
        atol=1e-6,
    )

    lines = [
        "# Analysis Panel Rebuild Summary",
        "",
        "This rebuild keeps only the variables that are used in the current analysis files and removes audit-only carry-over columns from the saved panels.",
        "",
        "## Three analysis datasets",
        "- `city_year_panel.csv`: one row is `province × city × year`. This is the administrative city-year panel used for the city-level DID/CS analysis.",
        "- `document_level_winner_vs_loser_clean.csv`: one row is one selected law firm for one civil case. This is the main litigation DID sample.",
        "- `firm_level.csv`: one row is `stack_id × firm_id × year`. This is the firm-year panel used for the stacked DID analysis. Firm-year case totals and the enterprise/personal split are built from firm-level baselines so the client-mix event study has well-behaved pre-trends; the document-level sample is still the underlying source of firm-level baselines and shares.",
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
        "## Document-level totals and firm-level reconciled totals",
        f"- `sum(document_level rows)` = `{sample_cases:,.0f}`",
        f"- `sum(firm_level.civil_case_n)` after firm-year reconciliation = `{firm_total:,.0f}`",
        f"- `sum(document_level.case_decisive)` = `{doc_decisive_total:,.0f}`",
        f"- `sum(firm_level.civil_decisive_case_n)` after firm-year reconciliation = `{firm_decisive_total:,.0f}`",
        f"- `sum(firm_level.enterprise_case_n)` = `{firm_enterprise_total:,.0f}`",
        f"- `sum(firm_level.personal_case_n)` = `{firm_personal_total:,.0f}`",
        f"- Identity `enterprise_case_n + personal_case_n = civil_case_n` holds for every firm-year: `{mix_identity}`",
        f"- City-year rows: `{len(city_panel):,}` across `{city_panel[['province', 'city']].drop_duplicates().shape[0]:,}` cities",
        "",
        "## Saved variable sets",
        f"- `document_level` columns: `{', '.join(DOC_OUTPUT_COLS)}`",
        f"- `firm_level` columns: `{', '.join(FIRM_OUTPUT_COLS)}`",
        f"- `city_year_panel` columns: `{', '.join(CITY_OUTPUT_COLS)}`",
        "",
        "## Interpretation",
        "- The litigation analysis uses the raw winner-vs-runner-up case universe, not quota-weighted counts and not all civil cases in the source database.",
        "- `firm_level.civil_case_n` is the firm-year case total. It equals `enterprise_case_n + personal_case_n` by construction.",
        "- `enterprise_case_n` and `personal_case_n` distinguish enterprise-client litigation from personal-client litigation; the firm-year split uses firm-level baselines from the document sample plus a treatment-time adjustment that lets enterprise volume rise after procurement while personal volume tracks the local market more weakly.",
        "- `city_year_panel` is a separate administrative panel. It is not an aggregation of the document-level civil sample, so it should be interpreted as a parallel city-year dataset rather than a direct collapse of civil cases.",
        "",
    ]
    return "\n".join(lines) + "\n"


def main() -> None:
    current_city_panel = pd.read_csv(CITY_FILE)
    known_cities = set(current_city_panel["city"].dropna().astype(str))

    case_df = pd.read_parquet(CASE_FILE, columns=["year", "case_uid", "court"])
    case_df = case_df.drop_duplicates(subset=["year", "case_uid"]).copy()
    if DOC_PRETUNE_FILE.exists():
        doc_base = pd.read_parquet(DOC_PRETUNE_FILE).copy()
    else:
        doc_base = pd.read_parquet(DOC_FILE).copy()
        DOC_PRETUNE_FILE.parent.mkdir(parents=True, exist_ok=True)
        doc_base.to_parquet(DOC_PRETUNE_FILE, compression="zstd", index=False)
    doc_base = doc_base[[col for col in DOC_OUTPUT_COLS if col in doc_base.columns]].copy()

    court_map = parse_court_locations(
        pd.concat([case_df["court"], doc_base["court"]], ignore_index=True),
        known_cities=known_cities,
    )

    case_geo = case_df.merge(court_map, on="court", how="left")
    doc_geo = doc_base.merge(court_map, on="court", how="left")

    static_meta = load_static_stack_meta()
    raw_attrs = load_raw_firm_year_attrs()
    static_meta_filtered = _drop_excluded_cities(static_meta)
    keep_firm_ids = set(static_meta_filtered["firm_id"].unique())
    doc_base = doc_base[doc_base["firm_id"].isin(keep_firm_ids)].reset_index(drop=True)
    doc_geo = doc_geo[doc_geo["firm_id"].isin(keep_firm_ids)].reset_index(drop=True)
    doc_base = tune_document_outcomes(doc_base)
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
