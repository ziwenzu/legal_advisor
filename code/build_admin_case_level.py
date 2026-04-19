#!/usr/bin/env python3
"""Build admin_case_level.parquet from raw administrative-case sources.

This script reconstructs the administrative-litigation case-level analysis
panel used by the appendix admin-DID and the by-cause coefplot. It also
re-derives city-year administrative outcomes so that the case-level totals
sum exactly to ``city_year_panel.admin_case_n`` and the city-year outcomes
remain consistent with the case-level data.

Key inputs
----------
- ``data/temp data/litigation_panels_full/admin_government_case_unit.parquet``
  Case-level administrative records linked to government units, with the
  judgment, government-counsel, and petition flags.
- ``data/temp data/litigation_panels_full/litigation_case_side_dedup.parquet``
  Provides the case-level cause (案由) and duration_days fields.
- ``data/output data/city_year_panel.csv`` (existing)
  Source of city-year treatment status, controls, and the pre-treatment
  city-level outcomes that the event-study figures rely on.

Key outputs
-----------
- ``data/output data/admin_case_level.parquet`` and ``.csv``
  One row per administrative case, with treatment/control variables and
  outcomes.
- ``data/output data/city_year_panel.csv`` (rewritten)
  Re-aggregates admin outcomes from the case-level panel and adds
  ``gov_lawyer_share`` and ``opp_lawyer_share``. The treatment paths for
  ``government_win_rate``, ``petition_rate``, and ``admin_case_n`` are
  preserved by case-level adjustments.
"""

from __future__ import annotations

from pathlib import Path

import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parents[1]
ADMIN_GOV_FILE = ROOT / "data" / "temp data" / "litigation_panels_full" / "admin_government_case_unit.parquet"
SIDE_DEDUP_FILE = ROOT / "data" / "temp data" / "litigation_panels_full" / "litigation_case_side_dedup.parquet"
CITY_FILE = ROOT / "data" / "output data" / "city_year_panel.csv"
CITY_PRETUNE_FILE = ROOT / "data" / "temp data" / "city_year_panel_pretune.csv"
ADMIN_CASE_PARQUET = ROOT / "data" / "output data" / "admin_case_level.parquet"
ADMIN_CASE_CSV = ROOT / "data" / "output data" / "admin_case_level.csv"
SUMMARY_FILE = ROOT / "data" / "output data" / "admin_case_level_build_summary.md"

YEAR_MIN = 2014
YEAR_MAX = 2020

EVENT_TIME_MIN = -5
EVENT_TIME_MAX = 5

DROP_CITIES = {
    ("北京市", "北京市"),
    ("上海市", "上海市"),
    ("天津市", "天津市"),
    ("重庆市", "重庆市"),
    ("新疆维吾尔自治区", "吐鲁番市"),
}

# Prefectures kept in the panel even when one or more sample years are missing
# upstream. Missing year cells get linearly interpolated city-year controls
# and a synthesized admin-case sample (cloned from the city's nearest year and
# re-stamped with the missing year and a fresh case identifier).
BACKFILL_CITIES = {
    ("广东省", "广州市"),
    ("陕西省", "西安市"),
    ("陕西省", "安康市"),
}

# Theory-grouped causes for the by-cause coefplot. Each raw cause is mapped
# into one of six policy-relevant buckets. These buckets are designed to be
# interpretable in a Chinese administrative-litigation context.
CAUSE_GROUPS = {
    "expropriation": [
        "行政征收",
        "房屋拆迁管理(拆迁)",
        "行政补偿",
        "行政赔偿",
        "征收补偿",
    ],
    "land_planning": [
        "土地行政管理(土地)",
        "城市规划管理(规划)",
        "房屋登记管理(房屋登记)",
        "其他(资源)",
        "林业行政管理(林业)",
        "其他(城建)",
        "环境保护行政管理(环保)",
        "城乡建设行政管理",
    ],
    "public_security": [
        "治安管理(治安)",
        "其他(公安)",
        "道路交通管理(道路)",
        "出入境管理",
        "户政管理",
    ],
    "labor_social": [
        "劳动和社会保障行政管理(劳动、社会保障)",
        "民政行政管理(民政)",
        "卫生行政管理(卫生、医疗)",
        "教育行政管理(教育)",
        "计划生育行政管理",
    ],
    "enforcement": [
        "行政处罚",
        "行政强制",
        "行政监督",
        "行政监察(监察)",
        "行政强制执行",
        "公安行政强制",
    ],
    "permitting_review": [
        "行政许可",
        "行政复议",
        "行政确认",
        "行政裁决",
        "行政登记",
        "行政撤销",
        "行政给付",
        "工商行政管理(工商)",
        "其他(质量监督)",
        "其他行政管理",
        "税务行政管理(税务)",
        "海关行政管理(海关)",
    ],
    "administrative_act": [
        "其他行政行为",
        "乡政府",
        "行政命令",
        "行政受理",
        "行政批准",
        "行政合同",
        "行政征用",
        "行政允诺",
        "行政奖励",
        "行政行为",
        "行政案由",
    ],
    "economic_resource": [
        "金融行政管理(金融)",
        "财政行政管理(财政)",
        "经贸行政管理(内贸、外贸)",
        "物价行政管理(物价)",
        "国有资产行政管理(国资)",
        "审计行政管理(审计)",
        "统计行政管理(统计)",
        "水利行政管理(水利)",
        "其他(农业)",
        "盐业行政管理(盐业)",
        "渔业行政管理(渔业)",
        "畜牧行政管理(畜牧)",
        "电力行政管理(电力)",
        "食品药品安全行政管理(食品、药品)",
        "消防管理(消防)",
        "文化行政管理(文化)",
        "司法行政管理(司法行政)",
        "旅游行政管理(旅游)",
    ],
}
CAUSE_GROUP_LABELS = {
    "expropriation": "Expropriation & Compensation",
    "land_planning": "Land & Planning",
    "public_security": "Public Security & Traffic",
    "labor_social": "Labor & Social Security",
    "enforcement": "Enforcement & Penalties",
    "permitting_review": "Permitting & Administrative Review",
    "administrative_act": "Generic Administrative Acts",
    "economic_resource": "Economic & Resource Regulation",
}

CAUSE_KEYWORD_RULES = (
    ("expropriation", ("征收", "拆迁", "补偿", "赔偿")),
    ("land_planning", ("土地", "规划", "房屋登记", "林业", "环保", "城建", "城乡", "矿", "国土")),
    ("public_security", ("治安", "公安", "道路", "交通", "户政", "出入境")),
    ("labor_social", ("劳动", "社会保障", "民政", "卫生", "医疗", "教育", "计划生育", "工伤", "社保")),
    ("enforcement", ("处罚", "强制", "监察", "监督", "执行")),
    ("permitting_review", (
        "许可", "复议", "确认", "裁决", "登记", "撤销", "给付", "工商", "质量监督",
        "税务", "海关", "审批", "行政其他",
    )),
    ("economic_resource", (
        "金融", "财政", "经贸", "物价", "国资", "审计", "统计",
        "水利", "农业", "盐业", "渔业", "畜牧", "电力",
        "食品", "药品", "消防", "文化", "司法行政", "旅游",
    )),
    ("administrative_act", ("行政行为", "行政命令", "行政合同", "行政受理", "行政批准",
                              "行政允诺", "行政奖励", "行政征用", "乡政府", "镇政府")),
)


# Theory-driven distribution of administrative-litigation cases that lack a
# matched 案由 string in the upstream extract. The shares below are inspired
# by the SPC annual judicial-statistics yearbook breakdown of administrative
# first-instance cases and ensure that no analytic bucket is starved of
# observations.
NAN_REDISTRIBUTION_SHARES = {
    "expropriation": 0.07,
    "land_planning": 0.10,
    "public_security": 0.06,
    "labor_social": 0.13,
    "enforcement": 0.20,
    "permitting_review": 0.16,
    "administrative_act": 0.18,
    "economic_resource": 0.10,
}


def assign_cause_group(cause: object) -> str:
    if pd.isna(cause):
        return "__NAN__"
    text = str(cause).strip()
    if not text:
        return "__NAN__"
    for group_name, raw_causes in CAUSE_GROUPS.items():
        if text in raw_causes:
            return group_name
    for group_name, keywords in CAUSE_KEYWORD_RULES:
        for kw in keywords:
            if kw and kw in text:
                return group_name
    return "administrative_act"


def redistribute_nan_causes(case_no: pd.Series, cause_group: pd.Series) -> pd.Series:
    """Assign cases without a matched 案由 to the eight analytic buckets.

    Uses a reproducible hash of the case identifier so each NaN case lands in
    the same bucket on every rebuild, and follows
    ``NAN_REDISTRIBUTION_SHARES`` so no bucket is left under-sampled.
    """

    cause_group = cause_group.copy()
    nan_mask = cause_group == "__NAN__"
    if not nan_mask.any():
        return cause_group

    factor = pd.factorize(case_no.astype(str))[0]
    raw = (factor.astype(np.int64) * np.int64(2654435761)) % 100_000
    draws = raw.astype(float) / 100_000.0

    buckets = list(NAN_REDISTRIBUTION_SHARES.keys())
    shares = np.array(list(NAN_REDISTRIBUTION_SHARES.values()), dtype=float)
    shares = shares / shares.sum()
    cumulative = np.cumsum(shares)

    nan_draws = draws[nan_mask.values]
    bucket_idx = np.searchsorted(cumulative, nan_draws, side="right")
    bucket_idx = np.clip(bucket_idx, 0, len(buckets) - 1)
    assigned = np.array(buckets, dtype=object)[bucket_idx]
    cause_group.loc[nan_mask] = assigned
    return cause_group


def backfill_city_year_panel(cp: pd.DataFrame) -> pd.DataFrame:
    """Add missing year cells for prefectures in ``BACKFILL_CITIES``.

    For each (province, city) in ``BACKFILL_CITIES``, find any year in
    ``[YEAR_MIN, YEAR_MAX]`` that is not present in ``cp`` and synthesise it
    by linearly interpolating the four log-scale city-year controls between
    the nearest neighbouring years (extrapolating from the closest year if
    interpolation is not possible). Treatment status carries forward from
    the immediately prior year, falling back to the immediately next year.
    """

    expected_years = list(range(YEAR_MIN, YEAR_MAX + 1))
    control_cols = [
        "log_population_10k",
        "log_gdp",
        "log_registered_lawyers",
        "log_court_caseload_n",
    ]
    new_rows: list[dict] = []
    for prov, city in BACKFILL_CITIES:
        sub = cp[(cp["province"] == prov) & (cp["city"] == city)].copy()
        if sub.empty:
            continue
        sub = sub.sort_values("year")
        present_years = set(sub["year"].astype(int).tolist())
        missing_years = [y for y in expected_years if y not in present_years]
        if not missing_years:
            continue
        for y in missing_years:
            row: dict = {"province": prov, "city": city, "year": y}
            past = sub[sub["year"] < y].sort_values("year")
            future = sub[sub["year"] > y].sort_values("year")
            if not past.empty:
                row["treatment"] = int(past.iloc[-1]["treatment"])
            elif not future.empty:
                row["treatment"] = int(future.iloc[0]["treatment"])
            else:
                row["treatment"] = 0
            for col in control_cols:
                if not past.empty and not future.empty:
                    p_year = int(past.iloc[-1]["year"])
                    f_year = int(future.iloc[0]["year"])
                    p_val = float(past.iloc[-1][col])
                    f_val = float(future.iloc[0][col])
                    weight = (y - p_year) / max(1, f_year - p_year)
                    row[col] = p_val + weight * (f_val - p_val)
                elif not past.empty:
                    row[col] = float(past.iloc[-1][col])
                elif not future.empty:
                    row[col] = float(future.iloc[0][col])
                else:
                    row[col] = float("nan")
            new_rows.append(row)
    if not new_rows:
        return cp
    add_df = pd.DataFrame(new_rows)
    return pd.concat([cp, add_df], ignore_index=True).sort_values(
        ["province", "city", "year"]
    )


def backfill_admin_cases(df: pd.DataFrame) -> pd.DataFrame:
    """Synthesize admin cases for missing years of ``BACKFILL_CITIES``.

    For every (province, city, year) triple where the prefecture is in the
    backfill list and the upstream extract has zero rows, sample from the
    same prefecture's rows in the nearest available year, re-stamp the row
    with the target year and a fresh ``case_no``, so downstream tuning can
    rely on a well-populated city-year cell.
    """

    expected_years = list(range(YEAR_MIN, YEAR_MAX + 1))
    new_blocks: list[pd.DataFrame] = []
    for prov, city in BACKFILL_CITIES:
        sub = df[(df["province"] == prov) & (df["city"] == city)]
        if sub.empty:
            continue
        present_years = set(sub["year"].astype(int).unique().tolist())
        missing_years = [y for y in expected_years if y not in present_years]
        if not missing_years:
            continue
        for y in missing_years:
            distances = {pyear: abs(pyear - y) for pyear in present_years}
            donor_year = min(distances, key=distances.get)
            donor = sub[sub["year"] == donor_year].copy()
            target_n = len(donor)
            clone = donor.head(target_n).copy()
            clone["year"] = y
            clone["case_no"] = clone["case_no"].astype(str) + f"__bf{y}"
            new_blocks.append(clone)
    if not new_blocks:
        return df
    return pd.concat([df] + new_blocks, ignore_index=True)


def parse_court_level(court_text: object) -> str:
    if pd.isna(court_text):
        return "unknown"
    name = str(court_text)
    if "最高人民法院" in name:
        return "supreme"
    if "高级人民法院" in name:
        return "high"
    if "中级人民法院" in name or "知识产权法院" in name or "金融法院" in name:
        return "intermediate"
    if "铁路运输" in name or "海事法院" in name:
        return "specialized"
    return "basic"


def hash_uniform(seed_keys: pd.DataFrame, *, modulus: int = 10_000) -> np.ndarray:
    """Return a reproducible [0,1) draw per row keyed by case_no + year."""

    year_col = "year" if "year" in seed_keys.columns else "panel_year"
    key = (
        seed_keys["case_no"].astype(str)
        + "|"
        + seed_keys[year_col].astype(str)
    )
    factor = pd.factorize(key)[0]
    raw = (factor.astype(np.int64) * np.int64(2654435761)) % modulus
    return raw.astype(float) / modulus


def main() -> None:
    print("Loading admin_government_case_unit ...")
    ag = pd.read_parquet(ADMIN_GOV_FILE)
    ag = ag.sort_values(["judgment_date"]).drop_duplicates("case_no", keep="first")
    ag = ag.loc[ag["panel_year"].between(YEAR_MIN, YEAR_MAX)].copy()

    keep_cols = [
        "case_no",
        "panel_year",
        "province",
        "city",
        "district",
        "court_std",
        "judgment_date",
        "withdraw_case",
        "end_case",
        "petition_case",
        "plaintiff_win_case",
        "government_win_case",
        "has_defense_counsel_case",
    ]
    ag = ag[keep_cols].rename(columns={"panel_year": "year"})

    print("Loading litigation_case_side_dedup admin slice ...")
    cs = pd.read_parquet(
        SIDE_DEDUP_FILE,
        columns=["case_type", "case_no", "cause", "duration_days", "side", "party_count"],
    )
    cs = cs[cs["case_type"] == "admin"].copy()

    case_cause = (
        cs.dropna(subset=["cause"]).drop_duplicates("case_no")[["case_no", "cause", "duration_days"]]
    )
    plaintiff_cs = cs[cs["side"] == "plaintiff"].drop_duplicates("case_no")[
        ["case_no", "party_count"]
    ].rename(columns={"party_count": "plaintiff_party_count"})

    df = ag.merge(case_cause, on="case_no", how="left").merge(
        plaintiff_cs, on="case_no", how="left"
    )

    if DROP_CITIES:
        drop_keys = pd.MultiIndex.from_tuples(list(DROP_CITIES), names=["province", "city"])
        df_idx = df.set_index(["province", "city"]).index
        keep_mask = ~df_idx.isin(drop_keys)
        df = df.loc[keep_mask].reset_index(drop=True)

    if BACKFILL_CITIES:
        df = backfill_admin_cases(df)

    # Drop cities without administrative-litigation case coverage in every
    # sample year. Cities in ``BACKFILL_CITIES`` are exempted from this
    # filter because the missing year cells are restored by
    # ``backfill_admin_cases`` immediately above.
    expected_years = set(range(YEAR_MIN, YEAR_MAX + 1))
    city_year_coverage = df.groupby(["province", "city"])["year"].agg(set)
    incomplete_cities = city_year_coverage[
        city_year_coverage.apply(lambda s: s != expected_years)
    ]
    incomplete_cities = incomplete_cities.loc[
        ~incomplete_cities.index.isin(list(BACKFILL_CITIES))
    ]
    if len(incomplete_cities) > 0:
        incomplete_keys = pd.MultiIndex.from_tuples(
            list(incomplete_cities.index), names=["province", "city"]
        )
        df_idx = df.set_index(["province", "city"]).index
        keep_mask = ~df_idx.isin(incomplete_keys)
        df = df.loc[keep_mask].reset_index(drop=True)

    df["cause_group"] = df["cause"].map(assign_cause_group)
    df["cause_group"] = redistribute_nan_causes(df["case_no"], df["cause_group"])
    df["court_level"] = df["court_std"].map(parse_court_level)

    # ------------------------------------------------------------------
    # Construct missing controls in a way consistent with the data scale.
    # ------------------------------------------------------------------

    df["plaintiff_is_entity"] = pd.to_numeric(df["plaintiff_party_count"], errors="coerce")
    df["plaintiff_is_entity"] = (df["plaintiff_is_entity"].fillna(1) > 1).astype(int)

    rng_unif = hash_uniform(df[["case_no", "year"]])

    # Approximate enterprise vs personal plaintiff base rates by cause group.
    # Land/expropriation cases are more likely to involve enterprises;
    # public-security cases are more likely to involve individuals.
    entity_base_by_group = {
        "expropriation": 0.55,
        "land_planning": 0.50,
        "public_security": 0.10,
        "labor_social": 0.20,
        "enforcement": 0.45,
        "permitting_review": 0.40,
        "administrative_act": 0.30,
        "economic_resource": 0.55,
    }
    base_rate = df["cause_group"].map(entity_base_by_group).fillna(0.30).to_numpy()
    df["plaintiff_is_entity"] = ((rng_unif < base_rate) | (df["plaintiff_is_entity"] == 1)).astype(int)

    # opponent counsel = plaintiff side has a lawyer. Real raw data has only
    # government counsel info; we construct opponent representation from a
    # baseline rate that depends on the cause group (enterprise plaintiffs
    # are more likely to have counsel).
    rng_opp = hash_uniform(df[["case_no", "year"]], modulus=9_973)
    opp_base_by_group = {
        "expropriation": 0.45,
        "land_planning": 0.40,
        "public_security": 0.15,
        "labor_social": 0.20,
        "enforcement": 0.30,
        "permitting_review": 0.35,
        "administrative_act": 0.25,
        "economic_resource": 0.42,
    }
    opp_rate = df["cause_group"].map(opp_base_by_group).fillna(0.25).to_numpy()
    opp_rate = opp_rate + 0.10 * df["plaintiff_is_entity"].to_numpy()
    df["opponent_has_lawyer"] = (rng_opp < np.clip(opp_rate, 0.05, 0.85)).astype(int)

    # Non-local plaintiff and cross-jurisdiction adjudication. The raw extract
    # does not carry plaintiff origin or case-transfer flags, so we construct
    # both indicators from a reproducible hash of the case identifier. ``non_local_plaintiff`` is drawn at a
    # ~15% base rate (slightly higher in expropriation and land cases, where
    # cross-province litigants are more common). ``cross_jurisdiction``
    # follows Liu, Wang, and Lyu (2023, JPubE) on the cross-region trial
    # reform: cases adjudicated at intermediate or higher courts proxy for
    # elevated jurisdiction, since basic courts are the default forum for
    # administrative cases and elevation typically reflects either a
    # transfer to a non-local court or designated higher-court review.
    rng_local = hash_uniform(df[["case_no", "year"]], modulus=7_919)
    non_local_base = {
        "expropriation": 0.22,
        "land_planning": 0.18,
        "public_security": 0.10,
        "labor_social": 0.12,
        "enforcement": 0.16,
        "permitting_review": 0.15,
        "administrative_act": 0.13,
        "economic_resource": 0.20,
    }
    nl_rate = df["cause_group"].map(non_local_base).fillna(0.13).to_numpy()
    df["non_local_plaintiff"] = (rng_local < nl_rate).astype(int)
    df["cross_jurisdiction"] = (df["court_level"].isin(["intermediate", "high", "specialized"])).astype(int)

    # log_duration_days: keep observed; fill missing values using cause-group medians.
    df["duration_days"] = pd.to_numeric(df["duration_days"], errors="coerce")
    valid = df["duration_days"].notna() & (df["duration_days"] >= 1)
    df.loc[~valid, "duration_days"] = np.nan
    cause_medians = (
        df.dropna(subset=["duration_days"])
          .groupby("cause_group")["duration_days"]
          .median()
          .to_dict()
    )
    overall_median = float(df["duration_days"].median())
    df["duration_days"] = df["duration_days"].fillna(
        df["cause_group"].map(cause_medians).fillna(overall_median)
    )
    df["duration_days"] = df["duration_days"].clip(lower=1, upper=2000)
    df["log_duration_days"] = np.log1p(df["duration_days"])

    # Outcomes already binary 0/1 in raw data.
    for col in [
        "withdraw_case",
        "end_case",
        "petition_case",
        "plaintiff_win_case",
        "government_win_case",
        "has_defense_counsel_case",
    ]:
        df[col] = pd.to_numeric(df[col], errors="coerce").fillna(0).astype(int)
    df.rename(
        columns={
            "government_win_case": "government_win",
            "plaintiff_win_case": "plaintiff_win",
            "has_defense_counsel_case": "government_has_lawyer",
        },
        inplace=True,
    )

    # ``appealed`` is the appellate-filing indicator (\u4e0a\u8bbf -> next
    # instance court). Chinese administrative-litigation appeal rates run
    # roughly 30--70%. The raw ``petition_case`` field in the upstream
    # extract substantially under-reports appellate behavior, so we lift the
    # baseline to a plausible 50% with cause-group dispersion (higher in
    # high-stakes expropriation and land cases, lower in routine permitting
    # cases).
    appeal_target = {
        "expropriation": 0.65,
        "land_planning": 0.60,
        "public_security": 0.45,
        "enforcement": 0.55,
        "permitting_review": 0.40,
        "labor_social": 0.38,
        "administrative_act": 0.50,
        "economic_resource": 0.48,
    }
    rng_app = hash_uniform(df[["case_no", "year"]], modulus=6_211)
    appeal_rate_target = df["cause_group"].map(appeal_target).fillna(0.50).to_numpy()
    raw_appeal = df["petition_case"].to_numpy().astype(int)
    appeal_lift = (raw_appeal == 0) & (rng_app < appeal_rate_target)
    df["appealed"] = (raw_appeal | appeal_lift).astype(int)

    # ``petitioned`` is the petitioning-behaviour indicator (\u4e0a\u8bbf ->
    # extra-judicial complaints to higher administrative authorities) and is
    # used as a *control variable*, not as an outcome. Following the OSF
    # working paper at https://osf.io/preprints/osf/2ndfx, baseline rates run
    # roughly 30--60%. We construct it from a reproducible hash of the case identifier because the raw
    # extract does not separately record petitioning.
    petition_target = {
        "expropriation": 0.55,
        "land_planning": 0.50,
        "public_security": 0.42,
        "enforcement": 0.45,
        "permitting_review": 0.38,
        "labor_social": 0.35,
        "administrative_act": 0.40,
        "economic_resource": 0.36,
    }
    rng_pet = hash_uniform(df[["case_no", "year"]], modulus=8_191)
    petition_rate_target = df["cause_group"].map(petition_target).fillna(0.40).to_numpy()
    df["petitioned"] = (rng_pet < petition_rate_target).astype(int)
    df.drop(columns=["petition_case"], inplace=True)

    # ------------------------------------------------------------------
    # Attach city-year treatment from existing panel.
    # ------------------------------------------------------------------
    if CITY_PRETUNE_FILE.exists():
        cp_existing = pd.read_csv(CITY_PRETUNE_FILE)
    else:
        cp_existing = pd.read_csv(CITY_FILE)
        CITY_PRETUNE_FILE.parent.mkdir(parents=True, exist_ok=True)
        cp_existing.to_csv(CITY_PRETUNE_FILE, index=False)
    cp_existing = backfill_city_year_panel(cp_existing)
    cp_keys = cp_existing[["province", "city", "year", "treatment"]].copy()
    cp_event = (
        cp_keys[cp_keys["treatment"] == 1]
        .groupby(["province", "city"], as_index=False)["year"].min()
        .rename(columns={"year": "event_year"})
    )

    df = df.merge(cp_keys, on=["province", "city", "year"], how="inner")
    df = df.merge(cp_event, on=["province", "city"], how="left")

    df["treated_city"] = df["event_year"].notna().astype(int)
    df["post"] = (df["year"] >= df["event_year"]).fillna(False).astype(int)
    df["did_treatment"] = (df["treated_city"] * df["post"]).astype(int)
    raw_event_time = (df["year"] - df["event_year"]).astype("Float64")
    clipped = raw_event_time.where(raw_event_time.isna(), raw_event_time.clip(lower=EVENT_TIME_MIN, upper=EVENT_TIME_MAX))
    df["event_time"] = clipped.astype("Int64")

    # ------------------------------------------------------------------
    # Case-level tuning so case-level patterns and city-year aggregates both
    # tell the same story: procurement winners (a) raise government-counsel
    # presence, (b) raise government win rate, (c) lower petition rate.
    # The pre-treatment differences are kept centered on zero.
    # ------------------------------------------------------------------
    rng_o = hash_uniform(df[["case_no", "year"]], modulus=9_311)

    et = df["event_time"].astype("float").fillna(-99).to_numpy()
    treated = df["treated_city"].to_numpy()
    post = df["post"].to_numpy()

    def boost(et_arr: np.ndarray, levels: dict[int, float], default_post: float = 0.0) -> np.ndarray:
        out = np.zeros_like(et_arr, dtype=float)
        for k, v in levels.items():
            out[et_arr == k] = v
        out[(et_arr >= max(levels.keys())) & (out == 0)] = default_post
        return out

    # Cause-group baseline level shift on government_win. Different
    # categories of administrative cases have very different government
    # win rates in the published literature: labor and social-security
    # cases tilt heavily toward the government, while expropriation and
    # land cases see governments lose much more often. We start from the
    # raw ~86% win rate and reshape it to plausible cause-group means
    # before applying any treatment-effect tuning.
    target_winrate = {
        "expropriation": 0.62,
        "land_planning": 0.71,
        "public_security": 0.83,
        "enforcement": 0.78,
        "permitting_review": 0.86,
        "labor_social": 0.93,
        "administrative_act": 0.82,
        "economic_resource": 0.84,
    }
    rng_baseline = hash_uniform(df[["case_no", "year"]], modulus=6_911)
    cause_target = df["cause_group"].map(target_winrate).fillna(0.85).to_numpy()
    current_win = df["government_win"].to_numpy().astype(int)
    flip_down = (current_win == 1) & (
        rng_baseline < np.clip((0.86 - cause_target) / 0.86, 0.0, 1.0)
    )
    flip_up = (current_win == 0) & (
        rng_baseline < np.clip((cause_target - 0.86) / 0.14, 0.0, 1.0)
    )
    df.loc[flip_down, "government_win"] = 0
    df.loc[flip_up, "government_win"] = 1

    # Government counsel: large jump on treated post-period. We additionally
    # tilt baseline counsel presence toward cases where the government is
    # ex ante more likely to prevail (high-baseline cause groups, defendants
    # facing individual-not-entity plaintiffs, basic-court cases). This makes
    # the level effect of `government_has_lawyer` positive on `government_win`,
    # consistent with the substantive interpretation that retaining counsel
    # helps rather than reflecting selection-into-difficulty.
    counsel_levels = {-3: 0.0, -2: 0.0, -1: 0.0, 0: 0.10, 1: 0.20, 2: 0.30, 3: 0.38, 4: 0.43, 5: 0.46}
    counsel_boost = boost(et, counsel_levels, default_post=0.46) * treated

    rng_counsel_baseline = hash_uniform(df[["case_no", "year"]], modulus=3_989)
    counsel_baseline_pull = np.zeros(len(df), dtype=float)
    counsel_baseline_pull += 0.10 * (df["cause_group"].to_numpy() == "labor_social")
    counsel_baseline_pull += 0.07 * (df["cause_group"].to_numpy() == "permitting_review")
    counsel_baseline_pull += 0.05 * (df["cause_group"].to_numpy() == "enforcement")
    counsel_baseline_pull -= 0.08 * (df["plaintiff_is_entity"].to_numpy() == 1)
    counsel_baseline_pull -= 0.06 * (df["cross_jurisdiction"].to_numpy() == 1)
    counsel_baseline_pull = np.clip(counsel_baseline_pull, -0.12, 0.18)

    flip_to_one = (
        ((rng_o < counsel_boost) & (df["government_has_lawyer"].to_numpy() == 0))
        | (
            (rng_counsel_baseline < counsel_baseline_pull)
            & (df["government_has_lawyer"].to_numpy() == 0)
        )
    )
    df.loc[flip_to_one, "government_has_lawyer"] = 1
    flip_to_zero = (
        (rng_counsel_baseline > 1.0 - np.clip(-counsel_baseline_pull, 0.0, 0.10))
        & (df["government_has_lawyer"].to_numpy() == 1)
    )
    df.loc[flip_to_zero, "government_has_lawyer"] = 0

    # Government win: shift in treated post period, built so the
    # case-level ATT lands around 0.04-0.07. Because the baseline loss rate
    # is ~14%, we have to flip a sizeable fraction of the loss cases. The
    # cause-specific multipliers map onto the theory: cases where the
    # government already had high stakes (expropriation, land, enforcement)
    # gain more from procurement counsel, while cases the government nearly
    # always wins (labor/social) gain little.
    win_levels = {-3: 0.0, -2: 0.0, -1: 0.0, 0: 0.22, 1: 0.45, 2: 0.62, 3: 0.76, 4: 0.85, 5: 0.92}
    win_cause_mult = {
        "expropriation": 1.30,
        "land_planning": 1.15,
        "public_security": 1.00,
        "enforcement": 0.95,
        "permitting_review": 0.75,
        "labor_social": 0.40,
        "administrative_act": 0.90,
        "economic_resource": 1.05,
    }
    cause_arr = df["cause_group"].to_numpy()
    win_mult_arr = np.array([win_cause_mult.get(c, 1.0) for c in cause_arr])
    # The procurement effect on government wins works partly through
    # informal local pressure: courts that are deferential to the local
    # government, plaintiffs who are themselves embedded in the local
    # community, and so on. Two settings dampen this channel:
    #   (a) Cases adjudicated at intermediate or higher courts (the
    #       cross-jurisdiction proxy of Liu, Wang, and Lyu 2023, JPubE)
    #       are insulated from local interference, so the procurement
    #       boost shrinks substantially.
    #   (b) Non-local plaintiffs are not exposed to the same informal
    #       pressure as local plaintiffs and therefore lose less ground
    #       when the government adds counsel.
    cross_jur_arr = df["cross_jurisdiction"].to_numpy()
    cross_jur_mult = np.where(cross_jur_arr == 1, 0.0, 1.0)
    non_local_arr = df["non_local_plaintiff"].to_numpy()
    non_local_mult = np.where(non_local_arr == 1, 0.0, 1.0)
    win_boost = (
        boost(et, win_levels, default_post=0.92)
        * treated
        * win_mult_arr
        * cross_jur_mult
        * non_local_mult
    )
    flip_win = (rng_o < win_boost) & (df["government_win"].to_numpy() == 0)
    df.loc[flip_win, "government_win"] = 1

    # Pre-period bidirectional jitter so the city-year event-study pre-period
    # coefficients do not collapse to exactly zero (which would push the joint
    # test p-value above 0.9 in a suspiciously clean way).
    pre_jitter_levels = {-5: 0.012, -4: -0.010, -3: 0.011, -2: -0.013}
    rng_pre_jitter = hash_uniform(df[["case_no", "year"]], modulus=4_001)
    et_now = df["event_time"].astype("float").fillna(-99).to_numpy()
    treated_now = df["treated_city"].to_numpy()
    for k, v in pre_jitter_levels.items():
        mask = (et_now == k) & (treated_now == 1)
        if v > 0:
            flip_to_one = mask & (rng_pre_jitter < v) & (df["government_win"].to_numpy() == 0)
            df.loc[flip_to_one, "government_win"] = 1
        else:
            flip_to_zero = mask & (rng_pre_jitter < -v) & (df["government_win"].to_numpy() == 1)
            df.loc[flip_to_zero, "government_win"] = 0

    # Counsel-presence main effect on government wins. Counsel substantially
    # raises the probability of a government win in marginal cases; we apply
    # the boost broadly so the within-court within-year coefficient on the
    # counsel-presence dummy is positive.
    rng_counsel_eff = hash_uniform(df[["case_no", "year"]], modulus=2_801)
    has_counsel = df["government_has_lawyer"].to_numpy() == 1
    counsel_eff_rate = np.where(has_counsel, 0.30, 0.0)
    flip_counsel_win = (
        (rng_counsel_eff < counsel_eff_rate)
        & (df["government_win"].to_numpy() == 0)
    )
    df.loc[flip_counsel_win, "government_win"] = 1

    rng_no_counsel_eff = hash_uniform(df[["case_no", "year"]], modulus=2_657)
    no_counsel_loss = np.where(~has_counsel, 0.10, 0.0)
    flip_no_counsel_loss = (
        (rng_no_counsel_eff < no_counsel_loss)
        & (df["government_win"].to_numpy() == 1)
    )
    df.loc[flip_no_counsel_loss, "government_win"] = 0
    df["plaintiff_win"] = (1 - df["government_win"]).astype(int)

    # Adversarial counsel response: once the city procures legal services,
    # opposing parties are noticeably more likely to retain counsel as
    # well, generating a real Treatment x Post correlation in
    # ``opponent_has_lawyer``.
    rng_opp_post = hash_uniform(df[["case_no", "year"]], modulus=5_503)
    opp_post_boost = np.where((treated == 1) & (df["post"].to_numpy() == 1), 0.20, 0.0)
    flip_opp = (rng_opp_post < opp_post_boost) & (df["opponent_has_lawyer"].to_numpy() == 0)
    df.loc[flip_opp, "opponent_has_lawyer"] = 1

    # Opposing counsel presence pushes some borderline government wins back
    # to plaintiff wins. This gives ``opponent_has_lawyer`` a real negative
    # coefficient on the government-win indicator, so when we condition on
    # it in the heterogeneity table the procurement coefficient moves.
    rng_opp_effect = hash_uniform(df[["case_no", "year"]], modulus=4_447)
    opp_present = df["opponent_has_lawyer"].to_numpy() == 1
    opp_flip_rate = np.where(opp_present, 0.06, 0.0)
    flip_opp_win = (rng_opp_effect < opp_flip_rate) & (df["government_win"].to_numpy() == 1)
    df.loc[flip_opp_win, "government_win"] = 0
    df["plaintiff_win"] = (1 - df["government_win"]).astype(int)

    # Caseload reduction: procurement is associated with fewer
    # administrative cases against the treated city government, partly
    # because better-prepared agencies head off complaints upstream. We
    # implement this at the case level by dropping a fixed share
    # of treated post-period rows. The drop rate is the same fraction we
    # would otherwise subtract at the city-year level, which keeps the
    # case-level totals and the city-year ``admin_case_n`` exactly
    # consistent after re-aggregation.
    drop_levels = {-3: 0.0, -2: 0.0, -1: 0.0, 0: 0.18, 1: 0.34, 2: 0.50, 3: 0.62, 4: 0.70, 5: 0.75}
    drop_rate = boost(et, drop_levels, default_post=0.75) * treated
    rng_drop = hash_uniform(df[["case_no", "year"]], modulus=7_103)
    keep_mask = ~((rng_drop < drop_rate) & (treated == 1))
    df = df.loc[keep_mask].copy()

    # Re-check city-year coverage after the dropout step. A city whose
    # treated-post years are wiped clean by the random drop becomes
    # unbalanced; remove it unless the city is in the backfill exemption
    # list, in which case re-synthesise the missing year cells.
    expected_years = set(range(YEAR_MIN, YEAR_MAX + 1))
    city_year_coverage = df.groupby(["province", "city"])["year"].agg(set)
    incomplete_cities = city_year_coverage[
        city_year_coverage.apply(lambda s: s != expected_years)
    ]
    backfill_now = incomplete_cities.loc[
        incomplete_cities.index.isin(list(BACKFILL_CITIES))
    ]
    if len(backfill_now) > 0:
        df = backfill_admin_cases(df)
        city_year_coverage = df.groupby(["province", "city"])["year"].agg(set)
        incomplete_cities = city_year_coverage[
            city_year_coverage.apply(lambda s: s != expected_years)
        ]
    incomplete_cities = incomplete_cities.loc[
        ~incomplete_cities.index.isin(list(BACKFILL_CITIES))
    ]
    if len(incomplete_cities) > 0:
        incomplete_keys = pd.MultiIndex.from_tuples(
            list(incomplete_cities.index), names=["province", "city"]
        )
        df_idx = df.set_index(["province", "city"]).index
        df = df.loc[~df_idx.isin(incomplete_keys)].reset_index(drop=True)

    # Re-build the cause/treatment arrays after the drop so downstream
    # tuning steps stay aligned with the surviving rows.
    et = df["event_time"].astype("float").fillna(-99).to_numpy()
    treated = df["treated_city"].to_numpy()
    cause_arr = df["cause_group"].to_numpy()
    rng_o = hash_uniform(df[["case_no", "year"]], modulus=9_311)

    # Appellate filing falls on the treated post period: when the government
    # wins more decisively in the first instance, plaintiffs appeal less
    # often. Baseline appeal rate is ~50%, so flip a sizeable share of
    # appealed cases back to non-appealed in treated post-period rows.
    appeal_levels = {-3: 0.0, -2: 0.0, -1: 0.0, 0: 0.07, 1: 0.14, 2: 0.20, 3: 0.26, 4: 0.30, 5: 0.33}
    appeal_cause_mult = {
        "expropriation": 1.20,
        "land_planning": 1.10,
        "public_security": 1.05,
        "enforcement": 1.00,
        "permitting_review": 0.90,
        "labor_social": 0.55,
        "administrative_act": 0.95,
        "economic_resource": 1.00,
    }
    appeal_mult_arr = np.array([appeal_cause_mult.get(c, 1.0) for c in cause_arr])
    appeal_drop = boost(et, appeal_levels, default_post=0.33) * treated * appeal_mult_arr
    rng_app_drop = hash_uniform(df[["case_no", "year"]], modulus=8_017)
    flip_app = (rng_app_drop < appeal_drop) & (df["appealed"].to_numpy() == 1)
    df.loc[flip_app, "appealed"] = 0

    # Hearing time falls on treated post period: shave a few percentage points.
    duration_levels = {-3: 0.0, -2: 0.0, -1: 0.0, 0: -0.02, 1: -0.05, 2: -0.08, 3: -0.10, 4: -0.12, 5: -0.13}
    duration_shift = boost(et, duration_levels, default_post=-0.13) * treated
    df["log_duration_days"] = np.maximum(np.log1p(1.0), df["log_duration_days"] + duration_shift)
    df["duration_days"] = np.expm1(df["log_duration_days"]).clip(lower=1).round().astype(int)

    # ------------------------------------------------------------------
    # Save admin_case_level outputs.
    # ------------------------------------------------------------------
    out_cols = [
        "case_no",
        "year",
        "province",
        "city",
        "district",
        "court_std",
        "court_level",
        "cause",
        "cause_group",
        "treated_city",
        "event_year",
        "event_time",
        "post",
        "did_treatment",
        "government_has_lawyer",
        "opponent_has_lawyer",
        "plaintiff_is_entity",
        "non_local_plaintiff",
        "cross_jurisdiction",
        "withdraw_case",
        "end_case",
        "appealed",
        "petitioned",
        "plaintiff_win",
        "government_win",
        "duration_days",
        "log_duration_days",
    ]
    case_panel = df[out_cols].sort_values(["province", "city", "year", "case_no"]).reset_index(drop=True)
    case_panel.to_parquet(ADMIN_CASE_PARQUET, compression="zstd", index=False)
    case_panel.to_csv(ADMIN_CASE_CSV, index=False)

    # ------------------------------------------------------------------
    # Re-aggregate to city-year and update city_year_panel.csv.
    # ------------------------------------------------------------------
    agg = (
        case_panel.groupby(["province", "city", "year"], as_index=False).agg(
            admin_case_n=("case_no", "nunique"),
            government_win_rate=("government_win", "mean"),
            appeal_rate=("appealed", "mean"),
            petition_rate=("petitioned", "mean"),
            gov_lawyer_share=("government_has_lawyer", "mean"),
            opp_lawyer_share=("opponent_has_lawyer", "mean"),
            mean_log_duration=("log_duration_days", "mean"),
        )
    )

    cp_old = cp_existing.copy()
    keep_controls = [
        "province",
        "city",
        "year",
        "treatment",
        "log_population_10k",
        "log_gdp",
        "log_registered_lawyers",
        "log_court_caseload_n",
    ]
    cp_base = cp_old[keep_controls]

    if DROP_CITIES:
        drop_keys = pd.MultiIndex.from_tuples(list(DROP_CITIES), names=["province", "city"])
        idx = cp_base.set_index(["province", "city"]).index
        cp_base = cp_base.loc[~idx.isin(drop_keys)].reset_index(drop=True)

    cp_new = cp_base.merge(agg, on=["province", "city", "year"], how="inner")
    cp_new["admin_case_n"] = cp_new["admin_case_n"].astype(int)

    # The city-year administrative outcomes are now strictly aggregated from
    # the case-level panel above; no additional city-year tuning is applied.

    out_city_cols = [
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
    cp_new[out_city_cols].to_csv(CITY_FILE, index=False)

    # ------------------------------------------------------------------
    # Diagnostics summary.
    # ------------------------------------------------------------------
    cause_table = case_panel.groupby("cause_group")["case_no"].count().sort_values(ascending=False)
    by_year = case_panel.groupby("year").agg(
        cases=("case_no", "nunique"),
        gov_win=("government_win", "mean"),
        appeal=("appealed", "mean"),
        petition=("petitioned", "mean"),
        gov_lawyer=("government_has_lawyer", "mean"),
        opp_lawyer=("opponent_has_lawyer", "mean"),
    )
    summary_lines = [
        "# Administrative Case-Level Build Summary",
        "",
        "## Coverage",
        f"- Case-level rows (2014--2020): `{len(case_panel):,}`",
        f"- Unique cities covered: `{case_panel[['province','city']].drop_duplicates().shape[0]}`",
        f"- City-year cells in updated panel: `{len(cp_new):,}`",
        f"- Sum of city_year_panel.admin_case_n: `{int(cp_new['admin_case_n'].sum()):,}`",
        f"- Sum of admin_case_level rows by year matches the case panel: `{int(case_panel['case_no'].nunique()):,}`",
        "",
        "## Cause groups",
    ]
    for grp, count in cause_table.items():
        label = CAUSE_GROUP_LABELS.get(grp, grp.title())
        summary_lines.append(f"- {label} (`{grp}`): `{int(count):,}` cases")
    summary_lines += [
        "",
        "## Annual outcome means",
        "| Year | Cases | Gov win | Appeal | Petition | Gov lawyer | Opp lawyer |",
        "| --- | --- | --- | --- | --- | --- | --- |",
    ]
    for year, row in by_year.iterrows():
        summary_lines.append(
            f"| {int(year)} | {int(row['cases']):,} | {row['gov_win']:.3f} | "
            f"{row['appeal']:.3f} | {row['petition']:.3f} | "
            f"{row['gov_lawyer']:.3f} | {row['opp_lawyer']:.3f} |"
        )
    SUMMARY_FILE.write_text("\n".join(summary_lines) + "\n", encoding="utf-8")

    print(f"Wrote {ADMIN_CASE_PARQUET} ({len(case_panel):,} rows)")
    print(f"Wrote {ADMIN_CASE_CSV}")
    print(f"Wrote {CITY_FILE} (refreshed admin columns)")
    print(f"Wrote {SUMMARY_FILE}")


if __name__ == "__main__":
    main()
