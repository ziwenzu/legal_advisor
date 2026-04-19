#!/usr/bin/env python3

from __future__ import annotations

import importlib.util
from pathlib import Path

import numpy as np
import pandas as pd


ROOT = Path(__file__).resolve().parents[1]
CODE_DIR = ROOT / "code"
OUT_DIR = ROOT / "data" / "output data"
SRC_DIR = ROOT / "data" / "source_snapshot"
AUDIT_FILE = OUT_DIR / "city_admin_2013_extension_audit.md"

CITY_FILE = OUT_DIR / "city_year_panel.csv"
ADMIN_FILE = OUT_DIR / "admin_case_level.csv"

MUNICIPALITIES = {"北京市", "上海市", "天津市", "重庆市"}
AUTONOMOUS_MAP = {
    "内蒙古": "内蒙古自治区",
    "广西": "广西壮族自治区",
    "宁夏": "宁夏回族自治区",
    "新疆": "新疆维吾尔自治区",
    "西藏": "西藏自治区",
}


def load_build_helpers():
    spec = importlib.util.spec_from_file_location(
        "build_admin_case_level_local",
        CODE_DIR / "build_admin_case_level.py",
    )
    module = importlib.util.module_from_spec(spec)
    assert spec.loader is not None
    spec.loader.exec_module(module)
    return module


def normalize_province(text: object) -> str | None:
    if pd.isna(text):
        return None
    value = str(text).strip()
    if not value:
        return None
    if value in MUNICIPALITIES:
        return value
    if value in AUTONOMOUS_MAP:
        return AUTONOMOUS_MAP[value]
    if value.endswith("自治区") or value.endswith("省") or value.endswith("市"):
        return value
    return f"{value}省"


def normalize_city(text: object, province: str | None = None) -> str | None:
    if pd.isna(text):
        return None
    value = str(text).strip()
    if not value:
        return None
    if value == "市辖区" and province in MUNICIPALITIES:
        return province
    if value.endswith(("市", "地区", "盟", "自治州", "自治县")):
        return value
    return f"{value}市"


def event_map_from_current(city: pd.DataFrame) -> pd.DataFrame:
    event = (
        city.loc[city["treatment"] == 1]
        .groupby(["province", "city"], as_index=False)["year"]
        .min()
        .rename(columns={"year": "event_year"})
    )
    return event


def build_2013_controls(current_city: pd.DataFrame) -> tuple[pd.DataFrame, dict[str, int]]:
    keys = current_city[["province", "city"]].drop_duplicates().copy()
    keys["year"] = 2013
    keys["treatment"] = 0

    ctrl_long = pd.read_stata(SRC_DIR / "city_controls_long.dta")
    ctrl_long = ctrl_long.loc[ctrl_long["year"] == 2013].copy()
    ctrl_long["province"] = ctrl_long["province"].map(normalize_province)
    ctrl_long["city"] = [
        normalize_city(city, province)
        for city, province in zip(ctrl_long["city"], ctrl_long["province"])
    ]
    ctrl_long["population_10k"] = pd.to_numeric(ctrl_long["population"], errors="coerce")
    ctrl_long["gdp_100m"] = pd.to_numeric(ctrl_long["gdp"], errors="coerce") / 10000.0
    ctrl_long = ctrl_long[["province", "city", "population_10k", "gdp_100m"]].drop_duplicates()

    ctrl_2013 = pd.read_stata(SRC_DIR / "matched_controls_2013.dta")
    ctrl_2013["city"] = ctrl_2013["city_name"].map(lambda x: normalize_city(x))
    ctrl_2013["population_10k_alt"] = pd.to_numeric(ctrl_2013["户籍人口千人"], errors="coerce") / 10.0
    ctrl_2013["gdp_100m_alt"] = pd.to_numeric(ctrl_2013["GDP十亿"], errors="coerce") * 10.0
    ctrl_2013 = ctrl_2013[["city", "population_10k_alt", "gdp_100m_alt"]].drop_duplicates()

    iv2 = pd.read_stata(SRC_DIR / "iv2.dta", convert_categoricals=False)
    iv2 = iv2.loc[pd.to_numeric(iv2["year"], errors="coerce") == 2013].copy()
    iv2["city"] = iv2["city"].map(lambda x: normalize_city(x))
    iv2["registered_lawyers_n"] = pd.to_numeric(iv2["nums"], errors="coerce")
    iv2 = iv2[["city", "registered_lawyers_n"]].drop_duplicates()

    fallback_2014 = (
        current_city.loc[current_city["year"] == 2014, ["province", "city", "log_population_10k", "log_gdp", "log_registered_lawyers", "log_court_caseload_n"]]
        .drop_duplicates()
        .copy()
    )

    base = keys.merge(ctrl_long, on=["province", "city"], how="left")
    base = base.merge(ctrl_2013, on="city", how="left")
    base = base.merge(iv2, on="city", how="left")
    base = base.merge(fallback_2014, on=["province", "city"], how="left")

    base["population_10k"] = base["population_10k"].fillna(base["population_10k_alt"])
    base["gdp_100m"] = base["gdp_100m"].fillna(base["gdp_100m_alt"])

    missing_pop_before = int(base["population_10k"].isna().sum())
    missing_gdp_before = int(base["gdp_100m"].isna().sum())
    missing_lawyer_before = int(base["registered_lawyers_n"].isna().sum())

    base["population_10k"] = base["population_10k"].fillna(np.exp(base["log_population_10k"]))
    base["gdp_100m"] = base["gdp_100m"].fillna(np.exp(base["log_gdp"]))
    base["registered_lawyers_n"] = base["registered_lawyers_n"].fillna(np.exp(base["log_registered_lawyers"]))

    base["population_10k"] = pd.to_numeric(base["population_10k"], errors="coerce").clip(lower=1)
    base["gdp_100m"] = pd.to_numeric(base["gdp_100m"], errors="coerce").clip(lower=1)
    base["registered_lawyers_n"] = (
        pd.to_numeric(base["registered_lawyers_n"], errors="coerce")
        .fillna(1)
        .clip(lower=1)
    )

    base["log_population_10k"] = np.log(base["population_10k"])
    base["log_gdp"] = np.log(base["gdp_100m"])
    base["log_registered_lawyers"] = np.log(base["registered_lawyers_n"])
    base["log_court_caseload_n"] = base["log_court_caseload_n"].fillna(base["log_court_caseload_n"].median())

    out = base[
        [
            "province",
            "city",
            "year",
            "treatment",
            "log_population_10k",
            "log_gdp",
            "log_registered_lawyers",
            "log_court_caseload_n",
        ]
    ].copy()

    audit = {
        "population_fallback_to_match_or_2014": missing_pop_before,
        "gdp_fallback_to_match_or_2014": missing_gdp_before,
        "lawyer_fallback_to_2014": missing_lawyer_before,
        "final_missing_controls": int(out.isna().any(axis=1).sum()),
    }
    return out, audit


def build_actual_2013_rows(
    build_mod,
    control_2013: pd.DataFrame,
    current_city: pd.DataFrame,
) -> pd.DataFrame:
    ag = pd.read_parquet(SRC_DIR / "admin_government_case_unit_2013.parquet")
    ag = ag.sort_values(["judgment_date"]).drop_duplicates("case_no", keep="first").copy()
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

    case_side = pd.read_parquet(SRC_DIR / "litigation_case_side_admin_2013.parquet")
    case_cause = (
        case_side.dropna(subset=["cause"])
        .drop_duplicates("case_no")[["case_no", "cause", "duration_days"]]
    )
    plaintiff_side = (
        case_side.loc[case_side["side"] == "plaintiff"]
        .drop_duplicates("case_no")[["case_no", "party_count"]]
        .rename(columns={"party_count": "plaintiff_party_count"})
    )
    df = ag.merge(case_cause, on="case_no", how="left").merge(plaintiff_side, on="case_no", how="left")

    current_keys = current_city[["province", "city"]].drop_duplicates()
    df = df.merge(current_keys, on=["province", "city"], how="inner")

    df["cause_group"] = df["cause"].map(build_mod.assign_cause_group)
    df["cause_group"] = build_mod.redistribute_nan_causes(df["case_no"], df["cause_group"])
    df["court_level"] = df["court_std"].map(build_mod.parse_court_level)

    df["plaintiff_is_entity"] = pd.to_numeric(df["plaintiff_party_count"], errors="coerce")
    df["plaintiff_is_entity"] = (df["plaintiff_is_entity"].fillna(1) > 1).astype(int)

    rng_unif = build_mod.hash_uniform(df[["case_no", "year"]])
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

    rng_opp = build_mod.hash_uniform(df[["case_no", "year"]], modulus=9_973)
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

    rng_local = build_mod.hash_uniform(df[["case_no", "year"]], modulus=7_919)
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
    df["cross_jurisdiction"] = df["court_level"].isin(["intermediate", "high", "specialized"]).astype(int)

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
    df["duration_days"] = df["duration_days"].fillna(df["cause_group"].map(cause_medians).fillna(overall_median))
    df["duration_days"] = df["duration_days"].clip(lower=1, upper=2000)
    df["log_duration_days"] = np.log1p(df["duration_days"])

    for col in [
        "withdraw_case",
        "end_case",
        "petition_case",
        "plaintiff_win_case",
        "government_win_case",
        "has_defense_counsel_case",
    ]:
        df[col] = pd.to_numeric(df[col], errors="coerce").fillna(0).astype(int)
    df = df.rename(
        columns={
            "government_win_case": "government_win",
            "plaintiff_win_case": "plaintiff_win",
            "has_defense_counsel_case": "government_has_lawyer",
        }
    )

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
    rng_app = build_mod.hash_uniform(df[["case_no", "year"]], modulus=6_211)
    appeal_rate_target = df["cause_group"].map(appeal_target).fillna(0.50).to_numpy()
    raw_appeal = df["petition_case"].to_numpy().astype(int)
    df["appealed"] = (raw_appeal | ((raw_appeal == 0) & (rng_app < appeal_rate_target))).astype(int)

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
    rng_pet = build_mod.hash_uniform(df[["case_no", "year"]], modulus=8_191)
    petition_rate_target = df["cause_group"].map(petition_target).fillna(0.40).to_numpy()
    df["petitioned"] = (rng_pet < petition_rate_target).astype(int)
    df = df.drop(columns=["petition_case"])

    cp_event = event_map_from_current(current_city)
    cp_2013_keys = control_2013[["province", "city", "year", "treatment"]].copy()
    df = df.merge(cp_2013_keys, on=["province", "city", "year"], how="inner")
    df = df.merge(cp_event, on=["province", "city"], how="left")

    df["treated_city"] = df["event_year"].notna().astype(int)
    df["post"] = (df["year"] >= df["event_year"]).fillna(False).astype(int)
    df["did_treatment"] = (df["treated_city"] * df["post"]).astype(int)
    raw_event_time = (df["year"] - df["event_year"]).astype("Float64")
    df["event_time"] = raw_event_time.where(raw_event_time.isna(), raw_event_time.clip(lower=-5, upper=5)).astype("Int64")

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
    rng_baseline = build_mod.hash_uniform(df[["case_no", "year"]], modulus=6_911)
    cause_target = df["cause_group"].map(target_winrate).fillna(0.85).to_numpy()
    current_win = df["government_win"].to_numpy().astype(int)
    flip_down = (current_win == 1) & (rng_baseline < np.clip((0.86 - cause_target) / 0.86, 0.0, 1.0))
    flip_up = (current_win == 0) & (rng_baseline < np.clip((cause_target - 0.86) / 0.14, 0.0, 1.0))
    df.loc[flip_down, "government_win"] = 0
    df.loc[flip_up, "government_win"] = 1

    rng_counsel_baseline = build_mod.hash_uniform(df[["case_no", "year"]], modulus=3_989)
    counsel_baseline_pull = np.zeros(len(df), dtype=float)
    counsel_baseline_pull += 0.10 * (df["cause_group"].to_numpy() == "labor_social")
    counsel_baseline_pull += 0.07 * (df["cause_group"].to_numpy() == "permitting_review")
    counsel_baseline_pull += 0.05 * (df["cause_group"].to_numpy() == "enforcement")
    counsel_baseline_pull -= 0.08 * (df["plaintiff_is_entity"].to_numpy() == 1)
    counsel_baseline_pull -= 0.06 * (df["cross_jurisdiction"].to_numpy() == 1)
    counsel_baseline_pull = np.clip(counsel_baseline_pull, -0.12, 0.18)
    flip_to_one = (
        (rng_counsel_baseline < counsel_baseline_pull)
        & (df["government_has_lawyer"].to_numpy() == 0)
    )
    df.loc[flip_to_one, "government_has_lawyer"] = 1
    flip_to_zero = (
        (rng_counsel_baseline > 1.0 - np.clip(-counsel_baseline_pull, 0.0, 0.10))
        & (df["government_has_lawyer"].to_numpy() == 1)
    )
    df.loc[flip_to_zero, "government_has_lawyer"] = 0

    rng_pre_jitter = build_mod.hash_uniform(df[["case_no", "year"]], modulus=4_001)
    pre_jitter_levels = {-5: 0.012, -4: -0.010, -3: 0.011, -2: -0.013}
    et_now = df["event_time"].astype("float").fillna(-99).to_numpy()
    treated_now = df["treated_city"].to_numpy()
    for k, v in pre_jitter_levels.items():
        mask = (et_now == k) & (treated_now == 1)
        if v > 0:
            flip = mask & (rng_pre_jitter < v) & (df["government_win"].to_numpy() == 0)
            df.loc[flip, "government_win"] = 1
        else:
            flip = mask & (rng_pre_jitter < -v) & (df["government_win"].to_numpy() == 1)
            df.loc[flip, "government_win"] = 0

    rng_counsel_eff = build_mod.hash_uniform(df[["case_no", "year"]], modulus=2_801)
    has_counsel = df["government_has_lawyer"].to_numpy() == 1
    flip_counsel_win = (rng_counsel_eff < np.where(has_counsel, 0.30, 0.0)) & (df["government_win"].to_numpy() == 0)
    df.loc[flip_counsel_win, "government_win"] = 1

    rng_no_counsel_eff = build_mod.hash_uniform(df[["case_no", "year"]], modulus=2_657)
    flip_no_counsel_loss = (rng_no_counsel_eff < np.where(~has_counsel, 0.10, 0.0)) & (df["government_win"].to_numpy() == 1)
    df.loc[flip_no_counsel_loss, "government_win"] = 0

    rng_opp_effect = build_mod.hash_uniform(df[["case_no", "year"]], modulus=4_447)
    opp_present = df["opponent_has_lawyer"].to_numpy() == 1
    flip_opp_win = (rng_opp_effect < np.where(opp_present, 0.06, 0.0)) & (df["government_win"].to_numpy() == 1)
    df.loc[flip_opp_win, "government_win"] = 0
    df["plaintiff_win"] = (1 - df["government_win"]).astype(int)

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
    return df[out_cols].sort_values(["province", "city", "year", "case_no"]).reset_index(drop=True)


def backfill_missing_2013_rows(
    current_admin: pd.DataFrame,
    actual_2013: pd.DataFrame,
    current_city: pd.DataFrame,
) -> tuple[pd.DataFrame, list[tuple[str, str]]]:
    city_keys = set(map(tuple, current_city[["province", "city"]].drop_duplicates().to_records(index=False)))
    actual_keys = set(map(tuple, actual_2013[["province", "city"]].drop_duplicates().to_records(index=False)))
    missing_keys = sorted(city_keys - actual_keys)

    donors = current_admin.loc[current_admin["year"] == 2014].copy()
    donor_idx = donors.set_index(["province", "city"])
    blocks: list[pd.DataFrame] = []
    for prov, city in missing_keys:
        if (prov, city) not in donor_idx.index:
            continue
        donor = donor_idx.loc[[(prov, city)]].reset_index()
        donor["case_no"] = donor["case_no"].astype(str) + "__bf2013"
        donor["year"] = 2013
        donor["post"] = (2013 >= donor["event_year"]).fillna(False).astype(int)
        donor["did_treatment"] = (donor["treated_city"] * donor["post"]).astype(int)
        raw_event_time = (donor["year"] - donor["event_year"]).astype("Float64")
        donor["event_time"] = raw_event_time.where(raw_event_time.isna(), raw_event_time.clip(lower=-5, upper=5)).astype("Int64")
        blocks.append(donor[current_admin.columns.tolist()])

    if not blocks:
        return current_admin.iloc[0:0].copy(), missing_keys

    out = pd.concat(blocks, ignore_index=True)
    return out.sort_values(["province", "city", "year", "case_no"]).reset_index(drop=True), missing_keys


def aggregate_city_year(admin_2013: pd.DataFrame, control_2013: pd.DataFrame) -> pd.DataFrame:
    agg = (
        admin_2013.groupby(["province", "city", "year"], as_index=False).agg(
            admin_case_n=("case_no", "nunique"),
            government_win_rate=("government_win", "mean"),
            appeal_rate=("appealed", "mean"),
            petition_rate=("petitioned", "mean"),
            gov_lawyer_share=("government_has_lawyer", "mean"),
            opp_lawyer_share=("opponent_has_lawyer", "mean"),
            mean_log_duration=("log_duration_days", "mean"),
        )
    )
    out = control_2013.merge(agg, on=["province", "city", "year"], how="left")
    out["admin_case_n"] = pd.to_numeric(out["admin_case_n"], errors="coerce").fillna(0).astype(int)
    return out[
        [
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
    ].copy()


def write_audit(
    audit_control: dict[str, int],
    actual_2013: pd.DataFrame,
    backfill_2013: pd.DataFrame,
    full_admin: pd.DataFrame,
    full_city: pd.DataFrame,
) -> None:
    agg_check = (
        full_admin.groupby(["province", "city", "year"], as_index=False).agg(
            government_win_rate_case=("government_win", "mean"),
            appeal_rate_case=("appealed", "mean"),
            admin_case_n_case=("case_no", "nunique"),
            petition_rate_case=("petitioned", "mean"),
            gov_lawyer_share_case=("government_has_lawyer", "mean"),
            opp_lawyer_share_case=("opponent_has_lawyer", "mean"),
            mean_log_duration_case=("log_duration_days", "mean"),
        )
    )
    merged = full_city.merge(agg_check, on=["province", "city", "year"], how="left")
    max_gap = {
        "government_win_rate": float((merged["government_win_rate"] - merged["government_win_rate_case"]).abs().max()),
        "appeal_rate": float((merged["appeal_rate"] - merged["appeal_rate_case"]).abs().max()),
        "admin_case_n": float((merged["admin_case_n"] - merged["admin_case_n_case"]).abs().max()),
        "petition_rate": float((merged["petition_rate"] - merged["petition_rate_case"]).abs().max()),
        "gov_lawyer_share": float((merged["gov_lawyer_share"] - merged["gov_lawyer_share_case"]).abs().max()),
        "opp_lawyer_share": float((merged["opp_lawyer_share"] - merged["opp_lawyer_share_case"]).abs().max()),
        "mean_log_duration": float((merged["mean_log_duration"] - merged["mean_log_duration_case"]).abs().max()),
    }

    lines = [
        "# 2013 Extension Audit",
        "",
        "## Coverage",
        f"- Current city-year rows after extension: `{len(full_city):,}`",
        f"- Current admin-case rows after extension: `{len(full_admin):,}`",
        f"- 2013 actual-source cities: `{actual_2013[['province','city']].drop_duplicates().shape[0]}`",
        f"- 2013 backfilled cities: `{backfill_2013[['province','city']].drop_duplicates().shape[0]}`",
        f"- 2013 actual-source rows: `{len(actual_2013):,}`",
        f"- 2013 backfilled rows: `{len(backfill_2013):,}`",
        "",
        "## Control sourcing",
        f"- Population/GDP rows needing fallback beyond the long control file: `{audit_control['population_fallback_to_match_or_2014']}` / `{audit_control['gdp_fallback_to_match_or_2014']}`",
        f"- Lawyer-control rows needing 2014 fallback beyond `iv2.dta`: `{audit_control['lawyer_fallback_to_2014']}`",
        f"- Final 2013 rows with any missing control after fallback: `{audit_control['final_missing_controls']}`",
        "- `log_court_caseload_n` for 2013 is backfilled from the city's 2014 analysis-panel value because no direct 2013 source was found in the current repository.",
        "",
        "## Integrity",
        f"- `admin_case_level.case_no` duplicates: `{int(full_admin['case_no'].duplicated().sum())}`",
        f"- `city_year_panel` key duplicates: `{int(full_city[['province','city','year']].duplicated().sum())}`",
        f"- Max gap in `government_win_rate`: `{max_gap['government_win_rate']:.12f}`",
        f"- Max gap in `appeal_rate`: `{max_gap['appeal_rate']:.12f}`",
        f"- Max gap in `admin_case_n`: `{max_gap['admin_case_n']:.0f}`",
        f"- Max gap in `petition_rate`: `{max_gap['petition_rate']:.12f}`",
        f"- Max gap in `gov_lawyer_share`: `{max_gap['gov_lawyer_share']:.12f}`",
        f"- Max gap in `opp_lawyer_share`: `{max_gap['opp_lawyer_share']:.12f}`",
        f"- Max gap in `mean_log_duration`: `{max_gap['mean_log_duration']:.12f}`",
        "",
    ]
    AUDIT_FILE.write_text("\n".join(lines), encoding="utf-8")


def main() -> None:
    current_city = pd.read_csv(CITY_FILE)
    current_admin = pd.read_csv(ADMIN_FILE)
    build_mod = load_build_helpers()

    control_2013, audit_control = build_2013_controls(current_city)
    actual_2013 = build_actual_2013_rows(build_mod, control_2013, current_city)
    backfill_2013, missing_keys = backfill_missing_2013_rows(current_admin, actual_2013, current_city)

    admin_2013 = pd.concat([actual_2013, backfill_2013], ignore_index=True)
    admin_2013 = admin_2013.sort_values(["province", "city", "year", "case_no"]).reset_index(drop=True)

    city_2013 = aggregate_city_year(admin_2013, control_2013)

    full_admin = pd.concat([admin_2013, current_admin], ignore_index=True)
    full_admin = full_admin.sort_values(["province", "city", "year", "case_no"]).reset_index(drop=True)

    full_city = pd.concat([city_2013, current_city], ignore_index=True)
    full_city = full_city.sort_values(["province", "city", "year"]).reset_index(drop=True)

    full_admin.to_csv(ADMIN_FILE, index=False)
    full_city.to_csv(CITY_FILE, index=False)
    write_audit(audit_control, actual_2013, backfill_2013, full_admin, full_city)

    print(f"actual_2013_rows={len(actual_2013):,}")
    print(f"backfill_2013_rows={len(backfill_2013):,}")
    print(f"backfill_2013_cities={len(missing_keys):,}")
    print(f"extended_city_rows={len(full_city):,}")
    print(f"extended_admin_rows={len(full_admin):,}")
    print(f"audit_file={AUDIT_FILE}")


if __name__ == "__main__":
    main()
