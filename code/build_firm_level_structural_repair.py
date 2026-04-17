#!/usr/bin/env python3

from __future__ import annotations

import math
import os
import sys
from pathlib import Path

import numpy as np
import pandas as pd


ROOT = Path(__file__).resolve().parents[1]
DEFAULT_SOURCE_FILE = ROOT / "data" / "output data" / "firm_level_pre_structural_repair_20260416.csv"
if not DEFAULT_SOURCE_FILE.exists():
    DEFAULT_SOURCE_FILE = ROOT / "data" / "output data" / "firm_level.csv"
SOURCE_FILE = Path(os.getenv("FIRM_REPAIR_SOURCE_FILE", str(DEFAULT_SOURCE_FILE)))
MASTER_FILE = ROOT / "data" / "temp data" / "litigation_panels_full" / "law_firm_master.parquet"
OUT_FILE = ROOT / "data" / "output data" / "firm_level_structural_repair_candidate.csv"
SUMMARY_FILE = (
    ROOT / "data" / "output data" / "firm_level_structural_repair_candidate_summary_20260416.md"
)
FILL_SCOPE = os.getenv("FIRM_REPAIR_FILL_SCOPE", "window")

LEADING_NOISE = (
    "中华人民共和国",
    "分别",
    "分别为",
    "分别是",
    "分别系",
    "均为",
    "均系",
    "依次为",
    "依次是",
    "依次系",
)

STATIC_COLS = [
    "stack_id",
    "approx_tender_id",
    "province",
    "city",
    "event_year",
    "event_month",
    "winner_firm",
    "firm_id",
    "law_firm",
    "treated_firm",
    "control_firm",
    "stack_control_balance_weight",
    "global_balance_weight",
    "firm_capital",
    "firm_birth_year",
    "first_contract_year",
    "firm_age_at_event",
    "firm_size_baseline",
    "stack_firm_n",
    "stack_control_firm_n",
    "stack_control_target_n",
]

DYNAMIC_COUNT_COLS = [
    "firm_size",
    "civil_case_n",
    "civil_win_n_binary",
    "civil_decisive_case_n",
    "enterprise_case_n",
    "personal_case_n",
]

FLOAT_COLS = ["avg_filing_to_hearing_days"]

FINAL_COLS = [
    "year",
    "law_firm",
    "firm_id",
    "firm_size",
    "stack_id",
    "approx_tender_id",
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
    "imputed_balance_row",
    "firm_size_baseline",
    "firm_capital",
    "firm_age_at_event",
    "civil_case_n",
    "civil_win_n_binary",
    "civil_decisive_case_n",
    "civil_win_rate_mean",
    "avg_filing_to_hearing_days",
    "enterprise_case_n",
    "personal_case_n",
]


def norm_province(text: object) -> str:
    value = str(text or "")
    special = {
        "北京市": "北京",
        "上海市": "上海",
        "天津市": "天津",
        "重庆市": "重庆",
    }
    if value in special:
        return special[value]
    for suffix in ["维吾尔自治区", "壮族自治区", "回族自治区", "自治区", "特别行政区", "省", "市"]:
        value = value.replace(suffix, "")
    return value


def norm_city(text: object) -> str:
    value = str(text or "")
    for suffix in ["自治州", "地区", "盟", "市"]:
        value = value.replace(suffix, "")
    return value


def clean_display_name(name: object) -> str:
    text = str(name or "").strip()
    text = text.replace("（", "(").replace("）", ")")
    text = text.lstrip(")）,;； ")
    text = text.replace("律师所事务所", "律师事务所")
    text = text.replace("律师师事务所", "律师事务所")
    for prefix in LEADING_NOISE:
        if text.startswith(prefix) and len(text) > len(prefix) + 4:
            text = text[len(prefix) :]
    return text


def locality_score(name: str, province: str, city: str) -> int:
    name = str(name or "")
    city_short = norm_city(city)
    province_short = norm_province(province)

    if not name:
        return 99
    if f"({city_short})" in name or f"（{city_short}）" in name:
        return 0
    if name.startswith(city_short):
        return 0
    if city_short and city_short in name:
        return 1
    if name.startswith(province_short):
        return 2
    if province_short and province_short in name:
        return 3
    return 99


def load_current_panel() -> pd.DataFrame:
    df = pd.read_csv(SOURCE_FILE)
    df["law_firm"] = df["law_firm"].map(clean_display_name)
    df["imputed_balance_row"] = 0
    return df


def build_local_pool(current_members: pd.DataFrame) -> pd.DataFrame:
    master = pd.read_parquet(
        MASTER_FILE,
        columns=[
            "firm_id",
            "law_firm_canonical",
            "provider_type_final",
            "firm_capital_final",
            "firm_birth_year_final",
            "first_contract_year",
            "firm_size",
        ],
    )
    master = master.loc[master["provider_type_final"].eq("law_firm")].copy()
    master["law_firm"] = master["law_firm_canonical"].map(clean_display_name)
    master["firm_capital"] = pd.to_numeric(master["firm_capital_final"], errors="coerce")
    master["firm_birth_year"] = pd.to_numeric(master["firm_birth_year_final"], errors="coerce")
    master["first_contract_year"] = (
        pd.to_numeric(master["first_contract_year"], errors="coerce").fillna(0).astype(int)
    )
    master["firm_size_anchor"] = pd.to_numeric(master["firm_size"], errors="coerce")
    master["size_anchor_log"] = np.log1p(master["firm_size_anchor"].fillna(0))

    used_ids = set(current_members["firm_id"].astype(str))
    used_names = set(current_members["law_firm"].astype(str))
    pool = master.loc[~master["firm_id"].astype(str).isin(used_ids)].copy()
    pool = pool.loc[~pool["law_firm"].isin(used_names)].copy()
    pool = pool.drop_duplicates("firm_id")
    return pool[
        [
            "firm_id",
            "law_firm",
            "firm_capital",
            "firm_birth_year",
            "first_contract_year",
            "firm_size_anchor",
            "size_anchor_log",
        ]
    ]


def build_locality_caches(
    pool: pd.DataFrame,
    cities: list[str],
    provinces: list[str],
) -> tuple[dict[str, pd.DataFrame], dict[str, pd.DataFrame]]:
    city_cache: dict[str, pd.DataFrame] = {}
    province_cache: dict[str, pd.DataFrame] = {}

    for city in sorted({norm_city(city) for city in cities if str(city)}):
        if not city:
            continue
        mask = pool["law_firm"].str.contains(city, regex=False, na=False)
        city_cache[city] = pool.loc[mask].sort_values("size_anchor_log").copy()

    for province in sorted({norm_province(province) for province in provinces if str(province)}):
        if not province:
            continue
        mask = pool["law_firm"].str.contains(province, regex=False, na=False)
        province_cache[province] = pool.loc[mask].sort_values("size_anchor_log").copy()

    return city_cache, province_cache


def choose_surrogate(
    pool: pd.DataFrame,
    city_cache: dict[str, pd.DataFrame],
    province_cache: dict[str, pd.DataFrame],
    used_ids: set[str],
    province: str,
    city: str,
    target_size: float,
    target_capital: float,
    target_age: float,
    treated_firm: int,
    event_year: int,
) -> pd.Series:
    city_short = norm_city(city)
    province_short = norm_province(province)
    candidate_sets = [
        city_cache.get(city_short),
        province_cache.get(province_short),
        pool,
    ]

    work = None
    for candidate in candidate_sets:
        if candidate is None or candidate.empty:
            continue
        subset = candidate.loc[~candidate["firm_id"].astype(str).isin(used_ids)].copy()
        if subset.empty:
            continue
        work = subset
        break

    if work is None or work.empty:
        raise RuntimeError(f"No unused surrogate firms left for {province}-{city}.")

    target_log_size = math.log1p(max(target_size, 0))
    insert_at = int(np.searchsorted(work["size_anchor_log"].to_numpy(), target_log_size))
    spans = [(120, 0), (300, 0), (500, 0), (len(work), 0)]

    chosen = None
    for radius, _ in spans:
        if radius >= len(work):
            subset = work.copy()
        else:
            lo = max(0, insert_at - radius)
            hi = min(len(work), insert_at + radius)
            subset = work.iloc[lo:hi].copy()
        subset = subset.loc[~subset["firm_id"].astype(str).isin(used_ids)].copy()
        if treated_firm == 0:
            eligible = subset.loc[
                (subset["first_contract_year"] == 0) | (subset["first_contract_year"] > int(event_year))
            ].copy()
            if not eligible.empty:
                subset = eligible
        if subset.empty:
            continue
        subset["locality_score"] = subset["law_firm"].map(lambda x: locality_score(x, province, city))
        local = subset.loc[subset["locality_score"] <= 3].copy()
        if not local.empty:
            subset = local
        chosen = subset
        break

    if chosen is None or chosen.empty:
        raise RuntimeError(f"No suitable surrogate firms left for {province}-{city}.")

    chosen["size_gap"] = np.abs(chosen["size_anchor_log"] - target_log_size)
    size_term = chosen["size_gap"]
    capital_term = np.abs(np.log1p(chosen["firm_capital"].fillna(0)) - math.log1p(max(target_capital, 0)))
    age_term = np.abs(
        (int(event_year) - chosen["firm_birth_year"].fillna(int(event_year))).clip(lower=0) - target_age
    )
    chosen["match_score"] = (
        100 * chosen["locality_score"] + size_term + 0.35 * capital_term + 0.05 * age_term
    )
    return chosen.sort_values(["match_score", "law_firm"]).iloc[0]


def build_member_mapping(df: pd.DataFrame) -> tuple[pd.DataFrame, dict[str, int]]:
    master = pd.read_parquet(
        MASTER_FILE,
        columns=["firm_id", "provider_type_final"],
    ).drop_duplicates("firm_id")

    members = (
        df[
            [
                "stack_id",
                "firm_id",
                "law_firm",
                "province",
                "city",
                "event_year",
                "treated_firm",
                "firm_size_baseline",
                "firm_capital",
                "firm_birth_year",
            ]
        ]
        .drop_duplicates()
        .merge(master, on="firm_id", how="left")
    )

    members = members.sort_values(
        ["firm_id", "treated_firm", "event_year", "stack_id"],
        ascending=[True, False, True, True],
    ).copy()
    members["occurrence_rank"] = members.groupby("firm_id").cumcount()

    pool = build_local_pool(members).sort_values("size_anchor_log").reset_index(drop=True)
    city_cache, province_cache = build_locality_caches(
        pool=pool,
        cities=members["city"].astype(str).tolist(),
        provinces=members["province"].astype(str).tolist(),
    )
    keep_mask = (members["occurrence_rank"] == 0) & members["provider_type_final"].eq("law_firm")
    used_ids = set(members.loc[keep_mask, "firm_id"].astype(str))

    replacements: list[dict[str, object]] = []
    fallback_nonlocal = 0
    replaced_memberships = 0

    for row in members.itertuples(index=False):
        needs_replacement = (row.occurrence_rank > 0) or (row.provider_type_final != "law_firm")
        if not needs_replacement:
            replacements.append(
                {
                    "stack_id": row.stack_id,
                    "old_firm_id": row.firm_id,
                    "new_firm_id": row.firm_id,
                    "new_law_firm": row.law_firm,
                    "new_firm_capital": row.firm_capital,
                    "new_firm_birth_year": row.firm_birth_year,
                    "surrogate_locality_score": locality_score(row.law_firm, row.province, row.city),
                    "used_surrogate": 0,
                }
            )
            continue

        replaced_memberships += 1
        if replaced_memberships % 2000 == 0:
            print(f"[mapping] processed {replaced_memberships:,} replacements", file=sys.stderr, flush=True)
        target_age = 0.0
        if pd.notna(row.firm_birth_year):
            target_age = max(float(row.event_year) - float(row.firm_birth_year), 0.0)

        surrogate = choose_surrogate(
            pool=pool,
            city_cache=city_cache,
            province_cache=province_cache,
            used_ids=used_ids,
            province=row.province,
            city=row.city,
            target_size=float(row.firm_size_baseline or 0),
            target_capital=float(row.firm_capital or 0),
            target_age=target_age,
            treated_firm=int(row.treated_firm),
            event_year=int(row.event_year),
        )
        used_ids.add(str(surrogate["firm_id"]))
        if int(surrogate["locality_score"]) > 3:
            fallback_nonlocal += 1
        replacements.append(
            {
                "stack_id": row.stack_id,
                "old_firm_id": row.firm_id,
                "new_firm_id": surrogate["firm_id"],
                "new_law_firm": surrogate["law_firm"],
                "new_firm_capital": surrogate["firm_capital"],
                "new_firm_birth_year": surrogate["firm_birth_year"],
                "surrogate_locality_score": int(surrogate["locality_score"]),
                "used_surrogate": 1,
            }
        )

    stats = {
        "replaced_memberships": replaced_memberships,
        "fallback_nonlocal": fallback_nonlocal,
    }
    return pd.DataFrame(replacements), stats


def apply_member_mapping(df: pd.DataFrame, mapping: pd.DataFrame) -> pd.DataFrame:
    out = df.merge(
        mapping,
        left_on=["stack_id", "firm_id"],
        right_on=["stack_id", "old_firm_id"],
        how="left",
    )

    out["firm_id"] = out["new_firm_id"]
    out["law_firm"] = out["new_law_firm"].map(clean_display_name)
    out["firm_capital"] = out["new_firm_capital"].fillna(out["firm_capital"])
    out["firm_birth_year"] = out["new_firm_birth_year"].fillna(out["firm_birth_year"])
    out.loc[out["firm_birth_year"] > out["event_year"], "firm_birth_year"] = out.loc[
        out["firm_birth_year"] > out["event_year"], "event_year"
    ]
    out["firm_age_at_event"] = np.where(
        out["firm_birth_year"].notna(),
        np.maximum(out["event_year"] - out["firm_birth_year"], 0),
        np.nan,
    )

    out["first_contract_year"] = np.where(out["treated_firm"] == 1, out["event_year"], 0).astype(int)
    out["already_treated_before_event"] = 0
    out["not_yet_treated_at_event"] = (out["treated_firm"] == 0).astype(int)
    out["treatment"] = ((out["treated_firm"] == 1) & (out["year"] >= out["event_year"])).astype(int)
    out["did_treatment"] = out["treated_firm"] * out["post_event"]

    winner_map = (
        out.loc[out["treated_firm"] == 1, ["stack_id", "law_firm"]]
        .drop_duplicates("stack_id")
        .rename(columns={"law_firm": "winner_firm_new"})
    )
    out = out.merge(winner_map, on="stack_id", how="left")
    out["winner_firm"] = out["winner_firm_new"]

    out = out.drop(
        columns=[
            "old_firm_id",
            "new_firm_id",
            "new_law_firm",
            "new_firm_capital",
            "new_firm_birth_year",
            "winner_firm_new",
        ]
    )
    return out


def interpolate_value(year: int, known_years: np.ndarray, known_values: np.ndarray) -> float:
    if len(known_years) == 0:
        return 0.0
    if year in known_years:
        return float(known_values[np.where(known_years == year)[0][0]])
    left_mask = known_years < year
    right_mask = known_years > year
    if left_mask.any() and right_mask.any():
        left_year = known_years[left_mask].max()
        right_year = known_years[right_mask].min()
        left_val = float(known_values[np.where(known_years == left_year)[0][0]])
        right_val = float(known_values[np.where(known_years == right_year)[0][0]])
        weight = (year - left_year) / (right_year - left_year)
        return left_val + weight * (right_val - left_val)
    if left_mask.any():
        left_years = np.sort(known_years[left_mask])
        left_year = left_years[-1]
        left_val = float(known_values[np.where(known_years == left_year)[0][0]])
        if len(left_years) >= 2:
            prev_left_year = left_years[-2]
            prev_left_val = float(known_values[np.where(known_years == prev_left_year)[0][0]])
            slope = (left_val - prev_left_val) / (left_year - prev_left_year)
            return left_val + slope * (year - left_year)
        return left_val
    right_years = np.sort(known_years[right_mask])
    right_year = right_years[0]
    right_val = float(known_values[np.where(known_years == right_year)[0][0]])
    if len(right_years) >= 2:
        next_right_year = right_years[1]
        next_right_val = float(known_values[np.where(known_years == next_right_year)[0][0]])
        slope = (next_right_val - right_val) / (next_right_year - right_year)
        return right_val + slope * (year - right_year)
    return right_val


def fill_member_panel(df: pd.DataFrame, scope: str = "window") -> tuple[pd.DataFrame, int]:
    year_min = int(df["year"].min())
    year_max = int(df["year"].max())

    member_keys = ["stack_id", "firm_id"]
    member_static = df[STATIC_COLS].drop_duplicates(member_keys).copy()
    if scope == "full":
        member_static["window_lo"] = year_min
        member_static["window_hi"] = year_max
    else:
        member_static["window_lo"] = member_static["event_year"].map(lambda x: max(year_min, int(x) - 5))
        member_static["window_hi"] = member_static["event_year"].map(lambda x: min(year_max, int(x) + 5))

    current_years = (
        df[member_keys + ["year"]]
        .drop_duplicates()
        .groupby(member_keys)["year"]
        .agg(lambda values: set(int(v) for v in values))
        .to_dict()
    )
    member_groups = {
        key: grp.sort_values("year").copy()
        for key, grp in df.groupby(member_keys, sort=False)
    }

    new_rows: list[dict[str, object]] = []

    for member in member_static.itertuples(index=False):
        member_dt = member_groups[(member.stack_id, member.firm_id)]
        observed_years = current_years.get((member.stack_id, member.firm_id), set())
        expected_years = set(range(int(member.window_lo), int(member.window_hi) + 1))
        missing_years = sorted(expected_years - observed_years)
        if not missing_years:
            continue

        years = member_dt["year"].to_numpy(dtype=int)
        counts = {col: member_dt[col].to_numpy(dtype=float) for col in DYNAMIC_COUNT_COLS}
        floats = {col: member_dt[col].to_numpy(dtype=float) for col in FLOAT_COLS}

        for year in missing_years:
            row = {col: getattr(member, col) for col in STATIC_COLS}
            row["year"] = year

            for col in DYNAMIC_COUNT_COLS:
                row[col] = max(int(round(interpolate_value(year, years, counts[col]))), 0)

            for col in FLOAT_COLS:
                row[col] = float(interpolate_value(year, years, floats[col]))

            row["civil_case_n"] = row["enterprise_case_n"] + row["personal_case_n"]
            row["civil_decisive_case_n"] = min(row["civil_decisive_case_n"], row["civil_case_n"])
            row["civil_win_n_binary"] = min(row["civil_win_n_binary"], row["civil_decisive_case_n"])
            row["civil_win_rate_mean"] = (
                row["civil_win_n_binary"] / row["civil_decisive_case_n"]
                if row["civil_decisive_case_n"] > 0
                else 0.0
            )
            row["event_time"] = int(year - member.event_year)
            row["post_event"] = int(year >= member.event_year)
            row["did_treatment"] = int(member.treated_firm) * row["post_event"]
            row["treatment"] = int(member.treated_firm) * row["post_event"]
            row["already_treated_before_event"] = 0
            row["not_yet_treated_at_event"] = int(member.control_firm)
            row["imputed_balance_row"] = 1
            new_rows.append(row)

    if not new_rows:
        return df, 0

    filled = pd.concat([df, pd.DataFrame(new_rows)], ignore_index=True, sort=False)
    return filled, len(new_rows)


def normalize_win_rate(df: pd.DataFrame) -> pd.DataFrame:
    out = df.copy()
    decisive_positive = out["civil_decisive_case_n"] > 0
    out["civil_win_rate_mean"] = np.where(
        decisive_positive,
        out["civil_win_n_binary"] / out["civil_decisive_case_n"],
        np.nan,
    )
    return out


def build_summary(df: pd.DataFrame, mapping: pd.DataFrame, stats: dict[str, int], added_rows: int) -> str:
    fy = df.groupby(["firm_id", "year"]).size()
    city_by_fy = df.groupby(["firm_id", "year"])["city"].nunique()
    prov_by_fy = df.groupby(["firm_id", "year"])["province"].nunique()
    all_sy = df.groupby(["stack_id", "year"]).agg(
        rows=("firm_id", "size"),
        stack_firm_n=("stack_firm_n", "first"),
        treated_sum=("treated_firm", "sum"),
    )
    win = df.loc[df["event_time"].between(-5, 5)].groupby(["stack_id", "year"]).agg(
        rows=("firm_id", "size"),
        stack_firm_n=("stack_firm_n", "first"),
        treated_sum=("treated_firm", "sum"),
    )

    provider = pd.read_parquet(
        MASTER_FILE,
        columns=["firm_id", "provider_type_final"],
    ).drop_duplicates("firm_id")
    member_provider = (
        df[["firm_id"]]
        .drop_duplicates()
        .merge(provider, on="firm_id", how="left")
    )

    added_row_label = (
        "Added synthetic member-year rows to fully balance 2010-2020 stack panels"
        if FILL_SCOPE == "full"
        else "Added synthetic member-year rows inside the analysis event window"
    )

    missing_total = int(df.isna().sum().sum())
    winrate_missing = int(df["civil_win_rate_mean"].isna().sum())

    lines = [
        "# Firm Structural Repair Candidate Summary (2026-04-16)",
        "",
        f"- Source: `{SOURCE_FILE}`",
        f"- Candidate: `{OUT_FILE}`",
        f"- Rows: `{len(df):,}`",
        f"- {added_row_label}: `{added_rows:,}`",
        f"- Stacks: `{df['stack_id'].nunique():,}`",
        f"- Unique firms: `{df['firm_id'].nunique():,}`",
        "",
        "## Structural checks",
        f"- `firm_id × year` duplicated groups: `{int((fy > 1).sum()):,}`",
        f"- `firm_id × year` in multiple cities: `{int((city_by_fy > 1).sum()):,}`",
        f"- `firm_id × year` in multiple provinces: `{int((prov_by_fy > 1).sum()):,}`",
        f"- All stack-years with fewer rows than `stack_firm_n`: `{int((all_sy['rows'] < all_sy['stack_firm_n']).sum()):,}`",
        f"- All stack-years with treated count != 1: `{int((all_sy['treated_sum'] != 1).sum()):,}`",
        f"- Event-window stack-years with fewer rows than `stack_firm_n`: `{int((win['rows'] < win['stack_firm_n']).sum()):,}`",
        f"- Event-window stack-years with treated count != 1: `{int((win['treated_sum'] != 1).sum()):,}`",
        "",
        "## Replacement checks",
        f"- Replaced stack memberships: `{stats['replaced_memberships']:,}`",
        f"- Surrogate fallback with locality score > 3: `{stats['fallback_nonlocal']:,}`",
        f"- Final unique firms classified as `law_firm` in master: `{int(member_provider['provider_type_final'].eq('law_firm').sum()):,}` of `{len(member_provider):,}`",
        "",
        "## Variable integrity",
        f"- `enterprise_case_n + personal_case_n == civil_case_n`: `{int((df['enterprise_case_n'] + df['personal_case_n'] == df['civil_case_n']).all())}`",
        f"- `civil_win_n_binary <= civil_decisive_case_n <= civil_case_n`: `{int(((df['civil_win_n_binary'] <= df['civil_decisive_case_n']) & (df['civil_decisive_case_n'] <= df['civil_case_n'])).all())}`",
        f"- `civil_win_rate_mean` is missing when `civil_decisive_case_n == 0`: `{int(df.loc[df['civil_decisive_case_n'] == 0, 'civil_win_rate_mean'].isna().all())}`",
        f"- Missing values in final file: `{missing_total:,}`",
        f"- Missing `civil_win_rate_mean` rows: `{winrate_missing:,}`",
        "",
    ]
    return "\n".join(lines)


def main() -> None:
    df = load_current_panel()
    mapping, stats = build_member_mapping(df)
    repaired = apply_member_mapping(df, mapping)
    repaired, added_rows = fill_member_panel(repaired, scope=FILL_SCOPE)
    repaired = normalize_win_rate(repaired)
    repaired = repaired.sort_values(["stack_id", "firm_id", "year"]).reset_index(drop=True)

    repaired = repaired.drop(columns=["surrogate_locality_score", "used_surrogate"], errors="ignore")
    repaired = repaired[FINAL_COLS].copy()
    summary_text = build_summary(repaired, mapping, stats, added_rows)
    OUT_FILE.parent.mkdir(parents=True, exist_ok=True)
    repaired.to_csv(OUT_FILE, index=False)
    SUMMARY_FILE.write_text(summary_text, encoding="utf-8")
    print(f"Wrote {OUT_FILE}")
    print(f"Wrote {SUMMARY_FILE}")


if __name__ == "__main__":
    main()
