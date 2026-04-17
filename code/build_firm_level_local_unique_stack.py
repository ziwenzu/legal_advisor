#!/usr/bin/env python3

from __future__ import annotations

import importlib.util
import math
import sys
from pathlib import Path

import numpy as np
import pandas as pd


ROOT = Path(__file__).resolve().parents[1]
TRUE_STACK_SCRIPT = ROOT / "code" / "build_firm_level_true_stack.py"
MASTER_FILE = ROOT / "data" / "temp data" / "litigation_panels_full" / "law_firm_master.parquet"
OUT_FILE = ROOT / "data" / "output data" / "firm_level_local_unique_stack.csv"
SUMMARY_FILE = ROOT / "data" / "output data" / "firm_level_local_unique_stack_summary_20260416.md"
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


def load_true_stack_module():
    spec = importlib.util.spec_from_file_location("firm_true_stack", TRUE_STACK_SCRIPT)
    module = importlib.util.module_from_spec(spec)
    sys.modules["firm_true_stack"] = module
    assert spec.loader is not None
    spec.loader.exec_module(module)
    return module


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


def build_base_panel(module):
    lookup, preferred_display = module.build_name_lookup()
    membership, summary = module.build_true_stack_membership(lookup, preferred_display)
    firm_year, attrs = module.build_raw_firm_year(preferred_display)
    case_mix = module.build_case_mix_by_firm_year(lookup)

    provider = pd.read_parquet(
        MASTER_FILE,
        columns=["firm_id", "provider_type_final", "law_firm_canonical"],
    ).drop_duplicates("firm_id")
    membership = membership.merge(provider, on="firm_id", how="left")

    clean_stack_ids = set(
        summary.loc[~summary["potential_grouping_ambiguity"], "stack_id"].tolist()
    )
    membership = membership.loc[membership["stack_id"].isin(clean_stack_ids)].copy()
    membership = membership.loc[membership["provider_type_final"].eq("law_firm")].copy()
    membership = membership.drop(columns=["provider_type_final", "law_firm_canonical"])

    balanced = module.build_balanced_panel(membership, firm_year, attrs, case_mix)
    final = membership.merge(balanced, on=["firm_id", "law_firm"], how="left")
    final["event_time"] = final["year"] - final["event_year"]
    final["post_event"] = (final["year"] >= final["event_year"]).astype(int)
    final["did_treatment"] = final["treated_firm"] * final["post_event"]
    final["treatment"] = (
        (final["first_contract_year"] > 0) & (final["year"] >= final["first_contract_year"])
    ).astype(int)
    final["already_treated_before_event"] = (
        (final["first_contract_year"] > 0) & (final["first_contract_year"] < final["event_year"])
    ).astype(int)
    final["not_yet_treated_at_event"] = (
        (final["first_contract_year"] == 0) | (final["first_contract_year"] > final["event_year"])
    ).astype(int)
    final["firm_age_at_event"] = np.where(
        final["firm_birth_year"].notna(),
        np.maximum(final["event_year"] - final["firm_birth_year"], 0),
        np.nan,
    )

    baseline = final.loc[
        final["year"] == final["event_year"], ["stack_id", "firm_id", "firm_size"]
    ].drop_duplicates()
    baseline = baseline.rename(columns={"firm_size": "firm_size_baseline"})
    final = final.merge(baseline, on=["stack_id", "firm_id"], how="left")
    final["firm_size_baseline"] = final["firm_size_baseline"].fillna(final["firm_size"])

    return final


def build_local_pool(base_members: pd.DataFrame) -> pd.DataFrame:
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

    used_names = set(base_members["law_firm"].astype(str))
    pool = master.loc[~master["law_firm"].isin(used_names)].copy()
    pool = pool.drop_duplicates("firm_id")
    return pool[
        [
            "firm_id",
            "law_firm",
            "firm_capital",
            "firm_birth_year",
            "first_contract_year",
            "firm_size_anchor",
        ]
    ]


def choose_surrogate(
    pool: pd.DataFrame,
    used_ids: set[str],
    province: str,
    city: str,
    target_size: float,
    target_capital: float,
    target_age: float,
    treated_firm: int,
    event_year: int,
) -> pd.Series:
    work = pool.loc[~pool["firm_id"].isin(used_ids)].copy()
    work["locality_score"] = work["law_firm"].map(lambda x: locality_score(x, province, city))
    work = work.loc[work["locality_score"] <= 3].copy()
    if work.empty:
        raise RuntimeError(f"No local surrogate pool left for {province}-{city}.")

    if treated_firm == 0:
        work = work.loc[
            (work["first_contract_year"] == 0) | (work["first_contract_year"] > int(event_year))
        ].copy()
        if work.empty:
            work = pool.loc[~pool["firm_id"].isin(used_ids)].copy()
            work["locality_score"] = work["law_firm"].map(lambda x: locality_score(x, province, city))
            work = work.loc[work["locality_score"] <= 3].copy()

    size_term = np.abs(np.log1p(work["firm_size_anchor"].fillna(0)) - math.log1p(max(target_size, 0)))
    capital_term = np.abs(np.log1p(work["firm_capital"].fillna(0)) - math.log1p(max(target_capital, 0)))
    age_term = np.abs(
        (int(event_year) - work["firm_birth_year"].fillna(int(event_year))).clip(lower=0) - target_age
    )
    work["match_score"] = (
        100 * work["locality_score"] + size_term + 0.35 * capital_term + 0.05 * age_term
    )
    return work.sort_values(["match_score", "law_firm"]).iloc[0]


def reassign_duplicate_firms(base: pd.DataFrame) -> pd.DataFrame:
    member_cols = [
        "stack_id",
        "firm_id",
        "law_firm",
        "treated_firm",
        "control_firm",
        "province",
        "city",
        "event_year",
        "firm_size_baseline",
        "firm_capital",
        "firm_birth_year",
    ]
    members = base[member_cols].drop_duplicates().copy()
    members["city_count"] = members.groupby("firm_id")["city"].transform("nunique")
    members["stack_count"] = members.groupby("firm_id")["stack_id"].transform("nunique")

    members = members.sort_values(
        ["firm_id", "treated_firm", "event_year", "stack_id"],
        ascending=[True, False, True, True],
    ).copy()
    members["occurrence_rank"] = members.groupby("firm_id").cumcount()

    pool = build_local_pool(members)
    used_ids = set(
        members.loc[members["occurrence_rank"] == 0, "firm_id"].astype(str).tolist()
    )

    replacements: list[dict[str, object]] = []
    for row in members.itertuples(index=False):
        needs_replacement = (
            row.occurrence_rank > 0 or row.city_count > 1 or row.stack_count > 1
        )
        if not needs_replacement:
            replacements.append(
                {
                    "stack_id": row.stack_id,
                    "old_firm_id": row.firm_id,
                    "new_firm_id": row.firm_id,
                    "new_law_firm": row.law_firm,
                    "new_firm_capital": row.firm_capital,
                    "new_firm_birth_year": row.firm_birth_year,
                }
            )
            continue

        target_age = 0.0
        if pd.notna(row.firm_birth_year):
            target_age = max(float(row.event_year) - float(row.firm_birth_year), 0.0)

        surrogate = choose_surrogate(
            pool=pool,
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
        replacements.append(
            {
                "stack_id": row.stack_id,
                "old_firm_id": row.firm_id,
                "new_firm_id": surrogate["firm_id"],
                "new_law_firm": surrogate["law_firm"],
                "new_firm_capital": surrogate["firm_capital"],
                "new_firm_birth_year": surrogate["firm_birth_year"],
            }
        )

    mapping = pd.DataFrame(replacements)
    out = base.merge(mapping, left_on=["stack_id", "firm_id"], right_on=["stack_id", "old_firm_id"], how="left")

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
    out["firm_size"] = out["firm_size"].fillna(0)
    out["firm_size_baseline"] = out["firm_size_baseline"].fillna(out["firm_size"])

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
    out = out.drop(columns=["old_firm_id", "new_firm_id", "new_law_firm", "winner_firm_new", "new_firm_capital", "new_firm_birth_year"])

    stack_counts = (
        out[["stack_id", "firm_id", "control_firm"]]
        .drop_duplicates()
        .groupby("stack_id", as_index=False)
        .agg(
            stack_firm_n=("firm_id", "nunique"),
            stack_control_firm_n=("control_firm", "sum"),
        )
    )
    stack_counts["stack_control_target_n"] = stack_counts["stack_control_firm_n"]
    out = out.drop(
        columns=["stack_firm_n", "stack_control_firm_n", "stack_control_target_n"]
    ).merge(stack_counts, on="stack_id", how="left")

    return out


def write_summary(df: pd.DataFrame) -> None:
    fy = df.groupby(["firm_id", "year"]).size()
    n_city = df.groupby(["firm_id", "year"])["city"].nunique()
    n_prov = df.groupby(["firm_id", "year"])["province"].nunique()
    by_stack_year = df.groupby(["stack_id", "year"]).agg(
        rows=("firm_id", "size"),
        stack_firm_n=("stack_firm_n", "first"),
        treated_sum=("treated_firm", "sum"),
    )

    lines = [
        "# Local Unique Firm Stack Summary (2026-04-16)",
        "",
        f"- File: `{OUT_FILE}`",
        f"- Rows: `{len(df)}`",
        f"- Stacks: `{df['stack_id'].nunique()}`",
        f"- Unique tenders: `{df['approx_tender_id'].nunique()}`",
        f"- Unique firms: `{df['firm_id'].nunique()}`",
        "",
        "## Structural checks",
        f"- `firm_id × year` duplicated groups: `{int((fy > 1).sum())}`",
        f"- `firm_id × year` appearing in multiple cities: `{int((n_city > 1).sum())}`",
        f"- `firm_id × year` appearing in multiple provinces: `{int((n_prov > 1).sum())}`",
        f"- Stack-years with fewer rows than `stack_firm_n`: `{int((by_stack_year['rows'] < by_stack_year['stack_firm_n']).sum())}`",
        f"- Stack-years with treated count != 1: `{int((by_stack_year['treated_sum'] != 1).sum())}`",
        "",
    ]
    SUMMARY_FILE.write_text("\n".join(lines), encoding="utf-8")


def main() -> None:
    module = load_true_stack_module()
    base = build_base_panel(module)
    final = reassign_duplicate_firms(base)
    final = final.sort_values(["stack_id", "firm_id", "year"]).reset_index(drop=True)
    final.to_csv(OUT_FILE, index=False)
    write_summary(final)
    print(f"Wrote {OUT_FILE}")
    print(f"Wrote {SUMMARY_FILE}")


if __name__ == "__main__":
    main()
