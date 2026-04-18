#!/usr/bin/env python3

from __future__ import annotations

import json
import re
from collections import defaultdict
from pathlib import Path

import numpy as np
import pandas as pd


ROOT = Path(__file__).resolve().parents[1]
TENDER_FILE = ROOT / "data" / "temp data" / "legal_procurement_tender_level.csv"
MASTER_FILE = ROOT / "data" / "temp data" / "litigation_panels_full" / "law_firm_master.parquet"
MERGED_FILE = ROOT / "data" / "temp data" / "litigation_panels_full" / "law_firm_year_panel_merged.parquet"
PROCUREMENT_FILE = ROOT / "data" / "temp data" / "litigation_panels_full" / "procurement_firm_year_panel.parquet"
CURRENT_FIRM_FILE = ROOT / "data" / "output data" / "firm_level.csv"
CASE_LEVEL_FILE = ROOT / "data" / "output data" / "case_level.csv"
OUT_FILE = ROOT / "data" / "output data" / "firm_level_true_stack.csv"
SUMMARY_FILE = ROOT / "data" / "output data" / "firm_level_true_stack_summary_20260416.md"

YEAR_MIN = 2010
YEAR_MAX = 2020
EVENT_MIN = 2014
EVENT_MAX = 2020

LEADING_NOISE = (
    "中华人民共和国",
    "分别为",
    "分别是",
    "分别系",
    "均为",
    "均系",
    "依次为",
    "依次是",
    "依次系",
    "二人系",
    "与本案代理人均有",
    "并与",
    "并向",
)


def clean_name(name: str) -> str:
    text = str(name).strip()
    text = text.replace("（", "(").replace("）", ")")
    text = re.sub(r"\s+", "", text)
    text = text.replace("律师事务所有限公司", "律师事务所")
    text = text.replace("律师所事务所", "律师事务所")
    text = text.replace("律师事事务所", "律师事务所")
    text = text.replace("律师师事务所", "律师事务所")
    text = text.replace("律师说事务所", "律师事务所")
    text = text.replace("律师实习事务所", "律师事务所")
    text = text.replace("律师律师事务所", "律师事务所")
    text = text.replace("律事务所", "律师事务所")
    text = text.replace("事务所", "事务所")
    for prefix in LEADING_NOISE:
        if text.startswith(prefix) and len(text) > len(prefix) + 4:
            text = text[len(prefix) :]
    if text.startswith("北京市") and not text.startswith("北京市("):
        text = "北京" + text[len("北京市") :]
    if text.startswith("上海市") and not text.startswith("上海市("):
        text = "上海" + text[len("上海市") :]
    return text


def generate_aliases(name: str) -> set[str]:
    base = clean_name(name)
    aliases = {base}

    m = re.match(r"^(.*)\((.+)\)律师事务所$", base)
    if m:
        stem, branch = m.groups()
        aliases.add(f"{stem}{branch}律师事务所")
        aliases.add(f"{stem}律师事务所{branch}分所")
        aliases.add(f"{stem}{branch}分所律师事务所")

    m = re.match(r"^(.*)律师事务所(.+?)分所$", base)
    if m:
        stem, branch = m.groups()
        aliases.add(f"{stem}({branch})律师事务所")

    m = re.match(r"^(.*?)([\u4e00-\u9fff]{2,4})律师事务所$", base)
    if m and "(" not in base:
        stem, branch = m.groups()
        aliases.add(f"{stem}({branch})律师事务所")

    if base.startswith("北京") and not base.startswith("北京市"):
        aliases.add("北京市" + base[len("北京") :])
    if base.startswith("上海") and not base.startswith("上海市"):
        aliases.add("上海市" + base[len("上海") :])

    return {alias for alias in aliases if alias}


def parse_name_list(value: object) -> list[str]:
    if pd.isna(value):
        return []
    text = str(value)
    try:
        loaded = json.loads(text)
        items = loaded if isinstance(loaded, list) else [loaded]
    except Exception:
        items = [text]

    out: list[str] = []
    for item in items:
        parts = re.split(r"\|\||/|／|,|，|;|；", str(item))
        for part in parts:
            cleaned = part.strip()
            if cleaned:
                out.append(cleaned)
    return out


def choose_display_name(options: list[str]) -> str:
    valid = [clean_name(x) for x in options if isinstance(x, str) and x and x != "nan"]
    if not valid:
        return ""
    valid.sort(key=lambda x: (len(x), x))
    return valid[0]


def choose_positive_min(values: pd.Series) -> int:
    vals = pd.to_numeric(values, errors="coerce")
    vals = vals[vals > 0]
    if vals.empty:
        return 0
    return int(vals.min())


def build_name_lookup() -> tuple[dict[str, tuple[str, str]], dict[str, str]]:
    alias_candidates: dict[str, list[tuple[int, str, str]]] = defaultdict(list)
    preferred_names: dict[str, list[str]] = defaultdict(list)

    current = pd.read_csv(CURRENT_FIRM_FILE, usecols=["law_firm", "firm_id"]).drop_duplicates()
    master = pd.read_parquet(
        MASTER_FILE,
        columns=["firm_id", "law_firm_clean", "law_firm_canonical"],
    )
    procurement = pd.read_parquet(
        PROCUREMENT_FILE,
        columns=["law_firm_clean", "firm_id"],
    ).drop_duplicates()

    def add_record(name: str, firm_id: str, priority: int) -> None:
        if pd.isna(name) or pd.isna(firm_id):
            return
        display_name = clean_name(name)
        preferred_names[firm_id].append(display_name)
        for alias in generate_aliases(name):
            alias_candidates[alias].append((priority, firm_id, display_name))

    for row in current.itertuples(index=False):
        add_record(row.law_firm, row.firm_id, 0)
    for row in master.itertuples(index=False):
        add_record(row.law_firm_canonical, row.firm_id, 1)
        add_record(row.law_firm_clean, row.firm_id, 2)
    for row in procurement.itertuples(index=False):
        add_record(row.law_firm_clean, row.firm_id, 3)

    lookup: dict[str, tuple[str, str]] = {}
    for alias, items in alias_candidates.items():
        best_priority = min(item[0] for item in items)
        best_items = [item for item in items if item[0] == best_priority]
        firm_ids = {item[1] for item in best_items}
        if len(firm_ids) != 1:
            continue
        priority, firm_id, display_name = sorted(best_items, key=lambda x: (len(x[2]), x[2]))[0]
        lookup[alias] = (firm_id, display_name)

    preferred_display = {
        firm_id: choose_display_name(names)
        for firm_id, names in preferred_names.items()
    }
    return lookup, preferred_display


def map_name(name: str, lookup: dict[str, tuple[str, str]]) -> tuple[str | None, str | None]:
    for alias in generate_aliases(name):
        if alias in lookup:
            return lookup[alias]
    return None, None


def build_case_mix_by_firm_year(lookup: dict[str, tuple[str, str]]) -> pd.DataFrame:
    pieces: list[pd.DataFrame] = []
    usecols = [
        "law_firm",
        "year",
        "side",
        "plaintiff_party_is_entity",
        "defendant_party_is_entity",
        "case_decisive",
        "case_win_rate_fee",
    ]

    for chunk in pd.read_csv(CASE_LEVEL_FILE, usecols=usecols, chunksize=1_000_000):
        mapped = chunk["law_firm"].map(lambda x: map_name(x, lookup)[0])
        chunk = chunk.loc[mapped.notna()].copy()
        chunk["firm_id"] = mapped.loc[chunk.index]
        chunk["enterprise_case"] = np.where(
            chunk["side"].eq("plaintiff"),
            chunk["plaintiff_party_is_entity"],
            chunk["defendant_party_is_entity"],
        ).astype(int)
        chunk["personal_case"] = 1 - chunk["enterprise_case"]
        chunk["case_decisive"] = pd.to_numeric(chunk["case_decisive"], errors="coerce").fillna(0).astype(int)
        chunk["case_win_rate_fee"] = pd.to_numeric(chunk["case_win_rate_fee"], errors="coerce")
        chunk["fee_winrate_available"] = (
            chunk["case_decisive"].eq(1) & chunk["case_win_rate_fee"].notna()
        ).astype(int)
        chunk["fee_winrate_sum"] = chunk["case_win_rate_fee"].where(
            chunk["fee_winrate_available"].eq(1), 0.0
        )
        agg = (
            chunk.groupby(["firm_id", "year"], as_index=False)
            .agg(
                civil_case_n_case=("firm_id", "size"),
                enterprise_case_n=("enterprise_case", "sum"),
                personal_case_n=("personal_case", "sum"),
                civil_fee_decisive_case_n=("fee_winrate_available", "sum"),
                civil_win_rate_fee_sum=("fee_winrate_sum", "sum"),
            )
        )
        pieces.append(agg)

    if not pieces:
        return pd.DataFrame(columns=["firm_id", "year", "enterprise_case_n", "personal_case_n"])

    out = pd.concat(pieces, ignore_index=True)
    out = (
        out.groupby(["firm_id", "year"], as_index=False)
        .agg(
            civil_case_n_case=("civil_case_n_case", "sum"),
            enterprise_case_n=("enterprise_case_n", "sum"),
            personal_case_n=("personal_case_n", "sum"),
            civil_fee_decisive_case_n=("civil_fee_decisive_case_n", "sum"),
            civil_win_rate_fee_sum=("civil_win_rate_fee_sum", "sum"),
        )
    )
    return out


def build_raw_firm_year(preferred_display: dict[str, str]) -> tuple[pd.DataFrame, pd.DataFrame]:
    merged = pd.read_parquet(
        MERGED_FILE,
        columns=[
            "firm_id",
            "law_firm_canonical",
            "panel_year",
            "civil_cases_total_n",
            "wins_n_civil_defendant",
            "wins_n_civil_plaintiff",
            "decisive_cases_n_civil_defendant",
            "decisive_cases_n_civil_plaintiff",
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
    merged["law_firm"] = merged["firm_id"].map(preferred_display).fillna(
        merged["law_firm_canonical"].map(clean_name)
    )
    merged["civil_case_n"] = merged["civil_cases_total_n"].fillna(0).astype(int)
    merged["civil_win_n_binary"] = (
        merged["wins_n_civil_defendant"].fillna(0) + merged["wins_n_civil_plaintiff"].fillna(0)
    ).astype(int)
    merged["civil_decisive_case_n"] = (
        merged["decisive_cases_n_civil_defendant"].fillna(0)
        + merged["decisive_cases_n_civil_plaintiff"].fillna(0)
    ).astype(int)
    duration_n = (
        merged["duration_obs_n_civil_defendant"].fillna(0)
        + merged["duration_obs_n_civil_plaintiff"].fillna(0)
    )
    duration_total = (
        merged["avg_duration_days_civil_defendant"].fillna(0)
        * merged["duration_obs_n_civil_defendant"].fillna(0)
        + merged["avg_duration_days_civil_plaintiff"].fillna(0)
        * merged["duration_obs_n_civil_plaintiff"].fillna(0)
    )
    merged["avg_filing_to_hearing_days"] = np.where(duration_n > 0, duration_total / duration_n, 0.0)
    merged["firm_capital"] = merged["firm_capital_final"]
    merged["firm_birth_year"] = merged["firm_birth_year_final"]
    merged["first_contract_year"] = merged["first_contract_year"].fillna(0).astype(int)
    merged["year"] = merged["panel_year"].astype(int)

    firm_year = merged[
        [
            "firm_id",
            "law_firm",
            "year",
            "civil_case_n",
            "civil_win_n_binary",
            "civil_decisive_case_n",
            "avg_filing_to_hearing_days",
            "firm_size",
            "firm_capital",
            "firm_birth_year",
            "first_contract_year",
        ]
    ].drop_duplicates(subset=["firm_id", "year"])

    attrs = pd.read_parquet(
        MASTER_FILE,
        columns=[
            "firm_id",
            "law_firm_canonical",
            "firm_size",
            "firm_capital_final",
            "firm_birth_year_final",
            "first_contract_year",
        ],
    ).drop_duplicates(subset=["firm_id"])
    attrs["law_firm"] = attrs["firm_id"].map(preferred_display).fillna(
        attrs["law_firm_canonical"].map(clean_name)
    )
    attrs["firm_capital"] = attrs["firm_capital_final"]
    attrs["firm_birth_year"] = attrs["firm_birth_year_final"]
    attrs["first_contract_year"] = attrs["first_contract_year"].fillna(0).astype(int)
    attrs = attrs[
        [
            "firm_id",
            "law_firm",
            "firm_size",
            "firm_capital",
            "firm_birth_year",
            "first_contract_year",
        ]
    ]

    current_meta = pd.read_csv(
        CURRENT_FIRM_FILE,
        usecols=[
            "firm_id",
            "law_firm",
            "firm_size",
            "firm_size_baseline",
            "firm_capital",
            "firm_birth_year",
            "first_contract_year",
        ],
    )
    current_meta = (
        current_meta.groupby("firm_id", as_index=False)
        .agg(
            law_firm_current=("law_firm", lambda s: choose_display_name(s.dropna().astype(str).tolist())),
            firm_size_current=("firm_size_baseline", lambda s: pd.to_numeric(s, errors="coerce").median()),
            firm_capital_current=("firm_capital", lambda s: pd.to_numeric(s, errors="coerce").median()),
            firm_birth_year_current=("firm_birth_year", lambda s: pd.to_numeric(s, errors="coerce").median()),
            first_contract_year_current=("first_contract_year", choose_positive_min),
        )
    )

    attrs = attrs.merge(current_meta, on="firm_id", how="left")
    attrs["law_firm"] = attrs["law_firm"].fillna(attrs["law_firm_current"])
    attrs["firm_size"] = attrs["firm_size"].fillna(attrs["firm_size_current"])
    attrs["firm_capital"] = attrs["firm_capital"].fillna(attrs["firm_capital_current"])
    attrs["firm_birth_year"] = attrs["firm_birth_year"].fillna(attrs["firm_birth_year_current"])
    attrs["first_contract_year"] = attrs["first_contract_year"].where(
        attrs["first_contract_year"] > 0,
        attrs["first_contract_year_current"].fillna(0),
    ).fillna(0).astype(int)
    attrs = attrs.drop(
        columns=[
            "law_firm_current",
            "firm_size_current",
            "firm_capital_current",
            "firm_birth_year_current",
            "first_contract_year_current",
        ]
    )

    return firm_year, attrs


def build_true_stack_membership(
    lookup: dict[str, tuple[str, str]],
    preferred_display: dict[str, str],
) -> tuple[pd.DataFrame, pd.DataFrame]:
    tender = pd.read_csv(TENDER_FILE)
    tender = tender.loc[tender["year"].between(EVENT_MIN, EVENT_MAX)].copy()

    stack_rows: list[dict[str, object]] = []
    summary_rows: list[dict[str, object]] = []
    inferred_first_contract: dict[str, int] = {}

    for row in tender.itertuples(index=False):
        winners = []
        for name in parse_name_list(row.winner_list_json):
            firm_id, display_name = map_name(name, lookup)
            if firm_id is not None:
                winners.append((firm_id, display_name or preferred_display.get(firm_id, clean_name(name))))
        if not winners:
            continue

        candidate_items = []
        for name in parse_name_list(row.candidate_list_json):
            firm_id, display_name = map_name(name, lookup)
            if firm_id is not None:
                candidate_items.append((firm_id, display_name or preferred_display.get(firm_id, clean_name(name))))

        winner_ids = {firm_id for firm_id, _ in winners}
        candidate_controls = {}
        for firm_id, display_name in candidate_items:
            if firm_id not in winner_ids:
                candidate_controls[firm_id] = display_name
        if not candidate_controls:
            continue

        for winner_id, winner_name in winners:
            controls = [
                (firm_id, display_name)
                for firm_id, display_name in sorted(candidate_controls.items())
                if firm_id != winner_id
            ]
            if not controls:
                continue

            inferred_first_contract[winner_id] = min(
                int(row.year),
                inferred_first_contract.get(winner_id, int(row.year)),
            )
            stack_id = f"{row.approx_tender_id}__{winner_id[:8]}"
            control_n = len(controls)

            stack_rows.append(
                {
                    "stack_id": stack_id,
                    "approx_tender_id": row.approx_tender_id,
                    "province": row.province,
                    "city": row.city,
                    "event_year": int(row.year),
                    "event_month": int(row.month),
                    "winner_firm": winner_name,
                    "firm_id": winner_id,
                    "law_firm": winner_name,
                    "treated_firm": 1,
                    "control_firm": 0,
                    "stack_control_target_n": control_n,
                    "stack_control_firm_n": control_n,
                    "stack_firm_n": control_n + 1,
                    "stack_control_balance_weight": 1.0,
                    "global_balance_weight": 1.0,
                }
            )

            control_weight = 1.0 / control_n
            for control_id, control_name in controls:
                stack_rows.append(
                    {
                        "stack_id": stack_id,
                        "approx_tender_id": row.approx_tender_id,
                        "province": row.province,
                        "city": row.city,
                        "event_year": int(row.year),
                        "event_month": int(row.month),
                        "winner_firm": winner_name,
                        "firm_id": control_id,
                        "law_firm": control_name,
                        "treated_firm": 0,
                        "control_firm": 1,
                        "stack_control_target_n": control_n,
                        "stack_control_firm_n": control_n,
                        "stack_firm_n": control_n + 1,
                        "stack_control_balance_weight": control_weight,
                        "global_balance_weight": control_weight,
                    }
                )

            summary_rows.append(
                {
                    "stack_id": stack_id,
                    "approx_tender_id": row.approx_tender_id,
                    "province": row.province,
                    "city": row.city,
                    "event_year": int(row.year),
                    "event_month": int(row.month),
                    "winner_firm": winner_name,
                    "winner_firm_id": winner_id,
                    "control_n": control_n,
                    "runner_up_info_observed": bool(row.runner_up_info_observed),
                    "multi_winner_tender": bool(row.multi_winner_tender),
                    "potential_grouping_ambiguity": bool(row.potential_grouping_ambiguity),
                }
            )

    membership = pd.DataFrame(stack_rows).drop_duplicates(
        subset=["stack_id", "firm_id"]
    )
    summary = pd.DataFrame(summary_rows).drop_duplicates(subset=["stack_id"])
    inferred = pd.DataFrame(
        [{"firm_id": firm_id, "first_contract_year_inferred": year} for firm_id, year in inferred_first_contract.items()]
    )
    return membership, summary.merge(inferred, how="left", left_on="winner_firm_id", right_on="firm_id")


def build_balanced_panel(membership: pd.DataFrame, firm_year: pd.DataFrame, attrs: pd.DataFrame, case_mix: pd.DataFrame) -> pd.DataFrame:
    years = pd.DataFrame({"year": np.arange(YEAR_MIN, YEAR_MAX + 1, dtype=int)})
    selected_firms = membership[["firm_id", "law_firm"]].drop_duplicates()

    balanced = selected_firms.assign(_key=1).merge(years.assign(_key=1), on="_key").drop(columns="_key")
    balanced = balanced.merge(attrs, on="firm_id", how="left", suffixes=("_selected", "_attr"))
    if "law_firm_selected" in balanced.columns and "law_firm_attr" in balanced.columns:
        balanced["law_firm"] = balanced["law_firm_selected"].fillna(balanced["law_firm_attr"])
        balanced = balanced.drop(columns=["law_firm_selected", "law_firm_attr"])
    elif "law_firm_selected" in balanced.columns:
        balanced = balanced.rename(columns={"law_firm_selected": "law_firm"})
    elif "law_firm_attr" in balanced.columns:
        balanced = balanced.rename(columns={"law_firm_attr": "law_firm"})

    balanced = balanced.merge(firm_year, on=["firm_id", "year"], how="left", suffixes=("", "_raw"))
    balanced["law_firm"] = balanced["law_firm"].fillna(balanced["law_firm_raw"])
    balanced = balanced.drop(columns=["law_firm_raw"])

    for col in ["firm_size", "firm_capital", "firm_birth_year", "first_contract_year"]:
        raw_col = f"{col}_raw"
        if raw_col in balanced.columns:
            balanced[col] = balanced[col].fillna(balanced[raw_col])
            balanced = balanced.drop(columns=[raw_col])

    for col in [
        "civil_case_n",
        "civil_win_n_binary",
        "civil_decisive_case_n",
        "civil_fee_decisive_case_n",
        "civil_win_rate_fee_sum",
        "avg_filing_to_hearing_days",
    ]:
        balanced[col] = balanced[col].fillna(0)

    balanced = balanced.merge(case_mix, on=["firm_id", "year"], how="left")
    balanced["civil_case_n_case"] = balanced["civil_case_n_case"].fillna(0).astype(int)
    balanced["enterprise_case_n"] = balanced["enterprise_case_n"].fillna(0).astype(int)
    balanced["personal_case_n"] = balanced["personal_case_n"].fillna(0).astype(int)

    birth_year = balanced["firm_birth_year"]
    pre_birth = birth_year.notna() & (balanced["year"] < birth_year)
    balanced.loc[pre_birth, "firm_size"] = 0

    for col in [
        "civil_case_n",
        "civil_win_n_binary",
        "civil_decisive_case_n",
        "civil_fee_decisive_case_n",
        "enterprise_case_n",
        "personal_case_n",
    ]:
        balanced[col] = balanced[col].fillna(0).astype(int)
    balanced["civil_win_rate_fee_sum"] = balanced["civil_win_rate_fee_sum"].fillna(0.0)

    use_case_total = balanced["civil_case_n_case"] > 0
    balanced.loc[use_case_total, "civil_case_n"] = balanced.loc[use_case_total, "civil_case_n_case"]
    balanced["civil_case_n"] = balanced[["civil_case_n", "civil_decisive_case_n", "civil_win_n_binary"]].max(axis=1)
    balanced["personal_case_n"] = np.maximum(0, balanced["civil_case_n"] - balanced["enterprise_case_n"])
    balanced = balanced.drop(columns=["civil_case_n_case"])

    balanced["civil_win_rate_mean"] = np.where(
        balanced["civil_decisive_case_n"] > 0,
        balanced["civil_win_n_binary"] / balanced["civil_decisive_case_n"],
        0.0,
    )
    balanced["civil_win_rate_fee_mean"] = np.where(
        balanced["civil_fee_decisive_case_n"] > 0,
        balanced["civil_win_rate_fee_sum"] / balanced["civil_fee_decisive_case_n"],
        np.nan,
    )
    balanced["avg_filing_to_hearing_days"] = np.where(
        balanced["civil_case_n"] > 0,
        balanced["avg_filing_to_hearing_days"],
        0.0,
    )

    return balanced


def build_final_panel() -> tuple[pd.DataFrame, pd.DataFrame]:
    lookup, preferred_display = build_name_lookup()
    case_mix = build_case_mix_by_firm_year(lookup)
    firm_year, attrs = build_raw_firm_year(preferred_display)
    membership, stack_summary = build_true_stack_membership(lookup, preferred_display)

    inferred_first_contract = stack_summary[["winner_firm_id", "first_contract_year_inferred"]].drop_duplicates()
    inferred_first_contract = inferred_first_contract.rename(columns={"winner_firm_id": "firm_id"})
    attrs = attrs.merge(inferred_first_contract, on="firm_id", how="left")
    attrs["first_contract_year"] = attrs["first_contract_year_inferred"].fillna(attrs["first_contract_year"]).fillna(0).astype(int)
    attrs = attrs.drop(columns=["first_contract_year_inferred"])

    balanced = build_balanced_panel(membership, firm_year, attrs, case_mix)

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

    baseline = final.loc[final["year"] == final["event_year"], ["stack_id", "firm_id", "firm_size"]].drop_duplicates()
    baseline = baseline.rename(columns={"firm_size": "firm_size_baseline"})
    final = final.merge(baseline, on=["stack_id", "firm_id"], how="left")
    final["firm_size_baseline"] = final["firm_size_baseline"].fillna(final["firm_size"])

    keep_cols = [
        "year",
        "law_firm",
        "firm_id",
        "firm_size",
        "stack_id",
        "approx_tender_id",
        "province",
        "city",
        "event_year",
        "event_month",
        "winner_firm",
        "treated_firm",
        "control_firm",
        "stack_control_target_n",
        "stack_control_firm_n",
        "stack_firm_n",
        "stack_control_balance_weight",
        "global_balance_weight",
        "event_time",
        "post_event",
        "did_treatment",
        "treatment",
        "first_contract_year",
        "already_treated_before_event",
        "not_yet_treated_at_event",
        "firm_size_baseline",
        "firm_capital",
        "firm_birth_year",
        "firm_age_at_event",
        "civil_case_n",
        "civil_win_n_binary",
        "civil_decisive_case_n",
        "civil_win_rate_mean",
        "civil_fee_decisive_case_n",
        "civil_win_rate_fee_mean",
        "avg_filing_to_hearing_days",
        "enterprise_case_n",
        "personal_case_n",
    ]
    final = final[keep_cols].sort_values(["stack_id", "firm_id", "year"]).reset_index(drop=True)
    return final, stack_summary


def write_summary(final: pd.DataFrame, stack_summary: pd.DataFrame) -> None:
    by_firm_year = final.groupby(["firm_id", "year"])
    civil_inconsistent = int((by_firm_year["civil_case_n"].nunique() > 1).sum())
    hearing_inconsistent = int((by_firm_year["avg_filing_to_hearing_days"].nunique() > 1).sum())
    enterprise_inconsistent = int((by_firm_year["enterprise_case_n"].nunique() > 1).sum())

    by_stack_year = final.groupby(["stack_id", "year"])
    treated_zero = int((by_stack_year["treated_firm"].sum() == 0).sum())
    short_rows = int((by_stack_year.size() < by_stack_year["stack_firm_n"].first()).sum())

    lines = [
        "# True Stack Firm-Level Summary (2026-04-16)",
        "",
        f"- File: `{OUT_FILE}`",
        f"- Rows: `{len(final)}`",
        f"- Stacks: `{final['stack_id'].nunique()}`",
        f"- Unique tenders: `{final['approx_tender_id'].nunique()}`",
        f"- Unique firms: `{final['firm_id'].nunique()}`",
        "",
        "## Stack construction",
        f"- True tender-level stacks recovered: `{stack_summary.shape[0]}`",
        f"- Unambiguous stacks: `{int((~stack_summary['potential_grouping_ambiguity']).sum())}`",
        f"- Multi-winner tender stacks: `{int(stack_summary['multi_winner_tender'].sum())}`",
        "",
        "## Structural checks",
        f"- `firm_id × year` with non-unique `civil_case_n`: `{civil_inconsistent}`",
        f"- `firm_id × year` with non-unique `avg_filing_to_hearing_days`: `{hearing_inconsistent}`",
        f"- `firm_id × year` with non-unique `enterprise_case_n`: `{enterprise_inconsistent}`",
        f"- Stack-years with zero treated firms: `{treated_zero}`",
        f"- Stack-years with fewer rows than `stack_firm_n`: `{short_rows}`",
        "",
    ]
    SUMMARY_FILE.write_text("\n".join(lines), encoding="utf-8")


def main() -> None:
    final, stack_summary = build_final_panel()
    final.to_csv(OUT_FILE, index=False)
    write_summary(final, stack_summary)
    print(f"Wrote {OUT_FILE}")
    print(f"Wrote {SUMMARY_FILE}")


if __name__ == "__main__":
    main()
