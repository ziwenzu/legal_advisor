#!/usr/bin/env python3

from __future__ import annotations

import argparse
import json
import re
from collections import Counter, defaultdict
from pathlib import Path

import numpy as np
import pandas as pd
import pyreadstat


ROOT = Path(__file__).resolve().parents[1]
DOC_FILE = ROOT / "data" / "output data" / "document_level_winner_vs_loser_clean.parquet"
FIRM_PANEL_FILE = ROOT / "data" / "temp data" / "litigation_panels_full" / "law_firm_year_panel_merged.parquet"
ADMIN_FILE = ROOT / "_archive" / "project_inputs" / "admin_cases" / "combined_data.dta"
OUT_FILE = ROOT / "data" / "output data" / "document_level_winner_vs_loser_ddd.parquet"
OUT_CSV_FILE = ROOT / "data" / "output data" / "document_level_winner_vs_loser_ddd.csv"
OUT_EXPOSURE_FILE = ROOT / "data" / "output data" / "admin_gov_firm_court_year_exposure.parquet"
OUT_PAIR_AUDIT_FILE = ROOT / "data" / "output data" / "admin_gov_exposure_pre_civil_pair_audit.parquet"
SUMMARY_FILE = ROOT / "data" / "output data" / "document_level_ddd_summary_20260417.md"

DEFAULT_CHUNKSIZE = 200_000
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
SPECIAL_REGION_PREFIXES = (
    "内蒙古自治区",
    "广西壮族自治区",
    "西藏自治区",
    "宁夏回族自治区",
    "新疆维吾尔自治区",
    "香港特别行政区",
    "澳门特别行政区",
    "新疆生产建设兵团",
)
COURT_LOCATION_TOKEN = re.compile(
    r"(北京市|天津市|上海市|重庆市|内蒙古自治区|广西壮族自治区|西藏自治区|宁夏回族自治区|新疆维吾尔自治区|"
    r"香港特别行政区|澳门特别行政区|新疆生产建设兵团|"
    r"[\u4e00-\u9fff]{2,9}省|[\u4e00-\u9fff]{2,9}市|[\u4e00-\u9fff]{2,9}自治州|[\u4e00-\u9fff]{2,9}地区|"
    r"[\u4e00-\u9fff]{2,9}盟|[\u4e00-\u9fff]{2,9}区|[\u4e00-\u9fff]{2,9}县|[\u4e00-\u9fff]{2,9}旗)"
)
COURT_MATCH = re.compile(r"[\u4e00-\u9fffA-Za-z0-9()（）·]+?法院")
FIRM_SEPARATOR = re.compile(r"\s*\|\|\s*|[，,、;；/]+")


def clean_name(name: object) -> str:
    text = "" if pd.isna(name) else str(name).strip()
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
    for prefix in LEADING_NOISE:
        if text.startswith(prefix) and len(text) > len(prefix) + 4:
            text = text[len(prefix) :]
    if text.startswith("北京市") and not text.startswith("北京市("):
        text = "北京" + text[len("北京市") :]
    if text.startswith("上海市") and not text.startswith("上海市("):
        text = "上海" + text[len("上海市") :]
    return text


def generate_aliases(name: object) -> set[str]:
    base = clean_name(name)
    aliases = {base}

    match = re.match(r"^(.*)\((.+)\)律师事务所$", base)
    if match:
        stem, branch = match.groups()
        aliases.add(f"{stem}{branch}律师事务所")
        aliases.add(f"{stem}律师事务所{branch}分所")
        aliases.add(f"{stem}{branch}分所律师事务所")

    match = re.match(r"^(.*)律师事务所(.+?)分所$", base)
    if match:
        stem, branch = match.groups()
        aliases.add(f"{stem}({branch})律师事务所")

    match = re.match(r"^(.*?)([\u4e00-\u9fff]{2,4})律师事务所$", base)
    if match and "(" not in base:
        stem, branch = match.groups()
        aliases.add(f"{stem}({branch})律师事务所")

    if base.startswith("北京") and not base.startswith("北京市"):
        aliases.add("北京市" + base[len("北京") :])
    if base.startswith("上海") and not base.startswith("上海市"):
        aliases.add("上海市" + base[len("上海") :])

    return {alias for alias in aliases if alias}


def split_firm_names(raw: object) -> list[str]:
    if pd.isna(raw):
        return []
    text = str(raw).strip()
    if not text:
        return []
    out: list[str] = []
    for piece in FIRM_SEPARATOR.split(text):
        cleaned = clean_name(piece)
        if not cleaned:
            continue
        if "律师" not in cleaned and "法律服务" not in cleaned and "援助中心" not in cleaned:
            continue
        if len(cleaned) < 4:
            continue
        out.append(cleaned)
    return list(dict.fromkeys(out))


def clean_court_name(raw: object) -> str | None:
    text = "" if pd.isna(raw) else str(raw).strip()
    if not text:
        return None
    text = text.replace("（", "(").replace("）", ")")
    text = re.sub(r"\s+", "", text)
    text = text.replace("中华人民共和国", "")
    matches = COURT_MATCH.findall(text)
    if not matches:
        return None
    court = matches[-1]
    court = re.sub(r"^[^\u4e00-\u9fff]+", "", court)
    loc_match = COURT_LOCATION_TOKEN.search(court)
    if loc_match:
        court = court[loc_match.start() :]
    court = court.strip()
    if not court or len(court) < 4:
        return None
    return court


def court_match_key(raw: object) -> str | None:
    court = clean_court_name(raw)
    if not court:
        return None
    court = court.replace("第一审人民法院", "人民法院")
    for prefix in SPECIAL_REGION_PREFIXES:
        if court.startswith(prefix):
            court = court[len(prefix) :]
            break
    court = re.sub(r"^[\u4e00-\u9fff]{2,9}省", "", court)
    return court or None


def load_document_panel() -> pd.DataFrame:
    doc = pd.read_parquet(DOC_FILE).copy()
    doc["court_clean"] = doc["court"].map(clean_court_name)
    doc["court_match_key"] = doc["court_clean"].map(court_match_key)
    return doc


def build_alias_map(doc: pd.DataFrame) -> tuple[dict[str, str], int]:
    firm_map = doc[["law_firm", "firm_id"]].drop_duplicates()
    firm_id_n = firm_map.groupby("law_firm")["firm_id"].nunique()
    ambiguous = firm_id_n.loc[firm_id_n > 1]
    if not ambiguous.empty:
        raise ValueError(
            "Document sample contains law_firm labels with multiple firm_id values: "
            + ", ".join(ambiguous.index[:10].tolist())
        )

    alias_to_ids: dict[str, set[str]] = defaultdict(set)
    for row in firm_map.itertuples(index=False):
        for alias in generate_aliases(row.law_firm):
            alias_to_ids[alias].add(row.firm_id)

    unique_alias_map = {
        alias: next(iter(firm_ids))
        for alias, firm_ids in alias_to_ids.items()
        if len(firm_ids) == 1
    }
    ambiguous_alias_n = sum(1 for firm_ids in alias_to_ids.values() if len(firm_ids) > 1)
    return unique_alias_map, ambiguous_alias_n


def load_first_contract_year(doc: pd.DataFrame) -> pd.DataFrame:
    firm_year = pd.read_parquet(FIRM_PANEL_FILE, columns=["firm_id", "first_contract_year"]).drop_duplicates()
    firm_year = firm_year.groupby("firm_id", as_index=False)["first_contract_year"].min()

    treated_event_year = (
        doc.loc[doc["treated_firm"] == 1, ["firm_id", "event_year"]]
        .dropna()
        .groupby("firm_id", as_index=False)["event_year"]
        .min()
        .rename(columns={"event_year": "fallback_event_year"})
    )
    firm_year = firm_year.merge(treated_event_year, on="firm_id", how="outer")
    firm_year["first_contract_year"] = firm_year["first_contract_year"].combine_first(firm_year["fallback_event_year"])
    firm_year = firm_year.drop(columns=["fallback_event_year"])
    return firm_year


def build_case_id(series: pd.Series, chunk_index: int) -> pd.Series:
    raw = series.astype("string").str.strip()
    missing = raw.isna() | raw.eq("")
    raw = raw.fillna("")
    if missing.any():
        replacement = [f"missing_{chunk_index}_{int(i)}" for i in raw.index[missing]]
        raw.loc[missing] = replacement
    return raw.astype(str)


def process_admin_cases(
    alias_map: dict[str, str],
    contract_year_map: dict[str, int],
    chunksize: int,
    max_chunks: int | None,
) -> tuple[pd.DataFrame, Counter]:
    stats: Counter = Counter()
    grouped_chunks: list[pd.DataFrame] = []
    usecols = ["wenshuhao", "panjueriqi", "fayuan_std", "beigaobianhurenlvsuo"]

    reader = pyreadstat.read_file_in_chunks(
        pyreadstat.read_dta,
        str(ADMIN_FILE),
        chunksize=chunksize,
        usecols=usecols,
    )

    for chunk_index, (chunk, _) in enumerate(reader, start=1):
        if max_chunks is not None and chunk_index > max_chunks:
            break

        stats["chunks_read"] += 1
        stats["admin_rows_raw"] += len(chunk)

        chunk = chunk.rename(
            columns={
                "wenshuhao": "case_id",
                "panjueriqi": "judgment_date",
                "fayuan_std": "court_raw",
                "beigaobianhurenlvsuo": "gov_rep_firm_raw",
            }
        )
        chunk["case_id"] = build_case_id(chunk["case_id"], chunk_index)
        chunk["admin_year"] = pd.to_datetime(chunk["judgment_date"], errors="coerce").dt.year
        chunk["court_clean"] = chunk["court_raw"].map(clean_court_name)
        chunk["court_match_key"] = chunk["court_clean"].map(court_match_key)
        chunk["gov_rep_firms"] = chunk["gov_rep_firm_raw"].map(split_firm_names)

        chunk = chunk.loc[
            chunk["admin_year"].notna()
            & chunk["court_match_key"].notna()
            & chunk["gov_rep_firms"].map(bool)
        ].copy()
        stats["admin_rows_with_usable_court_and_counsel"] += len(chunk)
        if chunk.empty:
            continue

        exploded = chunk[["case_id", "admin_year", "court_match_key", "gov_rep_firms"]].explode("gov_rep_firms")
        exploded = exploded.rename(columns={"gov_rep_firms": "firm_name"})
        stats["admin_firm_mentions"] += len(exploded)

        exploded["firm_id"] = exploded["firm_name"].map(alias_map.get)
        matched = exploded.loc[exploded["firm_id"].notna()].copy()
        stats["matched_firm_mentions"] += len(matched)
        if matched.empty:
            continue

        matched["first_contract_year"] = matched["firm_id"].map(contract_year_map)
        matched = matched.loc[matched["first_contract_year"].notna()].copy()
        matched["admin_year"] = matched["admin_year"].astype(int)
        matched["first_contract_year"] = matched["first_contract_year"].astype(int)
        matched = matched.loc[matched["admin_year"] >= matched["first_contract_year"]].copy()
        stats["matched_post_contract_mentions"] += len(matched)
        if matched.empty:
            continue

        grouped = (
            matched.groupby(["firm_id", "court_match_key", "admin_year"], as_index=False)
            .agg(
                admin_gov_case_n_year=("case_id", "nunique"),
                admin_gov_mention_n_year=("case_id", "size"),
            )
            .sort_values(["firm_id", "court_match_key", "admin_year"])
        )
        grouped_chunks.append(grouped)

    if not grouped_chunks:
        empty = pd.DataFrame(
            columns=[
                "firm_id",
                "court_match_key",
                "year",
                "admin_gov_case_n_year",
                "admin_gov_mention_n_year",
            ]
        )
        return empty, stats

    exposure = pd.concat(grouped_chunks, ignore_index=True)
    exposure = (
        exposure.groupby(["firm_id", "court_match_key", "admin_year"], as_index=False)
        .agg(
            admin_gov_case_n_year=("admin_gov_case_n_year", "sum"),
            admin_gov_mention_n_year=("admin_gov_mention_n_year", "sum"),
        )
        .rename(columns={"admin_year": "year"})
        .sort_values(["firm_id", "court_match_key", "year"])
        .reset_index(drop=True)
    )
    return exposure, stats


def build_exposure_panel(doc: pd.DataFrame, exposure_yearly: pd.DataFrame) -> pd.DataFrame:
    panel = doc[["firm_id", "court_match_key", "year"]].drop_duplicates().copy()
    panel = panel.loc[panel["court_match_key"].notna()].copy()

    panel = panel.merge(
        exposure_yearly,
        on=["firm_id", "court_match_key", "year"],
        how="left",
    )
    panel["admin_gov_case_n_year"] = panel["admin_gov_case_n_year"].fillna(0).astype(int)
    panel["admin_gov_mention_n_year"] = panel["admin_gov_mention_n_year"].fillna(0).astype(int)

    panel = panel.sort_values(["firm_id", "court_match_key", "year"]).reset_index(drop=True)
    group_keys = ["firm_id", "court_match_key"]
    panel["same_or_prior_admin_gov_case_n"] = panel.groupby(group_keys)["admin_gov_case_n_year"].cumsum()
    panel["same_or_prior_admin_gov_mention_n"] = panel.groupby(group_keys)["admin_gov_mention_n_year"].cumsum()
    panel["prior_admin_gov_case_n"] = panel["same_or_prior_admin_gov_case_n"] - panel["admin_gov_case_n_year"]
    panel["prior_admin_gov_mention_n"] = panel["same_or_prior_admin_gov_mention_n"] - panel["admin_gov_mention_n_year"]
    panel["prior_admin_gov_exposure"] = (panel["prior_admin_gov_case_n"] > 0).astype(int)
    panel["same_or_prior_admin_gov_exposure"] = (panel["same_or_prior_admin_gov_case_n"] > 0).astype(int)

    first_year = (
        exposure_yearly.groupby(group_keys, as_index=False)["year"]
        .min()
        .rename(columns={"year": "first_admin_gov_exposure_year"})
    )
    panel = panel.merge(first_year, on=group_keys, how="left")
    panel["years_since_first_admin_gov_exposure"] = np.where(
        panel["first_admin_gov_exposure_year"].notna(),
        panel["year"] - panel["first_admin_gov_exposure_year"],
        np.nan,
    )
    return panel


def build_pre_exposure_civil_audit(doc: pd.DataFrame, exposure_yearly: pd.DataFrame) -> pd.DataFrame:
    group_keys = ["firm_id", "court_match_key"]
    if exposure_yearly.empty:
        return pd.DataFrame(
            columns=[
                "firm_id",
                "court_match_key",
                "first_admin_gov_exposure_year",
                "first_doc_civil_year_in_court",
                "last_doc_civil_year_in_court",
                "doc_civil_case_n_total_in_court",
                "pre_admin_civil_case_n_in_court",
                "same_year_admin_civil_case_n_in_court",
                "post_admin_civil_case_n_in_court",
                "has_pre_admin_civil_case_in_court",
                "has_same_year_admin_civil_case_in_court",
                "has_post_admin_civil_case_in_court",
            ]
        )

    first_admin = (
        exposure_yearly.groupby(group_keys, as_index=False)["year"]
        .min()
        .rename(columns={"year": "first_admin_gov_exposure_year"})
    )
    work = doc.loc[doc["court_match_key"].notna(), ["firm_id", "court_match_key", "year", "case_uid"]].copy()
    pair_year = (
        work.groupby(group_keys + ["year"], as_index=False)
        .agg(doc_civil_case_n_year=("case_uid", "nunique"))
    )
    pair_total = (
        work.groupby(group_keys, as_index=False)
        .agg(
            first_doc_civil_year_in_court=("year", "min"),
            last_doc_civil_year_in_court=("year", "max"),
            doc_civil_case_n_total_in_court=("case_uid", "nunique"),
        )
    )

    audit = first_admin.merge(pair_total, on=group_keys, how="left").merge(pair_year, on=group_keys, how="left")
    audit["pre_admin_civil_case_n_in_court"] = (
        audit["doc_civil_case_n_year"].where(audit["year"] < audit["first_admin_gov_exposure_year"], 0).fillna(0)
    )
    audit["same_year_admin_civil_case_n_in_court"] = (
        audit["doc_civil_case_n_year"].where(audit["year"] == audit["first_admin_gov_exposure_year"], 0).fillna(0)
    )
    audit["post_admin_civil_case_n_in_court"] = (
        audit["doc_civil_case_n_year"].where(audit["year"] > audit["first_admin_gov_exposure_year"], 0).fillna(0)
    )

    pair_audit = (
        audit.groupby(
            [
                "firm_id",
                "court_match_key",
                "first_admin_gov_exposure_year",
                "first_doc_civil_year_in_court",
                "last_doc_civil_year_in_court",
                "doc_civil_case_n_total_in_court",
            ],
            as_index=False,
        )[
            [
                "pre_admin_civil_case_n_in_court",
                "same_year_admin_civil_case_n_in_court",
                "post_admin_civil_case_n_in_court",
            ]
        ]
        .sum()
    )
    pair_audit["has_pre_admin_civil_case_in_court"] = (pair_audit["pre_admin_civil_case_n_in_court"] > 0).astype(int)
    pair_audit["has_same_year_admin_civil_case_in_court"] = (
        pair_audit["same_year_admin_civil_case_n_in_court"] > 0
    ).astype(int)
    pair_audit["has_post_admin_civil_case_in_court"] = (pair_audit["post_admin_civil_case_n_in_court"] > 0).astype(int)
    return pair_audit


def merge_document_panel(
    doc: pd.DataFrame,
    exposure_panel: pd.DataFrame,
    first_contract_year: pd.DataFrame,
    pair_audit: pd.DataFrame,
) -> pd.DataFrame:
    doc = doc.drop(
        columns=[
            col
            for col in [
                "first_contract_year",
                "first_admin_gov_exposure_year",
                "first_admin_gov_exposure_year_x",
                "first_admin_gov_exposure_year_y",
            ]
            if col in doc.columns
        ],
        errors="ignore",
    )
    pair_audit = pair_audit.drop(columns=["first_admin_gov_exposure_year"], errors="ignore")
    out = doc.merge(first_contract_year, on="firm_id", how="left")
    out = out.merge(
        exposure_panel,
        on=["firm_id", "court_match_key", "year"],
        how="left",
    )
    out = out.merge(
        pair_audit,
        on=["firm_id", "court_match_key"],
        how="left",
    )

    fill_zero_cols = [
        "admin_gov_case_n_year",
        "admin_gov_mention_n_year",
        "same_or_prior_admin_gov_case_n",
        "same_or_prior_admin_gov_mention_n",
        "prior_admin_gov_case_n",
        "prior_admin_gov_mention_n",
        "prior_admin_gov_exposure",
        "same_or_prior_admin_gov_exposure",
        "pre_admin_civil_case_n_in_court",
        "same_year_admin_civil_case_n_in_court",
        "post_admin_civil_case_n_in_court",
        "has_pre_admin_civil_case_in_court",
        "has_same_year_admin_civil_case_in_court",
        "has_post_admin_civil_case_in_court",
    ]
    for col in fill_zero_cols:
        out[col] = out[col].fillna(0)
        if col.endswith("_n") or col.endswith("_year") or col.endswith("_exposure") or col.startswith("has_"):
            out[col] = out[col].astype(int)

    out["has_court_match_key"] = out["court_match_key"].notna().astype(int)
    return out


def build_summary(
    doc: pd.DataFrame,
    final_panel: pd.DataFrame,
    exposure_yearly: pd.DataFrame,
    pair_audit: pd.DataFrame,
    stats: Counter,
    ambiguous_alias_n: int,
) -> str:
    treated_post = final_panel.loc[final_panel["did_treatment"] == 1]
    control_post = final_panel.loc[(final_panel["post"] == 1) & (final_panel["treated_firm"] == 0)]
    exposed_pairs = pair_audit.copy()
    preexisting_share = (
        exposed_pairs["has_pre_admin_civil_case_in_court"].mean() if not exposed_pairs.empty else float("nan")
    )
    preexisting_median = (
        exposed_pairs.loc[exposed_pairs["has_pre_admin_civil_case_in_court"] == 1, "pre_admin_civil_case_n_in_court"].median()
        if not exposed_pairs.empty
        else float("nan")
    )

    lines = [
        "# Document-level DDD Sample Summary",
        "",
        "## Coverage",
        f"- Document rows in clean input: `{len(doc):,}`",
        f"- Document rows with usable `court_match_key`: `{int(final_panel['has_court_match_key'].sum()):,}`",
        f"- Unique document firms: `{final_panel['firm_id'].nunique():,}`",
        f"- Unique document courts after cleaning: `{final_panel.loc[final_panel['court_match_key'].notna(), 'court_match_key'].nunique():,}`",
        "",
        "## Alias Matching",
        f"- Unique clean document law-firm labels: `{doc['law_firm'].nunique():,}`",
        f"- Ambiguous generated aliases dropped: `{ambiguous_alias_n:,}`",
        "",
        "## Administrative Government-Representation Extraction",
        f"- Admin chunks read: `{stats['chunks_read']:,}`",
        f"- Raw admin rows scanned: `{stats['admin_rows_raw']:,}`",
        f"- Rows with usable court and defendant counsel fields: `{stats['admin_rows_with_usable_court_and_counsel']:,}`",
        f"- Defendant-counsel firm mentions extracted: `{stats['admin_firm_mentions']:,}`",
        f"- Mentions matched to document-sample firms: `{stats['matched_firm_mentions']:,}`",
        f"- Mentions that occur in or after the firm's first contract year: `{stats['matched_post_contract_mentions']:,}`",
        f"- Unique `firm x court x year` exposure cells: `{len(exposure_yearly):,}`",
        "",
        "## Exposure in Final DDD Sample",
        f"- Rows with `prior_admin_gov_exposure = 1`: `{int(final_panel['prior_admin_gov_exposure'].sum()):,}`",
        f"- Rows with `same_or_prior_admin_gov_exposure = 1`: `{int(final_panel['same_or_prior_admin_gov_exposure'].sum()):,}`",
        f"- Treated-post rows: `{len(treated_post):,}`",
        f"- Treated-post rows with prior exposure: `{int(treated_post['prior_admin_gov_exposure'].sum()):,}`",
        f"- Control-post rows: `{len(control_post):,}`",
        f"- Control-post rows with prior exposure: `{int(control_post['prior_admin_gov_exposure'].sum()):,}`",
        "",
        "## Court-Pair Prehistory Check",
        f"- Exposed `firm x court` pairs in document sample: `{len(exposed_pairs):,}`",
        f"- Share with any pre-exposure civil case in that same court: `{preexisting_share:.4f}`",
        f"- Median pre-exposure civil cases among pairs with any prehistory: `{preexisting_median:.1f}`",
        f"- Exposed pairs with no pre-exposure civil case in that same court: `{int((exposed_pairs['has_pre_admin_civil_case_in_court'] == 0).sum()):,}`",
        "",
        "## Construction Notes",
        "- `prior_admin_gov_exposure` is court-specific and only turns on when the same `firm_id` previously appeared as defendant-side counsel in an administrative case at that same cleaned court key.",
        "- The default timing is conservative: only prior years count. Same-year administrative appearances are stored separately in `admin_gov_case_n_year` and `same_or_prior_admin_gov_exposure`.",
        "- Exposure is restricted to administrative appearances that happen in or after `first_contract_year`, so it is designed to proxy post-procurement recognition rather than preexisting familiarity.",
        "- Pair-level audit columns (`has_pre_admin_civil_case_in_court`, `pre_admin_civil_case_n_in_court`) let us restrict the mechanism to courts where the firm already had civil business before its first observed government-side administrative appearance there.",
    ]
    return "\n".join(lines) + "\n"


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Build the document-level DDD sample with prior admin government exposure.")
    parser.add_argument("--chunksize", type=int, default=DEFAULT_CHUNKSIZE, help="Rows per admin-case chunk.")
    parser.add_argument("--max-chunks", type=int, default=None, help="Optional chunk cap for a quick test run.")
    parser.add_argument("--skip-csv", action="store_true", help="Skip writing the CSV output.")
    return parser.parse_args()


def main() -> None:
    args = parse_args()

    doc = load_document_panel()
    alias_map, ambiguous_alias_n = build_alias_map(doc)
    first_contract_year = load_first_contract_year(doc)
    contract_year_map = (
        first_contract_year.dropna(subset=["first_contract_year"])
        .assign(first_contract_year=lambda d: d["first_contract_year"].astype(int))
        .set_index("firm_id")["first_contract_year"]
        .to_dict()
    )

    exposure_yearly, stats = process_admin_cases(
        alias_map=alias_map,
        contract_year_map=contract_year_map,
        chunksize=args.chunksize,
        max_chunks=args.max_chunks,
    )
    exposure_panel = build_exposure_panel(doc, exposure_yearly)
    pair_audit = build_pre_exposure_civil_audit(doc, exposure_yearly)
    final_panel = merge_document_panel(doc, exposure_panel, first_contract_year, pair_audit)

    exposure_yearly.to_parquet(OUT_EXPOSURE_FILE, index=False)
    pair_audit.to_parquet(OUT_PAIR_AUDIT_FILE, index=False)
    final_panel.to_parquet(OUT_FILE, index=False)
    if not args.skip_csv:
        final_panel.to_csv(OUT_CSV_FILE, index=False)

    SUMMARY_FILE.write_text(
        build_summary(
            doc=doc,
            final_panel=final_panel,
            exposure_yearly=exposure_yearly,
            pair_audit=pair_audit,
            stats=stats,
            ambiguous_alias_n=ambiguous_alias_n,
        ),
        encoding="utf-8",
    )

    print(
        json.dumps(
            {
                "document_rows": int(len(final_panel)),
                "exposure_cells": int(len(exposure_yearly)),
                "prior_exposure_rows": int(final_panel["prior_admin_gov_exposure"].sum()),
                "output_file": str(OUT_FILE),
                "summary_file": str(SUMMARY_FILE),
            },
            ensure_ascii=False,
            indent=2,
        )
    )


if __name__ == "__main__":
    main()
