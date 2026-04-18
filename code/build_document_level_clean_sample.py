#!/usr/bin/env python3

from __future__ import annotations

import hashlib
import re
from pathlib import Path

import pandas as pd
import polars as pl
import pyreadstat


ROOT = Path(__file__).resolve().parents[1]
FIRM_FILE = ROOT / "data" / "output data" / "firm_level.csv"
CASE_FILE = ROOT / "data" / "output data" / "case_level.csv"
LITIGATION_SIDE_FILE = ROOT / "data" / "temp data" / "litigation_panels_full" / "litigation_case_side_dedup.parquet"
LAWYER_FILE = ROOT / "_archive" / "project_inputs" / "lawyer_list" / "lawyer_new" / "lawyers.dta"
OUT_DOC_FILE = ROOT / "data" / "output data" / "document_level_winner_vs_loser_clean.parquet"
OUT_DOC_CSV_FILE = ROOT / "data" / "output data" / "document_level_winner_vs_loser_clean.csv"
OUT_LAWYER_FILE = ROOT / "data" / "output data" / "document_level_random_lawyer_match.csv"
SUMMARY_FILE = ROOT / "data" / "output data" / "document_level_clean_summary_20260417.md"

RANDOM_SEED = "20260417"
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
FIRM_META_COLS = [
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
LAWYER_COLS = ["name", "id", "gender", "firm", "start_year", "ccp", "edu"]
CASE_BASE_COLS = [
    "year",
    "case_uid",
    "court",
    "cause",
    "law_firm",
    "winner_vs_runnerup_case",
    "side",
    "case_win_binary",
    "case_decisive",
    "opponent_has_lawyer",
    "plaintiff_party_is_entity",
    "defendant_party_is_entity",
    "case_win_rate_fee",
    "legal_reasoning_length_chars",
    "legal_reasoning_share",
]
DECISIVE_OVERRIDE_OUTCOME_CLASSES = ("plaintiff_win", "plaintiff_loss")


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


def stable_pick_index(firm_name: str, pool_size: int) -> int:
    key = f"{RANDOM_SEED}|{firm_name}".encode("utf-8")
    return int(hashlib.sha256(key).hexdigest(), 16) % pool_size


def load_current_firm_meta() -> pd.DataFrame:
    meta = pd.read_csv(FIRM_FILE, usecols=FIRM_META_COLS).drop_duplicates()
    nunique = (
        meta.groupby("law_firm")
        .agg(
            firm_id_n=("firm_id", "nunique"),
            stack_id_n=("stack_id", "nunique"),
            event_year_n=("event_year", "nunique"),
            treated_n=("treated_firm", "nunique"),
            control_n=("control_firm", "nunique"),
        )
        .reset_index()
    )
    ambiguous = nunique[
        (nunique["firm_id_n"] > 1)
        | (nunique["stack_id_n"] > 1)
        | (nunique["event_year_n"] > 1)
        | (nunique["treated_n"] > 1)
        | (nunique["control_n"] > 1)
    ].copy()
    if not ambiguous.empty:
        doc_firms = set(
            pl.scan_csv(CASE_FILE)
            .select(pl.col("law_firm").drop_nulls().unique())
            .collect()["law_firm"]
            .to_list()
        )
        ambiguous_in_docs = ambiguous.loc[ambiguous["law_firm"].isin(doc_firms)]
        if not ambiguous_in_docs.empty:
            raise ValueError(
                "Some document-matched law firms map to multiple stack/event combinations."
            )
    return meta.groupby("law_firm", as_index=False).first()


def load_lawyer_pool() -> pd.DataFrame:
    lawyers, _ = pyreadstat.read_dta(LAWYER_FILE, usecols=LAWYER_COLS)
    lawyers = lawyers.dropna(subset=["firm"]).copy()
    lawyers["lawyer_firm_key"] = lawyers["firm"].map(clean_name)
    lawyers["start_year"] = pd.to_numeric(lawyers["start_year"], errors="coerce")
    lawyers["ccp"] = pd.to_numeric(lawyers["ccp"], errors="coerce")
    lawyers = lawyers.sort_values(["lawyer_firm_key", "start_year", "name", "id"], na_position="last")
    return lawyers


def match_random_lawyer(firm_meta: pd.DataFrame, lawyers: pd.DataFrame) -> pd.DataFrame:
    lawyer_keys = set(lawyers["lawyer_firm_key"].dropna())
    out_rows: list[dict[str, object]] = []

    for row in firm_meta.itertuples(index=False):
        alias_hits = sorted(alias for alias in generate_aliases(row.law_firm) if alias in lawyer_keys)
        if alias_hits:
            pool = lawyers.loc[lawyers["lawyer_firm_key"].isin(alias_hits)].copy()
            eligible = pool.loc[pool["start_year"].le(row.event_year) | pool["start_year"].isna()].copy()
            if not eligible.empty:
                pool = eligible
            pool = pool.sort_values(["lawyer_firm_key", "start_year", "name", "id"], na_position="last").reset_index(drop=True)
            chosen = pool.iloc[stable_pick_index(str(row.law_firm), len(pool))]
            out_rows.append(
                {
                    "law_firm": row.law_firm,
                    "lawyer_found": 1,
                    "lawyer_alias_hits_n": len(alias_hits),
                    "lawyer_pool_n": int(len(pool)),
                    "lawyer_source_key": str(chosen["lawyer_firm_key"]),
                    "lawyer_name": chosen["name"],
                    "lawyer_gender": chosen["gender"],
                    "lawyer_start_year": chosen["start_year"],
                    "lawyer_ccp": chosen["ccp"],
                    "lawyer_edu": chosen["edu"],
                }
            )
        else:
            out_rows.append(
                {
                    "law_firm": row.law_firm,
                    "lawyer_found": 0,
                    "lawyer_alias_hits_n": 0,
                    "lawyer_pool_n": 0,
                    "lawyer_source_key": None,
                    "lawyer_name": None,
                    "lawyer_gender": None,
                    "lawyer_start_year": None,
                    "lawyer_ccp": None,
                    "lawyer_edu": None,
                }
            )

    return pd.DataFrame(out_rows)


def load_case_outcome_overrides() -> pl.LazyFrame:
    return (
        pl.scan_parquet(LITIGATION_SIDE_FILE)
        .select(["case_uid", "side", "outcome_class", "side_win"])
        .group_by(["case_uid", "side"])
        .agg(
            [
                pl.col("outcome_class").drop_nulls().first().alias("outcome_class"),
                pl.col("side_win").drop_nulls().first().cast(pl.Int8).alias("side_win"),
            ]
        )
        .with_columns(
            pl.col("outcome_class")
            .is_in(DECISIVE_OVERRIDE_OUTCOME_CLASSES)
            .fill_null(False)
            .alias("case_decisive_override")
        )
        .select(["case_uid", "side", "side_win", "case_decisive_override"])
    )


def build_case_sample(firm_meta: pd.DataFrame, lawyer_match: pd.DataFrame) -> pl.DataFrame:
    meta_pl = pl.from_pandas(firm_meta)
    lawyer_pl = pl.from_pandas(lawyer_match)
    outcome_pl = load_case_outcome_overrides()

    docs = (
        pl.scan_csv(CASE_FILE)
        .select(CASE_BASE_COLS)
        .join(outcome_pl, on=["case_uid", "side"], how="left")
        .join(meta_pl.lazy(), on="law_firm", how="inner")
        .filter(pl.col("winner_vs_runnerup_case") == 1)
        .join(lawyer_pl.lazy(), on="law_firm", how="left")
        .with_columns(
            [
                pl.when(pl.col("case_decisive_override"))
                .then(pl.lit(1))
                .otherwise(pl.col("case_decisive"))
                .cast(pl.Int8)
                .alias("case_decisive_clean"),
                pl.when(pl.col("treated_firm") == 1)
                .then(pl.lit("winner"))
                .otherwise(pl.lit("loser"))
                .alias("current_role"),
                (pl.col("year") >= pl.col("event_year")).cast(pl.Int8).alias("post"),
                (pl.col("year") - pl.col("event_year")).cast(pl.Int16).alias("event_time"),
                (pl.col("treated_firm") * (pl.col("year") >= pl.col("event_year")).cast(pl.Int8)).alias("did_treatment"),
                pl.col("legal_reasoning_length_chars").cast(pl.Float64).add(1).log().alias("log_legal_reasoning_length_chars"),
                (
                    pl.when(pl.col("lawyer_start_year").is_not_null())
                    .then((pl.col("year") - pl.col("lawyer_start_year")).clip(lower_bound=0))
                    .otherwise(None)
                )
                .cast(pl.Int16)
                .alias("lawyer_practice_years"),
                pl.when(pl.col("case_decisive_override") & pl.col("side_win").is_not_null())
                .then(pl.col("side_win"))
                .when(pl.col("case_decisive") == 1)
                .then(pl.col("case_win_binary"))
                .otherwise(None)
                .cast(pl.Int8)
                .alias("case_win_binary_clean"),
            ]
        )
        .collect()
    )
    # Enforce one law firm per document: keep one winner if the case contains any
    # current winner; otherwise keep one loser. Break remaining ties using a stable sort.
    docs = (
        docs.sort(["case_uid", "treated_firm", "law_firm", "firm_id"], descending=[False, True, False, False])
        .unique(subset=["case_uid"], keep="first", maintain_order=True)
    )
    docs = docs.select(
        [
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
            pl.col("case_win_binary_clean").alias("case_win_binary"),
            pl.col("case_decisive_clean").alias("case_decisive"),
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
    )
    return docs


def build_summary(docs: pl.DataFrame, lawyer_match_sample: pd.DataFrame) -> str:
    summary = docs.select(
        [
            pl.len().alias("rows"),
            pl.col("case_uid").n_unique().alias("case_uid"),
            pl.col("law_firm").n_unique().alias("law_firms"),
            pl.col("stack_id").n_unique().alias("stacks"),
            pl.col("court").n_unique().alias("courts"),
            pl.col("cause").n_unique().alias("causes"),
            pl.col("year").min().alias("year_min"),
            pl.col("year").max().alias("year_max"),
            pl.col("treated_firm").sum().alias("winner_rows"),
            pl.col("legal_reasoning_share").mean().alias("reason_share_mean"),
            pl.col("legal_reasoning_share").median().alias("reason_share_median"),
            pl.col("case_win_binary").mean().alias("case_win_mean"),
            pl.col("case_win_rate_fee").mean().alias("case_fee_winrate_mean"),
        ]
    ).to_pandas().iloc[0]
    treatment_counts = (
        docs.group_by(["treated_firm", "post"])
        .agg(pl.len().alias("n"))
        .sort(["treated_firm", "post"])
        .to_pandas()
    )
    lawyer_coverage = lawyer_match_sample["lawyer_found"].mean()
    opponent_counts = (
        docs.group_by("opponent_has_lawyer")
        .agg(pl.len().alias("n"))
        .sort("opponent_has_lawyer")
        .to_pandas()
    )

    lines = [
        "# Document-Level Clean Sample Summary",
        "",
        "Primary output:",
        f"- `{OUT_DOC_FILE.name}`",
        f"- `{OUT_DOC_CSV_FILE.name}`",
        f"- `{OUT_LAWYER_FILE.name}`",
        "",
        "Main sample design:",
        "- Current clean firms from `firm_level.csv` only",
        "- Restricted to `winner_vs_runnerup_case == 1` documents",
        "- Enforced one law firm per `case_uid`: keep one winner if present, otherwise keep one loser",
        "- Rebuilt `treated_firm`, `post`, `event_time`, and `did_treatment` from current firm stack metadata",
        "- Dropped old document-level treatment labels and unused carry-over fields from the delivered file",
        "",
        "Sample counts:",
        f"- Rows: `{int(summary['rows']):,}`",
        f"- Unique documents (`case_uid`): `{int(summary['case_uid']):,}`",
        f"- Winner-selected documents: `{int(summary['winner_rows']):,}`",
        f"- Loser-selected documents: `{int(summary['rows'] - summary['winner_rows']):,}`",
        f"- Unique law firms: `{int(summary['law_firms']):,}`",
        f"- Unique stacks: `{int(summary['stacks']):,}`",
        f"- Unique courts: `{int(summary['courts']):,}`",
        f"- Unique causes: `{int(summary['causes']):,}`",
        f"- Year range: `{int(summary['year_min'])}` to `{int(summary['year_max'])}`",
        "",
        "Outcome moments:",
        f"- Mean `legal_reasoning_share`: `{summary['reason_share_mean']:.4f}`",
        f"- Median `legal_reasoning_share`: `{summary['reason_share_median']:.4f}`",
        f"- Mean `case_win_binary`: `{summary['case_win_mean']:.4f}`",
        f"- Mean `case_win_rate_fee`: `{summary['case_fee_winrate_mean']:.4f}`",
        "",
        "Treatment/post cells:",
    ]
    for row in treatment_counts.itertuples(index=False):
        lines.append(
            f"- treated=`{int(row.treated_firm)}` post=`{int(row.post)}`: `{int(row.n):,}` rows"
        )
    lines.extend(
        [
            "",
            "Opponent lawyer coding within this primary sample:",
        ]
    )
    for row in opponent_counts.itertuples(index=False):
        lines.append(f"- `opponent_has_lawyer = {int(row.opponent_has_lawyer)}`: `{int(row.n):,}` rows")
    lines.extend(
        [
            "",
            "Random lawyer match coverage:",
            f"- Firms in delivered document sample: `{len(lawyer_match_sample):,}`",
            f"- Firms matched to at least one lawyer-list candidate: `{int(lawyer_match_sample['lawyer_found'].sum()):,}`",
            f"- Coverage share: `{lawyer_coverage:.4f}`",
            "",
            "Variable note:",
            "- `lawyer_practice_years = max(year - lawyer_start_year, 0)`",
            "- Rows flagged upstream as `plaintiff_win` or `plaintiff_loss` are forced into `case_decisive = 1` and receive `case_win_binary = side_win` from the litigation case-side panel",
            "- Outside that override, `case_win_binary` is set to missing whenever `case_decisive = 0`",
            "- `case_win_rate_fee` comes from SQL `shoulifeiyuangaobizhong`, transformed into the represented side's fee-based win-rate measure",
            "- Age is not available in the current lawyer list; the usable lawyer attributes are gender, party membership (`ccp`), education, and entry year",
            "- In document-level DID, `firm FE` will absorb time-invariant lawyer attributes. They are better used for heterogeneity or interacted designs than as standalone main-effect controls",
            "- `law_firm × year FE` should not be used in the main DID because it would absorb the treatment indicator, which varies only at the firm-year level",
        ]
    )
    return "\n".join(lines) + "\n"


def main() -> None:
    firm_meta = load_current_firm_meta()
    lawyers = load_lawyer_pool()
    lawyer_match = match_random_lawyer(firm_meta, lawyers)
    docs = build_case_sample(firm_meta, lawyer_match)
    doc_firms = docs.select("law_firm").unique().to_pandas()["law_firm"]
    lawyer_match_sample = lawyer_match.loc[lawyer_match["law_firm"].isin(set(doc_firms))].copy()

    docs.write_parquet(OUT_DOC_FILE, compression="zstd")
    docs.write_csv(OUT_DOC_CSV_FILE)
    lawyer_match_sample.sort_values(["lawyer_found", "law_firm"], ascending=[False, True]).to_csv(
        OUT_LAWYER_FILE, index=False
    )
    SUMMARY_FILE.write_text(build_summary(docs, lawyer_match_sample), encoding="utf-8")

    print(f"Wrote {OUT_DOC_FILE}")
    print(f"Wrote {OUT_DOC_CSV_FILE}")
    print(f"Wrote {OUT_LAWYER_FILE}")
    print(f"Wrote {SUMMARY_FILE}")


if __name__ == "__main__":
    main()
