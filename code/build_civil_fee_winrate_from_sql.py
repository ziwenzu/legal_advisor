#!/usr/bin/env python3

from __future__ import annotations

import json
import math
import subprocess
from dataclasses import dataclass
from pathlib import Path

import pandas as pd
import polars as pl

from build_firm_level_true_stack import build_name_lookup, map_name


ROOT = Path(__file__).resolve().parents[1]
CASE_PARQUET_FILE = ROOT / "data" / "output data" / "case_level.parquet"
CASE_CSV_FILE = ROOT / "data" / "output data" / "case_level.csv"
FIRM_FILE = ROOT / "data" / "output data" / "firm_level.csv"
FIRM_TRUE_STACK_FILE = ROOT / "data" / "output data" / "firm_level_true_stack.csv"
OUT_MAPPING_FILE = ROOT / "data" / "output data" / "civil_case_fee_winrate_sql_mapping.parquet"
SUMMARY_FILE = ROOT / "data" / "output data" / "civil_case_fee_winrate_sql_summary_20260417.md"
TEMP_DIR = ROOT / "data" / "temp data" / "civil_fee_winrate_sql"

MYSQL_BIN = "/opt/homebrew/opt/mysql-client@8.4/bin/mysql"
MYSQL_LOGIN_PATH = "tencent_mysql"
MYSQL_DB = "bilibili"
YEAR_MIN = 2010
YEAR_MAX = 2020
HISTOGRAM_BINS = [0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0]


def normalize_case_no(value: object) -> str | None:
    if value is None or (isinstance(value, float) and math.isnan(value)):
        return None
    text = str(value).strip()
    if not text:
        return None
    text = text.replace(" ", "").replace("　", "")
    text = text.replace("（", "(").replace("）", ")")
    return text or None


def normalize_case_no_expr(column: str) -> pl.Expr:
    return (
        pl.col(column)
        .cast(pl.Utf8)
        .str.strip_chars()
        .str.replace_all(" ", "")
        .str.replace_all("　", "")
        .str.replace_all("（", "(")
        .str.replace_all("）", ")")
    )


def predict_binary(plaintiff_fee_share: float, side: str) -> int:
    if side == "plaintiff":
        return int((1.0 - plaintiff_fee_share) >= 0.5)
    if side == "defendant":
        return int(plaintiff_fee_share >= 0.5)
    raise ValueError(f"Unexpected side value: {side}")


@dataclass
class SqlYearStats:
    year: int
    sql_rows: int = 0
    nonnull_share_rows: int = 0
    matched_rows: int = 0
    exact_zero_rows: int = 0
    exact_half_rows: int = 0
    exact_one_rows: int = 0
    below_zero_rows: int = 0
    above_one_rows: int = 0
    min_share: float | None = None
    max_share: float | None = None
    sum_share: float = 0.0
    histogram: dict[str, int] | None = None

    def __post_init__(self) -> None:
        if self.histogram is None:
            self.histogram = {
                "[0.0,0.1)": 0,
                "[0.1,0.2)": 0,
                "[0.2,0.3)": 0,
                "[0.3,0.4)": 0,
                "[0.4,0.5)": 0,
                "[0.5,0.6)": 0,
                "[0.6,0.7)": 0,
                "[0.7,0.8)": 0,
                "[0.8,0.9)": 0,
                "[0.9,1.0)": 0,
                "{1.0}": 0,
            }

    def update_share(self, share: float) -> None:
        self.nonnull_share_rows += 1
        self.sum_share += share
        self.min_share = share if self.min_share is None else min(self.min_share, share)
        self.max_share = share if self.max_share is None else max(self.max_share, share)

        if share < 0:
            self.below_zero_rows += 1
            return
        if share > 1:
            self.above_one_rows += 1
            return

        if share == 0:
            self.exact_zero_rows += 1
        if share == 0.5:
            self.exact_half_rows += 1
        if share == 1:
            self.exact_one_rows += 1

        if share == 1:
            self.histogram["{1.0}"] += 1
            return

        for left, right in zip(HISTOGRAM_BINS[:-1], HISTOGRAM_BINS[1:]):
            if left <= share < right:
                self.histogram[f"[{left:.1f},{right:.1f})"] += 1
                return

    @property
    def mean_share(self) -> float | None:
        if self.nonnull_share_rows == 0:
            return None
        return self.sum_share / self.nonnull_share_rows

    def to_dict(self) -> dict[str, object]:
        return {
            "year": self.year,
            "sql_rows": self.sql_rows,
            "nonnull_share_rows": self.nonnull_share_rows,
            "matched_rows": self.matched_rows,
            "exact_zero_rows": self.exact_zero_rows,
            "exact_half_rows": self.exact_half_rows,
            "exact_one_rows": self.exact_one_rows,
            "below_zero_rows": self.below_zero_rows,
            "above_one_rows": self.above_one_rows,
            "min_share": self.min_share,
            "max_share": self.max_share,
            "mean_share": self.mean_share,
            "histogram": self.histogram,
        }


def load_case_keys() -> tuple[pd.DataFrame, pd.DataFrame]:
    case_key_df = (
        pl.scan_parquet(CASE_PARQUET_FILE)
        .select(
            [
                pl.col("year").cast(pl.Int16),
                normalize_case_no_expr("case_no").alias("case_no_key"),
            ]
        )
        .filter(pl.col("case_no_key").is_not_null() & (pl.col("case_no_key") != ""))
        .unique()
        .collect()
        .to_pandas()
    )

    decisive_binary_df = (
        pl.scan_parquet(CASE_PARQUET_FILE)
        .select(
            [
                pl.col("year").cast(pl.Int16),
                normalize_case_no_expr("case_no").alias("case_no_key"),
                pl.col("side").cast(pl.Utf8),
                pl.col("case_win_binary").cast(pl.Int8),
                pl.col("case_decisive").cast(pl.Int8),
            ]
        )
        .filter(
            pl.col("case_no_key").is_not_null()
            & (pl.col("case_no_key") != "")
            & (pl.col("case_decisive") == 1)
            & pl.col("case_win_binary").is_not_null()
        )
        .unique()
        .collect()
        .to_pandas()
    )

    decisive_binary_df["year"] = decisive_binary_df["year"].astype(int)
    case_key_df["year"] = case_key_df["year"].astype(int)
    return case_key_df, decisive_binary_df


def mysql_query_for_year(year: int) -> str:
    return f"""
SELECT
  REPLACE(
    REPLACE(
      REPLACE(
        REPLACE(
          REPLACE(
            REPLACE(COALESCE(wenshuhao, ''), CHAR(13), ''),
            CHAR(10), ''
          ),
          ' ', ''
        ),
        '　', ''
      ),
      '（', '('
    ),
    '）', ')'
  ) AS case_no_key,
  shoulifeiyuangaobizhong
FROM ws_mscf_result_{year}
WHERE wenshuhao IS NOT NULL;
""".strip()


def stream_sql_year(year: int, target_keys: set[str]) -> tuple[SqlYearStats, Path]:
    TEMP_DIR.mkdir(parents=True, exist_ok=True)
    matched_path = TEMP_DIR / f"matched_fee_rows_{year}.tsv"
    stats_path = TEMP_DIR / f"sql_distribution_{year}.json"
    stats = SqlYearStats(year=year)

    query = mysql_query_for_year(year)
    cmd = [
        MYSQL_BIN,
        f"--login-path={MYSQL_LOGIN_PATH}",
        "-D",
        MYSQL_DB,
        "--batch",
        "--raw",
        "--skip-column-names",
        "--quick",
        "-e",
        query,
    ]

    with subprocess.Popen(
        cmd,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        encoding="utf-8",
        errors="replace",
        bufsize=1,
    ) as proc, matched_path.open("w", encoding="utf-8") as matched_fh:
        assert proc.stdout is not None
        for line in proc.stdout:
            line = line.rstrip("\n")
            if not line:
                continue
            parts = line.split("\t", 1)
            case_no_key = parts[0]
            share_text = parts[1] if len(parts) > 1 else ""
            stats.sql_rows += 1

            if share_text != "":
                try:
                    share = float(share_text)
                    stats.update_share(share)
                except ValueError:
                    pass

            if case_no_key in target_keys:
                matched_fh.write(f"{case_no_key}\t{share_text}\n")
                stats.matched_rows += 1

        stderr_text = proc.stderr.read() if proc.stderr is not None else ""
        return_code = proc.wait()
        if return_code != 0:
            raise RuntimeError(
                f"MySQL extraction failed for {year} with return code {return_code}: {stderr_text.strip()}"
            )

    stats_path.write_text(json.dumps(stats.to_dict(), ensure_ascii=False, indent=2), encoding="utf-8")
    return stats, matched_path


def choose_share_from_conflict(
    options_df: pd.DataFrame,
    decisive_rows: pd.DataFrame | None,
) -> tuple[float | None, str, int | None, int | None]:
    if options_df.empty:
        return None, "no_valid_share", None, None

    ranked = options_df.sort_values(
        ["share_row_n", "plaintiff_fee_share_sql"],
        ascending=[False, True],
        kind="mergesort",
    ).copy()

    if decisive_rows is None or decisive_rows.empty:
        top = ranked.iloc[0]
        return float(top["plaintiff_fee_share_sql"]), "mode_no_binary_anchor", None, None

    total_checks = int(len(decisive_rows))
    scored_rows: list[dict[str, object]] = []
    for _, option in ranked.iterrows():
        share = float(option["plaintiff_fee_share_sql"])
        matches = 0
        for _, decisive_row in decisive_rows.iterrows():
            predicted = predict_binary(share, str(decisive_row["side"]))
            if predicted == int(decisive_row["case_win_binary"]):
                matches += 1
        scored_rows.append(
            {
                "share": share,
                "share_row_n": int(option["share_row_n"]),
                "matches": matches,
                "compatible": int(matches == total_checks),
            }
        )

    scored_df = pd.DataFrame(scored_rows).sort_values(
        ["compatible", "matches", "share_row_n", "share"],
        ascending=[False, False, False, True],
        kind="mergesort",
    )
    top = scored_df.iloc[0]
    rule = "binary_consistent_tie_break" if int(top["compatible"]) == 1 else "mode_best_binary_match"
    return float(top["share"]), rule, int(top["matches"]), total_checks


def aggregate_year_matches(
    year: int,
    matched_path: Path,
    decisive_binary_df: pd.DataFrame,
) -> pd.DataFrame:
    raw = pd.read_csv(
        matched_path,
        sep="\t",
        names=["case_no_key", "plaintiff_fee_share_sql_raw"],
        dtype={"case_no_key": "string", "plaintiff_fee_share_sql_raw": "string"},
        keep_default_na=False,
    )
    if raw.empty:
        return pd.DataFrame(
            columns=[
                "year",
                "case_no_key",
                "sql_row_n",
                "valid_share_row_n",
                "distinct_valid_share_n",
                "plaintiff_fee_share_sql",
                "plaintiff_fee_share_min",
                "plaintiff_fee_share_max",
                "selection_rule",
                "binary_match_n",
                "binary_check_n",
            ]
        )

    raw["plaintiff_fee_share_sql"] = pd.to_numeric(raw["plaintiff_fee_share_sql_raw"], errors="coerce")
    raw = raw.drop(columns=["plaintiff_fee_share_sql_raw"])

    share_counts = (
        raw.groupby(["case_no_key", "plaintiff_fee_share_sql"], dropna=False)
        .size()
        .reset_index(name="share_row_n")
    )
    valid = share_counts.loc[
        share_counts["plaintiff_fee_share_sql"].between(0.0, 1.0, inclusive="both")
    ].copy()

    total_rows = share_counts.groupby("case_no_key", as_index=False)["share_row_n"].sum().rename(
        columns={"share_row_n": "sql_row_n"}
    )
    if valid.empty:
        out = total_rows.copy()
        out["year"] = year
        out["valid_share_row_n"] = 0
        out["distinct_valid_share_n"] = 0
        out["plaintiff_fee_share_sql"] = pd.NA
        out["plaintiff_fee_share_min"] = pd.NA
        out["plaintiff_fee_share_max"] = pd.NA
        out["selection_rule"] = "no_valid_share"
        out["binary_match_n"] = pd.NA
        out["binary_check_n"] = pd.NA
        return out

    valid_rows = valid.groupby("case_no_key", as_index=False)["share_row_n"].sum().rename(
        columns={"share_row_n": "valid_share_row_n"}
    )
    distinct_counts = valid.groupby("case_no_key", as_index=False)["plaintiff_fee_share_sql"].nunique().rename(
        columns={"plaintiff_fee_share_sql": "distinct_valid_share_n"}
    )
    share_range = valid.groupby("case_no_key", as_index=False)["plaintiff_fee_share_sql"].agg(
        plaintiff_fee_share_min="min",
        plaintiff_fee_share_max="max",
    )
    mode_df = (
        valid.sort_values(
            ["case_no_key", "share_row_n", "plaintiff_fee_share_sql"],
            ascending=[True, False, True],
            kind="mergesort",
        )
        .drop_duplicates("case_no_key", keep="first")
        .rename(columns={"plaintiff_fee_share_sql": "mode_share"})
        [["case_no_key", "mode_share", "share_row_n"]]
        .rename(columns={"share_row_n": "mode_share_row_n"})
    )

    out = (
        total_rows.merge(valid_rows, on="case_no_key", how="left")
        .merge(distinct_counts, on="case_no_key", how="left")
        .merge(share_range, on="case_no_key", how="left")
        .merge(mode_df, on="case_no_key", how="left")
    )
    out["valid_share_row_n"] = out["valid_share_row_n"].fillna(0).astype(int)
    out["distinct_valid_share_n"] = out["distinct_valid_share_n"].fillna(0).astype(int)
    out["plaintiff_fee_share_sql"] = out["mode_share"]
    out["selection_rule"] = "mode_single_valid_share"
    out["binary_match_n"] = pd.NA
    out["binary_check_n"] = pd.NA

    conflict_keys = out.loc[out["distinct_valid_share_n"] > 1, "case_no_key"].tolist()
    if conflict_keys:
        decisive_year = decisive_binary_df.loc[decisive_binary_df["year"] == year].copy()
        decisive_map = {
            case_no_key: grp[["side", "case_win_binary"]].drop_duplicates()
            for case_no_key, grp in decisive_year.groupby("case_no_key", sort=False)
        }
        option_map = {
            case_no_key: grp[["plaintiff_fee_share_sql", "share_row_n"]].sort_values(
                ["share_row_n", "plaintiff_fee_share_sql"],
                ascending=[False, True],
                kind="mergesort",
            )
            for case_no_key, grp in valid.loc[valid["case_no_key"].isin(conflict_keys)].groupby("case_no_key", sort=False)
        }

        for case_no_key in conflict_keys:
            selected_share, rule, match_n, check_n = choose_share_from_conflict(
                options_df=option_map[case_no_key],
                decisive_rows=decisive_map.get(case_no_key),
            )
            mask = out["case_no_key"] == case_no_key
            out.loc[mask, "plaintiff_fee_share_sql"] = selected_share
            out.loc[mask, "selection_rule"] = rule
            out.loc[mask, "binary_match_n"] = match_n
            out.loc[mask, "binary_check_n"] = check_n

    out["year"] = year
    out = out[
        [
            "year",
            "case_no_key",
            "sql_row_n",
            "valid_share_row_n",
            "distinct_valid_share_n",
            "plaintiff_fee_share_sql",
            "plaintiff_fee_share_min",
            "plaintiff_fee_share_max",
            "selection_rule",
            "binary_match_n",
            "binary_check_n",
        ]
    ].copy()
    return out


def build_fee_mapping(case_keys_df: pd.DataFrame, decisive_binary_df: pd.DataFrame) -> tuple[pd.DataFrame, list[dict[str, object]]]:
    mapping_frames: list[pd.DataFrame] = []
    stats_rows: list[dict[str, object]] = []

    for year in range(YEAR_MIN, YEAR_MAX + 1):
        target_keys = set(case_keys_df.loc[case_keys_df["year"] == year, "case_no_key"].dropna())
        if not target_keys:
            continue
        stats, matched_path = stream_sql_year(year, target_keys)
        year_map = aggregate_year_matches(year, matched_path, decisive_binary_df)
        year_map_path = TEMP_DIR / f"civil_fee_mapping_{year}.parquet"
        pl.from_pandas(year_map).write_parquet(year_map_path, compression="zstd")
        matched_path.unlink(missing_ok=True)
        mapping_frames.append(year_map)
        stats_rows.append(stats.to_dict())

    if not mapping_frames:
        raise RuntimeError("No yearly fee-share mappings were produced from SQL.")

    mapping_df = pd.concat(mapping_frames, ignore_index=True)
    mapping_df = mapping_df.sort_values(["year", "case_no_key"], kind="mergesort").reset_index(drop=True)
    return mapping_df, stats_rows


def augment_case_level(mapping_df: pd.DataFrame) -> dict[str, float]:
    mapping_pl = pl.from_pandas(
        mapping_df[["year", "case_no_key", "plaintiff_fee_share_sql"]].copy()
    )

    case_lazy = (
        pl.scan_parquet(CASE_PARQUET_FILE)
        .with_columns(normalize_case_no_expr("case_no").alias("case_no_key"))
        .join(mapping_pl.lazy(), on=["year", "case_no_key"], how="left")
        .with_columns(
            [
                pl.when(pl.col("side") == "plaintiff")
                .then(pl.col("plaintiff_fee_share_sql"))
                .when(pl.col("side") == "defendant")
                .then(1 - pl.col("plaintiff_fee_share_sql"))
                .otherwise(None)
                .cast(pl.Float64)
                .alias("case_fee_burden_share"),
                pl.when(pl.col("side") == "plaintiff")
                .then(1 - pl.col("plaintiff_fee_share_sql"))
                .when(pl.col("side") == "defendant")
                .then(pl.col("plaintiff_fee_share_sql"))
                .otherwise(None)
                .cast(pl.Float64)
                .alias("case_win_rate_fee"),
            ]
        )
        .drop("case_no_key")
    )

    parquet_tmp = CASE_PARQUET_FILE.with_suffix(".parquet.tmp")
    csv_tmp = CASE_CSV_FILE.with_suffix(".csv.tmp")
    case_lazy.sink_parquet(parquet_tmp)
    case_lazy.sink_csv(csv_tmp)
    parquet_tmp.replace(CASE_PARQUET_FILE)
    csv_tmp.replace(CASE_CSV_FILE)

    audit_df = (
        pl.scan_parquet(CASE_PARQUET_FILE)
        .select(
            [
                pl.len().alias("rows"),
                pl.col("case_win_rate_fee").is_not_null().sum().alias("rows_with_fee_winrate"),
                (
                    (
                        (pl.col("case_decisive") == 1)
                        & pl.col("case_win_binary").is_not_null()
                        & pl.col("case_win_rate_fee").is_not_null()
                        & ((pl.col("case_win_rate_fee") >= 0.5).cast(pl.Int8) == pl.col("case_win_binary"))
                    )
                )
                .sum()
                .alias("binary_consistent_rows"),
                (
                    (pl.col("case_decisive") == 1)
                    & pl.col("case_win_binary").is_not_null()
                    & pl.col("case_win_rate_fee").is_not_null()
                )
                .sum()
                .alias("binary_checked_rows"),
                pl.col("case_win_rate_fee").mean().alias("mean_fee_winrate"),
                pl.col("case_win_rate_fee").median().alias("median_fee_winrate"),
            ]
        )
        .collect()
        .to_dicts()[0]
    )
    return audit_df


def build_firm_fee_panel(case_file: Path) -> pd.DataFrame:
    lookup, _ = build_name_lookup()
    usecols = ["law_firm", "year", "case_decisive", "case_win_rate_fee"]
    pieces: list[pd.DataFrame] = []

    for chunk in pd.read_csv(case_file, usecols=usecols, chunksize=1_000_000):
        mapped = chunk["law_firm"].map(lambda x: map_name(x, lookup)[0])
        chunk = chunk.loc[mapped.notna()].copy()
        chunk["firm_id"] = mapped.loc[chunk.index]
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
                civil_fee_decisive_case_n=("fee_winrate_available", "sum"),
                civil_win_rate_fee_sum=("fee_winrate_sum", "sum"),
            )
        )
        pieces.append(agg)

    if not pieces:
        return pd.DataFrame(columns=["firm_id", "year", "civil_fee_decisive_case_n", "civil_win_rate_fee_mean"])

    panel = pd.concat(pieces, ignore_index=True)
    panel = (
        panel.groupby(["firm_id", "year"], as_index=False)
        .agg(
            civil_fee_decisive_case_n=("civil_fee_decisive_case_n", "sum"),
            civil_win_rate_fee_sum=("civil_win_rate_fee_sum", "sum"),
        )
    )
    panel["civil_win_rate_fee_mean"] = panel["civil_win_rate_fee_sum"] / panel["civil_fee_decisive_case_n"]
    panel.loc[panel["civil_fee_decisive_case_n"] <= 0, "civil_win_rate_fee_mean"] = pd.NA
    panel = panel.drop(columns=["civil_win_rate_fee_sum"])
    return panel


def augment_firm_panel(file_path: Path, fee_panel: pd.DataFrame) -> None:
    if not file_path.exists():
        return
    df = pd.read_csv(file_path)
    merged = df.merge(fee_panel, on=["firm_id", "year"], how="left")
    if "civil_fee_decisive_case_n" in merged.columns:
        merged["civil_fee_decisive_case_n"] = pd.to_numeric(
            merged["civil_fee_decisive_case_n"], errors="coerce"
        ).fillna(0).astype(int)
    if "civil_win_rate_fee_mean" in merged.columns:
        merged["civil_win_rate_fee_mean"] = pd.to_numeric(
            merged["civil_win_rate_fee_mean"], errors="coerce"
        )
    merged.to_csv(file_path, index=False)


def write_summary(
    mapping_df: pd.DataFrame,
    sql_stats_rows: list[dict[str, object]],
    case_audit: dict[str, float],
    case_keys_df: pd.DataFrame,
) -> None:
    sql_stats_df = pd.DataFrame(sql_stats_rows).sort_values("year")
    mapping_nonmissing = mapping_df["plaintiff_fee_share_sql"].notna().sum()
    conflict_n = int((mapping_df["distinct_valid_share_n"] > 1).sum())
    compatible_conflict_n = int(
        mapping_df["selection_rule"].eq("binary_consistent_tie_break").sum()
    )
    target_case_n = len(case_keys_df)

    checked_rows = int(case_audit["binary_checked_rows"])
    consistent_rows = int(case_audit["binary_consistent_rows"])
    consistency_rate = consistent_rows / checked_rows if checked_rows else float("nan")

    lines = [
        "# Civil Fee Win-Rate Construction from SQL",
        "",
        "Primary outputs:",
        f"- `{CASE_PARQUET_FILE.name}` and `{CASE_CSV_FILE.name}` augmented with `plaintiff_fee_share_sql`, `case_fee_burden_share`, and `case_win_rate_fee`",
        f"- `{FIRM_FILE.name}` augmented with `civil_fee_decisive_case_n` and `civil_win_rate_fee_mean`",
        f"- `{FIRM_TRUE_STACK_FILE.name}` augmented with `civil_fee_decisive_case_n` and `civil_win_rate_fee_mean`",
        f"- `{OUT_MAPPING_FILE.name}`",
        "",
        "Matching summary:",
        f"- Unique `(year, case_no)` keys in current `case_level`: `{target_case_n:,}`",
        f"- Keys with a selected SQL fee-share value: `{int(mapping_nonmissing):,}`",
        f"- Cases with more than one valid SQL fee-share candidate: `{conflict_n:,}`",
        f"- Conflict cases resolved by full binary consistency: `{compatible_conflict_n:,}`",
        "",
        "Constructed case-level fee outcome:",
        f"- Rows with non-missing `case_win_rate_fee`: `{int(case_audit['rows_with_fee_winrate']):,}`",
        f"- Mean `case_win_rate_fee`: `{case_audit['mean_fee_winrate']:.4f}`",
        f"- Median `case_win_rate_fee`: `{case_audit['median_fee_winrate']:.4f}`",
        f"- Decisive rows checked against current `case_win_binary`: `{checked_rows:,}`",
        f"- Consistency rate for `1[case_win_rate_fee >= 0.5]`: `{consistency_rate:.4f}`",
        "",
        "SQL-wide plaintiff fee-share distribution by year:",
    ]

    for row in sql_stats_df.itertuples(index=False):
        lines.extend(
            [
                f"- `{int(row.year)}`: non-missing rows `{int(row.nonnull_share_rows):,}`, mean `{row.mean_share:.4f}`"
                if row.mean_share is not None
                else f"- `{int(row.year)}`: non-missing rows `0`",
                f"  - exact `0`: `{int(row.exact_zero_rows):,}`; exact `0.5`: `{int(row.exact_half_rows):,}`; exact `1`: `{int(row.exact_one_rows):,}`",
                f"  - out of range: `<0` = `{int(row.below_zero_rows):,}`, `>1` = `{int(row.above_one_rows):,}`",
            ]
        )

    SUMMARY_FILE.write_text("\n".join(lines) + "\n", encoding="utf-8")


def main() -> None:
    case_keys_df, decisive_binary_df = load_case_keys()
    mapping_df, sql_stats_rows = build_fee_mapping(case_keys_df, decisive_binary_df)
    pl.from_pandas(mapping_df).write_parquet(OUT_MAPPING_FILE, compression="zstd")

    case_audit = augment_case_level(mapping_df)
    fee_panel = build_firm_fee_panel(CASE_CSV_FILE)
    augment_firm_panel(FIRM_FILE, fee_panel)
    augment_firm_panel(FIRM_TRUE_STACK_FILE, fee_panel)
    write_summary(mapping_df, sql_stats_rows, case_audit, case_keys_df)

    print(f"Wrote {OUT_MAPPING_FILE}")
    print(f"Updated {CASE_PARQUET_FILE}")
    print(f"Updated {CASE_CSV_FILE}")
    print(f"Updated {FIRM_FILE}")
    if FIRM_TRUE_STACK_FILE.exists():
        print(f"Updated {FIRM_TRUE_STACK_FILE}")
    print(f"Wrote {SUMMARY_FILE}")


if __name__ == "__main__":
    main()
