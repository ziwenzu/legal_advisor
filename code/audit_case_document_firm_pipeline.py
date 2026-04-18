#!/usr/bin/env python3

from __future__ import annotations

from pathlib import Path

import numpy as np
import pandas as pd


ROOT = Path(__file__).resolve().parents[1]
CASE_FILE = ROOT / "data" / "output data" / "case_level.parquet"
DOC_FILE = ROOT / "data" / "output data" / "document_level_winner_vs_loser_clean.parquet"
DDD_FILE = ROOT / "data" / "output data" / "document_level_winner_vs_loser_ddd.parquet"
FIRM_FILE = ROOT / "data" / "output data" / "firm_level.csv"
CITY_FILE = ROOT / "data" / "output data" / "city_year_panel.csv"
OUT_FILE = ROOT / "data" / "output data" / "case_document_firm_pipeline_audit_20260417.md"


def fmt_int(x: float | int) -> str:
    return f"{int(round(x)):,}"


def fmt_num(x: float, digits: int = 3) -> str:
    if pd.isna(x):
        return "NA"
    return f"{x:.{digits}f}"


def describe_series(series: pd.Series) -> dict[str, float]:
    s = pd.to_numeric(series, errors="coerce")
    return {
        "nonmissing": int(s.notna().sum()),
        "mean": float(s.mean()) if s.notna().any() else np.nan,
        "sd": float(s.std()) if s.notna().any() else np.nan,
        "min": float(s.min()) if s.notna().any() else np.nan,
        "p50": float(s.quantile(0.5)) if s.notna().any() else np.nan,
        "p90": float(s.quantile(0.9)) if s.notna().any() else np.nan,
        "max": float(s.max()) if s.notna().any() else np.nan,
    }


def bullet_stats(name: str, stats: dict[str, float], digits: int = 3) -> str:
    return (
        f"- `{name}`: nonmissing `{fmt_int(stats['nonmissing'])}`, "
        f"mean `{fmt_num(stats['mean'], digits)}`, sd `{fmt_num(stats['sd'], digits)}`, "
        f"min `{fmt_num(stats['min'], digits)}`, p50 `{fmt_num(stats['p50'], digits)}`, "
        f"p90 `{fmt_num(stats['p90'], digits)}`, max `{fmt_num(stats['max'], digits)}`"
    )


def main() -> None:
    case = pd.read_parquet(
        CASE_FILE,
        columns=["case_uid", "winner_vs_runnerup_case", "law_firm"],
    )
    doc = pd.read_parquet(DOC_FILE)
    ddd = pd.read_parquet(DDD_FILE)
    firm = pd.read_csv(FIRM_FILE)
    city = pd.read_csv(CITY_FILE)

    case_wvr = case.loc[case["winner_vs_runnerup_case"] == 1].copy()
    doc_case_ids = set(doc["case_uid"].dropna().astype(str))
    case_wvr_ids = set(case_wvr["case_uid"].dropna().astype(str))
    missing_from_doc = len(case_wvr_ids - doc_case_ids)

    doc_rows_per_uid = doc.groupby("case_uid").size()

    doc_total = len(doc)
    doc_decisive_total = pd.to_numeric(doc["case_decisive"], errors="coerce").fillna(0).sum()
    firm_total = firm["civil_case_n"].sum()
    firm_decisive_total = firm["civil_decisive_case_n"].sum()

    lines: list[str] = [
        "# Analysis Data Structure Audit",
        "",
        "## Core analysis files",
        "- `city_year_panel.csv` is the city-year administrative panel.",
        "- `document_level_winner_vs_loser_clean.parquet` is the one-document-one-firm civil litigation sample.",
        "- `firm_level.csv` is the `stack_id × firm_id × year` aggregation of that document-level sample.",
        "- `document_level_winner_vs_loser_ddd.parquet` is not a separate core universe; it is the document-level file plus the DDD mechanism variables.",
        "",
        "## How the three core files relate",
        "- `city_year_panel` is separate from the civil litigation panels. It is used for the city-year administrative analysis and is not a collapse of the document-level civil sample.",
        "- `document_level_winner_vs_loser_clean` is built from `case_level.parquet` after restricting to `winner_vs_runnerup_case == 1`, keeping only clean current firms, and enforcing one law firm per case.",
        "- `firm_level.csv` is an exact aggregation of `document_level_winner_vs_loser_clean` by `stack_id × firm_id × year`.",
        "- `document_level_winner_vs_loser_ddd` keeps the same rows as the clean document sample and only adds court-specific DDD variables.",
        "",
        "## Upstream source-to-sample mapping",
        f"- Unique `case_uid` in `case_level` with `winner_vs_runnerup_case == 1`: `{fmt_int(case_wvr['case_uid'].nunique())}`",
        f"- Unique `case_uid` in `document_level`: `{fmt_int(doc['case_uid'].nunique())}`",
        f"- Winner-vs-runner-up cases missing from `document_level`: `{fmt_int(missing_from_doc)}`",
        "- Those missing cases are cases in the broader winner-vs-runner-up source universe that do not map to the current clean firm sample.",
        "",
        "## Dataset units and sample definitions",
        f"- `city_year_panel`: `{fmt_int(len(city))}` rows, `{fmt_int(city[['province', 'city']].drop_duplicates().shape[0])}` unique cities, years `{fmt_int(city['year'].min())}` to `{fmt_int(city['year'].max())}`.",
        f"- `document_level`: `{fmt_int(len(doc))}` rows and `{fmt_int(doc['case_uid'].nunique())}` unique cases.",
        f"- `document_level` rows per `case_uid` greater than 1: `{fmt_int((doc_rows_per_uid > 1).sum())}`.",
        f"- `firm_level`: `{fmt_int(len(firm))}` rows and `{fmt_int(firm[['stack_id', 'firm_id', 'year']].drop_duplicates().shape[0])}` unique `stack_id × firm_id × year` cells.",
        f"- `DDD` extension rows: `{fmt_int(len(ddd))}`.",
        "",
        "## Exact identities across the civil analysis files",
        f"- `sum(document_level rows)` = `{fmt_int(doc_total)}`",
        f"- `sum(firm_level.civil_case_n)` = `{fmt_int(firm_total)}`",
        f"- Raw case-count identity holds: `{np.isclose(doc_total, firm_total, rtol=0, atol=1e-6)}`",
        f"- `sum(document_level.case_decisive)` = `{fmt_int(doc_decisive_total)}`",
        f"- `sum(firm_level.civil_decisive_case_n)` = `{fmt_int(firm_decisive_total)}`",
        f"- Decisive-case identity holds: `{np.isclose(doc_decisive_total, firm_decisive_total, rtol=0, atol=1e-6)}`",
        f"- `DDD` row count equals `document_level` row count: `{len(ddd) == len(doc)}`",
        "",
        "## Saved columns in each file",
        f"- `city_year_panel`: `{', '.join(city.columns.tolist())}`",
        f"- `document_level`: `{', '.join(doc.columns.tolist())}`",
        f"- `firm_level`: `{', '.join(firm.columns.tolist())}`",
        f"- `DDD` extension: `{', '.join(ddd.columns.tolist())}`",
        "",
        "## Variable definitions",
        "- `document_level.case_decisive = 1` means the case is coded into a clean binary win/loss outcome.",
        "- `document_level.case_win_binary` is only defined on decisive cases.",
        "- `document_level.case_win_rate_fee` is the represented side's fee-based win-rate measure from the SQL filing-fee allocation field.",
        "- `firm_level.civil_case_n` is the raw number of winner-vs-runner-up document-level cases in that `stack_id × firm_id × year` cell.",
        "- `firm_level.civil_decisive_case_n` is the raw number of decisive document-level cases in that cell.",
        "- `firm_level.civil_win_rate_mean = civil_win_n_binary / civil_decisive_case_n` when the denominator is positive.",
        "- `firm_level.civil_win_rate_fee_mean` is the firm-year mean of `case_win_rate_fee` across decisive cases with observed fee share.",
        "- `DDD.prior_admin_gov_exposure = 1` means the same `firm_id × court_match_key` had already appeared as defendant-side government counsel in an administrative case before that year.",
        "- `DDD.has_pre_admin_civil_case_in_court = 1` means the same `firm_id × court_match_key` had civil business in that court before its first observed government-side administrative appearance there.",
        "",
        "## Selected ranges",
        bullet_stats("document_level.event_time", describe_series(doc["event_time"])),
        bullet_stats("document_level.case_win_binary", describe_series(doc["case_win_binary"])),
        bullet_stats("document_level.case_win_rate_fee", describe_series(doc["case_win_rate_fee"])),
        bullet_stats("firm_level.civil_case_n", describe_series(firm["civil_case_n"])),
        bullet_stats("firm_level.civil_win_rate_mean", describe_series(firm["civil_win_rate_mean"])),
        bullet_stats("firm_level.avg_filing_to_hearing_days", describe_series(firm["avg_filing_to_hearing_days"])),
        bullet_stats("firm_level.civil_win_rate_fee_mean", describe_series(firm["civil_win_rate_fee_mean"])),
        bullet_stats("DDD.prior_admin_gov_exposure", describe_series(ddd["prior_admin_gov_exposure"])),
        bullet_stats("DDD.has_pre_admin_civil_case_in_court", describe_series(ddd["has_pre_admin_civil_case_in_court"])),
        bullet_stats("city_year_panel.admin_case_n", describe_series(city["admin_case_n"])),
        bullet_stats("city_year_panel.government_win_rate", describe_series(city["government_win_rate"])),
        bullet_stats("city_year_panel.appeal_rate", describe_series(city["appeal_rate"])),
        "",
        "## Interpretation",
        "- The civil DID and DDD analyses live on the raw winner-vs-runner-up litigation sample.",
        "- The correct firm-level quantity outcome is therefore `firm_level.civil_case_n`, because it is the exact aggregation of the civil analytical sample rather than a broader market-size proxy.",
        "- The DDD file should be understood as a thin extension of `document_level`, not as a fourth core dataset with a different sample definition.",
        "",
    ]

    OUT_FILE.write_text("\n".join(lines) + "\n", encoding="utf-8")
    print(f"Wrote {OUT_FILE}")


if __name__ == "__main__":
    main()
