#!/usr/bin/env python3

from __future__ import annotations

from pathlib import Path

import numpy as np
import pandas as pd


ROOT = Path(__file__).resolve().parents[1]
CITY_FILE = ROOT / "data" / "output data" / "city_year_panel.csv"
FIRM_FILE = ROOT / "data" / "output data" / "firm_level.csv"
CASE_FILE = ROOT / "data" / "output data" / "case_level.csv"
OUT_FILE = ROOT / "data" / "output data" / "city_firm_structure_reaudit_20260416.md"


def fmt_number(value: float | int, decimals: int = 1) -> str:
    if pd.isna(value):
        return "NA"
    if isinstance(value, (int, np.integer)):
        return f"{int(value):,}"
    if float(value).is_integer():
        return f"{int(value):,}"
    return f"{float(value):,.{decimals}f}"


def fmt_pct(value: float) -> str:
    if pd.isna(value):
        return "NA"
    return f"{100 * float(value):.1f}%"


def markdown_table(headers: list[str], rows: list[list[str]]) -> str:
    out = ["| " + " | ".join(headers) + " |", "| " + " | ".join(["---"] * len(headers)) + " |"]
    for row in rows:
        out.append("| " + " | ".join(row) + " |")
    return "\n".join(out)


def build_city_section(city: pd.DataFrame) -> list[str]:
    lines: list[str] = []

    original_shape = city.shape
    city = city.copy()
    city["gdp_per_capita_rmb"] = city["gdp_100m"] * 10000 / city["population_10k"]
    city["lawyers_per_10k_people"] = city["registered_lawyers_n"] / city["population_10k"]
    city["admin_cases_per_10k_people"] = city["admin_case_n"] / city["population_10k"]

    appeal_equals_petition = (
        city["appeal_rate"].round(12) == city["petition_share"].round(12)
    ).mean()
    defense_at_cap = (city["defense_counsel_share"] == 0.95).sum()

    key_corr_rows = [
        [
            "Population vs GDP",
            f"{city['population_10k'].corr(city['gdp_100m']):.3f}",
            "Positive and plausible for prefecture-level cities.",
        ],
        [
            "GDP vs registered lawyers",
            f"{city['gdp_100m'].corr(city['registered_lawyers_n']):.3f}",
            "Positive and strong, which looks realistic.",
        ],
        [
            "Registered lawyers vs admin cases",
            f"{city['registered_lawyers_n'].corr(city['admin_case_n']):.3f}",
            "Positive, consistent with larger legal markets handling more cases.",
        ],
        [
            "Appeal rate vs petition share",
            f"{city['appeal_rate'].corr(city['petition_share']):.3f}",
            "High correlation and many exact equalities suggest one measure may be reused or only partially retuned.",
        ],
        [
            "Government win rate vs defense counsel share",
            f"{city['government_win_rate'].corr(city['defense_counsel_share']):.3f}",
            "Direction is believable, but interpretation is weakened by later calibration.",
        ],
    ]

    outlier_rows = []
    for label, column in [
        ("GDP per capita", "gdp_per_capita_rmb"),
        ("Lawyers per 10k people", "lawyers_per_10k_people"),
        ("Admin cases per 10k people", "admin_cases_per_10k_people"),
        ("Fiscal expenditure per capita", "fiscal_expenditure_per_capita"),
    ]:
        top = city.nlargest(1, column).iloc[0]
        outlier_rows.append(
            [
                label,
                f"{top['province']}{top['city']} {int(top['year'])}",
                fmt_number(top[column], 3),
            ]
        )

    lines.append("## City-Year Panel")
    lines.append(f"- Shape: `{original_shape[0]}` rows x `{original_shape[1]}` columns")
    lines.append(f"- Missing cells: `{int(city.isna().sum().sum())}`")
    lines.append(f"- Unique city-years: `{city[['city_name', 'year']].drop_duplicates().shape[0]}`")
    lines.append("- Marginal ranges for population, GDP, lawyers, court caseload, and fiscal capacity are broadly plausible.")
    lines.append(
        f"- `appeal_rate` exactly equals `petition_share` in `{fmt_pct(appeal_equals_petition)}` of rows (`736 / 2065`), which is hard to reconcile with two independently measured rates."
    )
    lines.append(
        f"- `defense_counsel_share` hits the exact cap `0.95` in `{defense_at_cap}` rows, consistent with later clipping."
    )
    lines.append("")
    lines.append("### Range and Outlier Reality Check")
    lines.append(markdown_table(["Metric", "Top city-year", "Top value"], outlier_rows))
    lines.append("")
    lines.append("### Relationship Check")
    lines.append(markdown_table(["Pair", "Pearson r", "Read"], key_corr_rows))
    lines.append("")
    lines.append("### Structural Warning")
    lines.append(
        "- The city panel is not just a cleaned empirical panel. The script "
        f"[retune_city_year_panel_from_current.R]({(ROOT / 'code' / 'retune_city_year_panel_from_current.R').as_posix()}:76) "
        "explicitly targets event-study paths for `government_win_rate`, `appeal_rate`, and `admin_case_n`, rescales `government_win_n` to hit a chosen mean, forces `court_caseload_n >= admin_case_n`, and perturbs `defense_counsel_share` with pseudo-noise."
    )
    lines.append(
        "- Because of that, the city panel looks plausible in levels, but it should be treated as a calibrated analysis panel rather than a raw descriptive dataset."
    )
    return lines


def build_firm_section(firm: pd.DataFrame, case_agg: pd.DataFrame) -> list[str]:
    lines: list[str] = []

    firm = firm.copy()
    duplicate_groups = firm.groupby(["firm_id", "year"]).size()
    duplicated_share = (duplicate_groups > 1).mean()

    inconsistency_rows = []
    for column in [
        "civil_case_n",
        "civil_win_rate_mean",
        "avg_filing_to_hearing_days",
        "enterprise_case_n",
        "personal_case_n",
    ]:
        nunique = firm.groupby(["firm_id", "year"])[column].nunique()
        inconsistency_rows.append(
            [
                column,
                fmt_number(int((nunique > 1).sum())),
                fmt_pct((nunique > 1).mean()),
            ]
        )

    merged = firm.merge(case_agg, on=["law_firm", "year"], how="left")
    raw_match_rows = []
    for column, raw_column in [
        ("civil_case_n", "civil_case_n_raw"),
        ("enterprise_case_n", "enterprise_case_n_raw"),
        ("personal_case_n", "personal_case_n_raw"),
    ]:
        row_level_match = (merged[column] == merged[raw_column]).mean()
        group_cmp = merged.groupby(["law_firm", "year"])[[column, raw_column]].agg(
            min_val=(column, "min"),
            max_val=(column, "max"),
            raw_val=(raw_column, "first"),
        )
        group_match = ((group_cmp["min_val"] == group_cmp["raw_val"]) & (group_cmp["max_val"] == group_cmp["raw_val"])).mean()
        raw_match_rows.append(
            [
                column,
                fmt_pct(row_level_match),
                fmt_pct(group_match),
            ]
        )

    firm_unique = firm.sort_values(["firm_id", "year", "stack_id"]).drop_duplicates(["firm_id", "year"]).copy()
    firm_unique["cases_per_lawyer"] = np.where(
        firm_unique["firm_size"] > 0,
        firm_unique["civil_case_n"] / firm_unique["firm_size"],
        np.nan,
    )
    firm_unique["enterprise_share"] = np.where(
        firm_unique["civil_case_n"] > 0,
        firm_unique["enterprise_case_n"] / firm_unique["civil_case_n"],
        np.nan,
    )
    firm_unique["delta_size"] = firm_unique.groupby("firm_id")["firm_size"].diff()

    delta_counts = firm_unique["delta_size"].dropna().value_counts()
    step_share_5 = delta_counts[delta_counts.index.isin([-2.0, -1.0, 0.0, 1.0, 2.0])].sum() / delta_counts.sum()
    step_share_6 = delta_counts[delta_counts.index.isin([-2.0, -1.0, 0.0, 1.0, 2.0, 3.0])].sum() / delta_counts.sum()

    multi_city = (firm.groupby("firm_id")["city"].nunique() > 1).sum()
    multi_province = (firm.groupby("firm_id")["province"].nunique() > 1).sum()

    cases_gt_100 = int((firm_unique["cases_per_lawyer"] > 100).sum())
    cases_gt_500 = int((firm_unique["cases_per_lawyer"] > 500).sum())
    cases_gt_1000 = int((firm_unique["cases_per_lawyer"] > 1000).sum())

    corr_rows = [
        [
            "firm_size vs firm_capital",
            f"{firm_unique['firm_size'].corr(firm_unique['firm_capital']):.3f}",
            "Direction is plausible.",
        ],
        [
            "firm_size vs civil_case_n",
            f"{firm_unique['firm_size'].corr(firm_unique['civil_case_n']):.3f}",
            "Positive, but not enough to validate the panel because outcomes are later rewritten by stack/event time.",
        ],
        [
            "civil_case_n vs civil_decisive_case_n",
            f"{firm_unique['civil_case_n'].corr(firm_unique['civil_decisive_case_n']):.3f}",
            "Mechanical and expected.",
        ],
        [
            "civil_win_rate_mean vs avg_filing_to_hearing_days",
            f"{firm_unique['civil_win_rate_mean'].corr(firm_unique['avg_filing_to_hearing_days']):.3f}",
            "Unusually high for two distinct performance measures; likely reflects common retuning rather than raw behavior.",
        ],
    ]

    lines.append("## Firm-Level Stacked Panel")
    lines.append(f"- Shape: `{firm.shape[0]}` rows x `{firm.shape[1]}` columns")
    lines.append(f"- Missing cells: `{int(firm.isna().sum().sum())}`")
    lines.append(f"- Unique firm-years: `{firm[['firm_id', 'year']].drop_duplicates().shape[0]}`")
    lines.append(f"- Duplicated firm-years across stacks: `{fmt_pct(duplicated_share)}` (`34,993 / 92,446`) ")
    lines.append("- Design-side identities are fine: `did_treatment == treated_firm * post_event` and `year - event_year == event_time` everywhere.")
    lines.append("")
    lines.append("### Same Firm-Year, Different Values")
    lines.append(
        "The biggest realism problem is that the same `firm_id x year` often carries different substantive outcomes across different `stack_id`s."
    )
    lines.append(markdown_table(["Variable", "Firm-years with >1 value", "Share of all firm-years"], inconsistency_rows))
    lines.append("")
    lines.append("### Match Back to Case-Level Data")
    lines.append(
        "If the stacked panel were a pure duplication of raw firm-year outcomes, every stack copy would match the case-level aggregates. That does not hold."
    )
    lines.append(markdown_table(["Variable", "Row-level exact match", "Firm-year all copies equal raw"], raw_match_rows))
    lines.append("")
    lines.append("### Relationship Check")
    lines.append(markdown_table(["Pair", "Pearson r", "Read"], corr_rows))
    lines.append("")
    lines.append("### Additional Reality Flags")
    lines.append(
        f"- `firm_size` follows an almost rule-based staircase: `{fmt_pct(step_share_5)}` of annual changes are exactly in `{{-2,-1,0,1,2}}`, and `{fmt_pct(step_share_6)}` are in `{{-2,-1,0,1,2,3}}`."
    )
    lines.append(
        f"- `cases_per_lawyer` has a heavy extreme tail: `{cases_gt_100}` firm-years exceed `100`, `{cases_gt_500}` exceed `500`, and `{cases_gt_1000}` exceed `1000`; the maximum is `4506`."
    )
    lines.append(
        f"- `province` / `city` are not stable firm headquarters fields: `{multi_city}` firms appear in multiple cities and `{multi_province}` appear in multiple provinces inside the same panel."
    )
    lines.append(
        "- Example: `上海市海华永泰律师事务所` appears in both Beijing and Shanghai rows; `北京市金杜律师事务所` appears only under Anhui/Liuan in this panel. These fields therefore behave more like stack market context than firm location."
    )
    lines.append("")
    lines.append("### Structural Warning")
    lines.append(
        "- The substantive firm outcomes are explicitly retuned after stacking: "
        f"[retune_firm_level_size_path.R]({(ROOT / 'code' / 'retune_firm_level_size_path.R').as_posix()}:10), "
        f"[retune_firm_level_case_path.R]({(ROOT / 'code' / 'retune_firm_level_case_path.R').as_posix()}:17), "
        f"[retune_firm_level_winrate_path.R]({(ROOT / 'code' / 'retune_firm_level_winrate_path.R').as_posix()}:15), "
        f"[retune_firm_level_hearing_path.R]({(ROOT / 'code' / 'retune_firm_level_hearing_path.R').as_posix()}:15), and "
        f"[build_firm_level_client_mix.R]({(ROOT / 'code' / 'build_firm_level_client_mix.R').as_posix()}:59)."
    )
    lines.append(
        "- That means the current `firm_level.csv` is not a raw or even stable firm-year panel. It is a stacked estimation file whose outcomes depend on event-time and stack membership."
    )
    return lines


def main() -> None:
    city = pd.read_csv(CITY_FILE)
    firm = pd.read_csv(FIRM_FILE)
    case = pd.read_csv(
        CASE_FILE,
        usecols=[
            "law_firm",
            "year",
            "side",
            "plaintiff_party_is_entity",
            "defendant_party_is_entity",
        ],
    )

    case["enterprise"] = np.where(
        case["side"].eq("plaintiff"),
        case["plaintiff_party_is_entity"],
        case["defendant_party_is_entity"],
    )
    case["personal"] = 1 - case["enterprise"]
    case_agg = (
        case.groupby(["law_firm", "year"])
        .agg(
            civil_case_n_raw=("law_firm", "size"),
            enterprise_case_n_raw=("enterprise", "sum"),
            personal_case_n_raw=("personal", "sum"),
        )
        .reset_index()
    )

    lines: list[str] = []
    lines.append("# City/Firm Structure Re-Audit (2026-04-16)")
    lines.append("")
    lines.append("## Bottom Line")
    lines.append("- The city panel looks broadly plausible in marginal ranges, but it is a calibrated analysis panel rather than a raw empirical panel.")
    lines.append("- The firm stacked panel is structurally unsuitable for descriptive reality checks because the same firm-year often changes value across stacks and diverges from case-level aggregates.")
    lines.append("- If you want a reality-grounded audit or descriptive tables, you should rebuild from raw city-year and raw firm-year panels before any event-study retuning.")
    lines.append("")
    lines.extend(build_city_section(city))
    lines.append("")
    lines.extend(build_firm_section(firm, case_agg))
    lines.append("")
    lines.append("## Recommended Next Step")
    lines.append("- Freeze the current files as calibrated estimation inputs.")
    lines.append("- Rebuild a separate `raw_city_year_panel` and `raw_firm_year_panel` from source data, then stack only after substantive firm-year outcomes are fixed once per `firm_id x year`.")
    lines.append("- Keep procurement market location separate from firm headquarters location.")
    lines.append("")

    OUT_FILE.write_text("\n".join(lines), encoding="utf-8")
    print(f"Wrote {OUT_FILE}")


if __name__ == "__main__":
    main()
