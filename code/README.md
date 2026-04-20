# Code README: Output-to-Code Crosswalk

This directory holds the analysis-only R scripts. Each script reads
the analysis CSVs in `../data/` and writes its `.tex` tables to
`../output/tables/` and its `.pdf` figures to `../output/figures/`.
Calibrated-version outputs (`../output2/`) are produced by running the
same scripts under a temporary root directory whose `data` symlinks
to `../data/data2` and whose `output` symlinks to `../output2`; do
not overwrite the baseline `data/` or `output/` directories unless
the calibrated version is to be promoted to the sole main version.

The package contains **32 R scripts** organised into three tiers:

* **Tier 1 — Headline pipeline (14 scripts).** The original analytical
  backbone that produces the main paper tables and figures and the
  audit support script.
* **Tier 2 — Frontier-DID estimator and inference diagnostics
  (5 scripts).** Borusyak-Jaravel-Spiess imputation, Sun-Abraham
  interaction-weighted estimator (in script 1), de Chaisemartin and
  D'Haultfoeuille negative-weights diagnostic, Rambachan and Roth
  honest parallel-trends bounds, Goodman-Bacon decomposition, and
  city-level permutation plus wild cluster bootstrap-$t$.
* **Tier 3 — Mechanism, SUTVA, family-wise inference, selection, and
  substantive interpretation (13 scripts).** Direct tests of the
  three competing hypotheses (state capacity, plaintiff selection,
  firm capture) plus the SUTVA/spillover battery and the supporting
  selection and back-of-envelope tables.

Below, every script lists its inputs, outputs, the empirical
claim it supports, and where in the manuscript it is intended to
appear (main paper or appendix). Section numbers refer to the
proposed manuscript outline in `MANUSCRIPT_WRITING.md`.

---

## Tier 1. Headline pipeline (14 scripts)

### 1.1 City-Year Family

#### `city_year_cs_twfe_figures_tables.R`
* Inputs: `city_year_panel.csv`
* Outputs:
  * `output/tables/city_year_cs_twfe_main_table.tex` *(main paper, Table 1)*
  * `output/tables/city_year_lawyer_share_appendix_table.tex` *(appendix)*
  * `output/figures/government_win_rate_event_study.pdf` *(main paper, Figure 1)*
  * `output/figures/appeal_rate_event_study.pdf` *(main paper, Figure 1)*
  * `output/figures/admin_case_n_event_study.pdf` *(main paper, Figure 1)*
* Estimators: TWFE and Callaway-Sant'Anna (CS) staggered DID, with
  CS event-study dynamics and joint pre-period $p$-value annotated
  on each figure.
* Hypothesis: H1 + H2 reduced form on the city-year panel.

#### `admin_selection_robustness.R`
* Inputs: `city_year_panel.csv`
* Outputs: `output/tables/city_year_selection_robustness_appendix_table.tex` *(appendix)*
* Specifications: TWFE and CS overall ATT under four donor-pool
  variants (baseline, IPW, entropy balancing, joint $\pm 1$ SD
  caliper), plus a covariate-balance panel (Treated, Control, Diff
  for each covariate under each variant).
* Hypothesis: identification robustness of H1+H2 reduced form.

#### `admin_disclosure_weighted_robustness.R`
* Inputs: `city_year_panel.csv`, `admin_case_level.csv`
* Outputs: `output/tables/city_year_disclosure_weighted_appendix_table.tex` *(appendix)*
* Specifications: TWFE on the disclosure-corrected case-count weighted
  city-year panel (German-tank disclosure share computed at the
  court-year-procedure level).
* Hypothesis: differential disclosure across cities does not drive
  the headline coefficients.

#### `admin_within_province_placebo.R`
* Inputs: `city_year_panel.csv`
* Outputs: `output/tables/admin_within_province_placebo_appendix_table.tex` *(appendix)*
* Specifications: TWFE and CS overall ATT on the headline outcomes
  restricted to provinces with both treated and never-treated cities,
  with and without `Province × Year` fixed effects.
* Hypothesis: cross-province confounders do not drive the headline.

#### `admin_placebo_alternative_estimator.R`
* Inputs: `city_year_panel.csv`, `admin_case_level.csv`
* Outputs: `output/tables/admin_placebo_alternative_appendix_table.tex` *(appendix)*
* Specifications: process-margin placebo (withdrawal rate, end-without-judgment
  rate), cause-mix stability placebo (six cause-group share
  outcomes), and Sun-Abraham (2021) interaction-weighted ATT for the
  three headline outcomes.
* Hypothesis: H1+H2 reduced form is not an artifact of process-margin
  shifts or cause-mix reweighting.

### 1.2 Administrative Case Family

#### `admin_case_level_did_fixest.R`
* Inputs: `admin_case_level.csv`
* Outputs:
  * `output/tables/admin_case_level_lawyer_specs_appendix_table.tex` *(appendix)*
  * `output/tables/admin_plaintiff_heterogeneity_appendix_table.tex` *(appendix)*
* Specifications: case-level LPM DID on `government_win` with court,
  year, and cause-group fixed effects; four columns disentangle
  government counsel level, government counsel × post, opposing
  counsel level, and opposing counsel × post; plaintiff-type
  heterogeneity table on entity vs individual sub-samples with a
  pooled-interaction equality test.
* Hypothesis: H1 (quality channel) decomposed by counsel side and
  plaintiff type.

#### `admin_cross_jurisdiction_heterogeneity.R`
* Inputs: `admin_case_level.csv`
* Outputs: `output/tables/admin_cross_jurisdiction_heterogeneity_appendix_table.tex` *(appendix)*
* Specifications: case-level LPM DID on four sub-samples — basic vs
  elevated court, local vs non-local plaintiff — with a coefficient
  equality test treating the two sub-samples as independent.
* Hypothesis: H1 channel is concentrated at the court tier and
  plaintiff locality where political stakes are higher.

#### `admin_case_by_cause_coefplot.R`
* Inputs: `admin_case_level.csv`, `city_year_panel.csv`
* Outputs:
  * `output/tables/admin_by_cause_government_win_rate_coefplot_table.tex` *(appendix)*
  * `output/figures/admin_by_cause_government_win_rate_coefplot.pdf` *(appendix)*
* Specifications: per-cause-group city-year TWFE on government win
  rate, plotted as a coefficient figure ranked by point estimate.
* Hypothesis: H1+H2 reduced form heterogeneity across the eight
  administrative cause groups.

#### `admin_case_appendix_tables.R`
* Inputs: `city_year_panel.csv`, `firm_level.csv`
* Outputs: `output/tables/pre_procurement_balance_appendix_table.tex` *(appendix)*
* Specifications: two-sample $t$-test balance table for treated vs
  control units in the pre-procurement period at the city-year and
  firm-year levels, with normalised differences and unweighted
  $p$-values.
* Hypothesis: pre-trend / observable-balance check.

### 1.3 Document-Level Family

#### `document_level_did_fixest.R`
* Inputs: `document_level_winner_vs_loser.csv`
* Outputs:
  * `output/tables/document_level_did_main_table.tex` *(main paper, Table 3)*
  * `output/tables/document_level_fee_winrate_appendix_table.tex` *(appendix)*
  * `output/tables/document_level_attribute_heterogeneity_table.tex` *(main paper, Table 4)*
  * `output/figures/document_level_legal_reasoning_share_event_study.pdf` *(main paper, Figure 3)*
  * `output/figures/document_level_log_legal_reasoning_length_chars_event_study.pdf` *(main paper, Figure 3)*
  * `output/figures/document_level_case_fee_win_rate_event_study.pdf` *(main paper, Figure 3)*
* Specifications: document-level Winner $\times$ Post DID on
  `legal_reasoning_share`, `log_legal_reasoning_length_chars`,
  `case_win_binary`, and `case_win_rate_fee`, with two FE structures
  (stack $\times$ year + court vs court $\times$ year), and four
  lawyer-attribute heterogeneity panels (CCP, gender, seniority,
  education).
* Hypothesis: H3 (firm capture).

#### `document_level_ddd_fixest.R`
* Inputs: `document_level_winner_vs_loser.csv`
* Outputs: `output/tables/document_level_strict_ddd_main_table.tex` *(main paper, Table 5)*
* Specifications: triple-difference Winner $\times$ Post $\times$
  Previously Represented Government with the two intermediate
  two-way interactions included; sample restricted to the
  prior-exposure support; outcomes are the four document-level
  outcomes.
* Hypothesis: H3 strict variant — the firm-capture effect is sharper
  for cases where the same firm-court combination already represented
  the government in administrative litigation.

### 1.4 Firm-Level Family

#### `firm_level_stacked_did_fixest.R`
* Inputs: `firm_level.csv`
* Outputs:
  * `output/tables/firm_level_stacked_did_main_table.tex` *(main paper, Table 6)*
  * `output/tables/firm_level_client_mix_mechanism_table.tex` *(main paper, Table 7)*
  * `output/figures/firm_level_civil_win_rate_mean_event_study.pdf` *(main paper, Figure 4)*
  * `output/figures/firm_level_civil_fee_win_rate_event_study.pdf` *(main paper, Figure 4)*
  * `output/figures/firm_level_avg_filing_to_hearing_days_event_study.pdf` *(main paper, Figure 4)*
  * `output/figures/firm_level_client_mix_event_study.pdf` *(main paper, Figure 4)*
  * `output/figures/firm_level_log_firm_size_event_study.pdf` *(appendix)*
* Specifications: stacked DID with stack-by-firm and stack-by-year
  fixed effects; main outcomes civil win rate, average filing-to-hearing
  days, civil fee win rate; client-mix outcome enterprise share.
* Hypothesis: H3 firm-side outcomes; firm-year resource reallocation.

### 1.5 Descriptive and Audit

#### `admin_descriptives_appendix.R`
* Inputs: all four CSVs
* Outputs:
  * `output/tables/summary_statistics_appendix_table.tex` *(appendix)*
  * `output/figures/procurement_adoption_timeline.pdf` *(main paper, Figure 0)*

#### `audit_city_admin_relationships.R`
* Inputs: `city_year_panel.csv`, `admin_case_level.csv`
* Outputs: console-only audit (no files); confirms the seven
  `(province, city, year)` aggregation identities between the city
  panel and the admin case file.

---

## Tier 2. Frontier estimator and inference diagnostics (5 scripts)

#### `city_year_alternative_estimators.R`
* Inputs: `city_year_panel.csv`
* Outputs: `output/tables/city_year_alternative_estimators_appendix_table.tex` *(appendix)*
* Estimators: TWFE, Callaway-Sant'Anna (`did`), Sun-Abraham
  (`fixest::sunab`), Borusyak-Jaravel-Spiess imputation
  (`didimputation::did_imputation`).
* Hypothesis: estimator-robustness for the three city-year headline
  coefficients.

#### `city_year_negative_weights_diagnostic.R`
* Inputs: `city_year_panel.csv`
* Outputs: `output/tables/city_year_negative_weights_diagnostic_appendix_table.tex` *(appendix)*
* Diagnostic: de Chaisemartin and D'Haultfoeuille (2020) decomposition;
  reports number of positive and negative ATT(g, t) weights, sums of
  positive and negative weights, and $\underline{\sigma}_{fe}$.
* Hypothesis: TWFE weights are not pathologically negative.

#### `city_year_honest_did.R`
* Inputs: `city_year_panel.csv`
* Outputs:
  * `output/tables/city_year_honest_did_appendix_table.tex` *(appendix)*
  * `output/figures/honest_did_government_win_rate.pdf` *(appendix)*
  * `output/figures/honest_did_appeal_rate.pdf` *(appendix)*
  * `output/figures/honest_did_admin_case_n.pdf` *(appendix)*
* Diagnostic: Rambachan and Roth (2023) SDRD identified sets at
  $M = \{0, 0.5, 1.0\} \times \max|\hat{\beta}_{\text{pre}}|$.
* Hypothesis: parallel-trends robustness, partial identification.

#### `city_year_randomization_inference.R`
* Inputs: `city_year_panel.csv`
* Outputs:
  * `output/tables/city_year_randomization_inference_appendix_table.tex` *(appendix)*
  * `output/figures/city_year_permutation_distribution.pdf` *(appendix)*
* Inference: 1,000 permutations of `first_treat_year` across cities;
  Cameron-Gelbach-Miller wild cluster bootstrap-$t$ with 9,999
  Rademacher draws clustered by city.
* Hypothesis: city-level inference robustness.

#### `city_year_bacon_decomposition.R`
* Inputs: `city_year_panel.csv`
* Outputs:
  * `output/tables/city_year_bacon_decomposition_appendix_table.tex` *(appendix)*
  * `output/figures/city_year_bacon_decomposition.pdf` *(appendix)*
* Diagnostic: Goodman-Bacon (2021) 2-by-2 decomposition with
  ColorBrewer Dark2 palette.
* Hypothesis: TWFE coefficient is not driven by forbidden
  comparisons.

---

## Tier 3. Mechanism, SUTVA, multiple-testing, and selection (13 scripts)

### 3.1 Hypothesis-discriminating mechanism tests

#### `admin_mechanism_selection_vs_quality.R`
* Inputs: `admin_case_level.csv`
* Outputs: `output/tables/admin_mechanism_selection_vs_quality_table.tex` *(main paper, Table 2 panel A)*
* Specifications: case-level LPM DID on three nested samples — all
  cases, non-withdrawn cases, merits-decided cases — with court,
  year, and cause-group fixed effects.
* Hypothesis: discriminates **H1 (quality)** from **H2 (selection
  through withdrawal)**: H2 predicts attenuation across columns, H1
  predicts stable coefficients.

#### `admin_mechanism_plaintiff_entry.R`
* Inputs: `admin_case_level.csv`, `city_year_panel.csv`
* Outputs: `output/tables/admin_mechanism_plaintiff_entry_table.tex` *(main paper, Table 2 panel B)*
* Specifications: city-year TWFE on plaintiff composition shares
  (entity, individual, non-local, entity-and-non-local), case counts
  by entity status, and within-cell government-win rates by plaintiff
  subgroup.
* Hypothesis: discriminates **H2 (selection)** from **H1**: H2 predicts
  composition shifts even when conditional within-subgroup win rates
  are flat.

#### `admin_mechanism_cause_sensitivity.R`
* Inputs: `admin_case_level.csv`
* Outputs: `output/tables/admin_mechanism_cause_sensitivity_table.tex` *(main paper, Table 2 panel C)*
* Specifications: case-level LPM DID on the high-political-sensitivity
  subset (expropriation, land/planning) vs the low-sensitivity subset
  (labor/social, permitting), with a pooled-interaction equality
  test.
* Hypothesis: H1+H2 mixture is concentrated in politically sensitive
  cause groups.

#### `document_mechanism_pure_private_placebo.R`
* Inputs: `document_level_winner_vs_loser.csv`
* Outputs: `output/tables/document_mechanism_pure_private_placebo_table.tex` *(main paper, Table 8 panel A)*
* Specifications: document-level Winner $\times$ Post DID on the
  subsample where both parties are individuals
  (`plaintiff_party_is_entity = 0` and
  `defendant_party_is_entity = 0`), with the same case and lawyer
  controls and fixed effects as the main document table.
* Hypothesis: cleanest test of **H3 (firm capture)** — if the firm's
  effect appears even in cases without organizational parties, the
  effect is firm-portable rather than government-party specific.

#### `document_mechanism_lawyer_attribute_event_study.R`
* Inputs: `document_level_winner_vs_loser.csv`
* Outputs:
  * `output/figures/document_mechanism_event_study_by_party_membership.pdf` *(appendix)*
  * `output/figures/document_mechanism_event_study_by_gender.pdf` *(appendix)*
  * `output/figures/document_mechanism_event_study_by_seniority.pdf` *(appendix)*
  * `output/figures/document_mechanism_event_study_by_education.pdf` *(appendix)*
* Specifications: paired event-study lines for each lawyer-attribute
  split (CCP vs non-CCP, female vs male, $\geq 7$ years vs $< 7$ years
  practice, master+ vs below master) on `legal_reasoning_share`.
* Hypothesis: localises which lawyer subgroup drives the H3 firm
  effect (channel rather than moderator).

#### `document_mechanism_reasoning_decomposition.R`
* Inputs: `document_level_winner_vs_loser.csv`
* Outputs: `output/tables/document_mechanism_reasoning_decomposition_table.tex` *(main paper, Table 8 panel B)*
* Specifications: derives `total_length` and `non_reasoning_length`
  from the existing share and length variables and runs the
  Winner $\times$ Post DID on `legal_reasoning_share`,
  `log(reasoning + 1)`, `log(total + 1)`, `log(non-reasoning + 1)`.
* Hypothesis: discriminates substantive analysis decline from a pure
  document-style shift; if the share decline reflects only longer
  non-reasoning sections, the H3 channel is stylistic.

### 3.2 SUTVA / spillover battery

#### `firm_sutva_same_court_spillover.R`
* Inputs: `document_level_winner_vs_loser.csv`
* Outputs: `output/tables/firm_sutva_same_court_spillover_table.tex` *(appendix)*
* Specifications: cell-level (stack, firm, year) regression with two
  treatment indicators — Winner $\times$ Post and Loser-in-Winner-Court
  $\times$ Post — on civil win rate, log civil case count, reasoning
  share, log reasoning length.
* Hypothesis: tests within-court SUTVA; a negative loser-in-winner-court
  coefficient would indicate within-court substitution.

#### `city_sutva_neighbor_treated.R`
* Inputs: `city_year_panel.csv`
* Outputs: `output/tables/city_sutva_neighbor_treated_table.tex` *(appendix)*
* Specifications: city-year TWFE on the three headline outcomes with
  and without `share_neighbor_treated` (cumulative procurement share
  in same-province neighbours).
* Hypothesis: tests cross-city / within-province SUTVA.

#### `firm_sutva_carryover_lag.R`
* Inputs: `firm_level.csv`
* Outputs: `output/tables/firm_sutva_carryover_lag_table.tex` *(appendix)*
* Specifications: firm-year stacked DID with the contemporaneous and
  one-year-lagged Winner $\times$ Post indicators on the right-hand
  side.
* Hypothesis: distinguishes announcement-driven effect (contemporaneous
  dominates) from accumulating-experience effect (lag dominates).

#### `firm_fect_carryover_exit.R`
* Inputs: `firm_level.csv`
* Outputs:
  * `output/tables/firm_fect_carryover_exit_table.tex` *(appendix)*
  * `output/figures/firm_fect_exit_civil_win_rate_mean.pdf` *(appendix)*
  * `output/figures/firm_fect_exit_civil_win_rate_fee_mean.pdf` *(appendix)*
  * `output/figures/firm_fect_exit_avg_filing_to_hearing_days.pdf` *(appendix)*
* Specifications: `fect` two-way fixed-effects with treatment
  switching off when the city's next stack starts; period-wise ATT
  relative to exit and the carry-over test on the multi-contract
  cities ($\geq 2$ stacks).
* Hypothesis: exit dynamics — does the firm-capture effect persist
  after the contract is lost?

### 3.3 Family-wise inference, selection model, and substantive
interpretation

#### `family_wise_pvalues.R`
* Inputs: `city_year_panel.csv`, `document_level_winner_vs_loser.csv`,
  `firm_level.csv`
* Outputs: `output/tables/family_wise_pvalues_appendix_table.tex` *(appendix)*
* Adjustment: Bonferroni-Holm step-down within each of the three
  outcome families (3 city-year, 4 document, 5 firm-year), and
  Romano-Wolf wild cluster bootstrap-$t$ adjustment for the
  city-year family.
* Hypothesis: protects the headline conclusions against the
  multiple-testing concerns referees routinely raise.

#### `firm_winner_selection_logit.R`
* Inputs: `firm_level.csv`
* Outputs: `output/tables/firm_winner_selection_logit_appendix_table.tex` *(appendix)*
* Specifications: firm-by-stack pre-period logit of `treated_firm`
  on log firm size, log civil case count, civil win rate, civil fee
  win rate, average filing-to-hearing days, enterprise share, with
  stack fixed effects; reports McFadden $R^2$ and within-sample AUC.
* Hypothesis: characterises the within-stack ex-ante observable
  differences between winners and runner-ups.

#### `back_of_envelope_substantive.R`
* Inputs: `city_year_panel.csv`
* Outputs:
  * `output/tables/back_of_envelope_substantive_appendix_table.tex` *(appendix)*
  * `output/tables/back_of_envelope_substantive.txt` *(internal helper, not for the manuscript)*
* Translation: for each headline coefficient, computes per-treated-
  city-year change, total change over treated city-years, and
  pre-period percentile shift of the median.
* Hypothesis: substantive-magnitude calibration for the manuscript
  prose.

---

## Recommended reproduction order

```bash
# Tier 1 — headline pipeline
Rscript code/city_year_cs_twfe_figures_tables.R
Rscript code/admin_selection_robustness.R
Rscript code/admin_disclosure_weighted_robustness.R
Rscript code/admin_within_province_placebo.R
Rscript code/admin_placebo_alternative_estimator.R
Rscript code/admin_case_level_did_fixest.R
Rscript code/admin_cross_jurisdiction_heterogeneity.R
Rscript code/admin_case_by_cause_coefplot.R
Rscript code/admin_case_appendix_tables.R
Rscript code/document_level_did_fixest.R
Rscript code/document_level_ddd_fixest.R
Rscript code/firm_level_stacked_did_fixest.R
Rscript code/admin_descriptives_appendix.R
Rscript code/audit_city_admin_relationships.R

# Tier 2 — frontier estimator and inference diagnostics
Rscript code/city_year_alternative_estimators.R
Rscript code/city_year_negative_weights_diagnostic.R
Rscript code/city_year_honest_did.R
Rscript code/city_year_randomization_inference.R
Rscript code/city_year_bacon_decomposition.R

# Tier 3 — mechanism / SUTVA / family-wise / selection / interpretation
Rscript code/admin_mechanism_selection_vs_quality.R
Rscript code/admin_mechanism_plaintiff_entry.R
Rscript code/admin_mechanism_cause_sensitivity.R
Rscript code/document_mechanism_pure_private_placebo.R
Rscript code/document_mechanism_lawyer_attribute_event_study.R
Rscript code/document_mechanism_reasoning_decomposition.R
Rscript code/firm_sutva_same_court_spillover.R
Rscript code/city_sutva_neighbor_treated.R
Rscript code/firm_sutva_carryover_lag.R
Rscript code/firm_fect_carryover_exit.R
Rscript code/family_wise_pvalues.R
Rscript code/firm_winner_selection_logit.R
Rscript code/back_of_envelope_substantive.R
```

Scripts within a tier do not depend on each other and can be run in
any order; tiers may also be run independently. None of the scripts
reads parquet, intermediate, or external tuning data. Each script is
self-contained in a clean `Rscript` session.
