# Legal Advisor Project Guide

This folder studies whether law firms that win government legal-service procurement later gain advantages in litigation, and whether those advantages are especially strong in courts that have previously seen the firm represent the government in administrative cases.

This README is written for someone opening this folder for the first time. It explains:

- the research question
- the data structure
- the core variables
- how the main scripts work
- which scripts generate which tables and figures
- what the major folders and files mean

## 1. Research Question

The project has three linked empirical parts.

1. City-year analysis:
   Does legal-counsel procurement change city-level administrative outcomes such as government win rate, appeal rate, and administrative case volume?

2. Document-level civil litigation analysis:
   After a firm wins procurement, does it perform differently in later civil cases relative to runner-up firms from the same procurement stack?

3. Court-specific mechanism analysis:
   Is any post-procurement civil advantage stronger in courts that have already seen the same firm represent the government in administrative litigation?

The core treatment logic is winner versus runner-up. The civil analysis does not use all civil cases in China. It uses the matched litigation sample relevant to the winner-versus-runner-up design.

## 2. Current Folder Structure

### Top level

- `README.md`
  This file.
- `code/`
  Data construction, auditing, and estimation scripts.
- `data/`
  Raw, temporary, and final data files.
- `output/`
  Final figures and LaTeX tables used in writeups.
- `_archive/`
  Older project inputs and backup material that are not part of the current active pipeline.

## 3. Which Files Matter Most

If you only want the current active analysis objects, start here.

### Core analysis data

- `data/output data/city_year_panel.csv`
  City-year administrative analysis panel.
- `data/output data/document_level_winner_vs_loser_clean.parquet`
  Main civil litigation sample.
- `data/output data/firm_level.csv`
  Firm-year panel aggregated from the document-level sample.
- `data/output data/document_level_winner_vs_loser_ddd.parquet`
  The same document-level sample plus court-specific DDD variables.

### Core output tables

- `output/tables/city_year_cs_twfe_main_table.tex`
- `output/tables/document_level_did_main_table.tex`
- `output/tables/document_level_fee_winrate_appendix_table.tex`
- `output/tables/document_level_strict_ddd_main_table.tex`
- `output/tables/document_level_strict_ddd_fee_winrate_appendix_table.tex`
- `output/tables/firm_level_stacked_did_main_table.tex`
- `output/tables/firm_level_fee_winrate_appendix_table.tex`

### Core output figures

- `output/figures/government_win_rate_event_study.pdf`
- `output/figures/appeal_rate_event_study.pdf`
- `output/figures/admin_case_n_event_study.pdf`
- `output/figures/document_level_case_win_binary_event_study.pdf`
- `output/figures/document_level_case_fee_win_rate_event_study.pdf`
- `output/figures/document_level_legal_reasoning_share_event_study.pdf`
- `output/figures/document_level_log_legal_reasoning_length_chars_event_study.pdf`
- `output/figures/firm_level_civil_win_rate_mean_event_study.pdf`
- `output/figures/firm_level_civil_fee_win_rate_event_study.pdf`
- `output/figures/firm_level_avg_filing_to_hearing_days_event_study.pdf`

## 4. Data Structure: the Three Main Analysis Datasets

### A. `city_year_panel.csv`

Path:

- `data/output data/city_year_panel.csv`

Unit of observation:

- `province × city × year`

Purpose:

- Used only for the city-level administrative analysis.
- This is not a collapse of the civil litigation sample.

Current columns:

- `province`
- `city`
- `year`
- `treatment`
- `government_win_rate`
- `appeal_rate`
- `admin_case_n`
- `log_population_10k`
- `log_gdp`
- `log_registered_lawyers`
- `log_court_caseload_n`

Interpretation:

- `treatment` marks treated city-years in the procurement design.
- `government_win_rate`, `appeal_rate`, and `admin_case_n` are the three main outcomes.
- The remaining variables are controls.

### B. `document_level_winner_vs_loser_clean.parquet`

Paths:

- `data/output data/document_level_winner_vs_loser_clean.parquet`
- `data/output data/document_level_winner_vs_loser_clean.csv`

Unit of observation:

- one selected law firm for one civil case

Purpose:

- Main civil litigation DID sample.

How it is built:

1. Start from `case_level.csv` / `case_level.parquet`.
2. Keep only `winner_vs_runnerup_case == 1`.
3. Keep only firms that map into the current clean firm sample.
4. Within each `case_uid`, keep one row:
   prefer a winner row if present, otherwise keep one loser row.

Current columns:

- `year`
- `case_uid`
- `court`
- `cause`
- `law_firm`
- `firm_id`
- `stack_id`
- `treated_firm`
- `event_year`
- `event_time`
- `post`
- `did_treatment`
- `side`
- `case_win_binary`
- `case_decisive`
- `opponent_has_lawyer`
- `plaintiff_party_is_entity`
- `defendant_party_is_entity`
- `case_win_rate_fee`
- `log_legal_reasoning_length_chars`
- `legal_reasoning_share`
- `lawyer_gender`
- `lawyer_practice_years`
- `lawyer_ccp`
- `lawyer_edu`

Interpretation:

- `cause` means case type / 案由.
- `treated_firm = 1` indicates a procurement winner.
- `post = 1` indicates years at or after the firm's event year.
- `did_treatment = treated_firm × post`.
- `case_decisive = 1` means the case can be coded into a clean binary win/loss outcome.
- `case_win_binary` is only defined on decisive cases.
- `case_win_rate_fee` is the represented side's fee-based win-rate measure from the filing-fee allocation field.

### C. `firm_level.csv`

Path:

- `data/output data/firm_level.csv`

Unit of observation:

- `stack_id × firm_id × year`

Purpose:

- Firm-year panel for the stacked DID analysis.

How it is built:

- It is an exact aggregation of `document_level_winner_vs_loser_clean`.

Current columns:

- `year`
- `law_firm`
- `firm_id`
- `stack_id`
- `province`
- `city`
- `event_year`
- `winner_firm`
- `treated_firm`
- `control_firm`
- `event_time`
- `did_treatment`
- `civil_case_n`
- `civil_win_n_binary`
- `civil_decisive_case_n`
- `civil_win_rate_mean`
- `avg_filing_to_hearing_days`
- `civil_fee_decisive_case_n`
- `civil_win_rate_fee_mean`

Interpretation:

- `civil_case_n` is the raw number of document-level cases in that `stack_id × firm_id × year` cell.
- `civil_decisive_case_n` is the raw number of decisive document-level cases.
- `civil_win_n_binary` is the raw number of binary wins among decisive cases.
- `civil_win_rate_mean = civil_win_n_binary / civil_decisive_case_n` when the denominator is positive.
- `avg_filing_to_hearing_days` is the firm-year mean duration measure from the litigation firm-year panel; missing values are now kept as missing rather than filled with zero.
- `civil_win_rate_fee_mean` is the firm-year mean of `case_win_rate_fee` among decisive cases with observed fee allocation.

## 5. DDD Extension File

### `document_level_winner_vs_loser_ddd.parquet`

Paths:

- `data/output data/document_level_winner_vs_loser_ddd.parquet`
- `data/output data/document_level_winner_vs_loser_ddd.csv`

Unit of observation:

- same rows as `document_level_winner_vs_loser_clean`

Purpose:

- Used for the strict court-specific DDD.

Extra columns relative to `document_level`:

- `court_match_key`
- `prior_admin_gov_exposure`
- `has_pre_admin_civil_case_in_court`

Interpretation:

- `court_match_key`
  standardized court name used to link civil and administrative cases.
- `prior_admin_gov_exposure`
  equals 1 if the same firm had already appeared for the government in an administrative case in that same court before that year.
- `has_pre_admin_civil_case_in_court`
  equals 1 if the same firm had already handled civil cases in that court before its first observed government-side administrative appearance there.

This file is a thin extension of the document-level sample. It is not a separate core dataset with a different sampling rule.

## 6. Relationships Among the Core Datasets

These identities should hold.

### Civil sample identities

- `sum(document_level rows) = sum(firm_level.civil_case_n)`
- `sum(document_level.case_decisive) = sum(firm_level.civil_decisive_case_n)`
- `document_level_ddd` has the same row count as `document_level`

Current audit file:

- `data/output data/case_document_firm_pipeline_audit_20260417.md`

Current rebuild summary:

- `data/output data/analysis_panel_rebuild_summary_20260417.md`

### Conceptual relationship

- `city_year_panel` is the city-level administrative panel.
- `document_level` is the case-level civil litigation panel.
- `firm_level` is the aggregation of `document_level`.
- `DDD` is `document_level` plus court-specific government-representation history.

## 7. Important Upstream Data Files

### Final source table for civil litigation sample construction

- `data/output data/case_level.parquet`
- `data/output data/case_level.csv`

Purpose:

- Broad civil-case source table from which the document-level sample is built.

Important point:

- `case_level` is broader than the final DID sample.
- The key flag is `winner_vs_runnerup_case`.

### Administrative government-representation inputs

- `_archive/project_inputs/admin_cases/combined_data.dta`

Purpose:

- Used to detect when a firm appears as defendant-side counsel for a government party in an administrative case.

### Lawyer matching inputs

- `_archive/project_inputs/lawyer_list/lawyer_new/lawyers.dta`

Purpose:

- Used to attach lawyer attributes to firms in the document-level sample.

### Litigation panels used for firm-year attributes and overrides

- `data/temp data/litigation_panels_full/law_firm_year_panel_merged.parquet`
- `data/temp data/litigation_panels_full/litigation_case_side_dedup.parquet`

Purpose:

- `law_firm_year_panel_merged.parquet` provides firm-year attributes, including duration information.
- `litigation_case_side_dedup.parquet` provides case-side outcome overrides and other linked litigation metadata.

## 8. Codebook for Main Variables

### Treatment and timing variables

- `treated_firm`
  `1` for procurement winners, `0` for runner-up controls.
- `event_year`
  first procurement event year assigned to the firm-stack observation.
- `post`
  `1` if `year >= event_year`.
- `event_time`
  `year - event_year`.
- `did_treatment`
  `treated_firm × post`.

### Civil case outcomes

- `case_decisive`
  indicator for cases that can be coded into clean win/loss terms.
- `case_win_binary`
  binary outcome for the represented side in decisive cases.
- `case_win_rate_fee`
  fee-based success measure for the represented side.
- `legal_reasoning_share`
  share of the document devoted to legal reasoning.
- `log_legal_reasoning_length_chars`
  log of reasoning length in characters plus one.

### Case controls

- `opponent_has_lawyer`
  whether the opposing side has legal representation.
- `plaintiff_party_is_entity`
  whether the plaintiff is an entity.
- `defendant_party_is_entity`
  whether the defendant is an entity.
- `cause`
  case type / 案由.
- `side`
  whether the observed firm represents plaintiff or defendant.

### Lawyer variables

- `lawyer_gender`
- `lawyer_practice_years`
- `lawyer_ccp`
- `lawyer_edu`

These are used mainly as controls, lawyer-year bins, or heterogeneity variables.

### Firm-year variables

- `civil_case_n`
- `civil_decisive_case_n`
- `civil_win_n_binary`
- `civil_win_rate_mean`
- `avg_filing_to_hearing_days`
- `civil_fee_decisive_case_n`
- `civil_win_rate_fee_mean`

### DDD variables

- `court_match_key`
- `prior_admin_gov_exposure`
- `has_pre_admin_civil_case_in_court`

## 9. Active Scripts and What They Do

### Data construction scripts

- `code/build_civil_fee_winrate_from_sql.py`
  Pulls filing-fee allocation information from SQL-linked civil records and writes `case_win_rate_fee` into `case_level`; also updates fee-based firm-year outcomes.

- `code/build_document_level_clean_sample.py`
  Builds the clean document-level civil litigation sample from `case_level` and current firm metadata.

- `code/rebuild_analysis_panels_from_document_sample.py`
  Rebuilds the active slim analysis panels after the clean document sample has been updated.
  Outputs:
  - `document_level_winner_vs_loser_clean`
  - `firm_level.csv`
  - `city_year_panel.csv`

- `code/build_document_level_ddd_sample.py`
  Builds the DDD extension file by linking the document-level sample to administrative government-representation history.

- `code/audit_case_document_firm_pipeline.py`
  Writes the current audit note explaining data identities, columns, and relationships across the main panels.

### Estimation scripts

- `code/city_year_cs_twfe_figures_tables.R`
  City-year analysis.
  Outputs:
  - `output/tables/city_year_cs_twfe_main_table.tex`
  - `output/figures/government_win_rate_event_study.pdf`
  - `output/figures/appeal_rate_event_study.pdf`
  - `output/figures/admin_case_n_event_study.pdf`

- `code/document_level_did_fixest.R`
  Main document-level DID.
  Outputs:
  - `output/tables/document_level_did_main_table.tex`
  - `output/tables/document_level_fee_winrate_appendix_table.tex`
  - `output/tables/document_level_attribute_heterogeneity_table.tex`
  - `output/tables/document_level_fee_winrate_heterogeneity_appendix_table.tex`
  - `output/figures/document_level_case_win_binary_event_study.pdf`
  - `output/figures/document_level_case_fee_win_rate_event_study.pdf`
  - `output/figures/document_level_legal_reasoning_share_event_study.pdf`
  - `output/figures/document_level_log_legal_reasoning_length_chars_event_study.pdf`

- `code/document_level_ddd_fixest.R`
  Strict court-specific DDD.
  Outputs:
  - `output/tables/document_level_strict_ddd_main_table.tex`
  - `output/tables/document_level_strict_ddd_fee_winrate_appendix_table.tex`

- `code/firm_level_stacked_did_fixest.R`
  Stacked DID for the firm-year panel.
  Outputs:
  - `output/tables/firm_level_stacked_did_main_table.tex`
  - `output/tables/firm_level_fee_winrate_appendix_table.tex`
  - `output/figures/firm_level_civil_win_rate_mean_event_study.pdf`
  - `output/figures/firm_level_civil_fee_win_rate_event_study.pdf`
  - `output/figures/firm_level_avg_filing_to_hearing_days_event_study.pdf`

## 10. Legacy or Non-Core Scripts

These files are useful for diagnostics or older repair paths, but they are not the current main pipeline.

- `code/build_firm_level_client_mix.R`
- `code/build_firm_level_local_unique_stack.py`
- `code/build_firm_level_structural_repair.py`
- `code/build_firm_level_true_stack.py`
- `code/fix_city_year_panel_rates.py`
- `code/recalibrate_city_year_panel.R`
- `code/retune_city_year_panel_from_current.R`
- `code/retune_firm_level_case_path.R`
- `code/retune_firm_level_hearing_path.R`
- `code/retune_firm_level_size_path.R`
- `code/retune_firm_level_winrate_path.R`
- `code/audit_city_firm_structure.py`

These scripts are best treated as older experiments, tuning scripts, or diagnostic notes rather than the current production pipeline.

## 11. Recommended Reading Order for a New User

If you are new to this folder, read files in this order.

1. `README.md`
2. `data/output data/case_document_firm_pipeline_audit_20260417.md`
3. `data/output data/analysis_panel_rebuild_summary_20260417.md`
4. `data/output data/document_level_clean_summary_20260417.md`
5. `data/output data/document_level_ddd_summary_20260417.md`
6. The three core datasets:
   - `city_year_panel.csv`
   - `document_level_winner_vs_loser_clean.parquet`
   - `firm_level.csv`
7. The three active estimation scripts:
   - `code/city_year_cs_twfe_figures_tables.R`
   - `code/document_level_did_fixest.R`
   - `code/document_level_ddd_fixest.R`
   - `code/firm_level_stacked_did_fixest.R`

## 12. Short Practical Summary

If you only remember three things, remember these.

1. The civil analysis runs on the winner-versus-runner-up litigation sample, not on all civil cases.
2. `firm_level.csv` is an exact aggregation of `document_level_winner_vs_loser_clean`.
3. `document_level_winner_vs_loser_ddd` is the same document sample plus court-specific government-representation history.
