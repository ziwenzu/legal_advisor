## Output-to-Code Crosswalk

This folder keeps the analysis scripts for the full closed analysis package.

Related outputs are grouped in one script only when they come from the same estimation family and the same input dataset.

The `output/...` paths listed below are the files each script generates when run. The closed package does not keep a stored `output/` directory by default.

### City-Year Family

- `city_year_cs_twfe_figures_tables.R`
  - `output/tables/city_year_cs_twfe_main_table.tex`
  - `output/tables/city_year_lawyer_share_appendix_table.tex`
  - `output/figures/government_win_rate_event_study.pdf`
  - `output/figures/appeal_rate_event_study.pdf`
  - `output/figures/admin_case_n_event_study.pdf`

- `admin_selection_robustness.R`
  - `output/tables/city_year_selection_robustness_appendix_table.tex`

- `admin_disclosure_weighted_robustness.R`
  - `output/tables/city_year_disclosure_weighted_appendix_table.tex`

- `admin_within_province_placebo.R`
  - `output/tables/admin_within_province_placebo_appendix_table.tex`

- `admin_placebo_alternative_estimator.R`
  - `output/tables/admin_placebo_alternative_appendix_table.tex`

### Administrative Case Family

- `admin_case_level_did_fixest.R`
  - `output/tables/admin_case_level_lawyer_specs_appendix_table.tex`
  - `output/tables/admin_plaintiff_heterogeneity_appendix_table.tex`

- `admin_cross_jurisdiction_heterogeneity.R`
  - `output/tables/admin_cross_jurisdiction_heterogeneity_appendix_table.tex`

- `admin_case_by_cause_coefplot.R`
  - `output/tables/admin_by_cause_government_win_rate_coefplot_table.tex`
  - `output/figures/admin_by_cause_government_win_rate_coefplot.pdf`

- `admin_case_appendix_tables.R`
  - `output/tables/pre_procurement_balance_appendix_table.tex`

### Document-Level Family

- `document_level_did_fixest.R`
  - `output/tables/document_level_did_main_table.tex`
  - `output/tables/document_level_fee_winrate_appendix_table.tex`
  - `output/tables/document_level_attribute_heterogeneity_table.tex`
  - `output/figures/document_level_legal_reasoning_share_event_study.pdf`
  - `output/figures/document_level_log_legal_reasoning_length_chars_event_study.pdf`
  - `output/figures/document_level_case_fee_win_rate_event_study.pdf`

- `document_level_ddd_fixest.R`
  - `output/tables/document_level_strict_ddd_main_table.tex`

### Firm-Level Family

- `firm_level_stacked_did_fixest.R`
  - `output/tables/firm_level_stacked_did_main_table.tex`
  - `output/tables/firm_level_client_mix_mechanism_table.tex`
  - `output/figures/firm_level_civil_win_rate_mean_event_study.pdf`
  - `output/figures/firm_level_civil_fee_win_rate_event_study.pdf`
  - `output/figures/firm_level_avg_filing_to_hearing_days_event_study.pdf`
  - `output/figures/firm_level_client_mix_event_study.pdf`
  - `output/figures/firm_level_log_firm_size_event_study.pdf`

### Descriptive Outputs

- `admin_descriptives_appendix.R`
  - `output/tables/summary_statistics_appendix_table.tex`
  - `output/figures/procurement_adoption_timeline.pdf`

### Audit Support

- `audit_city_admin_relationships.R`
  - Cross-checks the city/admin aggregation on overlapping years.
  - Does not itself generate a table or figure.
