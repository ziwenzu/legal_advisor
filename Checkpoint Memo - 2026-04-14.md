# Checkpoint Memo - 2026-04-14

Project root:
`/Users/ziwenzu/Library/CloudStorage/Dropbox/research/1_Law_project/Legal_advisor`

This memo records the state of the project at the current pause point. It is meant to make it easy to restart work later without reconstructing the entire thread.

## 1. Core Research Question

The project has two linked empirical questions.

### 1.1 Government-side question

Do government agencies that purchase outside legal services perform better in administrative litigation?

Operationally, this means building a panel where the government-side treatment is public legal-service procurement and the government-side outcomes come from administrative litigation.

### 1.2 Law-firm-side question

Do law firms that obtain government contracts gain advantages in the broader legal market?

Operationally, this means building a `law_firm x year` panel where treatment is winning government procurement contracts and outcomes come from civil and administrative litigation, plus lawyer/law-firm background information.

## 2. Conceptual Reset

The cleanest framing remains:

- The paper is about `outsourced cooptation`
- The state uses procurement contracts to turn public resources into private legal rents
- Those rents help recruit lawyers and law firms into regime-serving governance
- The paper should not overclaim direct `judicial capture`

The most useful design decomposition now is:

1. `government_unit x year`
   - treatment: procurement by the government unit
   - outcomes: administrative litigation volume, government win rates, outside-counsel usage
2. `law_firm x year`
   - treatment: when the law firm first wins a government contract
   - outcomes: civil caseload, win rate, duration, lawyer scale

## 3. Current Folder and Data Structure

### 3.1 Root-level key files

- `Memo Judicial Outsourced Cooptation — Theory & Empirical Strategy.md`
  - main theory memo
- `memo2.md`
  - more operational empirical notes
- `Project Status Memo — Data, Theory, Pipeline, and Next Steps.md`
  - earlier broad project memo
- `Checkpoint Memo - 2026-04-14.md`
  - this pause-point memo

### 3.2 `data/raw data/`

- `legal_procurement.dta`
  - raw procurement dataset
- `treatment.dta`
  - older city-year treatment timing table

### 3.3 `data/temp data/`

Procurement outputs:

- `legal_procurement_row_cleaned.csv`
- `legal_procurement_row_cleaned.dta`
- `legal_procurement_tender_level.csv`
- `legal_procurement_tender_level.dta`
- `buyer_unit_lookup.csv`
- `buyer_unit_lookup.dta`
- `legal_procurement_cleaning_summary.txt`

Litigation outputs:

- `litigation_panels_full/`
  - main current output directory
- `litigation_panels_full_pre_restore_backup_20260414_100311/`
  - backup from before the restored-SQL rerun

### 3.4 `admin_cases/`

This remains the administrative-litigation workspace.

Most important files now are:

- `combined_data.dta`
  - administrative case mother table closest to full case coverage
- `data.dta`
  - older cleaned analysis table used in the prior workflow
- `firm_info.dta`
- `supp_firm.dta`
- `preliminary_analysis.do`

### 3.5 `lawyer_list/`

This directory is now central because it provides the official lawyer and law-firm registry used as the naming gold standard.

Most important files:

- `lawyer_list/lawyer_new/final.dta`
  - official lawyer roster with lawyer names and affiliated firms
- `lawyer_list/lawyer_new/lawfirms.dta`
- `lawyer_list/raw_data/lvsuo/law_firm.dta`
- `lawyer_list/raw_data/lvsuo/firm_all_info.dta`

These now serve as the canonical source for lawyer and law-firm names whenever possible.

## 4. What Was Completed Before This Checkpoint

### 4.1 Restored missing civil SQL years

The missing `ws_mscf_result_2014-2017` tables were re-extracted from Tencent Cloud, synchronized back to:

`/Volumes/Archive/mysql_exports/20260315_122701_tencent_mysql/bilibili_resumable/data`

and the temporary local recovery copies were cleaned up afterward.

### 4.2 Rebuilt the litigation pipeline

The litigation pipeline was rerun on the restored full SQL universe.

Key outputs in:
`data/temp data/litigation_panels_full/`

include:

- `litigation_case_side_dedup.parquet`
- `litigation_firm_year_panel.parquet`
- `lawyer_firm_year_panel.parquet`
- `lawyer_master.parquet`
- `law_firm_year_lawyer_counts.parquet`
- `law_firm_master.parquet`
- `law_firm_year_panel_merged.parquet`

### 4.3 Switched timing to judgment/document year

The litigation-side panel year now uses document or judgment year, not case-number year.

This rule now applies to the main litigation-derived panels.

### 4.4 Integrated official lawyer and law-firm sources

The `lawyer_list` directory was integrated as the naming gold standard through:

- `analysis/golden_rule_names.py`

This now supports:

- canonicalizing law-firm names
- canonicalizing lawyer names
- preserving legal aid centers and other non-standard providers as separate types rather than silently collapsing them

### 4.5 Built law-firm-side main panel

The current law-firm-side merged panel is:

- `data/temp data/litigation_panels_full/law_firm_year_panel_merged.parquet`

Latest scale:

- `firm_master_rows`: `200,745`
- `merged_firm_year_rows`: `567,973`
- `firms_in_both_litigation_and_procurement`: `3,308`
- `firms_matched_to_official_registry`: `37,081`
- `legal_aid_center_firms`: `8,389`

### 4.6 Built government-side main panel

This was the main new task completed immediately before this pause.

New script:

- `analysis/build_government_unit_year_panel.py`

New outputs:

- `data/temp data/litigation_panels_full/admin_government_case_unit.parquet`
- `data/temp data/litigation_panels_full/government_unit_master.parquet`
- `data/temp data/litigation_panels_full/government_unit_year_panel.parquet`
- `data/temp data/litigation_panels_full/government_unit_build_summary.json`

This step was also added to:

- `analysis/run_litigation_pipeline.sh`

## 5. Current Main Output Tables

### 5.1 Litigation base

- `litigation_case_side_dedup.parquet`
  - deduped case-side long table
- `litigation_firm_year_panel.parquet`
  - litigation-derived firm-year panel using judgment year

### 5.2 Law-firm-side analysis tables

- `law_firm_master.parquet`
- `law_firm_year_panel_merged.parquet`
- `procurement_firm_year_panel.parquet`
- `litigation_firm_year_wide.parquet`
- `law_firm_year_lawyer_counts.parquet`

### 5.3 Lawyer-side tables

- `lawyer_firm_year_panel.parquet`
- `lawyer_master.parquet`

### 5.4 Government-side tables

- `admin_government_case_unit.parquet`
  - `case x government defendant unit` table
- `government_unit_master.parquet`
  - one row per government unit in the union of admin defendants and procurement buyers
- `government_unit_year_panel.parquet`
  - balanced `government_unit x year` panel

## 6. Government-Side Panel: Construction Logic

### 6.1 Unit universe

The government-unit universe is defined as:

`administrative defendant units ∪ procurement buyer units`

More specifically:

- administrative side comes from `admin_cases/combined_data.dta`
  - field: `beigaomingcheng`
- procurement side comes from `legal_procurement_tender_level.dta` and `buyer_unit_lookup.dta`
  - field: `buyer_unit`

### 6.2 Why `combined_data.dta` rather than `data.dta`

`data.dta` is already a downstream cleaned analysis sample.

For the government-side mother table, the better source is `combined_data.dta` because it is closer to the full administrative-case universe and avoids inheriting earlier sample restrictions.

### 6.3 Year rule

The government-side panel year uses document or judgment year from `panjueriqi`.

The current year window is:

- `2013-2021`

### 6.4 Name cleaning rule

The government-unit cleaning is intentionally conservative.

The current logic:

- normalizes punctuation and spacing
- removes a few obvious procurement-source prefixes
- adds city/district/province context only when the raw unit name is clearly incomplete or overly generic
- does not aggressively merge similar-looking agencies

This is deliberate. The current version prioritizes avoiding false merges.

## 7. Government-Side Panel: Current Numbers

From `government_unit_build_summary.json`:

- `admin_case_unit_rows`: `1,047,352`
- `government_unit_master_rows`: `123,974`
- `government_unit_year_panel_rows`: `1,115,766`
- `panel_year_min`: `2013`
- `panel_year_max`: `2021`
- `units_observed_in_admin`: `121,426`
- `units_observed_in_procurement`: `4,540`
- `units_in_both_sources`: `1,992`
- `masked_units`: `733`
- `procurement_tender_total_n`: `9,730`

The panel is a true balanced panel:

- units: `123,974`
- years: `9`
- total rows: `123,974 x 9 = 1,115,766`

### 7.1 Source composition

- `admin_only`: `119,434`
- `procurement_only`: `2,548`
- `both`: `1,992`

This means the set most directly useful for joint government-procurement analysis is much smaller than the full administrative universe.

## 8. Government-Side Panel: What Variables It Already Contains

### 8.1 Government-side outcomes

- `admin_case_n`
- `admin_government_win_rate`
- `admin_plaintiff_win_rate`
- `admin_defense_counsel_case_share`
- `admin_unique_defense_counsel_firms_n`
- `admin_petition_case_n`
- `admin_reject_case_n`
- `admin_deny_case_n`
- `admin_withdraw_case_n`
- `admin_end_case_n`
- `admin_procedure_sample_n`

### 8.2 Government-side treatments

- `procurement_tender_n`
- `procurement_award_amount_total`
- `procurement_unique_winner_firms_n`
- `contracted_in_year`
- `first_procurement_year`
- `post_first_procurement`
- `event_time_first_procurement`

### 8.3 Static government-unit descriptors

- `government_unit_id`
- `government_unit_clean`
- `government_unit_name_preferred`
- `unit_source_category`
- `government_unit_type`
- `government_admin_tier`
- `province`
- `city`
- `district`

## 9. Key Analytical Findings from the Government-Side Diagnostics

### 9.1 Is it a balanced panel?

Yes.

But this is only a balanced storage structure. It does not mean all units are informative every year.

### 9.2 How sparse is administrative litigation over time?

Administrative litigation is clearly sparse in `2013` and especially `2021`.

Annual totals:

- `2013`: `13,429`
- `2014`: `54,837`
- `2015`: `99,382`
- `2016`: `127,768`
- `2017`: `163,487`
- `2018`: `186,867`
- `2019`: `204,059`
- `2020`: `172,066`
- `2021`: `25,457`

Units with any administrative case that year:

- `2013`: `4,804`
- `2014`: `16,126`
- `2015`: `26,376`
- `2016`: `30,538`
- `2017`: `35,606`
- `2018`: `38,973`
- `2019`: `42,377`
- `2020`: `37,401`
- `2021`: `11,158`

Interpretation:

- `2013` looks like an early thin year
- `2021` looks like a thin tail year and likely should not be treated as equally complete relative to `2018-2020`

### 9.3 Can we assume a government unit stays treated forever after first procurement?

Not confidently.

The current procurement data do not contain an explicit contract duration or service-term field.

This was checked in:

- `legal_procurement_tender_level.dta`
- `legal_procurement_row_cleaned.dta`
- raw `legal_procurement.dta`

No usable `contract start`, `contract end`, or `service term` field is currently present in the cleaned procurement pipeline.

Also, repeat contracting is not common enough to justify a strong “permanent relationship” assumption as the only main specification:

- `buyer_unit x firm` pairs observed in more than one year: `1,171 / 7,661` = `15.3%`
- `buyer_unit x firm` pairs observed in at least three years: `174 / 7,661` = `2.27%`
- buyers observed procuring in more than one year: `1,099 / 4,521` = `24.3%`
- buyers observed procuring in at least three years: `230 / 4,521` = `5.09%`

Implication:

- `contracted_in_year` is the safer main treatment
- `post_first_procurement` should be treated as a robustness or broader persistence specification, not the only main treatment

### 9.4 How sparse is the unit-level panel?

Very sparse.

Across all government units:

- units with exactly `1` administrative case total: `57,419`
- share: `46.3%`
- units with `<=5` cases total: `97,749`
- share: `78.9%`
- units with `<=10` cases total: `108,002`
- share: `87.1%`

Even among the most relevant `both-source` units:

- `1` case total: `460 / 1,992` = `23.1%`
- `<=5` cases total: `1,091 / 1,992` = `54.8%`
- `<=10` cases total: `1,349 / 1,992` = `67.7%`

This means that unit-level win-rate regressions will be noisy unless the design is restricted heavily.

### 9.5 Should the analysis move to city level?

This now looks like a serious option, possibly even the safer main specification.

After aggregating to city-year:

- cities observed: `399`
- city-year rows: `3,591`
- cities with any procured unit: `268`
- cities with exactly `1` total administrative case: `2`
- cities with `<=10` total administrative cases: `11`

So the sparsity problem is much smaller at city level than at government-unit level.

Interpretation:

- `government_unit x year` remains useful for mechanism and treatment timing
- `city x year` may be the better main regression level because it is much less sparse

## 10. Law-Firm-Side Status at This Pause Point

The law-firm-side infrastructure is already in place.

From `law_firm_build_summary.json`:

- `firm_master_rows`: `200,745`
- `litigation_wide_rows`: `567,697`
- `merged_firm_year_rows`: `567,973`
- `unique_procurement_firms`: `3,524`
- `firms_in_both_litigation_and_procurement`: `3,308`
- `firms_matched_to_official_registry`: `37,081`
- `official_registry_match_share`: `18.47%`
- `legal_aid_center_firms`: `8,389`

Important interpretation:

- procurement-side law-firm matching is now strong
- litigation-side names remain much noisier
- legal aid centers and bar associations are now separately visible and should not automatically be pooled with ordinary law firms in the main sample

## 11. Lawyer-Side Status at This Pause Point

From `lawyer_build_summary.json`:

- `lawyer_firm_year_panel_rows`: `7,841,510`
- `lawyer_master_rows`: `475,111`
- `law_firm_year_lawyer_counts_rows`: `566,611`

These outputs are already usable for enrichment, but they are not yet the first priority for the core identification strategy.

## 12. Main Bugs, Constraints, and Design Problems Encountered

### 12.1 Missing civil SQL years

The cleaned civil SQL tables for `2014-2017` had been accidentally moved or deleted. These were successfully re-extracted from Tencent Cloud and restored.

### 12.2 Table-year versus true year

The `ws_*_result_year` file names do not reliably define case year.

This was the reason for shifting to document or judgment year as the panel year.

### 12.3 Administrative source choice

`data.dta` was convenient but too downstream for the government-side mother table.

The government panel now correctly uses `combined_data.dta` as its administrative source.

### 12.4 Procurement duration information is missing

This is now one of the main substantive data constraints for the government-side treatment design.

### 12.5 Unit-name sparsity and noise

Government-unit names are cleaner than litigation-side law-firm names, but some problems remain:

- masked names such as `某某`
- very generic short names
- a small number of procurement-source prefixes or formatting artifacts

The current cleaning is intentionally conservative.

## 13. Current Recommended Design After This Checkpoint

The most coherent next-stage empirical strategy is:

### 13.1 Government-side

Keep both:

1. `government_unit x year`
   - for mechanism, unit-level treatment timing, and department-level interpretation
2. `city x year`
   - likely safer as the main regression level because the unit-level panel is very sparse

Suggested treatment priority:

1. main: `contracted_in_year`
2. secondary: `post_first_procurement`

### 13.2 Law-firm-side

Keep the current `law_firm_year_panel_merged.parquet` as the main firm-side workhorse.

Likely next step later:

- restrict to ordinary law firms
- exclude or separate legal aid centers and bar associations
- construct a regression-ready main sample

## 14. Concrete Restart Options

When work resumes, the most natural next commands would be one of the following:

1. Build `government_city_year_panel`
   - aggregate the new government-unit panel to city-year
   - compare city-level and unit-level sample density

2. Build a government-side regression-ready sample
   - likely restrict to `2014-2020`
   - compare `contracted_in_year` vs `post_first_procurement`
   - possibly focus on units in the overlap sample

3. Build a `government_unit x law_firm x year` relation table
   - useful if the design shifts toward tracking repeated government-firm ties instead of simple unit-level treatment

4. Build a cleaner law-firm-side main sample
   - ordinary law firms only
   - drop legal aid / bar association / obvious residual noise

## 15. Bottom-Line Status

At the moment of this pause:

- the missing civil SQL years have been restored
- the litigation pipeline has been rebuilt on the complete source universe
- official lawyer and law-firm data are integrated as the naming gold standard
- the law-firm-side merged panel exists and is usable
- the government-side main panel now exists and is usable

The project is no longer blocked by missing source data or missing core panels.

The main remaining challenge is no longer data assembly. It is research design discipline:

- decide whether the government-side main specification should be unit-year or city-year
- decide whether procurement treatment should be `in-year` or `post-first-procurement`
- keep the analysis centered on the original two linked research questions rather than continuing to widen the data-building scope
