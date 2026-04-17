# Project Status Memo — Data, Theory, Pipeline, and Next Steps

Date: 2026-04-14

Project root:
`/Users/ziwenzu/Library/CloudStorage/Dropbox/research/1_Law_project/Legal_advisor`

This memo summarizes:

1. The current folder structure and what each part contains
2. The research question, theory, and main empirical hypotheses
3. The current data assets, including newly added useful data
4. The processing pipeline and the scripts currently in use
5. What work has already been completed
6. The main bugs, data problems, and design constraints encountered so far
7. The recommended next-step plan

## 1. Project Goal in One Paragraph

The project studies how authoritarian governments use public legal-service contracts to coopt legal professionals without directly dismantling the formal autonomy of courts. The basic idea is that governments outsource legal functions to private law firms, converting public procurement into private rents. In exchange, those firms and lawyers help governments manage administrative litigation and then benefit in the broader private legal market. The empirical strategy links procurement contracts, administrative litigation, civil litigation, and lawyer/law-firm background data.

## 2. Current Folder Structure

### 2.1 Root-level files

- `Memo Judicial Outsourced Cooptation — Theory & Empirical Strategy.md`
  - Main theory memo
- `memo2.md`
  - More operational empirical plan
- `Project Status Memo — Data, Theory, Pipeline, and Next Steps.md`
  - This memo

### 2.2 `data/`

- `data/raw data/`
  - `legal_procurement.dta`
    - Raw procurement dataset
  - `treatment.dta`
    - City-year treatment timing table

- `data/temp data/`
  - Procurement cleaning outputs
    - `legal_procurement_row_cleaned.csv`
    - `legal_procurement_row_cleaned.dta`
    - `legal_procurement_tender_level.csv`
    - `legal_procurement_tender_level.dta`
    - `buyer_unit_lookup.csv`
    - `buyer_unit_lookup.dta`
    - `legal_procurement_cleaning_summary.txt`
  - Litigation panel outputs
    - `litigation_panels/`
      - early test outputs
    - `litigation_panels_2010_check/`
      - one-year extraction check
    - `litigation_panels_2010_dedup_check/`
      - one-year dedupe check
    - `litigation_panels_admin2011_check/`
      - admin schema compatibility check
    - `litigation_panels_civil2018_check/`
      - civil chunk compatibility check
    - `litigation_panels_full/`
      - the current main output directory
    - `litigation_panels_full_pre_restore_backup_20260414_100311/`
      - backup from before the restored SQL rerun

### 2.3 `admin_cases/`

- Existing administrative litigation analysis workspace
- Contains:
  - raw or semi-raw yearly CSVs
  - cleaned Stata datasets
  - matching artifacts
  - older Stata/R scripts
- Key files:
  - `combined_data.dta`
  - `data.dta`
  - `firm_info.dta`
  - `supp_firm.dta`
  - `pre_treatment.dta`
  - `preliminary_analysis.do`
  - `backup/ws_xzcf_result_2010-2020.csv`

### 2.4 `analysis/`

Current active scripts:

- `clean_legal_procurement.py`
- `extract_litigation_panels.py`
- `build_litigation_panels_from_parquet.py`
- `audit_litigation_sources.py`
- `build_lawyer_level_datasets.py`
- `build_firm_level_datasets.py`
- `run_litigation_pipeline.sh`
- `recover_ws_mscf_result_tables.sh`
- `watch_and_sync_recovered_tables.sh`

Old or not currently central:

- `clean.do`

### 2.5 `lawyer_list/`

This is an important newly useful directory. It contains lawyer and law-firm background data that can later be merged into the main project.

- `lawyer_list/lawyer_new/`
  - `final.dta`
  - `lawyers.dta`
  - `lawfirms.dta`
  - `location.dta`
  - `supplement.dta`
  - several CSVs for law firm and lawyer info
- `lawyer_list/raw_data/lvsuo/`
  - `firm_all_info.dta`
  - `law_firm.dta`
  - raw law-firm rosters and supporting files
- `lawyer_list/law firm/`
  - additional Qichacha-exported law firm background data

### 2.6 `output/`

- Currently empty
- We have been writing all intermediate and current analysis-ready outputs into `data/temp data/` instead

## 3. Research Question and Conceptual Framing

### 3.1 Core research question

How do authoritarian governments use market-based legal contracting to shape legal outcomes while preserving the formal appearance of judicial autonomy?

### 3.2 Theoretical concept

The project centers on `outsourced cooptation`.

The mechanism is:

1. Governments need courts, lawyers, and legal procedures to maintain an image of lawful governance.
2. Direct intervention in courts is often politically costly.
3. Governments therefore outsource legal work to private law firms through public contracts.
4. These contracts generate direct and indirect rents:
   - revenue
   - reputation
   - access to state actors
   - political insurance
   - client-side signaling value
5. Those rents induce firms and lawyers to align their behavior with regime interests.
6. This produces both governance effects and market effects.

### 3.3 What the paper is really about

The paper is not best framed as simple `judicial capture`.

It is better framed as:

- state capture of professional expertise
- authoritarian legal governance
- market-mediated cooptation of legal elites

## 4. Big Empirical Hypotheses

### 4.1 Government-side hypotheses

H1. Governments that retain outside legal counsel face fewer administrative suits.

H2. When sued, governments with outside counsel win more often.

H3. Governments with outside counsel face fewer appeals or lower downstream contestation.

H4. These effects should be strongest where contracting creates more durable or embedded relationships, not just one-off representation.

### 4.2 Firm-side hypotheses

H5. Firms receiving government legal-service contracts gain more private-side civil case volume after treatment.

H6. Treated firms improve their civil litigation performance, measured by higher win rates.

H7. Treated firms see faster case processing, proxied by shorter filing-to-judgment duration.

H8. Treated firms expand organizationally, including larger lawyer rosters or greater lawyer inflow.

### 4.3 Distributional hypotheses

H9. Government contracts concentrate among politically embedded or already-capable firms.

H10. That concentration widens inequality across firms in the legal market.

### 4.4 Additional scope conditions

Potential heterogeneity to test later:

- local vs non-local cases
- plaintiff-side vs defendant-side civil cases
- government defendant-side administrative representation
- competitive tenders vs direct or weakly competitive procurement
- first contract vs repeated contract

## 5. Data Assets Currently Available

### 5.1 Procurement data

Raw source:

- `data/raw data/legal_procurement.dta`

Current status:

- raw rows: `14,686`
- cleaned tender-level rows: `9,766`
- duplicate raw rows flagged during cleaning: `1,250`
- treatment city-year rows: `269`

Main cleaned outputs:

- `data/temp data/legal_procurement_tender_level.dta`
- `data/temp data/legal_procurement_tender_level.csv`
- `data/temp data/buyer_unit_lookup.dta`
- `data/temp data/legal_procurement_cleaning_summary.txt`

Useful features already present in the tender-level procurement data:

- `approx_tender_id`
- `province`, `city`, `district`
- `buyer_unit`
- `year`, `month`
- `winner_list_json`
- `candidate_list_json`
- `competitive_candidate_pool`
- `runner_up_info_observed`
- `multi_winner_tender`
- `award_amount_total`

Important substantive limitation:

- only `668` tenders show a competitive candidate pool
- only `163` tenders show runner-up information

This means the `winner vs runner-up` design is attractive conceptually but thin empirically.

### 5.2 Administrative litigation data

Key files:

- `admin_cases/combined_data.dta`
- `admin_cases/data.dta`

Current status:

- `combined_data.dta`: `1,204,875` rows, `31` columns
- `data.dta`: `997,352` rows, `39` columns

Additional supporting files:

- `firm_info.dta`: `36,934` law firms
- `supp_firm.dta`: `4,929` supplemental firms
- `pre_treatment.dta`
- yearly admin CSVs and backups

### 5.3 Civil + administrative SQL source data on Archive

Source root used for the rebuilt pipeline:

- `/Volumes/Archive/mysql_exports/20260315_122701_tencent_mysql/bilibili_resumable/data`

This source now contains the full restored civil and admin result tables used in the current full rerun.

Important restored addition:

- `ws_mscf_result_2014`
- `ws_mscf_result_2015`
- `ws_mscf_result_2016`
- `ws_mscf_result_2017`

These four civil year partitions had been missing locally and were re-extracted from Tencent Cloud, then synced back to the Archive drive.

### 5.4 Newly built litigation outputs

Main output directory:

- `data/temp data/litigation_panels_full/`

Main litigation summary:

- source files processed: `84`
- years covered: `2010-2022`
- raw case-side rows:
  - admin: `1,148,220`
  - civil: `25,489,129`
  - total: `26,637,349`

After conservative dedupe:

- deduped case-side rows:
  - admin: `1,036,639`
  - civil: `24,253,499`
  - total: `25,290,138`
- duplicate rows removed: `1,347,211`

### 5.5 Newly built firm-level outputs

Key files:

- `law_firm_master.parquet`
- `procurement_firm_year_panel.parquet`
- `litigation_firm_year_wide.parquet`
- `law_firm_year_panel_merged.parquet`
- `litigation_firm_year_panel_linked.parquet`
- `law_firm_build_summary.json`

Current status:

- `law_firm_master`: `202,232` firms
- valid-window litigation firm-year wide rows: `583,720`
- procurement firm-year rows: `5,663`
- merged firm-year rows: `584,020`
- unique litigation firms in 2010-2022 window: `200,630`
- unique procurement firms: `3,548`
- firms observed in both litigation and procurement: `3,309`

Interpretation:

- procurement-to-litigation overlap is strong on the procurement side
- most procurement firms can be found somewhere in litigation data
- but procurement firms remain a very small share of all litigation firms

### 5.6 Newly built lawyer-level outputs

Key files:

- `lawyer_firm_year_panel.parquet`
- `lawyer_master.parquet`
- `law_firm_year_lawyer_counts.parquet`
- `lawyer_build_summary.json`

Current status:

- lawyer-firm-year panel rows: `7,991,624`
- lawyer master rows: `515,964`
- firm-year lawyer count rows: `582,719`

The current lawyer outputs include:

- all lawyer mentions
- named-only lawyer counts

Important note:

- lawyer names in litigation texts are often masked or anonymized
- therefore the most reliable downstream lawyer metrics are currently:
  - `unique_named_lawyers_total_n`
  - `named_lawyer_mentions_total_n`
rather than the raw lawyer-name universe itself

### 5.7 Newly useful lawyer background data

From `lawyer_list/lawyer_new/` and `lawyer_list/raw_data/lvsuo/`:

- `final.dta`: `598,240` rows, `18` columns
  - lawyer + firm + location + CCP + education + firm background
- `lawyers.dta`: `633,893` rows, `9` columns
  - lawyer roster
- `lawfirms.dta`: `36,934` rows, `10` columns
  - law-firm roster
- `location.dta`: `2,997` rows
  - maps firm supervisors to province/city/county
- `firm_all_info.dta`: `39,292` rows
  - extended firm background
- `law_firm.dta`: `36,934` rows
  - cleaned firm-level background table

These are potentially very valuable for:

- firm characteristics
- lawyer composition
- lawyer mobility
- CCP membership
- education composition
- director characteristics
- firm location and supervising authority

## 6. Current Empirical Data Design

There are now three partially connected units of analysis.

### 6.1 Government department-year

This is still a target design, not the most complete current output.

Desired unit:

- government defendant or government procurement buyer unit by year

Desired outcomes:

- number of administrative suits
- government win rate
- appeal rate
- whether outside counsel is retained as advisor
- whether outside counsel appears as representative

This will be the main government-side panel.

### 6.2 Law firm-year

This is now largely built.

Current merged unit:

- `law_firm_clean × panel_year`

Current outcomes include:

- civil/admin case volume by side
- win rates by case type and side
- duration measures
- lawyer roster counts
- procurement treatment variables

This is currently the most analysis-ready dataset.

### 6.3 Lawyer-year or lawyer-firm-year

This is partly built.

Current outputs allow:

- lawyer-firm-year counts
- named vs masked lawyer intensity
- lawyer master roster

But this is not yet a fully cleaned person-level causal design because anonymization remains nontrivial.

## 7. Current Processing Pipeline

### 7.1 Procurement cleaning

Script:

- `analysis/clean_legal_procurement.py`

Role:

- cleans raw procurement data
- classifies buyer units
- creates row-level and tender-level outputs
- approximates tender identifiers

### 7.2 Litigation extraction

Script:

- `analysis/extract_litigation_panels.py`

Role:

- streams SQL dump files
- parses civil and administrative result tables
- reshapes to case-side long form
- builds raw litigation firm-year panel

Important design choice:

- each case can split into plaintiff-side and defendant-side firm observations

### 7.3 Litigation dedupe

Script:

- `analysis/build_litigation_panels_from_parquet.py`

Current dedupe keys:

- `case_type`
- `case_no`
- `court_std`
- `side`
- `law_firm_clean`

Role:

- chooses a best row among duplicates using information richness
- rebuilds a cleaner firm-year panel

### 7.4 Source audit

Script:

- `analysis/audit_litigation_sources.py`

Role:

- checks schema consistency
- compares source table year vs parsed case year vs judgment year
- helps identify overlap and snapshot-like source behavior

### 7.5 Lawyer-level build

Script:

- `analysis/build_lawyer_level_datasets.py`

Role:

- explodes `lawyer_raw`
- builds lawyer-firm-year panels
- builds lawyer master
- builds firm-year lawyer counts
- distinguishes total lawyer counts from named-only lawyer counts

### 7.6 Firm-level merged build

Script:

- `analysis/build_firm_level_datasets.py`

Role:

- standardizes law firm names
- constructs firm IDs
- turns litigation firm-year into wide form
- builds procurement firm-year panel from tender winners/candidates
- merges procurement and litigation
- adds contract timing variables
- merges lawyer count measures
- merges firm background metadata from `firm_info.dta` and `supp_firm.dta`

### 7.7 End-to-end wrappers

Scripts:

- `analysis/run_litigation_pipeline.sh`
- `analysis/watch_and_sync_recovered_tables.sh`

Current end-to-end order:

1. extract litigation case-side
2. dedupe case-side
3. audit sources
4. build lawyer-level datasets
5. build firm master and merged firm-year outputs

## 8. What Has Been Done So Far

### 8.1 Procurement side

- verified the procurement raw data
- confirmed tender-level cleaning outputs already exist
- confirmed treatment city-year table exists
- used existing cleaning outputs instead of rebuilding from scratch

### 8.2 SQL recovery and restoration

- diagnosed that `ws_mscf_result_2014-2017` were missing locally
- confirmed they had previously existed in the export history
- connected to Tencent Cloud MySQL
- re-extracted the missing civil tables in chunks
- synced the recovered files back to the Archive source directory
- removed temporary local recovery copies after verification

### 8.3 Litigation full rebuild

- reran full litigation extraction from the restored Archive source
- completed full civil + admin run through 2022
- wrote full raw case-side parquet
- deduped the case-side table
- rebuilt firm-year panels
- ran a fresh source audit

### 8.4 Firm-level build

- created a unified law-firm master
- created a procurement firm-year panel from tender winners and candidate lists
- created a litigation firm-year wide panel
- merged treatment and outcomes into one firm-year panel
- added timing variables:
  - `first_contract_year`
  - `contracted_in_year`
  - `post_first_contract`
  - `event_time_first_contract`

### 8.5 Lawyer-level build

- built lawyer-firm-year panel from litigation text
- built lawyer master
- added firm-year lawyer count measures
- created named-only lawyer metrics to partially address anonymization noise

## 9. Main Problems, Bugs, and Constraints Encountered

### 9.1 Missing civil years in local Archive source

Problem:

- `ws_mscf_result_2014-2017` had disappeared from the local Archive export

Likely cause:

- an earlier move/delete accident after copying from the external drive

Resolution:

- re-extracted from Tencent Cloud
- restored to Archive

### 9.2 Tencent Cloud client compatibility

Problem:

- the default local MySQL 9 client could not connect because of `mysql_native_password` compatibility issues

Resolution:

- installed and used `mysql-client@8.4`

### 9.3 Source table year is not case year

Problem:

- the `ws_*_result_YYYY` suffix is not a reliable case-year indicator
- older cases often appear in later result tables

Consequence:

- downstream year must be based on parsed case number, not source table name

Current mitigation:

- the extraction pipeline uses parsed `case_year`
- downstream firm build filters to study window `2010-2022`

Residual issue:

- parsed case-year values outside the analysis window still exist in source data
- `2,474` litigation firm-year rows were filtered out when building the firm master datasets

### 9.4 Cross-file duplication and snapshot behavior

Problem:

- different source files can contain overlapping observations

Consequence:

- naive concatenation overstates counts

Current mitigation:

- conservative dedupe on:
  - `case_type`
  - `case_no`
  - `court_std`
  - `side`
  - `law_firm_clean`

Residual concern:

- grouped case numbers can still make the concept of a “duplicate” imperfect

### 9.5 Administrative and civil schema inconsistency

Problem:

- admin 2010 had a different number of columns from later admin tables

Resolution:

- extraction script now handles both schemas

### 9.6 Procurement runner-up information is thin

Problem:

- runner-up information is sparse

Consequence:

- a close treated vs runner-up design is currently possible only on a limited subset

### 9.7 Not all procurement winners are true private law firms

Observed procurement provider types include:

- law firms
- bar associations
- other legal-service entities

Consequence:

- treatment definition must be explicit
- the cleanest firm-level analysis should likely focus on providers classified as `law_firm`

### 9.8 Lawyer names are noisy and often anonymized

Problem:

- litigation text contains masked names, label-like tokens, and role fragments

Current mitigation:

- lawyer parsing now strips many role prefixes
- outputs include both:
  - total lawyer counts
  - named-only lawyer counts

Residual issue:

- lawyer master is still not a clean individual-level register

### 9.9 Old scripts are not a single clean reproducible pipeline

Problem:

- older Stata/R scripts in `admin_cases/` and `analysis/clean.do` are not a single modern reproducible pipeline

Current situation:

- the new Python pipeline is the current reliable backbone
- older scripts still contain useful logic and context, but are not the main production pipeline

## 10. Newly Added Useful Data

The most important new additions since the beginning of this current round are:

### 10.1 Restored full civil SQL coverage for 2014-2017

This is the biggest improvement.

Without this restoration, the main civil-firm panel would have had a severe gap exactly in the middle of the study period.

### 10.2 Lawyer background and law-firm roster data in `lawyer_list/`

These files can substantially improve the paper because they may allow:

- firm size controls
- party membership composition
- education composition
- director characteristics
- firm founding year
- supervising authority
- firm location
- lawyer mobility

### 10.3 Newly built integrated outputs

The project now has a credible central analysis spine:

- restored SQL source
- deduped case-side table
- litigation firm-year panel
- firm master
- procurement firm-year panel
- merged firm-year panel
- lawyer-level counts

This is a substantial jump from the earlier state, which was still fragmented across procurement, admin-cases cleaning, and partial litigation extraction.

## 11. Recommended Immediate Next Steps

### 11.1 Build the government department-year panel

This is the most important missing table.

Target:

- `government_unit × year`

Needed fields:

- administrative suits received
- government win rate
- appeal rate
- whether any outside lawyer appears on the defendant side
- whether the unit procured legal services that year
- whether it ever procured before

This table is the cleanest way to test the government-side hypotheses.

### 11.2 Create a regression-ready firm-year sample

Starting from:

- `law_firm_year_panel_merged.parquet`

Need to decide:

- all providers vs law firms only
- all years vs balanced event window
- all case types vs focus on civil private-market outcomes
- all lawyer counts vs named-only lawyer counts

### 11.3 Tighten firm-name harmonization

Current overlap is already useful, but there is still room to improve match quality, especially for:

- branch offices / sub-offices
- foreign firms
- slash-joined procurement names
- law-firm names with OCR or spacing artifacts

### 11.4 Decide the main causal design

Current realistic options:

- government unit-year adoption design
- firm-year not-yet-treated event study
- repeated-contract or contract-intensity design

Less promising at present:

- runner-up-only design as the main design

### 11.5 Use `lawyer_list` to enrich the firm-year panel

Potential merges:

- firm size
- founding year
- CCP share
- education composition
- director traits
- local location / supervisory authority

## 12. Suggested Working Interpretation of Current Data Readiness

### Strongest ready-to-use asset

- `law_firm_year_panel_merged.parquet`

Why:

- litigation outcomes are already merged with procurement timing
- lawyer count measures are included
- basic firm metadata are included

### Most important missing analysis table

- government department-year panel

### Best new enrichment opportunity

- merge in `lawyer_list` firm and lawyer background information

### Biggest remaining data-quality caution

- lawyer person-level identity is still noisy
- procurement runner-up information is sparse
- some name harmonization work is still left

## 13. Current Main Files to Work From

If continuing the project right now, the main files to use are:

- procurement cleaned tender level:
  - `data/temp data/legal_procurement_tender_level.dta`
- litigation full case-side:
  - `data/temp data/litigation_panels_full/litigation_case_side_dedup.parquet`
- litigation firm-year:
  - `data/temp data/litigation_panels_full/litigation_firm_year_panel.parquet`
- integrated firm-year:
  - `data/temp data/litigation_panels_full/law_firm_year_panel_merged.parquet`
- firm master:
  - `data/temp data/litigation_panels_full/law_firm_master.parquet`
- lawyer counts:
  - `data/temp data/litigation_panels_full/law_firm_year_lawyer_counts.parquet`
- lawyer background assets:
  - `lawyer_list/lawyer_new/final.dta`
  - `lawyer_list/lawyer_new/lawfirms.dta`
  - `lawyer_list/raw_data/lvsuo/firm_all_info.dta`

## 14. Bottom Line

The project is no longer in a fragmented exploratory stage. It now has a functioning and reproducible data backbone.

What is done:

- procurement cleaned
- missing civil SQL restored
- full litigation extraction completed
- dedupe completed
- firm-year and lawyer-year derivative data built
- procurement and litigation merged at the firm-year level

What remains strategically most important:

- construct the government department-year panel
- enrich the firm-year panel with the external lawyer/law-firm roster data
- choose the main identification design

The current best operational base for new commands is:

- `data/temp data/litigation_panels_full/law_firm_year_panel_merged.parquet`

