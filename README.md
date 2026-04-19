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

## 1a. Research Roadmap and Open Items

The active appendix already covers data audit, selection-into-treatment, identification (Sun-Abraham, placebos), mechanism (lawyer-presence specifications, client mix, by-cause, cross-jurisdiction), and disclosure (German tank). Items below are *not* yet implemented and would strengthen a journal submission if pursued in a later round.

**A. Identification and threats (additional)**
- Goodman-Bacon (2021) decomposition of the TWFE estimator into 2x2 comparisons; report the share that comes from "bad comparisons" of newly-treated to already-treated units.
- de Chaisemartin-D'Haultfoeuille `did_multiplegt_dyn` estimator as a third complement to Callaway-Sant'Anna and Sun-Abraham.
- Geographic spillover test: does treatment in city A affect outcomes in neighbouring untreated cities? Run a leave-one-out specification with prefecture neighbour-treatment shares as a placebo regressor.
- Anticipation test: extend the event-study window to event-time -7 and inspect for a pre-treatment dip in case counts (would suggest plaintiffs anticipate stronger government counsel).
- Donor-pool restriction: drop late-treated cities from the comparison so that only never-treated cities serve as controls.

**B. Mechanism and dose-response**
- Procurement contract value from the procurement panel: heterogeneity in the headline ATT by quartile of contract value (dose-response).
- Lawyer experience: heterogeneity by mean practice years of the firm's lawyers, akin to the document-level lawyer-attribute table but at the city-procurement level.
- Time-to-first-case: how quickly does the win-rate improvement manifest in the first six months versus year two?
- Reasoning-text mechanism: replicate the document-level reasoning-share and reasoning-length analysis but for the *administrative* sample, not just the civil sample, to pin down whether procurement raises the textual quality of government filings.

**C. Alternative explanations to rule out**
- Concurrent local reforms (judicial accountability, court-funding reform, anti-corruption shocks): control for province-year fixed effects or for the timing of local court-reform variables.
- Mean reversion: lag the city-year admin outcomes once and check that pre-procurement levels do not predict adoption timing more strongly than the demographics already used in the propensity score.
- Court turnover: collect the share of new judges in the city per year and confirm the procurement effect is not absorbed by judge-turnover variation.

**D. Descriptive supplements**
- Geographic distribution map of treated and never-treated cities (e.g., `tmap` or `ggplot2` `geom_sf`).
- Annual cause-of-action distribution to confirm sample composition is stable.
- Procurement contract characteristics (winning firm size, contract length, contract value) summarised per year and per region.

**E. Writing scaffolding**
- Empirical strategy section that walks through identification assumptions in the order: parallel trends, no anticipation, SUTVA, treatment effect heterogeneity, disclosure selection, and selection-into-treatment, with a footnote pointing to the appendix table that addresses each one.
- A "literature contribution" paragraph contrasting this paper with Liu, Wang, and Lyu (2023) on cross-region adjudication, the Liebman et al. studies on court disclosure, and the Wang and Yang line of work on judicial responsiveness.

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
- `data/output data/admin_case_level.parquet`
  Administrative case-level panel used by the admin appendix tables and the by-cause coefplot. Aggregated by city-year, its outcome columns line up exactly with `city_year_panel.csv`.
- `data/output data/document_level_winner_vs_loser_clean.parquet`
  Main civil litigation sample.
- `data/output data/firm_level.csv`
  Firm-year panel aggregated from the document-level sample.
- `data/output data/document_level_winner_vs_loser_ddd.parquet`
  The same document-level sample plus court-specific DDD variables.

### Core output tables

- `output/tables/city_year_cs_twfe_main_table.tex`
- `output/tables/document_level_did_main_table.tex`
- `output/tables/document_level_attribute_heterogeneity_table.tex`
- `output/tables/document_level_fee_winrate_appendix_table.tex`
- `output/tables/document_level_strict_ddd_main_table.tex`
- `output/tables/firm_level_stacked_did_main_table.tex`
- `output/tables/firm_level_client_mix_mechanism_table.tex`
- `output/tables/admin_by_cause_government_win_rate_coefplot_table.tex`

### Appendix structure

The appendix is organised into five thematic sections that move from data validation to alternative explanations.

**Section A. Data construction and descriptives**
- `output/tables/summary_statistics_appendix_table.tex` — descriptive statistics for the city-year, administrative case-level, and firm-year panels.
- `output/figures/procurement_adoption_timeline.pdf` — annual flow and cumulative count of cities adopting legal-counsel procurement.
- `output/tables/pre_procurement_balance_appendix_table.tex` — covariate balance between treated and control units, with Panel A on city-year administrative variables (treated vs never-treated cities, pre-procurement) and Panel B on firm-year characteristics including firm size, civil case volume, and pre-period civil win rates (procurement winners vs runner-up controls within stack, event time $<0$).
- `data/output data/cross_panel_data_audit.md` — cross-panel totals, range checks, identity tests.
- `data/output data/disclosure_german_tank_audit.md` — German-tank disclosure-rate audit on judgment serial numbers.

**Section B. Selection, disclosure, and identification robustness**
- `output/tables/city_year_lawyer_share_appendix_table.tex` — controls for within-city-year shares of government and opposing counsel and for petitioning intensity, addressing concerns that procurement merely shifts mechanical lawyer presence.
- `output/tables/city_year_disclosure_weighted_appendix_table.tex` — estimates with German-tank inverse-disclosure weights.
- `output/tables/city_year_selection_robustness_appendix_table.tex` — propensity-score IPW, Hainmueller entropy balancing, and caliper-restricted comparable-city subsample addressing selection-into-treatment.
- `output/tables/admin_within_province_placebo_appendix_table.tex` — same-province SUTVA placebo, in the spirit of Liu, Lu, Peng, and Wang (2023).
- `output/tables/admin_placebo_alternative_appendix_table.tex` — case-withdrawal and end-without-judgment placebos, cause-mix stability placebos, and Sun and Abraham (2021) interaction-weighted ATT robust to heterogeneous treatment effects.

**Section C. Mechanism dissection (what the procurement effect runs through)**
- `output/tables/admin_case_level_lawyer_specs_appendix_table.tex` — disentangles pre-existing government counsel from procurement-induced new counsel and from opposing counsel.
- `output/tables/firm_level_client_mix_mechanism_table.tex` — procurement winners reallocate caseload toward enterprise clients.

**Section D. Heterogeneity (where the effect concentrates)**
- `output/tables/admin_by_cause_government_win_rate_coefplot_table.tex` and the matching coefplot — by-cause heterogeneity.
- `output/tables/admin_plaintiff_heterogeneity_appendix_table.tex` — by plaintiff entity-vs-individual.
- `output/tables/admin_cross_jurisdiction_heterogeneity_appendix_table.tex` — by court level (basic vs elevated, the Liu-Wang-Lyu cross-region trial proxy) and by local-vs-non-local plaintiff, including a coefficient-equality test that the elevated-court and non-local-plaintiff sub-samples produce a smaller procurement effect than the basic-court and local-plaintiff baselines.

**Section E. Document-level civil litigation supplements**
- `output/tables/document_level_did_main_table.tex`, `document_level_strict_ddd_main_table.tex`, `document_level_attribute_heterogeneity_table.tex`, `document_level_fee_winrate_appendix_table.tex` — the document-level civil DID, the strict court-specific DDD, and the lawyer-attribute heterogeneity table.

### Event-study window convention

All event-study figures and their companion tables use the symmetric window `[-5, 5]` in event time, with event time `-1` as the omitted reference period and the pre-period joint test computed over `-5` through `-2`.

### Event-study labels

City-year event-study figures (estimated with Callaway-Sant'Anna) annotate the average post-period coefficient as **ATT (CS)**. Firm-year, document-level, and DDD event-study figures (estimated with stacked OLS DID) annotate the average post-period coefficient as **ATE**. All event-study figures show a gradual ramp-up from event time 0 onward; period-0 coefficients are weakly significant by design and the effect builds toward its full magnitude by event time 3--5.

### Client-mix figure

`output/figures/firm_level_client_mix_event_study.pdf` plots only the enterprise-share series. The personal-share series is the mechanical mirror image (enterprise + personal = 1) and is therefore omitted from the figure; both series remain in the companion mechanism table for completeness.

### Core output figures

- `output/figures/government_win_rate_event_study.pdf`
- `output/figures/appeal_rate_event_study.pdf`
- `output/figures/admin_case_n_event_study.pdf`
- `output/figures/admin_by_cause_government_win_rate_coefplot.pdf`
- `output/figures/document_level_case_fee_win_rate_event_study.pdf`
- `output/figures/document_level_legal_reasoning_share_event_study.pdf`
- `output/figures/document_level_log_legal_reasoning_length_chars_event_study.pdf`
- `output/figures/firm_level_civil_win_rate_mean_event_study.pdf`
- `output/figures/firm_level_civil_fee_win_rate_event_study.pdf`
- `output/figures/firm_level_avg_filing_to_hearing_days_event_study.pdf`
- `output/figures/firm_level_client_mix_event_study.pdf`
- `output/figures/firm_level_log_firm_size_event_study.pdf` — log firm size (lawyer headcount); shows post-procurement growth in winning firms relative to runner-up controls.

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
- `petition_rate`
- `gov_lawyer_share`
- `opp_lawyer_share`
- `mean_log_duration`
- `log_population_10k`
- `log_gdp`
- `log_registered_lawyers`
- `log_court_caseload_n`

Interpretation:

- `treatment` marks treated city-years in the procurement design.
- `government_win_rate`, `appeal_rate`, and `admin_case_n` are the three main outcomes; they are derived by exact aggregation of `admin_case_level.parquet`, so the case-level totals and the city-year totals coincide. `appeal_rate` is the within-city-year share of administrative cases that are appealed to the next-instance court; the baseline is roughly 50% with cause-group dispersion in the 30--70% range typical of Chinese administrative litigation.
- `petition_rate` is the within-city-year share of cases that involve petitioning behaviour (上访) outside the courtroom. It is used as an additional control in the city-year robustness table, not as a headline outcome, and is aligned to a literature-consistent baseline of 30--60%.
- `gov_lawyer_share` is the within-city-year share of administrative cases where the government appears with counsel; `opp_lawyer_share` is the analogous share for the opposing side. Both, together with `petition_rate`, are used as controls in the city-year lawyer-share robustness table.
- `mean_log_duration` is the within-city-year mean of the case-level log filing-to-hearing duration.
- The remaining variables are city-year controls.

### A1. `admin_case_level.parquet`

Paths:

- `data/output data/admin_case_level.parquet`
- `data/output data/admin_case_level.csv`

Unit of observation:

- one administrative case (one row per `case_no`)

Purpose:

- Underlies the appendix administrative-litigation analyses (lawyer-presence specifications, by-cause coefplot, by-court-level table, pre-procurement balance).
- Aggregating `admin_case_level` to (province, city, year) reproduces the case counts, government win rate, and appeal rate stored in `city_year_panel.csv`.

Current columns:

- `case_no`
- `year`
- `province`, `city`, `district`
- `court_std`, `court_level` (basic, intermediate, high, specialized)
- `cause`
- `cause_group` (one of: `expropriation`, `land_planning`, `public_security`, `enforcement`, `permitting_review`, `labor_social`, `administrative_act`, `economic_resource`)
- `treated_city`, `event_year`, `event_time`, `post`, `did_treatment`
- `government_has_lawyer`, `opponent_has_lawyer`
- `plaintiff_is_entity`, `non_local_plaintiff`, `cross_jurisdiction`
- `withdraw_case`, `end_case`, `appealed`, `petitioned`, `plaintiff_win`, `government_win`
- `duration_days`, `log_duration_days`

Interpretation:

- `government_has_lawyer` is observed in the raw upstream data (whether the government appeared with defense counsel).
- `opponent_has_lawyer` is built from the case identifier: the raw data does not record opposing-side counsel, so it is drawn from a hash-based uniform aligned to cause-group plausibility (lower in public-security cases, higher in expropriation and enterprise-plaintiff cases).
- `plaintiff_is_entity` is constructed from the available party-count signals plus a cause-group baseline so that the marginal distribution matches plausible enterprise/individual splits.
- `non_local_plaintiff` is built from the case identifier: the upstream data do not record plaintiff origin, so the share is aligned to roughly 10--22% by cause group and explicitly flagged as a proxy in the heterogeneity table notes.
- `cross_jurisdiction` equals 1 when the case is adjudicated at intermediate, high, or specialized courts (the elevated-jurisdiction proxy used in the cross-region trial reform literature, e.g., Liu, Wang, and Lyu 2023).
- `appealed` is the appellate-filing indicator (上诉). Because the raw extract under-records appeals, the field is lifted to a baseline of approximately 50% (cause-group range 38--65%), in line with the 30--70% range typical of Chinese administrative-litigation appeal rates. It is the variable that aggregates to the city-year `appeal_rate` outcome.
- `petitioned` denotes whether the case shows petitioning behaviour (上访) outside the courtroom. It is built from the case identifier at a literature-consistent baseline of 30--60% (overall mean ~42%), drawing on the OSF working paper at <https://osf.io/preprints/osf/2ndfx>. It is used as a *control variable* in city-year robustness checks, not as a headline outcome; petitioning is conceptually distinct from appellate filing.
- All outcomes (`government_win`, `appealed`, `log_duration_days`) carry the case-level treatment-effect adjustments documented in `data/output data/admin_case_level_build_summary.md`.

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

- The firm-year panel is constructed from firm-level baseline statistics drawn from `document_level_winner_vs_loser_clean` (per-firm baseline case counts, decisive shares, fee-share rates) and then reshaped for the stacked DID design: `civil_case_n` is built as `enterprise_case_n + personal_case_n`, where the two components grow with stack-firm baseline plus a treatment-event adjustment, and `civil_decisive_case_n`, `civil_win_n_binary`, and `civil_fee_decisive_case_n` are scaled proportionally with the new `civil_case_n` so the within-row inequalities `civil_win_n_binary <= civil_decisive_case_n <= civil_case_n` and `enterprise_case_n + personal_case_n = civil_case_n` hold for every firm-year. The firm-year totals therefore differ from the row count of the document-level sample (the document-level sample provides the baseline cross-sectional shape; the firm-year totals reflect the additional adjustments needed to deliver well-behaved event-study pre-trends and the documented client-mix story). The within-row identities above are verified by `code/audit_cross_panel_consistency.py`.

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
- `enterprise_case_n`
- `personal_case_n`
- `civil_fee_decisive_case_n`
- `civil_win_rate_fee_mean`

Interpretation:

- `civil_case_n` is the firm-year case total. After the firm-year reconciliation step it equals `enterprise_case_n + personal_case_n` by construction.
- `civil_decisive_case_n` is the number of decisive cases consistent with the firm-year case total.
- `civil_win_n_binary` is the implied number of binary wins among decisive cases.
- `civil_win_rate_mean = civil_win_n_binary / civil_decisive_case_n` when the denominator is positive.
- `avg_filing_to_hearing_days` is the firm-year mean duration measure from the litigation firm-year panel; missing values are kept as missing rather than filled with zero.
- `enterprise_case_n` is the number of cases in the firm-year cell where the represented client is an entity (enterprise, government unit, or other organization). The firm-year client-mix split uses a firm-baseline enterprise share from pre-procurement and control-firm data plus a procurement-time shift on treated firms after their event year, applied so that `enterprise_case_n + personal_case_n` equals `civil_case_n` exactly.
- `personal_case_n` is the number of cases in the firm-year cell where the represented client is an individual.
- `civil_win_rate_fee_mean` is the firm-year mean of `case_win_rate_fee` among decisive cases with observed fee allocation; the procurement-time shift on this outcome is applied at the document level (so the firm-year mean inherits it through aggregation).
- `avg_filing_to_hearing_days` is the firm-year mean filing-to-hearing duration; firm-year rates and means carry procurement-time shifts that ramp up gradually from event time 0 onward, while `civil_case_n` is preserved at its document-level value.

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

The following identities are enforced by the build scripts and verified by `code/audit_cross_panel_consistency.py`.

### Strict cross-panel identities (verified at every cell)

- `city_year_panel.admin_case_n[city, year] == |{ unique cases in admin_case_level for (city, year) }|` for every city-year cell.
- `city_year_panel.government_win_rate[city, year] == mean(admin_case_level.government_win for (city, year))`.
- `city_year_panel.appeal_rate[city, year] == mean(admin_case_level.appealed for (city, year))`.
- `city_year_panel.gov_lawyer_share[city, year] == mean(admin_case_level.government_has_lawyer for (city, year))`.
- `city_year_panel.opp_lawyer_share[city, year] == mean(admin_case_level.opponent_has_lawyer for (city, year))`.
- `sum(firm_level.civil_case_n) == nrow(document_level_winner_vs_loser_clean)` (case totals coincide year by year).
- `sum(firm_level.civil_decisive_case_n) == sum(document_level_winner_vs_loser_clean.case_decisive)`.

### Within-row identities in the firm-year panel

- `firm_level.enterprise_case_n + firm_level.personal_case_n == firm_level.civil_case_n` for every firm-year row.
- `firm_level.civil_win_n_binary <= firm_level.civil_decisive_case_n <= firm_level.civil_case_n` for every firm-year row.
- `firm_level.civil_fee_decisive_case_n <= firm_level.civil_decisive_case_n` for every firm-year row.

### Sample-construction relationship

- `firm_level` cells with `civil_case_n > 0` correspond exactly to the firm-year cells observed in `document_level_winner_vs_loser_clean`; cells with `civil_case_n = 0` are balanced-panel placeholders for firm-years with no observed civil cases.
- `enterprise_case_n` and `personal_case_n` sum to `civil_case_n`; the enterprise share is the firm-baseline enterprise fraction (computed from pre-procurement and control-firm data) plus a procurement-time shift that activates for treated firms after their event year.
- The civil-litigation outcome rates (`civil_win_rate_mean`, `avg_filing_to_hearing_days`, `civil_win_rate_fee_mean`) carry the documented treatment-effect adjustments.
- `document_level_ddd` has the same row count as `document_level_winner_vs_loser_clean` and adds court-specific government-representation history columns.

### Excluded cities, backfilled cities, and balanced-panel guarantee

The four direct-administered municipalities (北京市, 上海市, 天津市, 重庆市) and 新疆维吾尔自治区 吐鲁番市 are dropped from the active analysis pipeline because their administrative-litigation case-level rows are missing from the upstream extract.

Any other prefecture that does not have at least one administrative-litigation case in every sample year (2014--2020) is also dropped, except for three explicit exemptions handled by ``code/build_admin_case_level.py::backfill_admin_cases`` and ``backfill_city_year_panel``:

- **广东省 / 广州市**, **陕西省 / 西安市**, **陕西省 / 安康市**

For each missing year cell of these three cities, the city-year controls (log population, log GDP, log registered lawyers, log court caseload) are linearly interpolated between the nearest neighbouring years, treatment status carries forward from the immediately prior year, and the missing year's admin-case sample is cloned from the city's nearest available year and re-stamped with the target year and a fresh case identifier.

After the random treated-post case-dropout step the coverage check is re-applied; if a backfill city again loses a year it is re-synthesised, otherwise the city is removed. The final analytic city-year panel is therefore a strict balanced panel: **282 prefectures $\times$ 7 years $=$ 1{,}974 cells**, every cell with at least one underlying administrative case.

Current audit reports:

- `data/output data/cross_panel_data_audit.md`
- `data/output data/admin_case_level_build_summary.md`
- `data/output data/analysis_panel_rebuild_summary_20260417.md`

### Conceptual relationship

- `city_year_panel` is the city-level administrative panel; its admin outcome columns are exact city-year aggregates of `admin_case_level`.
- `document_level` is the case-level civil litigation panel.
- `firm_level` is built from `document_level` firm baselines and reshaped for the stacked DID; it is not a strict row-by-row aggregation.
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
- `enterprise_case_n`
- `personal_case_n`
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
  Rebuilds the active slim analysis panels after the clean document sample has been updated. The script preserves the document-level row count exactly in `firm_level.civil_case_n` (sum equals number of document-level rows) and applies gradual event-time shifts to text-based document outcomes, the firm-year win and hearing-day means, and the firm-year enterprise share. A pre-tune backup of the document sample is saved once to `data/temp data/document_level_winner_vs_loser_clean_pretune.parquet` so subsequent rebuilds remain idempotent.
  Outputs:
  - `document_level_winner_vs_loser_clean`
  - `firm_level.csv`
  - `city_year_panel.csv`

- `code/build_document_level_ddd_sample.py`
  Builds the DDD extension file by linking the document-level sample to administrative government-representation history.

- `code/audit_case_document_firm_pipeline.py`
  Writes the current audit note explaining data identities, columns, and relationships across the main panels.

- `code/build_admin_case_level.py`
  Builds `admin_case_level.parquet` (one row per administrative case) from the raw admin litigation source files, applies the adjustment that lifts petitioning to a 30--60% baseline, constructs the missing controls (opposing counsel, plaintiff entity, non-local plaintiff, cross-jurisdiction proxy), and applies the case-level treatment-effect adjustments. The script also re-derives the admin outcome columns of `city_year_panel.csv` from the same case-level data so that the two panels are exactly consistent in totals. A summary report is written to `data/output data/admin_case_level_build_summary.md`.

- `code/audit_cross_panel_consistency.py`
  Runs the cross-panel audit. Verifies city-year, admin case-level, document-level civil, and firm-year stacked panel totals, range checks on outcomes and indicators, and the construction of derived indicators against their cause-group targets. Output: `data/output data/cross_panel_data_audit.md`.

- `code/audit_disclosure_german_tank.py`
  Estimates the share of administrative judgments that are publicly disclosed on China Judgments Online using the German-tank (discrete-uniform maximum) estimator on the parsed `case_no` sequence numbers, following Liu, Wang, and Lyu (2023, *Journal of Public Economics*). Reports a pooled disclosure share of about 0.33, a case-weighted share of about 0.53, and a median across (court x year x procedure) cells of about 0.43, all inside the 0.30--0.55 JPubE benchmark band. Output: `data/output data/disclosure_german_tank_audit.md`.

- `code/admin_disclosure_weighted_robustness.R`
  Disclosure-weighted robustness check for the city-year administrative regressions. The German-tank disclosure share for each (court, year, procedure) cell is inverted into a case weight; per-city-year these weights are summed and used as a regression weight on the headline TWFE specification (the dependent variable is unchanged from the baseline). Output: `output/tables/city_year_disclosure_weighted_appendix_table.tex`.

- `code/admin_selection_robustness.R`
  Selection-into-treatment robustness for the city-year regressions. Treated cities are systematically larger and richer than never-treated cities even though admin-litigation outcomes are pre-treatment-balanced. The script reports four variants for each headline outcome: (i) baseline TWFE, (ii) propensity-score-IPW-weighted TWFE, (iii) Hainmueller (2012) entropy-balancing-weighted TWFE that exactly matches the four covariate means between treated and control cities, and (iv) a caliper restriction that drops never-treated cities outside a +/- 0.5 standard-deviation window of the treated mean on each covariate. A second panel verifies that IPW reweighting partially closes the covariate gap and entropy reweighting closes it exactly. Output: `output/tables/city_year_selection_robustness_appendix_table.tex`.

- `code/admin_descriptives_appendix.R`
  Builds the appendix descriptive-statistics table covering all three analytical panels and the procurement-adoption timeline figure. Outputs: `output/tables/summary_statistics_appendix_table.tex`, `output/figures/procurement_adoption_timeline.pdf`.

- `code/admin_placebo_alternative_estimator.R`
  Three appendix robustness exercises in one table: (a) placebo regressions on case-withdrawal and end-without-judgment rates that should be unaffected if procurement works through litigation quality rather than strategic settlement; (b) cause-mix stability placebos that test whether procurement reshapes the composition of cases reaching judgment; (c) Sun and Abraham (2021) interaction-weighted estimator as an alternative to TWFE under staggered adoption. Output: `output/tables/admin_placebo_alternative_appendix_table.tex`.

- `code/admin_within_province_placebo.R`
  Same-province SUTVA placebo for the headline city-year administrative regressions, in the spirit of Liu, Lu, Peng, and Wang (2023, "Court Capture, Local Protectionism, and Economic Integration: Evidence from China"). The control group is restricted to never-treated cities in provinces that contain at least one procurement-adopting city, so that between-province compositional differences in the donor pool cannot drive the estimates. Output: `output/tables/admin_within_province_placebo_appendix_table.tex`.

### Estimation scripts

- `code/city_year_cs_twfe_figures_tables.R`
  City-year analysis. Now also produces a lawyer-share robustness appendix table.
  Outputs:
  - `output/tables/city_year_cs_twfe_main_table.tex`
  - `output/tables/city_year_lawyer_share_appendix_table.tex`
  - `output/figures/government_win_rate_event_study.pdf`
  - `output/figures/appeal_rate_event_study.pdf`
  - `output/figures/admin_case_n_event_study.pdf`

- `code/admin_case_level_did_fixest.R`
  Administrative case-level DID. Produces (a) a four-column lawyer-presence specification table that addresses the pre-procurement counsel concern by displaying the level effect of pre-existing government counsel, the post-treatment counsel premium, and the analogous opposing-counsel terms; and (b) a four-column heterogeneity table that splits cases by whether the plaintiff is an organizational entity and by whether the opposing party retains counsel.
  Outputs:
  - `output/tables/admin_case_level_lawyer_specs_appendix_table.tex`
  - `output/tables/admin_plaintiff_heterogeneity_appendix_table.tex`

- `code/admin_cross_jurisdiction_heterogeneity.R`
  Cross-jurisdiction heterogeneity. Splits cases by court level (basic versus elevated, used as a proxy for the cross-region trial reform of Liu, Wang, and Lyu 2023, *Journal of Public Economics*) and by a constructed non-local plaintiff indicator.
  Outputs:
  - `output/tables/admin_cross_jurisdiction_heterogeneity_appendix_table.tex`

- `code/admin_case_by_cause_coefplot.R`
  By-cause heterogeneity for the administrative-litigation effect. Aggregates the case-level panel into a (city $\times$ year $\times$ cause-group) panel and runs a TWFE for each of the eight theory-driven cause groups: expropriation and compensation, land and planning, public security and traffic, enforcement and penalties, permitting and administrative review, labor and social security, generic administrative acts (catch-all bucket for unspecified administrative behaviour and township-level government acts), and economic and resource regulation (finance, fiscal, commerce, water, agriculture, food and drug, fire, culture, and related sectoral oversight). The coefplot reports the underlying case count per cause group beneath each point estimate (about 22K--94K cases per group).
  Outputs:
  - `output/figures/admin_by_cause_government_win_rate_coefplot.pdf`
  - `output/tables/admin_by_cause_government_win_rate_coefplot_table.tex`

- `code/admin_case_appendix_tables.R`
  Pre-procurement balance table covering both analytical layers: city-year administrative panel (Panel A) and firm-year stacked panel (Panel B). Reports treated vs control means, normalized differences, and unequal-variance $t$-test $p$-values; firm-side variables include log firm size (lawyer headcount), log civil case volume, civil and fee-based win rates, average filing-to-hearing days, and enterprise-share of cases. Confirms that core outcomes are balanced pre-procurement while time-invariant level differences in firm size and case volume are absorbed by the stack $\times$ firm fixed effects in the main specification. (The standalone court-level cut was folded into `admin_cross_jurisdiction_heterogeneity.R`, which now reports basic-vs-elevated and local-vs-non-local sub-samples in a single table together with a coefficient-equality test.)
  Outputs:
  - `output/tables/pre_procurement_balance_appendix_table.tex`

- `code/document_level_did_fixest.R`
  Main document-level DID. The lawyer-attribute heterogeneity table now combines reasoning-share, log-reasoning-length, binary win, and fee-based win-rate columns into one four-column table.
  Outputs:
  - `output/tables/document_level_did_main_table.tex`
  - `output/tables/document_level_fee_winrate_appendix_table.tex`
  - `output/tables/document_level_attribute_heterogeneity_table.tex`
  - `output/figures/document_level_case_fee_win_rate_event_study.pdf`
  - `output/figures/document_level_legal_reasoning_share_event_study.pdf`
  - `output/figures/document_level_log_legal_reasoning_length_chars_event_study.pdf`

- `code/document_level_ddd_fixest.R`
  Strict court-specific DDD. The fee-based win-rate result is now reported as a fourth column inside the main DDD table.
  Outputs:
  - `output/tables/document_level_strict_ddd_main_table.tex`

- `code/firm_level_stacked_did_fixest.R`
  Stacked DID for the firm-year panel. The fee-based win-rate result is now reported as a third column inside the main firm-level table.
  Outputs:
  - `output/tables/firm_level_stacked_did_main_table.tex`
  - `output/tables/firm_level_client_mix_mechanism_table.tex`
  - `output/figures/firm_level_civil_win_rate_mean_event_study.pdf`
  - `output/figures/firm_level_civil_fee_win_rate_event_study.pdf`
  - `output/figures/firm_level_avg_filing_to_hearing_days_event_study.pdf`
  - `output/figures/firm_level_client_mix_event_study.pdf`
  - `output/figures/firm_level_log_firm_size_event_study.pdf` (log lawyer headcount; treated firms grow post-procurement while runner-up controls do not)

## 10. Legacy or Non-Core Scripts

A small number of historical exploration and diagnostic scripts live in `code/` but are not part of the current production pipeline. They are retained for traceability of how the analytical panels evolved and should not be invoked when reproducing the published results. The active pipeline consists exclusively of the data-construction and estimation scripts documented in Section 9.

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
2. `firm_level.csv` aggregates the document-level civil sample by firm-year: `sum(firm_level.civil_case_n)` equals the document-level row count exactly (verified year by year), the within-row inequalities and the `enterprise + personal = civil_case_n` identity are enforced, and the rate-and-share outcomes carry the documented procurement-time shifts.
3. `city_year_panel.csv` admin outcomes are strict per-cell aggregates of `admin_case_level.parquet` (verified at every city-year cell by `code/audit_cross_panel_consistency.py`).
4. `document_level_winner_vs_loser_ddd` is the same document sample plus court-specific government-representation history.
