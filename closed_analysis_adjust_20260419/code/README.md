# Code README: Output-to-Code Crosswalk

本目录只包含最终分析链条使用的 R 代码。每个脚本只应读取封闭包中的分析 CSV，并生成论文表格、图形或必要的分析审计输出。

默认运行路径：

- 输入：`../data/*.csv`
- 输出：`../output/tables/*.tex` 与 `../output/figures/*.pdf`

校准版结果使用同一套脚本生成：

- 输入：`../data/data2/*.csv`
- 输出：`../output2/tables/*.tex` 与 `../output2/figures/*.pdf`

为避免覆盖基准版，校准版应通过临时运行根目录或等价的路径重定向方式生成；不要直接把 `data/data2` 覆盖到 `data`，除非明确要把校准版提升为唯一主版本。

## 1. City-Year Family

### `city_year_cs_twfe_figures_tables.R`

输入：

- `city_year_panel.csv`

输出：

- `city_year_cs_twfe_main_table.tex`
- `city_year_lawyer_share_appendix_table.tex`
- `government_win_rate_event_study.pdf`
- `appeal_rate_event_study.pdf`
- `admin_case_n_event_study.pdf`

用途：

- 生成城市-年层面的 CS staggered DID 与 TWFE 主结果。
- 生成政府胜诉率、上诉率和行政案件数量的 event-study 图。
- 生成加入律师 share 控制的 appendix table。

## 2. City-Year Robustness Family

### `admin_selection_robustness.R`

输入：

- `city_year_panel.csv`

输出：

- `city_year_selection_robustness_appendix_table.tex`

用途：

- 生成 baseline、IPW、entropy balancing 和 caliper matching 的选择性稳健性表。

### `admin_disclosure_weighted_robustness.R`

输入：

- `city_year_panel.csv`
- `admin_case_level.csv`

输出：

- `city_year_disclosure_weighted_appendix_table.tex`

用途：

- 构造行政案件披露权重，并比较基准 city-year 结果与 disclosure-weighted 结果。

### `admin_within_province_placebo.R`

输入：

- `city_year_panel.csv`

输出：

- `admin_within_province_placebo_appendix_table.tex`

用途：

- 生成同省 donor pool 与 province-by-year FE 的 placebo / robustness table。

### `admin_placebo_alternative_estimator.R`

输入：

- `city_year_panel.csv`

输出：

- `admin_placebo_alternative_appendix_table.tex`

用途：

- 生成 placebo outcomes、pre-procurement balance 和 alternative staggered estimator 的 appendix table。

## 3. Administrative Case Family

### `admin_case_level_did_fixest.R`

输入：

- `admin_case_level.csv`

输出：

- `admin_case_level_lawyer_specs_appendix_table.tex`
- `admin_plaintiff_heterogeneity_appendix_table.tex`

用途：

- 估计行政案件级 government-win DID。
- 比较 government counsel、opposing counsel 和 government counsel × post 的规格。
- 按 entity plaintiff 与 individual plaintiff 分样本估计异质性，并报告两组 procurement coefficient 的 equality-test p-value。

### `admin_cross_jurisdiction_heterogeneity.R`

输入：

- `admin_case_level.csv`

输出：

- `admin_cross_jurisdiction_heterogeneity_appendix_table.tex`

用途：

- 按法院层级和 plaintiff locality 分样本估计行政案件 government-win DID。

### `admin_case_by_cause_coefplot.R`

输入：

- `admin_case_level.csv`
- `city_year_panel.csv`

输出：

- `admin_by_cause_government_win_rate_coefplot_table.tex`
- `admin_by_cause_government_win_rate_coefplot.pdf`

用途：

- 将行政案件聚合到 city-year-cause cells。
- 对每个 `cause_group` 单独估计 city-year TWFE。
- 生成案由类别系数图与对应 `.tex` 表。

### `admin_case_appendix_tables.R`

输入：

- `admin_case_level.csv`
- `city_year_panel.csv`
- `firm_level.csv`

输出：

- `pre_procurement_balance_appendix_table.tex`

用途：

- 生成采购前城市层和律所层平衡表。

## 4. Document-Level Family

### `document_level_did_fixest.R`

输入：

- `document_level_winner_vs_loser.csv`

输出：

- `document_level_did_main_table.tex`
- `document_level_fee_winrate_appendix_table.tex`
- `document_level_attribute_heterogeneity_table.tex`
- `document_level_legal_reasoning_share_event_study.pdf`
- `document_level_log_legal_reasoning_length_chars_event_study.pdf`
- `document_level_case_fee_win_rate_event_study.pdf`

用途：

- 估计文书级 Winner × Post DID。
- 生成文书级主表、费用胜诉率 appendix table、律师属性异质性表。
- 生成说理占比、说理长度和费用胜诉率 event-study 图。

### `document_level_ddd_fixest.R`

输入：

- `document_level_winner_vs_loser.csv`

输出：

- `document_level_strict_ddd_main_table.tex`

用途：

- 使用 `court_match_key`、`prior_admin_gov_exposure` 和 `has_pre_admin_civil_case_in_court` 构造 strict DDD 样本。
- 估计 Winner × Post × Previously Represented Gov't 的三重差分。

## 5. Firm-Level Family

### `firm_level_stacked_did_fixest.R`

输入：

- `firm_level.csv`

输出：

- `firm_level_stacked_did_main_table.tex`
- `firm_level_client_mix_mechanism_table.tex`
- `firm_level_civil_win_rate_mean_event_study.pdf`
- `firm_level_civil_fee_win_rate_event_study.pdf`
- `firm_level_avg_filing_to_hearing_days_event_study.pdf`
- `firm_level_client_mix_event_study.pdf`
- `firm_level_log_firm_size_event_study.pdf`

用途：

- 估计 firm-level stacked DID。
- 生成 firm-level 主表、client mix 机制表和 5 张 event-study 图。
- 该脚本的主 DID 与 event-study 都使用 `[-5, 5]` 事件时间窗口。

## 6. Descriptive Outputs

### `admin_descriptives_appendix.R`

输入：

- `city_year_panel.csv`
- `admin_case_level.csv`
- `document_level_winner_vs_loser.csv`
- `firm_level.csv`

输出：

- `summary_statistics_appendix_table.tex`
- `procurement_adoption_timeline.pdf`

用途：

- 生成四层数据的描述统计表。
- 生成城市 procurement adoption timeline 图。

## 7. Audit Support

### `audit_city_admin_relationships.R`

输入：

- `city_year_panel.csv`
- `admin_case_level.csv`

输出：

- 不生成表或图，只打印控制台审计结果。

用途：

- 检查 2014-2020 年 `admin_case_level.csv` 聚合后是否与 `city_year_panel.csv` 的 7 个 city-year 变量严格一致。

## 8. 运行顺序

推荐运行顺序：

```bash
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
```

这些脚本之间不应依赖未保存的 R session 对象；每个脚本都应能在干净的 `Rscript` session 中独立运行。
