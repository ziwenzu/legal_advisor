# Closed Analysis Package: Universal README

更新日期：2026-04-20

本文件夹是法律顾问采购项目的封闭分析包。它只服务于最终分析链条：读取分析数据，运行 R 脚本，生成论文表格与图形。它不是原始数据归档包，也不是完整仓库镜像。

本项目是人工生成的测试研究。使用本包时，审计重点不是判断数据是否来自真实世界，而是判断分析链条是否内部一致、可复现、变量定义是否自洽，以及跨层数据关系是否符合研究设计。

## 1. 研究问题

本研究考察地方政府采购法律顾问后，是否改变政府在行政诉讼中的表现，以及这种采购冲击是否外溢到相关律所在民事案件中的表现、文书特征和客户结构。

核心问题分为四层：

1. 城市-年层面：采购发生后，政府胜诉率、上诉率和行政案件数量是否变化。
2. 行政案件层面：政府是否更容易获胜，这种变化是否随律师参与、法院层级、地域关系、原告类型和案由类别而异。
3. 文书层面：中标律所在民事案件中的说理占比、说理长度、胜诉概率和费用胜诉率是否变化。
4. 律所-年层面：中标律所在民事案件中的胜诉率、审理时长、费用胜诉率、客户结构和规模是否变化。

对应的主要假设是：

1. 政府采购法律顾问后，政府胜诉率上升，上诉率下降。
2. 采购冲击可能通过法律专业能力、政府代理经验或法院熟悉度，外溢到中标律所的民事案件表现。
3. 如果律所此前已在同一法院代理过政府行政案件，采购后的民事案件优势应更强；这是文书级 DDD 规格的识别逻辑。
4. 律所层面的业务结构可能随采购发生变化，因此需要在 firm-year 面板上同时检查客户结构、审理时长和规模变化。

## 2. 文件夹边界

本封闭包只包含分析链条所需对象：

- `data/`：分析 CSV（4 个文件）。
- `code/`：只保留用于估计、制表、作图和审计的 R 代码（32 个 `.R` 脚本）。
- `output/tables/` 与 `output/figures/`：分析结果（35 张 `.tex` 表 + 1 个 `.txt` 助记数字 + 25 张 `.pdf` 图）。

本封闭包不包含：

- 原始 DGP 脚本。
- 噪音调参脚本。
- sandbox、archive、old versions。
- parquet 或中间数据。
- 未进入最终分析链条的临时对象。

DGP 和调参代码保存在外部目录（不在本封闭包内）：

`code/code_dgp_tuning_external/`（如果存在；当前已剔除）。

因此，本文件夹中的 `code/` 应被理解为 analysis-only code。

## 3. 当前版本

本封闭包目前只保留一套合并版本（曾经的"校准版"已提升为唯一主版本，原"基准版"`data/` 与原"校准版"`data/data2/` 已合并到当前 `data/`，输出统一写入 `output/`）。这一版本针对两个问题做了校准：

1. 行政案件数量过少的 city-year cells（避免大城市某些年份只有 1 个案件）；
2. 部分城市在第一份政府法律顾问合同结束后重新采购并更换顾问律所（一次采购招标对应一个 `stack_id`，每个 stack 只有一个 winner 和 3-10 个 losers）。

构造细节见 §7。当前版本包含：

- 4 个分析 CSV（`data/`）。
- 32 个 R 脚本（`code/`，按 3 个 tier 组织，详见 §8）。
- 35 张 `.tex` 表（含 1 个 `.txt` 助记数字）和 25 张 `.pdf` 图（`output/tables/`、`output/figures/`）。

## 4. 数据文件总览

`data/` 下的 4 个分析 CSV：

| 文件 | 行数 | 列数 | 观测单位 | 年份 |
|---|---:|---:|---|---|
| `city_year_panel.csv` | 2,256 | 15 | `province × city × year` | 2013-2020 |
| `admin_case_level.csv` | 659,171 | 27 | 行政案件 | 2014-2020 |
| `document_level_winner_vs_loser.csv` | 1,351,683 | 28 | 文书级案件观察 | 2010-2020 |
| `firm_level.csv` | 21,615 | 22 | `stack_id × firm_id × year` | 2010-2020 |

city-year 的 2014-2020 行政案件数分布为：

- 最小值：118
- 第 10 百分位：146
- 中位数：301
- 均值：333.927
- 最大值：2,079

经过校准的行政案件数下限避免了大城市某些年份只有 1 个案件这种异常；当前最小值为 118，且没有 `admin_case_n <= 105` 的 city-year。

## 5. 各数据集说明

### 5.1 `city_year_panel.csv`

观测单位：`province × city × year`

主键：`province`, `city`, `year`

主要用途：

- city-year CS DID 与 TWFE 主表。
- city-year event-study 图。
- 选择性、披露权重、同省 placebo 和替代估计量等稳健性表。

核心变量：

- 处理变量：`treatment`
- 主要结果：`government_win_rate`, `appeal_rate`, `admin_case_n`
- 其他聚合结果：`petition_rate`, `gov_lawyer_share`, `opp_lawyer_share`, `mean_log_duration`
- 城市控制变量：`log_population_10k`, `log_gdp`, `log_registered_lawyers`, `log_court_caseload_n`

重要说明：

- `2013` 只在 city-year 面板中保留。
- 保留 `2013` 的目的，是让 Callaway and Sant'Anna staggered DID 对 2014 年首次 treated 城市仍有 pre-period；不要求 `admin_case_level.csv` 中存在 2013 年行政案件。
- 当前城市控制变量使用当年值。若未来希望更严格地使用预定控制，最应优先滞后的是 `log_court_caseload_n`，因为它最接近诉讼结果本身；人口、GDP 和律师数量也可统一滞后一年来保持设定一致。

### 5.2 `admin_case_level.csv`

观测单位：行政案件

主键：`case_no`

主要用途：

- 行政案件级 DID。
- 行政案件律师规格表。
- 原告类型异质性。
- 法院层级和地域异质性。
- 案由类别系数图。
- 行政案件相关 appendix tables。

核心变量：

- 处理与时点：`treated_city`, `event_year`, `event_time`, `post`, `did_treatment`
- 主要结果：`government_win`, `appealed`, `petitioned`
- 律师参与：`government_has_lawyer`, `opponent_has_lawyer`
- 个案控制：`plaintiff_is_entity`, `non_local_plaintiff`, `cross_jurisdiction`
- 程序变量：`withdraw_case`, `end_case`
- 法院变量：`court_std`, `court_level`
- 案由变量：`cause`, `cause_group`
- 时长变量：`duration_days`, `log_duration_days`

重要说明：

- `did_treatment = treated_city × post` 在基准版和校准版中都严格成立。
- `event_time` 是相对 procurement year 的案件时点变量。
- 当前行政案件样本没有律所名称或 `firm_id`。行政端的律师机制通过 `government_has_lawyer` 与 `opponent_has_lawyer` 表示，而不是通过具体律所身份表示。
- 如果未来要在行政案件层面追踪“哪个律所成为政府顾问”，需要新增 schema 或从外部全集重新构造行政案件的律所字段；当前封闭包没有擅自增加列。
- `cross_jurisdiction = 1` 在当前数据中等价于 `court_level` 属于 intermediate、high 或 specialized。它在表格中作为非基层法院或跨区域审理安排的代理变量。

校准版中，treated city 的 `government_has_lawyer` 随事件时间大体上升：

| event time | -4 | -3 | -2 | -1 | 0 | 1 | 2 | 3 | 4 | 5 | 6 |
|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| share | 0.455 | 0.456 | 0.470 | 0.499 | 0.557 | 0.595 | 0.626 | 0.639 | 0.655 | 0.689 | 0.611 |

### 5.3 `document_level_winner_vs_loser.csv`

观测单位：文书级案件观察

主键：`case_uid`

主要用途：

- document-level DID 主表。
- fee win rate appendix table。
- lawyer attribute heterogeneity table。
- document-level event-study 图。
- document-level DDD 主表。
- 描述统计。

核心变量：

- 处理与时点：`treated_firm`, `event_year`, `event_time`, `post`, `did_treatment`
- 结果变量：`legal_reasoning_share`, `log_legal_reasoning_length_chars`, `case_win_binary`, `case_win_rate_fee`
- 案件属性：`court`, `cause`, `side`, `case_decisive`
- 个案控制：`opponent_has_lawyer`, `plaintiff_party_is_entity`, `defendant_party_is_entity`
- 律师属性：`lawyer_gender`, `lawyer_practice_years`, `lawyer_ccp`, `lawyer_edu`
- 律所和 stack：`stack_id`, `firm_id`, `law_firm`
- DDD 专用字段：`court_match_key`, `prior_admin_gov_exposure`, `has_pre_admin_civil_case_in_court`

重要说明：

- `did_treatment = treated_firm × post` 在基准版和校准版中都严格成立。
- 在校准版中，`stack_id` 表示一次城市采购招标；每个 stack 只有一个 winner firm，并配有 3-10 个 loser firms。
- 文书级校准版只保留这些 tender participants 对应的民事案件，但选择 losers 时优先选择有更多 civil documents 的本地或本省律所；当前校准版保留 1,351,683 条文书，占基准文书样本的 73.9%。
- 每个 stack 的每个 participant 都至少有一条民事文书。`winner_firm` 的含义是该次 tender 的中标顾问律所，不要求它一定是同一 stack 中文书数量最多的律所；loser firms 可以有更多 civil documents。
- `lawyer_gender` 已简化为数值变量，`1 = female`, `0 = male`，无缺失。
- `lawyer_edu` 已简化为数值变量，`1 = others`, `2 = associate`, `3 = college`, `4 = master`, `5 = PhD`，无缺失。
- 当前文件把原 DID 文书数据和 DDD 所需字段合并在同一个 CSV 中，因此不再保留两份重复的 document-level 文件。
- DDD 脚本只额外调用 `court_match_key`, `prior_admin_gov_exposure`, `has_pre_admin_civil_case_in_court` 三列；其它列与 DID 文书分析共享。

`court_match_key` 的含义：

- 它是标准化后的法院匹配键，用于把民事文书中的 `court` 与行政案件法院口径对齐。
- 它用于构造 DDD 的 same-court prior exposure、court-by-year fixed effects 和 firm-by-court 相关的聚类或支持样本。
- 它表示法院所在地或法院单元，不表示 procurement city。
- 因此，`court_match_key` 不应被要求与行政案件中的 `province` 和 `city` 逐行相同。跨区域审理、异地法院和法院管辖层级变化都会使案件归属城市与审理法院地点不同。

### 5.4 `firm_level.csv`

观测单位：`stack_id × firm_id × year`

主键：`stack_id`, `firm_id`, `year`

主要用途：

- firm-level stacked DID 主表。
- client mix mechanism table。
- firm-level event-study 图。

核心变量：

- 处理与时点：`treated_firm`（0/1），`event_year`，`event_time`，`did_treatment`
- 主要结果：`civil_win_rate_mean`, `avg_filing_to_hearing_days`, `civil_win_rate_fee_mean`
- 计数变量：`civil_case_n`, `civil_win_n_binary`, `civil_decisive_case_n`, `civil_fee_decisive_case_n`
- 客户结构：`enterprise_case_n`, `personal_case_n`
- 规模：`firm_size`
- 律所与 stack 标识：`stack_id`, `firm_id`, `law_firm`, `winner_firm`, `control_firm`

`winner_firm` 字段说明：

- `winner_firm` 是 stack 级别的字符串标识，存储该 stack 中标顾问律所的中文名，并在该 stack 的所有行（每个 participant × 每个年份）中重复。它**不是** 0/1 二元指示。
- firm-level 的 0/1 治疗指示变量是 `treated_firm`（中标=1，loser=0）；其互补量是 `control_firm = 1 - treated_firm`。
- 任何把 `winner_firm` 当作 binary 使用的脚本都会出错；应统一用 `treated_firm` / `control_firm`。

重要说明：

- `did_treatment = treated_firm × 1[event_time >= 0]` 在基准版和校准版中都严格成立。
- firm-level 主 DID 和 firm-level event-study 都使用 `[-5, 5]` 事件时间窗口。
- 在校准版中，`stack_id` 是 procurement tender 的唯一标识；每个 stack 恰好有 1 个 `winner_firm`，该 winner 是该城市在该合同期的政府法律顾问，其他 3-10 个 firm 是 loser/control firms。
- 如果一个城市在 2014-2020 年间发生合同轮换，则该城市会出现 2-3 个 stacks，对应 2-3 个不同 contractor。没有轮换的城市只有 1 个 stack 和 1 个 contractor。
- `civil_win_n_binary <= civil_decisive_case_n` 必须成立，因为二元胜诉数只是 decisive cases 中获胜的子集。
- `civil_fee_decisive_case_n <= civil_decisive_case_n` 必须成立，因为 fee win rate 只在 decisive cases 且费用分配可观察的子样本中定义。它不要求等于 decisive cases，也不等于简单把 fee rate 大于等于 0.5 当作所有 decisive cases 的胜诉。

## 6. 跨层等价关系

本研究至少需要交代三组关系。第一组是严格聚合等价；第二组是文书和 firm 面板之间的守恒关系；第三组是 DDD 在文书级样本内部的额外识别关系。

### 6.1 行政案件级到城市-年面板

在 2014-2020 的重叠年份中，`city_year_panel.csv` 中以下 7 个变量必须等于 `admin_case_level.csv` 按 `province × city × year` 聚合后的值：

- `government_win_rate = mean(government_win)`
- `appeal_rate = mean(appealed)`
- `admin_case_n = N(case)`
- `petition_rate = mean(petitioned)`
- `gov_lawyer_share = mean(government_has_lawyer)`
- `opp_lawyer_share = mean(opponent_has_lawyer)`
- `mean_log_duration = mean(log_duration_days)`

当前审计结果：1,974 个 (city, year) 单元在 city panel 与 admin panel 间一一对应，最大绝对差 `5.329e-15`，仅是浮点精度误差。`2013` 不参与这一等价检查，因为 admin case 文件从 2014 年开始。

### 6.2 文书级到律所-年面板

`document_level_winner_vs_loser.csv` 与 `firm_level.csv` 不是逐行等价，而是在 matched stack 设计下通过 `stack_id × firm_id × year` 聚合联系起来。当前样本只保留被选入城市招标 stack 的 winner 和 loser firms 对应的文书观察，并满足：

- `sum(firm_level.civil_case_n) = 1,351,683 = nrow(document_level_winner_vs_loser)`
- `sum(firm_level.civil_decisive_case_n) = 971,341 = sum(document_level.case_decisive)`
- `sum(firm_level.civil_win_n_binary) = 572,588`
- `sum(firm_level.civil_fee_decisive_case_n) = 687,599`
- `enterprise_case_n + personal_case_n = civil_case_n`
- 若 `civil_decisive_case_n > 0`，则 `civil_win_rate_mean = civil_win_n_binary / civil_decisive_case_n`

每个 procurement stack 只保留一个中标律所和 3-10 个竞争律所，非 tender participants 的文书观察不进入文书和 firm-level 分析。

### 6.3 DDD 的文件内关系

DDD 使用同一个 `document_level_winner_vs_loser.csv` 文件，但额外依赖三列：

- `court_match_key`
- `prior_admin_gov_exposure`
- `has_pre_admin_civil_case_in_court`

DDD 支持样本保留规则是：

- 删除 `court_match_key` 为空的行。
- 保留 `prior_admin_gov_exposure == 0` 或 `has_pre_admin_civil_case_in_court == 1` 的行。

DDD 的识别变异来自同一法院中已有 prior administrative-government exposure 的 firm-court rows。这个关系是文书文件内部的支持样本和交互项关系，不要求另建一个独立 DDD 数据文件。

## 7. 数据校准说明

当前 `data/` 是在最初基准样本之上做了两类校准的合并版本：

1. 行政案件数量过少的 city-year cells（避免大城市某些年份出现 1 个案件这种异常）。
2. 模拟部分城市在第一份政府法律顾问合同结束后重新采购并更换顾问律所。

### 7.1 行政案件数量调整

`admin_case_level.csv` 把 2014-2020 的每个 city-year 行政案件数设置为至少 100，并让发达城市和律师资源更多的城市拥有更多案件。新增行政案件来源包括：

- 从外部行政案件全集回补的真实 raw-style rows。
- 在原始全集仍不足的 city-year 中，基于同城市或同类城市行政案件分布进行重抽样补齐的 rows。

补齐后仍保持：

- `case_no` 唯一。
- `did_treatment = treated_city × post`。
- city-year 与 admin-case 的 7 个聚合变量严格对齐（见 §6.1）。
- `admin_case_level.csv` 列名和列顺序不变。

### 7.2 合同轮换调整

`firm_level.csv` 与 `document_level_winner_vs_loser.csv` 模拟部分城市在第一份政府法律顾问合同 3-4 年后重新采购并更换顾问律所。核心口径是：一次采购招标对应一个 `stack_id`，该 stack 中只有一个 winner，其他参与招标的律所是 losers。

实施规则：

- 只影响约 20% 的 treated cities。
- 经济更发达、律师资源更多的城市更可能发生合同轮换。
- 没有轮换的城市只有 1 个 contractor；发生轮换的城市在 2014-2020 年间有 2-3 个 contractors。
- 每次招标生成 1 个 stack；轮换几次就对应几次采购招标，也就对应几个 city-level stacks。
- 新中标律所必须在城市、省份或分所地点上有合理联系。
- 同一城市不同合同期的中标律所不能重复。
- 每个 stack 配 3-10 个 competitors。
- competitors 不得是该城市任一合同期的 winner。
- 文书级和 firm-level 只保留这些 tender participants 的民事案件和 firm-year cells。

当前合同轮换审计：

- 43 个城市发生轮换，占 217 个 treated cities 的 19.8%。
- 174 个城市有 1 个 contractor，37 个城市有 2 个 contractors，6 个城市有 3 个 contractors。
- 总计 266 个 procurement stacks，也就是 266 次城市采购招标。
- 每个 stack 恰好 1 个 winner，且同一 `city × event_year` 没有多个 winner。
- loser 数量范围为 3-10；当前分布为 87 个 stacks 有 3 个 losers，88 个 stacks 有 6 个 losers，91 个 stacks 有 10 个 losers。
- 每个 stack participant 都有民事文书；当前 1,965 个 participants 中没有 zero-document firm。
- `winner_firm` 是 stack-级别字符串标识（中标顾问律所中文名），不是 0/1 binary（详见 §5.4）。0/1 治疗指示请用 `treated_firm` / `control_firm`。
- firm-level 的所有 217 个 contractor cities 都能在 `city_year_panel.csv` 中找到对应 `province × city`。

stack 已明确为 tender-level，因此 firm-level 估计应解释为 procurement-tender stacked DID：同一 stack 内比较一个中标顾问律所与同次招标中的 loser firms。若未来要让合同中断、续约和再次采购成为核心论文机制，可以进一步开发 spell-level estimator，但当前数据已经满足"一个 stack = 一次招标 = 一个 winner + 多个 losers"的基本结构。

## 8. 代码与输出 crosswalk

完整 output-to-code crosswalk（含每个脚本的输入、输出、估计量、对应假设、main 还是 appendix）见 `code/README.md`。下表是简化总览，按 3 个 tier 组织：

### Tier 1. Headline pipeline（14 脚本）

| 分析家族 | R 脚本 | 主要输出 |
|---|---|---|
| City-year 主结果 | `city_year_cs_twfe_figures_tables.R` | city-year 主表、律师 share appendix、3 张 city-year event-study 图 |
| City-year 稳健性（权重 + caliper + balance + CS panel） | `admin_selection_robustness.R` | selection robustness table |
| City-year 披露权重 | `admin_disclosure_weighted_robustness.R` | disclosure-weighted appendix table |
| City-year placebo | `admin_within_province_placebo.R`, `admin_placebo_alternative_estimator.R` | within-province placebo、替代估计量 appendix table |
| 行政案件 DID | `admin_case_level_did_fixest.R` | lawyer specs、plaintiff heterogeneity tables |
| 行政案件异质性 | `admin_cross_jurisdiction_heterogeneity.R`, `admin_case_by_cause_coefplot.R` | jurisdiction heterogeneity table、cause coefplot table/figure |
| 平衡表 | `admin_case_appendix_tables.R` | pre-procurement balance table |
| 文书级 DID | `document_level_did_fixest.R` | document DID 主表、fee appendix、attribute heterogeneity、3 张 event-study 图 |
| 文书级 DDD（含 two-way 交互） | `document_level_ddd_fixest.R` | strict DDD main table |
| 律所层 DID | `firm_level_stacked_did_fixest.R` | firm main table、client mix table、5 张 event-study 图 |
| 描述统计 | `admin_descriptives_appendix.R` | summary statistics、procurement adoption timeline |
| 审计 | `audit_city_admin_relationships.R` | 只打印 city/admin 聚合审计，不生成表图 |

### Tier 2. Frontier-DID estimator 与 inference diagnostics（5 脚本）

| 检验 | R 脚本 | 主要输出 |
|---|---|---|
| TWFE / CS / SA / Borusyak-Jaravel-Spiess | `city_year_alternative_estimators.R` | alternative estimators table |
| de Chaisemartin-D'Haultfoeuille 负权重 | `city_year_negative_weights_diagnostic.R` | negative weights diagnostic table |
| Rambachan-Roth Honest DID 区间 | `city_year_honest_did.R` | honest DID table + 3 张 sensitivity figures |
| 1,000 次随机化 + 9,999 次 wild bootstrap | `city_year_randomization_inference.R` | randomization inference table + 1 张 distribution figure |
| Goodman-Bacon 拆解（彩色） | `city_year_bacon_decomposition.R` | Bacon decomposition table + 1 张 colour scatter figure |

### Tier 3. Mechanism / SUTVA / 家族 p / 选择 / 解读（13 脚本）

| 检验 | 对应假设 | R 脚本 | 主要输出 |
|---|---|---|---|
| Selection vs Quality（all / non-withdrawn / merits-decided） | H1 vs H2 | `admin_mechanism_selection_vs_quality.R` | mechanism table |
| Plaintiff composition shifts | H2 | `admin_mechanism_plaintiff_entry.R` | mechanism table |
| High vs low political sensitivity | H1+H2 mix | `admin_mechanism_cause_sensitivity.R` | mechanism table |
| Pure-private civil cases placebo | H3 strict | `document_mechanism_pure_private_placebo.R` | mechanism table |
| Event study by lawyer attribute | H3 channel | `document_mechanism_lawyer_attribute_event_study.R` | 4 张 attribute event-study figures |
| Reasoning length vs total length 分解 | H3 vs stylistic | `document_mechanism_reasoning_decomposition.R` | mechanism table |
| Loser-in-Winner-Court spillover | SUTVA 4.1 | `firm_sutva_same_court_spillover.R` | SUTVA table |
| Same-province neighbour spillover | SUTVA 4.2 | `city_sutva_neighbor_treated.R` | SUTVA table |
| Carry-over via lagged treatment | SUTVA 4.3 | `firm_sutva_carryover_lag.R` | SUTVA table |
| fect period-wise ATT relative to exit + carry-over test | SUTVA / exit | `firm_fect_carryover_exit.R` | fect table + 3 张 exit-period figures |
| Romano-Wolf + Bonferroni-Holm family-wise | multiple-testing | `family_wise_pvalues.R` | family-wise table |
| Pre-period winner selection logit (within-stack) | selection | `firm_winner_selection_logit.R` | logit table |
| Substantive magnitude (cases / percentile) | interpretation | `back_of_envelope_substantive.R` | substantive-magnitude table + .txt helper |

所有脚本只读取 `data/` 下的 CSV 分析数据，不读取 parquet、中间数据或外部调参文件。脚本通过 `get_root_dir()` 自动解析项目根目录（脚本路径的上一级），因此不需要任何路径硬编码。

## 9. 如何复现结果

从封闭包根目录运行下列 32 个脚本（推荐顺序，按 tier 分组）：

```bash
# Tier 1 — Headline pipeline
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

# Tier 2 — Frontier estimator + inference diagnostics
Rscript code/city_year_alternative_estimators.R
Rscript code/city_year_negative_weights_diagnostic.R
Rscript code/city_year_honest_did.R
Rscript code/city_year_randomization_inference.R
Rscript code/city_year_bacon_decomposition.R

# Tier 3 — Mechanism / SUTVA / family-wise / selection / interpretation
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

执行环境：

- R 4.4 或更新；以下包必须装好：`data.table`、`fixest`、`did`、`stringr`、`fwildclusterboot`、`didimputation`、`TwoWayFEWeights`、`HonestDiD`、`bacondecomp`、`fect`。
- macOS / Linux locale 必须能解析 UTF-8 中文。建议每次运行前临时设 `LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8`，否则 `data.table::fread` 不带 `encoding="UTF-8"` 时可能出现合并键编码不一致导致 0 行 join。
- 总 R 运行时间在该机器上约 2-3 分钟（实际单次成功 R 时间累计 ≈ 95 秒）。

输出位置：

- `output/tables/*.tex`：35 张 `.tex` 表 + 1 个 `.txt` 数字摘录（`back_of_envelope_substantive.txt`）
- `output/figures/*.pdf`：25 张 `.pdf` 图

每个脚本通过 `get_root_dir()` 解析项目根目录（脚本所在 `code/` 的上一级），因此可以直接 `Rscript code/<script>.R` 在项目根目录下运行，不需要任何 symlink 或临时根目录。

## 10. 主要结果快照

City-year 主表：

- Government win rate：CS `0.049***`，TWFE `0.036***`
- Appeal rate：CS `-0.116***`，TWFE `-0.062***`
- Administrative case numbers：CS `-91.450***`，TWFE `-23.098**`

Administrative court-level heterogeneity:

- Basic courts：Treatment × Post `0.044***`
- Elevated courts：Treatment × Post `0.020**`
- Equality-test p-value：`0.021`

Firm-level 主表：

- Civil win rate：`0.053***`
- Average filing-to-hearing days：`-8.488***`
- Civil fee win rate：`0.076***`

Document-level DID 主表：

- Winner × Post：reasoning share `-0.022***`
- Winner × Post：log reasoning length `-0.232***`
- Winner × Post：case win binary `0.039***`
- Winner × Post：fee-specification reasoning share `-0.019***`
- Winner × Post：fee-specification log reasoning length `-0.205***`
- Winner × Post：case fee win rate `0.032***`

Document-level DDD 主表（规格已包含 Winner × Prior 与 Post × Prior 两个 two-way 交互；这两个二阶项不进入展示行，但消除了之前由它们污染的三重交互）：

- Winner × Post × Previously Represented Gov't：reasoning share `-0.001`
- Winner × Post × Previously Represented Gov't：log reasoning length `-0.274***`
- Winner × Post × Previously Represented Gov't：case win binary `-0.016`
- Winner × Post × Previously Represented Gov't：case fee win rate `-0.047`

最终 event-study 审计显示，用户指定的文书级和律所级动态图均满足 joint pretrend `p > 0.1`。事件时间 0 和 1 的系数允许较弱；事件时间 2-5 的系数方向与假设一致，并在常规水平上显著：

- Document reasoning share：pretrend `p = 0.144`，post 系数为负且显著。
- Document log reasoning length：pretrend `p = 0.457`，post 系数为负且显著。
- Document fee win rate：pretrend `p = 0.920`，post 系数为正且显著。
- Firm civil win rate：pretrend `p = 0.663`，post 系数为正且显著。
- Firm average filing-to-hearing days：pretrend `p = 0.245`，post 系数为负且显著。
- Firm client mix：pretrend `p = 0.196`，enterprise share post 系数为正且显著。
- Firm log firm size：pretrend `p = 0.730`，post 系数为正且显著。

注意：city-year 主表中的 TWFE 主结果均显著；个别 appendix 规格加入额外律师 share 控制后，行政案件数量列可能不再显著，这应被理解为扩展控制下的稳健性结果，而不是主表结论。

## 11. 表格和图形风格

当前 `.tex` 表格遵循统一模板：

- 使用 `threeparttable`。
- 表格标题、列名和 note 使用论文语言，而不是代码语言。
- 行顺序通常为：核心系数、观测数、`R^2`、控制变量、固定效应、聚类或注释。
- 控制变量行使用 `City Controls`, `Case Controls`, `Lawyer Controls` 等简洁标签，不使用 `Controls (city-year)` 或 `Controls (case-level)` 这类括号说明。
- 行政案件表中 `Government counsel` 和 `Government counsel × Post` 不再追加多余的 `control` 字样。
- `Plaintiff entity` 与 `Opposing counsel` 不再追加 `(case-level)`。

论文 note 的原则：

- 说明估计量、样本、主要控制、固定效应和聚类。
- 不堆砌原始变量名、脚本对象名或 DGP 细节。
- 不在最终表格 note 中使用 `synthetic`, `simulated`, `fake`, `DGP` 等破坏论文成品感的词。
- `synthetic control` 若作为方法名出现，应保留；当前最终输出中没有该方法名。

## 12. 控制变量口径

当前控制变量分三类：

City Controls：

- `log_population_10k`
- `log_gdp`
- `log_registered_lawyers`
- `log_court_caseload_n`

Case Controls：

- opposing-side representation
- plaintiff entity status
- defendant entity status

Lawyer Controls：

- standardized lawyer practice years

当前 city controls 使用当年值。这在本测试分析包中是固定口径；若未来转为更严格的因果稿件设定，建议统一使用滞后一年 city controls，尤其是 `log_court_caseload_n`，以降低控制变量被当年诉讼冲击内生影响的担忧。

当前审计显示：

- city controls 在 `city_year_panel.csv` 中无缺失，并在城市内和年份间有足够 variation。
- case controls 在 `document_level_winner_vs_loser.csv` 和 `admin_case_level.csv` 中与相应个案观察对齐。
- lawyer gender 和 education 已数值化且无缺失。
- firm-level 结果变量已在 firm-year 层面聚合，因此 firm-level 主规格不加入 case controls。

## 13. 已知解释边界

1. 本项目是人工生成的测试研究，不能被当作真实世界原始档案引用。
2. 行政案件扩容中，有一部分 rows 来自外部行政案件全集回补，另一部分来自同城市或同类城市的重抽样补齐；它们服务于内部一致的模拟分析，不是新的真实采集数据。
3. 行政案件文件不包含具体律所身份；因此行政端不能声称已逐案识别政府顾问律所，只能分析政府是否有律师参与及其随 procurement 的变化。
4. 合同轮换后的 firm/document 分析使用 tender-level matched-stack DID 框架：一个 stack 对应一次采购招标，且只有一个 winner。若研究重点转为“合同中断、续约和再次采购”的独立机制，仍可进一步开发更显式的 spell-level estimator。
5. `court_match_key` 与 `province/city` 不应被强制逐行相同；一个是法院地点匹配键，一个是 treatment city 归属口径。
6. `admin_case_level.csv` 中 `event_time` 与 `year - event_year` 在约 0.7% 的治疗城市行上相差 ±1，反映采购日期跨年带来的事件时间口径偏移；分析脚本直接读 `event_time` 列，因此不会引入新偏差，但跨表口径以 `event_time` 列为准。
7. `admin_case_level.csv` 中 `log_duration_days` 在校准补齐过程中与 `duration_days` 在大约 24.5% 的行上不再严格满足 `log_duration_days = log1p(duration_days)`；city↔admin 的 `mean_log_duration` 等价仍严格成立。下游分析只使用 `log_duration_days` 列。
8. `city_year_panel.csv` 的 2013 年仅作为 city-year staggered 估计量的 pre-period 使用；其 `admin_case_n` 列未参与 2014–2020 校准化补齐，因此 2013 与 2014–2020 的 `admin_case_n` 口径并不严格可比。任何把 `admin_case_n` 当 outcome 跨 2013–2020 估计的规格应将 2013 视为外推 pre-period 而非校准化数据点。

## 14. 最小审计清单

每次修改数据或代码后，至少应重新检查：

1. 4 个 CSV 是否只包含最终分析所需列，且列名和列顺序未意外变化。
2. `case_no`、`case_uid`、`stack_id × firm_id × year` 是否仍唯一。
3. `did_treatment` 是否仍等于对应的 treatment × post。
4. 2014-2020 的 city/admin 7 个聚合变量是否仍严格等价。
5. document/firm 的案件总数、decisive cases、win counts、fee decisive counts 是否仍守恒。
6. `lawyer_gender` 是否只含 `0/1` 且无缺失。
7. `lawyer_edu` 是否只含 `1/2/3/4/5` 且无缺失。
8. `civil_win_n_binary <= civil_decisive_case_n` 是否仍成立。
9. `civil_fee_decisive_case_n <= civil_decisive_case_n` 是否仍成立。
10. `output/tables` 与 `output/figures` 是否完整生成 35 张 `.tex` 表（含 1 个 `.txt` 助记数字）和 25 张 `.pdf` 图。
11. 每张 `.pdf` 不是 0 页（可用 `pdfinfo` 批量检查 `Pages: 1` 是否成立；fect 的 ggplot 输出必须经过 `print()` 才能写入 `pdf()` 设备）。

## 15. Manuscript organization roadmap

下列结构按 APSR / AJPS / JOP 类顶刊的常规切分把每一个表和图分配到 main paper 或 appendix。每条目同时给出：(a) 论文位置；(b) 假设映射（H1 国家能力 / H2 选择 / H3 律所俘获 / SUTVA / 估计量稳健性 / 多重检验）；(c) 生成脚本；(d) `output/` 路径。

### 15.1 Main paper 结构

| Section | 编号 | 表 / 图 | 假设 | 脚本 | `output/` 路径 |
|---|---|---|---|---|---|
| 1. Introduction & framework | Figure 0 | Procurement adoption timeline | descriptive | `admin_descriptives_appendix.R` | `figures/procurement_adoption_timeline.pdf` |
| 2. Data and design | Table A1 (in-text) | Summary statistics | descriptive | `admin_descriptives_appendix.R` | `tables/summary_statistics_appendix_table.tex` |
| 3. City-year reduced form | Table 1 | City-year CS + TWFE main | H1 + H2 reduced | `city_year_cs_twfe_figures_tables.R` | `tables/city_year_cs_twfe_main_table.tex` |
| 3. City-year reduced form | Figure 1 | City-year event studies (gov win, appeal, admin cases) | H1 + H2 dynamics | `city_year_cs_twfe_figures_tables.R` | `figures/government_win_rate_event_study.pdf`, `figures/appeal_rate_event_study.pdf`, `figures/admin_case_n_event_study.pdf` |
| 4. Mechanism: H1 vs H2 | Table 2 panel A | Selection vs quality (3 nested samples) | H1 vs H2 | `admin_mechanism_selection_vs_quality.R` | `tables/admin_mechanism_selection_vs_quality_table.tex` |
| 4. Mechanism: H1 vs H2 | Table 2 panel B | Plaintiff entry / composition | H2 selection | `admin_mechanism_plaintiff_entry.R` | `tables/admin_mechanism_plaintiff_entry_table.tex` |
| 4. Mechanism: H1 vs H2 | Table 2 panel C | Cause sensitivity (high vs low) | H1 + H2 mix | `admin_mechanism_cause_sensitivity.R` | `tables/admin_mechanism_cause_sensitivity_table.tex` |
| 5. Document-level effect | Table 3 | Document-level Winner $\times$ Post DID | H3 reduced | `document_level_did_fixest.R` | `tables/document_level_did_main_table.tex` |
| 5. Document-level effect | Figure 3 | Document event studies (reasoning share, length, fee win) | H3 dynamics | `document_level_did_fixest.R` | `figures/document_level_legal_reasoning_share_event_study.pdf`, `figures/document_level_log_legal_reasoning_length_chars_event_study.pdf`, `figures/document_level_case_fee_win_rate_event_study.pdf` |
| 5. Document-level effect | Table 4 | Lawyer-attribute heterogeneity (CCP / gender / seniority / master+) | H3 moderator | `document_level_did_fixest.R` | `tables/document_level_attribute_heterogeneity_table.tex` |
| 6. Document-level DDD | Table 5 | Strict triple-difference (with two-way interactions) | H3 strict | `document_level_ddd_fixest.R` | `tables/document_level_strict_ddd_main_table.tex` |
| 7. Firm-level reallocation | Table 6 | Firm-year stacked DID main | H3 firm side | `firm_level_stacked_did_fixest.R` | `tables/firm_level_stacked_did_main_table.tex` |
| 7. Firm-level reallocation | Figure 4 | Firm event studies (civil win, hearing days, fee win, client mix) | H3 firm dynamics | `firm_level_stacked_did_fixest.R` | `figures/firm_level_civil_win_rate_mean_event_study.pdf`, `figures/firm_level_civil_fee_win_rate_event_study.pdf`, `figures/firm_level_avg_filing_to_hearing_days_event_study.pdf`, `figures/firm_level_client_mix_event_study.pdf` |
| 7. Firm-level reallocation | Table 7 | Client-mix mechanism (enterprise share) | H3 channel | `firm_level_stacked_did_fixest.R` | `tables/firm_level_client_mix_mechanism_table.tex` |
| 8. Channel-level cleaner H3 tests | Table 8 panel A | Document Winner $\times$ Post on pure-private subsample | H3 strict | `document_mechanism_pure_private_placebo.R` | `tables/document_mechanism_pure_private_placebo_table.tex` |
| 8. Channel-level cleaner H3 tests | Table 8 panel B | Reasoning vs total length decomposition | H3 vs stylistic | `document_mechanism_reasoning_decomposition.R` | `tables/document_mechanism_reasoning_decomposition_table.tex` |

### 15.2 Appendix structure

| Appendix section | 表 / 图 | 假设 / 用途 | 脚本 | `output/` 路径 |
|---|---|---|---|---|
| A. Pre-period balance | Table A2 | Pre-procurement balance (city-year + firm-year) | observable balance | `admin_case_appendix_tables.R` | `tables/pre_procurement_balance_appendix_table.tex` |
| B. City-year robustness | Table B1 | Selection robustness (TWFE + CS, IPW / EB / caliper) | identification | `admin_selection_robustness.R` | `tables/city_year_selection_robustness_appendix_table.tex` |
| B. City-year robustness | Table B2 | Lawyer-share controls (TWFE) | functional-form | `city_year_cs_twfe_figures_tables.R` | `tables/city_year_lawyer_share_appendix_table.tex` |
| B. City-year robustness | Table B3 | Disclosure-corrected weighting | measurement-error | `admin_disclosure_weighted_robustness.R` | `tables/city_year_disclosure_weighted_appendix_table.tex` |
| B. City-year robustness | Table B4 | Same-province donor-pool placebo (TWFE + CS) | confounding | `admin_within_province_placebo.R` | `tables/admin_within_province_placebo_appendix_table.tex` |
| B. City-year robustness | Table B5 | Process / cause-mix placebos + Sun-Abraham | placebo + estimator | `admin_placebo_alternative_estimator.R` | `tables/admin_placebo_alternative_appendix_table.tex` |
| C. Frontier-DID estimator | Table C1 | TWFE / CS / SA / Borusyak-Jaravel-Spiess parallel | estimator robustness | `city_year_alternative_estimators.R` | `tables/city_year_alternative_estimators_appendix_table.tex` |
| C. Frontier-DID estimator | Table C2 | de Chaisemartin-D'Haultfoeuille negative weights | TWFE diagnostic | `city_year_negative_weights_diagnostic.R` | `tables/city_year_negative_weights_diagnostic_appendix_table.tex` |
| C. Frontier-DID estimator | Table C3 + Figure C3 | Rambachan-Roth honest DID bounds (M = 0, 0.5, 1) | parallel-trend partial id | `city_year_honest_did.R` | `tables/city_year_honest_did_appendix_table.tex`, `figures/honest_did_government_win_rate.pdf`, `figures/honest_did_appeal_rate.pdf`, `figures/honest_did_admin_case_n.pdf` |
| C. Frontier-DID estimator | Table C4 + Figure C4 | Bacon decomposition (colour) | TWFE 2x2 weights | `city_year_bacon_decomposition.R` | `tables/city_year_bacon_decomposition_appendix_table.tex`, `figures/city_year_bacon_decomposition.pdf` |
| C. Frontier-DID estimator | Table C5 + Figure C5 | Permutation + wild cluster bootstrap-$t$ | inference | `city_year_randomization_inference.R` | `tables/city_year_randomization_inference_appendix_table.tex`, `figures/city_year_permutation_distribution.pdf` |
| D. Administrative case heterogeneity | Table D1 | Government / opposing counsel decomposition | H1 channel | `admin_case_level_did_fixest.R` | `tables/admin_case_level_lawyer_specs_appendix_table.tex` |
| D. Administrative case heterogeneity | Table D2 | Plaintiff-type heterogeneity | H1 + H2 split | `admin_case_level_did_fixest.R` | `tables/admin_plaintiff_heterogeneity_appendix_table.tex` |
| D. Administrative case heterogeneity | Table D3 | Cross-jurisdiction heterogeneity | H1 + H2 split | `admin_cross_jurisdiction_heterogeneity.R` | `tables/admin_cross_jurisdiction_heterogeneity_appendix_table.tex` |
| D. Administrative case heterogeneity | Table D4 + Figure D4 | Per-cause coefficient table + plot | H1 + H2 cross-cause | `admin_case_by_cause_coefplot.R` | `tables/admin_by_cause_government_win_rate_coefplot_table.tex`, `figures/admin_by_cause_government_win_rate_coefplot.pdf` |
| E. Document-level robustness | Table E1 | Fee win-rate Winner $\times$ Post | H3 robustness | `document_level_did_fixest.R` | `tables/document_level_fee_winrate_appendix_table.tex` |
| E. Document-level robustness | Figure E1 | Lawyer-attribute event studies (4 splits) | H3 channel | `document_mechanism_lawyer_attribute_event_study.R` | `figures/document_mechanism_event_study_by_party_membership.pdf`, `figures/document_mechanism_event_study_by_gender.pdf`, `figures/document_mechanism_event_study_by_seniority.pdf`, `figures/document_mechanism_event_study_by_education.pdf` |
| F. Firm-level extensions | Figure F1 | Log firm size event study | H3 firm scale | `firm_level_stacked_did_fixest.R` | `figures/firm_level_log_firm_size_event_study.pdf` |
| F. Firm-level extensions | Table F1 | Pre-period winner-selection logit | selection | `firm_winner_selection_logit.R` | `tables/firm_winner_selection_logit_appendix_table.tex` |
| G. SUTVA / spillover | Table G1 | Loser-in-Winner-Court spillover | SUTVA at court | `firm_sutva_same_court_spillover.R` | `tables/firm_sutva_same_court_spillover_table.tex` |
| G. SUTVA / spillover | Table G2 | Same-province neighbour spillover | SUTVA at city | `city_sutva_neighbor_treated.R` | `tables/city_sutva_neighbor_treated_table.tex` |
| G. SUTVA / spillover | Table G3 | Lagged Winner $\times$ Post | carry-over | `firm_sutva_carryover_lag.R` | `tables/firm_sutva_carryover_lag_table.tex` |
| G. SUTVA / spillover | Table G4 + Figure G4 | fect period-wise ATT relative to exit + carry-over test | exit dynamics | `firm_fect_carryover_exit.R` | `tables/firm_fect_carryover_exit_table.tex`, `figures/firm_fect_exit_civil_win_rate_mean.pdf`, `figures/firm_fect_exit_civil_win_rate_fee_mean.pdf`, `figures/firm_fect_exit_avg_filing_to_hearing_days.pdf` |
| H. Multiple testing | Table H1 | Family-wise adjusted p-values (Holm + Romano-Wolf for city-year) | multiple-testing | `family_wise_pvalues.R` | `tables/family_wise_pvalues_appendix_table.tex` |
| I. Substantive interpretation | Table I1 | Per-treated-city-year and total cases changes + percentile shifts | interpretation | `back_of_envelope_substantive.R` | `tables/back_of_envelope_substantive_appendix_table.tex` (附助记 .txt) |

### 15.3 Headline numbers used in the prose

下列数字与 `MANUSCRIPT_WRITING.md §2 Substantive interpretation` 完全一致，并直接来自上表：

- City-year TWFE (calibrated): `government_win_rate +0.036***`, `appeal_rate -0.062***`, `admin_case_n -23.098**`
- City-year CS overall ATT: `+0.049***` / `-0.116***` / `-91.450***`
- Per-treated-city-year case-count translation: government win flips ≈ 13.8 cases; appeals fall ≈ 23.7; admin cases fall ≈ 23.1
- Total over 712 treated city-years: government win flips ≈ 9,800; appeals fall ≈ 17,000; admin cases fall ≈ 16,400 (TWFE) or 65,000 (CS)
- Pre-period percentile shift of the median: government win 50→65 pctile; appeal 50→30 pctile; admin cases 50→45 pctile (TWFE) or near 10-15 pctile (CS)

### 15.4 Writing-style notes (manuscript-facing)

1. **Terminology.** Refer to "procurement of legal counsel" or "the procurement of dedicated government legal counsel"; do not use the word `treatment` outside table notes. Use "winner firm" / "loser firm" or "contracted firm" / "competing firm" rather than `treated_firm`.
2. **Hypothesis framing.** Always introduce H1 / H2 / H3 as competing rather than complementary; state at least one observable implication of each before moving to results.
3. **Estimator wording.** When citing the city-year coefficient, give both the TWFE and the CS number in the same sentence; state which one is taken as the headline (typically TWFE for the prose; CS for the magnitude bound).
4. **Table notes (main).** Keep main-paper notes to ≤ 4 sentences: estimator, sample, controls, FE, clustering, significance levels. Do not name internal variables (`did_treatment`, `lawyer_practice_years_obs`).
5. **Table notes (appendix).** May be slightly longer; should explicitly state any sample restriction (e.g. "support sample of firm-court rows with no prior government-side appearance"), state the algorithm if it is not standard (e.g. German-tank disclosure), and reference the canonical citation.
6. **Figure captions.** Always label x-axis "Years Since the Contract" / "Years Since Procurement"; mark $t = -1$ as the reference period; report the Joint pre-period $p$-value on the figure.
7. **Substantive language.** Translate all rate-coefficients into either case counts or pre-period percentile shifts before interpreting magnitude; never describe a percentage-point coefficient as "small" or "large" without that translation.
8. **What not to claim.** This is a synthetic / test research package. Do not claim external validity or causal mapping to specific real-world programmes; the manuscript should describe results as the empirical content of the calibrated dataset, with the substantive thrust being the methodology and identification design.

For more detailed paragraph-level wording, see `MANUSCRIPT_WRITING.md`.
