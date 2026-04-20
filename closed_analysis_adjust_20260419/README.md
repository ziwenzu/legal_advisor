# Closed Analysis Package: Universal README

本文件夹是这个研究的封闭分析包，只保留真正进入最终分析链条的数据、代码和结果。

这不是一个“原始数据归档包”，也不是完整仓库镜像。它的目标是让使用者在不接触外部 DGP、调参或历史版本代码的情况下，直接重现当前研究版本的全部分析结果。

本项目本身是一个模拟 / synthetic 测试研究。这里的审计标准是：

- 看分析链条是否内部一致、可复现、变量定义是否自洽
- 看 case-level 与 panel-level 之间该等价的地方是否真的等价
- 不把它当作真实世界原始档案去做外部事实核验

## 1. 本研究在问什么

研究核心问题是：地方政府采购法律顾问后，是否会改变政府在行政诉讼中的结果，以及这种变化是否会外溢到相关律所在民事案件中的表现、文书特征和客户结构。

更具体地说，当前分析链条覆盖四层问题：

1. 城市-年层面：采购发生后，政府胜诉率、上诉率、行政案件数量是否变化。
2. 行政案件层面：政府是否更容易获胜，这种变化是否与原有政府律师、新增律师、法院层级、地域关系、案由类别有关。
3. 文书层面：受到采购冲击的律所在民事案件中的文书长度、说理占比、费用胜诉率是否发生变化。
4. 律所-年层面：受到采购冲击的律所在民事案件中的胜诉率、费用胜诉率、平均审理时长、客户结构和律所规模是否变化。

## 2. 当前版本的分析假设

本封闭包对应的实证设定可以概括为四组假设：

1. 政府采购法律顾问会提升政府在行政诉讼中的结果表现，主要体现为更高的政府胜诉率和更低的上诉率。
2. 这种采购冲击不只体现在行政案件本身，还会外溢到被采购律所处理的民事案件，改变其诉讼表现与文书特征。
3. 如果某律所在同一法院此前已经有政府代理暴露，这种外溢效应会更强；这也是 DDD 规格的识别逻辑。
4. 采购还可能改变律所的案件来源与业务结构，因此需要在 firm-year 面板上同时考察 client mix、审理时长和规模变化。

## 3. 文件夹结构

- `code/`
  - 只保留分析代码
  - 每个表 / 图对应的生成脚本见 [code/README.md](/Users/ziwenzu/Library/CloudStorage/Dropbox/research/1_Law_project/Legal_advisor/closed_analysis_adjust_20260419/code/README.md)
- `data/`
  - 只保留分析实际读取的 4 个 CSV
- `output/tables/`
  - 当前版本生成的全部 `.tex` 表
- `output/figures/`
  - 当前版本生成的全部图

当前封闭包内已经包含：

- `18` 张表
- `13` 张图

## 4. 哪些代码不在这个文件夹里

本文件夹故意不包含数据生成过程、噪音调参和参数搜索脚本。那些脚本不属于“最终分析链条”，统一存放在外部目录：

- `/Users/ziwenzu/Library/CloudStorage/Dropbox/research/1_Law_project/Legal_advisor/code/code_dgp_tuning_external/`

换句话说：

- 本文件夹里的 `code/` 是 analysis-only code
- 外部 `code/code_dgp_tuning_external/` 是 DGP / tuning-only code

## 5. 四个分析数据文件

### 5.1 `city_year_panel.csv`

- 路径：[city_year_panel.csv](/Users/ziwenzu/Library/CloudStorage/Dropbox/research/1_Law_project/Legal_advisor/closed_analysis_adjust_20260419/data/city_year_panel.csv)
- 观测单位：`province × city × year`
- 年份：`2013-2020`
- 主键：`province + city + year`
- 当前行数：`2,256`
- 主键重复：`0`

这个文件用于 city-year 主表、event study 和城市层面的稳健性检验。

核心变量：

- 处理变量：`treatment`
- 结果变量：`government_win_rate`, `appeal_rate`, `admin_case_n`
- 其他聚合结果：`petition_rate`, `gov_lawyer_share`, `opp_lawyer_share`, `mean_log_duration`
- 控制变量：`log_population_10k`, `log_gdp`, `log_registered_lawyers`, `log_court_caseload_n`

特别说明：

- `2013` 只保留在这个城市面板里
- 它的目的不是扩展行政案件主样本，而是给 Callaway-Sant'Anna 估计器提供一个真实 pre-period，避免 `2014` 首次 treated 城市在 CS DID 中被整组丢掉
- 因此，`2013` 在 city-year 面板中是分析期的一部分，但不要求在 `admin_case_level.csv` 中存在逐案对应记录

### 5.2 `admin_case_level.csv`

- 路径：[admin_case_level.csv](/Users/ziwenzu/Library/CloudStorage/Dropbox/research/1_Law_project/Legal_advisor/closed_analysis_adjust_20260419/data/admin_case_level.csv)
- 观测单位：行政案件
- 年份：`2014-2020`
- 主键：`case_no`
- 当前行数：`444,992`
- 主键重复：`0`

这个文件用于所有行政案件级 DID、异质性、案由图和部分平衡 / placebo 表。

核心变量：

- 处理与时点：`treated_city`, `event_year`, `event_time`, `post`, `did_treatment`
- 结果变量：`government_win`, `appealed`, `petitioned`
- 律师暴露：`government_has_lawyer`, `opponent_has_lawyer`
- 个案属性：`plaintiff_is_entity`, `non_local_plaintiff`, `cross_jurisdiction`
- 程序与结案：`withdraw_case`, `end_case`
- 法院 / 案由：`court_std`, `court_level`, `cause`, `cause_group`
- 时长：`duration_days`, `log_duration_days`

说明：

- 在这个封闭包里，`cause_group` 已经是当前分析版本实际使用的案由分类
- `did_treatment = treated_city × post` 在当前 CSV 中严格成立
- `event_time` 在行政案件数据里是用于分析的相对时点变量，其支持被截在 `[-5, 5]`
- 在当前封闭包中，`cross_jurisdiction = 1` 与 `court_level %in% {intermediate, high, specialized}` 等价；它在代码里作为“非基层法院 / 跨区域审理安排”的代理变量使用
- `province` 和 `city` 是行政案件归属到 procurement treatment 的城市口径；`court_std` 是实际审理法院名称。二者在跨区域审理、异地法院或模拟数据中的法院分配下不要求逐行同地，因此不要用 `province/city` 强行覆盖 `court_std`

### 5.3 `document_level_winner_vs_loser.csv`

- 路径：[document_level_winner_vs_loser.csv](/Users/ziwenzu/Library/CloudStorage/Dropbox/research/1_Law_project/Legal_advisor/closed_analysis_adjust_20260419/data/document_level_winner_vs_loser.csv)
- 观测单位：文书级案件观察
- 年份：`2010-2020`
- 主键：`case_uid`
- 当前行数：`1,829,763`
- 主键重复：`0`

这个文件是文书级主文件，同时供 document-level DID、DDD 和描述统计使用。

核心变量：

- 处理与时点：`treated_firm`, `event_year`, `event_time`, `post`, `did_treatment`
- 结果变量：`case_win_rate_fee`, `log_legal_reasoning_length_chars`, `legal_reasoning_share`
- 基础诉讼信息：`court`, `cause`, `side`, `case_win_binary`, `case_decisive`
- 控制变量：`opponent_has_lawyer`, `plaintiff_party_is_entity`, `defendant_party_is_entity`
- 律师属性：`lawyer_gender`, `lawyer_practice_years`, `lawyer_ccp`, `lawyer_edu`
- 链接键：`stack_id`, `firm_id`, `law_firm`
- DDD 专用变量：`court_match_key`, `prior_admin_gov_exposure`, `has_pre_admin_civil_case_in_court`

说明：

- `did_treatment = treated_firm × post` 在当前 CSV 中严格成立
- 当前 `event_time` 支持到 `[-10, 6]`，但文书级 event-study 代码会把绘图窗口裁到 `[-5, 5]`
- `lawyer_gender` 在当前封闭包中编码为 `1 = female, 0 = male`，不保留缺失
- `lawyer_edu` 在当前封闭包中编码为 `1 = others, 2 = associate, 3 = college, 4 = master, 5 = PhD`，不保留缺失
- 这个文件以原主文书分析数据的共享变量为准，并接上 DDD 识别独有的 3 个法院暴露变量；因此它替代了原先两份重复的 document-level CSV
- `court_match_key` 是 DDD 规格使用的标准化法院匹配键；它把原始 `court` 名称归并到稳定的法院单元，用于构造 court-by-year 固定效应、firm-by-court 暴露变量和 court-level 聚类
- 当前版本已按行政案件数据中的标准法院名对开发区法院和带有文书噪声前缀的法院 key 做了地点规范化；例如 `成都高新技术产业开发区人民法院` 统一为 `成都市高新技术产业开发区人民法院`，以便与行政端 `court_std` 的地点口径对齐
- `court_match_key` 表示法院所在地口径，不表示案件归属的 procurement city；因此它与行政案件文件里的 `province/city` 不是逐行等价关系
### 5.4 `firm_level.csv`

- 路径：[firm_level.csv](/Users/ziwenzu/Library/CloudStorage/Dropbox/research/1_Law_project/Legal_advisor/closed_analysis_adjust_20260419/data/firm_level.csv)
- 观测单位：`stack_id × firm_id × year`
- 年份：`2010-2020`
- 主键：`stack_id + firm_id + year`
- 当前行数：`181,940`
- 主键重复：`0`

这个文件用于 firm-level DID 主表、机制表和 5 张 event-study 图。

核心变量：

- 处理与时点：`treated_firm`, `event_year`, `event_time`, `did_treatment`
- 主要结果：`civil_win_rate_mean`, `civil_win_rate_fee_mean`, `avg_filing_to_hearing_days`
- 计数变量：`civil_case_n`, `civil_win_n_binary`, `civil_decisive_case_n`, `civil_fee_decisive_case_n`
- 客户结构：`enterprise_case_n`, `personal_case_n`
- 规模：`firm_size`
- 链接键：`stack_id`, `firm_id`, `law_firm`

说明：

- `did_treatment = treated_firm × 1[event_time >= 0]` 在当前 CSV 中严格成立
- 当前 `event_time` 支持到 `[-10, 6]`，firm-level event-study 代码同样只画 `[-5, 5]`
- firm-level 主 DID 与 firm-level event-study 使用同一 `[-5, 5]` 事件时间窗口，而不是整段 `[-10, 6]`
- 这个面板保留了 matched stack 设计下的 zero-case firm-year cells，因此它不等于“只把 document CSV 简单 group by 一次”的裸结果
- `winner_firm` 记录 stack 中的中标律所名称；`treated_firm` 是该行 firm-year 单元是否被编码为处理组的指标。二者相关但不必逐行完全相同，因此不应把 `winner_firm == treated_firm` 当作数据恒等式

## 6. 数据之间的关系：哪些必须等价，哪些不需要硬等价

这是这个封闭包最重要的部分。

先给一个总括。就这个研究的分析链条来说，理论上至少有三组必须交代清楚的数据关系：

1. `admin_case_level.csv -> city_year_panel.csv`  
   这是“低层案件数据聚合到城市-年面板”的严格聚合恒等式。

2. `document_level_winner_vs_loser.csv -> firm_level.csv`  
   这是“文书级样本聚合到律所-年样本”的守恒关系与内部算术恒等式。

如果只写第 1 组，会让人误以为只有 city/admin 之间需要核对；其实文书级与 firm-level 之间的聚合关系，以及 DDD 在同一文书文件内额外调用哪些列，也都必须讲清楚。

### 6.1 必须严格等价的关系

#### A. `admin_case_level.csv` 与 `city_year_panel.csv`

在重叠年份 `2014-2020` 上，下面 7 个 city-year 变量必须等于行政案件级逐案聚合结果；当前封闭包中这些等式全部成立，只剩机器精度级误差：

对于每个 `province × city × year`：

- `government_win_rate = mean(government_win)`
- `appeal_rate = mean(appealed)`
- `admin_case_n = N(case)`
- `petition_rate = mean(petitioned)`
- `gov_lawyer_share = mean(government_has_lawyer)`
- `opp_lawyer_share = mean(opponent_has_lawyer)`
- `mean_log_duration = mean(log_duration_days)`

这也是 [audit_city_admin_relationships.R](/Users/ziwenzu/Library/CloudStorage/Dropbox/research/1_Law_project/Legal_advisor/closed_analysis_adjust_20260419/code/audit_city_admin_relationships.R) 检查的核心内容。

特别提醒：

- 这个等价关系只要求在 `2014-2020` 成立
- `2013` 是 city-year 的额外 pre-period，不要求在 admin case 文件中存在逐案镜像

#### B. `document_level_winner_vs_loser.csv` 与 `firm_level.csv`

这两层不是逐行等价，而是“文书级样本”和“按 `stack_id × firm_id × year` 聚合后的律所-年样本”。

在当前封闭包中，以下恒等式已经验证：

- `sum(firm_level.civil_case_n) = 1,829,763 = nrow(document_level_winner_vs_loser)`
- `sum(firm_level.civil_decisive_case_n) = 1,335,823 = sum(document_level.case_decisive)`
- 对每个 `stack_id × firm_id × year`，若 `civil_decisive_case_n > 0`，则  
  `civil_win_rate_mean = civil_win_n_binary / civil_decisive_case_n`
- 对每个 `stack_id × firm_id × year`，  
  `enterprise_case_n + personal_case_n = civil_case_n`
- 对每个 `stack_id × firm_id × year`，  
  `civil_fee_decisive_case_n <= civil_decisive_case_n`

因此，firm-level 面板与 document-level 面板的关系应理解为：

- 它们共享同一 matched-stack 研究设计
- `stack_id` 和 `firm_id` 是主要链接键
- case totals 和 decisive-case totals 在聚合意义上必须守恒
- rate variables 必须满足各自的内部算术恒等式

### 6.1A 一个更简洁的“理论等价关系”总结

如果把上面的关系压缩成论文或 replication note 里最应该交代的版本，可以写成下面 5 条：

1. 在 `2014-2020` 上，`city_year_panel.csv` 中 7 个核心 city-year 结果变量，等于 `admin_case_level.csv` 按 `province × city × year` 聚合后的均值或计数。

2. `document_level_winner_vs_loser.csv` 同时服务于 document DID 与 DDD；DDD 只是在同一文书级样本上额外调用 `court_match_key`、`prior_admin_gov_exposure` 和 `has_pre_admin_civil_case_in_court` 三个字段。

3. `firm_level.csv` 是 `document_level_winner_vs_loser.csv` 在 `stack_id × firm_id × year` 上的分析面板；因此总案件数和总 decisive-case 数在聚合后必须守恒。

4. `firm_level.csv` 内部的 rate 变量不是独立自由变量，而应满足分子 / 分母定义，例如 `civil_win_rate_mean = civil_win_n_binary / civil_decisive_case_n`。

5. 处理变量在不同层级上的含义必须一致：  
   城市层是 procurement-treated city，文书和律所层是 procurement-treated firm，`did_treatment` 都是“是否 treated × 是否 post”的实现，只是 firm-level 用 `event_time >= 0` 代替单独存储的 `post`。

### 6.2 不应该被硬写成“逐项等价”的关系

为了避免误读，下面几件事不应被写成更强的等价命题：

1. `city_year_panel.csv` 的 `2013` 不应被要求在 `admin_case_level.csv` 中有逐案镜像。  
   它存在的目的只是防止 CS DID 丢掉 `2014` 首治城市。

2. `firm_level.csv` 不应被理解为“直接从 document clean 逐列裸 group-by 一次即可完全重建”。  
   原因是 firm panel 保留了 matched stack 设计下的 zero-case firm-year cells，并直接把一组 firm-cell 汇总变量作为分析输入。

3. `document_level_winner_vs_loser.csv` 同时支持 DID 和 DDD，但 DDD 有额外的 estimation filter。  
   当前 DDD 脚本会：
   - 先删掉 `court_match_key` 为空的行
   - 再保留 `prior_admin_gov_exposure == 0` 或 `has_pre_admin_civil_case_in_court == 1` 的支持样本

## 7. 处理变量和时点变量如何在不同层级对应

### 城市-年层面

- `treatment` 是城市在该年是否处于采购后状态的时间变化处理变量
- `first_treat_year` 不存储在 CSV 中，而是在 [city_year_cs_twfe_figures_tables.R](/Users/ziwenzu/Library/CloudStorage/Dropbox/research/1_Law_project/Legal_advisor/closed_analysis_adjust_20260419/code/city_year_cs_twfe_figures_tables.R) 中根据 `treatment` 历史动态生成
- 当前封闭包中的 city controls 使用当年值；如果未来要改成严格预定控制，最适合优先滞后的是 `log_court_caseload_n`

### 行政案件层面

- `treated_city` 表示该案所在城市是否属于最终 treated city
- `post` 表示该案年份是否处于处理后时期
- `did_treatment = treated_city × post`

### 文书层面

- `treated_firm` 表示该律所是否属于 treated firm
- `post` 表示文书年份是否处于处理后时期
- `did_treatment = treated_firm × post`

### 律所-年层面

- `treated_firm` 表示该律所是否属于 treated firm
- `did_treatment = treated_firm × 1[event_time >= 0]`
- 这里没有单独存 `post` 列，但含义与 document-level 保持一致

## 8. 分析脚本与结果的关系

这个封闭包中的分析代码只读取 `data/` 下的 4 个 CSV，不读取 parquet，不读取中间文件，也不依赖外部数据路径。

每个表 / 图由哪个脚本生成，完整对应关系见：

- [code/README.md](/Users/ziwenzu/Library/CloudStorage/Dropbox/research/1_Law_project/Legal_advisor/closed_analysis_adjust_20260419/code/README.md)

简化理解如下：

- city-year 主结果与图：`city_year_cs_twfe_figures_tables.R`
- 行政案件主表 / 异质性 / 案由图：`admin_case_level_did_fixest.R`, `admin_cross_jurisdiction_heterogeneity.R`, `admin_case_by_cause_coefplot.R`, `admin_case_appendix_tables.R`
- 城市层面稳健性：`admin_selection_robustness.R`, `admin_disclosure_weighted_robustness.R`, `admin_within_province_placebo.R`, `admin_placebo_alternative_estimator.R`
- 文书级主结果：`document_level_did_fixest.R`
- DDD 主结果：`document_level_ddd_fixest.R`
- firm-level 主结果：`firm_level_stacked_did_fixest.R`（不加入额外 case-level controls，因为结果变量已经是 firm-year 汇总量）
- 描述统计与 adoption 时间线：`admin_descriptives_appendix.R`
- city/admin 聚合审计：`audit_city_admin_relationships.R`

## 9. 如何重现实证结果

在这个封闭包里，推荐直接从文件夹根目录运行各个 `Rscript`。

这些脚本会自动：

- 从 `data/` 读取当前分析 CSV
- 把结果写到 `output/tables/` 和 `output/figures/`

如果你只需要知道“哪个脚本负责哪个输出”，请直接看 [code/README.md](/Users/ziwenzu/Library/CloudStorage/Dropbox/research/1_Law_project/Legal_advisor/closed_analysis_adjust_20260419/code/README.md)。

## 10. 使用这个封闭包时最重要的三条规则

1. 不要把 `2013` 的 city-year 扩展误读成“行政案件也补到了 2013”。  
   这里只有 city panel 向前扩了一期，admin case 主样本仍然是 `2014-2020`。

2. 不要把 firm-level 面板误读成“文书级数据的机械 group-by”。  
   它是 matched-stack 设计下的分析输入面板，包含 zero-case cells 和一组已经定好的 firm-cell 汇总变量。

3. 理论上至少要交代两组跨文件关系，再补充一组文件内 DDD 识别关系：  
   `admin_case_level -> city_year_panel` 的聚合恒等式，  
   `document_level_winner_vs_loser -> firm_level` 的聚合守恒关系，  
   以及 `document_level_winner_vs_loser` 内部哪些额外字段只服务于 DDD 识别。  
   其中最严格、最应该逐项核对的，是 city-year 与 admin case 在 `2014-2020` 的那 7 个聚合指标。
