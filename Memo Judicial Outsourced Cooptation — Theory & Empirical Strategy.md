judicial qui pro quo





Autocracies use quasi-democratic institutions to signal legality, yet these institutions can also constrain the state. I develop a theory of outsourced cooptation: through public contracting, governments transform state demand into private rents for expert intermediaries, inducing them to steer outcomes toward regime objectives while preserving institutional legitimacy. I test the theory in China by linking government legal-service contracts, administrative litigation records, and lawyers' career histories. Governments that retain lawyers as advisors or litigation representatives face fewer administrative suits, higher win rates, and lower appeal rates. In exchange, contracted lawyers and firms attract more private clients, win more often, and advance faster. Because contracts concentrate among politically embedded firms, outsourced cooptation widens inequality in professional markets. The findings identify a general mechanism of expert capture in authoritarian governance and its distributional consequences.



成为政府法律顾问–> 政府胜算更高，行政诉讼减少

律师—> 案源更多 胜率更高 扩张 人才吸引



下面我把我们刚才的讨论整理成一份**可以直接用于论文设计/和coauthor讨论的完整memo**。我会按 APSR 风格来组织：先是核心问题与理论，再是识别挑战，然后是**分层的实证策略（重点是律所层面）**，最后是方法选择与写作建议。尽量做到你后面可以直接拿去改成 paper section。

------

# Memo: Judicial Outsourced Cooptation — Theory & Empirical Strategy

## 一、研究核心问题（重新聚焦）

本文研究的不是简单的“政府和律师互利”，而是一个更一般的问题：

> 在威权体制中，国家如何在不破坏法律制度表象独立性的前提下，系统性地影响法律过程？

核心机制：

**政府通过法律服务合同，将“国家背书”转化为可在私人市场兑现的租，从而诱导律师在制度内部生产对国家有利的结果。**

------

## 二、理论机制（精炼版）

机制链条可以写成五步：

1. **制度约束**
   威权政府需要法律制度（法院、行政诉讼、律师）来维持“依法治理”的表象，但直接干预会损害合法性。
2. **外包解决方案（outsourced cooptation）**
   政府通过采购法律顾问、代理服务，将部分法律功能外包给私人律师。
3. **租的生成（rent creation）**
   合同不仅提供直接收入，还提供：
   - 政治背书（credibility）
   - 接近政府的渠道（access）
   - 市场信号（“有关系”）
   - 风险保护（political insurance）
4. **激励扭曲（incentive channel）**
   律师为了维持并扩大这些租，会：
   - 在诉前减少案件进入
   - 在诉中提高政府胜率
   - 在诉后降低上诉概率
5. **市场后果（distributional consequences）**
   - 中标律所获得更多私单、扩张、人才流入
   - 政府合同集中 → 加剧法律市场不平等

------

## 三、核心实证挑战（你现在卡住的地方）

### 1. 律所层面 DID 的核心问题

不是简单的 selection bias，而是：

> **treated law firms 在中标前已经有不同的增长趋势（pretrend 不平行）**

原因：

- 更强/更有关系的律所更容易中标
- 这些律所本来就在扩张

👉 结果：传统 DID 无法解释为 causal

------

### 2. 城市层面 DID 的问题（你原本低估了）

即便是 staggered DID：

> 政府采用法律顾问的时间本身也是内生的

例如：

- 行政诉讼上升 → 才采购
- 上级考核变化 → 才采购

👉 所以城市层面也不是 automatically clean

------

### 3. 数据限制

你没有：

- 投标者列表
- 评分/排名（无法做 RD 或 close-call）

👉 意味着：

- 无法做最强的 procurement-level quasi-experiment

------

## 四、总体识别策略（关键思路）

最重要的一句话：

> **不要识别“是否中标”的因果效应，而要识别“在接近被选中的边际上，中标带来的变化”。**

换句话说：

- ❌ treated vs all untreated
- ✅ treated vs almost-treated / later-treated / same-market firms

------

# 五、律所层面：推荐识别策略（重点）

## Strategy 1（最重要）：晚中标者作为对照（stacked DID）

### 核心思想

比较：

> 早中标律所 vs 尚未中标但未来会中标的律所

### 为什么有效

- 都在“政府法律服务市场”边缘
- 选择偏误显著降低

### 实现

- stacked event study
- 控制组 = not-yet-treated firms
- 加：
  - firm FE
  - year FE
  - city × year FE

👉 推荐估计量：

- Callaway–Sant’Anna（csdid）
- 或 BJS imputation

------

## Strategy 2：pre-trend matching / reweighting

### 核心思想

在做 DID 之前，让 treated 和 control 在**处理前走势一致**

### 方法

- matching on:
  - pre-treatment outcomes（案源、增长率）
  - 规模
  - 行政案件经验
- 或 entropy balancing / IPW

👉 本质：解决你最担心的问题
**treated firms already trending up**

------

## Strategy 3：公告时点（signal-based identification）

### 核心思想

利用：

> 政府合同“被市场观察到”的时点

### 检验

看是否在公告后：

- 私人案源增加
- 新客户增加
- 律所扩张
- 律师流入

而不是等很久以后才变化

👉 这是最贴你理论的机制证据

------

## Strategy 4：续聘 / 升级合作（强烈推荐）

### 核心思想

不要看“第一次中标”，看：

- 是否续聘
- 是否从普通 → 嵌入型律所
- 是否扩大合作

### 优势

- 已经在制度内
- selection bias 大幅下降

👉 这是你**最接近 quasi-experimental 的 margin**

------

## Strategy 5：treatment 强度（dose-response）

把 treatment 从 0/1 改成：

- 顾问机关数量
- 合同层级
- 合作深度

👉 更接近连续处理，更有识别力

------

## Strategy 6：三重差分（机制强化）

比较：

> 同一律所内
> 受政府关系影响大的业务 vs 不受影响的业务

例如：

- 本地行政相关 vs 外地商业案件

👉 如果差异只出现在“应受影响的业务”，机制更可信

------

## Strategy 7：人才流动（非常推荐）

看：

- 是否吸引更资深律师
- 是否吸引更有资源的律师

👉 比胜率更干净

------

## Strategy 8：如果能观察投标人/候选人，就转向 procurement-level design

### 核心思想

一旦你知道：

- 投标人名单
- 候选人名单
- 候选排序 / 评分 / 报价

就不要再拿 “treated firm vs all other firms” 当主设计，而要比较：

> winner vs actual losers
> 尤其是 winner vs runner-up

### 为什么更强

- 真实竞争池变得可见
- 处理组和对照组都通过了同一轮筛选
- estimand 变成：
  **conditional on entering the same tender, what is the effect of winning?**

### 具体操作

- 如果只有投标人 / 候选人名单：
  - 做 winner vs losers 的 stacked DID / event study
- 如果还有评分 / 排名 / 报价：
  - 重点做 close-call design
  - 比较 winner vs runner-up
  - 只保留分数差或报价差非常小的招标

### 重要提醒

- 如果每个 tender 只有几个候选人，但总体 tender 数量很多：
  - 这不是坏事
  - 不适合单个 tender 做 synthetic control
  - 更适合把多个 tender pooled 起来，做 winner vs runner-up 的 close competition design
- 如果连真实投标人都看不到：
  - synthetic control 无法恢复那些“不可见竞争者”
  - 它只能在可见 donor pool 里改善拟合，不能解决 risk set 不清楚的问题

👉 所以 procurement-level 信息一旦可见，优先级应高于 generic synthetic control

------

## Strategy 9：把 treatment 异质性显性化，而不是揉成一个 0/1

不要把所有采购都定义成：

> “拿到政府法律顾问合同 = 1”

而应至少区分：

- 市政府层级合同
- 委办局 / 部门层级合同
- 常年法律顾问 vs 具体诉讼代理

### 为什么关键

- 不同层级的竞争池不同
- 不同层级的政治背书强度不同
- 不同层级的市场信号外溢范围不同
- 部门合同和市政府合同可能是动态升级关系，而不是同一种 treatment

👉 更好的写法是：

- effect of municipal-government contracts
- effect of bureau-level contracts
- marginal effect of moving from bureau-level to municipal-level affiliation

------

# 六、政府结果：不要从城市开始，而要从机关开始

### 1. 为什么 city-level treatment 太粗

如果你要估计：

> 一个城市采购法律顾问后，对行政诉讼胜率的影响

这里最大的问题不是只有 adoption endogeneity，而是：

> 告市政府和告环保局、税务局、公安局时，真正应诉的机关和代表律师并不一样

所以 treatment 不应是：

> city treated = 1

而应是：

> defendant unit treated = 1

也就是说，分析单位应优先设成：

- 机关 × 年
- 或案件级

而不是城市 × 年

------

### 2. 先区分两个 estimand

#### A. direct representation effect

采购机关自己的案件，在合同生效后是否更容易赢？

这是主结果，最干净。

#### B. organizational spillover effect

市政府采购后，是否连没有直接由该律所代理的下属部门案件也受影响？

这是次要结果，可以做，但识别更难。

👉 论文中最好把 A 放主文，B 放后面

------

### 3. 主分析单位：机关级或案件级

如果能从裁判文书里识别出：

- 被告机关
- 代理律所 / 代理律师

那最好的设计是案件级：

> 只比较同类机关、同类案件、同城同年的案件

如果暂时看不到具体代理律师，也至少做机关-年份面板：

- outcome = 该机关当年行政诉讼胜率
- treatment = 该机关当年是否有活跃法律顾问合同
- 控制：
  - unit FE
  - city × year FE
  - unit-type × year FE

### 样本切分建议

- 市政府级合同：
  - 主样本只看“被告是市政府本身”的案件
- 部门级合同：
  - 主样本只看“被告是该部门本身”的案件

不要把这两类直接并成一个统一回归

------

### 4. 如果只有 city-level，应该怎么写

city-level DID 仍然可以做，但它识别的不是：

> contract lawyer representation effect

而更接近：

> package effect of legal contracting at the city level

也就是：

- 市政府采购
- 统一法制指导
- 组织协调
- 可能的跨部门外溢

👉 所以 city-level 结果更适合放在 supporting evidence，而不是最核心的机制检验

------

### 5. Synthetic DID / synthetic control 在政府结果里的位置

适用于：

- city 或 unit adoption 明显内生
- donor pool 足够

👉 作用：

- 改善对照组
- 改善 pre-treatment fit
- 不是创造外生性

所以：

- 如果做机关级主结果：优先 FE + event study + not-yet-treated controls
- 如果做城市级支持性结果：可以加 synthetic DID / gsynth 作为稳健性

------

### 6. “采购后没有任何诉讼”不是缺失，而是 outcome

如果某个部门采购法律顾问后，后面没有任何行政诉讼：

- 胜率不是 0
- 胜率也不是 1
- 胜率是 undefined

而且这很可能正是理论机制的一部分：

> 法律顾问可能在诉前减少案件进入法院

所以政府结果不能只看 win rate，必须拆成两层：

#### 第一层：是否进入诉讼

- any administrative suit
- number of administrative suits

#### 第二层：条件于已经起诉后的结果

- government win rate conditional on filing
- appeal rate conditional on filing

👉 解释上要写清楚：

- 第一层识别“诉前筛选 / 化解 / 压制进入”的效应
- 第二层识别“在进入法院的案件中，政府是否更容易赢”

### 技术上怎么做

- 诉讼是否发生 / 诉讼数量：
  - 用 LPM / logit / Poisson / PPML
- 胜诉结果：
  - 最好做案件级的 govt_win
  - 或在 unit-year 上只对 num_cases > 0 的样本定义条件胜率

### 重要提醒

如果 treatment 先影响了“是否被起诉”，再影响“是否胜诉”，
那么只在有案件的样本上回归胜率，会有 post-treatment selection。

👉 所以主文最好同时报告：

- suits filed
- conditional win
- appeal

------

# 七、推荐的数据结构（把复杂层级拆开）

不要试图用一张超级宽表解决所有问题。最好的结构是：

- 原始长表
- 清洗匹配表
- 分析面板

### 1. 原始长表

#### units

每行一个政府单位：

- unit_id
- standardized unit name
- unit_level（municipal government / bureau）
- city_id
- parent_unit_id
- unit_type

#### firms

每行一个律所：

- firm_id
- standardized firm name
- city_id
- size
- specialty

#### contracts

每行一份合同或一次中标记录：

- contract_id
- tender_id
- buyer_unit_id
- buyer_level
- service_type
- announce_date
- start_date
- end_date
- winner_firm_id
- amount

#### bids

如果能拿到投标信息，每行一个 bidder：

- tender_id
- bidder_firm_id
- shortlisted
- score
- rank
- bid_amount
- won

#### cases

每行一个行政案件：

- case_id
- filing_date
- judgment_date
- defendant_unit_id
- city_id
- case_type
- court_id
- govt_win
- appealed

#### case_counsel

如果能从文书中抽出代理信息，每行一个案件一方的代理关系：

- case_id
- side
- firm_id
- lawyer_id
- role

------

### 2. 分析面板

#### firm-year panel

用于估计 procurement 对律所的影响：

- firm_id
- year
- municipal_contract_active
- bureau_contract_active
- num_active_contracts
- private_cases
- admin_cases
- win_rate
- hires

#### unit-year panel

用于估计 procurement 对政府机关结果的影响：

- unit_id
- year
- own_contract_active
- parent_city_contract_active
- direct_rep_share
- num_admin_cases
- any_admin_case
- govt_win_rate_conditional
- appeal_rate_conditional

#### case-level panel

用于最强的政府结果分析：

- case_id
- defendant_unit_id
- filing_year
- own_contract_active
- parent_city_contract_active
- directly_represented_by_winning_firm
- govt_win

------

### 3. 链接逻辑

- 用 unit_id 连接 contracts 和 cases
- 用 firm_id 连接 contracts、bids、case_counsel
- 用 parent_unit_id 区分“本机关合同”与“上级市政府合同”
- 用 start_date / end_date 给案件和机关-年份打 treatment

👉 一旦这些键建立起来，多单位、多部门本身不是问题；
真正的问题是把它们粗暴合并成一个 city treated 指标

------

# 八、方法选择（算法层面总结）

## 推荐组合（三件套）

### 主结果

- Callaway–Sant’Anna 或 BJS

### 稳健性

- generalized synthetic control（gsynth）
- matrix completion

### 再稳健性

- doubly robust DID / weighting

------

## 重要提醒

这些方法：

✔ 修复 TWFE 偏误
✔ 改善反事实拟合

❌ 不解决内生选择本身

------

# 九、如何判断能不能 claim 因果

看 event study：

### 情况 1：pretrend 平

→ 可以较强 causal claim

### 情况 2：远期 lead 不平

→ 可以 claim 短期因果

### 情况 3：中标前就明显上升

→ ❗必须降格为：

> “consistent with the theory”
> 而不是 causal effect

------

# 十、写作策略（APSR positioning）

## 不要写：

“政府请律师 → 胜率提高”

## 要写：

> Authoritarian regimes reallocate rents around legal institutions
> rather than directly undermining them.

------

## 律所层面不要写：

causal effect of contracting

## 要写：

> market consequences of state affiliation

------

# 十一、推荐最终实证结构

### Part 1：Selection（非因果）

- 谁被选中
- 政治嵌入 vs 能力

------

### Part 2：Law firm outcomes（主机制）

- stacked DID（晚中标对照）
- matching
- procurement-level winner vs losers / runner-up
- announcement effect
- talent inflow

------

### Part 3：Government outcomes（机制与支持性）

- unit-level / case-level direct effect（主）
- city-level DID / synthetic DID（辅）
- suits filed + conditional win + appeal

------

### Part 4：Mechanism

- signal
- direct representation vs spillover
- business composition
- inequality

------

# 最关键一句总结

> 你这篇文章的识别，不是来自“随机中标”，而是来自
> **在最接近被选中的边际上，国家背书如何转化为市场租，并改变行为。**

------

# 十二、Data Log（2026-04-01）

## 1. 原始数据检查

本轮检查的数据目录为：

- `data/raw data/legal_procurement.dta`
- `data/raw data/treatment.dta`

初步判断：

- `legal_procurement.dta` 是主原始表
- `treatment.dta` 更像由采购数据聚合出来的 city-level treatment 表，而不是原始 procurement log

### `legal_procurement.dta` 的结构

- 14,686 行
- 15 列
- 关键字段包括：
  - `招采单位`
  - `中标单位`
  - `中标候选单位`
  - `中标金额`
  - `原始中标金额`
  - `Year`
  - `Month`
  - `Winners_Count`

### 原始表的重要发现

- 数据不是单纯的 city-level treatment，而是 procurement-event level records
- `招采单位` 列可用于把 treatment 下沉到机关层级
- `中标候选单位` 全量存在，但并不总是包含真实 runner-up 信息
- 约四分之一记录存在候选池人数大于 1 的情况
- 同一采购经常以多条记录出现，原因包括：
  - 多家入围 / 多赢家
  - 不同公告类型重复抓取
  - 完全重复记录

### `treatment.dta` 的结构

- 269 行
- 2 列：`城市`、`Year`
- 每个城市仅出现一次

👉 因此，后续分析应以 `legal_procurement.dta` 为底稿；`treatment.dta` 只能作为派生的 city-level treatment timing 表

------

## 2. 本轮清洗输出

新增清洗脚本：

- `analysis/clean_legal_procurement.py`

新增输出文件：

- `data/temp data/legal_procurement_row_cleaned.csv`
- `data/temp data/legal_procurement_row_cleaned.dta`
- `data/temp data/legal_procurement_tender_level.csv`
- `data/temp data/legal_procurement_tender_level.dta`
- `data/temp data/buyer_unit_lookup.csv`
- `data/temp data/buyer_unit_lookup.dta`
- `data/temp data/legal_procurement_cleaning_summary.txt`

### tender-level 清洗结果

- 原始行数：14,686
- 完全重复行：1,250
- 近似 tender-level 行数：9,766
- 唯一招采单位数：4,687

这里的 tender-level 是：

> 用 `招采单位 + 年月 + 候选池/赢家 + 原始金额文本` 近似重建的招标单元

换句话说，这不是官方 tender ID，而是研究用途的 `approx_tender_id`

------

## 3. 新增的关键字段

### A. tender-level 表

在 `legal_procurement_tender_level` 中新增了：

- `approx_tender_id`
- `candidate_names_ordered`
- `candidate_names_sorted`
- `candidate_count_parsed`
- `winner_names`
- `winner_count_unique`
- `competitive_candidate_pool`
- `runner_up_info_observed`
- `multi_winner_tender`
- `potential_grouping_ambiguity`
- `award_amount_total`

### B. 招采单位分类表

在 `buyer_unit_lookup` 中新增了：

- `buyer_org_type`
- `buyer_study_level`
- `buyer_study_bucket`
- `buyer_admin_tier`
- `buyer_class_rule`
- `buyer_class_confidence`

------

## 4. 招采单位分层规则（当前版本）

### 用于研究的核心层级

- `buyer_study_level = government_level`
  - 政府本级 / 政府办公室 / 政府法制办公室 / 街道办事处等
- `buyer_study_level = department_level`
  - 司法局、财政局、公安局、委员会、厅、分局、支队等
- `buyer_study_level = other_public_entity`
  - 法院、检察院、协会、学校、医院、公司、基金会等不直接进入主分析的单位

### 更贴近本文设计的 bucket

- `city_government_level`
- `city_department_level`
- `non_city_government_level`
- `non_city_department_level`
- `other_public_entity`

### 当前分类分布

- `city_government_level`: 23
- `city_department_level`: 1,250
- `non_city_government_level`: 665
- `non_city_department_level`: 2,548
- `other_public_entity`: 201

👉 对本文最重要的是：

- 如果研究城市政府本级顾问，应优先使用 `city_government_level`
- 如果研究委办局 / 行政机关采购，应优先使用 `city_department_level` 和 `non_city_department_level`

------

## 5. 当前可用于识别的 procurement 信息

### 可以支持的识别

- firm-level 的 procurement exposure measure
- unit-level treatment timing
- city-government vs department-level procurement distinction
- 一部分 winner vs losers / candidate-pool analysis

### 当前不足

- 没有官方 tender ID
- 没有精确到日的完整公告日期
- `中标候选单位` 不等于稳定可用的完整投标人名单
- 真正可用于 runner-up / close-call 的样本仍然有限

本轮清洗后：

- `competitive_candidate_pool = 1` 的 tender 约为 668
- `runner_up_info_observed = 1` 的 tender 约为 163

这意味着：

- procurement-level design 是可做的
- 但 close-call 设计应被视为强化识别或补充分析，而不是唯一主设计

------

## 6. 当前最稳的实证使用方式

基于这轮清洗，现阶段最稳的做法是：

- 用 `legal_procurement_tender_level` 生成：
  - `firm-year` 采购暴露面板
  - `unit-year` treatment 面板
  - `city-year` 的补充 treatment 面板
- 主分析优先使用：
  - `city_government_level`
  - `city_department_level`
  - `non_city_department_level`
- procurement-level 的候选人信息先作为：
  - winner vs losers 的补充识别
  - 或 mechanism / robustness analysis

------

## 7. 下一步数据工程优先级

1. 从 tender-level 表向上构造 `firm-year` 和 `unit-year` treatment panels
2. 对 `buyer_unit_lookup` 做一轮人工复核，特别是：
   - `other_public_entity`
   - `unknown / low confidence`
   - 名称明显缩写或缺失的单位
3. 与行政诉讼数据做单位名匹配：
   - `招采单位` ↔ `被告机关`
4. 如果后续需要更强 procurement-level 设计：
   - 优先识别真实候选人列表较完整的 tender
   - 再考虑 winner vs runner-up / close competition subsample

------

如果你下一步要推进，我可以帮你做三件很具体的事：

1. 把 firm-level empirical section 写成论文语言（可直接用）
2. 给你一套 R / Stata 代码框架（csdid + matching + event study）
3. 帮你设计最容易过 APSR 审稿的 figure（特别是 event study + mechanism 图）

你现在已经在一个很对的方向上了，接下来就是把识别边界讲清楚、把最强的 margin 做扎实。
