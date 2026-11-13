library(readxl)
library(dplyr)
library(tidyr)
library(writexl)
library(KimNIRA)
library(lme4)
library(performance)


#####导入并清洗数据#####
#binary_data <- read_excel("binary_data4.xlsx")

# 早晨：原第 d 行的 day_ris* 是第 d+1 个日历日的早上
# -> 新第 d 天（dayvar = d）的 beep 1
#morning <- binary_data %>%
#  select(BH, dayvar = day_num, matches("^day_ris\\d+$")) %>%
#  rename_with(~ sub("^day_", "", .x), starts_with("day_ris")) %>%
#  mutate(beepvar = 1L, period = "day")

# 晚上：原第 d 行的 night_ris* 是第 d 个日历日的晚上
# -> 新第 d-1 天（dayvar = d - 1）的 beep 2
#evening <- binary_data %>%
#  mutate(dayvar = day_num - 1L) %>%
#  select(BH, dayvar, matches("^night_ris\\d+$")) %>%
#  rename_with(~ sub("^night_", "", .x), starts_with("night_ris")) %>%
#  mutate(beepvar = 2L, period = "night")

#long_data <- bind_rows(morning, evening) %>%
#  filter(dayvar >= 1, dayvar <= 20) %>%
#  mutate(period = factor(period, levels = c("day", "night"))) %>%
#  arrange(BH, dayvar, beepvar) %>%
#  select(BH, dayvar, beepvar, period, ris1:ris4)

#write_xlsx(long_data, "binary_data_20days.xlsx")

long_data <- read_excel("binary_data_20days.xlsx")

# 压缩成“被试 × 天”的早晚标记
day_level <- long_data %>%
  group_by(BH, dayvar) %>%
  summarise(
    has_b1 = any(beepvar == 1L),   # 当天有早上(beep1)
    has_b2 = any(beepvar == 2L),   # 当天有晚上(beep2)
    .groups = "drop"
  ) %>%
  arrange(BH, dayvar) %>%
  group_by(BH) %>%
  mutate(
    has_b1_next = lead(has_b1),                        # 后一天是否有 beep1
    dayvar_next = lead(dayvar),
    # 同一天：早上 + 晚上
    same_day_pair = has_b1 & has_b2,
    # 前一天 beep2 -> 后一天 beep1（要求确实是相邻的两天）
    lag_pair = has_b2 & has_b1_next & (dayvar_next == dayvar + 1)
  ) %>%
  ungroup()


# 每个被试的统计
bh_summary <- day_level %>%
  group_by(BH) %>%
  summarise(
    n_beep1      = sum(has_b1),                    # beep1 天数
    n_beep2      = sum(has_b2),                    # beep2 天数
    n_b1_b2      = sum(has_b1 & has_b2),           # 同一天 beep1-beep2 配对数
    n_b2_next_b1 = sum(lag_pair, na.rm = TRUE),    # 前一天 beep2 - 后一天 beep1 数
    .groups = "drop"
  )

# 整体数量（剔除前）
cat("=== 剔除前 ===\n")
cat("被试数：", nrow(bh_summary), "\n")
cat("beep1-beep2 配对数：", sum(bh_summary$n_b1_b2), "\n")
cat("前一天beep2-后一天beep1 数：", sum(bh_summary$n_b2_next_b1), "\n\n")

# 剔除 beep1-beep2 完整配对不足10天的被试
keep_BH <- bh_summary %>%
  filter(n_b1_b2 >= 10) %>%
  pull(BH)

long_data_filt <- long_data %>%
  filter(BH %in% keep_BH)

bh_summary_filt <- bh_summary %>%
  filter(BH %in% keep_BH)

# 整体数量（剔除后）
cat("=== 剔除后 ===\n")
cat("被试数：", nrow(bh_summary_filt), "\n")
cat("beep1-beep2 配对数：", sum(bh_summary_filt$n_b1_b2), "\n")
cat("前一天beep2-后一天beep1 数：", sum(bh_summary_filt$n_b2_next_b1), "\n\n")




#####3.1 Basic characteristics of the sample#######
sub_data <- read_excel("binary_data_with_characteristics.xlsx")
sub_data$age <- as.numeric(sub_data$age)
mean(sub_data$age)#22.68
sd(sub_data$age)#2.91
table(sub_data$gender)#male 16; female 81
table(sub_data$education)#Junior college students 10; Undergraduate studentsies 67; Graduate students  20
table(sub_data$nation)#Han ethnicity 95; ethnic minorit 2
table(sub_data$`living expenses`)#Below 1000 yuan  11; 1001–1500 yuan 23; 1501–2000 yuan  30; 2001–3000 yuan  21; Above 3000 yuan  12


ld <- long_data_filt 
#被试依从性，单个被试早晚应答零
compliance <- ld %>%
  group_by(BH) %>%
  summarise(
    n_morning = sum(beepvar == 1L),
    n_evening = sum(beepvar == 2L),
    n_days    = n_distinct(dayvar),
    .groups = "drop"
  )

compliance_summary <- compliance %>%
  summarise(
    N            = n(),
    morning_M    = mean(n_morning),  morning_SD = sd(n_morning),
    morning_min  = min(n_morning),   morning_max = max(n_morning),
    evening_M    = mean(n_evening),  evening_SD = sd(n_evening),
    evening_min  = min(n_evening),   evening_max = max(n_evening),
    days_M       = mean(n_days),     days_SD = sd(n_days),
    morning_rate = sum(n_morning) / (n() * 20) * 100,
    evening_rate = sum(n_evening) / (n() * 20) * 100,
    total_obs    = nrow(ld)
  )


# 每个被试早-晚匹配应答率
complete_days <- long_data_filt %>%
  group_by(BH, dayvar) %>%
  summarise(
    both_beeps = all(c(1L, 2L) %in% beepvar),
    .groups = "drop"
  ) %>%
  # 每个被试的完整配对数
  group_by(BH) %>%
  summarise(
    n_complete_days = sum(both_beeps),
    .groups = "drop"
  )

# 平均值、SD、范围
complete_days %>%
  summarise(
    N   = n(),
    M   = mean(n_complete_days),
    SD  = sd(n_complete_days),
    Min = min(n_complete_days),
    Max = max(n_complete_days)
  )


#####3.2 Network Estimation #####

symptoms <- c("SC",  "CB", "OT", "NF")
colnames(long_data_filt)[5:8] <- c("SC",  "CB", "OT", "NF")


fit <- fitKIM(dat              = long_data_filt,
              vars             = symptoms,
              id               = "BH",
              dayvar           = "dayvar",
              beepvar          = "beepvar",
              allow_overnight  = FALSE,      # 只保留日内转移
              between_person   = "re",
              screen           = "fdr",
              fdr_q            = 0.10,
              min_obs          = 10,
              estimate_between = TRUE,      
              verbose          = TRUE)

fit$temporal_network
fit$edge_table
fit$between_network

#####3.3 Centrality measures#####

## 网络图与中心性
#单张网络图：type = "temporal" 或 "between"
p_temporal <- kimPlotNetwork(fit, type = "temporal")
p_between  <- kimPlotNetwork(fit, type = "between")

# 同时画时间网络与 between 网络
kimPlotAllNetworks(fit)

# 中心性指标
#   temporal 默认包含 InStrength、OutStrength、InEI、OutEI
#   between  默认包含 Strength、Closeness、Betweenness、ExpectedInfluence
cent <- kimCentrality(fit, use_ggplot = TRUE)

cent$temporal    # 时间网络中心性数据框
cent$between     # between 网络中心性数据框
cent$plots$temporal
cent$plots$between

#####3.4 Dynamic simulated network intervention#####

#基于 2 个标准差（Lunansky et al., 2022 的默认规则），用默认 intervention_sd = 2
res <- runKIMNIRA(fit, method = "exact",
                  intervention_type = "alleviating",
                  intervention_sd = 2, verbose = FALSE)

cat("排名第一的靶点：", res$ranked_targets[1], "\n\n")
print(res$summary[, c("node", "benefit", "direct_benefit", "spillover_benefit", "rank")],
      digits = 4, row.names = FALSE)

##NIRA 图
p_nira <- kimPlotNIRA(
  res,
  perturbation_type = "alleviating"
)
print(p_nira)

#轨迹图
p_traj_node <- kimPlotTrajectory(res, type = "node", target = res$ranked_targets[1])
print(p_traj_node)


#稳定性检验
boot <- kimStabilityBootstrap(
  obj       = res,
  data      = long_data_filt,
  n_boot    = 1000,          # 先小规模试跑；正式分析改大
  seed      = 2025,
  parallel  = TRUE,        # Windows 下可设 TRUE 并给 ncores
  ncores = 6,
  progress  = TRUE
)
#saveRDS(boot, file = "boot_kimStability.rds")
boot$rank_proportions
p_stab <- kimPlotStability(boot, high = length(symptoms))
print(p_stab)




#基于 2 个标准差（Lunansky et al., 2022 的默认规则），用默认 intervention_sd = 2
res1 <- runKIMNIRA(fit, method = "exact",
                  intervention_type = "aggravating",
                  intervention_sd = 2, verbose = FALSE)

cat("排名第一的靶点：", res1$ranked_targets[1], "\n\n")
print(res1$summary[, c("node", "benefit", "direct_benefit", "spillover_benefit", "rank")],
      digits = 4, row.names = FALSE)


##NIRA 图
p_nira1 <- kimPlotNIRA(
  res1,
  perturbation_type = "aggravating"
)
print(p_nira1)

#轨迹图
p_traj_node1 <- kimPlotTrajectory(res1, type = "node", target = res1$ranked_targets[1])
print(p_traj_node1)


#稳定性检验
boot1 <- kimStabilityBootstrap(
  obj       = res1,
  data      = long_data_filt,
  n_boot    = 1000,          # 先小规模试跑；正式分析改大
  seed      = 2025,
  parallel  = TRUE,        # Windows 下可设 TRUE 并给 ncores
  ncores = 6,
  progress  = TRUE
)
#saveRDS(boot1, file = "boot1_kimStability.rds")
boot1$rank_proportions
p_stab1 <- kimPlotStability(boot1, high = length(symptoms))
print(p_stab1)


###拼图
library(patchwork)
library(ggplot2)

combined_plot <- (p_nira | p_traj_node) / 
  (p_stab| p_nira1)/
  (p_traj_node1| p_stab1)+
  plot_annotation(
    tag_levels = 'A', # 自动打上 A, B, C, D 的标签
    tag_prefix = '(', 
    tag_suffix = ')',
    title = "",
    theme = theme(plot.title = element_text(size = 4, face = "bold", hjust = 0.5))
  )

# 查看拼图
print(combined_plot)



##模拟缓解干预的稳定性景观
land_iv <- kimBistability(res)

p_land <- kimPlotLandscape(
  land_iv,
  conditions = c("baseline", "SC", "CB", "OT", "NF"),
  per_state = FALSE,          # 除以 C(p, K)，去掉组合数导致的中间态虚高
  type      = "potential"  #probability 直接画 P(K)，而不是势能 U(K)，potential
)
print(p_land)

kimPhaseStability(
  land       = land_iv,
  conditions = c("baseline", "SC", "CB", "OT", "NF"),
  threshold  = 3
)



##模拟加重干预的稳定性景观
land_iv1 <- kimBistability(res1)

p_land1 <- kimPlotLandscape(
  land_iv1,
  conditions = c("baseline", "SC", "CB", "OT", "NF"),
  per_state = FALSE,          # 除以 C(p, K)，去掉组合数导致的中间态虚高
  type      = "potential"  #probability 直接画 P(K)，而不是势能 U(K)，potential
)
print(p_land1)

kimPhaseStability(
  land       = land_iv1,
  conditions = c("baseline", "SC", "CB", "OT", "NF"),
  threshold  = 3
)


combined_plot1 <- (p_land | p_land1)+
  plot_annotation(
    tag_levels = 'A', # 自动打上 A, B, C, D 的标签
    tag_prefix = '(', 
    tag_suffix = ')',
    title = "",
    theme = theme(plot.title = element_text(size = 4, face = "bold", hjust = 0.5))
  )

# 查看拼图
print(combined_plot1)












##轨迹图

p_traj_summary <- kimPlotTrajectory(res, type = "summary")
p_traj_node    <- kimPlotTrajectory(res, type = "node", target = res$ranked_targets[1])
p_traj_diff    <- kimPlotTrajectory(res, type = "diff")
p_traj_panel   <- kimPlotTrajectory(res, type = "panel")


print(p_traj_summary)
print(p_traj_node)
print(p_traj_diff)
print(p_traj_panel)



p_traj_summary1 <- kimPlotTrajectory(res1, type = "summary")
p_traj_node1    <- kimPlotTrajectory(res1, type = "node", target = res1$ranked_targets[1])
p_traj_diff1    <- kimPlotTrajectory(res1, type = "diff")
p_traj_panel1   <- kimPlotTrajectory(res1, type = "panel")


print(p_traj_summary1)
print(p_traj_node1)
print(p_traj_diff1)
print(p_traj_panel1)












# 双稳/景观：如果关心是否存在多个稳定盆地
land <- kimBistability(fit)
print(land)

p_land2 <- kimPlotLandscape(
  land,
  per_state = TRUE,          # 除以 C(p, K)，去掉组合数导致的中间态虚高
  type      = "probability"  # 直接画 P(K)，而不是势能 U(K)，potential
)
print(p_land2)

# 景观字段说明：
land$count_distribution   # 各条件(基线/干预)下按激活数 K 的稳态概率
land$spectral             # 特征值/谱隙（双稳性的谱证据）

#想看干预如何改变景观,用下面的代码
#land2 <- kimBistability(fit, delta = -1, targets = symptoms)
#land2$interventions    # 每个目标干预后的景观变化


land_iv <- kimBistability(res)

#land_iv$count_distribution[
#  land_iv$count_distribution$condition == "Loneliness", ]

#land_iv$count_distribution[
#  land_iv$count_distribution$condition == "Anx", ]

p_land3 <- kimPlotLandscape(
  land_iv,
  conditions = c("baseline", "SC", "CB", "OT", "NF"),
  per_state = FALSE,          # 除以 C(p, K)，去掉组合数导致的中间态虚高
  type      = "potential"  #probability 直接画 P(K)，而不是势能 U(K)，potential
)
print(p_land3)


kimPhaseStability(
  land       = land_iv,
  conditions = c("baseline", "SC", "CB", "OT", "NF"),
  threshold  = 3
)



# 同期依赖：控制滞后后，其他节点的当期状态是否仍有额外解释力
# 显著不代表模型错误，但说明“仅滞后”的转移模型可能遗漏同期结构。
cont <- kimContemporaneousCheck(fit, long_data_filt)
print(cont)



