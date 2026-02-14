# -------------------------------------------------------------------
# NHANES 2011-2014: 阶段 0 (A) - 人口统计学数据处理
# -------------------------------------------------------------------

# 步骤 1: 加载必要的 R 包
# "haven" 用于读取 SAS (XPT) 文件
# "dplyr" 用于数据操作（选择、过滤、合并、创建新变量）
# 如果您没有安装，请先运行: install.packages(c("haven", "dplyr"))

library(haven)
library(dplyr)

# 步骤 2: 定义需要提取的变量列表
# 定义一个变量向量，方便后续在两个数据集中统一提取
# 我们必须包括 "SEQN"，这是每个参与者的唯一ID号
vars_to_keep <- c(
  "SEQN",       # 参与者唯一ID
  "RIDAGEYR",   # 年龄
  "RIDRETH1",   # 种族/民族
  "RIAGENDR",   # 性别
  "INDFMPIR",   # 家庭收入贫困比
  "SDMVPSU",    # 抽样：伪PSU
  "SDMVSTRA",   # 抽样：伪分层
  "WTMEC2YR"    # 抽样：2年MEC检查权重
)

# 步骤 3: 导入 2011-2012 (G周期) 数据
# 假设数据文件 "DEMO_G.XPT" 与R脚本在同一工作目录中
message("正在加载 DEMO_G (2011-2012)...")
demo_g <- read_xpt("DEMO_G.XPT")

# 检查：确认导入，并查看变量名
# print(colnames(demo_g)) 

# 仅保留我们需要的变量
demo_g_selected <- demo_g %>%
  select(all_of(vars_to_keep))

# 步骤 4: 导入 2013-2014 (H周期) 数据
message("正在加载 DEMO_H (2013-2014)...")
demo_h <- read_xpt("DEMO_H.XPT")

# 仅保留我们需要的变量
demo_h_selected <- demo_h %>%
  select(all_of(vars_to_keep))

# 步骤 5: 纵向合并两个周期的数据
# bind_rows() 函数会“垂直”堆叠两个数据框
message("正在合并两个周期的数据...")
demo_combined <- bind_rows(demo_g_selected, demo_h_selected)

# 检查：查看合并后的总行数
# message(paste("合并后总行数:", nrow(demo_combined)))

# -------------------------------------------------------------------
# 步骤 6: 预处理合并后的 DEMO 数据 (关键步骤)
# -------------------------------------------------------------------
# 我们将按以下顺序执行：
# 1. 处理权重（过滤缺失值、计算4年权重）
# 2. 处理分类变量（转换为因子 Factor）
# 3. 处理缺失值（NA）- (由 haven::read_xpt 自动完成)

demo_processed <- demo_combined %>%
  
  # --- 6.1 权重处理 (按您的要求) ---
  # 过滤掉 2年MEC权重 (WTMEC2YR) 为 NA (缺失) 的个体
  # 这是必要的，因为没有权重他们无法被纳入加权分析
  filter(!is.na(WTMEC2YR)) %>%
  
  # 检查：我们也应过滤掉权重为 0 的个体（虽然罕见，但存在）
  filter(WTMEC2YR > 0) %>%
  
  # 计算 4年MEC权重 (WTMEC4YR)
  # 算法: WTMEC4YR = (1/2) * WTMEC2YR (适用于2011-2014周期)
  mutate(WTMEC4YR = (1/2) * WTMEC2YR) %>%
  
  # --- 6.2 分类变量转换为因子 (按您的要求) ---
  mutate(
    
    # 性别 (RIAGENDR): 1 = Male, 2 = Female
    # 我们将其转换为R的因子(Factor)类型，并提供有意义的标签
    RIAGENDR = factor(RIAGENDR,
                      levels = c(1, 2),
                      labels = c("Male", "Female")),
    
    # 种族/民族 (RIDRETH1): 1=MexAm, 2=OthHisp, 3=NH White, 4=NH Black, 5=OthRace
    RIDRETH1 = factor(RIDRETH1,
                      levels = c(1, 2, 3, 4, 5),
                      labels = c("Mexican American",
                                 "Other Hispanic",
                                 "Non-Hispanic White",
                                 "Non-Hispanic Black",
                                 "Other Race/Multi-Racial"))
    
    # 注意: INDFMPIR (收入比) 是连续数值变量，我们保留为 numeric
    # 注意: RIDAGEYR (年龄) 是连续数值变量，我们保留为 numeric
  )

# -------------------------------------------------------------------
# 步骤 7: 完成与核查
# -------------------------------------------------------------------
message("DEMO 数据处理完毕。")

# 检查：显示最终数据框的头部 (前6行)
print("--- 最终 DEMO 数据 (头部) ---")
print(head(demo_processed))

# 检查：显示最终数据框的结构，以核查变量类型
print("--- 最终 DEMO 数据 (结构) ---")
str(demo_processed)

# 检查：查看 RIAGENDR 和 RIDRETH1 是否已成功转换为因子
print("--- 因子变量核查 (RIAGENDR) ---")
print(table(demo_processed$RIAGENDR))

print("--- 因子变量核查 (RIDRETH1) ---")
print(table(demo_processed$RIDRETH1))


# -------------------------------------------------------------------
# NHANES 2011-2014: 阶段 0 (A) - 导入结局与协变量 (续)
# -------------------------------------------------------------------

# 假设 `demo_processed` 数据框已存在于您的R环境中
# 假设 `haven` 和 `dplyr` 包已经加载

# 步骤 8: 定义一个“辅助函数”来简化数据加载
# -------------------------------------------------------------------
# 我们需要为G周期和H周期加载 *大量* 文件。
# 这个函数将自动：
# 1. 加载 G 周期文件 (e.g., "DXX_G.XPT")
# 2. 加载 H 周期文件 (e.g., "DXX_H.XPT")
# 3. 仅保留我们需要的变量
# 4. 将它们合并 (bind_rows)
# 这会让我们的主代码非常干净且易于阅读。

load_and_merge_cycles <- function(file_prefix, var_list) {
  
  # 检查：确保 SEQN (唯一ID) 始终被包括在内
  if (!"SEQN" %in% var_list) {
    var_list <- c("SEQN", var_list)
  }
  
  # 定义 G 和 H 文件的路径
  file_g <- paste0(file_prefix, "_G.XPT")
  file_h <- paste0(file_prefix, "_H.XPT")
  
  # message(paste("--- 正在加载:", file_prefix, "---")) # (调试时可取消注释)
  
  # 加载并选择变量
  data_g <- read_xpt(file_g) %>% select(all_of(var_list))
  data_h <- read_xpt(file_h) %>% select(all_of(var_list))
  
  # 合并两个周期并返回
  data_combined <- bind_rows(data_g, data_h)
  
  return(data_combined)
}


# -------------------------------------------------------------------
# 步骤 9: 应用年龄筛选并开始构建 "静态宽表"
# -------------------------------------------------------------------

# 步骤 9.1: 应用年龄筛选
# 我们的研究对象是 40 岁及以上
static_wide_data <- demo_processed %>%
  filter(RIDAGEYR >= 40)

message(paste("1. 年龄 >= 40 岁的样本 (N2):", nrow(static_wide_data), "人"))


# -------------------------------------------------------------------
# 步骤 10: 依次并入 (Left Join) 所有数据模块
# -------------------------------------------------------------------
# 我们将使用 `left_join`。
# 这意味着我们保留 `static_wide_data` 中的所有人 (我们的N2队列)，
# 并将其他数据 "粘贴" 到他们身上。
# 如果某人没有DXA数据，他们仍会留在表中，但DXA变量会显示为 NA。

# 步骤 10.1: 并入 DXX (DXA 瘦体重)
vars_dxx <- c("DXDLALE", "DXDRALE", "DXDLLLE", "DXDRLLE")
data_dxx <- load_and_merge_cycles("DXX", vars_dxx)
static_wide_data <- static_wide_data %>%
  left_join(data_dxx, by = "SEQN")

# 步骤 10.2: 并入 BMX (身高)
vars_bmx_ht <- c("BMXHT")
data_bmx_ht <- load_and_merge_cycles("BMX", vars_bmx_ht)
static_wide_data <- static_wide_data %>%
  left_join(data_bmx_ht, by = "SEQN")

# 步骤 10.3: 并入 MGX (握力)
vars_mgx <- c("MGXH1T1", "MGXH2T1", "MGXH1T2", "MGXH2T2", "MGXH1T3", "MGXH2T3")
data_mgx <- load_and_merge_cycles("MGX", vars_mgx)
static_wide_data <- static_wide_data %>%
  left_join(data_mgx, by = "SEQN")

# 步骤 10.4: 并入 BMX (BMI)
# (博士请注意：如您所见，我们再次加载了 BMX 文件。
# 在实际操作中，将 10.2 和 10.4 合并为一步加载会更高效。
# 但为了严格遵循您的列表顺序以便于核查，我在此处分开执行。)
vars_bmx_bmi <- c("BMXBMI")
data_bmx_bmi <- load_and_merge_cycles("BMX", vars_bmx_bmi)
static_wide_data <- static_wide_data %>%
  left_join(data_bmx_bmi, by = "SEQN")

# 步骤 10.5: 并入膳食数据 (Day 1)
vars_dr1 <- c("DR1TKCAL", "DR1TPROT")
data_dr1 <- load_and_merge_cycles("DR1TOT", vars_dr1)
static_wide_data <- static_wide_data %>%
  left_join(data_dr1, by = "SEQN")

# 步骤 10.6: 并入膳食数据 (Day 2)
vars_dr2 <- c("DR2TKCAL", "DR2TPROT")
data_dr2 <- load_and_merge_cycles("DR2TOT", vars_dr2)
static_wide_data <- static_wide_data %>%
  left_join(data_dr2, by = "SEQN")

# 步骤 10.7: 并入 VID (维生素D)
vars_vid <- c("LBXVIDMS")
data_vid <- load_and_merge_cycles("VID", vars_vid)
static_wide_data <- static_wide_data %>%
  left_join(data_vid, by = "SEQN")

# 步骤 10.8: 并入 SMQ (吸烟)
vars_smq <- c("SMQ020", "SMQ040")
data_smq <- load_and_merge_cycles("SMQ", vars_smq)
static_wide_data <- static_wide_data %>%
  left_join(data_smq, by = "SEQN")

# 步骤 10.9: 并入 ALQ (饮酒)
vars_alq <- c("ALQ130", "ALQ120U")
data_alq <- load_and_merge_cycles("ALQ", vars_alq)
static_wide_data <- static_wide_data %>%
  left_join(data_alq, by = "SEQN")

# 步骤 10.10: 并入 BPQ (高血压问卷)
vars_bpq <- c("BPQ040A", "BPQ050A")
data_bpq <- load_and_merge_cycles("BPQ", vars_bpq)
static_wide_data <- static_wide_data %>%
  left_join(data_bpq, by = "SEQN")

# 步骤 10.11: 并入 BPX (血压测量)
vars_bpx <- c("BPXSY1", "BPXSY2", "BPXSY3", "BPXDI1", "BPXDI2", "BPXDI3")
data_bpx <- load_and_merge_cycles("BPX", vars_bpx)
static_wide_data <- static_wide_data %>%
  left_join(data_bpx, by = "SEQN")

# 步骤 10.12: 并入 DIQ (糖尿病问卷)
vars_diq <- c("DIQ010", "DIQ050", "DIQ070")
data_diq <- load_and_merge_cycles("DIQ", vars_diq)
static_wide_data <- static_wide_data %>%
  left_join(data_diq, by = "SEQN")

# 步骤 10.13: 并入 GLU (空腹血糖)
vars_glu <- c("LBXGLU")
data_glu <- load_and_merge_cycles("GLU", vars_glu)
static_wide_data <- static_wide_data %>%
  left_join(data_glu, by = "SEQN")

# 步骤 10.14: 并入 GHB (糖化血红蛋白)
vars_ghb <- c("LBXGH")
data_ghb <- load_and_merge_cycles("GHB", vars_ghb)
static_wide_data <- static_wide_data %>%
  left_join(data_ghb, by = "SEQN")

# 步骤 10.15: 并入 FASTQX (空腹时长)
vars_fast <- c("PHAFSTHR")
data_fast <- load_and_merge_cycles("FASTQX", vars_fast)
static_wide_data <- static_wide_data %>%
  left_join(data_fast, by = "SEQN")

# 步骤 10.16: 并入 MCQ (慢性病史)
vars_mcq <- c("MCQ160A", "MCQ160B", "MCQ160C", "MCQ160E", "MCQ160F", "MCQ220")
# 注意：NHANES 文件名中的变量名有时是大写的 (MCQ160A)，
# 但在 XPT 文件中可能是小写 (mcq160a)。`load_and_merge_cycles` 
# 使用 `all_of()` 是区分大小写的。
# *更正*： `read_xpt` 默认将列名转为大写，所以使用大写是稳健的。
# 您的列表中是小写，我在此处使用大写，因为这是 XPT 导入的标准行为。
vars_mcq_upper <- toupper(vars_mcq)
data_mcq <- load_and_merge_cycles("MCQ", vars_mcq_upper)
static_wide_data <- static_wide_data %>%
  left_join(data_mcq, by = "SEQN")


# -------------------------------------------------------------------
# 步骤 11: 完成与核查
# -------------------------------------------------------------------
message("阶段 0 (A) 静态宽表构建完毕。")

# 检查：显示最终数据框的维度
message(paste("最终宽表维度 (N2):", 
              nrow(static_wide_data), "行 (参与者),",
              ncol(static_wide_data), "列 (变量)"))

# 检查：显示数据框的头部 (仅显示部分新加入的变量)
print("--- 静态宽表 (头部) ---")
print(head(static_wide_data %>% select(SEQN, RIDAGEYR, DXDLALE, BMXHT, MGXH1T1, BMXBMI, DR1TKCAL, SMQ020)))

# 检查：查看新加入变量的摘要，以快速浏览缺失值 (NA)
print("--- 静态宽表 (部分变量摘要) ---")
summary(static_wide_data %>% select(DXDLALE, BMXHT, MGXH1T1, BMXBMI, DR1TKCAL, LBXVIDMS, SMQ020))



# -------------------------------------------------------------------
# NHANES 2011-2014: 阶段 0 (A) - 变量衍生 (ASMI & Max Grip)
# -------------------------------------------------------------------

# 我们的起始数据框是 `static_wide_data`
# 假设 `dplyr` 包已加载

# 步骤 12: 计算结局变量 1 - ASMI (四肢骨骼肌质量指数)
# -------------------------------------------------------------------
message("正在衍生变量：ASMI ...")

# 我们将使用 `mutate` 函数在数据框中创建新列
derived_data <- static_wide_data %>%
  mutate(
    
    # 步骤 12.1: 计算总四肢瘦体重 (ASM)，单位：千克 (kg)
    # 原始 DXX 变量单位是 克 (g)，所以我们需要除以 1000
    # 注意: 如果任何一个 DXX 变量是 NA (缺失)，ASM_kg 也会是 NA
    ASM_kg = (DXDLALE + DXDRALE + DXDLLLE + DXDRLLE) / 1000,
    
    # 步骤 12.2: 计算身高，单位：米 (m)
    # 原始 BMXHT 变量单位是 厘米 (cm)，所以我们需要除以 100
    Height_m = BMXHT / 100,
    
    # 步骤 12.3: 计算 ASMI
    # 公式: ASMI = 总四肢瘦体重 (kg) / 身高 (m) 的平方
    ASMI = ASM_kg / (Height_m^2)
  )

# 步骤 13: 计算结局变量 2 - Max_Grip_Strength (最大握力)
# -------------------------------------------------------------------
message("正在衍生变量：Max_Grip_Strength ...")

# 目标：从6个握力测试值中 (3次左手, 3次右手)，找出最大的那个值
#
# 警告 (新手注意)：
# 我们不能简单地写 `max(MGXH1T1, MGXH2T1, ...)`
# 因为 `max()` 函数是 "垂直" 工作的 (计算整个列的最大值)
# 我们需要 "水平" 工作 (计算 *每一行* 的最大值)
#
# 解决方案：
# 1. 使用 `rowwise()` 告诉 dplyr 开始逐行操作
# 2. 使用 `max(c(...), na.rm = TRUE)` 来收集该行的所有值并计算最大值
# 3. 使用 `ungroup()` 恢复正常操作 (这步*至关重要*)

derived_data <- derived_data %>%
  rowwise() %>% # 告诉 dplyr: 下面的操作请逐行执行
  mutate(
    
    # 步骤 13.1: 计算临时最大值
    # `c(...)` 将6个变量组合成一个临时向量 (列表)
    # `na.rm = TRUE` 确保在计算max时忽略缺失值 (NA)
    temp_max = max(c(MGXH1T1, MGXH2T1, MGXH1T2, MGXH2T2, MGXH1T3, MGXH2T3), na.rm = TRUE),
    
    # 步骤 13.2: 处理全部缺失的特殊情况 (重要！)
    # 如果某人所有6次测试都是 NA，`max(..., na.rm=TRUE)` 会返回 `-Inf`
    # 这会破坏我们的回归模型。我们必须将其改回 NA。
    # `is.finite()` 可以完美地检测到 `-Inf`
    Max_Grip_Strength = if_else(is.finite(temp_max), temp_max, NA_real_)
    
  ) %>%
  ungroup() %>% # 恢复为标准的数据框操作模式
  select(-temp_max) # 移除我们不再需要的临时变量


# -------------------------------------------------------------------
# 步骤 14: 完成与核查
# -------------------------------------------------------------------
message("ASMI 和 Max_Grip_Strength 衍生完毕。")

# 检查：显示新变量的摘要，以核查计算结果
print("--- 新衍生变量 (ASMI, Max_Grip_Strength) 摘要 ---")
summary(derived_data %>% select(ASMI, Max_Grip_Strength))

# 检查：查看头部，确认新列已添加
print("--- 包含新变量的数据框 (头部) ---")
print(head(derived_data %>% select(SEQN, ASM_kg, Height_m, ASMI, Max_Grip_Strength)))





# -------------------------------------------------------------------
# NHANES 2011-2014: 阶段 0 (A) - 变量衍生 (高血压协变量)
# -------------------------------------------------------------------



message("正在衍生变量：Hypertension (htn) ...")

# 步骤 15: 衍生高血压 (Hypertension) 变量
# -------------------------------------------------------------------
# 我们将在一个 `mutate` 管道中完成你逻辑中的所有步骤

derived_data_v2 <- derived_data %>% # 将结果存入新数据框以便核查
  
  # -----------------
# 步骤 15.1: 问卷判定 (htn_q)
# -----------------
mutate(
  # 完全按照您的逻辑:
  # 1 = "是" (任一为1)
  # 2 = "否" (两者都为2)
  # NA = 其他 (包括 7, 9, 和原始 NA)
  htn_q = case_when(
    BPQ040A == 1 | BPQ050A == 1 ~ 1, # 曾被告知服药 或 正在服药
    BPQ040A == 2 & BPQ050A == 2 ~ 2, # 明确回答两个"否"
    TRUE                       ~ NA_real_ # 其他情况 (7, 9, .) 均为 NA
  ),
  
  # 检查: 查看 htn_q 的分布
  # print(table(htn_q, useNA = "ifany"))
  
  
  # -----------------
  # 步骤 15.2: 计算有效血压均值 (sbp_avg, dbp_avg)
  # -----------------
  # 我们使用 rowwise()，这和计算 Max_Grip_Strength 的逻辑一样
) %>%
  rowwise() %>% # 告诉 dplyr: 下面的操作请逐行执行
  mutate(
    
    # 计算收缩压 (Systolic) 平均值
    # mean(c(...), na.rm = TRUE) 会计算所有非NA读数的平均值
    sbp_avg = mean(c(BPXSY1, BPXSY2, BPXSY3), na.rm = TRUE),
    
    # 计算舒张压 (Diastolic) 平均值
    dbp_avg = mean(c(BPXDI1, BPXDI2, BPXDI3), na.rm = TRUE),
    
    # 关键核查：
    # 如果某人 3 次读数都是 NA, mean(..., na.rm=TRUE) 会返回 NaN
    # 我们必须像处理握力时一样，将 NaN 转换回 NA
    sbp_avg = ifelse(is.nan(sbp_avg), NA_real_, sbp_avg),
    dbp_avg = ifelse(is.nan(dbp_avg), NA_real_, dbp_avg)
    
  ) %>%
  ungroup() %>% # 必须 ungroup() 来恢复正常操作
  
  
  # -----------------
# 步骤 15.3 & 15.4: 血压判定 (htn_bp) 和 综合结果 (htn)
# -----------------
mutate(
  
  # 步骤 15.3: 血压判定 (htn_bp)
  # 1 = 高血压 (>=130 或 >=80)
  # 2 = 正常血压
  # NA = 血压数据缺失
  htn_bp = case_when(
    is.na(sbp_avg) | is.na(dbp_avg) ~ NA_real_,  # 只要有一个平均值是NA，就无法判断
    sbp_avg >= 130 | dbp_avg >= 80  ~ 1,         # 达到高血压标准
    TRUE                           ~ 2          # 未达到高血压标准
  ),
  
  # 步骤 15.4: 综合结果 (htn) - 我们的最终变量
  # 1 = 有高血压 (Yes)
  # 2 = 没有高血压 (No)
  # NA = 无法判断
  htn = case_when(
    htn_q == 1   ~ 1, # 问卷判定为 "是" -> 最终为 1
    htn_q == 2   ~ htn_bp, # 问卷判定为 "否" -> 采信血压测量结果
    is.na(htn_q) ~ htn_bp  # 问卷缺失 -> 采信血压测量结果
  ),
  
  # 将 htn 转换为因子 (Factor)，这在后续回归中是好习惯
  htn = factor(htn, levels = c(2, 1), labels = c("No", "Yes"))
)


# -------------------------------------------------------------------
# 步骤 16: 完成与核查
# -------------------------------------------------------------------
message("高血压 (htn) 衍生完毕。")

# 检查：显示新衍生的中间变量和最终变量
print("--- 高血压相关变量 (中间过程) 摘要 ---")
summary(derived_data_v2 %>% select(htn_q, sbp_avg, dbp_avg, htn_bp))

# 检查：显示最终的 htn 变量的频数表
print("--- 最终高血压 (htn) 变量分布 ---")
print(table(derived_data_v2$htn, useNA = "ifany"))

# 检查：清理掉我们不再需要的中间变量
derived_data_v2 <- derived_data_v2 %>%
  select(-htn_q, -sbp_avg, -dbp_avg, -htn_bp)

# 检查：查看数据框头部
print("--- 数据框头部 (已添加 htn) ---")
print(head(derived_data_v2 %>% select(SEQN, BPQ040A, BPQ050A, BPXSY1, BPXSY2, htn)))





# -------------------------------------------------------------------
# NHANES 2011-2014: 阶段 0 (A) - 变量衍生 (糖尿病协变量)
# -------------------------------------------------------------------

# 我们的起始数据框是 `derived_data_v2` (它已经包含了 htn)
# 假设 `dplyr` 包已加载

message("正在衍生变量：Diabetes (dm) ...")

# 步骤 17: 衍生糖尿病 (Diabetes) 变量
# -------------------------------------------------------------------
# 我们将使用与高血压类似的分步逻辑：
# 1. 问卷判定 (dm_q) - 基于 DIQ010, DIQ050, DIQ070
# 2. 实验室判定 (dm_lab) - 基于 LBXGH (HbA1c) 和 LBXGLU (空腹血糖)
# 3. 综合判定 (dm) - 合并 1 和 2

derived_data_v3 <- derived_data_v2 %>% # 将结果存入新数据框 v3
  
  # -----------------
# 步骤 17.1: 问卷判定 (dm_q)
# -----------------
# 逻辑：如果(1)被告知有 (DIQ010=1) 或 (2)在用胰岛素 (DIQ050=1) 或 (3)在用口服药 (DIQ070=1)
#      则为 "是" (1)
#      如果(1)明确说没有 (DIQ010=2 或 3) 且 (2)没用胰岛素 (DIQ050=2) 且 (3)没用口服药 (DIQ070=2)
#      则为 "否" (2)
#      其他情况 (7, 9, .) 均为 NA
mutate(
  dm_q = case_when(
    DIQ010 == 1 | DIQ050 == 1 | DIQ070 == 1 ~ 1, # 满足任一 "是"
    
    (DIQ010 == 2 | DIQ010 == 3) & # 明确说 "否" (2) 或 "边缘" (3)
      (DIQ050 == 2) &               # 并且 明确说 "否"
      (DIQ070 == 2)                 # 并且 明确说 "否"
    ~ 2, 
    
    TRUE ~ NA_real_ # 其他情况 (7, 9, .) 均为 NA
  ),
  
  # -----------------
  # 步骤 17.2: 实验室判定 (中间变量)
  # -----------------
  # 我们需要分别判断 HbA1c 和 空腹血糖 (FPG)
  
  # 17.2a: HbA1c (糖化血红蛋白) 标准
  # 1 = 是 (>= 6.5%), 2 = 否 (< 6.5%), NA = 缺失
  hba1c_criterion = case_when(
    LBXGH >= 6.5 ~ 1,
    LBXGH < 6.5  ~ 2,
    TRUE         ~ NA_real_
  ),
  
  # 17.2b: FPG (空腹血糖) 标准
  # 1 = 是 (>= 126 mg/dL 且 空腹 >= 8 小时)
  # 2 = 否 (   < 126 mg/dL 且 空腹 >= 8 小时)
  # NA = 其他 (空腹 < 8 小时 或 血糖/空腹时长数据缺失)
  fpg_criterion = case_when(
    PHAFSTHR >= 8 & LBXGLU >= 126 ~ 1,
    PHAFSTHR >= 8 & LBXGLU < 126  ~ 2,
    TRUE                          ~ NA_real_ # 空腹时间不够或数据缺失，无法判断
  ),
  
  # -----------------
  # 步骤 17.3: 综合实验室判定 (dm_lab)
  # -----------------
  # 逻辑: 只要 HbA1c 或 FPG 任一标准为 "是"(1)，则 dm_lab 为 "是"(1)
  #      仅在 *至少一个* 标准为 "否"(2) 且 *没有* 标准为 "是"(1) 时，才为 "否"(2)
  #      如果两个标准都无法判断 (NA)，则 dm_lab 为 NA
  
  dm_lab = case_when(
    hba1c_criterion == 1 | fpg_criterion == 1 ~ 1, # 任一为 "是"
    hba1c_criterion == 2 | fpg_criterion == 2 ~ 2, # 至少一个为 "否" (且无 "是")
    TRUE                                     ~ NA_real_ # 两个都是 NA
  ),
  
  # -----------------
  # 步骤 17.4: 最终综合结果 (dm) - 我们的最终变量
  # -----------------
  # 逻辑: 只要 问卷(dm_q) 或 实验室(dm_lab) 任一为 "是"(1)，则最终为 "是"(1)
  #      仅在 问卷(dm_q) 或 实验室(dm_lab) 至少一个为 "否"(2) 且 *没有* 一个为 "是"(1) 时，
  #      才判定为 "否"(2)
  
  dm = case_when(
    dm_q == 1 | dm_lab == 1 ~ 1, # 问卷或实验室任一判定为 "是"
    dm_q == 2 | dm_lab == 2 ~ 2, # 问卷或实验室至少一个判定为 "否" (且无 "是")
    TRUE                      ~ NA_real_ # 两者都无法判断
  ),
  
  # 将 dm 转换为因子 (Factor)，这在后续回归中是好习惯
  dm = factor(dm, levels = c(2, 1), labels = c("No", "Yes"))
)


# -------------------------------------------------------------------
# 步骤 18: 完成与核查
# -------------------------------------------------------------------
message("糖尿病 (dm) 衍生完毕。")

# 检查：显示新衍生的中间变量
print("--- 糖尿病相关变量 (中间过程) 摘要 ---")
summary(derived_data_v3 %>% select(dm_q, hba1c_criterion, fpg_criterion, dm_lab))

# 检查：显示最终的 dm 变量的频数表
print("--- 最终糖尿病 (dm) 变量分布 ---")
print(table(derived_data_v3$dm, useNA = "ifany"))

# 检查：清理掉我们不再需要的中间变量
# 注意：我们保留了 `derived_data_v3` 以便后续使用
final_derived_data <- derived_data_v3 %>%
  select(-dm_q, -hba1c_criterion, -fpg_criterion, -dm_lab)

# 检查：查看数据框头部
print("--- 数据框头部 (已添加 dm) ---")
print(head(final_derived_data %>% select(SEQN, DIQ010, LBXGH, LBXGLU, PHAFSTHR, dm)))

# 检查：确认 v3 和 final_derived_data 的行数一致
message(paste("v3 行数:", nrow(derived_data_v3), 
              "| final 行数:", nrow(final_derived_data)))

# -------------------------------------------------------------------
# NHANES 2011-2014: 阶段 0 (A) - 变量衍生 (慢性病计数)
# -------------------------------------------------------------------

# 我们的起始数据框是 `final_derived_data` (它已包含 htn 和 dm)
# 假设 `dplyr` 包已加载

message("正在衍生变量：Chronic Disease Count ...")

# 步骤 19: 衍生三种新的慢性病 (Arthritis, CVD, Cancer)
# -------------------------------------------------------------------
# 我们将首先创建这 3 种病的 0/1 (No/Yes) 变量
# 逻辑：1 = Yes, 2 = No, 其他 (7, 9, .) = NA
#
# 同时，我们也会为已有的 `htn` 和 `dm` 因子创建对应的 0/1 变量
# 以便 5 种病可以相加

derived_data_v4 <- final_derived_data %>% # 将结果存入新数据框 v4
  mutate(
    
    # --- 19.1: 转换已有的 htn 因子 ---
    # `htn` 是 "No" (level 2) / "Yes" (level 1) 因子
    # 我们需要 0 = "No", 1 = "Yes", NA = NA
    htn_numeric = case_when(
      htn == "Yes" ~ 1,
      htn == "No"  ~ 0,
      TRUE         ~ NA_real_ # 捕捉任何已存在的 NA
    ),
    
    # --- 19.2: 转换已有的 dm 因子 ---
    # `dm` 也是 "No" (level 2) / "Yes" (level 1) 因子
    dm_numeric = case_when(
      dm == "Yes" ~ 1,
      dm == "No"  ~ 0,
      TRUE        ~ NA_real_ # 捕捉任何已存在的 NA
    ),
    
    # --- 19.3: 衍生 Arthritis (关节炎) ---
    # 变量: MCQ160A
    # 逻辑: 1=Yes, 2=No, 其他=NA
    arthritis = case_when(
      MCQ160A == 1 ~ 1,
      MCQ160A == 2 ~ 0,
      TRUE         ~ NA_real_ # 将 7, 9, . (Missing) 全部设为 NA
    ),
    
    # --- 19.4: 衍生 Cardiovascular Disease (CVD) (心血管疾病) ---
    # 变量: MCQ160B, MCQ160C, MCQ160E, MCQ160F
    # 逻辑 (关键): 
    # 1 (Yes) = 只要 *任一个* 变量为 1
    # 0 (No)  = 必须 *所有* 变量都为 2
    # NA      = 其他情况 (例如，3个2 和 1个9)
    cvd = case_when(
      # "是" 的情况：任一为 1
      MCQ160B == 1 | MCQ160C == 1 | MCQ160E == 1 | MCQ160F == 1 ~ 1,
      
      # "否" 的情况：所有都为 2
      MCQ160B == 2 & MCQ160C == 2 & MCQ160E == 2 & MCQ160F == 2 ~ 0,
      
      # 其他情况 (7, 9, .) 均为 NA
      TRUE ~ NA_real_
    ),
    
    # --- 19.5: 衍生 Cancer (癌症) ---
    # 变量: MCQ220
    # 逻辑: 1=Yes, 2=No, 其他=NA
    cancer = case_when(
      MCQ220 == 1 ~ 1,
      MCQ220 == 2 ~ 0,
      TRUE        ~ NA_real_ # 将 7, 9, . (Missing) 全部设为 NA
    )
  )

# 步骤 20: 计算慢性病总数
# -------------------------------------------------------------------
# 我们现在有了 5 个 0/1/NA 格式的变量，可以直接相加

derived_data_v4 <- derived_data_v4 %>%
  mutate(
    
    # `+` 运算符会自动处理 NA：
    # 1 + 0 + 1 + 0 + 1 = 3
    # 1 + 0 + NA + 0 + 1 = NA
    # 这是正确的行为，因为如果一个人的状态未知(NA)，
    # 他的总数也应该是未知的(NA)，而不是被错误地计为 0。
    chronic_disease_count = htn_numeric + dm_numeric + arthritis + cvd + cancer
    
  )

# 步骤 21: (可选但推荐) 将新疾病变量转换为因子
# -------------------------------------------------------------------
# 为了与 htn 和 dm 保持一致，我们也把 arthritis, cvd, cancer 
# 转换为 "No"/"Yes" 因子，这在后续回归中用作协变量时很方便。
# 我们在 `derived_data_v4` 上直接修改

derived_data_v4 <- derived_data_v4 %>%
  mutate(
    arthritis = factor(arthritis, levels = c(0, 1), labels = c("No", "Yes")),
    cvd       = factor(cvd, levels = c(0, 1), labels = c("No", "Yes")),
    cancer    = factor(cancer, levels = c(0, 1), labels = c("No", "Yes"))
  )


# -------------------------------------------------------------------
# 步骤 22: 完成与核查
# -------------------------------------------------------------------
message("慢性病计数 (chronic_disease_count) 衍生完毕。")

# 检查：显示 3 种新疾病因子的频数表
print("--- 关节炎 (arthritis) 变量分布 ---")
print(table(derived_data_v4$arthritis, useNA = "ifany"))

print("--- 心血管疾病 (cvd) 变量分布 ---")
print(table(derived_data_v4$cvd, useNA = "ifany"))

print("--- 癌症 (cancer) 变量分布 ---")
print(table(derived_data_v4$cancer, useNA = "ifany"))

# 检查：显示最终的慢性病总数 (0 到 5)
print("--- 最终慢性病计数 (chronic_disease_count) 变量分布 ---")
print(table(derived_data_v4$chronic_disease_count, useNA = "ifany"))

# 检查：清理掉我们不再需要的中间 0/1 变量
# (我们保留 `derived_data_v4` 作为最新的数据框)
final_analysis_data <- derived_data_v4 %>%
  select(-htn_numeric, -dm_numeric)

# 检查：查看数据框头部
print("--- 数据框头部 (已添加 chronic_disease_count) ---")
print(head(final_analysis_data %>% 
             select(SEQN, htn, dm, arthritis, cvd, cancer, chronic_disease_count)))

# -------------------------------------------------------------------
# NHANES 2011-2014: 阶段 0 (A) - 修正酒精数据 (Step 10.9 Corrected)
# -------------------------------------------------------------------

# 我们的主数据框是 final_analysis_data
# 假设 haven, dplyr, 和 load_and_merge_cycles 函数已加载

message("正在修正：重新加载酒精(ALQ)数据...")

# 步骤 10.9 (修正)
# 我们使用 `load_and_merge_cycles` 辅助函数
# 这一次，我们包含所有需要的变量：ALQ120Q, ALQ120U, ALQ130
vars_alq_fixed <- c("SEQN", "ALQ120Q", "ALQ120U", "ALQ130")

# 从 ALQ_G.XPT 和 ALQ_H.XPT 加载并合并数据
data_alq_fixed <- load_and_merge_cycles("ALQ", vars_alq_fixed)

# -----------------
# 重要：将修正后的数据并入我们的主数据框
# -----------------
# 我们需要先移除在步骤 10.9 中错误加入的不完整列
# (如果它们存在的话，select(-...) 不会报错)
final_analysis_data <- final_analysis_data %>%
  select(-any_of(c("ALQ130", "ALQ120U"))) # 移除旧的、不完整的列

# 现在，left_join 完整的、修正后的酒精数据
final_analysis_data_v5 <- final_analysis_data %>%
  left_join(data_alq_fixed, by = "SEQN")

message("酒精数据修正并合并完毕。")
print(head(final_analysis_data_v5 %>% select(SEQN, ALQ120Q, ALQ120U, ALQ130)))


# -------------------------------------------------------------------
# NHANES 2011-2014: 阶段 0 (A) - 衍生酒精与吸烟 (Step 23 & 24)
# -------------------------------------------------------------------

# 我们的起始数据框是 `final_analysis_data_v5` (已包含正确的酒精数据)

message("正在衍生变量：Alcohol Intake 和 Smoking Status ...")

derived_data_v5 <- final_analysis_data_v5 %>% # 将结果存入新数据框 v5
  mutate(
    
    # --- 步骤 23: 衍生酒精摄入量 (Alcohol Intake) ---
    
    # 23.1: 清理 "饮酒日杯数" (ALQ130)
    # 777 (Refused) 和 999 (Don't Know) 必须设为 NA
    ALQ130_cleaned = if_else(ALQ130 >= 777, NA_real_, ALQ130),
    
    # 23.2: 计算 "年饮酒天数" (ANNUAL_DRINK_DAYS)
    # 我们使用 `ALQ120Q` (频次) 和 `ALQ120U` (单位)
    ANNUAL_DRINK_DAYS = case_when(
      # 缺失值处理：频次(777/999) 或 单位(7/9) 缺失
      ALQ120Q >= 777 | ALQ120U >= 7 ~ NA_real_,
      
      # 逻辑计算：
      ALQ120U == 1 ~ ALQ120Q * 52,  # 单位是周 (per week)
      ALQ120U == 2 ~ ALQ120Q * 12,  # 单位是月 (per month)
      ALQ120U == 3 ~ ALQ120Q,       # 单位是年 (per year)
      
      # 处理 ALQ120Q=0 (从不) 或其他未定义情况
      ALQ120Q == 0 ~ 0,
      
      TRUE ~ NA_real_ # 捕捉其他所有情况
    ),
    
    # 23.3: 计算 "日均酒精克数" (daily_alcohol_g)
    # 公式: (年天数 * 日杯数 * 14克/杯) / 365天
    # 注意：如果 `ANNUAL_DRINK_DAYS` 是 0, `ALQ130_cleaned` 可能是 NA (因为跳过了)
    # 我们需要处理这种情况
    daily_alcohol_g = case_when(
      ANNUAL_DRINK_DAYS == 0 ~ 0, # 如果从不喝酒，则为 0
      # 仅在天数和杯数都有效时才计算
      !is.na(ANNUAL_DRINK_DAYS) & !is.na(ALQ130_cleaned) ~ 
        (ANNUAL_DRINK_DAYS * ALQ130_cleaned * 14) / 365,
      TRUE ~ NA_real_ # 其他情况 (NA)
    ),
    
    
    # --- 步骤 24: 衍生吸烟状态 (Smoking Status) ---
    # (这是你提供的、经过验证的完美逻辑)
    
    smoking_status = case_when(
      # 当前吸烟者：每天至少吸 或 有时吸
      SMQ040 %in% c(1, 2) ~ "Current Smoker",
      
      # 已戒烟：曾经吸≥100 支，但现在完全不吸
      SMQ020 == 1 & SMQ040 == 3 ~ "Former Smoker",
      
      # 从不吸烟：从未吸过≥100 支
      SMQ020 == 2 ~ "Never Smoker",
      
      # 其他情况（缺失、拒绝、不知道）
      TRUE ~ NA_character_
    ),
    
    # 转换为因子 (Factor)，并设定参照组 (Never Smoker)
    smoking_status = factor(
      smoking_status,
      levels = c("Never Smoker", "Former Smoker", "Current Smoker")
    )
  )

# -------------------------------------------------------------------
# 步骤 25: 完成与核查
# -------------------------------------------------------------------
message("Alcohol 和 Smoking 衍生完毕。")

# 检查：显示日均酒精摄入量 (连续变量)
print("--- 日均酒精摄入量 (daily_alcohol_g) 摘要 ---")
summary(derived_data_v5$daily_alcohol_g)

# 检查：显示吸烟状态 (分类变量)
print("--- 吸烟状态 (smoking_status) 变量分布 ---")
print(table(derived_data_v5$smoking_status, useNA = "ifany"))

# 检查：清理掉我们不再需要的中间变量
# (我们保留 `derived_data_v5` 作为最新的数据框)
final_analysis_data_v6 <- derived_data_v5 %>%
  select(-ALQ130_cleaned, -ANNUAL_DRINK_DAYS)

# 检查：查看数据框头部
print("--- 数据框头部 (已添加 alcohol 和 smoking) ---")
print(head(final_analysis_data_v6 %>% 
             select(SEQN, daily_alcohol_g, smoking_status)))


# -------------------------------------------------------------------
# NHANES 2011-2014: 阶段 0 (A) - 衍生平均膳食摄入 (Step 26)
# -------------------------------------------------------------------

# 我们的起始数据框是 `final_analysis_data_v6`
# 假设 `dplyr` 包已加载

message("正在衍生变量：平均膳食能量 (energy_avg) 和 蛋白质 (protein_avg) ...")

# 步骤 26: 计算两日平均膳食摄入
# -------------------------------------------------------------------
# 我们将使用 rowwise() 逻辑，这与计算最大握力和平均血压时完全相同
# 这种方法非常稳健，可以正确处理只有 Day 1 或 Day 2 数据的人

derived_data_v7 <- final_analysis_data_v6 %>% # 将结果存入新数据框 v7
  rowwise() %>% # 告诉 dplyr: 下面的操作请逐行执行
  mutate(
    
    # 26.1: 计算平均能量 (kcal)
    # mean(c(...), na.rm = TRUE) 是关键：
    # - 如果有 Day1 和 Day2, 计算 (Day1 + Day2) / 2
    # - 如果只有 Day1 (Day2=NA), 结果是 Day1
    # - 如果只有 Day2 (Day1=NA), 结果是 Day2
    energy_avg = mean(c(DR1TKCAL, DR2TKCAL), na.rm = TRUE),
    
    # 26.2: 计算平均蛋白质 (gm)
    protein_avg = mean(c(DR1TPROT, DR2TPROT), na.rm = TRUE),
    
    # 26.3: 陷阱处理 (NaN -> NA) (新手易错点！)
    # 如果某人 Day1 和 Day2 *都是* NA, 
    # mean(..., na.rm=TRUE) 会返回 NaN (Not a Number)
    # 我们必须将其转换回 NA，否则回归模型会出错
    energy_avg = if_else(is.nan(energy_avg), NA_real_, energy_avg),
    protein_avg = if_else(is.nan(protein_avg), NA_real_, protein_avg)
    
  ) %>%
  ungroup() # 必须 ungroup() 来恢复正常操作, 这一点非常重要！

# -------------------------------------------------------------------
# 步骤 27: 完成与核查
# -------------------------------------------------------------------
message("平均膳食摄入量衍生完毕。")

# 检查：显示新变量的摘要，以核查计算结果和缺失值
print("--- 新衍生变量 (energy_avg, protein_avg) 摘要 ---")
summary(derived_data_v7 %>% select(energy_avg, protein_avg))

# 检查：查看头部，确认新列已添加
print("--- 包含新膳食变量的数据框 (头部) ---")
print(head(derived_data_v7 %>% 
             select(SEQN, DR1TKCAL, DR2TKCAL, energy_avg, DR1TPROT, DR2TPROT, protein_avg)))

# -------------------------------------------------------------------
# 步骤 28: 定义最新的数据框
# -------------------------------------------------------------------
# 为了清晰起见，我们将 `derived_data_v7` 命名为最终的 v7 数据框
final_analysis_data_v7 <- derived_data_v7

message(paste("最新数据框 final_analysis_data_v7 已创建，行数:", nrow(final_analysis_data_v7)))


# -------------------------------------------------------------------
# NHANES 2011-2014: 阶段 0 (B) - 加速度计 (ACC) 数据处理
# 目标: 计算 Avg_Daily_MIMS 并将其并入主数据框
# -------------------------------------------------------------------

# 步骤 28: (回顾) 确认我们的主数据框已准备就绪
# (我们假设 final_analysis_data_v7 已经存在)
message(paste("开始处理 ACC 数据。基于 v7 数据框，行数:", 
              nrow(final_analysis_data_v7)))











# ===================================================================
# 模块替换：NHANES 2011-2014: 阶段 0 (B) - ACC 数据处理 (修正版)
# 目标: 计算 "每日平均总 MIMS (总量)" 并并入主数据框
# ===================================================================

# 假设 final_analysis_data_v7 已经存在于环境中
# 如果之前的 pax_combined 被清理了，这里会重新加载

message("--- 开始执行修正后的 ACC 处理模块 (Plan A: Total MIMS) ---")

# -------------------------------------------------------------------
# 步骤 29: 定义筛选常量 (移除 MVPA 阈值，保留佩戴时间标准)
# -------------------------------------------------------------------
MIN_WEAR_TIME_PER_DAY <- 600 # 有效日标准: 10 小时 (600分钟)
MIN_VALID_DAYS <- 4          # 有效人标准: 至少 4 天

# -------------------------------------------------------------------
# 步骤 30: 加载并合并分钟级数据 (PAXMIN)
# -------------------------------------------------------------------
# 我们需要重新加载，因为之前的步骤可能已经把它们清理掉了
message("正在重新加载 PAXMIN 数据 (这可能需要一点时间)...")

# 加载 G 周期
pax_g <- read_xpt("PAXMIN_G.XPT") %>% 
  select(SEQN, PAXDAYM, PAXPREDM, PAXMTSM)

# 加载 H 周期
pax_h <- read_xpt("PAXMIN_H.XPT") %>% 
  select(SEQN, PAXDAYM, PAXPREDM, PAXMTSM)

# 合并
message("正在合并 G 和 H 周期...")
pax_combined <- bind_rows(pax_g, pax_h)

# 清理内存
rm(pax_g, pax_h)

# -------------------------------------------------------------------
# 步骤 31: 阶段 1 - 计算每日摘要 (Daily Summary)
# -------------------------------------------------------------------
message("正在计算每日 MIMS 总量 (Daily Volume)...")

pax_daily_summary <- pax_combined %>%
  group_by(SEQN, PAXDAYM) %>%
  summarize(
    # 1. 佩戴分钟数: Wake (1) + Sleep (2)
    Daily_Wear_Mins = sum(PAXPREDM == 1 | PAXPREDM == 2, na.rm = TRUE),
    
    # 2. 每日 MIMS 总量 (Volume): 
    #    仅计算 "清醒佩戴时间 (Wake Wear, PAXPREDM=1)" 的 MIMS 值总和
    #    这是目前最被认可的“总量”算法
    Daily_MIMS_Total = sum(PAXMTSM[PAXPREDM == 1], na.rm = TRUE),
    
    # 3. 有效日判断
    is_Valid_Day = (Daily_Wear_Mins >= MIN_WEAR_TIME_PER_DAY),
    .groups = 'drop' # 自动 ungroup
  )

# -------------------------------------------------------------------
# 步骤 32: 阶段 2 - 汇总个人摘要 (Person Summary)
# -------------------------------------------------------------------
message("正在计算个人平均 MIMS (Person Average)...")

pax_person_summary <- pax_daily_summary %>%
  group_by(SEQN) %>%
  summarize(
    # 总有效天数
    Total_Valid_Days = sum(is_Valid_Day, na.rm = TRUE),
    
    # 只有在有效日才计入总和 (过滤非有效日的数据)
    Grand_Total_MIMS = sum(Daily_MIMS_Total[is_Valid_Day == TRUE], na.rm = TRUE)
  ) %>%
  ungroup() %>%
  
  # 筛选: 只保留有效天数 >= 4 天的人
  filter(Total_Valid_Days >= MIN_VALID_DAYS) %>%
  
  # 计算最终指标: 日均 MIMS (Volume)
  mutate(
    Avg_Daily_MIMS = Grand_Total_MIMS / Total_Valid_Days
  ) %>%
  
  # 只保留需要的列
  select(SEQN, Avg_Daily_MIMS, Total_Valid_Days)

message(paste("计算完毕。有效佩戴者人数:", nrow(pax_person_summary)))

# -------------------------------------------------------------------
# 步骤 33: 将新指标并入主数据框 (v7 -> v8)
# -------------------------------------------------------------------
message("正在合并至 v8 数据框...")

# 注意：这里我们将 Avg_Daily_MIMS 并入
final_analysis_data_v8 <- final_analysis_data_v7 %>%
  left_join(pax_person_summary, by = "SEQN")

# -------------------------------------------------------------------
# 步骤 34: 完成与核查
# -------------------------------------------------------------------
message("ACC 模块 (修正版) 执行完毕。")

# 检查新变量分布 (不再是 0 了！)
print("--- 新变量摘要: Avg_Daily_MIMS ---")
summary(final_analysis_data_v8$Avg_Daily_MIMS)

# 检查头部
print(head(final_analysis_data_v8 %>% 
             select(SEQN, ASMI, Max_Grip_Strength, Avg_Daily_MIMS)))

# ===================================================================
# 模块结束
# ===================================================================



















# -------------------------------------------------------------------
# 步骤 35: (重新运行) 加载新包
# -------------------------------------------------------------------
message("步骤 35: 正在加载 fda, ineq, tidyr...")
# 假设您已运行过 install.packages(c("ineq", "fda", "tidyr"))
library(ineq)
library(fda)
library(tidyr)
library(dplyr) # 确保 dplyr 加载

# -------------------------------------------------------------------
# 步骤 36: 准备“特征工程”的基础数据 (已修正包冲突)
# -------------------------------------------------------------------
message("步骤 36: 正在准备特征工程的基础数据...")

# 36.1: 找出我们所有的 "有效佩戴者"
valid_wearer_seqns <- final_analysis_data_v8 %>%
  dplyr::filter(!is.na(Total_Valid_Days)) %>%
  pull(SEQN)

# 36.2: 将 "是否有效日" 的信息合并回分钟级数据
message("  正在将 '有效日' 信息合并回分钟级数据...")

pax_with_validity <- pax_combined %>%
  # !! 修正 !! (添加 dplyr::filter)
  dplyr::filter(SEQN %in% valid_wearer_seqns) %>%
  left_join(
    # !! 修正 !! (添加 dplyr::select)
    pax_daily_summary %>% dplyr::select(SEQN, PAXDAYM, is_Valid_Day),
    by = c("SEQN", "PAXDAYM")
  )

# 36.3: 创建最终的、干净的基础数据框：`pax_valid_days_data`
pax_valid_days_data <- pax_with_validity %>%
  dplyr::filter(is_Valid_Day == TRUE) %>%
  dplyr::select(-is_Valid_Day)

message(paste("基础数据准备完毕。`pax_valid_days_data` 已创建，行数:",
              nrow(pax_valid_days_data)))

# 36.4: (重要) 清理内存
rm(pax_combined, pax_with_validity)
message("内存已清理。")


# -------------------------------------------------------------------
# 步骤 37: 步骤 1.2 (A) - 计算结构特征 (Gini Index) (已修正)
# -------------------------------------------------------------------
message("步骤 37: 正在计算 Gini 指数...")

gini_scores <- pax_valid_days_data %>%
  dplyr::filter(PAXPREDM == 1) %>% # 只在 "清醒佩戴" 时计算
  group_by(SEQN) %>%
  dplyr::summarize( # !! 修正 !! (添加 dplyr::summarize)
    Gini_Index = ineq::Gini(PAXMTSM, na.rm = TRUE)
  ) %>%
  dplyr::mutate( # !! 修正 !! (添加 dplyr::mutate)
    Gini_Index = ifelse(is.na(Gini_Index), 0, Gini_Index)
  )

# 检查：
print("--- Gini 指数 (头部) ---")
print(head(gini_scores))


# -------------------------------------------------------------------
# 步骤 38: 步骤 1.2 (B) - 计算结构特征 (Avg. Sedentary Bout Length) (已修正)
# -------------------------------------------------------------------
message("步骤 38: 正在计算平均久坐时长...")

SEDENTARY_THRESHOLD <- 100

message("  正在识别所有久坐片段...")
sedentary_bouts <- pax_valid_days_data %>%
  dplyr::filter(PAXPREDM == 1) %>%
  group_by(SEQN, PAXDAYM) %>%
  dplyr::mutate(
    is_Sedentary = (PAXMTSM < SEDENTARY_THRESHOLD),
    bout_id = consecutive_id(is_Sedentary)
  ) %>%
  dplyr::filter(is_Sedentary == TRUE) %>%
  group_by(SEQN, PAXDAYM, bout_id) %>%
  dplyr::summarize(Bout_Length = n()) %>%
  ungroup()

message("  正在计算每个人的平均值...")
avg_sed_bout_len <- sedentary_bouts %>%
  group_by(SEQN) %>%
  dplyr::summarize(
    Avg_Sed_Bout_Len = mean(Bout_Length, na.rm = TRUE)
  ) %>%
  dplyr::mutate(
    Avg_Sed_Bout_Len = ifelse(is.nan(Avg_Sed_Bout_Len), 0, Avg_Sed_Bout_Len)
  )

# 检查：
print("--- 平均久坐时长 (头部) ---")
print(head(avg_sed_bout_len))


# -------------------------------------------------------------------
# 步骤 39: 步骤 1.1 (A) - 准备 fPCA 数据矩阵 (已修正)
# -------------------------------------------------------------------
message("步骤 39: 正在准备 fPCA 1440分钟曲线矩阵...")

message("  正在创建分钟索引 (MinuteOfDay)...")
fPCA_prep_data <- pax_valid_days_data %>%
  group_by(SEQN, PAXDAYM) %>%
  dplyr::mutate(MinuteOfDay = 1:n()) %>%
  ungroup()

message("  正在计算每个人的平均日曲线 (长表)...")
avg_profiles_long <- fPCA_prep_data %>%
  dplyr::mutate(
    Activity = ifelse(PAXPREDM == 1, PAXMTSM, 0)
  ) %>%
  group_by(SEQN, MinuteOfDay) %>%
  dplyr::summarize(
    Avg_Activity = mean(Activity, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  dplyr::mutate(
    Avg_Activity = ifelse(is.nan(Avg_Activity), 0, Avg_Activity)
  )

message("  正在将长表转换为宽表 (N x 1440 矩阵)...")
profile_matrix_wide <- avg_profiles_long %>%
  pivot_wider(
    id_cols = SEQN,
    names_from = MinuteOfDay,
    values_from = Avg_Activity
  )

fpca_seqns <- profile_matrix_wide$SEQN
fpca_matrix <- as.matrix(profile_matrix_wide %>% dplyr::select(-SEQN)) # !! 修正 !!
fpca_matrix[is.na(fpca_matrix)] <- 0

message("fPCA 矩阵准备完毕。")

# -------------------------------------------------------------------
# 步骤 40: 步骤 1.1 (B) - 执行功能性主成分分析 (fPCA)
# -------------------------------------------------------------------
message("步骤 40: 正在执行 fPCA...")

time_points <- 1:1440
n_basis <- 65
n_harm <- 3

basis_obj <- create.bspline.basis(rangeval = c(1, 1440), nbasis = n_basis)

message("  正在创建功能性数据对象 (fdObject)...")
fd_obj <- Data2fd(y = t(fpca_matrix), argvals = time_points, basisobj = basis_obj)

message("  正在计算 3 个功能主成分...")
fpca_result <- pca.fd(fd_obj, nharm = n_harm)

fpca_scores <- as.data.frame(fpca_result$scores)
colnames(fpca_scores) <- c("fPC1_Score", "fPC2_Score", "fPC3_Score")

fpca_scores_final <- bind_cols(SEQN = fpca_seqns, fpca_scores)

# 检查：
print("--- fPCA 得分 (头部) ---")
print(head(fpca_scores_final))

# -------------------------------------------------------------------
# 步骤 41: 步骤 1.3 - 构建最终特征矩阵并合并
# -------------------------------------------------------------------
message("步骤 41: 正在合并所有 5 个新特征...")

feature_matrix <- full_join(fpca_scores_final, gini_scores, by = "SEQN") %>%
  full_join(., avg_sed_bout_len, by = "SEQN")

print("--- 最终特征矩阵 (头部) ---")
print(head(feature_matrix))

message("  正在将特征矩阵并入主数据框 v8 -> v9...")
final_analysis_data_v9 <- final_analysis_data_v8 %>%
  left_join(feature_matrix, by = "SEQN")

# -------------------------------------------------------------------
# 步骤 42: 完成与核查 (v9) (已修正)
# -------------------------------------------------------------------
message("阶段 1 特征工程完毕！")
message(paste("v9 数据框已创建，行数:", nrow(final_analysis_data_v9)))

print("--- v9 数据框 (头部，包含所有 ACC 指标) ---")
print(head(final_analysis_data_v9 %>% 
             # !! 修正 !! (添加 dplyr::select)
             dplyr::select(SEQN, ASMI, 
                           Avg_Daily_MIMS,   # 旧指标
                           fPC1_Score,       # 新指标
                           fPC2_Score,       # 新指标
                           fPC3_Score,       # 新指标
                           Gini_Index,       # 新指标
                           Avg_Sed_Bout_Len  # 新指标
             )))

# 最终清理 (可选，但推荐)
rm(pax_valid_days_data, fPCA_prep_data, avg_profiles_long, 
   profile_matrix_wide, fpca_matrix, fd_obj, fpca_result,
   gini_scores, sedentary_bouts, avg_sed_bout_len, 
   fpca_scores, fpca_scores_final, feature_matrix, 
   valid_wearer_seqns, pax_daily_summary, 
   basis_obj, time_points, n_basis, n_harm, SEDENTARY_THRESHOLD,
   MIN_VALID_DAYS, MIN_WEAR_TIME_PER_DAY, MVPA_THRESHOLD)
message("所有临时计算对象已清理。")
# -------------------------------------------------------------------
# 步骤 43: 保存最终的 V9 数据框 (Checkpoint)
# -------------------------------------------------------------------

message("正在将最终数据框 (v9) 保存到磁盘...")

# 我们使用 saveRDS() 函数
# 1. 第一个参数: 要保存的对象 (final_analysis_data_v9)
# 2. file = : 您要保存的文件名 (以 .rds 结尾)

saveRDS(final_analysis_data_v9, file = "final_analysis_data_v9.rds")

message("保存完毕！文件 'final_analysis_data_v9.rds' 已创建。")
message("您的 R 脚本和这个 .rds 文件是您需要保留的两个文件。")
# (在未来的新 R 脚本开头)
















# 1. 加载您需要的包 (例如 dplyr)
library(dplyr)
library(survey) # (未来我们会用到)

# 2. 使用 readRDS() 读回您的数据
message("正在从 'final_analysis_data_v9.rds' 加载数据...")
final_analysis_data_v9 <- readRDS(file = "final_analysis_data_v9.rds")

# 3. 验证数据
message("加载成功！")
print(head(final_analysis_data_v9))


# (在未来的新 R 脚本开头)

# 1. 加载您需要的包 (例如 dplyr)
library(dplyr)
library(survey) # (未来我们会用到)

# 2. 使用 readRDS() 读回您的数据
message("正在从 'final_analysis_data_v9.rds' 加载数据...")
final_analysis_data_v9 <- readRDS(file = "final_analysis_data_v9.rds")

# 3. 验证数据
message("加载成功！")
print(head(final_analysis_data_v9))


# -------------------------------------------------------------------
# NHANES 2011-2014: 阶段 2 - 无监督学习 (表型发现)
# 
# 目标:
# 1. (步骤 2.1) 对 V9 中的特征进行标准化
# 2. (步骤 2.2) 探索最优聚类数 (K)
# 3. (步骤 2.3) 执行 K-Means 聚类
# 4. (步骤 2.4) 对聚类结果进行“画像”(Profiling) 和命名
#
# 产出: final_analysis_data_v10.rds
# -------------------------------------------------------------------

# -------------------------------------------------------------------
# 步骤 43: 加载必要的包和数据
# -------------------------------------------------------------------

message("步骤 43: 正在加载包...")

# "dplyr" 和 "tidyr" 用于数据操作
# "cluster" 和 "factoextra" 用于聚类分析和可视化
# "ggplot2" 用于高级绘图
# "svglite" 用于保存高质量、可编辑的 SVG 图像
# "viridis" 提供美观且色盲友好的调色板

# 如果您没有安装，请先运行:
# install.packages(c("dplyr", "tidyr", "cluster", "factoextra", "ggplot2", "svglite", "viridis"))

library(dplyr)
library(tidyr)
library(cluster)      # 包含 silhouette 函数
library(factoextra)   # 包含 fviz_nbclust 和 fviz_cluster
library(ggplot2)
library(svglite)      # 用于保存SVG
library(viridis)      # 用于美观的配色

# --- 加载 V9 数据 ---
# 这是我们上一个脚本 (阶段 1) 的最终产出
message("正在加载 'final_analysis_data_v9.rds'...")
tryCatch({
  final_analysis_data_v9 <- readRDS(file = "final_analysis_data_v9.rds")
  message(paste("V9 数据加载成功。总行数:", nrow(final_analysis_data_v9)))
}, error = function(e) {
  stop("错误: 无法加载 'final_analysis_data_v9.rds'。请确保文件在您的工作目录中。")
})


# -------------------------------------------------------------------
# 步骤 44: 准备聚类数据 (步骤 2.1)
# -------------------------------------------------------------------
# 我们的聚类分析只能在那些 "特征工程" 成功（即拥有全部5个新特征）
# 的参与者身上进行。

message("步骤 44: 正在准备聚类数据...")

# 44.1: 定义我们的5个特征变量
# (fPC1, fPC2, fPC3, Gini, Avg_Sed_Bout_Len)
feature_vars <- c(
  "fPC1_Score", 
  "fPC2_Score", 
  "fPC3_Score", 
  "Gini_Index", 
  "Avg_Sed_Bout_Len"
)

# 44.2: 筛选出用于聚类的队列 (过滤掉NA)
# 我们使用 fPC1_Score 是否为 NA 来判断
data_for_clustering_raw <- final_analysis_data_v9 %>%
  filter(!is.na(fPC1_Score))

message(paste("队列筛选完毕。用于聚类的有效样本量 (N3):", nrow(data_for_clustering_raw), "人"))

# 44.3: 提取纯特征矩阵 (原始值)
# !! 修正 !!: 使用 dplyr::select 防止函数名冲突
feature_matrix_raw <- data_for_clustering_raw %>%
  dplyr::select(all_of(feature_vars))

# 44.4: (关键) 对特征矩阵进行 Z-score 标准化
# K-Means 是基于距离的，标准化可确保所有特征权重均等
feature_matrix_scaled <- scale(feature_matrix_raw)

# 检查：核查标准化结果 (均值应约等于0，标准差应为1)
message("--- 标准化特征矩阵 (摘要) ---")
# 我们将其转换为 data.frame 以便使用 summary()
summary(as.data.frame(feature_matrix_scaled))


# -------------------------------------------------------------------
# 步骤 45: 确定最优聚类数 (K) (步骤 2.2)
# -------------------------------------------------------------------
# 这是一个数据驱动的决策。我们将使用两种最常用的方法：
# 1. "肘部法则" (Elbow Method / WSS)
# 2. "轮廓系数" (Silhouette Score)
# 我们必须设置随机数种子 (set.seed)，以确保结果可复现

message("步骤 45: 正在探索最优 K 值...")
set.seed(123) # 确保聚类结果可复现

# --- 方法 1: 肘部法则 (WSS) ---
message("  正在计算 '肘部法则' (WSS)...")
plot_wss <- fviz_nbclust(
  feature_matrix_scaled, 
  kmeans, 
  method = "wss",
  k.max = 10 # 检查 1 到 10 个聚类
) + 
  ggtitle("肘部法则 (WSS)") +
  theme_minimal()

# 保存为高质量 SVG
message("  正在保存 WSS 肘部图 (SVG)...")
svglite(
  file = "cluster_plot_1_elbow.svg", 
  width = 8, 
  height = 6
)
print(plot_wss) # 必须 'print' (打印) 图像才能保存
dev.off() # 关闭 SVG 设备

# --- 方法 2: 轮廓系数 ---
# (注意: 轮廓系数计算量很大，在NHANES数据上可能需要几分钟)
message("  正在计算 '轮廓系数' (可能需要几分钟)...")
plot_silhouette <- fviz_nbclust(
  feature_matrix_scaled, 
  kmeans, 
  method = "silhouette",
  k.max = 10 # 同样检查 1 到 10 个
) + 
  ggtitle("平均轮廓系数") +
  theme_minimal()

# 保存为高质量 SVG
message("  正在保存轮廓系数图 (SVG)...")
svglite(
  file = "cluster_plot_2_silhouette.svg", 
  width = 8, 
  height = 6
)
print(plot_silhouette)
dev.off()

message("K值探索完毕。请检查 'cluster_plot_1_elbow.svg' 和 'cluster_plot_2_silhouette.svg'。")

# --- 决策点 ---
# 博士，您需要查看这两个图：
# 1. 在 "肘部图" 中, 寻找下降速率明显变缓的 "拐点" (例如 K=3 或 K=4)。
# 2. 在 "轮廓图" 中, 寻找最高的那个峰值 (代表最佳K值)。
# 
# 为了让脚本能继续运行，我将暂时设置 K=4。
# !! 关键 !!: 您必须根据上一步的图，回来修改这个值！
OPTIMAL_K <- 2  # <--- !! 博士, 请根据图表修改此值 (例如改为 3 或 5) !!


# -------------------------------------------------------------------
# 步骤 46: 执行最终 K-Means 聚类 (步骤 2.3)
# -------------------------------------------------------------------

message(paste("步骤 46: 正在使用 K =", OPTIMAL_K, "执行最终 K-Means 聚类..."))

# 再次设置种子，确保与探索步骤一致
set.seed(123) 

# nstart = 25 是一个稳健的选择
# 它会尝试 25 次不同的随机起始点，并选择最好的一个结果
final_kmeans_result <- kmeans(
  feature_matrix_scaled, 
  centers = OPTIMAL_K, 
  nstart = 25
)

# 检查：查看聚类结果
# print(final_kmeans_result)

# 提取每个人的聚类分配 (例如，1, 2, 3, 4)
cluster_assignments <- final_kmeans_result$cluster


# -------------------------------------------------------------------
# 步骤 47: 表型画像 (Profiling) (步骤 2.4 - A)
# -------------------------------------------------------------------
# 这是最关键的 "翻译" 步骤。
# Cluster 1 到底代表什么？是 "高活跃" 还是 "碎片化久坐"？
# 我们需要计算每个 Cluster (表型) 的特征均值。

message("步骤 47: 正在计算每个聚类的特征均值 (Profiling)...")

# 47.1: 将聚类分配 (cluster_assignments) 添加回 *标准化* 数据
profiling_data_scaled <- as.data.frame(feature_matrix_scaled) %>%
  mutate(Cluster = as.factor(cluster_assignments))

# 47.2: 计算每个 Cluster 的 5 个特征的 *标准化均值*
cluster_profile_means <- profiling_data_scaled %>%
  group_by(Cluster) %>%
  summarize(
    across(all_of(feature_vars), mean, .names = "Mean_{.col}")
  )

# --- 打印“翻译表” ---
# 这是您命名的依据！
message("--- 关键: 各表型的特征均值 (标准化 Z-score) ---")
print(as.data.frame(cluster_profile_means)) # 打印到控制台
#
# !! 如何解读这张表 (重要) !!
# - 均值为正 (如 +0.8): 表示该表型的这个特征 *远高于* 人群平均水平。
# - 均值为负 (如 -1.2): 表示该表型的这个特征 *远低于* 人群平均水平。
# - 均值接近 0 (如 0.1): 表示该表型在这个特征上接近人群平均。
#
# 例如:
# 如果 Cluster 2 的 Mean_fPC1_Score = -1.5 (fPC1低=总量低)
# 且 Mean_Gini_Index = +1.1 (Gini高=不平等)
# 且 Mean_Avg_Sed_Bout_Len = +1.3 (久坐长=碎片化低)
# 那么 Cluster 2 就可能是 "低活跃、爆发式久坐型"


# -------------------------------------------------------------------
# 步骤 48: 表型画像可视化 (步骤 2.4 - B)
# -------------------------------------------------------------------
# 使用平行坐标图 (Parallel Coordinate Plot) 可视化上述表格

message("步骤 48: 正在生成表型画像图 (Profiling Plot)...")

# 48.1: 将宽表 (cluster_profile_means) 转换为长表 (long format) 以便 ggplot 绘图
profile_long_data <- cluster_profile_means %>%
  pivot_longer(
    cols = starts_with("Mean_"),      # 选择所有 "Mean_" 开头的列
    names_to = "Feature",            # 新列 "Feature", 存储原列名
    values_to = "Standardized_Mean"  # 新列 "Value", 存储均值
  ) %>%
  # 清理一下特征名称 (移除 "Mean_")
  mutate(Feature = gsub("Mean_", "", Feature))

# 48.2: 绘制平行坐标图
plot_profiling <- ggplot(
  profile_long_data, 
  aes(
    x = Feature,                 # X轴是 5 个特征
    y = Standardized_Mean,       # Y轴是标准化均值
    group = Cluster,             # 按聚类分组 (画线)
    color = Cluster              # 按聚类着色
  )
) +
  geom_line(linewidth = 1.2, alpha = 0.8) + # 绘制连接线
  geom_point(size = 3) +                   # 绘制数据点
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") + # 添加 0 参考线
  scale_color_viridis(discrete = TRUE, option = "D") + # 使用 Viridis 调色板
  labs(
    title = "Physical Activity Phenotype Profiles (K=4)", # !! 如果K不是4, 请修改 !!
    subtitle = "Mean Z-scores for each clustering feature",
    x = "Feature (特征)",
    y = "Standardized Mean (Z-score)",
    color = "PA Phenotype"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1) # X轴标签倾斜
  )

# 48.3: 保存为高质量 SVG
message("  正在保存画像图 (SVG)...")
svglite(
  file = "cluster_plot_3_profiling.svg", 
  width = 10, 
  height = 7
)
print(plot_profiling)
dev.off()

message("画像图已保存。请检查 'cluster_plot_3_profiling.svg'。")


# -------------------------------------------------------------------
# 步骤 49: 命名表型并合并回主数据框 (步骤 2.4 - C)
# -------------------------------------------------------------------
# 这是我们将算法 (Cluster 1, 2, 3...) 翻译为学术语言 (e.g., "Sustained Achievers") 的一步

message("步骤 49: 正在命名表型并合并回主数据框...")

# 49.1: 将聚类结果 (cluster_assignments) 添加到 N3 队列
# (我们还需要 SEQN 以便后续合并回 V9)
data_for_clustering_final <- data_for_clustering_raw %>%
  dplyr::select(SEQN) %>% # !! 修正 !!: 使用 dplyr::select
  mutate(Cluster = cluster_assignments) # 添加聚类编号

# 49.2: (关键) 命名表型
# 
# !! 博士，这是最重要的专家决策 !!
# 您必须查看 步骤 47 的 "翻译表" 和 步骤 48 的 "画像图"
# 来决定 Cluster 1, 2, 3, 4... 分别叫什么名字。
#
# 我在这里提供一个 *基于K=4的、合理的、但纯属假设的* 命名方案
# 您 *必须* 根据您的实际结果来修改这里的 'labels'
#
# 假设 (Hypothetical):
# - Cluster 1: fPC1高 (总量高), fPC2/3均衡, Gini低 (平等), 久坐短
# - Cluster 2: fPC1低 (总量低), Gini高 (不平等), 久坐长
# - Cluster 3: fPC1中, fPC2高 (早高峰), Gini中, 久坐短
# - Cluster 4: fPC1中, fPC2低 (晚高峰), Gini高 (不平等), 久坐中
#
# 对应的学术命名 (示例):
# - Cluster 1 -> "Active & Consistent" (活跃且一致)
# - Cluster 2 -> "Inactive & Fragmented" (不活跃且碎片化)
# - Cluster 3 -> "Structured Early-Risers" (结构化早起型)
# - Cluster 4 -> "Irregular Late-Movers" (不规律晚动型)

# (!! 博士: 请在此处修改您的命名 !!)
phenotype_labels <- c(
  `1` = "Structured Achievers",    # Cluster 1 (高活跃, 但久坐长)
  `2` = "Fragmented Inactives"   # Cluster 2 (低活跃, 但久坐短)
)

# 确保标签数量与 OPTIMAL_K 一致
if (length(phenotype_labels) != OPTIMAL_K) {
  warning(paste("警告: 您的 phenotype_labels 数量 (", length(phenotype_labels), ") 与 OPTIMAL_K (", OPTIMAL_K, ") 不匹配! 请检查 步骤 49.2。"))
}

# 49.3: 应用命名
data_for_clustering_final <- data_for_clustering_final %>%
  mutate(
    # revalue() 来自 dplyr (plyr::revalue 已被弃用),
    # 我们使用 case_when() 或 factor() 的 labels 参数，更现代
    PA_Phenotype = factor(Cluster, 
                          levels = 1:OPTIMAL_K,
                          labels = phenotype_labels[1:OPTIMAL_K])
  )

# 49.4: 将新的表型变量 (Cluster, PA_Phenotype) 合并回 V9
# 产出我们最终的 V10 数据框！
final_analysis_data_v10 <- final_analysis_data_v9 %>%
  left_join(
    data_for_clustering_final, 
    by = "SEQN"
  )

message("V10 数据框创建完毕！")


# -------------------------------------------------------------------
# 步骤 50: 最终统计与保存 (您的要求)
# -------------------------------------------------------------------

message("步骤 50: 正在统计各表型的人数...")

# 50.1: 统计每个表型的人数
phenotype_counts <- table(final_analysis_data_v10$PA_Phenotype, useNA = "ifany")

message("--- 最终表型统计 (N3 队列) ---")
print(phenotype_counts)
message("---------------------------------")
message(paste("总计 (N3):", sum(phenotype_counts), "人"))


# 50.2: 保存最终的 V10 数据框
message("正在将最终数据框 (v10) 保存到磁盘...")
saveRDS(final_analysis_data_v10, file = "final_analysis_data_v10.rds")

message("阶段 2 完毕！文件 'final_analysis_data_v10.rds' 已创建。")
message("它现在包含了所有原始协变量、结局指标 (ASMI, 握力) 以及我们新发现的 'PA_Phenotype'。")


# -------------------------------------------------------------------
# 步骤 51: (可选) 可视化聚类结果 (附加图)
# -------------------------------------------------------------------
# 这是一个降维图 (PCA)，展示聚类的分离程度

message("步骤 51: (可选) 正在生成聚类分离图...")

# fviz_cluster 使用 PCA 将 5 个特征降到 2 维进行可视化
plot_cluster_separation <- fviz_cluster(
  final_kmeans_result, 
  data = feature_matrix_scaled,
  geom = "point",
  ellipse.type = "confidence", # 使用置信椭圆
  palette = "viridis",
  ggtheme = theme_minimal()
) + 
  ggtitle("聚类分离图 (PCA 降维)")

# 51.2: 保存为高质量 SVG
message("  正在保存聚类分离图 (SVG)...")
svglite(
  file = "cluster_plot_4_separation.svg", 
  width = 9, 
  height = 7
)
print(plot_cluster_separation)
dev.off()

message("--- 所有任务完成 ---")




# -------------------------------------------------------------------
# NHANES 2011-2014: 阶段 3 (A) - 构建最终分析队列 (序贯版)
# 
# [!! 最终版 - 取代所有 "Phase 3A" 脚本 !!]
#
# 目标:
# 1. 加载 "v10" (全家福)
# 2. 创建 "Master" 数据框 (重命名, 仅含 Chronic_Count)
# 3. (A) 为 队列 A (ASMI) 执行详细的“序贯剔除”并保存
# 4. (B) 为 队列 B (Grip) 执行详细的“序贯剔除”并保存
#
# 产出:
# 1. 'analysis_data_asmi.rds'
# 2. 'analysis_data_grip.rds'
# -------------------------------------------------------------------

# -------------------------------------------------------------------
# 步骤 52: 加载包和 V10 数据
# -------------------------------------------------------------------
message("步骤 52: 正在加载包和 'final_analysis_data_v10.rds'...")
library(dplyr)
library(tidyr) 

tryCatch({
  final_analysis_data_v10 <- readRDS(file = "final_analysis_data_v10.rds")
  message(paste("V10 数据加载成功。总行数 (N_v10):", nrow(final_analysis_data_v10)))
}, error = function(e) {
  stop("错误: 无法加载 'final_analysis_data_v10.rds'。")
})

# (诊断代码已移除，继续执行步骤 53)


# -------------------------------------------------------------------
# 步骤 53: 创建“Master”数据框 (提取 & 重命名)
# -------------------------------------------------------------------
message("步骤 53: 正在创建重命名的 'Master' 数据框...")

# !! 修正: 使用 dplyr::select 防止函数名冲突 !!
master_data <- final_analysis_data_v10 %>%
  dplyr::select(
    # --- 抽样设计 ---
    SEQN, SDMVPSU, SDMVSTRA, WTMEC4YR,
    # --- 结局 ---
    ASMI,
    Grip_Strength = Max_Grip_Strength,
    # --- 预测变量 ---
    PA_Phenotype,
    MVPA = Avg_Daily_MIMS,
    # --- 协变量 ---
    Age = RIDAGEYR,
    Sex = RIAGENDR,
    Race = RIDRETH1,
    PIR = INDFMPIR,
    BMI = BMXBMI,
    VitaminD = LBXVIDMS,
    Energy = energy_avg,
    Protein = protein_avg,
    Smoking = smoking_status,
    Alcohol_g = daily_alcohol_g,
    Chronic_Count = chronic_disease_count # 仅使用综合指标
  )

message(paste("'Master' 数据框已创建，行数:", nrow(master_data)))


# ===================================================================
# 
#               开始构建 [队列 A: ASMI 模型]
#
# ===================================================================
message("--- 步骤 54: 开始构建 [队列 A: ASMI 模型] (40-59 岁) ---")

# N_A0: 起始点 (Master Data)
current_data_a <- master_data
current_n_a <- nrow(current_data_a)
message(paste("N_A0: 起始样本量 (Master):", current_n_a))

# --- 筛选 A1: 年龄限制 (关键) ---
current_data_a1 <- current_data_a %>%
  filter(Age >= 40 & Age <= 59)

n_after_a1 <- nrow(current_data_a1)
message(paste("  剔除年龄 > 59 岁 (不符合DXA):", current_n_a - n_after_a1, "人"))
message(paste("N_A1: 剩余样本量 (40-59岁):", n_after_a1))
current_n_a <- n_after_a1
current_data_a <- current_data_a1

# --- 筛选 A2: 核心预测变量 (PA_Phenotype) ---
current_data_a2 <- current_data_a %>%
  filter(!is.na(PA_Phenotype))

n_after_a2 <- nrow(current_data_a2)
message(paste("  剔除 'PA_Phenotype' 缺失 (无ACC):", current_n_a - n_after_a2, "人"))
message(paste("N_A2: 剩余样本量:", n_after_a2))
current_n_a <- n_after_a2
current_data_a <- current_data_a2

# --- 筛选 A3: 结局变量 (ASMI) ---
current_data_a3 <- current_data_a %>%
  filter(!is.na(ASMI))

n_after_a3 <- nrow(current_data_a3)
message(paste("  剔除 'ASMI' 缺失 (DXA数据缺失):", current_n_a - n_after_a3, "人"))
message(paste("N_A3: 剩余样本量:", n_after_a3))
current_n_a <- n_after_a3
current_data_a <- current_data_a3

# --- 筛选 A4: 关键协变量 (逐一) ---
# (注意: 我们 *不* 剔除 Grip_Strength)

current_data_a4 <- current_data_a %>%
  filter(!is.na(BMI))
n_after_a4 <- nrow(current_data_a4)
message(paste("  剔除 'BMI' 缺失:", current_n_a - n_after_a4, "人"))
current_n_a <- n_after_a4

current_data_a5 <- current_data_a4 %>%
  filter(!is.na(Race) & !is.na(Sex)) # Age 已在 A1 检查
n_after_a5 <- nrow(current_data_a5)
message(paste("  剔除 'Race' 或 'Sex' 缺失:", current_n_a - n_after_a5, "人"))
current_n_a <- n_after_a5

current_data_a6 <- current_data_a5 %>%
  filter(!is.na(PIR))
n_after_a6 <- nrow(current_data_a6)
message(paste("  剔除 'PIR' (收入) 缺失:", current_n_a - n_after_a6, "人"))
current_n_a <- n_after_a6

current_data_a7 <- current_data_a6 %>%
  filter(!is.na(Energy) & !is.na(Protein))
n_after_a7 <- nrow(current_data_a7)
message(paste("  剔除 'Energy' 或 'Protein' 缺失:", current_n_a - n_after_a7, "人"))
current_n_a <- n_after_a7

current_data_a8 <- current_data_a7 %>%
  filter(!is.na(VitaminD))
n_after_a8 <- nrow(current_data_a8)
message(paste("  剔除 'VitaminD' 缺失:", current_n_a - n_after_a8, "人"))
current_n_a <- n_after_a8

current_data_a9 <- current_data_a8 %>%
  filter(!is.na(Smoking) & !is.na(Alcohol_g))
n_after_a9 <- nrow(current_data_a9)
message(paste("  剔除 'Smoking' 或 'Alcohol_g' 缺失:", current_n_a - n_after_a9, "人"))
current_n_a <- n_after_a9

current_data_a10 <- current_data_a9 %>%
  filter(!is.na(Chronic_Count))
n_after_a10 <- nrow(current_data_a10)
message(paste("  剔除 'Chronic_Count' 缺失:", current_n_a - n_after_a10, "人"))
current_n_a <- n_after_a10

# --- 队列 A 最终产出 ---
cohort_asmi <- current_data_a10
message(paste("N_A-Final: [队列 A] 最终样本量:", nrow(cohort_asmi)))
# (这个数字现在应该是 1756)

saveRDS(cohort_asmi, file = "analysis_data_asmi.rds")
message("  'analysis_data_asmi.rds' 已保存。")


# ===================================================================
# 
#               开始构建 [队列 B: 握力模型]
#
# ===================================================================
message("--- 步骤 55: 开始构建 [队列 B: 握力模型] (40-80 岁) ---")

# N_B0: 起始点 (!!! 必须从 'Master' 重新开始 !!!)
current_data_b <- master_data
current_n_b <- nrow(current_data_b)
message(paste("N_B0: 起始样本量 (Master):", current_n_b))

# --- 筛选 B1: 年龄限制 (关键) ---
current_data_b1 <- current_data_b %>%
  filter(Age >= 40 & Age <= 80) # (80是ACC数据的最大值)

n_after_b1 <- nrow(current_data_b1)
message(paste("  剔除年龄 > 80 岁:", current_n_b - n_after_b1, "人"))
message(paste("N_B1: 剩余样本量 (40-80岁):", n_after_b1))
current_n_b <- n_after_b1
current_data_b <- current_data_b1

# --- 筛选 B2: 核心预测变量 (PA_Phenotype) ---
current_data_b2 <- current_data_b %>%
  filter(!is.na(PA_Phenotype))

n_after_b2 <- nrow(current_data_b2)
message(paste("  剔除 'PA_Phenotype' 缺失 (无ACC):", current_n_b - n_after_b2, "人"))
message(paste("N_B2: 剩余样本量:", n_after_b2))
current_n_b <- n_after_b2
current_data_b <- current_data_b2

# --- 筛选 B3: 结局变量 (Grip_Strength) ---
current_data_b3 <- current_data_b %>%
  filter(!is.na(Grip_Strength))

n_after_b3 <- nrow(current_data_b3)
message(paste("  剔除 'Grip_Strength' 缺失:", current_n_b - n_after_b3, "人"))
message(paste("N_B3: 剩余样本量:", n_after_b3))
current_n_b <- n_after_b3
current_data_b <- current_data_b3

# --- 筛选 B4: 关键协变量 (逐一) ---
# (注意: 我们 *不* 剔除 ASMI)

current_data_b4 <- current_data_b %>%
  filter(!is.na(BMI))
n_after_b4 <- nrow(current_data_b4)
message(paste("  剔除 'BMI' 缺失:", current_n_b - n_after_b4, "人"))
current_n_b <- n_after_b4

current_data_b5 <- current_data_b4 %>%
  filter(!is.na(Race) & !is.na(Sex)) # Age 已在 B1 检查
n_after_b5 <- nrow(current_data_b5)
message(paste("  剔除 'Race' 或 'Sex' 缺失:", current_n_b - n_after_b5, "人"))
current_n_b <- n_after_b5

current_data_b6 <- current_data_b5 %>%
  filter(!is.na(PIR))
n_after_b6 <- nrow(current_data_b6)
message(paste("  剔除 'PIR' (收入) 缺失:", current_n_b - n_after_b6, "人"))
current_n_b <- n_after_b6

current_data_b7 <- current_data_b6 %>%
  filter(!is.na(Energy) & !is.na(Protein))
n_after_b7 <- nrow(current_data_b7)
message(paste("  剔除 'Energy' 或 'Protein' 缺失:", current_n_b - n_after_b7, "人"))
current_n_b <- n_after_b7

current_data_b8 <- current_data_b7 %>%
  filter(!is.na(VitaminD))
n_after_b8 <- nrow(current_data_b8)
message(paste("  剔除 'VitaminD' 缺失:", current_n_b - n_after_b8, "人"))
current_n_b <- n_after_b8

current_data_b9 <- current_data_b8 %>%
  filter(!is.na(Smoking) & !is.na(Alcohol_g))
n_after_b9 <- nrow(current_data_a9)
message(paste("  剔除 'Smoking' 或 'Alcohol_g' 缺失:", current_n_b - n_after_b9, "人"))
current_n_b <- n_after_b9

current_data_b10 <- current_data_b9 %>%
  filter(!is.na(Chronic_Count))
n_after_b10 <- nrow(current_data_b10)
message(paste("  剔除 'Chronic_Count' 缺失:", current_n_b - n_after_b10, "人"))
current_n_b <- n_after_b10

# --- 队列 B 最终产出 ---
cohort_grip <- current_data_b10
message(paste("N_B-Final: [队列 B] 最终样本量:", nrow(cohort_grip)))

saveRDS(cohort_grip, file = "analysis_data_grip.rds")
message("  'analysis_data_grip.rds' 已保存。")

# -------------------------------------------------------------------
# 步骤 57: (最新数据) 确认
# -------------------------------------------------------------------
# 博士，请注意：
# 我们现在有两个“最新数据”文件，可用于后续的阶段 3B (回归):
# 1. 'analysis_data_asmi.rds' (队列 A)
# 2. 'analysis_data_grip.rds' (队列 B)
#
# 脚本 'phase_3A_sequential_cohorts_final.R' 已执行完毕。
# -------------------------------------------------------------------










# -------------------------------------------------------------------
# NHANES 2011-2014: 阶段 3B - Table 1 完整生成 (4表合一版)
# 作者: 智析博士
#
# 目标:
# 1. 加载数据并进行严格的类型检查 (Factor Check)
# 2. 为 队列 A (ASMI) 生成:
#    - 非加权表 (仅看 N) -> 导出 CSV
#    - 加权表 (看 %, Mean, P值) -> 导出 CSV
# 3. 为 队列 B (Grip) 生成:
#    - 非加权表 (仅看 N) -> 导出 CSV
#    - 加权表 (看 %, Mean, P值) -> 导出 CSV
# -------------------------------------------------------------------

# -------------------------------------------------------------------
# 步骤 58: 加载必要的 R 包
# -------------------------------------------------------------------
message("--- 步骤 58: 正在加载 R 包... ---")
library(dplyr)
library(survey)
library(tableone)

# -------------------------------------------------------------------
# 步骤 59: 加载最终数据集
# -------------------------------------------------------------------
message("--- 步骤 59: 正在加载数据... ---")

# 确保 exports 文件夹存在
if (!dir.exists("exports")) {
  dir.create("exports")
}

tryCatch({
  cohort_asmi <- readRDS(file = "analysis_data_asmi.rds")
  cohort_grip <- readRDS(file = "analysis_data_grip.rds")
}, error = function(e) {
  stop("错误: 无法加载 .rds 文件。请检查目录下是否存在 analysis_data_asmi.rds 和 analysis_data_grip.rds")
})

message(paste("ASMI 队列 N =", nrow(cohort_asmi)))
message(paste("Grip 队列 N =", nrow(cohort_grip)))


# -------------------------------------------------------------------
# 步骤 60 (关键): 强制类型修复与设计对象创建
# -------------------------------------------------------------------
message("--- 步骤 60: 数据类型诊断与修复... ---")

# 1. 强制转换为 Factor (解决 P 值消失的根本原因)
# ---------------------------------------------------------
# 必须确保 PA_Phenotype 是因子，否则 tableone 无法分组
cohort_asmi$PA_Phenotype <- factor(cohort_asmi$PA_Phenotype)
cohort_asmi <- droplevels(cohort_asmi)

cohort_grip$PA_Phenotype <- factor(cohort_grip$PA_Phenotype)
cohort_grip <- droplevels(cohort_grip)

# 2. 简单的安全检查
if (length(levels(cohort_asmi$PA_Phenotype)) < 2) stop("ASMI 队列分组少于 2 组！")
if (length(levels(cohort_grip$PA_Phenotype)) < 2) stop("Grip 队列分组少于 2 组！")

# 3. 创建复杂抽样设计对象
# ---------------------------------------------------------
asmi_design <- svydesign(
  id = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~WTMEC4YR,
  nest = TRUE, data = cohort_asmi
)

grip_design <- svydesign(
  id = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~WTMEC4YR,
  nest = TRUE, data = cohort_grip
)
message("抽样设计对象创建成功。")


# -------------------------------------------------------------------
# 步骤 61: 批量生成 4 张表格
# -------------------------------------------------------------------
message("--- 步骤 61: 开始生成 4 张 Table 1 CSV ... ---")

# 定义所有需要的变量
vars_table1 <- c(
  "Age", "Sex", "Race", "PIR", "BMI",
  "Energy", "Protein", "VitaminD",
  "Smoking", "Alcohol_g", "Chronic_Count",
  "ASMI", "Grip_Strength", "MVPA"
)
# 定义其中的分类变量
fvars_table1 <- c("Sex", "Race", "Smoking") 

# ===================================================================
# (A) 队列 A: ASMI (40-59岁)
# ===================================================================
message("\n>>> 处理 队列 A (ASMI) <<<")

# --- A1. 非加权表 (Unweighted) ---
# 目的: 获取真实的样本量 (N)
# 注意: test = FALSE (非加权不看 P 值)
tab1_asmi_unweighted <- CreateTableOne(
  vars = vars_table1, strata = "PA_Phenotype", data = cohort_asmi, 
  factorVars = fvars_table1, test = FALSE
)
# 导出 A1
mat_asmi_un <- print(tab1_asmi_unweighted, showAllLevels = TRUE, printToggle = FALSE)
write.csv(mat_asmi_un, "exports/Table1_ASMI_Unweighted_N.csv")
message("  [1/4] 已导出: exports/Table1_ASMI_Unweighted_N.csv")


# --- A2. 加权表 (Weighted) ---
# 目的: 获取代表全美人口的均值、比例和 P 值
# 注意: test = TRUE (需要 P 值), format = "p" (格式化 P 值)
tab1_asmi_weighted <- svyCreateTableOne(
  vars = vars_table1, strata = "PA_Phenotype", data = asmi_design, 
  factorVars = fvars_table1, test = TRUE
)
# 导出 A2
mat_asmi_wt <- print(tab1_asmi_weighted, showAllLevels = TRUE, format = "p", smd = TRUE, printToggle = FALSE)
write.csv(mat_asmi_wt, "exports/Table1_ASMI_Weighted_P.csv")
message("  [2/4] 已导出: exports/Table1_ASMI_Weighted_P.csv")


# ===================================================================
# (B) 队列 B: Grip (40-80岁)
# ===================================================================
message("\n>>> 处理 队列 B (Grip) <<<")

# --- B1. 非加权表 (Unweighted) ---
tab1_grip_unweighted <- CreateTableOne(
  vars = vars_table1, strata = "PA_Phenotype", data = cohort_grip, 
  factorVars = fvars_table1, test = FALSE
)
# 导出 B1
mat_grip_un <- print(tab1_grip_unweighted, showAllLevels = TRUE, printToggle = FALSE)
write.csv(mat_grip_un, "exports/Table1_Grip_Unweighted_N.csv")
message("  [3/4] 已导出: exports/Table1_Grip_Unweighted_N.csv")


# --- B2. 加权表 (Weighted) ---
tab1_grip_weighted <- svyCreateTableOne(
  vars = vars_table1, strata = "PA_Phenotype", data = grip_design, 
  factorVars = fvars_table1, test = TRUE
)
# 导出 B2
mat_grip_wt <- print(tab1_grip_weighted, showAllLevels = TRUE, format = "p", smd = TRUE, printToggle = FALSE)
write.csv(mat_grip_wt, "exports/Table1_Grip_Weighted_P.csv")
message("  [4/4] 已导出: exports/Table1_Grip_Weighted_P.csv")

message("\n--- 恭喜！全部 4 张表格已生成完毕。请检查 exports 文件夹。 ---")









# !! 博士请注意:
# 您需要重复 步骤 61 来生成 [队列 B (Grip)] 的表格
# (只需替换 'asmi' 为 'grip', 'ASMI' 为 'Grip_Strength')


# -------------------------------------------------------------------
# 步骤 62: 运行序贯回归 [队列 A: ASMI]
# -------------------------------------------------------------------
message("--- 步骤 62: 正在运行 [队列 A (ASMI)] 的序贯回归模型... ---")

# 62.1: 设置参照组
asmi_design_releveled <- update(
  asmi_design,
  PA_Phenotype = relevel(PA_Phenotype, ref = "Fragmented Inactives")
)

# 62.2: 定义协变量列表
covars_model2 <- c("Age", "Sex", "Race", "BMI")
covars_model3 <- c(
  "Age", "Sex", "Race", "BMI",
  "Energy", "Protein", "VitaminD",
  "Smoking", "Alcohol_g", "Chronic_Count"
)

# 62.3: 运行模型 (我们将在 步骤 65 中打印和保存它们)
m1_asmi <- svyglm(ASMI ~ PA_Phenotype, design = asmi_design_releveled)
m2_asmi <- svyglm(
  as.formula(paste("ASMI ~ PA_Phenotype +", paste(covars_model2, collapse = " + "))),
  design = asmi_design_releveled
)
m3_asmi <- svyglm(
  as.formula(paste("ASMI ~ PA_Phenotype +", paste(covars_model3, collapse = " + "))),
  design = asmi_design_releveled
)
# "旧" 基准模型
m3_asmi_OLD <- svyglm(
  as.formula(paste("ASMI ~ MVPA +", paste(covars_model3, collapse = " + "))),
  design = asmi_design_releveled
)
message("队列 A (ASMI) 的 4 个模型已运行。")

# -------------------------------------------------------------------
# 步骤 63: 运行序贯回归 [队列 B: Grip Strength]
# -------------------------------------------------------------------
message("--- 步骤 63: 正在运行 [队列 B (Grip)] 的序贯回归模型... ---")

# 63.1: 设置参照组
grip_design_releveled <- update(
  grip_design,
  PA_Phenotype = relevel(PA_Phenotype, ref = "Fragmented Inactives")
)

# 63.2: 运行模型 (covars 列表在 62.2 中已定义)
m1_grip <- svyglm(Grip_Strength ~ PA_Phenotype, design = grip_design_releveled)
m2_grip <- svyglm(
  as.formula(paste("Grip_Strength ~ PA_Phenotype +", paste(covars_model2, collapse = " + "))),
  design = grip_design_releveled
)
m3_grip <- svyglm(
  as.formula(paste("Grip_Strength ~ PA_Phenotype +", paste(covars_model3, collapse = " + "))),
  design = grip_design_releveled
)
# "旧" 基准模型
m3_grip_OLD <- svyglm(
  as.formula(paste("Grip_Strength ~ MVPA +", paste(covars_model3, collapse = " + "))),
  design = grip_design_releveled
)
message("队列 B (Grip) 的 4 个模型已运行。")


# -------------------------------------------------------------------
# 步骤 64: (!!! 核心论证 !!!) "公平竞赛" - 模型比较 (AIC)
# -------------------------------------------------------------------
message("--- 步骤 64: 正在运行 '公平竞赛' 模型比较 (AIC)... ---")

#
# (!! 博士: 这里的 .Rdata 文件名是假设的，只是为了演示)
#

# 64.3: (!!! 最终结果 !!!) 打印 AIC 进行比较
message("------------------------------------------------------")
message("--- 最终模型比较 (AIC: 值越低越好) ---")
message("------------------------------------------------------")

# !! 修正:
# 移除了 '$AIC'，因为 AIC() 返回一个原子向量 (数字)
# 我们使用 [1] 来确保我们总是取第一个 (也是唯一的) 值
message(paste(
  "  [队列 A: ASMI (N=", nrow(cohort_asmi), ")]:",
  "\n  1. 旧模型 (MVPA) AIC:", round(AIC(m3_asmi_OLD)[1], 2),
  "\n  2. 新模型 (Phenotype) AIC:", round(AIC(m3_asmi)[1], 2)
))

message("---")

# !! 修正: 同样移除 '$AIC'
message(paste(
  "  [队列 B: Grip (N=", nrow(cohort_grip), ")]:",
  "\n  1. 旧模型 (MVPA) AIC:", round(AIC(m3_grip_OLD)[1], 2),
  "\n  2. 新模型 (Phenotype) AIC:", round(AIC(m3_grip)[1], 2)
))
message("------------------------------------------------------")

library(broom)
library(ggplot2)
library(svglite)
# -------------------------------------------------------------------
# 步骤 65: (!! 新增 !!) 导出所有回归模型 (Table 2/3)
# -------------------------------------------------------------------
message("--- 步骤 65: 正在将所有模型结果导出为 .csv 文件... ---")

# 65.1: 队列 A (ASMI)
# tidy() 函数将模型转换为一个"整洁"的数据框
tidy_m1_asmi <- tidy(m1_asmi, conf.int = TRUE)
tidy_m2_asmi <- tidy(m2_asmi, conf.int = TRUE)
tidy_m3_asmi <- tidy(m3_asmi, conf.int = TRUE)
tidy_m3_asmi_OLD <- tidy(m3_asmi_OLD, conf.int = TRUE)

write.csv(tidy_m1_asmi, file = "exports/model_asmi_m1_crude.csv", row.names = FALSE)
write.csv(tidy_m2_asmi, file = "exports/model_asmi_m2_demog.csv", row.names = FALSE)
write.csv(tidy_m3_asmi, file = "exports/model_asmi_m3_full_PHENOTYPE.csv", row.names = FALSE)
write.csv(tidy_m3_asmi_OLD, file = "exports/model_asmi_m3_full_MVPA.csv", row.names = FALSE)

# 65.2: 队列 B (Grip)
tidy_m1_grip <- tidy(m1_grip, conf.int = TRUE)
tidy_m2_grip <- tidy(m2_grip, conf.int = TRUE)
tidy_m3_grip <- tidy(m3_grip, conf.int = TRUE)
tidy_m3_grip_OLD <- tidy(m3_grip_OLD, conf.int = TRUE)

write.csv(tidy_m1_grip, file = "exports/model_grip_m1_crude.csv", row.names = FALSE)
write.csv(tidy_m2_grip, file = "exports/model_grip_m2_demog.csv", row.names = FALSE)
write.csv(tidy_m3_grip, file = "exports/model_grip_m3_full_PHENOTYPE.csv", row.names = FALSE)
write.csv(tidy_m3_grip_OLD, file = "exports/model_grip_m3_full_MVPA.csv", row.names = FALSE)

message("所有 8 个回归模型的结果已导出到 'exports' 文件夹。")

# -------------------------------------------------------------------
# 步骤 66: (!! 新增 !!) 绘制并导出森林图 (Forest Plots)
# -------------------------------------------------------------------
# 我们将为我们的核心结果 (Model 3) 绘制森林图
message("--- 步骤 66: 正在生成并导出结果的森林图 (SVG)... ---")

# 66.1: 准备 [ASMI] 模型的数据
# 我们只绘制 "PA_Phenotype" 相关的系数
plot_data_asmi <- tidy_m3_asmi %>%
  filter(grepl("PA_Phenotype", term)) # 筛选出我们关心的行

# 检查:
# print(plot_data_asmi)

# 66.2: 绘制 [ASMI] 森林图
forest_plot_asmi <- ggplot(
  data = plot_data_asmi,
  aes(
    x = estimate,           # beta 系数
    y = term,               # 变量名
    xmin = conf.low,        # 95% CI 下限
    xmax = conf.high        # 95% CI 上限
  )
) +
  geom_point(color = "darkblue", size = 3) +
  geom_errorbarh(height = 0.2, color = "darkblue") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") + # 0 点参考线
  labs(
    title = "Model 3 (ASMI) - PA Phenotype Effect",
    subtitle = "Reference: Fragmented Inactives",
    x = "Beta Coefficient (95% CI)",
    y = "PA Phenotype"
  ) +
  theme_minimal(base_size = 14)

# 66.3: (!! 关键 !!) 保存为文本可编辑的 SVG
# ggsave 使用 svglite 包来创建高质量、可编辑的 SVG
ggsave(
  filename = "exports/forest_plot_asmi_model3.svg",
  plot = forest_plot_asmi,
  device = "svg", # 明确使用 SVG 设备
  width = 8,
  height = 5
)
message("已导出 'forest_plot_asmi_model3.svg'")

# 66.4: (练习) 绘制并保存 [Grip] 森林图
plot_data_grip <- tidy_m3_grip %>%
  filter(grepl("PA_Phenotype", term))

forest_plot_grip <- ggplot(
  data = plot_data_grip,
  aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high)
) +
  geom_point(color = "darkred", size = 3) +
  geom_errorbarh(height = 0.2, color = "darkred") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  labs(
    title = "Model 3 (Grip Strength) - PA Phenotype Effect",
    subtitle = "Reference: Fragmented Inactives",
    x = "Beta Coefficient (95% CI)",
    y = "PA Phenotype"
  ) +
  theme_minimal(base_size = 14)

ggsave(
  filename = "exports/forest_plot_grip_model3.svg",
  plot = forest_plot_grip,
  device = "svg",
  width = 8,
  height = 5
)
message("已导出 'forest_plot_grip_model3.svg'")

# -------------------------------------------------------------------
# 阶段 3 (B) - 脚本执行完毕
# -------------------------------------------------------------------