# =====================================================
# 🌿 TDP, PO4, NH4 dynamics (Surface / Bottom) – Scheme A (Mean Line with Error Bars)
# =====================================================

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(purrr)
library(cowplot)
library(magick)

# =====================================================
# 1. 读取数据
# =====================================================

setwd("D:/地湖所工作/春季热浪文章相关工作/XMGS02_new2/Figure_nutrient")
data <- read_excel("data_all_new2.xlsx")

# =====================================================
# 2. Treatment 缩写、颜色（新颜色方案）、线型（全部实线）
# =====================================================

treat_labels <- c(
  "Non-heatwave+Non-mixing",                   
  "Non-heatwave+Mixing",                 
  "Heatwave+Non-mixing",                    
  "Heatwave+Mixing"
)

# 新的颜色方案
treat_colors <- c(
  "Non-heatwave+Non-mixing"                   = "#ABD9E9",  # 浅蓝色
  "Non-heatwave+Mixing"                       = "#2166AC",  # 蓝色
  "Heatwave+Non-mixing"                       = "#FC8D59",  # 橙红色
  "Heatwave+Mixing"                           = "#B2182B"   # 红色
)

# 所有处理都使用实线（删除线型区分）
treat_linetypes <- c(
  "Non-heatwave+Non-mixing"                   = "solid",
  "Non-heatwave+Mixing"                       = "solid",
  "Heatwave+Non-mixing"                       = "solid",
  "Heatwave+Mixing"                           = "solid"
)

# =====================================================
# 3. 固定 Treatment 顺序
# =====================================================

data$Treatment <- factor(
  data$Treatment,
  levels = c(
    "Non-heatwave+Non-mixing",                   
    "Non-heatwave+Mixing",                 
    "Heatwave+Non-mixing",                    
    "Heatwave+Mixing"
  )
)

# =====================================================
# 4. 添加样本量标注（用于图注）
# =====================================================

# 计算每个处理的样本量
n_labels <- data %>%
  group_by(Treatment) %>%
  summarise(n = n_distinct(Tank), .groups = "drop") %>%
  mutate(
    label = case_when(
      Treatment == "Non-heatwave+Non-mixing" ~ "Non-heatwave+Non-mixing",
      Treatment == "Non-heatwave+Mixing" ~ "Non-heatwave+Mixing",
      Treatment == "Heatwave+Non-mixing" ~ "Heatwave+Non-mixing",
      Treatment == "Heatwave+Mixing" ~ "Heatwave+Mixing"
    )
  )

# =====================================================
# 5. 定义变量顺序：TDP → PO4 → NH4，每个包含表面和底层
# =====================================================

vars <- c(
  "TDP_surface",   # (A) Surface TDP
  "TDP_bottom",    # (B) Bottom TDP
  "PO4_surface",   # (C) Surface PO4
  "PO4_bottom",    # (D) Bottom PO4
  "NH4_surface",   # (E) Surface NH4
  "NH4_bottom"     # (F) Bottom NH4
)

# 转为长格式
data_long <- data %>%
  pivot_longer(
    cols = all_of(vars),
    names_to = "Parameter",
    values_to = "Value"
  ) %>%
  # 过滤底层只保留 2,4,8 天（如果您的数据中底层只有这些天数）
  filter(
    (Parameter %in% c("TDP_bottom", "PO4_bottom", "NH4_bottom") & Day %in% c(2,4,8)) |
      (Parameter %in% c("TDP_surface", "PO4_surface", "NH4_surface"))
  )

# =====================================================
# 5.1 计算每个 Treatment、Day、Parameter 的均值和标准差
# =====================================================

summary_stats <- data_long %>%
  group_by(Treatment, Day, Parameter) %>%
  summarise(
    Mean = mean(Value, na.rm = TRUE),
    SD = sd(Value, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    # 计算标准误（可选，用于误差棒）
    SE = SD / sqrt(n),
    # 计算置信区间（95% CI）
    CI_lower = Mean - 1.96 * SE,
    CI_upper = Mean + 1.96 * SE,
    # 格式化输出：均值 ± 标准差
    Mean_SD = paste0(round(Mean, 3), " ± ", round(SD, 3)),
    # 添加样本量信息
    Mean_SD_n = paste0(round(Mean, 3), " ± ", round(SD, 3), " (n=", n, ")")
  )

# =====================================================
# 5.2 生成宽格式表格（便于查看和导出）
# =====================================================

# 方法1：按参数分开展示
table_by_parameter <- summary_stats %>%
  select(Treatment, Day, Parameter, Mean_SD) %>%
  pivot_wider(
    names_from = Parameter,
    values_from = Mean_SD,
    names_sep = "_"
  ) %>%
  arrange(Treatment, Day)

# 方法2：完整表格（包含样本量）
table_full <- summary_stats %>%
  select(Treatment, Day, Parameter, Mean_SD_n) %>%
  pivot_wider(
    names_from = Parameter,
    values_from = Mean_SD_n,
    names_sep = "_"
  ) %>%
  arrange(Treatment, Day)

# 方法3：按处理分组的长格式表格（适合报告）
table_long_format <- summary_stats %>%
  select(Treatment, Day, Parameter, Mean, SD, n, SE, CI_lower, CI_upper) %>%
  arrange(Treatment, Parameter, Day)

# =====================================================
# 5.3 导出表格到CSV文件
# =====================================================

output_folder <- "TDP_PO4_NH4_surface_bottom_260405"
if (!dir.exists(output_folder)) dir.create(output_folder)

# 导出按参数分组的表格
write.csv(table_by_parameter, 
          file.path(output_folder, "summary_stats_by_parameter.csv"),
          row.names = FALSE)

# 导出完整表格（含样本量）
write.csv(table_full, 
          file.path(output_folder, "summary_stats_full.csv"),
          row.names = FALSE)

# 导出长格式表格（含原始Mean, SD, n, SE, CI）
write.csv(table_long_format, 
          file.path(output_folder, "summary_stats_long_format.csv"),
          row.names = FALSE)

# 导出为Excel格式（如果安装了writexl包）
if (requireNamespace("writexl", quietly = TRUE)) {
  library(writexl)
  
  # 创建多个sheet的Excel文件
  sheets_list <- list(
    "By_Parameter" = table_by_parameter,
    "Full_Table" = table_full,
    "Long_Format" = table_long_format
  )
  
  write_xlsx(sheets_list, 
             path = file.path(output_folder, "summary_statistics.xlsx"))
  cat("\n✓ Excel文件已生成（包含3个工作表）\n")
} else {
  cat("\n⚠ 未安装writexl包，仅输出CSV文件。如需Excel格式，请运行：install.packages('writexl')\n")
}

# =====================================================
# 5.4 在R控制台打印表格预览
# =====================================================

cat("\n", rep("=", 80), "\n", sep="")
cat("统计摘要表格（均值 ± 标准差）\n")
cat(rep("=", 80), "\n\n", sep="")

cat("【按参数分组的表格预览】\n")
print(table_by_parameter)
cat("\n")

cat("【完整表格预览（含样本量）】\n")
print(table_full)
cat("\n")

cat("【长格式表格预览（原始数据）】\n")
print(head(table_long_format, 20))
cat("\n")

# =====================================================
# 5.5 生成每个参数的详细统计（按天）
# =====================================================

for (param in vars) {
  cat("\n", rep("-", 60), "\n", sep="")
  cat(param, "\n")
  cat(rep("-", 60), "\n", sep="")
  
  param_stats <- summary_stats %>%
    filter(Parameter == param) %>%
    select(Treatment, Day, Mean, SD, n, SE, CI_lower, CI_upper) %>%
    mutate(
      Mean = round(Mean, 3),
      SD = round(SD, 3),
      SE = round(SE, 3),
      CI_lower = round(CI_lower, 3),
      CI_upper = round(CI_upper, 3)
    )
  
  print(param_stats)
}

# =====================================================
# 6. 计算每个参数的Y轴范围（基于均值 ± 标准差）
# =====================================================

# 扩展Y轴范围以容纳误差棒
mean_data <- summary_stats

# TDP范围
tdp_range <- mean_data %>%
  filter(Parameter %in% c("TDP_surface", "TDP_bottom")) %>%
  summarise(
    ymin = min(Mean - SD, na.rm = TRUE),
    ymax = max(Mean + SD, na.rm = TRUE)
  ) %>%
  mutate(
    ymin = ymin * 0.95,
    ymax = ymax * 1.05
  )

# PO4范围
po4_range <- mean_data %>%
  filter(Parameter %in% c("PO4_surface", "PO4_bottom")) %>%
  summarise(
    ymin = min(Mean - SD, na.rm = TRUE),
    ymax = max(Mean + SD, na.rm = TRUE)
  ) %>%
  mutate(
    ymin = ymin * 0.95,
    ymax = ymax * 1.05
  )

# NH4范围
nh4_range <- mean_data %>%
  filter(Parameter %in% c("NH4_surface", "NH4_bottom")) %>%
  summarise(
    ymin = min(Mean - SD, na.rm = TRUE),
    ymax = max(Mean + SD, na.rm = TRUE)
  ) %>%
  mutate(
    ymin = ymin * 0.95,
    ymax = ymax * 1.05
  )

# =====================================================
# 7. Y 轴标签
# =====================================================

y_labels <- list(
  "TDP_surface" = expression("TDP_surface" * " (mg L"^{-1}*")"),
  "TDP_bottom"  = expression("TDP_bottom" * " (mg L"^{-1}*")"),
  "PO4_surface" = expression("PO4_surface" * " (mg L"^{-1}*")"),
  "PO4_bottom"  = expression("PO4_bottom" * " (mg L"^{-1}*")"),
  "NH4_surface" = expression("NH4_surface" * " (mg L"^{-1}*")"),
  "NH4_bottom"  = expression("NH4_bottom" * " (mg L"^{-1}*")")
)

# =====================================================
# 8. 单 panel 作图函数（均值连线 + 误差棒）
# =====================================================

make_plot <- function(param) {
  
  # 根据参数类型选择Y轴范围
  if (grepl("TDP", param)) {
    y_limits <- c(tdp_range$ymin, tdp_range$ymax)
  } else if (grepl("PO4", param)) {
    y_limits <- c(po4_range$ymin, po4_range$ymax)
  } else {
    y_limits <- c(nh4_range$ymin, nh4_range$ymax)
  }
  
  # 筛选当前参数的均值数据
  plot_data <- mean_data %>% filter(Parameter == param)
  
  ggplot(
    plot_data,
    aes(
      x = Day,
      y = Mean,
      color = Treatment,
      group = Treatment
    )
  ) +
    # 添加误差棒（横线式，即两端带横线的误差棒）
    geom_errorbar(
      aes(
        ymin = Mean - SD,
        ymax = Mean + SD
      ),
      width = 0.8,  # 横线宽度（天数单位）
      linewidth = 0.8,
      position = position_dodge(0)
    ) +
    # 添加均值连线
    geom_line(linewidth = 1.2) +
    # 可选：添加均值点（如果需要的话，取消注释）
    # geom_point(size = 2.5, shape = 21, fill = "white", stroke = 1.2) +
    scale_color_manual(
      values = treat_colors,
      labels = treat_labels,
      breaks = levels(data$Treatment)
    ) +
    scale_x_continuous(breaks = unique(plot_data$Day)) +
    scale_y_continuous(limits = y_limits) +
    labs(x = NULL, y = y_labels[[param]]) +
    theme_bw(base_size = 16) +
    theme(
      legend.position   = "none",
      panel.grid.minor  = element_blank(),
      axis.title.y      = element_text(size = 16),
      axis.text         = element_text(size = 14),
      plot.margin       = margin(6, 6, 6, 6)
    )
}

# =====================================================
# 9. 生成 6 个 panel（3行2列布局）
# =====================================================

plots <- map(vars, make_plot)

combined_plot <- wrap_plots(
  plots,
  ncol = 2,  # 2列布局
  nrow = 3   # 3行布局
) +         
  plot_annotation(
    tag_levels = "A",  # 大写字母
    tag_prefix = "(",  # 前缀括号
    tag_suffix = ")",  # 后缀括号
    theme = theme(
      plot.tag = element_text(size = 17, face = "bold")
    )
  )

# =====================================================
# 10. 公共图例（两行居中，包含样本量标注）
# =====================================================

legend_plot <- ggplot(
  mean_data,
  aes(
    x = Day,
    y = Mean,
    color = Treatment
  )
) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(
    values = treat_colors,
    labels = n_labels$label,  # 使用带样本量的标签
    breaks = levels(data$Treatment)
  ) +
  theme_void(base_size = 14) +
  theme(
    legend.position   = "bottom",
    legend.text       = element_text(size = 16),
    legend.key.width  = unit(1.5, "cm"),
    legend.title      = element_blank(),
    legend.margin     = margin(t = 0, b = 0, unit = "cm"),
    legend.box.margin = margin(t = -10, b = 5, unit = "pt")
  ) +
  guides(
    color = guide_legend(
      ncol = 2,
      byrow = TRUE,
      title.position = "top",
      label.position = "right"
    )
  )

# 提取图例
legend <- cowplot::get_legend(legend_plot)

# =====================================================
# 11. 拼接主图 + 图例
# =====================================================

final_plot <- plot_grid(
  combined_plot,
  legend,
  ncol = 1,
  rel_heights = c(1, 0.15)  # 调整图例高度
)

# =====================================================
# 12. 输出（PDF + JPG）
# =====================================================

pdf_file <- file.path(output_folder, "TDP_PO4_NH4_surface_bottom_with_errorbars.pdf")
ggsave(
  pdf_file,
  final_plot,
  width = 14,      # 宽度增加以适应3行2列
  height = 15,     # 高度增加以适应3行2列
  device = cairo_pdf
)

# 转换为JPG
img <- image_read_pdf(pdf_file, density = 400)
image_write(
  img,
  path = file.path(output_folder, "TDP_PO4_NH4_surface_bottom_with_errorbars.jpg"),
  format = "jpg"
)

# =====================================================
# 13. 输出完成信息
# =====================================================

cat("\n", rep("=", 80), "\n", sep="")
cat("✅ 图形和表格已成功生成！（含误差棒，新配色方案）\n")
cat(rep("=", 80), "\n\n", sep="")

cat("📊 输出文件说明：\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("输出文件夹：", output_folder, "\n\n")

cat("【图形文件】\n")
cat("  • TDP_PO4_NH4_surface_bottom_with_errorbars.pdf\n")
cat("  • TDP_PO4_NH4_surface_bottom_with_errorbars.jpg\n\n")

cat("【统计表格】\n")
cat("  • summary_stats_by_parameter.csv - 按参数分组的表格\n")
cat("  • summary_stats_full.csv - 完整表格（含样本量）\n")
cat("  • summary_stats_long_format.csv - 长格式表格（含SE和95% CI）\n")

if (requireNamespace("writexl", quietly = TRUE)) {
  cat("  • summary_statistics.xlsx - Excel格式（包含3个工作表）\n")
}

cat("\n📈 图形排列顺序：\n")
cat("  (A) Surface TDP\n")
cat("  (B) Bottom TDP\n")
cat("  (C) Surface PO4\n")
cat("  (D) Bottom PO4\n")
cat("  (E) Surface NH4\n")
cat("  (F) Bottom NH4\n\n")

cat("🎨 图形特点：\n")
cat("  • 显示均值连线 + 标准差误差棒（横线式）\n")
cat("  • 误差棒使用标准差（SD），可修改为SE或95% CI\n")
cat("  • 新配色方案：\n")
cat("    - Non-heatwave+Non-mixing: 浅蓝色 (#ABD9E9)\n")
cat("    - Non-heatwave+Mixing: 蓝色 (#2166AC)\n")
cat("    - Heatwave+Non-mixing: 橙红色 (#FC8D59)\n")
cat("    - Heatwave+Mixing: 红色 (#B2182B)\n")
cat("  • 所有处理均使用实线（无线型区分）\n")
cat("  • 图例已设置为两行居中显示，并标注了样本量\n\n")

cat("📋 表格统计内容：\n")
cat("  • 均值 (Mean)\n")
cat("  • 标准差 (SD)\n")
cat("  • 标准误 (SE)\n")
cat("  • 95%置信区间 (CI_lower, CI_upper)\n")
cat("  • 样本量 (n)\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")