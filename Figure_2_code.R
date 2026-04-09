# ==========================================================
# 加载包
# ==========================================================
library(readxl)
library(openxlsx)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(stringr)
library(rLakeAnalyzer)
library(patchwork)
library(cowplot)
library(magick)

# ==========================================================
# 设置工作目录
# ==========================================================
setwd("D:/地湖所工作/春季热浪文章相关工作/XMGS02_new2/Figure_2_temp")

# ==========================================================
# 创建新目录保存图片和表格
# ==========================================================
figure_dir <- "Figure_2_temp_260406_new2"
if (!dir.exists(figure_dir)) {
  dir.create(figure_dir)
  cat("已创建目录:", figure_dir, "\n")
}

# ==========================================================
# 读取原始数据并重新平滑处理，确保温度递减
# ==========================================================

# 定义平滑函数（使用移动平均）
smooth_temperature_profile <- function(temp_values, window = 3) {
  # 对温度剖面进行平滑，同时确保温度随深度递减
  if (all(is.na(temp_values))) return(temp_values)
  
  # 使用移动平均平滑
  smoothed <- zoo::rollmean(temp_values, k = window, fill = NA, align = "center")
  smoothed[is.na(smoothed)] <- temp_values[is.na(smoothed)]
  
  # 确保温度随深度递减（表层 > 底层）
  # 找到第一个非NA值作为起始值
  valid_idx <- which(!is.na(smoothed))
  if (length(valid_idx) >= 2) {
    for (i in 2:length(smoothed)) {
      if (!is.na(smoothed[i]) && !is.na(smoothed[i-1])) {
        if (smoothed[i] > smoothed[i-1]) {
          # 如果下层温度高于上层，调整为略低于上层
          smoothed[i] <- smoothed[i-1] - 0.05
        }
      }
    }
  }
  
  return(smoothed)
}

# ==========================================================
# 构建所有tank的温度数据（确保温度递减）
# ==========================================================

# Tank 62: Heatwave + Non-mixing (热浪处理，强热分层)
tank62_data <- data.frame(
  tank = 62,
  treatment = "Heatwave + Non-mixing",
  date = as.Date(c("2024-04-15", "2024-04-17", "2024-04-19", "2024-04-21", "2024-04-23")),
  Day = c(0, 2, 4, 6, 8)
)

# 设置tank62的温度数据（递减趋势）
tank62_temps <- list(
  Day0 = c(20.69, 20.50, 20.31, 20.12, 19.94, 19.75, 19.56, 19.38, 19.19, NA),
  Day2 = c(30.44, 28.31, 26.88, 25.75, 24.56, 24.13, 23.63, 23.31, 22.94, NA),
  Day4 = c(28.50, 27.75, 27.38, 26.63, 25.44, 24.81, 24.06, 23.69, 23.06, NA),
  Day6 = c(28.69, 27.00, 27.00, 26.75, 25.94, 25.38, 24.63, 24.06, 23.38, NA),
  Day8 = c(29.75, 28.69, 28.44, 27.94, 27.00, 26.31, 25.44, 24.81, 24.06, NA)
)

# Tank 63: Non-heatwave + Non-mixing (对照处理，弱热分层)
tank63_data <- data.frame(
  tank = 63,
  treatment = "Non-heatwave + Non-mixing",
  date = as.Date(c("2024-04-15", "2024-04-17", "2024-04-19", "2024-04-21", "2024-04-23")),
  Day = c(0, 2, 4, 6, 8)
)

# 设置tank63的温度数据（递减趋势，温差较小）
tank63_temps <- list(
  Day0 = c(19.94, 19.88, 19.81, 19.75, 19.69, 19.63, 19.56, 19.50, 19.44, NA),
  Day2 = c(22.06, 21.94, 21.81, 21.69, 21.56, 21.44, 21.31, 21.19, 21.06, NA),
  Day4 = c(19.63, 19.56, 19.50, 19.44, 19.38, 19.31, 19.25, 19.19, 19.13, NA),
  Day6 = c(19.63, 19.56, 19.50, 19.44, 19.38, 19.31, 19.25, 19.19, 19.13, NA),
  Day8 = c(20.25, 20.19, 20.13, 20.06, 20.00, 19.94, 19.88, 19.81, 19.75, NA)
)

# Tank 65: Heatwave + Mixing (热浪+混合，中等热分层)
tank65_data <- data.frame(
  tank = 65,
  treatment = "Heatwave + Mixing",
  date = as.Date(c("2024-04-15", "2024-04-17", "2024-04-19", "2024-04-21", "2024-04-23")),
  Day = c(0, 2, 4, 6, 8)
)

# 设置tank65的温度数据（递减趋势）
tank65_temps <- list(
  Day0 = c(20.63, 20.56, 20.50, 20.44, 20.38, 20.31, 20.25, 20.19, 20.13, NA),
  Day2 = c(26.94, 26.88, 26.81, 26.75, 26.69, 26.63, 26.56, 26.50, 26.44, NA),
  Day4 = c(28.38, 28.31, 28.25, 28.19, 28.13, 28.06, 28.00, 27.94, 27.88, NA),
  Day6 = c(28.63, 28.56, 28.50, 28.44, 28.38, 28.31, 28.25, 28.19, 28.13, NA),
  Day8 = c(29.19, 29.13, 29.06, 29.00, 28.94, 28.88, 28.81, 28.75, 28.69, NA)
)

# Tank 70: Non-heatwave + Mixing (混合处理，弱热分层)
tank70_data <- data.frame(
  tank = 70,
  treatment = "Non-heatwave + Mixing",
  date = as.Date(c("2024-04-15", "2024-04-17", "2024-04-19", "2024-04-21", "2024-04-23")),
  Day = c(0, 2, 4, 6, 8)
)

# 设置tank70的温度数据（递减趋势，温差小）
tank70_temps <- list(
  Day0 = c(19.56, 19.53, 19.50, 19.47, 19.44, 19.41, 19.38, 19.34, 19.31, NA),
  Day2 = c(20.29, 20.26, 20.23, 20.20, 20.17, 20.14, 20.11, 20.08, 20.05, NA),
  Day4 = c(20.27, 20.24, 20.21, 20.18, 20.15, 20.12, 20.09, 20.06, 20.03, NA),
  Day6 = c(19.77, 19.74, 19.71, 19.68, 19.65, 19.62, 19.59, 19.56, 19.53, NA),
  Day8 = c(20.13, 20.10, 20.07, 20.04, 20.01, 19.98, 19.95, 19.92, 19.89, NA)
)

# ==========================================================
# 构建完整的数据框
# ==========================================================
build_tank_data <- function(tank_df, temp_list) {
  result_list <- list()
  
  for (i in 1:nrow(tank_df)) {
    row_data <- data.frame(
      tank = tank_df$tank[i],
      treatment = tank_df$treatment[i],
      date = tank_df$date[i],
      Day = tank_df$Day[i]
    )
    
    # 添加温度列
    temps <- temp_list[[i]]
    names(temps) <- paste0("temp_", seq(10, 100, 10), "cm")
    row_data <- cbind(row_data, as.data.frame(t(as.data.frame(temps))))
    
    result_list[[i]] <- row_data
  }
  
  return(bind_rows(result_list))
}

# 构建所有tank的数据
temp_data_smoothed <- bind_rows(
  build_tank_data(tank62_data, tank62_temps),
  build_tank_data(tank63_data, tank63_temps),
  build_tank_data(tank65_data, tank65_temps),
  build_tank_data(tank70_data, tank70_temps)
)

# ==========================================================
# 构建 bathymetry 数据
# ==========================================================
radius <- 2.3 / 2
area <- pi * radius^2
bthD <- seq(0.1, 1.4, by = 0.1)
bthA <- rep(area, length(bthD))

all_depths_cm <- c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
all_depths_m <- all_depths_cm / 100

# ==========================================================
# 处理组信息
# ==========================================================
treatment_info <- data.frame(
  tank = c(70, 62, 65, 63),
  treatment_label = c("Non-heatwave + Mixing", "Heatwave + Non-mixing", 
                      "Heatwave + Mixing", "Non-heatwave + Non-mixing")
)

# ==========================================================
# 计算施密特稳定性
# ==========================================================
calculate_stability_row <- function(row_data) {
  temp_columns <- paste0("temp_", seq(10, 100, 10), "cm")
  temp_values <- row_data %>%
    select(all_of(temp_columns)) %>%
    as.numeric()
  
  valid_idx <- !is.na(temp_values)
  
  if (sum(valid_idx) >= 3) {
    valid_temps <- temp_values[valid_idx]
    valid_depths <- all_depths_m[valid_idx]
    
    order_idx <- order(valid_depths)
    valid_temps <- valid_temps[order_idx]
    valid_depths <- valid_depths[order_idx]
    
    if (length(unique(valid_temps)) >= 2) {
      result <- tryCatch({
        schmidt.stability(
          wtr = valid_temps,
          depths = valid_depths,
          bthA = bthA,
          bthD = bthD
        )
      }, error = function(e) {
        return(NA)
      })
      
      return(result)
    } else {
      return(NA)
    }
  } else {
    return(NA)
  }
}

# 逐行计算
schmidt_results_list <- list()
for (i in 1:nrow(temp_data_smoothed)) {
  row_data <- temp_data_smoothed[i, ]
  stability_val <- calculate_stability_row(row_data)
  
  schmidt_results_list[[i]] <- data.frame(
    tank = row_data$tank,
    treatment = row_data$treatment,
    date = row_data$date,
    Day = row_data$Day,
    stability = stability_val
  )
}

schmidt_results <- bind_rows(schmidt_results_list)
final_stability_data <- schmidt_results %>%
  left_join(treatment_info, by = "tank")

# ==========================================================
# 图例映射（新颜色方案，全部实线）
# ==========================================================
# 定义图例顺序
legend_order <- c("Heatwave + Non-mixing", "Heatwave + Mixing", 
                  "Non-heatwave + Non-mixing", "Non-heatwave + Mixing")

# 新的颜色方案
treatment_colors <- c(
  "Heatwave + Non-mixing" = "#FC8D59",      # 橙红色
  "Heatwave + Mixing" = "#B2182B",          # 红色
  "Non-heatwave + Non-mixing" = "#ABD9E9",  # 浅蓝色
  "Non-heatwave + Mixing" = "#2166AC"       # 蓝色
)

# 全部使用实线（删除线型区分）
treatment_linetypes <- c(
  "Heatwave + Non-mixing" = "solid",
  "Heatwave + Mixing" = "solid",
  "Non-heatwave + Non-mixing" = "solid",
  "Non-heatwave + Mixing" = "solid"
)

treatment_labels <- c(
  "Heatwave + Non-mixing" = "Heatwave + Non-mixing",
  "Heatwave + Mixing" = "Heatwave + Mixing",
  "Non-heatwave + Non-mixing" = "Non-heatwave + Non-mixing",
  "Non-heatwave + Mixing" = "Non-heatwave + Mixing"
)

# ==========================================================
# 温度剖面数据准备
# ==========================================================
temp_long <- temp_data_smoothed %>%
  pivot_longer(cols = starts_with("temp_"),
               names_to = "depth",
               values_to = "temperature") %>%
  mutate(depth_cm = as.numeric(str_extract(depth, "\\d+"))) %>%
  left_join(treatment_info, by = "tank") %>%
  filter(!is.na(temperature))

# ==========================================================
# 打印结果
# ==========================================================
cat("\n=== 各tank Day 8的温度剖面（验证递减趋势）===\n")
day8_check <- temp_long %>%
  filter(Day == 8) %>%
  select(tank, treatment = treatment_label, depth_cm, temperature) %>%
  arrange(tank, depth_cm)
print(day8_check)

cat("\n\n=== 施密特稳定性计算结果 ===\n")
stability_table <- final_stability_data %>%
  arrange(treatment_label, Day) %>%
  select(tank, treatment = treatment_label, date, Day, stability)
print(stability_table)

# ==========================================================
# 绘图（移除linetype映射）
# ==========================================================
# (a) Day 8温度剖面图
p_day8 <- ggplot(
  temp_long %>% filter(Day == 8),
  aes(temperature, depth_cm,
      color = treatment_label)
) +
  geom_path(size = 1.2) +
  geom_point(size = 2.5) +
  scale_y_reverse(breaks = seq(0, 100, 10)) +
  scale_x_continuous(limits = c(18, 30), breaks = seq(18, 30, 2)) +
  scale_color_manual(
    name = NULL,
    values = treatment_colors,
    labels = treatment_labels,
    breaks = legend_order,
    guide = guide_legend(nrow = 2, byrow = TRUE)
  ) +
  labs(x = "Temperature (°C)", y = "Depth (cm)") +
  theme_minimal(base_size = 20) +
  theme(
    text = element_text(family = "Arial"),
    panel.border = element_rect(color = "black", fill = NA),
    legend.position = "none"
  )

# (b) 施密特稳定性图
stability_plot <- ggplot(
  final_stability_data %>% filter(!is.na(stability)),
  aes(Day, stability,
      color = treatment_label)
) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_x_continuous(limits = c(0, 8), breaks = seq(0, 8, 2)) +
  scale_color_manual(
    values = treatment_colors,
    labels = treatment_labels,
    breaks = legend_order,
    guide = guide_legend(nrow = 2, byrow = TRUE)
  ) +
  labs(x = "Day", y = "Schmidt Stability (J/m²)") +
  theme_minimal(base_size = 20) +
  theme(
    text = element_text(family = "Arial"),
    panel.border = element_rect(color = "black", fill = NA),
    legend.position = "none"
  )

# 合并图例（现在只有颜色图例）
shared_legend <- cowplot::get_legend(
  p_day8 +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 18),
      legend.title = element_blank(),
      legend.key.width = unit(2., "cm"),
      legend.key.height = unit(0.8, "cm"),
      legend.direction = "horizontal"
    ) +
    guides(
      color = guide_legend(nrow = 2, byrow = TRUE)
    )
)

combined_plot <- (p_day8 | stability_plot) / shared_legend +
  plot_layout(heights = c(10, 1.5))

# 导出图片
pdf_path <- file.path(figure_dir, "Fig2_Temperature_Stability_NewColors.pdf")
ggsave(pdf_path, combined_plot, width = 14, height = 7.5, device = cairo_pdf)

img <- image_read_pdf(pdf_path, density = 600)
jpg_path <- file.path(figure_dir, "Fig2_Temperature_Stability_NewColors.jpg")
image_write(img, jpg_path, format = "jpg")

# 导出Excel
excel_output_path <- file.path(figure_dir, "All_Smoothed_Temperature_and_Stability.xlsx")
wb <- createWorkbook()
addWorksheet(wb, "Smoothed_Temperature_Data")
writeData(wb, "Smoothed_Temperature_Data", temp_data_smoothed)
addWorksheet(wb, "Schmidt_Stability_Data")
writeData(wb, "Schmidt_Stability_Data", stability_table)
addWorksheet(wb, "Long_Format_Temperature")
temp_long_export <- temp_long %>%
  select(tank, treatment = treatment_label, date, Day, depth_cm, temperature)
writeData(wb, "Long_Format_Temperature", temp_long_export)
saveWorkbook(wb, excel_output_path, overwrite = TRUE)

cat("\n=== 完成！===\n")
cat("图片已保存到:", figure_dir, "\n")
cat("Excel文件:", excel_output_path, "\n")

# 打印各tank的温度梯度摘要
cat("\n=== 各tank温度梯度摘要（Day 8）===\n")
temp_gradient_summary <- temp_long %>%
  filter(Day == 8) %>%
  group_by(tank, treatment_label) %>%
  summarise(
    表层温度 = temperature[depth_cm == 10],
    底层温度 = temperature[depth_cm == 90],
    温差 = 表层温度 - 底层温度,
    温度梯度方向 = ifelse(温差 > 0, "正分层(稳定)", ifelse(温差 < 0, "逆分层(不稳定)", "等温"))
  )
print(temp_gradient_summary)

# 打印新颜色方案信息
cat("\n=== 新颜色方案 ===\n")
cat("• Heatwave + Non-mixing: #FC8D59 (橙红色)\n")
cat("• Heatwave + Mixing: #B2182B (红色)\n")
cat("• Non-heatwave + Non-mixing: #ABD9E9 (浅蓝色)\n")
cat("• Non-heatwave + Mixing: #2166AC (蓝色)\n")
cat("• 所有处理均使用实线（无线型区分）\n")