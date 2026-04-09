# ==========================================================
# 🌿 按变量类型分组的散点图绘制（基于筛选模型）
# ==========================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(openxlsx)
library(gridExtra)
library(grid)
library(purrr)

# ==========================================================
# 路径设置
# ==========================================================
setwd("D:/地湖所工作/春季热浪文章相关工作/XMGS02_new2/Figure_mul")
out_dir <- "Figure_mul_260326"  # 使用新的输出文件夹
plot_dir <- paste0(out_dir, "/Scatter_Plots_by_Group_Actual_Values_260402")  # 修改文件夹名称
if(!dir.exists(plot_dir)) dir.create(plot_dir)

# ==========================================================
# 读取数据
# ==========================================================
data <- read_excel("data_all_new2.xlsx")

# ==========================================================
# 添加处理组定义
# ==========================================================
data <- data %>%
  mutate(Treatment = case_when(
    Tank %in% c(64, 65) ~ "Heatwave + Mixing",
    Tank %in% c(62, 67) ~ "Heatwave + Non-mixing",
    Tank %in% c(54, 59, 68, 70) ~ "Non-heatwave + Mixing",
    Tank %in% c(57, 63) ~ "Non-heatwave + Non-mixing",
    TRUE ~ NA_character_
  ))

# ==========================================================
# 定义变量
# ==========================================================
X_vars <- c(
  "NH4_bottom", "PO4_bottom", "TDN_bottom", "TDP_bottom",
  "NH4_surface", "PO4_surface", "TDN_surface", "TDP_surface", 
  "NTU_surface", "Temp_surface", "DO_surface", "stability"
)

Y_vars <- c("total_biovolume", "total_cells", "Shannon", "Pielou", "Simpson", "Margalef", "Species_Number", "Fv_Fm",
            "Chlorophyta_bio", "Chlorophyta_cells", "Chlorophyta_Ratio",
            "Cyanophyceae_bio", "Cyanophyceae_cells", "Cyanophyceae_Ratio",
            "filamentous_ratio","filamentous_cells", "bloom_ratio", "bloom_cells", "Pseudanabaena_sp_cells",
            "Pseudanabaena_sp_Ratio", "Closterium_sp_bio", "Closterium_sp_cells", "Closterium_sp_Ratio",
            "Chlorella_sp_bio", "Chlorella_sp_cells", "Chlorella_sp_Ratio", 
            "Scenedesmus_sp_Ratio", "Kirchneriella_sp_bio", "Kirchneriella_sp_Ratio","Cryptomonas_sp_bio")

# 响应变量标签
response_labels <- c(
  "total_biovolume" = "Total biovolume (mm³ L⁻¹)",
  "total_cells" = "Total cell concentration (cells mL⁻¹)",
  "Shannon" = "Shannon diversity index",
  "Pielou" = "Pielou evenness",
  "Simpson" = "Simpson diversity index",
  "Margalef" = "Margalef richness index",
  "Species_Number" = "Species number",
  "Fv_Fm" = "Fv/Fm",
  "Chlorophyta_bio" = "Chlorophyta biovolume (mm³ L⁻¹)",
  "Chlorophyta_cells" = "Chlorophyta cell concentration (cells mL⁻¹)",
  "Chlorophyta_Ratio" = "Chlorophyta ratio (%)",
  "Cyanophyceae_bio" = "Cyanophycea biovolume (mm³ L⁻¹)",
  "Cyanophyceae_cells" = "Cyanophyceae cell concentration (cells mL⁻¹)",
  "Cyanophyceae_Ratio" = "Cyanophyceae ratio (%)",
  "filamentous_ratio" = "Filamentous ratio (%)",
  "filamentous_cells" = "Filamentous cell concentration (cells mL⁻¹)",
  "bloom_ratio" = "Bloom ratio (%)",
  "bloom_cells" = "Bloom cell concentration (cells mL⁻¹)",
  "Pseudanabaena_sp_cells" = "Pseudanabaena sp. cell concentration (cells mL⁻¹)",
  "Pseudanabaena_sp_Ratio" = "Pseudanabaena sp. ratio (%)",
  "Closterium_sp_bio" = "Closterium sp. biovolume (mm³ L⁻¹)",
  "Closterium_sp_cells" = "Closterium sp. cell concentration (cells mL⁻¹)",
  "Closterium_sp_Ratio" = "Closterium sp. ratio (%)",
  "Chlorella_sp_bio" = "Chlorella sp. biovolume (mm³ L⁻¹)",
  "Chlorella_sp_cells" = "Chlorella sp. cell concentration (cells mL⁻¹)",
  "Chlorella_sp_Ratio" = "Chlorella sp. ratio (%)",
  "Scenedesmus_sp_Ratio" = "Scenedesmus sp. ratio (%)",
  "Kirchneriella_sp_bio" = "Kirchneriella sp. biovolume (mm³ L⁻¹)",
  "Kirchneriella_sp_Ratio" = "Kirchneriella sp. ratio (%)",
  "Cryptomonas_sp_bio" = "Cryptomonas sp. biovolume (mm³ L⁻¹)"
)

# 变量分组定义
variable_groups <- list(
  Thermal = c("Temp_surface"),
  Stability = c("stability"),
  Nutrients = c("NH4_bottom", "PO4_bottom", "TDN_bottom", "TDP_bottom",
                "NH4_surface", "PO4_surface", "TDN_surface", "TDP_surface",
                "NTU_surface", "DO_surface")
)

# 变量标签
var_labels <- c(
  "Temp_surface" = "Temp_surface (°C)",
  "stability" = "stability (J m⁻²)",
  "NH4_bottom" = "NH4_bottom (mg L⁻¹)",
  "PO4_bottom" = "PO4_bottom (mg L⁻¹)",
  "TDN_bottom" = "TDN_bottom (mg L⁻¹)",
  "TDP_bottom" = "TDP_bottom (mg L⁻¹)",
  "NH4_surface" = "NH4_surface (mg L⁻¹)",
  "PO4_surface" = "PO4_surface (mg L⁻¹)",
  "TDN_surface" = "TDN_surface (mg L⁻¹)",
  "TDP_surface" = "TDP_surface (mg L⁻¹)",
  "NTU_surface" = "NTU_surface",
  "DO_surface" = "DO_surface (mg L⁻¹)"
)

# ==========================================================
# 读取筛选模型结果
# ==========================================================
model_summary <- read.xlsx(paste0(out_dir, "/Best_Model_Summary_Ecological_Priority.xlsx"))

# 筛选AdjR² > 0.3的模型用于绘图
selected_models <- model_summary %>% 
  filter(Best_AdjR2 > 0.3) %>%
  mutate(Best_Variables_List = strsplit(as.character(Best_Variables), ", "))

cat("找到", nrow(selected_models), "个AdjR² > 0.3的模型\n")

# ==========================================================
# 预计算所有筛选后的最佳模型（存储为模型对象）
# ==========================================================
cat("\n预计算筛选后的最佳模型...\n")
best_models <- list()

for(i in 1:nrow(selected_models)) {
  y_var <- selected_models$Response[i]
  all_vars <- unlist(strsplit(as.character(selected_models$Best_Variables[i]), ", "))
  
  if(length(all_vars) > 0 && all_vars[1] != "None") {
    formula <- as.formula(paste(y_var, "~", paste(all_vars, collapse = "+")))
    model <- lm(formula, data = data, na.action = na.omit)
    best_models[[y_var]] <- model
    cat("  已构建模型:", y_var, "~", paste(all_vars, collapse = " + "), "\n")
  }
}

# ==========================================================
# 修改后的绘图函数：使用实际值（非残差）
# ==========================================================
calculate_partial_residuals <- function(data, y_var, x_var, best_model) {
  # 获取最佳模型中的所有变量
  all_vars <- names(coef(best_model))[-1]  # 移除截距项
  
  # 计算其他变量的预测值（排除当前x变量）
  other_vars <- setdiff(all_vars, x_var)
  
  if(length(other_vars) > 0) {
    # 使用其他变量拟合模型
    formula_others <- as.formula(paste(y_var, "~", paste(other_vars, collapse = "+")))
    model_others <- lm(formula_others, data = data, na.action = na.omit)
    others_fitted <- predict(model_others, newdata = data)
  } else {
    # 如果没有其他变量，使用均值
    others_fitted <- mean(data[[y_var]], na.rm = TRUE)
  }
  
  # 计算偏残差（实际值减去其他变量的预测值）
  partial_residuals <- residuals(best_model) + 
    coef(best_model)[x_var] * data[[x_var]]
  
  # 从最佳模型中提取当前x变量的系数和p值（与条形图完全一致）
  coef_summary <- summary(best_model)$coefficients
  if(x_var %in% rownames(coef_summary)) {
    coefficient <- coef_summary[x_var, "Estimate"]
    p_value <- coef_summary[x_var, "Pr(>|t|)"]
    std_error <- coef_summary[x_var, "Std. Error"]
    t_value <- coef_summary[x_var, "t value"]
  } else {
    # 如果x变量不在最佳模型中（理论上不会发生，因为x_var是从模型中提取的）
    coefficient <- NA
    p_value <- NA
    std_error <- NA
    t_value <- NA
  }
  
  return(list(
    x = data[[x_var]],
    y_actual = data[[y_var]],  # 改为实际值
    y_partial = partial_residuals,  # 保留偏残差以备其他用途
    coefficient = coefficient,
    p_value = p_value,
    std_error = std_error,
    t_value = t_value,
    r_squared = summary(best_model)$adj.r.squared,
    all_vars = all_vars
  ))
}

# 修改后的散点图函数：使用实际值
plot_scatter_actual <- function(data, y_var, x_var, best_model, response_label, var_label) {
  # 计算数据（包含实际值）
  plot_data_raw <- calculate_partial_residuals(data, y_var, x_var, best_model)
  
  # 创建数据框用于绘图（使用实际值）
  plot_data <- data.frame(
    x = plot_data_raw$x,
    y_actual = plot_data_raw$y_actual,
    Treatment = data$Treatment  # 添加处理组信息
  ) %>% na.omit()
  
  # 创建显著性标签（与条形图完全一致）
  p_value <- plot_data_raw$p_value
  sig_label <- ifelse(is.na(p_value), "P = NA",
                      ifelse(p_value < 0.001, "P < 0.001***",
                             ifelse(p_value < 0.01, "P < 0.01**",
                                    ifelse(p_value < 0.05, "P < 0.05*",
                                           paste("P =", round(p_value, 3))))))
  
  # 定义处理组的颜色（蓝红配色）
  treatment_colors <- c(
    "Heatwave + Mixing" = "#B2182B",        # 红色
    "Heatwave + Non-mixing" = "#FC8D59",    # 橙红色
    "Non-heatwave + Mixing" = "#2166AC",    # 蓝色
    "Non-heatwave + Non-mixing" = "#ABD9E9"  # 浅蓝色
  )
  
  # 拟合线性模型（实际值）
  model_actual <- lm(y_actual ~ x, data = plot_data)
  r2_actual <- summary(model_actual)$r.squared
  r2_adj_actual <- summary(model_actual)$adj.r.squared
  
  # 创建散点图（Y轴为实际值）
  p <- ggplot(plot_data, aes(x = x, y = y_actual, color = Treatment)) +
    geom_point(alpha = 0.7, size = 2.5) +
    geom_smooth(method = "lm", se = TRUE, aes(group = 1), 
                color = "black", fill = "grey70", alpha = 0.2, linetype = "dashed") +
    scale_color_manual(values = treatment_colors, name = "Treatment") +
    labs(
      x = var_label,
      y = response_label,  # 直接使用响应变量标签
      title = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_blank(),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 9),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(fill = NA, color = "grey70", size = 0.5),
      legend.position = "right",
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8)
    )
  
  # 添加显著性标注
  x_range <- range(plot_data$x, na.rm = TRUE)
  y_range <- range(plot_data$y_actual, na.rm = TRUE)
  
  p <- p + annotate("text", 
                    x = x_range[2] - diff(x_range) * 0.05,
                    y = y_range[2] - diff(y_range) * 0.05,
                    label = sig_label,
                    hjust = 1, vjust = 1, size = 3.5, color = "grey30",
                    fontface = "italic")
  
  # 添加R²
  eq_label <- paste0("R² = ", round(r2_actual, 3))
  
  p <- p + annotate("text", 
                    x = x_range[2] - diff(x_range) * 0.05,
                    y = y_range[1] + diff(y_range) * 0.05,
                    label = eq_label,
                    hjust = 1, vjust = 0, size = 3, color = "grey30")
  
  # 返回图表和统计信息
  return(list(
    plot = p,
    stats = data.frame(
      Response = y_var,
      Predictor = x_var,
      Full_Model_AdjR2 = plot_data_raw$r_squared,
      Actual_R2 = r2_actual,
      Actual_AdjR2 = r2_adj_actual,
      Coefficient = plot_data_raw$coefficient,
      P_Value = p_value,
      stringsAsFactors = FALSE
    )
  ))
}

# ==========================================================
# 主函数：按响应变量分类出图
# ==========================================================

# 存储所有需要绘制的组合
group_plots_all <- list()
# 存储所有R²统计信息
actual_r2_stats <- data.frame()

# 遍历每个响应变量
for(i in 1:nrow(selected_models)) {
  y_var <- selected_models$Response[i]
  model_info <- selected_models[i, ]
  all_vars <- unlist(strsplit(as.character(model_info$Best_Variables), ", "))
  
  # 检查该响应变量是否有对应的最佳模型对象
  if(!(y_var %in% names(best_models))) {
    cat("跳过", y_var, "- 无模型对象\n")
    next
  }
  
  best_model <- best_models[[y_var]]
  
  cat("\n处理响应变量:", y_var, "\n")
  cat("  包含的预测变量:", paste(all_vars, collapse = ", "), "\n")
  
  # 收集该响应变量的所有图表
  response_plots <- list()
  response_stats <- data.frame()
  
  # 为每个预测变量绘制图表
  for(x_var in all_vars) {
    # 确定该预测变量属于哪个分组（用于后续分类）
    var_group <- NA
    for(group_name in names(variable_groups)) {
      if(x_var %in% variable_groups[[group_name]]) {
        var_group <- group_name
        break
      }
    }
    
    cat("    绘制:", x_var, "(", var_group, ")\n")
    
    # 绘制散点图（使用实际值）
    plot_result <- plot_scatter_actual(
      data = data,
      y_var = y_var,
      x_var = x_var,
      best_model = best_model,
      response_label = response_labels[y_var],
      var_label = var_labels[x_var]
    )
    
    # 存储图表
    plot_key <- paste(y_var, x_var, sep = "_")
    response_plots[[plot_key]] <- plot_result$plot
    
    # 添加分组信息到统计表
    plot_result$stats$Group <- var_group
    plot_result$stats$Variable_Label <- var_labels[x_var]
    plot_result$stats$Response_Label <- response_labels[y_var]
    
    response_stats <- rbind(response_stats, plot_result$stats)
    actual_r2_stats <- rbind(actual_r2_stats, plot_result$stats)
    
    # 保存单个图表
    single_filename <- paste0(plot_dir, "/", y_var, "_vs_", x_var, "_actual.png")
    ggsave(single_filename, plot_result$plot, width = 6, height = 5, dpi = 300)
  }
  
  # 为该响应变量创建组合图
  if(length(response_plots) > 0) {
    n_plots <- length(response_plots)
    
    # 动态设置布局：每行最多3个图
    ncol <- ifelse(n_plots <= 2, 2, ifelse(n_plots <= 3, 3, 3))
    nrow <- ceiling(n_plots / ncol)
    
    # 提取图表对象
    plots_to_arrange <- response_plots
    
    # 添加主标题
    main_title <- response_labels[y_var]
    
    # 创建组合图
    combined_plot <- arrangeGrob(
      grobs = plots_to_arrange,
      ncol = ncol,
      nrow = nrow,
      top = textGrob(main_title, gp = gpar(fontsize = 14, fontface = "bold"))
    )
    
    # 保存组合图
    combined_filename <- paste0(plot_dir, "/Response_", y_var, "_Summary_actual.png")
    ggsave(combined_filename, combined_plot, width = 6 * ncol, height = 5 * nrow, dpi = 300)
    
    cat("  保存组合图:", combined_filename, "\n")
    
    # 保存该响应变量的统计表
    if(nrow(response_stats) > 0) {
      stats_filename <- paste0(plot_dir, "/Stats_", y_var, "_Actual_R2.xlsx")
      write.xlsx(response_stats, stats_filename, overwrite = TRUE)
      cat("  保存统计表:", stats_filename, "\n")
    }
  }
}

# ==========================================================
# 按变量分组创建补充组合图（使用实际值）
# ==========================================================

cat("\n按变量分组创建补充组合图...\n")

# 重新按分组整理图表（用于补充）
for(group_name in names(variable_groups)) {
  group_vars <- variable_groups[[group_name]]
  
  # 收集该分组的所有图表
  group_plots <- list()
  
  for(i in 1:nrow(selected_models)) {
    y_var <- selected_models$Response[i]
    
    if(!(y_var %in% names(best_models))) next
    
    best_model <- best_models[[y_var]]
    all_vars <- unlist(strsplit(as.character(selected_models$Best_Variables[i]), ", "))
    common_vars <- intersect(all_vars, group_vars)
    
    for(x_var in common_vars) {
      plot_key <- paste(y_var, x_var, sep = "_")
      
      # 重新生成图表（使用实际值）
      plot_data_raw <- calculate_partial_residuals(data, y_var, x_var, best_model)
      
      plot_data <- data.frame(
        x = plot_data_raw$x,
        y_actual = plot_data_raw$y_actual,
        Treatment = data$Treatment
      ) %>% na.omit()
      
      p_value <- plot_data_raw$p_value
      sig_label <- ifelse(is.na(p_value), "P = NA",
                          ifelse(p_value < 0.001, "P < 0.001***",
                                 ifelse(p_value < 0.01, "P < 0.01**",
                                        ifelse(p_value < 0.05, "P < 0.05*",
                                               paste("P =", round(p_value, 3))))))
      
      treatment_colors <- c(
        "Heatwave + Mixing" = "#B2182B",
        "Heatwave + Non-mixing" = "#FC8D59",
        "Non-heatwave + Mixing" = "#2166AC",
        "Non-heatwave + Non-mixing" = "#ABD9E9"
      )
      
      model_actual <- lm(y_actual ~ x, data = plot_data)
      r2_actual <- summary(model_actual)$r.squared
      
      p <- ggplot(plot_data, aes(x = x, y = y_actual, color = Treatment)) +
        geom_point(alpha = 0.7, size = 2.5) +
        geom_smooth(method = "lm", se = TRUE, aes(group = 1), 
                    color = "black", fill = "grey70", alpha = 0.2, linetype = "dashed") +
        scale_color_manual(values = treatment_colors, name = "Treatment") +
        labs(
          x = var_labels[x_var],
          y = response_labels[y_var]
        ) +
        theme_minimal(base_size = 16) +
        theme(
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(fill = NA, color = "grey70", size = 0.5),
          legend.position = "right",
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 14)
        )
      
      x_range <- range(plot_data$x, na.rm = TRUE)
      y_range <- range(plot_data$y_actual, na.rm = TRUE)
      
      p <- p + annotate("text", 
                        x = x_range[2] - diff(x_range) * 0.05,
                        y = y_range[2] - diff(y_range) * 0.05,
                        label = sig_label,
                        hjust = 1, vjust = 1, size = 4, color = "grey30",
                        fontface = "italic")
      
      p <- p + annotate("text", 
                        x = x_range[2] - diff(x_range) * 0.05,
                        y = y_range[1] + diff(y_range) * 0.05,
                        label = paste0("R² = ", round(r2_actual, 3)),
                        hjust = 1, vjust = 0, size = 4, color = "grey30")
      
      group_plots[[plot_key]] <- p
    }
  }
  
  # 为该分组创建组合图
  if(length(group_plots) > 0) {
    n_plots <- length(group_plots)
    ncol <- ifelse(n_plots <= 2, 2, ifelse(n_plots <= 4, 2, 3))
    nrow <- ceiling(n_plots / ncol)
    
    combined_plot <- arrangeGrob(
      grobs = group_plots,
      ncol = ncol,
      nrow = nrow,
      top = textGrob(paste(group_name, "Variables - Effects on Phytoplankton"), 
                     gp = gpar(fontsize = 14, fontface = "bold"))
    )
    
    combined_filename <- paste0(plot_dir, "/Group_", group_name, "_Summary_actual.png")
    ggsave(combined_filename, combined_plot, width = 6 * ncol, height = 5 * nrow, dpi = 300)
    
    cat("  保存分组组合图:", combined_filename, "\n")
  }
}

# ==========================================================
# 创建总体汇总图（所有响应变量的关键关系）
# ==========================================================

cat("\n创建总体汇总图...\n")

# 定义关键响应变量（选择前6个最重要的）
key_responses <- intersect(c("total_biovolume", "total_cells", "Shannon", 
                             "Cyanophyceae_Ratio", "filamentous_ratio", "bloom_ratio"),
                           selected_models$Response)

# 收集关键图表（每个响应变量选一个最重要的预测变量）
key_plots <- list()

for(y_var in key_responses) {
  if(y_var %in% names(best_models)) {
    # 获取该响应变量的所有预测变量
    model_info <- selected_models[selected_models$Response == y_var, ]
    all_vars <- unlist(strsplit(as.character(model_info$Best_Variables), ", "))
    
    if(length(all_vars) > 0) {
      # 选择第一个预测变量
      x_var <- all_vars[1]
      
      plot_result <- plot_scatter_actual(
        data = data,
        y_var = y_var,
        x_var = x_var,
        best_model = best_models[[y_var]],
        response_label = response_labels[y_var],
        var_label = var_labels[x_var]
      )
      
      key_plots[[paste(y_var, x_var, sep = "_")]] <- plot_result$plot
    }
  }
}

if(length(key_plots) > 0) {
  n_plots <- length(key_plots)
  ncol <- 3
  nrow <- ceiling(n_plots / ncol)
  
  overall_plot <- arrangeGrob(
    grobs = key_plots,
    ncol = ncol,
    nrow = nrow,
    top = textGrob("Key Environmental Drivers of Phytoplankton Community", 
                   gp = gpar(fontsize = 16, fontface = "bold"))
  )
  
  ggsave(paste0(plot_dir, "/Overall_Summary_Key_Relationships_actual.png"), 
         overall_plot, width = 6 * ncol, height = 5 * nrow, dpi = 300)
  
  cat("保存总体汇总图\n")
}

# ==========================================================
# 导出完整的R²统计表格
# ==========================================================

cat("\n导出R²统计表格...\n")

if(nrow(actual_r2_stats) > 0) {
  # 按响应变量和R²排序
  actual_r2_stats <- actual_r2_stats %>%
    arrange(Response, desc(Actual_R2))
  
  # 保存主统计表
  write.xlsx(actual_r2_stats, paste0(plot_dir, "/Actual_R2_Statistics_Complete.xlsx"), overwrite = TRUE)
  
  # 创建摘要统计表（按响应变量汇总）
  summary_stats <- actual_r2_stats %>%
    group_by(Response, Response_Label) %>%
    summarise(
      Num_Predictors = n(),
      Mean_R2 = mean(Actual_R2, na.rm = TRUE),
      Max_R2 = max(Actual_R2, na.rm = TRUE),
      Min_R2 = min(Actual_R2, na.rm = TRUE),
      SD_R2 = sd(Actual_R2, na.rm = TRUE),
      Best_Predictor = Predictor[which.max(Actual_R2)],
      Best_Predictor_Group = Group[which.max(Actual_R2)],
      Best_R2 = max(Actual_R2, na.rm = TRUE),
      .groups = 'drop'
    )
  
  write.xlsx(summary_stats, paste0(plot_dir, "/Actual_R2_Summary_by_Response.xlsx"), overwrite = TRUE)
  
  # 创建按分组汇总的统计表
  group_summary <- actual_r2_stats %>%
    group_by(Group, Response) %>%
    summarise(
      Num_Predictors = n(),
      Mean_R2 = mean(Actual_R2, na.rm = TRUE),
      Max_R2 = max(Actual_R2, na.rm = TRUE),
      .groups = 'drop'
    )
  
  write.xlsx(group_summary, paste0(plot_dir, "/Actual_R2_Summary_by_Group.xlsx"), overwrite = TRUE)
  
  cat("保存R²统计表格:\n")
  cat("  - Actual_R2_Statistics_Complete.xlsx (完整统计表)\n")
  cat("  - Actual_R2_Summary_by_Response.xlsx (按响应变量汇总)\n")
  cat("  - Actual_R2_Summary_by_Group.xlsx (按分组汇总)\n")
}

# ==========================================================
# 导出绘图汇总表（包含所有模型信息）
# ==========================================================

cat("\n导出绘图汇总表...\n")

# 创建绘图任务汇总表
plot_summary <- data.frame()

for(i in 1:nrow(selected_models)) {
  y_var <- selected_models$Response[i]
  model_info <- selected_models[i, ]
  all_vars <- unlist(strsplit(as.character(model_info$Best_Variables), ", "))
  
  if(!(y_var %in% names(best_models))) next
  
  best_model <- best_models[[y_var]]
  
  for(x_var in all_vars) {
    # 从筛选后的最佳模型中提取系数和p值
    coef_summary <- summary(best_model)$coefficients
    if(x_var %in% rownames(coef_summary)) {
      coefficient <- coef_summary[x_var, "Estimate"]
      p_value <- coef_summary[x_var, "Pr(>|t|)"]
      std_error <- coef_summary[x_var, "Std. Error"]
      t_value <- coef_summary[x_var, "t value"]
    } else {
      coefficient <- NA
      p_value <- NA
      std_error <- NA
      t_value <- NA
    }
    
    # 获取实际值R²
    actual_r2 <- actual_r2_stats %>%
      filter(Response == y_var, Predictor == x_var) %>%
      pull(Actual_R2)
    
    if(length(actual_r2) == 0) actual_r2 <- NA
    
    # 创建显著性标签
    significance <- ifelse(is.na(p_value), "NA",
                           ifelse(p_value < 0.001, "***",
                                  ifelse(p_value < 0.01, "**",
                                         ifelse(p_value < 0.05, "*", "ns"))))
    
    # 确定变量分组
    var_group <- NA
    for(group_name in names(variable_groups)) {
      if(x_var %in% variable_groups[[group_name]]) {
        var_group <- group_name
        break
      }
    }
    
    plot_summary <- rbind(plot_summary, data.frame(
      Group = var_group,
      Predictor = x_var,
      Response = y_var,
      Response_Label = response_labels[y_var],
      Coefficient = round(coefficient, 3),
      Std_Error = round(std_error, 3),
      T_Value = round(t_value, 3),
      P_Value = p_value,
      Significance = significance,
      Actual_R2 = round(actual_r2, 3),
      Full_Model_AdjR2 = model_info$Best_AdjR2,
      Full_Model_PredR2 = model_info$Best_PredR2,
      Ecological_Score = model_info$Ecological_Score,
      Model_Size = model_info$Model_Size,
      Selection_Reason = model_info$Selection_Reason,
      stringsAsFactors = FALSE
    ))
  }
}

# 保存汇总表
if(nrow(plot_summary) > 0) {
  write.xlsx(plot_summary, paste0(plot_dir, "/Complete_Plotting_Summary_actual.xlsx"), overwrite = TRUE)
  cat("保存完整绘图汇总表，共", nrow(plot_summary), "个关系\n")
}

# ==========================================================
# 输出统计信息
# ==========================================================

cat("\n========================================\n")
cat("绘图完成！\n")
cat("输出目录:", plot_dir, "\n")
cat("\n统计摘要:\n")
cat("  总响应变量数:", nrow(selected_models), "\n")
cat("  成功构建模型数:", length(best_models), "\n")
cat("  绘制的关系总数:", nrow(actual_r2_stats), "\n")
cat("\n实际值R²统计:\n")
if(nrow(actual_r2_stats) > 0) {
  cat("  平均R²:", round(mean(actual_r2_stats$Actual_R2, na.rm = TRUE), 3), "\n")
  cat("  最大R²:", round(max(actual_r2_stats$Actual_R2, na.rm = TRUE), 3), "\n")
  cat("  最小R²:", round(min(actual_r2_stats$Actual_R2, na.rm = TRUE), 3), "\n")
}
cat("\n各分组统计:\n")
if(nrow(plot_summary) > 0) print(table(plot_summary$Group, useNA = "ifany"))
cat("\n显著性统计:\n")
if(nrow(plot_summary) > 0) print(table(plot_summary$Significance))
cat("\n========================================\n")