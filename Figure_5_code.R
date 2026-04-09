## 0. 环境准备 ==============================================================
rm(list = ls())
pacman::p_load(tidyverse, readxl, lubridate, patchwork, stringr, writexl, grDevices, grid, ggplot2)

## 1. 读数据 ================================================================
setwd("D:/地湖所工作/春季热浪文章相关工作/XMGS02_new2/Figure_dominance")
raw0 <- read_excel(
  "Cell_all.xlsx",
  col_types = c("skip","text","skip","skip","skip","numeric",
                "text","text","text","text")
)
colnames(raw0) <- c("Latin_species","Cell_ml","Date","Tank","Treatment","Latin_phylum")

## 2. 日期处理 ===============================================================
raw <- raw0 %>%
  mutate(Date = as.Date(Date)) %>%
  filter(Date >= "2024-04-13") %>%
  mutate(Day = as.numeric(Date - as.Date("2024-04-15")))

## 3. 清洗物种名、提取属名 ===================================================
raw <- raw %>%
  mutate(Latin_species = str_replace_all(Latin_species, "\\u00A0|\\u202F", " "),
         Genus = word(Latin_species, 1, sep = fixed(" ")))

## 4. 计算门、属、种 三级相对丰度 & 优势度 ================================
phylum_abundance <- raw %>%
  group_by(Day, Tank, Treatment) %>%
  mutate(N = sum(Cell_ml, na.rm = TRUE)) %>%
  group_by(Day, Tank, Treatment, Latin_phylum) %>%
  summarise(ni = sum(Cell_ml, na.rm = TRUE),
            N = first(N),
            .groups = "drop") %>%
  mutate(Density_Ratio = ni / N)

dominance_genus <- raw %>%
  group_by(Day, Tank, Treatment) %>%
  mutate(N = sum(Cell_ml, na.rm = TRUE)) %>%
  group_by(Day, Tank, Treatment, Genus) %>%
  summarise(ni = sum(Cell_ml, na.rm = TRUE),
            N = first(N),
            .groups = "drop") %>%
  mutate(fi = 1, Dominance = (ni / N) * fi)

dominance_species <- raw %>%
  group_by(Day, Tank, Treatment) %>%
  mutate(N = sum(Cell_ml, na.rm = TRUE)) %>%
  group_by(Day, Tank, Treatment, Latin_species) %>%
  summarise(ni = sum(Cell_ml, na.rm = TRUE),
            N = first(N),
            .groups = "drop") %>%
  mutate(fi = 1, Dominance = (ni / N) * fi)

## 5. Tank-Treatment 对应表 & 处理组分类 =====================================
# 定义四种分类
tank_treatment <- tibble(
  Tank = c(57, 63, 54, 59, 62, 67, 68, 70, 64, 65),
  Treatment = c("Control","Nutrients",
                "Mixing","Mixing",
                "Heatwave + Nutrients","Heatwave + Nutrients",
                "Mixing + Nutrients","Mixing + Nutrients",
                "Heatwave + Mixing + Nutrients",
                "Heatwave + Mixing + Nutrients")
)

# 定义处理组分类
treatment_classification <- tribble(
  ~Treatment, ~Category,
  "Control", "Non-heatwave + Non-mixing",
  "Nutrients", "Non-heatwave + Non-mixing",
  "Mixing", "Non-heatwave + Mixing",
  "Mixing + Nutrients", "Non-heatwave + Mixing",
  "Heatwave + Nutrients", "Heatwave + Non-mixing",
  "Heatwave + Mixing + Nutrients", "Heatwave + Mixing"
)

# 合并分类信息
tank_treatment <- tank_treatment %>%
  left_join(treatment_classification, by = "Treatment")

# 按照四种分类重新排序Tank
category_order <- c("Non-heatwave + Non-mixing", 
                    "Non-heatwave + Mixing",
                    "Heatwave + Non-mixing", 
                    "Heatwave + Mixing")

tank_treatment <- tank_treatment %>%
  mutate(
    Category = factor(Category, levels = category_order),
    Tank = as.numeric(Tank)
  ) %>%
  arrange(Category, Tank)

tank_order <- tank_treatment$Tank

# 获取每个分类下的处理组列表
category_groups <- tank_treatment %>%
  group_by(Category) %>%
  summarise(
    tanks = list(Tank),
    tank_count = n(),
    .groups = "drop"
  )

## 6. 定义门类颜色映射 ======================================================
phylum_colors <- c(
  "Cyanophyceae"        = "#3498db",
  "Chlorophyta"         = "#33A02C",
  "Cryptophyta"        = "#E31A1C",
  "Bacillariophyceae"   = "#9c640c"
)

## 7. 绘图函数（使用水平大括号，无任何X轴文本标签）==================
plot_heat <- function(df, y_var, title = "", threshold = 0.02, min_tank_count = 3){
  if(!"Dominance" %in% names(df)){
    stop("plot_heat(): 输入数据必须包含 'Dominance' 列。")
  }
  
  ## ---- 1. 筛物种 ----
  filter_sp <- df %>%
    group_by(!!sym(y_var)) %>%
    summarise(
      max_dom = max(Dominance, na.rm = TRUE),
      tank_n = n_distinct(Tank[Dominance > threshold], na.rm = TRUE)
    ) %>%
    filter(max_dom > threshold, tank_n >= min_tank_count) %>%
    pull(!!sym(y_var))
  
  df_filt <- df %>%
    filter(!!sym(y_var) %in% filter_sp)
  
  ## ---- 2. 保留 Day 0/2/4/6/8 ----
  plot_dat <- df_filt %>%
    filter(Day %in% c(0,2,4,6,8)) %>%
    filter(Dominance > threshold)
  
  plot_dat$Tank <- factor(as.character(plot_dat$Tank), levels = as.character(tank_order))
  
  plot_dat <- plot_dat %>%
    mutate(Tank_Day = paste0(Tank, "_", Day))
  
  tank_day_order <- expand_grid(Tank = tank_order, Day = c(0,2,4,6,8)) %>%
    mutate(Tank_Day = paste0(Tank, "_", Day)) %>%
    pull(Tank_Day)
  
  plot_dat$Tank_Day <- factor(plot_dat$Tank_Day, levels = tank_day_order)
  
  ## ---- 3. Phylum ----
  plot_dat <- plot_dat %>%
    left_join(distinct(raw, !!sym(y_var), Latin_phylum), by = y_var)
  
  y_order <- plot_dat %>%
    arrange(Latin_phylum, !!sym(y_var)) %>%
    pull(!!sym(y_var)) %>%
    unique()
  
  plot_dat[[y_var]] <- factor(plot_dat[[y_var]], levels = y_order)
  
  ## ---- 4. Phylum 分割线 ----
  phylum_order <- plot_dat %>%
    distinct(!!sym(y_var), Latin_phylum) %>%
    arrange(!!sym(y_var)) %>%
    mutate(row_num = row_number())
  
  phylum_splits <- phylum_order %>%
    group_by(Latin_phylum) %>%
    summarise(min_row = min(row_num),
              max_row = max(row_num),
              .groups = "drop") %>%
    mutate(label_pos = (min_row + max_row) / 2)
  
  ## ---- 5. Day 标签 ----
  tank_day_map <- expand_grid(Tank = tank_order, Day = c(0,2,4,6,8)) %>%
    mutate(Tank_Day = paste0(Tank, "_", Day))
  
  levels_vec <- levels(plot_dat$Tank_Day)
  tank_day_map <- tank_day_map %>%
    mutate(pos = match(Tank_Day, levels_vec))
  
  ## ---- 6. 计算线条位置（基于四种分类）----
  # 每个Tank有5个时间点
  tank_positions <- tibble(Tank = tank_order) %>%
    mutate(
      tank_index = match(Tank, tank_order),
      start_pos = (tank_index - 1) * 5 + 0.5,
      end_pos = tank_index * 5 + 0.5
    ) %>%
    left_join(tank_treatment, by = "Tank")
  
  # 计算虚线位置：同一分类的不同Tank之间
  dashed_positions <- c()
  for(i in 1:nrow(category_groups)) {
    group_tanks <- category_groups$tanks[[i]]
    if(length(group_tanks) > 1) {
      for(j in 1:(length(group_tanks)-1)) {
        tank1 <- group_tanks[j]
        pos1 <- tank_positions$end_pos[tank_positions$Tank == tank1]
        dashed_positions <- c(dashed_positions, pos1)
      }
    }
  }
  
  # 计算实线位置：不同分类之间
  solid_positions <- c()
  for(i in 1:(nrow(tank_positions)-1)) {
    current_category <- tank_positions$Category[i]
    next_category <- tank_positions$Category[i+1]
    if(current_category != next_category) {
      solid_positions <- c(solid_positions, tank_positions$end_pos[i])
    }
  }
  
  ## ---- 7. 计算每个分类组的起止位置（用于水平大括号）----
  category_positions <- tank_positions %>%
    group_by(Category) %>%
    summarise(
      start_pos = min(start_pos),
      end_pos = max(end_pos),
      .groups = "drop"
    )
  
  ## ---- 8. legend 最大值 ----
  max_dom <- max(plot_dat$Dominance, na.rm = TRUE)
  
  ## ---- 9. 计算Y轴标签颜色 ----
  phylum_color_map <- plot_dat %>%
    distinct(!!sym(y_var), Latin_phylum) %>%
    arrange(!!sym(y_var)) %>%
    mutate(
      color = case_when(
        Latin_phylum == "Cyanophyceae" ~ "#3498db",
        Latin_phylum == "Chlorophyta" ~ "#33A02C",
        Latin_phylum == "Cryptophyta" ~ "#E31A1C",
        Latin_phylum == "Bacillariophyceae" ~ "#9c640c",
        TRUE ~ "black"
      )
    )
  
  y_labels <- levels(plot_dat[[y_var]])
  y_colors <- phylum_color_map$color[match(y_labels, phylum_color_map[[y_var]])]
  
  ## ---- 10. 绘图 ----
  max_x <- length(unique(plot_dat$Tank_Day))
  y_max <- length(levels(plot_dat[[y_var]]))
  
  color_breaks <- seq(threshold, max_dom, length.out = 6)
  legend_height <- y_max * 0.5
  color_palette <- colorRampPalette(c("#8FBC8F", "#DC143C", "#414986"))
  
  p <- ggplot(plot_dat, aes(x = Tank_Day, y = !!sym(y_var), fill = Dominance)) +
    geom_tile(color = "white", linewidth = 0.3) +
    
    scale_fill_gradientn(
      colours = color_palette(10),
      limits = c(threshold, max_dom),
      breaks = color_breaks,
      labels = scales::number_format(accuracy = 0.01),
      name = "Y",
      guide = guide_colorbar(
        barwidth = unit(1.2, "cm"),
        barheight = unit(legend_height, "cm"),
        title.position = "top",
        title.hjust = 0.25,
        title.vjust = 0.5,
        title.theme = element_text(size = 16, margin = margin(b = 8)),
        label.theme = element_text(size = 16, face = "bold", margin = margin(l = 2, r = 2)),
        ticks.colour = "black",
        ticks.linewidth = 1,
        frame.colour = "black",
        frame.linewidth = 1,
        direction = "vertical"
      )
    ) +
    scale_x_discrete(expand = c(0,0)) +
    scale_y_discrete(expand = c(0,0)) +
    
    # Phylum 分割线
    geom_hline(data = phylum_splits[-nrow(phylum_splits),], 
               aes(yintercept = max_row + 0.5), 
               color = "black", linewidth = 0.8) +
    
    # 不同分类之间的竖线（实线）
    geom_vline(xintercept = solid_positions, colour = "black", linewidth = 0.8) +
    
    # 同一分类之间的竖线（虚线）
    geom_vline(xintercept = dashed_positions, colour = "black", linewidth = 0.6, linetype = "dashed") +
    
    # 外框
    annotate("rect", 
             xmin = 0.5, xmax = max_x + 0.5, 
             ymin = 0.5, ymax = y_max + 0.5, 
             colour = "black", fill = NA, linewidth = 1.2) +
    
    # Day 标签（在X轴上方）
    geom_text(data = tank_day_map,
              aes(x = pos, y = 0, label = Day),
              inherit.aes = FALSE, size = 6) +
    
    coord_fixed(ratio = 0.9, clip = "off", 
                xlim = c(0.5, max_x + 0.5), 
                ylim = c(0.5, y_max + 0.5)) +
    
    labs(title = title, x = NULL, y = NULL) +
    theme_minimal(base_size = 16) +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_text(face = "bold.italic", size = 16, colour = y_colors),
      axis.ticks.x = element_blank(),
      axis.line.x = element_blank(),
      panel.grid = element_blank(),
      plot.margin = margin(1, 3, 3, 1, "cm"),
    )
  
  # 添加水平下方括号和分类标签
  for(i in 1:nrow(category_positions)) {
    # 计算括号的位置
    x_start <- category_positions$start_pos[i]
    x_end <- category_positions$end_pos[i]
    y_pos <- -1.5  # 括号在Y轴下方的位置
    
    # 计算括号的高度和弧度
    bracket_height <- 0.4
    
    # 绘制左括号的弯钩（左侧向上弯曲）
    p <- p + annotate("curve",
                      x = x_start, xend = x_start - 0.25,
                      y = y_pos, yend = y_pos + bracket_height,
                      curvature = -0.5,
                      angle = 90,
                      colour = "black",
                      linewidth = 0.8)
    
    # 绘制右括号的弯钩（右侧向上弯曲）
    p <- p + annotate("curve",
                      x = x_end, xend = x_end + 0.25,
                      y = y_pos, yend = y_pos + bracket_height,
                      curvature = 0.5,
                      angle = 90,
                      colour = "black",
                      linewidth = 0.8)
    
    # 添加水平线连接左右括号
    p <- p + annotate("segment",
                      x = x_start, xend = x_end,
                      y = y_pos, yend = y_pos,
                      colour = "black",
                      linewidth = 0.8)
    
    # 添加分类标签（在括号下方）
    p <- p + annotate("text",
                      x = (x_start + x_end) / 2,
                      y = y_pos - 0.8,
                      label = category_positions$Category[i],
                      size = 4.5,
                      fontface = "bold")
  }
  
  return(list(plot = p, 
              plot_data = plot_dat,
              y_order = y_order,
              y_var = y_var))
}
## 8. 输出路径 ===============================================================
output_folder <- "Figure_dominance_260323"
if(!dir.exists(output_folder)) dir.create(output_folder)

## 9. 出图并保存 =============================================================
genus_result <- plot_heat(dominance_genus, "Genus")
species_result <- plot_heat(dominance_species, "Latin_species")

p_genus <- genus_result$plot
p_species <- species_result$plot

# 保存PDF格式
ggsave(file.path(output_folder, "heatmap_genus_fi1.pdf"), p_genus, 
       width = 18, height = 14)
ggsave(file.path(output_folder, "heatmap_species_fi1.pdf"), p_species, 
       width = 18, height = 14)

# 保存JPG格式
ggsave(file.path(output_folder, "heatmap_genus_fi1.jpg"), p_genus, 
       width = 18, height = 14, dpi = 300)
ggsave(file.path(output_folder, "heatmap_species_fi1.jpg"), p_species, 
       width = 18, height = 14, dpi = 300)

## 10. 组合图 ==========================================================
combined_plot <- p_genus / p_species + 
  plot_layout(heights = c(1, 1))

ggsave(file.path(output_folder, "combined_heatmaps.pdf"), combined_plot, 
       width = 18, height = 26)
ggsave(file.path(output_folder, "combined_heatmaps.jpg"), combined_plot, 
       width = 18, height = 26, dpi = 300)

## 11. 生成优势度数值宽表格 ===========================================
genus_order <- genus_result$y_order
species_order <- species_result$y_order

day_order <- c(0, 2, 4, 6, 8)

cat("\n=== 数据检查 ===\n")
cat("属水平物种数:", length(genus_order), "\n")
cat("种水平物种数:", length(species_order), "\n")
cat("Tank数量:", length(tank_order), "\n")
cat("\n=== Tank顺序（按分类排列）===\n")
print(tank_treatment %>% select(Tank, Category))
cat("\n=== 分类分组 ===\n")
for(i in 1:nrow(category_groups)) {
  cat(category_groups$Category[i], ": Tank", 
      paste(category_groups$tanks[[i]], collapse = ", "), "\n")
}

# 生成属水平的宽表格
genus_wide_table <- dominance_genus %>%
  filter(Genus %in% genus_order) %>%
  mutate(Genus = factor(Genus, levels = genus_order)) %>%
  filter(Day %in% day_order) %>%
  filter(Tank %in% tank_order) %>%
  mutate(Tank = factor(Tank, levels = tank_order)) %>%
  arrange(Tank, Day) %>%
  select(Tank, Treatment, Day, Genus, Dominance) %>%
  pivot_wider(
    names_from = c(Tank, Day),
    values_from = Dominance,
    names_sep = "_Day",
    values_fill = 0
  ) %>%
  arrange(Genus) %>%
  left_join(raw %>% distinct(Genus, Latin_phylum) %>% rename(Phylum = Latin_phylum), by = "Genus") %>%
  select(Phylum, Genus, everything())

# 生成种水平的宽表格
species_wide_table <- dominance_species %>%
  filter(Latin_species %in% species_order) %>%
  mutate(Latin_species = factor(Latin_species, levels = species_order)) %>%
  filter(Day %in% day_order) %>%
  filter(Tank %in% tank_order) %>%
  mutate(Tank = factor(Tank, levels = tank_order)) %>%
  arrange(Tank, Day) %>%
  select(Tank, Treatment, Day, Latin_species, Dominance) %>%
  pivot_wider(
    names_from = c(Tank, Day),
    values_from = Dominance,
    names_sep = "_Day",
    values_fill = 0
  ) %>%
  arrange(Latin_species) %>%
  left_join(raw %>% distinct(Latin_species, Latin_phylum, Genus) %>% rename(Phylum = Latin_phylum), by = "Latin_species") %>%
  select(Phylum, Genus, Latin_species, everything())

# 创建Tank-Day对应表
tank_day_info <- expand_grid(
  Tank = tank_order,
  Day = day_order
) %>%
  left_join(tank_treatment, by = "Tank") %>%
  mutate(Column_Name = paste0(Tank, "_Day", Day)) %>%
  select(Column_Name, Tank, Day, Treatment, Category)

# 保存Excel文件
dominance_tables <- list(
  Genus_Dominance = genus_wide_table,
  Species_Dominance = species_wide_table,
  Treatment_Classification = tank_treatment,
  Tank_Day_Columns = tank_day_info,
  Genus_Order_Info = data.frame(Order = 1:length(genus_order), Genus = genus_order),
  Species_Order_Info = data.frame(Order = 1:length(species_order), Species = species_order)
)

write_xlsx(dominance_tables, file.path(output_folder, "dominance_wide_tables_fi1.xlsx"))

# 保存CSV格式
write_csv(genus_wide_table, file.path(output_folder, "genus_dominance_wide_fi1.csv"))
write_csv(species_wide_table, file.path(output_folder, "species_dominance_wide_fi1.csv"))
write_csv(tank_day_info, file.path(output_folder, "tank_day_info_fi1.csv"))

# 输出用于回归分析的表格
phylum_wide <- phylum_abundance %>%
  mutate(Latin_phylum = str_c(Latin_phylum, "_Ratio")) %>%
  pivot_wider(names_from = Latin_phylum, values_from = Density_Ratio, values_fill = 0)

genus_wide_original <- dominance_genus %>%
  filter(Genus %in% genus_order) %>%
  mutate(Genus = str_c(Genus, "_Ratio")) %>%
  pivot_wider(names_from = Genus, values_from = Dominance, values_fill = 0)

species_wide_original <- dominance_species %>%
  filter(Latin_species %in% species_order) %>%
  mutate(Latin_species = str_c(Latin_species, "_Ratio")) %>%
  pivot_wider(names_from = Latin_species, values_from = Dominance, values_fill = 0)

regress_tab <- list(phylum_wide, genus_wide_original, species_wide_original) %>%
  reduce(left_join, by = c("Day","Tank","Treatment")) %>%
  mutate(across(where(is.numeric), ~replace_na(.,0)))

write_xlsx(regress_tab, file.path(output_folder, "ratio_cells_for_model_fi1.xlsx"))

cat("\n=== 文件保存信息 ===\n")
cat("宽表格已保存至:", output_folder, "\n")
cat("生成的文件:\n")
cat("1. dominance_wide_tables_fi1.xlsx - Excel格式宽表格\n")
cat("2. genus_dominance_wide_fi1.csv - CSV格式属水平表格\n")
cat("3. species_dominance_wide_fi1.csv - CSV格式种水平表格\n")
cat("4. tank_day_info_fi1.csv - Tank-Day对应信息\n")
cat("5. ratio_cells_for_model_fi1.xlsx - 用于回归分析的表格\n")
cat("6. heatmap_genus_fi1.pdf/jpg - 属水平热图\n")
cat("7. heatmap_species_fi1.pdf/jpg - 种水平热图\n")