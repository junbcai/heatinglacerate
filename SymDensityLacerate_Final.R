library(ggplot2)
library(ggpubr)
library(plotrix)
library(tidyverse)
library(dplyr)
library(car)
library(lme4)
library(emmeans)
library(qqplotr)
library(here)

rm(list = ls())
graphics.off()

#Set working directory

getwd()
setwd("/Users/junbc/Documents/GitHub/heatinglacerate/")

# Read symbiont density data
symdensity_raw <- read.csv(
  "data/Summer2025_SymDensity_new.csv",
  check.names = FALSE
)

# Fix blank or NA column names
bad_names <- is.na(names(symdensity_raw)) | names(symdensity_raw) == ""
if (any(bad_names)) {
  names(symdensity_raw)[bad_names] <- paste0("V", seq_len(sum(bad_names)))
}

# Filter to symbiont area measurements and remove excluded images
symdensity_filtered <- symdensity_raw %>%
  filter(
    Type == "Symbiont Area",
    day != "IGNORE",
    treatment != "IGNORE",
    Label != "Snap-1510.czi",
    Label != "Snap-1479.czi"
  ) %>%
  mutate(
    day = as.numeric(day),
    calculation = as.numeric(calculation)
  )

# Average across regions within each well
symdensity_wellmeans <- symdensity_filtered %>%
  group_by(day, treatment, well) %>%
  summarise(
    calculation = mean(calculation, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    day = factor(day, levels = c(0, 1, 5, 7, 9, 11, 13, 14))
  )

# Count wells per day x treatment and find plotting height for n labels
symdensity_n <- symdensity_wellmeans %>%
  group_by(day, treatment) %>%
  summarise(
    n = n(),
    y_max = max(calculation, na.rm = TRUE),
    .groups = "drop"
  )

# Plot
p_symdensity_v1 <- ggplot(
  symdensity_wellmeans,
  aes(x = day, y = calculation, fill = treatment)
) +
  geom_boxplot(
    position = position_dodge(width = 0.7),
    width = 0.6,
    outlier.shape = 21,
    color = "black"
  ) +
  geom_text(
    data = symdensity_n,
    aes(
      x = day,
      y = y_max * 1.05,
      label = paste0("n=", n),
      group = treatment
    ),
    position = position_dodge(width = 0.7),
    inherit.aes = FALSE,
    size = 3.5
  ) +
  scale_fill_manual(
    values = c("Sym-Control" = "blue", "Sym-HS" = "red"),
    labels = c("Sym-Control", "Sym-HS")
  ) +
  scale_x_discrete(
    name = "Days post laceration",
    limits = c("0", "1", "5", "7", "9", "11", "13", "14")
  ) +
  labs(
    x = NULL,
    y = "Symbiont density",
    title = "Symbiont density in pedal lacerates under control and heat stress conditions"
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    plot.title = element_text(size = 12, face = "bold")
  )

p_symdensity_v1


p_symdensity_v2 <- ggplot(symdensity_wellmeans,
                          aes(x = factor(day), y = calculation, color = treatment)) +
  
  geom_jitter(
    position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.7),
    size = 1.5,
    alpha = 0.7
  ) +
  
  geom_boxplot(
    aes(group = interaction(day, treatment)),
    position = position_dodge(width = 0.7),
    width = 0.5,
    fill = NA,
    linewidth = 1
  ) +
  
  scale_color_manual(values = c("Sym-Control" = "blue", "Sym-HS" = "red")) +
  
  labs(
    x = "Days post laceration",
    y = "Symbiont density"
  ) +
  
  theme_bw() +
  theme(panel.grid = element_blank())

p_symdensity_v2

library(dplyr)
library(ggplot2)
library(car)
library(emmeans)
library(multcomp)
library(multcompView)
library(broom)
library(rstatix)

## =================================
## Statistical analysis
## =================================

## =================================
## 1. Prepare data
## =================================
symdensity_stats <- symdensity_wellmeans %>%
  mutate(
    day = factor(day, levels = c(0, 1, 5, 7, 9, 11, 13, 14)),
    treatment = factor(treatment, levels = c("Sym-Control", "Sym-HS"))
  )

## =================================
## 2. Quick visual checks
## =================================

# Overall distribution
hist(symdensity_stats$calculation)
qqnorm(symdensity_stats$calculation)
qqline(symdensity_stats$calculation)

# Plot raw data distribution by group
p_symdensity_v2

## =================================
## 3. ANOVA framework
## =================================
mod_aov <- lm(
  calculation ~ treatment * day,
  data = symdensity_stats
)

## =================================
## 4. ANOVA model checks
## =================================

# Base residual plots
plot(mod_aov)

# Residual normality
qqnorm(residuals(mod_aov))
qqline(residuals(mod_aov))
shapiro.test(residuals(mod_aov))

# Homogeneity of variance
leveneTest(calculation ~ treatment * day, data = symdensity_stats)

## =================================
## 5. Type II ANOVA
## =================================
anova_symdensity <- Anova(mod_aov, type = "II")
print(anova_symdensity)

## =================================
## 6. Tukey post hoc
## Compare treatments within each day
## =================================
emm_symdensity <- emmeans(mod_aov, ~ treatment | day)
tukey_symdensity <- summary(pairs(emm_symdensity, adjust = "tukey"))
print(tukey_symdensity)

## =================================
## 7. Welch's t-tests by day
## =================================
#welch_symdensity <- symdensity_stats %>%
#  group_by(day) %>%
#  do(
#    tidy(t.test(calculation ~ treatment, data = ., var.equal = FALSE))
#  ) %>%
#  ungroup() %>%
#  mutate(
#    p_holm = p.adjust(p.value, method = "holm")
# )

#print(welch_symdensity)

## =================================
## 8. Light checks relevant to Welch's
## =================================

# Summary stats by group
symdensity_stats %>%
  group_by(day, treatment) %>%
  summarise(
    n = n(),
    mean = mean(calculation, na.rm = TRUE),
    sd = sd(calculation, na.rm = TRUE),
    .groups = "drop"
  )

# QQ plots by group
#ggplot(symdensity_stats, aes(sample = calculation)) +
#  stat_qq() +
#  stat_qq_line() +
#  facet_grid(treatment ~ day) +
#  theme_bw()

# Variance check by day
#welch_variance_checks <- symdensity_stats %>%
#  group_by(day) %>%
#  levene_test(calculation ~ treatment)

#welch_variance_checks

## =================================
## 9. Build custom letters from Tukey
## Force Sym-Control = a, Sym-HS = b when significant
## =================================
tukey_df_sym <- as.data.frame(tukey_symdensity)

letter_df_sym <- tukey_df_sym %>%
  mutate(
    group_control = "a",
    group_hs = ifelse(p.value < 0.05, "b", "a")
  ) %>%
  select(day, group_control, group_hs)

letter_df_long_sym <- letter_df_sym %>%
  pivot_longer(
    cols = c(group_control, group_hs),
    names_to = "group",
    values_to = ".group"
  ) %>%
  mutate(
    treatment = ifelse(group == "group_control", "Sym-Control", "Sym-HS")
  ) %>%
  select(day, treatment, .group)


## =================================
## 10. Build positions for letters
## Place both letters above the highest y in each day pair
## =================================
letter_positions_sym <- symdensity_stats %>%
  group_by(day) %>%
  summarise(
    y_pair_max = max(calculation, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(letter_df_long_sym, by = "day") %>%
  mutate(
    y_pos = case_when(
      .group == "a" ~ y_pair_max + 3.2,
      .group == "b" ~ y_pair_max + 1.5,
      TRUE ~ y_pair_max + 2.3
    ),
    treatment = factor(treatment, levels = c("Sym-Control", "Sym-HS"))
  )

letter_positions_sym


## =================================
## 11. Final plot with forced letters
## =================================
theme_set(
  theme_bw(base_size = 12, base_family = "sans")
)

p_symdensity_final <- ggplot(
  symdensity_stats,
  aes(x = day, y = calculation, color = treatment)
) +
  geom_jitter(
    position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.7),
    size = 1.5,
    alpha = 0.7
  ) +
  geom_boxplot(
    aes(group = interaction(day, treatment)),
    position = position_dodge(width = 0.7),
    width = 0.5,
    fill = NA,
    linewidth = 1
  ) +
  geom_text(
    data = letter_positions_sym,
    aes(x = day, y = y_pos, label = .group, group = treatment),
    position = position_dodge(width = 0.7),
    inherit.aes = FALSE,
    size = 4.5,
    fontface = "bold",
    family = "sans",
    color = "black",
    show.legend = FALSE
  ) +
  scale_color_manual(values = c("Sym-Control" = "blue", "Sym-HS" = "red")) +
  labs(
    x = "Days post laceration",
    y = "Symbiont density"
  ) +
  theme(
    panel.grid = element_blank(),
    legend.title = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11),
    legend.text = element_text(size = 11),
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.background = element_blank(),
    legend.key = element_blank()
  )

p_symdensity_final

## =================================
## 12. Save final figure
## =================================
ggsave(
  filename = "SymDensity_Figure.png",
  plot = p_symdensity_final,
  path = "figs",
  device = "png",
  width = 7,
  height = 5,
  units = "in",
  dpi = 600,
  #  compression = "lzw",
  bg = "white"
)

ggsave(
  filename = "SymDensity_Figure.pdf",
  plot = p_symdensity_final,
  path = "figs",
  device = pdf,
  width = 7,
  height = 5,
  units = "in",
  bg = "white"
)





## =================================
## 13. Make PANEL IMAGE
## =================================
library(tidyverse)
library(tibble)
library(ggplot2)
library(dplyr)
library(magick)
library(grid)
library(patchwork)

# Files too large to upload to GitHub

img_meta <- tribble(
  ~row_lab, ~col_lab, ~subcol, ~file,
  
  "Apo\n25°C", "Day 7",  1, "/Users/junbc/Pictures/Panel/A25_dpl7.png",
  "Apo\n25°C", "Day 14", 1, "/Users/junbc/Pictures/Panel/A25_dpl14.png",
  
  "Apo\n32°C", "Day 7",  1, "/Users/junbc/Pictures/Panel/A32_dpl7.png",
  "Apo\n32°C", "Day 14", 1, "/Users/junbc/Pictures/Panel/A32_dpl14.png",
  
  "Inoc\n25°C", "Day 7",  1, "/Users/junbc/Pictures/Panel/I25_dpl7_bf.png",
  "Inoc\n25°C", "Day 7",  2, "/Users/junbc/Pictures/Panel/I25_dpl7_fl.png",
  "Inoc\n25°C", "Day 14", 1, "/Users/junbc/Pictures/Panel/I25_dpl14_bf.png",
  "Inoc\n25°C", "Day 14", 2, "/Users/junbc/Pictures/Panel/I25_dpl14_fl.png",
  
  "Inoc\n32°C", "Day 7",  1, "/Users/junbc/Pictures/Panel/I32_dpl7_bf.png",
  "Inoc\n32°C", "Day 7",  2, "/Users/junbc/Pictures/Panel/I32_dpl7_fl.png",
  "Inoc\n32°C", "Day 14", 1, "/Users/junbc/Pictures/Panel/I32_dpl14_bf.png",
  "Inoc\n32°C", "Day 14", 2, "/Users/junbc/Pictures/Panel/I32_dpl14_fl.png",
  
  "Sym\n25°C", "Day 7",  1, "/Users/junbc/Pictures/Panel/S25_dpl7_bf.png",
  "Sym\n25°C", "Day 7",  2, "/Users/junbc/Pictures/Panel/S25_dpl7_fl.png",
  "Sym\n25°C", "Day 14", 1, "/Users/junbc/Pictures/Panel/S25_dpl14_bf.png",
  "Sym\n25°C", "Day 14", 2, "/Users/junbc/Pictures/Panel/S25_dpl14_fl.png",
  
  "Sym\n32°C", "Day 7",  1, "/Users/junbc/Pictures/Panel/S32_dpl7_bf.png",
  "Sym\n32°C", "Day 7",  2, "/Users/junbc/Pictures/Panel/S32_dpl7_fl.png",
  "Sym\n32°C", "Day 14", 1, "/Users/junbc/Pictures/Panel/S32_dpl14_bf.png",
  "Sym\n32°C", "Day 14", 2, "/Users/junbc/Pictures/Panel/S32_dpl14_fl.png"
)

make_image_panel <- function(img_meta) {
  
  row_levels <- unique(img_meta$row_lab)
  col_levels <- unique(img_meta$col_lab)
  
  img_meta <- img_meta %>%
    mutate(
      row_lab = factor(row_lab, levels = rev(row_levels)),
      col_lab = factor(col_lab, levels = col_levels),
      row_num = as.numeric(row_lab),
      col_num = case_when(
        col_lab == col_levels[1] & subcol == 1 ~ 1,
        col_lab == col_levels[1] & subcol == 2 ~ 2,
        col_lab == col_levels[2] & subcol == 1 ~ 3,
        col_lab == col_levels[2] & subcol == 2 ~ 4
      )
    )
  
  p <- ggplot() +
    xlim(0.0, 4.15) +
    ylim(0.35, length(row_levels) + 1.15) +
    theme_void(base_family = "sans")
  
  for (i in seq_len(nrow(img_meta))) {
    if (file.exists(img_meta$file[i])) {
      img <- image_read(img_meta$file[i])
      grob <- rasterGrob(as.raster(img), interpolate = TRUE)
      
      p <- p + annotation_custom(
        grob,
        xmin = img_meta$col_num[i] - 0.45,
        xmax = img_meta$col_num[i] + 0.45,
        ymin = img_meta$row_num[i] - 0.47,
        ymax = img_meta$row_num[i] + 0.37
      )
    }
  }
  
  row_df <- tibble(
    y = seq_along(rev(row_levels)),
    lab = rev(row_levels)
  )
  
  p +
    geom_text(
      data = row_df,
      aes(x = 0.3, y = y, label = lab),
      hjust = 1,
      size = 4,
      fontface = "bold",
      family = "sans"
    ) +
    annotate("text",
             x = 1.5,
             y = length(row_levels) + 0.95,
             label = col_levels[1],
             size = 5,
             family = "sans",
             fontface = "bold") +
    annotate("text",
             x = 3.5,
             y = length(row_levels) + 0.95,
             label = col_levels[2],
             size = 5,
             family = "sans",
             fontface = "bold") +
    annotate("text",
             x = 1,
             y = 0.35,
             label = "BF",
             size = 4,
             family = "sans",
             fontface = "bold") +
    annotate("text",
             x = 2,
             y = 0.35,
             label = "FL",
             size = 4,
             family = "sans",
             fontface = "bold") +
    annotate("text",
             x = 3,
             y = 0.35,
             label = "BF",
             size = 4,
             family = "sans",
             fontface = "bold") +
    annotate("text",
             x = 4,
             y = 0.35,
             label = "FL",
             size = 4,
             family = "sans",
             fontface = "bold")
}

image_panel <- make_image_panel(img_meta)

p_symdensity_final <- p_symdensity_final +
  theme(
    plot.margin = margin(t = 5, r = 5, b = 0, l = 5),
    axis.title.x = element_text(margin = margin(t = 1))
  )

final_fig <- image_panel + p_symdensity_final +
  plot_layout(widths = c(0.9, 1.7)) +
  plot_annotation(
    tag_levels = "A",
    theme = theme(
      text = element_text(family = "sans"),
      plot.tag = element_text(size = 16, face = "bold"),
      plot.tag.position = c(0.01, 0.99)
    )
  )

final_fig

ggsave(
  filename = "SymDensity_Combined_Figure.png",
  plot = final_fig,
  path = "figs",
  device = "png",
  width = 10,
  height = 5.75,
  units = "in",
  dpi = 600,
  bg = "white"
)

ggsave(
  filename = "SymDensity_Combined_Figure.pdf",
  plot = final_fig,
  path = "figs",
  device = pdf,
  width = 15,
  height = 7.5,
  units = "in",
  bg = "white"
)


## =================================
## 14. Supplemental tables 6 and 7 for sym dataset
## =================================

## Convert ANOVA to dataframe
anova_df_sym <- as.data.frame(anova_symdensity)

anova_df_sym$Source <- rownames(anova_df_sym)
rownames(anova_df_sym) <- NULL

## Clean column names
anova_df_sym <- anova_df_sym %>%
  rename(
    Sum_Sq = `Sum Sq`,
    df = Df,
    F_value = `F value`,
    p_value = `Pr(>F)`
  ) %>%
  select(Source, Sum_Sq, df, F_value, p_value)

## Optional: format p-values nicely
anova_df_sym$p_value <- format.pval(anova_df_sym$p_value, digits = 4, eps = 1e-4)

## Write CSV
write.csv(
  anova_df_sym,
  "~/Documents/GitHub/heatinglacerate/tables/Table_S6_ANOVA.csv",
  row.names = FALSE
)

## Convert Tukey to dataframe
tukey_table_sym <- as.data.frame(tukey_symdensity)

## Clean + reorder
tukey_table_sym <- tukey_table_sym %>%
  rename(
    Day = day,
    Contrast = contrast,
    Estimate = estimate,
    SE = SE,
    df = df,
    t_ratio = t.ratio,
    p_value = p.value
  ) %>%
  mutate(
    p_value = format.pval(p_value, digits = 4, eps = 1e-4)
  ) %>%
  select(Day, Contrast, Estimate, SE, df, t_ratio, p_value)

## Write CSV
write.csv(
  tukey_table_sym,
  "~/Documents/GitHub/heatinglacerate/tables/Table_S7_Tukey.csv",
  row.names = FALSE
)

tukey_table_sym <- tukey_table_sym %>%
  mutate(
    Estimate = round(Estimate, 2),
    SE = round(SE, 2),
    t_ratio = round(t_ratio, 3)
  )

anova_df_sym <- anova_df_sym %>%
  mutate(
    Sum_Sq = round(Sum_Sq, 1),
    F_value = round(F_value, 2)
  )


## =================================
## 15. Read and clean inoc symbiont density data
## =================================
library(tidyverse)
library(car)
library(emmeans)
library(janitor)

inoc_raw <- read.csv(
  "data/Lacerate-Inoc-Sym-Density_new.csv",
  check.names = FALSE
)

bad_names_inoc <- is.na(names(inoc_raw)) | names(inoc_raw) == ""
if (any(bad_names_inoc)) {
  names(inoc_raw)[bad_names_inoc] <- paste0("V", seq_len(sum(bad_names_inoc)))
}

inoc_filtered <- inoc_raw %>%
  filter(Type == "Symbiont Area") %>%
  filter(is.na(notes) | notes == "") %>%
  mutate(
    day = as.numeric(day),
    calculation = as.numeric(calculation),
    treatment = as.character(treatment)
  ) %>%
  filter(
    treatment %in% c("inoc-25C", "inoc-32C"),
    !is.na(day),
    !is.na(calculation)
  )


## =================================
## 16. Average inoc data within well
## =================================
inoc_wellmeans <- inoc_filtered %>%
  group_by(day, treatment, well) %>%
  summarise(
    calculation = mean(calculation, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    day = factor(day, levels = sort(unique(day))),
    treatment = factor(treatment, levels = c("inoc-25C", "inoc-32C"))
  )


## =================================
## 17. Inoc ANOVA and Tukey post hoc
## =================================
mod_aov_inoc <- lm(
  calculation ~ treatment * day,
  data = inoc_wellmeans
)

anova_inoc <- Anova(mod_aov_inoc, type = "II")
print(anova_inoc)

emm_inoc <- emmeans(mod_aov_inoc, ~ treatment | day)
tukey_inoc <- summary(pairs(emm_inoc, adjust = "tukey"))
print(tukey_inoc)


## =================================
## 18. Build inoc letters from Tukey
## =================================
tukey_df_inoc <- as.data.frame(tukey_inoc)

letter_df_inoc <- tukey_df_inoc %>%
  mutate(
    group_25 = "a",
    group_32 = ifelse(p.value < 0.05, "b", "a")
  ) %>%
  select(day, group_25, group_32)

letter_df_long_inoc <- letter_df_inoc %>%
  pivot_longer(
    cols = c(group_25, group_32),
    names_to = "group",
    values_to = ".group"
  ) %>%
  mutate(
    treatment = ifelse(group == "group_25", "inoc-25C", "inoc-32C")
  ) %>%
  select(day, treatment, .group)


## =================================
## 19. Build inoc letter positions
## Place both letters above the highest y in each day pair
## =================================
letter_positions_inoc <- inoc_wellmeans %>%
  group_by(day) %>%
  summarise(
    y_pair_max = max(calculation, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(letter_df_long_inoc, by = "day") %>%
  mutate(
    y_pos = case_when(
      .group == "a" ~ y_pair_max + 3.2,
      .group == "b" ~ y_pair_max + 1.5,
      TRUE          ~ y_pair_max + 2.3
    ),
    treatment = factor(treatment, levels = c("inoc-25C", "inoc-32C"))
  )

letter_positions_inoc

## =================================
## 20. Final inoc plot with forced letters
## =================================
theme_set(
  theme_bw(base_size = 12, base_family = "sans")
)

p_inoc_symdensity <- ggplot(
  inoc_wellmeans,
  aes(x = day, y = calculation, color = treatment)
) +
  geom_jitter(
    position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.7),
    size = 1.5,
    alpha = 0.7
  ) +
  geom_boxplot(
    aes(group = interaction(day, treatment)),
    position = position_dodge(width = 0.7),
    width = 0.5,
    fill = NA,
    linewidth = 1
  ) +
  geom_text(
    data = letter_positions_inoc,
    aes(x = day, y = y_pos, label = .group, group = treatment),
    position = position_dodge(width = 0.7),
    inherit.aes = FALSE,
    size = 4.5,
    fontface = "bold",
    family = "sans",
    color = "black",
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c("inoc-25C" = "#2C7FB8", "inoc-32C" = "#D55E00"),
    labels = c("Inoc-25C", "Inoc-32C")
  ) +
  labs(
    x = "Days post laceration",
    y = "Symbiont density"
  ) +
  theme(
    panel.grid = element_blank(),
    legend.title = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11),
    legend.text = element_text(size = 11),
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.background = element_blank(),
    legend.key = element_blank()
  )

p_inoc_symdensity


## =================================
## 21. Combined sym and inoc panels
## =================================
combined_inoc_sym_horizontal <- p_symdensity_final + p_inoc_symdensity +
  plot_layout(widths = c(1, 1)) +
  plot_annotation(
    tag_levels = "A",
    theme = theme(
      plot.tag = element_text(size = 16, face = "bold")
    )
  )

combined_inoc_sym_horizontal

combined_inoc_sym_vertical <- p_symdensity_final / p_inoc_symdensity +
  plot_layout(heights = c(1, 1)) +
  plot_annotation(
    tag_levels = "A",
    theme = theme(
      plot.tag = element_text(size = 16, face = "bold")
    )
  )

combined_inoc_sym_vertical


## =================================
## 22. Combined dataset plot across all treatments
## =================================
sym_plot_dat <- symdensity_wellmeans %>%
  mutate(
    treatment = factor(treatment, levels = c("Sym-Control", "Sym-HS"))
  )

inoc_plot_dat <- inoc_wellmeans %>%
  mutate(
    treatment = factor(treatment, levels = c("inoc-25C", "inoc-32C"))
  )

all_symdensity_dat <- bind_rows(sym_plot_dat, inoc_plot_dat) %>%
  mutate(
    day = as.numeric(as.character(day)),
    day = factor(day, levels = sort(unique(day))),
    treatment = factor(
      treatment,
      levels = c("Sym-Control", "Sym-HS", "inoc-25C", "inoc-32C")
    )
  )

p_all_symdensity <- ggplot(
  all_symdensity_dat,
  aes(x = day, y = calculation, color = treatment)
) +
  geom_jitter(
    position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.75),
    size = 1.5,
    alpha = 0.7
  ) +
  geom_boxplot(
    aes(group = interaction(day, treatment)),
    position = position_dodge(width = 0.75),
    width = 0.55,
    fill = NA,
    linewidth = 1
  ) +
  scale_color_manual(
    values = c(
      "Sym-Control" = "#1F4AE5",
      "Sym-HS" = "#FF3B1F",
      "inoc-25C" = "#2C7FB8",
      "inoc-32C" = "#D55E00"
    ),
    labels = c(
      "Sym-Control",
      "Sym-HS",
      "Inoc-25C",
      "Inoc-32C"
    )
  ) +
  labs(
    x = "Days post laceration",
    y = "Symbiont density"
  ) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    legend.title = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11),
    legend.text = element_text(size = 11)
  )

p_all_symdensity


## =================================
## 23. Match sym and inoc panel legends and y limits
## =================================
common_legend_theme <- theme(
  legend.position = c(0.03, 0.97),
  legend.justification = c(0, 1),
  legend.background = element_blank(),
  legend.key = element_blank(),
  legend.title = element_blank(),
  legend.text = element_text(size = 10),
  legend.key.width = unit(0.8, "cm"),
  legend.key.height = unit(0.5, "cm"),
  legend.spacing.y = unit(0.05, "cm"),
  legend.margin = margin(0, 0, 0, 0),
  legend.box.margin = margin(0, 0, 0, 0)
)

common_legend_guides <- guides(
  colour = guide_legend(
    ncol = 1,
    override.aes = list(size = 1.2)
  ),
  fill = guide_legend(
    ncol = 1,
    override.aes = list(size = 1.2)
  )
)

p_symdensity_final <- p_symdensity_final +
  coord_cartesian(ylim = c(0, 60)) +
  common_legend_theme +
  common_legend_guides +
  theme(
    plot.margin = margin(t = 5, r = 5, b = 2, l = 5),
    axis.title.x = element_text(margin = margin(t = 1))
  )

p_inoc_symdensity <- p_inoc_symdensity +
  coord_cartesian(ylim = c(0, 60)) +
  common_legend_theme +
  common_legend_guides +
  theme(
    plot.margin = margin(t = 2, r = 5, b = 0, l = 5),
    axis.title.x = element_text(margin = margin(t = 1))
  )


## =================================
## 24. Final figure with image panel plus sym and inoc panels
## =================================
library(patchwork)

design <- c(
  area(t = 1, l = 1, b = 1, r = 1),  # A
  area(t = 2, l = 1, b = 2, r = 1),  # B
  area(t = 1, l = 2, b = 2, r = 2)   # C
)

pA <- p_symdensity_final + labs(tag = "A")
pB <- p_inoc_symdensity + labs(tag = "B")
pC <- wrap_elements(full = image_panel) + labs(tag = "C")

final_fig_inoc_sym <- wrap_plots(
  A = pA,
  B = pB,
  C = pC,
  design = design
) +
  plot_layout(widths = c(1.45, 1.0), heights = c(1, 1)) &
  theme(
    text = element_text(family = "sans"),
    plot.tag = element_text(size = 16, face = "bold"),
    plot.tag.position = c(0.01, 0.99)
  )

final_fig_inoc_sym

ggsave(
  filename = "SymDensity_Combined_Figure_INOC.png",
  plot = final_fig_inoc_sym,
  path = "figs",
  device = "png",
  width = 10,
  height = 6.75,
  units = "in",
  dpi = 600,
  bg = "white"
)

ggsave(
  filename = "SymDensity_Combined_Figure_INOC.pdf",
  plot = final_fig_inoc_sym,
  path = "figs",
  device = pdf,
  width = 15,
  height = 6.5,
  units = "in",
  bg = "white"
)



