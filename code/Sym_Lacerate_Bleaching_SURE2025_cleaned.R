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
    day = factor(day, levels = c(0, 1, 5, 7, 9, 11, 13, 14)),
    treatment = factor(treatment, levels = c("Sym-Control", "Sym-HS"))
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
    values = c("Sym-Control" = "#3B6FB6", "Sym-HS" = "#E64B35"),
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
    size = 2,
    alpha = 0.7
  ) +
  
  geom_boxplot(
    aes(group = interaction(day, treatment)),
    position = position_dodge(width = 0.7),
    width = 0.5,
    fill = NA,
    linewidth = 1
  ) +
  
  scale_color_manual(values = c("Sym-Control" = "#3B6FB6", "Sym-HS" = "#E64B35")) +
  
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
welch_symdensity <- symdensity_stats %>%
  group_by(day) %>%
  do(
    tidy(t.test(calculation ~ treatment, data = ., var.equal = FALSE))
  ) %>%
  ungroup() %>%
  mutate(
    p_holm = p.adjust(p.value, method = "holm")
  )

print(welch_symdensity)

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
ggplot(symdensity_stats, aes(sample = calculation)) +
  stat_qq() +
  stat_qq_line() +
  facet_grid(treatment ~ day) +
  theme_bw()

# Variance check by day
welch_variance_checks <- symdensity_stats %>%
  group_by(day) %>%
  levene_test(calculation ~ treatment)

welch_variance_checks

## =================================
## 9. Build custom letters from Tukey
## Force Sym-Control = a, Sym-HS = b when significant
## =================================
tukey_df <- as.data.frame(tukey_symdensity)

letter_df <- tukey_df %>%
  mutate(
    group_control = "a",
    group_hs = ifelse(p.value < 0.05, "b", "a")
  ) %>%
  select(day, group_control, group_hs)

letter_df_long <- letter_df %>%
  pivot_longer(
    cols = c(group_control, group_hs),
    names_to = "group",
    values_to = ".group"
  ) %>%
  mutate(
    treatment = ifelse(group == "group_control", "Sym-Control", "Sym-HS")
  ) %>%
  select(day, treatment, .group)

letter_df_long

## =================================
## 10. Build positions for letters
## =================================
letter_positions <- symdensity_stats %>%
  group_by(day, treatment) %>%
  summarise(
    y_pos = max(calculation, na.rm = TRUE) + 2.9,
    .groups = "drop"
  ) %>%
  left_join(letter_df_long, by = c("day", "treatment"))

letter_positions

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
    size = 2,
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
    data = letter_positions,
    aes(x = day, y = y_pos, label = .group, group = treatment),
    position = position_dodge(width = 0.7),
    inherit.aes = FALSE,
    size = 4.5,
    fontface = "bold",
    family = "sans"
  ) +
  scale_color_manual(values = c("Sym-Control" = "#3B6FB6", "Sym-HS" = "#E64B35")) +
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
    legend.position = c(0.02, 0.98),   # x, y inside plot (0–1 scale)
    legend.justification = c(0, 1),    # anchor top-left corner
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
  path = "~/Documents/GitHub/heatinglacerate/figs/",
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
  path = "~/Documents/GitHub/heatinglacerate/figs/",
  device = pdf,
  width = 7,
  height = 5,
  units = "in",
  bg = "white"
)


## Make PANEL IMAGE ##
library(tidyverse)
library(tibble)
library(ggplot2)
library(dplyr)
library(magick)
library(grid)

#Files too large to upload to GitHub

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
    plot.margin = margin(t = 5, r = 5, b = 0, l = 5),   # remove extra bottom padding
    axis.title.x = element_text(margin = margin(t = 1)) # pull label closer
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
  path = "~/Documents/GitHub/heatinglacerate/figs/",
  device = "png",
#  width = 7.2,
#  height = 4.2,
   width = 10,
  height = 5.75,
  units = "in",
  dpi = 600,
##  compression = "lzw",
  bg = "white"
)

ggsave(
  filename = "SymDensity_Combined_Figure.pdf",
  plot = final_fig,
  path = "~/Documents/GitHub/heatinglacerate/figs/",
  device = pdf,
#  width = 7.2,
#  height = 4.2,
   width = 15,
   height = 7.5,
  units = "in",
  bg = "white"
)

## Supplemental tables 6 and 7 

## Convert ANOVA to dataframe
anova_df <- as.data.frame(anova_symdensity)

anova_df$Source <- rownames(anova_df)
rownames(anova_df) <- NULL

## Clean column names
anova_df <- anova_df %>%
  rename(
    Sum_Sq = `Sum Sq`,
    df = Df,
    F_value = `F value`,
    p_value = `Pr(>F)`
  ) %>%
  select(Source, Sum_Sq, df, F_value, p_value)

## Optional: format p-values nicely
anova_df$p_value <- format.pval(anova_df$p_value, digits = 4, eps = 1e-4)

## Write CSV
write.csv(anova_df,
          "~/Documents/GitHub/heatinglacerate/tables/Table_S6_ANOVA.csv",
          row.names = FALSE)


## Convert Tukey to dataframe
tukey_df <- as.data.frame(tukey_symdensity)

## Clean + reorder
tukey_df <- tukey_df %>%
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
write.csv(tukey_df,
          "~/Documents/GitHub/heatinglacerate/tables/Table_S7_Tukey.csv",
          row.names = FALSE)


tukey_df <- tukey_df %>%
  mutate(
    Estimate = round(Estimate, 2),
    SE = round(SE, 2),
    t_ratio = round(t_ratio, 3)
  )

anova_df <- anova_df %>%
  mutate(
    Sum_Sq = round(Sum_Sq, 1),
    F_value = round(F_value, 2)
  )






library(tidyverse)
library(car)
library(emmeans)
library(janitor)

# Read data
inoc_raw <- read.csv(
  "data/Lacerate-Inoc-Sym-Density_new.csv",
  check.names = FALSE
)

bad_names <- is.na(names(inoc_raw)) | names(inoc_raw) == ""
if (any(bad_names)) {
  names(inoc_raw)[bad_names] <- paste0("V", seq_len(sum(bad_names)))
}

# Clean data
inoc_filtered <- inoc_raw %>%
  filter(Type == "Symbiont Area") %>%
  filter(is.na(notes) | notes == "") %>%   # 🔥 remove rows with notes
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
# Average within well
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

# ANOVA
mod_inoc <- lm(calculation ~ treatment * day, data = inoc_wellmeans)
anova_inoc <- Anova(mod_inoc, type = "II")
print(anova_inoc)

# Tukey comparisons within each day
emm_inoc <- emmeans(mod_inoc, ~ treatment | day)
tukey_inoc <- summary(pairs(emm_inoc, adjust = "tukey"))
print(tukey_inoc)

# Build simple a/b letters
tukey_df <- as.data.frame(tukey_inoc)

letter_df <- tukey_df %>%
  mutate(
    group_25 = "a",
    group_32 = ifelse(p.value < 0.05, "b", "a")
  ) %>%
  select(day, group_25, group_32)

letter_df_long <- letter_df %>%
  pivot_longer(
    cols = c(group_25, group_32),
    names_to = "group",
    values_to = ".group"
  ) %>%
  mutate(
    treatment = ifelse(group == "group_25", "inoc-25C", "inoc-32C")
  ) %>%
  select(day, treatment, .group)

letter_positions <- inoc_wellmeans %>%
  group_by(day, treatment) %>%
  summarise(
    y_pos = max(calculation, na.rm = TRUE) * 1.12,
    .groups = "drop"
  ) %>%
  left_join(letter_df_long, by = c("day", "treatment"))

# Plot
p_inoc_symdensity <- ggplot(
  inoc_wellmeans,
  aes(x = day, y = calculation, color = treatment)
) +
  geom_jitter(
    position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.7),
    size = 2,
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
    data = letter_positions,
    aes(x = day, y = y_pos, label = .group, group = treatment),
    position = position_dodge(width = 0.7),
    inherit.aes = FALSE,
    size = 4.5,
    fontface = "bold"
  ) +
  scale_color_manual(
    values = c("inoc-25C" = "#3B88C3", "inoc-32C" = "#D95F02"),
    labels = c("Inoc-25C", "Inoc-32C")
  ) +
  labs(
    x = "Days post laceration",
    y = "Symbiont density (%)"
  ) +
  theme_bw(base_size = 12) +
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


library(patchwork)

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

library(tidyverse)

# Make sure treatment names are standardized
sym_plot_dat <- symdensity_wellmeans %>%
  mutate(
    treatment = factor(treatment, levels = c("Sym-Control", "Sym-HS"))
  )

inoc_plot_dat <- inoc_wellmeans %>%
  mutate(
    treatment = factor(treatment, levels = c("inoc-25C", "inoc-32C"))
  )

# Combine both datasets
all_symdensity_dat <- bind_rows(sym_plot_dat, inoc_plot_dat) %>%
  mutate(
    day = as.numeric(as.character(day)),
    day = factor(day, levels = c(0, 1, 5, 7, 10, 12, 14, 16, 18, 20, 21)),
    treatment = factor(
      treatment,
      levels = c("Sym-Control", "Sym-HS", "inoc-25C", "inoc-32C")
    )
  )

# Plot all 4 treatments together
p_all_symdensity <- ggplot(
  all_symdensity_dat,
  aes(x = day, y = calculation, color = treatment)
) +
  geom_jitter(
    position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.75),
    size = 2,
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
      "inoc-25C" = "#3B88C3",
      "inoc-32C" = "#D95F02"
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
    y = "Symbiont density (%)"
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


library(patchwork)
# =========================================================
# MATCH SYM + INOC PANEL LEGENDS AND Y LIMITS
# =========================================================
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

# =========================================================
# SYM PANEL
# =========================================================
p_symdensity_final <- p_symdensity_final +
  coord_cartesian(ylim = c(0, 60)) +
  common_legend_theme +
  common_legend_guides +
  theme(
    plot.margin = margin(t = 5, r = 5, b = 2, l = 5),
    axis.title.x = element_text(margin = margin(t = 1))
  )

# =========================================================
# INOC PANEL
# =========================================================
p_inoc_symdensity <- p_inoc_symdensity +
  coord_cartesian(ylim = c(0, 60)) +
  common_legend_theme +
  common_legend_guides +
  theme(
    plot.margin = margin(t = 2, r = 5, b = 0, l = 5),
    axis.title.x = element_text(margin = margin(t = 1))
  )

# =========================================================
# FINAL FIGURE: IMAGE PANEL + SYM + INOC
# =========================================================
final_fig_inoc_sym <- image_panel +
  (p_symdensity_final / p_inoc_symdensity) +
  plot_layout(widths = c(0.9, 1.7)) +
  plot_annotation(
    tag_levels = "A",
    theme = theme(
      text = element_text(family = "sans"),
      plot.tag = element_text(size = 16, face = "bold"),
      plot.tag.position = c(0.01, 0.99)
    )
  )

# show
final_fig_inoc_sym


# =========================================================
# LEFT SIDE = STACKED PLOTS
# RIGHT SIDE = IMAGE PANEL
# =========================================================

left_panels <- p_symdensity_final / p_inoc_symdensity +
  plot_layout(heights = c(1, 1))

# =========================================================
# FINAL FIGURE
# =========================================================
final_fig_inoc_sym <- wrap_plots(
  left_panels,
  image_panel,
  ncol = 2,
  widths = c(1.7, 1.1)
) +
  plot_annotation(
    tag_levels = "A",
    theme = theme(
      text = element_text(family = "sans"),
      plot.tag = element_text(size = 16, face = "bold"),
      plot.tag.position = c(0.01, 0.99)
    )
  )

# show
final_fig_inoc_sym



# =========================================================
# PANEL C WITH LARGE IMAGES AND FIXED LABEL SPACING
# Run this whole chunk at once
# =========================================================

library(tidyverse)
library(tibble)
library(ggplot2)
library(magick)
library(grid)
library(patchwork)

img_meta_c8 <- tribble(
  ~row_lab,      ~col_lab, ~file,
  
  "Inoc\n25°C", "BF", "~/Documents/GitHub/heatinglacerate/data/panel_inoc/I25_dpl14_bf.png",
  "Inoc\n25°C", "FL", "~/Documents/GitHub/heatinglacerate/data/panel_inoc/I25_dpl14_fl.png",
  
  "Inoc\n32°C", "BF", "~/Documents/GitHub/heatinglacerate/data/panel_inoc/I32_dpl14_bf_Snap-995_crop.png",
  "Inoc\n32°C", "FL", "~/Documents/GitHub/heatinglacerate/data/panel_inoc/I32_dpl14_fl_Snap-994_crop.png",
  
  "Sym\n25°C",  "BF", "~/Documents/GitHub/heatinglacerate/data/panel_inoc/S25_dpl14_bf.png",
  "Sym\n25°C",  "FL", "~/Documents/GitHub/heatinglacerate/data/panel_inoc/S25_dpl14_fl.png",
  
  "Sym\n32°C",  "BF", "~/Documents/GitHub/heatinglacerate/data/panel_inoc/S32_dpl14_bf.png",
  "Sym\n32°C",  "FL", "~/Documents/GitHub/heatinglacerate/data/panel_inoc/S32_dpl14_fl.png"
)

img_meta_c8

make_image_panel_c8 <- function(img_meta_c8) {
  
  row_levels <- unique(img_meta_c8$row_lab)
  col_levels <- unique(img_meta_c8$col_lab)
  
  img_meta_c8 <- img_meta_c8 %>%
    mutate(
      row_lab = factor(row_lab, levels = rev(row_levels)),
      col_lab = factor(col_lab, levels = col_levels),
      row_num = as.numeric(row_lab),
      col_num = as.numeric(col_lab)
    )
  
  image_size <- 0.98
  col_gap <- 1.04
  row_gap <- 1.12
  
  img_meta_c8 <- img_meta_c8 %>%
    mutate(
      x_center = col_num * col_gap + 0.22,
      y_center = row_num * row_gap
    )
  
  p <- ggplot() +
    xlim(0.55, 2.88) +
    ylim(0.35, max(img_meta_c8$y_center) + 0.65) +
    theme_void(base_family = "sans")
  
  for (i in seq_len(nrow(img_meta_c8))) {
    if (file.exists(img_meta_c8$file[i])) {
      img <- image_read(img_meta_c8$file[i])
      grob <- rasterGrob(as.raster(img), interpolate = TRUE)
      
      p <- p + annotation_custom(
        grob,
        xmin = img_meta_c8$x_center[i] - image_size / 2,
        xmax = img_meta_c8$x_center[i] + image_size / 2,
        ymin = img_meta_c8$y_center[i] - image_size / 2,
        ymax = img_meta_c8$y_center[i] + image_size / 2
      )
    }
  }
  
  row_df <- img_meta_c8 %>%
    distinct(row_lab, y_center) %>%
    mutate(lab = as.character(row_lab))
  
  p +
    geom_text(
      data = row_df,
      aes(x = 0.80, y = y_center, label = lab),
      hjust = 1,
      size = 3.8,
      fontface = "bold",
      family = "sans"
    ) +
    annotate(
      "text",
      x = 1.24,
      y = 0.35,
      label = "BF",
      size = 3.8,
      fontface = "bold",
      family = "sans"
    ) +
    annotate(
      "text",
      x = 2.28,
      y = 0.35,
      label = "FL",
      size = 3.8,
      fontface = "bold",
      family = "sans"
    )
}

image_panel_c8 <- make_image_panel_c8(img_meta_c8)

matched_legend_theme <- theme(
  legend.position = c(0.03, 0.97),
  legend.justification = c(0, 1),
  legend.background = element_blank(),
  legend.key = element_blank(),
  legend.title = element_blank(),
  legend.text = element_text(size = 11),
  legend.margin = margin(0, 0, 0, 0),
  legend.box.margin = margin(0, 0, 0, 0)
)

p_symdensity_final_legendmatched <- p_symdensity_final +
  matched_legend_theme

p_inoc_symdensity_legendmatched <- p_inoc_symdensity +
  matched_legend_theme

left_panels <- p_symdensity_final_legendmatched / p_inoc_symdensity_legendmatched +
  plot_layout(heights = c(1, 1))

final_fig_inoc_sym_image_panel_c8 <- wrap_plots(
  left_panels,
  image_panel_c8,
  ncol = 2,
  widths = c(1.60, 1.00)
) +
  plot_annotation(
    tag_levels = "A",
    theme = theme(
      text = element_text(family = "sans"),
      plot.tag = element_text(size = 16, face = "bold"),
      plot.tag.position = c(0.01, 0.99)
    )
  )

final_fig_inoc_sym_image_panel_c8


ggsave(
  filename = "SymDensity_Combined_Figure_INOC_FINAL.png",
  plot = final_fig_inoc_sym_image_panel_c8,
  path = "~/Documents/GitHub/heatinglacerate/figs/",
  device = "png",
  #  width = 7.2,
  #  height = 4.2,
  width = 10,
  height = 6.75,
  units = "in",
  dpi = 600,
  ##  compression = "lzw",
  bg = "white"
)

ggsave(
  filename = "SymDensity_Combined_Figure_INOC_FINAL.pdf",
  plot = final_fig_inoc_sym_image_panel_c8,
  path = "~/Documents/GitHub/heatinglacerate/figs/",
  device = pdf,
  #  width = 7.2,
  #  height = 4.2,
  width = 15,
  height = 6.5,
  units = "in",
  bg = "white"
)
