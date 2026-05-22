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
library(patchwork)
library(multcomp)
library(multcompView)
library(broom)
library(rstatix)
library(janitor)
library(tibble)
library(magick)
library(grid)

rm(list = ls())
graphics.off()

# ============================================================
# Set working directory
# ============================================================

getwd()
setwd("/Users/junbc/Documents/GitHub/heating_lacerates_final/")


# ============================================================
# 1) Symbiotic lacerate symbiont density
# ============================================================

symdensity_raw <- read.csv(
  "data/LacerateSymDensity.csv",
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


# ============================================================
# 2) Symbiotic lacerate statistics
# ============================================================

symdensity_stats <- symdensity_wellmeans %>%
  mutate(
    day = factor(day, levels = c(0, 1, 5, 7, 9, 11, 13, 14)),
    treatment = factor(treatment, levels = c("Sym-Control", "Sym-HS"))
  )

mod_aov <- lm(
  calculation ~ treatment * day,
  data = symdensity_stats
)

# ANOVA
anova_symdensity <- Anova(mod_aov, type = "II")
print(anova_symdensity)

# Convert to dataframe and save
anova_symdensity_df <- as.data.frame(anova_symdensity) %>%
  tibble::rownames_to_column("Factor")

write.csv(
  anova_symdensity_df,
  file = file.path(
    "~/Documents/GitHub/heating_lacerates_final/tables",
    "TableS4_anova_symdensity.csv"
  ),
  row.names = FALSE
)

# Tukey
emm_symdensity <- emmeans(mod_aov, ~ treatment | day)
tukey_symdensity <- summary(pairs(emm_symdensity, adjust = "tukey"))
print(tukey_symdensity)

# Convert to dataframe and save
tukey_symdensity_df <- as.data.frame(tukey_symdensity)

write.csv(
  tukey_symdensity_df,
  file = file.path(
    "~/Documents/GitHub/heating_lacerates_final/tables",
    "TableS5_tukey_symdensity.csv"
  ),
  row.names = FALSE
)

# ============================================================
# 3) Symbiotic lacerate Tukey letters
# ============================================================

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

letter_positions <- symdensity_stats %>%
  group_by(day, treatment) %>%
  summarise(
    y_pos = max(calculation, na.rm = TRUE) + 2.9,
    .groups = "drop"
  ) %>%
  left_join(letter_df_long, by = c("day", "treatment"))


# ============================================================
# 4) Final symbiotic lacerate plot
# ============================================================

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
  scale_color_manual(
    values = c("Sym-Control" = "#3B6FB6", "Sym-HS" = "#E64B35")
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

p_symdensity_final

# ============================================================
# 6) Inoculated lacerate symbiont density
# ============================================================

inoc_raw <- read.csv(
  "data/Lacerate-Inoc-Sym-Density.csv",
  check.names = FALSE
)

bad_names <- is.na(names(inoc_raw)) | names(inoc_raw) == ""
if (any(bad_names)) {
  names(inoc_raw)[bad_names] <- paste0("V", seq_len(sum(bad_names)))
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


# ============================================================
# 7) Inoculated lacerate statistics
# ============================================================

mod_inoc <- lm(
  calculation ~ treatment * day,
  data = inoc_wellmeans
)

#ANOVA
anova_inoc <- Anova(mod_inoc, type = "II")
print(anova_inoc)

anova_inoc_df <- as.data.frame(anova_inoc) %>%
  tibble::rownames_to_column("Factor")

write.csv(
  anova_inoc_df,
  file = file.path(
    "~/Documents/GitHub/heating_lacerates_final/tables",
    "TableS6_anova_inoc.csv"
  ),
  row.names = FALSE
)

#Tukey
emm_inoc <- emmeans(mod_inoc, ~ treatment | day)
tukey_inoc <- summary(pairs(emm_inoc, adjust = "tukey"))
print(tukey_inoc)

tukey_inoc_df <- as.data.frame(tukey_inoc)

write.csv(
  tukey_inoc_df,
  file = file.path(
    "~/Documents/GitHub/heating_lacerates_final/tables",
    "TableS7_tukey_inoc.csv"
  ),
  row.names = FALSE
)

# ============================================================
# 8) Inoculated lacerate Tukey letters
# ============================================================

tukey_inoc_df <- as.data.frame(tukey_inoc)

letter_df <- tukey_inoc_df %>%
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


# ============================================================
# 9) Final inoculated lacerate plot
# ============================================================

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


# ============================================================
# 10) Keep img_meta_c8 only
# ============================================================

img_meta_c8 <- tribble(
  ~row_lab,      ~col_lab, ~file,
  
  "Sym\n25°C",  "BF", "~/Documents/Github/heating_lacerates_final/images/panel_inoc/S25_dpl14_bf.png",
  "Sym\n25°C",  "FL", "~/Documents/Github/heating_lacerates_final/images/panel_inoc/S25_dpl14_fl.png",
  
  "Sym\n32°C",  "BF", "~/Documents/Github/heating_lacerates_final/images/panel_inoc/S32_dpl14_bf.png",
  "Sym\n32°C",  "FL", "~/Documents/Github/heating_lacerates_final/images/panel_inoc/S32_dpl14_fl.png",
  
  "Inoc\n25°C", "BF", "~/Documents/Github/heating_lacerates_final/images/panel_inoc/I25_dpl14_bf.png",
  "Inoc\n25°C", "FL", "~/Documents/Github/heating_lacerates_final/images/panel_inoc/I25_dpl14_fl.png",
  
  "Inoc\n32°C", "BF", "~/Documents/Github/heating_lacerates_final/images/panel_inoc/I32_dpl14_bf_Snap-995_crop.png",
  "Inoc\n32°C", "FL", "~/Documents/Github/heating_lacerates_final/images/panel_inoc/I32_dpl14_fl_Snap-994_crop.png"
)

img_meta_c8


# ============================================================
# 11) Make image panel but do not use it
# ============================================================

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


# ============================================================
# 12) Match plot legends and y limits
# ============================================================

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

p_symdensity_final <- p_symdensity_final +
  coord_cartesian(ylim = c(0, 60)) +
  matched_legend_theme +
  theme(
    plot.margin = margin(t = 5, r = 5, b = 2, l = 5),
    axis.title.x = element_text(margin = margin(t = 1))
  )

p_inoc_symdensity <- p_inoc_symdensity +
  coord_cartesian(ylim = c(0, 60)) +
  matched_legend_theme +
  theme(
    plot.margin = margin(t = 2, r = 5, b = 0, l = 5),
    axis.title.x = element_text(margin = margin(t = 1))
  )


# ============================================================
# 13) Final figure: stacked plots + image panel
# ============================================================

left_panels <- p_symdensity_final / p_inoc_symdensity +
  plot_layout(heights = c(1, 1))

final_fig4 <- wrap_plots(
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

final_fig4

# ============================================================
# 14) Save final figure
# ============================================================

ggsave(
  filename = "Fig4.png",
  plot = final_fig4,
  path = "~/Documents/Github/heating_lacerates_final/figs/",
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
  filename = "Fig4.pdf",
  plot = final_fig4,
  path = "~/Documents/Github/heating_lacerates_final/figs/",
  device = pdf,
  #  width = 7.2,
  #  height = 4.2,
  width = 15,
  height = 6.5,
  units = "in",
  bg = "white"
)
