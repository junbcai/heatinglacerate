# ============================================================
# Build 6 individual panels + assemble into a 2x3 figure
# Panels: (EdU, Caspase) x (Apo, Inoc, Sym)
# ============================================================

library(dplyr)
library(tidyverse)
library(stringr)
library(rstatix)
library(patchwork)

rm(list = ls())
graphics.off()

#Set working directory
getwd()
setwd("~/Documents/GitHub/heating_lacerates_final")

# ----------------------------
# Load and clean data
# ----------------------------
raw_confocal_data <- read_csv("data/Lacerate_EdU_Caspase.csv")

confocal_data <- raw_confocal_data %>%
  filter(True_Channel %in% c("EdU", "Caspase")) %>%
  mutate(
    marker = factor(True_Channel, levels = c("EdU", "Caspase")),
    time = factor(Time, levels = c("2dpl", "5dpl", "8dpl", "14dpl")),
    treatment = as.character(Treatment),
    state = case_when(
      str_detect(treatment, regex("Apo", ignore_case = TRUE)) ~ "Apo",
      str_detect(treatment, regex("Inoc", ignore_case = TRUE)) ~ "Inoc",
      str_detect(treatment, regex("Sym", ignore_case = TRUE)) ~ "Sym",
      TRUE ~ NA_character_
    ),
    heat = case_when(
      str_detect(treatment, regex("HS|Heat", ignore_case = TRUE)) ~ "Heat",
      str_detect(treatment, regex("Control", ignore_case = TRUE)) ~ "Control",
      TRUE ~ NA_character_
    ),
    state = factor(state, levels = c("Apo", "Inoc", "Sym")),
    heat = factor(heat, levels = c("Control", "Heat")),
    percentage = Percentage
  ) %>%
  drop_na(marker, state, heat, time, percentage)

# ----------------------------
# Check sample sizes
# ----------------------------
sample_size_table_wide <- confocal_data %>%
  count(marker, state, time, heat) %>%
  pivot_wider(names_from = heat, values_from = n)

print(sample_size_table_wide)


# n per panel and treatment for p6
p6_n <- confocal_data %>%
  count(marker, state, time, heat) %>%
  arrange(marker, state, time, heat)

print(p6_n, n = Inf)

# ----------------------------
# Shared y-limits
# ----------------------------
ymax_edu <- confocal_data %>%
  filter(marker == "EdU") %>%
  summarise(max_val = max(percentage, na.rm = TRUE)) %>%
  pull(max_val)

ymax_cas <- confocal_data %>%
  filter(marker == "Caspase") %>%
  summarise(max_val = max(percentage, na.rm = TRUE)) %>%
  pull(max_val)

# add headroom for letters
ymax_edu <- ymax_edu + 5
ymax_cas <- ymax_cas + 5

# ----------------------------
# Simple plotting function
# ----------------------------
make_panel <- function(data_sub, panel_title, ylab = NULL, show_legend = FALSE, y_limit = NULL) {
  
  fit <- lm(percentage ~ heat * time, data = data_sub)
  
  emm <- emmeans(fit, ~ heat | time)
  
  letters_df <- cld(emm, adjust = "tukey", Letters = letters) %>%
    as.data.frame() %>%
    mutate(.group = str_trim(.group))
  
  # get max per timepoint for positioning
  y_pos <- data_sub %>%
    group_by(time, heat) %>%
    summarise(y = max(percentage, na.rm = TRUE), .groups = "drop")
  
  letters_df <- left_join(letters_df, y_pos, by = c("time", "heat")) %>%
    mutate(y = y + 6.5)
  
  if (is.null(y_limit)) {
    y_max <- max(c(data_sub$percentage, letters_df$y), na.rm = TRUE) + 2
  } else {
    y_max <- y_limit
  }
  
  ggplot(data_sub, aes(x = time, y = percentage, color = heat)) +
    geom_jitter(
      position = position_jitterdodge(jitter.width = 0.12, dodge.width = 0.75),
      size = 2.2,
      alpha = 0.9
    ) +
    geom_boxplot(
      aes(group = interaction(time, heat)),
      outlier.shape = NA,
      width = 0.6,
      fill = NA,
      linewidth = 0.8,
      position = position_dodge(width = 0.75)
    ) +
    geom_text(
      data = letters_df,
      aes(x = time, y = y, label = .group, group = heat),
      position = position_dodge(width = 0.75),
      inherit.aes = FALSE,
      fontface = "bold",
      size = 4,
      color = "black"
    ) +
    scale_color_manual(values = c("Control" = "blue", "Heat" = "red")) +
    coord_cartesian(ylim = c(0, y_max), clip = "off") +
    labs(title = panel_title, x = NULL, y = ylab, color = NULL) +
    theme_classic(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      legend.position = if (show_legend) "top" else "none",
      plot.margin = margin(8, 8, 8, 8)
    )
}

# ----------------------------
# Make the 6 panels
# ----------------------------

# ---- EdU (shared scale)
p_edu_apo <- make_panel(
  filter(confocal_data, marker == "EdU", state == "Apo"),
  "EdU - Apo",
  "EdU-positive area (%)",
  y_limit = ymax_edu
)

p_edu_inoc <- make_panel(
  filter(confocal_data, marker == "EdU", state == "Inoc"),
  "EdU - Inoc",
  y_limit = ymax_edu
)

p_edu_sym <- make_panel(
  filter(confocal_data, marker == "EdU", state == "Sym"),
  "EdU - Sym",
  show_legend = TRUE,
  y_limit = ymax_edu
)

# ---- Caspase (shared scale)
p_cas_apo <- make_panel(
  filter(confocal_data, marker == "Caspase", state == "Apo"),
  "Caspase - Apo",
  "Caspase-positive area (%)",
  y_limit = ymax_cas
)

p_cas_inoc <- make_panel(
  filter(confocal_data, marker == "Caspase", state == "Inoc"),
  "Caspase - Inoc",
  y_limit = ymax_cas
)

p_cas_sym <- make_panel(
  filter(confocal_data, marker == "Caspase", state == "Sym"),
  "Caspase - Sym",
  show_legend = TRUE,
  y_limit = ymax_cas
)
# ----------------------------
# Assemble figures
# ----------------------------
p_edu3 <- (p_edu_apo | p_edu_inoc | p_edu_sym) +
  plot_layout(guides = "collect") &
  theme(legend.position = "top")

p_cas3 <- (p_cas_apo | p_cas_inoc | p_cas_sym) +
  plot_layout(guides = "collect") &
  theme(legend.position = "top")

p6 <- (p_edu_apo | p_edu_inoc | p_edu_sym) /
  (p_cas_apo | p_cas_inoc | p_cas_sym) +
  plot_layout(guides = "collect") &
  theme(legend.position = "top")

print(p6)

ggsave(
  filename = "Fig6_confocal.png",
  plot = p6,
  path = "figs",
  device = "png",
  width = 11,
  height = 8,
  units = "in",
  dpi = 600,
  bg = "white"
)

ggsave(
  filename = "Fig6_confocal.pdf",
  plot = p6,
  path = "figs",
  device = pdf,
  width = 11,
  height = 8,
  units = "in",
  bg = "white"
)

# ---- EdU (shared scale)
p_edu_apo <- make_panel(
  filter(confocal_data, marker == "EdU", state == "Apo"),
  "EdU - Apo",
  "EdU-positive area (%)",
  y_limit = ymax_edu
)

p_edu_inoc <- make_panel(
  filter(confocal_data, marker == "EdU", state == "Inoc"),
  "EdU - Inoc",
  y_limit = ymax_edu
)

p_edu_sym <- make_panel(
  filter(confocal_data, marker == "EdU", state == "Sym"),
  "EdU - Sym",
  show_legend = TRUE,
  y_limit = ymax_edu
)

# ---- Caspase (shared scale)
p_cas_apo <- make_panel(
  filter(confocal_data, marker == "Caspase", state == "Apo"),
  "Caspase - Apo",
  "Caspase-positive area (%)",
  y_limit = ymax_cas
)

p_cas_inoc <- make_panel(
  filter(confocal_data, marker == "Caspase", state == "Inoc"),
  "Caspase - Inoc",
  y_limit = ymax_cas
)

p_cas_sym <- make_panel(
  filter(confocal_data, marker == "Caspase", state == "Sym"),
  "Caspase - Sym",
  show_legend = TRUE,
  y_limit = ymax_cas
)

# ----------------------------
# Statistical analysis
# ----------------------------

# Global anova
fit_global <- lm(
  percentage ~ marker + state + heat * time + state * time,
  data = confocal_data
)

## Global model checks

# Base residual diagnostic plots
par(mfrow = c(2, 2))
plot(fit_global)
par(mfrow = c(1, 1))

# Residual normality
qqnorm(residuals(fit_global))
qqline(residuals(fit_global))
shapiro.test(residuals(fit_global))

# Homogeneity of variance
leveneTest(
  percentage ~ marker * state * heat * time,
  data = confocal_data
)

## Type II ANOVA

anova_global <- car::Anova(fit_global, type = 2)

anova_global_table <- as.data.frame(anova_global) %>%
  rownames_to_column("term") %>%
  mutate(
    across(where(is.numeric), ~ signif(.x, 3))
  )
anova_global_table

# ---------- EdU Apo ----------
edu_apo <- confocal_data %>%
  filter(marker == "EdU", state == "Apo") %>%
  droplevels()

fit_edu_apo <- lm(percentage ~ heat * time, data = edu_apo)
anova_edu_apo <- car::Anova(fit_edu_apo, type = 2)
emm_edu_apo <- emmeans(fit_edu_apo, ~ heat | time)
tukey_edu_apo <- pairs(emm_edu_apo, adjust = "tukey") %>%
  as.data.frame() %>%
  mutate(marker = "EdU", state = "Apo")
letters_edu_apo <- cld(emm_edu_apo, adjust = "tukey", Letters = letters) %>%
  as.data.frame() %>%
  mutate(.group = str_trim(.group), marker = "EdU", state = "Apo")

# ---------- EdU Inoc ----------
edu_inoc <- confocal_data %>%
  filter(marker == "EdU", state == "Inoc") %>%
  droplevels()

fit_edu_inoc <- lm(percentage ~ heat * time, data = edu_inoc)
anova_edu_inoc <- car::Anova(fit_edu_inoc, type = 2)
emm_edu_inoc <- emmeans(fit_edu_inoc, ~ heat | time)
tukey_edu_inoc <- pairs(emm_edu_inoc, adjust = "tukey") %>%
  as.data.frame() %>%
  mutate(marker = "EdU", state = "Inoc")
letters_edu_inoc <- cld(emm_edu_inoc, adjust = "tukey", Letters = letters) %>%
  as.data.frame() %>%
  mutate(.group = str_trim(.group), marker = "EdU", state = "Inoc")

# ---------- EdU Sym ----------
edu_sym <- confocal_data %>%
  filter(marker == "EdU", state == "Sym") %>%
  droplevels()

fit_edu_sym <- lm(percentage ~ heat * time, data = edu_sym)
anova_edu_sym <- car::Anova(fit_edu_sym, type = 2)
emm_edu_sym <- emmeans(fit_edu_sym, ~ heat | time)
tukey_edu_sym <- pairs(emm_edu_sym, adjust = "tukey") %>%
  as.data.frame() %>%
  mutate(marker = "EdU", state = "Sym")
letters_edu_sym <- cld(emm_edu_sym, adjust = "tukey", Letters = letters) %>%
  as.data.frame() %>%
  mutate(.group = str_trim(.group), marker = "EdU", state = "Sym")

# ---------- Caspase Apo ----------
cas_apo <- confocal_data %>%
  filter(marker == "Caspase", state == "Apo") %>%
  droplevels()

fit_cas_apo <- lm(percentage ~ heat * time, data = cas_apo)
anova_cas_apo <- car::Anova(fit_cas_apo, type = 2)
emm_cas_apo <- emmeans(fit_cas_apo, ~ heat | time)
tukey_cas_apo <- pairs(emm_cas_apo, adjust = "tukey") %>%
  as.data.frame() %>%
  mutate(marker = "Caspase", state = "Apo")
letters_cas_apo <- cld(emm_cas_apo, adjust = "tukey", Letters = letters) %>%
  as.data.frame() %>%
  mutate(.group = str_trim(.group), marker = "Caspase", state = "Apo")

# ---------- Caspase Inoc ----------
cas_inoc <- confocal_data %>%
  filter(marker == "Caspase", state == "Inoc") %>%
  droplevels()

fit_cas_inoc <- lm(percentage ~ heat * time, data = cas_inoc)
anova_cas_inoc <- car::Anova(fit_cas_inoc, type = 2)
emm_cas_inoc <- emmeans(fit_cas_inoc, ~ heat | time)
tukey_cas_inoc <- pairs(emm_cas_inoc, adjust = "tukey") %>%
  as.data.frame() %>%
  mutate(marker = "Caspase", state = "Inoc")
letters_cas_inoc <- cld(emm_cas_inoc, adjust = "tukey", Letters = letters) %>%
  as.data.frame() %>%
  mutate(.group = str_trim(.group), marker = "Caspase", state = "Inoc")

# ---------- Caspase Sym ----------
cas_sym <- confocal_data %>%
  filter(marker == "Caspase", state == "Sym") %>%
  droplevels()

fit_cas_sym <- lm(percentage ~ heat * time, data = cas_sym)
anova_cas_sym <- car::Anova(fit_cas_sym, type = 2)
emm_cas_sym <- emmeans(fit_cas_sym, ~ heat | time)
tukey_cas_sym <- pairs(emm_cas_sym, adjust = "tukey") %>%
  as.data.frame() %>%
  mutate(marker = "Caspase", state = "Sym")
letters_cas_sym <- cld(emm_cas_sym, adjust = "tukey", Letters = letters) %>%
  as.data.frame() %>%
  mutate(.group = str_trim(.group), marker = "Caspase", state = "Sym")

# ----------------------------
# Combine ANOVA tables
# ----------------------------
anova_table <- bind_rows(
  as.data.frame(anova_edu_apo)  %>% rownames_to_column("term") %>% mutate(marker = "EdU",     state = "Apo"),
  as.data.frame(anova_edu_inoc) %>% rownames_to_column("term") %>% mutate(marker = "EdU",     state = "Inoc"),
  as.data.frame(anova_edu_sym)  %>% rownames_to_column("term") %>% mutate(marker = "EdU",     state = "Sym"),
  as.data.frame(anova_cas_apo)  %>% rownames_to_column("term") %>% mutate(marker = "Caspase", state = "Apo"),
  as.data.frame(anova_cas_inoc) %>% rownames_to_column("term") %>% mutate(marker = "Caspase", state = "Inoc"),
  as.data.frame(anova_cas_sym)  %>% rownames_to_column("term") %>% mutate(marker = "Caspase", state = "Sym")
) %>%
  select(marker, state, term, everything())

# ----------------------------
# Combine Tukey tables
# This is the supplemental table that matches the graph
# ----------------------------
supplemental_tukey_table <- bind_rows(
  tukey_edu_apo,
  tukey_edu_inoc,
  tukey_edu_sym,
  tukey_cas_apo,
  tukey_cas_inoc,
  tukey_cas_sym
) %>%
  select(marker, state, time, contrast, estimate, SE, df, t.ratio, p.value) %>%
  mutate(
    estimate = round(estimate, 2),
    SE = round(SE, 2),
    t.ratio = round(t.ratio, 2),
    p.value = signif(p.value, 3)
  ) %>%
  arrange(marker, state, time)

# ----------------------------
# Save stats tables
# ----------------------------
write_csv(anova_global_table, "~/Documents/GitHub/heating_lacerates_final/tables/TableS10-confocal_anova_results.csv")
write_csv(supplemental_tukey_table, "~/Documents/GitHub/heating_lacerates_final/tables/TableS11-confocal_tukey_table.csv")
