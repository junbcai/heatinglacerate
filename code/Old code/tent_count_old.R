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
library(scales)
library(janitor)
library(gt)
library(ggh4x)

rm(list = ls())
graphics.off()

#Set working directory

getwd()
setwd("~/Documents/GitHub/heating_lacerates_final")

select <- dplyr::select
filter <- dplyr::filter
mutate <- dplyr::mutate
lag <- dplyr::lag

#Results of Experiment 2022

##Reading data table
tentcount_2022 <- read.csv("~/Documents/GitHub/heating_lacerates_final/data/2022_tentcount.csv")

str(tentcount_2022)

##Converting elements in table
tentcount_2022$tent_count <- as.numeric(tentcount_2022$tent_count)
tentcount_2022$ID <- as.factor(tentcount_2022$ID)
tentcount_2022$plate <- as.factor(tentcount_2022$plate)
tentcount_2022$well <- as.factor(tentcount_2022$well)
tentcount_2022$line <- as.factor(tentcount_2022$line)
tentcount_2022$temp <- as.factor(tentcount_2022$temp)
tentcount_2022$treatment <- as.factor(tentcount_2022$treatment)
tentcount_2022$symbiosis <- as.factor(tentcount_2022$symbiosis)
tentcount_2022$day <- as.numeric(tentcount_2022$day)
tentcount_2022$day_cat <- as.factor(tentcount_2022$day_cat)

tentcount_2022 <- tentcount_2022 %>%
  mutate(Day = as.factor(day)) %>%
  mutate(Day = dplyr::recode(Day, "0" = "00"))

##Saving table as new object
tentcount_2022_cleaned <- tentcount_2022

##Graphing results of Experiment 2
data_means <- tentcount_2022_cleaned %>%
  group_by(treatment, day) %>%
  summarise(mean = mean(tent_count, na.rm=TRUE),
            se = std.error(tent_count, na.rm=TRUE))

All_2022_plot <- ggplot(data = data_means, aes(x = day, y = mean)) +
  theme_classic(base_size = 15) +
  geom_line(aes(color = treatment, group = treatment),
            position = position_dodge(0.5)) +
  ylab(bquote("Mean tentacle number")) +
  xlab("Days post laceration (dpl)") +
  ggtitle("Effect of Temperature on Pedal Lacerate Tentacle Development in Aiptasia") +
  ylim(0,12) +
  geom_point(aes(color = treatment),
             size = 2.5,
             shape = 20,
             position = position_dodge(0.5)) +
  scale_x_continuous(
    breaks = round(seq(min(data_means$day), max(data_means$day), by = 1), 1)
  ) +
  geom_errorbar(
    aes(color = treatment, ymin = mean - se, ymax = mean + se),
    width = 0.2,
    position = position_dodge(0.5)
  ) +
  scale_color_manual(
    values = c(
      "CC7-APO-25C" = "aquamarine",
      "CC7-APO-32C" = "chocolate",
      "CC7-SYM-25C" = "darkorchid",
      "CC7-SYM-32C" = "coral1",
      "H2-APO-25C"  = "cornflowerblue",
      "H2-APO-32C"  = "orange",
      "H2-SYM-25C"  = "blue",
      "H2-SYM-32C"  = "red"
    ),
    breaks = c(
      "CC7-APO-25C","CC7-APO-32C",
      "CC7-SYM-25C","CC7-SYM-32C",
      "H2-APO-25C","H2-APO-32C",
      "H2-SYM-25C","H2-SYM-32C"
    ),
    labels = c(
      "CC7-APO-25°C",
      "CC7-APO-32°C",
      "CC7-SYM-25°C",
      "CC7-SYM-32°C",
      "H2-APO-25°C",
      "H2-APO-32°C",
      "H2-SYM-25°C",
      "H2-SYM-32°C"
    )
  ) +
  theme(legend.text.align = 0) +
  scale_size_manual(values = c(1.2,1.2,1.2,1.2)) +
  labs(colour = "Treatment")

All_2022_plot

# ============================================================
# Tentacle count analysis for Table S1-S2
# ============================================================
library(lme4)
library(car)
library(emmeans)
library(performance)
library(DHARMa)

#Remove CC7
data <- tentcount_2022_cleaned %>%
  filter(line != "CC7") %>%
  mutate(
    day_cat = factor(day_cat),
    temp = factor(temp),
    symbiosis = factor(symbiosis),
    ID = factor(ID)
  )

data <- tentcount_2022_cleaned

str(data)

model_pois <- glmer(
  tent_count ~ temp * symbiosis * day_cat + (1 | ID),
  family = poisson(link = "log"),
  data = data,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 200000)
  )
)

summary(model_pois)

# ----------------------------
# Model checks
# ----------------------------
plot(model_pois)
check_overdispersion(model_pois)
sim_res <- simulateResiduals(model_pois)
plot(sim_res)

# ----------------------------
# Type II ANOVA; S1 table
# ----------------------------
anova_tentacle <- Anova(model_pois, type = "II")

anova_tentacle

anova_tentacle_df <- as.data.frame(anova_tentacle) %>%
  tibble::rownames_to_column("Factor")

write.csv(
  anova_tentacle_df,
  file = file.path(
    "~/Documents/GitHub/heating_lacerates_final/tables",
    "TableS1_anova_tentacle_number.csv"
  ),
  row.names = FALSE
)

# ----------------------------
# Post hoc comparisons of temperature effect; S2 table
# ----------------------------
emm_temp <- emmeans(model_pois, ~ temp | day_cat, type = "response")
tukey_temp <- pairs(emm_temp, adjust = "tukey") 

tukey_temp_df <- as.data.frame(tukey_temp)

write.csv(
  tukey_temp_df,
  file = file.path(
    "~/Documents/GitHub/heating_lacerates_final/tables",
    "TableS2_tukey_temperature_tentacle_number.csv"
  ),
  row.names = FALSE
)

# ----------------------------
# Post hoc comparisons of symbiosis effect
# ----------------------------
emm_symbiosis <- emmeans(model_pois, ~ symbiosis | day_cat, type = "response")
tukey_sym <- pairs(emm_symbiosis, adjust = "tukey") 

tukey_sym_df <- as.data.frame(tukey_sym)

# ----------------------------
# Figure 1 Sym vs Apo for manuscript
# ----------------------------

# Start from the raw cleaned dataset
fig1_dat_raw <- tentcount_2022_cleaned %>%
  filter(line != "CC7") %>%
  mutate(
    day_cat = factor(day_cat),
    temp = factor(temp),
    symbiosis = factor(symbiosis),
    ID = factor(ID)
  )

fig1fuller_data <- fig1_dat_raw %>%
  group_by(temp, symbiosis, day) %>%
  summarise(
    mean = mean(tent_count, na.rm = TRUE),
    se = std.error(tent_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    treatment = case_when(
      symbiosis == "Apo" & temp == "25C (ambient)" ~ "Apo, 25°C",
      symbiosis == "Sym" & temp == "25C (ambient)" ~ "Sym, 25°C",
      symbiosis == "Apo" & temp == "32C (heat stress)" ~ "Apo, 32°C",
      symbiosis == "Sym" & temp == "32C (heat stress)" ~ "Sym, 32°C"
    ),
    treatment = factor(
      treatment,
      levels = c("Apo, 25°C", "Sym, 25°C", "Apo, 32°C", "Sym, 32°C")
    )
  )

my_theme <- theme_classic(base_size = 14) +
  theme(
    text = element_text(family = "sans", size = 14),
    axis.text = element_text(size = 14, colour = "black"),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14)
  )

fig1_fuller <- ggplot(
  fig1fuller_data,
  aes(
    x = day,
    y = mean,
    color = treatment,
    linetype = treatment,
    group = treatment
  )
) +
  my_theme +
  geom_line(linewidth = 1.2) +
  geom_errorbar(
    aes(ymin = mean - se, ymax = mean + se),
    width = 0.2,
    linewidth = 0.6,
    alpha = 0.8,
    linetype = "solid"
  ) +
  geom_point(size = 4) +
  
  # ----------------------------
# Temperature significance bars
# Based on:
# day 5-11 = p < 0.001
# day 12 = p < 0.01
# ----------------------------
annotate("segment", x = 5, xend = 11, y = 9.2, yend = 9.2, linewidth = 0.8) +
  annotate("text", x = 8, y = 9.45, label = "***", size = 6) +
  annotate("text", x = 12, y = 9.45, label = "**", size = 6) +
  
  ylab("Mean tentacle number") +
  xlab("Days post laceration (dpl)") +
  scale_y_continuous(
    breaks = seq(0, 10, 2),
    limits = c(0, 10)
  ) +
  scale_x_continuous(
    breaks = seq(min(fig1fuller_data$day), max(fig1fuller_data$day), 1)
  ) +
  scale_color_manual(
    values = c(
      "Apo, 25°C" = "#6FA3D9",
      "Sym, 25°C" = "#3B6FB6",
      "Apo, 32°C" = "#F39B7F",
      "Sym, 32°C" = "#E64B35"
    )
  ) +
  scale_linetype_manual(
    values = c(
      "Apo, 25°C" = "dashed",
      "Sym, 25°C" = "solid",
      "Apo, 32°C" = "dashed",
      "Sym, 32°C" = "solid"
    )
  ) +
  labs(
    color = "Treatment",
    linetype = "Treatment"
    #    caption = "* p < 0.05, ** p < 0.01, *** p < 0.001"
  ) +
  theme(
    legend.position = c(0.73, 0.45),
    legend.justification = c("center", "center"),
    legend.key.width = unit(2.8, "cm")
  )

fig1_fuller

ggsave(
  filename = "Fig2.png",
  plot = fig1_fuller,
  path = "~/Documents/GitHub/heating_lacerates_final/figs",
  device = "png",
  width = 7,
  height = 5,
  units = "in",
  dpi = 600,
  #  compression = "lzw",
  bg = "white"
)

ggsave(
  filename = "Fig2.pdf",
  plot = fig1_fuller,
  path = "~/Documents/GitHub/heating_lacerates_final/figs",
  device = "pdf",
  width = 7,
  height = 5,
  units = "in",
  bg = "white"
)


# =========================
# 2022 MORTALITY FIGURE
# =========================

# -------------------------
# Prepare mortality data
# -------------------------
df_mortality_2022 <- tentcount_2022_cleaned %>%
  filter(
    line == "H2",
    day_cat %in% c("day_14", "day_21")
  ) %>%
  mutate(
    Mortality = ifelse(is.na(tent_count) | tent_count == 0, "Dead", "Alive"),
    dead = ifelse(Mortality == "Dead", 1, 0),
    Mortality = factor(Mortality, levels = c("Alive", "Dead")),
    day_cat = factor(day_cat, levels = c("day_14", "day_21"))
  )

# -------------------------
# Shared theme
# -------------------------
my_theme <- theme_classic(base_size = 14) +
  theme(
    text = element_text(family = "sans", size = 14),
    axis.text = element_text(size = 14, colour = "black"),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14)
  )

# -------------------------
# Shared data prep
# -------------------------
mortality_2022_plot_df_flipped <- df_mortality_2022 %>%
  mutate(
    sym_state = case_when(
      grepl("APO", treatment) ~ "Aposymbiotic",
      grepl("SYM", treatment) ~ "Symbiotic"
    ),
    temp_label = case_when(
      grepl("25", treatment) ~ "25°C",
      grepl("32", treatment) ~ "32°C"
    ),
    day_label = case_when(
      day_cat == "day_14" ~ "14 dpl",
      day_cat == "day_21" ~ "21 dpl"
    )
  ) %>%
  group_by(sym_state, temp_label, day_label) %>%
  summarise(
    mortality_prop = mean(dead, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    survival_prop = 1 - mortality_prop,
    temp_label = factor(temp_label, levels = c("25°C", "32°C")),
    sym_state = factor(sym_state, levels = c("Aposymbiotic", "Symbiotic")),
    outline_group = case_when(
      sym_state == "Aposymbiotic" & temp_label == "25°C" ~ "apo_25",
      sym_state == "Symbiotic"    & temp_label == "25°C" ~ "sym_25",
      sym_state == "Aposymbiotic" & temp_label == "32°C" ~ "apo_32",
      sym_state == "Symbiotic"    & temp_label == "32°C" ~ "sym_32"
    ),
    group_order = case_when(
      sym_state == "Aposymbiotic" & day_label == "14 dpl" ~ 1,
      sym_state == "Aposymbiotic" & day_label == "21 dpl" ~ 2,
      sym_state == "Symbiotic"    & day_label == "14 dpl" ~ 3,
      sym_state == "Symbiotic"    & day_label == "21 dpl" ~ 4
    ),
    axis_text = case_when(
      day_label == "14 dpl" ~ "14 dpl",
      day_label == "21 dpl" ~ "21 dpl"
    )
  ) %>%
  arrange(temp_label, group_order) %>%
  group_by(temp_label, sym_state) %>%
  mutate(
    bar_label = factor(axis_text, levels = rev(unique(axis_text)))
  ) %>%
  ungroup()

# -------------------------
# Shared colors
# -------------------------
survival_fill_colors <- c(
  "sym_25" = "#3B6FB6",
  "apo_25" = "#6FA3D9",
  "sym_32" = "#E64B35",
  "apo_32" = "#F39B7F"
)

outline_colors <- c(
  "sym_25" = "#3B6FB6",
  "apo_25" = "#6FA3D9",
  "sym_32" = "#E64B35",
  "apo_32" = "#F39B7F"
)

# -------------------------
# Shared base plot
# -------------------------
p_survival_2022_base <- ggplot(
  mortality_2022_plot_df_flipped,
  aes(x = bar_label)
) +
  coord_flip() +
  ggh4x::facet_nested(
    rows = vars(temp_label, sym_state),
    scales = "free_y",
    space = "free_y",
    switch = "y"
  ) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, 1),
    expand = expansion(mult = c(0, 0.04))
  ) +
  labs(
    x = "Treatment Group",
    y = "Percent Survival"
  ) +
  my_theme +
  theme(
    panel.grid = element_blank(),
    strip.placement = "outside",
    strip.text.y.right = element_text(size = 14, face = "bold"),
    strip.background.y = element_rect(
      fill = "white",
      color = "black",
      linewidth = 1
    ),
    ggh4x.facet.nestline = element_blank(),
    panel.spacing.y = unit(0.08, "lines"),
    axis.text.y = element_text(size = 14, lineheight = 0.9),
    axis.text.x = element_text(size = 14),
    plot.margin = margin(8, 14, 8, 8)
  )

# -------------------------
# Version 1:
# grey survival bar + colored 100% outline
# -------------------------
p_survival_2022_flipped_outline <- p_survival_2022_base +
  geom_col(
    aes(y = survival_prop),
    fill = "grey80",
    width = 0.82
  ) +
  geom_col(
    aes(y = 1, color = outline_group),
    fill = NA,
    linewidth = 0.9,
    width = 0.82
  ) +
  scale_color_manual(
    values = outline_colors,
    guide = "none"
  )

# -------------------------
# Version 2:
# colored survival bar + black 100% outline
# -------------------------
p_survival_2022_flipped_colorbar <- p_survival_2022_base +
  geom_col(
    aes(y = survival_prop, fill = outline_group),
    width = 0.82,
    color = NA
  ) +
  geom_col(
    aes(y = 1),
    width = 0.82,
    fill = NA,
    color = "black",
    linewidth = 0.9
  ) +
  scale_fill_manual(
    values = survival_fill_colors,
    guide = "none"
  )

# -------------------------
# Draw both
# -------------------------
p_survival_2022_flipped_outline
p_survival_2022_flipped_colorbar

ggsave(
  filename = "Fig3_2022.png",
  plot = p_survival_2022_flipped_colorbar,
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
  filename = "Fig3_2022.pdf",
  plot = p_survival_2022_flipped_colorbar,
  path = "figs",
  device = pdf,
  width = 7,
  height = 5,
  units = "in",
  bg = "white"
)

### =========================
### 2022 Survival STATS; S3 table
### =========================

# Prepare stats dataset
survival_stats_2022 <- df_mortality_2022 %>%
  mutate(
    alive = 1 - dead,
    sym_state = case_when(
      grepl("APO", treatment) ~ "Aposymbiotic",
      grepl("SYM", treatment) ~ "Symbiotic"
    ),
    temp_label = case_when(
      grepl("25", treatment) ~ "25°C",
      grepl("32", treatment) ~ "32°C"
    )
  ) %>%
  filter(
    !is.na(sym_state),
    !is.na(temp_label)
  ) %>%
  mutate(
    sym_state = factor(
      sym_state,
      levels = c("Aposymbiotic", "Symbiotic")
    ),
    temp_label = factor(
      temp_label,
      levels = c("25°C", "32°C")
    )
  )

# Binomial GLM for survival
survival_glm_2022 <- glm(
  alive ~ temp_label * sym_state,
  data = survival_stats_2022,
  family = binomial
)

summary(survival_glm_2022)

# Create and save survival model coefficient table
table_survival_2022 <- summary(survival_glm_2022)$coefficients %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Term")

colnames(table_survival_2022) <- c(
  "Term",
  "Estimate",
  "SE",
  "z",
  "p"
)

write.csv(
  table_survival_2022,
  file = file.path(
    "~/Documents/GitHub/heating_lacerates_final/tables",
    "TableS3_survival_glm_2022.csv"
  ),
  row.names = FALSE
)

# Estimated marginal means
emm_surv_temp_2022 <- emmeans(
  survival_glm_2022,
  ~ temp_label | sym_state
)

emm_surv_sym_2022 <- emmeans(
  survival_glm_2022,
  ~ sym_state | temp_label
)

emm_surv_temp_2022
emm_surv_sym_2022


# =========================================================
# SUPPLEMENTARY HEAT RAMP (COLLAPSED, FIG1 STYLE)
# =========================================================

library(ggplot2)
library(plotrix)
library(dplyr)
library(janitor)
library(here)
library(ggh4x)

graphics.off()

setwd("~/Documents/GitHub/heating_lacerates_final")

# ----------------------------
# Read heat-ramp dataset
# ----------------------------
supp_heat_ramp_long_collapsed <- read.csv("~/Documents/GitHub/heating_lacerates_final/data/heat ramp.csv")

str(supp_heat_ramp_long_collapsed)

# ----------------------------
# Clean
# ----------------------------
supp_heat_ramp_long_collapsed$tent_count <- as.numeric(supp_heat_ramp_long_collapsed$tent_count)
supp_heat_ramp_long_collapsed$ID <- as.factor(supp_heat_ramp_long_collapsed$ID)
supp_heat_ramp_long_collapsed$plate <- as.factor(supp_heat_ramp_long_collapsed$plate)
supp_heat_ramp_long_collapsed$well <- as.factor(supp_heat_ramp_long_collapsed$well)
supp_heat_ramp_long_collapsed$line <- as.factor(supp_heat_ramp_long_collapsed$line)
supp_heat_ramp_long_collapsed$temp <- as.factor(supp_heat_ramp_long_collapsed$temp)
supp_heat_ramp_long_collapsed$treatment <- as.character(supp_heat_ramp_long_collapsed$treatment)
supp_heat_ramp_long_collapsed$symbiosis <- as.factor(supp_heat_ramp_long_collapsed$symbiosis)
supp_heat_ramp_long_collapsed$day <- as.numeric(supp_heat_ramp_long_collapsed$day)
supp_heat_ramp_long_collapsed$day_cat <- as.factor(supp_heat_ramp_long_collapsed$day_cat)

# ----------------------------
# Filter, collapse replicate labels, and stop at day 14
# ----------------------------
supp_heat_ramp_data <- supp_heat_ramp_long_collapsed %>%
  filter(
    treatment %in% c(
      "H2-SYM-25C",
      "H2-SYM-25C (a)",
      "H2-SYM-25C (b)",
      "H2-SYM-25C (c)",
      "H2-SYM-32C",
      "H2-SYM-33.5C",
      "H2-SYM-35C"
    ),
    day <= 14
  ) %>%
  mutate(
    temp = gsub("H2-SYM-", "", treatment),
    temp = gsub(" \\(.*\\)", "", temp),
    temp = factor(
      temp,
      levels = c("25C", "32C", "33.5C", "35C")
    )
  )

# ----------------------------
# Counts
# ----------------------------
supp_heat_ramp_counts_rows <- supp_heat_ramp_data %>%
  count(temp, name = "n_rows")

print(supp_heat_ramp_counts_rows)

supp_heat_ramp_counts_ids <- supp_heat_ramp_data %>%
  distinct(ID, temp) %>%
  count(temp, name = "n_ids")

print(supp_heat_ramp_counts_ids)

# ----------------------------
# Summarise
# ----------------------------
supp_heat_ramp_fig_data <- supp_heat_ramp_data %>%
  group_by(temp, day) %>%
  summarise(
    mean = mean(tent_count, na.rm = TRUE),
    se = std.error(tent_count, na.rm = TRUE),
    .groups = "drop"
  )

print(supp_heat_ramp_fig_data, n = Inf)

# ----------------------------
# Theme to match fig1 style
# ----------------------------
my_theme <- theme_classic(base_size = 14) +
  theme(
    text = element_text(family = "sans", size = 14),
    axis.text = element_text(size = 14, colour = "black"),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14)
  )

# ----------------------------
# Plot
# ----------------------------
supp_heat_ramp_fig <- ggplot(
  supp_heat_ramp_fig_data,
  aes(
    x = day,
    y = mean,
    color = temp,
    linetype = temp,
    shape = temp,
    group = temp
  )
) +
  my_theme +
  geom_line(linewidth = 0.9) +
  geom_errorbar(
    aes(ymin = mean - se, ymax = mean + se),
    width = 0.2,
    linewidth = 0.6
  ) +
  geom_point(size = 4) +
  ylab("Mean tentacle number") +
  xlab("Days post laceration (dpl)") +
  scale_y_continuous(
    breaks = seq(0, 15, 2),
    limits = c(0, 10)
  ) +
  scale_x_continuous(
    breaks = seq(min(supp_heat_ramp_fig_data$day), max(supp_heat_ramp_fig_data$day), 1)
  ) +
  
  scale_color_manual(
    values = c(
      "25C"   = "#3B6FB6",
      "32C"   = "#E64B35",
      "33.5C" = "#A50F15",
      "35C"   = "#3B0000"
    ),
    labels = c(
      "25C"   = "Sym 25°C",
      "32C"   = "Sym 32°C",
      "33.5C" = "Sym 33.5°C",
      "35C"   = "Sym 35°C"
    )
  ) +
  scale_linetype_manual(
    values = c(
      "25C"   = "solid",
      "32C"   = "solid",
      "33.5C" = "dotdash",
      "35C"   = "solid"
    ),
    labels = c(
      "25C"   = "Sym 25°C",
      "32C"   = "Sym 32°C",
      "33.5C" = "Sym 33.5°C",
      "35C"   = "Sym 35°C"
    )
  ) +
  scale_shape_manual(
    values = c(
      "25C"   = 16,
      "32C"   = 16,
      "33.5C" = 17,
      "35C"   = 15
    ),
    labels = c(
      "25C"   = "Sym 25°C",
      "32C"   = "Sym 32°C",
      "33.5C" = "Sym 33.5°C",
      "35C"   = "Sym 35°C"
    )
  ) +
  
  labs(
    color = "Temperature",
    linetype = "Temperature",
    shape = "Temperature"
  ) +
  guides(
    color = guide_legend(
      override.aes = list(
        linewidth = 1.5,
        linetype = c("solid", "solid", "dotdash", "solid"),
        shape = c(16, 16, 17, 15)
      )
    ),
    linetype = "none",
    shape = "none"
  ) +
  theme(
    legend.position = c(0.79, 0.43),
    legend.justification = c("center", "center"),
    legend.key.width = unit(1.6, "cm")
  )

supp_heat_ramp_fig

# ----------------------------
# Save
# ----------------------------
ggsave(
  filename = "FigS1_heat_ramp.png",
  plot = supp_heat_ramp_fig,
  path = "~/Documents/GitHub/heating_lacerates_final/figs",
  device = "png",
  width = 7,
  height = 5,
  units = "in",
  dpi = 600,
  bg = "white"
)

ggsave(
  filename = "FigS1_heat_ramp.pdf",
  plot = supp_heat_ramp_fig,
  path = "~/Documents/GitHub/heating_lacerates_final/figs",
  device = "pdf",
  width = 7,
  height = 5,
  units = "in",
  bg = "white"
)


#This is just for BioRender matching
treatment_cols <- c(
  "apo_25"  = "#6FA3D9",
  "inoc_25" = "#3B88C3",
  "sym_25"  = "#3B6FB6",
  "apo_32"  = "#F39B7F",
  "inoc_32" = "#D95F02",
  "sym_32"  = "#E64B35"
)