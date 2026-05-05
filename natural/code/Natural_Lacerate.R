# =========================================================
# PROJECT: Thermal stress and pedal laceration experiment
# PURPOSE:
#   Organize analyses into 4 main sections:
#   1) Number of lacerates produced
#   2) Tentacle rate data
#   3) Lacerate symbiont area data
#   4) Parent physiology
# NOTE:
#   Each section is written independently.
#   No helper functions are used.
# =========================================================


# =========================================================
# 0. LOAD PACKAGES
# =========================================================

library(tidyverse)
library(janitor)
library(lubridate)
library(grid)
library(emmeans)
library(multcomp)
library(multcompView)
library(car)
library(glmmTMB)
library(DHARMa)
library(performance)
library(lme4)
library(patchwork)
library(readr)


# =========================================================
# 1. HOUSEKEEPING
# =========================================================

rm(list = ls())
graphics.off()

getwd()
setwd("/Users/junbc/Documents/GitHub/heatinglacerate/natural")

# =========================================================
# 2. COMMON PLOT SETTINGS
# =========================================================

my_theme <- theme_classic(base_size = 18) +
  theme(
    #    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1.0),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14),
    strip.text = element_text(face = "bold", size = 16),
    strip.background = element_rect(
      fill = "white",
      color = "black",
      linewidth = 1.2
    ),
    panel.spacing = unit(1.4, "lines"),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 14)
  )

my_colors <- c(
  "H2_25"  = "#3B6FB6",
  "H2_32"  = "#D62728",
  "VWA_25" = "#7B6FD0",
  "VWA_32" = "#8C564B"
)


# =========================================================
# =========================================================
# SECTION 1. NUMBER OF LACERATES PRODUCED
# =========================================================
# =========================================================
# Question:
#   How many pedal lacerates were produced under each treatment?
#
# Data source:
#   natural_lacerate_aiptasia_experiment_datasheets - lacerates_metadata.csv
#
# Main outputs:
#   - lacerates per tub
#   - lacerates per parent
#   - total lacerates across all 10 weeks
# =========================================================


# ---------------------------------------------------------
# 1.1 Read lacerate metadata
# ---------------------------------------------------------

lacerate_meta <- read_csv(
  "~/Documents/GitHub/heatinglacerate/natural/data/natural_lacerate_aiptasia_experiment_datasheets - lacerates_metadata_dup_removed.csv") %>%
  clean_names() %>%
  mutate(
    cohort = factor(cohort, levels = c("W1-2", "W3-4", "W5-6", "W7-8", "W9-10")),
    treatment = factor(treatment, levels = c("H2_25", "H2_32", "VWA_25", "VWA_32")),
    genotype = factor(genotype, levels = c("H2", "VWA")),
    temp = factor(temp, levels = c(25, 32)),
    tub_id = as.factor(tub_id),
    parent_id = as.factor(parent_id),
    lacerate_id = as.character(lacerate_id)
  )

lacerate_meta <- lacerate_meta %>%
  filter(
    !is.na(treatment),
    !is.na(genotype),
    !is.na(cohort),
    lacerate_id != "DUPLICATE_REMOVED",
    treatment != "",
    genotype != "",
    cohort != ""
  )

# ---------------------------------------------------------
# 1.2 Keep one row per unique lacerate
# ---------------------------------------------------------

lacerate_unique <- lacerate_meta %>%
  filter(!is.na(lacerate_id), lacerate_id != "") %>%
  distinct(lacerate_id, cohort, treatment, genotype, temp, tub_id, parent_id)

lacerate_dup_counts <- lacerate_meta %>%
  count(lacerate_id, name = "n") %>%
  filter(n > 1)

#Check where duplicates
lacerate_dup_counts_cohort <- lacerate_meta %>%
  count(lacerate_id, cohort, name = "n") %>%
  filter(n > 1)
lacerate_dup_counts_cohort %>%
  count(cohort)

#Summary
lacerate_repeat_summary <- lacerate_meta %>%
  filter(!is.na(lacerate_id), lacerate_id != "") %>%
  group_by(lacerate_id) %>%
  summarise(
    n = n(),
    cohorts = paste(sort(unique(cohort)), collapse = ", "),
    n_cohorts = n_distinct(cohort),
    .groups = "drop"
  ) %>%
  filter(n > 1) %>%
  arrange(desc(n), lacerate_id)

# write to CSV
# write_csv(lacerate_repeat_summary, "~/Documents/GitHub/heatinglacerate/natural/data/lacerate_duplicate_counts.csv")

# ---------------------------------------------------------
# 1.3 Count lacerates per tub
# ---------------------------------------------------------

lacerates_per_tub <- lacerate_unique %>%
  count(cohort, treatment, genotype, temp, tub_id, name = "n_lacerates")

print(lacerates_per_tub)

# ---------------------------------------------------------
# 1.4 Create Tukey letters for lacerates per tub
# ---------------------------------------------------------

letters_tub <- lacerates_per_tub %>%
  group_by(cohort) %>%
  group_modify(~{
    dat <- .x
    
    if (n_distinct(dat$treatment) < 2) {
      dat %>%
        distinct(treatment) %>%
        mutate(.group = "a")
    } else {
      mod <- lm(n_lacerates ~ treatment, data = dat)
      em <- emmeans(mod, ~ treatment)
      cld_df <- multcomp::cld(em, Letters = letters, adjust = "sidak")
      
      cld_df %>%
        as.data.frame() %>%
        dplyr::select(treatment, .group) %>%
        mutate(.group = stringr::str_trim(.group))
    }
  }) %>%
  ungroup()

positions_tub <- lacerates_per_tub %>%
  group_by(cohort, treatment) %>%
  summarise(
    y_pos = max(n_lacerates, na.rm = TRUE) + 0.8,
    .groups = "drop"
  )

letters_tub <- positions_tub %>%
  left_join(letters_tub, by = c("cohort", "treatment"))

# ---------------------------------------------------------
# 1.5 Plot number of lacerates per tub
# ---------------------------------------------------------

p_lacerates_tub <- ggplot(
  lacerates_per_tub,
  aes(x = treatment, y = n_lacerates, color = treatment)
) +
  geom_boxplot(
    width = 0.58,
    fill = NA,
    linewidth = 1.2,
    outlier.shape = NA
  ) +
  geom_jitter(
    width = 0.14,
    height = 0.05,
    size = 2.8,
    alpha = 0.65,
    stroke = 0.9
  ) +
  geom_text(
    data = letters_tub,
    aes(x = treatment, y = y_pos, label = .group),
    inherit.aes = FALSE,
    size = 5,
    fontface = "bold",
    color = "black"
  ) +
  facet_wrap(~ cohort, ncol = 3, scales = "free_y") +
  scale_color_manual(name = "treatment", values = my_colors) +
  #  coord_cartesian(ylim = c(0, 70)) +
  labs(
    x = "Treatment",
    y = "Number of lacerates per tub"
  ) +
  my_theme +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5)
  )

p_lacerates_tub

# ---------------------------------------------------------
# 1.6 Count lacerates per parent
# ---------------------------------------------------------

lacerates_per_parent <- lacerate_unique %>%
  count(parent_id, cohort, treatment, genotype, temp, name = "n_lacerates")

print(lacerates_per_parent)

# ---------------------------------------------------------
# 1.7 Create Tukey letters for lacerates per parent
# ---------------------------------------------------------

letters_parent <- lacerates_per_parent %>%
  group_by(cohort) %>%
  group_modify(~{
    dat <- .x
    
    if (n_distinct(dat$treatment) < 2) {
      dat %>%
        distinct(treatment) %>%
        mutate(.group = "a")
    } else {
      mod <- lm(n_lacerates ~ treatment, data = dat)
      em <- emmeans(mod, ~ treatment)
      cld_df <- multcomp::cld(em, Letters = letters, adjust = "sidak")
      
      cld_df %>%
        as.data.frame() %>%
        dplyr::select(treatment, .group) %>%
        mutate(.group = stringr::str_trim(.group))
    }
  }) %>%
  ungroup()

positions_parent <- lacerates_per_parent %>%
  group_by(cohort, treatment) %>%
  summarise(
    y_pos = max(n_lacerates, na.rm = TRUE) + 0.8,
    .groups = "drop"
  )

letters_parent <- positions_parent %>%
  left_join(letters_parent, by = c("cohort", "treatment"))

# ---------------------------------------------------------
# 1.8 Plot number of lacerates per parent
# ---------------------------------------------------------

p_lacerates_parent <- ggplot(
  lacerates_per_parent,
  aes(x = treatment, y = n_lacerates, color = treatment)
) +
  geom_boxplot(
    width = 0.58,
    fill = NA,
    linewidth = 1.2,
    outlier.shape = NA
  ) +
  geom_jitter(
    width = 0.14,
    height = 0.05,
    size = 2.8,
    alpha = 0.65,
    stroke = 0.9
  ) +
  geom_text(
    data = letters_parent,
    aes(x = treatment, y = y_pos, label = .group),
    inherit.aes = FALSE,
    size = 5,
    fontface = "bold",
    color = "black"
  ) +
  facet_wrap(~ cohort, ncol = 2, scales = "free_y") +
  scale_color_manual(name = "Treatment", values = my_colors) +
  labs(
    x = "Treatment",
    y = "Number of lacerates per parent"
  ) +
  my_theme

p_lacerates_parent


# ---------------------------------------------------------
# 1.9 Total lacerates across all 10 weeks
# ---------------------------------------------------------

total_lacerates <- lacerate_unique %>%
  distinct(lacerate_id, treatment, genotype, temp) %>%
  count(treatment, genotype, temp, name = "total_lacerates") %>%
  arrange(genotype, temp)

print(total_lacerates)


# ---------------------------------------------------------
# 1.10 Total lacerates table in wide format
# ---------------------------------------------------------

total_lacerates_wide <- total_lacerates %>%
  dplyr::select(genotype, temp, total_lacerates) %>%
  mutate(temp = as.character(temp)) %>%
  tidyr::pivot_wider(names_from = temp, values_from = total_lacerates) %>%
  rename(
    `25 °C` = `25`,
    `32 °C` = `32`
  ) %>%
  mutate(
    Change = `32 °C` - `25 °C`
  )

print(total_lacerates_wide)


# ---------------------------------------------------------
# 1.11 Optional stats for lacerates per tub
# ---------------------------------------------------------

lacerates_per_tub <- lacerates_per_tub %>%
  mutate(
    genotype = factor(genotype, levels = c("H2", "VWA")),
    temp = factor(temp, levels = c(25, 32))
  )

cat("\n==============================\n")
cat("SECTION 1: LACERATES PER TUB MODEL\n")
cat("==============================\n")

mod_tub_pois <- glmer(
  n_lacerates ~ genotype * temp + (1 | cohort),
  data = lacerates_per_tub,
  family = poisson(link = "log"),
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 200000)
  )
)

sim_tub <- simulateResiduals(mod_tub_pois)
plot(sim_tub)
print(testDispersion(sim_tub))
print(testZeroInflation(sim_tub))
print(performance::check_overdispersion(mod_tub_pois))

if (performance::check_overdispersion(mod_tub_pois)$dispersion_ratio > 1.5) {
  mod_tub_final <- glmmTMB(
    n_lacerates ~ genotype * temp + (1 | cohort),
    data = lacerates_per_tub,
    family = nbinom2()
  )
} else {
  mod_tub_final <- mod_tub_pois
}

print(Anova(mod_tub_final, type = "II"))
print(emmeans(mod_tub_final, ~ genotype * temp))
print(emmeans(mod_tub_final, pairwise ~ genotype | temp, adjust = "tukey"))
print(emmeans(mod_tub_final, pairwise ~ temp | genotype, adjust = "tukey"))

# =========================================================
# =========================================================
# SECTION 2. TENTACLE RATE DATA
# =========================================================
# =========================================================

lacerate_counts <- read_csv(
  "~/Documents/GitHub/heatinglacerate/natural/data/natural_lacerate_aiptasia_experiment_datasheets - lacerate_counts.csv"
) %>%
  clean_names() %>%
  mutate(
    date = ymd(date),
    cohort = factor(cohort, levels = c("W1-2", "W3-4", "W5-6", "W7-8", "W9-10")),
    treatment = factor(treatment, levels = c("H2_25", "H2_32", "VWA_25", "VWA_32")),
    genotype = factor(genotype, levels = c("H2", "VWA")),
    temp = factor(temp, levels = c("25", "32")),
    lacerate_id = as.character(lacerate_id),
    rate = as.numeric(rate),
    rate2 = as.numeric(rate2),
    tentacle_count = as.numeric(tentacle_count)
  )

final_rate_dat <- lacerate_counts %>%
  filter(
    !is.na(rate2),
    !(cohort == "W9-10" & rate2 > 6 & rate2 > 6 & days_since_formation <= 2 )
  ) %>%
  arrange(lacerate_id, cohort, date) %>%
  group_by(lacerate_id, cohort, treatment, genotype, temp) %>%
  slice_max(order_by = date, n = 1, with_ties = FALSE) %>%
  ungroup()

cld_rate_df <- as.data.frame(
  cld(emmeans(lm(rate2 ~ genotype * temp * cohort, data = final_rate_dat), ~ genotype * temp | cohort),
      Letters = letters, adjust = "tukey")
) %>%
  mutate(
    treatment = factor(paste(genotype, temp, sep = "_"),
                       levels = c("H2_25", "H2_32", "VWA_25", "VWA_32")),
    .group = str_trim(.group)
  ) %>%
  left_join(
    final_rate_dat %>%
      group_by(cohort) %>%
      summarise(y = max(rate2, na.rm = TRUE) * 1.08, .groups = "drop"),
    by = "cohort"
  )

p_final_rate <- ggplot(
  final_rate_dat,
  aes(x = treatment, y = rate2, color = treatment)
) +
  geom_boxplot(outlier.shape = NA, width = 0.6) +
  geom_jitter(
    width = 0.15,
    size = 2.8,
    alpha = 0.7
  ) +
  geom_text(
    data = cld_rate_df,
    aes(x = treatment, y = y, label = .group),
    color = "black",
    size = 5,
    inherit.aes = FALSE
  ) +
  facet_wrap(~ cohort, scales = "free_y") +
  coord_cartesian(ylim = c(0, 8)) +
  scale_color_manual(values = my_colors) +
  labs(
    x = "Treatment",
    y = "Final tentacle growth rate per day"
  ) +
  my_theme +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5)
  )

p_final_rate

final_rate_dat_no0 <- final_rate_dat %>%
  filter(!is.na(rate2), rate2 != 0)

cld_rate_df <- as.data.frame(
  multcomp::cld(
    emmeans::emmeans(lm(rate2 ~ genotype * temp * cohort, data = final_rate_dat_no0),
                     ~ genotype * temp | cohort),
    Letters = letters,
    adjust = "tukey"
  )
) %>%
  mutate(
    treatment = factor(paste(genotype, temp, sep = "_"),
                       levels = c("H2_25", "H2_32", "VWA_25", "VWA_32")),
    .group = stringr::str_trim(.group)
  ) %>%
  left_join(
    final_rate_dat_no0 %>%
      group_by(cohort) %>%
      summarise(y = max(rate2, na.rm = TRUE) * 1.08, .groups = "drop"),
    by = "cohort"
  )

p_final_rate_nozeros <- ggplot(
  final_rate_dat_no0,
  aes(x = treatment, y = rate2, color = treatment)
) +
  geom_boxplot(outlier.shape = NA, width = 0.6) +
  geom_jitter(
    width = 0.15,
    size = 2.8,
    alpha = 0.7
  ) +
  geom_text(
    data = cld_rate_df,
    aes(x = treatment, y = y, label = .group),
    color = "black",
    size = 5,
    inherit.aes = FALSE
  ) +
  facet_wrap(~ cohort, scales = "free_y") +
  coord_cartesian(ylim = c(0, 8)) +
  scale_color_manual(values = my_colors) +
  labs(
    x = "Treatment",
    y = "Final tentacle growth rate per day"
  ) +
  my_theme +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5)
  )

p_final_rate_nozeros

final_tentacle_dat <- lacerate_counts %>%
  filter(!is.na(tentacle_count)) %>%
  arrange(lacerate_id, cohort, date) %>%
  group_by(lacerate_id, cohort, treatment, genotype, temp) %>%
  slice_tail(n = 1) %>%
  ungroup()

final_tentacle_nonzero <- final_tentacle_dat %>%
  filter(tentacle_count > 0)

cld_tent_zero_df <- as.data.frame(
  cld(
    emmeans(
      glmmTMB(tentacle_count ~ genotype * temp * cohort,
              family = nbinom2(),
              data = final_tentacle_dat),
      ~ genotype * temp | cohort,
      type = "response"
    ),
    Letters = letters,
    adjust = "tukey"
  )
) %>%
  mutate(
    treatment = factor(paste(genotype, temp, sep = "_"),
                       levels = c("H2_25", "H2_32", "VWA_25", "VWA_32")),
    .group = str_trim(.group)
  ) %>%
  left_join(
    final_tentacle_dat %>%
      group_by(cohort) %>%
      summarise(y = max(tentacle_count, na.rm = TRUE) * 1.08, .groups = "drop"),
    by = "cohort"
  )

p_final_tentacles <- ggplot(
  final_tentacle_dat,
  aes(x = treatment, y = tentacle_count, color = treatment)
) +
  geom_boxplot(outlier.shape = NA, width = 0.6) +
  geom_jitter(
    width = 0.15,
    size = 2.8,
    alpha = 0.7
  ) +
  geom_text(
    data = cld_tent_zero_df,
    aes(x = treatment, y = y, label = .group),
    color = "black",
    size = 5,
    inherit.aes = FALSE
  ) +
  facet_wrap(~ cohort, scales = "free_y") +
  coord_cartesian(ylim = c(0, 27)) +
  scale_color_manual(values = my_colors) +
  labs(
    x = "Treatment",
    y = "Final tentacle count"
  ) +
  my_theme

p_final_tentacles

cld_tent_nonzero_df <- as.data.frame(
  cld(
    emmeans(
      glmmTMB(tentacle_count ~ genotype * temp * cohort,
              family = nbinom2(),
              data = final_tentacle_nonzero),
      ~ genotype * temp | cohort,
      type = "response"
    ),
    Letters = letters,
    adjust = "tukey"
  )
) %>%
  mutate(
    treatment = factor(paste(genotype, temp, sep = "_"),
                       levels = c("H2_25", "H2_32", "VWA_25", "VWA_32")),
    .group = str_trim(.group)
  ) %>%
  left_join(
    final_tentacle_nonzero %>%
      group_by(cohort) %>%
      summarise(y = max(tentacle_count, na.rm = TRUE) * 1.08, .groups = "drop"),
    by = "cohort"
  )

p_final_tentacles_nonzero <- ggplot(
  final_tentacle_nonzero,
  aes(x = treatment, y = tentacle_count, color = treatment)
) +
  geom_boxplot(outlier.shape = NA, width = 0.6) +
  geom_jitter(
    width = 0.15,
    size = 2.8,
    alpha = 0.7
  ) +
  geom_text(
    data = cld_tent_nonzero_df,
    aes(x = treatment, y = y, label = .group),
    color = "black",
    size = 5,
    inherit.aes = FALSE
  ) +
  facet_wrap(~ cohort, scales = "free_y") +
  coord_cartesian(ylim = c(0, 27)) +
  scale_color_manual(values = my_colors) +
  labs(
    x = "Treatment",
    y = "Final tentacle count (non-zero only)"
  ) +
  my_theme +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5)
  )

p_final_tentacles_nonzero

# ---------------------------------------------------------
# 2.14 Max tentacle count by treatment across whole experiment
# ---------------------------------------------------------

max_tentacles <- final_tentacle_dat %>%
  group_by(treatment, genotype, temp) %>%
  summarise(
    max_tentacles = max(tentacle_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(genotype, temp)

print(max_tentacles)


# ---------------------------------------------------------
# 2.15 Wide table of max tentacle count
# ---------------------------------------------------------

max_tentacles_wide <- max_tentacles %>%
  dplyr::select(genotype, temp, max_tentacles) %>%
  dplyr::mutate(temp = as.character(temp)) %>%
  tidyr::pivot_wider(
    names_from = temp,
    values_from = max_tentacles
  ) %>%
  dplyr::rename(
    `25 °C` = `25`,
    `32 °C` = `32`
  ) %>%
  dplyr::mutate(
    Change = `32 °C` - `25 °C`
  )

print(max_tentacles_wide)

# =========================================================
# =========================================================
# SECTION 3. LACERATE SYMBIONT AREA DATA
# =========================================================
# =========================================================
# Question:
#   How did mean symbiont area calculation per well vary by treatment?
#
# Data source:
#   natural_lacerate_sym_density.csv
#
# Main outputs:
#   - cleaned symbiont area dataset
#   - Symbiont Area-only subset
#   - mean calculation per well
#   - treatment-level plot with Tukey letters
# =========================================================

# ---------------------------------------------------------
# 3.1 Read lacerate symbiont density data
# ---------------------------------------------------------

lacerate_sym_density <- read_csv(
  "~/Documents/GitHub/heatinglacerate/natural/data/natural_lacerate_sym_density  - W7-8.csv",
  show_col_types = FALSE
) %>%
  clean_names() %>%
  mutate(
    label = as.character(label),
    region = as.character(region),
    plate = as.character(plate),
    notes = na_if(str_trim(as.character(notes)), ""),
    calculation = as.character(calculation),
    well = str_trim(as.character(well)),
    type = str_trim(as.character(type)),
    symbiotic_state = str_trim(as.character(symbiotic_state)),
    
    area = parse_number(as.character(area)),
    min_threshold = parse_number(as.character(min_threshold)),
    max_threshold = parse_number(as.character(max_threshold)),
    percent_raw = parse_number(as.character(percent_raw)),
    
    treatment = str_trim(as.character(treatment)),
    genotype = str_trim(as.character(genotype)),
    temperature = str_trim(as.character(temperature))
  ) %>%
  filter(
    treatment != "#VALUE!",
    genotype != "#VALUE!",
    temperature != "#VALUE!"
  ) %>%
  mutate(
    treatment = factor(treatment),
    genotype = factor(genotype),
    temperature = factor(temperature)
  )

print(lacerate_sym_density)

# Optional checks
lacerate_sym_density %>%
  count(treatment, sort = TRUE)

lacerate_sym_density %>%
  count(genotype, sort = TRUE)

lacerate_sym_density %>%
  count(temperature, sort = TRUE)

lacerate_sym_density %>%
  summarise(
    missing_area = sum(is.na(area)),
    missing_percent_raw = sum(is.na(percent_raw)),
    missing_well = sum(is.na(well) | well == "")
  )

str(lacerate_sym_density)


# ---------------------------------------------------------
# 3.2 Keep only Symbiont Area rows
# ---------------------------------------------------------

symbiont_area_only <- lacerate_sym_density %>%
  filter(type == "Symbiont Area") %>%
  mutate(
    calculation_num = parse_number(as.character(calculation))
  )

print(symbiont_area_only)

# Optional checks
symbiont_area_only %>%
  count(treatment, sort = TRUE)

symbiont_area_only %>%
  summarise(
    missing_calculation = sum(is.na(calculation_num)),
    missing_well = sum(is.na(well) | well == "")
  )


# ---------------------------------------------------------
# 3.3 Average calculation within each well
# ---------------------------------------------------------

symbiont_area_well_avg <- symbiont_area_only %>%
  group_by(treatment, genotype, temperature, well) %>%
  summarise(
    mean_calculation = mean(calculation_num, na.rm = TRUE),
    n_images = n(),
    .groups = "drop"
  )

print(symbiont_area_well_avg)

# Optional checks
symbiont_area_well_avg %>%
  count(treatment, sort = TRUE)

symbiont_area_well_avg %>%
  summarise(
    min_mean = min(mean_calculation, na.rm = TRUE),
    max_mean = max(mean_calculation, na.rm = TRUE),
    mean_overall = mean(mean_calculation, na.rm = TRUE)
  )


# ---------------------------------------------------------
# 3.4 Tukey letters for treatment plot
# ---------------------------------------------------------

mod_sym_calc <- lm(mean_calculation ~ treatment, data = symbiont_area_well_avg)

print(Anova(mod_sym_calc, type = 2))

emm_sym_calc <- emmeans(mod_sym_calc, ~ treatment)
print(emm_sym_calc)
print(pairs(emm_sym_calc, adjust = "tukey"))

cld_sym_calc <- multcomp::cld(
  emm_sym_calc,
  Letters = letters,
  adjust = "tukey"
)

cld_sym_calc_df <- as.data.frame(cld_sym_calc) %>%
  mutate(
    .group = stringr::str_trim(.group)
  )

sym_calc_ypos <- symbiont_area_well_avg %>%
  group_by(treatment) %>%
  summarise(
    y = max(mean_calculation, na.rm = TRUE) * 1.08,
    .groups = "drop"
  )

cld_sym_calc_df <- cld_sym_calc_df %>%
  left_join(sym_calc_ypos, by = "treatment")


# ---------------------------------------------------------
# 3.5 Colors for symbiont area plot
# ---------------------------------------------------------

sym_density_colors <- c(
  "H2-25" = "#3B6FB6",
  "H2-32" = "#D62728",
  "VWA-25" = "#7B6FD0",
  "VWA-32" = "#8C564B"
)


# ---------------------------------------------------------
# 3.6 Plot average calculation by treatment
# ---------------------------------------------------------

p_symbiont_calc <- ggplot(
  symbiont_area_well_avg,
  aes(x = treatment, y = mean_calculation, color = treatment)
) +
  geom_boxplot(
    width = 0.6,
    fill = NA,
    linewidth = 1.2,
    outlier.shape = NA
  ) +
  geom_jitter(
    width = 0.12,
    size = 2.8,
    alpha = 0.7
  ) +
  geom_text(
    data = cld_sym_calc_df,
    aes(x = treatment, y = y, label = .group),
    color = "black",
    size = 5,
    inherit.aes = FALSE
  ) +
  scale_color_manual(values = sym_density_colors) +
  labs(
    x = "Treatment",
    y = "Mean symbiont area"
  ) +
  my_theme +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.position = "none"
  )

p_symbiont_calc


p_symbiont_calc + p_symbiont_calc

# =========================================================
# =========================================================
# SECTION 4. PARENT PHYSIOLOGY
# =========================================================
# =========================================================
# Question:
#   How did temperature affect parent physiology over time?
#
# Data source:
#   natural_lacerate_aiptasia_experiment_datasheets - parent_weekly.csv
#
# Main outputs:
#   - pedal disc diameter through time
#   - Fv/Fm through time
# =========================================================

show_letters <- TRUE

# ---------------------------------------------------------
# 4.1 Read parent weekly data
# ---------------------------------------------------------

parent_weekly <- read_csv(
  "~/Documents/GitHub/heatinglacerate/natural/data/natural_lacerate_aiptasia_experiment_datasheets - parent_weekly.csv",
  show_col_types = FALSE
) %>%
  clean_names() %>%
  mutate(
    date = ymd(date),
    week = factor(week, levels = paste0("W", 1:10)),
    
    genotype = str_trim(as.character(genotype)),
    temp = str_trim(as.character(temp)),
    
    temp = case_when(
      temp %in% c("25", "25C", "25 c", "25 C") ~ "25",
      temp %in% c("32", "32C", "32 c", "32 C") ~ "32",
      TRUE ~ temp
    ),
    
    treatment = paste0(genotype, "_", temp),
    treatment = factor(
      treatment,
      levels = c("H2_25", "H2_32", "VWA_25", "VWA_32")
    ),
    
    pedal_disc_diameter_mm = parse_number(as.character(pedal_disc_diameter_mm)),
    pedal_disc_area_mm2 = parse_number(as.character(pedal_disc_area_mm2)),
    fv_fm = parse_number(as.character(fv_fm))
  ) %>%
  filter(
    !is.na(week),
    !is.na(genotype),
    !is.na(temp),
    !is.na(treatment)
  )

print(parent_weekly)

parent_weekly %>%
  count(treatment, week)

parent_weekly %>%
  summarise(
    missing_diameter = sum(is.na(pedal_disc_diameter_mm)),
    missing_area = sum(is.na(pedal_disc_area_mm2)),
    missing_fvfm = sum(is.na(fv_fm))
  )


# ---------------------------------------------------------
# 4.2 Summarize parent physiology by week and treatment
# ---------------------------------------------------------

parent_summary <- parent_weekly %>%
  group_by(week, treatment) %>%
  summarise(
    n_size = sum(!is.na(pedal_disc_diameter_mm)),
    mean_size = ifelse(n_size > 0, mean(pedal_disc_diameter_mm, na.rm = TRUE), NA_real_),
    se_size = ifelse(n_size > 1, sd(pedal_disc_diameter_mm, na.rm = TRUE) / sqrt(n_size), NA_real_),
    
    n_area = sum(!is.na(pedal_disc_area_mm2)),
    mean_area = ifelse(n_area > 0, mean(pedal_disc_area_mm2, na.rm = TRUE), NA_real_),
    se_area = ifelse(n_area > 1, sd(pedal_disc_area_mm2, na.rm = TRUE) / sqrt(n_area), NA_real_),
    
    n_fvfm = sum(!is.na(fv_fm)),
    mean_fvfm = ifelse(n_fvfm > 0, mean(fv_fm, na.rm = TRUE), NA_real_),
    se_fvfm = ifelse(n_fvfm > 1, sd(fv_fm, na.rm = TRUE) / sqrt(n_fvfm), NA_real_),
    
    .groups = "drop"
  )

print(parent_summary)

# ---------------------------------------------------------
# 4.3 Colors and theme
# ---------------------------------------------------------

parent_colors <- c(
  "H2_25" = "#3B6FB6",
  "H2_32" = "#E41A1C",
  "VWA_25" = "#7B6FD0",
  "VWA_32" = "#8C564B"
)

parent_theme <- theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.position = "right",
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12)
  )


# ---------------------------------------------------------
# 4.4 Tukey letters for parent physiology plots
# ---------------------------------------------------------

letters_diameter <- parent_weekly %>%
  filter(!is.na(pedal_disc_diameter_mm)) %>%
  group_by(week) %>%
  group_modify(~{
    dat <- .x
    
    if (n_distinct(dat$treatment) < 2) {
      dat %>%
        distinct(treatment) %>%
        mutate(.group = "a")
    } else {
      mod <- lm(pedal_disc_diameter_mm ~ treatment, data = dat)
      em <- emmeans(mod, ~ treatment)
      
      multcomp::cld(em, Letters = letters, adjust = "tukey") %>%
        as.data.frame() %>%
        dplyr::select(treatment, .group) %>%
        mutate(.group = str_trim(.group))
    }
  }) %>%
  ungroup()

diameter_range <- diff(range(parent_summary$mean_size + parent_summary$se_size, na.rm = TRUE))
diameter_offset <- diameter_range * 0.06

diameter_letter_pos <- parent_summary %>%
  group_by(week) %>%
  summarise(
    week_top = max(mean_size + se_size, na.rm = TRUE),
    .groups = "drop"
  )

letters_diameter <- letters_diameter %>%
  mutate(treatment_rank = as.numeric(treatment)) %>%
  left_join(diameter_letter_pos, by = "week") %>%
  mutate(y = week_top + diameter_offset * treatment_rank)


letters_area <- parent_weekly %>%
  filter(
    week %in% c("W2", "W4", "W6", "W8", "W10"),
    !is.na(pedal_disc_area_mm2)
  ) %>%
  group_by(week) %>%
  group_modify(~{
    dat <- .x
    
    if (n_distinct(dat$treatment) < 2) {
      dat %>%
        distinct(treatment) %>%
        mutate(.group = "a")
    } else {
      mod <- lm(pedal_disc_area_mm2 ~ treatment, data = dat)
      em <- emmeans(mod, ~ treatment)
      
      multcomp::cld(em, Letters = letters, adjust = "tukey") %>%
        as.data.frame() %>%
        dplyr::select(treatment, .group) %>%
        mutate(.group = str_trim(.group))
    }
  }) %>%
  ungroup()

area_range <- diff(range(parent_summary$mean_area + parent_summary$se_area, na.rm = TRUE))
area_offset <- area_range * 0.06

area_letter_pos <- parent_summary %>%
  filter(week %in% c("W2", "W4", "W6", "W8", "W10")) %>%
  group_by(week) %>%
  summarise(
    week_top = max(mean_area + se_area, na.rm = TRUE),
    .groups = "drop"
  )

letters_area <- letters_area %>%
  mutate(treatment_rank = as.numeric(treatment)) %>%
  left_join(area_letter_pos, by = "week") %>%
  mutate(y = week_top + area_offset * treatment_rank)


letters_fvfm <- parent_weekly %>%
  filter(!is.na(fv_fm)) %>%
  group_by(week) %>%
  group_modify(~{
    dat <- .x
    
    if (n_distinct(dat$treatment) < 2) {
      dat %>%
        distinct(treatment) %>%
        mutate(.group = "a")
    } else {
      mod <- lm(fv_fm ~ treatment, data = dat)
      em <- emmeans(mod, ~ treatment)
      
      multcomp::cld(em, Letters = letters, adjust = "tukey") %>%
        as.data.frame() %>%
        dplyr::select(treatment, .group) %>%
        mutate(.group = str_trim(.group))
    }
  }) %>%
  ungroup()

fvfm_range <- diff(range(parent_summary$mean_fvfm + parent_summary$se_fvfm, na.rm = TRUE))
fvfm_offset <- fvfm_range * 0.06

fvfm_letter_pos <- parent_summary %>%
  group_by(week) %>%
  summarise(
    week_top = max(mean_fvfm + se_fvfm, na.rm = TRUE),
    .groups = "drop"
  )

letters_fvfm <- letters_fvfm %>%
  mutate(treatment_rank = as.numeric(treatment)) %>%
  left_join(fvfm_letter_pos, by = "week") %>%
  mutate(y = week_top + fvfm_offset * treatment_rank)


# ---------------------------------------------------------
# 4.5 Pedal disc diameter plot
# ---------------------------------------------------------

weeks_to_plot_diameter <- c("W1", "W2", "W3", "W4", "W5", "W6", "W7", "W8", "W9", "W10")

p_diameter_base <- parent_summary %>%
  filter(week %in% weeks_to_plot_diameter) %>%
  ggplot(
    aes(x = week, y = mean_size, color = treatment, group = treatment)
  ) +
  geom_line(linewidth = 1, na.rm = TRUE) +
  geom_point(size = 3, na.rm = TRUE) +
  geom_errorbar(
    aes(ymin = mean_size - se_size, ymax = mean_size + se_size),
    width = 0.15,
    linewidth = 0.6,
    na.rm = TRUE
  ) +
  scale_color_manual(values = parent_colors) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.25))) +
  labs(
    x = "Week",
    y = "Pedal disc diameter (mm)",
    color = "Treatment"
  ) +
  parent_theme

p_diameter_letters <- p_diameter_base +
  geom_text(
    data = letters_diameter %>% filter(week %in% weeks_to_plot_diameter),
    aes(x = week, y = y, label = .group, color = treatment, group = treatment),
    position = position_dodge(width = 0.45),
    size = 4,
    fontface = "bold",
    show.legend = FALSE,
    na.rm = TRUE
  )

p_diameter <- if (show_letters) p_diameter_letters else p_diameter_base


# ---------------------------------------------------------
# 4.6 Pedal disc area plot
# ---------------------------------------------------------

weeks_to_plot_area <- c("W2", "W4", "W6", "W8", "W10")
#weeks_to_plot_area <- c("W1", "W2", "W3", "W4", "W5", "W6", "W7", "W8", "W9", "W10")

p_area_base <- parent_summary %>%
  filter(week %in% weeks_to_plot_area) %>%
  ggplot(
    aes(x = week, y = mean_area, color = treatment, group = treatment)
  ) +
  geom_line(linewidth = 1, na.rm = TRUE) +
  geom_point(size = 3, na.rm = TRUE) +
  geom_errorbar(
    aes(ymin = mean_area - se_area, ymax = mean_area + se_area),
    width = 0.15,
    linewidth = 0.6,
    na.rm = TRUE
  ) +
  coord_cartesian(ylim = c(25, 85)) +   # <- this controls visible y range
  scale_color_manual(values = parent_colors) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.25))) +
  labs(
    x = "Week",
    y = expression(paste("Pedal disc area (mm"^2, ")")),
    color = "Treatment"
  ) +
  parent_theme

p_area_letters <- p_area_base +
  geom_text(
    data = letters_area %>% filter(week %in% weeks_to_plot_area),
    aes(x = week, y = y, label = .group, color = treatment, group = treatment),
    position = position_dodge(width = 0.45),
    size = 4,
    fontface = "bold",
    show.legend = FALSE,
    na.rm = TRUE
  )
  
p_area <- if (show_letters) p_area_letters else p_area_base
p_area
p_area_base
p_area_letters

# ---------------------------------------------------------
# 4.7 Fv/Fm plot
# ---------------------------------------------------------

weeks_to_plot_fvfm <- c("W1", "W2", "W3", "W4", "W5", "W6", "W7", "W8", "W9", "W10")

p_fvfm_base <- parent_summary %>%
  filter(week %in% weeks_to_plot_fvfm) %>%
  ggplot(
    aes(x = week, y = mean_fvfm, color = treatment, group = treatment)
  ) +
  geom_line(linewidth = 1, na.rm = TRUE) +
  geom_point(size = 3, na.rm = TRUE) +
  geom_errorbar(
    aes(ymin = mean_fvfm - se_fvfm, ymax = mean_fvfm + se_fvfm),
    width = 0.15,
    linewidth = 0.6,
    na.rm = TRUE
  ) +
  scale_color_manual(values = parent_colors) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.25))) +
  labs(
    x = "Week",
    y = "Fv/Fm",
    color = "Treatment"
  ) +
  parent_theme

p_fvfm_letters <- p_fvfm_base +
  geom_text(
    data = letters_fvfm %>% filter(week %in% weeks_to_plot_fvfm),
    aes(x = week, y = y, label = .group, color = treatment, group = treatment),
    position = position_dodge(width = 0.45),
    size = 4,
    fontface = "bold",
    show.legend = FALSE,
    na.rm = TRUE
  )

p_fvfm <- if (show_letters) p_fvfm_letters else p_fvfm_base


# ---------------------------------------------------------
# 4.8 Combined plots
# ---------------------------------------------------------

parent_combined_plot_no_letters <- p_area_base / p_diameter_base / p_fvfm_base +
  plot_annotation(tag_levels = "A")

parent_combined_plot_letters <- p_area_letters / p_diameter_letters / p_fvfm_letters +
  plot_annotation(tag_levels = "A")

parent_combined_plot <- if (show_letters) {
  parent_combined_plot_letters
} else {
  parent_combined_plot_no_letters
}


parent_combined_plot_no_letters_sizeonly <- p_area_base / p_diameter_base +
  plot_annotation(tag_levels = "A")

parent_combined_plot_letters_sizeonly <- p_area_letters / p_diameter_letters +
  plot_annotation(tag_levels = "A")

parent_combined_plot_sizeonly <- if (show_letters) {
  parent_combined_plot_letters_sizeonly
} else {
  parent_combined_plot_no_letters_sizeonly
}


# ---------------------------------------------------------
# 4.9 Print plots
# ---------------------------------------------------------

cat("\n\n==============================\n")
cat("PARENT PHYSIOLOGY WITHOUT LETTERS\n")
cat("==============================\n")

print(p_area_base)
print(p_diameter_base)
print(p_fvfm_base)
print(parent_combined_plot_no_letters)


cat("\n\n==============================\n")
cat("PARENT PHYSIOLOGY WITH TUKEY LETTERS\n")
cat("==============================\n")

print(p_area_letters)
print(p_diameter_letters)
print(p_fvfm_letters)
print(parent_combined_plot_letters)

# =========================================================
# SECTION 5. TOTAL TENTACLES COUNTED (UNIQUE vs ALL COUNTS)
# =========================================================
# Question:
#   How many tentacles were counted across the experiment?
#   1) Using one value per lacerate (final count)
#   2) Using all repeated measurements
# =========================================================

cat("\n==============================\n")
cat("SECTION 5: TOTAL TENTACLES COUNTED\n")
cat("==============================\n")

# ---------------------------------------------------------
# 5.1 Final tentacle count per lacerate (unique individuals)
# ---------------------------------------------------------

final_tentacles_per_lacerate <- lacerate_counts %>%
  filter(!is.na(lacerate_id), !is.na(tentacle_count)) %>%
  arrange(lacerate_id, date) %>%
  group_by(lacerate_id, treatment) %>%
  slice_tail(n = 1) %>%
  ungroup()


# ---------------------------------------------------------
# 5.2 Total tentacles (unique lacerates only)
# ---------------------------------------------------------

total_unique_tentacles <- final_tentacles_per_lacerate %>%
  group_by(treatment) %>%
  summarise(
    n_unique_lacerates = n(),
    total_tentacles_unique = sum(tentacle_count, na.rm = TRUE),
    .groups = "drop"
  )


# ---------------------------------------------------------
# 5.3 Total tentacles (all repeated counts)
# ---------------------------------------------------------

total_all_counts <- lacerate_counts %>%
  filter(!is.na(lacerate_id), !is.na(tentacle_count)) %>%
  group_by(treatment) %>%
  summarise(
    total_tentacles_all_counts = sum(tentacle_count, na.rm = TRUE),
    n_total_observations = n(),
    .groups = "drop"
  )


# ---------------------------------------------------------
# 5.4 Combine and add derived metrics
# ---------------------------------------------------------

tentacle_totals <- total_unique_tentacles %>%
  left_join(total_all_counts, by = "treatment") %>%
  mutate(
    avg_tentacles_per_animal = total_tentacles_unique / n_unique_lacerates,
    repeat_multiplier = total_tentacles_all_counts / total_tentacles_unique
  )


# ---------------------------------------------------------
# 5.5 Add grand total row
# ---------------------------------------------------------

grand_row <- tentacle_totals %>%
  summarise(
    treatment = "Total",
    n_unique_lacerates = sum(n_unique_lacerates),
    total_tentacles_unique = sum(total_tentacles_unique),
    avg_tentacles_per_animal = total_tentacles_unique / n_unique_lacerates,
    total_tentacles_all_counts = sum(total_tentacles_all_counts),
    n_total_observations = sum(n_total_observations),
    repeat_multiplier = total_tentacles_all_counts / total_tentacles_unique
  )


# ---------------------------------------------------------
# 5.6 Final table
# ---------------------------------------------------------

tentacle_totals_final <- bind_rows(tentacle_totals, grand_row) %>%
  rename(
    Group = treatment,
    `Number of animals` = n_unique_lacerates,
    `Total tentacles (final per animal)` = total_tentacles_unique,
    `Average tentacles per animal` = avg_tentacles_per_animal,
    `Total tentacles counted (all timepoints)` = total_tentacles_all_counts,
    `Number of measurements` = n_total_observations,
    `Average times each tentacle was counted` = repeat_multiplier
  )

print(tentacle_totals_final)

# =========================================================
# Reprint all the plots
# =========================================================

plot_list <- list(
  p_lacerates_tub,
  p_lacerates_parent,
  p_final_rate,
  p_final_rate_nozeros,
  p_final_tentacles,
  p_final_tentacles_nonzero,
  p_symbiont_calc,
  p_area,
  p_diameter,
  p_fvfm,
  parent_combined_plot
)

lapply(plot_list, print)


p_lacerates_tub
p_final_rate
p_final_rate_nozeros
p_final_tentacles_nonzero

p_symbiont_calc
p_symbiont_calc + p_symbiont_calc



p_area
p_diameter
p_fvfm
parent_combined_plot_sizeonly
parent_combined_plot
p_fvfm

p_area_no_legend <- p_area + theme(legend.position = "none")
p_fvfm_no_legend <- p_fvfm + theme(legend.position = "none")


p_area_no_legend
p_fvfm_no_legend


ggsave(
  filename = "p_area_legend.png",
  plot = p_area,
  path = "~/Documents/GitHub/heatinglacerate/natural/figs",
  width = 6.1,
  height = 3.6,
  units = "in",
  dpi = 1200,
  bg = "white"
)
