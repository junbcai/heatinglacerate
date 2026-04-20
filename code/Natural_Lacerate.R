# =========================================================
# PROJECT: Thermal stress and pedal laceration experiment
# PURPOSE:
#   Organize analyses into 3 main sections:
#   1) Number of lacerates produced
#   2) Tentacle rate data
#   3) Parent physiology
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


# =========================================================
# 1. HOUSEKEEPING
# =========================================================

rm(list = ls())
graphics.off()

# Optional:
# setwd("~/Downloads/...")

# =========================================================
# 2. COMMON PLOT SETTINGS
# =========================================================

my_theme <- theme_classic(base_size = 18) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
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
  "VWA_25" = "#3B6FB6",
  "VWA_32" = "#D62728"
)

my_shapes <- c(
  "H2" = 16,
  "VWA" = 15
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
  "~/Downloads/natural_lacerate_aiptasia_experiment_datasheets - lacerates_metadata.csv"
) %>%
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


# ---------------------------------------------------------
# 1.2 Keep one row per unique lacerate
# ---------------------------------------------------------

lacerate_unique <- lacerate_meta %>%
  filter(!is.na(lacerate_id), lacerate_id != "") %>%
  distinct(lacerate_id, cohort, treatment, genotype, temp, tub_id, parent_id)


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
    aes(shape = genotype),
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
  scale_shape_manual(name = "genotype", values = my_shapes) +
  scale_color_manual(name = "treatment", values = my_colors) +
  labs(
    x = "Treatment",
    y = "Number of lacerates per tub"
  ) +
  guides(
    color = guide_legend(order = 2),
    shape = guide_legend(order = 1)
  ) +
  my_theme

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
    aes(shape = genotype),
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
  scale_shape_manual(name = "Genotype", values = my_shapes) +
  scale_color_manual(name = "Treatment", values = my_colors) +
  labs(
    x = "Treatment",
    y = "Number of lacerates per parent"
  ) +
  guides(
    color = guide_legend(order = 1),
    shape = guide_legend(order = 2)
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
# Question:
#   How did temperature affect lacerate development?
#
# Data source:
#   natural_lacerate_aiptasia_experiment_datasheets - lacerate_counts-6.csv
#
# Main outputs:
#   - final tentacle growth rate
#   - final tentacle count
#   - max tentacle count by treatment
# =========================================================


# ---------------------------------------------------------
# 2.1 Read lacerate development data
# ---------------------------------------------------------

lacerate_counts <- read_csv(
  "~/Downloads/natural_lacerate_aiptasia_experiment_datasheets - lacerate_counts-6.csv"
) %>%
  clean_names() %>%
  mutate(
    date = ymd(date),
    cohort = factor(cohort, levels = c("W1-2", "W3-4", "W5-6", "W7-8", "W9-10")),
    treatment = factor(treatment, levels = c("H2_25", "H2_32", "VWA_25", "VWA_32")),
    genotype = factor(genotype, levels = c("H2", "VWA")),
    temp = factor(temp, levels = c("25", "32")),
    lacerate_id_og = as.character(lacerate_id_og),
    rate = as.numeric(rate),
    rate2 = as.numeric(rate2),
    tentacle_count = as.numeric(tentacle_count)
  )


# ---------------------------------------------------------
# 2.2 Final rate per lacerate within each cohort
# ---------------------------------------------------------

final_rate_dat <- lacerate_counts %>%
  filter(!is.na(rate2)) %>%
  arrange(lacerate_id_og, cohort, date) %>%
  group_by(lacerate_id_og, cohort, treatment, genotype, temp) %>%
  slice_tail(n = 1) %>%
  ungroup()

print(final_rate_dat)


# ---------------------------------------------------------
# 2.3 Rate model
# ---------------------------------------------------------

cat("\n==============================\n")
cat("SECTION 2: RATE2 MODEL\n")
cat("==============================\n")

mod_rate <- lm(rate2 ~ genotype * temp * cohort, data = final_rate_dat)

print(Anova(mod_rate, type = 2))

par(mfrow = c(2, 2))
plot(mod_rate)
par(mfrow = c(1, 1))

print(shapiro.test(residuals(mod_rate)))
print(car::leveneTest(rate2 ~ genotype * temp * cohort, data = final_rate_dat))

emm_rate <- emmeans(mod_rate, ~ genotype * temp | cohort)
print(emm_rate)
print(pairs(emm_rate, adjust = "tukey"))


# ---------------------------------------------------------
# 2.4 Tukey letters for final rate plot
# ---------------------------------------------------------

cld_rate <- cld(
  emm_rate,
  Letters = letters,
  adjust = "tukey"
)

cld_rate_df <- as.data.frame(cld_rate) %>%
  mutate(
    treatment = factor(
      paste(genotype, temp, sep = "_"),
      levels = c("H2_25", "H2_32", "VWA_25", "VWA_32")
    ),
    .group = str_trim(.group)
  )

rate_ypos <- final_rate_dat %>%
  group_by(cohort) %>%
  summarise(
    y = max(rate2, na.rm = TRUE) * 1.08,
    .groups = "drop"
  )

cld_rate_df <- cld_rate_df %>%
  left_join(rate_ypos, by = "cohort")


# ---------------------------------------------------------
# 2.5 Plot final rate per lacerate
# ---------------------------------------------------------

p_final_rate <- ggplot(
  final_rate_dat,
  aes(x = treatment, y = rate2, color = treatment)
) +
  geom_boxplot(outlier.shape = NA, width = 0.6) +
  geom_jitter(
    aes(shape = genotype),
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
  scale_color_manual(values = my_colors) +
  scale_shape_manual(values = my_shapes) +
  labs(
    x = "Treatment",
    y = "Final tentacle growth rate per day"
  ) +
  my_theme

p_final_rate


# ---------------------------------------------------------
# 2.5.1 Remove zero rates
# ---------------------------------------------------------

final_rate_dat_no0 <- final_rate_dat %>%
  dplyr::filter(!is.na(rate2), rate2 != 0)

cat("\n==============================\n")
cat("SECTION 2.5.1: RATE2 MODEL (zeros removed)\n")
cat("==============================\n")

print(table(final_rate_dat$rate2 == 0, useNA = "ifany"))
print(table(final_rate_dat_no0$cohort, final_rate_dat_no0$treatment))


# ---------------------------------------------------------
# 2.5.2 Rate model
# ---------------------------------------------------------

mod_rate <- lm(rate2 ~ genotype * temp * cohort, data = final_rate_dat_no0)

print(car::Anova(mod_rate, type = 2))

par(mfrow = c(2, 2))
plot(mod_rate)
par(mfrow = c(1, 1))

print(shapiro.test(residuals(mod_rate)))
print(car::leveneTest(rate2 ~ genotype * temp * cohort, data = final_rate_dat_no0))

emm_rate <- emmeans::emmeans(mod_rate, ~ genotype * temp | cohort)
print(emm_rate)
print(pairs(emm_rate, adjust = "tukey"))


# ---------------------------------------------------------
# 2.5.3 Tukey letters for final rate plot
# ---------------------------------------------------------

cld_rate <- multcomp::cld(
  emm_rate,
  Letters = letters,
  adjust = "tukey"
)

cld_rate_df <- as.data.frame(cld_rate) %>%
  dplyr::mutate(
    treatment = factor(
      paste(genotype, temp, sep = "_"),
      levels = c("H2_25", "H2_32", "VWA_25", "VWA_32")
    ),
    .group = stringr::str_trim(.group)
  )


# ---------------------------------------------------------
# 2.5.4 Calculate letter positions
# ---------------------------------------------------------

rate_ypos <- final_rate_dat_no0 %>%
  dplyr::group_by(cohort) %>%
  dplyr::summarise(
    y = max(rate2, na.rm = TRUE) * 1.08,
    .groups = "drop"
  )


# ---------------------------------------------------------
# 2.5.5 Join letters with positions
# ---------------------------------------------------------

cld_rate_df <- cld_rate_df %>%
  dplyr::left_join(rate_ypos, by = "cohort")


# ---------------------------------------------------------
# 2.5.6 Plot final rate per lacerate
# ---------------------------------------------------------

p_final_rate_nozeros <- ggplot(
  final_rate_dat_no0,
  aes(x = treatment, y = rate2, color = treatment)
) +
  geom_boxplot(outlier.shape = NA, width = 0.6) +
  geom_jitter(
    aes(shape = genotype),
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
  scale_color_manual(values = my_colors) +
  scale_shape_manual(values = my_shapes) +
  labs(
    x = "Treatment",
    y = "Final tentacle growth rate per day"
  ) +
  my_theme

p_final_rate_nozeros

final_rate_dat_no0 %>%
  dplyr::group_by(cohort, treatment) %>%
  dplyr::summarise(
    n_zero = sum(rate2 == 0, na.rm = TRUE),
    min_val = min(rate2, na.rm = TRUE),
    .groups = "drop"
  )
# ---------------------------------------------------------
# 2.5.7 Optional check of sample sizes after zero removal
# ---------------------------------------------------------

final_rate_dat_no0 %>%
  dplyr::count(cohort, treatment)


# ---------------------------------------------------------
# 2.5.8 Optional check of mean and SD after zero removal
# ---------------------------------------------------------

final_rate_dat_no0 %>%
  dplyr::group_by(cohort, treatment) %>%
  dplyr::summarise(
    n = dplyr::n(),
    mean_rate2 = mean(rate2, na.rm = TRUE),
    sd_rate2 = sd(rate2, na.rm = TRUE),
    .groups = "drop"
  )


# ---------------------------------------------------------
# 2.6 Final tentacle count per lacerate within each cohort
# ---------------------------------------------------------

final_tentacle_dat <- lacerate_counts %>%
  filter(!is.na(tentacle_count)) %>%
  arrange(lacerate_id_og, cohort, date) %>%
  group_by(lacerate_id_og, cohort, treatment, genotype, temp) %>%
  slice_tail(n = 1) %>%
  ungroup()

print(final_tentacle_dat)


# ---------------------------------------------------------
# 2.7 Final tentacle count, non-zero only
# ---------------------------------------------------------

final_tentacle_nonzero <- final_tentacle_dat %>%
  filter(tentacle_count > 0)

print(final_tentacle_nonzero)


# ---------------------------------------------------------
# 2.8 Tentacle count model with zeros
# ---------------------------------------------------------

cat("\n==============================\n")
cat("SECTION 2: TENTACLE COUNT MODEL (WITH ZEROS)\n")
cat("==============================\n")

mod_tent_zero <- glmmTMB(
  tentacle_count ~ genotype * temp * cohort,
  family = nbinom2(),
  data = final_tentacle_dat
)

sim_tent_zero <- simulateResiduals(mod_tent_zero)
plot(sim_tent_zero)
print(testDispersion(sim_tent_zero))
print(testZeroInflation(sim_tent_zero))
print(performance::check_overdispersion(mod_tent_zero))

print(Anova(mod_tent_zero, type = 2))

emm_tent_zero <- emmeans(mod_tent_zero, ~ genotype * temp | cohort, type = "response")
print(emm_tent_zero)
print(pairs(emm_tent_zero, adjust = "tukey"))


# ---------------------------------------------------------
# 2.9 Tukey letters for final tentacle count plot
# ---------------------------------------------------------

cld_tent_zero <- cld(
  emm_tent_zero,
  Letters = letters,
  adjust = "tukey"
)

cld_tent_zero_df <- as.data.frame(cld_tent_zero) %>%
  mutate(
    treatment = factor(
      paste(genotype, temp, sep = "_"),
      levels = c("H2_25", "H2_32", "VWA_25", "VWA_32")
    ),
    .group = str_trim(.group)
  )

tent_zero_ypos <- final_tentacle_dat %>%
  group_by(cohort) %>%
  summarise(
    y = max(tentacle_count, na.rm = TRUE) * 1.08,
    .groups = "drop"
  )

cld_tent_zero_df <- cld_tent_zero_df %>%
  left_join(tent_zero_ypos, by = "cohort")


# ---------------------------------------------------------
# 2.10 Plot final tentacle count with zeros
# ---------------------------------------------------------

p_final_tentacles <- ggplot(
  final_tentacle_dat,
  aes(x = treatment, y = tentacle_count, color = treatment)
) +
  geom_boxplot(outlier.shape = NA, width = 0.6) +
  geom_jitter(
    aes(shape = genotype),
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
  scale_color_manual(values = my_colors) +
  scale_shape_manual(values = my_shapes) +
  labs(
    x = "Treatment",
    y = "Final tentacle count"
  ) +
  my_theme

p_final_tentacles


# ---------------------------------------------------------
# 2.11 Tentacle count model, non-zero only
# ---------------------------------------------------------

cat("\n==============================\n")
cat("SECTION 2: TENTACLE COUNT MODEL (NONZERO ONLY)\n")
cat("==============================\n")

mod_tent_nonzero <- glmmTMB(
  tentacle_count ~ genotype * temp * cohort,
  family = nbinom2(),
  data = final_tentacle_nonzero
)

sim_tent_nonzero <- simulateResiduals(mod_tent_nonzero)
plot(sim_tent_nonzero)
print(testDispersion(sim_tent_nonzero))
print(testZeroInflation(sim_tent_nonzero))
print(performance::check_overdispersion(mod_tent_nonzero))

print(Anova(mod_tent_nonzero, type = 2))

emm_tent_nonzero <- emmeans(mod_tent_nonzero, ~ genotype * temp | cohort, type = "response")
print(emm_tent_nonzero)
print(pairs(emm_tent_nonzero, adjust = "tukey"))


# ---------------------------------------------------------
# 2.12 Tukey letters for final tentacle count plot, non-zero only
# ---------------------------------------------------------

cld_tent_nonzero <- cld(
  emm_tent_nonzero,
  Letters = letters,
  adjust = "tukey"
)

cld_tent_nonzero_df <- as.data.frame(cld_tent_nonzero) %>%
  mutate(
    treatment = factor(
      paste(genotype, temp, sep = "_"),
      levels = c("H2_25", "H2_32", "VWA_25", "VWA_32")
    ),
    .group = str_trim(.group)
  )

tent_nonzero_ypos <- final_tentacle_nonzero %>%
  group_by(cohort) %>%
  summarise(
    y = max(tentacle_count, na.rm = TRUE) * 1.08,
    .groups = "drop"
  )

cld_tent_nonzero_df <- cld_tent_nonzero_df %>%
  left_join(tent_nonzero_ypos, by = "cohort")


# ---------------------------------------------------------
# 2.13 Plot final tentacle count, non-zero only
# ---------------------------------------------------------

p_final_tentacles_nonzero <- ggplot(
  final_tentacle_nonzero,
  aes(x = treatment, y = tentacle_count, color = treatment)
) +
  geom_boxplot(outlier.shape = NA, width = 0.6) +
  geom_jitter(
    aes(shape = genotype),
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
  scale_color_manual(values = my_colors) +
  scale_shape_manual(values = my_shapes) +
  labs(
    x = "Treatment",
    y = "Final tentacle count (non-zero only)"
  ) +
  my_theme

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
# SECTION 3. PARENT PHYSIOLOGY
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


# ---------------------------------------------------------
# 3.1 Read parent weekly data
# ---------------------------------------------------------

parent_weekly <- read_csv(
  "~/Downloads/natural_lacerate_aiptasia_experiment_datasheets - parent_weekly.csv"
) %>%
  clean_names() %>%
  mutate(
    date = ymd(date),
    treatment = paste0(genotype, "_", temp),
    treatment = factor(treatment, levels = c("H2_25", "H2_32", "VWA_25", "VWA_32")),
    pedal_disc_diameter_mm = as.numeric(pedal_disc_diameter_mm),
    fv_fm = as.numeric(fv_fm)
  ) %>%
  filter(
    !is.na(date),
    !is.na(treatment)
  )

print(parent_weekly)


# ---------------------------------------------------------
# 3.2 Summarize parent physiology by date and treatment
# ---------------------------------------------------------

parent_summary <- parent_weekly %>%
  group_by(date, treatment) %>%
  summarise(
    n_size = sum(!is.na(pedal_disc_diameter_mm)),
    mean_size = mean(pedal_disc_diameter_mm, na.rm = TRUE),
    se_size = sd(pedal_disc_diameter_mm, na.rm = TRUE) / sqrt(n_size),
    
    n_fvfm = sum(!is.na(fv_fm)),
    mean_fvfm = mean(fv_fm, na.rm = TRUE),
    se_fvfm = sd(fv_fm, na.rm = TRUE) / sqrt(n_fvfm),
    
    .groups = "drop"
  ) %>%
  mutate(
    se_size = ifelse(is.nan(se_size), NA, se_size),
    se_fvfm = ifelse(is.nan(se_fvfm), NA, se_fvfm)
  )

print(parent_summary)


# ---------------------------------------------------------
# 3.3 Colors for parent physiology plots
# ---------------------------------------------------------

parent_colors <- c(
  "H2_25" = "#3B6FB6",
  "H2_32" = "#8C564B",
  "VWA_25" = "#4DAF4A",
  "VWA_32" = "#E41A1C"
)

parent_theme <- theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )


# ---------------------------------------------------------
# 3.4 Plot pedal disc diameter through time
# ---------------------------------------------------------

p_size <- ggplot(
  parent_summary,
  aes(x = date, y = mean_size, color = treatment, group = treatment)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(ymin = mean_size - se_size, ymax = mean_size + se_size),
    width = 0.2,
    linewidth = 0.6,
    na.rm = TRUE
  ) +
  scale_color_manual(values = parent_colors) +
  labs(
    x = "Date",
    y = "Pedal disc diameter (mm)",
    color = "Treatment"
  ) +
  parent_theme

p_size


# ---------------------------------------------------------
# 3.5 Plot Fv/Fm through time
# ---------------------------------------------------------

p_fvfm <- ggplot(
  parent_summary,
  aes(x = date, y = mean_fvfm, color = treatment, group = treatment)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(ymin = mean_fvfm - se_fvfm, ymax = mean_fvfm + se_fvfm),
    width = 0.2,
    linewidth = 0.6,
    na.rm = TRUE
  ) +
  scale_color_manual(values = parent_colors) +
  labs(
    x = "Date",
    y = "Fv/Fm",
    color = "Treatment"
  ) +
  parent_theme

p_fvfm


# ---------------------------------------------------------
# 3.6 Combine parent physiology plots
# ---------------------------------------------------------

parent_combined_plot <- p_size / p_fvfm +
  plot_annotation(tag_levels = "A")

parent_combined_plot


# =========================================================
# TOTAL LACERATES ACROSS ALL 10 WEEKS (TREATMENT-LEVEL)
# =========================================================

total_lacerates <- lacerate_meta %>%
  filter(!is.na(lacerate_id), lacerate_id != "") %>%
  distinct(lacerate_id, treatment, genotype, temp) %>%
  count(treatment, genotype, temp, name = "total_lacerates")

print(total_lacerates)

# add grand total
total_lacerates_with_total <- total_lacerates %>%
  bind_rows(
    total_lacerates %>%
      summarise(
        treatment = "Total",
        genotype = NA,
        temp = NA,
        total_lacerates = sum(total_lacerates)
      )
  )

print(total_lacerates_with_total)
