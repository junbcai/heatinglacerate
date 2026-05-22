library(dplyr)
library(ggplot2)
library(plotrix)
library(scales)
library(janitor)
library(lme4)
library(car)
library(emmeans)
library(ggh4x)

graphics.off()

setwd("~/Documents/GitHub/heating_lacerates_final")

select <- dplyr::select
filter <- dplyr::filter
mutate <- dplyr::mutate
lag <- dplyr::lag

### =========================
### 1. READ AND CLEAN 2024 DATA
### =========================

ursa2024_data <- read.csv("~/Documents/GitHub/heating_lacerates_final/data/2024_tentcount.csv") %>%
  clean_names() %>%
  mutate(
    tent_count = suppressWarnings(as.numeric(tent_count)),
    id = factor(id),
    plate = factor(plate),
    well = factor(well),
    line = factor(line),
    temp = factor(temp),
    treatment = as.character(treatment),
    symbiosis = factor(symbiosis),
    lacerate = factor(lacerate),
    day = as.numeric(day),
    day_cat = as.character(day_cat)
  ) %>%
  mutate(
    treatment = gsub("APO", "Apo", treatment),
    treatment = gsub("SYM", "Sym", treatment),
    treatment = gsub("INO", "Ino", treatment),
    treatment = gsub("25C$", "25", treatment),
    treatment = gsub("32C$", "32", treatment),
    day_cat = gsub("^([0-9]+)_day$", "day_\\1", day_cat)
  ) %>%
  mutate(
    treatment = factor(treatment),
    day_cat = factor(day_cat)
  )

str(ursa2024_data)
table(ursa2024_data$treatment)
table(ursa2024_data$day_cat)

### =========================
### 2. MORTALITY PLOT
### =========================
df_mortality <- ursa2024_data %>%
  filter(
    line == "H2",
    day_cat %in% c("day_14", "day_21"),
    symbiosis != "Inoc",
    !treatment %in% c("H2-Ino-25", "H2-Ino-32")
  ) %>%
  mutate(
    Mortality = ifelse(is.na(tent_count) | tent_count == 0, "Dead", "Alive"),
    dead = ifelse(Mortality == "Dead", 1, 0),
    Mortality = factor(Mortality, levels = c("Alive", "Dead")),
    day_cat = factor(day_cat, levels = c("day_14", "day_21"))
  )

nrow(df_mortality)
table(df_mortality$day_cat)
table(df_mortality$treatment)
table(df_mortality$Mortality)

# =========================
# Flipping the Survival Plot with nested facets
# generate both versions with shared data and shared theme
# =========================
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
mortality_plot_df_flipped <- df_mortality %>%
  mutate(
    sym_state = case_when(
      grepl("Apo", treatment) ~ "Aposymbiotic",
      grepl("Sym", treatment) ~ "Symbiotic"
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
p_survival_base <- ggplot(
  mortality_plot_df_flipped,
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
    labels = percent,
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
p_survival_flipped_outline <- p_survival_base +
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
p_survival_flipped_colorbar <- p_survival_base +
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
p_survival_flipped_outline
p_survival_flipped_colorbar

### =========================
### 3A. MORTALITY STATS
### =========================

mortality_glm <- glm(
  dead ~ temp * symbiosis,
  data = df_mortality,
  family = binomial
)

summary(mortality_glm)
car::Anova(mortality_glm, type = 3)

emm_mort_temp <- emmeans(mortality_glm, ~ temp | symbiosis)
emm_mort_sym <- emmeans(mortality_glm, ~ symbiosis | temp)
emm_mort_temp
emm_mort_sym

### =========================
### 3B. SURVIVAL STATS
### =========================

# Create survival variable (1 = alive, 0 = dead)
df_mortality <- df_mortality %>%
  mutate(
    alive = 1 - dead
  )

# Binomial GLM on survival
survival_glm <- glm(
  alive ~ temp * symbiosis,
  data = df_mortality,
  family = binomial
)

# Model summary and Type III ANOVA
summary(survival_glm)
car::Anova(survival_glm, type = 3)

# Estimated marginal means
emm_surv_temp <- emmeans(survival_glm, ~ temp | symbiosis)
emm_surv_sym  <- emmeans(survival_glm, ~ symbiosis | temp)

emm_surv_temp
emm_surv_sym


### =========================
### 4. SYMBIOTIC STATE PLOT
### =========================

data_means <- ursa2024_data %>%
  filter(line == "H2") %>%
  group_by(treatment, day) %>%
  summarise(
    mean = mean(tent_count, na.rm = TRUE),
    se = std.error(tent_count, na.rm = TRUE),
    .groups = "drop"
  )

my_theme <- theme_classic(base_size = 14) +
  theme(
    text = element_text(family = "sans", size = 14),
    axis.text = element_text(size = 14, colour = "black"),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14)
  )

p_sym_state_shape <- ggplot(
  data = data_means %>%
    filter(treatment %in% c("H2-Apo-25", "H2-Ino-25", "H2-Sym-25")),
  aes(x = day, y = mean, group = treatment,
      shape = treatment, color = treatment)
) +
  my_theme +
  
  # All lines blue
  geom_line(color = "#3B6FB6", linewidth = 0.9) +
  
  # Error bars colored by treatment
  geom_errorbar(
    aes(ymin = mean - se, ymax = mean + se),
    width = 0.2,
    linewidth = 0.6
  ) +
  
  # Points colored + shaped by treatment
  geom_point(size = 4) +
  
  annotate("text", x = 7,  y = 10.2, label = "***", size = 6) +
  annotate("text", x = 10, y = 12.0, label = "***", size = 6) +
  annotate("text", x = 11, y = 12.0, label = "**",  size = 6) +
  
  ylab("Mean tentacle number") +
  xlab("Days post laceration (dpl)") +
  
  scale_y_continuous(breaks = seq(0, 13, 2), limits = c(0, 13)) +
  scale_x_continuous(breaks = seq(min(data_means$day), max(data_means$day), 1)) +
  
  # Your original colors (now applied to points + error bars)
  scale_color_manual(
    values = c(
      "H2-Apo-25" = "grey50",
      "H2-Ino-25" = "#4DAF4A",
      "H2-Sym-25" = "#8C564B"
    ),
    labels = c(
      "H2-Apo-25" = "Apo 25°C",
      "H2-Ino-25" = "Inoc 25°C",
      "H2-Sym-25" = "Sym 25°C"
    )
  ) +
  
  # Shapes
  scale_shape_manual(
    values = c(
      "H2-Apo-25" = 16,
      "H2-Ino-25" = 17,
      "H2-Sym-25" = 15
    ),
    labels = c(
      "H2-Apo-25" = "Apo 25°C",
      "H2-Ino-25" = "Inoc 25°C",
      "H2-Sym-25" = "Sym 25°C"
    )
  ) +
  
  labs(color = "Treatment", shape = "Treatment") +
  
  theme(
    legend.position = c(0.75, 0.45),
    legend.justification = c("center", "center")
  )

p_sym_state_shape

p_sym_state <- ggplot(
  data = data_means %>%
    filter(treatment %in% c("H2-Apo-25", "H2-Ino-25", "H2-Sym-25")),
  aes(x = day, y = mean, color = treatment, group = treatment)
) +
  my_theme +
  geom_line(linewidth = 0.9) +
  geom_errorbar(
    aes(ymin = mean - se, ymax = mean + se),
    width = 0.2,
    linewidth = 0.6
  ) +
  geom_point(size = 4) +
  
  annotate("text", x = 7,  y = 10.2, label = "***", size = 6) +
  annotate("text", x = 10, y = 12.0, label = "***", size = 6) +
  annotate("text", x = 11, y = 12.0, label = "**",  size = 6) +
  
  ylab("Mean tentacle number") +
  xlab("Days post laceration (dpl)") +
  scale_y_continuous(breaks = seq(0, 13, 2), limits = c(0, 13)) +
  scale_x_continuous(breaks = seq(min(data_means$day), max(data_means$day), 1)) +
  scale_color_manual(
    values = c(
      "H2-Apo-25" = "grey50",
      "H2-Ino-25" = "#4DAF4A",
      "H2-Sym-25" = "#8C564B"
    ),
    labels = c(
      "H2-Apo-25" = "Apo 25°C",
      "H2-Ino-25" = "Inoc 25°C",
      "H2-Sym-25" = "Sym 25°C"
    )
  ) +
  labs(colour = "Treatment") +
  theme(
    legend.position = c(0.75, 0.45),
    legend.justification = c("center", "center")
  )

p_sym_state

### =========================
### INOC + SYM + APO 2024 PLOT
### =========================

p_sym_state_color_shape_2024 <- ggplot(
  data = data_means %>%
    filter(treatment %in% c(
      "H2-Apo-25", "H2-Ino-25", "H2-Sym-25",
      "H2-Apo-32", "H2-Ino-32", "H2-Sym-32"
    )) %>%
    mutate(
      temp = ifelse(grepl("25", treatment), "25°C", "32°C"),
      state = case_when(
        grepl("Apo", treatment) ~ "Apo",
        grepl("Ino", treatment) ~ "Inoc",
        grepl("Sym", treatment) ~ "Sym"
      )
    ),
  aes(
    x = day,
    y = mean,
    group = treatment,
    color = temp,
    shape = state
  )
) +
  my_theme +
  geom_line(linewidth = 1.1) +
  geom_errorbar(
    aes(ymin = mean - se, ymax = mean + se),
    width = 0.2,
    linewidth = 0.6,
    alpha = 0.8
  ) +
  geom_point(size = 4) +
  ylab("Mean tentacle number") +
  xlab("Days post laceration (dpl)") +
  scale_y_continuous(breaks = seq(0, 13, 2), limits = c(0, 13)) +
  scale_x_continuous(breaks = seq(min(data_means$day), max(data_means$day), 1)) +
  scale_color_manual(
    values = c(
      "25°C" = "#3B6FB6",
      "32°C" = "#D55E00"
    )
  ) +
  scale_shape_manual(
    values = c(
      "Apo" = 16,
      "Inoc" = 17,
      "Sym" = 15
    )
  ) +
  labs(color = "Temperature", shape = "Symbiotic state") +
  theme(
    legend.position = c(0.60, 0.01),
    legend.justification = c("left", "bottom"),
    legend.background = element_blank()
  )

p_sym_state_color_shape_2024

p_sym_state_color_linetype_2024 <- ggplot(
  data = data_means %>%
    filter(treatment %in% c(
      "H2-Apo-25", "H2-Ino-25", "H2-Sym-25",
      "H2-Apo-32", "H2-Ino-32", "H2-Sym-32"
    )) %>%
    mutate(
      treatment_label = case_when(
        treatment == "H2-Apo-25" ~ "Apo, 25°C",
        treatment == "H2-Ino-25" ~ "Inoc, 25°C",
        treatment == "H2-Sym-25" ~ "Sym, 25°C",
        treatment == "H2-Apo-32" ~ "Apo, 32°C",
        treatment == "H2-Ino-32" ~ "Inoc, 32°C",
        treatment == "H2-Sym-32" ~ "Sym, 32°C"
      ),
      treatment_label = factor(
        treatment_label,
        levels = c(
          "Apo, 25°C", "Inoc, 25°C", "Sym, 25°C",
          "Apo, 32°C", "Inoc, 32°C", "Sym, 32°C"
        )
      )
    ),
  aes(
    x = day,
    y = mean,
    color = treatment_label,
    linetype = treatment_label,
    group = treatment_label
  )
) +
  my_theme +
  geom_line(linewidth = 1.1) +
  geom_errorbar(
    aes(ymin = mean - se, ymax = mean + se),
    width = 0.2,
    linewidth = 0.6,
    alpha = 0.8,
    linetype = "solid"
  ) +
  geom_point(size = 4) +
  ylab("Mean tentacle number") +
  xlab("Days post laceration (dpl)") +
  scale_y_continuous(breaks = seq(0, 13, 2), limits = c(0, 13)) +
  scale_x_continuous(breaks = seq(min(data_means$day), max(data_means$day), 1)) +
  scale_color_manual(
    values = c(
      "Apo, 25°C"  = "#6FA3D9",
      "Inoc, 25°C" = "#6A51A3",
      "Sym, 25°C"  = "#3B6FB6",
      "Apo, 32°C"  = "#F39B7F",
      "Inoc, 32°C" = "#B35806",
      "Sym, 32°C"  = "#E64B35"
    )
  ) +
  scale_linetype_manual(
    values = c(
      "Apo, 25°C"  = "dashed",
      "Inoc, 25°C" = "dotdash",
      "Sym, 25°C"  = "solid",
      "Apo, 32°C"  = "dashed",
      "Inoc, 32°C" = "dotdash",
      "Sym, 32°C"  = "solid"
    )
  ) +
  labs(color = "Treatment", linetype = "Treatment") +
  theme(
    legend.position = c(0.60, 0.12),
    legend.justification = c("left", "bottom"),
    legend.key.width = unit(2.8, "cm"),
    legend.background = element_blank()
  )

p_sym_state_color_linetype_2024

p_sym_state_full_encoding_2024 <- ggplot(
  data = data_means %>%
    filter(treatment %in% c(
      "H2-Apo-25", "H2-Ino-25", "H2-Sym-25",
      "H2-Apo-32", "H2-Ino-32", "H2-Sym-32"
    )) %>%
    mutate(
      treatment_label = case_when(
        treatment == "H2-Apo-25" ~ "Apo, 25°C",
        treatment == "H2-Ino-25" ~ "Inoc, 25°C",
        treatment == "H2-Sym-25" ~ "Sym, 25°C",
        treatment == "H2-Apo-32" ~ "Apo, 32°C",
        treatment == "H2-Ino-32" ~ "Inoc, 32°C",
        treatment == "H2-Sym-32" ~ "Sym, 32°C"
      ),
      treatment_label = factor(
        treatment_label,
        levels = c(
          "Apo, 25°C", "Inoc, 25°C", "Sym, 25°C",
          "Apo, 32°C", "Inoc, 32°C", "Sym, 32°C"
        )
      )
    ),
  aes(
    x = day,
    y = mean,
    group = treatment_label,
    color = treatment_label,
    linetype = treatment_label,
    shape = treatment_label
  )
) +
  my_theme +
  geom_line(linewidth = 1.1) +
  geom_errorbar(
    aes(ymin = mean - se, ymax = mean + se),
    width = 0.2,
    linewidth = 0.6,
    alpha = 0.8,
    linetype = "solid"
  ) +
  geom_point(size = 4) +
  ylab("Mean tentacle number") +
  xlab("Days post laceration (dpl)") +
  scale_y_continuous(
    breaks = seq(0, 13, 2),
    limits = c(0, 13)
  ) +
  scale_x_continuous(
    breaks = seq(min(data_means$day), max(data_means$day), 1)
  ) +
  scale_color_manual(
    values = c(
      "Apo, 25°C"  = "#6FA3D9",
      "Inoc, 25°C" = "#6A51A3",
      "Sym, 25°C"  = "#3B6FB6",
      "Apo, 32°C"  = "#F39B7F",
      "Inoc, 32°C" = "#B35806",
      "Sym, 32°C"  = "#E64B35"
    )
  ) +
  scale_linetype_manual(
    values = c(
      "Apo, 25°C"  = "dashed",
      "Inoc, 25°C" = "dotdash",
      "Sym, 25°C"  = "solid",
      "Apo, 32°C"  = "dashed",
      "Inoc, 32°C" = "dotdash",
      "Sym, 32°C"  = "solid"
    )
  ) +
  scale_shape_manual(
    values = c(
      "Apo, 25°C"  = 16,
      "Inoc, 25°C" = 17,
      "Sym, 25°C"  = 15,
      "Apo, 32°C"  = 16,
      "Inoc, 32°C" = 17,
      "Sym, 32°C"  = 15
    )
  ) +
  labs(
    color = "Treatment",
    linetype = "Treatment",
    shape = "Treatment"
  ) +
  guides(
    color = guide_legend(
      override.aes = list(
        linewidth = 1.2,
        size = 4
      )
    )
  ) +
  theme(
    legend.position = c(0.60, 0.12),
    legend.justification = c("left", "bottom"),
    legend.background = element_blank(),
    legend.key.width = unit(2.4, "cm")
  )

p_sym_state_full_encoding_2024

### =========================
### INOC ONLY 2024 PLOT
### =========================

data_means_inoc <- data_means %>%
  filter(treatment %in% c("H2-Ino-25", "H2-Ino-32")) %>%
  mutate(
    treatment_label = case_when(
      treatment == "H2-Ino-25" ~ "Inoc, 25°C",
      treatment == "H2-Ino-32" ~ "Inoc, 32°C"
    ),
    treatment_label = factor(
      treatment_label,
      levels = c("Inoc, 25°C", "Inoc, 32°C")
    )
  )

p_inoc_only <- ggplot(
  data_means_inoc,
  aes(
    x = day,
    y = mean,
    color = treatment_label,
    linetype = treatment_label,
    group = treatment_label
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
  
  annotate("text", x = 4, y = 3.5, label = "**", size = 6) +
  annotate("text", x = 5, y = 8.1, label = "***", size = 6) +
  annotate("text", x = 7, y = 10.8, label = "*", size = 6) +
  
  ylab("Mean tentacle number") +
  xlab("Days post laceration (dpl)") +
  
  scale_y_continuous(
    breaks = seq(0, 12, 2),
    limits = c(0, 12)
  ) +
  scale_x_continuous(
    breaks = seq(min(data_means_inoc$day), max(data_means_inoc$day), 1)
  ) +
  scale_color_manual(
    values = c(
      "Inoc, 25°C" = "#3B88C3",
      "Inoc, 32°C" = "#D95F02"
    )
  ) +
  scale_linetype_manual(
    values = c(
      "Inoc, 25°C" = "dotdash",
      "Inoc, 32°C" = "twodash"
    )
  ) +
  labs(
    color = "Treatment",
    linetype = "Treatment"
  ) +
  theme(
    legend.position = c(0.73, 0.45),
    legend.justification = c("center", "center"),
    legend.key.width = unit(2.8, "cm")
  )

p_inoc_only


### =========================
### 5. SYMBIOTIC STATE STATS
### =========================

df_sym_state <- ursa2024_data %>%
  filter(
    line == "H2",
    treatment %in% c("H2-Apo-25", "H2-Ino-25", "H2-Sym-25")
  ) %>%
  mutate(
    treatment = factor(treatment, levels = c("H2-Apo-25", "H2-Ino-25", "H2-Sym-25")),
    day = factor(day),
    id = factor(id)
  )
df_sym_state$treatment <- factor(df_sym_state$treatment)
df_sym_state$day <- factor(df_sym_state$day)
df_sym_state$id <- factor(df_sym_state$id)

# Explore distribution
hist(df_sym_state$tent_count)

# ----------------------------
# Fit Poisson GLMM
# ----------------------------
model_sym_state <- glmer(
  tent_count ~ treatment * day + (1 | id),
  family = poisson(link = "log"),
  data = df_sym_state,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 200000)
  )
)

# ----------------------------
# Light model checks
# ----------------------------

# 1. Basic diagnostic plots
plot(model_sym_state)

# 2. Check for overdispersion
check_overdispersion(model_sym_state)

# 3. Residual simulation plot
# Use mainly as a visual check, not a strict pass/fail test
sim_res_sym <- simulateResiduals(model_sym_state)
plot(sim_res_sym)

# ----------------------------
# Type II ANOVA
# ----------------------------
Anova(model_sym_state, type = "II") 

# ----------------------------
# Estimated marginal means
# ----------------------------
emm_sym <- emmeans(model_sym_state, ~ treatment | day)

# Pairwise comparisons with Tukey adjustment
pairs(emm_sym, adjust = "tukey")