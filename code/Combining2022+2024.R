library(dplyr)
library(janitor)
library(plotrix)

rm(list = ls())
graphics.off()

#Set working directory

getwd()
setwd("~/Documents/GitHub/heatinglacerate")

select <- dplyr::select
filter <- dplyr::filter
mutate <- dplyr::mutate
lag <- dplyr::lag

# =========================
# CLEAN 2022 long DATA
# =========================
#Results of Experiment 2022

##Reading data table
long <- read.csv("data/Exp 2 Lacerate Development in Heat Data Sheet - Long Data.csv")
str(long)

##Converting elements in table
long$tent_count <- as.numeric(long$tent_count)
long$ID <- as.factor(long$ID)
long$plate <- as.factor(long$plate)
long$well <- as.factor(long$well)
long$line <- as.factor(long$line)
long$temp <- as.factor(long$temp)
long$treatment <- as.factor(long$treatment)
long$symbiosis <- as.factor(long$symbiosis)
long$day <- as.numeric(long$day)
long$day_cat <- as.factor(long$day_cat)

df <- long %>%
  mutate(Day = as.factor(day)) %>%
  mutate(Day = dplyr::recode(Day, "0" = "00"))

long_clean <- long %>%
  clean_names() %>%   # ID -> id
  mutate(
    id = as.character(id),
    plate = as.character(plate),
    treatment = as.character(treatment),
    line = as.character(line),
    symbiosis = as.character(symbiosis),
    well = as.character(well),
    temp = as.character(temp),
    day = as.numeric(day),
    day_cat = as.character(day_cat),
    tent_count = as.numeric(tent_count),
    lacerate = NA_character_,
    dataset = "2022"
  ) %>%
  mutate(
    treatment = gsub("APO", "Apo", treatment),
    treatment = gsub("SYM", "Sym", treatment),
    treatment = gsub("INO", "Ino", treatment),
    treatment = gsub("25C$", "25", treatment),
    treatment = gsub("32C$", "32", treatment),
    id = paste0("2022_", id)
  )

# =========================
# CLEAN 2024 DATA
# =========================

### =========================
### 1. READ AND CLEAN 2024 DATA
### =========================

ursa2024_data <- read.csv("Grace/Experimental Schedule for URSA - Grace Kelly - Grace_tent_count.csv") %>%
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

ursa2024_clean <- ursa2024_data %>%
  mutate(
    id = as.character(id),
    plate = as.character(plate),
    treatment = as.character(treatment),
    line = as.character(line),
    symbiosis = as.character(symbiosis),
    well = as.character(well),
    temp = as.character(temp),
    lacerate = as.character(lacerate),
    day = as.numeric(day),
    day_cat = as.character(day_cat),
    tent_count = as.numeric(tent_count),
    dataset = "2024",
    id = paste0("2024_", id)
  )

# =========================
# KEEP SAME COLUMNS AND COMBINE
# =========================

combined_data <- bind_rows(
  long_clean %>%
    select(id, plate, treatment, line, symbiosis, well, temp, lacerate, day, day_cat, tent_count, dataset),
  ursa2024_clean %>%
    select(id, plate, treatment, line, symbiosis, well, temp, lacerate, day, day_cat, tent_count, dataset)
)

# check
str(combined_data)
table(combined_data$dataset)
table(combined_data$treatment)
table(combined_data$line)


combined_h2 <- combined_data %>%
  filter(
    line == "H2",
    treatment %in% c("H2-Apo-25", "H2-Apo-32", "H2-Sym-25", "H2-Sym-32")
  )


combined_h2_overlap <- combined_h2 %>%
  filter(day %in% c(0, 4, 5, 7, 10, 11, 14, 21))


filtered_combined <- combined_h2_overlap %>%
  group_by(id) %>%
  mutate(
    zero_day4 = any(day == 4 & tent_count == 0, na.rm = TRUE),
    develops_5_21 = any(day >= 5 & day <= 21 & tent_count > 0, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(!(zero_day4 & !develops_5_21))



data_means_combined <- filtered_combined %>%
  group_by(dataset, treatment, day) %>%
  summarise(
    mean = mean(tent_count, na.rm = TRUE),
    se = plotrix::std.error(tent_count, na.rm = TRUE),
    n = sum(!is.na(tent_count)),
    .groups = "drop"
  )

print(data_means_combined, n = Inf)


data_means_pooled <- filtered_combined %>%
  group_by(treatment, day) %>%
  summarise(
    mean = mean(tent_count, na.rm = TRUE),
    se = plotrix::std.error(tent_count, na.rm = TRUE),
    n = sum(!is.na(tent_count)),
    .groups = "drop"
  )









library(dplyr)
library(ggplot2)
library(plotrix)

# =========================================================
# SUBSET DATA CORRECTLY
# =========================================================

plot_data <- combined_data %>%
  filter(
    (dataset == "2022" & treatment %in% c(
      "H2-Apo-25", "H2-Apo-32",
      "H2-Sym-25", "H2-Sym-32"
    )) |
      (dataset == "2024" & treatment %in% c(
        "H2-Ino-25", "H2-Ino-32"
      ))
  ) %>%
  filter(
    line == "H2",
    day %in% c(0, 4, 5, 7, 10, 11, 14, 21)
  )

# =========================================================
# OPTIONAL FILTER (non-developers)
# =========================================================

plot_data_filtered <- plot_data %>%
  group_by(id) %>%
  mutate(
    zero_day4 = any(day == 4 & tent_count == 0, na.rm = TRUE),
    develops_5_21 = any(day >= 5 & day <= 21 & tent_count > 0, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(!(zero_day4 & !develops_5_21))

# =========================================================
# CALCULATE MEANS
# =========================================================

data_means_mixed <- plot_data_filtered %>%
  group_by(treatment, day) %>%
  summarise(
    mean = mean(tent_count, na.rm = TRUE),
    se = plotrix::std.error(tent_count, na.rm = TRUE),
    n = sum(!is.na(tent_count)),
    .groups = "drop"
  ) %>%
  mutate(
    temp = ifelse(grepl("25", treatment), "25°C", "32°C"),
    state = case_when(
      grepl("Apo", treatment) ~ "Apo",
      grepl("Ino", treatment) ~ "Inoc",
      grepl("Sym", treatment) ~ "Sym"
    )
  )

print(data_means_mixed, n = Inf)

# =========================================================
# THEME
# =========================================================

my_theme <- theme_classic(base_size = 14) +
  theme(
    text = element_text(family = "sans", size = 14),
    axis.text = element_text(size = 14, colour = "black"),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14)
  )

# =========================================================
# PLOT
# =========================================================

p_mixed <- ggplot(
  data_means_mixed,
  aes(
    x = day,
    y = mean,
    group = interaction(state, temp),
    color = temp,
    shape = state
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
  scale_y_continuous(breaks = seq(0, 13, 2), limits = c(0, 13)) +
  scale_x_continuous(breaks = c(0, 4, 5, 7, 10, 11, 14, 21)) +
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
    legend.position = c(0.75, 0.35),
    legend.justification = c("center", "center")
  )

p_mixed







# ============================================================
# Summer 2025 pedal lacerate data
# Full workflow for long-format CSV
# ============================================================

# ----------------------------
# Load packages
# ----------------------------
library(tidyverse)
library(janitor)

# ----------------------------
# Read CSV
# ----------------------------
summer2025_long <- read_csv(
  "Summer 2025 Data Sheet - Pedal Lacerate - long_data.csv",
  show_col_types = FALSE
)

# ----------------------------
# Inspect structure
# ----------------------------
glimpse(summer2025_long)
names(summer2025_long)

# ============================================================
# 1. Clean and standardize
# ============================================================

summer2025_long_clean <- summer2025_long %>%
  janitor::clean_names() %>%
  mutate(
    # convert tent_count safely
    tent_count = na_if(tent_count, "na"),
    tent_count = na_if(tent_count, "NA"),
    tent_count = as.numeric(tent_count),
    
    # make dpl numeric just in case
    dpl = as.numeric(dpl),
    
    # standardize sym state labels
    sym = case_when(
      sym %in% c("Sym", "SYM", "sym") ~ "Sym",
      sym %in% c("Apo", "APO", "apo") ~ "Apo",
      sym %in% c("Inoc", "INO", "inoc", "Ino") ~ "Inoc",
      TRUE ~ as.character(sym)
    ),
    
    # standardize temp labels
    temp = case_when(
      str_detect(temp, "^25") ~ "25C",
      str_detect(temp, "^32") ~ "32C",
      TRUE ~ as.character(temp)
    ),
    
    # rebuild treatment so it is consistent
    treatment = paste(sym, temp, sep = "_"),
    
    # make factors for plotting order
    sym = factor(sym, levels = c("Apo", "Inoc", "Sym")),
    temp = factor(temp, levels = c("25C", "32C")),
    treatment = factor(
      treatment,
      levels = c("Apo_25C", "Apo_32C",
                 "Inoc_25C", "Inoc_32C",
                 "Sym_25C", "Sym_32C")
    ),
    
    # optional: make day_cat ordered from dpl
    day_cat = factor(day_cat, levels = paste0("day_", sort(unique(dpl))))
  )

# ----------------------------
# Quick checks
# ----------------------------
summary(summer2025_long_clean$tent_count)
table(summer2025_long_clean$sym, useNA = "ifany")
table(summer2025_long_clean$temp, useNA = "ifany")
table(summer2025_long_clean$treatment, useNA = "ifany")

# number of unique IDs by treatment
summer2025_long_clean %>%
  distinct(id, treatment) %>%
  count(treatment)

# ============================================================
# 2. Summary table by treatment and day
# ============================================================

summer2025_summary <- summer2025_long_clean %>%
  filter(!is.na(tent_count)) %>%
  group_by(treatment, sym, temp, dpl) %>%
  summarise(
    n = n(),
    mean = mean(tent_count, na.rm = TRUE),
    sd = sd(tent_count, na.rm = TRUE),
    se = sd / sqrt(n),
    .groups = "drop"
  )

print(summer2025_summary)

# ============================================================
# 3. Plot theme
# ============================================================

my_theme <- theme_classic(base_size = 15) +
  theme(
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.title = element_blank(),
    legend.position = "right"
  )

# ============================================================
# 4. Plot all 6 treatments together
# Similar to old multi-treatment development plots
# ============================================================

fig_summer2025_all_treatments <- ggplot(
  summer2025_summary,
  aes(x = dpl, y = mean, color = treatment, group = treatment)
) +
  my_theme +
  geom_line(linewidth = 0.9) +
  geom_errorbar(
    aes(ymin = mean - se, ymax = mean + se),
    width = 0.2,
    linewidth = 0.6
  ) +
  geom_point(size = 3) +
  scale_x_continuous(
    breaks = sort(unique(summer2025_summary$dpl))
  ) +
  scale_y_continuous(
    limits = c(0, NA)
  ) +
  ylab("Mean tentacle number") +
  xlab("Days post laceration (dpl)") +
  scale_color_manual(
    values = c(
      "Apo_25C"  = "#3B6FB6",
      "Apo_32C"  = "#6BA3D6",
      "Inoc_25C" = "#4E8B57",
      "Inoc_32C" = "#7DBA84",
      "Sym_25C"  = "#8C564B",
      "Sym_32C"  = "#C27D73"
    ),
    labels = c(
      "Apo_25C"  = "Apo 25°C",
      "Apo_32C"  = "Apo 32°C",
      "Inoc_25C" = "Inoc 25°C",
      "Inoc_32C" = "Inoc 32°C",
      "Sym_25C"  = "Sym 25°C",
      "Sym_32C"  = "Sym 32°C"
    )
  ) +
  ggtitle("Pedal lacerate tentacle development across treatments")

fig_summer2025_all_treatments




## 2022 + 2025

library(tidyverse)
library(janitor)
library(plotrix)

graphics.off()

select <- dplyr::select
filter <- dplyr::filter
mutate <- dplyr::mutate
lag <- dplyr::lag

# =========================================================
# 1. READ AND CLEAN 2022 DATA
#    keep only H2 Apo + Sym from 2022
# =========================================================

long_2022 <- read.csv("data/Exp 2 Lacerate Development in Heat Data Sheet - Long Data.csv")

long_2022_clean <- long_2022 %>%
  clean_names() %>%
  mutate(
    id = as.character(id),
    plate = as.character(plate),
    treatment = as.character(treatment),
    line = as.character(line),
    symbiosis = as.character(symbiosis),
    well = as.character(well),
    temp = as.character(temp),
    day = as.numeric(day),
    day_cat = as.character(day_cat),
    tent_count = suppressWarnings(as.numeric(tent_count)),
    lacerate = NA_character_,
    dataset = "2022"
  ) %>%
  mutate(
    treatment = gsub("APO", "Apo", treatment),
    treatment = gsub("SYM", "Sym", treatment),
    treatment = gsub("INO", "Ino", treatment),
    treatment = gsub("25C$", "25", treatment),
    treatment = gsub("32C$", "32", treatment),
    id = paste0("2022_", id)
  ) %>%
  filter(
    line == "H2",
    treatment %in% c("H2-Apo-25", "H2-Apo-32", "H2-Sym-25", "H2-Sym-32")
  )

# check
table(long_2022_clean$treatment, useNA = "ifany")
table(long_2022_clean$line, useNA = "ifany")

# =========================================================
# 2. READ AND CLEAN SUMMER 2025 DATA
#    keep only Inoc treatments
# =========================================================

summer2025_long <- read_csv(
  "Summer 2025 Data Sheet - Pedal Lacerate - long_data.csv",
  show_col_types = FALSE
)

summer2025_clean <- summer2025_long %>%
  clean_names() %>%
  mutate(
    id = as.character(id),
    plate = as.character(plate),
    sym = as.character(sym),
    well = as.character(well),
    temp = as.character(temp),
    treatment = as.character(treatment),
    lacerate = as.character(lacerate),
    day = as.character(day),
    dpl = as.numeric(dpl),
    day_cat = as.character(day_cat),
    tent_count = na_if(tent_count, "na"),
    tent_count = na_if(tent_count, "NA"),
    tent_count = suppressWarnings(as.numeric(tent_count)),
    dataset = "2025"
  ) %>%
  mutate(
    sym = case_when(
      sym %in% c("Sym", "SYM", "sym") ~ "Sym",
      sym %in% c("Apo", "APO", "apo") ~ "Apo",
      sym %in% c("Inoc", "INO", "inoc", "Ino") ~ "Ino",
      TRUE ~ sym
    ),
    temp = case_when(
      str_detect(temp, "^25") ~ "25",
      str_detect(temp, "^32") ~ "32",
      TRUE ~ temp
    ),
    treatment = paste0("H2-", sym, "-", temp),
    line = "H2",
    symbiosis = sym,
    day = dpl,
    id = paste0("2025_", id)
  ) %>%
  select(
    id, plate, treatment, line, symbiosis, well,
    temp, lacerate, day, day_cat, tent_count, dataset
  ) %>%
  filter(
    treatment %in% c("H2-Ino-25", "H2-Ino-32")
  )

# check
table(summer2025_clean$treatment, useNA = "ifany")
table(summer2025_clean$line, useNA = "ifany")

# =========================================================
# 3. COMBINE 2022 + SUMMER 2025 INOC
# =========================================================

combined_data <- bind_rows(
  long_2022_clean %>%
    select(id, plate, treatment, line, symbiosis, well, temp, lacerate, day, day_cat, tent_count, dataset),
  summer2025_clean %>%
    select(id, plate, treatment, line, symbiosis, well, temp, lacerate, day, day_cat, tent_count, dataset)
)

# checks
str(combined_data)
table(combined_data$dataset, useNA = "ifany")
table(combined_data$treatment, useNA = "ifany")

# =========================================================
# 4. KEEP ONLY OVERLAPPING DAYS USED IN 2022 STYLE PLOT
# =========================================================

plot_data <- combined_data %>%
  filter(day %in% c(0, 4, 5, 7, 10, 11, 14, 21))

table(plot_data$treatment, plot_data$day)

# =========================================================
# 5. OPTIONAL FILTER:
#    remove IDs that are 0 at day 4 and never develop after
# =========================================================

plot_data_filtered <- plot_data %>%
  group_by(id) %>%
  mutate(
    zero_day4 = any(day == 4 & tent_count == 0, na.rm = TRUE),
    develops_5_21 = any(day >= 5 & day <= 21 & tent_count > 0, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(!(zero_day4 & !develops_5_21))

# check how many remain
plot_data_filtered %>%
  distinct(id, treatment, dataset) %>%
  count(dataset, treatment)

# =========================================================
# 6. SUMMARIZE MEAN ± SE FOR ALL SIX TREATMENTS
# =========================================================

data_means_all6 <- plot_data_filtered %>%
  group_by(treatment, day) %>%
  summarise(
    mean = mean(tent_count, na.rm = TRUE),
    se = plotrix::std.error(tent_count, na.rm = TRUE),
    n = sum(!is.na(tent_count)),
    .groups = "drop"
  ) %>%
  mutate(
    treatment = factor(
      treatment,
      levels = c(
        "H2-Apo-25", "H2-Apo-32",
        "H2-Ino-25", "H2-Ino-32",
        "H2-Sym-25", "H2-Sym-32"
      )
    )
  )

print(data_means_all6, n = Inf)

# =========================================================
# 7. MAKE LABELING VARIABLES FOR PLOTTING
# =========================================================

data_means_all6 <- data_means_all6 %>%
  mutate(
    temp_group = case_when(
      grepl("25$", treatment) ~ "25°C",
      grepl("32$", treatment) ~ "32°C"
    ),
    sym_state = case_when(
      grepl("Apo", treatment) ~ "Apo",
      grepl("Ino", treatment) ~ "Inoc",
      grepl("Sym", treatment) ~ "Sym"
    )
  )

# =========================================================
# 8. THEME
# =========================================================

my_theme <- theme_classic(base_size = 14) +
  theme(
    text = element_text(family = "sans", size = 14),
    axis.text = element_text(size = 14, colour = "black"),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14)
  )

# =========================================================
# 9. PLOT ALL SIX TREATMENTS
# =========================================================

p_all6 <- ggplot(
  data_means_all6,
  aes(
    x = day,
    y = mean,
    group = treatment,
    color = temp_group,
    shape = sym_state
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
    breaks = seq(0, 13, 2),
    limits = c(0, 13)
  ) +
  scale_x_continuous(
    breaks = c(0, 4, 5, 7, 10, 11, 14, 21)
  ) +
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
  labs(
    color = "Temperature",
    shape = "Symbiotic state"
  ) +
  theme(
    legend.position = c(0.77, 0.35),
    legend.justification = c("center", "center")
  )

p_all6

# =========================================================
# 9. PLOT ALL SIX TREATMENTS (DISTINCT COLORS)
# =========================================================

P_all6_color <- ggplot(
  data_means_all6,
  aes(
    x = day,
    y = mean,
    group = treatment,
    color = treatment,        # <-- key change
    shape = sym_state
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
    breaks = seq(0, 13, 2),
    limits = c(0, 13)
  ) +
  scale_x_continuous(
    breaks = c(0, 4, 5, 7, 10, 11, 14, 21)
  ) +
  scale_color_manual(
    values = c(
      "H2-Apo-25" = "#1f78b4",   # blue
      "H2-Apo-32" = "#a6cee3",   # light blue
      "H2-Ino-25" = "#33a02c",   # green
      "H2-Ino-32" = "#b2df8a",   # light green
      "H2-Sym-25" = "#e31a1c",   # red
      "H2-Sym-32" = "#fb9a99"    # light red
    ),
    labels = c(
      "H2-Apo-25" = "Apo 25°C",
      "H2-Apo-32" = "Apo 32°C",
      "H2-Ino-25" = "Inoc 25°C",
      "H2-Ino-32" = "Inoc 32°C",
      "H2-Sym-25" = "Sym 25°C",
      "H2-Sym-32" = "Sym 32°C"
    )
  ) +
  scale_shape_manual(
    values = c(
      "Apo" = 16,
      "Inoc" = 17,
      "Sym" = 15
    )
  ) +
  labs(
    color = "Treatment",
    shape = "Symbiotic state"
  ) +
  theme(
    legend.position = c(0.77, 0.35),
    legend.justification = c("center", "center")
  )

P_all6_color


P_inoc <- data_means_all6 %>%
  filter(sym_state == "Inoc") %>%
  ggplot(
    aes(
      x = day,
      y = mean,
      group = temp_group,
      color = temp_group
    )
  ) +
  my_theme +
  geom_line(linewidth = 1) +
  geom_errorbar(
    aes(ymin = mean - se, ymax = mean + se),
    width = 0.2,
    linewidth = 0.7
  ) +
  geom_point(size = 4) +
  ylab("Mean tentacle number") +
  xlab("Days post laceration (dpl)") +
  scale_y_continuous(
    breaks = seq(0, 13, 2),
    limits = c(0, 13)
  ) +
  scale_x_continuous(
    breaks = c(0, 4, 5, 7, 10, 11, 14, 21)
  ) +
  scale_color_manual(
    values = c(
      "25°C" = "#33a02c",
      "32°C" = "#b15928"
    )
  ) +
  labs(color = "Temperature") +
  ggtitle("Inoculated")

P_inoc



library(dplyr)
library(ggplot2)
library(plotrix)

# =========================================================
# 1. SUBSET 2024 INOC DATA
# =========================================================

inoc_2024 <- ursa2024_clean %>%
  filter(
    line == "H2",
    treatment %in% c("H2-Ino-25", "H2-Ino-32"),
    day %in% c(0, 4, 5, 7, 10, 11, 14, 21)
  )

# =========================================================
# 2. OPTIONAL: REMOVE NON-DEVELOPERS
# =========================================================

inoc_2024_filtered <- inoc_2024 %>%
  group_by(id) %>%
  mutate(
    zero_day4 = any(day == 4 & tent_count == 0, na.rm = TRUE),
    develops_5_21 = any(day >= 5 & day <= 21 & tent_count > 0, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(!(zero_day4 & !develops_5_21))

# =========================================================
# 3. SUMMARIZE
# =========================================================

inoc_means <- inoc_2024_filtered %>%
  group_by(treatment, day) %>%
  summarise(
    mean = mean(tent_count, na.rm = TRUE),
    se = plotrix::std.error(tent_count, na.rm = TRUE),
    n = sum(!is.na(tent_count)),
    .groups = "drop"
  ) %>%
  mutate(
    temp = ifelse(grepl("25", treatment), "25°C", "32°C")
  )

# =========================================================
# 4. PLOT
# =========================================================

my_theme <- theme_classic(base_size = 14)

p_inoc_2024 <- ggplot(
  inoc_means,
  aes(
    x = day,
    y = mean,
    group = temp,
    color = temp
  )
) +
  my_theme +
  geom_line(linewidth = 1) +
  geom_errorbar(
    aes(ymin = mean - se, ymax = mean + se),
    width = 0.2,
    linewidth = 0.7
  ) +
  geom_point(size = 4) +
  ylab("Mean tentacle number") +
  xlab("Days post laceration (dpl)") +
  scale_y_continuous(
    breaks = seq(0, 13, 2),
    limits = c(0, 13)
  ) +
  scale_x_continuous(
    breaks = c(0, 4, 5, 7, 10, 11, 14, 21)
  ) +
  scale_color_manual(
    values = c(
      "25°C" = "#33a02c",
      "32°C" = "#b15928"
    )
  ) +
  labs(color = "Temperature") +
  ggtitle("Inoculated (2024 only)")

p_inoc_2024




