# ============================================================
# Figure 1 Sym vs Apo for manuscript
# H2 only with hard-coded temperature significance bars
# ============================================================

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
library(multcomp)
library(grid)

rm(list = ls())
graphics.off()

#Set working directory

getwd()
setwd("~/Documents/GitHub/heatinglacerate")

select <- dplyr::select
filter <- dplyr::filter
mutate <- dplyr::mutate
lag <- dplyr::lag

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

##Saving table as outpu
newlong <- long

# ----------------------------
# H2-only dataset for figure/model
# ----------------------------
fig1_dat_raw <- newlong %>%
  filter(line != "CC7") %>%
  mutate(
    day_cat = factor(day_cat),
    temp = factor(temp),
    symbiosis = factor(symbiosis),
    ID = factor(ID)
  )

# ----------------------------
# Summary data for plotting
# ----------------------------
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

# ----------------------------
# Theme
# ----------------------------
my_theme <- theme_classic(base_size = 14) +
  theme(
    text = element_text(family = "sans", size = 14),
    axis.text = element_text(size = 14, colour = "black"),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    plot.caption = element_text(size = 12, hjust = 0)
  )

# ----------------------------
# Plot
# ----------------------------
fig1_fuller_H2CC7 <- ggplot(
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
      "Apo, 25°C" = "#3B6FB6",
      "Sym, 25°C" = "#3B6FB6",
      "Apo, 32°C" = "#E64B35",
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

fig1_fuller_H2CC7



fig1_fuller_H2only <- ggplot(
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
# H2-only temperature significance
# * p < 0.05, ** p < 0.01, *** p < 0.001
# Day 5-8 = ***
# Day 9 = *
# ----------------------------
annotate("segment", x = 5, xend = 8, y = 9.2, yend = 9.2, linewidth = 0.8) +
  annotate("text", x = 6.5, y = 9.45, label = "***", size = 6) +
  annotate("text", x = 9, y = 9.45, label = "*", size = 6) +
  
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
      "Apo, 25°C" = "#3B6FB6",
      "Sym, 25°C" = "#3B6FB6",
      "Apo, 32°C" = "#E64B35",
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
    # caption = "* p < 0.05, ** p < 0.01, *** p < 0.001"
  ) +
  theme(
    legend.position = c(0.73, 0.45),
    legend.justification = c("center", "center"),
    legend.key.width = unit(2.8, "cm")
  )

fig1_fuller_H2only






# ----------------------------
# Plot Changes
# ----------------------------

fig1_fuller_change <- ggplot(
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
    alpha = 0.8
  ) +
  geom_point(size = 4) +
  
  # ----------------------------
# Temperature significance bars
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
  
  # ----------------------------
# Color: Sym = bold, Apo = lighter
# ----------------------------
scale_color_manual(
  values = c(
    "Sym, 25°C" = "#3B6FB6",
    "Apo, 25°C" = "#9BBCE0",
    "Sym, 32°C" = "#E64B35",
    "Apo, 32°C" = "#F2A7A0"
  )
) +
  
  # ----------------------------
# Linetype: Apo dashed, Sym solid
# ----------------------------
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
    # caption = "* p < 0.05, ** p < 0.01, *** p < 0.001"
  ) +
  
  theme(
    legend.position = c(0.73, 0.45),
    legend.justification = c("center", "center"),
    legend.key.width = unit(2.8, "cm")
  )

fig1_fuller_change

ggsave(
  filename = "Man_Fig1_fuller_Change_fourcolors_dash.png",
  plot = fig1_fuller_change,
  path = "figs",
  device = "png",
  width = 7,
  height = 5,
  units = "in",
  dpi = 600,
  bg = "white"
)



library(tidyverse)
library(janitor)
library(plotrix)
library(patchwork)
library(grid)

# ============================================================
# PANEL B: SUMMER 2025 INOC
# ============================================================

summer2025_long <- read_csv(
  "Summer 2025 Data Sheet - Pedal Lacerate - long_data.csv",
  show_col_types = FALSE
)

fig2025_inoc_data <- summer2025_long %>%
  clean_names() %>%
  mutate(
    sym = as.character(sym),
    temp = as.character(temp),
    dpl = as.numeric(dpl),
    tent_count = na_if(tent_count, "na"),
    tent_count = na_if(tent_count, "NA"),
    tent_count = suppressWarnings(as.numeric(tent_count))
  ) %>%
  mutate(
    sym = case_when(
      sym %in% c("Inoc", "INO", "inoc", "Ino") ~ "Inoc",
      TRUE ~ sym
    ),
    temp_simple = case_when(
      str_detect(temp, "^25") ~ "25",
      str_detect(temp, "^32") ~ "32",
      TRUE ~ temp
    ),
    day = dpl
  ) %>%
  filter(
    sym == "Inoc",
    day %in% c(0, 4, 5, 7, 10, 11, 14, 21)
  ) %>%
  group_by(temp_simple, day) %>%
  summarise(
    mean = mean(tent_count, na.rm = TRUE),
    se = plotrix::std.error(tent_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    treatment = case_when(
      temp_simple == "25" ~ "Inoc, 25°C",
      temp_simple == "32" ~ "Inoc, 32°C"
    ),
    treatment = factor(
      treatment,
      levels = c("Inoc, 25°C", "Inoc, 32°C")
    )
  )

fig2025_inoc_change <- ggplot(
  fig2025_inoc_data,
  aes(x = day, y = mean, color = treatment, linetype = treatment, group = treatment)
) +
  my_theme +
  geom_line(linewidth = 1.2) +
  geom_errorbar(
    aes(ymin = mean - se, ymax = mean + se),
    width = 0.2,
    linewidth = 0.6,
    alpha = 0.8
  ) +
  geom_point(size = 4) +
  ylab("Mean tentacle number") +
  xlab("Days post laceration (dpl)") +
  ggtitle("Summer 2025 Inoc") +
  scale_y_continuous(breaks = seq(0, 10, 2), limits = c(0, 10)) +
  scale_x_continuous(breaks = seq(0, 21, 1), limits = c(0, 21)) +
  scale_color_manual(values = c(
    "Inoc, 25°C" = "#3B6FB6",
    "Inoc, 32°C" = "#E64B35"
  )) +
  scale_linetype_manual(values = c(
    "Inoc, 25°C" = "solid",
    "Inoc, 32°C" = "solid"
  )) +
  labs(color = "Treatment", linetype = "Treatment") +
  theme(
    legend.position = c(0.73, 0.82),
    legend.justification = c("center", "center"),
    legend.key.width = unit(2.8, "cm")
  )

# ============================================================
# PANEL C: URSA INOC
# ============================================================

ursa2024_data <- read.csv(
  "Grace/Experimental Schedule for URSA - Grace Kelly - Grace_tent_count.csv"
) %>%
  clean_names() %>%
  mutate(
    tent_count = suppressWarnings(as.numeric(tent_count)),
    line = as.character(line),
    treatment = as.character(treatment),
    day = as.numeric(day)
  ) %>%
  mutate(
    treatment = gsub("APO", "Apo", treatment),
    treatment = gsub("SYM", "Sym", treatment),
    treatment = gsub("INO", "Ino", treatment),
    treatment = gsub("25C$", "25", treatment),
    treatment = gsub("32C$", "32", treatment)
  ) %>%
  filter(
    line == "H2",
    treatment %in% c("H2-Ino-25", "H2-Ino-32"),
    day %in% c(0, 4, 5, 7, 10, 11, 14, 21)
  )

df_inoc <- ursa2024_data %>%
  mutate(
    treatment = factor(treatment, levels = c("H2-Ino-25", "H2-Ino-32")),
    day = factor(day),
    id = factor(id)
  )

model_inoc <- glmer(
  tent_count ~ treatment * day + (1 | id),
  family = poisson(link = "log"),
  data = df_inoc,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 200000)
  )
)

Anova(model_inoc, type = "II")

emm_inoc <- emmeans(model_inoc, ~ treatment | day)

pairwise_inoc <- pairs(emm_inoc, adjust = "tukey") %>%
  as.data.frame() %>%
  mutate(
    day_num = as.numeric(as.character(day)),
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE            ~ ""
    )
  )

fig_ursa_inoc_data <- ursa2024_data %>%
  group_by(treatment, day) %>%
  summarise(
    mean = mean(tent_count, na.rm = TRUE),
    se = plotrix::std.error(tent_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    treatment = case_when(
      treatment == "H2-Ino-25" ~ "Inoc, 25°C",
      treatment == "H2-Ino-32" ~ "Inoc, 32°C"
    ),
    treatment = factor(
      treatment,
      levels = c("Inoc, 25°C", "Inoc, 32°C")
    )
  )

sig_labels_inoc <- fig_ursa_inoc_data %>%
  group_by(day) %>%
  summarise(
    y = max(mean + se, na.rm = TRUE) + 0.6,
    .groups = "drop"
  ) %>%
  mutate(day_num = day) %>%
  left_join(
    pairwise_inoc %>% select(day_num, sig),
    by = "day_num"
  ) %>%
  filter(sig != "")

fig_ursa_inoc_change <- ggplot(
  fig_ursa_inoc_data,
  aes(x = day, y = mean, color = treatment, linetype = treatment, group = treatment)
) +
  my_theme +
  geom_line(linewidth = 1.2) +
  geom_errorbar(
    aes(ymin = mean - se, ymax = mean + se),
    width = 0.2,
    linewidth = 0.6,
    alpha = 0.8
  ) +
  geom_point(size = 4) +
  geom_text(
    data = sig_labels_inoc,
    aes(x = day_num, y = y, label = sig),
    inherit.aes = FALSE,
    color = "black",
    size = 5
  ) +
  ylab("Mean tentacle number") +
  xlab("Days post laceration (dpl)") +
  scale_y_continuous(
    breaks = seq(0, 12, 2),
    limits = c(0, 12)
  ) +
  scale_x_continuous(
    breaks = seq(0, 21, 1),
    limits = c(0, 21)
  ) +
  scale_color_manual(values = c(
    "Inoc, 25°C" = "#3B6FB6",
    "Inoc, 32°C" = "#E64B35"
  )) +
  scale_linetype_manual(values = c(
    "Inoc, 25°C" = "dotdash",
    "Inoc, 32°C" = "dotdash"
  )) +
  labs(color = "Treatment", linetype = "Treatment") +
  theme(
    legend.position = c(0.73, 0.52),
    legend.justification = c("center", "center"),
    legend.key.width = unit(2.8, "cm")
  )

fig_ursa_inoc_change
pairwise_inoc


# ============================================================
# STACK A / C
# ============================================================

fig1_vertical_AC <- fig1_fuller_change / fig_ursa_inoc_change +
  plot_annotation(tag_levels = "A")

fig1_vertical_AC

fig1_horizontal_AC <- (fig1_fuller_change | fig_ursa_inoc_change) +
  plot_annotation(tag_levels = "A")

fig1_horizontal_AC

# ============================================================
# SAVE FINAL FIGURE
# ============================================================

ggsave(
  filename = "Man_Fig1_AC_vertical.png",
  plot = fig1_vertical_AC,
  path = "figs",
  width = 7,
  height = 9,
  units = "in",
  dpi = 600,
  bg = "white"
)




ggsave(
  filename = "Man_Fig1_AC_horizontal.png",
  plot = fig1_horizontal_AC,
  path = "figs",
  width = 14,
  height = 7,
  units = "in",
  dpi = 600,
  bg = "white"
)
