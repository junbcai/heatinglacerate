library(dplyr)
library(ggplot2)
library(plotrix)
library(scales)
library(janitor)
library(lme4)
library(car)
library(emmeans)
library(grid)

rm(list = ls())
graphics.off()

setwd("~/Documents/GitHub/heatinglacerate")

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

### =========================
### 2. MORTALITY DATA
### =========================

df_mortality <- ursa2024_data %>%
  filter(
    line == "H2",
    day_cat %in% c("day_14", "day_21")
  ) %>%
  mutate(
    Mortality = ifelse(is.na(tent_count) | tent_count == 0, "Dead", "Alive"),
    dead = ifelse(Mortality == "Dead", 1, 0),
    Mortality = factor(Mortality, levels = c("Alive", "Dead")),
    day_cat = factor(day_cat, levels = c("day_14", "day_21")),
    treatment = factor(
      treatment,
      levels = c(
        "H2-Apo-25", "H2-Apo-32",
        "H2-Ino-25", "H2-Ino-32",
        "H2-Sym-25", "H2-Sym-32"
      )
    )
  )

nrow(df_mortality)
table(df_mortality$day_cat)
table(df_mortality$treatment)
table(df_mortality$Mortality)

### =========================
### 3. STANDARD MORTALITY PLOT
### =========================

p_mortality <- ggplot(df_mortality, aes(x = treatment, fill = Mortality)) +
  geom_bar(position = "fill", color = "white", linewidth = 0.2) +
  labs(
    x = "Treatment Group",
    y = "Percent",
    fill = "Mortality"
  ) +
  facet_wrap(
    ~ day_cat,
    ncol = 2,
    labeller = as_labeller(c("day_14" = "14 dpl", "day_21" = "21 dpl"))
  ) +
  scale_x_discrete(
    labels = c(
      "H2-Apo-25" = "Apo (25°C)",
      "H2-Apo-32" = "Apo (32°C)",
      "H2-Ino-25" = "Ino (25°C)",
      "H2-Ino-32" = "Ino (32°C)",
      "H2-Sym-25" = "Sym (25°C)",
      "H2-Sym-32" = "Sym (32°C)"
    )
  ) +
  scale_fill_manual(values = c("Alive" = "green", "Dead" = "black")) +
  scale_y_continuous(labels = percent, expand = c(0, 0)) +
  theme_minimal() +
  theme(
    text = element_text(family = "sans"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    strip.text = element_text(size = 11)
  )

p_mortality

ggsave(
  filename = "Manuscript_Mortality_INOC.png",
  plot = p_mortality,
  path = "figs",
  device = "png",
  width = 7,
  height = 5,
  units = "in",
  dpi = 600,
  bg = "white"
)

ggsave(
  filename = "Manuscript_Mortality_INOC.pdf",
  plot = p_mortality,
  path = "figs",
  device = pdf,
  width = 7,
  height = 5,
  units = "in",
  bg = "white"
)

### =========================
### 4. FLIPPED MORTALITY PLOT
### =========================

my_theme <- theme_classic(base_size = 14) +
  theme(
    text = element_text(family = "sans", size = 14),
    axis.text = element_text(size = 14, colour = "black"),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14)
  )

mortality_plot_df_flipped <- df_mortality %>%
  mutate(
    sym_state = case_when(
      grepl("Apo", treatment) ~ "Aposymbiotic",
      grepl("Ino", treatment) ~ "Inoculated",
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
    temp_label = factor(temp_label, levels = c("25°C", "32°C")),
    group_order = case_when(
      temp_label == "25°C" & sym_state == "Aposymbiotic" & day_label == "14 dpl" ~ 1,
      temp_label == "25°C" & sym_state == "Inoculated"   & day_label == "14 dpl" ~ 2,
      temp_label == "25°C" & sym_state == "Symbiotic"    & day_label == "14 dpl" ~ 3,
      temp_label == "25°C" & sym_state == "Aposymbiotic" & day_label == "21 dpl" ~ 4,
      temp_label == "25°C" & sym_state == "Inoculated"   & day_label == "21 dpl" ~ 5,
      temp_label == "25°C" & sym_state == "Symbiotic"    & day_label == "21 dpl" ~ 6,
      temp_label == "32°C" & sym_state == "Aposymbiotic" & day_label == "14 dpl" ~ 1,
      temp_label == "32°C" & sym_state == "Inoculated"   & day_label == "14 dpl" ~ 2,
      temp_label == "32°C" & sym_state == "Symbiotic"    & day_label == "14 dpl" ~ 3,
      temp_label == "32°C" & sym_state == "Aposymbiotic" & day_label == "21 dpl" ~ 4,
      temp_label == "32°C" & sym_state == "Inoculated"   & day_label == "21 dpl" ~ 5,
      temp_label == "32°C" & sym_state == "Symbiotic"    & day_label == "21 dpl" ~ 6
    ),
    axis_text = case_when(
      sym_state == "Aposymbiotic" & day_label == "14 dpl" ~ "Apo, 14 dpl",
      sym_state == "Inoculated"   & day_label == "14 dpl" ~ "Inoc, 14 dpl",
      sym_state == "Symbiotic"    & day_label == "14 dpl" ~ "Sym, 14 dpl",
      sym_state == "Aposymbiotic" & day_label == "21 dpl" ~ "Apo, 21 dpl",
      sym_state == "Inoculated"   & day_label == "21 dpl" ~ "Inoc, 21 dpl",
      sym_state == "Symbiotic"    & day_label == "21 dpl" ~ "Sym, 21 dpl"
    )
  ) %>%
  arrange(temp_label, group_order) %>%
  group_by(temp_label) %>%
  mutate(
    bar_label = factor(axis_text, levels = rev(unique(axis_text)))
  ) %>%
  ungroup() %>%
  mutate(
    bar_text = scales::percent(mortality_prop, accuracy = 1)
  )

p_mortality_flipped <- ggplot(
  mortality_plot_df_flipped,
  aes(x = bar_label, y = mortality_prop)
) +
  geom_col(
    fill = "grey60",
    width = 0.72
  ) +
  geom_col(
    aes(y = 1, color = temp_label),
    fill = NA,
    linewidth = 0.9,
    width = 0.72
  ) +
  # geom_text(
  #   aes(
  #     y = mortality_prop / 2,
  #     label = bar_text
  #   ),
  #   size = 4.2,
  #   color = "black"
  # ) +
  coord_flip() +
  facet_grid(
    rows = vars(temp_label),
    scales = "free_y",
    space = "free_y",
    switch = "y"
  ) +
  scale_color_manual(
    values = c(
      "25°C" = "#3B6FB6",
      "32°C" = "#D55E00"
    ),
    guide = "none"
  ) +
  scale_y_continuous(
    labels = percent,
    limits = c(0, 1),
    breaks = c(0, 0.25, 0.5, 0.75, 1),
    expand = expansion(mult = c(0, 0.04))
  ) +
  labs(
    x = "Treatment Group",
    y = "Percent Mortality"
  ) +
  my_theme +
  theme(
    panel.grid = element_blank(),
    strip.placement = "outside",
    strip.text.y.right = element_text(size = 13, face = "bold"),
    strip.background = element_rect(
      fill = "white",
      color = "black",
      linewidth = 1
    ),
    panel.spacing.y = unit(1.3, "lines"),
    axis.text.y = element_text(size = 11, lineheight = 0.95),
    axis.text.x = element_text(size = 13),
    axis.title.y = element_text(size = 16, margin = margin(r = 18)),
    axis.title.x = element_text(size = 16),
    plot.margin = margin(10, 18, 10, 16)
  )

p_mortality_flipped

ggsave(
  filename = "Manuscript_Mortality_flipped_INOC.png",
  plot = p_mortality_flipped,
  path = "figs",
  device = "png",
  width = 7,
  height = 5,
  units = "in",
  dpi = 600,
  bg = "white"
)

ggsave(
  filename = "Manuscript_Mortality_flipped_INOC.pdf",
  plot = p_mortality_flipped,
  path = "figs",
  device = pdf,
  width = 7,
  height = 5,
  units = "in",
  bg = "white"
)