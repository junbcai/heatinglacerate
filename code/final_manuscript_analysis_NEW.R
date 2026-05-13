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
saveRDS(newlong, file = "tables/Data_Table_Summer2022Data.RDS")

##Graphing results of Experiment 2
data_means <- newlong %>%
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

ggsave("Figure_Exp2.png", plot = All_2022_plot, device = "png", path = "figs",
       width = 7, height = 5, units = "in", dpi = 600)


# ============================================================
# Tentacle count analysis for Table S1-S2
# ============================================================
library(lme4)
library(car)
library(emmeans)
library(performance)
library(DHARMa)

#Remove CC7
data <- newlong %>%
  filter(line != "CC7") %>%
  mutate(
    day_cat = factor(day_cat),
    temp = factor(temp),
    symbiosis = factor(symbiosis),
    ID = factor(ID)
  )

data <- newlong

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
Anova(model_pois, type = "II")

# ----------------------------
# Post hoc comparisons of temperature effect; S2 table
# ----------------------------
emm_temp <- emmeans(model_pois, ~ temp | day_cat, type = "response")
pairs(emm_temp, adjust = "tukey") 

# ----------------------------
# Post hoc comparisons of symbiosis effect
# ----------------------------
emm_symbiosis <- emmeans(model_pois, ~ symbiosis | day_cat, type = "response")
pairs(emm_symbiosis, adjust = "tukey") 

# ----------------------------
# Figure 1 Sym vs Apo for manuscript
# ----------------------------

# Start from the raw cleaned dataset
fig1_dat_raw <- newlong %>%
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
  filename = "Man_Fig1_fuller.png",
  plot = fig1_fuller,
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
  filename = "Man_Fig1_fuller.pdf",
  plot = fig1_fuller,
  path = "figs",
  device = "pdf",
  width = 7,
  height = 5,
  units = "in",
  bg = "white"
)


# =========================
# 2022 MORTALITY FIGURE, H2 ONLY
# =========================

# -------------------------
# Prepare mortality data
# -------------------------
df_mortality_2022 <- newlong %>%
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


### =========================
### 2024 DATA
### =========================


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

setwd("~/Documents/GitHub/heatinglacerate")


select <- dplyr::select
filter <- dplyr::filter
mutate <- dplyr::mutate
lag <- dplyr::lag

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
### 2. MORTALITY FIGURE
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

ggsave(
  filename = "Manuscript_survival_flipped_outline.png",
  plot = p_survival_flipped_outline,
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
  filename = "Manuscript_survival_flipped_outline.pdf",
  plot = p_survival_flipped_outline,
  path = "figs",
  device = pdf,
  width = 7,
  height = 5,
  units = "in",
  bg = "white"
)


p_survival_flipped_colorbar

ggsave(
  filename = "Manuscript_survival_flipped_colorbar.png",
  plot = p_survival_flipped_colorbar,
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
  filename = "Manuscript_survival_flipped_colorbar.pdf",
  plot = p_survival_flipped_colorbar,
  path = "figs",
  device = pdf,
  width = 7,
  height = 5,
  units = "in",
  bg = "white"
)

### =========================
### 3A. MORTALITY STATS; S3 table
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
### 3B. SURVIVAL STATS; S3 table
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
### 4. p_sym_state FIGURE
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

ggsave(
  filename = "URSA2024_ApoInocSym_Figure.png",
  plot = p_sym_state,
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
  filename = "URSA2024_ApoInocSym_Figure.pdf",
  plot = p_sym_state,
  path = "figs",
  device = pdf,
  width = 7,
  height = 5,
  units = "in",
  bg = "white"
)

### =========================
### INOC+SYM+APO 2024 FIGURE
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

ggsave(
  filename = "URSA2024_ApoInocSym_heat_Figure_shapes.png",
  plot = p_sym_state_color_shape_2024,
  path = "figs",
  device = "png",
  width = 7,
  height = 5,
  units = "in",
  dpi = 600,
  bg = "white"
)

ggsave(
  filename = "URSA2024_ApoInocSym_heat_Figure_shapes.pdf",
  plot = p_sym_state_color_shape_2024,
  path = "figs",
  device = pdf,
  width = 7,
  height = 5,
  units = "in",
  bg = "white"
)

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

ggsave(
  filename = "URSA2024_ApoInocSym_heat_Figure_linetype.png",
  plot = p_sym_state_color_linetype_2024,
  path = "figs",
  device = "png",
  width = 7,
  height = 5,
  units = "in",
  dpi = 600,
  bg = "white"
)

ggsave(
  filename = "URSA2024_ApoInocSym_heat_Figure_linetype.pdf",
  plot = p_sym_state_color_linetype_2024,
  path = "figs",
  device = pdf,
  width = 7,
  height = 5,
  units = "in",
  bg = "white"
)




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

ggsave(
  filename = "URSA2024_ApoInocSym_heat_Figure_line_shape_color.png",
  plot = p_sym_state_full_encoding_2024,
  path = "figs",
  device = "png",
  width = 7,
  height = 5,
  units = "in",
  dpi = 600,
  bg = "white"
)

ggsave(
  filename = "URSA2024_ApoInocSym_heat_Figure_line_shape_color.pdf",
  plot = p_sym_state_full_encoding_2024,
  path = "figs",
  device = pdf,
  width = 7,
  height = 5,
  units = "in",
  bg = "white"
)


### =========================
### 4B. INOC ONLY 2024 FIGURE
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

ggsave(
  filename = "URSA2024_InocOnly_Figure.png",
  plot = p_inoc_only,
  path = "figs",
  device = "png",
  width = 7,
  height = 5,
  units = "in",
  dpi = 600,
  bg = "white"
)

ggsave(
  filename = "URSA2024_InocOnly_Figure.pdf",
  plot = p_inoc_only,
  path = "figs",
  device = "pdf",
  width = 7,
  height = 5,
  units = "in",
  bg = "white"
)

library(patchwork)
fig1_fuller_12 <- fig1_fuller +
  scale_y_continuous(
    breaks = seq(0, 12, 2),
    limits = c(0, 12)
  )

fig1_fuller_12
combined_panel <- fig1_fuller_12 + p_inoc_only +
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = "A")

combined_panel

### =========================
### 5. p_sym_state STATS
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
# Type II ANOVA; S4 table
# ----------------------------
Anova(model_sym_state, type = "II") 

# ----------------------------
# Estimated marginal means
# ----------------------------
emm_sym <- emmeans(model_sym_state, ~ treatment | day)

# Pairwise comparisons with Tukey adjustment; S5 table
pairs(emm_sym, adjust = "tukey")


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

setwd("~/Documents/GitHub/heatinglacerate")

# ----------------------------
# Read heat-ramp dataset
# ----------------------------
supp_heat_ramp_long_collapsed <- read.csv("data/Exp3-heat ramp_filled.csv")

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
  filename = "Supp_Fig_heat_ramp.png",
  plot = supp_heat_ramp_fig,
  path = "figs",
  device = "png",
  width = 7,
  height = 5,
  units = "in",
  dpi = 600,
  bg = "white"
)

ggsave(
  filename = "Supp_Fig_heat_ramp.pdf",
  plot = supp_heat_ramp_fig,
  path = "figs",
  device = "pdf",
  width = 7,
  height = 5,
  units = "in",
  bg = "white"
)



# =========================================================
# SUPPLEMENTARY HEAT RAMP STATS
# =========================================================
# ----------------------------
# Prepare data (exclude 35 °C)
# ----------------------------
df_heat_nb <- supp_heat_ramp_data  %>%
  dplyr::filter(temp %in% c("25C", "32C", "33.5C")) %>%
  dplyr::mutate(
    temp = factor(temp, levels = c("25C", "32C", "33.5C")),
    day = factor(day),
    ID = factor(ID)
  )

# ----------------------------
# Fit Negative Binomial GLMM
# ----------------------------
library(glmmTMB)

model_heat_nb <- glmmTMB(
  tent_count ~ temp * day + (1 | ID),
  family = nbinom2,
  data = df_heat_nb
)

# ----------------------------
# Model checks
# ----------------------------
library(DHARMa)

sim_res_nb <- simulateResiduals(model_heat_nb)
plot(sim_res_nb)

testDispersion(sim_res_nb)
testZeroInflation(sim_res_nb)
testUniformity(sim_res_nb)

# ----------------------------
# Type II ANOVA
# ----------------------------
library(car)
Anova(model_heat_nb, type = "II") 

# ----------------------------
# Estimated marginal means
# ----------------------------
library(emmeans)

emm_heat_nb <- emmeans(model_heat_nb, ~ temp | day)
pairs(emm_heat_nb, adjust = "tukey")

# ----------------------------
# Compact letter display
# ----------------------------
heat_nb_cld <- emmeans(model_heat_nb, ~ temp | day) %>%
  multcomp::cld(adjust = "tukey", Letters = letters) %>%
  as.data.frame()

heat_nb_cld$.group <- gsub(" ", "", heat_nb_cld$.group)




# =========================================================
# FINAL STATS PIPELINE FOR SUPPLEMENTARY HEAT RAMP
# Growth phase only: 25C, 32C, 33.5C; days 5-14
# =========================================================

library(dplyr)
library(glmmTMB)
library(DHARMa)
library(car)
library(emmeans)
library(multcomp)
library(multcompView)

# ----------------------------
# 1. Prepare data
# ----------------------------
df_heat_final <- supp_heat_ramp_data  %>%
  dplyr::filter(
    temp %in% c("25C", "32C", "33.5C"),
    as.numeric(as.character(day)) >= 5
  ) %>%
  dplyr::mutate(
    temp = factor(temp, levels = c("25C", "32C", "33.5C")),
    day = factor(day),
    ID = factor(ID)
  )

# Optional checks
str(df_heat_final)
table(df_heat_final$temp, df_heat_final$day)

# ----------------------------
# 2. Explore distribution
# ----------------------------
hist(df_heat_final$tent_count)

# ----------------------------
# 3. Fit negative binomial GLMM
# ----------------------------
model_heat_final <- glmmTMB(
  tent_count ~ temp * day + (1 | ID),
  family = nbinom2,
  data = df_heat_final
)

summary(model_heat_final)

# ----------------------------
# 4. Model checks
# ----------------------------
sim_res_final <- simulateResiduals(model_heat_final)
plot(sim_res_final)

testDispersion(sim_res_final)
testZeroInflation(sim_res_final)
testUniformity(sim_res_final)

# Optional additional residual checks
#plotResiduals(sim_res_final, as.numeric(as.character(df_heat_final$day)))
#plotResiduals(sim_res_final, df_heat_final$temp)

# ----------------------------
# 5. Type II ANOVA
# ----------------------------
Anova(model_heat_final, type = "II")

# If Anova gives trouble, use this instead:
# car::Anova(model_heat_final)

# ----------------------------
# 6. Estimated marginal means
# ----------------------------
emm_heat_final <- emmeans(model_heat_final, ~ temp | day)

# Pairwise comparisons with Tukey adjustment
pairs(emm_heat_final, adjust = "tukey")

# ----------------------------
# 7. Compact letter display
# ----------------------------
heat_final_cld <- emmeans(model_heat_final, ~ temp | day) %>%
  multcomp::cld(adjust = "tukey", Letters = letters) %>%
  as.data.frame()

heat_final_cld$.group <- gsub(" ", "", heat_final_cld$.group)

# View letters table
heat_final_letters_table <- heat_final_cld %>%
  dplyr::select(day, temp, emmean, SE, .group) %>%
  dplyr::arrange(as.numeric(as.character(day)), temp)

heat_final_letters_table

# Optional wide table
heat_final_letters_wide <- heat_final_letters_table %>%
  tidyr::pivot_wider(
    names_from = temp,
    values_from = .group
  )

heat_final_letters_wide

# =========================================================
# 8. ADD LETTERS TO EXISTING GGPLOT
# Uses objects already created earlier:
#   supp_heat_ramp_fig_data
#   supp_heat_ramp_fig
# =========================================================

# Keep only days included in the final model
supp_heat_ramp_fig_data_final <- supp_heat_ramp_fig_data %>%
  dplyr::filter(
    temp %in% c("25C", "32C", "33.5C"),
    day >= 5
  )

# Merge letters onto plotting data
supp_heat_ramp_letters_final <- supp_heat_ramp_fig_data_final %>%
  left_join(
    heat_final_cld %>%
      dplyr::select(day, temp, .group) %>%
      dplyr::mutate(day = as.numeric(as.character(day))),
    by = c("temp", "day")
  ) %>%
  group_by(day) %>%
  mutate(
    day_top = max(mean + se, na.rm = TRUE),
    letter_y = case_when(
      temp == "25C"   ~ day_top + 0.20,
      temp == "32C"   ~ day_top + 0.45,
      temp == "33.5C" ~ day_top + 0.70
    )
  ) %>%
  ungroup()

# Add letters to original plot
supp_heat_ramp_fig_letters_final <- supp_heat_ramp_fig +
  geom_text(
    data = supp_heat_ramp_letters_final,
    aes(
      x = day,
      y = letter_y,
      label = .group,
      color = temp
    ),
    size = 4,
    show.legend = FALSE
  ) +
  scale_y_continuous(
    breaks = seq(0, 15, 2),
    limits = c(0, 10.8)
  )

supp_heat_ramp_fig_letters_final

# ----------------------------
# 9. Save updated figure
# ----------------------------
ggsave(
  filename = "Supp_Fig_heat_ramp_letters_final.png",
  plot = supp_heat_ramp_fig_letters_final,
  path = "figs",
  device = "png",
  width = 7,
  height = 5,
  units = "in",
  dpi = 600,
  bg = "white"
)

ggsave(
  filename = "Supp_Fig_heat_ramp_letters_final.pdf",
  plot = supp_heat_ramp_fig_letters_final,
  path = "figs",
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