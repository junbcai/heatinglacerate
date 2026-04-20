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

rm(list = ls())
graphics.off()

#Set working directory

rm(list = ls())
graphics.off()

#Set working directory

getwd()
setwd("~/Documents/GitHub/heatinglacerate")

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

ggplot(data = data_means, aes(x = day, y = mean)) +
  theme_classic(base_size = 15) +
  geom_line(aes(color = treatment, group = treatment), position = position_dodge(0.5)) +
  ylab(bquote("Mean tentacle number"))+
  xlab("Days post laceration (dpl)") +
  ggtitle("Effect of Temperature on Pedal Lacerate Tentacle Development in Aiptasia") +
  ylim(0,15) +
  geom_point(aes(color = treatment), size = 2.5, shape = 20, position = position_dodge(0.5)) +
  scale_x_continuous(breaks = round(seq(min(data_means$day), max(data_means$day), by = 1),1)) +
  geom_errorbar(aes(color = treatment, x = day, ymin = mean - se, ymax = mean + se), width = 0.2, position = position_dodge(0.5)) +
  scale_color_discrete(breaks=c("CC7-APO-25C","CC7-APO-32C","CC7-SYM-25C","CC7-SYM-32C","H2-APO-25C","H2-APO-32C","H2-SYM-25C","H2-SYM-32C")) +
  scale_color_manual(values = c("CC7-APO-25C" = "aquamarine",
                                "CC7-APO-32C" = "chocolate",
                                "CC7-SYM-25C" = "darkorchid",
                                "CC7-SYM-32C" = "coral1",
                                "H2-APO-25C" = "cornflowerblue",
                                "H2-APO-32C" = "orange",
                                "H2-SYM-25C" = "blue",
                                "H2-SYM-32C" = "red"),
                     labels=c("CC7-APO-25C",
                              expression(paste("CC7-APO-32C")),
                              expression(paste("CC7-SYM-25C")),
                              expression(paste("CC7-SYM-32C")),
                              expression(paste("H2-APO-25C")),
                              expression(paste("H2-APO-32C")),
                              expression(paste("H2-SYM-25C")),
                              expression(paste("H2-SYM-32C")))) +
  theme(legend.text.align = 0) +
  scale_size_manual(values=c(1.2,1.2,1.2,1.2)) +
  labs(colour = "Treatment")

ggsave("Figure_Exp2.png", plot = last_plot(), device = "png", path = "figs",
       width = 7, height = 5, units = "in", dpi = 600)


# ============================================================
# Tentacle count analysis for Table S1-S2
# ============================================================
library(lme4)
library(car)
library(emmeans)
library(performance)
library(DHARMa)

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

#Figure 1 Sym vs Apo for manuscript

# Start from the raw cleaned dataset

fig1fuller_data <- newlong %>%
  filter(line != "CC7") %>%
  group_by(temp, symbiosis, day) %>%
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

fig1_fuller <- ggplot(
  fig1fuller_data,
  aes(
    x = day,
    y = mean,
    color = temp,
    shape = symbiosis,
    group = interaction(temp, symbiosis)
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
  
  annotate("segment", x = 5, xend = 11, y = 9.2, yend = 9.2, linewidth = 0.8) +
  annotate("text", x = 8, y = 9.45, label = "***", size = 6) +
  annotate("text", x = 12, y = 9.45, label = "**", size = 6) +
  
  ylab("Mean tentacle number") +
  xlab("Days post laceration (dpl)") +
  scale_y_continuous(breaks = seq(0, 10, 2), limits = c(0, 10)) +
  scale_x_continuous(breaks = seq(min(fig1fuller_data$day), max(fig1fuller_data$day), 1)) +
  
  scale_color_manual(
    values = c(
      "25C (ambient)" = "#3B6FB6",
      "32C (heat stress)" = "#E64B35"
    )
  ) +
  
  scale_shape_manual(
    values = c(
      "Apo" = 16,
      "Sym" = 15
    )
  ) +
  
  labs(
    color = "Temperature",
    shape = "Symbiotic state"
  ) +
  theme(
    legend.position = c(0.72, 0.45),
    legend.justification = c("center", "center")
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

fig1_data <- newlong %>%
  group_by(temp, day) %>%
  summarise(
    mean = mean(tent_count, na.rm = TRUE),
    se = std.error(tent_count, na.rm = TRUE),
    .groups = "drop"
  )

fig1 <- ggplot(fig1_data, aes(x = day, y = mean, color = temp, group = temp)) +
  my_theme +
  geom_line(linewidth = 0.9) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                width = 0.2, linewidth = 0.6) +
  geom_point(size = 4) +
  
  annotate("segment", x = 5, xend = 11, y = 9.2, yend = 9.2, linewidth = 0.8) +
  annotate("text", x = 8, y = 9.45, label = "***", size = 6) +
  annotate("text", x = 12, y = 9.45, label = "**", size = 6) +
  
  ylab("Mean tentacle number") +
  xlab("Days post laceration (dpl)") +
  scale_y_continuous(breaks = seq(0, 10, 2), limits = c(0, 10)) +
  scale_x_continuous(breaks = seq(min(fig1_data$day), max(fig1_data$day), 1)) +
  scale_color_manual(
    values = c(
      "25C (ambient)" = "#3B6FB6",
      "32C (heat stress)" = "#E64B35"
    )
  ) +
  labs(colour = "Treatment") +
  theme(
    legend.position = c(0.72, 0.45),
    legend.justification = c("center", "center")
  )

fig1

ggsave(
  filename = "Man_Fig1.png",
  plot = fig1,
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
  filename = "Man_Fig1.pdf",
  plot = fig1,
  path = "figs",
  device = "pdf",
  width = 7,
  height = 5,
  units = "in",
  bg = "white"
)

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
  filename = "Manuscript_Mortality.png",
  plot = p_mortality,
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
  filename = "Manuscript_Mortality.pdf",
  plot = p_mortality,
  path = "figs",
  device = pdf,
  width = 7,
  height = 5,
  units = "in",
  bg = "white"
)

# =========================
# Flipping the Mortality Plot
# =========================

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
    temp_label = factor(temp_label, levels = c("25°C", "32°C")),
    group_order = case_when(
      temp_label == "25°C" & sym_state == "Aposymbiotic" & day_label == "14 dpl" ~ 1,
      temp_label == "25°C" & sym_state == "Symbiotic"    & day_label == "14 dpl" ~ 2,
      temp_label == "25°C" & sym_state == "Aposymbiotic" & day_label == "21 dpl" ~ 3,
      temp_label == "25°C" & sym_state == "Symbiotic"    & day_label == "21 dpl" ~ 4,
      temp_label == "32°C" & sym_state == "Aposymbiotic" & day_label == "14 dpl" ~ 1,
      temp_label == "32°C" & sym_state == "Symbiotic"    & day_label == "14 dpl" ~ 2,
      temp_label == "32°C" & sym_state == "Aposymbiotic" & day_label == "21 dpl" ~ 3,
      temp_label == "32°C" & sym_state == "Symbiotic"    & day_label == "21 dpl" ~ 4
    ),
    axis_text = case_when(
      sym_state == "Aposymbiotic" & day_label == "14 dpl" ~ "Apo\n14 dpl",
      sym_state == "Symbiotic"    & day_label == "14 dpl" ~ "Sym\n14 dpl",
      sym_state == "Aposymbiotic" & day_label == "21 dpl" ~ "Apo\n21 dpl",
      sym_state == "Symbiotic"    & day_label == "21 dpl" ~ "Sym\n21 dpl"
    )
  ) %>%
  arrange(temp_label, group_order) %>%
  group_by(temp_label) %>%
  mutate(
    bar_label = factor(axis_text, levels = rev(unique(axis_text)))
  ) %>%
  ungroup() %>%
  mutate(
    # keep TRUE labels in the data
    bar_text = scales::percent(mortality_prop, accuracy = 1)
  )

# =========================
# PLOT
# =========================

p_mortality_flipped <- ggplot(
  mortality_plot_df_flipped,
  aes(x = bar_label, y = mortality_prop)
) +
  geom_col(
    fill = "grey60",
    width = 0.82
  ) +
  geom_col(
    aes(y = 1, color = temp_label),
    fill = NA,
    linewidth = 0.9,
    width = 0.82
  ) +
  
  # =========================
# LABELS (kept but hidden)
# =========================
# geom_text(
#   aes(
#     y = mortality_prop / 2,
#     label = bar_text
#   ),
#   size = 4.5,
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
    strip.text.y.right = element_text(size = 14, face = "bold"),
    strip.background = element_rect(
      fill = "white",
      color = "black",
      linewidth = 1
    ),
    panel.spacing.y = unit(0.9, "lines"),
    axis.text.y = element_text(size = 14, lineheight = 0.9),
    axis.text.x = element_text(size = 14),
    plot.margin = margin(8, 14, 8, 8)
  )

p_mortality_flipped

ggsave(
  filename = "Manuscript_Mortality_flipped.png",
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
  filename = "Manuscript_Mortality_flipped.pdf",
  plot = p_mortality_flipped,
  path = "figs",
  device = pdf,
  width = 7,
  height = 5,
  units = "in",
  bg = "white"
)

### =========================
### 3. MORTALITY STATS; S3 table
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
      "H2-Apo-25" = "Apo-25",
      "H2-Ino-25" = "Inoc-25",
      "H2-Sym-25" = "Sym-25"
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
      "H2-Apo-25" = "Apo-25",
      "H2-Ino-25" = "Inoc-25",
      "H2-Sym-25" = "Sym-25"
    )
  ) +
  
  labs(color = "Treatment", shape = "Treatment") +
  
  theme(
    legend.position = c(0.75, 0.45),
    legend.justification = c("center", "center")
  )

p_sym_state_shape


p_sym_state_temp <- ggplot(
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
  aes(x = day, y = mean,
      group = treatment,
      color = temp,
      shape = state)
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
  
  # Color = temperature
  scale_color_manual(
    values = c(
      "25°C" = "#3B6FB6",
      "32°C" = "#D55E00"
    )
  ) +
  
  # Shape = symbiotic state
  scale_shape_manual(
    values = c(
      "Apo" = 16,   # ●
      "Inoc" = 17,  # ▲
      "Sym" = 15    # ■
    )
  ) +
  
  labs(color = "Temperature", shape = "Symbiotic state") +
  
  theme(
    legend.position = c(0.75, 0.35),
    legend.justification = c("center", "center")
  )

p_sym_state_temp

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
      "H2-Apo-25" = "Apo-25",
      "H2-Ino-25" = "Inoc-25",
      "H2-Sym-25" = "Sym-25"
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
