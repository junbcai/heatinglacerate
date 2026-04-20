# ============================================================
# Figure 1 Sym vs Apo for manuscript
# H2 only with Tukey letters from GLMM
# ============================================================

library(multcomp)
library(emmeans)
select <- dplyr::select
filter <- dplyr::filter
mutate <- dplyr::mutate
lag <- dplyr::lag
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
# GLMM for H2-only data
# ----------------------------
model_fig1 <- glmer(
  tent_count ~ temp * symbiosis * day_cat + (1 | ID),
  family = poisson(link = "log"),
  data = fig1_dat_raw,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 200000)
  )
)

summary(model_fig1)
Anova(model_fig1, type = "II")

# ----------------------------
# Estimated marginal means:
# compare all 4 treatments within each day
# ----------------------------
emm_fig1 <- emmeans(
  model_fig1,
  ~ temp * symbiosis | day_cat,
  type = "response"
)

# Pairwise Tukey if you want the table
pairs(emm_fig1, adjust = "tukey")

# Compact letter display
cld_fig1 <- cld(
  emm_fig1,
  by = "day_cat",
  adjust = "tukey",
  Letters = letters,
  sort = FALSE
)

# ----------------------------
# Clean letters for plotting
# ----------------------------
cld_plot <- as.data.frame(cld_fig1) %>%
  mutate(
    .group = gsub(" ", "", .group),
    
    day = parse_number(as.character(day_cat)),
    
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
# Join letters onto plotting means
# ----------------------------
cld_plot2 <- fig1fuller_data %>%
  left_join(
    cld_plot %>%
      select(temp, symbiosis, day, .group, treatment),
    by = c("temp", "symbiosis", "day", "treatment")
  ) %>%
  mutate(
    y_letter = mean + se + 0.35
  )

# optional check
cld_plot2 %>%
  select(temp, symbiosis, day, treatment, mean, se, .group, y_letter) %>%
  arrange(day, treatment)

# ----------------------------
# Theme
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
  geom_point(size = 2.5, alpha = 0.9) +
  geom_text(
    data = cld_plot2,
    aes(
      x = day,
      y = y_letter,
      label = .group,
      color = treatment
    ),
    size = 4,
    fontface = "bold",
    show.legend = FALSE
  ) +
  ylab("Mean tentacle number") +
  xlab("Days post laceration (dpl)") +
  scale_y_continuous(
    breaks = seq(0, 10, 2),
    limits = c(0, 10.6)
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
  ) +
  theme(
    legend.position = c(0.73, 0.45),
    legend.justification = c("center", "center"),
    legend.key.width = unit(1.8, "cm")
  )

fig1_fuller


cld_plot2 <- fig1fuller_data %>%
  left_join(
    cld_plot %>%
      dplyr::select(temp, symbiosis, day, .group, treatment),
    by = c("temp", "symbiosis", "day", "treatment")
  ) %>%
  mutate(
    treatment = factor(
      treatment,
      levels = c("Apo, 25°C", "Sym, 25°C", "Apo, 32°C", "Sym, 32°C")
    )
  ) %>%
  group_by(day) %>%
  arrange(treatment, .by_group = TRUE) %>%
  mutate(
    y_top = max(mean + se, na.rm = TRUE) + 0.25,
    y_letter = y_top + c(0.00, 0.22, 0.44, 0.66)
  ) %>%
  ungroup()


#ggsave(
  filename = "Man_Fig1_fuller_Change5.png",
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
