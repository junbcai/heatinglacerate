## Merge the 2022 and URSA Datasets ## 
library(dplyr)
library(ggplot2)
library(scales)
library(janitor)
library(car)

rm(list = ls())
graphics.off()

#Set working directory

getwd()
setwd("~/Documents/GitHub/heatinglacerate")

### =========================
### 1. READ AND CLEAN BOTH DATASETS
### =========================

clean_lacerate_data <- function(file, source_name) {
  df <- read.csv(file) %>%
    janitor::clean_names()
  
  if (!"lacerate" %in% names(df)) {
    df$lacerate <- NA
  }
  
  df %>%
    mutate(
      source = source_name,
      
      id = as.character(id),
      plate = as.character(plate),
      well = as.character(well),
      line = trimws(toupper(as.character(line))),
      treatment = trimws(as.character(treatment)),
      symbiosis = trimws(as.character(symbiosis)),
      temp = trimws(as.character(temp)),
      day_cat = trimws(as.character(day_cat)),
      lacerate = as.character(lacerate),
      
      # 🔥 FIXED (no recode)
      symbiosis = case_when(
        toupper(symbiosis) == "APO" ~ "Apo",
        toupper(symbiosis) == "SYM" ~ "Sym",
        toupper(symbiosis) == "INO" ~ "Ino",
        TRUE ~ symbiosis
      ),
      
      # temp cleanup
      temp = case_when(
        temp == "25C" ~ "25C (ambient)",
        temp == "32C" ~ "32C (heat stress)",
        TRUE ~ temp
      ),
      
      tent_count = as.numeric(tent_count),
      day = as.numeric(day),
      
      id = factor(id),
      plate = factor(plate),
      well = factor(well),
      line = factor(line),
      symbiosis = factor(symbiosis, levels = c("Apo", "Ino", "Sym")),
      temp = factor(temp, levels = c("25C (ambient)", "32C (heat stress)")),
      day_cat = factor(day_cat),
      lacerate = factor(lacerate),
      source = factor(source)
    )
}

df_2022 <- clean_lacerate_data(
  "data/Exp 2 Lacerate Development in Heat Data Sheet - Long Data.csv",
  "2022"
)

df_2024 <- clean_lacerate_data(
  "Grace/Experimental Schedule for URSA - Grace Kelly - Grace_tent_count.csv",
  "URSA_2024"
)

### =========================
### 2. COMBINE DATASETS
### =========================

combined <- bind_rows(df_2022, df_2024) %>%
  filter(line == "H2") %>%
  droplevels()

### optional checks
table(combined$source)
table(combined$line)
table(combined$symbiosis)
table(combined$temp)
table(combined$treatment)
table(combined$day_cat) 

#clean up incconsisten syntax
combined <- combined %>%
  mutate(
    treatment = gsub("APO", "Apo", treatment),
    treatment = gsub("SYM", "Sym", treatment),
    treatment = gsub("INO", "Ino", treatment),
    treatment = gsub("25C$", "25", treatment),
    treatment = gsub("32C$", "32", treatment),
    
    day_cat = as.character(day_cat),
    day_cat = gsub("^([0-9]+)_day$", "day_\\1", day_cat)
  )



library(dplyr)
library(ggplot2)
library(plotrix)

# summarize combined data
data_means <- combined %>%
  filter(treatment %in% c("H2-Apo-25", "H2-Ino-25", "H2-Sym-25")) %>%
  group_by(day, treatment) %>%
  summarise(
    mean = mean(tent_count, na.rm = TRUE),
    se = std.error(tent_count, na.rm = TRUE),
    .groups = "drop"
  )

# plot
p_sym_state <- ggplot(
  data = data_means,
  aes(x = day, y = mean, color = treatment, group = treatment)
) +
  theme_classic(base_size = 15) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 6, shape = 20) +
  geom_errorbar(
    aes(ymin = mean - se, ymax = mean + se),
    width = 0.4,
    linewidth = 1
  ) +
  ylab("Mean tentacle number") +
  xlab("Days post laceration (dpl)") +
  ggtitle("Effect of Symbiotic State on Pedal Lacerate Tentacle Development in Aiptasia") +
  scale_x_continuous(
    breaks = seq(min(data_means$day), max(data_means$day), by = 1)
  ) +
  scale_color_manual(
    values = c(
      "H2-Apo-25" = "Blue",
      "H2-Ino-25" = "Green",
      "H2-Sym-25" = "Brown"
    ),
    labels = c(
      "H2-Apo-25" = "Apo-25",
      "H2-Ino-25" = "Inoc-25",
      "H2-Sym-25" = "Sym-25"
    )
  ) +
  labs(color = "Treatment") +
  coord_cartesian(ylim = c(0, 15)) +
  theme(
    legend.text.align = 0,
    axis.title.x = element_text(size = 24),
    axis.title.y = element_text(size = 24),
    axis.text.x = element_text(size = 20, family = "sans"),
    axis.text.y = element_text(size = 20, family = "sans"),
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 20)
  )

p_sym_state