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


### =========================
### 3. BUILD MORTALITY DATASET
### =========================


df_mortality <- combined %>%
  filter(day %in% c(14, 21)) %>%
  mutate(
    Mortality = ifelse(is.na(tent_count) | tent_count == 0, "Dead", "Alive"),
    dead = ifelse(Mortality == "Dead", 1, 0),
    day_facet = factor(day, levels = c(14, 21), labels = c("14 dpl", "21 dpl")),
    temp_short = case_when(
      as.character(temp) == "25C (ambient)" ~ "25",
      as.character(temp) == "32C (heat stress)" ~ "32",
      TRUE ~ as.character(temp)
    ),
    treatment_clean = factor(
      paste(line, symbiosis, temp_short, sep = "-"),
      levels = c(
        "H2-Apo-25", "H2-Apo-32",
        "H2-Ino-25", "H2-Ino-32",
        "H2-Sym-25", "H2-Sym-32"
      )
    ),
    Mortality = factor(Mortality, levels = c("Alive", "Dead"))
  )

### checks
table(df_mortality$day_facet)
table(df_mortality$treatment_clean)
table(df_mortality$treatment_clean, df_mortality$day_facet)
table(df_mortality$source)

### =========================
### 4. MORTALITY PLOT
### =========================

p_mortality <- ggplot(df_mortality, aes(x = treatment_clean, fill = Mortality)) +
  geom_bar(position = "fill", color = "white", linewidth = 0.2) +
  facet_wrap(~ day_facet, ncol = 2) +
  scale_fill_manual(values = c("Alive" = "green", "Dead" = "black")) +
  scale_y_continuous(labels = percent, expand = c(0, 0)) +
  labs(
    x = "Treatment group",
    y = "Percent",
    fill = "Mortality"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "sans"),
    axis.title = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    strip.text = element_text(size = 11)
  )

p_mortality

### =========================
### 5. SAVE PLOT
### =========================

ggsave(
  filename = "Mortality_combined_Fig.tif",
  plot = p_mortality,
  path = "figs",
  device = "tiff",
  width = 7,
  height = 5,
  units = "in",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

ggsave(
  filename = "Mortality_combined_Fig.pdf",
  plot = p_mortality,
  path = "figs",
  device = pdf,
  width = 7,
  height = 5,
  units = "in",
  bg = "white"
)

### =========================
### 6. MORTALITY STATS
### =========================

# same structure as your original model
mortality_glm <- glm(
  dead ~ temp * symbiosis,
  data = df_mortality,
  family = binomial
)

summary(mortality_glm)

car::Anova(mortality_glm, type = 3)

### =========================
### 7. OPTIONAL MODEL INCLUDING DATASET SOURCE
### =========================

mortality_glm_source <- glm(
  dead ~ temp * symbiosis + source,
  data = df_mortality,
  family = binomial
)

summary(mortality_glm_source)
car::Anova(mortality_glm_source, type = 3)

### =========================
### 8. OPTIONAL FULL INTERACTION MODEL
### =========================

mortality_glm_full <- glm(
  dead ~ temp * symbiosis * source,
  data = df_mortality,
  family = binomial
)

summary(mortality_glm_full)
car::Anova(mortality_glm_full, type = 3)