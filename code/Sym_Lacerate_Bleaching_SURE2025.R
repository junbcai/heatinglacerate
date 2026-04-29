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


rm(list = ls())
graphics.off()

#Set working directory

getwd()
setwd("/Users/junbc/Documents/GitHub/Need_to_sort/")
list.files()


##Reading data table
long <- read.csv("Sym_Lacerate_Quantification.csv")
str(long)


long <- long %>%
  filter(treatment != "IGNORE", Type != "Tentacle Area")

long$treatment <- as.factor(long$treatment)
long$calculation <- as.numeric(long$calculation)

ggplot(long, aes(x = treatment, y = calculation, color = treatment)) +
  
  # Boxplot with colored outline but no fill
  geom_boxplot(fill = NA, linewidth = 1.1, outlier.shape = NA) +
  
  # Points in same group color
  geom_point(size = 2.5, alpha = 0.8) +
  
  # OPTIONAL: Connect paired points if you have repeated measures
  # geom_line(aes(group = pairing_id), alpha = 0.4, color = "grey60")
  
  scale_color_manual(
    values = c(
      "Sym-Control" = "blue",
      "Sym-HS" = "red"
    )
  ) +
  
  labs(
    title = "Symbiont Density Across Treatment Groups",
    x = "Treatment",
    y = "Symbiont Density"
  ) +
  
  theme_bw(base_size = 16) +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank()
  )


ggplot(long, aes(x = treatment, y = calculation, color = treatment)) +
  geom_boxplot(fill = NA, linewidth = 1.1, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.25, size = 2) +
  scale_color_manual(values = c("Sym-Control" = "blue", "Sym-HS" = "red")) +
  labs(x = "Treatment", y = "Symbiont Density") +
  theme_bw(base_size = 16) +
  theme(legend.position = "none")


install.packages("ggbeeswarm")
library(ggbeeswarm)

ggplot(long, aes(x = treatment, y = calculation, color = treatment)) +
  geom_boxplot(fill = NA, linewidth = 1.1, outlier.shape = NA) +
  geom_quasirandom(alpha = 0.5, size = 2) +
  scale_color_manual(values = c("Sym-Control" = "blue", "Sym-HS" = "red")) +
  labs(x = "Treatment", y = "Symbiont Density") +
  theme_bw(base_size = 16) +
  theme(legend.position = "none")

ggplot(long, aes(x = treatment, y = calculation, color = treatment)) +
  
  # Boxplot: thinner outline, clean style
  geom_boxplot(
    fill = NA,
    linewidth = 0.8,
    outlier.shape = NA
  ) +
  
  # Light jittered points, no stroke
  geom_jitter(
    width = 0.12,
    alpha = 0.3,
    size = 2,
    stroke = 0
  ) +
  
  scale_color_manual(
    values = c("Sym-Control" = "steelblue3",
               "Sym-HS" = "tomato2")
  ) +
  
  labs(
    x = "Treatment",
    y = "Symbiont Density"
  ) +
  
  theme_bw(base_size = 16) +
  theme(
    legend.position = "none",
    
    # Cleaned gridlines
    
    # Remove ALL gridlines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    # Keep no vertical lines either
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    
    # Remove dark border
    panel.border = element_rect(color = "black", linewidth = 0.6)
  )


clean_long  <- long

by(clean_long$calculation, clean_long$treatment,
   function(x) shapiro.test(x))


library(car)
leveneTest(calculation ~ treatment, data = clean_long)

t.test(calculation ~ treatment, data = clean_long, var.equal = TRUE)


##Converting elements in table
long$area <- as.numeric(long$area)
long$ID <- as.factor(long$ID)
long$plate <- as.factor(long$plate)
long$well <- as.factor(long$well)
long$temp <- as.factor(long$temp)
long$treatment <- as.factor(long$treatment)
long$sym <- as.factor(long$sym)
long$day <- as.factor(long$day)
long$dpl <- as.numeric(long$dpl)
long$day_cat <- as.factor(long$day_cat)





##Saving table as output
newlong <- long


##Graphing results of Experiment 2
data_means <- newlong %>%
  group_by(treatment, dpl) %>%
  summarise(
    mean = mean(area, na.rm = TRUE),
    se = std.error(area, na.rm = TRUE),
    n = sum(!is.na(area))  # count non-NA observations
  )

pd <- position_dodge(width = 0.3)

ggplot(data_means, aes(x = dpl, y = mean, color = treatment, group = treatment)) +
  geom_line(position = pd) +
  geom_point(size = 2, position = pd) +
  geom_errorbar(
    aes(ymin = pmax(mean - se, 0), ymax = mean + se),
    width = 0.15,
    position = pd
  ) +
  scale_x_continuous(breaks = sort(unique(data_means$dpl))) +
  labs(
    title = "Experiment 1: Pedal disc area over time",
    x = "Days post laceration (dpl)",
    y = "Pedal disc area (micron^2)",
    color = "Treatment"
  ) +
  theme_classic(base_size = 12) +
  theme(legend.position = "right")



#Sym_25 vs Sym_32
ggplot(
  data_means %>% filter(treatment %in% c("Sym_25","Sym_32")),   # <- inline filter
  aes(x = dpl, y = mean, color = treatment, group = treatment)
) +
  geom_line(position = pd) +
  geom_point(size = 2, position = pd) +
  geom_errorbar(aes(ymin = pmax(mean - se, 0), ymax = mean + se),
                width = 0.15, position = pd) +
  geom_text(aes(label = n), vjust = -1, size = 3, position = pd) +  # <- Add this
  scale_x_continuous(breaks = sort(unique(data_means$dpl))) +
  scale_color_manual(
    values = c("Sym_25" = "#1b9e77",  # Teal green
               "Sym_32" = "#d95f02")  # Orange
  ) +
  labs(
    title = "Experiment 1: Pedal disc area over time",
    x = "Days post laceration (dpl)",
    y = "Pedal disc area (micron^2)",
    color = "Treatment"
  ) +
  theme_classic(base_size = 12)
