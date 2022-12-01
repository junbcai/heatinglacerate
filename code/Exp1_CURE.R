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
setwd("C:/GitHub/heatinglacerate")

#Results of Experiment 1

##Reading data table
long <- read.csv("data/Exp 1 Z362 Master Data Sheet - Long.csv")
View(long)
str(long)

##Converting elements in table
long$tent_count <- as.numeric(long$tent_count)
long$ID <- as.factor(long$ID)
long$section <- as.factor(long$section)
long$feeding <- as.factor(long$feeding)
long$lacerate_num <- as.factor(long$lacerate_num)
long$temp <- as.factor(long$temp)
long$day <- as.numeric(long$day)
long$day_cat <- as.factor(long$day_cat)

##Saving table as output
newlong <- long
saveRDS(newlong, file = "tables/data_table_cURE2022data.rds")


##Graphing results of Experiment 1 on Feeding
newlong$treatment <- newlong$feeding
data_means <- newlong %>%
  group_by(treatment, day) %>%
  summarise(mean = mean(tent_count, na.rm=TRUE),
            se = std.error(tent_count, na.rm=TRUE))

ggplot(data = data_means, aes(x = day, y = mean)) +
  theme_classic(base_size = 15) +
  geom_line(aes(color = treatment, group = treatment), position = position_dodge(0.5)) +
  ylab(bquote("Mean tentacle number"))+
  xlab("Days post laceration (dpl)") +
  ggtitle("Effect of Feeding on Pedal Lacerate Tentacle Development in Aiptasia") +
  ylim(0,15) +
  geom_point(aes(color = treatment), size = 2.5, shape = 20, position = position_dodge(0.5)) +
  scale_x_continuous(breaks = round(seq(min(data_means$day), max(data_means$day), by = 1),1)) +
  geom_errorbar(aes(color = treatment, x = day, ymin = mean - se, ymax = mean + se), width = 0.2, position = position_dodge(0.5)) +
  theme(legend.text.align = 0) +
  scale_size_manual(values=c(1.2,1.2,1.2,1.2)) +
  labs(colour = "Treatment") +
  geom_vline(xintercept=c(3), linetype="dashed")

ggsave("Figure_Exp1_Feeding.tif", plot = last_plot(), device = "tiff", path = here("figs"),
       
       width = 11, height = 8, units = "in", dpi = 600)


##Graphing results of Experiment 1 on Feeding
newlong$treatment <- newlong$temp
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
  theme(legend.text.align = 0) +
  scale_size_manual(values=c(1.2,1.2,1.2,1.2)) +
  labs(colour = "Treatment") +
  geom_vline(xintercept=c(3), linetype="dashed")

ggsave("Figure_Exp1_Temp.tif", plot = last_plot(), device = "tiff", path = here("figs"),
       
       width = 11, height = 8, units = "in", dpi = 600)


#ANOVA Analysis

view(long)

anova(aov(tent_count ~ temp*feeding, data=long))
anova(aov(tent_count ~ feeding, data=long))
anova(aov(tent_count ~ temp, data=long))

#General linear mix model 
library(car)
library(lme4)
library(emmeans)

data <- long

str(data)

# Distribution of the data
hist(data$tent_count)

# Convert column day as a numeric factor
as.factor(data$day)

# Choosing the correct model
model <- lmer(tent_count ~ temp*feeding*day_cat + (1|ID),
              data = data)

plot(model)
qqnorm(residuals(model))
qqline(residuals(model))
Anova(model)


emmeans(model, list(pairwise ~ temp | day_cat), adjust = "tukey")
emmeans(model, list(pairwise ~ feeding | day_cat), adjust = "tukey")



model <- glmer(tent_count_1 ~ treatment * day_cat + (1|ID), family = Gamma,
              data = data)
plot(model)
qqnorm(residuals(model))
qqline(residuals(model))
Anova(model)
emmeans(model, list(pairwise ~ treatment | day_cat), adjust = "tukey")

