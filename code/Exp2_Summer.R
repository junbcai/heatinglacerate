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

#Results of Experiment 2

##Reading data table
long <- read.csv("data/Exp 2 Lacerate Development in Heat Data Sheet - Long Data.csv")
View(long)
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
  mutate(day = as.factor(.$day)) %>%
  mutate(day = recode(day, 0 = "00"))

View(df)

  select(-X) %>%
  
  gather(key = "stream", value = "density.anomaly", -Year, na.rm = TRUE) %>%
  
  mutate(stream = recode(stream, "MAR_MeanSD" = "MAR", "SEC_MeanSD" = "LAK", "BVA_MeanSD" = "BVA", "VCR_MeanSD" = "VAL", "SFS_MeanSD" = "SFS"))


##Saving table as output
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
  labs(colour = "Treatment") +
  geom_vline(xintercept=c(3), linetype="dashed")

ggsave("Figure_Exp2.tif", plot = last_plot(), device = "tiff", path = here("figs"),
       
       width = 11, height = 8, units = "in", dpi = 600)

#ANOVA Analysis

view(long)

anova(aov(tent_count ~ temp*symbiosis*line, data=long))
anova(aov(tent_count ~ treatment, data=long))

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
model <- lmer(tent_count ~ temp*symbiosis*day_cat + line + (1|ID),
              data = data)

plot(model)
qqnorm(residuals(model))
qqline(residuals(model))
Anova(model)


emmeans(model, list(pairwise ~ temp*symbiosis | day_cat), adjust = "tukey")
emmeans(model, list(pairwise ~ temp | day_cat), adjust = "tukey")
emmeans(model, list(pairwise ~ symbiosis | day_cat), adjust = "tukey")
emmeans(model, list(pairwise ~ line | day_cat), adjust = "tukey")




model <- glmer(tent_count_1 ~ treatment * day_cat + (1|ID), family = Gamma,
              data = data)
plot(model)
qqnorm(residuals(model))
qqline(residuals(model))
Anova(model)
emmeans(model, list(pairwise ~ treatment | day_cat), adjust = "tukey")

#NEW GRAPH
newlong

new_df <- newlong[newlong$line  != "CC7", ]

data_means <- new_df %>%
  group_by(treatment, day) %>%
  summarise(mean = mean(tent_count, na.rm=TRUE),
            se = std.error(tent_count, na.rm=TRUE))


ggplot(data = data_means, aes(x = day, y = mean)) +
  theme_classic(base_size = 15) +
  geom_line(aes(color = treatment, group = treatment), position = position_dodge(0.5)) +
  ylab(bquote("Mean tentacle number"))+
  xlab("Days post laceration (dpl)") +
  ggtitle("Effect of Temperature and Symbiosis on Pedal Lacerate Tentacle Development in Aiptasia") +
  ylim(0,15) +
  geom_point(aes(color = treatment), size = 2.5, shape = 20, position = position_dodge(0.5)) +
  scale_x_continuous(breaks = round(seq(min(data_means$day), max(data_means$day), by = 1),1)) +
  geom_errorbar(aes(color = treatment, x = day, ymin = mean - se, ymax = mean + se), width = 0.2, position = position_dodge(0.5)) +
  scale_color_discrete(breaks=c("H2-APO-25C","H2-APO-32C","H2-SYM-25C","H2-SYM-32C")) +
  scale_color_manual(values = c("H2-APO-25C" = "cornflowerblue",
                                "H2-APO-32C" = "orange",
                                "H2-SYM-25C" = "blue",
                                "H2-SYM-32C" = "red"),
                     labels=c("H2-APO-25C",
                              expression(paste("H2-APO-32C")),
                              expression(paste("H2-SYM-25C")),
                              expression(paste("H2-SYM-32C")))) +
  theme(legend.text.align = 0) +
  scale_size_manual(values=c(1.2,1.2,1.2,1.2)) +
  labs(colour = "Treatment")

