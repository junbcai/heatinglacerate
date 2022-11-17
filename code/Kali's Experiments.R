library(ggplot2)
library(ggpubr)
library(plotrix)
library(tidyverse)
library(dplyr)


#Set working director
setwd("~/GitHub/heatinglacerate")

long <- read.csv("data/Kali's Salinity Exp.xlsx - HS+Salinity.csv")

str(long)

long$tent_count <- as.numeric(long$tent_count)
long$ID <- as.factor(long$ID)
long$plate <- as.factor(long$plate)
long$well <- as.factor(long$well)
long$line <- as.factor(long$line)
long$temp <- as.factor(long$temp)
long$treatment <- as.factor(long$treatment)
long$salinity <- as.factor(long$salinity)
long$symbiosis <- as.factor(long$symbiosis)
long$day <- as.numeric(long$day)
long$day_cat <- as.factor(long$day_cat)

newlong <- long


#Comparing Temp and Sal
data_means <- newlong %>%
  group_by(treatment, day) %>%
  summarise(mean = mean(tent_count, na.rm=TRUE),
            se = std.error(tent_count, na.rm=TRUE))

ggplot(data = data_means, aes(x = day, y = mean)) +
  theme_classic(base_size = 15) +
  geom_line(aes(color = treatment, group = treatment), position = position_dodge(0.5)) +
  ylab(bquote("Mean tentacle number"))+
  xlab("Days post laceration (dpl)") +
  ggtitle("Effect of Temperature and Salinity on Pedal Lacerate Tentacle Development in Aiptasia") +
  ylim(0,15) +
  geom_point(aes(color = treatment), size = 2.5, shape = 20, position = position_dodge(0.5)) +
  scale_x_continuous(breaks = round(seq(min(data_means$day), max(data_means$day), by = 1),1)) +
  geom_errorbar(aes(color = treatment, x = day, ymin = mean - se, ymax = mean + se), width = 0.2, position = position_dodge(0.5)) +

  theme(legend.text.align = 0) +
  scale_size_manual(values=c(1.2,1.2,1.2,1.2)) +
  labs(colour = "Treatment") +
  geom_vline(xintercept=c(3), linetype="dashed")


#Comparing Just Sal
data_means <- newlong %>%
  group_by(salinity, day) %>%
  summarise(mean = mean(tent_count, na.rm=TRUE),
            se = std.error(tent_count, na.rm=TRUE))

ggplot(data = data_means, aes(x = day, y = mean)) +
  theme_classic(base_size = 15) +
  geom_line(aes(color = salinity, group = salinity), position = position_dodge(0.5)) +
  ylab(bquote("Mean tentacle number"))+
  xlab("Days post laceration (dpl)") +
  ggtitle("Effect of Temperature and Salinity on Pedal Lacerate Tentacle Development in Aiptasia") +
  ylim(0,15) +
  geom_point(aes(color = salinity), size = 2.5, shape = 20, position = position_dodge(0.5)) +
  scale_x_continuous(breaks = round(seq(min(data_means$day), max(data_means$day), by = 1),1)) +
  geom_errorbar(aes(color = salinity, x = day, ymin = mean - se, ymax = mean + se), width = 0.2, position = position_dodge(0.5)) +
  
  theme(legend.text.align = 0) +
  scale_size_manual(values=c(1.2,1.2,1.2,1.2)) +
  labs(colour = "Treatment")



#Comparing Just Temp 
data_means <- newlong %>%
  group_by(temp, day) %>%
  summarise(mean = mean(tent_count, na.rm=TRUE),
            se = std.error(tent_count, na.rm=TRUE))

View(newlong)

ggplot(data = data_means, aes(x = day, y = mean)) +
  theme_classic(base_size = 15) +
  geom_line(aes(color = temp, group = temp), position = position_dodge(0.5)) +
  ylab(bquote("Mean tentacle number"))+
  xlab("Days post laceration (dpl)") +
  ggtitle("Effect of Temperature and Salinity on Pedal Lacerate Tentacle Development in Aiptasia") +
  ylim(0,15) +
  geom_point(aes(color = temp), size = 2.5, shape = 20, position = position_dodge(0.5)) +
  scale_x_continuous(breaks = round(seq(min(data_means$day), max(data_means$day), by = 1),1)) +
  geom_errorbar(aes(color = temp, x = day, ymin = mean - se, ymax = mean + se), width = 0.2, position = position_dodge(0.5)) +
  
  theme(legend.text.align = 0) +
  scale_size_manual(values=c(1.2,1.2,1.2,1.2)) +
  labs(colour = "Treatment") +
  geom_vline(xintercept=c(3), linetype="dashed")



#Pilot

long <- read.csv("data/Kali's Salinity Exp.xlsx - Pilot.csv")

str(long)

long$tent_count <- as.numeric(long$tent_count)
long$ID <- as.factor(long$ID)
long$plate <- as.factor(long$plate)
long$well <- as.factor(long$well)
long$line <- as.factor(long$line)
long$temp <- as.factor(long$temp)
long$treatment <- as.factor(long$treatment)
long$salinity <- as.factor(long$salinity)
long$symbiosis <- as.factor(long$symbiosis)
long$day <- as.numeric(long$day)
long$day_cat <- as.factor(long$day_cat)

newlong <- long


#Comparing just Sal
data_means <- newlong %>%
  group_by(salinity, day) %>%
  summarise(mean = mean(tent_count, na.rm=TRUE),
            se = std.error(tent_count, na.rm=TRUE))

ggplot(data = data_means, aes(x = day, y = mean)) +
  theme_classic(base_size = 15) +
  geom_line(aes(color = salinity, group = salinity), position = position_dodge(0.5)) +
  ylab(bquote("Mean tentacle number"))+
  xlab("Days post laceration (dpl)") +
  ggtitle("Pliot on Effect of Salinity on Pedal Lacerate Tentacle Development in Aiptasia") +
  ylim(0,15) +
  geom_point(aes(color = salinity), size = 2.5, shape = 20, position = position_dodge(0.5)) +
  scale_x_continuous(breaks = round(seq(min(data_means$day), max(data_means$day), by = 1),1)) +
  geom_errorbar(aes(color = salinity, x = day, ymin = mean - se, ymax = mean + se), width = 0.2, position = position_dodge(0.5)) +
  
  theme(legend.text.align = 0) +
  scale_size_manual(values=c(1.2,1.2,1.2,1.2)) +
  labs(colour = "Treatment") +
  geom_vline(xintercept=c(3), linetype="dashed")




#ANOVA Analysis

anova(aov(tent_count ~ treatment, data=long))
anova(aov(tent_count ~ salinity, data=long))


