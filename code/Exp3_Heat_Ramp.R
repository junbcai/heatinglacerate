library(ggplot2)
library(ggpubr)
library(plotrix)
library(tidyverse)
library(dplyr)
library(car)
library(lme4)
library(emmeans)
library(qqplotr)

#Set working director
setwd("~/GitHub/heatinglacerate")

long <- read.csv("data/Exp 3 Lacerate Development in Heat Data - 25, 33.5, 35.csv")
View(long)

str(long)

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


newlong <- long

data_means <- newlong %>%
  group_by(treatment, day) %>%
  summarise(mean = mean(tent_count, na.rm=TRUE),
            se = std.error(tent_count, na.rm=TRUE))

ggplot(data = data_means, aes(x = day, y = mean)) +
  theme_classic(base_size = 15) +
  geom_line(aes(color = treatment, group = treatment), position = position_dodge(0.5)) +
  ylab(bquote("Mean tentacle number"))+
  xlab("Days post laceration (dpl)") +
  ggtitle("Effect of Temperature, Symbiotic State and Clonal Line on Pedal Lacerate Tentacle Development in Aiptasia") +
  ylim(0,15) +
  geom_point(aes(color = treatment), size = 2.5, shape = 20, position = position_dodge(0.5)) +
  scale_x_continuous(breaks = round(seq(min(data_means$day), max(data_means$day), by = 1),1)) +
  geom_errorbar(aes(color = treatment, x = day, ymin = mean - se, ymax = mean + se), width = 0.2, position = position_dodge(0.5)) +
  scale_color_discrete(breaks=c("H2-SYM-25C (a)","H2-SYM-35C","H2-SYM-25C (b)","H2-SYM-33.5C")) +
  scale_color_manual(values = c("H2-SYM-25C (a)" = "blue",
                                "H2-SYM-35C" = "red",
                                "H2-SYM-25C (b)" = "green",
                                "H2-SYM-33.5C" = "orange"),
                     labels=c("H2-SYM-25C (a)",
                              expression(paste("H2-SYM-35C")),
                              expression(paste("H2-SYM-25C (b)")),
                              expression(paste("H2-SYM-33.5C")))) +
  theme(legend.text.align = 0) +
  scale_size_manual(values=c(1.2,1.2,1.2,1.2)) +
  labs(colour = "Treatment") +
  geom_vline(xintercept=c(3), linetype="dashed")


anova(aov(tent_count ~ temp, data=long))


##Comparing all the experiments on H2


long <- read.csv("data/Exp 3 Lacerate Development in Heat Data - Exp3+Exp2_Data21.csv")
View(long)

str(long)

long$tent_count_1 <- long$tent_count+1

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


newlong <- long

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
  scale_color_discrete(breaks=c("H2-SYM-25C","H2-SYM-32C","H2-SYM-25C (a)","H2-SYM-35C","H2-SYM-25C (b)","H2-SYM-33.5C")) +
  scale_color_manual(values = c("H2-SYM-25C" = "blue",
                                "H2-SYM-32C" = "red",
                                "H2-SYM-25C (a)" = "cyan",
                                "H2-SYM-35C" = "brown",
                                "H2-SYM-25C (b)" = "green",
                                "H2-SYM-33.5C" = "orange"),
                     labels=c("H2-SYM-25C",
                              expression(paste("H2-SYM-32C")),
                              expression(paste("H2-SYM-25C (a)")),
                              expression(paste("H2-SYM-35C")),
                              expression(paste("H2-SYM-25C (b)")),
                              expression(paste("H2-SYM-33.5C")))) +
  theme(legend.text.align = 0) +
  scale_size_manual(values=c(1.2,1.2,1.2,1.2)) +
  labs(colour = "Treatment") +
  geom_vline(xintercept=c(3), linetype="dashed")


#Effect of Temperature

data_means <- newlong %>%
  group_by(temp, day) %>%
  summarise(mean = mean(tent_count, na.rm=TRUE),
            se = std.error(tent_count, na.rm=TRUE))

ggplot(data = data_means, aes(x = day, y = mean)) +
  theme_classic(base_size = 15) +
  geom_line(aes(color = temp, group = temp), position = position_dodge(0.5)) +
  ylab(bquote("Mean tentacle number"))+
  xlab("Days post laceration (dpl)") +
  ggtitle("Effect of Temperature on Pedal Lacerate Tentacle Development in Aiptasia") +
  ylim(0,15) +
  geom_point(aes(color = temp), size = 2.5, shape = 20, position = position_dodge(0.5)) +
  scale_x_continuous(breaks = round(seq(min(data_means$day), max(data_means$day), by = 1),1)) +
  geom_errorbar(aes(color = temp, x = day, ymin = mean - se, ymax = mean + se), width = 0.2, position = position_dodge(0.5)) +
  scale_color_discrete(breaks=c("25C (ambient)","32C (heat stress)","33.5C (heat stress)","35C (heat stress)")) +
  scale_color_manual(values = c("25C (ambient)" = "blue",
                                "32C (heat stress)" = "red",
                                "33.5C (heat stress)" = "orange",
                                "35C (heat stress)" = "brown"),
                     labels=c("25C",
                              expression(paste("32C")),
                              expression(paste("33.5C")),
                              expression(paste("35C")))) +
             theme(legend.text.align = 0) +
             scale_size_manual(values=c(1.2,1.2,1.2,1.2)) +
             labs(colour = "Treatment") +
             geom_vline(xintercept=c(3), linetype="dashed")


#ANOVA Analysis

view(long)

anova(aov(tent_count ~ treatment, data=long))
anova(aov(tent_count ~ temp, data=long))

data <- long


#General linear mix model 


library(car)
library(lme4)
library(emmeans)

# Distribution of the data
hist(data$tent_count)
# Convert column day as a numeric factor
as.factor(data$day)
# Choosing the correct model
model <- lmer(tent_count ~ treatment * day_cat + (1|ID),
              data = data)
plot(model)
qqnorm(residuals(model))
qqline(residuals(model))
Anova(model)

emmeans(model, list(pairwise ~ temp | day_cat), adjust = "tukey")




model <- glmer(tent_count_1 ~ treatment * day_cat + (1|ID), family = Gamma,
              data = data)
plot(model)
qqnorm(residuals(model))
qqline(residuals(model))
Anova(model)
emmeans(model, list(pairwise ~ treatment | day_cat), adjust = "tukey")