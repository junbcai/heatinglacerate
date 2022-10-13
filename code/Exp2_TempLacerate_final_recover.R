library(ggplot2)
library(ggpubr)
library(plotrix)
library(tidyverse)
library(dplyr)


#Set working director
setwd("~/GitHub/heatinglacerate")

long <- read.csv("data/longdata.csv")
View(newlong)

str(long)

long$tent_count <- as.numeric(long$tent_count)
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

