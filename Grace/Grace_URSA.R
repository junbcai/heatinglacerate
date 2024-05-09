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
long <- read.csv("Grace/Experimental Schedule for URSA - Grace Kelly - Grace_tent_count.csv")
View(long)
str(long)

##Converting elements in table
long$Tent_count <- as.numeric(long$Tent_count)
long$ID <- as.factor(long$ID)
long$Plate <- as.factor(long$Plate)
long$Well <- as.factor(long$Well)
long$Line <- as.factor(long$Line)
long$Temp <- as.factor(long$Temp)
long$Treatment <- as.factor(long$Treatment)
long$Symbiosis <- as.factor(long$Symbiosis)
long$Lacerate <- as.factor(long$Lacerate)
long$Day <- as.numeric(long$Day)
long$Day_cat <- as.factor(long$Day_cat)


df <- long %>%
  mutate(Day = as.factor(.$Day)) %>%
  mutate(Day = recode(Day, 0 = "00"))

View(df)

select(-X) %>%
  
  gather(key = "stream", value = "density.anomaly", -Year, na.rm = TRUE) %>%
  
  mutate(stream = recode(stream, "MAR_MeanSD" = "MAR", "SEC_MeanSD" = "LAK", "BVA_MeanSD" = "BVA", "VCR_MeanSD" = "VAL", "SFS_MeanSD" = "SFS"))


##Saving table as output
newlong <- long
newlong

##Graphing results of Experiment 2
data_means <- newlong %>%
  group_by(Treatment, Day) %>%
  summarise(mean = mean(Tent_count, na.rm=TRUE),
            se = std.error(Tent_count, na.rm=TRUE))

#Everything
ggplot(data = data_means, aes(x = Day, y = mean)) +
  theme_classic(base_size = 15) +
  geom_line(aes(color = Treatment, group = Treatment), position = position_dodge(0.5)) +
  ylab(bquote("Mean tentacle number"))+
  xlab("Days post laceration (dpl)") +
  ggtitle("Effect of Temperature on Pedal Lacerate Tentacle Development in Aiptasia") +
  geom_point(aes(color = Treatment), size = 2.5, shape = 20, position = position_dodge(0.5)) +
  scale_x_continuous(breaks = round(seq(min(data_means$Day), max(data_means$Day), by = 1),1)) +
  geom_errorbar(aes(color = Treatment, x = Day, ymin = mean - se, ymax = mean + se), width = 0.2, position = position_dodge(0.5)) +
  scale_color_discrete(breaks=c("H2-Apo-25","H2-Apo-32","H2-Ino-25","H2-Ino-3","H2-SYM-Sym","H2-Sym-32")) +
  scale_color_manual(values = c("H2-Apo-25" = "aquamarine",
                                "H2-Apo-32" = "chocolate",
                                "H2-Ino-25" = "darkorchid",
                                "H2-Ino-32" = "coral1",
                                "H2-Sym-25" = "blue",
                                "H2-Sym-32" = "red"),
                     labels=c("H2-Apo-25",
                              expression(paste("H2-Apo-32")),
                              expression(paste("H2-Ino-25")),
                              expression(paste("H2-Ino-32")),
                              expression(paste("H2-Sym-25")),
                              expression(paste("H2-Sym-32")))) +
  theme(legend.text.align = 0) +
  scale_size_manual(values=c(1.2,1.2,1.2,1.2)) +
  labs(colour = "Treatment")

#Ino vs Ino

ggplot(data = data_means[data_means$Treatment %in% c("H2-Ino-25", "H2-Ino-32"), ], aes(x = Day, y = mean)) +
  theme_classic(base_size = 15) +
  geom_line(aes(color = Treatment, group = Treatment), position = position_dodge(0.5)) +
  ylab(bquote("Mean tentacle number")) +
  xlab("Days post laceration (dpl)") +
  ggtitle("Effect of Temperature on Inoculated Pedal Lacerate Tentacle Development in Aiptasia") +
  geom_point(aes(color = Treatment), size = 2.5, shape = 20, position = position_dodge(0.5)) +
  scale_x_continuous(breaks = round(seq(min(data_means$Day), max(data_means$Day), by = 1), 1)) +
  geom_errorbar(aes(color = Treatment, x = Day, ymin = mean - se, ymax = mean + se), width = 0.2, position = position_dodge(0.5)) +
  scale_color_manual(values = c("H2-Ino-25" = "Blue",
                                "H2-Ino-32" = "Red"),
                     labels = c("H2-Ino-25", "H2-Ino-32")) +
  theme(legend.text.align = 0) +
  scale_size_manual(values = c(1.2, 1.2)) +
  labs(colour = "Treatment") +
  coord_cartesian(ylim = c(0, 15))


#Apo vs Apo
ggplot(data = data_means[data_means$Treatment %in% c("H2-Apo-25", "H2-Apo-32"), ], aes(x = Day, y = mean)) +
  theme_classic(base_size = 15) +
  geom_line(aes(color = Treatment, group = Treatment), position = position_dodge(0.5)) +
  ylab(bquote("Mean tentacle number")) +
  xlab("Days post laceration (dpl)") +
  ggtitle("Effect of Temperature on Aposymbiotic Pedal Lacerate Tentacle Development in Aiptasia") +
  geom_point(aes(color = Treatment), size = 2.5, shape = 20, position = position_dodge(0.5)) +
  scale_x_continuous(breaks = round(seq(min(data_means$Day), max(data_means$Day), by = 1), 1)) +
  geom_errorbar(aes(color = Treatment, x = Day, ymin = mean - se, ymax = mean + se), width = 0.2, position = position_dodge(0.5)) +
  scale_color_manual(values = c("H2-Apo-25" = "Blue",
                                "H2-Apo-32" = "Red"),
                     labels = c("H2-Apo-25", "H2-Apo-32")) +
  theme(legend.text.align = 0) +
  scale_size_manual(values = c(1.2, 1.2)) +
  labs(colour = "Treatment") +
  coord_cartesian(ylim = c(0, 15))



#Sym vs Sym
ggplot(data = data_means[data_means$Treatment %in% c("H2-Sym-25", "H2-Sym-32"), ], aes(x = Day, y = mean)) +
  theme_classic(base_size = 15) +
  geom_line(aes(color = Treatment, group = Treatment), position = position_dodge(0.5)) +
  ylab(bquote("Mean tentacle number")) +
  xlab("Days post laceration (dpl)") +
  ggtitle("Effect of Temperature on Symbiotc Pedal Lacerate Tentacle Development in Aiptasia") +
  geom_point(aes(color = Treatment), size = 2.5, shape = 20, position = position_dodge(0.5)) +
  scale_x_continuous(breaks = round(seq(min(data_means$Day), max(data_means$Day), by = 1), 1)) +
  geom_errorbar(aes(color = Treatment, x = Day, ymin = mean - se, ymax = mean + se), width = 0.2, position = position_dodge(0.5)) +
  scale_color_manual(values = c("H2-Sym-25" = "Blue",
                                "H2-Sym-32" = "Red"),
                     labels = c("H2-Sym-25", "H2-Sym-32")) +
  theme(legend.text.align = 0) +
  scale_size_manual(values = c(1.2, 1.2)) +
  labs(colour = "Treatment") +
  coord_cartesian(ylim = c(0, 15))



#Sym vs Sym
ggplot(data = data_means[data_means$Treatment %in% c("H2-Apo-25", "H2-Ino-25", "H2-Sym-25"), ], aes(x = Day, y = mean)) +
  theme_classic(base_size = 15) +
  geom_line(aes(color = Treatment, group = Treatment), position = position_dodge(0.5)) +
  ylab(bquote("Mean tentacle number")) +
  xlab("Days post laceration (dpl)") +
  ggtitle("Effect of Symbiotic State on Pedal Lacerate Tentacle Development in Aiptasia") +
  geom_point(aes(color = Treatment), size = 2.5, shape = 20, position = position_dodge(0.5)) +
  scale_x_continuous(breaks = round(seq(min(data_means$Day), max(data_means$Day), by = 1), 1)) +
  geom_errorbar(aes(color = Treatment, x = Day, ymin = mean - se, ymax = mean + se), width = 0.2, position = position_dodge(0.5)) +
  scale_color_manual(values = c("H2-Apo-25" = "Blue",
                                "H2-Ino-25" = "Green",
                                "H2-Sym-25" = "Brown"),
                     labels = c("H2-Apo-25", "H2-Ino-25", "H2-Sym-25")) +
  theme(legend.text.align = 0) +
  scale_size_manual(values = c(1.2, 1.2)) +
  labs(colour = "Treatment") +
  coord_cartesian(ylim = c(0, 15))








#ANOVA Analysis

view(long)
anova(aov(Tent_count ~ Temp*Symbiosis, data=long))

#General linear mix model 
library(car)
library(lme4)
library(emmeans)

data <- long

Apo_subset <- subset(long, Treatment %in% c("H2-Apo-25", "H2-Apo-32"))
Ino_subset <- subset(long, Treatment %in% c("H2-Ino-25", "H2-Ino-32"))
Sym_subset <- subset(long, Treatment %in% c("H2-Sym-25", "H2-Sym-32"))
Symbiois_subset <- subset(long, Treatment %in% c("H2-Sym-25", "H2-Ino-25", "H2-Apo-25"))


data <- Sym_subset

str(data)

# Distribution of the data
hist(data$Tent_count)

# Convert column day as a numeric factor
as.factor(data$Day)

# Choosing the correct mode
model <- lmer(Tent_count ~ Temp*Day_cat + (1|ID),
              data = data)

plot(model)
qqnorm(residuals(model))
qqline(residuals(model))
Anova(model)


emmeans(model, list(pairwise ~ Temp | Day_cat), adjust = "tukey")
emmeans(model, list(pairwise ~ Symbiosis | Day_cat), adjust = "tukey")
View(long)

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
  scale_y_continuous(breaks = seq(0, 10, by = 2), limits = c(0, 11)) +  # Adjusted y-axis range and breaks
  geom_point(aes(color = treatment), size = 8, shape = 20, position = position_dodge(0.5)) +
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
  theme(legend.text.align = 0, legend.position = c(0.75, 0.5), legend.justification = c("center", "center")) +
  scale_size_manual(values=c(1.2,1.2,1.2,1.2)) +
  labs(colour = "Treatment")


ggsave("Figure_Exp2_Proposal_1.tif", plot = last_plot(), device = "tiff", path = here("C:/GitHub/heatinglacerate/figs"),
       
       width = 11, height = 8, units = "in", dpi = 600)





##Graphing results of Experiment 2 for just Ambient vs Heat Stress
data_means <- newlong %>%
  group_by(temp, day) %>%
  summarise(mean = mean(tent_count, na.rm=TRUE),
            se = std.error(tent_count, na.rm=TRUE))

ggplot(data = data_means, aes(x = day, y = mean)) +
  theme_classic(base_size = 15) +
  geom_line(aes(color = temp, group = temp), position = position_dodge(0.5)) +
  ylab(bquote("Mean tentacle number"))+
  xlab("Days post laceration (dpl)") +
  #  ggtitle("Effect of Temperature on Pedal Lacerate Tentacle Development in Aiptasia") +
  scale_y_continuous(breaks = seq(0, 10, by = 2), limits = c(0, 10)) +  # Adjusted y-axis range and breaks
  geom_point(aes(color = temp), size = 10, shape = 20, position = position_dodge(0.5)) +
  scale_x_continuous(breaks = round(seq(min(data_means$day), max(data_means$day), by = 1),1)) +
  geom_errorbar(aes(color = temp, x = day, ymin = mean - se, ymax = mean + se), width = 0.2, position = position_dodge(0.5)) +
  scale_color_discrete(breaks=c("25C (ambient)","32C (heat stress)")) +
  scale_color_manual(values = c("25C (ambient)" = "blue",
                                "32C (heat stress)" = "red"),
                     labels=c("25C (ambient)",
                              expression(paste("32C (heat stress)")))) +
  theme(legend.text.align = 0, legend.position = c(0.75, 0.5), legend.justification = c("center", "center"),
        axis.text = element_text(size = 30),  # Set size of axis labels
        axis.title = element_text(size = 30),  # Set size of axis titles
        legend.text = element_text(size = 30),  # Set size of legend text
        legend.title = element_text(size = 30)) +  # Set size of legend title
  scale_size_manual(values = c(1.2, 1.2, 1.2, 1.2)) +
  labs(colour = "Treatment")


ggsave("Figure_Exp2_Proposal_2.tif", plot = last_plot(), device = "tiff", path = here("C:/GitHub/heatinglacerate/figs"),
       
       width = 11, height = 8, units = "in", dpi = 600)

##Graphing results of Experiment 2 for just SYM vs APO
data_means <- newlong %>%
  group_by(symbiosis, day) %>%
  summarise(mean = mean(tent_count, na.rm=TRUE),
            se = std.error(tent_count, na.rm=TRUE))

ggplot(data = data_means, aes(x = day, y = mean)) +
  theme_classic(base_size = 15) +
  geom_line(aes(color = symbiosis, group = symbiosis), position = position_dodge(0.5)) +
  ylab(bquote("Mean tentacle number"))+
  xlab("Days post laceration (dpl)") +
  #  ggtitle("Effect of Symbiotic State on Pedal Lacerate Tentacle Development in Aiptasia") +
  scale_y_continuous(breaks = seq(0, 10, by = 2), limits = c(0, 10)) +  # Adjusted y-axis range and breaks
  geom_point(aes(color = symbiosis), size = 10, shape = 20, position = position_dodge(0.5)) +
  scale_x_continuous(breaks = round(seq(min(data_means$day), max(data_means$day), by = 1),1)) +
  geom_errorbar(aes(color = symbiosis, x = day, ymin = mean - se, ymax = mean + se), width = 0.2, position = position_dodge(0.5)) +
  scale_color_discrete(breaks=c("Apo","Sym")) +
  scale_color_manual(values = c("Apo" = "orange",
                                "Sym" = "brown"),
                     labels=c("Aposymbiotic",
                              expression(paste("Symbiotic")))) +
  theme(legend.text.align = 0, legend.position = c(0.75, 0.5), legend.justification = c("center", "center"),
        axis.text = element_text(size = 30),  # Set size of axis labels
        axis.title = element_text(size = 30),  # Set size of axis titles
        legend.text = element_text(size = 30),  # Set size of legend text
        legend.title = element_text(size = 30)) +  # Set size of legend title
  scale_size_manual(values = c(1.2, 1.2, 1.2, 1.2)) +
  labs(colour = "Treatment")
