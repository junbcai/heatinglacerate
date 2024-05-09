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

#Results of Experiment - Tent Count

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
  labs(colour = "Treatment") +
  coord_cartesian(ylim = c(0, 15))

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
  ggtitle("Effect of Temperature on Symbiotic Pedal Lacerate Tentacle Development in Aiptasia") +
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



#Sym States
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

############################



##Reading data table
pedal <- read.csv("Grace/Experimental Schedule for URSA - Pedal Disc Size.csv")
View(pedal)
str(pedal)

##Converting elements in table
pedal$Pedal <- as.numeric(pedal$Pedal)
pedal$ID <- as.factor(pedal$ID)
pedal$Plate <- as.factor(pedal$Plate)
pedal$Well <- as.factor(pedal$Well)
pedal$Line <- as.factor(pedal$Line)
pedal$Temp <- as.factor(pedal$Temp)
pedal$Treatment <- as.factor(pedal$Treatment)
pedal$Symbiosis <- as.factor(pedal$Symbiosis)
pedal$Lacerate <- as.factor(pedal$Lacerate)
pedal$Day <- as.numeric(pedal$Day)
pedal$Day_cat <- as.factor(pedal$Day_cat)


data_means <- pedal %>%
  group_by(Treatment, Day) %>%
  summarise(mean = mean(Pedal, na.rm=TRUE),
            se = std.error(Pedal, na.rm=TRUE))

#Everything
ggplot(data = data_means, aes(x = Day, y = mean)) +
  theme_classic(base_size = 15) +
  geom_line(aes(color = Treatment, group = Treatment), position = position_dodge(0.5)) +
  ylab(bquote("Mean pedal disc size (um^2)")) +
  xlab("Days post laceration (dpl)") +
  ggtitle("Effect of Temperature on Pedal Disc Size in Pedal Lacerates") +
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
  labs(colour = "Treatment") +
  coord_cartesian(xlim = c(2, 14))


#Ino vs Ino

ggplot(data = data_means[data_means$Treatment %in% c("H2-Ino-25", "H2-Ino-32"), ], aes(x = Day, y = mean)) +
  theme_classic(base_size = 15) +
  geom_line(aes(color = Treatment, group = Treatment), position = position_dodge(0.5)) +
  ylab(bquote("Mean pedal disc size (um^2)")) +
  xlab("Days post laceration (dpl)") +
  ggtitle("Effect of Temperature on Pedal Disc Size in Inoculated Lacerates") +
  geom_point(aes(color = Treatment), size = 2.5, shape = 20, position = position_dodge(0.5)) +
  scale_x_continuous(breaks = round(seq(min(data_means$Day), max(data_means$Day), by = 1), 1)) +
  geom_errorbar(aes(color = Treatment, x = Day, ymin = mean - se, ymax = mean + se), width = 0.2, position = position_dodge(0.5)) +
  scale_color_manual(values = c("H2-Ino-25" = "Blue",
                                "H2-Ino-32" = "Red"),
                     labels = c("H2-Ino-25", "H2-Ino-32")) +
  theme(legend.text.align = 0) +
  scale_size_manual(values = c(1.2, 1.2)) +
  labs(colour = "Treatment")+
  coord_cartesian(xlim = c(2, 14))


#Apo vs Apo
ggplot(data = data_means[data_means$Treatment %in% c("H2-Apo-25", "H2-Apo-32"), ], aes(x = Day, y = mean)) +
  theme_classic(base_size = 15) +
  geom_line(aes(color = Treatment, group = Treatment), position = position_dodge(0.5)) +
  ylab(bquote("Mean pedal disc size (um^2)")) +
  xlab("Days post laceration (dpl)") +
  ggtitle("Effect of Temperature on Pedal Disc Size in Apo Lacerates") +
  geom_point(aes(color = Treatment), size = 2.5, shape = 20, position = position_dodge(0.5)) +
  scale_x_continuous(breaks = round(seq(min(data_means$Day), max(data_means$Day), by = 1), 1)) +
  geom_errorbar(aes(color = Treatment, x = Day, ymin = mean - se, ymax = mean + se), width = 0.2, position = position_dodge(0.5)) +
  scale_color_manual(values = c("H2-Apo-25" = "Blue",
                                "H2-Apo-32" = "Red"),
                     labels = c("H2-Apo-25", "H2-Apo-32")) +
  theme(legend.text.align = 0) +
  scale_size_manual(values = c(1.2, 1.2)) +
  labs(colour = "Treatment")+
  coord_cartesian(xlim = c(2, 14))


#Sym vs Sym
ggplot(data = data_means[data_means$Treatment %in% c("H2-Sym-25", "H2-Sym-32"), ], aes(x = Day, y = mean)) +
  theme_classic(base_size = 15) +
  geom_line(aes(color = Treatment, group = Treatment), position = position_dodge(0.5)) +
  ylab(bquote("Mean pedal disc size (um^2)")) +
  xlab("Days post laceration (dpl)") +
  ggtitle("Effect of Temperature on Pedal Disc Size in Sym Lacerates") +
  geom_point(aes(color = Treatment), size = 2.5, shape = 20, position = position_dodge(0.5)) +
  scale_x_continuous(breaks = round(seq(min(data_means$Day), max(data_means$Day), by = 1), 1)) +
  geom_errorbar(aes(color = Treatment, x = Day, ymin = mean - se, ymax = mean + se), width = 0.2, position = position_dodge(0.5)) +
  scale_color_manual(values = c("H2-Sym-25" = "Blue",
                                "H2-Sym-32" = "Red"),
                     labels = c("H2-Sym-25", "H2-Sym-32")) +
  theme(legend.text.align = 0) +
  scale_size_manual(values = c(1.2, 1.2)) +
  labs(colour = "Treatment")+
  coord_cartesian(xlim = c(2, 14))



#Sym States
ggplot(data = data_means[data_means$Treatment %in% c("H2-Sym-25", "H2-Ino-25", "H2-Apo-25"), ], aes(x = Day, y = mean)) +
  theme_classic(base_size = 15) +
  geom_line(aes(color = Treatment, group = Treatment), position = position_dodge(0.5)) +
  ylab(bquote("Mean pedal disc size (um^2)")) +
  xlab("Days post laceration (dpl)") +
  ggtitle("Effect of Symbiotic State on Pedal Disc Size ") +
  geom_point(aes(color = Treatment), size = 2.5, shape = 20, position = position_dodge(0.5)) +
  scale_x_continuous(breaks = round(seq(min(data_means$Day), max(data_means$Day), by = 1), 1)) +
  geom_errorbar(aes(color = Treatment, x = Day, ymin = mean - se, ymax = mean + se), width = 0.2, position = position_dodge(0.5)) +
  scale_color_manual(values = c("H2-Apo-25" = "Blue",
                                "H2-Ino-25" = "Green",
                                "H2-Sym-25" = "Brown"),
                     labels = c("H2-Apo-25", "H2-Ino-25", "H2-Sym-25")) +
  theme(legend.text.align = 0) +
  scale_size_manual(values = c(1.2, 1.2)) +
  labs(colour = "Treatment")+
  coord_cartesian(xlim = c(2, 14))




#ANOVA Analysis

view(pedal)
anova(aov(Pedal ~ Temp*Symbiosis, data=pedal))

#General linear mix model 
library(car)
library(lme4)
library(emmeans)

data <- pedal

Apo_subset <- subset(pedal, Treatment %in% c("H2-Apo-25", "H2-Apo-32"))
Ino_subset <- subset(pedal, Treatment %in% c("H2-Ino-25", "H2-Ino-32"))
Sym_subset <- subset(pedal, Treatment %in% c("H2-Sym-25", "H2-Sym-32"))
Symbiois_subset <- subset(pedal, Treatment %in% c("H2-Sym-25", "H2-Ino-25", "H2-Apo-25"))


data <- Sym_subset

str(data)

# Distribution of the data
hist(data$Tent_count)

# Convert column day as a numeric factor
as.factor(data$Day)

# Choosing the correct mode
model <- lmer(Pedal ~ Temp*Symbiosis*Day_cat + (1|ID),
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



