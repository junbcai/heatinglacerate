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


##Reading data table
long <- read.csv("data/Lacerate Experiment with EdU - 12_18_23 - Copy of Pedal Disk Area.csv")
#long <- read.csv("data/Lacerate Experiment with EdU - 12_18_23 - Copy of Pivot.csv") # REMOVED SOME

View(long)
str(long)

##Converting elements in table
long$Day1_PD <- as.numeric(long$Day1_PD)
long$Day8_PD <- as.numeric(long$Day8_PD)
long$Day14_PD <- as.numeric(long$Day14_PD)
long$ID <- as.factor(long$ID)
long$Treatment <- as.factor(long$Treatment)
long$Day1_ID <- as.factor(long$Day1_ID)
long$Day8_ID <- as.factor(long$Day8_ID)
long$Day14_ID <- as.factor(long$Day14_ID)


##Saving table as output
LacPD <- long
saveRDS(LacPD, file = "tables/Data_Table_Dec2023Data.RDS")


df <- long

df$Change <- df$Day14_PD - df$Day1_PD

df$Change

##Graphing results of Experiment

ggplot(df, aes(x = Treatment, y = Day14_PD, fill = Treatment)) +
  geom_boxplot(width = 0.6, position = position_dodge(width = 0.8)) +
  labs(title = "Pedal Disks at 14 Days Post Laceration ", x = "Treatment", y = "Pedal Disk") +
  scale_fill_manual(values = c("skyblue", "blue", "salmon", "red")) + 
  theme(
    panel.background = element_rect(fill = "transparent"),  # Set background to transparent
    axis.text = element_text(size = 20),  # Set size of axis labels
    axis.title = element_text(size = 20), # Set size of axis titles
    plot.title = element_text(size = 20)  # Set size of plot title
  )

ggplot(df, aes(x = Treatment, y = Day1_PD, fill = Treatment)) +
  geom_boxplot(width = 0.6, position = position_dodge(width = 0.8)) +
  labs(title = "Pedal Disks at 1 Day Post Laceration ", x = "Treatment", y = "Pedal Disk") +
  scale_fill_manual(values = c("skyblue", "blue", "salmon", "red")) + 
  theme(
    panel.background = element_rect(fill = "transparent"),  # Set background to transparent
    axis.text = element_text(size = 20),  # Set size of axis labels
    axis.title = element_text(size = 20), # Set size of axis titles
    plot.title = element_text(size = 20)  # Set size of plot title
  )

ggplot(df, aes(x = Treatment, y = Change, fill = Treatment)) +
  geom_boxplot(width = 0.6, position = position_dodge(width = 0.8)) +
  labs(title = "Changes in Pedal Disk Size at 14 Days Post Laceration ", x = "Treatment", y = "Pedal disc size (" ~ mu*m^2 ~ ")") +
  scale_fill_manual(values = c("skyblue", "blue", "salmon", "red")) + 
                      theme(
                        panel.background = element_rect(fill = "transparent"),  # Set background to transparent
                        axis.text = element_text(size = 20),  # Set size of axis labels
                        axis.title = element_text(size = 20), # Set size of axis titles
                        plot.title = element_text(size = 20),  # Set size of plot title
                        #legend.position = "bottom",  # Move legend to the bottom
                        #legend.direction = "horizontal",  # Set legend direction to horizontal
                        #legend.text.align = 0, legend.position = c(0.85, 0.85), legend.justification = c("center", "center"),
                        legend.title = element_text(size = 20),  # Set size of legend text
                        legend.text = element_text(size = 20)  # Set size of legend text
                      )


##Reading data table
newdf <- read.csv("data/Pedal Disc Size_Long.csv")
View(newdf)
str(newdf)

##Converting elements in table
newdf$PD_Size   <- as.numeric(newdf$PD_Size)
newdf$ID <- as.factor(newdf$ID)
newdf$Treatment <- as.factor(newdf$Treatment)
newdf$Day1_ID <- as.factor(newdf$Day1_ID)
newdf$day <- as.numeric(newdf$day)
newdf$day_cat <- as.factor(newdf$day_cat)


##Saving table as output
Long_PD <- newdf

df <- Long_PD

##Graphing results of Experiment 2
data_means <- Long_PD %>%
  group_by(Treatment, day) %>%
  summarise(mean = mean(PD_Size, na.rm=TRUE),
            se = std.error(PD_Size, na.rm=TRUE))

ggplot(data = data_means, aes(x = day, y = mean)) +
  theme_classic(base_size = 15) +
  geom_line(aes(color = Treatment, group = Treatment), position = position_dodge(0.5)) +
  ylab(bquote("Mean pedal disc size (" ~ mu*m^2 ~ ")")) +
  xlab("Days post laceration (dpl)") +
#  ggtitle("Effect of Temperature and Symbiotic State on Pedal Disc Size ") +
  geom_point(aes(color = Treatment), size = 8, shape = 20, position = position_dodge(0.5)) +
  geom_errorbar(aes(color = Treatment, x = day, ymin = mean - se, ymax = mean + se), width = 0.2, position = position_dodge(0.5)) +
  scale_color_discrete(breaks=c("APO_25","APO_32","SYM_25","SYM_32")) +
  scale_color_manual(values = c("APO_25" = "skyblue",
                                "APO_32" = "blue",
                                "SYM_25" = "salmon",
                                "SYM_32" = "red"),
                     labels = c("APO-25C",
                                expression(paste("APO-32C")),
                                expression(paste("SYM-25C")),
                                expression(paste("SYM-32C")))) +
  theme(legend.text.align = 0, legend.position = c(0.85, 0.85), legend.justification = c("center", "center"),
        axis.text = element_text(size = 20),  # Set size of axis labels
        axis.title = element_text(size = 20),  # Set size of axis titles
        legend.text = element_text(size = 20),  # Set size of legend text
        legend.title = element_text(size = 20)) +  # Set size of legend title
  scale_size_manual(values = c(1.2, 1.2, 1.2, 1.2)) +
  labs(colour = "Treatment") +
  scale_x_continuous(breaks = c(1, 8, 14))


df <- df %>%
  mutate(symbiosis = ifelse(grepl("APO", Treatment), "APO", "SYM"),
         temp = gsub("\\D", "", Treatment))

df$temp <- as.factor(df$temp)
df$symbiosis <- as.factor(df$symbiosis)

anova(aov(PD_Size ~ Treatment, data=df))
anova(aov(PD_Size ~ temp*symbiosis*day, data=df))

#General linear mix model 
library(car)
library(lme4)
library(emmeans)

data <- df

str(data)


# Distribution of the data
hist(data$PD_Size)

# Convert column day as a numeric factor
as.factor(data$day)

# Choosing the correct model
model <- lmer(PD_Size ~ temp*symbiosis*day_cat + (1|ID),
              data = data)

plot(model)
qqnorm(residuals(model))
qqline(residuals(model))
Anova(model)


emmeans(model, list(pairwise ~ symbiosis | day_cat), adjust = "tukey")
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


# Create a sample data frame
data <- data.frame(
  category = c("Chlorophyll A", "Hoechst", "EdU"),
  value = c(5, 8, 3)
)

# Define colors for each category
colors <- c("Chlorophyll A" = "red", "Hoechst" = "blue", "EdU" = "green")

# Create a bar plot with a custom legend
plot <- ggplot(data, aes(x = category, y = value, fill = category)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colors) +  # Specify the colors
  labs(fill = "Legend") +  # Set legend title
  theme_minimal() +
  theme(legend.text = element_text(size = 30),  # Set the size of legend text
        legend.title = element_text(size = 30))  # Set the size of legend title

print(plot)
