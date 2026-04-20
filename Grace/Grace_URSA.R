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
library(scales)


rm(list = ls())
graphics.off()

#Set working directory

getwd()
setwd("C:/GitHub/heatinglacerate")

#Results of Experiment - Tent Count

##Reading data table
long <- read.csv("Grace/Experimental Schedule for URSA - Grace Kelly - Grace_tent_count.csv")
str(long)

names(long) <- tolower(names(long))


##Converting elements in table
long$tent_count <- as.numeric(long$tent_count)
long$id <- as.factor(long$id)
long$plate <- as.factor(long$plate)
long$well <- as.factor(long$well)
long$line <- as.factor(long$line)
long$temp <- as.factor(long$temp)
long$treatment <- as.factor(long$treatment)
long$symbiosis <- as.factor(long$symbiosis)
long$lacerate <- as.factor(long$lacerate)
long$day <- as.numeric(long$day)
long$day_cat <- as.factor(long$day_cat)

df <- long %>%
  mutate(day = as.factor(day)) %>%
  mutate(day = dplyr::recode(day, "0" = "00"))

newlong <- long


#select(-X) %>%
  
#  gather(key = "stream", value = "density.anomaly", -Year, na.rm = TRUE) %>%
  
#  mutate(stream = recode(stream, "MAR_MeanSD" = "MAR", "SEC_MeanSD" = "LAK", "BVA_MeanSD" = "BVA", "VCR_MeanSD" = "VAL", "SFS_MeanSD" = "SFS"))


# --- Mortality classification ---
# Assign mortality status based on tentacle count
df <- newlong %>%
  mutate(Mortality = ifelse(is.na(tent_count) | tent_count == 0, "Dead", "Alive"))

# Restrict dataset to 14 and 21 dpl timepoints
df_filtered <- newlong %>%
  filter(day_cat %in% c("14_day", "21_day")) %>%
  mutate(Mortality = ifelse(is.na(tent_count) | tent_count == 0, "Dead", "Alive"))

# --- Mortality figure ---
# Plot percent alive vs dead by treatment and timepoint
ggplot(df_filtered, aes(x = treatment, fill = Mortality)) +
  geom_bar(position = "fill", color = "white", linewidth = 0.2) +
  labs(
    x = "Treatment Group",
    y = "Percent",
    fill = "Mortality"
  ) +
  facet_wrap(~ day,
             ncol = 2,
             labeller = as_labeller(c("14" = "14 dpl",
                                      "21" = "21 dpl"))) +
  scale_fill_manual(values = c("Dead" = "black", "Alive" = "green")) +
  scale_y_continuous(labels = scales::percent, expand = c(0,0)) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 12, family = "Arial"),
    axis.title.y = element_text(size = 12, family = "Arial"),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10, family = "Arial"),
    axis.text.y = element_text(size = 10, family = "Arial"),
    legend.title = element_text(size = 11, family = "Arial"),
    legend.text = element_text(size = 10, family = "Arial"),
    strip.text = element_text(size = 11, family = "Arial")
  )

# --- Export figures ---
# Save high-resolution TIFF for publication
ggsave(
  filename = "Mortality_Fig.tif",
  plot = last_plot(),
  path = "figs",
  device = "tiff",
  width = 7,
  height = 5,
  units = "in",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

# Save vector PDF for manuscript/thesis
ggsave(
  filename = "Mortality_Fig.pdf",
  plot = last_plot(),
  path = "figs",
  device = "pdf",
  width = 7,
  height = 5,
  units = "in",
  bg = "white"
)

# --- Mortality statistics ---
# Convert mortality to binary response
df_filtered$dead <- ifelse(df_filtered$Mortality == "Dead", 1, 0)

# Test effects of temperature, symbiotic state, and interaction on mortality
mortality_glm <- glm(dead ~ temp * symbiosis,
                     data = df_filtered,
                     family = binomial)

summary(mortality_glm)







##Graphing results of Experiment URSA
data_means <- newlong %>%
  group_by(treatment, day) %>%
  summarise(mean = mean(tent_count, na.rm=TRUE),
            se = std.error(tent_count, na.rm=TRUE))

#Everything

# Filter data for the treatments of interest
treatments_of_interest <- c("H2-Apo-25", "H2-Apo-32", "H2-Ino-25", "H2-Ino-32", "H2-Sym-25", "H2-Sym-32")
df_filtered <- data_means %>%
  filter(treatment %in% treatments_of_interest)

# Custom labels for the facets
facet_labels <- c(
  "H2-Apo-25 vs H2-Apo-32" = "H2-Apo-25 vs H2-Apo-32",
  "H2-Ino-25 vs H2-Ino-32" = "H2-Ino-25 vs H2-Ino-32",
  "H2-Sym-25 vs H2-Sym-32" = "H2-Sym-25 vs H2-Sym-32",
  "H2-Apo-25, H2-Ino-25, H2-Sym-25" = "H2-Apo-25, H2-Ino-25, H2-Sym-25"
)

# Create the ggplot graph with facet_wrap
ggplot(df_filtered, aes(x = day, y = mean)) +
  theme_classic(base_size = 15) +
  geom_line(aes(color = treatment, group = treatment), position = position_dodge(0.5), size = 1.5) +
  ylab(bquote("Mean tentacle number")) +
  xlab("Days post laceration (dpl)") +
  ggtitle("Effect of Temperature and Symbiotic State on Pedal Lacerate Tentacle Development in Aiptasia") +
  geom_point(aes(color = treatment), size = 6, shape = 20, position = position_dodge(0.5)) +
  scale_x_continuous(breaks = round(seq(min(df_filtered$day), max(df_filtered$day), by = 1), 1)) +
  geom_errorbar(aes(color = treatment, x = day, ymin = mean - se, ymax = mean + se), width = 1.2, size = 1, position = position_dodge(0.5)) +
  scale_color_manual(values = c(
    "H2-Apo-25" = "aquamarine",
    "H2-Apo-32" = "chocolate",
    "H2-Ino-25" = "darkorchid",
    "H2-Ino-32" = "coral1",
    "H2-Sym-25" = "blue",
    "H2-Sym-32" = "red"
  ),
  labels = c(
    "H2-Apo-25",
    expression(paste("H2-Apo-32")),
    expression(paste("H2-Ino-25")),
    expression(paste("H2-Ino-32")),
    expression(paste("H2-Sym-25")),
    expression(paste("H2-Sym-32"))
  )) +
  theme(legend.text.align = 0,
        axis.title.x = element_text(size = 24),
        axis.title.y = element_text(size = 24),
        axis.text.x = element_text(size = 12, family = "Arial"),
        axis.text.y = element_text(size = 20, family = "Arial"),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20),
        strip.text = element_text(size = 20)) +  # Change size of facet title    
  scale_size_manual(values = c(1.2, 1.2, 1.2, 1.2)) +
  labs(colour = "treatment") +
  coord_cartesian(ylim = c(0, 15)) +
  facet_wrap(~ treatment, ncol = 2)


#Everything
ggplot(data = data_means, aes(x = day, y = mean)) +
  theme_classic(base_size = 15) +
  geom_line(aes(color = treatment, group = treatment), position = position_dodge(0.5), size = 1.5) +  # Increase line thickness to 1.5
  ylab(bquote("Mean tentacle number"))+
  xlab("Days post laceration (dpl)") +
  ggtitle("Effect of Temperature on Pedal Lacerate Tentacle Development in Aiptasia") +
  geom_point(aes(color = treatment), size = 10, shape = 20, position = position_dodge(0.5)) +
  scale_x_continuous(breaks = round(seq(min(data_means$day), max(data_means$day), by = 1),1)) +
  geom_errorbar(aes(color = treatment, x = day, ymin = mean - se, ymax = mean + se), width = 3.2, size = 2, position = position_dodge(0.5)) +
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
  theme(legend.text.align = 0,
        axis.title.x = element_text(size = 24),  # Increase X-axis title size to 20
        axis.title.y = element_text(size = 24),  # Increase Y-axis title size to 22
        axis.text.x = element_text(size = 20, family = "Arial"),  # Increase X-axis text size and use Arial font
        axis.text.y = element_text(size = 20, family = "Arial"),  # Increase Y-axis text size and use Times New Roman font
        legend.text = element_text(size = 18),   # Increase legend text size to 18
        legend.title = element_text(size = 20)) +  # Increase legend title size to 20    
  scale_size_manual(values=c(1.2,1.2,1.2,1.2)) +
  labs(colour = "treatment") +
  coord_cartesian(ylim = c(0, 15))

#Ino vs Ino

ggplot(data = data_means[data_means$treatment %in% c("H2-Ino-25", "H2-Ino-32"), ], aes(x = day, y = mean)) +
  theme_classic(base_size = 15) +
  geom_line(aes(color = treatment, group = treatment), position = position_dodge(0.5), size = 1.5) +  # Increase line thickness to 1.5
  ylab(bquote("Mean tentacle number")) +
  xlab("Days post laceration (dpl)") +
  ggtitle("Effect of Temperature on Inoculated Pedal Lacerate Tentacle Development in Aiptasia") +
  geom_point(aes(color = treatment), size = 10, shape = 20, position = position_dodge(0.5)) +
  scale_x_continuous(breaks = round(seq(min(data_means$day), max(data_means$day), by = 1),1)) +
  geom_errorbar(aes(color = treatment, x = day, ymin = mean - se, ymax = mean + se), width = 1.2, size = 2, position = position_dodge(0.5)) +
  scale_color_manual(values = c("H2-Ino-25" = "Blue",
                                "H2-Ino-32" = "Red"),
                     labels = c("H2-Ino-25", "H2-Ino-32")) +
  theme(legend.text.align = 0,
        axis.title.x = element_text(size = 24),  # Increase X-axis title size to 20
        axis.title.y = element_text(size = 24),  # Increase Y-axis title size to 22
        axis.text.x = element_text(size = 20, family = "Arial"),  # Increase X-axis text size and use Arial font
        axis.text.y = element_text(size = 20, family = "Arial"),  # Increase Y-axis text size and use Times New Roman font
        legend.text = element_text(size = 18),   # Increase legend text size to 18
        legend.title = element_text(size = 20)) +  # Increase legend title size to 20    
  scale_size_manual(values=c(1.2,1.2,1.2,1.2)) +
  labs(colour = "treatment") +
  coord_cartesian(ylim = c(0, 15))


#Apo vs Apo
ggplot(data = data_means[data_means$treatment %in% c("H2-Apo-25", "H2-Apo-32"), ], aes(x = day, y = mean)) +
  theme_classic(base_size = 15) +
  geom_line(aes(color = treatment, group = treatment), position = position_dodge(0.5), size = 1.5) +  # Increase line thickness to 1.5
  ylab(bquote("Mean tentacle number")) +
  xlab("Days post laceration (dpl)") +
  ggtitle("Effect of Temperature on Aposymbiotic Pedal Lacerate Tentacle Development in Aiptasia") +
  geom_point(aes(color = treatment), size = 10, shape = 20, position = position_dodge(0.5)) +
  scale_x_continuous(breaks = round(seq(min(data_means$day), max(data_means$day), by = 1),1)) +
  geom_errorbar(aes(color = treatment, x = day, ymin = mean - se, ymax = mean + se), width = 1.2, size = 2, position = position_dodge(0.5)) +
  scale_color_manual(values = c("H2-Apo-25" = "Blue",
                                "H2-Apo-32" = "Red"),
                     labels = c("H2-Apo-25", "H2-Apo-32")) +
  theme(legend.text.align = 0,
        axis.title.x = element_text(size = 24),  # Increase X-axis title size to 20
        axis.title.y = element_text(size = 24),  # Increase Y-axis title size to 22
        axis.text.x = element_text(size = 20, family = "Arial"),  # Increase X-axis text size and use Arial font
        axis.text.y = element_text(size = 20, family = "Arial"),  # Increase Y-axis text size and use Times New Roman font
        legend.text = element_text(size = 18),   # Increase legend text size to 18
        legend.title = element_text(size = 20)) +  # Increase legend title size to 20    
  scale_size_manual(values=c(1.2,1.2,1.2,1.2)) +
  labs(colour = "treatment") +
  coord_cartesian(ylim = c(0, 15))



#Sym vs Sym
ggplot(data = data_means[data_means$treatment %in% c("H2-Sym-25", "H2-Sym-32"), ], aes(x = day, y = mean)) +
  theme_classic(base_size = 15) +
  geom_line(aes(color = treatment, group = treatment), position = position_dodge(0.5), size = 1.5) +  # Increase line thickness to 1.5
  ylab(bquote("Mean tentacle number")) +
  xlab("Days post laceration (dpl)") +
  ggtitle("Effect of Temperature on Symbiotic Pedal Lacerate Tentacle Development in Aiptasia") +
  geom_point(aes(color = treatment), size = 10, shape = 20, position = position_dodge(0.5)) +
  scale_x_continuous(breaks = round(seq(min(data_means$day), max(data_means$day), by = 1),1)) +
  geom_errorbar(aes(color = treatment, x = day, ymin = mean - se, ymax = mean + se), width = 1.2, size = 2, position = position_dodge(0.5)) +
  scale_color_manual(values = c("H2-Sym-25" = "Blue",
                                "H2-Sym-32" = "Red"),
                     labels = c("H2-Sym-25", "H2-Sym-32")) +
  theme(legend.text.align = 0,
        axis.title.x = element_text(size = 24),  # Increase X-axis title size to 20
        axis.title.y = element_text(size = 24),  # Increase Y-axis title size to 22
        axis.text.x = element_text(size = 20, family = "Arial"),  # Increase X-axis text size and use Arial font
        axis.text.y = element_text(size = 20, family = "Arial"),  # Increase Y-axis text size and use Times New Roman font
        legend.text = element_text(size = 18),   # Increase legend text size to 18
        legend.title = element_text(size = 20)) +  # Increase legend title size to 20    
  scale_size_manual(values=c(1.2,1.2,1.2,1.2)) +
  labs(colour = "treatment") +
  coord_cartesian(ylim = c(0, 15))



#Sym States
p_sym_state  <- ggplot(data = data_means[data_means$treatment %in% c("H2-Apo-25", "H2-Ino-25", "H2-Sym-25"), ], aes(x = day, y = mean)) +
  theme_classic(base_size = 15) +
  geom_line(aes(color = treatment, group = treatment), position = position_dodge(0.5), size = 1.5) +  # Increase line thickness to 1.5
  ylab(bquote("Mean tentacle number")) +
  xlab("Days post laceration (dpl)") +
  ggtitle("Effect of Symbiotic State on Pedal Lacerate Tentacle Development in Aiptasia") +
  geom_point(aes(color = treatment), size = 10, shape = 20, position = position_dodge(0.5)) +
  scale_x_continuous(breaks = round(seq(min(data_means$day), max(data_means$day), by = 1),1)) +
  geom_errorbar(aes(color = treatment, x = day, ymin = mean - se, ymax = mean + se), width = 1.2, size = 2, position = position_dodge(0.5)) +
  scale_color_manual(values = c("H2-Apo-25" = "Blue",
                                "H2-Ino-25" = "Green",
                                "H2-Sym-25" = "Brown"),
                     labels = c("Apo-25", "Inoc-25", "Sym-25")) +
  theme(legend.text.align = 0,
        axis.title.x = element_text(size = 24),  # Increase X-axis title size to 20
        axis.title.y = element_text(size = 24),  # Increase Y-axis title size to 22
        axis.text.x = element_text(size = 20, family = "Arial"),  # Increase X-axis text size and use Arial font
        axis.text.y = element_text(size = 20, family = "Arial"),  # Increase Y-axis text size and use Times New Roman font
        legend.text = element_text(size = 18),   # Increase legend text size to 18
        legend.title = element_text(size = 20)) +  # Increase legend title size to 20    
  scale_size_manual(values=c(1.2,1.2,1.2,1.2)) +
  labs(colour = "treatment") +
  coord_cartesian(ylim = c(0, 15))

p_sym_state

ggsave("Grace_ApoInocSym_Figure.pdf", plot = p_sym_state, device = "pdf", path = here("figs"),  width = 11,  height = 8, units = "in")
ggsave("Grace_ApoInocSym_Figure.tiff", plot = p_sym_state, device = "tiff", path = here("figs"),  width = 11,  height = 8, units = "in", dpi = 600, compression = "lzw")




#ANOVA Analysis

view(long)
anova(aov(tent_count ~ temp*symbiosis, data=long))

#General linear mix model 
library(car)
library(lme4)
library(emmeans)

data <- long

Apo_subset <- subset(long, treatment %in% c("H2-Apo-25", "H2-Apo-32"))
Ino_subset <- subset(long, treatment %in% c("H2-Ino-25", "H2-Ino-32"))
Sym_subset <- subset(long, treatment %in% c("H2-Sym-25", "H2-Sym-32"))
Symbiois_subset <- subset(long, treatment %in% c("H2-Sym-25", "H2-Ino-25", "H2-Apo-25"))


data <- Sym_subset

str(data)

# Distribution of the data
hist(data$tent_count)

# Convert column day as a numeric factor
as.factor(data$day)

# Choosing the correct mode
model <- lmer(tent_count ~ temp*day_cat + (1|id),
              data = data)

plot(model)
qqnorm(residuals(model))
qqline(residuals(model))
Anova(model)


emmeans(model, list(pairwise ~ temp | day_cat), adjust = "tukey")
emmeans(model, list(pairwise ~ symbiosis | day_cat), adjust = "tukey")

############################



##Reading data table
pedal <- read.csv("Grace/Experimental Schedule for URSA - Pedal Disc Size.csv")
View(pedal)
str(pedal)

##Converting elements in table
pedal$Pedal <- as.numeric(pedal$Pedal)
pedal$id <- as.factor(pedal$id)
pedal$plate <- as.factor(pedal$plate)
pedal$well <- as.factor(pedal$well)
pedal$line <- as.factor(pedal$line)
pedal$temp <- as.factor(pedal$temp)
pedal$treatment <- as.factor(pedal$treatment)
pedal$symbiosis <- as.factor(pedal$symbiosis)
pedal$lacerate <- as.factor(pedal$lacerate)
pedal$day <- as.numeric(pedal$day)
pedal$day_cat <- as.factor(pedal$day_cat)


data_means <- pedal %>%
  group_by(treatment, day) %>%
  summarise(mean = mean(Pedal, na.rm=TRUE),
            se = std.error(Pedal, na.rm=TRUE))


# Filter data for the treatments of interest
treatments_of_interest <- c("H2-Apo-25", "H2-Apo-32", "H2-Ino-25", "H2-Ino-32", "H2-Sym-25", "H2-Sym-32")
df_filtered <- data_means %>%
  filter(treatment %in% treatments_of_interest)

# Custom labels for the facets
facet_labels <- c(
  "H2-Apo-25 vs H2-Apo-32" = "H2-Apo-25 vs H2-Apo-32",
  "H2-Ino-25 vs H2-Ino-32" = "H2-Ino-25 vs H2-Ino-32",
  "H2-Sym-25 vs H2-Sym-32" = "H2-Sym-25 vs H2-Sym-32",
  "H2-Apo-25, H2-Ino-25, H2-Sym-25" = "H2-Apo-25, H2-Ino-25, H2-Sym-25"
)

# Create the ggplot graph with facet_wrap
ggplot(df_filtered, aes(x = day, y = mean)) +
  theme_classic(base_size = 15) +
  geom_line(aes(color = treatment, group = treatment), position = position_dodge(0.5), size = 1.5) +
  ylab(bquote("Mean pedal disc size (um^2)")) +
  xlab("Days post laceration (dpl)") +
  ggtitle("Effect of Temperature and Symbiotic State on Pedal Disc Size in Pedal Lacerates") +
  geom_point(aes(color = treatment), size = 6, shape = 20, position = position_dodge(0.5)) +
  scale_x_continuous(breaks = round(seq(min(df_filtered$day), max(df_filtered$day), by = 1), 1)) +
  geom_errorbar(aes(color = treatment, x = day, ymin = mean - se, ymax = mean + se), width = 1.2, size = 1, position = position_dodge(0.5)) +
  scale_color_manual(values = c(
    "H2-Apo-25" = "aquamarine",
    "H2-Apo-32" = "chocolate",
    "H2-Ino-25" = "darkorchid",
    "H2-Ino-32" = "coral1",
    "H2-Sym-25" = "blue",
    "H2-Sym-32" = "red"
  ),
  labels = c(
    "H2-Apo-25",
    expression(paste("H2-Apo-32")),
    expression(paste("H2-Ino-25")),
    expression(paste("H2-Ino-32")),
    expression(paste("H2-Sym-25")),
    expression(paste("H2-Sym-32"))
  )) +
  theme(legend.text.align = 0,
        axis.title.x = element_text(size = 24),
        axis.title.y = element_text(size = 24),
        axis.text.x = element_text(size = 12, family = "Arial"),
        axis.text.y = element_text(size = 20, family = "Arial"),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20)) +
  scale_size_manual(values = c(1.2, 1.2, 1.2, 1.2)) +
  labs(colour = "treatment") +
  coord_cartesian(ylim = c(200, 800)) +
  facet_wrap(~ treatment, ncol = 2)




#Everything
ggplot(data = data_means, aes(x = day, y = mean)) +
  theme_classic(base_size = 15) +
  geom_line(aes(color = treatment, group = treatment), position = position_dodge(0.5)) +
  ylab(bquote("Mean pedal disc size (um^2)")) +
  xlab("Days post laceration (dpl)") +
  ggtitle("Effect of Temperature on Pedal Disc Size in Pedal Lacerates") +
  geom_point(aes(color = treatment), size = 2.5, shape = 20, position = position_dodge(0.5)) +
  scale_x_continuous(breaks = round(seq(min(data_means$day), max(data_means$day), by = 1),1)) +
  geom_errorbar(aes(color = treatment, x = day, ymin = mean - se, ymax = mean + se), width = 0.2, position = position_dodge(0.5)) +
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
  labs(colour = "treatment") +
  coord_cartesian(xlim = c(2, 14))


#Ino vs Ino

ggplot(data = data_means[data_means$treatment %in% c("H2-Ino-25", "H2-Ino-32"), ], aes(x = day, y = mean)) +
  theme_classic(base_size = 15) +
  geom_line(aes(color = treatment, group = treatment), position = position_dodge(0.5)) +
  ylab(bquote("Mean pedal disc size (um^2)")) +
  xlab("Days post laceration (dpl)") +
  ggtitle("Effect of Temperature on Pedal Disc Size in Inoculated Lacerates") +
  geom_point(aes(color = treatment), size = 2.5, shape = 20, position = position_dodge(0.5)) +
  scale_x_continuous(breaks = round(seq(min(data_means$day), max(data_means$day), by = 1), 1)) +
  geom_errorbar(aes(color = treatment, x = day, ymin = mean - se, ymax = mean + se), width = 0.2, position = position_dodge(0.5)) +
  scale_color_manual(values = c("H2-Ino-25" = "Blue",
                                "H2-Ino-32" = "Red"),
                     labels = c("H2-Ino-25", "H2-Ino-32")) +
  theme(legend.text.align = 0) +
  scale_size_manual(values = c(1.2, 1.2)) +
  labs(colour = "treatment")+
  coord_cartesian(xlim = c(2, 14))


#Apo vs Apo
ggplot(data = data_means[data_means$treatment %in% c("H2-Apo-25", "H2-Apo-32"), ], aes(x = day, y = mean)) +
  theme_classic(base_size = 15) +
  geom_line(aes(color = treatment, group = treatment), position = position_dodge(0.5)) +
  ylab(bquote("Mean pedal disc size (um^2)")) +
  xlab("Days post laceration (dpl)") +
  ggtitle("Effect of Temperature on Pedal Disc Size in Apo Lacerates") +
  geom_point(aes(color = treatment), size = 2.5, shape = 20, position = position_dodge(0.5)) +
  scale_x_continuous(breaks = round(seq(min(data_means$day), max(data_means$day), by = 1), 1)) +
  geom_errorbar(aes(color = treatment, x = day, ymin = mean - se, ymax = mean + se), width = 0.2, position = position_dodge(0.5)) +
  scale_color_manual(values = c("H2-Apo-25" = "Blue",
                                "H2-Apo-32" = "Red"),
                     labels = c("H2-Apo-25", "H2-Apo-32")) +
  theme(legend.text.align = 0) +
  scale_size_manual(values = c(1.2, 1.2)) +
  labs(colour = "treatment")+
  coord_cartesian(xlim = c(2, 14))


#Sym vs Sym
ggplot(data = data_means[data_means$Treatment %in% c("H2-Sym-25", "H2-Sym-32"), ], aes(x = day, y = mean)) +
  theme_classic(base_size = 15) +
  geom_line(aes(color = Treatment, group = Treatment), position = position_dodge(0.5)) +
  ylab(bquote("Mean pedal disc size (um^2)")) +
  xlab("Days post laceration (dpl)") +
  ggtitle("Effect of Temperature on Pedal Disc Size in Sym Lacerates") +
  geom_point(aes(color = Treatment), size = 2.5, shape = 20, position = position_dodge(0.5)) +
  scale_x_continuous(breaks = round(seq(min(data_means$day), max(data_means$day), by = 1), 1)) +
  geom_errorbar(aes(color = Treatment, x = day, ymin = mean - se, ymax = mean + se), width = 0.2, position = position_dodge(0.5)) +
  scale_color_manual(values = c("H2-Sym-25" = "Blue",
                                "H2-Sym-32" = "Red"),
                     labels = c("H2-Sym-25", "H2-Sym-32")) +
  theme(legend.text.align = 0) +
  scale_size_manual(values = c(1.2, 1.2)) +
  labs(colour = "Treatment")+
  coord_cartesian(xlim = c(2, 14))



#Sym States
ggplot(data = data_means[data_means$treatment %in% c("H2-Sym-25", "H2-Ino-25", "H2-Apo-25"), ], aes(x = day, y = mean)) +
  theme_classic(base_size = 15) +
  geom_line(aes(color = treatment, group = treatment), position = position_dodge(0.5)) +
  ylab(bquote("Mean pedal disc size (um^2)")) +
  xlab("Days post laceration (dpl)") +
  ggtitle("Effect of Symbiotic State on Pedal Disc Size ") +
  geom_point(aes(color = treatment), size = 2.5, shape = 20, position = position_dodge(0.5)) +
  scale_x_continuous(breaks = round(seq(min(data_means$day), max(data_means$day), by = 1), 1)) +
  geom_errorbar(aes(color = treatment, x = day, ymin = mean - se, ymax = mean + se), width = 0.2, position = position_dodge(0.5)) +
  scale_color_manual(values = c("H2-Apo-25" = "Blue",
                                "H2-Ino-25" = "Green",
                                "H2-Sym-25" = "Brown"),
                     labels = c("H2-Apo-25", "H2-Ino-25", "H2-Sym-25")) +
  theme(legend.text.align = 0) +
  scale_size_manual(values = c(1.2, 1.2)) +
  labs(colour = "treatment")+
  coord_cartesian(xlim = c(2, 14))




#ANOVA Analysis

view(pedal)
anova(aov(Pedal ~ temp*symbiosis, data=pedal))

#General linear mix model 
library(car)
library(lme4)
library(emmeans)

data <- pedal

Apo_subset <- subset(pedal, treatment %in% c("H2-Apo-25", "H2-Apo-32"))
Ino_subset <- subset(pedal, treatment %in% c("H2-Ino-25", "H2-Ino-32"))
Sym_subset <- subset(pedal, treatment %in% c("H2-Sym-25", "H2-Sym-32"))
Symbiois_subset <- subset(pedal, treatment %in% c("H2-Sym-25", "H2-Ino-25", "H2-Apo-25"))


data <- Sym_subset

str(data)

# Distribution of the data
hist(data$tent_count)

# Convert column day as a numeric factor
as.factor(data$day)

# Choosing the correct mode
model <- lmer(Pedal ~ temp*symbiosis*day_cat + (1|ID),
              data = data)

plot(model)
qqnorm(residuals(model))
qqline(residuals(model))
Anova(model)


emmeans(model, list(pairwise ~ Temp | day_cat), adjust = "tukey")
emmeans(model, list(pairwise ~ symbiosis | day_cat), adjust = "tukey")


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





# Generalized linear mixed model
library(car)
library(lme4)
library(emmeans)

# Keep only the three 25C symbiotic states
data <- newlong

data <- data %>%
  filter(treatment %in% c("H2-Apo-25","H2-Ino-25","H2-Sym-25"))

data$treatment <- factor(data$treatment)
data$day <- factor(data$day)
data$id <- factor(data$id)

# Explore distribution
hist(data$tent_count)

# Fit generalized linear mixed model (Poisson)
model_sym_state <- glmer(
  tent_count ~ treatment * day + (1|id),
  family = poisson(link = "log"), #remove log if you want 5dpl to be significant
  data = data,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 200000)
  )
)

# Residual diagnostics
plot(model_sym_state)
qqnorm(residuals(model_sym_state))
qqline(residuals(model_sym_state))

# ANOVA (Type II Wald Chi-square tests)
Anova(model_sym_state)

# Post hoc pairwise comparisons
emm <- emmeans(model_sym_state, ~ treatment | day)
pairs(emm, adjust = "tukey")











# Results for manuscript
# Symbiotic state comparison at 25C

p_sym_state <- ggplot(
  data = data_means[data_means$treatment %in% c("H2-Apo-25","H2-Ino-25","H2-Sym-25"),],
  aes(x = day, y = mean, color = treatment, group = treatment)
) +
  theme_classic(base_size = 14) +
  
  # lines
  geom_line(linewidth = 0.9) +
  
  # error bars
  geom_errorbar(
    aes(ymin = mean - se, ymax = mean + se),
    width = 0.2,
    linewidth = 0.6
  ) +
  
  # points
  geom_point(size = 4) +
  
  # significance stars
  annotate("text", x = 7,  y = 10.2, label = "***", size = 7) +
  annotate("text", x = 10, y = 12.0, label = "***", size = 7) +
  annotate("text", x = 11, y = 12.0, label = "**",  size = 7) +
  
  ylab("Mean tentacle number") +
  xlab("Days post laceration (dpl)") +
  
  scale_y_continuous(
    breaks = seq(0,15,2),
    limits = c(0,15)
  ) +
  
  scale_x_continuous(
    breaks = seq(min(data_means$day),
                 max(data_means$day),1)
  ) +
  
  scale_color_manual(
    values = c(
      "H2-Apo-25" = "#3B6FB6",
      "H2-Ino-25" = "#4DAF4A",
      "H2-Sym-25" = "#8C564B"
    ),
    labels = c("Apo-25","Inoc-25","Sym-25")
  ) +
  
  labs(colour = "Treatment") +
  
  theme(
    axis.text = element_text(size = 14, colour = "black", family = "Arial"),
    axis.title = element_text(size = 14, family = "Arial"),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.position = c(0.75,0.45),
    legend.justification = c("center","center")
  )
p_sym_state

ggsave(
  "figs/Grace_ApoInocSym_Figure.tiff",
  plot = p_sym_state,
  width = 11,
  height = 8,
  units = "in",
  dpi = 600,
  compression = "lzw"
)


ggsave(
  "figs/Grace_ApoInocSym_Figure.pdf",
  plot = p_sym_state,
  device = cairo_pdf,
  width = 11,
  height = 8,
  units = "in"
)
