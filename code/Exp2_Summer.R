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
setwd("~/Documents/GitHub/heatinglacerate")

#Results of Experiment 2

##Reading data table
long <- read.csv("data/Exp 2 Lacerate Development in Heat Data Sheet - Long Data.csv")
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
  mutate(Day = as.factor(day)) %>%
  mutate(Day = dplyr::recode(Day, "0" = "00"))

#View(df)

#  select(-X) %>%
  
#  gather(key = "stream", value = "density.anomaly", -Year, na.rm = TRUE) %>%
  
#  mutate(stream = recode(stream, "MAR_MeanSD" = "MAR", "SEC_MeanSD" = "LAK", "BVA_MeanSD" = "BVA", "VCR_MeanSD" = "VAL", "SFS_MeanSD" = "SFS"))


##Saving table as output
newlong <- long
saveRDS(newlong, file = "tables/Data_Table_Summer2022Data.RDS")



# --- Mortality classification ---
# Assign mortality status based on tentacle count
df <- newlong %>%
  mutate(Mortality = ifelse(is.na(tent_count) | tent_count == 0, "Dead", "Alive"))

# Restrict dataset to 14 and 21 dpl timepoints
df_filtered <- newlong %>%
  filter(line =="H2", day_cat %in% c("day_14", "day_21")) %>%
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
  filename = "Mortality_2022_Fig.tif",
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
  filename = "Mortality_2022_Fig.pdf",
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

# Mortality table for plotting
df_filtered <- newlong %>%
  mutate(day_cat = trimws(as.character(day_cat))) %>%
  filter(day_cat %in% c("day_14", "day_21"),
         line == "H2") %>% 
  mutate(
    Mortality = ifelse(is.na(tent_count) | tent_count == 0, "Dead", "Alive"),
    day_cat = factor(day_cat, levels = c("day_14", "day_21"))
  )

nrow(df_filtered)
table(df_filtered$day_cat)

#Mortality for Experiment with Kali and Riley in 2022
ggplot(df_filtered, aes(x = treatment, fill = Mortality)) +
  geom_bar(position = "fill") +
  labs(x = "Treatment Group", y = "Percent", fill = "Mortality") +
  ggtitle("Mortality by Treatment Group (H2 line)") +
  facet_wrap(~ day_cat, ncol = 2) +
  scale_fill_manual(values = c("Dead" = "black", "Alive" = "green")) +
  scale_y_continuous(labels = percent) +
  theme_minimal()


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
  labs(colour = "Treatment")

ggsave("Figure_Exp2.tif", plot = last_plot(), device = "tiff", path = "figs",
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




model <- glmer(tent_count ~ treatment * day_cat + (1|ID), family = poisson,
              data = data)
plot(model)
qqnorm(residuals(model))
qqline(residuals(model))
Anova(model)
emmeans(model, list(pairwise ~ treatment | day_cat), adjust = "tukey")



# ============================================================
# Tentacle count analysis following Sam Bedgood-style workflow
# ============================================================
library(lme4)
library(car)
library(emmeans)
library(performance)
library(DHARMa)

data <- newlong
str(data)

model_pois <- glmer(
  tent_count ~ temp * symbiosis + day_cat + (1 | ID),
  family = poisson(link = "log"),
  data = data,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 200000)
  )
)

summary(model_pois)

# ----------------------------
# Model checks
# ----------------------------
plot(model_pois)››
check_overdispersion(model_pois)
sim_res <- simulateResiduals(model_pois)
plot(sim_res)
testDispersion(sim_res)
testZeroInflation(sim_res)

# ----------------------------
# ANOVA
# ----------------------------
Anova(model_pois, type = 3)

# ----------------------------
# Post hoc comparisons
# ----------------------------
# ANOVA
Anova(model_pois, type = 3)

# Post-hoc comparisons
emm <- emmeans(model_pois, ~ temp * symbiosis | day_cat)
pairs(emm, adjust = "tukey")

emm_temp <- emmeans(model_pois, ~ temp | symbiosis * day_cat, type = "response")
pairs(emm_temp, adjust = "tukey")

emm_sym <- emmeans(model_pois, ~ symbiosis | temp * day_cat, type = "response")
pairs(emm_sym, adjust = "tukey")








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
                     labels=c("APO-25C",
                              expression(paste("APO-32C")),
                              expression(paste("SYM-25C")),
                              expression(paste("SYM-32C")))) +
  theme(legend.text.align = 0, legend.position = c(0.75, 0.5), legend.justification = c("center", "center")) +
  scale_size_manual(values=c(1.2,1.2,1.2,1.2)) +
  labs(colour = "Treatment")


ggsave("Figure_Exp2_Proposal_1.tif", plot = last_plot(), device = "tiff", path = "figs",
       
       width = 11, height = 8, units = "in", dpi = 600)





##Graphing results of Experiment 2 for just Ambient vs Heat Stress
data_means <- newlong %>%
  group_by(temp, day) %>%
  summarise(mean = mean(tent_count, na.rm=TRUE),
            se = std.error(tent_count, na.rm=TRUE))

ggplot(data = data_means, aes(x = day, y = mean, color = temp, group = temp)) +
  theme_classic(base_size = 14) +

  # thinner connecting lines
  geom_line(linewidth = 0.9) +

  # error bars
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                width = 0.2,
                linewidth = 0.6) +

  # smaller points
  geom_point(size = 4) +

  ylab("Mean tentacle number") +
  xlab("Days post laceration (dpl)") +

  scale_y_continuous(breaks = seq(0,10,2), limits = c(0,10)) +
  scale_x_continuous(breaks = seq(min(data_means$day),
                                  max(data_means$day),1)) +

  # softer colors (closer to Jason's)
  scale_color_manual(
    values = c(
      "25C (ambient)" = "#3B6FB6",
      "32C (heat stress)" = "#E64B35"
    ),
    labels = c("25C (ambient)", "32C (heat stress)")
  ) +

  labs(colour = "Treatment") +

  theme(
    axis.text = element_text(size = 12, colour = "black"),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13),

    # legend placement similar to Jason
    legend.position = c(0.72, 0.45),
    legend.justification = c("center","center")
  )

ggsave("Figure_Exp2_Proposal_2.tif", plot = last_plot(), device = "tiff", path = "figs",
       
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





# ============================================================
# Tentacle count analysis following Sam Bedgood-style workflow
# ============================================================
library(lme4)
library(car)
library(emmeans)
library(performance)
library(DHARMa)

model_pois <- glmer(
  tent_count ~ temp * symbiosis + day_cat + (1 | ID),
  family = poisson(link = "log"),
  data = data,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 200000)
  )
)

summary(model_pois)

# ----------------------------
# Model checks
# ----------------------------
plot(model_pois)
check_overdispersion(model_pois)
sim_res <- simulateResiduals(model_pois)
plot(sim_res)
testDispersion(sim_res)
testZeroInflation(sim_res)

# ----------------------------
# ANOVA
Anova(model_pois, type = 3)

# Post-hoc comparisons
emm <- emmeans(model_pois, ~ temp * symbiosis | day_cat)
pairs(emm, adjust = "tukey")

emm_temp <- emmeans(model_pois, ~ temp | symbiosis * day_cat, type = "response")
pairs(emm_temp, adjust = "tukey")

emm_sym <- emmeans(model_pois, ~ symbiosis | temp * day_cat, type = "response")
pairs(emm_sym, adjust = "tukey")







#Results for manuscript


data_means

## Graph of temp vs heat
ggplot(data = data_means, aes(x = day, y = mean, color = temp, group = temp)) +
  theme_classic(base_size = 14) +
  
  # thinner connecting lines
  geom_line(linewidth = 0.9) +
  
  # error bars
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                width = 0.2,
                linewidth = 0.6) +
  
  # smaller points
  geom_point(size = 4) +
  
  
  # significance bars
  geom_segment(aes(x = 5, xend = 11, y = 9.2, yend = 9.2),
               inherit.aes = FALSE, linewidth = 0.8) +
  
  annotate("text", x = 8, y = 9.5, label = "***", size = 7) +
  
  geom_segment(aes(x = 12, xend = 12, y = 9.5, yend = 9.5),
               inherit.aes = FALSE, linewidth = 0.8) +
  
  annotate("text", x = 12, y = 9.5, label = "**", size = 7) +
  
  ylab("Mean tentacle number") +
  xlab("Days post laceration (dpl)") +
  
  scale_y_continuous(breaks = seq(0,10,2), limits = c(0,10)) +
  scale_x_continuous(breaks = seq(min(data_means$day),
                                  max(data_means$day),1)) +
  
  # softer colors (closer to Jason's)
  scale_color_manual(
    values = c(
      "25C (ambient)" = "#3B6FB6",
      "32C (heat stress)" = "#E64B35"
    ),
    labels = c("25C (ambient)", "32C (heat stress)")
  ) +
  
  labs(colour = "Treatment") +
  
  theme(
    axis.text = element_text(size = 14, colour = "black"),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    
    # legend placement similar to Jason
    legend.position = c(0.72, 0.45),
    legend.justification = c("center","center")
  )

ggsave(
  filename = "Man_Fig1.tif",
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

ggsave(
  filename = "Man_Fig1.pdf",
  plot = last_plot(),
  path = "figs",
  device = "pdf",
  width = 7,
  height = 5,
  units = "in",
  bg = "white"
)


model_pois2 <- glmer(
  tent_count ~ temp * day_cat + (1 | ID),
  data = data,
  family = poisson
)

emm_temp_day <- emmeans(model_pois2, ~ temp | day_cat, type = "response")
pairs(emm_temp_day, adjust = "tukey")
formula(model_pois)
