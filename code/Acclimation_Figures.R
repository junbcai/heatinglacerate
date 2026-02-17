install.packages("ggplot2")
library(ggplot2)
library(dplyr)
library(tidyr)

data <- data.frame(
  Time = c(0:21),
  Max_25 = rep(25, 22),
  Max_32 = c(25, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32)
)

data_long <- pivot_longer(data, starts_with("Max"), names_to = "Max", values_to = "Value")

ggplot(data_long, aes(x = Time, y = Value, color = Max)) +
  geom_line(size = 1.25) +
  labs(title = "Heat-hold with Multiple Temperatures",
       x = "Time (days)", y = "Temperature (°C)") +
  scale_color_manual(values = c("Max_25" = "blue",
                                "Max_32" = "red"),
                     labels = c("Max_25" = "Control (25 C)",
                                "Max_32" = "Heat Stress (32 C)")) +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14))  +
  coord_cartesian(ylim = c(25, 35)) 



#Acclimation
data <- data.frame(
  Time = c(0:8),
  Max_25 = c(25, 25, 25, 25, 25, 25, 34, 34, 34),
  Max_32 = c(25, 30, 30, 30, 25, 25, 34, 34, 34)
)

data_long <- pivot_longer(data, starts_with("Max"), names_to = "Max", values_to = "Value")

ggplot(data_long, aes(x = Time, y = Value, color = Max, linetype = Max)) +
  geom_line(size = 1.25, position = position_dodge(width = 0.2)) +
  labs(title = "Acclimation Regime and Heat Shock Experiment",
       x = "Time", y = "Temperature (°C)") +
  scale_color_manual(values = c("Max_25" = "blue", "Max_32" = "red"),
                     breaks = c("Max_25", "Max_32"),
                     labels = c("Control (25°C)", "Heat Stress (30°C)")) +
  scale_linetype_manual(values = c("Max_25" = "solid", "Max_32" = "dashed"),
                        breaks = c("Max_25", "Max_32")) +
  scale_y_continuous(breaks = seq(25, 36, by = 1)) +
  scale_x_continuous(breaks = data_long$Time) +  # Set x-axis breaks to all Time values
  theme_minimal() +
  theme(axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(size = 26),
        axis.text.y = element_text(size = 26),
        axis.line = element_line(color = "black", size = 1),
        panel.grid.major = element_line(color = "gray", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 26),
        legend.title = element_text(size = 24),
        plot.title = element_text(size = 40),
        plot.margin = margin(20, 20, 20, 20)) +
  coord_cartesian(ylim = c(25, 35))
