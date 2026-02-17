# ============================================================
# EdU + Caspase: replicate-mean boxplots (6 groups each) + stats
# - Averages technical replicates within each biological replicate
# - Optional Tukey (1.5*IQR) outlier removal within each Treatment x Time x Marker group
# - Two-way ANOVA (Treatment * Time) for each marker
# - Welch t-tests (Control vs HS) at each timepoint for each marker
# ============================================================

library(tidyverse)
library(stringr)
library(rstatix)
library(patchwork)

rm(list = ls())
graphics.off()

#Set working directory

getwd()
setwd("~/Documents/GitHub/heatinglacerate")

#Results of Experiment 1

# ---- Load ----
list.files("data")
dat <- read_csv("data/Lacerate_EdU_Caspase.csv")


# ---- Keep only EdU and Caspase and set up columns ----
x <- dat %>%
  filter(True_Channel %in% c("EdU", "Caspase")) %>%
  mutate(
    marker = factor(True_Channel, levels = c("EdU", "Caspase")),
    time   = factor(Time, levels = c("2dpl", "5dpl", "8dpl")),
    treatment = factor(Treatment),  # Sym_Control / Sym_HS
    treatment_short = case_when(
      str_detect(as.character(treatment), "Control") ~ "Control",
      str_detect(as.character(treatment), "HS") ~ "HS",
      TRUE ~ as.character(treatment)
    ),
    biorep = str_extract(Sample, "sample\\d+") %>% str_remove("sample") %>% as.integer()
  ) %>%
  drop_na(time, biorep)

# ---- Average technical replicates within each biological replicate ----
rep_means <- x %>%
  group_by(marker, treatment, treatment_short, time, biorep) %>%
  summarize(percentage = mean(Percentage, na.rm = TRUE), .groups = "drop")

# ---- OPTIONAL: remove outliers (Tukey 1.5*IQR) within each marker x treatment x time ----
remove_outliers_tukey <- function(v) {
  q1 <- quantile(v, 0.25, na.rm = TRUE)
  q3 <- quantile(v, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lo <- q1 - 1.5 * iqr
  hi <- q3 + 1.5 * iqr
  v >= lo & v <= hi
}

rep_means_clean <- rep_means %>%
  group_by(marker, treatment_short, time) %>%
  filter(remove_outliers_tukey(percentage)) %>%
  ungroup()

# ---- 6-level group label for plotting (matches your screenshot layout) ----
plot_df <- rep_means_clean %>%
  mutate(
    group = factor(
      paste0(time, "\n", treatment_short),
      levels = c(
        "2dpl\nControl","2dpl\nHS",
        "5dpl\nControl","5dpl\nHS",
        "8dpl\nControl","8dpl\nHS"
      )
    )
  )

# ============================================================
# FIGURES (boxplot + jitter)
# ============================================================

make_box <- function(df_marker, ylab) {
  ggplot(df_marker, aes(x = group, y = percentage)) +
    geom_boxplot(
      outlier.shape = NA,
      width = 0.6,
      fill = "#d9dfb5",
      color = "grey50",
      linewidth = 0.8
    ) +
    geom_jitter(width = 0.08, size = 2.2, alpha = 0.9, color = "black") +
    labs(x = "Time and Treatment", y = ylab) +
    theme_classic(base_size = 14) +
    theme(
      axis.line = element_line(color = "black", linewidth = 0.9),
      axis.ticks = element_line(color = "black"),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14),
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16),
      plot.margin = margin(8, 8, 8, 8)
    )
}

p_edu <- make_box(filter(plot_df, marker == "EdU"), "EdU-positive area (%)")
p_cas <- make_box(filter(plot_df, marker == "Caspase"), "Caspase-positive area (%)")

print(p_edu)
print(p_cas)

# or as a 2-panel figure
p_pair <- p_edu + p_cas + plot_layout(ncol = 2)
print(p_pair)

# Save
ggsave("EdU_boxplot.tiff", p_edu, width = 7, height = 4.5, dpi = 300, compression = "lzw")
ggsave("Caspase_boxplot.tiff", p_cas, width = 7, height = 4.5, dpi = 300, compression = "lzw")
ggsave("EdU_Caspase_boxplots.pdf", p_pair, width = 12, height = 4.5)

# ============================================================
# STATS
# For each marker:
#   - Two-way ANOVA: percentage ~ treatment * time
#   - Welch t-tests: treatment comparison at each timepoint
#   - Cohen's d per timepoint
# ============================================================

# ---- Two-way ANOVA per marker ----
anova_by_marker <- plot_df %>%
  group_by(marker) %>%
  anova_test(percentage ~ treatment * time) %>%
  ungroup()

anova_by_marker

# ---- Per-timepoint Welch t-tests per marker ----
ttest_by_marker <- plot_df %>%
  group_by(marker, time) %>%
  t_test(percentage ~ treatment, var.equal = FALSE) %>%
  add_significance("p") %>%
  ungroup()

ttest_by_marker

# Optional: FDR correction within each marker (3 timepoints)
ttest_by_marker_fdr <- ttest_by_marker %>%
  group_by(marker) %>%
  mutate(p.adj = p.adjust(p, method = "fdr")) %>%
  ungroup()

ttest_by_marker_fdr

# ---- Cohen's d (HS vs Control) per timepoint per marker ----
d_by_marker <- plot_df %>%
  group_by(marker, time) %>%
  cohens_d(percentage ~ treatment, var.equal = FALSE) %>%
  ungroup()

d_by_marker
