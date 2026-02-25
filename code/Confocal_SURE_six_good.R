# ============================================================
# Build 6 individual panels + assemble into a 2x3 figure
# Panels: (EdU, Caspase) x (Apo, Inoc, Sym)
# ============================================================

library(dplyr)
library(tidyverse)
library(stringr)
library(rstatix)
library(patchwork)

rm(list = ls())
graphics.off()

#Set working directory

getwd()
setwd("~/Documents/GitHub/heatinglacerate")

# ---- Load ----
list.files("data")
dat <- read_csv("data/Lacerate_EdU_Caspase.csv")

# ---- Prep ----
x <- dat %>%
  dplyr::filter(True_Channel %in% c("EdU", "Caspase")) %>%
  dplyr::mutate(
    marker = factor(True_Channel, levels = c("EdU", "Caspase")),
    time   = factor(Time, levels = c("2dpl", "5dpl", "8dpl", "14dpl")),
    treatment = as.character(Treatment),

    state = dplyr::case_when(
      stringr::str_detect(treatment, stringr::regex("Apo",  ignore_case = TRUE))  ~ "Apo",
      stringr::str_detect(treatment, stringr::regex("Inoc", ignore_case = TRUE))  ~ "Inoc",
      stringr::str_detect(treatment, stringr::regex("Sym",  ignore_case = TRUE))  ~ "Sym",
      TRUE ~ NA_character_
    ),
    heat = dplyr::case_when(
      stringr::str_detect(treatment, stringr::regex("HS|Heat", ignore_case = TRUE)) ~ "Heat",
      stringr::str_detect(treatment, stringr::regex("Control", ignore_case = TRUE)) ~ "Control",
      TRUE ~ NA_character_
    ),

    state = factor(state, levels = c("Apo", "Inoc", "Sym")),
    heat  = factor(heat,  levels = c("Control", "Heat"))
  ) %>%
  tidyr::drop_na(time, state, heat) %>%
  dplyr::arrange(marker, state, heat, time) %>%          # makes numbering reproducible
  dplyr::mutate(biorep = dplyr::row_number())            # unique bio-rep id per row

# ---- "Replicate means" (no averaging; just rename) ----
rep_means <- x %>%
  dplyr::transmute(
    marker, state, heat, time,
    percentage = Percentage
  ) %>%
  tidyr::drop_na(percentage)

# ---- OPTIONAL outlier removal (Tukey 1.5*IQR) within marker x state x heat x time ----
remove_outliers_tukey <- function(v) {
  q1 <- stats::quantile(v, 0.25, na.rm = TRUE)
  q3 <- stats::quantile(v, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lo <- q1 - 1.5 * iqr
  hi <- q3 + 1.5 * iqr
  v >= lo & v <= hi
}

df <- rep_means %>%
  dplyr::group_by(marker, state, heat, time) %>%
  dplyr::filter(remove_outliers_tukey(percentage)) %>%
  dplyr::ungroup()

# ============================================================
# Panel function (single marker + single state)
# ============================================================

panel_plot <- function(df_sub, title_text = NULL, ylab = "Positive area (%)", show_legend = FALSE) {
  ggplot2::ggplot(df_sub, ggplot2::aes(x = time, y = percentage, fill = heat)) +
    ggplot2::geom_boxplot(outlier.shape = NA, width = 0.6, color = "grey50",
                          linewidth = 0.8, position = ggplot2::position_dodge(width = 0.75)) +
    ggplot2::geom_jitter(position = ggplot2::position_jitterdodge(jitter.width = 0.14, dodge.width = 0.75),
                         size = 2.2, alpha = 0.9, color = "black") +
    ggplot2::scale_fill_manual(values = c("Control" = "#00BFC4", "Heat" = "#F8766D")) +
    ggplot2::labs(title = title_text, x = NULL, y = ylab, fill = NULL) +
    ggplot2::coord_cartesian(ylim = c(0, 35)) +
    ggplot2::theme_classic(base_size = 14) +
    ggplot2::theme(
      axis.line = ggplot2::element_line(color = "black", linewidth = 0.9),
      axis.ticks = ggplot2::element_line(color = "black"),
      plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5),
      legend.position = ifelse(show_legend, "top", "none")
    )
}

# ============================================================
# Build each of the 6 panels as separate objects
# ============================================================

p_edu_apo  <- panel_plot(filter(df, marker == "EdU",     state == "Apo"),
                         title_text = "EdU – Apo", ylab = "EdU-positive area (%)")

p_edu_inoc <- panel_plot(filter(df, marker == "EdU",     state == "Inoc"),
                         title_text = "EdU – Inoc", ylab = NULL)

p_edu_sym  <- panel_plot(filter(df, marker == "EdU",     state == "Sym"),
                         title_text = "EdU – Sym", ylab = NULL, show_legend = TRUE)

p_cas_apo  <- panel_plot(filter(df, marker == "Caspase", state == "Apo"),
                         title_text = "Caspase – Apo", ylab = "Caspase-positive area (%)")

p_cas_inoc <- panel_plot(filter(df, marker == "Caspase", state == "Inoc"),
                         title_text = "Caspase – Inoc", ylab = NULL)

p_cas_sym  <- panel_plot(filter(df, marker == "Caspase", state == "Sym"),
                         title_text = "Caspase – Sym", ylab = NULL)

# Make ONLY ONE panel have a legend (farthest right)
p_edu_apo  <- p_edu_apo  + theme(legend.position = "none")
p_edu_inoc <- p_edu_inoc + theme(legend.position = "none")
p_edu_sym  <- p_edu_sym  + theme(legend.position = "right")
p_cas_apo  <- p_cas_apo  + theme(legend.position = "none")
p_cas_inoc <- p_cas_inoc + theme(legend.position = "none")
p_cas_sym  <- p_cas_sym  + theme(legend.position = "right")

# ============================================================
# 3-panel EdU figure (Apo | Inoc | Sym)
# ============================================================
p_edu3 <- p_edu_apo | p_edu_inoc | p_edu_sym 

p_edu3 <- (p_edu_apo | p_edu_inoc | p_edu_sym) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "top",
    legend.justification = "right"
  )
print(p_edu3)

ggsave("EdU_3panel_ApoInocSym.pdf", plot = p_edu3, device = "pdf", path = here("figs"),  width = 12,  height = 3.8, units = "in")
ggsave("EdU_3panel_ApoInocSym.tiff", plot = p_edu3, device = "tiff", path = here("figs"),  width = 12,  height = 3.8, units = "in", dpi = 600, compression = "lzw")

# ============================================================
# 3-panel Caspase figure (Apo | Inoc | Sym)
# ============================================================
p_cas3 <- p_cas_apo | p_cas_inoc | p_cas_sym

p_cas3 <- (p_cas_apo | p_cas_inoc | p_cas_sym) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "top",
    legend.justification = "right"
  )
print(p_cas3)

ggsave("Caspase_3panel_ApoInocSym.pdf", plot = p_cas3, device = "pdf", path = here("figs"),  width = 12,  height = 3.8, units = "in")
ggsave("Caspase_3panel_ApoInocSym.tiff", plot = p_cas3, device = "tiff", path = here("figs"),  width = 12,  height = 3.8, units = "in", dpi = 600, compression = "lzw")

# ============================================================
# 6-panel combined figure (EdU row / Caspase row)
# ============================================================
p6 <- (p_edu_apo | p_edu_inoc | p_edu_sym) /
  (p_cas_apo | p_cas_inoc | p_cas_sym)


p6 <- (p_edu_apo | p_edu_inoc | p_edu_sym) /
  (p_cas_apo | p_cas_inoc | p_cas_sym) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "top",
    legend.justification = "right"
  )
print(p6)

ggsave("EdU_Caspase_6panel.pdf", plot = p6, device = "pdf", path = here("figs"),  width = 11,  height = 8, units = "in")
ggsave("EdU_Caspase_6panel.tiff", plot = p6, device = "tiff", path = here("figs"),  width = 11,  height = 8, units = "in", dpi = 600, compression = "lzw")


library(dplyr)
library(rstatix)



# ============================================================
# 1) Add significance stars onto your figure
# 2) Make a stats summary table for paper
# 3) Test time trends (within each state)
# 4) Two-way ANOVA interpretation (heat * time, within each state)
# 5) Effect sizes (Cohen's d) + power-ish flags (low n)
# Assumes you already have:
#   df   = cleaned replicate-mean data with columns: marker, state, heat, time, percentage
#   p6   = your 6-panel patchwork plot (optional, for annotation)
# ============================================================

library(tidyverse)
library(rstatix)
library(patchwork)

# Make sure factors are set (adjust levels if needed)
df <- df %>%
  mutate(
    marker = factor(marker, levels = c("EdU", "Caspase")),
    state  = factor(state,  levels = c("Apo", "Inoc", "Sym")),
    heat   = factor(heat,   levels = c("Control", "Heat")),
    time   = factor(time,   levels = c("2dpl", "5dpl", "8dpl", "14dpl"))
  )

# Helper: quick Ns per group (useful everywhere)
n_by_group <- df %>%
  group_by(marker, state, time, heat) %>%
  summarise(n = n(), .groups = "drop")

# ============================================================
# (5) EFFECT SIZES (Cohen's d, Heat vs Control at each timepoint)
# ============================================================
d_by_time <- df %>%
  group_by(marker, state, time) %>%
  cohens_d(percentage ~ heat, var.equal = FALSE) %>%
  ungroup()

# ============================================================
# (2) PER-TIMEPOINT TESTS (Welch t-tests) + FDR correction
# ============================================================
ttest_results <- df %>%
  group_by(marker, state, time) %>%
  t_test(percentage ~ heat, var.equal = FALSE) %>%
  ungroup() %>%
  left_join(n_by_group %>% filter(heat == "Control") %>% rename(n_control = n) %>% select(-heat),
            by = c("marker","state","time")) %>%
  left_join(n_by_group %>% filter(heat == "Heat") %>% rename(n_heat = n) %>% select(-heat),
            by = c("marker","state","time")) %>%
  left_join(d_by_time %>% select(marker, state, time, effsize) %>% rename(cohens_d = effsize),
            by = c("marker","state","time")) %>%
  group_by(marker) %>%                           # FDR across all timepoints+states within marker
  mutate(p.adj.fdr = p.adjust(p, method = "fdr")) %>%
  ungroup() %>%
  mutate(
    signif_raw = case_when(p < 0.001 ~ "***", p < 0.01 ~ "**", p < 0.05 ~ "*", TRUE ~ "ns"),
    signif_fdr = case_when(p.adj.fdr < 0.001 ~ "***", p.adj.fdr < 0.01 ~ "**", p.adj.fdr < 0.05 ~ "*", TRUE ~ "ns"),
    low_n_flag = if_else(pmin(n_control, n_heat) < 4, "LOW_N", "")
  )

# Paper-friendly table (edit columns as you like)
paper_table <- ttest_results %>%
  transmute(
    marker, state, time,
    n_control, n_heat,
    statistic, df,
    p_raw = p,
    p_fdr = p.adj.fdr,
    cohens_d,
    signif_raw, signif_fdr,
    note = low_n_flag
  ) %>%
  arrange(marker, state, time)

View(paper_table)

# Save table
write_csv(paper_table, file = here("tables", "stats_EdU_Caspase_timepoint_ttests_effectsizes.csv"))

# ============================================================
# (4) TWO-WAY ANOVA within each marker + state: percentage ~ heat * time
#     (Uses Type II/III-like behavior in rstatix via anova_test)
# ============================================================
anova_results <- df %>%
  group_by(marker, state) %>%
  anova_test(percentage ~ heat * time) %>%
  ungroup()

anova_results

write_csv(anova_results, file = here("tables", "stats_EdU_Caspase_twoWayANOVA_by_state.csv"))

# Optional: add simple interpretation flags
anova_flags <- anova_results %>%
  mutate(sig = case_when(p < 0.001 ~ "***", p < 0.01 ~ "**", p < 0.05 ~ "*", TRUE ~ "ns")) %>%
  select(marker, state, Effect, DFn, DFd, F, p, sig)

anova_flags

# ============================================================
# (3) TIME TRENDS within each heat group (Control and Heat separately)
#     One-way ANOVA across time, within marker+state+heat
#     then Tukey posthoc if desired
# ============================================================
time_trend_anova <- df %>%
  group_by(marker, state, heat) %>%
  anova_test(percentage ~ time) %>%
  ungroup()

time_trend_anova
write_csv(time_trend_anova, file = here("tables", "stats_timeTrend_ANOVA_within_heat.csv"))

# Optional Tukey posthoc for time trends (only meaningful if ANOVA is significant)
time_trend_tukey <- df %>%
  group_by(marker, state, heat) %>%
  tukey_hsd(percentage ~ time) %>%
  ungroup() %>%
  mutate(
    signif = case_when(p.adj < 0.001 ~ "***", p.adj < 0.01 ~ "**", p.adj < 0.05 ~ "*", TRUE ~ "ns")
  )

time_trend_tukey
write_csv(time_trend_tukey, file = here("tables", "stats_timeTrend_TukeyHSD_within_heat.csv"))

# ============================================================
# (1) ADD STARS ONTO FIGURE
# Option A (recommended): annotate each of the 6 individual panels before combining
#   - Requires you have p_edu_apo, p_edu_inoc, p_edu_sym, p_cas_apo, p_cas_inoc, p_cas_sym
#   - Adds stars for Control vs Heat at each time within that panel
# ============================================================

# Build a df of annotation positions: one label per marker+state+time
ann <- df %>%
  group_by(marker, state, time) %>%
  summarise(y = max(percentage, na.rm = TRUE) * 1.08, .groups = "drop") %>%
  left_join(ttest_results %>% select(marker, state, time, signif_raw), by = c("marker","state","time")) %>%
  mutate(group1 = "Control", group2 = "Heat")

# Helper to add stats to a panel (expects the panel is plotting marker+state subset)
add_stars <- function(p, marker_name, state_name, results_tbl = paper_table, use_fdr = TRUE) {
  star_col <- if (use_fdr) "signif_fdr" else "signif_raw"
  
  ann <- results_tbl %>%
    dplyr::filter(marker == marker_name, state == state_name) %>%
    dplyr::transmute(time, star = .data[[star_col]]) %>%
    dplyr::distinct(time, star) %>%
    dplyr::filter(star %in% c("*","**","***","****"))  # <- ONLY stars
  
  if (nrow(ann) == 0) return(p)
  
  y_max <- max(p$data$percentage, na.rm = TRUE)
  
  p + ggplot2::geom_text(
    data = ann,
    ggplot2::aes(x = time, y = y_max * 1.05, label = star),
    inherit.aes = FALSE,
    size = 4,
    fontface = "bold"
  )
}

# ============================================================
# Build panels WITH stars (all commented so nothing runs)
# ============================================================

# p_edu_apo_star  <- panel_plot(df %>% dplyr::filter(marker=="EdU", state=="Apo"),
#                          title_text="EdU – Apo", ylab="EdU-positive area (%)", show_legend=FALSE)

# p_edu_inoc_star <- panel_plot(df %>% dplyr::filter(marker=="EdU", state=="Inoc"),
#                          title_text="EdU – Inoc", ylab="EdU-positive area (%)", show_legend=FALSE)

# p_edu_sym_star  <- panel_plot(df %>% dplyr::filter(marker=="EdU", state=="Sym"),
#                          title_text="EdU – Sym", ylab="EdU-positive area (%)", show_legend=FALSE)


# p_cas_apo_star  <- panel_plot(df %>% dplyr::filter(marker=="Caspase", state=="Apo"),
#                              title_text="Caspase – Apo", ylab="Caspase-positive area (%)", show_legend=FALSE)

# p_cas_inoc_star <- panel_plot(df %>% dplyr::filter(marker=="Caspase", state=="Inoc"),
#                              title_text="Caspase – Inoc", ylab="Caspase-positive area (%)", show_legend=FALSE)

# p_cas_sym_star  <- panel_plot(df %>% dplyr::filter(marker=="Caspase", state=="Sym"),
#                              title_text="Caspase – Sym", ylab="Caspase-positive area (%)", show_legend=FALSE)
# ============================================================
# Add stars (FDR by default) — still commented
# ============================================================

# p_edu_apo_star  <- add_stars(p_edu_apo_star,  "EdU",    "Apo",  use_fdr=TRUE)
# p_edu_inoc_star <- add_stars(p_edu_inoc_star, "EdU",    "Inoc", use_fdr=TRUE)
# p_edu_sym_star  <- add_stars(p_edu_sym_star,  "EdU",    "Sym",  use_fdr=TRUE)

# p_cas_apo_star  <- add_stars(p_cas_apo_star,  "Caspase","Apo",  use_fdr=TRUE)
# p_cas_inoc_star <- add_stars(p_cas_inoc_star, "Caspase","Inoc", use_fdr=TRUE)
# p_cas_sym_star  <- add_stars(p_cas_sym_star,  "Caspase","Sym",  use_fdr=TRUE)

# ============================================================
# Combine annotated panels — commented
# ============================================================

# p6_annot <- (p_edu_apo_star | p_edu_inoc_star | p_edu_sym_star) /
#             (p_cas_apo_star | p_cas_inoc_star | p_cas_sym_star)

# print(p6_annot)

# ggsave("EdU_Caspase_6panel_annot.pdf",  p6_annot, width=11, height=8, units="in")
# ggsave("EdU_Caspase_6panel_annot.tiff", p6_annot, width=11, height=8, units="in",
#        dpi=600, compression="lzw")



# ============================================================
# QUICK CHECKS (highly recommended)
# ============================================================

# Which groups have <=3 replicates?
low_n_groups <- n_by_group %>% filter(n <= 3) %>% arrange(marker, state, time, heat)
low_n_groups

# What is the max y per marker/state (useful for setting coord_cartesian)?
max_y <- df %>% group_by(marker, state) %>% summarise(max_y = max(percentage, na.rm = TRUE), .groups = "drop")
max_y









# ============================================================
# 3-WAY ANOVA (heat × time × state) + post-hoc tests
# Works with your cleaned replicate-level data: df
# Columns needed in df: marker, percentage, heat, time, state
# ============================================================

library(tidyverse)
library(rstatix)

# ---- Ensure factor levels (edit if your time levels differ) ----
df3 <- df %>%
  mutate(
    marker = factor(marker, levels = c("EdU", "Caspase")),
    heat   = factor(heat, levels = c("Control", "Heat")),
    state  = factor(state, levels = c("Apo", "Inoc", "Sym")),
    time   = factor(time, levels = c("2dpl", "5dpl", "8dpl", "14dpl"))
  ) %>%
  drop_na(marker, heat, state, time, percentage)

# ============================================================
# 1) 3-way ANOVA per marker: percentage ~ heat * time * state
# ============================================================
anova3 <- df3 %>%
  group_by(marker) %>%
  anova_test(percentage ~ heat * time * state) %>%
  ungroup()

anova3


anova_table_pub <- anova3 %>%
  transmute(
    Marker   = marker,
    Effect   = Effect,
    DFn      = DFn,
    DFd      = DFd,
    F        = round(F, 3),
    p_value  = signif(p, 3),
    ges      = round(ges, 3),
    Signif   = case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      TRUE ~ "ns"
    )
  )

anova_table_pub

# (Optional) tidy flag table
anova3_flags <- anova3 %>%
  as_tibble() %>%
  mutate(sig = case_when(p < 0.001 ~ "***", p < 0.01 ~ "**", p < 0.05 ~ "*", TRUE ~ "ns")) %>%
  select(marker, Effect, DFn, DFd, F, p, sig)

anova3_flags



starify <- function(.df) {
  .df %>%
    dplyr::group_by(marker) %>%
    dplyr::mutate(p_fdr = p.adjust(p, method = "fdr")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      signif_raw = dplyr::case_when(p < 0.001 ~ "***", p < 0.01 ~ "**", p < 0.05 ~ "*", TRUE ~ "ns"),
      signif_fdr = dplyr::case_when(p_fdr < 0.001 ~ "***", p_fdr < 0.01 ~ "**", p_fdr < 0.05 ~ "*", TRUE ~ "ns")
    )
}

posthoc_heat <- df3 %>%
  dplyr::group_by(marker, time, state) %>%
  rstatix::t_test(percentage ~ heat, var.equal = FALSE) %>%
  dplyr::ungroup() %>%
  starify()

posthoc_state <- df3 %>%
  dplyr::group_by(marker, time, heat) %>%
  rstatix::pairwise_t_test(percentage ~ state, p.adjust.method = "none") %>%
  dplyr::ungroup() %>%
  dplyr::rename(p = p) %>%   # (noop, but keeps your intent explicit)
  starify()

posthoc_time <- df3 %>%
  dplyr::group_by(marker, state, heat) %>%
  rstatix::pairwise_t_test(percentage ~ time, p.adjust.method = "none") %>%
  dplyr::ungroup() %>%
  dplyr::rename(p = p) %>%   # (noop)
  starify()

# ============================================================
# 2) Post-hoc A: Heat (Control vs Heat) within each time × state
#    Welch t-tests + FDR correction within marker
# ============================================================

posthoc_heat

# ============================================================
# 3) Post-hoc B: State (Apo vs Inoc vs Sym) within each time × heat
#    Pairwise Welch t-tests + FDR correction within marker
# ============================================================

posthoc_state

# ============================================================
# 4) Post-hoc C: Time (2dpl vs 5dpl vs 8dpl vs 14dpl) within each state × heat
#    Pairwise Welch t-tests + FDR correction within marker
# ============================================================

posthoc_time

# ============================================================
# 5) Effect sizes (recommended)
# ============================================================

# Heat effect size within time × state
eff_heat <- df3 %>%
  group_by(marker, time, state) %>%
  cohens_d(percentage ~ heat, var.equal = FALSE) %>%
  ungroup() %>%
  rename(cohens_d = effsize)

eff_heat

# State effect size within time × heat (pairwise; rstatix returns comparisons)
eff_state <- df3 %>%
  group_by(marker, time, heat) %>%
  cohens_d(percentage ~ state, var.equal = FALSE) %>%
  ungroup() %>%
  rename(cohens_d = effsize)

eff_state

# ============================================================
# 6) Quick n table (useful for reporting / checking power)
# ============================================================
n_table <- df3 %>%
  group_by(marker, state, time, heat) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(marker, state, time, heat)

n_table

# ============================================================
# 7) Optional saves (edit path as needed)
# ============================================================
# write_csv(anova3_flags,   here::here("data", "anova3_by_marker.csv"))
# write_csv(posthoc_heat,   here::here("data", "posthoc_heat_within_time_state.csv"))
# write_csv(posthoc_state,  here::here("data", "posthoc_state_within_time_heat.csv"))
# write_csv(posthoc_time,   here::here("data", "posthoc_time_within_state_heat.csv"))
# write_csv(eff_heat,       here::here("data", "effectsize_heat_within_time_state.csv"))
# write_csv(eff_state,      here::here("data", "effectsize_state_within_time_heat.csv"))
# write_csv(n_table,        here::here("data", "n_table_marker_state_time_heat.csv"))




# ============================================================
# Publication-ready supplemental tables (CSV + nicely formatted)
# Requires: dplyr, tidyr, rstatix, gt (optional)
# Assumes you already have:
#   - df3   (your analysis dataframe; columns: marker, state, heat, time, percentage)
#   - posthoc_heat, posthoc_state, posthoc_time (from prior code)
# ============================================================

library(dplyr)
library(tidyr)

# ---------- 1) Heat posthoc table (Control vs Heat within marker×state×time) ----------
supp_heat <- posthoc_heat %>%
  transmute(
    marker, state, time,
    contrast = "Heat: Control vs Heat",
    group1, group2,
    n1, n2,
    statistic = round(statistic, 3),
    df = round(df, 2),
    p_raw = signif(p, 3),
    p_fdr = signif(p_fdr, 3),
    signif_raw, signif_fdr
  ) %>%
  arrange(marker, state, time)

write.csv(supp_heat, "Supplement_Posthoc_Heat.csv", row.names = FALSE)

colnames(posthoc_heat)
colnames(posthoc_state)
colnames(posthoc_time)

# ---------- 2) State posthoc table (Apo vs Inoc vs Sym within marker×heat×time) ----------
supp_state <- posthoc_state %>%
  transmute(
    marker, heat, time,
    contrast = "State: pairwise within time×heat",
    group1, group2,
    n1, n2,
    statistic = round(statistic, 3),
    df = round(df, 2),
    p_raw = signif(p, 3),
    p_fdr = signif(p_fdr, 3),
    signif_raw, signif_fdr
  ) %>%
  arrange(marker, heat, time, group1, group2)

write.csv(supp_state, "Supplement_Posthoc_State.csv", row.names = FALSE)

# ---------- 3) Time posthoc table (time pairwise within marker×state×heat) ----------
supp_time <- posthoc_time %>%
  transmute(
    marker, state, heat,
    contrast = "Time: pairwise within state×heat",
    group1, group2,                # here these are time levels (e.g., 2dpl vs 5dpl)
    n1, n2,
    statistic = round(statistic, 3),
    df = round(df, 2),
    p_raw = signif(p, 3),
    p_fdr = signif(p_fdr, 3),
    signif_raw, signif_fdr
  ) %>%
  arrange(marker, state, heat, group1, group2)

write.csv(supp_time, "Supplement_Posthoc_Time.csv", row.names = FALSE)

# ============================================================
# Optional: Make one combined “master supplemental” table
# ============================================================

supp_all <- bind_rows(
  supp_heat  %>% mutate(test_family = "Heat (Control vs Heat)"),
  supp_state %>% mutate(test_family = "State (Apo/Inoc/Sym pairwise)"),
  supp_time  %>% mutate(test_family = "Time (pairwise)")
) %>%
  relocate(test_family, .before = marker)

write.csv(supp_all, "Supplement_All_Posthoc.csv", row.names = FALSE)

# ============================================================
# Optional: Pretty table in R (gt) for PDFs/Word copy-paste
# ============================================================
library(gt)
gt(supp_all) %>%
   tab_header(title = "Supplementary Table: Post-hoc Comparisons") %>%
   fmt_number(columns = c(statistic, df), decimals = 2)



library(dplyr)
library(rstatix)
library(tibble)

# 1) Convert to a plain tibble (this strips the grouped_anova_test/rstatix_test classes)
anova3_tbl <- anova3 %>%
  rstatix::get_anova_table() %>%
  unclass() %>%
  as.data.frame() %>%
  as_tibble()

# 2) Make the publication-ready ANOVA table
anova_table_pub <- anova3_tbl %>%
  transmute(
    Marker  = marker,
    Effect  = Effect,
    DFn     = DFn,
    DFd     = DFd,
    F       = round(F, 3),
    p_value = signif(p, 3),
    ges     = round(ges, 3),
    Signif  = case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      TRUE ~ "ns"
    )
  )

anova_table_pub
anova_table_pub <- anova_table_pub %>%
  mutate(
    Effect = factor(
      Effect,
      levels = c("heat","time","state","heat:time","heat:state","time:state","heat:time:state")
    )
  ) %>%
  arrange(Marker, Effect)

write.csv(anova_table_pub, file = here("tables", "ANOVA_3way_heat_time_state.csv"), row.names = FALSE)
