# ============================================================
# PreSens SDR respirometry workflow
# OXYGEN-CONCENTRATION VERSION
# Multi-run version using:
# 1) real clock-time windows
# 2) manual well assignment
# 3) manual discard wells
# 4) pedal disk area normalization
# 5) oxygen converted from % air saturation to umol/L
# ============================================================

# =========================
# 0) Packages
# =========================
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(ggplot2)
library(lubridate)
library(broom)
library(car)
library(emmeans)
library(multcomp)
library(multcompView)
library(scales)
library(respirometry)

# =========================
# 1) Setup
# =========================

rm(list = ls())
graphics.off()

getwd()

select <- dplyr::select
filter <- dplyr::filter
mutate <- dplyr::mutate
lag <- dplyr::lag

# ============================================================
# 1) SETUP
# ============================================================

## 1.1 Working directory + output
setwd("/Users/junbc/Documents/GitHub/heating_lacerates_final")

out_dir <- "~/Documents/GitHub/heating_lacerates_final/data/respirometry/processed_outputs_respirometry"
dir.create(
  out_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

## 1.2 Chamber volume
chamber_volume_l <- 80e-6

# ============================================================
# 2) INPUT DATA
# ============================================================

## 2.1 Run metadata
run_info <- tribble(
  ~plate_name, ~file, ~run_date, ~tissue_type, ~temperature, ~date_modified, ~start_clock, ~win_start, ~win_end, ~discard, ~real_blanks, ~apo, ~sym,
  
  "20260326_Lacerate_25C_Plate4",
  "SDR 1106_032626_65531 PM_lacerate_sym_apo_plate4_25C_03.26.26.csv",
  "03/26/26", "Lacerate", "25C", "03/27/26 2:08 PM", "6:58:14 PM", "20:30", "23:30",
  "D3, D6, B4, C3, C4",
  "A1, A3, D2, D4",
  "C1, C2, C5, C6, D1, D5",
  "A2, A4, A5, A6, B1, B2, B3, B5, B6",
  
  "20260324_Lacerate_32C_Plate3",
  "SDR 1106_032426_54943 PM_lacerate_sym_apo_plate3_32C_03.24.26.csv",
  "03/24/26", "Lacerate", "32C", "03/25/26 3:53 PM", "5:53:09 PM", "19:15", "22:15",
  "D2, D3, D6, C1, C6",
  "A2, A3, D5",
  "C2, C3, C4, C5, D1, D4",
  "A1, A4, A5, A6, B1, B2, B3, B4, B5, B6",
  
  "20260319_Lacerate_25C_Plate2",
  "SDR 1106_031926_24601 PM_lacerate_sym_apo_25C_03.19.26.csv",
  "03/19/26", "Lacerate", "25C", "03/20/26 12:54 PM", "2:49:47 PM", "16:30", "19:00",
  "B2, A6",
  "B1, B6, D1, D3",
  "C1, C2, C3, C4, C5, C6, D2, D4, D5, D6",
  "A1, A2, A3, A4, A5, B3, B4, B5",
  
  "20260317_Lacerate_32C_Plate1",
  "SDR 1106_031726_21405 PM_lacerate_sym_apo_32C_03.17.26.csv",
  "03/17/26", "Lacerate", "32C", "03/18/26 10:41 AM", "2:16:43 PM", "15:15", "18:15",
  "D3",
  "A1, A6, D1, D6",
  "C1, C2, C3, C4, C5, C6, D2, D4, D5",
  "A2, A3, A4, A5, B1, B2, B3, B4, B5, B6"
)

## 2.2 Area data
area_data <- read_csv("~/Documents/GitHub/heating_lacerates_final/data/respirometry_area_data_raw.csv") %>%
  mutate(
    area = area / 100,
    side_estimated = str_detect(str_to_lower(notes), "on side"),
    note_rank = ifelse(side_estimated, 1, 0)
  ) %>%
  arrange(plate_name, well, note_rank) %>%
  group_by(plate_name, well) %>%
  slice(1) %>%
  ungroup() %>%
  select(label, area, notes, well, plate_name)

# ============================================================
# 3) HELPER FUNCTIONS
# ============================================================

split_wells <- function(x) {
  if (is.na(x) || x == "") return(character(0))
  str_split(x, ",")[[1]] |> str_trim()
}

time_to_sec <- function(x) {
  parts <- str_split(x, ":", simplify = TRUE)
  as.numeric(parts[, 1]) * 3600 +
    as.numeric(parts[, 2]) * 60 +
    as.numeric(parts[, 3])
}

get_settings <- function(file) {
  lines <- read_lines(file, locale = locale(encoding = "UTF-16"))
  
  get_val <- function(label) {
    hit <- lines[str_detect(lines, fixed(label))][1]
    if (is.na(hit)) return(NA_character_)
    
    parts <- str_split(hit, "\t", simplify = TRUE)
    parts <- str_trim(parts)
    parts <- parts[parts != ""]
    
    if (length(parts) >= 2) return(parts[length(parts)])
    
    hit |> str_remove(fixed(label)) |> str_trim()
  }
  
  tibble(
    barometric_pressure_hpa = as.numeric(get_val("Barometric pressure [hPa]:")),
    temperature_c_file = as.numeric(get_val("Temperature [°C]:")),
    salinity_ppt_file = as.numeric(get_val("Salinity [‰]:")),
    chamber_volume_ul_file = as.numeric(get_val("Chamber volume [µL]:")),
    oxygen_unit_file = get_val("Oxygen unit:")
  )
}

fit_slope <- function(df) {
  df <- df %>% arrange(time_s)
  
  if (nrow(df) < 2) {
    return(tibble(
      n_points = nrow(df),
      slope_o2_umol_l_per_s = NA_real_,
      slope_o2_umol_l_per_h = NA_real_,
      r2 = NA_real_,
      start_time = as.POSIXct(NA),
      end_time = as.POSIXct(NA)
    ))
  }
  
  mod <- lm(oxygen_conc_umol_l ~ time_s, data = df)
  
  tibble(
    n_points = nrow(df),
    slope_o2_umol_l_per_s = coef(mod)[["time_s"]],
    slope_o2_umol_l_per_h = coef(mod)[["time_s"]] * 3600,
    r2 = summary(mod)$r.squared,
    start_time = min(df$clock_time, na.rm = TRUE),
    end_time = max(df$clock_time, na.rm = TRUE)
  )
}

# ============================================================
# 4) STORAGE OBJECTS
# ============================================================

all_outputs <- vector("list", nrow(run_info))
names(all_outputs) <- run_info$plate_name

all_settings_used     <- list()
all_results           <- list()
all_rates_flagged     <- list()
all_manual_plate_keys <- list()
all_area_used         <- list()
all_rates_with_area   <- list()
all_rates_bio         <- list()
all_missing_area      <- list()
all_dat_long          <- list()

# ============================================================
# 5) RUN EACH PLATE
# ============================================================

for (i in seq_len(nrow(run_info))) {
  
  meta <- run_info[i, , drop = FALSE]
  this_plate_name <- meta$plate_name[[1]]
  file <- file.path(
    path.expand("~/Documents/GitHub/heating_lacerates_final/data/respirometry"),
    meta$file[[1]]
  )  
  message("Processing: ", this_plate_name)
  
  # ----------------------------------------------------------
  # 5.1 Settings + raw data
  # ----------------------------------------------------------
  
  settings <- get_settings(file)
  
  lines <- read_lines(file, locale = locale(encoding = "UTF-16"))
  header_line <- which(str_detect(lines, "^Date \\[DD-MM-YYYY\\]"))[1]
  
  raw_dat <- read_tsv(
    I(paste(lines[header_line:length(lines)], collapse = "\n")),
    show_col_types = FALSE
  )
  
  oxygen_cols <- names(raw_dat)[str_detect(names(raw_dat), "\\[Oxygen\\]")]
  
  # ----------------------------------------------------------
  # 5.2 Time + window
  # ----------------------------------------------------------
  
  start_datetime <- mdy_hms(paste(meta$run_date[[1]], meta$start_clock[[1]]))
  window_start   <- mdy_hm(paste(meta$run_date[[1]], meta$win_start[[1]]))
  window_end     <- mdy_hm(paste(meta$run_date[[1]], meta$win_end[[1]]))
  
  # ----------------------------------------------------------
  # 5.3 Manual well groups
  # ----------------------------------------------------------
  
  manual_plate_key <- bind_rows(
    tibble(well = split_wells(meta$discard[[1]]),     manual_group = "Discard"),
    tibble(well = split_wells(meta$real_blanks[[1]]), manual_group = "Blank"),
    tibble(well = split_wells(meta$apo[[1]]),         manual_group = "Apo"),
    tibble(well = split_wells(meta$sym[[1]]),         manual_group = "Sym")
  ) %>%
    distinct(well, .keep_all = TRUE) %>%
    mutate(plate_name = this_plate_name)
  
  # ----------------------------------------------------------
  # 5.4 Long format + oxygen conversion
  # ----------------------------------------------------------
  
  dat_long_raw <- raw_dat %>%
    select(`Relative time [HH:MM:SS]`, all_of(oxygen_cols)) %>%
    mutate(
      time_s = time_to_sec(`Relative time [HH:MM:SS]`),
      clock_time = start_datetime + seconds(time_s)
    ) %>%
    pivot_longer(
      cols = -c(`Relative time [HH:MM:SS]`, time_s, clock_time),
      names_to = "well",
      values_to = "oxygen_percent_air_sat"
    ) %>%
    mutate(
      well = str_remove(well, " \\[Oxygen\\]"),
      oxygen_percent_air_sat = as.numeric(oxygen_percent_air_sat)
    ) %>%
    left_join(
      manual_plate_key %>% select(well, manual_group),
      by = "well"
    ) %>%
    mutate(
      oxygen_conc_umol_l = respirometry::conv_o2(
        o2 = oxygen_percent_air_sat,
        from = "percent_a.s.",
        to = "umol_per_l",
        temp = settings$temperature_c_file,
        sal = settings$salinity_ppt_file,
        atm_pres = settings$barometric_pressure_hpa / 1013.25
      ),
      plate_name = this_plate_name,
      temperature = meta$temperature[[1]]
    )
  
  dat_use <- dat_long_raw %>%
    filter(
      clock_time >= window_start,
      clock_time <= window_end
    )
  
  dat_analysis <- dat_use %>%
    filter(
      !is.na(manual_group),
      manual_group != "Discard"
    )
  
  # ----------------------------------------------------------
  # 5.5 Fit slopes
  # ----------------------------------------------------------
  
  results <- map_dfr(unique(dat_analysis$well), function(w) {
    this <- dat_analysis %>% filter(well == w)
    
    bind_cols(
      tibble(
        plate_name = this_plate_name,
        well = w,
        treatment = unique(this$manual_group)
      ),
      fit_slope(this)
    )
  }) %>%
    mutate(
      oxygen_decline_rate_umol_l_h = -slope_o2_umol_l_per_h
    )
  
  # ----------------------------------------------------------
  # 5.6 Blank threshold + QC flags
  # ----------------------------------------------------------
  
  blank_mean <- mean(
    results$oxygen_decline_rate_umol_l_h[results$treatment == "Blank"],
    na.rm = TRUE
  )
  
  blank_sd <- sd(
    results$oxygen_decline_rate_umol_l_h[results$treatment == "Blank"],
    na.rm = TRUE
  )
  
  threshold <- blank_mean + 3 * blank_sd
  
  rates_flagged <- results %>%
    mutate(
      blank_threshold = threshold,
      is_biological = oxygen_decline_rate_umol_l_h > threshold,
      poor_fit = r2 < 0.9,
      unreliable = !is_biological | poor_fit
    )
  
  # ----------------------------------------------------------
  # 5.7 Join area
  # ----------------------------------------------------------
  
  area_this_plate <- area_data %>%
    filter(plate_name == this_plate_name)
  
  rates_with_area <- rates_flagged %>%
    left_join(area_this_plate, by = c("plate_name", "well")) %>%
    mutate(
      respiration_norm = oxygen_decline_rate_umol_l_h / area
    )
  
  rates_bio <- rates_with_area %>%
    filter(treatment %in% c("Apo", "Sym"))
  
  missing_area <- rates_with_area %>%
    filter(is.na(area))
  
  # ----------------------------------------------------------
  # 5.8 SAVE OBJECTS
  # ----------------------------------------------------------
  
  all_outputs[[i]] <- list(
    settings = settings %>% mutate(plate_name = this_plate_name, file = file),
    manual_plate_key = manual_plate_key,
    dat_long = dat_long_raw,
    dat_use = dat_use,
    dat_analysis = dat_analysis,
    results = results,
    rates_flagged = rates_flagged,
    area_this_plate = area_this_plate,
    rates_with_area = rates_with_area,
    rates_bio = rates_bio,
    missing_area = missing_area
  )
  
  all_settings_used[[this_plate_name]]     <- all_outputs[[i]]$settings
  all_results[[this_plate_name]]           <- all_outputs[[i]]$results
  all_rates_flagged[[this_plate_name]]     <- all_outputs[[i]]$rates_flagged
  all_manual_plate_keys[[this_plate_name]] <- all_outputs[[i]]$manual_plate_key
  all_area_used[[this_plate_name]]         <- all_outputs[[i]]$area_this_plate
  all_rates_with_area[[this_plate_name]]   <- all_outputs[[i]]$rates_with_area
  all_rates_bio[[this_plate_name]]         <- all_outputs[[i]]$rates_bio
  all_missing_area[[this_plate_name]]      <- all_outputs[[i]]$missing_area
  all_dat_long[[this_plate_name]]          <- all_outputs[[i]]$dat_long
  
  write_csv(
    rates_with_area,
    file.path(out_dir, paste0(this_plate_name, "_results.csv"))
  )
}




# ============================================================
# 6) COMBINE OUTPUTS
# ============================================================

all_settings_used     <- bind_rows(all_settings_used)
all_results           <- bind_rows(all_results)
all_rates_flagged     <- bind_rows(all_rates_flagged)
all_manual_plate_keys <- bind_rows(all_manual_plate_keys)
all_area_used         <- bind_rows(all_area_used)
all_rates_with_area   <- bind_rows(all_rates_with_area)
all_rates_bio         <- bind_rows(all_rates_bio)
all_missing_area      <- bind_rows(all_missing_area)
all_dat_long          <- bind_rows(all_dat_long)

# optional alias if you used this old name anywhere
all_rates <- all_results

# ============================================================
# 9) Quick checks
# ============================================================

print(all_rates_bio)

all_rates_bio %>%
  select(
    plate_name,
    well,
    treatment,
    slope_o2_umol_l_per_h,
    oxygen_decline_rate_umol_l_h,
    area,
    respiration_norm,
    r2,
    unreliable,
    notes
  ) %>%
  arrange(plate_name, treatment, well)

all_missing_area

# ============================================================
# 13) Full trace plots with captured window overlaid
# ============================================================

all_full_traces <- purrr::map_dfr(all_outputs, "dat_long") %>%
  filter(!is.na(oxygen_conc_umol_l)) %>%
  mutate(
    well = as.character(well),
    oxygen_conc_nmol_l = oxygen_conc_umol_l * 1000,
    manual_group = case_when(
      is.na(manual_group) ~ "Unassigned",
      TRUE ~ manual_group
    ),
    manual_group = factor(
      manual_group,
      levels = c("Unassigned", "Discard", "Blank", "Apo", "Sym")
    )
  )

window_info <- run_info %>%
  mutate(
    plate_start_clock = lubridate::mdy_hms(paste(run_date, start_clock)),
    window_start_clock = lubridate::mdy_hm(paste(run_date, win_start)),
    window_end_clock   = lubridate::mdy_hm(paste(run_date, win_end))
  ) %>%
  select(
    plate_name,
    plate_start_clock,
    window_start_clock,
    window_end_clock
  )

all_full_traces <- all_full_traces %>%
  left_join(window_info, by = "plate_name")

trace_labels <- all_full_traces %>%
  group_by(plate_name, well, manual_group) %>%
  filter(clock_time == max(clock_time, na.rm = TRUE)) %>%
  ungroup()



manual_trace_counts <- all_full_traces %>%
  distinct(plate_name, well, manual_group) %>%
  count(plate_name, manual_group)

write_csv(
  manual_trace_counts,
  file.path(out_dir, "ALL_RUNS_manual_trace_counts_oxygen_concentration_nmol.csv")
)

## Rework for Supplemental Figure to make hours 0 to 24
# ----------------------------
# Create elapsed time in hours for each plate
# ----------------------------
all_full_traces <- all_full_traces %>%
  group_by(plate_name) %>%
  filter(
    manual_group != "Discard",
  ) %>%
  mutate(
    time_hours = as.numeric(difftime(clock_time, min(clock_time), units = "hours"))
  ) %>%
  ungroup()

# ----------------------------
# Recompute shaded window positions on the SAME time scale
# ----------------------------
window_info_plot <- window_info %>%
  mutate(
    window_start_hours_plot = as.numeric(difftime(window_start_clock, plate_start_clock, units = "hours")),
    window_end_hours_plot   = as.numeric(difftime(window_end_clock,   plate_start_clock, units = "hours"))
  )

# ----------------------------
# Label positions at RIGHTMOST point of each well
# ----------------------------
trace_labels_right <- all_full_traces %>%
  group_by(plate_name, well) %>%
  slice_max(order_by = time_hours, n = 1, with_ties = FALSE) %>%
  ungroup()

# ----------------------------
# Plot with elapsed time (hours)
# ----------------------------
FigS3_all_tracing <- ggplot(
  all_full_traces,
  aes(
    x = time_hours,
    y = oxygen_conc_nmol_l,
    group = well,
    color = manual_group
  )
) +
  geom_rect(
    data = window_info_plot,
    aes(
      xmin = window_start_hours_plot,
      xmax = window_end_hours_plot,
      ymin = -Inf,
      ymax = Inf
    ),
    inherit.aes = FALSE,
    fill = "grey70",
    alpha = 0.15
  ) +
  geom_line(alpha = 0.85, linewidth = 1.2) +
  geom_text(
    data = trace_labels_right,
    aes(
      x = time_hours,
      y = oxygen_conc_nmol_l,
      label = well
    ),
    hjust = -0.05,
    size = 2.7,
    show.legend = FALSE
  ) +
  facet_wrap(
    ~ plate_name,
    scales = "free_x",
    ncol = 2,
    labeller = labeller(
      plate_name = c(
        "20260317_Lacerate_32C_Plate1" = "Plate A - 32°C",
        "20260319_Lacerate_25C_Plate2" = "Plate B - 25°C",
        "20260324_Lacerate_32C_Plate3" = "Plate C - 32°C",
        "20260326_Lacerate_25C_Plate4" = "Plate D - 25°C"
      )
    )
  ) +
  scale_color_manual(
    values = c(
      "Unassigned" = "#D9D9D9",
#     "Discard"    = "#E66101",
      "Blank"      = "#7A5DC7",
      "Apo"        = "#8F8F8F",
      "Sym"        = "#A65628"
    ),
    na.value = "#EAEAEA"
  ) +
  scale_x_continuous(
    breaks = seq(
      0,
      ceiling(max(all_full_traces$time_hours, na.rm = TRUE)),
      by = 3
    ),
    expand = expansion(mult = c(0.01, 0.10))
  ) +
  labs(
    x = "Time (hours)",
    y = expression(paste("Oxygen concentration (nmol/L)")),
    color = "Manual group"
  ) +
  theme_bw() +
  theme(
    text = element_text(family = "sans"),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    strip.background = element_rect(fill = "grey85", color = "black", linewidth = 1),
    strip.text = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    axis.text.x = element_text(angle = 0, hjust = 1),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13),
    legend.key.size = unit(1.2, "lines"),
    legend.position = "right"
  )

FigS3_all_tracing

ggsave(
  filename = "FigS3_ALL_RUNS_full_trace_manual_groups_labeled_oxygen_concentration_nmol_tight.png",
  plot = FigS3_all_tracing,
  path = "~/Documents/GitHub/heating_lacerates_final/figs",
  device = "png",
  width = 15,
  height = 9,
  units = "in",
  dpi = 600,
  bg = "white"
)

ggsave(
  filename = "FigS3_ALL_RUNS_full_trace_manual_groups_labeled_oxygen_concentration_nmol_tight.pdf",
  plot = FigS3_all_tracing,
  path = "~/Documents/GitHub/heating_lacerates_final/figs",
  device = pdf,
  width = 15,
  height = 9,
  units = "in",
  bg = "white"
)

# ============================================================
# 14) Pedal disc size plot
# ============================================================
area_plot_dat <- area_data %>%
  left_join(
    all_manual_plate_keys %>% select(plate_name, well, manual_group),
    by = c("plate_name", "well")
  ) %>%
  filter(manual_group %in% c("Apo", "Sym")) %>%
  mutate(
    temperature = case_when(
      str_detect(plate_name, "25C") ~ "25C",
      str_detect(plate_name, "32C") ~ "32C"
    ),
    treatment = factor(manual_group, levels = c("Apo", "Sym")),
    temperature = factor(temperature, levels = c("25C", "32C"))
  )

p_pedal_area <- ggplot(
  area_plot_dat %>%
    mutate(
      color_group = case_when(
        treatment == "Sym" & temperature == "25C" ~ "sym_25",
        treatment == "Apo" & temperature == "25C" ~ "apo_25",
        treatment == "Sym" & temperature == "32C" ~ "sym_32",
        treatment == "Apo" & temperature == "32C" ~ "apo_32"
      )
    ),
  aes(
    x = temperature,
    y = area,
    color = color_group
  )
) +
  geom_boxplot(
    width = 0.42,
    fill = NA,
    linewidth = 1.4,
    outlier.shape = NA,
    show.legend = FALSE
  ) +
  geom_jitter(
    width = 0.08,
    size = 2.0,
    alpha = 0.7,
    show.legend = FALSE
  ) +
  facet_wrap(~ factor(treatment, levels = c("Sym", "Apo"))) +
  
  # 🔥 4-color scheme
  scale_color_manual(
    values = c(
      "sym_25" = "#3B6FB6",
      "apo_25" = "#6FA3D9",
      "sym_32" = "#E64B35",
      "apo_32" = "#F39B7F"
    )
  ) +
  
  scale_x_discrete(
    labels = c("25C" = "25°C", "32C" = "32°C")
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0.02, 0.04))
  ) +
  coord_cartesian(ylim = c(0.05, 0.5)) +
  labs(
    x = "Temperature",
    y = expression(paste("Pedal disc area (mm"^2, ")"))
  ) +
  theme_bw() +
  theme(
    text = element_text(family = "sans"),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    strip.background = element_rect(fill = "grey85", color = "black", linewidth = 1),
    strip.text = element_text(size = 13, face = "bold"),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 12),
    legend.position = "none"
  )

p_pedal_area

ggsave(
  filename = "FigS2_Resp_pedal_area.png",
  plot = p_pedal_area,
  path = "~/Documents/GitHub/heating_lacerates_final/figs",
  device = "png",
  width = 7,
  height = 5,
  units = "in",
  dpi = 600,
  bg = "white"
)

ggsave(
  filename = "FigS2_Resp_pedal_area.pdf",
  plot = p_pedal_area,
  path = "~/Documents/GitHub/heating_lacerates_final/figs",
  device = pdf,
  width = 7,
  height = 5,
  units = "in",
  bg = "white"
)

# ============================================================
# 15) Temperature x treatment model on normalized oxygen respiration
# ============================================================

stats_dat2 <- all_rates_bio %>%
  filter(
    treatment %in% c("Sym", "Apo"),
    !is.na(oxygen_decline_rate_umol_l_h),
    !is.na(respiration_norm)
  ) %>%
  mutate(
    oxygen_decline_rate_nmol_l_h = oxygen_decline_rate_umol_l_h * 1000,
    respiration_norm_nmol = respiration_norm * 1000,
    temperature = case_when(
      str_detect(plate_name, "25C") ~ "25C",
      str_detect(plate_name, "32C") ~ "32C"
    ),
    temperature = factor(temperature, levels = c("25C", "32C")),
    treatment = factor(treatment, levels = c("Apo", "Sym")),
    plate_name = factor(plate_name)
  )

mod <- lm(respiration_norm_nmol ~ treatment * temperature, data = stats_dat2)

summary(mod)
anova(mod)

par(mfrow = c(2, 2))
plot(mod)
par(mfrow = c(1, 1))

shapiro.test(residuals(mod))
car::leveneTest(respiration_norm_nmol ~ treatment * temperature, data = stats_dat2)

diag_dat <- data.frame(
  fitted = fitted(mod),
  resid = residuals(mod)
)

ggplot(diag_dat, aes(fitted, resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = 2) +
  theme_bw() +
  labs(
    title = "Residuals vs fitted",
    x = "Fitted values",
    y = "Residuals"
  )

ggplot(diag_dat, aes(sample = resid)) +
  stat_qq() +
  stat_qq_line() +
  theme_bw() +
  labs(title = "QQ plot of residuals")

emmeans(mod, pairwise ~ treatment | temperature)
emmeans(mod, pairwise ~ temperature | treatment)

## Log-transformed model ##
mod_log <- lm(log(respiration_norm_nmol) ~ treatment * temperature, data = stats_dat2)

par(mfrow = c(2, 2))
plot(mod_log)
par(mfrow = c(1, 1))

shapiro.test(residuals(mod_log))
car::leveneTest(log(respiration_norm_nmol) ~ treatment * temperature, data = stats_dat2)

summary(mod_log)


car::Anova(mod_log, type = "II")
emmeans(mod_log, pairwise ~ treatment | temperature)
emmeans(mod_log, pairwise ~ temperature | treatment)

emm_log_treat <- emmeans(mod_log, ~ treatment | temperature)

emm_log_treat_resp <- emmeans(mod_log, ~ treatment | temperature, type = "response")

emm_log_temp_resp <- emmeans(mod_log, ~ temperature | treatment, type = "response")

# Table S8. Type II ANOVA for respiration rate

anova_respiration <- car::Anova(mod_log, type = "II")
anova_respiration
anova_respiration_df <- as.data.frame(anova_respiration) %>%
  tibble::rownames_to_column("Factor")
anova_respiration_df
write.csv(
  anova_respiration_df,
  file = file.path(
    "~/Documents/GitHub/heating_lacerates_final/tables",
    "TableS8_anova_respiration.csv"
  ),
  row.names = FALSE
)

# Table S9A. Treatment comparisons within temperature

emm_log_treat <- emmeans(mod_log, ~ treatment | temperature)
tukey_resp_treat <- summary(
  pairs(emm_log_treat, adjust = "tukey")
)
tukey_resp_treat_df <- as.data.frame(tukey_resp_treat)

write.csv(
  tukey_resp_treat_df,
  file = file.path(
    "~/Documents/GitHub/heating_lacerates_final/tables",
    "TableS9A_treatment_within_temperature.csv"
  ),
  row.names = FALSE
)

# Table S9B. Temperature comparisons within treatment

emm_log_temp <- emmeans(mod_log, ~ temperature | treatment)

tukey_resp_temp <- summary(
  pairs(emm_log_temp, adjust = "tukey")
)

tukey_resp_temp_df <- as.data.frame(tukey_resp_temp)

write.csv(
  tukey_resp_temp_df,
  file = file.path(
    "~/Documents/GitHub/heating_lacerates_final/tables",
    "TableS9B_temperature_within_treatment.csv"
  ),
  row.names = FALSE
)

# ============================================================
# 16) Tukey letters + final oxygen-based figures
# ============================================================
plot_dat <- all_rates_bio %>%
  mutate(
    temp_group = case_when(
      plate_name %in% c("20260317_Lacerate_32C_Plate1",
                        "20260324_Lacerate_32C_Plate3") ~ "32C",
      plate_name %in% c("20260319_Lacerate_25C_Plate2",
                        "20260326_Lacerate_25C_Plate4") ~ "25C",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(
    !is.na(temp_group),
    treatment %in% c("Apo", "Sym")
  ) %>%
  mutate(
    oxygen_decline_rate_nmol_l_h = oxygen_decline_rate_umol_l_h * 1000,
    respiration_norm_nmol = respiration_norm * 1000,
    treatment = factor(treatment, levels = c("Apo", "Sym")),
    temp_group = factor(temp_group, levels = c("25C", "32C")),
    temperature = temp_group,
    combined_treatment = case_when(
      treatment == "Apo" & temp_group == "25C" ~ "Apo 25°C",
      treatment == "Sym" & temp_group == "25C" ~ "Sym 25°C",
      treatment == "Apo" & temp_group == "32C" ~ "Apo 32°C",
      treatment == "Sym" & temp_group == "32C" ~ "Sym 32°C"
    ),
    combined_treatment = factor(
      combined_treatment,
      levels = c("Apo 25°C", "Sym 25°C", "Apo 32°C", "Sym 32°C")
    )
  )

emm_groups <- emmeans(
  mod_log,
  ~ treatment * temperature,
  type = "response"
)

cld_groups <- cld(
  emm_groups,
  Letters = letters
)

cld_plot <- as.data.frame(cld_groups) %>%
  mutate(
    treatment = factor(treatment, levels = c("Apo", "Sym")),
    temperature = factor(temperature, levels = c("25C", "32C")),
    combined_treatment = case_when(
      treatment == "Apo" & temperature == "25C" ~ "Apo 25°C",
      treatment == "Sym" & temperature == "25C" ~ "Sym 25°C",
      treatment == "Apo" & temperature == "32C" ~ "Apo 32°C",
      treatment == "Sym" & temperature == "32C" ~ "Sym 32°C"
    ),
    combined_treatment = factor(
      combined_treatment,
      levels = c("Apo 25°C", "Sym 25°C", "Apo 32°C", "Sym 32°C")
    ),
    .group = gsub(" ", "", .group)
  )

label_pos_raw <- plot_dat %>%
  group_by(combined_treatment) %>%
  summarise(
    y = max(oxygen_decline_rate_nmol_l_h, na.rm = TRUE) * 1.08,
    .groups = "drop"
  )

label_pos_norm <- plot_dat %>%
  group_by(combined_treatment) %>%
  summarise(
    y = max(respiration_norm_nmol, na.rm = TRUE) * 1.08,
    .groups = "drop"
  )

cld_plot_raw <- cld_plot %>%
  left_join(label_pos_raw, by = "combined_treatment")

cld_plot_norm <- cld_plot %>%
  left_join(label_pos_norm, by = "combined_treatment")

print(cld_plot_raw)
print(cld_plot_norm)


# ============================================================
# 16) LOG-TRANSFORMED DATA FOR FINAL FIGURE
# ============================================================

plot_dat_final <- plot_dat %>%
  mutate(
    log10_respiration = log10(respiration_norm_nmol)
  )

# ============================================================
# LABEL POSITIONS FOR TUKEY LETTERS
# ============================================================

label_pos_log_norm <- plot_dat_final %>%
  group_by(combined_treatment) %>%
  summarise(
    y = max(log10_respiration, na.rm = TRUE) + 0.10,
    .groups = "drop"
  )

cld_plot_log_norm <- cld_plot %>%
  left_join(label_pos_log_norm, by = "combined_treatment")


# ============================================================
# FINAL FIGURE
# ============================================================

Fig5 <- ggplot(
  plot_dat_final %>%
    mutate(
      color_group = case_when(
        treatment == "Sym" & temperature == "25C" ~ "sym_25",
        treatment == "Apo" & temperature == "25C" ~ "apo_25",
        treatment == "Sym" & temperature == "32C" ~ "sym_32",
        treatment == "Apo" & temperature == "32C" ~ "apo_32"
      )
    ),
  aes(
    x = temperature,
    y = log10_respiration,
    color = color_group
  )
) +
  geom_boxplot(
    width = 0.42,
    fill = NA,
    linewidth = 1.4,
    outlier.shape = NA,
    show.legend = FALSE
  ) +
  geom_jitter(
    width = 0.08,
    size = 2.0,
    alpha = 0.7,
    show.legend = FALSE
  ) +
  geom_text(
    data = cld_plot_log_norm,
    aes(
      x = temperature,
      y = y,
      label = .group
    ),
    inherit.aes = FALSE,
    size = 4.8,
    fontface = "bold",
    family = "sans",
    color = "black"
  ) +
  facet_wrap(~ factor(treatment, levels = c("Sym", "Apo"))) +
  scale_color_manual(
    values = c(
      "sym_25" = "#3B6FB6",
      "apo_25" = "#6FA3D9",
      "sym_32" = "#E64B35",
      "apo_32" = "#F39B7F"
    )
  ) +
  scale_x_discrete(
    labels = c("25C" = "25°C", "32C" = "32°C")
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0.02, 0.04))
  ) +
  coord_cartesian(ylim = c(1.15, 2.65)) +
  labs(
    x = "Temperature",
    y = expression(paste("Log"[10], " respiration rate (nmol/L/h/mm"^2, ")"))
  ) +
  theme_bw() +
  theme(
    text = element_text(family = "sans"),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    strip.background = element_rect(fill = "grey85", color = "black", linewidth = 1),
    strip.text = element_text(size = 13, face = "bold"),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 12, color = "black"),
    legend.position = "none"
  )

Fig5

ggsave(
  filename = "Fig5_final_plot_May4.png",
  plot = Fig5,
  path = "~/Documents/GitHub/heating_lacerates_final/figs",
  device = "png",
  width = 7,
  height = 5,
  units = "in",
  dpi = 600,
  bg = "white"
)

ggsave(
  filename = "Fig5_final_plot_May4.pdf",
  plot = Fig5,
  path = "~/Documents/GitHub/heating_lacerates_final/figs",
  device = pdf,
  width = 7,
  height = 5,
  units = "in",
  bg = "white"
)