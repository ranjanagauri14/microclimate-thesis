
library(tidyverse)
library(readxl)
library(lubridate)
library(lme4)
library(lmerTest)
library(performance)
library(car)

# ============================================================================
# 1: DATA PREPARATION
# ============================================================================

read_ibutton_folder <- function(folder_path) {
  csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
  bind_rows(lapply(csv_files, function(f) {
    id_line <- read.csv(f, header = FALSE, skip = 1, nrows = 1, stringsAsFactors = FALSE)
    ibutton_id <- str_remove(id_line$V1, "1-Wire/iButton Registration Number: ")
    dat <- read.csv(f, skip = 18, header = TRUE, stringsAsFactors = FALSE)
    dat$ID <- trimws(ibutton_id); dat
  }))
}
parse_ibutton <- function(raw_data) {
  raw_data %>% mutate(
    Date_Time = dmy_hms(Date.Time), Date = as.Date(Date_Time),
    Time = format(Date_Time, "%H:%M:%S"), Month = month(Date_Time), Year = year(Date_Time)
  ) %>% select(ID, Unit, Value, Date, Time, Month, Year)
}
ibutton_2023 <- parse_ibutton(read_ibutton_folder("data/raw/ibutton_2023"))
ibutton_2024 <- parse_ibutton(read_ibutton_folder("data/raw/ibutton_2024"))

deploy_2023 <- read_excel("data/metadata/iButtons info_2023.xlsx") %>%
  select(iButton_ID, Plot_ID) %>% mutate(across(everything(), trimws))
deploy_2024 <- read_excel("data/metadata/iButtons info_2024.xlsx", skip = 1) %>%
  select(iButton_ID, Plot_ID) %>% mutate(across(everything(), trimws))
join_ibutton <- function(s, d) s %>% left_join(d, by = c("ID" = "iButton_ID")) %>% filter(!is.na(Plot_ID))
ib_2023 <- join_ibutton(ibutton_2023, deploy_2023)
ib_2024 <- join_ibutton(ibutton_2024, deploy_2024)

split_T_RH <- function(dat, equipment_label) {
  temp <- dat %>% filter(Unit == "C") %>% rename(Temperature_C = Value) %>%
    select(Plot_ID, ID, Date, Time, Month, Year, Temperature_C)
  rh <- dat %>% filter(Unit == "%RH") %>% rename(Relative_humidity = Value) %>%
    select(ID, Date, Time, Relative_humidity)
  temp %>% left_join(rh, by = c("ID", "Date", "Time")) %>% mutate(Equipment = equipment_label)
}
ibutton_data_2023 <- split_T_RH(ib_2023, "iButton")
ibutton_data_2024 <- split_T_RH(ib_2024, "iButton")

# --- Kestrel (2024)  ---
kestrel_files <- list.files("data/raw/kestrel_2024", pattern = "\\.csv$", full.names = TRUE)
kestrel_raw <- bind_rows(lapply(kestrel_files, function(f) {
  id_line <- read.csv(f, header = FALSE, nrows = 1, stringsAsFactors = FALSE)
  dat <- read.csv(f, skip = 3, header = TRUE, stringsAsFactors = FALSE)
  dat$ID <- trimws(id_line$V2); dat
}))
kestrel_data <- kestrel_raw %>% mutate(
    Date_Time = ymd_hms(FORMATTED.DATE_TIME, quiet = TRUE), Date = as.Date(Date_Time),
    Time = format(Date_Time, "%H:%M:%S"), Month = month(Date_Time), Year = year(Date_Time),
    Temperature = suppressWarnings(as.numeric(Temperature)),
    Relative.Humidity = suppressWarnings(as.numeric(Relative.Humidity))) %>%
  filter(!is.na(Temperature), Year == 2024) %>%
  mutate(Temperature_C = (Temperature - 32) * 5 / 9, ID = str_remove_all(ID, "D2 - |#") %>% trimws()) %>%
  rename(Relative_humidity = Relative.Humidity) %>%
  select(ID, Temperature_C, Relative_humidity, Date, Time, Month, Year)
kestrel_deploy <- read_excel("data/metadata/Kestrel info_2024.xlsx") %>%
  select(Kestrel_ID, Plot_ID) %>%
  mutate(Kestrel_ID = as.character(Kestrel_ID) %>% trimws(), Plot_ID = trimws(Plot_ID))
kestrel_final <- kestrel_data %>% left_join(kestrel_deploy, by = c("ID" = "Kestrel_ID")) %>%
  filter(!is.na(Plot_ID)) %>% mutate(Equipment = "Kestrel") %>%
  select(Plot_ID, ID, Date, Time, Month, Year, Temperature_C, Relative_humidity, Equipment)

# --- Combine; derive site / line / line_unique / distance / edge_type ---
derive_cols <- function(df) df %>% mutate(
    Plot_ID = trimws(Plot_ID),
    site = case_when(
      str_detect(Plot_ID, "^KT20|^KT_20") ~ "KT20",
      str_detect(Plot_ID, "^KT21|^KT_21") ~ "KT21",
      str_detect(Plot_ID, "^KDM") ~ "KDM", str_detect(Plot_ID, "^MG") ~ "MG",
      str_detect(Plot_ID, "^DG") ~ "DG", str_detect(Plot_ID, "^NCF") ~ "NCF",
      TRUE ~ str_extract(Plot_ID, "^[A-Z0-9]+(?=_)")),
    line = str_extract(Plot_ID, "L[0-9X]+"),
    # site-unique line so L1@KT20 != L1@MG. NCF -> "NCF_LX".
    line_unique = paste(site, line, sep = "_"),
    plot_number = as.numeric(str_extract(str_extract(Plot_ID, "[PQ][0-9]+$"), "[0-9]+")),
    distance_to_edge_m = if_else(str_detect(Plot_ID, "^NCF"), plot_number, (plot_number - 1) * 20),
    edge_type = case_when(site %in% c("KT20", "KT21", "KDM") ~ "open",
                          site %in% c("MG", "DG", "NCF") ~ "closed", TRUE ~ NA_character_))

microclimate_all <- bind_rows(ibutton_data_2023, ibutton_data_2024, kestrel_final) %>% derive_cols()

# --- RH quality control (iButton saturation): cap 100-105 -> 100; <0 or >105 -> NA ---
microclimate_all <- microclimate_all %>% mutate(
  RH_raw = Relative_humidity,
  Relative_humidity = case_when(
    Relative_humidity < 0 ~ NA_real_, Relative_humidity > 105 ~ NA_real_,
    Relative_humidity > 100 ~ 100, TRUE ~ Relative_humidity))

##RH CEILING CENSORING (data-quality caveat for all RH results
rh_dry <- microclimate_all %>% filter(Equipment == "iButton", Month %in% c(1,2,3,4))
cat(sprintf("Jan-Apr iButton RH at/above 100%% ceiling: %.1f%% of valid readings\n",
            100 * mean(rh_dry$RH_raw >= 100, na.rm = TRUE)))
cat(sprintf("  by edge type -> closed: %.1f%%, open: %.1f%%\n",
  100*mean(rh_dry$RH_raw[rh_dry$edge_type=="closed"] >= 100, na.rm=TRUE),
  100*mean(rh_dry$RH_raw[rh_dry$edge_type=="open"]   >= 100, na.rm=TRUE)))

# ----------------------------------------------------------------------------
# SEASON / WINDOW DEFINITION
# ----------------------------------------------------------------------------
dry_months_full   <- c(12, 1, 2, 3, 4, 5)   # conceptual dry season
common_months      <- c(1, 2, 3, 4)          # primary modelling window

# PRIMARY analysis dataset: iButton + Kestrel, common months
prim <- microclimate_all %>%
  filter(Month %in% common_months) %>%
  mutate(year_factor = factor(Year), month_factor = factor(Month),
         week = floor_date(Date, "week"))

#observations by month and site
print(prim %>% distinct(site, Year, Plot_ID) %>% count(site, Year) %>%
        pivot_wider(names_from = Year, values_from = n, values_fill = 0))

# ============================================================================
# 2: AGGREGATION
# ============================================================================

weekly_means <- prim %>%
  group_by(Plot_ID, site, line, line_unique, edge_type, distance_to_edge_m, year_factor, week) %>%
  summarise(weekly_mean_T = mean(Temperature_C, na.rm = TRUE),
            weekly_mean_RH = mean(Relative_humidity, na.rm = TRUE),
            n_obs = n(), .groups = "drop") %>%
  filter(n_obs >= 6)
weekly_means_rh <- weekly_means %>% filter(!is.na(weekly_mean_RH), is.finite(weekly_mean_RH))

daily_stats <- prim %>%
  group_by(Plot_ID, site, line, line_unique, edge_type, distance_to_edge_m,
           year_factor, month_factor, Date) %>%
  summarise(n_obs = n(),
            n_valid_T = sum(!is.na(Temperature_C)), n_valid_RH = sum(!is.na(Relative_humidity)),
            daily_mean_T = mean(Temperature_C, na.rm = TRUE),
            daily_range_T = if_else(n_valid_T >= 2, max(Temperature_C, na.rm=TRUE) - min(Temperature_C, na.rm=TRUE), NA_real_),
            daily_sd_T    = if_else(n_valid_T >= 2, sd(Temperature_C, na.rm = TRUE), NA_real_),
            # daily_cv_T DELIBERATELY DROPPED: CV is undefined for interval-scale (Celsius) data.
            daily_mean_RH  = if_else(n_valid_RH >= 2, mean(Relative_humidity, na.rm=TRUE), NA_real_),
            daily_range_RH = if_else(n_valid_RH >= 2, max(Relative_humidity, na.rm=TRUE) - min(Relative_humidity, na.rm=TRUE), NA_real_),
            daily_sd_RH    = if_else(n_valid_RH >= 2, sd(Relative_humidity, na.rm = TRUE), NA_real_),
            daily_cv_RH    = if_else(!is.na(daily_sd_RH) & daily_mean_RH > 0, daily_sd_RH / daily_mean_RH * 100, NA_real_),
            .groups = "drop") %>%
  filter(n_obs >= 6)
daily_stats_rh <- daily_stats %>% filter(!is.na(daily_range_RH), is.finite(daily_range_RH))

cat("\nWeekly records:", nrow(weekly_means), " | Daily records:", nrow(daily_stats), "\n")

# ============================================================================
# 3: FIGURES — one per question × sub-question
# ============================================================================
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

# --- Q1: Weekly means ~ distance × year (no edge_type in model) ---

ggsave("output/figures/fig_q1_weekly_T.png",
  ggplot(weekly_means, aes(distance_to_edge_m, weekly_mean_T, color = year_factor)) +
    geom_point(alpha = 0.25, size = 1) + geom_smooth(method = "lm", se = TRUE) +
    theme_bw() +
    labs(x = "Distance from edge (m)", y = "Weekly mean T (°C)", color = "Year",
         title = "Q1a: Temperature gradient (Jan–Apr)"),
  width = 8, height = 5, dpi = 300)

ggsave("output/figures/fig_q1_weekly_RH.png",
  ggplot(weekly_means_rh, aes(distance_to_edge_m, weekly_mean_RH, color = year_factor)) +
    geom_point(alpha = 0.25, size = 1) + geom_smooth(method = "lm", se = TRUE) +
    theme_bw() +
    labs(x = "Distance from edge (m)", y = "Weekly mean RH (%)", color = "Year",
         title = "Q1b: Relative humidity gradient (Jan–Apr)"),
  width = 8, height = 5, dpi = 300)

# --- Q2: Daily variability ~ distance × year (no edge_type in model) ---



ggsave("output/figures/fig_q2_daily_T_sd.png",
  ggplot(daily_stats, aes(distance_to_edge_m, daily_sd_T, color = year_factor)) +
    geom_point(alpha = 0.15, size = 0.7) + geom_smooth(method = "lm", se = TRUE) +
    theme_bw() +
    labs(x = "Distance from edge (m)", y = "Daily T SD (°C)", color = "Year",
         title = "Q2b: Daily temperature SD gradient"),
  width = 8, height = 5, dpi = 300)


ggsave("output/figures/fig_q2_daily_RH_sd.png",
  ggplot(daily_stats_rh %>% filter(!is.na(daily_sd_RH)),
         aes(distance_to_edge_m, daily_sd_RH, color = year_factor)) +
    geom_point(alpha = 0.15, size = 0.7) + geom_smooth(method = "lm", se = TRUE) +
    theme_bw() +
    labs(x = "Distance from edge (m)", y = "Daily RH SD (%)", color = "Year",
         title = "Q2d: Daily RH standard deviation gradient"),
  width = 8, height = 5, dpi = 300)

ggsave("output/figures/fig_q2_daily_RH_cv.png",
  ggplot(daily_stats_rh %>% filter(!is.na(daily_cv_RH)),
         aes(distance_to_edge_m, daily_cv_RH, color = year_factor)) +
    geom_point(alpha = 0.15, size = 0.7) + geom_smooth(method = "lm", se = TRUE) +
    theme_bw() +
    labs(x = "Distance from edge (m)", y = "Daily RH CV (%)", color = "Year",
         title = "Q2e: Daily RH coefficient of variation gradient"),
  width = 8, height = 5, dpi = 300)

# --- Q3: Three-way interaction ~ distance × year × edge_type ---

ggsave("output/figures/fig_q3_weekly_T_by_edge.png",
  ggplot(weekly_means, aes(distance_to_edge_m, weekly_mean_T, color = year_factor)) +
    geom_point(alpha = 0.25, size = 1) + geom_smooth(method = "lm", se = TRUE) +
    facet_wrap(~edge_type) + theme_bw() +
    labs(x = "Distance from edge (m)", y = "Weekly mean T (°C)", color = "Year",
         title = "Q3a: Temperature gradient by matrix type"),
  width = 10, height = 5, dpi = 300)

ggsave("output/figures/fig_q3_weekly_RH_by_edge.png",
  ggplot(weekly_means_rh, aes(distance_to_edge_m, weekly_mean_RH, color = year_factor)) +
    geom_point(alpha = 0.25, size = 1) + geom_smooth(method = "lm", se = TRUE) +
    facet_wrap(~edge_type) + theme_bw() +
    labs(x = "Distance from edge (m)", y = "Weekly mean RH (%)", color = "Year",
         title = "Q3b: RH gradient by matrix type"),
  width = 10, height = 5, dpi = 300)

ggsave("output/figures/fig_q3_daily_T_range_by_edge.png",
  ggplot(daily_stats, aes(distance_to_edge_m, daily_range_T, color = year_factor)) +
    geom_point(alpha = 0.15, size = 0.7) + geom_smooth(method = "lm", se = TRUE) +
    facet_wrap(~edge_type) + theme_bw() +
    labs(x = "Distance from edge (m)", y = "Daily T range (°C)", color = "Year",
         title = "Q3c: Daily T range gradient by matrix type"),
  width = 10, height = 5, dpi = 300)

ggsave("output/figures/fig_q3_daily_T_sd_by_edge.png",
  ggplot(daily_stats, aes(distance_to_edge_m, daily_sd_T, color = year_factor)) +
    geom_point(alpha = 0.15, size = 0.7) + geom_smooth(method = "lm", se = TRUE) +
    facet_wrap(~edge_type) + theme_bw() +
    labs(x = "Distance from edge (m)", y = "Daily T SD (°C)", color = "Year",
         title = "Q3d: Daily T SD gradient by matrix type"),
  width = 10, height = 5, dpi = 300)

ggsave("output/figures/fig_q3_daily_RH_range_by_edge.png",
  ggplot(daily_stats_rh, aes(distance_to_edge_m, daily_range_RH, color = year_factor)) +
    geom_point(alpha = 0.15, size = 0.7) + geom_smooth(method = "lm", se = TRUE) +
    facet_wrap(~edge_type) + theme_bw() +
    labs(x = "Distance from edge (m)", y = "Daily RH range (%)", color = "Year",
         title = "Q3e: Daily RH range gradient by matrix type"),
  width = 10, height = 5, dpi = 300)

# ============================================================================
# 4: MODELS  (random effects: (1 | line_unique/Plot_ID))
# ============================================================================
RE_weekly <- "(1 | line_unique/Plot_ID)"
RE_daily  <- "(1 | line_unique/Plot_ID) + (1 | month_factor)"

fit_report <- function(formula_str, data, label) {
  m <- lmer(as.formula(formula_str), data = data, REML = TRUE,
            control = lmerControl(check.conv.singular = .makeCC("ignore", tol = 1e-4)))
  cat("\n----------------------------------------------------------------\n")
  cat(label, "\n  ", formula_str, "\n")
  cat("  singular fit:", isSingular(m), "\n\n")
  print(round(summary(m)$coefficients, 5))
  m
}
# Pull a single named term's p-value safely
pval <- function(m, term) {
  co <- summary(m)$coefficients
  if (term %in% rownames(co)) round(co[term, "Pr(>|t|)"], 4) else NA
}

cat("\n############################ QUESTION 1 ############################\n")
q1_T  <- fit_report(paste("weekly_mean_T  ~ distance_to_edge_m * year_factor +", RE_weekly), weekly_means,    "Q1 Temperature")
q1_RH <- fit_report(paste("weekly_mean_RH ~ distance_to_edge_m * year_factor +", RE_weekly), weekly_means_rh, "Q1 Relative humidity")

cat("\n############################ QUESTION 2 ############################\n")
q2_T_range <- fit_report(paste("daily_range_T ~ distance_to_edge_m * year_factor +", RE_daily), daily_stats, "Q2 Daily T range")
q2_T_sd    <- fit_report(paste("daily_sd_T    ~ distance_to_edge_m * year_factor +", RE_daily), daily_stats, "Q2 Daily T SD")
q2_RH_range<- fit_report(paste("daily_range_RH~ distance_to_edge_m * year_factor +", RE_daily), daily_stats_rh, "Q2 Daily RH range")
q2_RH_sd   <- fit_report(paste("daily_sd_RH   ~ distance_to_edge_m * year_factor +", RE_daily), daily_stats_rh %>% filter(!is.na(daily_sd_RH)), "Q2 Daily RH SD")
q2_RH_cv   <- fit_report(paste("daily_cv_RH   ~ distance_to_edge_m * year_factor +", RE_daily), daily_stats_rh %>% filter(!is.na(daily_cv_RH)), "Q2 Daily RH CV (RH only; T-CV omitted by design)")

cat("\n############################ QUESTION 3 ############################\n")
cat("Three-way: distance_to_edge_m:year_factor2024:edge_typeopen\n")
q3_T       <- fit_report(paste("weekly_mean_T  ~ distance_to_edge_m * year_factor * edge_type +", RE_weekly), weekly_means,    "Q3 Weekly T")
q3_RH      <- fit_report(paste("weekly_mean_RH ~ distance_to_edge_m * year_factor * edge_type +", RE_weekly), weekly_means_rh, "Q3 Weekly RH")
q3_T_range <- fit_report(paste("daily_range_T  ~ distance_to_edge_m * year_factor * edge_type +", RE_daily),  daily_stats,    "Q3 Daily T range  [primary Q3 metric]")
q3_T_sd    <- fit_report(paste("daily_sd_T     ~ distance_to_edge_m * year_factor * edge_type +", RE_daily),  daily_stats,    "Q3 Daily T SD     [primary Q3 metric]")
q3_RH_range<- fit_report(paste("daily_range_RH ~ distance_to_edge_m * year_factor * edge_type +", RE_daily),  daily_stats_rh, "Q3 Daily RH range")


# ============================================================================
# PHASE 5: TABLES, ANOVAS, FIGURES, SUMMARY
# ============================================================================
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

extract_model_table <- function(model, model_name) {
  co <- summary(model)$coefficients
  pvals <- co[,"Pr(>|t|)"]
  tibble(Model = model_name, Term = rownames(co),
         Estimate = round(co[,"Estimate"],4), SE = round(co[,"Std. Error"],4),
         t_value = round(co[,"t value"],3),
         p_value = ifelse(pvals < 0.0001, "<0.0001", as.character(round(pvals, 4))))
}
all_coefs <- bind_rows(
  extract_model_table(q1_T,"Q1_weekly_T"), extract_model_table(q1_RH,"Q1_weekly_RH"), extract_model_table(q2_T_sd,"Q2_daily_T_SD"), extract_model_table(q2_RH_sd,"Q2_daily_RH_SD"),
  extract_model_table(q2_RH_cv,"Q2_daily_RH_CV"),
  extract_model_table(q3_T,"Q3_weekly_T"), extract_model_table(q3_RH,"Q3_weekly_RH"), extract_model_table(q3_T_sd,"Q3_daily_T_SD"))
write.csv(all_coefs, "output/tables/model_coefficients_corrected.csv", row.names = FALSE)


