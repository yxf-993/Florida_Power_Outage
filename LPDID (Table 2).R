# ==============================================================================
# LP-DID — Excluding Hurricane / Storm Months (Robustness Check)
# Method: Dube et al. (2023) first-difference stack on the monthly panel.
# Goal: Test if ATT survives after excluding extreme weather months.
# ==============================================================================

library(fixest)
library(dplyr)
library(tidyr)

# 1. Define Storm/Hurricane Months to Exclude ----------------------------------
hurr_info <- data.frame(
  hurricane = c("FRED_21","IAN_22","MICHAEL_18","IRMA_17","ALBERTO_18",
                "DEBBY_24","DORIAN_19","ELSA_21","ETA_20","HELENE_24",
                "IDALIA_23","MILTON_24","SALLY_20","NICOLE_22",
                "MWE_24","JWE_24"),
  year  = c(2021,2022,2018,2017,2018,2024,2019,2021,2020,2024,2023,2024,2020,2022,2024,2024),
  month = c(8,9,10,9,5,8,9,7,11,9,8,10,9,11,1,5),
  stringsAsFactors = FALSE
)

excl_ym <- hurr_info |>
  distinct(year, month) |>
  mutate(year_month = sprintf("%d-%02d", year, month)) |>
  pull(year_month)

# 2. Load Data (Paths updated to relative paths for replication privacy) -------
outage <- read.csv("/Users/yexiaofeng/Desktop/36/Data/FL_monthly_DC_2014_2025.csv",
                   stringsAsFactors = FALSE)
dc_ann <- read.csv("/Users/yexiaofeng/Desktop/36/FL/data/FL_DC_panel_2014_2025.csv",
                   stringsAsFactors = FALSE)
always_set <- unique(dc_ann$county_clean[dc_ann$pre2014_stock > 0])

first_dc <- dc_ann |>
  filter(n_new > 0) |>
  group_by(county_clean) |>
  summarise(gvar_year = min(year), .groups = "drop")

ym_sorted <- sort(unique(outage$year_month))
ym_index  <- setNames(seq_along(ym_sorted), ym_sorted)

# 3. Panel Construction Function -----------------------------------------------
build_base <- function(panel) {
  panel |>
    mutate(
      log_cust_hours = log(cust_hours_out + 1),
      always_treated = as.integer(county_clean %in% always_set),
      county_id      = as.integer(factor(county_clean)),
      time           = ym_index[year_month],
      ym             = factor(year_month)
    ) |>
    left_join(first_dc, by = "county_clean") |>
    mutate(gvar_year = replace(gvar_year, is.na(gvar_year), 0L)) |>
    filter(always_treated == 0) |>
    mutate(
      # Onset: January of the year following the first DC arrival
      # If first DC is >= 2025, treat as never-treated (outside sample period)
      treat_year = ifelse(gvar_year == 0 | gvar_year >= 2025, 0L, gvar_year + 1L),
      gvar_month = ifelse(treat_year == 0, 0L, ym_index[sprintf("%d-01", treat_year)]),
      D          = as.integer(gvar_month > 0 & time >= gvar_month)
    )
}

df_full  <- build_base(outage)
df_clean <- build_base(outage |> filter(!year_month %in% excl_ym))

# 4. LP-DID Stack Construction Function ----------------------------------------
PRE_WINDOW  <- 24L
POST_WINDOW <- 60L

build_lpdid_stack <- function(df) {
  cohorts      <- sort(unique(df$gvar_month[df$gvar_month > 0]))
  never_treated <- df |> filter(gvar_month == 0)
  
  lapply(cohorts, function(g) {
    ref_t <- g - 1L
    t_min <- g - PRE_WINDOW
    t_max <- g + POST_WINDOW
    
    # Baseline outcome values at reference month (g - 1)
    baselines <- df |>
      filter(time == ref_t) |>
      select(county_id, y_base_ch = log_cust_hours, y_base_mp = max_pct_out)
    
    bind_rows(df |> filter(gvar_month == g), never_treated) |>
      filter(time >= t_min, time <= t_max) |>
      left_join(baselines, by = "county_id") |>
      filter(!is.na(y_base_ch)) |> 
      mutate(
        delta_ch  = log_cust_hours - y_base_ch,
        delta_mp  = max_pct_out    - y_base_mp,
        D         = as.integer(gvar_month == g & time >= g),
        cohort    = g,
        c_time    = paste0(g, "_", time),
        month_num = as.integer(sub(".*-", "", year_month))
      )
  }) |> bind_rows()
}

stack_full  <- build_lpdid_stack(df_full)
stack_clean <- build_lpdid_stack(df_clean)

# 5. Model Estimations ---------------------------------------------------------
# Baseline Specification: Fixed effects at cohort-by-month level
fit_ch_full  <- feols(delta_ch ~ D | c_time, stack_full,  cluster = ~county_id)
fit_ch_clean <- feols(delta_ch ~ D | c_time, stack_clean, cluster = ~county_id)
fit_mp_full  <- feols(delta_mp ~ D | c_time, stack_full,  cluster = ~county_id)
fit_mp_clean <- feols(delta_mp ~ D | c_time, stack_clean, cluster = ~county_id)

# Alternative Specification: Adding month-of-year FE 
# (Note: May be redundant/collinear with c_time in monthly panel setup)
fit_ch_full_m  <- feols(delta_ch ~ D | c_time + factor(month_num), stack_full,  cluster = ~county_id)
fit_ch_clean_m <- feols(delta_ch ~ D | c_time + factor(month_num), stack_clean, cluster = ~county_id)
fit_mp_full_m  <- feols(delta_mp ~ D | c_time + factor(month_num), stack_full,  cluster = ~county_id)
fit_mp_clean_m <- feols(delta_mp ~ D | c_time + factor(month_num), stack_clean, cluster = ~county_id)

# 6. Display Clean Summary Results ---------------------------------------------
print_row <- function(label, fit) {
  b  <- coef(fit)["D"]
  s  <- se(fit)["D"]
  pv <- pvalue(fit)["D"]
  ci <- confint(fit, level = 0.95)["D", ]
  st <- if (pv < 0.01) "***" else if (pv < 0.05) "**" else if (pv < 0.1) "*" else ""
  cat(sprintf("  %-55s ATT=%7.4f%s (SE=%.4f) [%.4f, %.4f] N=%d\n",
              label, b, st, s, ci[1], ci[2], nobs(fit)))
}

DIV <- strrep("=", 85)
cat("\n", DIV, "\n", "LP-DID Robustness Check: Overall ATT Estimates\n", DIV, "\n", sep = "")

cat("\n[Outcome: Delta log(cust_hours_out + 1)]\n")
print_row("Baseline - Full Panel (with storm months)", fit_ch_full)
print_row("Baseline - Clean Panel (excl. storm months)", fit_ch_clean)
print_row("Sensitivity - Full Panel + Month-of-Year FE", fit_ch_full_m)
print_row("Sensitivity - Clean Panel + Month-of-Year FE", fit_ch_clean_m)

cat("\n[Outcome: Delta max_pct_out]\n")
print_row("Baseline - Full Panel (with storm months)", fit_mp_full)
print_row("Baseline - Clean Panel (excl. storm months)", fit_mp_clean)
print_row("Sensitivity - Full Panel + Month-of-Year FE", fit_mp_full_m)
print_row("Sensitivity - Clean Panel + Month-of-Year FE", fit_mp_clean_m)
cat(DIV, "\n")