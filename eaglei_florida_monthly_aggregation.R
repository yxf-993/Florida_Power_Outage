# =============================================================================
# EAGLE-I Florida Power Outage Data: Monthly County-Level Aggregation
# -----------------------------------------------------------------------------
# Description : Aggregates 15-minute EAGLE-I customer-out snapshots into
#               monthly, county-level outage metrics for the State of Florida,
#               2014-2025. The script harmonises schema differences across
#               years, merges in a static Maximum Customer Count (MCC) table,
#               and outputs a single tidy CSV suitable for downstream analysis.
#
# Output columns:
#   fips_code         5-digit county FIPS code (character)
#   county            County name
#   year_month        Calendar month, formatted "YYYY-MM"
#   cust_hours_out    Cumulative customer-hours without power in the month
#                     (sum of customers_out across non-zero snapshots * 0.25 h)
#   max_customers_out Peak number of customers without power in the month
#   n_intervals       Count of non-zero 15-minute snapshots in the month
#   mcc               County Maximum Customer Count (static, from MCC.csv)
#   has_mcc           1 if county is present in the MCC table, NA otherwise
#   max_pct_out       Peak outage fraction (max_customers_out / mcc),
#                     capped at 1 to handle rare overcounts
#
# Inputs (in DATA_DIR):
#   eaglei_outages_<YEAR>.csv  for 2014-2022, 2024, 2025
#   outage_data_2023.csv       for 2023 (different filename convention)
#   MCC.csv                    Maximum Customer Count per county
#
# Output (in DATA_DIR):
#   FL_monthly_2014_2025.csv
#
# Methodological notes:
#   * Each EAGLE-I row represents one 15-minute snapshot for one county.
#   * `customers_out` is a *stock* (customers currently without power), not a
#     flow. Customer-hours are obtained by multiplying by INTERVAL_HR = 0.25.
#   * Snapshots with zero customers out are excluded from the source data, so
#     `n_intervals` counts only non-zero observation windows.
#   * The schema is not stable across years: 2014-2023 use a column named
#     "sum", while 2024-2025 use "customers_out". `load_eaglei_year()`
#     normalises this. The 2024 file additionally carries `total_customers`,
#     which is *not* used here; the static MCC table is preferred as the
#     normalising denominator because it is consistent across years.
#
# Reproducibility:
#   * Run on R >= 4.3 with the package versions printed by sessionInfo() at
#     the end of this script.
#   * A fixed random seed is set even though no stochastic step is currently
#     present, to guard against changes in future revisions.
#
# Author      : <Xiaofeng Ye>
# Created     : 2026-03-23
# Last update : 2026-05-15
#
# Acknowledgement:
#   This script was polished and reorganised for publication readability by
#   Claude (Anthropic). All analytical logic, parameter choices, and scientific
#   interpretation remain the responsibility of the author(s).
# =============================================================================


# ---- 0. Setup --------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table)   # fast I/O and in-memory aggregation
  library(lubridate)    # ISO-8601 timestamp parsing
  library(readr)        # CSV reader used for the small MCC reference table
  library(dplyr)        # tidy reshaping of the MCC table
  library(stringr)      # zero-padding FIPS codes
})

set.seed(42L)


# ---- 1. Configuration ------------------------------------------------------

#' Configurable paths and parameters. Edit these to retarget the script.
DATA_DIR    <- "~/Desktop/36/Data/EAGLE-I Power Outage Data/FL_EAGLE_Power_Outage"
MCC_FILE    <- file.path(DATA_DIR, "MCC.csv")
OUTPUT_FILE <- file.path(DATA_DIR, "FL_monthly_2014_2025.csv")

STATE       <- "Florida"
YEARS       <- 2014:2025
INTERVAL_HR <- 0.25  # length of each EAGLE-I snapshot in hours (15 minutes)


# ---- 2. Helper functions ---------------------------------------------------

#' Locate the EAGLE-I CSV for a given year
#'
#' The filename convention changes for 2023, which is shipped as
#' "outage_data_2023.csv" rather than "eaglei_outages_2023.csv".
#'
#' @param year integer year in 2014-2025.
#' @param data_dir character directory containing the EAGLE-I CSVs.
#' @return character absolute path to the year's CSV.
eaglei_filepath <- function(year, data_dir = DATA_DIR) {
  fname <- if (year == 2023L) {
    "outage_data_2023.csv"
  } else {
    sprintf("eaglei_outages_%d.csv", year)
  }
  file.path(data_dir, fname)
}


#' Load one year of EAGLE-I outage data with a harmonised schema
#'
#' Reads the EAGLE-I CSV for `year` and returns a data.table with a
#' consistent set of columns across years. In particular, the customer-out
#' column is renamed from "sum" to "customers_out" for pre-2024 files, and
#' coerced to numeric with NA replaced by 0.
#'
#' @param year integer year in 2014-2025.
#' @param data_dir character directory containing the EAGLE-I CSVs.
#' @return data.table with columns: fips_code (chr), county (chr),
#'   state (chr), run_start_time (chr), customers_out (num).
load_eaglei_year <- function(year, data_dir = DATA_DIR) {
  fpath <- eaglei_filepath(year, data_dir)

  dt <- fread(
    fpath,
    colClasses = list(
      character = c("fips_code", "county", "state", "run_start_time")
    )
  )

  # Harmonise the customer-out column name (2014-2023: "sum"; 2024-2025: "customers_out")
  if ("sum" %in% names(dt) && !("customers_out" %in% names(dt))) {
    setnames(dt, "sum", "customers_out")
  }

  dt[, customers_out := as.numeric(customers_out)]
  dt[is.na(customers_out), customers_out := 0]

  dt
}


#' Restrict an outage data.table to a single state
#'
#' @param dt data.table with a `state` column.
#' @param state_name character state name (default: \code{STATE}).
#' @return filtered data.table.
filter_state <- function(dt, state_name = STATE) {
  out <- dt[state == state_name]
  message(sprintf("  %s rows: %s",
                  state_name, format(nrow(out), big.mark = ",")))
  out
}


#' Aggregate 15-minute snapshots to monthly county totals
#'
#' Parses `run_start_time` as UTC, derives a "YYYY-MM" key, and computes
#' three monthly summaries per county: cumulative customer-hours without
#' power, peak customers out, and number of non-zero snapshots.
#'
#' @param dt data.table from \code{load_eaglei_year()}.
#' @param interval_hr numeric snapshot length in hours
#'   (default: \code{INTERVAL_HR} = 0.25).
#' @return data.table keyed by (fips_code, county, year_month) with
#'   numeric columns: cust_hours_out, max_customers_out, n_intervals.
aggregate_monthly <- function(dt, interval_hr = INTERVAL_HR) {
  dt <- copy(dt)
  dt[, run_start_time := ymd_hms(run_start_time, tz = "UTC")]
  dt[, year_month     := format(run_start_time, "%Y-%m")]

  dt[, .(
    cust_hours_out    = sum(customers_out) * interval_hr,
    max_customers_out = max(customers_out),
    n_intervals       = .N
  ), by = .(fips_code, county, year_month)]
}


#' Load the Maximum Customer Count (MCC) reference table
#'
#' Aggregates the raw MCC file to the county (FIPS) level, zero-pads FIPS
#' codes to five characters, and drops the "Grand Total" footer row.
#'
#' @param path character path to MCC.csv.
#' @return tibble with columns: fips_code (chr, 5-digit), mcc (num), has_mcc (int).
load_mcc <- function(path = MCC_FILE) {
  read_csv(path, show_col_types = FALSE) |>
    rename(fips_code = County_FIPS) |>
    group_by(fips_code) |>
    summarise(mcc = sum(Customers, na.rm = TRUE), .groups = "drop") |>
    mutate(fips_code = str_pad(as.character(fips_code),
                               width = 5, pad = "0", side = "left")) |>
    filter(fips_code != "Grand Total") |>
    mutate(has_mcc = 1L)
}


#' Add the peak outage fraction relative to county MCC
#'
#' Computes `max_pct_out = max_customers_out / mcc` and caps the result at 1
#' to handle (rare) cases where a snapshot exceeds the static MCC.
#'
#' @param dt monthly data.table containing \code{max_customers_out} and \code{mcc}.
#' @return the same data.table, modified in place, with \code{max_pct_out} added.
add_outage_fraction <- function(dt) {
  dt[, mcc := as.numeric(mcc)]
  dt[, max_pct_out := max_customers_out / mcc]
  dt[max_pct_out > 1, max_pct_out := 1]
  dt[]
}


# ---- 3. Main pipeline ------------------------------------------------------

message("Loading MCC reference table...")
mcc <- load_mcc(MCC_FILE)

message("Processing yearly EAGLE-I files...")
monthly_list <- vector("list", length(YEARS))
names(monthly_list) <- as.character(YEARS)

for (yr in YEARS) {
  message(sprintf("Year %d:", yr))
  dt_raw <- load_eaglei_year(yr)
  dt_fl  <- filter_state(dt_raw, STATE)
  monthly_list[[as.character(yr)]] <- aggregate_monthly(dt_fl)

  # Release the large yearly file before reading the next one
  rm(dt_raw, dt_fl)
  invisible(gc(verbose = FALSE))
}


# ---- 4. Combine years and merge with MCC -----------------------------------

message("Combining years and merging with MCC...")
fl_all <- rbindlist(monthly_list, use.names = TRUE, fill = TRUE)
fl_all <- merge(fl_all, mcc, by = "fips_code", all.x = TRUE)
fl_all <- add_outage_fraction(fl_all)
setorder(fl_all, fips_code, year_month)


# ---- 5. Diagnostics and save -----------------------------------------------

message(sprintf(
  "Final dataset: %s rows | %d counties | %d months",
  format(nrow(fl_all), big.mark = ","),
  uniqueN(fl_all$fips_code),
  uniqueN(fl_all$year_month)
))
message("Columns: ", paste(names(fl_all), collapse = ", "))

fwrite(fl_all, OUTPUT_FILE)
message("Saved to: ", OUTPUT_FILE)


# ---- 6. Reproducibility footer ---------------------------------------------
# Printed to the console so it can be captured in a log file for the
# methods/supplementary materials of the manuscript.

sessionInfo()
