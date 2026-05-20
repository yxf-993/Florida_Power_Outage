# =============================================================================
# Florida Data Centers x Hurricane Power Outages: Event-Level Panels
# -----------------------------------------------------------------------------
# Description : Builds a 2017-2025 county x year data-center (DC) stock panel
#               and merges it onto two storm-event datasets:
#                 (1) maximum power outage per hurricane x county, and
#                 (2) outage duration per hurricane x county.
#               The result is two event-level panels that pair each
#               hurricane's outage outcome in a county with the DC stock in
#               place that year.
#
# Inputs:
#   * DC_FILE            Florida_DataCenter_CapacityArea_with_county.xlsx
#   * MAX_OUTAGE_FILE    Data_maximum_power_outage_12-10-2025.csv
#   * DURATION_FILE      Data_outage_duration_12-10-2025.csv
#
# Outputs (the two outcome datasets):
#   * FL_hurricane_DC_panel.csv            max-outage events x DC stock
#   * FL_hurricane_duration_DC_panel.csv   outage-duration events x DC stock
#   (FL_DC_panel_2017_2025.csv is also written as an intermediate.)
#
# Key methodological notes:
#   * The DC panel window starts in 2017; DCs that began operating before
#     2017 are captured in a pre-2017 baseline so the stock series reflects
#     all operating DCs, not only post-2017 entries.
#   * `entry_year` prefers `renovated`, then falls back to `Built`; the
#     source is recorded in `year_source`. DCs with neither are excluded
#     from the stock series and counted in `n_unknown`.
#   * Excluded inventory rows are *planned* (not-yet-operational) DCs.
#   * Hurricane keys are matched to the storm reference tables on the event
#     code (e.g. "Irma_17"). The two outcome files use different case
#     conventions in their `hurricane` column, which the respective merges
#     preserve. The duration dataset omits Isaias (2020).
#   * Counties absent from the DC panel receive zeros for all DC columns.
#
# Author      : <Xiaofeng Ye>
# Created     : 2026-04-05
# Last update : 2026-05-20
#
# Acknowledgement:
#   This script was polished and reorganised for publication readability by
#   Claude (Anthropic). All analytical logic, parameter choices, and scientific
#   interpretation remain the responsibility of the author(s).
# =============================================================================


# ---- 0. Setup --------------------------------------------------------------

suppressPackageStartupMessages({
  library(readxl)       # .xlsx ingestion (DC inventory)
  library(data.table)   # fast aggregation and joins
  library(stringr)      # county-name standardisation
})

set.seed(42L)


# ---- 1. Configuration ------------------------------------------------------

#' Configurable paths and parameters. Edit these to retarget the script.
DC_FILE         <- "~/Desktop/36/FL/data/Florida_DataCenter_CapacityArea_with_county.xlsx"
MAX_OUTAGE_FILE <- "~/Desktop/36/FL/3. Data - Copy/Data_maximum_power_outage_12-10-2025.csv"
DURATION_FILE   <- "~/Desktop/36/FL/3. Data - Copy/Data_outage_duration_12-10-2025.csv"

DC_PANEL_OUT    <- "FL_DC_panel_2017_2025.csv"
MAX_OUT         <- "FL_hurricane_DC_panel.csv"
DURATION_OUT    <- "FL_hurricane_duration_DC_panel.csv"

YEARS           <- 2017:2025

# Rows of the DC inventory to exclude (1-indexed positions in the source
# spreadsheet): planned data centers that are not yet operational.
DC_ROWS_TO_DROP <- c(16, 39, 44, 93, 110)

# DC-panel columns merged onto each event dataset.
DC_MERGE_COLS <- c("county_clean", "year", "n_new", "stock_total",
                   "stock_capacity_mw", "stock_area_sqft",
                   "pre2017_stock", "n_unknown",
                   "cum_capacity_mw", "cum_area_sqft")


# ---- 2. Helper functions ---------------------------------------------------

#' Standardise a county name to an upper-case join key
#'
#' Strips a trailing " County" and surrounding whitespace, then upper-cases.
#'
#' @param x character vector of county names.
#' @return character vector of normalised county keys.
to_county_key <- function(x) {
  str_to_upper(str_trim(str_remove(x, " County$")))
}


#' Replace NA values with zero in selected data.table columns, in place
#'
#' @param dt data.table.
#' @param cols character vector of column names; missing columns are skipped.
#' @return the data.table, modified in place (returned invisibly).
fill_na_with_zero <- function(dt, cols) {
  for (col in intersect(cols, names(dt))) {
    set(dt, i = which(is.na(dt[[col]])), j = col, value = 0)
  }
  invisible(dt)
}


#' Add integer event-time and first-of-month event-date columns
#'
#' @param dt data.table with integer `year` and `month` columns.
#' @return the data.table with `event_time` (year*12 + month) and
#'   `event_date` (Date, first of month) added in place.
add_event_time <- function(dt) {
  dt[, event_time := year * 12L + month]
  dt[, event_date := as.Date(sprintf("%d-%02d-01", year, month))]
  dt[]
}


#' Load and clean the data-center inventory
#'
#' @param path character path to the inventory `.xlsx`.
#' @param rows_to_drop integer 1-indexed rows to exclude (planned DCs).
#' @return data.table with original columns plus `entry_year` (int),
#'   `year_source` (chr), and `county_clean` (chr).
load_datacenters <- function(path = DC_FILE, rows_to_drop = DC_ROWS_TO_DROP) {
  DC <- read_excel(path)
  if (length(rows_to_drop) > 0L) DC <- DC[-rows_to_drop, ]
  DC <- as.data.table(DC)

  DC[, entry_year := fifelse(
    !is.na(renovated), as.integer(renovated),
    fifelse(!is.na(Built), as.integer(Built), NA_integer_)
  )]
  DC[, year_source := fifelse(
    !is.na(renovated), "renovated",
    fifelse(!is.na(Built), "built", "unknown")
  )]
  DC[, county_clean := to_county_key(County)]
  DC
}


#' Build the county x year DC panel with cumulative stocks and a baseline
#'
#' Baseline columns are named after the panel's first year (e.g.
#' `pre2017_stock` when `years` starts in 2017), so `stock_total` reflects
#' all DCs operating in a given year, including pre-window entries. DCs
#' with a missing `entry_year` are summarised in `n_unknown` and excluded
#' from the stock series.
#'
#' @param dc data.table from `load_datacenters()`.
#' @param years integer vector of years to expand over.
#' @return data.table keyed by (county_clean, year) with annual flows,
#'   cumulative stocks, pre-window baseline, `n_unknown`, and the combined
#'   `stock_total` / `stock_capacity_mw` / `stock_area_sqft`.
build_dc_panel <- function(dc, years = YEARS) {
  yr_min <- min(years); yr_max <- max(years)

  base_stock <- sprintf("pre%d_stock",       yr_min)
  base_cap   <- sprintf("pre%d_capacity_mw", yr_min)
  base_area  <- sprintf("pre%d_area_sqft",   yr_min)

  panel <- CJ(county_clean = unique(dc$county_clean), year = years)

  dc_new <- dc[!is.na(entry_year) & entry_year >= yr_min & entry_year <= yr_max,
               .(n_new           = .N,
                 new_capacity_mw = sum(capacity_mw,        na.rm = TRUE),
                 new_area_sqft   = sum(occupied_area_sqft, na.rm = TRUE)),
               by = .(county_clean, year = as.integer(entry_year))]
  panel <- merge(panel, dc_new, by = c("county_clean", "year"), all.x = TRUE)
  fill_na_with_zero(panel, c("n_new", "new_capacity_mw", "new_area_sqft"))

  setorder(panel, county_clean, year)
  panel[, stock           := cumsum(n_new),           by = county_clean]
  panel[, cum_capacity_mw := cumsum(new_capacity_mw), by = county_clean]
  panel[, cum_area_sqft   := cumsum(new_area_sqft),   by = county_clean]

  dc_pre <- dc[!is.na(entry_year) & entry_year < yr_min,
               .(pre_stock = .N,
                 pre_cap   = sum(capacity_mw,        na.rm = TRUE),
                 pre_area  = sum(occupied_area_sqft, na.rm = TRUE)),
               by = county_clean]
  setnames(dc_pre, c("pre_stock", "pre_cap", "pre_area"),
           c(base_stock, base_cap, base_area))

  dc_unknown <- dc[is.na(entry_year), .(n_unknown = .N), by = county_clean]

  panel <- merge(panel, dc_pre,     by = "county_clean", all.x = TRUE)
  panel <- merge(panel, dc_unknown, by = "county_clean", all.x = TRUE)
  fill_na_with_zero(panel, c(base_stock, base_cap, base_area, "n_unknown"))

  panel[, stock_total       := get(base_stock) + stock]
  panel[, stock_capacity_mw := get(base_cap)   + cum_capacity_mw]
  panel[, stock_area_sqft   := get(base_area)  + cum_area_sqft]

  panel[]
}


#' Merge DC-panel columns onto an event dataset, zero-fill, and sort
#'
#' @param df event data.table containing `county_clean` and `year`.
#' @param dc_panel data.table from `build_dc_panel()`.
#' @param dc_cols character vector of panel columns to merge (must include
#'   the join keys `county_clean` and `year`).
#' @return merged data.table sorted by (county_clean, event_time).
merge_dc_panel <- function(df, dc_panel, dc_cols = DC_MERGE_COLS) {
  out <- merge(df, dc_panel[, ..dc_cols],
               by = c("county_clean", "year"), all.x = TRUE)
  fill_na_with_zero(out, setdiff(dc_cols, c("county_clean", "year")))
  setorder(out, county_clean, event_time)
  out
}


# ---- 3. Hurricane event reference tables -----------------------------------

#' Reference table for the maximum-outage dataset (17 events; mixed-case keys)
#'
#' @return data.table with hurricane, year, month, type, event_time, event_date.
hurricane_table_max <- function() {
  ht <- data.table(
    hurricane = c("Fred_21","Ian_22","Michael_18","Irma_17","Isaias_20","Alberto_18",
                  "Debby_24","Dorian_19","Elsa_21","Eta_20","Helene_24","Idalia_23",
                  "Milton_24","Sally_20","Nicole_22","MWE_24","JWE_24"),
    year      = c(2021,2022,2018,2017,2020,2018,
                  2024,2019,2021,2020,2024,2023,
                  2024,2020,2022,2024,2024),
    month     = c(8,9,10,9,8,5,
                  8,9,7,11,9,8,
                  10,9,11,1,5),
    type      = c("hurricane","hurricane","hurricane","hurricane","hurricane","hurricane",
                  "hurricane","hurricane","hurricane","hurricane","hurricane","hurricane",
                  "hurricane","hurricane","hurricane","storm","storm")
  )
  add_event_time(ht)
}


#' Reference table for the duration dataset (16 events; upper-case keys, no Isaias)
#'
#' @return data.table with hurricane, year, month, type, event_time, event_date.
hurricane_table_duration <- function() {
  ht <- data.table(
    hurricane = c("FRED_21","IAN_22","MICHAEL_18","IRMA_17","ALBERTO_18",
                  "DEBBY_24","DORIAN_19","ELSA_21","ETA_20","HELENE_24",
                  "IDALIA_23","MILTON_24","SALLY_20","NICOLE_22",
                  "MWE_24","JWE_24"),
    year      = c(2021,2022,2018,2017,2018,
                  2024,2019,2021,2020,2024,
                  2023,2024,2020,2022,
                  2024,2024),
    month     = c(8,9,10,9,5,
                  8,9,7,11,9,
                  8,10,9,11,
                  1,5),
    type      = c("hurricane","hurricane","hurricane","hurricane","hurricane",
                  "hurricane","hurricane","hurricane","hurricane","hurricane",
                  "hurricane","hurricane","hurricane","hurricane",
                  "storm","storm")
  )
  add_event_time(ht)
}


# ---- 4. Main pipeline ------------------------------------------------------

# 4a. Data-center stock panel (2017-2025)
message("Building 2017-2025 DC stock panel...")
DC       <- load_datacenters(DC_FILE, DC_ROWS_TO_DROP)
dc_panel <- build_dc_panel(DC, YEARS)
fwrite(dc_panel, DC_PANEL_OUT)

meta_cols <- c("hurricane", "year", "month", "type", "event_time", "event_date")

# 4b. Maximum-outage events x DC stock
message("Merging DC stock onto maximum-outage events...")
df_out <- fread(MAX_OUTAGE_FILE)
df_out <- merge(df_out, hurricane_table_max()[, ..meta_cols],
                by = "hurricane", all.x = TRUE)
df_out[, county_clean := to_county_key(county)]
merged_max <- merge_dc_panel(df_out, dc_panel)
fwrite(merged_max, MAX_OUT)

# 4c. Outage-duration events x DC stock
message("Merging DC stock onto outage-duration events...")
df_dur <- fread(DURATION_FILE)
df_dur[, hurricane := toupper(trimws(hurricane))]  # duration file uses upper-case keys
df_dur <- merge(df_dur, hurricane_table_duration()[, ..meta_cols],
                by = "hurricane", all.x = TRUE)
df_dur[, county_clean := to_county_key(county)]
merged_dur <- merge_dc_panel(df_dur, dc_panel)
fwrite(merged_dur, DURATION_OUT)

message(sprintf("DC panel saved to:        %s", DC_PANEL_OUT))
message(sprintf("Max-outage panel saved to: %s", MAX_OUT))
message(sprintf("Duration panel saved to:   %s", DURATION_OUT))


# ---- 5. Reproducibility footer ---------------------------------------------

sessionInfo()
