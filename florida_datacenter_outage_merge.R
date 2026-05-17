# =============================================================================
# Florida Data Centers x Power Outages: County-Level Panel Construction
# -----------------------------------------------------------------------------
# Description : Builds a balanced county x year panel of Florida data-center
#               (DC) stock and merges it with the monthly EAGLE-I power-outage
#               aggregates produced by `eaglei_florida_monthly_aggregation.R`.
#               The output is a county x month panel that combines outage
#               burden and cumulative DC presence, suitable for downstream
#               panel-regression or event-study analyses.
#
# Pipeline:
#   1. Load and de-duplicate the data-center inventory.
#   2. Derive an `entry_year` for each DC using a renovated-then-built
#      fallback rule.
#   3. Expand to a county x year (2014-2025) panel with annual flows
#      (`n_new`, `new_capacity_mw`, `new_area_sqft`) and cumulative stocks
#      (counts, capacity in MW, occupied area in sq ft), incorporating a
#      pre-2014 baseline so the stock represents *all* DCs in operation,
#      not only post-2014 entries.
#   4. Merge the county x year DC panel onto the county x month outage
#      panel; counties with no DCs receive zeros.
#
# Inputs:
#   * DC_FILE         Florida_DataCenter_CapacityArea_with_county.xlsx
#                     (one row per data center; columns include `County`,
#                     `Built`, `renovated`, `capacity_mw`, `occupied_area_sqft`)
#   * OUTAGE_FILE     FL_monthly_2014_2025.csv produced upstream
#
# Outputs:
#   * FL_DC_panel_2014_2025.csv         county x year DC panel
#   * FL_monthly_DC_2014_2025.csv       county x month merged panel
#
# Key methodological notes:
#   * `entry_year` prefers the most recent `renovated` year when present,
#     and otherwise falls back to `Built`. The chosen source is recorded
#     in `year_source` for auditing.
#   * DCs with neither `renovated` nor `Built` (`year_source == "unknown"`)
#     are tracked separately in `n_unknown` and are *not* included in the
#     cumulative stock series. Robustness checks should re-run the analysis
#     with these reassigned to the earliest possible year (e.g., 2014) to
#     test sensitivity.
#   * County names are standardised by stripping the trailing " County"
#     and upper-casing, so that the DC inventory's `County` field aligns
#     with the EAGLE-I `county` field.
#   * The DC panel skeleton uses only counties with at least one DC; the
#     final left-merge onto outage data fills unmatched counties with
#     zeros, yielding the same result as a 67-county skeleton at lower
#     memory cost.
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
  library(readxl)       # .xlsx ingestion (DC inventory)
  library(data.table)   # fast aggregation and joins
  library(stringr)      # county-name standardisation
})

set.seed(42L)


# ---- 1. Configuration ------------------------------------------------------

#' Configurable paths and parameters. Edit these to retarget the script.
DC_FILE       <- "~/Desktop/36/FL/data/Florida_DataCenter_CapacityArea_with_county.xlsx"
OUTAGE_FILE   <- "~/Desktop/36/Data/EAGLE-I Power Outage Data/FL_EAGLE_Power_Outage/FL_monthly_2014_2025.csv"
DC_PANEL_OUT  <- "FL_DC_panel_2014_2025.csv"
MERGED_OUT    <- "FL_monthly_DC_2014_2025.csv"

YEARS         <- 2014:2025

# Rows of the DC inventory to exclude (1-indexed positions in the source
# spreadsheet). These correspond to *planned* data centers that are not yet
# operational and therefore should not contribute to the stock series.
DC_ROWS_TO_DROP <- c(16, 39, 44, 93, 110)


# ---- 2. Helper functions ---------------------------------------------------

#' Standardise a Florida county name to an upper-case join key
#'
#' Strips a trailing " County" (case-sensitive) and any surrounding
#' whitespace, then upper-cases. Matches the convention used by EAGLE-I.
#'
#' @param x character vector of county names.
#' @return character vector of normalised county keys.
to_county_key <- function(x) {
  str_to_upper(str_trim(str_remove(x, " County$")))
}


#' Replace NA values with zero in selected data.table columns, in place
#'
#' @param dt data.table.
#' @param cols character vector of column names. Missing columns are skipped.
#' @return the data.table, modified in place (returned invisibly).
fill_na_with_zero <- function(dt, cols) {
  for (col in intersect(cols, names(dt))) {
    set(dt, i = which(is.na(dt[[col]])), j = col, value = 0)
  }
  invisible(dt)
}


#' Load and clean the data-center inventory
#'
#' Reads the inventory spreadsheet, drops planned (not-yet-operational)
#' data-center rows, derives
#' `entry_year` using a renovated-then-built fallback, records the source
#' of each year in `year_source`, and adds a standardised `county_clean`
#' join key.
#'
#' @param path character path to the inventory `.xlsx` file.
#' @param rows_to_drop integer 1-indexed row positions to exclude.
#' @return data.table with all original columns plus `entry_year` (int),
#'   `year_source` (chr in {"renovated","built","unknown"}), and
#'   `county_clean` (chr).
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


#' Build the balanced county x year DC panel with cumulative stocks
#'
#' For each DC-bearing county and each year in `years`, computes annual
#' flows (count, MW capacity, occupied area) and cumulative stocks since
#' 2014, then adds a pre-2014 baseline so that `stock_total` reflects all
#' DCs operating in that year, including those installed earlier. DCs
#' with `entry_year == NA` are summarised separately in `n_unknown` and
#' excluded from the stock series.
#'
#' @param dc data.table returned by `load_datacenters()`.
#' @param years integer vector of years to expand over.
#' @return data.table keyed by (county_clean, year) with columns:
#'   n_new, new_capacity_mw, new_area_sqft,                # annual flows
#'   stock, cum_capacity_mw, cum_area_sqft,                # post-2014 cumsum
#'   pre2014_stock, pre2014_capacity_mw, pre2014_area_sqft,# baseline
#'   n_unknown,                                            # entry-year missing
#'   stock_total, stock_capacity_mw, stock_area_sqft       # baseline + cumsum
build_dc_panel <- function(dc, years = YEARS) {
  yr_min <- min(years); yr_max <- max(years)

  panel <- CJ(county_clean = unique(dc$county_clean), year = years)

  # Annual flows for entries within the panel window
  dc_new <- dc[!is.na(entry_year) & entry_year >= yr_min & entry_year <= yr_max,
               .(n_new           = .N,
                 new_capacity_mw = sum(capacity_mw,        na.rm = TRUE),
                 new_area_sqft   = sum(occupied_area_sqft, na.rm = TRUE)),
               by = .(county_clean, year = as.integer(entry_year))]

  panel <- merge(panel, dc_new, by = c("county_clean", "year"), all.x = TRUE)
  fill_na_with_zero(panel, c("n_new", "new_capacity_mw", "new_area_sqft"))

  # Cumulative stock from `yr_min` onward
  setorder(panel, county_clean, year)
  panel[, stock           := cumsum(n_new),           by = county_clean]
  panel[, cum_capacity_mw := cumsum(new_capacity_mw), by = county_clean]
  panel[, cum_area_sqft   := cumsum(new_area_sqft),   by = county_clean]

  # Pre-window baseline (DCs that already existed before `yr_min`)
  dc_pre <- dc[!is.na(entry_year) & entry_year < yr_min,
               .(pre2014_stock       = .N,
                 pre2014_capacity_mw = sum(capacity_mw,        na.rm = TRUE),
                 pre2014_area_sqft   = sum(occupied_area_sqft, na.rm = TRUE)),
               by = county_clean]

  # Counties with DCs of unknown entry year
  dc_unknown <- dc[is.na(entry_year), .(n_unknown = .N), by = county_clean]

  panel <- merge(panel, dc_pre,     by = "county_clean", all.x = TRUE)
  panel <- merge(panel, dc_unknown, by = "county_clean", all.x = TRUE)
  fill_na_with_zero(panel, c("pre2014_stock", "pre2014_capacity_mw",
                             "pre2014_area_sqft", "n_unknown"))

  # Total operating stock = baseline + post-2014 cumulative
  panel[, stock_total       := pre2014_stock       + stock]
  panel[, stock_capacity_mw := pre2014_capacity_mw + cum_capacity_mw]
  panel[, stock_area_sqft   := pre2014_area_sqft   + cum_area_sqft]

  panel[]
}


#' Load the monthly EAGLE-I outage panel and add a `year` and county key
#'
#' @param path character path to FL_monthly_2014_2025.csv.
#' @return data.table with original columns plus `year` (int) and
#'   `county_clean` (chr).
load_outages <- function(path = OUTAGE_FILE) {
  dt <- fread(path, colClasses = list(character = "fips_code"))
  dt[, year         := as.integer(substr(year_month, 1, 4))]
  dt[, county_clean := to_county_key(county)]
  dt[]
}


#' Left-merge the DC panel onto the monthly outage panel and zero-fill
#'
#' Counties absent from the DC panel (no data centers in the inventory)
#' receive zeros for all DC-derived columns.
#'
#' @param outages data.table from `load_outages()`.
#' @param panel data.table from `build_dc_panel()`.
#' @return merged data.table sorted by (fips_code, year_month).
merge_dc_into_outages <- function(outages, panel) {
  keep_cols <- c("county_clean", "year", "n_new", "stock",
                 "stock_total", "pre2014_stock", "n_unknown",
                 "stock_capacity_mw", "stock_area_sqft",
                 "cum_capacity_mw", "cum_area_sqft")

  merged <- merge(outages,
                  panel[, ..keep_cols],
                  by = c("county_clean", "year"),
                  all.x = TRUE)

  zero_cols <- c("stock_total", "stock", "n_new", "pre2014_stock", "n_unknown",
                 "stock_capacity_mw", "stock_area_sqft",
                 "cum_capacity_mw", "cum_area_sqft")
  fill_na_with_zero(merged, zero_cols)

  setorder(merged, fips_code, year_month)
  merged
}


# ---- 3. Main pipeline ------------------------------------------------------

message("Loading data-center inventory...")
DC <- load_datacenters(DC_FILE, DC_ROWS_TO_DROP)

message("Building county x year DC panel...")
dc_panel <- build_dc_panel(DC, YEARS)
fwrite(dc_panel, DC_PANEL_OUT)

message("Loading monthly outage panel...")
fl_monthly <- load_outages(OUTAGE_FILE)

message("Merging DC panel into monthly outage data...")
fl_dc_monthly <- merge_dc_into_outages(fl_monthly, dc_panel)
fwrite(fl_dc_monthly, MERGED_OUT)

message(sprintf("DC panel saved to:     %s", DC_PANEL_OUT))
message(sprintf("Merged panel saved to: %s", MERGED_OUT))


# ---- 4. Reproducibility footer ---------------------------------------------

sessionInfo()
