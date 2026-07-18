# TWFE Model 1 — Monthly Panel (no aggregation)
# Outcomes  : log_cust_hours := log(cust_hours_out + 1)   [monthly]
#             max_pct_out                                  [monthly]
# Treatment : log_stock    := log(stock_total + 1)
#             log_capacity := log(stock_capacity_mw + 1)
# FE        : county + year-month, SE clustered by county
# N         : 8,891 county-month obs

library(fixest)
library(dplyr)

# ── 1. Load data ──────────────────────────────────────────────────────────────
outage <- read.csv("/Users/yexiaofeng/Desktop/36/Data/FL_monthly_DC_2014_2025.csv",
                   stringsAsFactors = FALSE)

dc_ann <- read.csv("/Users/yexiaofeng/Desktop/36/FL/data/FL_DC_panel_2014_2025.csv",
                   stringsAsFactors = FALSE)

# ── 2. Merge annual DC attributes onto monthly outage rows ────────────────────
# outage already has stock_total, pre2014_stock; pull capacity & area from dc_ann
dc_attr <- dc_ann |>
  select(county_clean, year, stock_capacity_mw, stock_area_sqft)

df <- left_join(outage, dc_attr, by = c("county_clean", "year"))

df <- df |>
  mutate(
    stock_total       = replace(stock_total,       is.na(stock_total),       0),
    stock_capacity_mw = replace(stock_capacity_mw, is.na(stock_capacity_mw), 0),
    pre2014_stock     = replace(pre2014_stock,      is.na(pre2014_stock),     0)
  )

# ── 3. Construct variables ────────────────────────────────────────────────────
df <- df |>
  mutate(
    log_cust_hours = log(cust_hours_out + 1),
    log_stock      = log(stock_total       + 1),
    log_capacity   = log(stock_capacity_mw + 1),
    county_id      = as.integer(factor(county_clean)),
    ym             = factor(year_month)   # year-month FE
  )

cat("Monthly panel:", nrow(df), "obs |",
    n_distinct(df$county_clean), "counties |",
    n_distinct(df$year_month), "year-months\n\n")

# ── 4. TWFE specifications ─────────────────────────────────────────────────────
# county FE (county_id) + year-month FE (ym), SE clustered by county

fits <- list(
  "(1) log_stock → log_cust_hours" =
    feols(log_cust_hours ~ log_stock    | county_id + ym, df, cluster = ~county_id),
  "(2) log_capacity → log_cust_hours" =
    feols(log_cust_hours ~ log_capacity | county_id + ym, df, cluster = ~county_id),
  "(3) log_stock → max_pct_out" =
    feols(max_pct_out    ~ log_stock    | county_id + ym, df, cluster = ~county_id),
  "(4) log_capacity → max_pct_out" =
    feols(max_pct_out    ~ log_capacity | county_id + ym, df, cluster = ~county_id)
)

# ── 5. Print each model ───────────────────────────────────────────────────────
for (nm in names(fits)) {
  fit <- fits[[nm]]
  cat("──", nm, "──\n")
  print(coeftable(fit))
  cat(sprintf("  N=%d  R²=%.3f  R²-within=%.4f\n\n",
              nobs(fit), r2(fit, "r2"), r2(fit, "wr2")))
}

# ── 6. Joint display table ────────────────────────────────────────────────────
cat(strrep("=", 72), "\n", sep = "")
cat("TWFE SUMMARY TABLE — Monthly Panel (county + year-month FE)\n")
cat(strrep("=", 72), "\n\n", sep = "")

etable(
  fits,
  headers = c("log_cust_hours\n[log count]",
              "log_cust_hours\n[log capacity]",
              "max_pct_out\n[log count]",
              "max_pct_out\n[log capacity]"),
  depvar     = FALSE,
  se.below   = TRUE,
  fitstat    = c("n", "r2", "wr2"),
  signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1)
)

# ── 7. LaTeX table ────────────────────────────────────────────────────────────
cat("\n\nLaTeX:\n")
etable(
  fits,
  headers = c("\\shortstack{log(cust\\_hours)\\\\{[log count]}}",
              "\\shortstack{log(cust\\_hours)\\\\{[log capacity]}}",
              "\\shortstack{max\\_pct\\_out\\\\{[log count]}}",
              "\\shortstack{max\\_pct\\_out\\\\{[log capacity]}}"),
  depvar     = FALSE,
  se.below   = TRUE,
  fitstat    = c("n", "r2", "wr2"),
  signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
  tex = TRUE
)
