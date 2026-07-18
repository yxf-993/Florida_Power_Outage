# ══════════════════════════════════════════════════════════════════════════════
# SUPPLEMENTARY TABLE 1
# Descriptive Comparison of Counties With and Without Data Centers
# ══════════════════════════════════════════════════════════════════════════════
# Notes: 
# Data center counties host at least one data center facility during the sample period 
# (15 counties; 1,994 county-month observations). 
# Non-data center counties are counties without any data center facility during the sample period 
# (52 counties; 6,897 county-month observations). 
# Mean differences are calculated as the mean for non-data center counties minus the mean for data center counties.


# ── Libraries ──────────────────────────────────────────────────────────────────
library(modelsummary)
library(data.table)

# ── Load data ──────────────────────────────────────────────────────────────────
# NOTE: Update the file paths below to match your local directory before running
DC         <- fread("~/Desktop/36/FL/data/FL_DC_panel_2014_2025.csv")
fl_monthly <- fread("~/Desktop/36/FL/data/FL_monthly_DC_2014_2025.csv")

# ── Create county-level data center indicator ──────────────────────────────────
# Flag any county that ever hosted a data center over the study period
dc_ever <- DC[, .(has_dc_ever = 1L), by = county_clean]

fl_monthly <- merge(fl_monthly, dc_ever, by = "county_clean", all.x = TRUE)
fl_monthly[is.na(has_dc_ever), has_dc_ever := 0L]
fl_monthly[, has_dc := fifelse(has_dc_ever == 1L, "Has DC", "No DC")]

# ── Preview table in console before export ─────────────────────────────────────
supp_table1 <- datasummary_balance(
  ~ has_dc,
  data  = fl_monthly[, .(cust_hours_out, max_pct_out,
                         max_customers_out, mcc, has_dc)],
  title = "Supplementary Table 1: Descriptive Comparison of Counties With and Without Data Centers",
  notes = "Differences tested via two-sample t-test.",
  output = "dataframe"
)

print(supp_table1)

# ── Export to Word (.docx) ─────────────────────────────────────────────────────
# NOTE: The output file will be saved to your current working directory.
#       To save elsewhere, replace the filename with a full path, e.g.:
#       output = "~/Documents/FL_Supp_Table1_balance.docx"
datasummary_balance(
  ~ has_dc,
  data  = fl_monthly[, .(cust_hours_out, max_pct_out,
                         max_customers_out, mcc, has_dc)],
  title = "Supplementary Table 1: Descriptive Comparison of Counties With and Without Data Centers",
  notes = "Differences tested via two-sample t-test.",
  output = "FL_Supp_Table1_balance.docx"
)
