# Hurricane Panel — TWFE Best Models + Combined Table
# ─────────────────────────────────────────────────────────────────────────────
# Dataset 1: FL_hurricane_DC_panel.csv
#   Y = Peak % Customers Out (rescaled to 0–1)
#   Model A: X1(cur wind) + X2(past wind) + X3(recov time) + log(pop) + X1×Urban
#
# Dataset 2: FL_hurricane_duration_DC_panel.csv
#   Y = log(Duration + 1) days
#   Model D: flood X1–X4 + wind X5–X7 + log(pop)
#     fl_X1 = cur_vel_max_a  : no. of flood events (integer 0–19)
#     fl_X2 = max_vel_max_a  : flood area proportion (0–1)
#     fl_X3 = rec_t_max_a    : max flood recovery time
#     fl_X4 = rec_t_min_a    : min flood recovery time
#     X5    = max past wind speed
#     X6    = max current wind speed
#     X7    = avg current wind speed
#
# All models: County FE + Hurricane FE | SE clustered by county
# ─────────────────────────────────────────────────────────────────────────────

library(fixest)
library(dplyr)

# ── 1. DATASET 1: Peak % Customers Out ───────────────────────────────────────
d1 <- read.csv("/Users/yexiaofeng/Desktop/36/FL/FL_hurricane_DC_panel.csv",
               stringsAsFactors = FALSE)

d1 <- d1 |>
  mutate(
    Y_pct        = Y / 100,
    log_capacity = log(stock_capacity_mw + 1),
    log_pop      = log(X18),
    urban        = as.integer(X5 == "Urban"),
    county_id    = as.integer(factor(county_clean)),
    hurricane_id = as.integer(factor(hurricane))
  )

# Model A: wind controls + X1×Urban
mA <- feols(Y_pct ~ log_capacity + X1 + X2 + X3 + log_pop + X1:urban |
              county_id + hurricane_id,
            data = d1, cluster = ~county_id)

# ── 2. DATASET 2: Outage Duration ────────────────────────────────────────────
d2 <- read.csv("/Users/yexiaofeng/Desktop/36/FL/FL_hurricane_duration_DC_panel.csv",
               stringsAsFactors = FALSE)

# Rename flood variables (X1–X4) to fl_X* to distinguish from d1's wind X1–X3
d2 <- d2 |>
  mutate(
    log_Y        = log(Y + 1),
    log_capacity = log(stock_capacity_mw + 1),
    log_pop      = log(X21),
    county_id    = as.integer(factor(county_clean)),
    hurricane_id = as.integer(factor(hurricane)),
    # Flood variables (X1–X4 in this dataset)
    fl_X1        = X1,   # cur_vel_max_a : no. of flood events
    fl_X2        = X2,   # max_vel_max_a : flood area proportion
    fl_X3        = X3,   # rec_t_max_a   : max flood recovery time
    fl_X4        = X4    # rec_t_min_a   : min flood recovery time
    # Wind variables kept as X5, X6, X7
  )

cat("Dataset 2 sample sizes:\n")
cat(sprintf("  Full N                     : %d\n", nrow(d2)))
cat(sprintf("  Complete on fl_X1+fl_X2    : %d\n",
            sum(complete.cases(d2[, c("fl_X1","fl_X2")]))))
cat(sprintf("  Complete on fl_X1–fl_X4    : %d\n",
            sum(complete.cases(d2[, c("fl_X1","fl_X2","fl_X3","fl_X4")]))))
cat(sprintf("  Complete on all (fl+wind)  : %d\n\n",
            sum(complete.cases(d2[, c("fl_X1","fl_X2","fl_X3","fl_X4","X5","X6","X7")]))))

# Model D: flood X1–X4 + wind X5–X7 + log(pop)
# fl_X3/fl_X4 have 150 NAs → N = 267
mD <- feols(log_Y ~ log_capacity + fl_X1 + fl_X2 + fl_X3 + fl_X4 +
              X5 + X6 + X7 + log_pop |
              county_id + hurricane_id,
            data = d2, cluster = ~county_id)

# ── 3. VARIABLE LABELS ────────────────────────────────────────────────────────
dict <- c(
  log_capacity = "log(DC Capacity + 1)",
  log_pop      = "log(Population)",
  # Dataset 1 — wind controls
  X1           = "X1: Current max wind speed",
  X2           = "X2: Past max wind speed",
  X3           = "X3: Wind recovery time (max)",
  "X1:urban"   = "X1 $\\times$ Urban",
  # Dataset 2 — flood controls
  fl_X1        = "Flood X1: Median time flooded",
  fl_X2        = "Flood X2: Maximum time flooded",
  fl_X3        = "Flood X3: The median flood percentage",
  fl_X4        = "Flood X4: The max flood percentage",
  # Dataset 2 — wind controls
  X5           = "X5: Max past wind speed",
  X6           = "X6: Max current wind speed",
  X7           = "X7: Avg current wind speed"
)

# ── 4. CONSOLE TABLE ─────────────────────────────────────────────────────────
etable(
  mA, mD,
  headers  = list("Peak \\% Customers Out (0--1)" = 1,
                  "log(Outage Duration + 1)"       = 1),
  dict     = dict,
  digits   = 4,
  se.below = TRUE,
  fitstat  = c("n", "r2", "ar2"),
  depvar   = FALSE
)

# ── 5. LATEX TABLE ───────────────────────────────────────────────────────────
etable(
  mA, mD,
  headers  = list("Peak \\% Customers Out (0--1)" = 1,
                  "log(Outage Duration + 1)"       = 1),
  dict        = dict,
  digits      = 4,
  se.below    = TRUE,
  fitstat     = c("n", "r2", "ar2"),
  depvar      = FALSE,
  style.tex   = style.tex("base"),
  tex         = TRUE,
  label       = "tab:hurricane_twfe",
  title       = "TWFE Estimates of Data-Centre Effects on Hurricane Outage Outcomes",
  notes       = paste0(
    "\\textit{Notes:} All models include county and hurricane-event fixed effects. ",
    "SE clustered by county in parentheses. ",
    "Dataset 1: 67 counties, 17 hurricanes, $N=1{,}085$. ",
    "Dataset 2: 67 counties, 16 hurricanes; flood variables Flood X3/X4 have 150 ",
    "missing values, reducing the estimation sample to $N=267$. ",
    "Flood X1 = number of flood events (cur\\_vel\\_max\\_a); ",
    "Flood X2 = flood area proportion (max\\_vel\\_max\\_a); ",
    "Flood X3 = maximum flood recovery time (rec\\_t\\_max\\_a); ",
    "Flood X4 = minimum flood recovery time (rec\\_t\\_min\\_a). ",
    "Urban status in Dataset 1 is time-invariant within county and absorbed by county FE ",
    "except via the X1$\\times$Urban interaction. ",
    "*** $p<0.001$; ** $p<0.01$; * $p<0.05$; $\\cdot$ $p<0.1$."
  ),
  file = "/Users/yexiaofeng/Desktop/36/FL/tab_hurricane_twfe.tex"
)

cat("\nLaTeX table saved: tab_hurricane_twfe.tex\n")
