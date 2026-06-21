# Month-by-Month Interaction: TWFE Only
# ─────────────────────────────────────────────────────────────────────────────
# Spec: y_it = Σₘ βₘ (log_cap × 1[month=m]) + η_i + λ_t + ε_it
# Outcomes: log(cust_hours_out + 1)  and  max_pct_out
# All 67 counties (including always-treated), county + ym FE, SE clustered by county
# ─────────────────────────────────────────────────────────────────────────────
library(fixest)
library(dplyr)
library(ggplot2)
library(patchwork)

# ── 1. Data ────────────────────────────────────────────────────────────────────
# NOTE: Update the file paths below to match your local directory before running
outage <- read.csv("/Users/yexiaofeng/Desktop/36/Data/FL_monthly_DC_2014_2025.csv",
                   stringsAsFactors = FALSE)
dc_ann <- read.csv("/Users/yexiaofeng/Desktop/36/FL/data/FL_DC_panel_2014_2025.csv",
                   stringsAsFactors = FALSE)

dc_attr <- dc_ann |> select(county_clean, year, stock_capacity_mw)

df <- outage |>
  left_join(dc_attr, by = c("county_clean", "year")) |>
  mutate(
    stock_capacity_mw = replace(stock_capacity_mw, is.na(stock_capacity_mw), 0),
    log_cust_hours    = log(cust_hours_out + 1),
    log_capacity      = log(stock_capacity_mw + 1),
    county_id         = as.integer(factor(county_clean)),
    ym                = factor(year_month),
    month_num         = as.integer(sub(".*-", "", year_month))
  )

# ── 2. TWFE interaction models (ref = March) ──────────────────────────────────
fit_ch <- feols(log_cust_hours ~ i(month_num, log_capacity, ref = 3) | county_id + ym,
                df, cluster = ~county_id)
fit_mp <- feols(max_pct_out   ~ i(month_num, log_capacity, ref = 3) | county_id + ym,
                df, cluster = ~county_id)

# Joint F-test: H0 = all 12 month-specific slopes are zero
wald_ch <- wald(fit_ch, "month_num")
wald_mp <- wald(fit_mp, "month_num")

# ── 3. Extract coefficients ───────────────────────────────────────────────────
month_labs  <- c("Jan","Feb","Mar","Apr","May","Jun",
                 "Jul","Aug","Sep","Oct","Nov","Dec")
month_order <- c("Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb")
REF_MONTH   <- 3L   # March

extract_coefs <- function(fit, outcome) {
  cf  <- coef(fit);  se_ <- se(fit);  pv <- pvalue(fit)
  ci  <- confint(fit, level = 0.95)
  idx <- grep("month_num::", names(cf))
  m   <- as.integer(gsub("^month_num::([0-9]+):.*", "\\1", names(cf)[idx]))
  ok  <- !is.na(m)
  res <- data.frame(
    month = m[ok], est = cf[idx][ok], se = se_[idx][ok],
    lo = ci[idx, 1][ok], hi = ci[idx, 2][ok], pval = pv[idx][ok],
    stringsAsFactors = FALSE
  )
  bind_rows(
    res,
    data.frame(month = REF_MONTH, est = 0, se = 0, lo = 0, hi = 0, pval = 1)
  ) |>
    arrange(month) |>
    mutate(
      month_lab = factor(month_labs[month], levels = month_order),
      sig       = pval < 0.05,
      outcome   = outcome
    )
}

coef_df <- bind_rows(
  extract_coefs(fit_ch, "log(Customer-Hours-Out + 1)"),
  extract_coefs(fit_mp, "Peak % Customers Out")
) |>
  mutate(outcome = factor(outcome,
                          levels = c("log(Customer-Hours-Out + 1)",
                                     "Peak % Customers Out")))

# ── 4. Plot ────────────────────────────────────────────────────────────────────
TWFE_COL <- "#1565C0"

build_panel <- function(df_sub, y_label, title_text) {
  ggplot(df_sub, aes(x = month_lab, y = est, group = 1)) +
    geom_vline(xintercept = 3.5, linetype = "dashed", linewidth = 0.45, colour = "#C62828") +
    geom_vline(xintercept = 9.5, linetype = "dashed", linewidth = 0.45, colour = "#C62828") +
    annotate("text", x = 6.5, y = Inf, label = "Hurricane season (Jun-Nov)",
             vjust = 1.5, size = 4.0, colour = "grey35") +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.45, colour = "grey55") +
    geom_ribbon(aes(ymin = lo, ymax = hi), fill = TWFE_COL, alpha = 0.12, colour = NA) +
    geom_line(colour = TWFE_COL, linewidth = 0.7) +
    geom_point(aes(shape = sig, size = sig), colour = TWFE_COL, stroke = 0.9) +
    scale_shape_manual(values = c(`FALSE` = 1L,  `TRUE` = 19L), guide = "none") +
    scale_size_manual( values = c(`FALSE` = 1.8, `TRUE` = 2.3), guide = "none") +
    scale_x_discrete(drop = FALSE) +
    labs(title = title_text, y = y_label, x = NULL) +
    theme_minimal(base_size = 14, base_family = "Arial") +
    theme(
      panel.border        = element_rect(color = "black", fill = NA, linewidth = 0.8),
      panel.grid.major.x  = element_blank(),
      panel.grid.minor.x  = element_blank(),
      panel.grid.major.y  = element_blank(),
      panel.grid.minor.y  = element_blank(),
      legend.position     = "none",
      plot.title          = element_text(face = "plain", size = 14),
      axis.title          = element_text(size = 14),
      axis.text           = element_text(size = 12)
    )
}

df_ch <- coef_df |> filter(grepl("Hours", as.character(outcome)))
df_mp <- coef_df |> filter(grepl("Peak",  as.character(outcome)))

p_ch <- build_panel(df_ch, "Coefficient", "Outcome: log(Customer-Hours-Out + 1)")
p_mp <- build_panel(df_mp, "Coefficient", "Outcome: Peak Customers Out")

fig <- (p_ch / p_mp) +
  plot_annotation(
    title = "Data center effects on power outages month by month",
    theme = theme(plot.title = element_text(size = 16, face = "plain", family = "Arial"))
  )

# ── 5. Save outputs ────────────────────────────────────────────────────────────
ggsave("/Users/yexiaofeng/Desktop/36/FL/fig_month_twfe_only.png",
       fig, width = 7.09, height = 6, dpi = 900)

write.csv(coef_df |> select(-sig),
          "/Users/yexiaofeng/Desktop/36/FL/month_twfe_only_coefs.csv",
          row.names = FALSE)