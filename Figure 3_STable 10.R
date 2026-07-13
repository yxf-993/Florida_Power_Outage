# ══════════════════════════════════════════════════════════════════════════════
# HETEROGENEITY ANALYSIS — Socioeconomic Subgroups
# Merges monthly outage panel with annual ACS county-level data, then runs
# TWFE interaction models separately by income/poverty/race subgroup and season
# ══════════════════════════════════════════════════════════════════════════════

# ── Libraries ──────────────────────────────────────────────────────────────────
library(tidyverse)
library(readr)
library(fixest)

# ── Load data ──────────────────────────────────────────────────────────────────
# NOTE: Update file paths below to match your local directory before running
my_data <- read_csv("~/Desktop/socioeconomic_variable/data/data/processed/FL_county_ACS_panel_2014_2025.csv")
outage  <- read.csv("/Users/yexiaofeng/Desktop/36/Data/FL_monthly_DC_2014_2025.csv",
                    stringsAsFactors = FALSE)
dc_ann  <- read.csv("/Users/yexiaofeng/Desktop/36/FL/data/FL_DC_panel_2014_2025.csv",
                    stringsAsFactors = FALSE)

# ── Construct outage panel variables ───────────────────────────────────────────
dc_attr <- dc_ann |> select(county_clean, year, stock_capacity_mw)

df <- outage |>
  left_join(dc_attr, by = c("county_clean", "year")) |>
  mutate(
    stock_capacity_mw = replace(stock_capacity_mw, is.na(stock_capacity_mw), 0),
    log_cust_hours    = log(cust_hours_out + 1),
    log_capacity      = log(stock_capacity_mw + 1),
    county_id         = as.integer(factor(county_clean)),
    ym                = factor(year_month),
    month_num         = as.integer(sub(".*-", "", year_month)),
    fips_join         = as.integer(fips_code)
  )

# ── Construct ACS subgroup indicators ──────────────────────────────────────────
# High income: county median income above the cross-county median in that year
my_data <- my_data |>
  group_by(year) |>
  mutate(
    high_income = if_else(median_income >= median(median_income, na.rm = TRUE), 1L, 0L)
  ) |>
  ungroup() |>
  mutate(fips_join = as.integer(fips))

# ── Merge annual ACS data onto monthly outage panel ────────────────────────────
df_final <- df |>
  left_join(my_data |> select(-fips, -county_name), by = c("fips_join", "year")) |>
  select(-fips_join)

# Verify all observations matched with ACS data
missing_acs <- sum(is.na(df_final$median_income))
if (missing_acs > 0)
  warning(sprintf("%d rows did not match ACS data — check FIPS keys.", missing_acs))

# ── Define mutually exclusive income/poverty subgroups ─────────────────────────
# High Income with Low Poverty:  above-median income AND below-median poverty
# High Income with High Poverty: above-median income AND above-median poverty
# Low Income with High Poverty:  below-median income AND above-median poverty
# Low Income with Low Poverty:   below-median income AND below-median poverty
df_1 <- df_final |>
  mutate(
    high_income_low_poverty  = if_else(high_income == 1 & high_poverty == 0, 1L, 0L),
    high_income_high_poverty = if_else(high_income == 1 & high_poverty == 1, 1L, 0L),
    low_income_high_poverty  = if_else(high_income == 0 & high_poverty == 1, 1L, 0L),
    low_income_low_poverty   = if_else(high_income == 0 & high_poverty == 0, 1L, 0L)
  )

# ── Subset to hurricane season (June–November) ─────────────────────────────────
df_hurricane <- df_1 |> filter(month_num >= 6 & month_num <= 11)

# ── Subgroup labels and model storage ─────────────────────────────────────────
core_vars <- c(
  "high_income"             = "High Income",
  "high_poverty"            = "High Poverty",
  "high_income_low_poverty" = "High Income with Low Poverty",
  "high_income_high_poverty"= "High Income with High Poverty",
  "low_income_high_poverty" = "Low Income with High Poverty",
  "low_income_low_poverty"  = "Low Income with Low Poverty",
  "high_nonwhite"           = "High Non-White Share",
  "high_black"              = "High Black Share",
  "high_hispanic"           = "High Hispanic Share"
)

m_hours_full <- list()
m_peak_full  <- list()
m_hours_hurr <- list()
m_peak_hurr  <- list()

# ── Regression loop: full sample and hurricane season ─────────────────────────
for (var_name in names(core_vars)) {
  fml_hours <- as.formula(sprintf(
    "log_cust_hours ~ log_capacity + log_capacity:%s | county_id + ym", var_name))
  fml_peak  <- as.formula(sprintf(
    "max_pct_out ~ log_capacity + log_capacity:%s | county_id + ym", var_name))
  
  m_hours_full[[var_name]] <- feols(fml_hours, data = df_1,        cluster = ~county_id)
  m_peak_full[[var_name]]  <- feols(fml_peak,  data = df_1,        cluster = ~county_id)
  m_hours_hurr[[var_name]] <- feols(fml_hours, data = df_hurricane, cluster = ~county_id)
  m_peak_hurr[[var_name]]  <- feols(fml_peak,  data = df_hurricane, cluster = ~county_id)
}

# ── Assemble model lists ───────────────────────────────────────────────────────
hours_full_list <- setNames(lapply(names(core_vars), function(v) m_hours_full[[v]]), names(core_vars))
hours_hurr_list <- setNames(lapply(names(core_vars), function(v) m_hours_hurr[[v]]), names(core_vars))
peak_full_list  <- setNames(lapply(names(core_vars), function(v) m_peak_full[[v]]),  names(core_vars))
peak_hurr_list  <- setNames(lapply(names(core_vars), function(v) m_peak_hurr[[v]]),  names(core_vars))

# ── Extract interaction coefficients from model list ───────────────────────────
extract_vulnerability_data <- function(model_list, sample_label) {
  purrr::map_dfr(names(model_list), function(nm) {
    mod <- model_list[[nm]]
    cf  <- coef(mod)
    ci  <- confint(mod, level = 0.95)
    idx <- grep("log_capacity:", names(cf))
    if (length(idx) == 0) return(NULL)
    data.frame(
      vulnerability = nm,
      estimate      = cf[idx],
      conf.low      = ci[idx, 1],
      conf.high     = ci[idx, 2],
      sample_group  = sample_label,
      stringsAsFactors = FALSE
    )
  })
}

df_hours_all <- bind_rows(
  extract_vulnerability_data(hours_full_list, "Full Year"),
  extract_vulnerability_data(hours_hurr_list, "Hurricane Season (Jun-Nov)")
)
df_peak_all <- bind_rows(
  extract_vulnerability_data(peak_full_list, "Full Year"),
  extract_vulnerability_data(peak_hurr_list, "Hurricane Season (Jun-Nov)")
)

# ── Display labels and plot formatting ─────────────────────────────────────────
clean_labels <- c(
  "high_income"             = "High Income",
  "high_poverty"            = "High Poverty",
  "high_income_low_poverty" = "High Income with Low Poverty",
  "high_income_high_poverty"= "High Income with High Poverty",
  "low_income_high_poverty" = "Low Income with High Poverty",
  "low_income_low_poverty"  = "Low Income with Low Poverty",
  "high_nonwhite"           = "High Non-White Share",
  "high_black"              = "High Black Share",
  "high_hispanic"           = "High Hispanic Share"
)

format_df_for_plot <- function(df) {
  df |>
    mutate(
      vulnerability_lab = clean_labels[vulnerability],
      vulnerability_lab = factor(vulnerability_lab, levels = rev(clean_labels)),
      sample_group      = factor(sample_group,
                                 levels = c("Hurricane Season (Jun-Nov)", "Full Year"))
    )
}

df_hours_plot <- format_df_for_plot(df_hours_all)
df_peak_plot  <- format_df_for_plot(df_peak_all)

# ── Color and shape palette ────────────────────────────────────────────────────
col_no_dc  <- "#1565C0"   # Blue  — Full Year
col_has_dc <- "#E65100"   # Orange — Hurricane Season

col_vals   <- c("Hurricane Season (Jun-Nov)" = col_has_dc,
                "Full Year"                  = col_no_dc)
fill_vals  <- c("Hurricane Season (Jun-Nov)" = col_has_dc,
                "Full Year"                  = col_no_dc)
shape_vals <- c("Hurricane Season (Jun-Nov)" = 21,
                "Full Year"                  = 24)

# ── Shared plot theme ──────────────────────────────────────────────────────────
theme_coef <- function() {
  theme_bw(base_size = 10, base_family = "Arial") +
    theme(
      panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.45),
      panel.grid.major.x = element_line(color = "grey93", linewidth = 0.3),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.ticks         = element_line(color = "black", linewidth = 0.3),
      axis.ticks.length  = unit(3, "pt"),
      axis.text          = element_text(size = 8.5, color = "black"),
      axis.title         = element_text(size = 9.5, color = "black"),
      plot.subtitle      = element_text(size = 10, face = "plain", color = "black",
                                        hjust = 0, margin = margin(b = 8)),
      legend.position    = "none"
    )
}

# ── Panel A: Log cumulative customer-hours without power ──────────────────────
p_duration <- ggplot(df_hours_plot,
                     aes(x = estimate, y = vulnerability_lab,
                         color = sample_group, fill = sample_group, shape = sample_group)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.7) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.25, linewidth = 0.5, position = position_dodge(width = 0.5)) +
  geom_point(size = 2.2, stroke = 0.7, position = position_dodge(width = 0.5)) +
  scale_color_manual(values = col_vals,   name = NULL) +
  scale_fill_manual( values = fill_vals,  name = NULL) +
  scale_shape_manual(values = shape_vals, name = NULL) +
  labs(
    x        = "Marginal Interaction Coefficient",
    y        = "Socioeconomic Subgroup",
    subtitle = "Log(Cumulative Customer-Hours without Power + 1)"
  ) +
  theme_coef() +
  theme(plot.margin = margin(12, 12, 10, 12))

# ── Panel B: Peak outage rate ──────────────────────────────────────────────────
p_peak <- ggplot(df_peak_plot,
                 aes(x = estimate, y = vulnerability_lab,
                     color = sample_group, fill = sample_group, shape = sample_group)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.7) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.25, linewidth = 0.5, position = position_dodge(width = 0.5)) +
  geom_point(size = 2.2, stroke = 0.7, position = position_dodge(width = 0.5)) +
  scale_color_manual(values = col_vals,   name = NULL) +
  scale_fill_manual( values = fill_vals,  name = NULL) +
  scale_shape_manual(values = shape_vals, name = NULL) +
  labs(
    x        = "Marginal Interaction Coefficient",
    y        = NULL,
    subtitle = "Peak Outage Rate"
  ) +
  theme_coef() +
  theme(plot.margin = margin(12, 16, 10, 4))


# ── Combine panels and add shared legend ──────────────────────────────────────
final_plot <- (p_duration | p_peak) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title      = "Heterogeneous Effects of Data Center Capacity on Power Outages",
    tag_levels = "a",
    theme = theme(
      plot.title      = element_text(size = 13, face = "plain", color = "black",
                                     family = "Arial", hjust = 0, margin = margin(t = 5, b = 2)),
      legend.position = "bottom"
    )
  ) &
  theme(
    legend.position   = "bottom",
    legend.background = element_rect(fill = alpha("white", 0.88), color = NA, linewidth = 0),
    legend.key        = element_rect(fill = NA, color = NA, linewidth = 0),
    legend.key.width  = unit(18, "pt"),
    legend.key.height = unit(10, "pt"),
    legend.text       = element_text(size = 10, color = "black", family = "Arial"),
    legend.margin     = margin(t = 2, b = 2)
  )

print(final_plot)

# ── Save ──────────────────────────────────────────────────────────────────────
ggsave(
  filename = "fig_vulnerability_heterogeneity.pdf",
  plot     = final_plot,
  width    = 7.09, height = 5,
  device   = cairo_pdf
)





final_plot <- (p_duration | p_peak) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title      = "Heterogeneous Effects of Data Center Capacity on Power Outages",
    tag_levels = "a",
    theme = theme(
      plot.title      = element_text(size = 11, face = "plain",
                                     family = "Arial", hjust = 0),
      legend.position = "bottom"
    )
  ) &
  theme(
    legend.position   = "bottom",
    legend.background = element_rect(fill = alpha("white", 0.88), color = NA),
    legend.key        = element_rect(fill = NA, color = NA),
    legend.key.width  = unit(18, "pt"),
    legend.key.height = unit(10, "pt"),
    legend.text       = element_text(size = 9, color = "black", family = "Arial"),
    legend.margin     = margin(t = 2, b = 2)
  )

ggsave(
  "fig_vulnerability_heterogeneity.pdf",
  final_plot,
  width  = 16,
  height = 8,
  device = cairo_pdf
)












# Panel A: all four outcome-season combos side by side
etable(m_hours_full[names(core_vars)[1:5]],
       m_hours_hurr[names(core_vars)[1:5]],
       m_peak_full[names(core_vars)[1:5]],
       m_peak_hurr[names(core_vars)[1:5]])

# Panel B
etable(m_hours_full[names(core_vars)[6:9]],
       m_hours_hurr[names(core_vars)[6:9]],
       m_peak_full[names(core_vars)[6:9]],
       m_peak_hurr[names(core_vars)[6:9]])




