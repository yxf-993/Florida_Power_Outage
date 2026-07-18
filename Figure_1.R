# ══════════════════════════════════════════════════════════════════════════════
# FIGURE 1
# Average Peak Outage Rate by Data Center Presence, Florida 2014-2025
# ══════════════════════════════════════════════════════════════════════════════

# ── Libraries ──────────────────────────────────────────────────────────────────
library(data.table)
library(ggplot2)
library(lubridate)
library(dplyr)
library(scales)
library(ggnewscale)

# ── Load data ──────────────────────────────────────────────────────────────────
# NOTE: Update the file paths below to match your local directory before running
DC         <- fread("~/Desktop/36/FL/data/FL_DC_panel_2014_2025.csv")
fl_monthly <- fread("~/Desktop/36/FL/data/FL_monthly_DC_2014_2025.csv")

# ── Create county-level data center indicator ──────────────────────────────────
# Flag any county that ever hosted a data center over the study period
dc_ever <- DC[, .(has_dc_ever = 1L), by = county_clean]

fl_dc_monthly <- merge(fl_monthly, dc_ever, by = "county_clean", all.x = TRUE)
fl_dc_monthly[is.na(has_dc_ever), has_dc_ever := 0L]
fl_dc_monthly[, has_dc_label := fifelse(has_dc_ever == 1L, "Has Data Center", "No Data Center")]
fl_dc_monthly[, date := as.Date(paste0(year_month, "-01"))]

# ── Monthly group average ───────────────────────────────────────────────────────
grp <- fl_dc_monthly[, .(avg_max_pct = mean(max_pct_out, na.rm = TRUE)),
                     by = .(date, has_dc_label)]
setorder(grp, date)

# ── Storm events ─────────────────────────────────────────────────────────────────
storms <- tribble(
  ~storm,    ~year, ~month, ~type,
  "Irma",    2017,  9,  "hurricane",
  "Michael", 2018, 10,  "hurricane",
  "Alberto", 2018,  5,  "hurricane",
  "Dorian",  2019,  9,  "hurricane",
  "Isaias",  2020,  8,  "hurricane",
  "Sally",   2020,  9,  "hurricane",
  "Eta",     2020, 11,  "hurricane",
  "Elsa",    2021,  7,  "hurricane",
  "Fred",    2021,  8,  "hurricane",
  "Ian",     2022,  9,  "hurricane",
  "Nicole",  2022, 11,  "hurricane",
  "Idalia",  2023,  8,  "hurricane",
  "Debby",   2024,  8,  "hurricane",
  "Helene",  2024,  9,  "hurricane",
  "Milton",  2024, 10,  "hurricane",
  "MWE",     2024,  1,  "storm",
  "JWE",     2024,  5,  "storm"
) %>%
  mutate(date = as.Date(sprintf("%d-%02d-01", year, month)))

hurricanes <- storms %>% filter(type == "hurricane")
tstorms    <- storms %>% filter(type == "storm")

storms_all <- bind_rows(
  hurricanes %>% mutate(type_label = "Hurricane"),
  tstorms    %>% mutate(type_label = "Storm")
)

# ── Colors ───────────────────────────────────────────────────────────────────────
col_no_dc  <- "#1565C0"
col_has_dc <- "#E65100"
col_hurr   <- "#C62828"
col_storm  <- "#78909C"

# ── Plot ──────────────────────────────────────────────────────────────────────────
p <- ggplot() +
  # Hurricane / Storm
  geom_vline(data = storms_all, aes(xintercept = date, color = type_label),
             linetype = "dashed", linewidth = 0.2, key_glyph = "path")  +
  scale_color_manual(
    name   = NULL,
    breaks = c("Hurricane", "Storm"),
    values = c("Hurricane" = col_hurr, "Storm" = col_storm)
  ) +
  guides(color = guide_legend(override.aes = list(linetype = "dashed"))) +
  
  new_scale_color() +
  
  # Has Data Center / No Data Center
  geom_line(data = grp, aes(x = date, y = avg_max_pct, color = has_dc_label),
            linewidth = 0.65) +
  scale_color_manual(
    name   = NULL,
    breaks = c("Has Data Center", "No Data Center"),
    values = c("Has Data Center" = col_has_dc, "No Data Center" = col_no_dc)
  ) +
  
  scale_y_continuous(labels = label_percent(scale = 100)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title = "Average Peak Outage Rate by Data Center Presence, Florida 2014-2025",
    x     = "Year",
    y     = "Average Peak Outage Rate"
  ) +
  theme_minimal(base_size = 14, base_family = "Arial") +
  theme(
    panel.border        = element_rect(color = "black", fill = NA, linewidth = 0.8),
    panel.grid.major.x  = element_blank(),
    panel.grid.minor.x  = element_blank(),
    panel.grid.major.y  = element_blank(),
    panel.grid.minor.y  = element_blank(),
    legend.position     = c(0.13, 0.80),
    legend.background = element_rect(fill = "white", color = "grey90", linewidth = 0.3),
    legend.text         = element_text(size = 12),
    plot.title          = element_text(face = "plain", size = 16),
    axis.title          = element_text(size = 14),
    axis.text           = element_text(size = 12)
  )

print(p)

# ── Save ──────────────────────────────────────────────────────────────────────────
ggsave("FL_Fig1_outage_by_dc_presence.pdf", plot = p, 
       width = 10, height = 6, units = "in", dpi = 600, device = cairo_pdf)








