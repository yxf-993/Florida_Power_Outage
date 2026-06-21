# ══════════════════════════════════════════════════════════════════════════════
# INSTRUMENTAL VARIABLE (IV) ANALYSIS
# Effect of Data Center Capacity on Power Outages, Florida 2014-2025
#
# Identification strategy: distance from each county centroid to the nearest
# active internet exchange point (IXP), interacted with a linear time trend.
# This instrument captures the differential timing of data center capacity
# build-out driven by proximity to internet backbone infrastructure, rather
# than by local outage conditions.
# ══════════════════════════════════════════════════════════════════════════════

# ── Libraries ──────────────────────────────────────────────────────────────────
library(data.table)
library(dplyr)
library(tidyr)
library(fixest)
library(geosphere)
library(tidygeocoder)
library(tigris)
library(sf)

# ── Load data ──────────────────────────────────────────────────────────────────
# NOTE: Update the file path below to match your local directory before running
fl_monthly <- fread("~/Desktop/36/FL/data/FL_monthly_DC_2014_2025.csv")

# ── Construct core variables ──────────────────────────────────────────────────────
fl_monthly$log_capacity <- log(fl_monthly$stock_capacity_mw + 1)
fl_monthly$county_id    <- as.integer(factor(fl_monthly$county_clean))
fl_monthly$ym           <- as.integer(gsub("-", "", fl_monthly$year_month))

# ── Baseline two-way fixed effects (TWFE) model, full sample ─────────────────────
fit_twfe_mp_full <- feols(
  max_pct_out ~ log_capacity | county_id + ym,
  data    = fl_monthly,
  cluster = ~county_id
)

# ════════════════════════════════════════════════════════════════════════════════
# STEP 1: Geocode internet exchange point (IXP) locations
# ════════════════════════════════════════════════════════════════════════════════

ixp_addresses <- data.frame(
  name = c("Miami_NAP1", "Miami_NAP2", "Miami_NW22", "Doral",
           "Orlando_Lakemont", "Orlando_JYP", "Tampa", "Jacksonville"),
  address = c(
    "50 NE 9th Street, Miami, FL 33132",
    "36 NE 2nd St, Miami, FL",
    "2115 NW 22nd St, Miami, FL 33142",
    "2100 NW 84th Ave, Doral, FL 33122",
    "2420 S Lakemont Ave, Orlando, FL 32792",
    "9701 S John Young Parkway, Orlando, FL 33142",
    "655 N Franklin St, Tampa, FL 33602",
    "421 W Church Street, Jacksonville, FL 32202"
  ),
  start_yr = c(2014, 2014, 2014, 2014, 2015, 2014, 2015, 2014)
)

ixp_geocoded <- ixp_addresses %>%
  geocode(address, method = "osm")

# Manual correction: OSM geocoding returned an inaccurate match for this
# address; coordinates below were verified manually against the known
# facility location.
ixp_geocoded[ixp_geocoded$name == "Orlando_JYP", "lat"]  <- 28.427123
ixp_geocoded[ixp_geocoded$name == "Orlando_JYP", "long"] <- -81.419818

# ════════════════════════════════════════════════════════════════════════════════
# STEP 2: Florida county centroids
# ════════════════════════════════════════════════════════════════════════════════

fl_counties <- counties(state = "FL", cb = TRUE, year = 2020) %>%
  st_transform(4326) %>%
  st_centroid() %>%
  mutate(
    lon = st_coordinates(.)[, 1],
    lat = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry() %>%
  select(GEOID, NAME, lon, lat)

# ════════════════════════════════════════════════════════════════════════════════
# STEP 3: Construct instrument — distance to nearest active IXP × year
# ════════════════════════════════════════════════════════════════════════════════

years <- 2014:2025

panel <- expand.grid(GEOID = fl_counties$GEOID, year = years) %>%
  left_join(fl_counties, by = "GEOID")

# For each county-year, compute the distance (km) to the nearest IXP that
# was already active by that year (start_yr <= year)
panel$dist_min_ixp_km <- mapply(function(clat, clon, yr) {
  active <- ixp_geocoded[ixp_geocoded$start_yr <= yr, ]
  dists  <- distHaversine(cbind(clon, clat), cbind(active$long, active$lat)) / 1000
  min(dists)
}, panel$lat, panel$lon, panel$year)

summary(panel$dist_min_ixp_km)

panel <- panel %>%
  mutate(fips_code = as.numeric(GEOID))

fl_monthly_iv <- fl_monthly %>%
  left_join(panel %>% select(fips_code, year, dist_min_ixp_km),
            by = c("fips_code", "year"))

# Instrument: distance to nearest IXP interacted with a linear time trend
fl_monthly_iv <- fl_monthly_iv %>%
  mutate(
    year_c = year - 2013,
    iv     = dist_min_ixp_km * year_c
  )

# ════════════════════════════════════════════════════════════════════════════════
# STEP 4: Two-stage least squares (2SLS) estimation
# ════════════════════════════════════════════════════════════════════════════════

# First stage instruments log_capacity with the distance × year interaction
fit_iv_mp <- feols(
  max_pct_out ~ 1 | county_id + ym | log_capacity ~ iv,
  data    = fl_monthly_iv,
  cluster = ~county_id
)

summary(fit_iv_mp)
fitstat(fit_iv_mp, "ivf")   # First-stage F-statistic; rule of thumb: F > 10 to rule out weak instrument

# OLS on the same IV-restricted sample, for direct comparison
fit_ols_mp <- feols(
  max_pct_out ~ log_capacity | county_id + ym,
  data    = fl_monthly_iv,
  cluster = ~county_id
)

# Compare full-sample TWFE, restricted-sample OLS, and IV estimates side by side
etable(fit_twfe_mp_full, fit_ols_mp, fit_iv_mp)

# ════════════════════════════════════════════════════════════════════════════════
# STEP 5: Secondary outcome — customer-hours lost
# ════════════════════════════════════════════════════════════════════════════════

fl_monthly_iv$log_com_out <- log(fl_monthly_iv$cust_hours_out + 1)

fit_iv_out <- feols(
  log_com_out ~ 1 | county_id + ym | log_capacity ~ iv,
  data    = fl_monthly_iv,
  cluster = ~county_id
)

summary(fit_iv_out)
fitstat(fit_iv_out, "ivf")