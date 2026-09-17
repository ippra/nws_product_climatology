# Paths ------------------------------------------------------------------------
# Every script sources this so data locations are defined once. Paths resolve
# against the project root (the .here file), so nothing here needs configuring
# on a new machine. data/ and outputs/ are gitignored.

project_root <- here::here()

data_dir <- file.path(project_root, "data")
reference_dir <- file.path(project_root, "reference")
site_src <- file.path(project_root, "site")
outputs <- file.path(project_root, "outputs")

# Years ------------------------------------------------------------------------
# The dashboard covers whole calendar years only, so an average per year is
# never pulled down by a year still in progress. The last year is the most
# recent one IEM can have finished: the one before today's.
first_year <- 2010
last_year <- as.integer(format(Sys.Date(), "%Y")) - 1

# Alerts are dated by the local calendar day of the county they cover. A
# warning issued at 7 PM Central on 31 December is 01:00 UTC on 1 January, so
# the edges of the archive need a little of the years either side: a month
# before the first year catches long river flood warnings still in force on
# 1 January, and a day after the last catches evening alerts on 31 December.
boundary_before_days <- 31
boundary_after_days <- 1

# Source -----------------------------------------------------------------------
# NWS watches, warnings and advisories (VTEC), every office, from the Iowa
# Environmental Mesonet. One request per year returns a zip holding a shapefile
# and a CSV of the same rows: one row per zone or county (NWS UGC) per alert,
# plus one row per storm-based polygon.
# https://mesonet.agron.iastate.edu/request/gis/watchwarn.phtml
iem_wwa_url <- paste0(
  "https://mesonet.agron.iastate.edu/cgi-bin/request/gis/watchwarn.py"
)

# Years downloaded by 01_refresh_data.R. Years 2010-2024 were downloaded from
# the same service in January 2025 for the earlier county analysis and live in
# wwa_data/<year>_all/; 01 reads those in place rather than fetch 18 GB again.
archive_dir <- file.path(data_dir, "iem_wwa")
legacy_archive_dir <- file.path(project_root, "wwa_data")

# Reference Geography ----------------------------------------------------------
# Counties: Census cartographic boundary file, 2023 vintage, 1:500,000.
# https://www2.census.gov/geo/tiger/GENZ2023/shp/cb_2023_us_county_500k.zip
census_counties_url <- paste0(
  "https://www2.census.gov/geo/tiger/GENZ2023/shp/cb_2023_us_county_500k.zip"
)

# NWS counties (time zone and forecast office of each county) and county
# warning areas, 16 April 2026 vintage.
# https://www.weather.gov/gis/Counties
# https://www.weather.gov/gis/CWABounds
nws_counties_url <- paste0(
  "https://www.weather.gov/source/gis/Shapefiles/County/c_16ap26.zip"
)
nws_cwa_url <- paste0(
  "https://www.weather.gov/source/gis/Shapefiles/WSOM/w_16ap26.zip"
)

reference_raw_dir <- file.path(data_dir, "reference_raw")

# Reference Tables -------------------------------------------------------------
# Every phenomenon-significance pair in the archive, with its name, its group
# in the product picker, and whether it is a real product. 03 stops on a pair
# not listed here.
products_reference <- file.path(reference_dir, "products.csv")

# Issuers in the archive that are not in the county warning area file: an old
# identifier for a current office, or a national center.
issuers_reference <- file.path(reference_dir, "issuers.csv")

# Crosswalk Rules --------------------------------------------------------------
# A zone counts toward a county when it covers at least 5% of the county's area,
# or when at least 25% of the zone lies inside the county (a small zone within a
# large county). Calibrated on the 2024 county-coded UGCs, whose true county is
# known: boundary slivers between a county and its neighbours measured 0.2% of
# county area at the median and 1.75% at the 99th percentile.
min_county_share <- 0.05
min_zone_share <- 0.25

# Equal Earth: an equal-area projection that covers Alaska, Hawaii, the
# territories and the lower 48 alike, so area shares mean the same everywhere.
area_crs <- 8857

# NWS time zone codes to IANA zones. Split counties take their first letter.
nws_time_zones <- c(
  E = "America/New_York",
  C = "America/Chicago",
  M = "America/Denver",
  m = "America/Phoenix",
  P = "America/Los_Angeles",
  A = "America/Anchorage",
  H = "Pacific/Honolulu",
  V = "America/Puerto_Rico",
  G = "Pacific/Guam",
  S = "Pacific/Pago_Pago",
  F = "Pacific/Pohnpei",
  J = "Pacific/Palau",
  K = "Pacific/Majuro"
)

# UGC prefixes for marine zones. They cover water, never a county, so 02 does
# not expect them to map to one.
marine_prefixes <- c(
  "AM", "AN", "GM", "LC", "LE", "LH", "LM", "LO", "LS", "PH", "PK", "PM", "PS",
  "PZ", "SL"
)

# Freely associated states with NWS zones but no Census counties.
no_county_prefixes <- c("FM", "MH", "PW")

# Archive Files ----------------------------------------------------------------
# One IEM request per year, plus the two boundary windows. IEM names each file
# for its window, wwa_<start>_<end> in UTC, which is the name used on disk.
iem_stem <- function(start, end) {
  paste0("wwa_", format(start, "%Y%m%d%H%M"), "_", format(end, "%Y%m%d%H%M"))
}

archive_parts <- function() {
  years <- first_year:last_year
  before_start <- as.POSIXct(paste0(first_year, "-01-01"), tz = "UTC") -
    boundary_before_days * 86400
  after_start <- as.POSIXct(paste0(last_year + 1, "-01-01"), tz = "UTC")

  parts <- data.frame(
    part = c("before", as.character(years), "after"),
    year = c(first_year - 1, years, last_year + 1),
    start = c(
      before_start,
      as.POSIXct(paste0(years, "-01-01"), tz = "UTC"),
      after_start
    ),
    end = c(
      as.POSIXct(paste0(first_year - 1, "-12-31 23:59"), tz = "UTC"),
      as.POSIXct(paste0(years, "-12-31 23:59"), tz = "UTC"),
      after_start + boundary_after_days * 86400 - 60
    )
  )
  parts$stem <- iem_stem(parts$start, parts$end)

  # A legacy year folder is used when it holds this exact window; everything
  # else lives under data/iem_wwa/<part>/.
  legacy <- file.path(legacy_archive_dir, paste0(parts$year, "_all"))
  in_legacy <- file.exists(file.path(legacy, paste0(parts$stem, ".csv"))) &
    file.exists(file.path(legacy, paste0(parts$stem, ".shp")))
  parts$dir <- ifelse(in_legacy, legacy, file.path(archive_dir, parts$part))
  parts$csv <- file.path(parts$dir, paste0(parts$stem, ".csv"))
  parts
}

# Grid -------------------------------------------------------------------------
# The gridded view: square 5 km cells on an Albers equal-area projection, one
# grid per region so every cell is 25 square km on the ground. The lower 48 use
# the parameters of EPSG:5070 and Alaska those of EPSG:3338; Hawaii's are
# ESRI:102007's; the island groups have their own. A cell is land when its
# centre falls in a Census county, and belongs to that county.
grid_cell_m <- 5000

grid_domains <- tibble::tribble(
  ~domain,      ~states,          ~lat_0, ~lon_0, ~lat_1, ~lat_2,
  "conus",      NA,               23,     -96,    29.5,   45.5,
  "alaska",     "AK",             50,     -154,   55,     65,
  "hawaii",     "HI",             13,     -157,   8,      18,
  "caribbean",  "PR VI",          18,     -66,    17.8,   18.4,
  "marianas",   "GU MP",          16,     145.5,  13.5,   19,
  "samoa",      "AS",             -13,    -170,   -14.4,  -11.1
)

grid_proj <- function(lat_0, lon_0, lat_1, lat_2) {
  paste0(
    "+proj=aea +lat_0=", lat_0, " +lon_0=", lon_0, " +lat_1=", lat_1,
    " +lat_2=", lat_2, " +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"
  )
}
