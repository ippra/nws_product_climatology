library(tidyverse)
library(sf)

source(here::here("00_paths.R"))

# Site Data --------------------------------------------------------------------
# Turns 03's counts into the files the browser reads: one JSON file per
# product, loaded only when someone picks that product, plus a manifest and the
# county, office and state outlines. 04's grid files are copied in beside them
# and listed in the manifest. Computes no new counts - every number is 03's or
# 04's, reshaped.
#
# A product file holds, for each county and office with any of that product:
# alerts and days by year (one value per archive year) and by month of the year
# (summed over all years). And for the nation, alerts and days by year and
# month. Counties and offices with none are left out and read as zero.
#
# Writes outputs/05_site_data/.

counts_dir <- file.path(outputs, "03_counts")
crosswalk_dir <- file.path(outputs, "02_crosswalk")
grid_dir <- file.path(outputs, "04_grid")
out <- file.path(outputs, "05_site_data")

needed <- file.path(
  counts_dir,
  c("03_products.csv", "03_county_counts.rds", "03_office_counts.rds",
    "03_national_counts.csv")
)
if (!all(file.exists(needed))) {
  print(needed[!file.exists(needed)])
  stop("Counts above are missing - run 03_build_counts.R first.")
}

grid_meta_path <- file.path(grid_dir, "grid.json")
if (!file.exists(grid_meta_path)) {
  stop("No grid - run 04_build_grid.R first.")
}

staging <- paste0(out, ".next")
unlink(staging, recursive = TRUE)
dir.create(file.path(staging, "products"), recursive = TRUE)

years <- first_year:last_year

products <- read_csv(
  file.path(counts_dir, "03_products.csv"),
  col_types = cols(
    id = col_integer(), alerts = col_integer(), first_year = col_integer(),
    last_year = col_integer(), counties = col_integer(),
    .default = col_character()
  )
)
county_ref <- read_csv(
  file.path(crosswalk_dir, "02_counties.csv"),
  col_types = cols(county_km2 = col_double(), .default = col_character())
)
county_counts <- readRDS(file.path(counts_dir, "03_county_counts.rds"))
office_counts <- readRDS(file.path(counts_dir, "03_office_counts.rds"))
national_counts <- read_csv(
  file.path(counts_dir, "03_national_counts.csv"),
  col_types = cols(.default = col_integer())
)

cwa <- read_sf(file.path(reference_raw_dir, "w_16ap26"))
offices <- cwa |>
  st_drop_geometry() |>
  distinct(WFO, CITYSTATE) |>
  arrange(WFO)

# Guards -----------------------------------------------------------------------
stray_counties <- setdiff(county_counts$GEOID, county_ref$GEOID)
stray_offices <- setdiff(office_counts$WFO, offices$WFO)
if (length(stray_counties) > 0 || length(stray_offices) > 0) {
  print(c(stray_counties, stray_offices))
  stop("Counts above name counties or offices the outlines do not have.")
}

outside <- county_counts |> filter(!year %in% years)
if (nrow(outside) > 0) {
  print(count(outside, year))
  stop("Counts above fall outside ", first_year, "-", last_year, ".")
}

# Product Files ----------------------------------------------------------------
# Wide matrices: rows are units, columns years or months. Integer vectors in
# JSON are compact and compress well, and the browser indexes them directly.
unit_block <- function(counts, unit_col, unit_ids) {
  if (nrow(counts) == 0) {
    return(list(i = list(), ay = list(), dy = list(), am = list(), dm = list()))
  }

  by_year <- counts |>
    group_by(unit = .data[[unit_col]], year) |>
    summarise(alerts = sum(alerts), days = sum(days), .groups = "drop")
  by_month <- counts |>
    group_by(unit = .data[[unit_col]], month) |>
    summarise(alerts = sum(alerts), days = sum(days), .groups = "drop")

  units <- sort(unique(by_year$unit))
  index <- match(units, unit_ids)

  grid <- function(tbl, column, key, levels) {
    tbl |>
      mutate(unit = factor(unit, units), key = factor(.data[[key]], levels)) |>
      complete(unit, key, fill = set_names(list(0L), column)) |>
      arrange(unit, key) |>
      pull(all_of(column)) |>
      as.integer()
  }

  list(
    i = index - 1L,
    ay = grid(by_year, "alerts", "year", years),
    dy = grid(by_year, "days", "year", years),
    am = grid(by_month, "alerts", "month", 1:12),
    dm = grid(by_month, "days", "month", 1:12)
  )
}

national_block <- function(counts) {
  full <- expand_grid(year = years, month = 1:12) |>
    left_join(counts, by = c("year", "month")) |>
    mutate(across(c(alerts, days), \(x) coalesce(x, 0L)))
  list(a = full$alerts, d = full$days)
}

county_split <- split(county_counts, county_counts$product)
office_split <- split(office_counts, office_counts$product)
national_split <- split(national_counts, national_counts$product)
empty <- county_counts[0, ]

product_files <- map_chr(seq_len(nrow(products)), \(i) {
  p <- products[i, ]
  key <- as.character(p$id)

  payload <- list(
    id = p$product,
    national = national_block(national_split[[key]] %||% national_counts[0, ]),
    counties = unit_block(
      county_split[[key]] %||% empty, "GEOID", county_ref$GEOID
    ),
    offices = unit_block(
      office_split[[key]] %||% office_counts[0, ], "WFO", offices$WFO
    )
  )

  # The content hash in the name lets a host cache each file indefinitely.
  temp <- file.path(staging, "products", paste0(p$product, ".json"))
  write_file(jsonlite::toJSON(payload, auto_unbox = TRUE), temp)
  hash <- substr(unname(tools::md5sum(temp)), 1, 10)
  name <- paste0("products/", p$product, "_", hash, ".json")
  file.rename(temp, file.path(staging, name))
  name
})

# Outlines ---------------------------------------------------------------------
# Simplified for the browser: about 4% of the Census vertices keeps every
# county recognizable at state zoom, and 0.6% of the NWS office outlines, whose
# coastlines are far more detailed than their borders need. Four decimal places
# are 11 metres.
write_outline <- function(shape, name) {
  path <- file.path(staging, name)
  st_write(
    shape, path,
    layer_options = c("COORDINATE_PRECISION=4", "RFC7946=YES"),
    quiet = TRUE, delete_dsn = TRUE
  )
  invisible(name)
}

census_dir <- file.path(reference_raw_dir, "cb_2023_us_county_500k")
counties_sf <- read_sf(census_dir) |>
  select(GEOID, STATEFP) |>
  st_transform(4326)

counties_simple <- rmapshaper::ms_simplify(
  counties_sf, keep = 0.04, keep_shapes = TRUE, sys = FALSE
)

if (nrow(counties_simple) != nrow(county_ref) ||
    !setequal(counties_simple$GEOID, county_ref$GEOID)) {
  stop("Simplified counties no longer match 02_counties.csv one to one.")
}

counties_simple |>
  mutate(i = match(GEOID, county_ref$GEOID) - 1L) |>
  select(i) |>
  write_outline("counties.geojson")

states_simple <- rmapshaper::ms_dissolve(
  counties_simple, field = "STATEFP", sys = FALSE
) |>
  rmapshaper::ms_lines(sys = FALSE) |>
  select()
write_outline(states_simple, "states.geojson")

offices_simple <- cwa |>
  select(WFO) |>
  st_transform(4326) |>
  rmapshaper::ms_simplify(keep = 0.006, keep_shapes = TRUE, sys = FALSE) |>
  rmapshaper::ms_dissolve(field = "WFO", sys = FALSE) |>
  mutate(i = match(WFO, offices$WFO) - 1L) |>
  select(i)
write_outline(offices_simple, "offices.geojson")

# Grid -------------------------------------------------------------------------
grid_meta <- jsonlite::read_json(grid_meta_path, simplifyVector = TRUE)
grid_files <- read_csv(
  file.path(grid_dir, "04_grid_files.csv"),
  col_types = cols(bytes = col_integer(), max = col_integer(),
                   .default = col_character())
)

if (!identical(as.integer(grid_meta$years), years)) {
  stop("04's grid covers different years from 03's counts - rerun 04.")
}

gridded <- products |>
  filter(counties > 0) |>
  pull(product)
ungridded <- setdiff(gridded, grid_files$product)
if (length(ungridded) > 0) {
  print(ungridded)
  stop("Land products above have no grid files - rerun 04.")
}

grid_copy <- c(grid_files$file, "mask.bin.gz", "county.bin.gz")
missing_grid <- grid_copy[!file.exists(file.path(grid_dir, grid_copy))]
if (length(missing_grid) > 0) {
  print(missing_grid)
  stop("Grid files above are listed but not in ", grid_dir, ".")
}

dir.create(file.path(staging, "grid"))
invisible(file.copy(file.path(grid_dir, grid_copy), file.path(staging, "grid")))

grid_entry <- function(code) {
  f <- filter(grid_files, product == code)
  if (nrow(f) == 0) return(NULL)
  f |>
    split(f$measure) |>
    map(\(m) list(file = m$file, bytes = m$bytes, max = m$max))
}

# Manifest ---------------------------------------------------------------------
build <- format(Sys.time(), "%Y%m%d%H%M", tz = "UTC")

states <- county_ref |>
  distinct(STUSPS, STATE_NAME) |>
  arrange(STATE_NAME)

group_order <- c(
  "All alerts", "Severe storms", "Flooding", "Winter", "Heat and cold",
  "Frost and freeze", "Fire weather", "Wind", "Dust, fog and smoke",
  "Tropical", "Coastal and lakeshore", "Tsunami", "Marine", "Other"
)

unordered <- setdiff(products$group, group_order)
if (length(unordered) > 0) {
  print(unordered)
  stop("Product groups above are not in group_order.")
}

manifest <- list(
  build = build,
  built = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  years = years,
  rules = list(
    min_county_share = min_county_share,
    min_zone_share = min_zone_share
  ),
  groups = group_order,
  counties = list(
    geoid = county_ref$GEOID,
    name = county_ref$NAMELSAD,
    state = county_ref$STUSPS,
    office = county_ref$CWA,
    km2 = round(county_ref$county_km2)
  ),
  states = list(abbr = states$STUSPS, name = states$STATE_NAME),
  offices = list(wfo = offices$WFO, name = offices$CITYSTATE),
  grid = list(
    cell_m = grid_meta$cell_m,
    cells = grid_meta$cells,
    domains = grid_meta$domains,
    mask = "mask.bin.gz",
    county = "county.bin.gz"
  ),
  products = products |>
    mutate(file = product_files) |>
    transmute(
      id = product, kind, phenom = PHENOM, sig = SIG, label, group,
      note = coalesce(note, ""),
      first = first_year, last = last_year, alerts, counties, file,
      grid = map(product, grid_entry)
    )
)

jsonlite::write_json(
  manifest, file.path(staging, "manifest.json"),
  auto_unbox = TRUE, digits = NA
)

unlink(out, recursive = TRUE)
invisible(file.rename(staging, out))

site_files <- list.files(out, recursive = TRUE)
message(
  "Site data: ", nrow(products), " products, ",
  round(sum(file.size(file.path(out, site_files))) / 1e6, 1), " MB, build ",
  build
)
