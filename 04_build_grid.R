library(tidyverse)
library(sf)

source(here::here("00_paths.R"))

# Gridded Counts ---------------------------------------------------------------
# The same two measures as 03 - days in effect and alerts issued, by product
# and year - for every 5 km land cell instead of every county.
#
# Where an alert was drawn matters here. Storm-based warnings (tornado, severe
# thunderstorm, flash flood, most flood products, snow squall, dust storm)
# carry a polygon, and a cell counts an alert when the polygon covers the
# cell's centre, at the polygon's own times: a warning trimmed by a later
# statement stops covering the trimmed-off cells when it was trimmed. Every
# other alert covers the cells of the zones and counties it names, using the
# same zone outlines 02 matched to counties.
#
# Most cells in one zone share everything, so zone-based alerts are counted
# once per group of identical cells (same zones and counties, same time zone)
# and copied to its cells. Polygon alerts are counted per cell. The two meet
# where a product has both kinds of alert - a river flood warning with a
# polygon in 2018 and without one in 2012 - and a day both cover is counted
# once.
#
# Writes outputs/04_grid/:
#   grid.json            domains, projection parameters, cell counts
#   mask.bin.gz          which slots of each domain are land cells
#   county.bin.gz        the county of each cell (index into 02_counties.csv)
#   <product>_<measure>_<hash>.bin.gz  one value per cell per year
#   04_grid_files.csv    the files above, by product and measure
#   04_grid_cells.csv    every cell's domain, row, column, centre and county

out <- file.path(outputs, "04_grid")
crosswalk_dir <- file.path(outputs, "02_crosswalk")
counts_dir <- file.path(outputs, "03_counts")

cores <- max(1, parallel::detectCores() - 2)
sf_use_s2(FALSE)

years <- first_year:last_year
n_years <- length(years)

needed <- c(
  file.path(crosswalk_dir, c("02_counties.csv", "02_ugc_county.csv",
                             "02_ugc_outlines.rds")),
  file.path(counts_dir, c("03_products.csv", "03_ceilings.csv",
                          "03_rows.rds"))
)
if (!all(file.exists(needed))) {
  print(needed[!file.exists(needed)])
  stop("Inputs above are missing - run 02 and 03 first.")
}

county_ref <- read_csv(
  file.path(crosswalk_dir, "02_counties.csv"),
  col_types = cols(county_km2 = col_double(), .default = col_character())
)
crosswalk <- read_csv(
  file.path(crosswalk_dir, "02_ugc_county.csv"),
  col_types = cols(.default = col_character())
)
outlines <- readRDS(file.path(crosswalk_dir, "02_ugc_outlines.rds"))
products <- read_csv(
  file.path(counts_dir, "03_products.csv"),
  col_types = cols(
    id = col_integer(), alerts = col_integer(), first_year = col_integer(),
    last_year = col_integer(), counties = col_integer(),
    .default = col_character()
  )
)
ceilings <- read_csv(
  file.path(counts_dir, "03_ceilings.csv"),
  col_types = cols(ceiling_days = col_double(), .default = col_character())
)

# Local days, as in 03: whole days since 1970-01-01 in a given time zone.
local_day <- function(time, tz) {
  day <- integer(length(time))
  for (zone in unique(tz)) {
    in_zone <- tz == zone
    day[in_zone] <- as.integer(as_date(with_tz(time[in_zone], zone)))
  }
  day
}

first_day <- as.integer(as.Date(paste0(first_year, "-01-01")))
last_day <- as.integer(as.Date(paste0(last_year, "-12-31")))
day_year <- function(day) as.POSIXlt(as.Date(day))$year + 1900L

# Cells ------------------------------------------------------------------------
counties <- read_sf(file.path(reference_raw_dir, "cb_2023_us_county_500k")) |>
  select(GEOID, STUSPS) |>
  mutate(county = match(GEOID, county_ref$GEOID))

if (anyNA(counties$county)) {
  stop("Census counties and 02_counties.csv no longer match one to one.")
}

state_domains <- grid_domains |>
  filter(!is.na(states)) |>
  separate_longer_delim(states, " ")
counties$domain <- coalesce(
  state_domains$domain[match(counties$STUSPS, state_domains$states)],
  "conus"
)

build_domain <- function(k) {
  d <- grid_domains[k, ]
  crs <- grid_proj(d$lat_0, d$lon_0, d$lat_1, d$lat_2)
  shapes <- counties |>
    filter(domain == d$domain) |>
    st_transform(crs)

  bb <- st_bbox(shapes)
  x0 <- floor(bb[["xmin"]] / grid_cell_m) * grid_cell_m
  x1 <- ceiling(bb[["xmax"]] / grid_cell_m) * grid_cell_m
  y0 <- floor(bb[["ymin"]] / grid_cell_m) * grid_cell_m
  y1 <- ceiling(bb[["ymax"]] / grid_cell_m) * grid_cell_m
  nx <- as.integer(round((x1 - x0) / grid_cell_m))
  ny <- as.integer(round((y1 - y0) / grid_cell_m))

  # Slots run row by row from the top left, the order the browser reads them.
  slots <- expand_grid(row = seq_len(ny) - 1L, col = seq_len(nx) - 1L) |>
    mutate(
      x = x0 + (col + 0.5) * grid_cell_m,
      y = y1 - (row + 0.5) * grid_cell_m
    )
  centres <- st_as_sf(slots, coords = c("x", "y"), crs = crs, remove = FALSE)
  hit <- st_intersects(centres, shapes)
  land <- lengths(hit) > 0

  lonlat <- st_coordinates(st_transform(centres[land, ], 4326))

  list(
    meta = tibble(
      domain = d$domain, lat_0 = d$lat_0, lon_0 = d$lon_0, lat_1 = d$lat_1,
      lat_2 = d$lat_2, x0 = x0, y_top = y1, nx = nx, ny = ny,
      cells = sum(land)
    ),
    land = land,
    cells = slots[land, ] |>
      transmute(
        domain = d$domain, row, col,
        lon = lonlat[, 1], lat = lonlat[, 2],
        county = shapes$county[map_int(hit[land], 1)]
      )
  )
}

started <- Sys.time()
domains <- parallel::mclapply(
  seq_len(nrow(grid_domains)), build_domain,
  mc.cores = min(cores, nrow(grid_domains))
)
failed <- keep(domains, \(d) inherits(d, "try-error"))
if (length(failed) > 0) stop(failed[[1]])

domain_meta <- map(domains, "meta") |>
  bind_rows() |>
  mutate(first_cell = cumsum(lag(cells, default = 0L)))

cells <- map(domains, "cells") |>
  bind_rows() |>
  mutate(
    cell = row_number() - 1L,
    tz = county_ref$tz[county]
  )
n_cells <- nrow(cells)

message(
  "Grid: ", format(n_cells, big.mark = ","), " land cells in ",
  nrow(domain_meta), " domains (",
  paste0(domain_meta$domain, " ", domain_meta$cells, collapse = ", "), ") in ",
  round(as.numeric(difftime(Sys.time(), started, units = "secs"))), " s"
)

# A county smaller than a cell can hold no cell centre. Its alerts still count
# in the county view; on the grid they fall to the surrounding cells.
cellless <- setdiff(seq_len(nrow(county_ref)), cells$county)
message(
  "Counties with no cell centre: ", length(cellless),
  if (length(cellless) > 0) {
    paste0(" (", paste(head(county_ref$NAMELSAD[cellless], 6), collapse = ", "),
           if (length(cellless) > 6) ", ..." else "", ")")
  }
)

cell_points <- cells |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) |>
  st_transform(area_crs)

# Zone Cells -------------------------------------------------------------------
keys <- crosswalk |>
  distinct(NWS_UGC, AREA_KM2, method) |>
  mutate(key = row_number())

fips_cells <- crosswalk |>
  filter(method == "fips") |>
  left_join(select(keys, NWS_UGC, AREA_KM2, key),
            by = c("NWS_UGC", "AREA_KM2")) |>
  mutate(county = match(GEOID, county_ref$GEOID)) |>
  inner_join(select(cells, cell, county), by = "county",
             relationship = "many-to-many") |>
  select(key, cell)

shaped <- outlines |>
  inner_join(
    filter(keys, method != "fips") |> select(NWS_UGC, AREA_KM2, key, method),
    by = c("NWS_UGC", "AREA_KM2")
  )

unshaped <- keys |>
  filter(method != "fips") |>
  anti_join(st_drop_geometry(shaped), by = "key")
if (nrow(unshaped) > 0) {
  print(unshaped)
  stop("Crosswalk keys above have no outline in 02_ugc_outlines.rds.")
}

shape_cells <- shaped |>
  mutate(chunk = ceiling(row_number() / 400)) |>
  group_split(chunk) |>
  parallel::mclapply(\(chunk) {
    hit <- st_intersects(chunk, cell_points)
    tibble(key = rep(chunk$key, lengths(hit)), cell = cells$cell[unlist(hit)])
  }, mc.cores = cores)
failed <- keep(shape_cells, \(x) inherits(x, "try-error"))
if (length(failed) > 0) stop(failed[[1]])
shape_cells <- bind_rows(shape_cells)

# An outline holding no cell centre is a fragment (02 gave it the counties of
# its code's largest outline, so it takes that outline's cells) or a zone
# smaller than a cell, which takes the nearest cell within 5 km. Anything
# farther - an islet with no land cell near it - is off the grid.
empty <- shaped |>
  filter(!key %in% shape_cells$key)

largest <- shaped |>
  st_drop_geometry() |>
  filter(key %in% shape_cells$key) |>
  group_by(NWS_UGC) |>
  slice_max(as.numeric(AREA_KM2), n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(NWS_UGC, donor = key)

borrowed <- empty |>
  st_drop_geometry() |>
  filter(method == "same code") |>
  inner_join(largest, by = "NWS_UGC") |>
  inner_join(shape_cells, by = c("donor" = "key"),
             relationship = "many-to-many") |>
  select(key, cell)

small <- filter(empty, !key %in% borrowed$key)
nearest <- tibble(key = integer(), cell = integer())
if (nrow(small) > 0) {
  inside <- st_point_on_surface(st_geometry(small))
  near <- st_nearest_feature(inside, cell_points)
  near_km <- as.numeric(
    st_distance(inside, cell_points[near, ], by_element = TRUE)
  ) / 1000
  nearest <- tibble(key = small$key, cell = cells$cell[near])[near_km <= 5, ]
}

off_grid <- setdiff(empty$key, c(borrowed$key, nearest$key))
message(
  "Zone cells: ", nrow(empty), " outlines hold no cell centre; ",
  n_distinct(borrowed$key), " take their code's largest outline, ",
  nrow(nearest), " the nearest cell, ", length(off_grid), " are off the grid",
  if (length(off_grid) > 0) {
    paste0(" (", paste(keys$NWS_UGC[off_grid], collapse = ", "), ")")
  }
)

key_cells <- bind_rows(fips_cells, shape_cells, borrowed, nearest) |>
  distinct(key, cell)

# Groups of identical cells: the same keys and the same time zone.
cell_keys <- key_cells |>
  arrange(cell, key) |>
  group_by(cell) |>
  summarise(keys = paste(key, collapse = " "), .groups = "drop")

cells <- cells |>
  left_join(cell_keys, by = "cell") |>
  mutate(
    group_key = paste(tz, coalesce(keys, "")),
    group = match(group_key, unique(group_key))
  ) |>
  select(-keys, -group_key)
n_groups <- max(cells$group)

key_groups <- key_cells |>
  mutate(group = cells$group[cell + 1L]) |>
  distinct(key, group)

group_tz <- cells |>
  distinct(group, tz) |>
  arrange(group) |>
  pull(tz)

message(
  "Groups: ", format(n_groups, big.mark = ","), " groups of identical cells, ",
  format(nrow(key_groups), big.mark = ","), " key-group pairs"
)

# Products ---------------------------------------------------------------------
# Marine products reach no county and are not gridded; the "any" products
# include only the land products.
grid_products <- filter(products, counties > 0)

specific_ids <- products |>
  filter(kind == "product") |>
  transmute(
    PHENOM, SIG, id,
    phenomenon_id = products$id[match(paste0(PHENOM, ".all"),
                                      products$product)],
    significance_id = products$id[match(paste0("all.", SIG),
                                        products$product)]
  )

on_grid <- function(ids) ifelse(ids %in% grid_products$id, ids, NA_integer_)

# Polygons ---------------------------------------------------------------------
parts <- archive_parts()

read_polygons <- function(i) {
  p <- parts[i, ]

  # Attributes come from the CSV: read through GDAL the DBF's ETN field is
  # empty for polygon rows. The CSV and shapefile hold the same rows in the same
  # order (checked in 02), so the polygons, read in file order, line up with the
  # CSV's polygon rows; WFO and PHENOM are compared to be sure.
  attributes <- read_csv(
    p$csv,
    col_select = c(WFO, ISSUED, EXPIRED, PHENOM, GTYPE, SIG, ETN, POLYBEGIN,
                   POLYEND),
    col_types = cols(.default = col_character())
  ) |>
    filter(GTYPE == "P")

  query <- paste0(
    "SELECT WFO, PHENOM FROM \"", p$stem, "\" WHERE GTYPE = 'P'"
  )
  shapes <- read_sf(p$dir, query = query)

  if (nrow(shapes) != nrow(attributes) ||
      !identical(shapes$WFO, attributes$WFO) ||
      !identical(shapes$PHENOM, attributes$PHENOM)) {
    stop(p$part, ": shapefile polygons do not line up with the CSV's.")
  }

  polys <- st_sf(
    select(attributes, -GTYPE),
    geometry = st_geometry(shapes)
  ) |>
    mutate(
      part = p$part,
      file_year = p$year,
      issued = ymd_hm(ISSUED, tz = "UTC", quiet = TRUE),
      expired = ymd_hm(EXPIRED, tz = "UTC", quiet = TRUE),
      begin = coalesce(ymd_hm(POLYBEGIN, tz = "UTC", quiet = TRUE), issued),
      end = coalesce(ymd_hm(POLYEND, tz = "UTC", quiet = TRUE), expired)
    )

  if (anyNA(polys$begin) || anyNA(polys$end)) {
    stop(p$part, ": polygon times that do not parse.")
  }

  polys <- polys |>
    inner_join(specific_ids, by = c("PHENOM", "SIG")) |>
    filter(id %in% grid_products$id) |>
    left_join(ceilings, by = c("PHENOM", "SIG")) |>
    mutate(
      end = if_else(
        !is.na(ceiling_days) & end > begin + ceiling_days * 86400,
        begin + ceiling_days * 86400,
        end
      ),
      phenomenon_id = on_grid(phenomenon_id),
      significance_id = on_grid(significance_id)
    ) |>
    st_transform(area_crs)

  hit <- st_intersects(polys, cell_points)

  # A polygon narrower than a cell may hold no centre: it takes the cell
  # nearest its interior, if that is land within 5 km.
  empty <- which(lengths(hit) == 0)
  placed <- 0L
  if (length(empty) > 0) {
    inside <- suppressWarnings(
      st_point_on_surface(st_make_valid(st_geometry(polys)[empty]))
    )
    near <- st_nearest_feature(inside, cell_points)
    near_km <- as.numeric(
      st_distance(inside, cell_points[near, ], by_element = TRUE)
    ) / 1000
    for (j in which(near_km <= 5)) hit[[empty[j]]] <- near[j]
    placed <- sum(near_km <= 5)
  }

  list(
    polys = st_drop_geometry(polys) |>
      select(WFO, PHENOM, SIG, ETN, file_year, begin, end, id, phenomenon_id,
             significance_id),
    cells = hit,
    empty = length(empty),
    placed = placed
  )
}

started <- Sys.time()
polygon_parts <- parallel::mclapply(
  seq_len(nrow(parts)), read_polygons, mc.cores = cores
)
failed <- keep(polygon_parts, \(x) inherits(x, "try-error"))
if (length(failed) > 0) stop(failed[[1]])

polys <- map(polygon_parts, "polys") |>
  bind_rows() |>
  mutate(
    event = paste(WFO, PHENOM, SIG, ETN, file_year),
    event_id = match(event, unique(event))
  )
poly_hits <- do.call(c, map(polygon_parts, "cells"))
poly_pairs <- tibble(
  poly = rep(seq_along(poly_hits), lengths(poly_hits)),
  cell = cells$cell[unlist(poly_hits)]
)

message(
  "Polygons: ", format(nrow(polys), big.mark = ","), " on land products, ",
  format(nrow(poly_pairs), big.mark = ","), " polygon-cell pairs; ",
  sum(map_int(polygon_parts, "empty")), " held no cell centre, ",
  sum(map_int(polygon_parts, "placed")), " placed in the nearest cell, in ",
  round(as.numeric(difftime(Sys.time(), started, units = "mins")), 1),
  " minutes"
)

# Zone Rows --------------------------------------------------------------------
# Rows of alerts that have a polygon are counted by the polygon alone.
rows <- readRDS(file.path(counts_dir, "03_rows.rds")) |>
  mutate(event = paste(WFO, PHENOM, SIG, ETN, file_year))

# Every polygon belongs to an alert that also has zone or county rows. If the
# two stop matching - a field read differently - polygon alerts would be
# counted twice, once from each, without any error.
orphans <- setdiff(unique(polys$event), rows$event)
if (length(orphans) > 0.001 * n_distinct(polys$event)) {
  print(head(orphans, 20))
  stop(length(orphans), " polygon alerts match no zone or county rows.")
}

rows <- rows |>
  filter(!event %in% polys$event) |>
  mutate(key = keys$key[match(paste(NWS_UGC, AREA_KM2),
                              paste(keys$NWS_UGC, keys$AREA_KM2))]) |>
  filter(!is.na(key)) |>
  mutate(
    event_id = match(event, unique(event)),
    id = on_grid(id),
    phenomenon_id = on_grid(phenomenon_id),
    significance_id = on_grid(significance_id)
  ) |>
  arrange(key)

key_first <- match(seq_len(nrow(keys)), rows$key)
key_n <- tabulate(rows$key, nbins = nrow(keys))

message(
  "Zone rows: ", format(nrow(rows), big.mark = ","),
  " land rows of alerts without a polygon"
)

# Counting ---------------------------------------------------------------------
# Unique (unit, product, day) triples packed into one double: unit below 1e6,
# product below 1000, day below 1e5.
pack <- function(unit, product, day) (unit * 1000 + product) * 1e5 + day

expand <- function(start_day, end_day) {
  n <- pmax(end_day - start_day + 1L, 0L)
  index <- rep.int(seq_along(n), n)
  day <- start_day[index] + sequence(n) - 1L
  keep <- day >= first_day & day <= last_day
  list(index = index[keep], day = day[keep])
}

product_columns <- c("id", "phenomenon_id", "significance_id")

# Days per unit, product and year from packed keys.
tally_keys <- function(keys) {
  if (length(keys) == 0) {
    return(tibble(unit = integer(), product = integer(), year = integer(),
                  n = integer()))
  }
  tibble(
    unit = as.integer(keys %/% 1e8),
    product = as.integer((keys %/% 1e5) %% 1000),
    year = day_year(as.integer(keys %% 1e5))
  ) |>
    count(unit, product, year)
}

# The packed keys of every product column for expanded rows.
day_keys <- function(unit, day, index, ids) {
  map(product_columns, \(column) {
    product <- ids[[column]][index]
    has <- !is.na(product)
    pack(unit[has], product[has], day[has])
  }) |>
    unlist() |>
    unique()
}

# Alerts per unit, product and year: each (unit, event) once, dated by the
# unit's first day under it.
tally_alerts <- function(unit, event, first_time, tz, ids) {
  if (length(unit) == 0) {
    return(tibble(unit = integer(), product = integer(), year = integer(),
                  n = integer()))
  }
  o <- order(first_time)
  firsts <- o[!duplicated((unit * 2^22 + event)[o])]
  day <- local_day(first_time[firsts], tz[firsts])
  keep <- day >= first_day & day <= last_day
  firsts <- firsts[keep]
  year <- day_year(day[keep])

  map(product_columns, \(column) {
    tibble(unit = unit[firsts], product = ids[[column]][firsts], year = year)
  }) |>
    bind_rows() |>
    filter(!is.na(product)) |>
    count(unit, product, year)
}

n_chunks <- 96
cell_group <- cells$group
cell_tz <- cells$tz

count_chunk <- function(k) {
  # Zone-based alerts, by group.
  kg <- key_groups[key_groups$group %% n_chunks == k, ]
  n <- key_n[kg$key]
  idx <- rep(key_first[kg$key], n) + sequence(n) - 1L
  group <- rep(kg$group, n)
  tz <- group_tz[group]
  issued <- rows$issued[idx]
  expired <- rows$expired[idx]
  ids <- list(
    id = rows$id[idx],
    phenomenon_id = rows$phenomenon_id[idx],
    significance_id = rows$significance_id[idx]
  )

  on <- which(expired > issued)
  expanded <- expand(
    local_day(issued[on], tz[on]),
    local_day(expired[on] - 60, tz[on])
  )
  z_keys <- day_keys(
    group[on][expanded$index], expanded$day, on[expanded$index], ids
  )
  z_days <- tally_keys(z_keys) |> rename(group = unit, days = n)
  z_alerts <- tally_alerts(group, rows$event_id[idx], issued, tz, ids) |>
    rename(group = unit, alerts = n)

  # Polygon alerts, by cell.
  pp <- poly_pairs[cell_group[poly_pairs$cell + 1L] %% n_chunks == k, ]
  cell <- pp$cell
  ptz <- cell_tz[cell + 1L]
  begin <- polys$begin[pp$poly]
  end <- polys$end[pp$poly]
  pids <- list(
    id = polys$id[pp$poly],
    phenomenon_id = polys$phenomenon_id[pp$poly],
    significance_id = polys$significance_id[pp$poly]
  )

  on <- which(end > begin)
  expanded <- expand(
    local_day(begin[on], ptz[on]),
    local_day(end[on] - 60, ptz[on])
  )
  p_keys <- day_keys(
    cell[on][expanded$index], expanded$day, on[expanded$index], pids
  )
  p_days <- tally_keys(p_keys) |> rename(cell = unit, days = n)

  # Days a cell has under both kinds of alert, counted once.
  p_cell <- p_keys %/% 1e8
  shared <- p_keys[
    pack(cell_group[p_cell + 1], (p_keys %/% 1e5) %% 1000, p_keys %% 1e5) %in%
      z_keys
  ]
  p_shared <- tally_keys(shared) |> rename(cell = unit, shared = n)

  p_alerts <- tally_alerts(cell, polys$event_id[pp$poly], begin, ptz, pids) |>
    rename(cell = unit, alerts = n)

  list(
    zone = full_join(z_days, z_alerts, by = c("group", "product", "year")),
    polygon = p_days |>
      full_join(p_shared, by = c("cell", "product", "year")) |>
      full_join(p_alerts, by = c("cell", "product", "year"))
  )
}

started <- Sys.time()
counted <- parallel::mclapply(0:(n_chunks - 1), count_chunk, mc.cores = cores)
failed <- keep(counted, \(x) inherits(x, "try-error"))
if (length(failed) > 0) stop(failed[[1]])

zone_counts <- map(counted, "zone") |>
  bind_rows() |>
  mutate(across(c(days, alerts), \(x) coalesce(x, 0L)))
polygon_counts <- map(counted, "polygon") |>
  bind_rows() |>
  mutate(across(c(days, shared, alerts), \(x) coalesce(x, 0L)))
rm(counted)

message(
  "Counts: ", format(nrow(zone_counts), big.mark = ","), " group rows, ",
  format(nrow(polygon_counts), big.mark = ","), " cell rows in ",
  round(as.numeric(difftime(Sys.time(), started, units = "mins")), 1),
  " minutes"
)

# Files ------------------------------------------------------------------------
staging <- paste0(out, ".next")
unlink(staging, recursive = TRUE)
dir.create(staging, recursive = TRUE)

# Values are written year by year, each year a run of one value per cell, as
# unsigned 8-bit integers when every value fits and 16-bit otherwise, then
# gzipped: the browser inflates them with DecompressionStream. Neighbouring
# cells mostly share values, which is what makes them compress.
write_values <- function(values, stem) {
  bytes <- if (max(values) <= 255) 1L else 2L
  temp <- file.path(staging, paste0(stem, ".bin.gz"))
  con <- gzfile(temp, "wb", compression = 9)
  writeBin(as.integer(values), con, size = bytes, endian = "little")
  close(con)
  hash <- substr(unname(tools::md5sum(temp)), 1, 10)
  name <- paste0(stem, "_", hash, ".bin.gz")
  file.rename(temp, file.path(staging, name))
  tibble(file = name, bytes = bytes, max = max(values))
}

zone_split <- split(zone_counts, zone_counts$product)
polygon_split <- split(polygon_counts, polygon_counts$product)

build_product <- function(pid) {
  code <- products$product[products$id == pid]
  z <- zone_split[[as.character(pid)]]
  p <- polygon_split[[as.character(pid)]]

  grids <- map(c(days = "days", alerts = "alerts"), \(measure) {
    by_group <- matrix(0L, n_groups, n_years)
    if (!is.null(z)) {
      by_group[cbind(z$group, z$year - first_year + 1L)] <- z[[measure]]
    }
    values <- by_group[cell_group, , drop = FALSE]
    if (!is.null(p)) {
      at <- cbind(p$cell + 1L, p$year - first_year + 1L)
      added <- if (measure == "days") p$days - p$shared else p$alerts
      values[at] <- values[at] + added
    }
    values
  })

  bad_days <- sum(grids$days > 366 | grids$days < 0)
  if (bad_days > 0) {
    stop(code, ": ", bad_days, " cell-years with impossible day counts.")
  }

  imap(grids, \(values, measure) {
    write_values(values, paste0(code, "_", measure)) |>
      mutate(product = code, measure = measure)
  }) |>
    bind_rows()
}

started <- Sys.time()
files <- parallel::mclapply(grid_products$id, build_product,
                            mc.cores = min(cores, 8))
failed <- keep(files, \(x) inherits(x, "try-error"))
if (length(failed) > 0) stop(failed[[1]])
files <- bind_rows(files)

# Masks: one bit per slot, row by row, least significant bit first.
mask_con <- gzfile(file.path(staging, "mask.bin.gz"), "wb", compression = 9)
for (d in domains) {
  writeBin(packBits(c(d$land, rep(FALSE, (-length(d$land)) %% 8))), mask_con)
}
close(mask_con)

county_con <- gzfile(file.path(staging, "county.bin.gz"), "wb",
                     compression = 9)
writeBin(cells$county - 1L, county_con, size = 2, endian = "little")
close(county_con)

jsonlite::write_json(
  list(
    cell_m = grid_cell_m,
    cells = n_cells,
    years = years,
    domains = domain_meta
  ),
  file.path(staging, "grid.json"),
  auto_unbox = TRUE, digits = NA
)

write_csv(files, file.path(staging, "04_grid_files.csv"))
write_csv(
  cells |>
    transmute(cell, domain, row, col, lon = round(lon, 5), lat = round(lat, 5),
              GEOID = county_ref$GEOID[county]),
  file.path(staging, "04_grid_cells.csv")
)

unlink(out, recursive = TRUE)
invisible(file.rename(staging, out))

message(
  "Grid files: ", nrow(files), " for ", n_distinct(files$product),
  " products, ", round(sum(file.size(file.path(out, files$file))) / 1e6, 1),
  " MB, in ",
  round(as.numeric(difftime(Sys.time(), started, units = "mins")), 1),
  " minutes"
)
