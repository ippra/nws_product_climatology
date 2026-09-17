library(tidyverse)
library(sf)

source(here::here("00_paths.R"))

# UGC-County Crosswalk ---------------------------------------------------------
# Every alert row names a zone or county by its NWS UGC code (OKZ025, OKC027).
# This script decides which Census counties each code covers, so 03 can count
# alerts by county without any geometry.
#
# County codes name their county directly: OKC027 is Oklahoma's county 027,
# GEOID 40027. Zone codes, and county codes for counties the 2023 Census no
# longer has (Connecticut's eight counties, Bedford city VA), are matched by
# area overlap with IEM's own outline of that code, which travels with each row
# in the yearly shapefile. Zones are redrawn over time, so the key is the code
# and its area together: AKZ317 appears at 21,164 and 20,374 square km in 2024.
#
# Writes outputs/02_crosswalk/:
#   02_counties.csv     every Census county with its time zone and NWS office
#   02_ugc_county.csv   every (UGC, area) key in the archive and its counties
#   02_ugc_outlines.rds IEM's outline of every key matched by overlap

out <- file.path(outputs, "02_crosswalk")
dir.create(out, recursive = TRUE, showWarnings = FALSE)

cores <- max(1, parallel::detectCores() - 2)
sf_use_s2(FALSE)

# Counties ---------------------------------------------------------------------
counties <- read_sf(file.path(reference_raw_dir, "cb_2023_us_county_500k")) |>
  select(GEOID, NAME, NAMELSAD, STUSPS, STATE_NAME) |>
  st_transform(area_crs) |>
  st_make_valid()
counties$county_km2 <- as.numeric(st_area(counties)) / 1e6

nws_counties <- read_sf(file.path(reference_raw_dir, "c_16ap26")) |>
  select(NWS_FIPS = FIPS, CWA, TIME_ZONE) |>
  st_transform(area_crs) |>
  st_make_valid()

# Time zone and forecast office come from the NWS county the Census county's
# interior point falls in. Matching by point rather than FIPS carries them
# across to Connecticut's planning regions and Kalawao County, which the NWS
# file does not list by those codes.
# The Census outline includes coastal water the NWS outline does not, so a
# point can land offshore (Plaquemines Parish, Monroe County FL); those take the
# nearest NWS county, provided it is close.
interior <- st_point_on_surface(st_geometry(counties))
nws_match <- st_intersects(interior, nws_counties) |>
  map_int(\(hit) if (length(hit) > 0) hit[1] else NA_integer_)

offshore <- which(is.na(nws_match))
nearest <- st_nearest_feature(interior[offshore], nws_counties)
nearest_km <- as.numeric(
  st_distance(interior[offshore], nws_counties[nearest, ], by_element = TRUE)
) / 1000

if (any(nearest_km > 25)) {
  print(tibble(
    GEOID = counties$GEOID[offshore],
    nearest = nws_counties$NWS_FIPS[nearest],
    km = nearest_km
  ))
  stop("Counties above are more than 25 km from any NWS county.")
}
nws_match[offshore] <- nearest
message(
  "Time zones: ", length(offshore), " counties matched to the nearest NWS ",
  "county, at most ", round(max(c(0, nearest_km)), 1), " km away"
)

county_ref <- counties |>
  st_drop_geometry() |>
  mutate(
    TIME_ZONE = nws_counties$TIME_ZONE[nws_match],
    CWA = nws_counties$CWA[nws_match],
    tz = unname(nws_time_zones[substr(TIME_ZONE, 1, 1)])
  )

unzoned <- filter(county_ref, is.na(tz))
if (nrow(unzoned) > 0) {
  print(unzoned)
  stop("NWS time zone codes above are not in nws_time_zones (00_paths.R).")
}

# 17 NWS counties are split between offices, written as the codes run together
# ("STOREV" for Sierra County CA). The county card names the first.
county_ref <- county_ref |>
  mutate(CWA = str_extract(CWA, "^[A-Z]{3}")) |>
  mutate(county_km2 = round(county_km2, 2)) |>
  select(GEOID, NAME, NAMELSAD, STUSPS, STATE_NAME, county_km2, tz, CWA) |>
  arrange(GEOID)

message(
  "Counties: ", nrow(county_ref), ", ", n_distinct(county_ref$CWA),
  " offices, ", n_distinct(county_ref$tz), " time zones"
)

# Keys in the Archive ----------------------------------------------------------
parts <- archive_parts()

if (!all(file.exists(parts$csv))) {
  print(parts$csv[!file.exists(parts$csv)])
  stop("Archive files above are missing - run 01_refresh_data.R first.")
}

state_fips <- tigris::fips_codes |>
  distinct(state, state_code) |>
  deframe()

# The CSV and the shapefile hold the same rows in the same order, which is what
# lets a CSV row number fetch that row's outline. Checked against the DBF on
# every part, because a mismatch would attach outlines to the wrong zones.
read_keys <- function(i) {
  p <- parts[i, ]
  rows <- read_csv(
    p$csv,
    col_select = c(GTYPE, NWS_UGC, AREA_KM2),
    col_types = cols(.default = col_character())
  )
  dbf <- foreign::read.dbf(sub("\\.csv$", ".dbf", p$csv), as.is = TRUE)

  aligned <- nrow(dbf) == nrow(rows) &&
    identical(coalesce(dbf$NWS_UGC, ""), coalesce(rows$NWS_UGC, "")) &&
    identical(dbf$GTYPE, rows$GTYPE)
  if (!aligned) {
    stop("CSV and shapefile rows differ for ", p$part, ".")
  }

  rows |>
    mutate(fid = row_number() - 1L, part = i) |>
    filter(GTYPE == "C") |>
    distinct(NWS_UGC, AREA_KM2, .keep_all = TRUE) |>
    select(NWS_UGC, AREA_KM2, part, fid)
}

keys <- parallel::mclapply(seq_len(nrow(parts)), read_keys, mc.cores = cores)
failed <- keep(keys, \(k) inherits(k, "try-error"))
if (length(failed) > 0) stop(failed[[1]])

keys <- bind_rows(keys) |>
  distinct(NWS_UGC, AREA_KM2, .keep_all = TRUE) |>
  mutate(
    prefix = substr(NWS_UGC, 1, 2),
    type = substr(NWS_UGC, 3, 3),
    geoid = paste0(state_fips[prefix], substr(NWS_UGC, 4, 6)),
    method = case_when(
      prefix %in% marine_prefixes                     ~ "marine",
      prefix %in% no_county_prefixes                  ~ "no county",
      type == "C" & geoid %in% county_ref$GEOID       ~ "fips",
      TRUE                                            ~ "overlap"
    )
  )

bad_codes <- filter(keys, !str_detect(NWS_UGC, "^[A-Z]{2}[CZ]\\d{3}$"))
if (nrow(bad_codes) > 0) {
  print(bad_codes)
  stop("UGC codes above are not of the form SSC000 or SSZ000.")
}

count(keys, method)

# Overlap ----------------------------------------------------------------------
to_overlap <- filter(keys, method == "overlap")

fetch_outlines <- function(part_keys) {
  p <- parts[part_keys$part[1], ]
  layer <- p$stem
  chunks <- split(part_keys, ceiling(seq_len(nrow(part_keys)) / 400))

  map(chunks, \(chunk) {
    query <- paste0(
      "SELECT NWS_UGC, AREA_KM2 FROM \"", layer, "\" WHERE FID IN (",
      paste(chunk$fid, collapse = ","), ")"
    )
    outlines <- read_sf(p$dir, query = query)
    if (!identical(outlines$NWS_UGC, chunk$NWS_UGC)) {
      stop("Outlines fetched for ", p$part, " do not match their keys.")
    }
    outlines$AREA_KM2 <- chunk$AREA_KM2
    outlines
  }) |>
    bind_rows()
}

started <- Sys.time()
outlines <- to_overlap |>
  arrange(part, fid) |>
  group_split(part) |>
  parallel::mclapply(fetch_outlines, mc.cores = cores)

failed <- keep(outlines, \(o) inherits(o, "try-error"))
if (length(failed) > 0) stop(failed[[1]])

outlines <- bind_rows(outlines) |>
  st_transform(area_crs) |>
  st_make_valid()
outlines$zone_km2 <- as.numeric(st_area(outlines)) / 1e6

overlaps <- outlines |>
  mutate(chunk = ceiling(row_number() / 250)) |>
  group_split(chunk) |>
  parallel::mclapply(\(chunk) {
    x <- suppressWarnings(st_intersection(
      select(chunk, NWS_UGC, AREA_KM2, zone_km2),
      select(counties, GEOID, county_km2)
    ))
    x$overlap_km2 <- as.numeric(st_area(x)) / 1e6
    st_drop_geometry(x)
  }, mc.cores = cores)

failed <- keep(overlaps, \(o) inherits(o, "try-error"))
if (length(failed) > 0) stop(failed[[1]])

overlaps <- bind_rows(overlaps) |>
  mutate(
    share_county = overlap_km2 / county_km2,
    share_zone = overlap_km2 / zone_km2
  )

message(
  "Overlap: ", nrow(outlines), " outlines against counties in ",
  round(as.numeric(difftime(Sys.time(), started, units = "mins")), 1),
  " minutes"
)

# Crosswalk --------------------------------------------------------------------
matched <- overlaps |>
  filter(share_county >= min_county_share | share_zone >= min_zone_share) |>
  transmute(
    NWS_UGC, AREA_KM2, GEOID,
    share_county = round(share_county, 4),
    share_zone = round(share_zone, 4),
    method = "overlap"
  )

direct <- keys |>
  filter(method == "fips") |>
  transmute(
    NWS_UGC, AREA_KM2,
    GEOID = geoid,
    share_county = NA_real_,
    share_zone = NA_real_,
    method
  )

crosswalk <- bind_rows(direct, matched)

# A few outlines in IEM's archive are fragments of their zone - PRZ001 at 0.15
# square km, ASZ003 at 0.81 - and overlap nothing. Those take the counties of
# the largest outline the same code has elsewhere in the archive.
fragments <- to_overlap |>
  anti_join(crosswalk, by = c("NWS_UGC", "AREA_KM2")) |>
  select(NWS_UGC, fragment_area = AREA_KM2)

borrowed <- crosswalk |>
  semi_join(fragments, by = "NWS_UGC") |>
  group_by(NWS_UGC) |>
  filter(as.numeric(AREA_KM2) == max(as.numeric(AREA_KM2))) |>
  ungroup() |>
  inner_join(fragments, by = "NWS_UGC") |>
  transmute(
    NWS_UGC,
    AREA_KM2 = fragment_area,
    GEOID,
    share_county = NA_real_,
    share_zone = NA_real_,
    method = "same code"
  )

message(
  "Fragments: ", nrow(fragments), " outlines overlap no county, ",
  n_distinct(borrowed$NWS_UGC), " take their code's largest outline"
)

crosswalk <- bind_rows(crosswalk, borrowed)

# An outline with no larger version (ASZ003, Swains Island, 0.81 square km)
# takes the county it sits beside, if one is within 5 km: the Census outline of
# a small island need not overlap NWS's.
stranded <- outlines |>
  semi_join(
    anti_join(fragments, borrowed, by = "NWS_UGC"),
    by = c("NWS_UGC", "AREA_KM2" = "fragment_area")
  )
beside <- st_nearest_feature(stranded, counties)
beside_km <- as.numeric(
  st_distance(stranded, counties[beside, ], by_element = TRUE)
) / 1000

nearest <- tibble(
  NWS_UGC = stranded$NWS_UGC,
  AREA_KM2 = stranded$AREA_KM2,
  GEOID = counties$GEOID[beside],
  share_county = NA_real_,
  share_zone = NA_real_,
  method = "nearest",
  km = beside_km
)
message(
  "Stranded: ", nrow(nearest), " outlines assigned to a county within ",
  round(max(c(0, beside_km)), 2), " km"
)

crosswalk <- bind_rows(crosswalk, filter(nearest, km <= 5) |> select(-km)) |>
  arrange(NWS_UGC, AREA_KM2, GEOID)

# Guard: a land zone that reaches no county would drop its alerts from every
# county count without an error.
unmatched <- to_overlap |>
  anti_join(crosswalk, by = c("NWS_UGC", "AREA_KM2")) |>
  left_join(
    st_drop_geometry(outlines) |> select(NWS_UGC, AREA_KM2, zone_km2),
    by = c("NWS_UGC", "AREA_KM2")
  ) |>
  left_join(
    overlaps |>
      group_by(NWS_UGC, AREA_KM2) |>
      summarise(
        best_share_county = max(share_county),
        best_share_zone = max(share_zone),
        .groups = "drop"
      ),
    by = c("NWS_UGC", "AREA_KM2")
  )

if (nrow(unmatched) > 0) {
  print(unmatched, n = Inf, width = Inf)
  stop("Land UGCs above match no county - their alerts would vanish.")
}

message(
  "Crosswalk: ", nrow(keys), " keys, ",
  sum(keys$method %in% c("fips", "overlap")), " on land -> ",
  nrow(crosswalk), " UGC-county pairs"
)

write_csv(county_ref, file.path(out, "02_counties.csv"))
write_csv(crosswalk, file.path(out, "02_ugc_county.csv"), na = "")

# The outlines themselves, for 04's grid, which needs the shape of each zone
# rather than its county shares.
saveRDS(
  select(outlines, NWS_UGC, AREA_KM2, zone_km2),
  file.path(out, "02_ugc_outlines.rds")
)
