library(tidyverse)

source(here::here("00_paths.R"))

# Alert Counts -----------------------------------------------------------------
# Counts, for every product, county, forecast office, year and month:
#
#   days     calendar days with the alert in effect for at least part of the
#            day, in the county's (or office's) own time zone
#   alerts   distinct alerts (VTEC events) that included the county, dated by
#            when they first covered it
#
# An alert is in effect from its ISSUED time to its EXPIRED time on each
# zone or county row. Those are IEM's per-row times: a county added to a
# warning later starts when it was added, and a watch upgraded before it began
# ends before it started, so it counts as an alert issued but adds no days.
#
# Writes outputs/03_counts/:
#   03_products.csv        every product the dashboard offers
#   03_county_counts.rds   county x product x year x month (input to 04)
#   03_office_counts.rds   office x product x year x month (input to 04)
#   03_national_counts.csv product x year x month
#   03_county_year_counts.csv  county x product x year, for analysis
#   03_capped_rows.csv     rows shortened as stuck alerts (see below)
#   03_ceilings.csv        each product's duration cap, which 04 also applies
#   03_rows.rds            the cleaned zone and county rows, input to 04

out <- file.path(outputs, "03_counts")
dir.create(out, recursive = TRUE, showWarnings = FALSE)

cores <- max(1, parallel::detectCores() - 2)
years <- first_year:last_year

products_ref <- read_csv(
  products_reference,
  col_types = cols(include = col_logical(), .default = col_character())
)
issuers_ref <- read_csv(
  issuers_reference,
  col_types = cols(.default = col_character())
)

crosswalk_dir <- file.path(outputs, "02_crosswalk")
county_ref <- read_csv(
  file.path(crosswalk_dir, "02_counties.csv"),
  col_types = cols(county_km2 = col_double(), .default = col_character())
)
crosswalk <- read_csv(
  file.path(crosswalk_dir, "02_ugc_county.csv"),
  col_types = cols(.default = col_character())
) |>
  select(NWS_UGC, AREA_KM2, GEOID)

cwa <- sf::read_sf(file.path(reference_raw_dir, "w_16ap26")) |>
  sf::st_drop_geometry() |>
  distinct(WFO, CITYSTATE)

# Read the Archive -------------------------------------------------------------
parts <- archive_parts()

# readr reads a malformed row rather than stopping at it, and reports it in
# problems(); the line count catches anything it read short.
#
# One malformed row is expected. Files from 2025 end in FCSTER, the forecaster's
# free-text sign-off, which IEM does not quote: "MGF, JD" splits into two
# fields. FCSTER comes after every column read here, so such a row's values are
# intact. Any other problem stops the build.
read_part <- function(i) {
  p <- parts[i, ]
  columns <- c(
    "WFO", "ISSUED", "EXPIRED", "PHENOM", "GTYPE", "SIG", "ETN", "NWS_UGC",
    "AREA_KM2"
  )
  header <- str_split_1(read_lines(p$csv, n_max = 1), ",")

  rows <- read_csv(
    p$csv,
    col_select = all_of(columns),
    col_types = cols(.default = col_character())
  )
  lines <- length(read_lines(p$csv)) - 1

  signoff <- match("FCSTER", header)
  tolerated <- problems(rows) |>
    filter(
      !is.na(signoff),
      signoff > max(match(columns, header)),
      expected == paste(length(header), "columns"),
      as.integer(str_extract(actual, "^\\d+")) > length(header)
    )
  untolerated <- nrow(problems(rows)) - nrow(tolerated)

  if (untolerated > 0 || nrow(rows) != lines) {
    stop(
      p$part, ": read ", nrow(rows), " rows of ", lines, " lines with ",
      untolerated, " parsing problems."
    )
  }
  if (nrow(tolerated) > 0) {
    cat(p$part, ":", nrow(tolerated), "rows with a comma in FCSTER\n")
  }

  rows |>
    filter(GTYPE == "C") |>
    select(-GTYPE) |>
    mutate(
      part = p$part,
      file_year = p$year,
      issued = ymd_hm(ISSUED, tz = "UTC", quiet = TRUE),
      expired = ymd_hm(EXPIRED, tz = "UTC", quiet = TRUE)
    )
}

started <- Sys.time()
rows <- parallel::mclapply(seq_len(nrow(parts)), read_part, mc.cores = cores)
failed <- keep(rows, \(r) inherits(r, "try-error"))
if (length(failed) > 0) stop(failed[[1]])
rows <- bind_rows(rows)

message(
  "Read ", format(nrow(rows), big.mark = ","), " zone and county rows from ",
  nrow(parts), " parts in ",
  round(as.numeric(difftime(Sys.time(), started, units = "mins")), 1),
  " minutes"
)

# Guards -----------------------------------------------------------------------
unparsed <- filter(rows, is.na(issued) | is.na(expired))
if (nrow(unparsed) > 0) {
  print(count(unparsed, part, ISSUED, EXPIRED))
  stop("Rows above have an ISSUED or EXPIRED time that does not parse.")
}

unknown <- rows |>
  distinct(PHENOM, SIG) |>
  anti_join(products_ref, by = c("PHENOM", "SIG"))
if (nrow(unknown) > 0) {
  print(unknown)
  stop("Products above are not in reference/products.csv - add them.")
}

# Stuck Alerts -----------------------------------------------------------------
# Some alerts in the archive were never closed: a Flood Advisory for seven
# Puerto Rico municipios runs 1,034 days from October 2012, a Flash Flood
# Warning 32 days, a High Wind Warning 30. Counted as issued, each would add
# weeks or years of days to the counties it names. Real alerts run long too -
# James River flood warnings in South Dakota stayed in force from April 2019
# into autumn 2020 - so no single limit fits every product.
#
# Each row is capped at three times the 99.9th percentile duration of its own
# product, and never below 3 days. Across 2010-2025 this shortens about 150 of
# 6.2 million rows. Beyond two years a span is not credible at all, and stops.
rows <- rows |>
  mutate(span_days = as.numeric(expired - issued, units = "days"))

ceilings <- rows |>
  filter(span_days > 0) |>
  group_by(PHENOM, SIG) |>
  summarise(
    ceiling_days = max(3, 3 * quantile(span_days, 0.999, names = FALSE)),
    .groups = "drop"
  )

rows <- rows |>
  left_join(ceilings, by = c("PHENOM", "SIG")) |>
  mutate(capped = !is.na(ceiling_days) & span_days > ceiling_days)

incredible <- filter(rows, span_days > 731)
if (nrow(incredible) > 0 && any(incredible$capped == FALSE)) {
  print(filter(incredible, !capped))
  stop("Rows above are in effect for more than two years.")
}

message(
  "Capped: ", sum(rows$capped), " rows of ",
  format(nrow(rows), big.mark = ","), " exceeded their product's ceiling, in ",
  n_distinct(paste(rows$PHENOM, rows$SIG)[rows$capped]), " products"
)

write_csv(ceilings, file.path(out, "03_ceilings.csv"))

write_csv(
  rows |>
    filter(capped) |>
    select(WFO, PHENOM, SIG, ETN, NWS_UGC, ISSUED, EXPIRED, span_days,
           ceiling_days) |>
    mutate(across(c(span_days, ceiling_days), \(x) round(x, 2))),
  file.path(out, "03_capped_rows.csv")
)

rows <- rows |>
  mutate(
    expired = if_else(capped, issued + ceiling_days * 86400, expired)
  ) |>
  select(-span_days, -ceiling_days, -capped)

excluded <- products_ref |>
  filter(!include) |>
  semi_join(x = rows, by = c("PHENOM", "SIG"))
message(
  "Excluded: ", nrow(excluded), " rows of products marked include = FALSE"
)

rows <- rows |>
  semi_join(filter(products_ref, include), by = c("PHENOM", "SIG")) |>
  left_join(select(issuers_ref, issuer, wfo), by = c("WFO" = "issuer")) |>
  mutate(
    office = case_when(
      WFO %in% cwa$WFO ~ WFO,
      !is.na(wfo)      ~ wfo,
      TRUE             ~ NA_character_
    )
  ) |>
  select(-wfo)

unknown_issuers <- rows |>
  filter(is.na(office), !WFO %in% issuers_ref$issuer) |>
  count(WFO)
if (nrow(unknown_issuers) > 0) {
  print(unknown_issuers)
  stop("Issuers above are neither offices nor in reference/issuers.csv.")
}

# Products ---------------------------------------------------------------------
# Three kinds: each phenomenon-significance pair (TO.W), each phenomenon with
# more than one significance in any significance (TO.all: a tornado watch or
# warning), and each significance across phenomena (all.W: any warning).
sig_order <- c(W = 1, A = 2, Y = 3, S = 4, O = 5)
sig_names <- c(
  W = "Any warning", A = "Any watch", Y = "Any advisory", S = "Any statement",
  O = "Any outlook"
)

specific <- rows |>
  distinct(PHENOM, SIG) |>
  left_join(products_ref, by = c("PHENOM", "SIG")) |>
  mutate(product = paste0(PHENOM, ".", SIG), kind = "product") |>
  arrange(PHENOM, sig_order[SIG])

join_labels <- function(labels) {
  if (length(labels) == 1) return(labels)
  paste(
    paste(head(labels, -1), collapse = ", "),
    tail(labels, 1),
    sep = " or "
  )
}

by_phenomenon <- specific |>
  group_by(PHENOM) |>
  filter(n() > 1) |>
  summarise(
    label = join_labels(label),
    group = first(group),
    .groups = "drop"
  ) |>
  mutate(product = paste0(PHENOM, ".all"), SIG = "all", kind = "phenomenon")

by_significance <- specific |>
  distinct(SIG) |>
  mutate(
    product = paste0("all.", SIG),
    PHENOM = "all",
    label = sig_names[SIG],
    group = "All alerts",
    kind = "significance"
  )

products <- bind_rows(by_significance, specific, by_phenomenon) |>
  mutate(id = row_number()) |>
  select(id, product, kind, PHENOM, SIG, label, group, note)

specific_ids <- products |>
  filter(kind == "product") |>
  left_join(
    select(products, phenomenon_id = id, PHENOM, kind) |>
      filter(kind == "phenomenon") |>
      select(-kind),
    by = "PHENOM"
  ) |>
  left_join(
    select(products, significance_id = id, SIG, kind) |>
      filter(kind == "significance") |>
      select(-kind),
    by = "SIG"
  ) |>
  select(PHENOM, SIG, id, phenomenon_id, significance_id)

rows <- left_join(rows, specific_ids, by = c("PHENOM", "SIG"))

# 04 counts the same rows on the grid, so it reads them as cleaned here: capped,
# filtered to real products, with their product ids.
saveRDS(
  select(rows, WFO, PHENOM, SIG, ETN, NWS_UGC, AREA_KM2, file_year, issued,
         expired, id, phenomenon_id, significance_id),
  file.path(out, "03_rows.rds")
)

# Local Dates ------------------------------------------------------------------
# Dates are whole days in a given time zone, as integers (days since
# 1970-01-01). A row ending exactly at midnight does not touch the next day.
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

# Every date a row touches, one output row per date, limited to the archive.
expand_days <- function(start_day, end_day) {
  n <- pmax(end_day - start_day + 1L, 0L)
  index <- rep.int(seq_along(n), n)
  day <- start_day[index] + sequence(n) - 1L
  keep <- day >= first_day & day <= last_day
  list(index = index[keep], day = day[keep])
}

day_year <- function(day) as.POSIXlt(as.Date(day))$year + 1900L
day_month <- function(day) as.POSIXlt(as.Date(day))$mon + 1L

# Counts days per unit x product x year x month from unique (unit, product,
# day) triples, packed into one double so unique() runs on a plain vector.
count_days <- function(unit, product, day) {
  key <- unique((unit * 1000 + product) * 1e5 + day)
  tibble(
    unit = as.integer(key %/% 1e8),
    product = as.integer((key %/% 1e5) %% 1000),
    day = as.integer(key %% 1e5)
  ) |>
    mutate(year = day_year(day), month = day_month(day)) |>
    count(unit, product, year, month, name = "days")
}

# Counties ---------------------------------------------------------------------
land <- rows |>
  inner_join(crosswalk, by = c("NWS_UGC", "AREA_KM2"),
             relationship = "many-to-many")

lost <- rows |>
  anti_join(crosswalk, by = c("NWS_UGC", "AREA_KM2")) |>
  filter(!substr(NWS_UGC, 1, 2) %in% c(marine_prefixes, no_county_prefixes))
if (nrow(lost) > 0) {
  print(count(lost, part, NWS_UGC, AREA_KM2))
  stop("Land UGC keys above are missing from the crosswalk - rerun 02.")
}

county_index <- set_names(seq_len(nrow(county_ref)), county_ref$GEOID)
land <- land |>
  mutate(
    unit = county_index[GEOID],
    tz = county_ref$tz[unit],
    start_day = local_day(issued, tz),
    end_day = local_day(expired - 60, tz)
  )

message(
  "Counties: ", format(nrow(land), big.mark = ","), " county rows from ",
  format(n_distinct(land$NWS_UGC), big.mark = ","), " UGCs"
)

# Alerts: the event key carries the issuing office because ETNs are per office
# and per year. The year is the file's, since ETNs restart each January.
county_alerts <- land |>
  group_by(unit, WFO, PHENOM, SIG, ETN, file_year) |>
  summarise(
    first = min(issued),
    tz = first(tz),
    id = first(id),
    phenomenon_id = first(phenomenon_id),
    significance_id = first(significance_id),
    .groups = "drop"
  ) |>
  mutate(day = local_day(first, tz)) |>
  filter(day >= first_day, day <= last_day) |>
  mutate(year = day_year(day), month = day_month(day))

tally_alerts <- function(alerts) {
  alerts |>
    select(unit, year, month, id, phenomenon_id, significance_id) |>
    pivot_longer(
      c(id, phenomenon_id, significance_id),
      values_to = "product"
    ) |>
    filter(!is.na(product)) |>
    count(unit, product, year, month, name = "alerts")
}

tally_days <- function(unit_rows) {
  in_effect <- filter(unit_rows, expired > issued)
  expanded <- expand_days(in_effect$start_day, in_effect$end_day)
  unit <- in_effect$unit[expanded$index]

  map(c("id", "phenomenon_id", "significance_id"), \(column) {
    product <- in_effect[[column]][expanded$index]
    has <- !is.na(product)
    count_days(unit[has], product[has], expanded$day[has])
  }) |>
    bind_rows()
}

started <- Sys.time()
county_counts <- full_join(
  tally_days(land),
  tally_alerts(county_alerts),
  by = c("unit", "product", "year", "month")
) |>
  mutate(across(c(days, alerts), \(x) replace_na(x, 0L)))

message(
  "County counts: ", format(nrow(county_counts), big.mark = ","), " rows in ",
  round(as.numeric(difftime(Sys.time(), started, units = "mins")), 1),
  " minutes"
)

# Offices ----------------------------------------------------------------------
# An office's days are in the time zone most of its counties keep, from the
# NWS county file, which also lists the Micronesian counties of PQE and PQW
# that the Census file lacks. Every row counts, marine zones included, since
# offices issue those too.
office_tz <- foreign::read.dbf(
  file.path(reference_raw_dir, "c_16ap26", "c_16ap26.dbf"),
  as.is = TRUE
) |>
  transmute(
    CWA = str_extract(CWA, "^[A-Z]{3}"),
    tz = unname(nws_time_zones[substr(TIME_ZONE, 1, 1)])
  ) |>
  filter(!is.na(tz)) |>
  count(CWA, tz) |>
  group_by(CWA) |>
  slice_max(n, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(CWA, tz)

offices <- cwa |>
  left_join(office_tz, by = c("WFO" = "CWA")) |>
  arrange(WFO)

no_tz <- filter(offices, is.na(tz))
if (nrow(no_tz) > 0) {
  print(no_tz)
  stop("Offices above have no counties, so no time zone.")
}

office_index <- set_names(seq_len(nrow(offices)), offices$WFO)
office_rows <- rows |>
  filter(!is.na(office)) |>
  mutate(
    unit = office_index[office],
    tz = offices$tz[unit],
    start_day = local_day(issued, tz),
    end_day = local_day(expired - 60, tz)
  )

office_alerts <- office_rows |>
  group_by(unit, PHENOM, SIG, ETN, file_year) |>
  summarise(
    first = min(issued),
    tz = first(tz),
    id = first(id),
    phenomenon_id = first(phenomenon_id),
    significance_id = first(significance_id),
    .groups = "drop"
  ) |>
  mutate(day = local_day(first, tz)) |>
  filter(day >= first_day, day <= last_day) |>
  mutate(year = day_year(day), month = day_month(day))

office_counts <- full_join(
  tally_days(office_rows),
  tally_alerts(office_alerts),
  by = c("unit", "product", "year", "month")
) |>
  mutate(across(c(days, alerts), \(x) replace_na(x, 0L)))

# Nation -----------------------------------------------------------------------
# Tornado and severe thunderstorm watches are numbered nationally by SPC, and
# each office in a watch issues its own copy, so a national count keys them
# without the office. Every other ETN is per office. Dated in the time zone of
# the office that first issued it; national centers in UTC.
national_rows <- rows |>
  mutate(
    issuer = if_else(PHENOM %in% c("SV", "TO") & SIG == "A", "SPC", WFO),
    tz = coalesce(offices$tz[office_index[office]], "UTC")
  )

national_alerts <- national_rows |>
  arrange(issued) |>
  group_by(issuer, PHENOM, SIG, ETN, file_year) |>
  summarise(
    first = first(issued),
    tz = first(tz),
    id = first(id),
    phenomenon_id = first(phenomenon_id),
    significance_id = first(significance_id),
    .groups = "drop"
  ) |>
  mutate(day = local_day(first, tz), unit = 1L) |>
  filter(day >= first_day, day <= last_day) |>
  mutate(year = day_year(day), month = day_month(day))

# National days: days with the alert in effect in at least one county or
# office, from the same local days counted above.
national_days <- bind_rows(
  select(land, id, phenomenon_id, significance_id, issued, expired,
         start_day, end_day),
  select(office_rows, id, phenomenon_id, significance_id, issued, expired,
         start_day, end_day)
) |>
  mutate(unit = 1L)

national_counts <- full_join(
  tally_days(national_days),
  tally_alerts(national_alerts),
  by = c("unit", "product", "year", "month")
) |>
  mutate(across(c(days, alerts), \(x) replace_na(x, 0L))) |>
  select(-unit)

# Checks -----------------------------------------------------------------------
over_full <- county_counts |>
  mutate(month_days = days_in_month(make_date(year, month, 1))) |>
  filter(days > month_days)
if (nrow(over_full) > 0) {
  print(over_full)
  stop("Rows above count more days than the month has.")
}

# Any significance of a phenomenon can have no more days than its parts added
# together, and no fewer than its busiest part.
component_days <- county_counts |>
  inner_join(
    select(specific_ids, id, phenomenon_id),
    by = c("product" = "id")
  ) |>
  filter(!is.na(phenomenon_id)) |>
  group_by(unit, phenomenon_id, year, month) |>
  summarise(sum_days = sum(days), max_days = max(days), .groups = "drop")

union_check <- county_counts |>
  inner_join(component_days,
             by = c("unit", "product" = "phenomenon_id", "year", "month")) |>
  filter(days > sum_days | days < max_days)
if (nrow(union_check) > 0) {
  print(union_check)
  stop("Combined products above disagree with their parts.")
}

missing_units <- setdiff(seq_len(nrow(county_ref)), county_counts$unit)
message(
  "Counties with no alert of any kind: ", length(missing_units),
  if (length(missing_units) > 0) {
    paste0(" (", paste(head(county_ref$GEOID[missing_units], 10),
                       collapse = ", "), ")")
  }
)

# Products Summary -------------------------------------------------------------
product_summary <- national_counts |>
  filter(alerts > 0 | days > 0) |>
  group_by(product) |>
  summarise(
    alerts = sum(alerts),
    first_year = min(year[alerts > 0 | days > 0]),
    last_year = max(year[alerts > 0 | days > 0]),
    .groups = "drop"
  )

county_reach <- county_counts |>
  group_by(product) |>
  summarise(
    counties = n_distinct(unit[alerts > 0 | days > 0]),
    .groups = "drop"
  )

products_out <- products |>
  left_join(product_summary, by = c("id" = "product")) |>
  left_join(county_reach, by = c("id" = "product")) |>
  mutate(counties = replace_na(counties, 0L)) |>
  filter(!is.na(alerts))

message(
  "Products: ", nrow(products_out), " (",
  sum(products_out$counties == 0), " reach no county)"
)

# Write ------------------------------------------------------------------------
write_csv(products_out, file.path(out, "03_products.csv"), na = "")

saveRDS(
  county_counts |> mutate(GEOID = county_ref$GEOID[unit]) |> select(-unit),
  file.path(out, "03_county_counts.rds")
)
saveRDS(
  office_counts |> mutate(WFO = offices$WFO[unit]) |> select(-unit),
  file.path(out, "03_office_counts.rds")
)
write_csv(national_counts, file.path(out, "03_national_counts.csv"))

county_counts |>
  group_by(GEOID = county_ref$GEOID[unit], product, year) |>
  summarise(days = sum(days), alerts = sum(alerts), .groups = "drop") |>
  left_join(select(products, id, code = product), by = c("product" = "id")) |>
  select(GEOID, product = code, year, days, alerts) |>
  write_csv(file.path(out, "03_county_year_counts.csv"))
