library(tidyverse)

source(here::here("00_paths.R"))

# Refresh Data -----------------------------------------------------------------
# Makes sure every year of the archive, both boundary windows and the reference
# geography are on disk. Incremental: a file already present is never fetched
# again. Years 2010-2024 are read in place from wwa_data/; a year that is not
# there is requested from IEM, which assembles a whole year for several minutes
# before it sends anything, then sends about 150 MB.

dir.create(archive_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reference_raw_dir, recursive = TRUE, showWarnings = FALSE)

# Downloads a zip to a .part file and unpacks it only once it reads as a whole
# archive, so an interrupted transfer never leaves a file that looks complete.
fetch_zip <- function(url, dest_dir) {
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  part <- file.path(dest_dir, "download.zip.part")
  handle <- curl::new_handle(
    timeout = 3600,
    connecttimeout = 60,
    failonerror = TRUE
  )

  curl::curl_download(url, part, handle = handle, quiet = TRUE)

  listing <- tryCatch(unzip(part, list = TRUE), error = \(e) NULL)
  if (is.null(listing) || nrow(listing) == 0) {
    stop("Download from ", url, " is not a readable zip.")
  }

  unzip(part, exdir = dest_dir)
  unlink(part)
  listing$Name
}

# Archive ----------------------------------------------------------------------
parts <- archive_parts()

for (i in seq_len(nrow(parts))) {
  p <- parts[i, ]
  shp <- sub("\\.csv$", ".shp", p$csv)
  if (file.exists(p$csv) && file.exists(shp)) next

  url <- paste0(
    iem_wwa_url,
    "?year1=", format(p$start, "%Y"), "&month1=", format(p$start, "%m"),
    "&day1=", format(p$start, "%d"), "&hour1=", format(p$start, "%H"),
    "&minute1=", format(p$start, "%M"),
    "&year2=", format(p$end, "%Y"), "&month2=", format(p$end, "%m"),
    "&day2=", format(p$end, "%d"), "&hour2=", format(p$end, "%H"),
    "&minute2=", format(p$end, "%M"),
    "&accept=shapefile"
  )

  cat("Fetching", p$part, "from IEM ...\n")
  started <- Sys.time()
  names_in_zip <- fetch_zip(url, p$dir)

  expected <- paste0(p$stem, c(".csv", ".shp", ".shx", ".dbf", ".prj"))
  missing <- setdiff(expected, names_in_zip)
  if (length(missing) > 0) {
    print(names_in_zip)
    stop(
      "IEM's zip for ", p$part, " lacks ", paste(missing, collapse = ", "),
      " - its file naming may have changed."
    )
  }
  cat(
    "  ", p$part, "done in",
    round(as.numeric(difftime(Sys.time(), started, units = "mins")), 1),
    "minutes\n"
  )
}

message(
  "Archive: ", nrow(parts), " parts on disk, ",
  sum(startsWith(parts$dir, legacy_archive_dir)), " read from wwa_data/"
)

# Reference Geography ----------------------------------------------------------
references <- tribble(
  ~name,                      ~url,
  "cb_2023_us_county_500k",   census_counties_url,
  "c_16ap26",                 nws_counties_url,
  "w_16ap26",                 nws_cwa_url
)

for (i in seq_len(nrow(references))) {
  dest <- file.path(reference_raw_dir, references$name[i])
  if (file.exists(file.path(dest, paste0(references$name[i], ".shp")))) next
  cat("Fetching", references$name[i], "...\n")
  fetch_zip(references$url[i], dest)
}

missing_refs <- references$name[
  !file.exists(file.path(
    reference_raw_dir, references$name, paste0(references$name, ".shp")
  ))
]
if (length(missing_refs) > 0) {
  print(missing_refs)
  stop("Reference files above did not unpack to a shapefile of that name.")
}
