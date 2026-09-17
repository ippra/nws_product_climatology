rm(list = ls())
library(tidyverse)
library(sf)
library(lubridate)
library(units)
library(maps)
library(plotly)

# read and prepare watch shapefile(s) (https://mesonet.agron.iastate.edu/request/gis/spc_watch.phtml) -----------------------------
watch_shp <- st_read("~/Univ. of Oklahoma Dropbox/Joe Ripberger/nws_product_climatology/watch_data/watches_202001010000_202312312359/", quiet = TRUE)
watch_shp <- watch_shp |> 
  mutate(ISSUE_DT = ymd_hm(ISSUE),
         EXPIRE_DT = ymd_hm(EXPIRE), 
         ISSUE_DAY = date(ISSUE_DT)) |> 
  mutate(YR_EVENT_ID = paste0(year(ISSUE_DT), NUM)) |> 
  filter(TYPE == "TOR") |> 
  filter(year(ISSUE_DT) %in% 2020:2023) |>
  arrange(ISSUE_DT) |> 
  mutate(WATCH_ROW_NUM = row_number())

watch_shp <- watch_shp %>%
  st_transform(3857) %>%
  st_make_valid() %>%
  st_simplify(dTolerance = 0.05)

# read and prepare wwa shapefile(s) (https://mesonet.agron.iastate.edu/request/gis/watchwarn.phtml) -----------------------------
# wwa_shp_20 <- st_read("~/Univ. of Oklahoma Dropbox/Joe Ripberger/nws_product_climatology/wwa_data/2020_all/", quiet = TRUE) |>
#   filter(PHENOM == "TO" & SIG == "A") # limit to tornado watches
# wwa_shp_21 <- st_read("~/Univ. of Oklahoma Dropbox/Joe Ripberger/nws_product_climatology/wwa_data/2021_all/", quiet = TRUE) |>
#   filter(PHENOM == "TO" & SIG == "A") # limit to tornado watches
# wwa_shp_22 <- st_read("~/Univ. of Oklahoma Dropbox/Joe Ripberger/nws_product_climatology/wwa_data/2022_all/", quiet = TRUE) |>
#   filter(PHENOM == "TO" & SIG == "A") # limit to tornado watches
# wwa_shp_23 <- st_read("~/Univ. of Oklahoma Dropbox/Joe Ripberger/nws_product_climatology/wwa_data/2023_all/", quiet = TRUE) |>
#   filter(PHENOM == "TO" & SIG == "A") # limit to tornado watches
# wwa_shp <- bind_rows(wwa_shp_20, wwa_shp_21, wwa_shp_22, wwa_shp_23) 
# st_write(wwa_shp, "~/Univ. of Oklahoma Dropbox/Joe Ripberger/nws_product_climatology/watch_data/to_watches_202001010000_202312312359.shp")

wwa_shp <- st_read("~/Univ. of Oklahoma Dropbox/Joe Ripberger/nws_product_climatology/watch_data/to_watches_202001010000_202312312359/", quiet = TRUE)
wwa_shp <- wwa_shp |>
  mutate(ISSUE_DT = ymd_hm(ISSUED),
         EXPIRE_DT = ymd_hm(EXPIRED),
         ISSUE_DAY = date(ISSUE_DT)) |>
  filter(year(ISSUE_DT) %in% 2020:2023) |>
  arrange(ISSUE_DT) |>
  mutate(WATCH_ROW_NUM = row_number())

wwa_shp <- wwa_shp %>%
  st_transform(3857) %>%
  st_make_valid() %>%
  st_simplify(dTolerance = 0.05)

# wwa_csv_20 <- read_csv("~/Univ. of Oklahoma Dropbox/Joe Ripberger/nws_product_climatology/watch_data/wwa_202001010000_202012312359.csv")
# wwa_csv_21 <- read_csv("~/Univ. of Oklahoma Dropbox/Joe Ripberger/nws_product_climatology/watch_data/wwa_202101010000_202112312359.csv")
# wwa_csv_22 <- read_csv("~/Univ. of Oklahoma Dropbox/Joe Ripberger/nws_product_climatology/watch_data/wwa_202201010000_202212312359.csv")
# wwa_csv_23 <- read_csv("~/Univ. of Oklahoma Dropbox/Joe Ripberger/nws_product_climatology/watch_data/wwa_202301010000_202312312359.csv")
# wwa_csv <- bind_rows(wwa_csv_20, wwa_csv_21, wwa_csv_22, wwa_csv_23) 
# write_csv(wwa_csv, "~/Univ. of Oklahoma Dropbox/Joe Ripberger/nws_product_climatology/watch_data/to_watches_202001010000_202312312359.csv")

wwa_csv <- read_csv("~/Univ. of Oklahoma Dropbox/Joe Ripberger/nws_product_climatology/watch_data/to_watches_202001010000_202312312359.csv") |> 
  filter(phenomena == "TO" & significance == "A") |> 
  mutate(ISSUE_DT = utc_issue,
         EXPIRE_DT = utc_expire,
         ISSUE_DAY = date(ISSUE_DT)) |>
  mutate(YR_EVENT_ID = paste0(year(ISSUE_DT), eventid)) |> 
  filter(year(ISSUE_DT) %in% 2020:2023) |>
  arrange(ISSUE_DT) |>
  mutate(WATCH_ROW_NUM = row_number())

table(wwa_shp$ISSUE_DT == wwa_csv$ISSUE_DT) # make sure this is all true!
wwa_shp <- bind_cols(wwa_shp, wwa_csv |> select(YR_EVENT_ID))

table(wwa_shp$YR_EVENT_ID %in% watch_shp$YR_EVENT_ID)
wwa_shp |> filter(!YR_EVENT_ID %in% watch_shp$YR_EVENT_ID) # YR_EVENT_ID 20221009 in wwa_shp not in watch_shp
wwa_shp <- wwa_shp |> filter(!YR_EVENT_ID == "20221009") # remove this event
table(watch_shp$YR_EVENT_ID %in% wwa_shp$YR_EVENT_ID)

wwa_shp |> tibble() |> count(YR_EVENT_ID) |> print(n = Inf)
watch_shp |> tibble() |> count(YR_EVENT_ID) |> print(n = Inf)

wwa_shp <- left_join(wwa_shp, watch_shp |> st_drop_geometry() |> select(YR_EVENT_ID, P_TORTWO:MV_SKNT), by = c("YR_EVENT_ID"))

# read and prepare report shapefile(s) (https://www.spc.noaa.gov/gis/svrgis/) -----------------------------
report_shp <- st_read("~/Univ. of Oklahoma Dropbox/Joe Ripberger/nws_product_climatology/watch_data/1950-2023-torn-initpoint/", quiet = TRUE) %>%
  mutate(REPORT_DT = ymd_hms(paste(date, time), tz = "America/Chicago"),
         REPORT_DT = with_tz(REPORT_DT, tzone = "UTC"),
         REPORT_DAY = date(REPORT_DT)) |> 
  filter(year(REPORT_DT) %in% 2020:2023) |> 
  arrange(REPORT_DT) |>
  mutate(REPORT_ROW_NUM = row_number()) |> 
  st_transform(st_crs(watch_shp))

# intersect reports and watches polygons -----------------------------
report_wwa_intersections <- st_intersects(report_shp, wwa_shp, sparse = TRUE)

report_wwa_matches <- tibble(
  REPORT_ROW_NUM = rep(report_shp$REPORT_ROW_NUM, lengths(report_wwa_intersections)),
  WATCH_ROW_NUM = wwa_shp$WATCH_ROW_NUM[unlist(report_wwa_intersections)])

report_wwa_matches <- report_wwa_matches |> 
  left_join(report_shp |>  st_drop_geometry() |>  select(REPORT_ROW_NUM, REPORT_DT), by = "REPORT_ROW_NUM") |> 
  left_join(wwa_shp |>  st_drop_geometry() |>  select(WATCH_ROW_NUM, YR_EVENT_ID, ISSUE_DT, EXPIRE_DT), by = "WATCH_ROW_NUM") |> 
  filter(REPORT_DT >= ISSUE_DT, REPORT_DT <= EXPIRE_DT)

report_shp <- report_shp |> 
  left_join(report_wwa_matches |> select(-REPORT_DT), by = "REPORT_ROW_NUM") |> 
  mutate(WATCH = if_else(is.na(WATCH_ROW_NUM), "NO", "YES")) |> 
  distinct(REPORT_ROW_NUM, .keep_all = TRUE) # there are a few duplicate reports, this keeps only the first occurrence

report_shp |> st_drop_geometry() |> count(WATCH) # 2338 (of 4860) reports with no watch # ref: https://www.spc.noaa.gov/faq/tornado/watchver.html
write_csv(report_shp, "~/Univ. of Oklahoma Dropbox/Joe Ripberger/nws_product_climatology/watch_data/watch_report_join.csv")


wwa_shp |>
  st_drop_geometry() |>
  left_join(report_wwa_matches |> select(-ISSUE_DT, -EXPIRE_DT, -YR_EVENT_ID), by = "WATCH_ROW_NUM") |>
  mutate(REPORT = if_else(is.na(REPORT_ROW_NUM), 0, 1)) |>
  group_by(YR_EVENT_ID) |> 
  summarise(SUM_REPORT = sum(REPORT, na.rm = TRUE)) |> 
  mutate(REPORT = if_else(SUM_REPORT == 0, "NO", "YES")) |> 
  count(REPORT) # 240 (of 686) watches with no report(s)

# quick maps -----------------------------
state_map <- st_as_sf(map("state", plot = FALSE, fill = TRUE)) |> st_transform(3857)
county_map <- st_as_sf(map("county", plot = FALSE, fill = TRUE)) |> st_transform(3857)
# report_shp <- report_shp |> mutate(WATCH_ID = factor(WATCH_ID))
# watch_shp <- watch_shp |> mutate(WATCH_ID = factor(WATCH_ID))
# wwa_shp <- wwa_shp |> mutate(WATCH_ID = factor(WATCH_ID))

count(report_shp |> st_drop_geometry(), REPORT_DAY, sort = TRUE)
sample_day <- report_shp |> filter(REPORT_DAY == "2023-04-01")

p <- ggplot() +
  geom_sf(data = st_cast(wwa_shp) |> filter(YR_EVENT_ID %in% sample_day$YR_EVENT_ID), 
          aes(text = paste("YR_EVENT_ID: ", YR_EVENT_ID, 
                           "\nISSUE_DT: ", ISSUE_DT, 
                           "\nEXPIRE_DT: ", EXPIRE_DT)), color = "lightblue", linewidth = 0.25, fill = "lightblue", alpha = 0.5) +
  geom_sf(data = st_cast(watch_shp) |> filter(YR_EVENT_ID %in% sample_day$YR_EVENT_ID), 
          aes(text = paste("YR_EVENT_ID: ", YR_EVENT_ID, 
                           "\nISSUE_DT: ", ISSUE_DT, 
                           "\nEXPIRE_DT: ", EXPIRE_DT)), fill = NA, color = "blue", linewidth = 0.25) +
  geom_sf(data = st_cast(sample_day), 
          aes(color = WATCH, text = paste("REPORT_ROW_NUM: ", REPORT_ROW_NUM, 
                                          "\nREPORT_DT: ", REPORT_DT))) +
  theme_minimal()
p
ggplotly(p)


