rm(list = ls())
library(tidyverse)
library(sf)
library(lubridate)
library(units)

# read and prepare outlook shapefile(s) -----------------------------
outlooks_shp <- st_read("~/Univ. of Oklahoma Dropbox/Joe Ripberger/nws_product_climatology/outlooks_data/outlooks_202001010000_202412312359/", quiet = TRUE) %>%
  filter(CATEGORY == "CATEGORICAL") %>%
  filter(CYCLE == 20) %>% # limit to 20Z outlooks
  mutate(ISSUE_DT = ymd_hm(ISSUE),
         EXPIRE_DT = ymd_hm(EXPIRE),
         THRESHOLD_ID = 1:nrow(.)) %>%
  group_by(ISSUE, CYCLE) %>%
  mutate(OUTLOOK_ID = cur_group_id()) %>%
  ungroup() %>%
  mutate(
    THRESHOLD = case_when(
      THRESHOLD == "TSTM" ~ "(0) TSTM", 
      THRESHOLD == "MRGL" ~ "(1) MRGL",
      THRESHOLD == "SLGT" ~ "(2) SLGT",
      THRESHOLD == "ENH"  ~ "(3) ENH",
      THRESHOLD == "MDT"  ~ "(4) MDT",
      THRESHOLD == "HIGH" ~ "(5) HIGH"),
    THRESHOLD_NUM = case_when(
      THRESHOLD == "(0) TSTM" ~ 0, 
      THRESHOLD == "(1) MRGL" ~ 1,
      THRESHOLD == "(2) SLGT" ~ 2,
      THRESHOLD == "(3) ENH"  ~ 3,
      THRESHOLD == "(4) MDT"  ~ 4,
      THRESHOLD == "(5) HIGH" ~ 5))

outlooks_shp <- outlooks_shp %>%
  st_transform(3857) %>%
  st_make_valid() %>%
  st_simplify(dTolerance = 0.05)

outlooks_shp <- outlooks_shp %>%
  st_transform(crs = 5070) %>%  # Albers Equal Area for US
  mutate(
    AREA_KM2 = set_units(st_area(.), "km^2"),
    AREA_KM2 = as.numeric(AREA_KM2)) %>%
  select(OUTLOOK_ID, THRESHOLD_ID, AREA_KM2) %>%
  st_drop_geometry() %>%
  right_join(outlooks_shp, by = c("THRESHOLD_ID"))

# read and prepare local storm report shapefile(s) -----------------------------
reports_shp <- st_read("~/Univ. of Oklahoma Dropbox/Joe Ripberger/nws_product_climatology/outlooks_data/lsr_202001010000_202412312359/", quiet = TRUE) %>%
  mutate(VALID_DT = ymd_hm(VALID),
         REPORT_ID = row_number()) %>%
  st_transform(st_crs(outlooks_shp))

# intersect reports and outlook polygons -----------------------------
report_outlook_intersections <- st_intersects(reports_shp, outlooks_shp, sparse = TRUE)

report_outlook_pairs <- tibble(
  REPORT_IDX = rep(seq_len(nrow(reports_shp)), lengths(report_outlook_intersections)),
  OUTLOOK_IDX = unlist(report_outlook_intersections)) %>%
  mutate(REPORT_ID = reports_shp$REPORT_ID[REPORT_IDX],
         OUTLOOK_ID = outlooks_shp$OUTLOOK_ID[OUTLOOK_IDX],
         ISSUE_DT = outlooks_shp$ISSUE_DT[OUTLOOK_IDX],
         EXPIRE_DT = outlooks_shp$EXPIRE_DT[OUTLOOK_IDX],
         THRESHOLD = outlooks_shp$THRESHOLD[OUTLOOK_IDX],
         THRESHOLD_NUM = outlooks_shp$THRESHOLD_NUM[OUTLOOK_IDX],
         ISSUE = outlooks_shp$ISSUE[OUTLOOK_IDX],
         EXPIRE = outlooks_shp$EXPIRE[OUTLOOK_IDX],
         DAY = outlooks_shp$DAY[OUTLOOK_IDX],
         CYCLE = outlooks_shp$CYCLE[OUTLOOK_IDX], 
         VALID_DT = reports_shp$VALID_DT[REPORT_IDX],
         REMARK = reports_shp$REMARK[REPORT_IDX],
         CITY = reports_shp$CITY[REPORT_IDX],
         COUNTY = reports_shp$COUNTY[REPORT_IDX],
         STATE = reports_shp$STATE[REPORT_IDX]) %>%
  filter(VALID_DT >= ISSUE_DT, VALID_DT <= EXPIRE_DT)

x <- report_outlook_pairs %>% filter(OUTLOOK_ID == 1671)

# count tornado reports by outlook threshold -----------------------------
tornado_count_by_outlook_category_data <- report_outlook_pairs %>%
  group_by(OUTLOOK_ID, THRESHOLD) %>%
  summarise(TOR_COUNT = n(), .groups = "drop") %>%
  right_join(outlooks_shp %>% st_drop_geometry(), by = c("OUTLOOK_ID", "THRESHOLD")) %>%
  mutate(TOR_COUNT = replace_na(TOR_COUNT, 0)) %>%
  group_by(OUTLOOK_ID) %>%
  mutate(TOR_PROP = TOR_COUNT / sum(TOR_COUNT),
         MAX_THRESHOLD_NUM = max(THRESHOLD_NUM)) %>%
  ungroup() %>%
  arrange(OUTLOOK_ID, THRESHOLD) %>%
  mutate(SPC_VERIF_LINK = paste0("https://www.spc.noaa.gov/products/outlook/archive/", substr(ISSUE, 1, 4), "/day1otlk_v_", substr(ISSUE, 1, 8), "_2000.gif")) %>%
  select(OUTLOOK_ID, ISSUE, EXPIRE, DAY, CYCLE, THRESHOLD, THRESHOLD_NUM, TOR_COUNT, TOR_PROP, MAX_THRESHOLD_NUM, SPC_VERIF_LINK)

# save the data -----------------------------
write_csv(tornado_count_by_outlook_category_data, "~/Univ. of Oklahoma Dropbox/Joe Ripberger/nws_product_climatology/outlooks_data/tornado_count_by_outlook_category_data.csv")
