rm(list = ls())
library(tidyverse)
library(sf)
library(lubridate)

# data -----------------------------
tornado_count_by_outlook_category_data <- read_csv("~/Univ. of Oklahoma Dropbox/Joe Ripberger/nws_product_climatology/outlooks_data/tornado_count_by_outlook_category_data.csv")

# example analysis -----------------------------
tornado_count_by_outlook_category_data %>% 
  filter(MAX_THRESHOLD_NUM == 3) %>%
  group_by(THRESHOLD) %>% 
  summarise(MEAN_TOR_PROP = mean(TOR_PROP, na.rm = TRUE))

tornado_count_by_outlook_category_data %>% 
  filter(MAX_THRESHOLD_NUM == 4) %>%
  group_by(THRESHOLD) %>% 
  summarise(MEAN_TOR_PROP = mean(TOR_PROP, na.rm = TRUE))

tornado_count_by_outlook_category_data %>% 
  filter(MAX_THRESHOLD_NUM == 5) %>%
  group_by(THRESHOLD) %>% 
  summarise(MEAN_TOR_PROP = mean(TOR_PROP, na.rm = TRUE))
