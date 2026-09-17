# start from scratch -----------------------------
rm(list = ls())

# libraries -----------------------------
library(tidyverse)
library(sf)
library(lubridate)

# file path -----------------------------
file_path <- "~/Univ. of Oklahoma Dropbox/Joe Ripberger/nws_product_climatology/extended_outlooks_data/outlooks_202001010000_202412312359" # data: https://mesonet.agron.iastate.edu/request/gis/outlooks.phtml

# read shapefile and convert to tibble -----------------------------
outlooks_data <- st_read(file_path, quiet = TRUE) |> 
  as_tibble() |> 
  filter(CYCLE == 10) |>                             # keep only on-cycle outlooks
  mutate(
    ISSUE_DT = ymd_hm(ISSUE),
    EXPIRE_DT = ymd_hm(EXPIRE),
    VALID_DATE = as.Date(ISSUE_DT),
    THRESHOLD = as.numeric(THRESHOLD)
  ) |> 
  arrange(ISSUE_DT, PRODISS) |> 
  filter(ISSUE_DT >= as.Date("2020-01-09")) |>       # filter to dates with full set of outlooks (day 8 - 4)
  group_by(ISSUE_DT) |> 
  mutate(forecast_day_group = cur_group_id()) |> 
  ungroup() |> 
  group_by(forecast_day_group) |> 
  filter(any(!is.na(THRESHOLD))) |>                 # only keep groups with at least one valid threshold in day 8 - 4 forecast
  ungroup() |> 
  mutate(THRESHOLD = replace_na(THRESHOLD, 0))      # define missing thresholds as 0

# progression table -----------------------------
outlooks_data %>%
  group_by(forecast_day_group, DAY) |> 
  slice_max(THRESHOLD, with_ties = TRUE, na_rm = TRUE) |>       # only keep max threshold per day per group (some days have 15 and 30)
  ungroup() |> 
  select(VALID_DATE, DAY, THRESHOLD) |> 
  mutate(DAY = paste0("DAY_", DAY)) |> 
  pivot_wider(names_from = DAY, values_from = THRESHOLD) |>
  mutate(progression = str_c(DAY_8, DAY_7, DAY_6, DAY_5, DAY_4, sep = " → ")) |> 
  count(progression, sort = TRUE)

# progression plot -----------------------------
p <- outlooks_data %>%
  group_by(forecast_day_group, DAY) %>%
  slice_max(THRESHOLD, with_ties = TRUE, na_rm = TRUE) |>
  ungroup() |> 
  ggplot(aes(x = DAY, y = THRESHOLD, group = VALID_DATE)) + 
  geom_line() +
  geom_point(size = 0.5) +
  scale_x_reverse(breaks = 8:4, minor_breaks = NULL) +
  scale_y_continuous(breaks = c(0, 0.15, 0.30), minor_breaks = NULL) +
  facet_wrap(~VALID_DATE) +
  labs(title = "Progression of Maximum Extended-Range SPC Outlook Thresholds by Forecast Day (2020-2024)",
       subtitle = "Includes only forecast days with at least one extended-range outlook issued",
       caption = "Data from IEM SPC Convective and Fire Weather + WPC Excessive Rainfall Outlooks") + 
  theme_minimal(base_size = 7) +
  theme(
    strip.text = element_text(size = 6),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 7),
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 8),
    plot.caption = element_text(size = 6),
    panel.grid.minor = element_blank()
  )
ggsave("extended_outlook_analysis.pdf", plot = p, width = 10, height = 9)


