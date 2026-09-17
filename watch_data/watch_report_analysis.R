rm(list = ls())
library(tidyverse)
library(patchwork)

report_data <- read_csv("~/Univ. of Oklahoma Dropbox/Joe Ripberger/nws_product_climatology/watch_data/watch_report_join.csv")
report_data <- report_data |> 
  mutate(YEAR = year(date),
         MONTH = month(date, label = TRUE, abbr = TRUE),
         HOUR = hour(REPORT_DT),
         EF = mag, 
         FATALITIES = case_when(fat == 0 ~ "0", fat %in% 1:2 ~ "1-2", fat >= 3 ~ "3+"),
         INJURIES = case_when(inj == 0 ~ "0", inj %in% 1:5 ~ "1-5", inj %in% 6:15 ~ "6-15", inj >= 15 ~ "15+"),
         INJURIES = factor(INJURIES, levels = c("0", "1-5", "6-15", "15+"))) |> 
  mutate(IN_WATCH = if_else(WATCH == "YES", 1, 0))

agerage <- report_data |> 
  group_by(WATCH) |> 
  summarise(n = n()) |> 
  mutate(p = n / sum(n)) |> 
  filter(WATCH == "YES")

p1 <- report_data |> 
  group_by(YEAR, WATCH) |> 
  summarise(n = n()) |> 
  mutate(p = n / sum(n)) |> 
  filter(WATCH == "YES") |> 
  ggplot(aes(x = YEAR, y = p, group = 1)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = agerage |> pull(p), linetype = "dashed") + 
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1), expand = c(0, 0)) +
  labs(y = "% of Reports in Watch", x = "Year") +
  theme_classic(base_size = 10)

p2 <- report_data |> 
  group_by(MONTH, WATCH) |> 
  summarise(n = n()) |> 
  mutate(p = n / sum(n)) |> 
  filter(WATCH == "YES") |> 
  ggplot(aes(x = MONTH, y = p, group = 1)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = agerage |> pull(p), linetype = "dashed") + 
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1), expand = c(0, 0)) +
  labs(y = "% of Reports in Watch", x = "Month") +
  theme_classic(base_size = 10)

p3 <- report_data |> 
  group_by(HOUR, WATCH) |> 
  summarise(n = n()) |> 
  mutate(p = n / sum(n)) |> 
  filter(WATCH == "YES") |> 
  ggplot(aes(x = HOUR, y = p, group = 1)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = agerage |> pull(p), linetype = "dashed") + 
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1), expand = c(0, 0)) +
  labs(y = "% of Reports in Watch", x = "Hour (UTC)") +
  theme_classic(base_size = 10)

p4 <- report_data |> 
  group_by(EF, WATCH) |> 
  summarise(n = n()) |> 
  mutate(p = n / sum(n)) |> 
  filter(WATCH == "YES") |> 
  filter(EF != -9) |> 
  ggplot(aes(x = EF, y = p, group = 1)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = agerage |> pull(p), linetype = "dashed") + 
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1), expand = c(0, 0)) +
  labs(y = "% of Reports in Watch", x = "EF") +
  theme_classic(base_size = 10)

p5 <- report_data |> 
  group_by(FATALITIES, WATCH) |> 
  summarise(n = n()) |> 
  mutate(p = n / sum(n)) |> 
  filter(WATCH == "YES") |> 
  ggplot(aes(x = FATALITIES, y = p, group = 1)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = agerage |> pull(p), linetype = "dashed") + 
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1), expand = c(0, 0)) +
  labs(y = "% of Reports in Watch", x = "Fatalities") +
  theme_classic(base_size = 10)

p6 <- report_data |> 
  group_by(INJURIES, WATCH) |> 
  summarise(n = n()) |> 
  mutate(p = n / sum(n)) |> 
  filter(WATCH == "YES") |> 
  ggplot(aes(x = INJURIES, y = p, group = 1)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = agerage |> pull(p), linetype = "dashed") + 
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1), expand = c(0, 0)) +
  labs(y = "% of Reports in Watch", x = "Injuries") +
  theme_classic(base_size = 10)


p <- (p1 | p2 | p3) / (p4 | p5 | p6) + 
  plot_annotation(
    title = "Percent of Tornado Reports Occurring Within a Tornado Watch (2000–2023)",
    theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)))
ggsave("~/Univ. of Oklahoma Dropbox/Joe Ripberger/nws_product_climatology/watch_data/watch_stats.pdf", height = 8, width = 12)
