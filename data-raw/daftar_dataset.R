library(ckanr)
library(tidyverse)
library(usethis)

url <- "http://data.jakarta.go.id"

resources <- resource_search(q = "name:data", url = url, as = "table")

daftar_dataset <-
  resources %>%
  `[[`("results") %>%
  as_tibble() %>%
  filter(format == "CSV") %>%
  filter(!str_detect(url, pattern = "git")) %>%
  select(name, description, url, created, last_modified) %>%
  rename(
    nama = name,
    deskripsi = description,
    dibuat = created,
    diperbaharui = last_modified
  ) %>%
  mutate_all(~str_squish(.x)) %>%
  mutate_at(vars(dibuat, diperbaharui), ~ str_extract(.x, pattern = "\\d{2}-\\d{2}-\\d{2}")) %>%
  mutate_at(vars(dibuat, diperbaharui), ~ str_c("20", .x)) %>%
  mutate(
    diperbaharui = if_else(is.na(diperbaharui), dibuat, diperbaharui)
  )

use_data(daftar_dataset, internal = TRUE, overwrite = TRUE)
