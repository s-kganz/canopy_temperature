# This file adds MODIS LAI and LST measurements to half-hourly measurements
# from the FLUXNET 2015 dataset.

library(tidyverse)
library(lubridate)

# Clean MODIS ----
# QC was already done on GEE so no filtering needed here.
lst <- read_csv("data_in/gee/fluxnet_modis_lst.csv") %>%
  mutate(lst_view_datetime = str_sub(`system:index`, 1, 10),
         lst_view_datetime = parse_datetime(lst_view_datetime, "%Y_%m_%d"),
         # Observations are in local time
         lst_view_datetime = lst_view_datetime + 
           hours(as.integer(Day_view_time)) +
           minutes(as.integer((Day_view_time %% 1) * 60)),
         lst_view_window_start = floor_date(lst_view_datetime, "30 minutes")) %>%
  select(-.geo, -`system:index`)

lai <- read_csv("data_in/gee/fluxnet_modis_lai.csv") %>%
  mutate(lai_view_date = str_sub(`system:index`, 1, 10),
         lai_view_date = parse_date(lai_view_date, "%Y_%m_%d")) %>%
  select(-.geo, -`system:index`)

# Join to forested FLUXNET sites ----
fluxnet <- read_csv("data_working/fluxnet_forest_cleaned.csv")

# Join date for LAI depends on MODIS compositing schedule. Date of LAI
# observation corresponds to start of 4-day interval. As in,
# Jan 1, Jan 5, Jan 9, ...
# Snap flux observations to this interval.
fluxnet$LAI_JOIN_DATE <- as.Date(fluxnet$TIMESTAMP_START_LOCAL)
yday(fluxnet$LAI_JOIN_DATE) <- 4 * (yday(fluxnet$TIMESTAMP_START_LOCAL) %/% 4) + 1

fluxnet_modis <- fluxnet %>%
  left_join(lst, by=c("SITE"="SITE_ID", "TIMESTAMP_START_LOCAL"="lst_view_window_start")) %>%
  left_join(lai, by=c("SITE"="SITE_ID", "LAI_JOIN_DATE"="lai_view_date")) %>%
  select(-LAI_JOIN_DATE, -lst_view_datetime) %>%
  filter(!is.na(LST_Day_1km), !is.na(Lai)) %>%
  write_csv("data_working/fluxnet_modis_lst_lai.csv")
