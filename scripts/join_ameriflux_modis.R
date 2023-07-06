# This file combines the temperature/LAI observations from Ameriflux and MODIS.
# MODIS times are recorded in local time, so they are floor'd to the nearest
# half hour to match the resolution of the Ameriflux data. LAI data are produced
# every 4 days, while LST data are produced daily.

library(tidyverse)
library(lubridate)
library(foreach)

modis <- read_csv("data_working/MODIS_LST.csv") %>%
  mutate(hours = floor(Day_view_time),
         # Isolate decimal part of the view time
         minutes = round(60 * (Day_view_time %% 1)),
         Day_view_datetime = Date + hours(hours) + minutes(minutes),
         # Floor to nearest 30 min to match Ameriflux
         Day_view_datetime = floor_date(Day_view_datetime, "30 minutes")) %>%
  select(-hours, -minutes)

# Snap date to the correct yday to match the LAI composite schedule.
modis$lai_join_date <- modis$Date
yday(modis$lai_join_date) <- 4 * (yday(modis$Date) %/% 4) + 1

lai <- read_csv("data_in/gee/modis_4day_lai_ameriflux.csv") %>%
  mutate(date = str_sub(`system:index`, 1, 10),
         date = parse_date(date, "%Y_%m_%d")) %>%
  select(-`system:index`, -.geo)

# Join LAI to MODIS first to not duplicate effort.
modis_lai <- modis %>%
  left_join(
    lai, 
    by=c("ID"="SITE_ID", "lai_join_date"="date"),
  )

data_dir <- "data_working/ameriflux_cleaned_max_heights"
cleaned_ameriflux <- list.files(data_dir, "*.csv")

modis_ameriflux_joined <- foreach(f=cleaned_ameriflux, .combine=bind_rows) %do% {
  # Join the MODIS observations for this site by date
  this_flux <- read_csv(file.path(data_dir, f), col_types=cols())
  this_site <- str_split_i(f, "_", 2)
  modis_lai %>%
    filter(ID == this_site) %>%
    left_join(this_flux, by=c("Day_view_datetime"="TIMESTAMP_START"))
}

# Filter down to rows with canopy T data
modis_ameriflux_joined <- modis_ameriflux_joined %>%
  filter(!is.na(T_CANOPY)) %>%
  write_csv("data_working/ameriflux_modis_joined.csv")

colSums(is.na(modis_ameriflux_joined)) / nrow(modis_ameriflux_joined) 
