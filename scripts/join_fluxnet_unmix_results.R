# This file summarizes half-hourly FLUXNET observations within the 8-day
# MODIS compositing schedule and joins these to the results of the MODIS
# LST unmixing procedure.
library(tidyverse)
library(lubridate)
source("scripts/leaf_eb_kibler.R")

# Clean MODIS ----
modis <- read_csv("data_in/gee/fluxnet_unmixed.csv") %>%
  mutate(
    window_start = parse_date(str_sub(date, 1, 10), "%Y_%m_%d"),
    # End of the composite period is either 7 days in the future OR the last
    # day of the year.
    window_end   = pmin(
      window_start + days(7), 
      ceiling_date(window_start, "years") - days(1)),
    Day_view_time = Day_view_time * 0.1,
    # Round to nearest half hour
    Day_view_time = floor(Day_view_time / 0.5)  * 0.5
  ) %>%
  select(-`system:index`, -.geo, -date)

# Calculate FLUXNET temperature ----
e  <- 0.98    # Canopy emissivity
sb <- 5.67e-8 # SB constant
fluxnet <- read_csv("data_working/fluxnet_forest_cleaned.csv") %>%
  mutate(
    T_CANOPY_KIBLER = leaf_temperature_direct(
      TA_F, RH, PA_F, WS_F, USTAR, NETRAD, le=LE_F_MDS, g=G_F_MDS
    ) + 273.15,
    T_CANOPY_LW = ((1 / (e * sb)) * (LW_OUT - (1-e) * LW_IN_F))^0.25,
    T_CANOPY_BEST = ifelse(is.na(T_CANOPY_KIBLER), T_CANOPY_LW, T_CANOPY_KIBLER)
  )
# Determine the composite window for each observation
fluxnet$MODIS_JOIN_DATE <- as.Date(fluxnet$TIMESTAMP_START_LOCAL)
yday(fluxnet$MODIS_JOIN_DATE) <- 8 * (yday(fluxnet$TIMESTAMP_START_LOCAL) %/% 8) + 1

# Summarize canopy temperature within each window
fluxnet_summary <- fluxnet %>%
  mutate(HOUR = hour(TIMESTAMP_START_LOCAL) + minute(TIMESTAMP_START_LOCAL)/60) %>%
  group_by(SITE, HOUR, MODIS_JOIN_DATE) %>%
  summarize(across(contains("T_CANOPY"), ~ mean(.x, na.rm=TRUE)),
            n = n())

# Join unmixing results with fluxnet summary
flux_modis <- fluxnet_summary %>%
  inner_join(modis, by=c("SITE"="SITE_ID", "MODIS_JOIN_DATE"="window_start",
                         "HOUR"="Day_view_time"))

# Unmixing slightly reduces estimation, but bias is still very high.
flux_modis %>%
  # growing season only
  filter(month(MODIS_JOIN_DATE) %in% 6:9) %>% 
  pivot_longer(c(LST, T_for_log)) %>%
  ggplot(aes(x=T_CANOPY_BEST, y=value)) +
  geom_point(aes(color=Lai)) +
  facet_wrap(~ name) +
  geom_abline(slope=1, intercept=0, color="red")
