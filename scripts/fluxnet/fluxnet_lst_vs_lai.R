library(tidyverse)
library(lubridate)
library(colorspace)
source("scripts/leaf_eb_kibler.R")

fluxnet_modis <- read_csv("data_working/fluxnet_modis_lst_lai.csv")
fluxnet_meta  <- read_csv("data_working/fluxnet_site_metadata.csv")
lai           <- read_csv("")
epoch <- parse_date("1970-01-01", "%Y-%m-%d")

# Maybe filter to observations within the growing season of each site??
# grow_szn <- read_csv("data_in/gee/fluxnet_growing_season.csv") %>%
#   mutate(year = parse_number(str_sub(`system:index`, 1, 4)),
#          across(c(contains("Greenup"), contains("Senescence")),
#                 ~ yday(epoch + days(round(.x))))) %>%
#   select(-.geo, -`system:index`)

e  <- 0.98     # emissivity
sb <- 5.67e-8  # Stefan-Boltzmann constant
fluxnet_temp <- fluxnet_modis %>%
  mutate(
    T_CANOPY_KIBLER = leaf_temperature_direct(
      TA_F, RH, PA_F, WS_F, USTAR, NETRAD, le=LE_F_MDS, g=G_F_MDS
    ) + 273.15, #K
    T_CANOPY_LW = ((1 / (e * sb)) * (LW_OUT - (1-e) * LW_IN_F))^0.25,
    T_CANOPY_BEST = ifelse(is.na(T_CANOPY_KIBLER), T_CANOPY_LW, T_CANOPY_KIBLER)
  ) %>%
  left_join(fluxnet_meta, by=c("SITE"="SITE_ID"))

# Make sure it worked - yeah we chillin
palette <- choose_palette()
fluxnet_temp %>%
  select(contains("T_CANOPY"), LST_Day_1km, Lai) %>%
  pivot_longer(contains("T_CANOPY")) %>%
  ggplot(aes(x=value, y=LST_Day_1km)) + 
  geom_point(aes(color=Lai), alpha=0.2) +
  geom_abline(slope=1, intercept=0, color="red") +
  facet_wrap(~ name)

# How does the error vary with LAI by site and by IGBP?
# fluxnet_temp %>%
#   filter(month(TIMESTAMP_START_LOCAL) %in% 6:9) %>%
#   group_by(SITE) %>%
#   # filter(n() > 20,
#   #        max(Lai) - min(Lai) > 1) %>%
#   filter(Lai < 3) %>%
#   mutate(T_CANOPY_ERROR = LST_Day_1km - T_CANOPY_BEST) %>%
#   ggplot(aes(x=Lai, y=T_CANOPY_ERROR)) +
#   stat_ellipse(
#     aes(fill=SITE),
#     geom="polygon",
#     show.legend=FALSE,
#     alpha=0.3
#   ) +
#   facet_wrap(~ IGBP) +
#   labs(x = "Leaf Area Index",
#        y = "LST - T_CANOPY Difference") +
#   theme_bw()
