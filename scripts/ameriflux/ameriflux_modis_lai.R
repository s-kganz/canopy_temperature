library(tidyverse)
library(lubridate)

ameriflux_modis <- read_csv("data_working/ameriflux_modis_joined.csv")
ameriflux_meta  <- read_csv("data_working/ameriflux_site_metadata.csv")
ameriflux_cover <- read_csv("data_working/ameriflux_combined_forest_cover.csv")
ameriflux_lai   <- read_csv("data_in/gee/mod_unmix_8day_k2_log_linear.csv") %>%
  select(SITE_ID, Lai, date) %>%
  mutate(
    date = str_sub(date, 1, 10),
    date = parse_date(date, "%Y_%m_%d")
  )

forest_igbps <- c("EBF", "ENF", "DBF", "MF")
forest_sites <- ameriflux_meta$SITE_ID[ameriflux_meta$IGBP %in% forest_igbps]

ameriflux_forest <- ameriflux_modis %>%
  filter(ID %in% forest_sites) %>%
  left_join(ameriflux_lai, by=c("ID"="SITE_ID", "Date"="date"))

ameriflux_forest %>%
  filter(!is.na(Lai)) %>%
  #filter(month(Date) %in% 5:9) %>%
  mutate(T_CANOPY_K = T_CANOPY + 273.15,
         T_diff = LST_Day_1km - T_CANOPY_K) %>%
  ggplot(aes(x=Lai, y=T_diff)) + 
  geom_point()
