library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(terra)

theme_set(theme_bw())

cover <- read_csv("data_in/ameriflux_site_forest_cover.csv") %>%
  st_as_sf(coords=c("LOCATION_LONG", "LOCATION_LAT"))

st_crs(cover) <- 4326

world <- ne_countries(scale = "small", returnclass = "sf")

xticks <- seq(0, 80, by=20)
yticks <- seq(-160, -60, by=20)

ggplot(world) +
  geom_sf() +
  geom_sf(data=cover, size=2) +
  coord_sf(xlim = c(-170, -50), ylim = c(0, 90), expand = TRUE) +
  scale_x_continuous(labels=function(x) paste0(abs(x), "°W")) +
  scale_y_continuous(labels=function(x) paste0(x, "°N")) +
  labs(x="", y="")
