library(tidyverse)
library(lubridate)

mod_terra <- read_csv(file.path(
  "data_in/appears_modis_ecostress/",
  "Ameriflux-canopy-temperature-MODIS-ECOSTRESS-2-MOD11A1-061-results.csv"
)) %>%
  # Drop unnecessary columns
  select(-Category, -MODIS_Tile, -MOD11A1_061_Line_Y_1km,
         -MOD11A1_061_Sample_X_1km) %>%
  # Columns after the full bitmask duplicate QA information so
  # we can just drop it.
  select(-contains("MOD11A1_061_QC_Day"),
         MOD11A1_061_QC_Day_bitmask) %>%
  # Convert bitmask to binary
  mutate(
    MOD11A1_061_QC_Day_bitmask = strtoi(
      str_sub(MOD11A1_061_QC_Day_bitmask, 3),
      base=2
    ),
    platform="terra"
  ) %>%
  # Drop the product name from the columns
  rename_with(
    function(x) {str_replace(x, "MOD11A1_061_", "")},
    contains("MOD11A1_061_")
  )

mod_aqua <- read_csv(file.path(
  "data_in/appears_modis_ecostress/",
  "Ameriflux-canopy-temperature-MODIS-ECOSTRESS-2-MYD11A1-061-results.csv"
)) %>%
  # Drop unnecessary columns
  select(-Category, -MODIS_Tile, -MYD11A1_061_Line_Y_1km,
         -MYD11A1_061_Sample_X_1km) %>%
  # Columns after the full bitmask duplicate QA information so
  # we can just drop it.
  select(-contains("MYD11A1_061_QC_Day"),
         MYD11A1_061_QC_Day_bitmask) %>%
  # Convert bitmask to binary
  mutate(
    MYD11A1_061_QC_Day_bitmask = strtoi(
      str_sub(MYD11A1_061_QC_Day_bitmask, 3),
      base=2
    ),
    platform="aqua"
  ) %>%
  # Drop the product name from the columns
  rename_with(
    function(x) {str_replace(x, "MYD11A1_061_", "")},
    contains("MYD11A1_061_")
  ) %>% select(-Emis_31)

# Join the two tables
mod <- mod_aqua %>% bind_rows(mod_terra) %>%
  # Drop rows where LST wasn't produced
  filter(
    bitwAnd(QC_Day_bitmask, 2) == 0
  ) %>%
  write_csv("data_working/MODIS_LST.csv")

mod %>%
  sample_frac(0.1) %>%
  ggplot(aes(x=Day_view_angl, y=Latitude)) + 
  geom_density_2d_filled()

mod %>%
  group_by(ID) %>%
  summarize(prop_ok = sum(QC_Day_bitmask == 0) / n()) %>%
  arrange(prop_ok)
