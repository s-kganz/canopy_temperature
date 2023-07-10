library(tidyverse)
library(readxl)

target_meta <- c(
  "IGBP", "LOCATION_LAT", "LOCATION_LONG", "UTC_OFFSET", "URL_FLUXNET", "HEIGHTC"
)

meta_allsites <- read_xlsx(
  "data_in/fluxnet_fullset/FLX_AA-Flx_BIF_ALL_20200501/FLX_AA-Flx_BIF_HH_20200501.xlsx"
) %>%
  select(SITE_ID, VARIABLE, DATAVALUE) %>%
  filter(VARIABLE %in% target_meta) %>%
  pivot_wider(names_from=VARIABLE, values_from=DATAVALUE, values_fn=first) %>%
  mutate(
    across(c(LOCATION_LAT, LOCATION_LONG, UTC_OFFSET, HEIGHTC), parse_number)
  )

meta_allsites %>%
  write_csv("data_working/fluxnet_site_metadata.csv")

table(meta_allsites$IGBP)

meta_allsites %>%
  filter(IGBP %in% c("ENF", "EBF", "DNF", "DBF", "MF")) %>%
  ggplot(aes(x=HEIGHTC)) + geom_histogram()
