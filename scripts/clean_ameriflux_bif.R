# This script pulls useful metadata from the ameriflux BIF files

library(tidyverse)
library(readxl)
library(lubridate)
library(foreach)

data_dir <- "data_in/ameriflux_canopy_eb/"
base_files <- list.files(data_dir, pattern="BIF")

target_vars <- c(
  "MAT", "MAP", "CLIMATE_KOEPPEN", "IGBP", "LOCATION_LAT", "LOCATION_LONG",
  "TERRAIN", "ASPECT", "UTC_OFFSET", "REFERENCE_PAPER"
)

# mybif <- read_xlsx(file.path(data_dir, base_files[1]))

bif_metadata <- foreach(f=base_files, .combine=bind_rows) %do% {
  mybif <- read_xlsx(file.path(data_dir, f))
  values <- mybif$DATAVALUE[match(target_vars, mybif$VARIABLE)]
  
  out <- c(
    list(SITE_ID=mybif$SITE_ID[1]),
    setNames(as.list(values), target_vars)
  )
}

write_csv(bif_metadata, file="data_working/ameriflux_site_metadata.csv")
