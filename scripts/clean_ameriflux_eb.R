# This file pares down the raw ameriflux BASE data to the parts we need
# for energy balance modeling and comparison with the satellite record.
source("scripts/util.R")
library(tidyverse)
library(lubridate)
library(foreach)

data_dir <- "data_in/ameriflux_canopy_eb/"
base_files <- list.files(data_dir, pattern="BASE_HH")

read_ameriflux_base <- function(f) {
  target_vars <- c("TIMESTAMP", "T_CANOPY", "TA_", "PA", "RH", "VPD", "LW_IN", 
                   "LW_OUT", "SW_IN", "SW_OUT", "WS", "G", "H", "USTAR", "LE",
                   "SAP_FLOW")
  
  mybase <- read_csv(file.path(data_dir, f), skip=2, na=c("", "NA", "-9999"),
                     col_types = cols()) %>%
    select(contains(target_vars)) %>%
    select(-contains("WS_MAX")) %>%
    # Parse timestamp
    mutate(across(
      contains("TIMESTAMP"),
      function(x) parse_date_time(as.character(x), "%Y%m%d%H%M")
    ))
  
  # Drop rows where all of the canopy T sensors are NA. Other variables can be
  # NA but this is the main one we want.
  canopy_all_na <- apply(
    mybase[, grep("T_CANOPY", names(mybase))], 
    MARGIN = 1, 
    FUN = function(x) all(is.na(x))
  )
  mybase[!canopy_all_na, ]
}

# To eliminate qualifiers, take the measurement of each variable highest
# along the vertical profile.
for (f in base_files) {
  print(f)
  this_flux <- read_ameriflux_base(f)
  
  # Ignore the timestamps since these don't have qualifiers
  basenames <- unique(get_all_basenames(this_flux))
  basenames <- basenames[!str_detect(basenames, "TIMESTAMP")]
  site <- str_split_i(f, "_", 2)
  
  # Skeleton of the output
  max_height_flux <- data.frame(
    site=rep(site, nrow(this_flux))
  ) %>% cbind(this_flux %>% select(contains("TIMESTAMP")))
  
  for (bname in basenames) {
    max_height_flux[[bname]] <- rowMeans(
      this_flux[, get_highest_measurement(this_flux, bname)],
      na.rm=TRUE
    )
  }
  
  write_csv(
    max_height_flux,
    file.path("data_working/ameriflux_cleaned_max_heights", f)
  )
}





