library(tidyverse)
library(lubridate)

data_dir <- "data_in/ameriflux_canopy_eb/"
base_files <- list.files(data_dir, pattern="BASE_HH")

read_ameriflux_base <- function(f) {
  target_vars <- c("TIMESTAMP", "T_CANOPY", "TA_", "PA", "RH", "VPD", "LW_IN", 
                   "LW_OUT", "SW_IN", "SW_OUT", "WS")
  
  mybase <- read_csv(file.path(data_dir, f), skip=2, na=c("", "NA", "-9999")) %>%
    select(contains(target_vars)) %>%
    select(-contains("WS_MAX")) %>%
    # Parse timestamp
    mutate(across(
      contains("TIMESTAMP"),
      function(x) parse_date_time(as.character(x), "%Y%m%d%H%M")
    ))
  
  # Drop rows where all of the canopy T sensors are NA
  all_na <- apply(
    mybase[, grep("T_CANOPY", names(mybase))], 
    MARGIN = 1, 
    FUN = function(x) all(is.na(x))
  )
  mybase[!all_na, ]
}

for (f in base_files) {
  read_ameriflux_base(f) %>% write_csv(
    file.path("data_working/ameriflux_cleaned", f)
  )
}






