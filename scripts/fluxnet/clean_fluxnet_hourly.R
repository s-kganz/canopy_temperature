library(tidyverse)
library(foreach)

data_dir <- file.path("data_in", "fluxnet_fullset")

forest_igbp <- c("ENF", "EBF", "DNF", "DBF", "MF")
fluxnet_meta <- read_csv("data_working/fluxnet_site_metadata.csv") %>%
  filter(IGBP %in% forest_igbp)

# Unzip all the compressed archives for the forest sites
# for (site in fluxnet_meta$SITE_ID) {
#   # Find the archive for this site
#   archive <- list.files(path=data_dir, 
#                         pattern=paste0("FLX_", site, ".*\\.zip"),
#                         full.names=TRUE)
#   
#   if (length(archive) != 1) {
#     print(paste("Archive for site", site, "not found"))
#   }
#   
#   # Unzip the archive
#   outdir <- gsub(".zip", "", archive)
#   status <- system(paste("unzip", archive, "-d", outdir), ignore.stdout=TRUE)
#   if (status != 0) {
#     stop(paste("Problem with site", site))
#   }
# }

# Check that names are consistent across files
hourly_flux_files <- list.files(data_dir, "FULLSET_HH",
                                recursive=TRUE, full.names=TRUE)

# Which files have the data necessary for T_lw or T_aero calculation?
t_lw_vars <- c(
  "LW_IN_F", "LW_IN_F_QC", "LW_OUT"
)
# Using the parameterization in Kibler et al. (2023)
t_aero_vars <- c(
  "TA_F", "TA_F_QC", "PA_F", "PA_F_QC", "WS_F", "WS_F_QC",
  "USTAR", "RH", "NETRAD", "G_F_MDS", "G_F_MDS_QC",
  "LE_F_MDS", "LE_F_MDS_QC", "H_F_MDS", "H_F_MDS_QC"
)

# missing_t_lw <- c()
# missing_t_aero <- c()
# for (f in hourly_flux_files) {
#   this_names <- names(read_csv(f, col_types=cols()))
#   if (length(setdiff(t_lw_vars, this_names)) > 0) {
#     missing_t_lw <- c(missing_t_lw, f)
#   }
#   if (length(setdiff(t_aero_vars, this_names)) > 0) {
#     missing_t_aero <- c(missing_t_aero, f)
#   }
# }

# More sites have the vars needed for T_aero than T_lw. So, although T_lw is
# easier to calculate, we will go with T_aero instead.
all_flux <- foreach(f=hourly_flux_files, .combine=bind_rows) %do% {
  this_site <- str_match(f, "FLX_\\s*(.*?)\\s*_FLUXNET")[, 2]
  this_f <- read_csv(f, na=c("", "NA", "9999"), col_types = cols()) %>%
    mutate(SITE=this_site) %>%
    select(SITE, TIMESTAMP_START, TIMESTAMP_END,
           any_of(t_aero_vars),
           any_of(t_lw_vars))
}

all_flux_qc <- all_flux %>%
  filter(
    # Rows must have either all vars for T_aero or all vars for T_lw
    if_all(t_aero_vars, ~ !is.na(.x)) | if_all(t_lw_vars, ~ !is.na(.x)),
    # QC schema for _MDS and _F vars are different but in both cases a flag
    # < 2 is desirable.
    if_all(contains("QC"), ~ .x < 2)
  ) %>%
  # Calculate local time using UTC offset in metadata
  left_join(
    fluxnet_meta %>% select(SITE_ID, UTC_OFFSET),
    by=c("SITE"="SITE_ID")
  ) %>%
  mutate(
    across(
      contains("TIMESTAMP"), 
      ~ parse_datetime(as.character(.x), format="%Y%m%d%H%M")
    ),
    TIMESTAMP_START_LOCAL = TIMESTAMP_START + hours(as.integer(UTC_OFFSET)),
    TIMESTAMP_END_LOCAL   = TIMESTAMP_END + hours(as.integer(UTC_OFFSET))
  )

all_flux_qc %>%
  write_csv("data_working/fluxnet_forest_cleaned.csv")
