# This file calculates canopy-scale g1 for the Medlyn stomatal conductance
# model.

library(tidyverse)
library(bigleaf)
library(RcppRoll)
library(robustbase)
source("scripts/energy_balance/air.R")
source("scripts/energy_balance/leaf.R")
source("scripts/tower_util.R")

RAIN_FILTER <- 3 # days

meta <- read_csv("data_working/neon_site_metadata.csv") %>%
  left_join(read_csv("data_out/neon_sampled_dhp_lai.csv"), 
            by=c("site_neon"="site")) %>%
  mutate(L_best = ifelse(is.na(lai), L_dhp, lai))

do_medlyn_fit <- function(tower, d, site, roughness_d, roughness_z0m, zr, zh) {
  # Filtering ----
  # See p. 697 in Knauer et al. (2018)
  tower_filter <- tower %>%
    filter(
      TA > 5,
      RH < 95,
      LE > 0,
      NETRAD - G > 0
    )
  # Calculate conductances ----
  tower_gs <- tower_filter %>%
    bind_cols(
      # Calculate Ga_h and Gb_h
      aerodynamic.conductance(
        .,
        .$TA,
        .$PA,
        .$WS,
        .$USTAR,
        .$H,
        zr = zr,
        zh = zh,
        d = roughness_d,
        z0m = roughness_z0m,
        # Dl is leaf characteristic dimension - we are working with needleleaves
        # here and other papers use this estimate
        Dl = d,
        wind_profile = TRUE
      )
    ) %>%
    mutate(
      # Calculate corrected air T, CO2 conc, VPD after Knauer et al. (2018). These
      # seem to be less accurate than top-of-canopy measurements based on
      # instrumentation lower down on the tower so I don't use them.
      rho = dry_air_density(TA + 273.15, PA),
      gamma = psychrometric_constant(PA),
      e = saturation_vapor_pressure(TA + 273.15) * (1 - RH / 100),
      TA_corr = TA + H / (rho * Ga_h * eb_constants_$cp),
      e_corr = e + (LE * gamma) / (rho * Ga_h * eb_constants_$cp),
      VPD_corr = saturation_vapor_pressure(TA_corr + 273.15) - e_corr,
      CO2_corr = CO2 + NEE / Ga_h
    ) %>%
    bind_cols(
      # Finally, calculate surface conductance!
      surface.conductance(
        .,
        Tair = .$TA,
        pressure = .$PA,
        Rn = .$NETRAD,
        G = .$G,
        S = 0,
        VPD = .$VPD,
        LE = .$LE,
        Ga = .$Ga_h
      )
    ) %>%
    filter(
      # Discard NAs, negatives
      Gs_mol >= 0, !is.na(Gs_mol)
    )
  
  # Fit Medlyn model ----
  
  # A couple options for fitting.
  
  # V1: numerical optimization. This is more sensitive to outliers, mitigate by
  # weighting by sqrt(VPD)
  # medlyn_optimizer_helper <- function(Gs, VPD, GPP, CO2) {
  #   optimize(function(g1) {
  #     sum(((medlyn_gs(GPP, VPD, CO2, g1) - Gs) ^ 2) * sqrt(VPD))
  #   },
  #   c(1, 10))
  # }
  # 
  # fit_v1 <-
  #   with(tower_gs, medlyn_optimizer_helper(Gs_mol, VPD, GPP_U50_f, CO2))
  
  # V2: Theil-Sen. This option is less sensitive to outliers but it doesn't
  # strictly follow the model specification.
  # fit_v2 <- theil_sen_regression(Gs_mol ~ GPP_U50_f / (sqrt(VPD) * CO2), data=tower_gs)
  
  # V3: robustbase. This is the method used in Knauer et al. (2018) 
  fit_v3 <- nlrob(
    Gs_mol ~ (1 + g1 / sqrt(VPD)) * (GPP_uStar_f / CO2),
    data=tower_gs,
    algorithm="port",
    start=c(g1=3),
    lower=c(g1=1),
    upper=c(g1=10)
  )
  
  str(coef(fit_v3))
  
  tower_gs$Gs_mol_predicted <- medlyn_gs(
    tower_gs$GPP_uStar_f, tower_gs$VPD, tower_gs$CO2, unname(coef(fit_v3))
  )
  tower_gs$site <- site
  
  list(site=site,
       g0 = 0,
       g1 = unname(coef(fit_v3)),
       data = tower_gs)
}

neon_fit_medlyn <- function(site, tower, flux, zr, zh, L) {
  cat("Now processing", site, "\n")
  # Select needed columns from tower ----
  
  # For CO2, the MIXING_RATIO name means the mole fraction is per mole of dry 
  # air, while the CO2 name means the mole fraction is per mole of wet air. 
  # Both are in ppm and I don't think the differences matters so just lump 
  # the two versions together.
  co2_regex <- "CO2.+1_1_\\d"
  sw_regex  <- "SW_IN_1_1_\\d"
  ta_regex  <- "TA_1_1_\\d"
  g_regex   <- "G_1_1_\\d"
  
  # Other vars that don't need to be averaged. Don't pull LE because we use the
  # quality controlled fluxes from NEON.
  req_vars <-
    c("TIMESTAMP",
      "WS_1_1_1",
      "USTAR",
      "P",
      "PA",
      "RH",
      "H",
      "LW_IN",
      "LW_OUT",
      "SW_OUT")
  
  tower_select <- tower %>%
    select(all_of(req_vars)) %>%
    mutate(
      SW_IN = rowMeans(tower %>% select(matches(sw_regex)), na.rm = TRUE),
      TA = rowMeans(tower %>% select(matches(ta_regex)), na.rm = TRUE),
      CO2 = rowMeans(tower %>% select(matches(co2_regex)), na.rm = TRUE),
      G = rowMeans(tower %>% select(matches(g_regex)), na.rm = TRUE),
      NETRAD = SW_IN + LW_IN - SW_OUT - LW_OUT,
      VPD = vapor_pressure_deficit(TA + 273.15, RH)
    ) %>%
    rename(WS = WS_1_1_1) %>% drop_na()
  
  cat("Initial # observations:", nrow(tower_select), "\n")
  
  # Identify periods where T/ET is close to 1 ----
  no_precip <- tower_select %>%
    mutate(date = as.Date(TIMESTAMP)) %>%
    group_by(date) %>%
    summarize(P = sum(P, na.rm = TRUE)) %>%
    mutate(P_roll_sum = roll_sumr(P, n = RAIN_FILTER+1)) %>%
    filter(P_roll_sum == 0)
  
  tower_transp_only <- tower_select %>%
    filter(as.Date(TIMESTAMP) %in% no_precip$date) %>%
    # Drop night time data
    filter(SW_IN > 100)
  
  cat("...after T/ET filtering", nrow(tower_transp_only), "\n")
  
  # Calculate roughness parameters ----
  roughness <- roughness.parameters(
    method = "wind_profile",
    zh = zh,
    zr = zr,
    Tair = tower_transp_only$TA,
    pressure = tower_transp_only$PA,
    wind = tower_transp_only$WS,
    ustar = tower_transp_only$USTAR,
    H = tower_transp_only$H
  )
  
  # Join QC'd flux measurements ----
  tower_transp_only_flux <- tower_transp_only %>%
    inner_join(flux, by = c("TIMESTAMP" = "timeBgn")) %>%
    rename(LE = data.fluxH2o.nsae.flux,
           NEE = data.fluxCo2.nsae.flux)
  
  cat("...after joining flux", nrow(tower_transp_only_flux), "\n")
  
  if (nrow(tower_transp_only_flux) < 50) {
    cat("Too few observations! Returning NA...\n")
    return(list(site=site, g0=NA, g1=NA, data=NA))
  } else {
    return(do_medlyn_fit(tower_transp_only_flux, 0.01, site, 
                         roughness$d, roughness$z0m, zr, zh))
  }
}

neon_fit_driver <- function() {
  flux <- read_csv("data_out/cross_site_flux_partition_qc.csv")
  
  medlyn_fits <- lapply(1:nrow(meta), function(i) {
    this_site_amf  <- meta$site_ameriflux[i]
    this_site_neon <- meta$site_neon[i]
    this_zr        <- meta$tower_height[i]
    this_zh        <- meta$canopy_height[i]
    this_L         <- meta$L_best[i]
    
    this_flux <- flux %>%
      filter(site == this_site_neon)
    
    if (nrow(this_flux) == 0) {
      cat(this_site_neon, "has no flux! Skipping...\n")
      return(list(site=this_site_amf, g0=NA, g1=NA, data=NA))
    }
    
    this_tower_file <- file.path("data_working/neon_flux", str_c(this_site_amf, ".csv"))
    
    this_tower <- read_csv(this_tower_file, col_types=cols()) # Quiet, you!
    
    neon_fit_medlyn(this_site_amf, this_tower,
                    this_flux, this_zr, this_zh, this_L)
  })
  
  medlyn_data <- lapply(medlyn_fits, function(x) x$data)
  medlyn_df <- medlyn_data[!is.na(medlyn_data)] %>% bind_rows()
  medlyn_coefs <- lapply(medlyn_fits, function(x) x[-4]) %>% bind_rows()
  
  write_if_not_exist(medlyn_df, "data_out/cross_site_medlyn_fit_results.csv")
  write_if_not_exist(medlyn_coefs, "data_out/cross_site_medlyn_coefficients.csv")
}

old_wref_fit_driver <- function() {
  wref_flux <- read_csv("data_out/old_wref_flux_partition.csv")
  wref_meta <- meta %>% filter(site_neon == "WREF")

  wref_zr <- wref_meta$tower_height
  wref_zh <- wref_meta$canopy_height
  wref_L  <- wref_meta$L_best
  
  wref_tower <- read_csv("data_out/old_wref_clean.csv") %>%
    # Calculate additional cols to match what later functions expect
    mutate(
      TA = TA_70,
      WS = WS_70,
      RH = RH_70,
      NETRAD = SW_IN + LW_IN - SW_OUT - LW_OUT,
      VPD = vapor_pressure_deficit(TA + 273.15, RH)
    )
  
  # Filter to transpiration only
  no_precip <- wref_tower %>%
    mutate(date = as.Date(TIMESTAMP)) %>%
    group_by(date) %>%
    summarize(P = sum(P, na.rm = TRUE)) %>%
    mutate(P_roll_sum = roll_sumr(P, n = RAIN_FILTER+1)) %>%
    filter(P_roll_sum == 0)
  
  wref_transp_only <- wref_tower %>%
    filter(as.Date(TIMESTAMP) %in% no_precip$date) %>%
    # Drop night time data
    filter(SW_IN > 100)
  
  # Join partitioned flux, drop missing values
  wref_tower_flux <- wref_transp_only %>%
    inner_join(
      wref_flux %>% select(DateTime, GPP_uStar_f),
      by=c("TIMESTAMP"="DateTime")
    ) %>%
    filter(
      !is.na(CO2), !is.na(H)
    )
  
  # Calculate roughness parameters
  wref_roughness <- roughness.parameters(
    method = "wind_profile",
    zh = wref_zh,
    zr = wref_zr,
    Tair = wref_transp_only$TA_70,
    pressure = wref_transp_only$PA,
    wind = wref_transp_only$WS_70,
    ustar = wref_transp_only$USTAR,
    H = wref_transp_only$H
  )
  
  # Pass to fitting function
  wref_fit <- do_medlyn_fit(
    wref_tower_flux,
    0.01,
    "US-xWR",
    wref_roughness$d,
    wref_roughness$z0m,
    wref_zr,
    wref_zh
  )
  
  # Write results
  write_if_not_exist(wref_fit$data, "data_out/old_wref_medlyn_fit_result.csv")
  
  # Save roughness and g1
  wref_pars <- c(wref_fit[2:3], wref_roughness)
  saveRDS(wref_pars, file="data_out/old_wref_medlyn_roughness_params.rds")
}

if (sys.nframe() == 0) {
  neon_fit_driver()
  old_wref_fit_driver()
}
