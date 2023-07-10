# A port of the leaf temperature model described in Kibler et al. (2023).
# Equations come from either:
#   The paper: https://www.sciencedirect.com/science/article/pii/S0168192323002514
#   The GitHub repo: https://github.com/kiblerchris/Kibler_et_al_AFM_2023

#' Net radiation available at the leaf surface for a forced canopy condition. Use
#' this version when modeling a non-transpiring canopy to account for feedbacks
#' between leaf temperature and outgoing longwave radiation.
#'
#' @param sw_in Incoming shortwave radiation (W/m2)
#' @param sw_out Outgoing shortwave radiation (W/m2)
#' @param lw_in Incoming longwave radiation (W/m2)
#' @param t_leaf Leaf temperature (K)
#' @param g Soil heat flux (W/m2).
#' @param e Leaf emissivity
#'
#' @return *Qa*, net radiation available at the leaf surface (W/m2).
#' @export
#'
#' @examples
net_radiation_forced <- function(sw_in, sw_out, lw_in, t_leaf, g=0, e=0.98) {
  sb_constant <- 5.6704e-8 # Stefan-Boltzmann constant for blackbody radiation
  return(
    sw_in + e*lw_in - sw_out - e*sb_constant*t_leaf^4
  )
}

#' Resistance to sensible heat transfer. Ignores stability functions and uses
#' an approximation for the excess resistance from Thom (1973). See SI in
#' Kibler et al. (2023).
#'
#' @param u Wind speed (m/sec)
#' @param ustar Friction velocity (m/sec)
#'
#' @return
#' @export *rH*, resistance to sensible hat transfer (m/sec).
#'
#' @examples
resistance_sensible_heat <- function(u, ustar) {
  k    <- 0.41 # von Karman constant
  r_am <- u / (ustar^2)
  kb   <- 1.35 * k * (100 * ustar)^(1/3)
  r_bh <- kb / (k * ustar)
  return(r_am + r_bh)
}

#' Partial pressure of water vapor using Tetens' formula.
#'
#' @param ta Air temperature (K)
#' @param rh Relative humidity (% 0-100) 
#'
#' @return *ea*, partial pressure of water vapor (kPa).
#' @export
#'
#' @examples
vapor_pressure <- function(ta, rh) {
  ta_c <- ta - 273.15
  es <- 0.61078 * exp((17.27 * ta_c) / (ta_c + 237.3))
  ea <- rh/100 * es
  return(ea)
}

#' Density of moist air. Based on Bonan, Ecological Climatology (2016) eq. 3.29.
#'
#' @param ta Air temperature (K)
#' @param pa Air pressure (kPa)
#' @param rh Relative humidity (%)
#'
#' @return *mad*, air density (kg / m3)
#' @export
#'
#' @examples
moist_air_density <- function(ta, pa, rh) {
  M <- 28.97 / 1000 # Molecular mass of dry air (kg/mol)
  R <- 8.314 # Universal gas constant (J / K mol)
  # Convert both pressures to Pa
  ea <- vapor_pressure(ta, rh) * 1000
  pa_pa <- pa * 1000
  
  mad <- (pa_pa * M) / (R * ta) * (1 - 0.378 * (ea / pa_pa))
  return(mad)
}

#' Heat capacity of moist air at constant pressure.
#'
#' @param ta Air temperature (K)
#' @param pa Air pressure (kPa) 
#' @param rh Relative humidity (%)
#' @param cpd Heat capacity of dry air (J / kg K)
#' @param cpv Heat capacity of water vapor (J / kg K)
#'
#' @return *cp*, heat capacity of air (J / kg K)
#' @export
#'
#' @examples
heat_capacity_const_pressure <- function(ta, pa, rh, cpd=1005, cpv=1875) {
  ea <- vapor_pressure(ta, rh)
  w <- 0.622 * (ea/(pa-ea))
  cp <- (1-w) * cpd * (1 + ((cpv/cpd) * w))
  return(cp)
}

#' Directly calculate leaf temperature using the energy balance model in
#' Kibler et al. (2023). Use this version if modeling leaf temperature directly
#' from flux tower data. The evaporative fraction can either be specified
#' directly or calculated from latent heat flux. If both `fe` and `le` are
#' provided, `fe` takes priority.
#'
#' @param ta Air temperature (K)
#' @param rh Relative humidity (% 0-100)
#' @param pa Air pressure (kPa)
#' @param u Wind speed (m/sec)
#' @param ustar Friction velocity (m/sec)
#' @param netrad Net radiation (W/m2)
#' @param g Soil heat flux (W/m2)
#' @param k Number of faces of leaf participating in heat transfer.
#' @param fe Evaporative fraction, ratio of latent heat flux to net radiation
#' @param le Latent heat flux (W/m2).
#'
#' @return *Tl*, modeled leaf temperature (K)
#' @export
#'
#' @examples
leaf_temperature_direct <- function(
    ta, rh, pa, u, ustar, netrad, fe=NULL, le=NULL, g=0, k=1
) {
  if (is.null(fe) & is.null(le)) {
    stop("Either evaporative fraction (fe) or latent heat flux (le) must
         be specified.")
  }
  
  Qa <- netrad - g
  rho <- moist_air_density(ta, pa, rh)
  cp <- heat_capacity_const_pressure(ta, pa, rh)
  resis <- resistance_sensible_heat(u, ustar)
  
  if (is.null(fe)) {
    fe <- le / Qa
  }
  
  Tl <- ta + (1 - fe) * (Qa * resis) / (k * rho * cp)
  return(Tl)
}

leaf_temperature_error <- function(
    tl_guess, ta, rh, pa, u, ustar, sw_in, sw_out, lw_in, e=0.98, 
    fe=NULL, le=NULL, g=0, k=1
) {
  Qa <- net_radiation_forced(sw_in, sw_out, lw_in, tl_guess, g, e)
  rho <- moist_air_density(ta, pa, rh)
  cp <- heat_capacity_const_pressure(ta, pa, rh)
  resis <- resistance_sensible_heat(u, ustar)
  
  if (is.null(fe)) {
    fe <- le / Qa
  }
  
  tl_calc <- ta + (Qa * resis * (1-fe)) / (k * rho * cp)
  error <- abs(tl_calc - tl_guess)
  return(error)
}

#' Calculate leaf temperature numerically from hypothetical conditions using
#' the model in Kibler et al. (2023). Use this version when `fe`, `le` or other
#' parameters do not come from flux tower measurements. This version solves the
#' energy balance numerically, accounting for the feedback between leaf
#' temperature and outgoing longwave radiation.
#'
#' @param ta 
#' @param rh 
#' @param pa 
#' @param u 
#' @param ustar 
#' @param sw_in 
#' @param sw_out 
#' @param lw_in 
#' @param e 
#' @param fe 
#' @param le 
#' @param g 
#' @param k 
#'
#' @return
#' @export
#'
#' @examples
leaf_temperature_forced <- function(
    ta, rh, pa, u, ustar, sw_in, sw_out, lw_in, e=0.98, fe=NULL, le=NULL, g=0, k=1
) {
  if (is.null(fe) & is.null(le)) {
    stop("Either evaporative fraction (fe) or latent heat flux (le) must
         be specified.")
  }
  
  # Solve the energy balance model numerically.
  solve_result <- optimize(
    function(tl) {leaf_temperature_error(tl, ta, rh, pa, u, ustar, sw_in, sw_out,
                                         lw_in, e, fe, le, g, k)},
    interval=c(0, 1e3)
  )
  return(solve_result$minimum)
}

leaf_temperature_forced <- purrr::safely(leaf_temperature_forced)

