#' Do the chl a flux calculations from a lipd object
#'
#' @param ts a lipd-ts-tibble object, or somethign that can be converted to one.
#' @inheritParams estimate_sed_rate_from_median
#' @import lipdR
#'
#' @return a tibble with output data
#' @export
estimate_chla_flux_lipd <- function(ts,smooth = TRUE){
  ts <- lipdR::as.lipdTsTibble(ts)
  rabd_ts <- dplyr::filter(ts,paleoData_variableName == "RABD660670")

  if(nrow(rabd_ts) > 1){
    stop("need to pick which column")
  }

  #pull relevant vectors and metadata
  time <- rabd_ts$age[[1]]
  depth <- rabd_ts$depth[[1]]
  rabd660670 <- rabd_ts$paleoData_values[[1]]
  geoClass <- rabd_ts$geo_geoClass

  mts <- estimate_chla_flux(depth, time,rabd660670, geoClass, smooth = smooth )

  ots <- dplyr::bind_cols(rabd_ts,mts)
  return(ots)
}

#' Do the chl a flux calculations
#'
#' @param depth depth in cm
#' @param time time in BP or AD
#' @param rabd660670 RABD 660 670 vector
#' @param geoClass geoclass
#' @inheritParams estimate_sed_rate_from_median
#'
#' @return a tibble with output data
#' @export
#'
estimate_chla_flux <- function(depth, time = NA, rabd660670, geoClass, smooth = TRUE){
  #calculate sub-components
  chla_conc <- calibrate_rabd660670(rabd660670,geoClass)
  sed_rate <- estimate_sed_rate_from_median(time,depth,smooth = smooth)
  dbd <- estimate_dbd(depth)

  #calculate fluc
  chla_flux <- chla_conc * sed_rate * dbd

  #add new columns to a tibble
  mts <- tibble::tibble(time = list(time),
                        depth = list(depth),
                        chla_flux = list(chla_flux),
                        chla_conc = list(chla_conc),
                        sed_rate = list(sed_rate),
                        dbd = list(dbd))

  return(mts)
}
