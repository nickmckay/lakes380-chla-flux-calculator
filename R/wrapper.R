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
#' @param dbd a vector of dry bulk density value the same length as depth, or NA to use estimated DBD (default)
#' @param rabd660670 RABD 660 670 vector
#' @param geoClass geoclass
#' @inheritParams estimate_sed_rate_from_median
#'
#' @return a tibble with output data
#' @export
#'
estimate_chla_flux <- function(depth,
                               time = NA,
                               dbd = NA,
                               rabd660670,
                               geoClass,
                               smooth = TRUE,
                               max.time = NA){

  if(is.na(max.time) | all(is.na(time))){
    time.index <- seq_along(depth)
  }else{
    time.index <- which(time < max.time)
  }

  if(length(depth) != length(rabd660670)){
    stop(glue::glue("depth (n = {length(depth)}) and RABD660670 (n = {length(rabd660670)}) must be the same the length"))
  }

  if(!all(is.na(time))){
    if(length(depth) != length(time)){
      stop(glue::glue("depth (n = {length(depth)}) and time (n = {length(time)}) must be the same the length"))
    }
  }


  if(!all(is.na(dbd))){
    if(length(depth) != length(dbd)){
      stop(glue::glue("depth (n = {length(depth)}) and dbd (n = {length(dbd)}) must be the same the length"))
    }
  }


  #calculate sub-components
  chla_conc <- calibrate_rabd660670(rabd660670[time.index],geoClass)
  sed_rate <- estimate_sed_rate_from_median(time[time.index],depth[time.index],smooth = smooth)

  if(all(is.na(dbd))){
    dbd <- estimate_dbd(depth[time.index])
  }

  #calculate fluc
  chla_flux <- chla_conc * sed_rate * dbd

  #add new columns to a tibble
  mts <- tibble::tibble(time = list(time[time.index]),
                        depth = list(depth[time.index]),
                        chla_flux = list(chla_flux),
                        chla_conc = list(chla_conc),
                        sed_rate = list(sed_rate),
                        dbd = list(dbd))

  return(mts)
}
