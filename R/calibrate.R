five_root <- function(x) x ^ (1/5)
five <- function(x) x ^ 5
minus_root <- function(x) (x-.9) ^ (1/10)
minus <- function(x) (x^10)+.9

#' Convert RABD to chl a concentration
#'
#' @param rabd660670 RABD 660-670
#' @param geoClass Geomorphic class of the lake
#'
#' @return chla concentrations in ug/g
#' @export
calibrate_rabd660670 <- function(rabd660670,geoClass){
  minRoot <- minus_root(rabd660670)
  calibration <-  five(-7.800122 +(11.481781 * minRoot))
  return(calibration)
}
