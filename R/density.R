#' Estimate wet bulk density from depth
#'
#' @param dblf depth below lake floor in cm
#'
#' @return wet bulk density in g/cm3
#' @export
#'
#' @examples
#' wbd <- estimate_wbd(seq(1,150))
estimate_wbd <- function(dblf){
  bulkDensityEst <- log(dblf+1)/12+.9
}


#' Estimate wet bulk density from depth
#'
#' @param dblf depth below lake floor in cm
#'
#' @return dry bulk density in g/cm3
#' @export
#'
#' @examples
#' dbd <- estimate_dbd(seq(1,150))
estimate_dbd <- function(dblf){
  bulkDensityEst <- log(dblf+1)/9+.3
}
