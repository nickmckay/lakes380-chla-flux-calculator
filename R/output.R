#' Create a tibble from the flux output
#'
#' @param tib the output of estimate_chla_flux_lipd or estimate_chla_flux
#'
#' @return a tibble of output data
#' @export
chla_flux_to_tibble <- function(tib){
  unitGuess <- heuristicUnits(tib$time[[1]])

  if(unitGuess == "BP"){
    tib$time[[1]] <- 1950-tib$time[[1]]
  }

  long <- tib %>%
    tidyr::unchop(cols = c("time","chla_flux","chla_conc","sed_rate","dbd")) %>%
    dplyr::select(c("time","chla_flux","chla_conc","sed_rate","dbd"))
  return(long)
}
