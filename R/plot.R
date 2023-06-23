#' Plot the key datasets in the flux calculation
#'
#' @param tib the output of estimate_chla_flux_lipd() or estimate_chla_flux()
#'
#' @return a ggplot object
#' @export
#' @import magrittr
plot_flux <- function(tib){

  if(all(is.na(tib$time))){#plot on depth
    long <- chla_flux_to_tibble(tib) %>%
      tidyr::pivot_longer(cols = -c("depth","time"),names_to = "variable")


  fluxPlot <- ggplot2::ggplot(long) +
    ggplot2::geom_line(ggplot2::aes(x = depth, y = value)) +
    ggplot2::facet_grid(factor(variable,levels = c("chla_flux","chla_conc","sed_rate","dbd")) ~ .,scales = "free_y",switch = "both") +
    ggplot2::theme_bw() +
    ggplot2::xlab("Depth (mm)") +
    ggplot2::ylab("")

  }else{
    long <- chla_flux_to_tibble(tib) %>%
      tidyr::pivot_longer(cols = -time,names_to = "variable")



    fluxPlot <- ggplot2::ggplot(long) +
      ggplot2::geom_line(ggplot2::aes(x = time, y = value)) +
      ggplot2::facet_grid(factor(variable,levels = c("chla_flux","chla_conc","sed_rate","dbd")) ~ .,scales = "free_y",switch = "both") +
      ggplot2::theme_bw() +
      ggplot2::xlab("Year AD") +
      ggplot2::ylab("")

  }

  return(fluxPlot)

}


heuristicUnits <- function(X,range.min = 25){
  if(is.list(X)){
    if(all(is.na(X$values))){
      return(NA)
    }
    miv <- min(X$values,na.rm = TRUE)
    mav <- max(X$values,na.rm = TRUE)
    rv <- diff(range(X$values, na.rm = TRUE))
  }else{
    if(all(is.na(X))){
      return(X)
    }
    miv <- min(X,na.rm = TRUE)
    mav <- max(X,na.rm = TRUE)
    rv <- diff(range(X, na.rm = TRUE))
  }

  #See if the highest value would be in the future for AD
  todayCheckAD <-  mav > as.numeric(substring(date(),21))

  #See if the lowest values would be in the future for BP
  todayCheckBP <- miv < 1950 - (as.numeric(substring(date(),21)))

  #see if range of values implies ka not BP
  rangeCheck <- rv < range.min

  #now work out some scenarios
  if(todayCheckBP & !todayCheckAD){
    unitGuess <- "AD"
  }else if(!todayCheckBP & todayCheckAD){
    unitGuess <- "BP"
  }else if(rangeCheck){
    unitGuess <- "ka"
  }else if(todayCheckBP & todayCheckAD){
    unitGuess <- "somethings wrong here, doesn't seem to be AD, BP, or ka"
  }else{
    if(miv > 0 & mav > 1900){
      unitGuess <- "AD"
    }else if(miv > 1950 - (as.numeric(substring(date(),21)))  & miv < 100){
      unitGuess <- "BP"
    }else{
      unitGuess <- "cant make a reasonable guess"
    }
  }

  return(unitGuess)
}
