#' Simple sed rate estimation from a median time estimate and depth
#'
#' @param time vector of age or year values
#' @param depth vector of depth values
#' @param smooth optionally smooth out sedrate curve to avoid unrealistic sharp jumps. NA (default) will do nothing. Specify degrees of freedom in spline to smooth. TRUE will use df = span in cm divided by 2
#'
#' @return estimate sed rate
#' @export
estimate_sed_rate_from_median <- function(time, depth ,smooth = NA){
  # time <- ts$age[[1]]
  # depth <- ts$depth[[1]]

  #get change in time
  dt <- diff(time)
  dd <- diff(depth)
  sr <- dd/dt
  depth_mid <- rowMeans(cbind(depth[-1],depth[-length(depth)]))

  bad <- which(!is.finite(sr) | !is.finite(depth_mid))

  if(length(bad) > 0){
    sr <- sr[-bad]
    depth_mid <- depth_mid[-bad]
  }

  if(!all(is.na(smooth))){
    if(!is.numeric(smooth)){
      smooth <- abs(diff(range(depth_mid)))/2
    }
    spline <- smooth.spline(depth_mid,y = sr, df = smooth)
    sr <- predict(spline)$y

  }

  srOut <- Hmisc::approxExtrap(depth_mid,sr,xout = depth)$y

  if(length(bad) > 0){
    srOut[bad] <- NA
  }
  return(srOut)

}
