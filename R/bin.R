#' Bin a vector
#'
#' @param time time vector
#' @param values value vector
#' @param bin.vec edges of the bins
#' @param bin.fun function to bin with
#'
#' @return data frame
#' @export
bin <- function(time,values,bin.vec,bin.fun = mean){
  bin_y = rep(NA,times = length(bin.vec)-1)
  bin_x = apply(cbind(bin.vec[-1],bin.vec[-length(bin.vec)]),1,mean)

  for(i in 1:length(bin_y)){
    be <- sort(bin.vec[i:(i+1)])
    q = which(time > be[1] & time <= be[2])
    bin_y[i] = bin.fun(values[q],na.rm=TRUE)
  }

  binned = data.frame(x=bin_x,y=bin_y)
  return(binned)
}
