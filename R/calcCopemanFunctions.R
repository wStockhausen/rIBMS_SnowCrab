#' 
#' @title Calculate temperature-dependent snow crab growth rates
#' 
#' @description Function to calculate temperature-dependent snow crab growth rates.
#' 
#' @param T - vector of temperatures
#' 
#' @return  vector of growth rates
#' 
#' @export
#' 
calcCopeman_GrowthRate<-function(T){
  gr = 0.011+0.0020*T+0.0002*T^2-1.7015E-005*T^3;
  return(gr);
}

#' 
#' @title Calculate temperature-dependent snow crab cumulative survival, C1-C5
#' 
#' @description Function to calculate temperature-dependent snow crab cumulative survival, C1-C5.
#' 
#' @param T - vector of temperatures
#' 
#' @return  vector of survival probablities
#' 
#' @export
#' 
calcCopeman_Survival<-function(T){
  s = 11.19 + 26.22*T -3.60*T^2 + 0.12*T^3;
  return(s);
}

