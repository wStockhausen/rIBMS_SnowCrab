#'
#' @title Get a package layer object
#' 
#' @description Function to get a package layer dataset
#' 
#' @param name - name of type of object to return
#' @param xyType - "LL" or "AA" for lat/lon or Alaska Albers coordinates
#' @param bwType - "BW" or "RGB" for black-and-white or color
#' 
#' @return object
#'   
#' @details Following names are understood:
#' \itemize{
#' \item{Land}
#' \item{BathymContours}
#' \item{BathymLegend}
#' }
#' 
#' @export
#' 
getPackageData<-function(name,
                         xyType=c("LL","AA"),
                         bwType=c("BW","RGB")){
  if (tolower(name)=="land") {
    fn = paste0("extdata/Land.",toupper(xyType),".RData");
    return(wtsUtilities::getObj(system.file(fn,package="rIBMsSnowCrab")));
  }
  
  if (tolower(name)=="bathymcontours"){
    fn = paste0("extdata/BathymContours.",toupper(xyType),".RData");
    return(wtsUtilities::getObj(system.file(fn,package="rIBMsSnowCrab")));
  } 
  
  if (tolower(name)=="bathymlegend"){
    fn = paste0("extdata/BathymLegend.",toupper(bwType),".RData");
    return(wtsUtilities::getObj(system.file(fn,package="rIBMsSnowCrab")));
  }
}