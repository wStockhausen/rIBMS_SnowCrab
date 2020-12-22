#'
#' @title Get a \pkg{sf} dataframe representing a directed web for a set of connectivity zones
#' 
#' @description Function to get a \pkg{sf} dataframe representing a directed web for a set of connectivity zones.
#' 
#' @param type - type of connectivity zone to get ("2020","30-layer","2019", or "10-layer")
#' @param crs_type - type of crs ("LL" for lat/lon, "AA" for Alaska Albers projection)
#' 
#' @return \pkg{sf} dataframe with a directed web for the connectivity zones in given crs.
#' 
#' @details None.
#' 
#' @export
#' 
getConnectivityZonesWeb<-function(type=c("2020","30-layer","2019","10-layer"),
                                 crs_type=c("LL","AA")){
  if (tolower(type[1]) %in% c("2020","30-layer")){
    message("Retrieving 2020/30-layer connectivity zone web");
    if (tolower(crs_type[1])=="ll"){
      message("\tin lat/lon coordinates");
      fn<-system.file(file.path("extdata","CZArrows2020.LL.RData"),package="rIBMsSnowCrab");
    } else {
      message("\tin Alaska Albers coordinates");
      fn<-system.file(file.path("extdata","CZArrows2020.XY.RData"),package="rIBMsSnowCrab");
    }
  } else if (tolower(type[1]) %in% c("2019","10-layer")){
    message("Retrieving 2019/10-layer connectivity zones");
    if (tolower(crs_type[1])=="ll"){
      message("\tin lat/lon coordinates");
      fn<-system.file(file.path("extdata","CZArrows2019.LL.RData"),package="rIBMsSnowCrab");
    } else {
      message("\tin Alaska Albers coordinates");
      fn<-system.file(file.path("extdata","CZArrows2019.XY.RData"),package="rIBMsSnowCrab");
    }
  } else {
    stop(paste0("Error in getConnectivityZonesWeb. type='",type,"' not recognized"));
    return(NULL);
  }
  load(fn);#--loaded object is "obj"
  return(obj);
}