#'
#' @title Get a set of connectivity zones
#' 
#' @description Function to get a set of connectivity zones as a \code{sf} dataframe.
#' 
#' @param type - type of connectivity zone to get ("2020","30-layer","2019", or "10-layer")
#' @param crs_type - type of crs ("LL" for lat/lon, "AA" for Alaska Albers projection)
#' 
#' @return \code{sf} dataframe with connectivity zones in given crs.
#' 
#' @details None.
#' 
#' @export
#' 
getConnectivityZones<-function(type=c("2020","30-layer","2019","10-layer"),
                               crs_type=c("LL","AA")){
  if (tolower(type[1]) %in% c("2020","30-layer")){
    message("Retrieving 2020/30-layer connectivity zones");
    if (tolower(crs[1])=="ll"){
      message("\tin lat/lon coordinates");
      fn<-system.file(file.path("extdata","ConnectivityZones2020.LL.RData"),package="rIBMsSnowCrab");
    } else {
      message("\tin Alaska Albers coordinates");
      fn<-system.file(file.path("extdata","ConnectivityZones2020.XY.RData"),package="rIBMsSnowCrab");
    }
  } else if (tolower(type[1]) %in% c("2019","10-layer")){
    message("Retrieving 2019/10-layer connectivity zones");
    if (tolower(crs[1])=="ll"){
      message("\tin lat/lon coordinates");
      fn<-system.file(file.path("extdata","ConnectivityZones2019.LL.RData"),package="rIBMsSnowCrab");
    } else {
      message("\tin Alaska Albers coordinates");
      fn<-system.file(file.path("extdata","ConnectivityZones2019.XY.RData"),package="rIBMsSnowCrab");
    }
  } else {
    stop(paste0("Error in getConnectivityZones. type='",type,"' not recognized"));
    return(NULL);
  }
  load(fn);#--loaded object is "obj"
  return(obj);
}