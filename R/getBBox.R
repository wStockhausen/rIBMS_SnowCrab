#' 
#' @title Get the standard bbox for maps of model output
#' 
#' @description Function to get the standard bbox for maps of model output
#' 
#' @return \code{sf}-style version of standard bounding box in WGS84
#' 
#' @import magrittr
#' @importFrom sf st_crs
#' @importFrom wtsGIS transformBBox
#' 
#' @export
#' 
getBBox<-function(ll=TRUE){
  if (ll){
    bbx = c(xmin=168,ymin=51.67,xmax=203,ymax=68.49);
    bbx %<>% wtsGIS::transformBBox(sf::st_crs(4326));
  } else {
    bbx = c(xmin=-2641743.2,xmax=-123570.1,ymin=415108.7,ymax=2640678.2);
    bbx %<>% wtsGIS::transformBBox(sf::st_crs(3338));
  }
  return(bbx);
}