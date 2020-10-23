#' 
#' @title Get the standard bbox for maps of model output
#' 
#' @description Function to get the standard bbox for maps of model output
#' 
#' @return \code{sf}-style version of standard bounding box in WGS84
#' 
#' @import magrittr
#' 
#' @export
#' 
getBBox<-function(){
  bbx = c(xmin=168,ymin=51.67,xmax=203,ymax=68.49);
  bbx %<>% wtsGIS::transformBBox(sf::st_crs(4326));
  return(bbx);
}