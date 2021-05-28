#'
#' @title Plot \pkg{ggplot2} layers of a basemap
#' 
#' @description Function to plot a set of \pkg{ggplot2} layers for a basemap from call to \code{\link{getBasemapLayers}}.
#' 
#' @param bmls - basemap layers from call to \code{\link{getBasemapLayers}}
#' 
#' @return list with ggplot2 plot object.
#' 
#' @details None.
#' 
#' @examples 
#' #A default lat/lon basemap in color with bathymetry can be constructed and printed using the following:\cr
#' bmls = getBaseMapLayers(czType="2019",xy=FALSE,bw=FALSE,noBathym=FALSE);
#' print(plotBasemapLayers(bmls));
#' 
#' #A default Alaska Albers basemap in color can be constructed and printed using the following:\cr
#' #--In this case, the bathymetry (an annotation layer in lat/lon) will not be included.
#' bmls = getBaseMapLayers(czType="2019",xy=TRUE,bw=FALSE,noBathym=FALSE);
#' print(plotBasemapLayers(bmls));
#' 
#' @import ggplot2
#' 
#' @export
#' 
plotBasemapLayers<-function(bmls){
    p=ggplot2::ggplot()+
        bmls$bathym+
        bmls$land+
        bmls$zones+
        bmls$labels+
        bmls$map_scale+
        bmls$theme;
    return(p);
}


