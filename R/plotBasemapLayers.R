#'
#' @title Get \pkg{ggplot2} layers for a basemap
#' 
#' @description Function to get a set of \pkg{ggplot2} layers for a basemap.
#' 
#' @param czType - type of connectivity zone to get ("2020"or "2019")
#' @param xy - flag (T/F) for coordinate type (TRUE=Alaska Albers projection; FALSE=WGS84 lat/lon)
#' @param bw - flag (T/F) for black & white or color scheme
#' @param noBathym - flag (T/F) to not include bathymetry layer (default=FALSE)
#' 
#' @return list with ggplot2 layers 'bathym', 'land', 'zones', 'labels', 'map_scale', and 'theme'.
#' 
#' @details If \code{xy} is TRUE, 'bathym' and 'map_scale' in the returned list will be NULL (currently).
#' Note that including 'theme' in the basemap simply removes the axis labels, which would otherwise be 'x' and 'y'.
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
#' @importFrom wtsGIS get_crs
#' @importFrom wtsGIS transformBBox
#' @importFrom wtsUtilities getObj
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


