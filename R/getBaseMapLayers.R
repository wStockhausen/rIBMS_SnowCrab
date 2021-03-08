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
#' @examples A default lat/lon basemap in color can be constructed using the following:\cr
#' require(ggplot2);
#' lst = getBaseMapLayers("2020",FALSE,FALSE);
#' ggplot()+lst$bathym+lst$land+lst$zones+lst$labels+lst$map_scale+lst$theme;
#' 
#' @import ggplot2
#' @importFrom grid unit
#' @importFrom wtsGIS get_crs
#' @importFrom wtsGIS transformBBox
#' @importFrom wtsUtilities getObj
#' 
#' @export
#' 
getBasemapLayers<-function(czType=c("2020","2019"),
                           xy=FALSE,
                           bw=FALSE,
                           noBathym=FALSE){
  czType = czType[1];#--connectivity zone type
  if (xy){xyType ="XY";} else {xyType ="LL";}
  if (bw){clrType="BW";} else {clrType="RGB";}
  
  #--connectivity zones
  fn = paste0("extdata/ConnectivityZones",czType,".",xyType,".RData");
  sf_czs = wtsUtilities::getObj(system.file(fn,package="rIBMsSnowCrab"));
  #--connectivty zone labels
  fn = paste0("extdata/CZLabels",czType,".",xyType,".RData");
  sf_lbs = wtsUtilities::getObj(system.file(fn,package="rIBMsSnowCrab"));

  #--get land
  fn = paste0("extdata/Land.",xyType,".RData");
  sf_land = wtsUtilities::getObj(system.file(fn,package="rIBMsSnowCrab"));
  
  if (bw){
    lyr_land  = ggplot2::geom_sf(data=sf_land,fill="black",inherit.aes=FALSE);
    lyr_zones = ggplot2::geom_sf(data=sf_czs,colour="black",fill=NA,inherit.aes=FALSE);
    lyr_labs  = ggplot2::geom_sf_text(data=sf_lbs,mapping=ggplot2::aes(label=zone),colour="black",inherit.aes=FALSE);
  } else {
    lyr_land  = ggplot2::geom_sf(data=sf_land,fill="green",inherit.aes=FALSE);  
    lyr_zones = ggplot2::geom_sf(data=sf_czs,colour="blue",fill=NA,inherit.aes=FALSE);
    lyr_labs  = ggplot2::geom_sf_text(data=sf_lbs,mapping=ggplot2::aes(label=zone),colour="blue",inherit.aes=FALSE);
  }

  #--get bathymetry layer
  lyr_bathym=NULL;
  if (!noBathym){
    fn = paste0("extdata/BathymAnnotLayer",clrType,".",xyType,".RData");
    lyr_bathym = wtsUtilities::getObj(system.file(fn,package="rIBMsSnowCrab"));
  }
  
  #--define coordinate scale for default basemap
  map_scale = NULL;
  bbx = rIBMsSnowCrab::getBBox(ll=!xy);#--bounding box in lat/lon (WGS84) or xy (Alaska ALbers)
#  if (xy) bbx = wtsGIS::transformBBox(bbx,wtsGIS::get_crs(3338));
  map_scale = ggplot2::coord_sf(xlim=c(bbx["xmin"],bbx["xmax"]),
                                ylim=c(bbx["ymin"],bbx["ymax"]),
                                crs=wtsGIS::get_crs(bbx),
                                expand=FALSE,
                                clip="on",
                                default=TRUE);
  
  #--define theme 
  #----define aspect ratio for panels
  asp=NULL; #--let ggplot2 work it out
  if (xy) asp = (bbx["ymax"]-bbx["ymin"])/(bbx["xmax"]-bbx["xmin"]);
  #----remove axis titles (necessary when connectivity zone labels are included in map)
  theme = ggplot2::theme(axis.title.x = element_blank(),
                         axis.title.y = element_blank(),
                         panel.spacing=grid::unit(0.05,"cm"),
                         aspect.ratio=asp);
  return(list(bathym=lyr_bathym,land=lyr_land,zones=lyr_zones,labels=lyr_labs,
              map_scale=map_scale,theme=theme));
}

