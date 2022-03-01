#' 
#' @title Map the spatial distribution of IMD for a life stage
#' 
#' @description Map the spatial distribution of IMD for a life stage based on
#' temperature from ROMS model output
#' 
#' @param sf_imd - \pkg{sf} dataframe with IMD info (i.e., from [spatialIMD_calc])
#' @param subtitle - subtitle for map
#' @param scale_type_imd - fill scale type ("d","c","b") for IMD (default is "b"--binned)
#' @param breaks_imd - vector of values to use as breaks in scale for IMDs
#' @param limits_imd - limits to use when plotting IMD values
#' @param scale_type_temp - fill scale type ("d","c","b") for temperatures (default is "c"--continuous)
#' @param breaks_temp - vector of values to use as breaks in scale for temperatures
#' @param limits_temp - limits to use when plotting temperature values
#' @param bbx - \pkg{sf}-style bounding box
#' @param doHist - include histogram of IMD values as inset on IMD map
#' @param doTemp - add map of temperatures
#' @param horiz - flag (TRUE/FALSE) to arrange temperature, IMD maps horizontally
#' 
#' @return ggplot2 map object or gg list object
#' 
#' @details If an input limit (limit_imd, limit_temp) is NULL, the resulting scale limit 
#' is: 1) based on the range of the data if the corresponding breaks is set to ggplot2::waiver() or 
#' 2) equal to c(min(breaks),max(breaks)) if breaks is a numeric vector.
#' 
#' @importFrom scales squish
#' 
#' @import cowplot
#' @import ggplot2
#' @import grid
#' @import magrittr
#' @import sf
#' @import wtsGIS 
#' 
#' @export
#' 
spatialIMD_map<-function(sf_imd,
                         subtitle="",
                         scale_type_imd="b",
                         breaks_imd=ggplot2::waiver(),
                         limits_imd=NULL,
                         scale_type_temp="c",
                         breaks_temp=ggplot2::waiver(),
                         limits_temp=NULL,
                         bbx=wtsGIS::getStandardBBox("EBS"),
                         land=wtsGIS::getPackagedLayer("Alaska"),
                         bath=wtsGIS::getPackagedLayer("ShelfBathymetry"),
                         doHist=TRUE,
                         doTemp=TRUE,
                         horiz=TRUE){
  #--get crs of bbx and determine if plot should be xy or lat/lon
  crs = wtsGIS::get_crs(bbx);
  LL  = sf::st_is_longlat(crs);
  bbx %<>% wtsGIS::shift_longitude();
  
  #--transform and crop land and bathymetry
  # land %<>% sf::st_transform(crs) %>%
  #           shift_longitude() %>%
  #           sf::st_crop(bbx);
  # bath %<>% sf::st_transform(crs) %>%
  #           shift_longitude() %>%
  #           sf::st_crop(bbx);
  
  #--transform sf_imd to crs
  sf_imd %<>% sf::st_transform(crs);
  
  #--create function to specify scale
  scale<-function(type="c",
                  direction=1,
                  breaks=ggplot2::waiver(),
                  limits=NULL){
    if (type=="c"){
      scl = ggplot2::scale_fill_viridis_c(option="plasma",direction=direction,breaks=breaks,
                                          oob=scales::squish,limits=limits);
    } else if (type=="b"){
      scl = ggplot2::scale_fill_viridis_b(option="plasma",direction=direction,breaks=breaks,
                                          oob=scales::squish,limits=limits);
    }
    return(scl)
  }
  
  #--create ggplot2 map of temperature
  if (doTemp){
    limits = NULL;
    if (!inherits(breaks_temp,"waiver")) {limits = c(min(breaks_temp),max(breaks_temp));}
    if (!is.null(limits_temp))           {limits = limits_temp;}
    pt = ggplot2::ggplot(sf_imd,mapping=ggplot2::aes(fill=temp)) +
           ggplot2::geom_sf(colour=NA) + 
           scale(scale_type_temp,direction=1,breaks_temp,limits) +
           ggplot2::geom_sf(data=land,fill="light green",colour=NA) + 
           ggplot2::geom_sf(data=bath,fill=NA,colour="black") +
           ggplot2::labs(fill="temperature\n(deg. C)",label=NA,subtitle=subtitle) +
           ggplot2::coord_sf(xlim=c(bbx["xmin"],bbx["xmax"]),ylim=c(bbx["ymin"],bbx["ymax"]),
                            crs=crs,datum=crs,label_axes="----",expand=FALSE,clip="on");
  }
  
  #--create ggplot2 map of IMD
  limits = NULL;
  if (!inherits(breaks_imd,"waiver")) {limits = c(min(breaks_imd),max(breaks_imd));}
  if (!is.null(limits_imd))           {limits = limits_imd;}
  pd = ggplot2::ggplot(sf_imd,mapping=ggplot2::aes(fill=D)) +
         ggplot2::geom_sf(colour=NA) + 
         scale(scale_type_imd,direction=-1,breaks_imd,limits) +
         ggplot2::geom_sf(data=land,fill="light green",colour=NA) + 
         ggplot2::geom_sf(data=bath,fill=NA,colour="black") +
         ggplot2::labs(fill="intermolt\nduration (days)",label=NA) +
         ggplot2::coord_sf(xlim=c(bbx["xmin"],bbx["xmax"]),ylim=c(bbx["ymin"],bbx["ymax"]),
                          crs=crs,datum=crs,label_axes="----",expand=FALSE,clip="on") +
         ggplot2::theme(legend.key.height=grid::unit(2,"lines"));
  if (doHist){
    ph = ggplot(sf_imd,aes(D))+geom_histogram(breaks=seq(0,350,5))+labs(x="IMD (days)");
    pg = ggplotGrob(ph);
    pd = pd + annotation_custom(pg,xmin=bbx["xmin"],
                                    ymin=bbx["ymin"],
                                    xmax=bbx["xmin"]+0.40*(bbx["xmax"]-bbx["xmin"]),
                                    ymax=bbx["ymin"]+0.25*(bbx["ymax"]-bbx["ymin"]));
  }
  
  pc = pd;
  if (doTemp){
    ncol = 1; nrow = 2;
    if (horiz) {ncol = 2; nrow = 1;}
    pc = cowplot::plot_grid(pt,pd,ncol=ncol,nrow=nrow);
  }  
  return(pc);
}

