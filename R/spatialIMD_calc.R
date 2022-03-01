#' 
#' @title Calculate the spatial distribution of IMD for a life stage
#' 
#' @description Calculate the spatial distribution of IMD for a life stage based on
#' temperature from ROMS model output
#' 
#' @param fn - path to ROMS model output file
#' @param sf_temp - sigma-level temperature layer from ROMS model output as \pkg{sf} dataframe 
#' @param sf_crop - \pkg{sf} dataframe with polygon to crop by using [sf::st_intersection()]
#' 
#' @return \pkg{sf} dataframe with spatial distribution of IMD
#' 
#' @import dplyr
#' @import magrittr
#' @import sf
#' 
#' @export
#' 
spatialIMD_calc<-function(stage,
                          source,
                          sf_temp,
                          sf_crop=NULL
                          ){
  if (!is.null(sf_crop)) {
    #--transform to crs of crop object and crop
    crs = wtsGIS::get_crs(sf_crop);
    sf_temp %<>% sf::st_transform(crs) %>% sf::st_intersection(sf_crop);
  }
  dfr_imd = calcIMDbyStage_FixedT(sf_temp$temp,stages=stage,source=source);
  dfr_imd<-sf::st_sf(dplyr::bind_cols(dfr_imd,sf_temp));
  return(dfr_imd);
}

