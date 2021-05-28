#--create R data objects for package
require(ggplot2);
require(magrittr);
#--also requires packages
# raster
# rDisMELS
# RStoolbox
# sf
# stars
# wtsGIS
# wtsUtilities

#--define coordinate reference systems
crs_4326 = sf::st_crs(4326);#--WGS84
crs_3338 = sf::st_crs(3338);#--NAD83 Alaska Albers


#--create connectivity zones for 30-layer ROMS runs
#----read in shapefile for connectivity zones for the 30-layer ROMS datasets
#------shapefile is in NAD83 Alaska Albers 
fn_cz20  = "./inst/gisdata/ConnectivityZones2020/ConnectivityZones.SnowCrab.2020.shp";
sf_cz20_xy  = wtsGIS::readFeatureLayer(fn_cz20);
names(sf_cz20_xy)[1] = "zone";
if (sf::st_crs(sf_cz20_xy)!=crs_3338) stop("Error! CRS for sf_cz20_xy is not Alaska Albers!");
bbx_cz20_xy = wtsGIS::getBBox(sf_cz20_xy);
#----convert to WGS84 with longitudes 0-360
sf_cz20_ll  = sf_cz20_xy %>% sf::st_transform(sf::st_crs(4326)) %>% sf::st_shift_longitude();
bbx_cz20_ll = wtsGIS::getBBox(sf_cz20_ll);
ggplot(data=sf_cz20_ll,mapping=aes(fill=zone), colour="black")+geom_sf();
ggplot(data=sf_cz20_xy,mapping=aes(fill=zone), colour="black")+geom_sf();
#--save objects
wtsUtilities::saveObj(sf_cz20_xy,"./inst/extdata/ConnectivityZones2020.XY.RData");
wtsUtilities::saveObj(sf_cz20_ll,"./inst/extdata/ConnectivityZones2020.LL.RData");


#--create connectivity zones for the 10-layer ROMS hindcast study
#--read in Mike Torre's connectivity object ("ConGrid1", a sp::SpatialPolygonsDataFrame)
fn_cz19 = "./inst/gisdata/ConGridFinal.RData";
load(fn_cz19,envir=environment());
#----convert to sf dataframe, shifting longitudes to 0-360 and coalescing polygons cut at IDL
sf_cz19_ll = sf::st_as_sf(ConGrid1) %>% sf::st_shift_longitude() %>% sf::st_union(by_feature=TRUE);
rm(ConGrid1);
sf_cz19_ll %<>% select(OBJECTID,geometry);
names(sf_cz19_ll)[1] = "zone";
sf_cz19_ll$zone = as.integer(as.character(sf_cz19_ll$zone));
sf::st_crs(sf_cz19_ll) = crs_4326;
bbx_cz19_ll = sf::st_bbox(sf_cz19_ll);#--bbox in lat/lon
# #----reorder levels in numerical order
# lvls = as.character(sort(as.numeric(levels(sf_cz19_ll$OBJECTID))));
# sf_cz19_ll$OBJECTID = factor(as.character(sf_cz19_ll$OBJECTID),levels=lvls);
# rm(lvls);
#----transform to Alaska Albers projection
sf_cz19_xy  = sf_cz19_ll %>% sf::st_transform(crs_3338);
bbx_cz19_xy = sf::st_bbox(sf_cz19_xy);#--bbox in projected crs
ggplot(data=sf_cz19_ll,mapping=aes(fill=zone), colour="black")+geom_sf();
ggplot(data=sf_cz19_xy,mapping=aes(fill=zone), colour="black")+geom_sf();
#----save objects
wtsUtilities::saveObj(sf_cz19_ll,"./inst/extdata/ConnectivityZones2019.LL.RData");
wtsUtilities::saveObj(sf_cz19_xy,"./inst/extdata/ConnectivityZones2019.XY.RData");

#----compare connectivity zones
ggplot()+
  geom_sf(data=sf_cz19_ll,colour="red", fill="pink")+
  geom_sf(data=sf_cz20_ll,colour="blue",fill="light blue",alpha=0.5);


#--create sf dataframes with labels for connectivity zones
dfr_lbls19_ll = data.frame(zone=sf_cz19_ll$zone,
                           lon = rep(NA, nrow(sf_cz19_ll)),
                           lat = rep(NA, nrow(sf_cz19_ll)));
for (r in 1:nrow(dfr_lbls19_ll)){
  ctr = sf::st_coordinates(sf::st_centroid(sf_cz19_ll$geometry[r],of_largest_polygon=TRUE));
  dfr_lbls19_ll$lon[r] = ctr[1,1];
  dfr_lbls19_ll$lat[r] = ctr[1,2];
}
dfr_lbls20_ll = data.frame(zone=sf_cz20_ll$zone,
                           lon = rep(NA, nrow(sf_cz20_ll)),
                           lat = rep(NA, nrow(sf_cz20_ll)));
for (r in 1:nrow(dfr_lbls20_ll)){
  ctr = sf::st_coordinates(sf::st_centroid(sf_cz20_ll$geometry[r],of_largest_polygon=TRUE));
  dfr_lbls20_ll$lon[r] = ctr[1,1];
  dfr_lbls20_ll$lat[r] = ctr[1,2];
}
#------adjust locations for some labels (note row number != zone for dfr_lbls20)
dfr_lbls20_ll$lon[ 4] = dfr_lbls19_ll$lon[ 4] = 188.5;
dfr_lbls20_ll$lon[ 7] = dfr_lbls19_ll$lon[ 7] = 189.0;
dfr_lbls20_ll$lat[18] = dfr_lbls19_ll$lat[18] = 62.0;

#----create sf dataframe
sf_lbls19_ll = wtsGIS::createSF_points(dfr_lbls19_ll,xCol="lon",yCol="lat");
sf::st_crs(sf_lbls19_ll) = crs_4326;#--reassign WGS84 crs
sf_lbls19_xy = sf_lbls19_ll %>% sf::st_transform(crs_3338);
sf_lbls20_ll = wtsGIS::createSF_points(dfr_lbls20_ll,xCol="lon",yCol="lat");
sf::st_crs(sf_lbls20_ll) = crs_4326;#--reassign WGS84 crs
sf_lbls20_xy = sf_lbls20_ll %>% sf::st_transform(crs_3338);

#----check plot
ggplot()+
  geom_sf(data=sf_cz19_ll,colour="red", fill="pink")+
  geom_sf_text(data=sf_lbls19_ll,mapping=ggplot2::aes(label=zone),colour="red") +
  geom_sf(data=sf_cz20_ll,colour="blue",fill="light blue",alpha=0.5) +
  geom_sf_text(data=sf_lbls20_ll,mapping=ggplot2::aes(label=zone),colour="blue")+
  theme(axis.title.x = element_blank(),axis.title.y = element_blank());
ggplot()+
  geom_sf(data=sf_cz19_xy,colour="red", fill="pink")+
  geom_sf_text(data=sf_lbls19_xy,mapping=ggplot2::aes(label=zone),colour="red") +
  geom_sf(data=sf_cz20_xy,colour="blue",fill="light blue",alpha=0.5) +
  geom_sf_text(data=sf_lbls20_xy,mapping=ggplot2::aes(label=zone),colour="blue")+
  theme(axis.title.x = element_blank(),axis.title.y = element_blank());
  
#----save objects
wtsUtilities::saveObj(sf_lbls19_ll,"./inst/extdata/CZLabels2019.LL.RData")
wtsUtilities::saveObj(sf_lbls19_xy,"./inst/extdata/CZLabels2019.XY.RData")
wtsUtilities::saveObj(sf_lbls20_ll,"./inst/extdata/CZLabels2020.LL.RData")
wtsUtilities::saveObj(sf_lbls20_xy,"./inst/extdata/CZLabels2020.XY.RData")


#--create vector mapping between connectivity zones
#----get sf dataframes with labels
sf_lbls19_ll=wtsUtilities::getObj("./inst/extdata/CZLabels2019.LL.RData")
sf_lbls19_xy=wtsUtilities::getObj("./inst/extdata/CZLabels2019.XY.RData")
sf_lbls20_ll=wtsUtilities::getObj("./inst/extdata/CZLabels2020.LL.RData")
sf_lbls20_xy=wtsUtilities::getObj("./inst/extdata/CZLabels2020.XY.RData")

createConnections<-function(sf_dfr){
  tmp1 = sf_dfr  %>% sf::st_drop_geometry();
  tmp2 = tmp1 %>% dplyr::inner_join(tmp1,by=character(),suffix=c("_start","_end"));
  nr   = nrow(tmp2);
  geoms = vector(length=nr,mode="list");
  for (rw in 1:nr){
    tbl = tibble::tibble(x=c(tmp2$lon_start[rw],tmp2$lon_end[rw]),
                         y=c(tmp2$lat_start[rw],tmp2$lat_end[rw]));
    geoms[[rw]] = sfheaders::sfg_linestring(tbl,x="x",y="y");
  }
  sfc_geoms = sf::st_sfc(geoms,crs=sf::st_crs(sf_dfr));
  sf_dfr1 = sf::st_sf(dplyr::bind_cols(tmp2,sf::st_sf(geometry=sfc_geoms,crs=sf::st_crs(sf_dfr))));
  names(sf_dfr1) = c("startZone","startLon","startLat","endZone","endLon","endLat","geometry");
  return(sf_dfr1);
}
sf_arrs19_ll = rDisMELS::createConnectivityWeb(sf_lbls19_ll,xCol="lon",yCol="lat");
sf_arrs19_xy = sf_arrs19_ll %>% sf::st_transform(3338);
sf_arrs20_ll = rDisMELS::createConnectivityWeb(sf_lbls20_ll,xCol="lon",yCol="lat");
sf_arrs20_xy = sf_arrs20_ll %>% sf::st_transform(3338);
#--check plot
tmp = sf_arrs19_ll;
ggplot2::ggplot(tmp,ggplot2::aes(colour=factor(startZone)))+
  ggplot2::geom_sf()+ggplot2::scale_colour_discrete();
ggplot2::ggplot(tmp,ggplot2::aes(colour=factor(startZone)))+
  ggplot2::geom_segment(mapping=ggplot2::aes(x=startlon,y=startlat,xend=endlon,yend=endlat),arrow=ggplot2::arrow());
#----save objects
wtsUtilities::saveObj(sf_arrs19_ll,"./inst/extdata/CZArrows2019.LL.RData")
wtsUtilities::saveObj(sf_arrs19_xy,"./inst/extdata/CZArrows2019.XY.RData")
wtsUtilities::saveObj(sf_arrs20_ll,"./inst/extdata/CZArrows2020.LL.RData")
wtsUtilities::saveObj(sf_arrs20_xy,"./inst/extdata/CZArrows2020.XY.RData")
                                

#--convert map object "m" to sf dataframe
fn_land = "./inst/gisdata/AlaskaLand.RData";
load(fn_land,envir=environment());#--restore "m", a map object
#----convert map object to sp object and assign coordinate reference system
m2 <- maptools::map2SpatialPolygons(m, IDs=m$names, proj4string=sp::CRS("+proj=longlat +datum=WGS84"));
sf_land_ll = sf::st_as_sf(m2);#--convert sp object to sf dataframe
#----check for invalid polygons and fix as necessary
idxs = which(!sf::st_is_valid(sf_land_ll));
if (length(idxs)>0) for (idx in idxs) {sf_land_ll$geometry[idx] = sf::st_make_valid(sf_land_ll$geometry[idx]);}
sf_land_ll %<>% sf::st_crop(bbx_cz20_ll);               #--crop to bounding box for connectivity zones
sf_land_xy  = sf_land_ll %>% sf::st_transform(crs_3338);#--transform to Alaska Albers
#-----plot maps
ggplot2::ggplot() +
  ggplot2::geom_sf(data=sf_land_ll,fill="grey");
ggplot2::ggplot() +
  ggplot2::geom_sf(data=sf_land_xy,fill="grey");
#----save objects
wtsUtilities::saveObj(sf_land_ll,"./inst/extdata/Land.LL.RData");
wtsUtilities::saveObj(sf_land_xy,"./inst/extdata/Land.XY.RData");

#--- Convert North Pacific elevation raster ("bathym", a sp::RasterLayer object)
fn_bathym = "./inst/gisdata/NorthPacificBathymetryRaster.RData";
load(fn_bathym,envir=environment());#--already cropped to EBS c(168,210,48,70)
bathym[bathym>=0]<-NA;              #--set land areas to NA
# convert to RasterBrick with rgb channels
myRmp <- grDevices::colorRamp(c("blue3", "dodgerblue1" , "cyan","seagreen1","lightgoldenrod1","tan1"), bias=0.015);
bathym_min  = bathym@data@min;
bathym_max  = bathym@data@max;
bathym_vals = bathym@data@values;
mat_rgb = myRmp((bathym_vals-bathym_min)/(bathym_max-bathym_min));
bathym_r = bathym; bathym_r@data@values = mat_rgb[,1]; bathym_r@data@min = min(mat_rgb[,1],na.rm=TRUE); bathym_r@data@max=max(mat_rgb[,1],na.rm=TRUE);
bathym_g = bathym; bathym_g@data@values = mat_rgb[,2]; bathym_g@data@min = min(mat_rgb[,2],na.rm=TRUE); bathym_g@data@max=max(mat_rgb[,2],na.rm=TRUE);
bathym_b = bathym; bathym_b@data@values = mat_rgb[,3]; bathym_b@data@min = min(mat_rgb[,3],na.rm=TRUE); bathym_b@data@max=max(mat_rgb[,3],na.rm=TRUE);
bathym_rb = raster::brick(bathym_r,bathym_g,bathym_b);
#--bin scale
cutpts = c(seq(-8000,-1000,1000),-500,-250,-200,-150,-100,-50,-25, 0); nc = length(cutpts);
rcl_dfr = data.frame(from=cutpts[1:(nc-1)],to=cutpts[2:nc],value=cutpts[1:(nc-1)]);
rcl_mat = as.matrix(rcl_dfr);
bathym_c = raster::reclassify(bathym,rcl_mat,include.lowest=TRUE);
#--transform to stars objects and Alaska Albers
stars_c_ll = stars::st_as_stars(bathym_c);
stars_c_xy = stars::st_warp(stars_c_ll,crs=crs_3338,cellsize=1000,method="near");
stars_rb_ll = stars::st_as_stars(bathym_rb);
stars_rb_xy = stars::st_warp(stars_rb_ll,crs=crs_3338,cellsize=1000,method="near");
#--create contours at cutpts
cntr_ll = stars::st_contour(stars_c_ll,contour_lines=TRUE,breaks=cutpts);
cntr_xy = stars::st_contour(stars_c_xy,contour_lines=TRUE,breaks=cutpts);
#--switch .._xy stars objects back to raster objects
rstr_c_xy  = stars:::st_as_raster(stars_c_xy); #--RasterLayer
rstr_rb_xy = stars:::st_as_raster(stars_rb_xy);#--RasterBrick
#--create ggplot2 annotation_raster layers
bathym_ggbw_ll  = RStoolbox::ggR(bathym_c,   ggLayer=TRUE,stretch="hist");
bathym_ggrgb_ll = RStoolbox::ggRGB(bathym_rb,ggLayer=TRUE,r=1,g=2,b=3);
bathym_ggbw_xy  = RStoolbox::ggR(rstr_c_xy,  ggLayer=TRUE,stretch="hist");
bathym_ggrgb_xy = RStoolbox::ggRGB(rstr_rb_xy,ggLayer=TRUE,r=1,g=2,b=3);
#--create ggplot2 legends for bathymetry layers
lgnd_bw  = (ggplot() + 
              RStoolbox::ggR(bathym_c, ggLayer=TRUE,geom_raster=TRUE,stretch="hist",forceCat=TRUE) +
              ggplot2::scale_fill_grey(start=0.01,end=0.95,na.value=NA)) %>%
             cowplot::get_legend();
myClrs = myRmp((cutpts-min(cutpts))/(max(cutpts)-min(cutpts)))
myPal  = grDevices::colorRampPalette(rgb(myClrs[,1],myClrs[,2],myClrs[,3],maxColorValue=255));
lgnd_rgb = (ggplot() + 
              RStoolbox::ggR(bathym_c, ggLayer=TRUE,geom_raster=TRUE,forceCat=TRUE) +
              ggplot2::scale_fill_discrete(type=myPal(nc),name="depth (m)")) %>%
             cowplot::get_legend();
rm(bathym,bathym_min,bathym_max,bathym_vals,mat_rgb,bathym_r,bathym_g,bathym_b,bathym_rb);
rm(stars_c_ll,stars_c_xy,stars_rb_ll,stars_rb_xy);

#plot bathymetry rasters
ggplot()+
  bathym_ggrgb_ll + 
  geom_sf(data=sf_land_ll,fill="green",colour=NA) +
  geom_sf(data=sf_cz20_ll,colour="blue",fill=NA,alpha=0.5) +
  coord_sf(xlim=c(168,203),y=c(51.67,68.49),expand=FALSE,clip="on");
ggplot()+
  bathym_ggbw_ll + 
  geom_sf(data=sf_land_ll,fill="black",colour=NA) +
  geom_sf(data=sf_cz20_ll,colour="black",fill=NA,alpha=0.5) + 
  coord_sf(xlim=c(168,203),y=c(51.67,68.49),expand=FALSE,clip="on");
ggplot()+
  bathym_ggrgb_xy +
  geom_sf(data=sf_land_xy,fill="green",colour=NA) + 
  geom_sf(data=sf_cz20_xy,colour="blue",fill=NA,alpha=0.5) + 
  coord_sf(xlim=c(-2641743.2,-123570),y=c(415108.7,2640678.2),crs=crs_3338,expand=FALSE,clip="on");
ggplot()+
  bathym_ggbw_xy + 
  geom_sf(data=sf_land_xy,fill="black",colour=NA) +
  geom_sf(data=sf_cz20_xy,colour="black",fill=NA,alpha=0.5); + 
  coord_sf(xlim=c(-2641743.2,-123570),y=c(415108.7,2640678.2),crs=crs_3338,expand=FALSE,clip="on");
#--save objects
wtsUtilities::saveObj(myRmp,          "./inst/extdata/BathymColourRamp.RData");
wtsUtilities::saveObj(myPal,          "./inst/extdata/BathymColourPalette.RData");
wtsUtilities::saveObj(lgnd_bw,        "./inst/extdata/BathymLegend.BW.RData");
wtsUtilities::saveObj(lgnd_rgb,       "./inst/extdata/BathymLegend.RGB.RData");
wtsUtilities::saveObj(cntr_ll,        "./inst/extdata/BathymContours.LL.RData");
wtsUtilities::saveObj(cntr_xy,        "./inst/extdata/BathymContours.XY.RData");
wtsUtilities::saveObj(bathym_ggbw_ll, "./inst/extdata/BathymAnnotLayerBW.LL.RData");
wtsUtilities::saveObj(bathym_ggrgb_ll,"./inst/extdata/BathymAnnotLayerRGB.LL.RData");
wtsUtilities::saveObj(bathym_ggbw_xy, "./inst/extdata/BathymAnnotLayerBW.XY.RData");
wtsUtilities::saveObj(bathym_ggrgb_xy,"./inst/extdata/BathymAnnotLayerRGB.XY.RData");
