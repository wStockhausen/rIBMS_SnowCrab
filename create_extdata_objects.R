#--create R data objects for package
require(ggplot2);
require(magrittr);

#--define coordinate reference systems
crs_4326 = sf::st_crs(4326);#--WGS84
crs_3338 = sf::st_crs(3338);#--NAD83 Alaska Albers

#--create connectivity zones for 30-layer ROMS runs
#----read in shapefile for connectivity zones for the 30-layer ROMS datasets
#------shapefile is in NAD83 Alaska Albers 
fn_cz20  = "./inst/gisdata/ConnectivityZones2020/ConnectivityZones.SnowCrab.2020.shp";
sf_cz20_xy  = wtsGIS::readFeatureLayer(fn_cz20);
if (sf::st_crs(sf_cz20_xy)!=crs_3338) stop("Error! CRS for sf_cz20_xy is not Alaska Albers!");
bbx_cz20_xy = wtsGIS::getBBox(sf_cz20_xy);
#----convert to WGS84 with longitudes 0-360
sf_cz20_ll  = sf_cz20_xy %>% sf::st_transform(sf::st_crs(4326)) %>% sf::st_shift_longitude();
bbx_cz20_ll = wtsGIS::getBBox(sf_cz20_ll);
ggplot()+
  geom_sf(data=sf_cz20_xy,colour="blue",fill="light blue");
ggplot()+
  geom_sf(data=sf_cz20_ll,colour="blue",fill="light blue");
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
sf::st_crs(sf_cz19_ll) = crs_4326;
bbx_cz19_ll = sf::st_bbox(sf_cz19_ll);#--bbox in lat/lon
#----reorder levels in numerical order
lvls = as.character(sort(as.numeric(levels(sf_cz19_ll$OBJECTID))));
sf_cz19_ll$OBJECTID = factor(as.character(sf_cz19_ll$OBJECTID),levels=lvls);
rm(lvls);
#----transform to Alaska Albers projection
sf_cz19_xy  = sf_cz19_ll %>% sf::st_transform(crs_3338);
bbx_cz19_xy = sf::st_bbox(sf_cz19_xy);#--bbox in projected crs
ggplot()+
  geom_sf(data=sf_cz19_ll,colour="red", fill="pink");
ggplot()+
  geom_sf(data=sf_cz19_xy,colour="red", fill="pink");
#----save objects
wtsUtilities::saveObj(sf_cz19_ll,"./inst/extdata/ConnectivityZones2019.LL.RData");
wtsUtilities::saveObj(sf_cz19_xy,"./inst/extdata/ConnectivityZones2019.XY.RData");

#----compare connectivity zones
ggplot()+
  geom_sf(data=sf_cz19_ll,colour="red", fill="pink")+
  geom_sf(data=sf_cz20_ll,colour="blue",fill="light blue",alpha=0.5);

#--create sf dataframes with labels for connectivity zones
dfr_acs19_ll = data.frame(zone=sf_cz19_ll$OBJECTID,
                         lon = rep(NA, length(sf_cz19_ll$OBJECTID)),
                         lat = rep(NA, length(sf_cz19_ll$OBJECTID)));
for (r in 1:nrow(dfr_acs19_ll)){
  ctr = sf::st_coordinates(sf::st_centroid(sf_cz19_ll$geometry[r],of_largest_polygon=TRUE));
  dfr_acs19_ll$lon[r] = ctr[1,1];
  dfr_acs19_ll$lat[r] = ctr[1,2];
}
dfr_acs20_ll = data.frame(zone=sf_cz20_ll$Id,
                         lon = rep(NA, length(sf_cz20_ll$Id)),
                         lat = rep(NA, length(sf_cz20_ll$Id)));
for (r in 1:nrow(dfr_acs20_ll)){
  ctr = sf::st_coordinates(sf::st_centroid(sf_cz20_ll$geometry[r],of_largest_polygon=TRUE));
  dfr_acs20_ll$lon[r] = ctr[1,1];
  dfr_acs20_ll$lat[r] = ctr[1,2];
}
#------adjust locations for some labels (note row != Id for acs20)
dfr_acs20_ll$lon[ 4] = dfr_acs19_ll$lon[ 4] = 188.5;
dfr_acs20_ll$lon[ 7] = dfr_acs19_ll$lon[ 7] = 189.0;
dfr_acs20_ll$lat[18] = dfr_acs19_ll$lat[18] = 62.0;

#----create sf dataframe
sf_acs19_ll = wtsGIS::createSF_points(dfr_acs19_ll,xCol="lon",yCol="lat");
sf::st_crs(sf_acs19_ll) = crs_4326;#--reassign WGS84 crs
sf_acs19_xy = sf_acs19_ll %>% sf::st_transform(crs_3338);
sf_acs20_ll = wtsGIS::createSF_points(dfr_acs20_ll,xCol="lon",yCol="lat");
sf::st_crs(sf_acs20_ll) = crs_4326;#--reassign WGS84 crs
sf_acs20_xy = sf_acs20_ll %>% sf::st_transform(crs_3338);

#----check plot
ggplot()+
  geom_sf(data=sf_cz19_ll,colour="red", fill="pink")+
  geom_sf_text(data=sf_acs19_ll,mapping=ggplot2::aes(label=zone),colour="red") +
  geom_sf(data=sf_cz20_ll,colour="blue",fill="light blue",alpha=0.5) +
  geom_sf_text(data=sf_acs20_ll,mapping=ggplot2::aes(label=zone),colour="blue")+
  theme(axis.title.x = element_blank(),axis.title.y = element_blank());
ggplot()+
  geom_sf(data=sf_cz19_xy,colour="red", fill="pink")+
  geom_sf_text(data=sf_acs19_xy,mapping=ggplot2::aes(label=zone),colour="red") +
  geom_sf(data=sf_cz20_xy,colour="blue",fill="light blue",alpha=0.5) +
  geom_sf_text(data=sf_acs20_xy,mapping=ggplot2::aes(label=zone),colour="blue")+
  theme(axis.title.x = element_blank(),axis.title.y = element_blank());
  
#----save objects
wtsUtilities::saveObj(sf_acs19_ll,"./inst/extdata/CZLabels2019.LL.RData")
wtsUtilities::saveObj(sf_acs19_xy,"./inst/extdata/CZLabels2019.XY.RData")
wtsUtilities::saveObj(sf_acs20_ll,"./inst/extdata/CZLabels2020.LL.RData")
wtsUtilities::saveObj(sf_acs20_xy,"./inst/extdata/CZLabels2020.XY.RData")

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
cutpts = c(seq(-8000,-1000,1000),-500,-250,-200,-150,-100,-50,0); nc = length(cutpts);
rcl_dfr = data.frame(from=cutpts[1:(nc-1)],to=cutpts[2:nc],value=cutpts[1:(nc-1)]);
rcl_mat = as.matrix(rcl_dfr);
bathym_c = raster::reclassify(bathym,rcl_mat,include.lowest=TRUE);
#--create ggplot2 annotation_raster layers
bathym_ggbw  = RStoolbox::ggR(bathym_c,ggLayer=TRUE,stretch="hist");
bathym_ggrgb = RStoolbox::ggRGB(bathym_rb,r=1,g=2,b=3,ggLayer=TRUE);
rm(bathym,bathym_min,bathym_max,bathym_vals,mat_rgb,bathym_r,bathym_g,bathym_b,bathym_rb);
#plot bathymetry rasters
ggplot()+
  bathym_ggrgb + 
  geom_sf(data=sf_land_ll,fill="green",colour=NA) +
  geom_sf(data=sf_cz20_ll,colour="blue",fill=NA,alpha=0.5) + 
  coord_sf(xlim=c(168,203),y=c(51.67,68.49),expand=FALSE,clip="on");
ggplot()+
  bathym_ggbw + 
  geom_sf(data=sf_land_ll,fill="black",colour=NA) +
  geom_sf(data=sf_cz20_ll,colour="black",fill=NA,alpha=0.5) + 
  coord_sf(xlim=c(168,203),y=c(51.67,68.49),expand=FALSE,clip="on");
#--save objects
wtsUtilities::saveObj(bathym_ggbw, "./inst/extdata/BathymAnnotLayerBW.LL.RData");
wtsUtilities::saveObj(bathym_ggrgb,"./inst/extdata/BathymAnnotLayerRGB.LL.RData");

