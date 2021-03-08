#--code to consider different approaches to maps
require(dplyr);
require(ggplot2);
require(magrittr);
#require(rIBMsSnowCrab);
#require(sf);
#require(wtsGIS)
#require(wtsROMS)
source('./R/getPackageData.R')
source('./R/spatialIMD_calc.R')
source('./R/spatialIMD_map.R')

#----define crs's
crs_LL = wtsGIS::get_crs(4326);#--WGS84
crs_XY = wtsGIS::get_crs(3338);#--Alaska Albers

#----define bounding boxes
#------bbx's defined in rIBMsSnowCrab
bbx_sc_LL = rIBMsSnowCrab::getBBox(ll=TRUE);
bbx_sc_LLtoXY = bbx_sc_LL %>% wtsGIS::transformBBox(crs_XY);
bbx_sc_XY = rIBMsSnowCrab::getBBox(ll=FALSE);
bbx_sc_XYtoLL = bbx_sc_XY %>% wtsGIS::transformBBox(crs_LL);
#------bbx's defined in wtsGIS
bbx_ebs_LL<-wtsGIS::getStandardBBox("EBS");
bbx_ebs_XY = bbx_ebs_LL %>% wtsGIS::transformBBox(crs_XY);

#----get ROMS grid
roms_grid_LL = wtsROMS::getGrid("Bering10K");
if (!identical(sf::st_is_longlat(roms_grid_LL),TRUE)) cat("roms_grid_LL is not LL\n");
roms_grid_XY = roms_grid_LL %>% wtsGIS::transformCRS(crs_XY);
if (!identical(sf::st_is_longlat(roms_grid_XY),FALSE)) cat("roms_grid_XY is not XY\n");

#----get connectivity zones
czs_LL = rIBMsSnowCrab::getConnectivityZones("30-layer",crs_type="LL");
crop_LL_zoea1 = czs_LL %>% subset(zone %in% c(2:5,7:10)) %>% sf::st_union();
crop_LL_shelf = czs_LL %>% subset(zone %in% 1:17) %>% sf::st_union();
#--note that bbx_ebs_ll is -180 to 180, but crop_sf_LL is 0 to 360
#----no overlap in the following
ggplot()+geom_sf(data=crop_LL_shelf)+geom_sf(data=bbx_ebs_LL %>% sf::st_as_sfc(),colour="red",fill="red");
#----proper overlap when bbx is shifted to 0-360
ggplot()+geom_sf(data=crop_LL_shelf)+geom_sf(data=bbx_ebs_LL %>% sf::st_as_sfc() %>% sf::st_shift_longitude(),colour="red",fill="red");
czs_XY = rIBMsSnowCrab::getConnectivityZones("30-layer",crs_type="XY");
crop_XY_zoea1 = czs_XY %>% subset(zone %in% c(2:5,7:10)) %>% sf::st_union();
crop_XY_shelf = czs_XY %>% subset(zone %in% 1:17) %>% sf::st_union();

#----get land
#------land defined in rIBMsSnowCrab
land_sc_LL = getPackageData("land","LL");
sf::st_is_longlat(land_sc_LL);
if (!identical(sf::st_is_longlat(land_sc_LL),TRUE)) cat("land_sc_LL is not LL\n");
land_sc_XY = getPackageData("land","XY");
if (!identical(sf::st_is_longlat(land_sc_XY),FALSE)) cat("land_sc_XY is not XY\n");
#------land defined in wtsGIS
land_ebs_XY = wtsGIS::getPackagedLayer("Alaska");
if (!identical(sf::st_is_longlat(land_ebs_XY),FALSE)) cat("land_ebs_XY is not XY\n");
land_ebs_XYu = land_ebs_XY %>% sf::st_union() %>% sf::st_as_sf();
land_ebs_LL = land_ebs_XY %>% sf::st_transform(crs_LL);
if (!identical(sf::st_is_longlat(land_ebs_LL),TRUE)) cat("land_ebs_LL is not LL\n");

#----get bathymetry
bath_ebs_XY = wtsGIS::getPackagedLayer("ShelfBathymetry");
if (!identical(sf::st_is_longlat(bath_ebs_XY),FALSE)) cat("bath_ebs_XY is not XY\n");
bath_ebs_LL = bath_ebs_XY %>% sf::st_transform(crs_LL);
if (!identical(sf::st_is_longlat(bath_ebs_LL),TRUE)) cat("bath_ebs_LL is not LL\n");

#----make maps
#----WARNING: takes awhile to plot the following
#----map projection is XY
#----graticules are LL
ggplot() + 
  geom_sf(data=land_ebs_XY,colour=NA,fill="green",alpha=0.2);

#----OK!
#----WARNING: takes awhile to plot the following
#----map projection is XY
#----graticules are XY, graticule labels are turned off
ggplot() + 
  geom_sf(data=land_ebs_XY,colour=NA,fill="green",alpha=0.2) +
  coord_sf(datum=crs_XY,label_axes="----");

#----same as above: no real speed-up
ggplot() + 
  geom_sf(data=land_ebs_XYu,colour=NA,fill="green",alpha=0.2) +
  coord_sf(datum=crs_XY,label_axes="----");

#----WARNING: definitely don't want to do the following--takes a very long time!
#----map projection is LL
#----map extends -180 to 180 long because "land" includes Aleutians
#----graticules are LL
ggplot() + 
  geom_sf(data=land_ebs_LL,colour=NA,fill="blue",alpha=0.2);

#----WARNING: definitely don't want to do the following--takes a very long time!
#----map projection is LL
#----map extends -180 to 180 long
#----graticules are LL
ggplot() + 
  geom_sf(data=land_ebs_LL,colour=NA,fill="blue",alpha=0.2) +
  geom_sf(data=land_ebs_XY,colour=NA,fill="green",alpha=0.2);

#----map projection is XY
#----graticules are XY
ggplot() + 
  geom_sf(data=land_ebs_XY,colour=NA,fill="green",alpha=0.2) +
  geom_sf(data=land_ebs_LL,colour=NA,fill="blue",alpha=0.2) +
  coord_sf(datum=crs_XY,label_axes="----");

#----map projection is LL
#----map extent is fine because of longitude shift
#----graticules are LL
land_ebs_LLw = land_ebs_LL %>% sf::st_shift_longitude();
ggplot() + 
  geom_sf(data=land_ebs_LLw,colour=NA,fill="blue",alpha=0.2)+
  coord_sf(datum=crs_LL,label_axes="----");

#----want to plot in crs_XY using ebs bounding box
#----DOESN"T WORK! coordinate system appears to be changed from coord_sf(...) definition:
#------doesn't crop to limits
#------graticules are LL
ggplot() + coord_sf(crs=crs_XY,datum=crs_XY,label_axes="----",default=TRUE,clip="on",
                    xlim=c(bbx_ebs_XY["xmin"],bbx_ebs_XY["xmax"]),ylim=c(bbx_ebs_XY["ymin"],bbx_ebs_XY["ymax"])) +
  geom_sf(data=land_ebs_XY,colour=NA,fill="green",alpha=1.0) +
  geom_sf(data=bath_ebs_XY,colour="black",alpha=1.0) + 
  geom_sf(data=crop_XY_shelf,colour="blue",alpha=1.0) +
  geom_sf(data=bbx_ebs_XY %>% sf::st_as_sfc() %>% sf::st_as_sf(),colour="red",fill=NA);
#----TRY AGAIN!
#----WORKS! appears coord_sf(...) definition must be at end
#------clips to limits
#------graticules are XY
xlim=c(bbx_ebs_XY["xmin"],bbx_ebs_XY["xmax"]);
ylim=c(bbx_ebs_XY["ymin"],bbx_ebs_XY["ymax"]);
ggplot() + 
  geom_sf(data=land_ebs_XY,colour=NA,fill="green",alpha=1.0) +
  geom_sf(data=bath_ebs_XY,colour="black",alpha=1.0) + 
  geom_sf(data=crop_XY_shelf,colour="blue",fill=NA,alpha=1.0) +
  geom_sf(data=bbx_ebs_XY %>% sf::st_as_sfc() %>% sf::st_as_sf(),colour="red",fill=NA) + 
  coord_sf(crs=crs_XY,datum=crs_XY,label_axes="----",default=TRUE,clip="on",xlim=xlim,ylim=ylim,expand=FALSE);

#----want to plot in crs_LL using ebs bounding box
#----MISSING crop_LL_shelf, but everything else looks good
xlim=c(bbx_ebs_LL["xmin"],bbx_ebs_LL["xmax"]);
ylim=c(bbx_ebs_LL["ymin"],bbx_ebs_LL["ymax"]);
ggplot() + 
  geom_sf(data=land_ebs_LL,colour=NA,fill="green",alpha=1.0) +
  geom_sf(data=bath_ebs_LL,colour="black",alpha=1.0) + 
  geom_sf(data=crop_LL_shelf,colour="blue",fill=NA,alpha=1.0) +
  geom_sf(data=bbx_ebs_LL %>% sf::st_as_sfc() %>% sf::st_as_sf(),colour="red",fill=NA) + 
  coord_sf(crs=crs_LL,datum=crs_LL,label_axes="----",default=TRUE,clip="on",xlim=xlim,ylim=ylim,expand=FALSE);
#----TRY AGAIN!
#----FINALLY WORKS, but requires shifting longitude 
bbx_ebs_LLp = bbx_ebs_LL;
bbx_ebs_LLp["xmin"] = 360+bbx_ebs_LLp["xmin"];
bbx_ebs_LLp["xmax"] = 360+bbx_ebs_LLp["xmax"];
xlim=c(bbx_ebs_LLp["xmin"],bbx_ebs_LLp["xmax"]);
ylim=c(bbx_ebs_LLp["ymin"],bbx_ebs_LLp["ymax"]);
ggplot() + 
  geom_sf(data=land_ebs_LL %>% sf::st_shift_longitude(),colour=NA,fill="green",alpha=1.0) +
  geom_sf(data=bath_ebs_LL %>% sf::st_shift_longitude(),colour="black",alpha=1.0) + 
  geom_sf(data=crop_LL_shelf,colour="blue",fill=NA,alpha=1.0) +
  geom_sf(data=bbx_ebs_LLp %>% sf::st_as_sfc(),colour="red",fill=NA) + 
  coord_sf(crs=crs_LL,datum=crs_LL,label_axes="----",default=TRUE,clip="on",xlim=xlim,ylim=ylim,expand=FALSE);

