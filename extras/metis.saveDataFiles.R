
library(tibble);library(dplyr);library(rgdal);library(devtools);library(metis); library(rmapshaper); library(tmaptools)
library(rgeos)

# Current Data
#data(package="metis")

#-----------------
# World Maps (Countries, States)
#-----------------

# Worldmap countries
#-------------------
if(!exists("mapCountries")){
  examplePolyFolder<-paste(getwd(),"/dataFiles/gis/metis/naturalEarth",sep="")
  examplePolyFile<-paste("ne_10m_admin_0_countries",sep="")
  x=rgdal::readOGR(dsn=examplePolyFolder,layer=examplePolyFile,use_iconv=T,encoding='UTF-8')
  head(x@data); names(x@data)
  mapx <- x
  mapx@data <- mapx@data %>%
    dplyr::select(subRegion=NAME,subRegionAlt=ADM0_A3, POP_EST) %>%
    dplyr::mutate(region="World",subRegionType="country", source="https://www.naturalearthdata.com/downloads/",
                  subRegion = as.character(subRegion),
                  subRegion=if_else(subRegion=="United States of America","USA",subRegion))
  mapx <- mapx[!grepl("Antarctica",mapx$subRegion),]
  mapx@data <- mapx@data%>%droplevels()
  format(object.size(mapx), units="Mb")
  mapx<-as(simplify_shape(mapx, fact = 0.1),Class="Spatial")
  format(object.size(mapx), units="Mb")
  #sp::plot(mapx)
  #metis.map(dataPolygon=mapx,fillColumn = "subRegion",labels=F,printFig=F, facetsON=F)
  mapx<-rgeos::gBuffer(mapx, byid=TRUE, width=0)
  format(object.size(mapx), units="Mb")
  mapCountries <- mapx
  use_data(mapCountries, overwrite=T)
}

# Worldmap states
#-------------------
if(!exists("mapStates")){
  examplePolyFolder<-paste(getwd(),"/dataFiles/gis/metis/naturalEarth",sep="")
  examplePolyFile<-paste("ne_10m_admin_1_states_provinces",sep="")
  x=rgdal::readOGR(dsn=examplePolyFolder,layer=examplePolyFile,use_iconv=T,encoding='UTF-8')
  head(x@data); names(x@data)
  mapx <- x
  mapx@data <- mapx@data %>%
    dplyr::select(region=admin,subRegion=name,subRegionAlt=postal) %>%
    dplyr::mutate(subRegionType="states", source="https://www.naturalearthdata.com/downloads/",
                  region = as.character(region),
                  region=if_else(region=="United States of America","USA",region))
  mapx <- mapx[!grepl("Antarctica",mapx$region),]
  mapx@data <- mapx@data%>%droplevels()
  format(object.size(mapx), units="Mb")
  mapx<-as(simplify_shape(mapx, fact = 0.01),Class="Spatial")
  format(object.size(mapx), units="Mb")
  # sp::plot(mapx)
  # metis.map(dataPolygon=mapx,fillColumn = "subRegion",labels=F,printFig=F, facetsON=F, fileName="factp1")
  mapx<-rgeos::gBuffer(mapx, byid=TRUE, width=0)
  format(object.size(mapx), units="Mb")
  mapStates <- mapx
  use_data(mapStates, overwrite=T)
}

#-----------------
# GCAM Maps (Regions, Basins, Land)
#-----------------

# GCAM 32 Regions
#------------------
if(!exists("mapGCAMReg32")){
  examplePolyFolder<-paste(getwd(),"/dataFiles/gis/metis/gcam",sep="")
  examplePolyFile<-paste("region32_0p5deg",sep="")
  x=rgdal::readOGR(dsn=examplePolyFolder,layer=examplePolyFile,use_iconv=T,encoding='UTF-8')
  head(x@data); names(x@data)
  mapx <- x
  idMapping <- tibble::as_tibble(data.table::fread(paste(getwd(),"/dataFiles/gis/metis/gcam/GCAM_region_names.csv",sep=""),skip=4,header=T))
  mapx@data <- mapx@data %>%
    dplyr::mutate(reg32_id=as.character(reg32_id))%>%
    left_join(idMapping%>%dplyr::mutate(reg32_id=as.character(GCAM_region_ID)),by="reg32_id")%>%
    dplyr::mutate(subRegionType="GCAMReg32", source="https://confluence.pnnl.gov/confluence/display/JGCRI/GCAM+Shape+Files",
                  subRegion=region,
                  region = "World",
                  subRegionAlt=reg32_id)%>%
    dplyr::select(-ID,-GRIDCODE,-GCAM_region_ID,-reg32_id)
  mapx@data <- droplevels(mapx@data)
  format(object.size(mapx), units="Mb")
  #mapx<-as(simplify_shape(mapx, fact = 0.05),Class="Spatial")
  #format(object.size(mapx), units="Mb")
  #sp::plot(mapx)
  # metis.map(dataPolygon=mapx,fillColumn = "subRegion",labels=F,printFig=F, facetsON=F, fileName="factp1")
  mapx<-rgeos::gBuffer(mapx, byid=TRUE, width=0)
  format(object.size(mapx), units="Mb")
  mapGCAMReg32 <- mapx
  use_data(mapGCAMReg32, overwrite=T)
}


# GCAM Basins
#------------------
if(!exists("mapGCAMBasins")){
  examplePolyFolder<-paste(getwd(),"/dataFiles/gis/metis/gcam",sep="")
  examplePolyFile<-paste("Global235_CLM_final_5arcmin_multipart",sep="")
  x=rgdal::readOGR(dsn=examplePolyFolder,layer=examplePolyFile,use_iconv=T,encoding='UTF-8')
  head(x@data); names(x@data)
  mapx <- x
  mapx@data <- mapx@data %>%
    dplyr::select(subRegion=basin_name, subRegionAlt=basin_id) %>%
    dplyr::mutate(region = "World",subRegionType="GCAMBasin",subRegion=gsub("_Basin","",subRegion))
  format(object.size(mapx), units="Mb")
  #mapx<-as(simplify_shape(mapx, fact = 0.05),Class="Spatial")
  #format(object.size(mapx), units="Mb")
  #sp::plot(mapx)
  # metis.map(dataPolygon=mapx,fillColumn = "subRegion",labels=F,printFig=F, facetsON=F, fileName="factp1")
  mapx<-rgeos::gBuffer(mapx, byid=TRUE, width=0)
  format(object.size(mapx), units="Mb")
  mapGCAMBasins <- mapx
  use_data(mapGCAMBasins, overwrite=T)
}


# GCAM Land
#------------------
if(!exists("mapGCAMLand")){
  examplePolyFolder<-paste(getwd(),"/dataFiles/gis/metis/gcam",sep="")
  examplePolyFile<-paste("region32glu_moirai_out_vect",sep="")
  x=rgdal::readOGR(dsn=examplePolyFolder,layer=examplePolyFile,use_iconv=T,encoding='UTF-8')
  head(x@data); names(x@data)
  mapx <- x
  idMapping <- data.table::fread(paste(getwd(),"/dataFiles/gis/metis/gcam/GCAM_region_names.csv",sep=""),skip=4,header=T)
  mapx@data <- mapx@data %>%
    dplyr::select(subRegion=glu_name, subRegionAlt=Rg32Glu_id, glu_id, reg32_id) %>%
    dplyr::mutate(subRegionType="GCAMRegLand", source="https://confluence.pnnl.gov/confluence/display/JGCRI/GCAM+Shape+Files",
                  region = "World")%>%
    dplyr::left_join(idMapping%>%
                       dplyr::select(reg32_name=region, reg32_id=GCAM_region_ID)%>%
                       unique())
  head(mapx@data)
  format(object.size(mapx), units="Mb")
  #mapx<-as(simplify_shape(mapx, fact = 0.05),Class="Spatial")
  #format(object.size(mapx), units="Mb")
  #sp::plot(mapx)
  # metis.map(dataPolygon=mapx,fillColumn = "subRegion",labels=F,printFig=F, facetsON=F, fileName="factp1")
  mapx<-rgeos::gBuffer(mapx, byid=TRUE, width=0)
  format(object.size(mapx), units="Mb")
  mapGCAMLand <- mapx
  use_data(mapGCAMLand, overwrite=T)
}



#-----------------
# Hydrology Maps (HydroShed, HUC)
#-----------------


# Hydro sheds
# https://www.hydrosheds.org/page/hydrobasins
# Lehner, B., Grill G. (2013): Global river hydrography and network routing:
# baseline data and new approaches to study the world’s large river systems.
# Hydrological Processes, 27(15): 2171–2186. Data is available at www.hydrosheds.org

# HydroSheds Level 1
#-------------------
if(!exists("mapHydroShed1")){
  examplePolyFolder<-paste(getwd(),"/dataFiles/gis/metis/subbasin_hydrobasin",sep="")
  examplePolyFile<-paste("hydrobasins_level_1",sep="")
  x=rgdal::readOGR(dsn=examplePolyFolder,layer=examplePolyFile,use_iconv=T,encoding='UTF-8')
  head(x@data); names(x@data)
  mapx <- x
  mapx@data <- mapx@data %>%
    dplyr::select(subRegion=HYBAS_ID) %>%
    dplyr::mutate(region="World",subRegionType="basin", subRegionAlt=subRegion,source="https://www.naturalearthdata.com/downloads/")
  head(mapx@data); mapx@data%>%distinct(region)%>%arrange(region)
  format(object.size(mapx), units="Mb")
  a<-tmaptools::simplify_shape(mapx, fact = 0.01)
  mapx <- as(sf::st_collection_extract(x = st_geometry(a),
                                       type = "POLYGON"), "Spatial")
  format(object.size(mapx), units="Mb")
  # Need to Covnert this back to an spdf
  p.df <- data.frame( ID=1:length(mapx))
  pid <- sapply(slot(mapx, "polygons"), function(x) slot(x, "ID")) # Extract polygon ID's
  p.df <- data.frame( ID=1:length(mapx), row.names = pid) # Create dataframe with correct rownames
  p <- SpatialPolygonsDataFrame(mapx, p.df)
  p@data <- a%>%as.data.frame()%>%dplyr::select(-geometry)
  mapx<-p
  format(object.size(mapx), units="Mb")
  # sp::plot(mapx)
  # metis.map(dataPolygon=mapx,fillColumn = "subRegion",labels=F,printFig=F, facetsON=F,fileName = "HydroShed1")
  mapx<-rgeos::gBuffer(mapx, byid=TRUE, width=0)
  format(object.size(mapx), units="Mb")
  mapHydroShed1 <- mapx
  use_data(mapHydroShed1, overwrite=T)
}

# HydroSheds Level 2#-------------------
if(!exists("mapHydroShed2")){
  examplePolyFolder<-paste(getwd(),"/dataFiles/gis/metis/subbasin_hydrobasin",sep="")
  examplePolyFile<-paste("hydrobasins_level_2",sep="")
  x=rgdal::readOGR(dsn=examplePolyFolder,layer=examplePolyFile,use_iconv=T,encoding='UTF-8')
  head(x@data); names(x@data)
  mapx <- x
  mapx@data <- mapx@data %>%
    dplyr::select(subRegion=HYBAS_ID, SUB_AREA) %>%
    dplyr::mutate(region="World",subRegionType="basin", subRegionAlt=subRegion,source="https://www.naturalearthdata.com/downloads/")
  head(mapx@data); mapx@data%>%distinct(region)%>%arrange(region)
  a<-tmaptools::simplify_shape(mapx, fact = 0.01)
  mapx <- as(sf::st_collection_extract(x = st_geometry(a),
                                       type = "POLYGON"), "Spatial")
  format(object.size(mapx), units="Mb")
  # Need to Covnert this back to an spdf
  p.df <- data.frame( ID=1:length(mapx))
  pid <- sapply(slot(mapx, "polygons"), function(x) slot(x, "ID")) # Extract polygon ID's
  p.df <- data.frame( ID=1:length(mapx), row.names = pid) # Create dataframe with correct rownames
  p <- SpatialPolygonsDataFrame(mapx, p.df)
  p@data <- a%>%as.data.frame()%>%dplyr::select(-geometry)
  mapx<-p
  format(object.size(mapx), units="Mb")
  # sp::plot(mapx)
  # metis.map(dataPolygon=mapx,fillColumn = "subRegion",labels=F,printFig=F, facetsON=F,fileName = "HydroShed1")
  mapx<-rgeos::gBuffer(mapx, byid=TRUE, width=0)
  format(object.size(mapx), units="Mb")
  mapHydroShed2 <- mapx
  use_data(mapHydroShed2, overwrite=T)
}

# HydroSheds Level 3
#-------------------
if(!exists("mapHydroShed3")){
  examplePolyFolder<-paste(getwd(),"/dataFiles/gis/metis/subbasin_hydrobasin",sep="")
  examplePolyFile<-paste("hydrobasins_level_3",sep="")
  x=rgdal::readOGR(dsn=examplePolyFolder,layer=examplePolyFile,use_iconv=T,encoding='UTF-8')
  head(x@data); names(x@data)
  mapx <- x
  mapx@data <- mapx@data %>%
    dplyr::select(subRegion=HYBAS_ID, SUB_AREA) %>%
    dplyr::mutate(region="World",subRegionType="basin", subRegionAlt=subRegion,source="https://www.naturalearthdata.com/downloads/")
  head(mapx@data); mapx@data%>%distinct(region)%>%arrange(region)
  a<-tmaptools::simplify_shape(mapx, fact = 0.01)
  mapx <- as(sf::st_collection_extract(x = st_geometry(a),
                                       type = "POLYGON"), "Spatial")
  format(object.size(mapx), units="Mb")
  # Need to Covnert this back to an spdf
  p.df <- data.frame( ID=1:length(mapx))
  pid <- sapply(slot(mapx, "polygons"), function(x) slot(x, "ID")) # Extract polygon ID's
  p.df <- data.frame( ID=1:length(mapx), row.names = pid) # Create dataframe with correct rownames
  p <- SpatialPolygonsDataFrame(mapx, p.df)
  p@data <- a%>%as.data.frame()%>%dplyr::select(-geometry)
  mapx<-p
  format(object.size(mapx), units="Mb")
  # sp::plot(mapx)
  # metis.map(dataPolygon=mapx,fillColumn = "subRegion",labels=F,printFig=F, facetsON=F,fileName = "HydroShed1")
  mapx<-rgeos::gBuffer(mapx, byid=TRUE, width=0)
  format(object.size(mapx), units="Mb")
  mapHydroShed3 <- mapx
  use_data(mapHydroShed3, overwrite=T)
}

# # HydroSheds Level 4
# #-------------------
# if(!exists("mapHydroShed4")){
# examplePolyFolder<-paste(getwd(),"/dataFiles/gis/metis/subbasin_hydrobasin",sep="")
# examplePolyFile<-paste("hydrobasins_level_4",sep="")
# x=rgdal::readOGR(dsn=examplePolyFolder,layer=examplePolyFile,use_iconv=T,encoding='UTF-8')
# head(x@data); names(x@data)
# mapx <- x
# mapx@data <- mapx@data %>%
#   dplyr::select(subRegion=HYBAS_ID, SUB_AREA) %>%
#   dplyr::mutate(region="World",subRegionType="basin", subRegionAlt=subRegion,source="https://www.naturalearthdata.com/downloads/")
# head(mapx@data); mapx@data%>%distinct(region)%>%arrange(region)
# a<-tmaptools::simplify_shape(mapx, fact = 0.01)
# mapx <- as(sf::st_collection_extract(x = st_geometry(a),
#                                      type = "POLYGON"), "Spatial")
# format(object.size(mapx), units="Mb")
# # Need to Covnert this back to an spdf
# p.df <- data.frame( ID=1:length(mapx))
# pid <- sapply(slot(mapx, "polygons"), function(x) slot(x, "ID")) # Extract polygon ID's
# p.df <- data.frame( ID=1:length(mapx), row.names = pid) # Create dataframe with correct rownames
# p <- SpatialPolygonsDataFrame(mapx, p.df)
# p@data <- a%>%as.data.frame()%>%dplyr::select(-geometry)
# mapx<-p
# format(object.size(mapx), units="Mb")
# # sp::plot(mapx)
# # metis.map(dataPolygon=mapx,fillColumn = "subRegion",labels=F,printFig=F, facetsON=F,fileName = "HydroShed1")
# mapHydroShed4 <- mapx
# use_data(mapHydroShed4, overwrite=T)
# }

# HUC USGS
# https://water.usgs.gov/GIS/huc.html
# https://datagateway.nrcs.usda.gov/Catalog/ProductDescription/WBD.html
# https://nrcs.app.box.com/v/huc

# USGS HUC Levels
# US52 HUC 2
#-------------------
if(!exists("mapUS52HUC2")){
  examplePolyFolder<-paste(getwd(),"/dataFiles/gis/metis/USA/WBD_LatestVersion/wbdhu2_a_us_september2019",sep="")
  examplePolyFile<-paste("WBDHU2",sep="")
  x=rgdal::readOGR(dsn=examplePolyFolder,layer=examplePolyFile,use_iconv=T,encoding='UTF-8')
  head(x@data); names(x@data); x@data%>%distinct(STATES)
  mapx <- x
  mapx@data <- mapx@data %>%
    dplyr::select(subRegion=HUC2, subRegionAlt=NAME,STATES) %>%
    dplyr::mutate(region="USA",subRegionType="basin", source="https://water.usgs.gov/GIS/huc.html")
  head(mapx@data); mapx@data%>%distinct(subRegionAlt)%>%arrange(subRegionAlt)
  format(object.size(mapx), units="Mb")
  mapx<-as(simplify_shape(mapx, fact = 0.01),Class="Spatial")
  format(object.size(mapx), units="Mb")
  #sp::plot(mapx)
  #metis.map(dataPolygon=mapx,fillColumn = "subRegion",labels=T,printFig=F, facetsON=F)
  mapx<-rgeos::gBuffer(mapx, byid=TRUE, width=0)
  format(object.size(mapx), units="Mb")
  mapUS52HUC2 <- mapx
  use_data(mapUS52HUC2, overwrite=T)
}

# US49 HUC 2
#-------------------
if(!exists("mapUS49HUC2")){
  mapx <- mapUS52HUC2[!grepl("19|20|21|22",mapUS52HUC2$subRegion),]
  mapx@data <- mapx@data%>%droplevels()
  head(mapx@data); mapx@data%>%distinct(subRegionAlt)%>%arrange(subRegionAlt)
  format(object.size(mapx), units="Mb")
  # mapx<-as(simplify_shape(mapx, fact = 0.01),Class="Spatial")
  # format(object.size(mapx), units="Mb")
  #sp::plot(mapx)
  #metis.map(dataPolygon=mapx,fillColumn = "subRegion",labels=T,printFig=F, facetsON=F, fileName = "HUC2")
  mapx<-rgeos::gBuffer(mapx, byid=TRUE, width=0)
  format(object.size(mapx), units="Mb")
  mapUS49HUC2 <- mapx
  use_data(mapUS49HUC2, overwrite=T)
}

# US52 HUC 4
#-------------------
if(!exists("mapUS52HUC4")){
  examplePolyFolder<-paste(getwd(),"/dataFiles/gis/metis/USA/WBD_LatestVersion/wbdhu4_a_us_september2019",sep="")
  examplePolyFile<-paste("WBDHU4",sep="")
  x=rgdal::readOGR(dsn=examplePolyFolder,layer=examplePolyFile,use_iconv=T,encoding='UTF-8')
  head(x@data); names(x@data); unique(x@data$STATES)
  mapx <- x
  mapx@data <- mapx@data %>%
    dplyr::select(subRegion=HUC4, subRegionAlt=NAME,STATES) %>%
    dplyr::mutate(region="USA",subRegionType="basin", source="https://water.usgs.gov/GIS/huc.html")
  head(mapx@data); mapx@data%>%distinct(subRegionAlt)%>%arrange(subRegionAlt)
  format(object.size(mapx), units="Mb")
  mapx<-as(simplify_shape(mapx, fact = 0.01),Class="Spatial")
  format(object.size(mapx), units="Mb")
  #sp::plot(mapx)
  #metis.map(dataPolygon=mapx,fillColumn = "subRegion",labels=F,printFig=F, facetsON=F)
  mapx<-rgeos::gBuffer(mapx, byid=TRUE, width=0)
  format(object.size(mapx), units="Mb")
  mapUS52HUC4 <- mapx
  use_data(mapUS52HUC4, overwrite=T)
}

# US49 HUC 4
#-------------------
if(!exists("mapUS49HUC4")){
  mapx <- mapUS52HUC4[!grepl("PR|HI|AK|CN|AS|GU|MP",mapUS52HUC4$STATES),]
  mapx@data <- mapx@data%>%droplevels()
  head(mapx@data); mapx@data%>%distinct(subRegionAlt)%>%arrange(subRegionAlt)
  format(object.size(mapx), units="Mb")
  #mapx<-as(simplify_shape(mapx, fact = 0.01),Class="Spatial")
  #format(object.size(mapx), units="Mb")
  #sp::plot(mapx)
  #metis.map(dataPolygon=mapx,fillColumn = "subRegion",labels=T,printFig=F, facetsON=F, fileName = "HUC4")
  mapx<-rgeos::gBuffer(mapx, byid=TRUE, width=0)
  format(object.size(mapx), units="Mb")
  mapUS49HUC4 <- mapx
  use_data(mapUS49HUC4, overwrite=T)
}

# # US52 HUC 6
# #-------------------
# if(!exists("mapUS52HUC6")){
# examplePolyFolder<-paste(getwd(),"/dataFiles/gis/metis/USA/WBD_LatestVersion/wbdhu6_a_us_september2019",sep="")
# examplePolyFile<-paste("WBDHU6",sep="")
# x=rgdal::readOGR(dsn=examplePolyFolder,layer=examplePolyFile,use_iconv=T,encoding='UTF-8')
# head(x@data); names(x@data)
# mapx <- x
# mapx@data <- mapx@data %>%
#   dplyr::select(subRegion=HUC6, subRegionAlt=NAME,STATES) %>%
#   dplyr::mutate(region="USA",subRegionType="basin", source="https://water.usgs.gov/GIS/huc.html")
# head(mapx@data); mapx@data%>%distinct(subRegionAlt)%>%arrange(subRegionAlt)
# format(object.size(mapx), units="Mb")
# # mapx<-as(simplify_shape(mapx, fact = 0.01),Class="Spatial")
# # format(object.size(mapx), units="Mb")
# #sp::plot(mapx)
# #metis.map(dataPolygon=mapx,fillColumn = "subRegion",labels=F,printFig=F, facetsON=F)
# mapUS52HUC6 <- mapx
# use_data(mapUS52HUC6, overwrite=T)
# }
#
# # US49 HUC 6
# #-------------------
# if(!exists("mapUS49HUC6")){
# mapx <- mapUS52HUC6[(!mapUS52HUC6$subRegionAlt %in% c("Alaska Region","Hawaii Region","Caribbean Region",
#                                                       "South Pacific Region")),]
# mapx@data <- mapx@data%>%droplevels()
# head(mapx@data); mapx@data%>%distinct(subRegionAlt)%>%arrange(subRegionAlt)
# #format(object.size(mapx), units="Mb")
# # mapx<-as(simplify_shape(mapx, fact = 0.01),Class="Spatial")
# # format(object.size(mapx), units="Mb")
# #sp::plot(mapx)
# #metis.map(dataPolygon=mapx,fillColumn = "subRegion",labels=T,printFig=F, facetsON=F, fileName = "HUC2")
# mapUS49HUC6 <- mapx
# use_data(mapUS49HUC6, overwrite=T)
# }
#
# # US52 HUC 8
# #-------------------
# if(!exists("mapUS52HUC8")){
# examplePolyFolder<-paste(getwd(),"/dataFiles/gis/metis/USA/WBD_LatestVersion/wbdhu8_a_us_september2019",sep="")
# examplePolyFile<-paste("WBDHU8",sep="")
# x=rgdal::readOGR(dsn=examplePolyFolder,layer=examplePolyFile,use_iconv=T,encoding='UTF-8')
# head(x@data); names(x@data)
# mapx <- x
# mapx@data <- mapx@data %>%
#   dplyr::select(subRegion=HUC8, subRegionAlt=NAME,STATES) %>%
#   dplyr::mutate(region="USA",subRegionType="basin", source="https://water.usgs.gov/GIS/huc.html")
# head(mapx@data); mapx@data%>%distinct(subRegionAlt)%>%arrange(subRegionAlt)
# format(object.size(mapx), units="Mb")
# # mapx<-as(simplify_shape(mapx, fact = 0.01),Class="Spatial")
# # format(object.size(mapx), units="Mb")
# #sp::plot(mapx)
# #metis.map(dataPolygon=mapx,fillColumn = "subRegion",labels=F,printFig=F, facetsON=F)
# mapUS52HUC8 <- mapx
# use_data(mapUS52HUC8, overwrite=T)
# }
#
# # US49 HUC 8
# #-------------------
# if(!exists("mapUS49HUC8")){
# mapx <- mapUS52HUC8[(!mapUS52HUC8$subRegionAlt %in% c("Alaska Region","Hawaii Region","Caribbean Region",
#                                                       "South Pacific Region")),]
# mapx@data <- mapx@data%>%droplevels()
# head(mapx@data); mapx@data%>%distinct(subRegionAlt)%>%arrange(subRegionAlt)
# #format(object.size(mapx), units="Mb")
# # mapx<-as(simplify_shape(mapx, fact = 0.01),Class="Spatial")
# # format(object.size(mapx), units="Mb")
# #sp::plot(mapx)
# #metis.map(dataPolygon=mapx,fillColumn = "subRegion",labels=T,printFig=F, facetsON=F, fileName = "HUC2")
# mapUS49HUC8 <- mapx
# use_data(mapUS49HUC8, overwrite=T)
# }

#-----------------
# US Maps ( 52 State, 49 State, Counties, Regions, Grid Regions)
#-----------------

# US 52 (including Alaska, Hawaii and Puerto Rico)
#-------------------
if(!exists("mapUS52")){
  examplePolyFolder<-paste(getwd(),"/dataFiles/gis/metis/USA/cb_2018_us_state_20m",sep="")
  examplePolyFile<-paste("cb_2018_us_state_20m",sep="")
  x=rgdal::readOGR(dsn=examplePolyFolder,layer=examplePolyFile,use_iconv=T,encoding='UTF-8')
  head(x@data); names(x@data); nrow(x)
  mapx <- x
  mapx@data <- mapx@data %>%
    dplyr::select(subRegion=STUSPS,subRegionAlt=NAME, STATEFP) %>%
    dplyr::mutate(region="USA",subRegionType="state", source="https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html")
  head(mapx@data); unique(mapx$subRegion)
  # sp::plot(mapx)
  # metis.map(dataPolygon=mapx,fillColumn = "subRegion",labels=F,printFig=F, facetsON=F)
  mapx<-rgeos::gBuffer(mapx, byid=TRUE, width=0)
  format(object.size(mapx), units="Mb")
  mapUS52 <- mapx
  use_data(mapUS52, overwrite=T)
}

# US 49 (Excluding Alsaka, Hawaii and Puerto Rico)
#-------------------
if(!exists("mapUS49")){
  mapx <- mapUS52[(mapUS52$region=="USA" & !mapUS52$subRegion %in% c("AK","HI","PR")),]
  mapx@data <- mapx@data%>%droplevels()
  head(mapx@data); nrow(mapx); mapx@data%>%distinct(subRegion)
  # sp::plot(mapx)
  # metis.map(dataPolygon=mapx,fillColumn = "subRegion",labels=F,printFig=F, facetsON=F)
  mapUS49<-mapx
  use_data(mapUS49, overwrite=T)
}


# US 52 Counties
#-------------------
if(!exists("mapUS52County")){
  examplePolyFolder<-paste(getwd(),"/dataFiles/gis/metis/USA/cb_2018_us_county_20m",sep="")
  examplePolyFile<-paste("cb_2018_us_county_20m",sep="")
  x=rgdal::readOGR(dsn=examplePolyFolder,layer=examplePolyFile,use_iconv=T,encoding='UTF-8')
  head(x@data); names(x@data); nrow(x)
  mapx <- x
  mapx@data <- mapx@data %>%
    dplyr::select(subRegion=NAME,subRegionAlt=COUNTYFP,STATEFP) %>%
    dplyr::mutate(region="USA",subRegionType="county", source="https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html") %>%
    dplyr::left_join(mapUS52@data%>%dplyr::select(STATEFP,STATECODE=subRegion, STATENAME=subRegionAlt))
  head(mapx@data); unique(mapx$subRegion)
  # sp::plot(mapx)
  # metis.map(dataPolygon=mapx,fillColumn = "subRegion",labels=F,printFig=F, facetsON=F)
  mapx<-rgeos::gBuffer(mapx, byid=TRUE, width=0)
  format(object.size(mapx), units="Mb")
  mapUS52County <- mapx
  use_data(mapUS52County, overwrite=T)
}



# US 49 Counties
#-------------------
if(!exists("mapUS49County")){
  mapx <- mapUS52County[(!mapUS52County$STATECODE %in% c("AK","HI","PR")),]
  mapx@data <- mapx@data%>%droplevels()
  head(mapx@data); nrow(mapx); mapx@data%>%distinct(subRegion)
  # sp::plot(mapx)
  # metis.map(dataPolygon=mapx,fillColumn = "subRegion",labels=F,printFig=F, facetsON=F)
  mapx<-rgeos::gBuffer(mapx, byid=TRUE, width=0)
  format(object.size(mapx), units="Mb")
  mapUS49County<-mapx
  use_data(mapUS49County, overwrite=T)
}


# Intersections
#-------------------

# Intersection of GCAM Basins and Countries
if(!exists("mapIntersectGCAMBasinCountry")){
  m1 <- mapGCAMBasins
  m2 <- mapCountries
  raster::projection(m1)<-sp::proj4string(m2)
  mapx<-raster::intersect(m1,m2)
  mapx@data <- mapx@data %>%
    dplyr::mutate(subRegion=paste(subRegion.1,subRegion.2,sep="_X_"),
                  region=region.1,
                  subRegionAlt=paste(subRegionAlt.1,subRegionAlt.2,sep="_X_"),
                  subRegionType=paste(subRegionType.1,subRegionType.2,sep="_X_")) %>%
    dplyr::rename(subRegion_GCAMBasin=subRegion.1,
                  subRegion_Country=subRegion.2,
                  subRegionAlt_GCAMBasin=subRegionAlt.1,
                  subRegionAlt_Country=subRegionAlt.2,
                  subRegionType_GCAMBasin=subRegionType.1,
                  subRegionType_Country=subRegionType.2) %>%
    dplyr::select(-region.1,-region.2, -POP_EST)
  head(mapx@data)
  # sp::plot(mapx)
  # metis.map(dataPolygon=mapx,fillColumn = "subRegion",labels=F,printFig=F, facetsON=F)
  mapx<-rgeos::gBuffer(mapx, byid=TRUE, width=0)
  format(object.size(mapx), units="Mb")
  mapIntersectGCAMBasinCountry<-mapx
  use_data(mapIntersectGCAMBasinCountry, overwrite=T)
}

# Intersection of GCAM Basins and 32 GCAM Regions
if(!exists("mapIntersectGCAMBasin32Reg")){
  m1 <- mapGCAMBasins
  m2 <- mapGCAMReg32
  m2 <- sp::spTransform(m2,raster::crs(m1))
  mapx<-raster::intersect(m1,m2)
  mapx@data <- mapx@data %>%
    dplyr::mutate(subRegion=paste(subRegion.1,subRegion.2,sep="_X_"),
                  region=region.1,
                  subRegionAlt=paste(subRegionAlt.1,subRegionAlt.2,sep="_X_"),
                  subRegionType=paste(subRegionType.1,subRegionType.2,sep="_X_")) %>%
    dplyr::rename(subRegion_GCAMBasin=subRegion.1,
                  subRegion_GCAMReg32=subRegion.2,
                  subRegionAlt_GCAMBasin=subRegionAlt.1,
                  subRegionAlt_GCAMreg32=subRegionAlt.2,
                  subRegionType_GCAMBasin=subRegionType.1,
                  subRegionType_GCAMReg32=subRegionType.2) %>%
    dplyr::select(-region.1,-region.2)
  head(mapx@data)
  # sp::plot(mapx)
  # metis.map(dataPolygon=mapx,fillColumn = "subRegion",labels=F,printFig=F, facetsON=F)
  mapx<-rgeos::gBuffer(mapx, byid=TRUE, width=0)
  format(object.size(mapx), units="Mb")
  mapIntersectGCAMBasin32Reg<-mapx
  use_data(mapIntersectGCAMBasin32Reg, overwrite=T)
}

# Intersection of GCAM Basins and US 52
if(!exists("mapGCAMBasinsUS52")){
  m1 <- metis::mapGCAMBasins
  m2 <- metis::mapUS52
  m2 <- sp::spTransform(m2,raster::crs(m1))
  mapx<-raster::crop(m1,m2)
  # sp::plot(mapx)
  # metis.map(dataPolygon=mapx,fillColumn = "subRegion",labels=F,printFig=F, facetsON=F)
  mapx<-rgeos::gBuffer(mapx, byid=TRUE, width=0)
  format(object.size(mapx), units="Mb")
  mapGCAMBasinsUS52<-mapx
  use_data(mapGCAMBasinsUS52, overwrite=T)
}

# Intersection of GCAM Basins and US 49 States
if(!exists("mapGCAMBasinsUS49")){
  m1 <- metis::mapGCAMBasins
  m2 <- metis::mapUS49
  m2 <- sp::spTransform(m2,raster::crs(m1))
  mapx<-raster::crop(m1,m2)
  head(mapx@data)
  # sp::plot(mapx)
  # metis.map(dataPolygon=mapx,fillColumn = "subRegion",labels=F,printFig=F, facetsON=F)
  mapx<-rgeos::gBuffer(mapx, byid=TRUE, width=0)
  format(object.size(mapx), units="Mb")
  mapGCAMBasinsUS49<-mapx
  use_data(mapGCAMBasinsUS49, overwrite=T)
}


# Grid Files
#-------------------
if(!exists("grid025")){
  gridx <-  tibble::as_tibble(data.table::fread(paste(getwd(),"/dataFiles/grids/emptyGrids/grid_025.csv",sep="")));
  grid025<-gridx
  use_data(grid025, overwrite=T)
}

if(!exists("grid050")){
  gridx <-  tibble::as_tibble(data.table::fread(paste(getwd(),"/dataFiles/grids/emptyGrids/grid_050.csv",sep="")));
  grid050<-gridx
  use_data(grid050, overwrite=T)
}


#-------------------
# Save Data Files
#-------------------

# Metis XMl Query Files
xmlFilePath = paste(getwd(),"/dataFiles/gcam/metisQueries.xml",sep="")
xmlfile <- XML::xmlTreeParse(xmlFilePath)
xmltop <- XML::xmlRoot(xmlfile)
top <- XML::xmlNode(XML::xmlName(xmltop))
for(i in 1:length(xmltop)){
      top <- XML::addChildren(top, xmltop[[i]])
}
xmlMetisQueries <- top
use_data(xmlMetisQueries, overwrite=T)


# Capacity factors
data_capfactors <- data.table::fread(file=paste(getwd(),"/dataFiles/gcam/capacity_factor_by_elec_gen_subsector.csv",sep=""),skip=5,encoding="Latin-1")
data_cap_cost_tech <- data.table::fread(paste(getwd(),"/dataFiles/gcam/investCalcs/L2233.GlobalTechCapital_elecPassthru.csv", sep=""), skip=1, stringsAsFactors = FALSE)
data_cap_cost_cool <- data.table::fread(paste(getwd(),"/dataFiles/gcam/investCalcs/L2233.GlobalTechCapital_elec_cool.csv", sep=""), skip=1, stringsAsFactors = FALSE)
data_cap_cost_int_tech <- data.table::fread(paste(getwd(),"/dataFiles/gcam/investCalcs/L2233.GlobalIntTechCapital_elec.csv", sep=""), skip=1, stringsAsFactors = FALSE)
data_cap_cost_int_cool <- data.table::fread(paste(getwd(),"/dataFiles/gcam/investCalcs/L2233.GlobalIntTechCapital_elec_cool.csv", sep=""), skip=1, stringsAsFactors = FALSE)
data_A23.globaltech_retirement <- data.table::fread(paste(getwd(),"/dataFiles/gcam/investCalcs/A23.globaltech_retirement.csv",sep=""), skip=1)
data_capac_fac <- data.table::fread(paste(getwd(),"/dataFiles/gcam/investCalcs/L223.GlobalTechCapFac_elec.csv", sep=""), skip=1, stringsAsFactors = FALSE)
data_capac_fac_int <- data.table::fread(paste(getwd(),"/dataFiles/gcam/investCalcs/L223.GlobalIntTechCapFac_elec.csv", sep=""), skip=1, stringsAsFactors = FALSE)
data_tech_mapping <- data.table::fread(paste(getwd(),"/dataFiles/gcam/investCalcs/agg_tech_mapping.csv", sep=""), skip=1)
use_data(data_capfactors, overwrite=T)
use_data(data_cap_cost_tech, overwrite=T)
use_data(data_cap_cost_cool, overwrite=T)
use_data(data_cap_cost_int_tech, overwrite=T)
use_data(data_cap_cost_int_cool, overwrite=T)
use_data(data_A23.globaltech_retirement, overwrite=T)
use_data(data_capac_fac, overwrite=T)
use_data(data_capac_fac_int, overwrite=T)
use_data(data_tech_mapping, overwrite=T)


#-------------------
# Save Plots
#-------------------

# GCAM Maps (Regions, Basins, Land)
# US Maps (States, Counties)
# HydroShedMaps (HydroShed1,HydroShed2,HydroShed3)
# HUCMaps (HUC2, HUC4, HUC6)
# Grids (Grid0p5, Grid0p25)

if(F){

  library(metis);

  # Check Holes or Invalid shapes
  #------------
  # World
  rgeos::gIsValid(metis::mapCountries)
  rgeos::gIsValid(metis::mapStates)
  # GCAM
  rgeos::gIsValid(metis::mapGCAMReg32)
  rgeos::gIsValid(metis::mapGCAMBasins)
  rgeos::gIsValid(metis::mapGCAMLand)
  # US
  rgeos::gIsValid(metis::mapUS52)
  rgeos::gIsValid(metis::mapUS52County)
  rgeos::gIsValid(metis::mapUS49)
  rgeos::gIsValid(metis::mapUS49County)
  # HydroSheds
  rgeos::gIsValid(metis::mapHydroShed1)
  rgeos::gIsValid(metis::mapHydroShed2)
  rgeos::gIsValid(metis::mapHydroShed3)
  # USGS HUC
  rgeos::gIsValid(metis::mapUS52HUC2)
  rgeos::gIsValid(metis::mapUS52HUC4)
  rgeos::gIsValid(metis::mapUS49HUC2)
  rgeos::gIsValid(metis::mapUS49HUC4)

  # Plotting
  #-------------
  # World
  metis.map(dataPolygon=metis::mapCountries,fillColumn = "subRegion",labels=F,printFig=F, facetsON=F)
  metis.map(dataPolygon=metis::mapStates,fillColumn = "subRegion",labels=F,printFig=F, facetsON=F)

  # GCAM
  metis.map(dataPolygon=metis::mapGCAMReg32,fillColumn = "subRegion",labels=F,printFig=F, facetsON=F)
  metis.map(dataPolygon=metis::mapGCAMBasins,fillColumn = "subRegion",labels=F,printFig=F, facetsON=F)
  metis.map(dataPolygon=metis::mapGCAMLand,fillColumn = "subRegion",labels=F,printFig=F, facetsON=F)
  # US
  metis.map(dataPolygon=metis::mapUS52,fillColumn = "subRegion",labels=F,printFig=F, facetsON=F)
  metis.map(dataPolygon=metis::mapUS52County,fillColumn = "subRegion",labels=F,printFig=F, facetsON=F)
  metis.map(dataPolygon=metis::mapUS49,fillColumn = "subRegion",labels=F,printFig=F, facetsON=F)
  metis.map(dataPolygon=metis::mapUS49County,fillColumn = "subRegion",labels=F,printFig=F, facetsON=F)
  # HydroSheds
  metis.map(dataPolygon=metis::mapHydroShed1,fillColumn = "subRegion",labels=F,printFig=F, facetsON=F)
  metis.map(dataPolygon=metis::mapHydroShed2,fillColumn = "subRegion",labels=F,printFig=F, facetsON=F)
  metis.map(dataPolygon=metis::mapHydroShed3,fillColumn = "subRegion",labels=F,printFig=F, facetsON=F)
  # USGS HUC
  metis.map(dataPolygon=metis::mapUS52HUC2,fillColumn = "subRegion",labels=F,printFig=F, facetsON=F)
  metis.map(dataPolygon=metis::mapUS52HUC4,fillColumn = "subRegion",labels=F,printFig=F, facetsON=F)
  metis.map(dataPolygon=metis::mapUS49HUC2,fillColumn = "subRegion",labels=F,printFig=F, facetsON=F)
  metis.map(dataPolygon=metis::mapUS49HUC4,fillColumn = "subRegion",labels=F,printFig=F, facetsON=F)
  # Intersections
  metis.map(dataPolygon=metis::mapIntersectGCAMBasin32Reg,fillColumn = "subRegion",labels=F,printFig=F, facetsON=F)
  metis.map(dataPolygon=metis::mapIntersectGCAMBasinCountry,fillColumn = "subRegion",labels=F,printFig=F, facetsON=F)
  # Grids
  metis::grid025
  metis::grid050

}

