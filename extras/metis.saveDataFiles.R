
library(tibble);library(dplyr);library(rgdal);library(devtools);library(metis); library(tmaptools)
library(rgeos); library(rgcam)

redoMaps = F

# Current Data
#data(package="metis")

#-----------------
# World Maps (Countries, States)
#-----------------

# Worldmap countries
#-------------------
if(redoMaps){
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
if(redoMaps){
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
if(redoMaps){
  examplePolyFolder<-paste(getwd(),"/dataFiles/gis/metis/gcam",sep="")
  examplePolyFile<-paste("region32_0p5deg",sep="")
  x=rgdal::readOGR(dsn=examplePolyFolder,layer=examplePolyFile,use_iconv=T,encoding='UTF-8')
  head(x@data); names(x@data)
  mapx <- x
  idMapping <- tibble::as_tibble(data.table::fread(paste(getwd(),"/dataFiles/gis/metis/gcam/GCAM_region_names.csv",sep=""),skip=4,header=T))
  mapx@data <- mapx@data %>%
    dplyr::mutate(reg32_id=as.character(reg32_id))%>%
    left_join(idMapping%>%dplyr::mutate(reg32_id=as.character(GCAM_region_ID)),by="reg32_id")%>%
    dplyr::mutate(subRegionType="GCAMReg32",
                  source="https://confluence.pnnl.gov/confluence/display/JGCRI/GCAM+Shape+Files",
                  subRegion=region,
                  region = "World",
                  subRegionAlt=reg32_id,
                  region=gsub("-","_",region),
                  subRegion=gsub("-","_",subRegion))%>%
    dplyr::select(-ID,-GRIDCODE,-GCAM_region_ID,-reg32_id)
  mapx@data <- droplevels(mapx@data)
  # Add Taiwan
  # a <- metis::mapCountries
  # a <- a[a$subRegion %in% "Taiwan",];
  # a@data <- droplevels(a@data)
  # b <- mapx
  # a <- sp::spTransform(a,raster::crs(b))
  # mapx1 <- raster::union(a,b)
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
if(redoMaps){

  # Get list of basins from a GCAM database
  # Compare with existing shape file
  # Mapping file is available in metis.mappings.R

  # Changes to make to shapefile basins (in metis.saveDataFiles)
  #   - Remove "_Basin"
  #   - Fix "R(hne" to "Rohne"
  #   - Fix "Yucat_µ„ön_Peninsula"
  #   - Replace all "-" with "_"
  #   - "HamuniMashkel"~"Hamun-i-Mashkel"
  # Changes to make to GCAMdata in metis.readgcam()
  #   - Replace all "-" with "_"

  # dataGCAM<-metis.readgcam (#gcamdatabase = "C:/Z/gcam-v5.2-Windows-Release-Package/output/example",
  #                          dataProjFile = "C:/Z/projects/metis/dataFiles/examples/example.proj")
  #
  # df <- dataGCAM$data
  # head(df); unique(df$subRegion); unique(df$param)
  # df%>%filter(subRegion=="Cauvery")%>%as.data.frame()%>%head()
  # gcamBasins <- as.character(unique((df%>%dplyr::filter(param=="watSupRunoffBasin"))$subRegion)); gcamBasins
  #
  # examplePolyFolder<-paste(getwd(),"/dataFiles/gis/metis/gcam",sep="")
  # examplePolyFile<-paste("Global235_CLM_final_5arcmin_multipart",sep="")
  # x=rgdal::readOGR(dsn=examplePolyFolder,layer=examplePolyFile,use_iconv=T,encoding='UTF-8')
  # head(x@data); names(x@data)
  #
  # gcamMapBasins <- as.character(unique(x@data$basin_name)); gcamMapBasins
  # basins2Change <- gcamBasins[!gcamBasins %in% gcamMapBasins]%>%sort(); basins2Change
  # data.table::fwrite(as.data.frame(gcamBasins)%>%arrange(gcamBasins),"gcamBasins.csv")
  # data.table::fwrite(as.data.frame(gcamMapBasins)%>%arrange(gcamMapBasins),"gcamMapBasins.csv")
  # data.table::fwrite(as.data.frame(basins2Change)%>%arrange(basins2Change),"basins2Change.csv")

  mapx <- x
  mapx@data <- mapx@data %>%
    dplyr::select(subRegion=basin_name, subRegionAlt=basin_id) %>%
    dplyr::mutate(region = "World",
                  region=gsub("-","_",region),
                  subRegion=gsub("-","_",subRegion),
                  subRegionType="GCAMBasin",
                  subRegion=gsub("_Basin","",subRegion),
                  subRegion=gsub("Rfo","Rio",subRegion),
                  subRegion=gsub("-","_",subRegion),
                  subRegion=case_when(subRegion=="HamuniMashkel"~"Hamun_i_Mashkel",
                                      subRegion=="Yucat_µ„ön_Peninsula"~"Yucatan_Peninsula",
                                      subRegion=="Rh(ne"~"Rhone",
                                      subRegion=="Hong_(Red_River)"~"Hong_Red_River",
                                      TRUE~subRegion))

  # gcamMapBasinsNew <- as.character(unique(mapx@data$subRegion)); gcamMapBasinsNew
  # basins2Change1 <- gcamBasins[!gcamBasins %in% gcamMapBasinsNew]%>%sort(); basins2Change1


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
if(redoMaps){
  examplePolyFolder<-paste(getwd(),"/dataFiles/gis/metis/gcam",sep="")
  examplePolyFile<-paste("region32glu_moirai_out_vect",sep="")
  x=rgdal::readOGR(dsn=examplePolyFolder,layer=examplePolyFile,use_iconv=T,encoding='UTF-8')
  head(x@data); names(x@data)
  mapx <- x
  idMapping <- data.table::fread(paste(getwd(),"/dataFiles/gis/metis/gcam/GCAM_region_names.csv",sep=""),skip=4,header=T)
  mapx@data <- mapx@data %>%
    dplyr::select(subRegion=glu_name, subRegionAlt=Rg32Glu_id, glu_id, reg32_id) %>%
    dplyr::mutate(subRegionType="GCAMRegLand",
                  source="https://confluence.pnnl.gov/confluence/display/JGCRI/GCAM+Shape+Files",
                  region = "World",
                  region=gsub("-","_",region),
                  subRegion=gsub("-","_",subRegion),
                  subRegion=gsub("_Basin","",subRegion),
                  subRegion=gsub("Rfo","Rio",subRegion),
                  subRegion=gsub("-","_",subRegion),
                  subRegion=case_when(subRegion=="HamuniMashkel"~"Hamun_i_Mashkel",
                                      subRegion=="Yucat_µ„ön_Peninsula"~"Yucatan_Peninsula",
                                      subRegion=="Rh(ne"~"Rhone",
                                      subRegion=="Hong_(Red_River)"~"Hong_Red_River",
                                      TRUE~subRegion))%>%
    dplyr::left_join(idMapping%>%
                       dplyr::select(reg32_name=region, reg32_id=GCAM_region_ID)%>%
                       unique())
  head(mapx@data)
  format(object.size(mapx), units="Mb")
  #mapx<-as(simplify_shape(mapx, fact = 0.05),Class="Spatial")
  #format(object.size(mapx), units="Mb")
  sp::plot(mapx)
  metis.map(dataPolygon=mapx,fillColumn = "subRegion",labels=F,printFig=F, facetsON=F, fileName="factp1")
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
if(redoMaps){
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
if(redoMaps){
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
if(redoMaps){
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
# if(redoMaps){
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
if(redoMaps){
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
  m1 <- mapx
  m2 <- metis::mapUS52
  m2 <- sp::spTransform(m2,raster::crs(m1))
  mapx<-raster::crop(m1,m2)
  mapx@data <- mapx@data%>%droplevels()
  sp::plot(mapx)
  metis.map(dataPolygon=mapx,fillColumn = "subRegion",labels=T,printFig=F, facetsON=F)
  mapx<-rgeos::gBuffer(mapx, byid=TRUE, width=0)
  format(object.size(mapx), units="Mb")
  mapUS52HUC2 <- mapx
  use_data(mapUS52HUC2, overwrite=T)
}

# US49 HUC 2
#-------------------
if(redoMaps){
  mapx <- mapUS52HUC2
  m1 <- mapx
  m2 <- metis::mapUS49
  m2 <- sp::spTransform(m2,raster::crs(m1))
  mapx<-raster::crop(m1,m2)
  mapx@data <- mapx@data%>%droplevels()
  head(mapx@data); mapx@data%>%distinct(subRegionAlt)%>%arrange(subRegionAlt)
  format(object.size(mapx), units="Mb")
  # mapx<-as(simplify_shape(mapx, fact = 0.01),Class="Spatial")
  # format(object.size(mapx), units="Mb")
  sp::plot(mapx)
  metis.map(dataPolygon=mapx,fillColumn = "subRegion",labels=T,printFig=F, facetsON=F, fileName = "HUC2")
  mapx<-rgeos::gBuffer(mapx, byid=TRUE, width=0)
  format(object.size(mapx), units="Mb")
  mapUS49HUC2 <- mapx
  use_data(mapUS49HUC2, overwrite=T)
}

# US52 HUC 4
#-------------------
if(redoMaps){
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
  mapx<-rgeos::gBuffer(mapx, byid=TRUE, width=0)
  format(object.size(mapx), units="Mb")
  mapUS52HUC4 <- mapx
  m1 <- mapx
  m2 <- metis::mapUS52
  m2 <- sp::spTransform(m2,raster::crs(m1))
  mapx<-raster::crop(m1,m2)
  mapx@data <- mapx@data%>%droplevels()
  sp::plot(mapx)
  metis.map(dataPolygon=mapx,fillColumn = "subRegion",labels=F,printFig=F, facetsON=F)
  use_data(mapUS52HUC4, overwrite=T)
}

# US49 HUC 4
#-------------------
if(redoMaps){
  mapx <- mapUS52HUC4
  m1 <- mapx
  m2 <- metis::mapUS49
  m2 <- sp::spTransform(m2,raster::crs(m1))
  mapx<-raster::crop(m1,m2)
  mapx@data <- mapx@data%>%droplevels()
  head(mapx@data); mapx@data%>%distinct(subRegionAlt)%>%arrange(subRegionAlt)
  format(object.size(mapx), units="Mb")
  #mapx<-as(simplify_shape(mapx, fact = 0.01),Class="Spatial")
  #format(object.size(mapx), units="Mb")
  mapx<-rgeos::gBuffer(mapx, byid=TRUE, width=0)
  format(object.size(mapx), units="Mb")
  mapUS49HUC4 <- mapx
  sp::plot(mapx)
  metis.map(dataPolygon=mapx,fillColumn = "subRegion",labels=T,printFig=F, facetsON=F, fileName = "HUC4")
  use_data(mapUS49HUC4, overwrite=T)
}


#-----------------
# US Maps ( 52 State, 49 State, Counties, Regions, Grid Regions)
#-----------------

# US 52 (including Alaska, Hawaii and Puerto Rico)
#-------------------
if(redoMaps){
  examplePolyFolder<-paste(getwd(),"/dataFiles/gis/metis/USA/cb_2018_us_state_20m",sep="")
  examplePolyFile<-paste("cb_2018_us_state_20m",sep="")
  x=rgdal::readOGR(dsn=examplePolyFolder,layer=examplePolyFile,use_iconv=T,encoding='UTF-8')
  head(x@data); names(x@data); nrow(x)
  mapx <- x
  mapx@data <- mapx@data %>%
    dplyr::select(subRegion=STUSPS,subRegionAlt=NAME, STATEFP) %>%
    dplyr::mutate(region="USA",subRegionType="state", source="https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html")
  head(mapx@data); unique(mapx$subRegion)
  mapx<-rgeos::gBuffer(mapx, byid=TRUE, width=0)
  format(object.size(mapx), units="Mb")
  mapUS52 <- mapx
  sp::plot(mapx)
  metis.map(dataPolygon=mapx,fillColumn = "subRegion",labels=F,printFig=F, facetsON=F)
  use_data(mapUS52, overwrite=T)
}

# US 49 (Excluding Alsaka, Hawaii and Puerto Rico)
#-------------------
if(redoMaps){
  mapx <- mapUS52[(mapUS52$region=="USA" & !mapUS52$subRegion %in% c("AK","HI","PR")),]
  mapx@data <- mapx@data%>%droplevels()
  head(mapx@data); nrow(mapx); mapx@data%>%distinct(subRegion)
  mapUS49<-mapx
  sp::plot(mapx)
  metis.map(dataPolygon=mapx,fillColumn = "subRegion",labels=F,printFig=F, facetsON=F)
  use_data(mapUS49, overwrite=T)
}


# US 52 Counties
#-------------------
if(redoMaps){
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
  mapx<-rgeos::gBuffer(mapx, byid=TRUE, width=0)
  format(object.size(mapx), units="Mb")
  mapUS52County <- mapx
  sp::plot(mapx)
  metis.map(dataPolygon=mapx,fillColumn = "subRegion",labels=F,printFig=F, facetsON=F)
  use_data(mapUS52County, overwrite=T)
}



# US 49 Counties
#-------------------
if(redoMaps){
  mapx <- mapUS52County[(!mapUS52County$STATECODE %in% c("AK","HI","PR")),]
  mapx@data <- mapx@data%>%droplevels()
  head(mapx@data); nrow(mapx); mapx@data%>%distinct(subRegion)
  mapx<-rgeos::gBuffer(mapx, byid=TRUE, width=0)
  format(object.size(mapx), units="Mb")
  mapUS49County<-mapx
  sp::plot(mapx)
  metis.map(dataPolygon=mapx,fillColumn = "subRegion",labels=F,printFig=F, facetsON=F)
  use_data(mapUS49County, overwrite=T)
}


# Merge
#-------------------

# Merge US52 with GCAM Regs
if(redoMaps){
  m1 <- mapGCAMReg32
  m2 <- mapUS52
  m2 <- sp::spTransform(m2,raster::crs(m1))
  mapx<-raster::union(m1,m2)
  mapx@data <- mapx@data %>%
    dplyr::mutate(region="World",
                  subRegionType="GCAMReg32US52",
                  subRegion.2=as.character(subRegion.2),
                  subRegion.1=as.character(subRegion.1),
                  subRegionAlt.1=as.numeric(subRegionAlt.1),
                  subRegionAlt.2=as.numeric(subRegionAlt.2),
                  subRegion=case_when(subRegion.2 %in% metis::metis.assumptions()$US52~subRegion.2,
                                      TRUE~subRegion.1),
                  subRegionAlt=case_when(subRegion.2 %in% metis::metis.assumptions()$US52~1,
                                         TRUE~subRegionAlt.1),
                  subRegionAlt=as.integer(subRegionAlt)) %>%
    dplyr::select(region,subRegion,subRegionType)
  head(mapx@data)
  mapx<-rgeos::gBuffer(mapx, byid=TRUE, width=0)
  format(object.size(mapx), units="Mb")
  mapGCAMReg32US52<-mapx
  sp::plot(mapx)
  metis.map(dataPolygon=mapx,fillColumn = "subRegion",labels=F,printFig=F, facetsON=F)
  use_data(mapGCAMReg32US52, overwrite=T)
}


# Merge US52 with Countries file
if(redoMaps){
  m1 <- mapCountries
  m2 <- mapUS52
  m2 <- sp::spTransform(m2,raster::crs(m1))
  mapx<-raster::union(m1,m2)
  mapx@data <- mapx@data %>%
    dplyr::mutate(region="World",
                  subRegionType="CountriesUS52",
                  subRegion.2=as.character(subRegion.2),
                  subRegion.1=as.character(subRegion.1),
                  subRegionAlt.1=as.numeric(subRegionAlt.1),
                  subRegionAlt.2=as.numeric(subRegionAlt.2),
                  subRegion=case_when(subRegion.2 %in% metis::metis.assumptions()$US52~subRegion.2,
                                      TRUE~subRegion.1),
                  subRegionAlt=case_when(subRegion.2 %in% metis::metis.assumptions()$US52~1,
                                         TRUE~subRegionAlt.1),
                  subRegionAlt=as.integer(subRegionAlt)) %>%
    dplyr::select(region,subRegion,subRegionType)
  head(mapx@data)
  mapx<-rgeos::gBuffer(mapx, byid=TRUE, width=0)
  format(object.size(mapx), units="Mb")
  mapCountriesUS52<-mapx
  sp::plot(mapx)
  metis.map(dataPolygon=mapx,fillColumn = "subRegion",labels=F,printFig=F, facetsON=F)
  use_data(mapCountriesUS52, overwrite=T)
}


# Intersections
#-------------------

# Intersection of GCAM Basins and Countries
if(redoMaps){
  m1 <- mapGCAMBasins
  m2 <- mapCountries
  m2 <- sp::spTransform(m2,raster::crs(m1))
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
  mapx<-rgeos::gBuffer(mapx, byid=TRUE, width=0)
  format(object.size(mapx), units="Mb")
  mapIntersectGCAMBasinCountry<-mapx
  sp::plot(mapx)
  metis.map(dataPolygon=mapx,fillColumn = "subRegion",labels=F,printFig=F, facetsON=F)
  use_data(mapIntersectGCAMBasinCountry, overwrite=T)
}

# Intersection of GCAM Basins and 32 GCAM Regions
if(redoMaps){
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
  mapx<-rgeos::gBuffer(mapx, byid=TRUE, width=0)
  format(object.size(mapx), units="Mb")
  mapIntersectGCAMBasin32Reg<-mapx
  sp::plot(mapx)
  metis.map(dataPolygon=mapx,fillColumn = "subRegion",labels=F,printFig=F, facetsON=F)
  use_data(mapIntersectGCAMBasin32Reg, overwrite=T)
}


# Cropped Files
#------------------------------

# Cropped GCAM Basins and US 52
if(redoMaps){
  m1 <- metis::mapGCAMBasins
  m2 <- metis::mapUS52
  m2 <- sp::spTransform(m2,raster::crs(m1))
  mapx<-raster::crop(m1,m2)
  mapx<-rgeos::gBuffer(mapx, byid=TRUE, width=0)
  format(object.size(mapx), units="Mb")
  mapGCAMBasinsUS52<-mapx
  sp::plot(mapx)
  metis.map(dataPolygon=mapx,fillColumn = "subRegion",labels=F,printFig=F, facetsON=F)
  use_data(mapGCAMBasinsUS52, overwrite=T)
}

# Cropped GCAM Basins and US 49 States
if(redoMaps){
  m1 <- metis::mapGCAMBasins
  m2 <- metis::mapUS49
  m2 <- sp::spTransform(m2,raster::crs(m1))
  mapx<-raster::crop(m1,m2)
  head(mapx@data)
  mapx<-rgeos::gBuffer(mapx, byid=TRUE, width=0)
  format(object.size(mapx), units="Mb")
  mapGCAMBasinsUS49<-mapx
  sp::plot(mapx)
  metis.map(dataPolygon=mapx,fillColumn = "subRegion",labels=F,printFig=F, facetsON=F)
  use_data(mapGCAMBasinsUS49, overwrite=T)
}


# Cropped GCAM Land and US 52
if(redoMaps){
  m1 <- metis::mapGCAMLand
  m2 <- metis::mapUS52
  m2 <- sp::spTransform(m2,raster::crs(m1))
  mapx<-raster::crop(m1,m2)
  mapx<-rgeos::gBuffer(mapx, byid=TRUE, width=0)
  format(object.size(mapx), units="Mb")
  mapGCAMLandUS52<-mapx
  sp::plot(mapx)
  metis.map(dataPolygon=mapx,fillColumn = "subRegion",labels=F,printFig=F, facetsON=F)
  use_data(mapGCAMLandUS52, overwrite=T)
}

# Cropped GCAM Land and US 49 States
if(redoMaps){
  m1 <- metis::mapGCAMLand
  m2 <- metis::mapUS49
  m2 <- sp::spTransform(m2,raster::crs(m1))
  mapx<-raster::crop(m1,m2)
  head(mapx@data)
  mapx<-rgeos::gBuffer(mapx, byid=TRUE, width=0)
  format(object.size(mapx), units="Mb")
  mapGCAMLandUS49<-mapx
  sp::plot(mapx)
  metis.map(dataPolygon=mapx,fillColumn = "subRegion",labels=F,printFig=F, facetsON=F)
  use_data(mapGCAMLandUS49, overwrite=T)
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
# Example Data
#-------------------

# Example .proj file
#projFile <-"C:/Z/projects/metisGCAMUSA/metisOutputs/readGCAM/exampleGCAMproj.proj"
metis.readgcam(gcamdatabase = "C:/Z/projects/metis/dataFiles/gcam/example_GCAM52release_SSP3SSP5",
               paramsSelect =c("elecByTechTWh", "pop","watWithdrawBySec","watSupRunoffBasin",
                                   "landAlloc","agProdByCrop"),
               dataProjFile = "exampleGCAM52releaseSSP3SSP5.proj",saveData = F,reReadData = T)
projFile <-"C:/Z/projects/metis/dataFiles/examples/exampleGCAM52releaseSSP3SSP5.proj"
exampleGCAMproj <- rgcam::loadProject(projFile)
use_data(exampleGCAMproj, overwrite=T)

# Examples .metisMapProcess
dataGCAM<-metis.readgcam (dataProjFile = exampleGCAMproj,
                          paramsSelect =c("elecByTechTWh", "pop","watWithdrawBySec","watSupRunoffBasin",
                                          "landAlloc","agProdByCrop"),saveData = F)
df <- dataGCAM$data;unique(df$scenario); unique(df$param);
exampleMapDataParam <- dataGCAM$dataAggParam
exampleMapDataClass <- dataGCAM$dataAggClass1
use_data(exampleMapDataParam, overwrite=T)
use_data(exampleMapDataClass, overwrite=T)

#-------------------
# Data
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
  # Merge
  metis.map(dataPolygon=metis::mapGCAMReg32US52,fillColumn = "subRegion",labels=F,printFig=F, facetsON=F)
  # Intersections
  metis.map(dataPolygon=metis::mapIntersectGCAMBasin32Reg,fillColumn = "subRegion",labels=F,printFig=F, facetsON=F)
  metis.map(dataPolygon=metis::mapIntersectGCAMBasinCountry,fillColumn = "subRegion",labels=F,printFig=F, facetsON=F)
  # Grids
  metis::grid025
  metis::grid050

}

