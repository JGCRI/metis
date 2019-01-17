
#----------------------------
# Read GCAM Data
#---------------------------

gcamdatabasePath <-paste(getwd(),"/dataFiles/gcam",sep="")
gcamdatabaseName <-"database_basexdb"
gcamdataProjFile <-"Example_dataProj.proj"
regionsSelect <- c("China")
# Choose Parameters or set to "All" for all params. For complete list see ?metis.readgcam
paramsSelect=c("finalNrgbySec", "primNrgConsumByFuel", "elecByTech",
               "watConsumBySec", "watWithdrawBySec","gdp", "gdpGrowthRate", "pop",
               "agProdByCrop", "aggLandAlloc","co2emissionByEndUse")

# Use function localDBConn from package rgcam to get a list of scenarios if needed.
# localDBConn(gcamdatabasePath,gcamdatabaseName)
# dataProjLoaded <- loadProject(paste(gcamdatabasePath, "/", gcamdataProjFile, sep = ""))
#  listScenarios(dataProjLoaded)  # List of Scenarios in GCAM database
# queries <- listQueries(dataProjLoaded)  # List of Queries in queryxml


dataGCAM<-metis.readgcam(reReadData=F, # Default Value is T
                         scenOrigNames=c("SSP2_CI_Ref","SSP2_CI_AgWat"),
                         scenNewNames=c("Reference","Climate Impacts"),
                         gcamdatabasePath=gcamdatabasePath,
                         gcamdatabaseName=gcamdatabaseName,
                         queryxml="metisQueries.xml",  # Default Value is "metisQueries.xml"
                         dirOutputs= paste(getwd(),"/outputs",sep=""), # Default Value is paste(getwd(),"/outputs",sep="")
                         regionsSelect=regionsSelect, # Default Value is NULL
                         paramsSelect="All", # Default value is "All"
                         queriesSelect="All" # Default is "All"
)

dataGCAM$data # To view the data read that was read.




#------------
# India Polygons
#----------------

NE0<-readOGR(dsn=paste(getwd(),"/dataFiles/gis/naturalEarth",sep=""),
             layer="ne_10m_admin_0_countries",use_iconv=T,encoding='UTF-8')

SEAsiaNE0<-NE0[(NE0$NAME %in% c("Myanmar","Philippines", "Malaysia", "Singapore", "Thailand", "Vietnam", "Cambodia", "Laos")),]
head(SEAsiaNE0@data)
plot(SEAsiaNE0)
projX<-proj4string(SEAsiaNE0)
metis.map(dataPolygon=SEAsiaNE0,fillColumn = "NAME",printFig=F,labels = T)


SEAsiaGCAMBasin<-readOGR(dsn=paste(getwd(),"/dataFiles/gis/basin_GCAM",sep=""),
                         layer="Global235_CLM_final_5arcmin_multipart",use_iconv=T,encoding='UTF-8')
SEAsiaGCAMBasin<-spTransform(SEAsiaGCAMBasin,CRS(projX))
SEAsiaGCAMBasin<-raster::crop(SEAsiaGCAMBasin,SEAsiaNE0)
head(SEAsiaGCAMBasin@data)
plot(SEAsiaGCAMBasin)
writeOGR(obj=SEAsiaGCAMBasin, dsn=paste(getwd(),"/dataFiles/gis/admin_SEAsia",sep=""), layer=paste("SEAsiaGCAMBasin",sep=""), driver="ESRI Shapefile", overwrite_layer=TRUE)
metis.map(dataPolygon=SEAsiaGCAMBasin,fillColumn = "basin_name",printFig=F)


# Boundaries
#-----------------

SEAsiaGCAMBasin<-readOGR(dsn=paste(getwd(),"/dataFiles/gis/admin_SEAsia",sep=""),
                         layer="SEAsiaGCAMBasin",use_iconv=T,encoding='UTF-8')

GCAMBasin<-readOGR(dsn=paste(getwd(),"/dataFiles/gis/basin_GCAM",sep=""),
                   layer="Global235_CLM_final_5arcmin_multipart",use_iconv=T,encoding='UTF-8')
GCAMBasin<-spTransform(GCAMBasin,CRS(projX))

SEAsiaBasinBoundaries<- metis.boundaries(
  boundaryRegShape=NULL,#
  boundaryRegShpFolder=paste(getwd(),"/dataFiles/gis/naturalEarth",sep=""),#
  boundaryRegShpFile=paste("ne_10m_admin_0_countries",sep=""),#
  boundaryRegCol="NAME",#
  boundaryRegionsSelect="SEAsia",#
  subRegShape=SEAsiaGCAMBasin,#
  #subRegShpFolder=paste(getwd(),"/dataFiles/gis/naturalEarth",sep=""),#
  #subRegShpFile=paste("ne_10m_admin_1_states_provinces",sep=""),#
  subRegCol="basin_name",#
  #subRegionsSelect=NULL,#
  subRegType="Basin",#
  #dirOutputs=paste(getwd(),"/outputs",sep=""),#
  nameAppend="_local",#
  expandPercent=6,#
  overlapShape=SEAsiaGCAMBasin,#
  #overlapShpFile="Global235_CLM_final_5arcmin_multipart",#
  #overlapShpFolder=paste(getwd(),"/dataFiles/gis/basin_gcam",sep=""),#
  labelsSize=0.7,#
  extension = T,
  cropSubShape2Bound = F
)

#------------

head(SEAsiaGCAMBasin@data)

#-----------
# Mapping
#-------------

#exampleGridTable<-paste(getwd(),"/dataFiles/examples/example_grid_ArgentinaBermejo3_Eg1Eg2.csv",sep="")
examplePolygonTable<-paste(getwd(),"/dataFiles/gcam/regag_trdprt_SEAsia.csv",sep="")
a<-read.csv(examplePolygonTable,header=T);
head(a);
unique(a$scenario)

metis.mapProcess(polygonDataTables=examplePolygonTable,
                 #gridDataTables=exampleGridTable,
                 xRange=c(2015,2030,2050),
                 boundaryRegionsSelect="Southeast Asia",
                 subRegShape=NULL,
                 subRegShpFolder=paste(getwd(),"/dataFiles/gis/admin_SEAsia",sep=""),
                 subRegShpFile=paste("SEAsiaGCAMBasin",sep=""),
                 subRegCol="basin_name",
                 subRegType="subBasin",
                 nameAppend="_regag",
                 legendPosition=c("RIGHT","top"),
                 animateOn=T,
                 delay=100,
                 scenRef="SSP2_Ref"
)


# to add outside of map
# extension=T,
# boundaryRegShpFolder=paste(getwd(),"/dataFiles/gis/naturalEarth",sep=""),
# boundaryRegShpFile=paste("ne_10m_admin_0_countries",sep=""),
# boundaryRegCol="NAME",

# polygonDataTables=examplePolygonTable
# #gridDataTables=exampleGridTable
# xRange=c(2015,2030,2050)
# boundaryRegionsSelect="SEAsia"
# subRegShape=NULL
# subRegShpFolder=paste(getwd(),"/dataFiles/gis/admin_SEAsia",sep="")
# subRegShpFile=paste("SEAsiaGCAMBasin",sep="")
# subRegCol="basin_name"
# subRegType="subBasin"
# nameAppend="_ChrisTest"
# legendPosition=c("RIGHT","top")
# animateOn=T
# delay=100
# scenRef="SSP2_Ref"

examplePolygonTable<-paste(getwd(),"/dataFiles/examples/example_poly_SEAsia.csv",sep="")
a<-read.csv(examplePolygonTable,header=T);
a<-a%>%dplyr::mutate(valueA=value,valueB=value*3)
head(a)

colA="valueA"
colB="valueB"
a1 <-a%>%
  dplyr::mutate(!!paste("Diff_ABS_",colA,"_",colB,sep=""):=get(colA)-get(colB),
                classPalette="pal_div")
