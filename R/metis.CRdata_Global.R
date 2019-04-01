
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
# Boundaries
#-----------------

GCAMBasins<-readOGR(dsn=paste(getwd(),"/dataFiles/gis/basin_GCAM",sep=""),
                    layer="Global235_CLM_final_5arcmin_multipart",use_iconv=T,encoding='UTF-8')
metis.map(GCAMBasins,fillColumn = "basin_name",facetsON = F,printFig = F)

projX<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
GCAMBasins<-spTransform(GCAMBasins,CRS(projX))


GlobalBasinBoundaries<- metis.boundaries(
  boundaryRegShape=GCAMBasins,#
  #boundaryRegShpFolder=paste(getwd(),"/dataFiles/gis/naturalEarth",sep=""),#
  #boundaryRegShpFile=paste("ne_10m_admin_0_countries",sep=""),#
  boundaryRegCol="basin_name",#
  #boundaryRegionsSelect="China",#
  subRegShape=GCAMBasins,#
  #subRegShpFolder=paste(getwd(),"/dataFiles/gis/naturalEarth",sep=""),#
  #subRegShpFile=paste("ne_10m_admin_1_states_provinces",sep=""),#
  subRegCol="basin_name",#
  #subRegionsSelect=NULL,#
  subRegType="Basin",#
  #dirOutputs=paste(getwd(),"/outputs",sep=""),#
  nameAppend="_local",#
  expandPercent=6,#
  #overlapShape=ChinaGCAMBasin,#
  #overlapShpFile="Global235_CLM_final_5arcmin_multipart",#
  #overlapShpFolder=paste(getwd(),"/dataFiles/gis/basin_gcam",sep=""),#
  labelsSize=0.7,#
  extension = T,
  cropSubShape2Bound = F
)

#------------

head(ChinaGCAMBasin@data)

#-----------
# Mapping
#-------------

#exampleGridTable<-paste(getwd(),"/dataFiles/examples/example_grid_ArgentinaBermejo3_Eg1Eg2.csv",sep="")
examplePolygonTable<-paste(getwd(),"/dataFiles/gcam/GlobalAgProd.csv",sep="")
a<-read.csv(examplePolygonTable,header=T);
head(a);
unique(a$scenario)

# Create dummy global data
# ------------------------

x<-GCAMBasins@data
#a1<-a[1:235,]
a1 <- left_join(a,x, by = c("subRegion" = "basin_name")) %>% mutate(param="param"); head(a1)

scaleRange_i=data.frame(param=c("param"),
                        maxScale=c(1),
                        minScale=c(0))

metis.mapProcess(polygonDataTables=a1,
                 #gridDataTables=exampleGridTable,
                 xRange="All",
                 boundaryRegionsSelect="Global",
                 subRegShape=GCAMBasins,
                 #subRegShpFolder=paste(getwd(),"/dataFiles/gis/admin_China",sep=""),
                 #subRegShpFile=paste("ChinaGCAMBasin",sep=""),
                 subRegCol="basin_name",
                 subRegType="subBasin",
                 nameAppend="_test",
                 legendPosition=c("RIGHT","top"),
                 scaleRange = scaleRange_i,
                 animateOn=T,
                 delay=100,
                 scenRef="SSP2_Ref"
)

# polygonDataTables=a2
# #gridDataTables=exampleGridTable
# xRange=c(2100)
# boundaryRegionsSelect="Global"
# subRegShape=GCAMBasins
# #subRegShpFolder=paste(getwd(),"/dataFiles/gis/admin_China",sep="")
# #subRegShpFile=paste("ChinaGCAMBasin",sep="")
# subRegCol="basin_name"
# subRegType="subBasin"
# nameAppend="_test"
# legendPosition=c("RIGHT","top")
# animateOn=T
# delay=100#
# #scenRef="SSP2_Ref"


# to add outside of map
# extension=T,
# boundaryRegShpFolder=paste(getwd(),"/dataFiles/gis/naturalEarth",sep=""),
# boundaryRegShpFile=paste("ne_10m_admin_0_countries",sep=""),
# boundaryRegCol="NAME",

# polygonDataTables=examplePolygonTable
# #gridDataTables=exampleGridTable
# xRange=c(2015,2030,2050)
# boundaryRegionsSelect="China"
# subRegShape=NULL
# subRegShpFolder=paste(getwd(),"/dataFiles/gis/admin_China",sep="")
# subRegShpFile=paste("ChinaGCAMBasin",sep="")
# subRegCol="basin_name"
# subRegType="subBasin"
# nameAppend="_ChrisTest"
# legendPosition=c("RIGHT","top")
# animateOn=T
# delay=100
# scenRef="SSP2_Ref"

examplePolygonTable<-paste(getwd(),"/dataFiles/examples/example_poly_China.csv",sep="")
a<-read.csv(examplePolygonTable,header=T);
a<-a%>%dplyr::mutate(valueA=value,valueB=value*3)
head(a)

colA="valueA"
colB="valueB"
a1 <-a%>%
  dplyr::mutate(!!paste("Diff_ABS_",colA,"_",colB,sep=""):=get(colA)-get(colB),
                classPalette="pal_div")
