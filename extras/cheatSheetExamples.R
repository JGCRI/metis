
#---------------------------
# Example metis.readgcam
#--------------------------
library(metis)

dataGCAM<-metis.readgcam (
  #gcamdatabase = "C:/Z/projects/metisGCAMUSA/gcam-core/output/metisUSAOld",
  dataProjFile = metis::exampleGCAMproj)

df <- dataGCAM$data; head(df)
dfparam <- dataGCAM$dataAggParam; head(dfparam)
dfclass <- dataGCAM$dataAggClass1; head(dfclass)

#----------------------------
# Example metis.mapsProcess
#----------------------------
data = data.frame(
  subRegion = c("TX","AZ"),
  value = c(32,54))
metis.mapsProcess (data)


# Countries crop to Boundary
data = data.frame(subRegion = c("India","China"), value = c(32,54))
metis.mapsProcess(polygonTable = data, cropToBoundary=T)

# Countries crop to Boundary
data = data.frame(subRegion = c("La_plata","Amazon"),value = c(32,54))
metis.mapsProcess(polygonTable = data , cropToBoundary=T)

# Multi-Scenario Diff plots
data = data.frame(subRegion = c("TX","TX", "AZ", "AZ"),
                  scenario = c("scen1","scen2","scen1","scen2"),
                  value = c(32, 38, 54, 63))
metis.mapsProcess(polygonTable = data, scenRef="scen1")

# Multi-Year
data = data.frame(subRegion = c("TX","TX", "AZ", "AZ"),
                  year = c("2050","2100","2050","2100"), value = c(32, 38, 54, 63))
metis.mapsProcess(polygonTable = data, folderName="multiyear")

# Multi-Class
data = data.frame(subRegion = c("TX","TX", "AZ", "AZ"),
                  class = c("class1","class2","class1","class2"),
                  value = c(32, 38, 54, 63))
metis.mapsProcess(polygonTable = data)


# Scale Range
data = data.frame(subRegion = c("TX","TX", "AZ", "AZ"),
                  scenario = c("scen1","scen2","scen1","scen2"),
                  value = c(32, 38, 54, 63))
metis.mapsProcess(polygonTable = data,
                  scaleRange = c(30,50), scaleRangeDiffPrcnt = c(10,30))

# Colors
data = data.frame(subRegion = c("TX","TX", "AZ", "AZ"),
                  scenario = c("scen1","scen2","scen1","scen2"),
                  value = c(32, 38, 54, 63))
metis.mapsProcess(polygonTable = data, scenRef="scen1",
                  classPalette = "pal_wet", classPaletteDiff = "pal_green")

# Extended Boundary
data = data.frame(
  subRegion = c("India","China"), value = c(32,54))
metis.mapsProcess(polygonTable = data,
extension = T)

# Color Palette
metis.colors("pal_hot")  # to test color palette
metis.colors("pal_div_BrGn")  # to test color palette

# Map
library(metis);  head(mapGCAMReg32@data)
metis::metis.map(mapUS49, labels=T)



