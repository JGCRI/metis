
library(metis); library(dplyr); library(tidyr); library(magrittr)

# Example 1: Peru
countryName = "Uruguay"
exampleSubRegion=metis::mapCountries
head(exampleSubRegion@data)
subRegCol_i = "subRegion"
# Crop the shapefile to the desired subRegion
exampleSubRegionCropped<-exampleSubRegion[(exampleSubRegion$subRegion==countryName),]
head(exampleSubRegionCropped@data)
metis.map(dataPolygon=exampleSubRegionCropped,fillColumn = subRegCol_i,labels=T ,printFig=F,facetsON=F)

exampleBoundaries<- metis.boundaries(
  boundaryRegShape = metis::mapCountries,
  boundaryRegCol = "subRegion",
  boundaryRegionsSelect=countryName,
  subRegShape=exampleSubRegionCropped,
  subRegCol=subRegCol_i,
  subRegType="state",
  nameAppend="_example",
  expandPercentWidth = 2,
  expandPercentHeight = 2,
  expandPercentWidthOV = 35,
  expandPercentHeightOV = 60,
  extension = T,
  folderName="Uruguay",
  compassScale = T)
