
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
library(metis); library(dplyr)

data = metis::exampleMapDataParam %>%
        dplyr::filter(param %in% c("elecByTechTWh","agProdByCrop",
                                   "watSupRunoffBasin"))
metis.mapsProcess(polygonDataTables=data,
                  xRange=c(2010,2020,2030))

data = metis::exampleMapDataClass %>%
  dplyr::filter(param %in% c("watWithdrawBySec"))
metis.mapsProcess(polygonDataTables=data,
                  xRange=c(2010,2020,2030),
                  scenRef="SSP3", # For Diff maps
                  nameAppend="class")

