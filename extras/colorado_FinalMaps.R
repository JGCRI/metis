
#----------------------------
# Install necessary packages
#----------------------------
if("devtools" %in% rownames(installed.packages()) == F){install.packages("devtools")}
library(devtools)
if("rgcam" %in% rownames(installed.packages()) == F){install_github(repo="JGCRI/rgcam")}
library(rgcam)
if("tibble" %in% rownames(installed.packages()) == F){install.packages("tibble")}
library(tibble)
if("dplyr" %in% rownames(installed.packages()) == F){install.packages("dlpyr")}
library(dplyr)
if("rgdal" %in% rownames(installed.packages()) == F){install.packages("rgdal")}
library(rgdal)
if("tmap" %in% rownames(installed.packages()) == F){install.packages("tmap")}
library(tmap)
if("rgeos" %in% rownames(installed.packages()) == F){install.packages("rgeos")}
library(rgeos)
if("tools" %in% rownames(installed.packages()) == F){install.packages("tools")}
library(tools)
if("metis" %in% rownames(installed.packages()) == F){install.packages("metis")}
library(metis)

setwd('C:/Users/twild/all_git_repositories/metis/metis')
countryName="Argentina"


# Plot 10 subregions in Colorado
localBasinShapeFileFolder = paste(getwd(),"/dataFiles/gis/shapefiles_Argentina",sep="")
localBasinShapeFile = "colorado_10_subregions"
countryLocalBasin <-readOGR(dsn=localBasinShapeFileFolder,
                            layer=localBasinShapeFile,use_iconv=T,encoding='UTF-8')
localBasinsShapeFileColName = "cuenca" # Will need to load the file to see which name this would be
countryLocalBasin<-countryLocalBasin[(!countryLocalBasin$cuenca %in%
                                        c("media","baja","RioGrande","Barrancas")) & !is.na(countryLocalBasin$cuenca),]
countryLocalBasin@data <- droplevels(countryLocalBasin@data)
head(countryLocalBasin@data)
plot(countryLocalBasin)
writeOGR(obj=countryLocalBasin, dsn=paste(getwd(),"/dataFiles/gis/shapefiles_",countryName,sep=""), layer=paste(countryName,"LocalBasin",sep=""), driver="ESRI Shapefile", overwrite_layer=TRUE)
metis.map(dataPolygon=countryLocalBasin,fillColumn = localBasinsShapeFileColName,printFig=F, facetsON = F, labels=F, folderName = NULL)




subRegShpFolder_i = paste(getwd(),"/dataFiles/gis/shapefiles_",countryName,sep = "")
subRegShpFile_i = paste("colorado_10_subregions",sep= "") # localBasinShapeFile # paste("colombiaLocalBasin",sep= "")
subRegCol_i = localBasinsShapeFileColName  #
subRegType_i = "subBasin"
nameAppend_i = "_local"
ScenarioNames <- c('Reference', 'Policy')
paramNames <- c('total_ag_supply', 'livestock_water_demand', 'available_water', 'total_elec_supply',
                'electricity_water_demand', 'irrigation_water_demand', 'municipal_water_demand',
                'population', 'runoff', 'total_water_demand', 'griddedScarcity', 'total_livestock_supply',
                'Ag_cereals_supply', 'Ag_fruittrees_supply', 'Ag_pasture_supply', 'Ag_specialty_supply',
                'Ag_vegetables_supply', 'population', 'ElecDemand_commercial', 'ElecDemand_export',
                'ElecDemand_industrial', 'ElecDemand_other', 'ElecDemand_residential', 'ElecDemand_Total')

for(scen in ScenarioNames){
  for(param in paramNames){
    scenario_name <- scen
    polygonDataTables_i=paste(getwd(),"/outputs/Maps/Tables/", param, '_', scenario_name, ".csv",sep="")
    a1<-read.csv(polygonDataTables_i); head(a1); unique(a1$scenario); unique(a1$param); unique(a1$x)
    xRange_i= seq(from=2000,to=2050,by=5)
    legendPosition_i=c("LEFT","bottom")
    legendOutsideSingle_i=T
    animateOn_i=F
    delay_i=100
    scenRef_i="gfdl-esm2m_rcp2p6_NA_NA"
    paramsSelect_i = c("All")
    indvScenarios_i = "All"
    GCMRCPSSPPol_i=F
#    scaleRange_i=data.frame(param=param,
#                            maxScale=c(2000),
#                            minScale=c(0))
    scaleRange_i = NULL
    numeric2Cat_param <- list(param)
    numeric2Cat_legendTextSize <- list(c(0.7))
    numeric2Cat_list <-list(numeric2Cat_param=numeric2Cat_param,
                            numeric2Cat_legendTextSize=numeric2Cat_legendTextSize)
    if(param=='griddedScarcity'){
      numeric2Cat_breaks <- list(c(-Inf, 0.1, 0.2, 0.4, Inf))
      numeric2Cat_labels <- list(c("None (0<WSI<0.1)","Low (0.1<WSI<0.2)","Moderate (0.2<WSI<0.4)","Severe (WSI>0.4)"))
      numeric2Cat_palette <- list(c("pal_ScarcityCat")) # Can be a custom scale or an R brewer paletter or a metis.pal
      numeric2Cat_list[['numeric2Cat_labels']] <- numeric2Cat_labels
      numeric2Cat_list[['numeric2Cat_breaks']] <- numeric2Cat_breaks
      numeric2Cat_list[['numeric2Cat_palette']] <- numeric2Cat_palette
    }else if(param %in% c('Ag_cereals_supply', 'Ag_fruittrees_supply', 'Ag_pasture_supply', 'Ag_specialty_supply',
                          'Ag_vegetables_supply')){
        numeric2Cat_breaks <- list(c(-Inf, 5, 10, 25, 50, 100, 300, 500, 1000))
        numeric2Cat_labels <- list(c("0-5", "5-10", "10-25","25-50","50-100","100-300", "300-500", "500-1000"))
        numeric2Cat_palette <- list(c("pal_ColoradoLandAlloc")) # Can be a custom scale or an R brewer paletter or a metis.pal
        numeric2Cat_list[['numeric2Cat_labels']] <- numeric2Cat_labels
        numeric2Cat_list[['numeric2Cat_breaks']] <- numeric2Cat_breaks
        numeric2Cat_list[['numeric2Cat_palette']] <- numeric2Cat_palette

       #scaleRange_i=data.frame(param=param,
       #                       maxScale=c(1000),
       #                       minScale=c(0))
      #numeric2Cat_list[['numeric2Cat_breaks']] <- numeric2Cat_breaks
    }else if(param %in% c('ElecDemand_commercial', 'ElecDemand_export', 'ElecDemand_industrial', 'ElecDemand_other',
                          'ElecDemand_residential')){
      numeric2Cat_breaks <- list(c(-Inf, 5, 15, 25, 50, 100, 200, 400, 600))
      numeric2Cat_labels <- list(c("0-5", "5-15", "15-25","25-50","50-100","100-200", "200-400", "400-600"))
      numeric2Cat_palette <- list(c("pal_ColoradoElecDemand")) # Can be a custom scale or an R brewer paletter or a metis.pal
      numeric2Cat_list[['numeric2Cat_labels']] <- numeric2Cat_labels
      numeric2Cat_list[['numeric2Cat_breaks']] <- numeric2Cat_breaks
      numeric2Cat_list[['numeric2Cat_palette']] <- numeric2Cat_palette
    }else if(param %in% c('ElecDemand_Total', 'total_elec_supply')){
      numeric2Cat_breaks <- list(c(-Inf, 0.5, 1.5, 79.9, 429.1, 700.51, 1000, 1200))
      numeric2Cat_labels <- list(c("0-0.5", "0.5-1.5", "1.5-79.9",'79.9-429.1','429.1-700.51', '700.51-1000', '1000-1200'))
      numeric2Cat_palette <- list(c("pal_ColoradoTotalElecDemand")) # Can be a custom scale or an R brewer paletter or a metis.pal
      numeric2Cat_list[['numeric2Cat_labels']] <- numeric2Cat_labels
      numeric2Cat_list[['numeric2Cat_breaks']] <- numeric2Cat_breaks
      numeric2Cat_list[['numeric2Cat_palette']] <- numeric2Cat_palette
    }else if(param %in% c('livestock_water_demand', 'available_water', 'electricity_water_demand',
                          'irrigation_water_demand', 'municipal_water_demand', 'runoff', 'total_water_demand')){
      numeric2Cat_breaks <- list(c(-Inf, 0.004, 0.07, 0.25, 1, 2, 3, 4, 4.61))
      numeric2Cat_labels <- list(c("0-0.004", "0.004-0.07", "0.07-0.25",'0.25-1','1-2', '2-3', '3-4', '3-4.61'))
      numeric2Cat_palette <- list(c("pal_ColoradoWater")) # Can be a custom scale or an R brewer paletter or a metis.pal
      numeric2Cat_list[['numeric2Cat_labels']] <- numeric2Cat_labels
      numeric2Cat_list[['numeric2Cat_breaks']] <- numeric2Cat_breaks
      numeric2Cat_list[['numeric2Cat_palette']] <- numeric2Cat_palette
    }

    list_index <- which(numeric2Cat_list$numeric2Cat_param==param)
    catBreaks <- numeric2Cat_list$numeric2Cat_breaks[[list_index]]; catBreaks
    #catLabels <- numeric2Cat_list$numeric2Cat_labels[[list_index]]; catLabels
    catPalette <- numeric2Cat_list$numeric2Cat_palette[[list_index]]; catPalette
    catLegendTextSize <- numeric2Cat_list$numeric2Cat_legendTextSize[[list_index]];catLegendTextSize

    boundaryRegShape_i = NULL
    boundaryRegShpFolder_i=paste(getwd(),"/dataFiles/gis/naturalEarth",sep="")
    boundaryRegShpFile_i=paste("ne_10m_admin_0_countries",sep="")
    boundaryRegCol_i="NAME"
    boundaryRegionsSelect_i=countryName

    # xRange=xRange_i,

    metis.mapsProcess(polygonDataTables=polygonDataTables_i,
                      #gridDataTables=gridDataTables_i,
                      # boundaryRegShape=boundaryRegShape_i,
                      # boundaryRegShpFolder=boundaryRegShpFolder_i,
                      # boundaryRegShpFile=boundaryRegShpFile_i,
                      # boundaryRegCol=boundaryRegCol_i,
                      boundaryRegionsSelect=boundaryRegionsSelect_i,
                      #subRegShape=subRegShape_i,
                      subRegShpFolder=subRegShpFolder_i,
                      subRegShpFile=subRegShpFile_i,
                      subRegCol=subRegCol_i,
                      #subRegType=subRegType_i,
                      nameAppend=nameAppend_i,
                      legendOutsideSingle=legendOutsideSingle_i,
                      legendPosition=legendPosition_i,
                      animateOn=animateOn_i,
                      #delay=delay_i,
                      scenRef=scenRef_i,
                      extension=T,
                      expandPercent = 3,
                      figWidth=6,
                      figHeight=7,
                      paramsSelect = paramsSelect_i,
                      scaleRange = scaleRange_i,
                      indvScenarios=indvScenarios_i,
                      GCMRCPSSPPol=GCMRCPSSPPol_i,
                      multiFacetCols="scenarioRCP",
                      multiFacetRows="scenarioGCM",
                      legendOutsideMulti=T,
                      legendPositionMulti=NULL,
                      legendTitleSizeMulti=NULL,
                      legendTextSizeAnim=NULL,
                      legendTextSizeMulti=NULL,
                      refGCM="gfdl-esm2m",
                      refRCP="rcp2p6",
                      chosenRefMeanYears=c(2000:2050),
                      numeric2Cat_list=numeric2Cat_list,
                      frameShow = F,
                      folderName="ColoradoFinalMaps",
                      pdfpng = 'pdf')
  }
}
