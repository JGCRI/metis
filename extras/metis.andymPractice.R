#' metis.prepGrid
#'
#' This function prepares gridded data for use with other metis modules.
#'
#' @param biaFolder andym Bia Folder Path
#' @param biaFiles andym Bia Files to Read
#' @param biaScenarioAssign andym Default "NA". Scenario name if testing a single scenario.
#' @param zelusFolder andym Full path to zelus outputs
#' @param zelusFiles andym Default =c(?_?'edtrnsp','edbld','edindus'?_?)
#' @param zelusScenario andym Scenario name for zelus run
#' @param zelusUnits andym No Default
#' @param popFolder Default = <-paste(getwd(),"/dataFiles/grids/griddedIDsPop/",sep="")
#' @param popFiles Default = <-"grid_pop_map"
#' @param popUnits Default = <-"person"
#' @param dirOutputs Default =paste(getwd(),"/outputs",sep=""),
#' @param reReadData Default =1,
#' @param gridMetisData Default = paste(dirOutputs, "/Grids/gridMetis.RData", sep = "")
#' @param sqliteUSE Default = T,
#' @param sqliteDBNamePath Default = paste(getwd(),"/outputs/Grids/gridMetis.sqlite", sep = "")
#' @return A table with data by polygon ID for each shapefile provided
#' @keywords gcam, gcam database, query
#' @export

metis.andymPractice<- function(biaFolder="NA",
                        biaFiles="NA",
                        biaScenarioAssign="NA",
                        zelusFolder="NA",
                        zelusScenario="NA",
                        zelusUnits="NA",
                        zelusFiles="NA",
                        popFolder="NA",
                        popFiles="NA",
                        popUnits="NA",
                        dirOutputs=paste(getwd(),"/outputs",sep=""),
                        reReadData=1,
                        gridMetisData=paste(getwd(),"/outputs/Grids/gridMetis.RData", sep = ""),
                        sqliteUSE = F,
                        sqliteDBNamePath = paste(getwd(),"/outputs/Grids/gridMetis.sqlite", sep = "")
                        ){



#----------------
# Initialize variables by setting to NULL
#----------------

NULL -> lat -> lon -> latitude -> longitude -> aez_id -> region_id ->X..ID->
  ilon->ilat->param->V2->V3->scenario->classPalette->rollingMean->x->scarcity->value->id->
    biaScenarios->biaYears->zelusScenarios->zelusYears->commonYears->commonScenarios->V1->
    Area_hec->Area_km2->lowess->valueBia->valueZelus->commonYears_i


#------------------
# Create Folders if needed
#------------------
if (!dir.exists(dirOutputs)){
    dir.create(dirOutputs)}
if (!dir.exists(paste(dirOutputs, "/Grids", sep = ""))){
    dir.create(paste(dirOutputs, "/Grids", sep = ""))}

  if (!dir.exists(paste(dirOutputs, "/Grids/diagnostics",sep=""))){
    dir.create(paste(dirOutputs, "/Grids/diagnostics",sep=""))}


#------------------
# If reread data
#------------------

if(reReadData==1){

gridMetis<-tibble::tibble()

if(sqliteUSE==T){
if(file.exists(sqliteDBNamePath)){file.remove(sqliteDBNamePath)}
  dbConn<-DBI::dbConnect(RSQLite::SQLite(), sqliteDBNamePath)
DBI::dbDisconnect(dbConn);dbConn
}

#library(RMySQL)
#dbListConnections( dbDriver( drv = "MySQL"))



#----------------
# Prepare Zelus Files
#---------------

# if(!dir.exists(zelusFolder)){
#   print(paste("zelus folder: ", zelusFolder ," is incorrect or doesn't exist.",sep=""))
#   print(paste("Skipping zelus runs",sep=""))}else {
#
#     if(sqliteUSE==T){dbConn <- DBI::dbConnect(RSQLite::SQLite(), sqliteDBNamePath)}
#
#     zelusScenarios<-character()
#     zelusYears<-numeric()
#
#     for(zelusFile_i in zelusFiles){
#
#       class_i=gsub(".csv","",zelusFile_i)
#       if(!grepl(".csv",zelusFile_i)){zelusFile_i=paste(zelusFile_i,".csv",sep="")}
#
#       if(!file.exists(paste(zelusFolder,"/",zelusFile_i,sep=""))){
#         print(paste("zelus file: ", zelusFolder,"/",zelusFile_i," is incorrect or doesn't exist.",sep=""))
#         print(paste("Skipping file: ",zelusFolder,"/",zelusFile_i,sep=""))
#       }else{
#         print(paste("Reading zelus data file: ",zelusFile_i,"...",sep=""))
#         gridx<-data.table::fread(paste(zelusFolder,"/",zelusFile_i,sep=""),fill=T)%>%
#           tibble::as_tibble()%>%dplyr::select(-'# ID',-ilon,-ilat)                      andym this is where I stopped replacing 'tethys' with 'zelus'
#         print("File read.")
#         names(gridx)<-gsub("X","",names(gridx))
#         if(grepl("mm",tethysUnits)){aggType="depth"}else{aggType="vol"}
#         gridx<-gridx%>%dplyr::select(-dplyr::contains("Unit"))
#         gridx<-gridx%>%
#           dplyr::mutate(lat=lat,lon=lon,
#                         scenarioGCM=NA,
#                         scenarioRCP=NA,
#                         scenarioSSP=NA,
#                         scenarioPolicy=NA,
#                         scenario=tethysScenario,
#                         param="tethysWatWithdraw",
#                         units=tethysUnits,
#                         aggType=aggType,
#                         classPalette="pal_wet",
#                         class=class_i)%>%
#           tidyr::gather(key="x",value="value",-c("lat","lon","scenario","scenarioPolicy","scenarioGCM","scenarioRCP","scenarioSSP","aggType","param","units","classPalette","class"))
#
#         gridx$x<-as.numeric(gridx$x)
#
#         gridx<-gridx%>%
#           dplyr::mutate(class=dplyr::case_when(grepl("wddom",class)~"Domestic",
#                                                grepl("elec",class)~"Electric",
#                                                grepl("irr",class)~"Irrigation",
#                                                grepl("liv",class)~"Livestock",
#                                                grepl("mfg",class)~"Manufacturing",
#                                                grepl("min",class)~"Mining",
#                                                grepl("nonag",class)~"Non Agriculture",
#                                                grepl("total",class)~"Total",
#                                                TRUE~class))
#
#         tethysScenarios<-c(tethysScenarios,tethysScenario)
#         tethysYears<-unique(gridx$x)
#
#         if(sqliteUSE==T){
#           DBI::dbWriteTable(dbConn, "gridMetis", gridx, append=T)
#           print(paste("Saving data to sqlite as sqlitUSE = ",sqliteUSE,sep=""))
#         }else{
#           print(paste("Using .Rdata format to save data.",sep=""))
#           gridMetis<-dplyr::bind_rows(gridMetis,gridx)
#         }
#
#         rm(gridx)
#
#       } # Close if tethys file exists
#     } # close tethys file loops
#
#     if(sqliteUSE==T){DBI::dbDisconnect(dbConn)}
#
#   } # Close tethys folder

#

#----------------
# Prepare Bia Files
#---------------




if(!dir.exists(biaFolder)){
  print(paste("bia folder: ", biaFolder ," is incorrect or doesn't exist.",sep=""))
  print(paste("Skipping bia runs",sep=""))}else {

    if(sqliteUSE==T){dbConn <- DBI::dbConnect(RSQLite::SQLite(), sqliteDBNamePath)}

    biaScenarios<-character()
    biaYears<-numeric()

    for(biaFile_i in biaFiles){

      if(!grepl(".csv",biaFile_i)){biaFile_i=paste(biaFile_i,".csv",sep="")}

      if(!file.exists(paste(biaFolder,"/",biaFile_i,sep=""))){
        print(paste("bia file: ", biaFolder,"/",biaFile_i," is incorrect or doesn't exist.",sep=""))
        print(paste("Skipping file: ",biaFolder,"/",biaFile_i,sep=""))
      }else{

        #biaCoords<-data.table::fread(biaCoordinatesPath, header=F);
        #biaCoords<-biaCoords%>%dplyr::rename(lon=V2,lat=V3)%>%dplyr::select(lon,lat)
        #biaGridArea<-data.table::fread(biaGridAreaHecsPath, header=F);
        #biaGridArea<-biaGridArea%>%dplyr::rename(Area_hec=V1)%>%dplyr::mutate(Area_km2=Area_hec)%>%
        #  dplyr::select(Area_hec,Area_km2)

        print(paste("Reading bia data file: ",biaFile_i,"...",sep=""))
        gridx<-data.table::fread(paste(biaFolder,"/",biaFile_i,sep=""), header=T,stringsAsFactors = F)%>%
          tibble::as_tibble()%>%dplyr::select(-country,-name,-country_long,-gppd_idnr,-fuel2,-fuel3,-fuel4,-owner,-source,-url,-geolocation_source,-generation_gwh_2013,-generation_gwh_2014,-generation_gwh_2015,-generation_gwh_2016,-estimated_generation_gwh)
        print(paste("Bia data file: ",biaFile_i," read.",sep=""))

        #names(gridx)<-gsub("X","",names(gridx))

        # if(nrow(gridx)!=nrow(biaCoords)){
        #   stop(paste("Rows in bia file: ", biaFolder,"/",biaFile_i,
        #              " not equal to rows in bia coords file: ",
        #              biaCoordinatesPath,sep=""))}

        # if(nrow(gridx)!=nrow(biaGridArea)){
        #   stop(paste("Rows in bia file: ", biaFolder,"/",biaFile_i,
        #              " not equal to rows in bia coords file: ",
        #              biaCoordinatesPath,sep=""))}


        if(grepl("GW",biaFile_i)){
          print(paste("Based on bia file name: ", biaFile_i, " has GW capacity data. Converting to MW...", sep=""))
          gridx<-gridx%>%dplyr::mutate(capacity_gw = capacity_gw*1000)%>%
            dplyr::rename(capacity_mw=capacity_gw)
        #  gridx<-dplyr::bind_cols(biaCoords,gridx)
        #  biaUnits="Runoff (mm)"                       #andym ?should we make biaUnits?
          print(paste("GW data converted to MW", sep=""))
        }else{
          print(paste("Based on bia filename: ", biaFile_i, " has MW data. Using MW.", sep=""))
        #  gridx<-dplyr::bind_cols(biaCoords,gridx)
          }

        # if(grepl("pm_abcd_mrtm",biaFile_i)){                                #andym add these to inspect biafiles for infromation on scenario
        #   biaScenario<-sub("^pm_abcd_mrtm_", "", biaFile_i);biaScenario
        #   biaScenario<-sub("\\_[0-9].*", "", biaScenario);biaScenario
        #   biaGCM<-sub("_.*","",biaScenario); biaGCM
        #   biaRCP<-sub(".*_","",biaScenario); biaRCP}else{
        #     biaScenario<-biaScenarioAssign
        #     biaGCM=NA;biaRCP=NA
        #   }

        biaScenario<-biaScenarioAssign       #andym take this out if use the code above. And should this be biaScenarios instead?
        biaGCM=NA;biaRCP=NA

        aggType="vol"

        #print(paste("Gathering data for bia filename: ", biaFile_i, " into year columns...", sep=""))

        gridx<-gridx%>%dplyr::mutate(lat=latitude,
                                     lon=longitude,
                                     scenario=biaScenario,
                                     scenarioGCM=biaGCM,
                                     scenarioRCP=biaRCP,
                                     scenarioSSP=NA,
                                     scenarioPolicy=NA,
                                     param="biaElecGen",
                                     units= "Capacity (MW)",
                                     aggType=aggType,
                                     classPalette="pal_elec_subsec",
                                     class=fuel1,
                                     value=capacity_mw,
                                     x=NA)%>%
          tibble::as_tibble()%>%dplyr::select(-latitude,-longitude,-fuel1,-capacity_mw)%>%
          #tidyr::gather(key="x",value="value",
           #             -c("lat","lon","scenario","scenarioPolicy","scenarioGCM","scenarioRCP","scenarioSSP","aggType","param","units","classPalette","class","commissioning_year","year_of_capacity_data"))  #%>%


          tibble::as_tibble()
        print(paste("Data for bia file gathered into columns.", sep=""))

        gridx$x<-as.numeric(gridx$x)

        biaScenarios<-c(biaScenarios,biaScenario)
        biaYears<-unique(gridx$x)


        if(sqliteUSE==T){
          DBI::dbWriteTable(dbConn, "gridMetis", gridx, append=T)
          print(paste("Saving data to sqlite as sqlitUSE = ",sqliteUSE,sep=""))
        }else{
          print(paste("Using .Rdata format to save data.",sep=""))
          gridMetis<-dplyr::bind_rows(gridMetis,gridx)
          }



        rm(gridx)


      } # Close if bia file exists
    } # close bia file loops

    if(sqliteUSE==T){DBI::dbDisconnect(dbConn)}

  } # Close bia folder



#----------------
# Analyze Bia Capcity Factors
#---------------

library(ggplot2)

for(biaFile_i in biaFiles){

  if(!grepl(".csv",biaFile_i)){biaFile_i=paste(biaFile_i,".csv",sep="")}


  print(paste("Reading bia data file: ",biaFile_i,"...",sep=""))
  gridaP<-data.table::fread(paste(biaFolder,"/",biaFile_i,sep=""), header=T,stringsAsFactors = F)%>%
    tibble::as_tibble()%>%dplyr::select(-name,-country_long,-gppd_idnr,-fuel2,-fuel3,-fuel4,-owner,-source,-url,-geolocation_source)   #andym could take out country instead of country_long
  print(paste("Bia data file: ",biaFile_i," read.",sep=""))




  if(grepl("GW",biaFile_i)){
    print(paste("Based on bia file name: ", biaFile_i," has GW capacity data. Converting to MW...", sep=""))
    gridaP<-gridaP%>%dplyr::mutate(capacity_gw = capacity_gw*1000)%>%
      dplyr::rename(capacity_mw=capacity_gw)

    print(paste("GW data converted to MW", sep=""))
  }else{
    print(paste("Based on bia filename: ", biaFile_i," has MW data. Using MW.", sep=""))
  }



  biaScenario<-biaScenarioAssign       #andym take this out if use the code above. And should this be biaScenarios instead?
  biaGCM=NA;biaRCP=NA

  aggType="vol"

  gridaP<-gridaP%>%dplyr::mutate(lat=latitude,
                               lon=longitude,
                               scenario=biaScenario,
                               scenarioGCM=biaGCM,
                               scenarioRCP=biaRCP,
                               scenarioSSP=NA,
                               scenarioPolicy=NA,
                               param="biaElecGen",
                               units= "Capacity (MW)",
                               aggType=aggType,
                               classPalette="pal_elec_subsec",
                               class=fuel1,
                               value=capacity_mw,
                               x=NA,
                               BackCalcCapFactor=estimated_generation_gwh/capacity_mw*(1000/(365*24)),
                               BCCF_gen2015=generation_gwh_2015/capacity_mw*(1000/(365*24)),
                               BCCF_gen2016=(1000/(365*24))*generation_gwh_2016/capacity_mw,
                               est_gen_gwh=estimated_generation_gwh,
                               gen_gwh_2013=generation_gwh_2013,
                               gen_gwh_2014=generation_gwh_2014,
                               gen_gwh_2015=generation_gwh_2015,
                               gen_gwh_2016=generation_gwh_2016)%>%
    tibble::as_tibble()%>%dplyr::select(-latitude,-longitude,-fuel1,-capacity_mw,-generation_gwh_2013,-generation_gwh_2014,-generation_gwh_2015,-generation_gwh_2016,-estimated_generation_gwh)%>%

    tibble::as_tibble()
  print(paste("Data for bia file gathered into columns.", sep=""))

  gridaP$x<-as.numeric(gridaP$x)


  ggplot(data = gridaP, aes(class, BackCalcCapFactor))+geom_boxplot()+coord_cartesian(ylim = c(0, 7))

  #print(mean(gridaP$BackCalcCapFactor~gridaP$class))


} #andym Close biaFile_i for loop



  mns<-with(gridaP, tapply(BackCalcCapFactor, class, mean, na.rm = TRUE))
  mns
  mdns<-with(gridaP, tapply(BackCalcCapFactor, class, median, na.rm = TRUE))
  mdns
  mns_gen2015<-with(gridaP, tapply(BCCF_gen2015, class, mean, na.rm = TRUE))
  mns_gen2015
  mdns_gen2015<-with(gridaP, tapply(BCCF_gen2015, class, median, na.rm = TRUE))
  mdns_gen2015
  mns_gen2016<-with(gridaP, tapply(BCCF_gen2016, class, mean, na.rm = TRUE))
  mns_gen2016
  mdns_gen2016<-with(gridaP, tapply(BCCF_gen2016, class, median, na.rm = TRUE))
  mdns_gen2016

  ##Do THIS FOR MEDIAN AS WELL (without disregarding the NAs)
  #dlply(gridaP, .(class), summarize, mean=mean(gridaP$BackCalcCapFactor))
  #meansaP<-ddply(gridaP$BackCalcCapFactor, .(gridaP$class), summarize, mean=mean(value))
  #meansaP

  length(gridaP$BCCF_gen2015[gridaP$class=="Biomass" & is.finite(gridaP$BCCF_gen2015) & gridaP$BCCF_gen2015!=0])
  length(gridaP$BCCF_gen2015[gridaP$class=="Coal" & is.finite(gridaP$BCCF_gen2015) & gridaP$BCCF_gen2015!=0])
  length(gridaP$BCCF_gen2015[gridaP$class=="Cogeneration" & is.finite(gridaP$BCCF_gen2015) & gridaP$BCCF_gen2015!=0])
  length(gridaP$BCCF_gen2015[gridaP$class=="Gas" & is.finite(gridaP$BCCF_gen2015) & gridaP$BCCF_gen2015!=0])
  length(gridaP$BCCF_gen2015[gridaP$class=="Geothermal" & is.finite(gridaP$BCCF_gen2015) & gridaP$BCCF_gen2015!=0])
  length(gridaP$BCCF_gen2015[gridaP$class=="Hydro" & is.finite(gridaP$BCCF_gen2015) & gridaP$BCCF_gen2015!=0])
  length(gridaP$BCCF_gen2015[gridaP$class=="Nuclear" & is.finite(gridaP$BCCF_gen2015) & gridaP$BCCF_gen2015!=0])
  length(gridaP$BCCF_gen2015[gridaP$class=="Oil" & is.finite(gridaP$BCCF_gen2015) & gridaP$BCCF_gen2015!=0])
  length(gridaP$BCCF_gen2015[gridaP$class=="Other" & is.finite(gridaP$BCCF_gen2015) & gridaP$BCCF_gen2015!=0])
  length(gridaP$BCCF_gen2015[gridaP$class=="Petcoke" & is.finite(gridaP$BCCF_gen2015) & gridaP$BCCF_gen2015!=0])
  length(gridaP$BCCF_gen2015[gridaP$class=="Solar" & is.finite(gridaP$BCCF_gen2015) & gridaP$BCCF_gen2015!=0])
  length(gridaP$BCCF_gen2015[gridaP$class=="Storage" & is.finite(gridaP$BCCF_gen2015) & gridaP$BCCF_gen2015!=0])
  length(gridaP$BCCF_gen2015[gridaP$class=="Waste" & is.finite(gridaP$BCCF_gen2015) & gridaP$BCCF_gen2015!=0])
  length(gridaP$BCCF_gen2015[gridaP$class=="Wave and Tidal" & is.finite(gridaP$BCCF_gen2015) & gridaP$BCCF_gen2015!=0])
  length(gridaP$BCCF_gen2015[gridaP$class=="Wind" & is.finite(gridaP$BCCF_gen2015) & gridaP$BCCF_gen2015!=0])

  length(gridaP$BCCF_gen2016[gridaP$class=="Biomass" & is.finite(gridaP$BCCF_gen2016) & gridaP$BCCF_gen2016!=0])
  length(gridaP$BCCF_gen2016[gridaP$class=="Coal" & is.finite(gridaP$BCCF_gen2016) & gridaP$BCCF_gen2016!=0])
  length(gridaP$BCCF_gen2016[gridaP$class=="Cogeneration" & is.finite(gridaP$BCCF_gen2016) & gridaP$BCCF_gen2016!=0])
  length(gridaP$BCCF_gen2016[gridaP$class=="Gas" & is.finite(gridaP$BCCF_gen2016) & gridaP$BCCF_gen2016!=0])
  length(gridaP$BCCF_gen2016[gridaP$class=="Geothermal" & is.finite(gridaP$BCCF_gen2016) & gridaP$BCCF_gen2016!=0])
  length(gridaP$BCCF_gen2016[gridaP$class=="Hydro" & is.finite(gridaP$BCCF_gen2016) & gridaP$BCCF_gen2016!=0])
  length(gridaP$BCCF_gen2016[gridaP$class=="Nuclear" & is.finite(gridaP$BCCF_gen2016) & gridaP$BCCF_gen2016!=0])
  length(gridaP$BCCF_gen2016[gridaP$class=="Oil" & is.finite(gridaP$BCCF_gen2016) & gridaP$BCCF_gen2016!=0])
  length(gridaP$BCCF_gen2016[gridaP$class=="Other" & is.finite(gridaP$BCCF_gen2016) & gridaP$BCCF_gen2016!=0])
  length(gridaP$BCCF_gen2016[gridaP$class=="Petcoke" & is.finite(gridaP$BCCF_gen2016) & gridaP$BCCF_gen2016!=0])
  length(gridaP$BCCF_gen2016[gridaP$class=="Solar" & is.finite(gridaP$BCCF_gen2016) & gridaP$BCCF_gen2016!=0])
  length(gridaP$BCCF_gen2016[gridaP$class=="Storage" & is.finite(gridaP$BCCF_gen2016) & gridaP$BCCF_gen2016!=0])
  length(gridaP$BCCF_gen2016[gridaP$class=="Waste" & is.finite(gridaP$BCCF_gen2016) & gridaP$BCCF_gen2016!=0])
  length(gridaP$BCCF_gen2016[gridaP$class=="Wave and Tidal" & is.finite(gridaP$BCCF_gen2016) & gridaP$BCCF_gen2016!=0])
  length(gridaP$BCCF_gen2016[gridaP$class=="Wind" & is.finite(gridaP$BCCF_gen2016) & gridaP$BCCF_gen2016!=0])




  gridbP<-data.table::fread(paste(biaFolder,"/capacity_factor_gcam_called_A23_globaltech.csv",sep=""), header=T,stringsAsFactors = F)    #%>%
#    tibble::as_tibble()%>%dplyr::select(-name,-country_long,-gppd_idnr,-fuel2,-fuel3,-fuel4,-owner,-source,-url,-geolocation_source)   #andym could take out country instead of country_long

  gridbP<-gridbP%>%dplyr::mutate(class=subsector,
                                 gcamCapFactor=cf_2100)
                                 #wriCapFactor=mns)         #%>%
    #tibble::as_tibble()%>%dplyr::select(-latitude,-longitude,-fuel1,-capacity_mw,-generation_gwh_2013,-generation_gwh_2014,-generation_gwh_2015,-generation_gwh_2016,-estimated_generation_gwh)%>%

  #gridaP<-gridaP%>%data.table

  griddP <- data.table::data.table(gridaP)

  grideP <- griddP[,.(BCCFmean=mean(BackCalcCapFactor,na.rm=TRUE)),by=class]
  gridfP <- merge(grideP,griddP[,.(BCCFmedian=median(BackCalcCapFactor,na.rm=TRUE)),by=class])
  gridgP <- merge(gridfP,griddP[,.(BCCF_gen2015mean=mean(BCCF_gen2015,na.rm=TRUE)),by=class])
  gridhP <- merge(gridgP,griddP[,.(BCCF_gen2015median=median(BCCF_gen2015,na.rm=TRUE)),by=class])
  gridjP <- merge(gridhP,griddP[,.(BCCF_gen2016mean=mean(BCCF_gen2016,na.rm=TRUE)),by=class])
  gridkP <- merge(gridjP,griddP[,.(BCCF_gen2016median=median(BCCF_gen2016,na.rm=TRUE)),by=class])%>%
    dplyr::mutate(class=toupper(class))

  #gridbP[,class]=gridbP[,gsub("(?<=\\b)([a-z])", "\\U\\1", tolower(class), perl=TRUE)]
  gridbP<-gridbP%>%dplyr::mutate(class=toupper(class))
  gridbP[gridbP=="REFINED LIQUIDS"]<-"OIL"

  gridiP <- merge(gridkP,gridbP)

  chrt <- ggplot(data = gridiP, aes(class, gcamCapFactor))+geom_point()+coord_cartesian(ylim = c(0, 1))
  chrt<-chrt+geom_point(data = gridiP, aes(class, BCCFmean), color="green", shape=6)
  chrt<-chrt+geom_point(data = gridiP, aes(class, BCCFmedian), color="green", shape=1)
  chrt<-chrt+geom_point(data = gridiP, aes(class, BCCF_gen2015mean), color="yellow", shape=6)
  chrt<-chrt+geom_point(data = gridiP, aes(class, BCCF_gen2015median), color="yellow", shape=1)
  chrt<-chrt+geom_point(data = gridiP, aes(class, BCCF_gen2016mean), color="blue", shape=6)
  chrt<-chrt+geom_point(data = gridiP, aes(class, BCCF_gen2016median), color="blue", shape=1)
  chrt



#----------------
# Compare WRI and GCAM electricity generation values (2015)
#---------------


  gridWRI <- gridaP%>%
    dplyr::group_by(country, class)%>%
    dplyr::summarise(WRI_total_capacity=sum(value))%>%
    dplyr::filter(country %in% c("ARG","COL"))%>%
    dplyr::mutate(class=toupper(class))



  gridGCAMelec<-data.table::fread(paste(biaFolder,"/elec_gen_by_subsector_Col_Arg_gcam.csv",sep=""), header=T,stringsAsFactors = F)

  gGeSlim<-gridGCAMelec%>%tibble::as_tibble()%>%dplyr::select(country=region, class=subsector, Elec_Gen_GCAM_2015="2015")%>%
    dplyr::mutate(class=toupper(class))
  gGeSlim[gGeSlim=="Argentina"]<-"ARG"
  gGeSlim[gGeSlim=="Colombia"]<-"COL"
  gGeSlim[gGeSlim=="REFINED LIQUIDS"]<-"OIL"

  GCAMcapFactors<-gridbP
  GCAMcapFactors$gcamCapFactor[GCAMcapFactors$technology=="CSP_storage"]<-NA
  GCAMcapFactors<-GCAMcapFactors%>%dplyr::group_by(class)%>%dplyr::summarise(gcamCapFactorAv=mean(gcamCapFactor, na.rm = TRUE))

  gGeSlim<-dplyr::full_join(gGeSlim,GCAMcapFactors, by="class")
  gGeSlim<-gGeSlim%>%dplyr::mutate(GCAMestCapVals=Elec_Gen_GCAM_2015/gcamCapFactorAv*(10^12)/(365*24*3600))



  gridComparingCapacity<-dplyr::full_join(gridWRI,gGeSlim, by = c("country", "class"))

  gridComparingCapacityARG<-gridComparingCapacity%>%dplyr::filter(country %in% c("ARG"))%>%
    dplyr::select(-c("gcamCapFactorAv","Elec_Gen_GCAM_2015"))%>%
    tidyr::gather(key="data_source",value="est_installed_capacity",-c("country", "class"))


  gridComparingCapacityCOL<-gridComparingCapacity%>%dplyr::filter(country %in% c("COL"))%>%
    dplyr::select(-c("gcamCapFactorAv","Elec_Gen_GCAM_2015"))%>%
    tidyr::gather(key="data_source",value="est_installed_capacity",-c("country", "class"))


  chrt2 <- ggplot(data = gridComparingCapacity, aes(fill = country, x = class, y = WRI_total_capacity))+geom_bar(position = "dodge", stat="identity")


  chrt3<-ggplot(data = gridComparingCapacityARG, aes(fill = data_source, x = class, y = est_installed_capacity))+geom_bar(position = "dodge", stat="identity")
  chrt3

  chrt4<-ggplot(data = gridComparingCapacityCOL, aes(fill = data_source, x = class, y = est_installed_capacity))+geom_bar(position = "dodge", stat="identity")
  chrt4




#----------------
# Creating a nation -> region list
#---------------

region32list<-data.table::fread(file=paste(getwd(),"/dataFiles/grids/xanthosReference/region32_grids.csv",sep=""), header=T,stringsAsFactors = F)
countrylist<-data.table::fread(file=paste(getwd(),"/dataFiles/grids/xanthosReference/country.csv",sep=""), header=T,stringsAsFactors = F)
regionnames<-data.table::fread(file=paste(getwd(),"/dataFiles/grids/xanthosReference/Rgn32Names.csv",sep=""), header=T,stringsAsFactors = F)%>%
  rename(region_32_code = region_id)
countrynames<-data.table::fread(file=paste(getwd(),"/dataFiles/grids/xanthosReference/country-names.csv",sep=""), header=F,stringsAsFactors = F)%>%
  rename(ctry_code=V1,ctry_name=V2)

countrylistFixed<-countrylist %>%
  mutate(ctry_code = as.numeric(ctry_code),
         ctry_code = case_when((ctry_code>-1)~(ctry_code-1),
                               TRUE~ctry_code))



countrytoregion<-dplyr::bind_cols(region32list,countrylistFixed)%>%
  filter(ctry_code>(-1))%>%
  dplyr::left_join(regionnames,by="region_32_code")%>%
  dplyr::left_join(countrynames,by="ctry_code")%>%
  unique()%>%
  remove_rownames()

# countrytoregion<-dplyr::bind_cols(region32list,countrylist)
# x<-regionnames%>%dplyr::left_join(countrytoregion,by="region_32_code")
#   dplyr::left_join(countrynames,by="ctry_code")%>%
#   unique()%>%
#   remove_rownames()


#countrynames[!(unique(countrynames$ctry_name)%in%unique(countrytoregion$ctry_name))]
#filter(countrynames,ctry_name=="Canada")
#unique(filter(countrylist,ctry_code==34))
#it seems like the Netherlands is in the countrytoregion but not in countrynames?? - no it is
#unique(b1$ctry_name)[!unique(b1$ctry_name) %in% unique(b$ctry_name)]->missingNames
#countrynames%>%filter(ctry_name %in% missingNames)
#a<-a[-1,]
#summary(compare(a,regionnames))
#summary(arsenal::compare(a,regionnames))
#unique(countrytoregion%>%select(ctry_code,ctry_name))%>%arrange(ctry_code)->b



write.csv(countrytoregion,paste(getwd(),"/dataFiles/grids/xanthosReference/country_to_region.csv",sep=""), row.names=F)


#----------------
# Prepare gridded Population
#---------------

if(!dir.exists(popFolder)){

  print(paste("pop folder: ", popFolder ," is incorrect or doesn't exist.",sep=""))
  print(paste("Skipping pop runs",sep=""))}else {

    if(sqliteUSE==T){dbConn <- DBI::dbConnect(RSQLite::SQLite(), sqliteDBNamePath)}

    for(popFile_i in popFiles){

      popFile_i=gsub(".csv","",popFile_i)
      if(!grepl(".csv",popFile_i)){popFile_i=paste(popFile_i,".csv",sep="")}

      if(!file.exists(paste(popFolder,"/",popFile_i,sep=""))){
        print(paste("pop file: ", popFolder,"/",popFile_i," is incorrect or doesn't exist.",sep=""))
        print(paste("Skipping file: ",popFolder,"/",popFile_i,sep=""))
      }else{
        print(paste("Reading population data file: ",popFile_i,"...",sep=""))

        gridx<-data.table::fread(paste(popFolder,"/",popFile_i,sep=""))%>%
          tibble::as_tibble()%>%dplyr::select(lon,lat,dplyr::contains("popGWP"))%>%
          tidyr::gather(key="key",value="value",-c("lat","lon"))%>%
          tidyr::separate(col="key",into=c("scenario","x"),sep="_")%>%
          dplyr::mutate(param="population",
                        units=popUnits,
                        aggType="vol",
                        classPalette="pal_hot",
                        class="class")
        gridx$x<-as.numeric(gridx$x)

        print("File read.")

        if(sqliteUSE==T){
          DBI::dbWriteTable(dbConn, "gridMetis", gridx, append=T)
          print(paste("Saving data to sqlite as sqlitUSE = ",sqliteUSE,sep=""))
        }else{
          print(paste("Using .Rdata format to save data.",sep=""))
          gridMetis<-dplyr::bind_rows(gridMetis,gridx)
        }

        rm(gridx)

      } # Close if pop file exists
    } # close pop file loops

    if(sqliteUSE==T){DBI::dbDisconnect(dbConn)}

  } # Close pop folder


#----------------
# Prepare gridded Agricultural Production
#---------------
# Based on LU crop distribution and Ag production GCAM
# For each Scenario and each GCAM Region Calculate:
# relative percentage of area in each grid cell
# Total ag production of crop
# Distribute ag production by percentage


#----------------
# Prepare gridded Electricity Demands
#---------------
# WRI database + GCAM Elec demands
# Distribute electric demands by population percentage


#----------------
# Prepare gridded Power Generation (Supply)
#---------------
# WRI database + GCAM Power Generated
# Calculate total power by fuel by region
# calculate relative prcnt by fuel by region
# Distirbute capacity by prnct distirbution

#----------------
# Prepare gridded Power Capacity (Supply)
#---------------
# WRI database power capacity + GCAM Cum Power Capacity


#----------------------

# Test Unique Values
#a<-gridMetis%>%tidyr::unite(col="key",names(gridMetis)[!names(gridMetis) %in% c("lat","lon","value")],sep="_",remove=T)
#a<-a%>%tidyr::spread(key=key,value=value)

#--------------
# Save RData and csv.
#----------------

  if(sqliteUSE==F){
    if(nrow(gridMetis)>0){
      save(gridMetis,file=gridMetisData)}
#data.table::fwrite(gridMetis,file = paste(dirOutputs, "/Grids/gridMetis.csv", sep = ""),row.names = F)
print(paste("gridMetis params: ", paste(unique(gridMetis$param),collapse=", "),sep=""))
#print(paste("gridMetis.csv saved in: ", paste(dirOutputs, "/Grids/gridMetis.csv", sep = ""),sep=""))
}else{
  if(file.exists(sqliteDBNamePath)){paste("Gridded data saved in SQLite database : ",sqliteDBNamePath, sep="")}else{
  print("No data added to gridMetis. Check datafiles folders to see if data is available.")}
}


}else{ # Close if reRead==1

  if(sqliteUSE==T){
    if(file.exists(sqliteDBNamePath)){paste("Re-read set to 0. Use data saved in SQLite database : ",sqliteDBNamePath, sep="")}}else{
if(!file.exists(gridMetisData)){stop(paste("File gridMetisData not found: ",gridMetisData,sep=""))}else{
load(gridMetisData)
  paste("Re-read set to 0. Usig saved data from .R data : ",gridMetisData, sep="")}
    }}


return(gridMetis)


} # Close Function
