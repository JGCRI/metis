#' metis.prepGrid
#'
#' This function prepares gridded data for use with other metis modules.
#' @param demeterFolder Full path to demeter outputs
#' @param demeterScenario Name of demeter scenario
#' @param demeterTimesteps Default is seq(from=2005,to=2100,by=5)
#' @param tethysFolder Folder for tethys results
#' @param tethysScenario Scenario name for tethys run
#' @param tethysFiles Default =c("wddom","wdelec","wdirr","wdliv","wdmfg","wdmin","wdnonag","wdtotal"),
#' @param tethysUnits No Default
#' @param demeterUnits No Default
#' @param xanthosFolder Xanthos Folder Path
#' @param xanthosFiles Xanthos Files to Read
#' @param xanthosScenarioAssign Default "NA". Scenario name if testing single scenario.
#' @param xanthosCoordinatesPath paste(getwd(),"/dataFiles/grids/xanthosCoords/coordinates.csv",sep="")
#' @param xanthosGridAreaHecsPath =paste(getwd(),"/dataFiles/grids/xanthosRunsChris/reference/Grid_Areas_ID.csv",sep=""),
#' @param scarcityXanthosRollMeanWindow Default = 10,
#' @param popFolder Default = <-paste(getwd(),"/dataFiles/grids/griddedIDsPop/",sep="")
#' @param popFiles Default = <-"grid_pop_map"
#' @param popUnits Default = <-"person"
#' @param dirOutputs Default =paste(getwd(),"/outputs",sep=""),
#' @param reReadData Default =1,
#' @param gridMetisData Default = paste(dirOutputs, "/Grids/gridMetis.RData", sep = "")
#' @param spanLowess Default = 0.25
#' @param sqliteUSE Default = T,
#' @param sqliteDBNamePath Default = paste(getwd(),"/outputs/Grids/gridMetis.sqlite", sep = "")
#' @return A table with data by polygon ID for each shapefile provided
#' @keywords gcam, gcam database, query
#' @export

metis.prepGrid<- function(demeterFolder="NA",
                        demeterScenario="NA",
                        demeterTimesteps=seq(from=2005,to=2100,by=5),
                        demeterUnits="NA",
                        tethysFolder="NA",
                        tethysScenario="NA",
                        tethysUnits="NA",
                        tethysFiles=c("wddom","wdelec","wdirr","wdliv","wdmfg","wdmin","wdnonag","wdtotal"),
                        copySingleTethysScenbyXanthos=NULL,
                        xanthosFolder="NA",
                        xanthosFiles="NA",
                        xanthosScenarioAssign="NA",
                        xanthosCoordinatesPath="NA",
                        xanthosGridAreaHecsPath="NA",
                        scarcityXanthosRollMeanWindow=10,
                        spanLowess=0.25,
                        popFolder="NA",
                        popFiles="NA",
                        popUnits="NA",
                        dirOutputs=paste(getwd(),"/outputs",sep=""),
                        reReadData=1,
                        gridMetisData=paste(getwd(),"/outputs/Grids/gridMetis.RData", sep = ""),
                        sqliteUSE = F,
                        sqliteDBNamePath = paste(getwd(),"/outputs/Grids/gridMetis.sqlite", sep = "")
                        ){

  # demeterFolder="NA"
  # demeterScenario="NA"
  # demeterTimesteps=seq(from=2005,to=2100,by=5)
  # demeterUnits="NA"
  # tethysFolder="NA"
  # tethysScenario="NA"
  # tethysUnits="NA"
  # tethysFiles=c("wddom","wdelec","wdirr","wdliv","wdmfg","wdmin","wdnonag","wdtotal")
  # xanthosFolder="NA"
  # xanthosFiles="NA"
  # xanthosScenarioAssign="NA"
  # xanthosCoordinatesPath="NA"
  # xanthosGridAreaHecsPath="NA"
  # scarcityXanthosRollMeanWindow=10
  # spanLowess=025
  # popFolder="NA"
  # popFiles="NA"
  # popUnits="NA"
  # dirOutputs=paste(getwd(),"/outputs",sep="")
  # reReadData=1
  # gridMetisData=paste(getwd(),"/outputs/Grids/gridMetis.RData", sep = "")
  # sqliteUSE = T
  # sqliteDBNamePath = paste(getwd(),"/outputs/Grids/gridMetis.sqlite", sep = "")


#----------------
# Initialize variables by setting to NULL
#----------------

NULL -> lat -> lon -> latitude -> longitude -> aez_id -> region_id ->X..ID->
  ilon->ilat->param->V2->V3->scenario->classPalette->rollingMean->x->scarcity->value->id->
    tethysScenarios->tethysYears->xanthosScenarios->xanthosYears->commonYears->commonScenarios->
    V1->Area_hec->Area_km2->lowess->valueXanthos->valueTethys->commonYears_i


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
# Prepare Demeter Files
#---------------

if(!dir.exists(demeterFolder)){

  print(paste("Demeter folder: ", demeterFolder ," is incorrect or doesn't exist.",sep=""))
  print(paste("Skipping demeter runs",sep=""))}else {

    if(sqliteUSE==T){dbConn <- DBI::dbConnect(RSQLite::SQLite(), sqliteDBNamePath)}


for(timestepx in demeterTimesteps){

if(!file.exists(paste(demeterFolder,"/landcover_",timestepx,"_timestep.csv",sep=""))){
  print(paste("Demeter file: ", demeterFolder,"/landcover_",timestepx,"_timestep.csv is incorrect or doesn't exist.",sep=""))
  print(paste("Skipping file: ",demeterFolder,"/landcover_",timestepx,"_timestep.csv",sep=""))
}else{
  print(paste("Reading demeter data file: ",demeterFolder,"/landcover_",timestepx,"_timestep.csv...",sep=""))
gridx<-data.table::fread(paste(demeterFolder,"/landcover_",timestepx,"_timestep.csv",sep=""))%>%
  tibble::as_tibble()%>%
  dplyr::mutate(lat=latitude,lon=longitude,
                scenarioGCM=NA,
                scenarioRCP=NA,
                scenarioSSP=NA,
                scenarioPolicy=NA,
         scenario=demeterScenario,
         param="demeterLandUse",
         units=demeterUnits,
         aggType="depth",
         x=timestepx,
         classPalette="pal_green")%>%
  dplyr::select(-aez_id,-region_id,-longitude,-latitude)%>%
  tidyr::gather(key="class",value="value",-c("lat","lon","scenario","scenarioPolicy","scenarioGCM","scenarioRCP","scenarioSSP","aggType","param","units","x","classPalette"))
print("File read.")

if(sqliteUSE==T){
  DBI::dbWriteTable(dbConn, "gridMetis", gridx, append=T)
  print(paste("Saving data to sqlite as sqlitUSE = ",sqliteUSE,sep=""))
}else{
  print(paste("Using .Rdata format to save data.",sep=""))
  gridMetis<-dplyr::bind_rows(gridMetis,gridx)
}

rm(gridx)

} # Close if demeter file exists
} # close demeter file loops

if(sqliteUSE==T){DBI::dbDisconnect(dbConn)}

} # Close Demeter folder


#----------------
# Prepare Tethys Files
#---------------

if(!dir.exists(tethysFolder)){
  print(paste("tethys folder: ", tethysFolder ," is incorrect or doesn't exist.",sep=""))
  print(paste("Skipping tethys runs",sep=""))}else {

    if(sqliteUSE==T){dbConn <- DBI::dbConnect(RSQLite::SQLite(), sqliteDBNamePath)}

    tethysScenarios<-character()
    tethysGCMRCPs<-tibble()
    tethysYears<-numeric()

    for(tethysFile_i in tethysFiles){

      class_i=gsub(".csv","",tethysFile_i)
      if(!grepl(".csv",tethysFile_i)){tethysFile_i=paste(tethysFile_i,".csv",sep="")}

      if(!file.exists(paste(tethysFolder,"/",tethysFile_i,sep=""))){
        print(paste("tethys file: ", tethysFolder,"/",tethysFile_i," is incorrect or doesn't exist.",sep=""))
        print(paste("Skipping file: ",tethysFolder,"/",tethysFile_i,sep=""))
      }else{
        print(paste("Reading tethys data file: ",tethysFile_i,"...",sep=""))
        gridx<-data.table::fread(paste(tethysFolder,"/",tethysFile_i,sep=""),fill=T)%>%
          tibble::as_tibble()%>%dplyr::select(-'# ID',-ilon,-ilat)
        print("File read.")
        names(gridx)<-gsub("X","",names(gridx))
        if(grepl("mm",tethysUnits)){aggType="depth"}else{aggType="vol"}
        gridx<-gridx%>%dplyr::select(-dplyr::contains("Unit"))
        gridx<-gridx%>%
              dplyr::mutate(lat=lat,lon=lon,
                            scenarioGCM=NA,
                            scenarioRCP=NA,
                            scenarioSSP=NA,
                            scenarioPolicy=NA,
                            scenario=paste(tethysScenario,scenarioSSP,scenarioPolicy,sep="_"),
                            param="tethysWatWithdraw",
                            units=tethysUnits,
                            aggType=aggType,
                            classPalette="pal_wet",
                            class=class_i)%>%
               tidyr::gather(key="x",value="value",-c("lat","lon","scenario","scenarioPolicy","scenarioGCM","scenarioRCP","scenarioSSP","aggType","param","units","classPalette","class"))

        gridx$x<-as.numeric(gridx$x)

        gridx<-gridx%>%
          dplyr::mutate(class=dplyr::case_when(grepl("wddom",class)~"Domestic",
                                               grepl("elec",class)~"Electric",
                                               grepl("irr",class)~"Irrigation",
                                               grepl("liv",class)~"Livestock",
                                               grepl("mfg",class)~"Manufacturing",
                                               grepl("min",class)~"Mining",
                                               grepl("nonag",class)~"Non Agriculture",
                                               grepl("total",class)~"Total",
                                               TRUE~class))

        tethysScenarios<-unique(c(tethysScenarios,unique(gridx$scenario)))
        tethysGCMRCP<-gridx %>%
          dplyr::select(scenarioGCM,scenarioRCP) %>% distinct()
        tethysGCMRCPs<-dplyr::bind_rows(tethysGCMRCPs,tethysGCMRCP)
        tethysYears<-unique(gridx$x)

        if(sqliteUSE==T){
          DBI::dbWriteTable(dbConn, "gridMetis", gridx, append=T)
          print(paste("Saving data to sqlite as sqlitUSE = ",sqliteUSE,sep=""))
        }else{
          print(paste("Using .Rdata format to save data.",sep=""))
          gridMetis<-dplyr::bind_rows(gridMetis,gridx)
        }

        rm(gridx)

      } # Close if tethys file exists
    } # close tethys file loops

   if(sqliteUSE==T){DBI::dbDisconnect(dbConn)}

  } # Close tethys folder

#----------------
# Prepare Xanthos Files
#---------------


if(!file.exists(xanthosGridAreaHecsPath)){
  print(paste("xanthos grid Area path: ", xanthosGridAreaHecsPath ," is incorrect or doesn't exist.",sep=""))
  print(paste("Skipping xanthos runs",sep=""))}else {


if(!file.exists(xanthosCoordinatesPath)){
  print(paste("xanthos coordinate path: ", xanthosCoordinatesPath ," is incorrect or doesn't exist.",sep=""))
  print(paste("Skipping xanthos runs",sep=""))}else {

if(!dir.exists(xanthosFolder)){
  print(paste("xanthos folder: ", xanthosFolder ," is incorrect or doesn't exist.",sep=""))
  print(paste("Skipping xanthos runs",sep=""))}else {

    if(sqliteUSE==T){dbConn <- DBI::dbConnect(RSQLite::SQLite(), sqliteDBNamePath)}

    xanthosScenarios<-character()
    xanthosGCMRCPs<-tibble()
    xanthosYears<-numeric()

    for(xanthosFile_i in xanthosFiles){

      if(!grepl(".csv",xanthosFile_i)){xanthosFile_i=paste(xanthosFile_i,".csv",sep="")}

      if(!file.exists(paste(xanthosFolder,"/",xanthosFile_i,sep=""))){
        print(paste("xanthos file: ", xanthosFolder,"/",xanthosFile_i," is incorrect or doesn't exist.",sep=""))
        print(paste("Skipping file: ",xanthosFolder,"/",xanthosFile_i,sep=""))
      }else{

        xanthosCoords<-data.table::fread(xanthosCoordinatesPath, header=F);
        xanthosCoords<-xanthosCoords%>%dplyr::rename(lon=V2,lat=V3)%>%dplyr::select(lon,lat)
        xanthosGridArea<-data.table::fread(xanthosGridAreaHecsPath, header=F);
        xanthosGridArea<-xanthosGridArea%>%dplyr::rename(Area_hec=V1)%>%dplyr::mutate(Area_km2=Area_hec)%>%
          dplyr::select(Area_hec,Area_km2)

        print(paste("Reading xanthos data file: ",xanthosFile_i,"...",sep=""))
        gridx<-data.table::fread(paste(xanthosFolder,"/",xanthosFile_i,sep=""), header=T,stringsAsFactors = F)%>%
          tibble::as_tibble()%>%dplyr::select(-id)
        print(paste("Xanthos data file: ",xanthosFile_i," read.",sep=""))

        names(gridx)<-gsub("X","",names(gridx))

        if(nrow(gridx)!=nrow(xanthosCoords)){
          stop(paste("Rows in xanthos file: ", xanthosFolder,"/",xanthosFile_i,
                     " not equal to rows in xanthos coords file: ",
                     xanthosCoordinatesPath,sep=""))}

        if(nrow(gridx)!=nrow(xanthosGridArea)){
          stop(paste("Rows in xanthos file: ", xanthosFolder,"/",xanthosFile_i,
                     " not equal to rows in xanthos coords file: ",
                     xanthosCoordinatesPath,sep=""))}


        if(grepl("km3",xanthosFile_i)){
          print(paste("Based on xanthos file name: ", xanthosFile_i, " has km3 data. Converting to mm...", sep=""))
        gridx<-gridx/(xanthosGridArea$Area_km2/1000000)
        gridx<-dplyr::bind_cols(xanthosCoords,gridx)
        xanthosUnits="Runoff (mm)"
        print(paste("km3 data converted to mm.", sep=""))
        }else{
          print(paste("Baed on xanthos filename: ", xanthosFile_i, " has mm data. Using mm.", sep=""))
          gridx<-dplyr::bind_cols(xanthosCoords,gridx)}

        if(grepl("pm_abcd_mrtm",xanthosFile_i)){
        xanthosScenario<-sub("^pm_abcd_mrtm_", "", xanthosFile_i);xanthosScenario
        xanthosScenario<-sub("\\_[0-9].*", "", xanthosScenario);xanthosScenario
        xanthosGCM<-sub("_.*","",xanthosScenario); xanthosGCM
        xanthosRCP<-sub(".*_","",xanthosScenario); xanthosRCP}else{
          xanthosScenario<-xanthosScenarioAssign
          xanthosGCM=NA;xanthosRCP=NA
        }


        if(grepl("mm",xanthosUnits)){aggType="depth"}else{aggType="vol"}
        print(paste("Gathering data for xanthos filename: ", xanthosFile_i, " into year columns...", sep=""))
        gridx<-gridx%>%dplyr::mutate(
                        scenarioGCM=xanthosGCM,
                        scenarioRCP=xanthosRCP,
                        scenarioSSP=NA,
                        scenarioPolicy=NA,
                        scenario=paste(xanthosScenario,scenarioSSP,scenarioPolicy,sep="_"),
                        param="xanthosRunoff",
                        units=xanthosUnits,
                        aggType=aggType,
                        classPalette="pal_wet",
                        class="Runoff")%>%
          tidyr::gather(key="x",value="value",
                        -c("lat","lon","scenario","scenarioPolicy","scenarioGCM","scenarioRCP","scenarioSSP","aggType","param","units","classPalette","class"))%>%
          tibble::as_tibble()
        print(paste("Data for xanthos file gathered into columns.", sep=""))

        gridx$x<-as.numeric(gridx$x)

        xanthosScenarios<-unique(c(xanthosScenarios,unique(gridx$scenario)))
        xanthosGCMRCP<-gridx %>%
                       dplyr::select(scenarioGCM,scenarioRCP) %>% distinct()
        xanthosGCMRCPs<-dplyr::bind_rows(xanthosGCMRCPs,xanthosGCMRCP)
        xanthosYears<-unique(gridx$x)

        # Apply Lowess Filter
        # https://stat.ethz.ch/pipermail/bioconductor/2003-September/002337.html
        # https://www.rdocumentation.org/packages/gplots/versions/3.0.1/topics/lowess


        print(paste("Applying lowess filter to file: ", xanthosFile_i, " using lowess span of ",spanLowess,"...", sep=""))
        gridx <- gridx %>%
          dplyr::group_by(lat,lon,scenario,param,units,aggType,classPalette,class) %>%
          dplyr::arrange(lat,lon) %>%
          dplyr::mutate(lowess = stats::lowess(y=value, x=x, f=spanLowess )$y)
        print(paste("Lowess filter applied.", sep=""))


        for(i in c(1,5,40,100,149,180)){
        gridC<-gridx[(gridx$lat==unique(gridx$lat)[i] & gridx$lon==unique(gridx$lon)[i]),]
        fname=paste(unique(gridC$scenario),"_",unique(gridC$param),
                    "_lat",unique(gridC$lat),"_lon", unique(gridC$lon),
                    "_lowessSpan",spanLowess,sep="")
        metis.printPdfPng(figure=graphics::plot(gridC$x,gridC$value,type="l",
                             main=paste(unique(gridC$scenario),
                                       "\nlat = ",unique(gridC$lat),", lon = ", unique(gridC$lon),
                                       ", Lowess Span =  ",spanLowess,sep=""),
                             ylab=unique(gridC$units),xlab="Year")+
                             graphics::lines(gridC$x,gridC$lowess,type="l",col="red"),
                          dir=paste(dirOutputs, "/Grids/diagnostics",sep=""),filename=fname,figWidth=9,figHeight=7,pdfpng="png")
            }

        gridx<-gridx%>%dplyr::mutate(value=lowess)%>%dplyr::select(-lowess)

        if(sqliteUSE==T){
        DBI::dbWriteTable(dbConn, "gridMetis", gridx, append=T)
        print(paste("Saving data to sqlite as sqlitUSE = ",sqliteUSE,sep=""))
        }else{
        print(paste("Using .Rdata format to save data.",sep=""))
        gridMetis<-dplyr::bind_rows(gridMetis,gridx)
        }

        rm(gridx)


      } # Close if xanthos file exists
    } # close xanthos file loops
  } # Close xanthos folder
} # close If xanthosCoords path exists

    if(sqliteUSE==T){DBI::dbDisconnect(dbConn)}

} # close If xanthosGridAreaHecsPath path exists

#----------------
# Prepare Gridded Scarcity
#---------------
tethysGCMRCPs<-tethysGCMRCPs%>%unique()%>%dplyr::mutate(GCMRCP=paste(scenarioGCM,scenarioRCP,sep="_"))
xanthosGCMRCPs<-xanthosGCMRCPs%>%unique()%>%dplyr::mutate(GCMRCP=paste(scenarioGCM,scenarioRCP,sep="_"))

xanthosScenarios;tethysScenarios;
xanthosGCMRCPs;tethysGCMRCPs;
xanthosYears;tethysYears;
commonYears<-tethysYears[tethysYears %in% xanthosYears];commonYears
commonScenarios<-tethysScenarios[tethysScenarios %in% xanthosScenarios];commonScenarios
commonGCMRCPs<-xanthosGCMRCPs %>% dplyr::filter(GCMRCP %in% unique(tethysGCMRCPs$GCMRCP));commonGCMRCPs

if(!is.null(copySingleTethysScenbyXanthos)){
  commonGCMRCPsX<-xanthosGCMRCPs}else{
    commonGCMRCPsX<-commonGCMRCPs}

uniqueGCMs<-unique(commonGCMRCPsX$scenarioGCM)
uniqueRCPs<-unique(commonGCMRCPsX$scenarioRCP)


if(sqliteUSE==T){

  if(length(commonYears)>0 & any(length(commonGCMRCPs)>0,!is.null(copySingleTethysScenbyXanthos))){

  if(file.exists(sqliteDBNamePath)){
  dbConn <- DBI::dbConnect(RSQLite::SQLite(), sqliteDBNamePath)
  gridMetis<-dplyr::tbl(dbConn,"gridMetis")

      print(paste("Extracting data from sqlite database for tethys/xanthos scenarios...",sep=""))
      gridMetisTethys<-gridMetis%>%
        dplyr::filter(class=="Total" & param=="tethysWatWithdraw" & x %in% commonYears)%>%dplyr::collect()%>%
        dplyr::rename(valueTethys=value, scenarioTethys=scenario)
      gridMetisXanthos<-gridMetis%>%dplyr::filter(param=="xanthosRunoff" & x %in% commonYears &
                                                    scenarioGCM %in% uniqueGCMs &
                                                    scenarioRCP %in% uniqueRCPs)%>%dplyr::collect()%>%
        dplyr::rename(valueXanthos=value, scenarioXanthos=scenario)

      if(!is.null(copySingleTethysScenbyXanthos)){
        if(grepl(copySingleTethysScenbyXanthos,unique(gridMetisTethys$scenarioTethys))){

          gridMetisTethysX<-tibble()

          paste("Copying tethys results for all xanthos GCM and RCPs...")
          for(row_i in 1:nrow(xanthosGCMRCPs)){
            gridMetisTethysX<-dplyr::bind_rows(gridMetisTethysX,
                                                gridMetisTethys %>%
                                                  dplyr::filter(grepl(copySingleTethysScenbyXanthos,scenarioTethys)) %>%
                                                  dplyr::mutate(scenarioGCM=xanthosGCMRCPs[row_i,]$scenarioGCM,
                                                                scenarioRCP=xanthosGCMRCPs[row_i,]$scenarioRCP))
          }
          gridMetisTethys<-gridMetisTethysX
          rm(gridMetisTethysX)
          paste("Tethys results for all xanthos GCM and RCPs copied.")

        } else {paste(copySingleTethysScenbyXanthos, " not present in tethys scenarios: ",
                      paste(unique(gridMetisTethys$scenarioTethys),collapse=", "))}
      }

      gridx<-dplyr::left_join(gridMetisTethys,gridMetisXanthos%>%dplyr::select(lat,lon,x,scenarioGCM,scenarioRCP,valueXanthos,scenarioXanthos),
                              by=c("lat","lon","x","scenarioGCM","scenarioRCP"))%>%
        dplyr::mutate(scarcity=valueTethys/valueXanthos,
                      units="Gridded Scarcity (Fraction)",
                      param="griddedScarcity",
                      class="Scarcity",
                      classPalette="pal_hot",
                      scenario=paste("T",scenarioTethys,"X",scenarioXanthos,sep=""))%>%
        dplyr::select(-valueXanthos,-valueTethys,-scenarioTethys,-scenarioXanthos)%>%
        dplyr::rename(value=scarcity)%>%
        dplyr::filter(!is.na(value));
      print(paste("Data extracted and saved.",sep=""))
      if(sqliteUSE==T){
        DBI::dbWriteTable(dbConn, "gridMetis", gridx, append=T)
        print(paste("Saving data to sqlite as sqlitUSE = ",sqliteUSE,sep=""))
      }

      rm(gridx)


   if(sqliteUSE==T){DBI::dbDisconnect(dbConn)}

  }} else { # Closing if xanthos & tethys checks have commonYears & commonScenarios
      print(paste("No common years or scenarios for Xanthos and Tethys. ",
                      " Skipping gridded scarcity calculation",sep=""))
    }}else{

if(!is.null(gridMetis)){
  if(any(grepl("xanthos",unique(gridMetis$param))) & any(grepl("tethys",unique(gridMetis$param)))){

  if(length(commonYears)>0 & length(commonScenarios)>0){

    gridMetisTethys<-gridMetis%>%dplyr::filter(class=="Total" & param=="tethysWatWithdraw" & x %in% commonYears)%>%
      dplyr::rename(valueTethys=value,scenarioTethys=scenario)
    gridMetisXanthos<-gridMetis%>%dplyr::filter(param=="xanthosRunoff" & x %in% commonYears &
                                                  scenarioGCM %in% uniqueGCMs &
                                                  scenarioRCP %in% uniqueRCPs)%>%
      dplyr::rename(valueXanthos=value, scenarioXanthos=scenario)


    if(!is.null(copySingleTethysScenbyXanthos)){
      if(grepl(copySingleTethysScenbyXanthos,unique(gridMetisTethys$scenarioTethys))){

        gridMetisTethysX<-tibble()

        paste("Copying tethys results for all xanthos GCM and RCPs...")
        for(row_i in 1:nrow(xanthosGCMRCPs)){
          gridMetisTethysX<-dplyr::bind_rows(gridMetisTethysX,
                                             gridMetisTethys %>%
                                               dplyr::filter(grepl(copySingleTethysScenbyXanthos,scenarioTethys)) %>%
                                               dplyr::mutate(scenarioGCM=xanthosGCMRCPs[row_i,]$scenarioGCM,
                                                             scenarioRCP=xanthosGCMRCPs[row_i,]$scenarioRCP))
        }
        gridMetisTethys<-gridMetisTethysX
        rm(gridMetisTethysX)
        paste("Tethys results for all xanthos GCM and RCPs copied.")

      } else {paste(copySingleTethysScenbyXanthos, " not present in tethys scenarios: ",
                    paste(unique(gridMetisTethys$scenarioTethys),collapse=", "))}
    }



         gridx<-dplyr::left_join(gridMetisTethys,gridMetisXanthos%>%dplyr::select(lat,lon,x,scenarioXanthos,valueXanthos),
                                by=c("lat","lon","x","scenario"))%>%
          dplyr::mutate(scarcity=valueTethys/valueXanthos,
                        units="Gridded Scarcity (Fraction)",
                        param="griddedScarcity",
                        class="Scarcity",
                        classPalette="pal_hot",
                        scenario=paste("T",scenarioTethys,"X",scenarioXanthos,sep="")) %>%
          dplyr::select(-valueXanthos,-valueTethys, -scenarioTethys,-scenarioXanthos) %>%
          dplyr::rename(value=scarcity)%>%
          dplyr::filter(!is.na(value));

         print(paste("Using .Rdata format to save data.",sep=""))
         gridMetis<-dplyr::bind_rows(gridMetis,gridx)
         rm(gridx)


  }}}else {print(paste("gridMetis is NULL, skipping gridded scracity calculation.",sep=""))}
    } # Close sql Loop



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
