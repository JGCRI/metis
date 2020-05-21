#' metis.prepGrid
#'
#' This function prepares gridded data for use with other metis modules.
#' @param demeterFolders Full path to demeter outputs
#' @param demeterScenarios Name of demeter scenario
#' @param demeterTimesteps Default is seq(from=2005,to=2100,by=5)
#' @param tethysFolders Folder for tethys results
#' @param tethysScenarios Scenario name for tethys run
#' @param tethysFiles Default =c("wddom","wdelec","wdirr","wdliv","wdmfg","wdmin","wdnonag","wdtotal"),
#' @param tethysUnits No Default
#' @param demeterUnits No Default
#' @param xanthosScenarios Default=NULL,
#' @param xanthosFiles Xanthos Files to Read
#' @param xanthosScenarioAssign Default NULL. Scenario name if testing single scenario.
#' @param xanthosCoordinatesPath paste(getwd(),"/dataFiles/grids/xanthosCoords/coordinates.csv",sep="")
#' @param xanthosGridAreaHecsPath =paste(getwd(),"/dataFiles/grids/xanthosRunsChris/reference/Grid_Areas_ID.csv",sep=""),
#' @param popFolder Default = <-paste(getwd(),"/dataFiles/grids/griddedIDsPop/",sep="")
#' @param popFiles Default = <-"grid_pop_map"
#' @param biaFolder Default = <-paste(getwd(),"/dataFiles/grids/griddedIDsbia/",sep="")
#' @param biaFiles Default = <-"grid_bia_map"
#' @param popUnits Default = <-"person"
#' @param dirOutputs Default =paste(getwd(),"/outputs",sep=""),
#' @param spanLowess Default = 0.25
#' @param folderName Default=NULL
#' @param tethysFilesScarcity Default=NULL,
#' @param xanthosFilesScarcity Default=NULL,
#' @param saveFormat Default="rds". Choose between "rds" (Native R much faster) or "csv" or "both".
#' @param filterYears Default=seq(1980,2100,by=5)
#' @param diagnosticsOn Default =F
#' @return A table with data by polygon ID for each shapefile provided
#' @keywords gcam, gcam database, query
#' @export

metis.prepGrid<- function(demeterFolders=NULL,
                        demeterScenarios=NULL,
                        demeterTimesteps=seq(from=2005,to=2100,by=5),
                        demeterUnits=NULL,
                        tethysFolders=NULL,
                        tethysScenarios=NULL,
                        tethysUnits=NULL,
                        tethysFiles=c("wddom","wdelec","wdirr","wdliv","wdmfg","wdmin","wdnonag","wdtotal"),
                        xanthosFiles=NULL,
                        xanthosScenarios=NULL,
                        xanthosScenarioAssign=NULL,
                        xanthosCoordinatesPath=NULL,
                        xanthosGridAreaHecsPath=NULL,
                        tethysFilesScarcity=NULL,
                        xanthosFilesScarcity=NULL,
                        spanLowess=0.25,
                        popFolder=NULL,
                        popFiles=NULL,
                        biaFolder=NULL,
                        biaFiles=NULL,
                        popUnits=NULL,
                        dirOutputs=paste(getwd(),"/outputs",sep=""),
                        folderName=NULL,
                        saveFormat="rds",
                        filterYears=NULL,
                        diagnosticsOn=F
                        ){

  # demeterFolders=NULL
  # demeterScenarios=NULL
  # demeterTimesteps=seq(from=2005,to=2100,by=5)
  # demeterUnits=NULL
  # tethysFolders=NULL
  # tethysScenarios=NULL
  # tethysUnits=NULL
  # tethysFiles=c("wddom","wdelec","wdirr","wdliv","wdmfg","wdmin","wdnonag","wdtotal")
  # xanthosFiles=NULL
  # xanthosScenarios=NULL
  # xanthosScenarioAssign=NULL
  # xanthosCoordinatesPath=NULL
  # xanthosGridAreaHecsPath=NULL
  # tethysFilesScarcity=NULL
  # xanthosFilesScarcity=NULL
  # spanLowess=0.25
  # popFolder=NULL
  # popFiles=NULL
  # biaFolder=NULL
  # biaFiles=NULL
  # popUnits=NULL
  # dirOutputs=paste(getwd(),"/outputs",sep="")
  # folderName=NULL
  # saveFormat="rds"
  # filterYears=NULL
  # diagnosticsOn=F


#----------------
# Initialize variables by setting to NULL
#----------------

NULL -> lat -> lon -> latitude -> longitude -> aez_id -> region_id ->X..ID->
  ilon->ilat->param->V2->V3->scenario->classPalette->rollingMean->x->scarcity->value->id->
   tethysYears->xanthosYears->
    commonYears->commonScenarios->V1->Area_hec->Area_km2->lowess->valueXanthos->valueTethys->commonYears_i->
    tethysGCMRCPs->xanthosGCMRCPs->scenarioMultiA->scenarioMultiB->
    country->name->GCMRCP->datax->
    region->regionsSelect->rowid->scenarioTethys->scenarioXanthos->
    year->origValue->gridlat->gridlon->class1->valueDistrib->origValueDistrib->gridCellPercentage->
    region_32_code->ctry_name->ctry_code->aggregate->gridID->diagnosticFig-> class2


#------------------
# Function for adding any missing columns if needed
# -----------------

  addMissing<-function(data){
    if(!"scenario"%in%names(data)){data<-data%>%dplyr::mutate(scenario="scenario")}
    if(!"x"%in%names(data)){if("year"%in%names(data)){
      data<-data%>%dplyr::mutate(x=year)}else{data<-data%>%dplyr::mutate(x="x")}}
    if(!"region"%in%names(data)){data<-data%>%dplyr::mutate(region="region")}
    if(!"classPalette"%in%names(data)){data<-data%>%dplyr::mutate(classPalette="pal_hot")}
    if(!"param"%in%names(data)){data<-data%>%dplyr::mutate(param="param")}
    if(!"scenarioMultiA"%in%names(data)){data<-data%>%dplyr::mutate(scenarioMultiA="scenarioMultiA")}
    if(!"scenarioMultiB"%in%names(data)){data<-data%>%dplyr::mutate(scenarioMultiB="scenarioMultiB")}
    if(!"class"%in%names(data)){data<-data%>%dplyr::mutate(class="scenarioPolicy")}
    if(!"class2"%in%names(data)){data<-data%>%dplyr::mutate(class2="class2")}
    if(!"aggType"%in%names(data)){data<-data%>%dplyr::mutate(aggType="aggType")}
    if(!"lon"%in%names(data)){data<-data%>%dplyr::mutate(lon="lon")}
    if(!"lat"%in%names(data)){data<-data%>%dplyr::mutate(lat="lat")}
    if(!"units"%in%names(data)){data<-data%>%dplyr::mutate(units="units")}
    return(data)
  }



#------------------
# Create Folders if needed
#------------------
if (!dir.exists(dirOutputs)){
    dir.create(dirOutputs)}
if (!dir.exists(paste(dirOutputs, "/prepGrid", sep = ""))){
    dir.create(paste(dirOutputs, "/prepGrid", sep = ""))}
  if(!is.null(folderName)){
    if (!dir.exists(paste(dirOutputs, "/prepGrid/",folderName, sep = ""))){
      dir.create(paste(dirOutputs, "/prepGrid/",folderName, sep = ""))}
  if (!dir.exists(paste(dirOutputs, "/prepGrid/",folderName,"/diagnostics",sep=""))){
    dir.create(paste(dirOutputs, "/prepGrid/",folderName,"/diagnostics",sep=""))}
  dirDiagnostic = paste(dirOutputs, "/prepGrid/",folderName,"/diagnostics",sep="")
  dir=paste(dirOutputs, "/prepGrid/",folderName,sep="")
  }else{if (!dir.exists(paste(dirOutputs, "/prepGrid/diagnostics",sep=""))){
    dir.create(paste(dirOutputs, "/prepGrid/diagnostics",sep=""))}
    dir=paste(dirOutputs, "/prepGrid",sep="")
    dirDiagnostic = paste(dirOutputs, "/prepGrid/diagnostics",sep="")}


#------------------
# If reread data
#------------------

paramScenarios<-tibble::tibble()

#----------------
# Prepare Demeter Files
#---------------

if(!is.null(demeterFolders)){

  if(!is.null(demeterScenarios)){
    demeterFolderScen <- data.frame("folder"=c(demeterFolders),"scenario"=c(demeterScenarios))
    print("The following folder scenarios found for Demeter.")
    print(demeterFolderScen)


  for(i in 1:nrow(demeterFolderScen)){

if(!dir.exists(paste(demeterFolderScen$folder[i],sep=""))){

  print(paste("Demeter folder: ", demeterFolderScen$folder[i] ," is incorrect or doesn't exist.",sep=""))
  print(paste("Skipping demeter runs",sep=""))
  }

for(timestepx in demeterTimesteps){

if(!file.exists(paste(demeterFolderScen$folder[i],"/landcover_",timestepx,"_timestep.csv",sep=""))){
  print(paste("Demeter file: ", demeterFolderScen$folder[i],"/landcover_",timestepx,"_timestep.csv is incorrect or doesn't exist.",sep=""))
  print(paste("Skipping file: ",demeterFolderScen$folder[i],"/landcover_",timestepx,"_timestep.csv",sep=""))
}else{
  print(paste("Reading demeter data file: ",demeterFolderScen$folder[i],"/landcover_",timestepx,"_timestep.csv...",sep=""))
gridx<-data.table::fread(paste(demeterFolderScen$folder[i],"/landcover_",timestepx,"_timestep.csv",sep=""),encoding="Latin-1")%>%
  tibble::as_tibble()%>%
  dplyr::mutate(lat=latitude,lon=longitude,
                scenarioMultiA=NA,
                scenarioMultiB=NA,
         scenario=demeterFolderScen$scenario[i],
         param="demeterLandUse",
         units=demeterUnits,
         aggType="depth",
         x=timestepx,
         classPalette="pal_green",
         region="region")%>%
  dplyr::select(-aez_id,-region_id,-longitude,-latitude)%>%
  tidyr::gather(key="class",value="value",-c("lat","lon","region","scenario","scenarioMultiA","scenarioMultiB","aggType","param","units","x","classPalette"))
print("File read.")

colsSelect <- names(gridx)[names(gridx) %in% c( "lon","lat","region","scenarioMultiA","scenarioMultiB","scenario",
                                                "param","units","aggType","classPalette","class","x","value")]
gridx <- gridx %>% dplyr::select(colsSelect) %>% dplyr::ungroup()
gridx<-addMissing(gridx); gridx

if(!is.null(filterYears)){gridx <- gridx %>% dplyr::filter(x %in% filterYears)}

if(saveFormat=="rds"){
  saveRDS(gridx,paste(dir,"/demeter_",demeterFolderScen$scenario[i],"_",timestepx,".rds",sep=""))
  print(paste("Saving file as: ",dir,"/demeter_",demeterFolderScen$scenario[i],"_",timestepx,".rds",sep=""))

}
if(saveFormat=="csv"){
  data.table::fwrite(gridx,paste(dir,"/demeter_",demeterFolderScen$scenario[i],"_",timestepx,".csv",sep=""))
  print(paste("Saving file as: ",dir,"/demeter_",demeterFolderScen$scenario[i],"_",timestepx,".csv",sep=""))

}
if(saveFormat=="both"){
  saveRDS(gridx,paste(dir,"/demeter_",demeterFolderScen$scenario[i],"_",timestepx,".rds",sep=""))
  data.table::fwrite(gridx,paste(dir,"/demeter_",demeterFolderScen$scenario[i],"_",timestepx,".csv",sep=""))
  print(paste("Saving file as: ",dir,"/demeter_",demeterFolderScen$scenario[i],"_",timestepx,".rds",sep=""))
  print(paste("Saving file as: ",dir,"/demeter_",demeterFolderScen$scenario[i],"_",timestepx,".csv",sep=""))

}
paramScenarios <- paramScenarios%>%
  dplyr::bind_rows(gridx[1,]%>%dplyr::select(param,scenario)%>%unique()) %>%
  dplyr::ungroup()%>%
  dplyr::select(param,scenario)%>%
  unique();paramScenarios
rm(gridx)

} # Close if demeter file exists
} # close demeter file loops
} # CLose each demeter folder
}else{print("Demeter scenarios not specified")}
}# Close Demeter folder

#----------------
# Prepare Tethys Files
#---------------

if(!file.exists(xanthosGridAreaHecsPath)){
  print(paste("xanthos grid Area path: ", xanthosGridAreaHecsPath ," is incorrect or doesn't exist.",sep=""))
  print(paste("Skipping xanthos runs",sep=""))}else {


    if(!file.exists(xanthosCoordinatesPath)){
      print(paste("xanthos coordinate path: ", xanthosCoordinatesPath ," is incorrect or doesn't exist.",sep=""))
      print(paste("Skipping xanthos runs",sep=""))}else {


        xanthosCoords<-data.table::fread(xanthosCoordinatesPath, header=F,encoding="Latin-1");
        xanthosCoords<-xanthosCoords%>%dplyr::rename(lon=V2,lat=V3)%>%dplyr::select(lon,lat)
        xanthosGridArea<-data.table::fread(xanthosGridAreaHecsPath, header=F,encoding="Latin-1");
        xanthosGridArea<-xanthosGridArea%>%dplyr::rename(Area_hec=V1)%>%dplyr::mutate(Area_km2=0.01*Area_hec)%>%
          dplyr::select(Area_hec,Area_km2)


if(!is.null(tethysFolders)){

  if(!is.null(tethysScenarios)){
    tethysFolderScen <- data.frame("folder"=c(tethysFolders),"scenario"=c(tethysScenarios))
    print("The following folder scenarios found for Tethys.")
    print(tethysFolderScen)

  for(i in 1:nrow(tethysFolderScen)){

if(!dir.exists(paste(tethysFolderScen$folder[i],sep=""))){
  print(paste("tethys folder: ", tethysFolderScen$folder[i] ," is incorrect or doesn't exist.",sep=""))
  print(paste("Skipping tethys runs",sep=""))}else {

    tethysScenario_i<-tethysFolderScen$scenario[i]
    tethysGCMRCPs<-tibble::tibble()
    tethysYears<-numeric()

    for(tethysFile_i in tethysFiles){

      class_i=gsub(".csv","",tethysFile_i)
      if(!grepl(".csv",tethysFile_i)){tethysFile_i=paste(tethysFile_i,".csv",sep="")}

      if(!file.exists(paste(tethysFolderScen$folder[i],"/",tethysFile_i,sep=""))){
        print(paste("tethys file: ", tethysFolderScen$folder[i],"/",tethysFile_i," is incorrect or doesn't exist.",sep=""))
        print(paste("Skipping file: ",tethysFolderScen$folder[i],"/",tethysFile_i,sep=""))
      }else{
        print(paste("Reading tethys data file: ",tethysFile_i," for scenario ",tethysFolderScen$scenario[i],"...",sep=""))
        gridx<-data.table::fread(paste(tethysFolderScen$folder[i],"/",tethysFile_i,sep=""),fill=T,encoding="Latin-1")%>%
          tibble::as_tibble()%>%dplyr::select(-'ID',-ilon,-ilat)
        print("File read.")
        names(gridx)<-gsub("X","",names(gridx))


        if(nrow(gridx)!=nrow(xanthosCoords)){
          stop(paste("Rows in tethys file: ", tethysFile_i,
                     " not equal to rows in coords file: ",
                     xanthosCoordinatesPath,sep=""))}

        if(nrow(gridx)!=nrow(xanthosGridArea)){
          stop(paste("Rows in tethys file: ", tethysFile_i,
                     " not equal to rows in coords file: ",
                     xanthosCoordinatesPath,sep=""))}


        if(grepl("km3",tethysFile_i) | grepl("km3",tethysUnits)){
          print(paste("Based on tethys file name: ", tethysFile_i, " or given tethys units: ", tethysUnits," data is in km3. Converting to mm...", sep=""))
          gridx<-gridx/(xanthosGridArea$Area_km2/1000000)
          gridx[gridx<0]=0
          gridx<-dplyr::bind_cols(xanthosCoords,gridx)
          tethysUnits="Water Withdrawals (mm)"
          print(paste("km3 data converted to mm.", sep=""))
        }else{
          print(paste("Based on tethys file name: ", tethysFile_i, " or given tethys units: ", tethysUnits," data is in mm. Using mm.", sep=""))
          gridx<-dplyr::bind_cols(xanthosCoords,gridx)}

        if(grepl("mm",tethysUnits)){aggType="depth"}else{aggType="vol"}
        gridx<-gridx%>%dplyr::select(-dplyr::contains("Unit"))
        gridx<-gridx%>%
              dplyr::mutate(lat=lat,lon=lon,
                            scenarioMultiA=NA,
                            scenarioMultiB=NA,
                            scenario=paste(tethysScenario_i,sep="_"),
                            param="tethysWatWithdraw",
                            units=tethysUnits,
                            aggType=aggType,
                            classPalette="pal_wet",
                            class=class_i,
                            region="region")%>%
               tidyr::gather(key="x",value="value",-c("lat","lon","region","scenario","scenarioMultiA","scenarioMultiB","aggType","param","units","classPalette","class"))

        gridx$x<-as.numeric(gridx$x)

        gridx<-gridx%>%
          dplyr::mutate(param=dplyr::case_when(grepl("nonag",class,ignore.case = T)~paste(param,"_nonAg",sep=""),
                                               grepl("total",class,ignore.case = T)~paste(param,"_total",sep=""),
                                               TRUE~paste(param,"_indv",sep="")),
                        class=dplyr::case_when(grepl("wddom",class,ignore.case = T)~"Domestic",
                                               grepl("elec",class,ignore.case = T)~"Electric",
                                               grepl("irr",class,ignore.case = T)~"Irrigation",
                                               grepl("liv",class,ignore.case = T)~"Livestock",
                                               grepl("mfg",class,ignore.case = T)~"Manufacturing",
                                               grepl("min",class,ignore.case = T)~"Mining",
                                               grepl("nonag",class,ignore.case = T)~"Non Agriculture",
                                               grepl("total",class,ignore.case = T)~"Total",
                                               TRUE~class))

        tethysScenariosX<-unique(c(tethysScenario_i,unique(gridx$scenario)))
        tethysGCMRCP<-gridx %>%
          dplyr::select(scenarioMultiA,scenarioMultiB) %>% dplyr::distinct()
        tethysGCMRCPs<-dplyr::bind_rows(tethysGCMRCPs,tethysGCMRCP)
        tethysGCMRCPs<-tethysGCMRCPs[stats::complete.cases(tethysGCMRCPs),]
        tethysYears<-unique(gridx$x)

        colsSelect <- names(gridx)[names(gridx) %in% c( "lon","lat","region","scenarioMultiA","scenarioMultiB","scenario",
                                           "param","units","aggType","classPalette","class","x","value")]
        gridx <- gridx %>% dplyr::select(colsSelect) %>% dplyr::ungroup()
        gridx<-addMissing(gridx) %>% dplyr::filter(!is.na(x)); gridx

        if(!is.null(filterYears)){gridx <- gridx %>% dplyr::filter(x %in% filterYears)}

        tethysFile_i<-gsub(".csv","",tethysFile_i)

        if(saveFormat=="rds"){
          saveRDS(gridx,paste(dir,"/tethys_",tethysFolderScen$scenario[i],"_",tethysFile_i,"_",min(gridx$x),"to",max(gridx$x),".rds",sep=""))
          print(paste("Saving file as: ",dir,"/tethys_",tethysFolderScen$scenario[i],"_",tethysFile_i,"_",min(gridx$x),"to",max(gridx$x),".rds",sep=""))

        }
        if(saveFormat=="csv"){
          data.table::fwrite(gridx,paste(dir,"/tethys_",tethysFolderScen$scenario[i],"_",tethysFile_i,"_",min(gridx$x),"to",max(gridx$x),".csv",sep=""))
          print(paste("Saving file as: ",dir,"/tethys_",tethysFolderScen$scenario[i],"_",tethysFile_i,"_",min(gridx$x),"to",max(gridx$x),".csv",sep=""))

        }
        if(saveFormat=="both"){
          saveRDS(gridx,paste(dir,"/tethys_",tethysFolderScen$scenario[i],"_",tethysFile_i,"_",min(gridx$x),"to",max(gridx$x),".rds",sep=""))
          data.table::fwrite(gridx,paste(dir,"/tethys_",tethysFolderScen$scenario[i],"_",tethysFile_i,"_",min(gridx$x),"to",max(gridx$x),".csv",sep=""))
          print(paste("Saving file as: ",dir,"/tethys_",tethysFolderScen$scenario[i],"_",tethysFile_i,"_",min(gridx$x),"to",max(gridx$x),".rds",sep=""))
          print(paste("Saving file as: ",dir,"/tethys_",tethysFolderScen$scenario[i],"_",tethysFile_i,"_",min(gridx$x),"to",max(gridx$x),".csv",sep=""))

        }
        paramScenarios <- paramScenarios%>%
          dplyr::bind_rows(gridx[1,]%>%dplyr::select(param,scenario)%>%unique()) %>%
          dplyr::ungroup()%>%
          dplyr::select(param,scenario)%>%
          unique();paramScenarios

        rm(gridx)

      } # Close if tethys file exists
    } # close tethys file loops
  }}}else{print("Tethys scenarios not specified")}
  } # Close tethys folder
}} # Closing grid Area files

#----------------
# Prepare Xanthos Files
#---------------


if(!file.exists(xanthosGridAreaHecsPath)){
  print(paste("xanthos grid Area path: ", xanthosGridAreaHecsPath ," is incorrect or doesn't exist.",sep=""))
  print(paste("Skipping xanthos runs",sep=""))}else {


if(!file.exists(xanthosCoordinatesPath)){
  print(paste("xanthos coordinate path: ", xanthosCoordinatesPath ," is incorrect or doesn't exist.",sep=""))
  print(paste("Skipping xanthos runs",sep=""))}else {


if(!is.null(xanthosFiles)){

      if(!is.null(xanthosScenarios)){
        xanthosFilesScen <- data.frame("file"=c(xanthosFiles),"scenario"=c(xanthosScenarios))
        print("The following file scenarios found for Xanthos.")
        print(xanthosFilesScen)

for(i in 1:nrow(xanthosFilesScen)){

if(!file.exists(paste(xanthosFilesScen$file[i],sep=""))){
  print(paste("xanthos file: ", xanthosFilesScen$file[i] ," is incorrect or doesn't exist.",sep=""))
  print(paste("Skipping xanthos runs",sep=""))}else {

    xanthosScenariosx<-character()
    xanthosGCMRCPs<-tibble::tibble()
    xanthosYears<-numeric()

    xanthosFile_i <- xanthosFilesScen$file[i]

      if(!grepl(".csv",xanthosFile_i)){xanthosFile_i=paste(xanthosFile_i,".csv",sep="")}

      if(!file.exists(paste(xanthosFile_i,sep=""))){
        print(paste("xanthos file: ", xanthosFile_i," is incorrect or doesn't exist.",sep=""))
        print(paste("Skipping file: ",xanthosFile_i,sep=""))
      }else{

        xanthosCoords<-data.table::fread(xanthosCoordinatesPath, header=F,encoding="Latin-1");
        xanthosCoords<-xanthosCoords%>%dplyr::rename(lon=V2,lat=V3)%>%dplyr::select(lon,lat)
        xanthosGridArea<-data.table::fread(xanthosGridAreaHecsPath, header=F,encoding="Latin-1");
        xanthosGridArea<-xanthosGridArea%>%dplyr::rename(Area_hec=V1)%>%dplyr::mutate(Area_km2=0.01*Area_hec)%>%
          dplyr::select(Area_hec,Area_km2)

        print(paste("Reading xanthos data file: ",xanthosFile_i,"...",sep=""))
        gridx<-data.table::fread(paste(xanthosFile_i,sep=""), header=T,stringsAsFactors = F,encoding="Latin-1")%>%
          tibble::as_tibble()%>%dplyr::select(-id)
        print(paste("Xanthos data file: ",xanthosFile_i," read.",sep=""))

        names(gridx)<-gsub("X","",names(gridx))

        if(nrow(gridx)!=nrow(xanthosCoords)){
          stop(paste("Rows in xanthos file: ", xanthosFile_i,
                     " not equal to rows in xanthos coords file: ",
                     xanthosCoordinatesPath,sep=""))}

        if(nrow(gridx)!=nrow(xanthosGridArea)){
          stop(paste("Rows in xanthos file: ", xanthosFile_i,
                     " not equal to rows in xanthos coords file: ",
                     xanthosCoordinatesPath,sep=""))}


        if(grepl("km3",xanthosFile_i)){
          print(paste("Based on xanthos file name: ", xanthosFile_i, " has km3 data. Converting to mm...", sep=""))
        gridx<-gridx/(xanthosGridArea$Area_km2/1000000)
        gridx[gridx<0]=0
        gridx<-dplyr::bind_cols(xanthosCoords,gridx)
        xanthosUnits="Runoff (mm)"
        print(paste("km3 data converted to mm.", sep=""))
        }else{
          print(paste("Based on xanthos filename: ", xanthosFile_i, " has mm data. Using mm.", sep=""))
          gridx<-dplyr::bind_cols(xanthosCoords,gridx)}

        if(grepl("pm_abcd_mrtm",xanthosFile_i)){
        xanthosScenario<-sub("^.*pm_abcd_mrtm_", "", xanthosFile_i);xanthosScenario
        xanthosScenario<-sub("\\_[0-9].*", "", xanthosScenario);xanthosScenario
        xanthosGCM<-sub("_.*","",xanthosScenario); xanthosGCM
        xanthosRCP<-sub(".*_","",xanthosScenario); xanthosRCP}else{
          if(grepl("q_km3peryear_",xanthosFile_i)){
            xanthosScenario<-sub(".*q_km3peryear_", "", xanthosFile_i);xanthosScenario
            xanthosScenario<-sub("\\_[0-9].*", "", xanthosScenario);xanthosScenario
            xanthosGCM<-sub("_.*","",xanthosScenario); xanthosGCM
            xanthosRCP<-sub(".*_","",xanthosScenario); xanthosRCP}else{
          xanthosScenario<-xanthosScenarioAssign
          xanthosGCM=NA;xanthosRCP=NA
        }}


        if(grepl("mm",xanthosUnits)){aggType="depth"}else{aggType="vol"}
        print(paste("Gathering data for xanthos filename: ", xanthosFile_i, " into year columns...", sep=""))
        gridx<-gridx%>%dplyr::mutate(
                        scenarioMultiA=xanthosGCM,
                        scenarioMultiB=xanthosRCP,
                        scenario=paste(xanthosScenario,sep="_"),
                        param="xanthosRunoff",
                        units=xanthosUnits,
                        aggType=aggType,
                        classPalette="pal_wet",
                        class="Runoff")%>%
          tidyr::gather(key="x",value="value",
                        -c("lat","lon","scenario","scenarioMultiA","scenarioMultiB","aggType","param","units","classPalette","class"))%>%
          tibble::as_tibble()
        print(paste("Data for xanthos file gathered into columns.", sep=""))

        gridx$x<-as.numeric(gridx$x)

        xanthosScenariosx<-unique(c(xanthosScenariosx,unique(gridx$scenario)))
        xanthosGCMRCP<-gridx %>%
                       dplyr::select(scenarioMultiA,scenarioMultiB) %>% dplyr::distinct()
        xanthosGCMRCPs<-dplyr::bind_rows(xanthosGCMRCPs,xanthosGCMRCP)
        xanthosGCMRCPs<-xanthosGCMRCPs[stats::complete.cases(xanthosGCMRCPs),]
        xanthosYears<-unique(gridx$x)

        # Apply Lowess dplyr::filter
        # https://stat.ethz.ch/pipermail/bioconductor/2003-September/002337.html
        # https://www.rdocumentation.org/packages/gplots/versions/3.0.1/topics/lowess


        print(paste("Applying lowess dplyr::filter to file: ", xanthosFile_i, " using lowess span of ",spanLowess,"...", sep=""))
        gridx <- gridx %>%
          dplyr::group_by(lat,lon,scenario,param,units,aggType,classPalette,class) %>%
          dplyr::arrange(lat,lon) %>%
          dplyr::mutate(lowess = stats::lowess(y=value, x=x, f=spanLowess )$y)
        print(paste("Lowess dplyr::filter applied.", sep=""))


        if(diagnosticsOn){ # Close Diagnostics
        for(j in c(1,5,40,100,149,180)){
        gridC<-gridx[(gridx$lat==unique(gridx$lat)[j] & gridx$lon==unique(gridx$lon)[j]),]
        fname=paste(unique(gridC$scenario),"_",unique(gridC$param),
                    "_lat",unique(gridC$lat),"_lon", unique(gridC$lon),
                    "_lowessSpan",spanLowess,sep="")
        graphics::plot(gridC$x,gridC$value,type="l",
                                        main=paste(unique(gridC$scenario),
                                                   "\nlat = ",unique(gridC$lat),", lon = ", unique(gridC$lon),
                                                   ", Lowess Span =  ",spanLowess,sep=""),
                                        ylab=unique(gridC$units),xlab="Year") +
        graphics::lines(gridC$x,gridC$lowess,type="l",col="red")
        diagnosticFig <- grDevices::recordPlot()
        metis.printPdfPng(figure=diagnosticFig,
                          dir=dirDiagnostic,filename=fname,figWidth=9,figHeight=7,pdfpng="png")
        }
      } # Close Diagnostic

        gridx<-gridx%>%dplyr::mutate(value=lowess,region="region")%>%dplyr::select(-lowess)

        colsSelect <- names(gridx)[names(gridx) %in% c( "lon","lat","region","scenarioMultiA","scenarioMultiB","scenario",
                                                        "param","units","aggType","classPalette","class","x","value")]
        gridx <- gridx %>% dplyr::select(colsSelect) %>% dplyr::ungroup()
        gridx<-addMissing(gridx); gridx

        if(!is.null(filterYears)){gridx <- gridx %>% dplyr::filter(x %in% filterYears)}

        if(nrow(gridx>0)){
        x10Chunks <- split(unique(gridx$x), ceiling(seq_along(unique(gridx$x))/25))

        for(j in 1:length(x10Chunks)){

          x_temp <-x10Chunks[[j]]
          gridxSub <- gridx%>%dplyr::filter(x %in% x_temp)

        if(saveFormat=="rds"){
          saveRDS(gridxSub,paste(dir,"/xanthos_",xanthosFilesScen$scenario[i],"_",min(x_temp),"to",max(x_temp),".rds",sep=""))
          print(paste("Saving file as: ",dir,"/xanthos_",xanthosFilesScen$scenario[i],"_",min(x_temp),"to",max(x_temp),".rds",sep=""))
        }
        if(saveFormat=="csv"){
          data.table::fwrite(gridxSub,paste(dir,"/xanthos_",xanthosFilesScen$scenario[i],"_",min(x_temp),"to",max(x_temp),".csv",sep=""))
          print(paste("Saving file as: ",dir,"/xanthos_",xanthosFilesScen$scenario[i],"_",min(x_temp),"to",max(x_temp),".csv",sep=""))
        }
        if(saveFormat=="both"){
          saveRDS(gridxSub,paste(dir,"/xanthos_",xanthosFilesScen$scenario[i],"_",min(x_temp),"to",max(x_temp),".rds",sep=""))
          data.table::fwrite(gridxSub,paste(dir,"/xanthos_",xanthosFilesScen$scenario[i],"_",min(x_temp),"to",max(x_temp),".csv",sep=""))
          print(paste("Saving file as: ",dir,"/xanthos_",xanthosFilesScen$scenario[i],"_",min(x_temp),"to",max(x_temp),".rds",sep=""))
          print(paste("Saving file as: ",dir,"/xanthos_",xanthosFilesScen$scenario[i],"_",min(x_temp),"to",max(x_temp),".csv",sep=""))
        }
        }

        paramScenarios <- paramScenarios%>%
          dplyr::bind_rows(gridx[1,]%>%dplyr::select(param,scenario)%>%unique()) %>%
          dplyr::ungroup()%>%
          dplyr::select(param,scenario)%>%
          unique();paramScenarios
        }else{"No data for Xanthos for chosen parameters. (Check filterYears)."}
        rm(gridx)

      } # Close if xanthos file exists
  }
  } # Close xanthos folder
}}} # close If xanthosCoords path exists
} # close If xanthosGridAreaHecsPath path exists



#----------------
# Prepare Gridded Scarcity
#---------------

if(T){
# Read in Tethys and Xanthos Data
  # List files in dir
  #tethysFilesScarcity=NULL;xanthosFilesScarcity=NULL
  if(is.null(tethysFilesScarcity)){tethysFilesx<-list.files(dir)[grepl("tethys",list.files(dir)) & grepl("total",list.files(dir))]}else{
    tethysFilesx<-tethysFilesScarcity}; tethysFilesx
  if(saveFormat=="rds" | saveFormat=="both"){tethysFilesx <- tethysFilesx[grepl("rds",tethysFilesx)]}
  if(saveFormat=="csv"){tethysFilesx <- tethysFilesx[grepl("csv",tethysFilesx)]}; tethysFilesx
  if(is.null(xanthosFilesScarcity)){xanthosFilesx<-list.files(dir)[grepl("xanthos",list.files(dir))]}else{
    xanthosFilesx<-xanthosFilesScarcity}; xanthosFilesx
  if(saveFormat=="rds" | saveFormat=="both"){xanthosFilesx <- xanthosFilesx[grepl("rds",xanthosFilesx)]}
  if(saveFormat=="csv"){xanthosFilesx <- xanthosFilesx[grepl("csv",xanthosFilesx)]}; xanthosFilesx

  print(paste("Tethys files include: ",paste(tethysFilesx,collapse=", "),sep=""))
  print(paste("Xanthos files include: ",paste(xanthosFilesx,collapse=", "),sep=""))
  print(paste("Total combinations are: ", length(tethysFilesx)*length(xanthosFilesx)))

  count=0;

  for(xanthosFile_i in xanthosFilesx){
    for(tethysFile_i in tethysFilesx){

      tethysStartYear <- as.integer(stringr::str_sub(tethysFile_i,-14,-11));tethysStartYear
      tethysEndYear <- as.integer(stringr::str_sub(tethysFile_i,-8,-5));tethysEndYear
      xanthosStartYear <- as.integer(stringr::str_sub(xanthosFile_i,-14,-11));xanthosStartYear
      xanthosEndYear <- as.integer(stringr::str_sub(xanthosFile_i,-8,-5));xanthosEndYear
      if(any(c(tethysStartYear:tethysEndYear) %in% c(xanthosStartYear:xanthosEndYear))){

      count=count+1;

      print(paste("Calculating scarcity for combination",sep=""))
      print(paste("Xanthos file: ",xanthosFile_i," and tethys file: ",tethysFile_i,sep=""))
      if(grepl(".csv",xanthosFile_i)){
      x <- data.table::fread(paste(dir,"/",xanthosFile_i,sep="")) %>% dplyr::filter(grepl("xanthos",param))}
      if(grepl(".rds",xanthosFile_i)){
        x <- readRDS(paste(dir,"/",xanthosFile_i,sep="")) %>% dplyr::filter(grepl("xanthos",param))};
      if(grepl(".csv",tethysFile_i)){
        t <- data.table::fread(paste(dir,"/",tethysFile_i,sep="")) %>% dplyr::filter(grepl("tethys",param))}
      if(grepl(".rds",tethysFile_i)){
        t <- readRDS(paste(dir,"/",tethysFile_i,sep="")) %>% dplyr::filter(grepl("tethys",param))};
      xGCM<-paste(unique(x$scenarioMultiA),sep="");xRCP<-paste(unique(x$scenarioMultiB),sep="")
      t1 <- t %>% tibble::as_tibble() %>%
        dplyr::mutate(scenarioMultiA=as.character(scenarioMultiA),scenarioMultiB=as.character(scenarioMultiB),
          scenarioMultiA=dplyr::case_when(is.na(scenarioMultiA)~xGCM,
                                TRUE~scenarioMultiA),
          scenarioMultiB=dplyr::case_when(is.na(scenarioMultiB)~xRCP,
                                TRUE~scenarioMultiB))
      for(col_i in names(x)){class(t1[[col_i]])<-class(x[[col_i]])}
      if(unique(x$scenarioMultiA)==unique(t1$scenarioMultiA) & unique(x$scenarioMultiB)==unique(t1$scenarioMultiB)){
        commonyears <- unique(x$x)[unique(x$x) %in% unique(t$x)]
        s <- x %>% dplyr::filter(x %in% commonyears) %>% dplyr::bind_rows(t1 %>% dplyr::filter(x %in% commonyears)) %>% tibble::as_tibble();s
        s1 <- s %>% dplyr::select(lon,lat,scenario,scenarioMultiA,scenarioMultiB,param,units,aggType,classPalette,class,x,value,region,class2)%>%
        dplyr::mutate(scenario=paste(scenario,"_",param,sep=""))%>%
        dplyr::select(-param,-units,-class,-class2,-classPalette)%>%dplyr::filter(!is.na(x));s1
      s2 <- s1 %>% tidyr::spread(key="scenario",value="value");s2 %>% as.data.frame() %>% utils::head()
      scarcityScen <- paste("X",
                            gsub("_xanthosRunoff","",paste(unique(s1$scenario)[grepl("xanthos",unique(s1$scenario))],sep="")),
                            "T",
                            gsub("_tethysWatWithdraw_total","",paste(unique(s1$scenario)[grepl("tethys",unique(s1$scenario))],sep="")),
                            sep=""); scarcityScen
      s3 <- s2 %>%
        dplyr::mutate(!!(rlang::sym("value")):=!!(rlang::sym(unique(s1$scenario)[grepl("tethys",unique(s1$scenario))]))/
                        !!(rlang::sym(unique(s1$scenario)[grepl("xanthos",unique(s1$scenario))])),
                      scenario=scarcityScen)%>%
        dplyr::filter(!is.na(value))%>%
        dplyr::select(-!!(rlang::sym(unique(s1$scenario)[grepl("xanthos",unique(s1$scenario))])),
                      -!!(rlang::sym(unique(s1$scenario)[grepl("tethys",unique(s1$scenario))])))%>%
        dplyr::mutate(param="griddedScarcity",
                      units="Gridded Scarcity (Ratio)",
                      class="class",
                      class2="class2",
                      classPalette="pal_ScarcityCat");

      if(!is.null(filterYears)){s3 <- s3 %>% dplyr::filter(x %in% filterYears)}


      if(saveFormat=="rds"){
        saveRDS(s3,paste(dir,"/griddedScarcity_",scarcityScen,"_",min(s3$x),"to",max(s3$x),".rds",sep=""))
        print(paste("Saving file as: ",dir,"/griddedScarcity_",scarcityScen,"_",min(s3$x),"to",max(s3$x),".rds",sep=""))
      }
      if(saveFormat=="csv"){
        data.table::fwrite(s3,paste(dir,"/griddedScarcity_",scarcityScen,"_",min(s3$x),"to",max(s3$x),".csv",sep=""))
        print(paste("Saving file as: ",dir,"/griddedScarcity_",scarcityScen,"_",min(s3$x),"to",max(s3$x),".csv",sep=""))
      }
      if(saveFormat=="both"){
        saveRDS(s3,paste(dir,"/griddedScarcity_",scarcityScen,"_",min(s3$x),"to",max(s3$x),".rds",sep=""))
        data.table::fwrite(s3,paste(dir,"/griddedScarcity_",scarcityScen,"_",min(s3$x),"to",max(s3$x),".csv",sep=""))
        print(paste("Saving file as: ",dir,"/griddedScarcity_",scarcityScen,"_",min(s3$x),"to",max(s3$x),".rds",sep=""))
        print(paste("Saving file as: ",dir,"/griddedScarcity_",scarcityScen,"_",min(s3$x),"to",max(s3$x),".csv",sep=""))
      }

      paramScenarios <- paramScenarios%>%
        dplyr::bind_rows(s3[1,]%>%dplyr::select(param,scenario)%>%unique()) %>%
        dplyr::ungroup()%>%
        dplyr::select(param,scenario)%>%
        unique();paramScenarios

     }else{print("Xanthos/Tethys GCM RCP's not the same so skipping...")}
      }else{print("No common years in xanthos and tethys files being run.")} # Close if common years
    }
  }
}

#----------------
# Prepare gridded Population
#---------------

if(!is.null(popFolder)){
if(!dir.exists(popFolder)){

  print(paste("pop folder: ", popFolder ," is incorrect or doesn't exist.",sep=""))
  print(paste("Skipping pop runs",sep=""))}else {

    count=0;
    for(popFile_i in popFiles){

      count=count+1;

      popFile_i=gsub(".csv","",popFile_i)
      if(!grepl(".csv",popFile_i)){popFile_i=paste(popFile_i,".csv",sep="")}

      if(!file.exists(paste(popFolder,"/",popFile_i,sep=""))){
        print(paste("pop file: ", popFolder,"/",popFile_i," is incorrect or doesn't exist.",sep=""))
        print(paste("Skipping file: ",popFolder,"/",popFile_i,sep=""))
      }else{
        print(paste("Reading population data file: ",popFile_i,"...",sep=""))

        gridx<-data.table::fread(paste(popFolder,"/",popFile_i,sep=""),encoding="Latin-1")%>%
          tibble::as_tibble()%>%dplyr::select(lon,lat,dplyr::contains("popGWP"))%>%
          tidyr::gather(key="key",value="value",-c("lat","lon"))%>%
          tidyr::separate(col="key",into=c("scenario","x"),sep="_")%>%
          dplyr::mutate(param="population",
                        units=popUnits,
                        aggType="vol",
                        classPalette="pal_hot",
                        class="class",
                        region="region")
        gridx$x<-as.numeric(gridx$x)

        colsSelect <- names(gridx)[names(gridx) %in% c( "lon","lat","region","scenarioMultiA","scenarioMultiB","scenario",
                                                        "param","units","aggType","classPalette","class","x","value")]
        gridx <- gridx %>% dplyr::select(colsSelect) %>% dplyr::ungroup()
        gridx<-addMissing(gridx); gridx

        if(!is.null(filterYears)){gridx <- gridx %>% dplyr::filter(x %in% filterYears)}

        print("File read.")

        if(saveFormat=="rds"){
          saveRDS(gridx,paste(dir,"/pop_",count,"_",min(gridx$x),"to",max(gridx$x),".rds",sep=""))
          print(paste("Saving file as: ",dir,"/pop_",count,"_",min(gridx$x),"to",max(gridx$x),".rds",sep=""))
        }
        if(saveFormat=="csv"){
          data.table::fwrite(gridx,paste(dir,"/pop_",count,"_",min(gridx$x),"to",max(gridx$x),".csv",sep=""))
          print(paste("Saving file as: ",dir,"/pop_",count,"_",min(gridx$x),"to",max(gridx$x),".csv",sep=""))
        }
        if(saveFormat=="both"){
          saveRDS(gridx,paste(dir,"/pop_",count,".rds",sep=""))
          data.table::fwrite(gridx,paste(dir,"/pop_",count,"_",min(gridx$x),"to",max(gridx$x),".csv",sep=""))
          print(paste("Saving file as: ",dir,"/pop_",count,"_",min(gridx$x),"to",max(gridx$x),".rds",sep=""))
          print(paste("Saving file as: ",dir,"/pop_",count,"_",min(gridx$x),"to",max(gridx$x),".csv",sep=""))
        }

        paramScenarios <- paramScenarios%>%
          dplyr::bind_rows(gridx[1,]%>%dplyr::select(param,scenario)%>%unique()) %>%
          dplyr::ungroup()%>%
          dplyr::select(param,scenario)%>%
          unique();paramScenarios
        rm(gridx)

      } # Close if pop file exists
    } # close pop file loops
  } # Close pop folder
}

#----------------
# Prepare gridded Electricity generation and capacity from Bia
#---------------

if(!is.null(biaFolder)){
if(!dir.exists(biaFolder)){

  print(paste("bia folder: ", biaFolder ," is incorrect or doesn't exist.",sep=""))
  print(paste("Skipping bia runs",sep=""))
  }else {

    for(biaFile_i in biaFiles){

      biaFile_i=gsub(".csv","",biaFile_i)
      if(!grepl(".csv",biaFile_i)){biaFile_i=paste(biaFile_i,".csv",sep="")}

      if(!file.exists(paste(biaFolder,"/",biaFile_i,sep=""))){
        print(paste("bia file: ", biaFolder,"/",biaFile_i," is incorrect or doesn't exist.",sep=""))
        print(paste("Skipping file: ",biaFolder,"/",biaFile_i,sep=""))
      }else{
        print(paste("Reading bia data file: ",biaFile_i,"...",sep=""))

        gridx<-data.table::fread(paste(biaFolder,"/",biaFile_i,sep=""),encoding="Latin-1")%>%
          tibble::as_tibble()%>%
          dplyr::select(-value, -origValue)%>%
          dplyr::mutate(aggType = "vol")%>%
          dplyr::rename(lat = gridlat, lon = gridlon, class = class1, value = valueDistrib, origValue = origValueDistrib) %>%
          dplyr::select(-gridCellPercentage,-region_32_code,-ctry_name,-ctry_code, -aggregate, -dplyr::contains("orig"),-gridID)
        gridx$x<-as.numeric(gridx$x)

        colsSelect <- names(gridx)[names(gridx) %in% c( "lon","lat","region","scenarioMultiA","scenarioMultiB","scenario",
                                                        "param","units","aggType","classPalette","class","x","value")]
        gridx <- gridx %>% dplyr::select(colsSelect) %>% dplyr::ungroup()
        gridx<-addMissing(gridx); gridx

        if(!is.null(filterYears)){gridx <- gridx %>% dplyr::filter(x %in% filterYears)}

        print("File read.")


        if(saveFormat=="rds"){
          saveRDS(gridx,paste(dir,"/bia_",biaFile_i,"_",min(gridx$x),"to",max(gridx$x),".rds",sep=""))
          print(paste("Saving file as: ",dir,"/bia_",biaFile_i,"_",min(gridx$x),"to",max(gridx$x),".rds",sep=""))
        }
        if(saveFormat=="csv"){
          data.table::fwrite(gridx,paste(dir,"/bia_",biaFile_i,"_",min(gridx$x),"to",max(gridx$x),".csv",sep=""))
          print(paste("Saving file as: ",dir,"/bia_",biaFile_i,"_",min(gridx$x),"to",max(gridx$x),".csv",sep=""))
        }
        if(saveFormat=="both"){
          saveRDS(gridx,paste(dir,"/bia_",biaFile_i,".rds",sep=""))
          data.table::fwrite(gridx,paste(dir,"/bia_",biaFile_i,"_",min(gridx$x),"to",max(gridx$x),".csv",sep=""))
          print(paste("Saving file as: ",dir,"/bia_",biaFile_i,"_",min(gridx$x),"to",max(gridx$x),".rds",sep=""))
          print(paste("Saving file as: ",dir,"/bia_",biaFile_i,"_",min(gridx$x),"to",max(gridx$x),".csv",sep=""))
        }

        paramScenarios <- paramScenarios%>%
            dplyr::bind_rows(gridx[1,]%>%dplyr::select(param,scenario)%>%unique()) %>%
            dplyr::ungroup()%>%
            dplyr::select(param,scenario)%>%
            unique();paramScenarios
        rm(gridx)

      } # Close if bia file exists
    } # close bia file loops
  } # Close bia folder
} # Close if biaFolder is null



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



# Test Unique Values
#a<-gridMetis%>%tidyr::unite(col="key",names(gridMetis)[!names(gridMetis) %in% c("lat","lon","value")],sep="_",remove=T)
#a<-a%>%tidyr::spread(key=key,value=value)

#--------------
# Save RData and csv.
#----------------

print("Saving paramScenarios to: ")
data.table::fwrite(paramScenarios %>% dplyr::select(param,scenario) %>% unique(),paste(dir,"/paramScenarios.csv",sep=""))
print(paramScenarios)


return(paramScenarios)


} # Close Function
