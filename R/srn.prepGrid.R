#' srn.prepGrid
#'
#' This function prepares gridded data for use with other srn modules.
#' @param demeterFolder Full path to demeter outputs
#' @param demeterScenario Name of demeter scenario
#' @param demeterTimesteps Default is seq(from=2005,to=2100,by=5)
#' @param tethysFolder Folder for tethys results
#' @param tethysScenario Scenario name for tethys run
#' @param tethysFiles Default =c("wddom","wdelec","wdirr","wdliv","wdmfg","wdmin","wdnonag","wdtotal"),
#' @param tethysUnits No Default
#' @param demeterUnits No Default
#' @param xanthosFolder Xanthos Folder Path
#' @param xanthosScenario Xanthos Scenario Name
#' @param xanthosUnits Xanthos Untis
#' @param xanthosFiles Xanthos Files to Read
#' @param xanthosCoordinatesPath paste(getwd(),"/dataFiles/grids/xanthosCoords/coordinates.csv",sep="")
#' @param scarcityXanthosRollMeanWindow Default = 10,
#' @param dirOutputs Default =paste(getwd(),"/outputs",sep=""),
#' @param reReadData Default =1,
#' @param gridSRNData Default = paste(dirOutputs, "/Grids/gridSRN.RData", sep = "")
#' @return A table with data by polygon ID for each shapefile provided
#' @keywords gcam, gcam database, query
#' @export

srn.prepGrid<- function(demeterFolder,
                        demeterScenario,
                        demeterTimesteps=seq(from=2005,to=2100,by=5),
                        demeterUnits,
                        tethysFolder,
                        tethysScenario,
                        tethysUnits,
                        tethysFiles=c("wddom","wdelec","wdirr","wdliv","wdmfg","wdmin","wdnonag","wdtotal"),
                        xanthosFolder,
                        xanthosScenario,
                        xanthosUnits,
                        xanthosFiles,
                        xanthosCoordinatesPath=paste(getwd(),"/dataFiles/grids/xanthosCoords/coordinates.csv",sep=""),
                        scarcityXanthosRollMeanWindow=10,
                        dirOutputs=paste(getwd(),"/outputs",sep=""),
                        reReadData=1,
                        gridSRNData=paste(dirOutputs, "/Grids/gridSRN.RData", sep = "")
                        ){

#------------------
# Load required Libraries
# -----------------
requireNamespace("tibble",quietly = T)
requireNamespace("dplyr",quietly = T)
requireNamespace("utils",quietly = T)
requireNamespace("zoo",quietly = T)

#----------------
# Initialize variables by setting to NULL
#----------------

NULL -> lat -> lon -> latitude -> longitude -> aez_id -> region_id ->X..ID->
  ilon->ilat->param->V2->V3->scenario->classPalette->rollingMean->x->scarcity->value

#------------------
# Create Folders if needed
#------------------
if (!dir.exists(dirOutputs)){
    dir.create(dirOutputs)}
if (!dir.exists(paste(dirOutputs, "/Grids", sep = ""))){
    dir.create(paste(dirOutputs, "/Grids", sep = ""))}

if(reReadData==1){

gridSRN<-tibble::tibble()

#----------------
# Prepare Demeter Files
#---------------

if(!dir.exists(demeterFolder)){
  print(paste("Demeter folder: ", demeterFolder ," is incorrect or doesn't exist.",sep=""))
  print(paste("Skipping demeter runs",sep=""))}else {

for(timestepx in demeterTimesteps){

if(!file.exists(paste(demeterFolder,"/landcover_",timestepx,"_timestep.csv",sep=""))){
  print(paste("Demeter file: ", demeterFolder,"/landcover_",timestepx,"_timestep.csv is incorrect or doesn't exist.",sep=""))
  print(paste("Skipping file: ",demeterFolder,"/landcover_",timestepx,"_timestep.csv",sep=""))
}else{
gridx<-utils::read.csv(paste(demeterFolder,"/landcover_",timestepx,"_timestep.csv",sep=""), stringsAsFactors = F)%>%
  tibble::as_tibble()%>%
  dplyr::mutate(lat=latitude,lon=longitude,
         scenario=demeterScenario,
         param="demeterLandUse",
         units=demeterUnits,
         aggType="depth",
         x=timestepx,
         classPalette="pal_green")%>%
  dplyr::select(-aez_id,-region_id,-longitude,-latitude)%>%
  tidyr::gather(key="class",value="value",-c("lat","lon","scenario","aggType","param","units","x","classPalette"))

gridSRN<-dplyr::bind_rows(gridSRN,gridx)

} # Close if demeter file exists
} # close demeter file loops
} # Close Demeter folder


#----------------
# Prepare Tethys Files
#---------------

if(!dir.exists(tethysFolder)){
  print(paste("tethys folder: ", tethysFolder ," is incorrect or doesn't exist.",sep=""))
  print(paste("Skipping tethys runs",sep=""))}else {

    for(tethysFile_i in tethysFiles){

      class_i=gsub(".csv","",tethysFile_i)
      if(!grepl(".csv",tethysFile_i)){tethysFile_i=paste(tethysFile_i,".csv",sep="")}

      if(!file.exists(paste(tethysFolder,"/",tethysFile_i,sep=""))){
        print(paste("tethys file: ", tethysFolder,"/",tethysFile_i," is incorrect or doesn't exist.",sep=""))
        print(paste("Skipping file: ",tethysFolder,"/",tethysFile_i,sep=""))
      }else{
        gridx<-utils::read.csv(paste(tethysFolder,"/",tethysFile_i,sep=""), stringsAsFactors = F)%>%
          tibble::as_tibble()%>%dplyr::select(-X..ID,-ilon,-ilat)
        names(gridx)<-gsub("X","",names(gridx))
        if(grepl("mm",tethysUnits)){aggType="depth"}else{aggType="vol"}
        gridx<-gridx%>%dplyr::select(-dplyr::contains("Unit"))
        gridx<-gridx%>%
              dplyr::mutate(lat=lat,lon=lon,
                            scenario=tethysScenario,
                            param="tethysWatWithdraw",
                            units=tethysUnits,
                            aggType=aggType,
                            classPalette="pal_wet",
                            class=class_i)%>%
               tidyr::gather(key="x",value="value",-c("lat","lon","scenario","aggType","param","units","classPalette","class"))

        gridx$x<-as.numeric(gridx$x)

        gridSRN<-dplyr::bind_rows(gridSRN,gridx)

      } # Close if tethys file exists
    } # close tethys file loops
  } # Close tethys folder

gridSRN<-gridSRN%>%
  dplyr::mutate(class=dplyr::case_when(grepl("wddom",class)~"Domestic",
                  grepl("elec",class)~"Electric",
                  grepl("irr",class)~"Irrigation",
                  grepl("liv",class)~"Livestock",
                  grepl("mfg",class)~"Manufacturing",
                  grepl("min",class)~"Mining",
                  grepl("nonag",class)~"Non Agriculture",
                  grepl("total",class)~"Total",
                        TRUE~class))

#----------------
# Prepare Xanthos Files
#---------------

if(!file.exists(xanthosCoordinatesPath)){
  print(paste("xanthos coordinate path: ", xanthosCoordinatesPath ," is incorrect or doesn't exist.",sep=""))
  print(paste("Skipping xanthos runs",sep=""))}else {

if(!dir.exists(xanthosFolder)){
  print(paste("xanthos folder: ", xanthosFolder ," is incorrect or doesn't exist.",sep=""))
  print(paste("Skipping xanthos runs",sep=""))}else {


    for(xanthosFile_i in xanthosFiles){

      if(!grepl(".csv",xanthosFile_i)){xanthosFile_i=paste(xanthosFile_i,".csv",sep="")}

      if(!file.exists(paste(xanthosFolder,"/",xanthosFile_i,sep=""))){
        print(paste("xanthos file: ", xanthosFolder,"/",xanthosFile_i," is incorrect or doesn't exist.",sep=""))
        print(paste("Skipping file: ",xanthosFolder,"/",xanthosFile_i,sep=""))
      }else{

        xanthosCoords<-utils::read.csv(xanthosCoordinatesPath, header=F, stringsAsFactors = F);
        xanthosCoords<-xanthosCoords%>%dplyr::rename(lon=V2,lat=V3)%>%dplyr::select(lon,lat)
        gridx<-utils::read.csv(paste(xanthosFolder,"/",xanthosFile_i,sep=""), header=F,stringsAsFactors = F)%>%
          tibble::as_tibble()
        names(gridx)<-c(1950:2005)

        if(nrow(gridx)!=nrow(xanthosCoords)){
          stop(paste("Rows in xanthos file: ", xanthosFolder,"/",xanthosFile_i,
                     " not equal to rows in xanthos coords file: ",
                     xanthosCoordinatesPath,sep=""))}

        gridx<-dplyr::bind_cols(xanthosCoords,gridx)
        if(grepl("mm",xanthosUnits)){aggType="depth"}else{aggType="vol"}
        gridx<-gridx%>%
          dplyr::mutate(lat=lat,lon=lon,
                        scenario=xanthosScenario,
                        param="xanthosRunoff",
                        units=xanthosUnits,
                        aggType=aggType,
                        classPalette="pal_wet",
                        class="Runoff")%>%
          tidyr::gather(key="x",value="value",-c("lat","lon","scenario","aggType","param","units","classPalette","class"))

        gridx$x<-as.numeric(gridx$x)

        gridSRN<-dplyr::bind_rows(gridSRN,gridx)

      } # Close if xanthos file exists
    } # close xanthos file loops
  } # Close xanthos folder
} # close If xanthosCoords path exists

#----------------
# Prepare Gridded Scarcity
#---------------

if(!is.null(gridSRN)){
  if(any(grepl("xanthos",unique(gridSRN$param))) & any(grepl("tethys",unique(gridSRN$param)))){

gridSRNTethys<-gridSRN%>%dplyr::filter(class=="Total",grepl("tethys",param))
gridSRNXanthos<-gridSRN%>%dplyr::filter(grepl("xanthos",param))%>%
  dplyr::group_by(scenario,param,units,aggType,classPalette,class)%>%
  dplyr::mutate(rollingMean=zoo::rollmean(x=value,k=scarcityXanthosRollMeanWindow,fill=NA))%>%
  dplyr::filter(!is.na(rollingMean),x %in% unique(gridSRNTethys$x))%>%dplyr::ungroup()
gridSRNScarcity<-dplyr::left_join(gridSRNTethys,gridSRNXanthos%>%dplyr::select(lat,lon,x,rollingMean),
                                  by=c("lat","lon","x"))%>%
  dplyr::mutate(scarcity=value/rollingMean,
                units="Gridded Scarcity (Fraction)",
                param="griddedScarcity",
                class="Scarcity",
                grid="pal_hot")%>%
  dplyr::select(-value,-rollingMean)%>%
  dplyr::rename(value=scarcity)%>%
  dplyr::filter(!is.na(value));

gridSRN<-dplyr::bind_rows(gridSRN,gridSRNScarcity)


  } else {print(paste("Either Xanthos or tethys not available in gridSRN params: ", unique(gridSRN$param),
                      " Skipping gridded scarcity calculation",sep=""))}
}else {print(paste("gridSRN is NULL, skipping gridded scracity calculation.",sep=""))}


#----------------------

# Test Unique Values
#a<-gridSRN%>%tidyr::unite(col="key",names(gridSRN)[!names(gridSRN) %in% c("lat","lon","value")],sep="_",remove=T)
#a<-a%>%tidyr::spread(key=key,value=value)

#--------------
# Save RData and csv.
#----------------

save(gridSRN,file=gridSRNData)
utils::write.csv(gridSRN,file = paste(dirOutputs, "/Grids/gridSRN.csv", sep = ""),row.names = F)
print(paste("gridSRN params: ", unique(gridSRN$param),sep=""))
print(paste("gridSRN.csv saved in: ", paste(dirOutputs, "/Grids/gridSRN.csv", sep = ""),sep=""))

}else{ # Close if reRead==1

if(!file.exists(gridSRNData)){stop(paste("File gridSRNData not found: ",gridSRNData,sep=""))}else{
load(gridSRNData)
}

}

return(gridSRN)

} # Close Function
