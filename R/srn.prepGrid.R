#' srn.prepGrid
#'
#' This function prepares gridded data for use with other srn modules.
#' @param demeterFolder Full path to demeter outputs
#' @param demeterScenario Name of demeter scenario
#' @param demeterTimesteps Default is seq(from=2005,to=2100,by=5)
#' @param tethysFolder Folder for tethys results
#' @param tethysScenario Scenario name for tethys run
#' @param tethysFiles Default =c("wddom","wdelec","wdirr","wdliv","wdmfg","wdmin","wdnonag","wdtotal"),
#' @param dirOutputs Default =paste(getwd(),"/outputs",sep=""),
#' @param reReadData Default =1,
#' @param gridSRNData Default = paste(dirOutputs, "/Grids/gridSRN.RData", sep = "")
#' @return A table with data by polygon ID for each shapefile provided
#' @keywords gcam, gcam database, query
#' @export

srn.prepGrid<- function(demeterFolder,
                        demeterScenario,
                        demeterTimesteps=seq(from=2005,to=2100,by=5),
                        tethysFolder,
                        tethysScenario,
                        tethysFiles=c("wddom","wdelec","wdirr","wdliv","wdmfg","wdmin","wdnonag","wdtotal"),
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

#----------------
# Initialize variables by setting to NULL
#----------------

NULL -> lat -> lon -> latitude -> longitude -> aez_id -> region_id ->X..ID->
  ilon->ilat

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
         units="Landuse (Fraction)",
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
        units<-gsub("Unit ","",gsub("\\."," ",names(gridx)[grepl("Unit",(names(gridx)))]))
        if(grepl("mm",units)){aggType="depth"}else{aggType="vol"}
        gridx<-gridx%>%dplyr::select(-dplyr::contains("Unit"))
        gridx<-gridx%>%
              dplyr::mutate(lat=lat,lon=lon,
                            scenario=tethysScenario,
                            param="tethysWatWithdraw",
                            units=paste("Water withdrawal (",units,")",sep=""),
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

# Test Unique Values
#a<-gridSRN%>%tidyr::unite(col="key",names(gridSRN)[!names(gridSRN) %in% c("lat","lon","value")],sep="_",remove=T)
#a<-a%>%tidyr::spread(key=key,value=value)

#--------------
# Save RData and csv.
#----------------

save(gridSRN,file=gridSRNData)
utils::write.csv(gridSRN,file = paste(dirOutputs, "/Grids/gridSRN.csv", sep = ""),row.names = F)

}else{ # Close if reRead==1

if(!file.exists(gridSRNData)){stop(paste("File gridSRNData not found: ",gridSRNData,sep=""))}else{
load(gridSRNData)
}

}

return(gridSRN)

} # Close Function
