#' srn.prepGrid
#'
#' This function prepares gridded data for use with other srn modules.
#' @param demeterFolder Full path to demeter outputs
#' @param demeterScenario Name of demeter scenario
#' @param demeterTimesteps Default is seq(from=2005,to=2100,by=5)
#' @param dirOutputs Default =paste(getwd(),"/outputs",sep=""),
#' @param reReadData Default =1,
#' @param gridSRNData Default = paste(dirOutputs, "/Grids/gridSRN.RData", sep = "")
#' @return A table with data by polygon ID for each shapefile provided
#' @keywords gcam, gcam database, query
#' @import tibble dplyr
#' @export

srn.prepGrid<- function(demeterFolder,
                        demeterScenario,
                        demeterTimesteps=seq(from=2005,to=2100,by=5),
                        dirOutputs=paste(getwd(),"/outputs",sep=""),
                        reReadData=1,
                        gridSRNData=paste(dirOutputs, "/Grids/gridSRN.RData", sep = "")
                        ){

#------------------
# Load required Libraries
# -----------------
requireNamespace("tibble",quietly = T)
requireNamespace("dplyr",quietly = T)

#----------------
# Initialize variables by setting to NULL
#----------------

NULL -> lat -> lon -> latitude -> longitude -> aez_id -> region_id

#------------------
# Create Folders if needed
#------------------
if (!dir.exists(dirOutputs)){
    dir.create(dirOutputs)}
if (!dir.exists(paste(dirOutputs, "/Grids", sep = ""))){
    dir.create(paste(dirOutputs, "/Grids", sep = ""))}

if(reReadData==1){

gridSRN<-tibble()

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
gridx<-read.csv(paste(demeterFolder,"/landcover_",timestepx,"_timestep.csv",sep=""), stringsAsFactors = F)%>%
  as_tibble%>%
  mutate(lat=latitude,lon=longitude,
         scenario=demeterScenario,
         param="demeterLandUse",
         units="Landuse (Fraction)",
         x=timestepx)%>%
  dplyr::select(-aez_id,-region_id,-longitude,-latitude)%>%
  tidyr::gather(key="class1",value="value",-c("lat","lon","scenario","param","units","x"))

gridSRN<-bind_rows(gridSRN,gridx)

} # Close if demeter file exists
} # close demeter file loops
} # Close Demeter folder

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
