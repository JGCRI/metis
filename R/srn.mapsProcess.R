#' srn.map
#'
#' This function produce different kinds of maps for the srn package.
#' Each figure is accompanied with a csv table.
#'
#' @param data data table for charting
#' @keywords charts, diffplots
#' @return Returns the formatted data used to produce chart
#' @export
#' @import rgdal


srn.map<-function(regionsSelect=c("Argentina"),
                  shpFolder=paste(getwd(),"/dataFiles/gis/admin_gadm36_1",sep=""),
                  shpFile=paste("gadm36_1",sep=""),
                  shpName="NAME_0",
                  dirOutputs=paste(getwd(),"/outputs",sep=""),
                  reReadData=1,
                  shapeFilesData=paste(dirOutputs, "/Maps/shapeFiles.RData", sep = "")
                  ){

#----------------
# Load Libraries
#---------------

  requireNamespace("rgdal",quietly = T)
  # requireNamespace("ggplot2",quietly = T)
  # requireNamespace("scales",quietly = T)
  # requireNamespace("dplyr",quietly = T)
  # requireNamespace("tibble",quietly = T)

#------------------
# Initialize variables to remove binding errors if needed
# -----------------

#------------------------------------------
# Read data and check inputs
#------------------------------------------

if (!dir.exists(dirOutputs)){dir.create(dirOutputs)}
if (!dir.exists(paste(dirOutputs, "/Maps", sep = ""))){dir.create(paste(dirOutputs, "/Maps", sep = ""))}

if(reReadData==1){

if(!dir.exists(shpFolder)){
    print(stop("Shapefile folder: ", shpFolder ," is incorrect or doesn't exist.",sep=""))}
if(!file.exists(paste(shpFolder,"/",shpFile,".shp",sep=""))){
    print(stop("Shape file: ", paste(shpFolder,"/",shpFile,".shp",sep="")," is incorrect or doesn't exist.",sep=""))}

shp<-readOGR(dsn=shpFolder,layer=shpFile,use_iconv=T,encoding='UTF-8')
projX<-proj4string(shp) # Setting projection

save(shp,file=shapeFilesData)

}else{ # Close if reRead==1

  if(!file.exists(shapeFilesData)){stop(paste("File shapefilesData not found: ",shapeFilesData,sep=""))}else{
    load(shapeFilesData)
  }}

# Regional Selected Region
shp0a<<-shp[which(shp[[shpName]] %in% regionsSelect),]
shp0a@data<-droplevels(shp0a@data)
shp0a<<-shp0a
plot(shp0a)

  return(p)
}


