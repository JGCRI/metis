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
   stop("Shapefile folder: ", shpFolder ," is incorrect or doesn't exist.",sep="")}
if(!file.exists(paste(shpFolder,"/",shpFile,".shp",sep=""))){
    stop("Shape file: ", paste(shpFolder,"/",shpFile,".shp",sep="")," is incorrect or doesn't exist.",sep="")}

assign(shpFile,readOGR(dsn=shpFolder,layer=shpFile,use_iconv=T,encoding='UTF-8'))
projX<-proj4string(get(shpFile)) # Setting projection

save(list=shpFile,file=shapeFilesData)

}else{ # Close if reRead==1

  if(!file.exists(shapeFilesData)){stop(paste("File shapefilesData not found: ",shapeFilesData,sep=""))}else{
    load(shapeFilesData)
  }}

# Regional Selected Region
shp0a<<-shp[which(shp[[shpName]] %in% regionsSelect),]
shp0a@data<-droplevels(shp0a@data)
shp0a<<-shp0a
plot(shp0a)


data=shp0a
fillPalette=c("white","red","blue")
legendShow=F
borderColor="gray20"
lwd=1
lty=1
bgColor="white"
frame=F
labelCol="NAME_1" # Or give column data with

map<-tm_shape(data) +
  tm_fill(col=labelCol,palette = fillPalette,style="kmeans",n=100,
                            midpoint = 0, legend.show=T,showNA=F) +
  tm_legend(outside = T, text.size = .8)+
  tm_layout(panel.labels=gsub("X","",names(data)),
            panel.label.bg.color = "white",
            panel.label.size = 2,
            legend.position = c("LEFT","TOP"),
            legend.format = list(digits = 2),legend.title.size = 1,legend.text.size = 0.8) +
  tm_borders(borderColor,lwd=lwd, lty=lty) +
  tm_layout(frame = frame, bg.color=bgColor) + srn.tmapLayout()

if(labels!=F){
  if(labelCol %in% names(data)){
  map= map + tm_text(labelCol,scale=0.7,auto.placement=F, col="black")}else {
    stop(paste("Label column: ", labelCol," not available in data"))
  }
}

map

fname<<-paste("map_test",sep="")
srn.printPdfPng(map,dir=paste(dirOutputs,"/Maps",sep=""),
                filename=fname,
                figWidth=13,
                figHeight=9)


  return(p)
}


