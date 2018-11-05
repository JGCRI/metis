#' srn.readgcam
#'
#' This function connects to a gcamdatabase and uses a query file to
#' out results into a table ready for plotting.
#' @param dirOutputs Full path to directory for outputs
#' @param gcamdatabasePath Path to gcam database folder
#' @param gcamdatabaseName Name of gcam database
#' @param queryxml Full path to query.xml file
#' @param scenOrigNames Original Scenarios names in GCAM database in a string vector.
#' For example c("scenario1","scenario2).
#' @param scenNewNames New Names which may be shorter and more useful for figures etc.
#' Default will use Original Names. For example c("scenario1","scenario2)
#' @param reReadData If TRUE will read the GCAM data base and create a queryData.proj file
#' in the same folder as the GCAM database. If FALSE will load a ".proj" file if a file
#' with full path is provided otherwise it will search for a dataProj.proj file in the existing
#' folder which may have been created from an old run.
#' @param dataProj Optional. A default "dataProj.proj" is produced if no .Proj file is specified.
#' @param regions The regions to analyze in a vector. Example c("Colombia","Pakistan")
#' @return A list with the scenarios in the gcam database, queries in the queryxml file and a
#' tibble with gcam data formatted for srn charts.
#' @keywords gcam, gcam database, query
#' @import rgcam tibble dplyr
#' @export

srn.readgcam <- function(gcamdatabasePath,gcamdatabaseName,queryxml,scenOrigNames,
                         scenNewNames=NULL,
                         reReadData=T,
                         dataProj="dataProj.proj",
                         dirOutputs=paste(getwd(),"/outputs",sep=""),
                         regions=NULL) {


# Initialize variables by setting to NULL
NULL->Units->aggregate->connx->dataproj->fill1->fill2->fillLabel1->fillLabel2->
  fillPalette1->fillPalette2->origQuery->origScen->origUnits->origValue->
  origX->param->region->scenario->sector->value->x->xLabel->year->vintage

# Create necessary directories if they dont exist.
if(!dir.exists(dirOutputs)){dir.create(dirOutputs)}  # Output Directory
if(!dir.exists(paste(dirOutputs,"/gcam",sep=""))){dir.create(paste(dirOutputs,"/gcam",sep=""))} # GCAM output directory

# Check for new scenario names
if(is.null(scenNewNames)){scenNewNames<-scenOrigNames}

# Read gcam database or existing dataProj.proj
if(!reReadData){
  if(file.exists(paste(gcamdatabasePath,"/",dataProj,sep=""))){
  dataProjLoaded<-loadProject(paste(gcamdatabasePath,"/",dataProj,sep=""))}else{
    stop(paste("No ",dataProj," file exists. Please set reReadData=T to create dataProj.proj"))
  }}else{
      if(file.exists(dataProj)){file.remove(dataProj)} # Delete old project file
      for (scenario_i in scenOrigNames){
        dataProj.proj<-addScenario(conn=connx, proj=dataProj,scenario=scenario_i,queryFile=paste(gcamdatabasePath,"/",queryxml,sep=""))  # Check your queries file
      }
    file.copy(from=paste(getwd(),"/",dataProj,sep=""), to=gcamdatabasePath,
            overwrite = T,copy.mode = TRUE)
    file.remove(dataproj)
    }

# Save list of scenarios and queries
scenarios<-listScenarios(dataProjLoaded) # List of Scenarios in GCAM database
queries<-listQueries(dataProjLoaded) # List of Queries in queryxml

# Conversions
convEJ2TWh<-277.77777777778

# Read in paramaters from query file and format for later use
data<-tibble()

# Total final energy by aggregate end-use sector
paramx<-"Total final energy by aggregate end-use sector"
if(paramx %in% queries){
tbl <- getQuery(dataProjLoaded, paramx) # Tibble
if(!is.null(regions)){df<-df%>%filter(region %in% regions)}
tbl<-tbl%>%
  left_join(data_frame(scenOrigNames,scenNewNames),by=c("scenario"="scenOrigNames"))%>%
  mutate(param="finalNrgbySec",
         origScen=scenario, origQuery=paramx, origValue=value, origUnits=Units, origX=year,
         scenario=scenNewNames,
         value=value*convEJ2TWh,
         units="Final Energy (TWh)",
         vintage=paste("Vint_",year,sep=""),
         x=year,
         xLabel="Year",
         aggregate="sum",
         fill1=sector,
         fillLabel1="Sector",
         fillPalette1="pal_finalNrg_sec",
         fill2="fill2",
         fillLabel2="fillLabel2",
         fillPalette2="fillPalette2")%>%
  dplyr::select(origScen,origQuery,origValue,origUnits,origX,
                region,param,scenario,value,units,vintage,x,xLabel,aggregate,
                fill1,fillLabel1,fillPalette1,fill2,fillLabel2,fillPalette2)
data<-bind_rows(data,tbl)} else {print(paste("Paramater '",paramx,"' not found in database",sep=""))}

# GDP MER by region
paramx<-"GDP MER by region"
if(paramx %in% queries){
  tbl <- getQuery(dataProjLoaded, paramx) # Tibble
  if(!is.null(regions)){df<-df%>%filter(region %in% regions)}
  tbl<-tbl%>%
    left_join(data_frame(scenOrigNames,scenNewNames),by=c("scenario"="scenOrigNames"))%>%
    mutate(param="gdp",
           origScen=scenario, origQuery=paramx, origValue=value, origUnits=Units, origX=year,
           scenario=scenNewNames,
           value=value/1000,
           units="GDP (Billion 1990 USD)",
           vintage=paste("Vint_",year,sep=""),
           x=year,
           xLabel="Year",
           aggregate="sum",
           fill1="fill1",
           fillLabel1="GDP",
           fillPalette1="pal_16",
           fill2="fill2",
           fillLabel2="fillLabel2",
           fillPalette2="fillPalette2")%>%
    dplyr::select(origScen,origQuery,origValue,origUnits,origX,
                  region,param,scenario,value,units,vintage,x,xLabel,aggregate,
                  fill1,fillLabel1,fillPalette1,fill2,fillLabel2,fillPalette2)
  data<-bind_rows(data,tbl)} else {print(paste("Paramater '",paramx,"' not found in database",sep=""))}


return (list(data=data,scenarios=scenarios,queries=queries))


}
