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
#' @return A list with the scenarios in the gcam database, queries in the queryxml file and a
#' tibble with gcam data formatted for srn charts.
#' @keywords gcam, gcam database, query
#' @import rgcam
#' @export

srn.readgcam <- function(gcamdatabasePath,gcamdatabaseName,queryxml,scenOrigNames,
                         scenNewNames=NULL,
                         reReadData=T,
                         dataProj=NULL,
                         dirOutputs=paste(getwd(),"/outputs",sep="")) {

# Create necessary directories if they dont exist.
if(!dir.exists(dirOutputs)){dir.create(dirOutputs)}  # Output Directory
if(!dir.exists(paste(dirOutputs,"/gcam",sep=""))){dir.create(paste(dirOutputs,"/gcam",sep=""))} # GCAM output directory

# Check for new scenario names
if(is.null(scenNewNames)){scenNewNames<-scenOrigNames}

connx<- localDBConn(gcamdatabasePath,gcamdatabaseName)    # Connect to database

# Read gcam database or existing dataProj.proj
if(reReadData){
    if(file.exists("dataProj.proj")){file.remove("dataProj.proj")} # Delete old project file
    for (scenario_i in scenOrigNames){
      dataProj.proj<-addScenario(conn=connx, proj="dataProj.proj",scenario=scenario_i,queryFile=queryxml)  # Check your queries file
    }
  }else{ # Use already saved database
    if(!is.null(dataProj)){dataProj.proj<-loadProject(dataProj)}else{
    dataProj.proj<-loadProject("dataProj.proj")}
  }

# Save list of scenarios and queries
scenarios<-listScenarios(dataProj.proj) # List of Scenarios in GCAM database
queries<-listQueries(dataProj.proj) # List of Queries in queryxml

return (list(scenarios,queries))


}
