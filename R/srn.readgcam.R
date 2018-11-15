#' srn.readgcam
#'
#' This function connects to a gcamdatabase and uses a query file to
#' out results into a table ready for plotting.
#' @param dirOutputs Full path to directory for outputs
#' @param gcamdatabasePath Path to gcam database folder
#' @param gcamdatabaseName Name of gcam database
#' @param queryxml Full path to query.xml file
#' @param scenOrigNames Original Scenarios names in GCAM database in a string vector.
#' For example c('scenario1','scenario2).
#' @param scenNewNames New Names which may be shorter and more useful for figures etc.
#' Default will use Original Names. For example c('scenario1','scenario2)
#' @param reReadData If TRUE will read the GCAM data base and create a queryData.proj file
#' in the same folder as the GCAM database. If FALSE will load a '.proj' file if a file
#' with full path is provided otherwise it will search for a dataProj.proj file in the existing
#' folder which may have been created from an old run.
#' @param dataProj Optional. A default 'dataProj.proj' is produced if no .Proj file is specified.
#' @param regionsSelect The regions to analyze in a vector. Example c('Colombia','Argentina')
#' @param queriesSelect Default = "All". Vector of queries to read from the queryxml for example
#' c("Total final energy by aggregate end-use sector", "Population by region"). The queries must be
#' availble in the queryxml file. Current list of available paramaters are:
#' \itemize{
#' \item "Total final energy by aggregate end-use sector"
#' \item "GDP per capita MER by region" : Where MER is "Market Exchange Rate"
#' \item "GDP MER by region" : Where MER is "Market Exchange Rate"
#' \item "GDP Growth Rate (Percent)" : Calculated based on the GDP MER by region.
#' \item "Population by region"
#' \item "ag production by tech" : Where technologies signify irrigated or rainfed
#' }
#' @return A list with the scenarios in the gcam database, queries in the queryxml file and a
#' tibble with gcam data formatted for srn charts.
#' @keywords gcam, gcam database, query
#' @import rgcam tibble dplyr
#' @export

srn.readgcam <- function(gcamdatabasePath, gcamdatabaseName, queryxml = "srnQueries.xml",
                         scenOrigNames, scenNewNames = NULL,
                         reReadData = T, dataProj = "dataProj.proj", dirOutputs = paste(getwd(), "/outputs", sep = ""),
                         regionsSelect = NULL, queriesSelect="All") {

#------------------
# Load required Libraries
# -----------------
requireNamespace("tibble",quietly = T)
requireNamespace("dplyr",quietly = T)

#----------------
# Initialize variables by setting to NULL
#----------------

    NULL -> vintage -> year -> xLabel -> x -> value -> sector -> scenario -> region -> param -> origX -> origValue ->
    origUnits -> origScen -> origQuery -> classPalette2 -> classPalette1 -> classLabel2 -> classLabel1 -> class2 ->
    class1 -> connx -> aggregate -> Units -> sources

    # Create necessary directories if they dont exist.
    if (!dir.exists(dirOutputs)){
      dir.create(dirOutputs)}  # Output Directory
    if (!dir.exists(paste(dirOutputs,"/Tables",sep=""))){
      dir.create(paste(dirOutputs,"/Tables",sep=""))}  # Output Directory
    if (!dir.exists(paste(dirOutputs, "/Tables/Tables_gcam", sep = ""))){
        dir.create(paste(dirOutputs, "/Tables/Tables_gcam", sep = ""))}  # GCAM output directory
    if (!dir.exists(paste(dirOutputs, "/Tables/Tables_Templates", sep = ""))){
         dir.create(paste(dirOutputs, "/Tables/Tables_Templates", sep = ""))}  # GCAM output directory
    if (!dir.exists(paste(dirOutputs, "/Tables/Tables_Local", sep = ""))){
         dir.create(paste(dirOutputs, "/Tables/Tables_Local", sep = ""))}  # GCAM output directory


    # Check for new scenario names
    if (is.null(scenNewNames)) {
      scenNewNames <- scenOrigNames}

    # Read gcam database or existing dataProj.proj
    if (!reReadData) {
        if (file.exists(paste(gcamdatabasePath, "/", dataProj, sep = ""))) {
            dataProjLoaded <- loadProject(paste(gcamdatabasePath, "/", dataProj, sep = ""))
        } else {
            stop(paste("No ", dataProj, " file exists. Please set reReadData=T to create dataProj.proj"))
        }
    } else {
        if (file.exists(dataProj)){
                file.remove(dataProj)}  # Delete old project file
        for (scenario_i in scenOrigNames) {
            dataProj.proj <- addScenario(conn = localDBConn(gcamdatabasePath, gcamdatabaseName), proj = dataProj,
                scenario = scenario_i, queryFile = paste(gcamdatabasePath, "/", queryxml, sep = ""))  # Check your queries file
        }
        file.copy(from = paste(getwd(), "/", dataProj, sep = ""), to = gcamdatabasePath, overwrite = T,
                  copy.mode = TRUE)
        file.remove(dataProj)
        dataProjLoaded <- loadProject(paste(gcamdatabasePath, "/", dataProj, sep = ""))
    }

    # Save list of scenarios and queries
    scenarios <- listScenarios(dataProjLoaded)  # List of Scenarios in GCAM database
    queries <- listQueries(dataProjLoaded)  # List of Queries in queryxml


    # Read in paramaters from query file to create formatted table
    datax <- tibble()

    if(queriesSelect=="All"){queriesx <- queries} else{
      if(!all(queriesSelect %in% queries)){stop("No parameters are available in queryxml.
                               Please check your queriesSelect entries or your queryxml")} else {
      if(length(queriesSelect[!(queriesSelect %in% queries)])>0){
        print(paste("Parameters not available in queryxml: ", paste(queriesSelect[!(queriesSelect %in% queries)],collapse=", "), sep=""))
        print(paste("Running remaining queriesSelect: ",  paste(queriesSelect[(queriesSelect %in% queries)],collapse=", "), sep=""))}
      queriesx <- queriesSelect}
    }

    # Total final energy by aggregate end-use sector
    paramx <- "Total final energy by aggregate end-use sector"
    if (paramx %in% queriesx) {
        tbl <- getQuery(dataProjLoaded, paramx)  # Tibble
        if (!is.null(regionsSelect)) {
            tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
        }
        tbl <- tbl %>%
          left_join(data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
          mutate(param = "finalNrgbySec",
                 sources = "Sources",
                 origScen = scenario,
                 origQuery = paramx,
                 origValue = value,
                 origUnits = Units,
                 origX = year,
                 scenario = scenNewNames,
                 value = value * srn.assumptions()$convEJ2TWh,
                 units = "Final Energy (TWh)",
                 vintage = paste("Vint_", year, sep = ""),
                 x = year,
                 xLabel = "Year",
                 aggregate = "sum",
                 class1 = sector,
                 classLabel1 = "Sector",
                 classPalette1 = "pal_finalNrg_sec",
                 class2 = "class2",
                 classLabel2 = "classLabel2",
                 classPalette2 = "classPalette2")%>%
          dplyr::select(origScen,origQuery, origValue, origUnits, origX, region, param, scenario,
                        value, units, vintage, x, xLabel, aggregate, class1, classLabel1, classPalette1,
                        class2, classLabel2, classPalette2)
        datax <- bind_rows(datax, tbl)
    } else {
        print(paste("Paramater '", paramx, "' not found in database", sep = ""))
    }

    # GDP MER by region
    paramx <- "GDP MER by region"
    if (paramx %in% queriesx) {
        tbl <- getQuery(dataProjLoaded, paramx)  # Tibble
        if (!is.null(regionsSelect)) {
            tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
        }
        tbl <- tbl %>%
          left_join(data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
          mutate(param = "gdp",
                 sources = "Sources",
                 origScen = scenario,
                 origQuery = paramx,
                 origValue = value,
                 origUnits = Units,
                 origX = year,
                 scenario = scenNewNames,
                 value = value/1000,
                 units = "GDP (Billion 1990 USD)",
                 vintage = paste("Vint_", year, sep = ""),
                 x = year,
                 xLabel = "Year",
                 aggregate = "sum",
                 class1 = "class1",
                 classLabel1 = "GDP",
                 classPalette1 = "pal_16",
                 class2 = "class2",
                 classLabel2 = "classLabel2",
                 classPalette2 = "classPalette2") %>%
          dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origValue, origUnits, origX)
        datax <- bind_rows(datax, tbl)
    } else {
        print(paste("Paramater '", paramx, "' not found in database", sep = ""))
    }


    #---------------------
    # Create Data Template
    #---------------------

    dataTemplate <- datax %>%
      mutate(scenario = "Local Data", value = 0, sources="Sources", x=2010, vintage="vintage if available") %>%
      dplyr::select(scenario, region, sources, units, class1, class2, x, value, vintage) %>%
      unique()

    fullTemplateMap <- datax %>%
      dplyr::select(units, param, class1, class2, units, xLabel, aggregate,
                    classLabel1, classPalette1, classLabel2, classPalette2) %>%
      unique()

    #---------------------
    # Save Data in CSV
    #---------------------

    if (!dir.exists(paste(getwd(),"/dataFiles", sep = ""))){
      dir.create(paste(getwd(),"/dataFiles", sep = ""))}  # dataFiles directory (should already exist)
    if (!dir.exists(paste(getwd(),"/dataFiles/mapping", sep = ""))){
      dir.create(paste(getwd(),"/dataFiles/mapping", sep = ""))}  # mapping directory
    utils::write.csv(fullTemplateMap, file = paste(getwd(),"/dataFiles/mapping/template_Regional_mapping.csv", sep = ""),
                     row.names = F)

    if (is.null(regionsSelect)) {
        utils::write.csv(datax, file = paste(dirOutputs, "/Tables/Tables_gcam/gcamDataTable_AllRegions_", min(range(datax$x)),
            "to", max(range(datax$x)), ".csv", sep = ""), row.names = F)
        utils::write.csv(dataTemplate, file = paste(dirOutputs, "/Tables/Tables_Template/template_Regional_AllRegions.csv", sep = ""),
                         row.names = F)
    } else {

       if(!all(regionsSelect %in% unique(datax$region))){
          print(paste("Regions not available in data: ", paste(regionsSelect[!(regionsSelect %in% unique(datax$region))],collapse=", "), sep=""))
          print(paste("Running remaining regions: ",  paste(regionsSelect[(regionsSelect %in% unique(datax$region))],collapse=", "), sep=""))
       }

        for (region_i in regionsSelect[(regionsSelect %in% unique(datax$region))]) {
            utils::write.csv(datax %>% dplyr::filter(region == region_i),
                             file = paste(dirOutputs, "/Tables/Tables_gcam/gcamDataTable_",region_i,"_", min(range(datax$x)),
                                          "to", max(range(datax$x)), ".csv", sep = ""),row.names = F)
            utils::write.csv(dataTemplate %>% dplyr::filter(region == region_i),
                             file = paste(dirOutputs, "/Tables/Tables_Templates/template_Regional_",region_i,".csv", sep = ""),row.names = F)
            #utils::write.csv(dataTemplate %>% dplyr::filter(region == region_i),
            #                 file = paste(dirOutputs, "/Tables/Tables_Local/local_Regional_",region_i,".csv", sep = ""),row.names = F)
        }
    }

    return(list(data = datax, dataTemplate = dataTemplate, scenarios = scenarios, queries = queries))

}
