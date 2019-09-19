#' metis.readgcam
#'
#' This function connects to a gcamdatabase and uses a query file to
#' out results into a table ready for plotting.
#' @param dirOutputs Full path to directory for outputs
#' @param gcamdatabasePath Path to gcam database folder
#' @param gcamdatabaseName Name of gcam database
#' @param queryxml Name of the query.xml file. By default it is "metisQueries.xml"
#' @param queryPath Folder that contains the query.xml file.By default it is
#' the same folder as specified by gcamdatabasePath
#' @param scenOrigNames Original Scenarios names in GCAM database in a string vector.
#' For example c('scenario1','scenario2).
#' @param scenNewNames New Names which may be shorter and more useful for figures etc.
#' Default will use Original Names. For example c('scenario1','scenario2)
#' @param reReadData If TRUE will read the GCAM data base and create a queryData.proj file
#' in the same folder as the GCAM database. If FALSE will load a '.proj' file if a file
#' with full path is provided otherwise it will search for a dataProj.proj file in the existing
#' folder which may have been created from an old run.
#' @param dataProj Optional. A default 'dataProj.proj' is produced if no .Proj file is specified.
#' @param dataProjPath Folder that contains the dataProj or where it will be produced.
#' By default it is the same folder as specified by gcamdatabasePath
#' @param regionsSelect The regions to analyze in a vector. Example c('Colombia','Argentina'). Full list:
#' c(USA, Africa_Eastern, Africa_Northern, Africa_Southern, Africa_Western, Australia_NZ, Brazil, Canada
#' Central America and Caribbean, Central Asia, China, EU-12, EU-15, Europe_Eastern, Europe_Non_EU,
#' European Free Trade Association, India, Indonesia, Japan, Mexico, Middle East, Pakistan, Russia,
#' South Africa, South America_Northern, South America_Southern, South Asia, South Korea, Southeast Asia,
# Taiwan, Argentina, Colombia, Uruguay)
#' @param queriesSelect Default = "All". Predetermined subsets or a vector of queries to read from the queryxml for example
#' predetermined subsets would be c('water','energy') or
#' selection of queries would be c("Total final energy by aggregate end-use sector", "Population by region").
#' The queries must be availble in the queryxml file.
#' Queryset names include: c("water", "energy", "land", "emissions", "ag", "socioecon", "transport")
#' Current list of queries for each set include:
#' water
#' \itemize{
#' \item "water withdrawals by crop"
#' \item "water withdrawals by water mapping source"
#' \item "water consumption by water mapping source"
#' \item "water withdrawals by sector"
#' \item "water consumption by sector"
#' \item "biophysical water demand by crop type and land region"}
#' energy
#' \itemize{
#' \item "primary energy consumption by region (direct equivalent) ORDERED SUBSECTORS"
#' \item "Electricity generation by aggregate technology ORDERED SUBSECTORS"
#' \item "Final energy by detailed end-use sector and fuel"
#' \item "total final energy by aggregate sector"
#' \item "refined liquids production by subsector"
#' \item "building final energy by fuel"
#' \item "industry final energy by fuel"
#' \item "building final energy by subsector"
#' \item "transport final energy by fuel"}
#' land
#' \itemize{
#' \item "land allocation by crop and water source",
#' \item "aggregated land allocation",
#' \item "land allocation by crop"}
#' emissions
#' \itemize{"nonCO2 emissions by resource production",
#' \item "nonCO2 emissions by sector"
#' \item "Land Use Change Emission (future)"
#' \item "CO2 emissions by sector (no bio)"
#' \item "CO2 emissions by sector"}
#' ag
#' \itemize{
#' \item "Ag Production by Crop Type"
#' \item "ag production by tech"}
#' socioecon
#' \itemize{
#' \item "GDP MER by region"
#' \item "GDP per capita MER by region"
#' \item "Population by region"}
#' transport
#' \itemize{
#' \item "transport service output by mode"
#' \item "transport service output by tech (new)"}
#'
#' @param paramsSelect Default = "All". If desired dplyr::select a subset of paramaters to analyze from the full list of parameters:
#' c(# Energy
#' "energyPrimaryByFuelEJ","energyPrimaryRefLiqProdEJ",
#' "energyFinalConsumBySecEJ","energyFinalByFuelBySectorEJ","energyFinalSubsecByFuelTranspEJ",
#' "energyFinalSubsecByFuelBuildEJ", "energyFinalSubsecByFuelIndusEJ","energyFinalSubsecBySectorBuildEJ",
#' "energyPrimaryByFuelMTOE","energyPrimaryRefLiqProdMTOE",
#' "energyFinalConsumBySecMTOE","energyFinalbyFuelMTOE","energyFinalSubsecByFuelTranspMTOE",
#' "energyFinalSubsecByFuelBuildMTOE", "energyFinalSubsecByFuelIndusMTOE","energyFinalSubsecBySectorBuildMTOE",
#' "energyPrimaryByFuelTWh","energyPrimaryRefLiqProdTWh",
#' "energyFinalConsumBySecTWh","energyFinalbyFuelTWh","energyFinalSubsecByFuelTranspTWh",
#' "energyFinalSubsecByFuelBuildTWh", "energyFinalSubsecByFuelIndusTWh","energyFinalSubsecBySectorBuildTWh",
#' # Electricity
#' "elecByTechTWh","elecCapByFuel","elecFinalBySecTWh","elecFinalByFuelTWh",
#' # Transport
#' "transportPassengerVMTByMode", "transportFreightVMTByMode", "transportPassengerVMTByFuel", "transportFreightVMTByFuel",
#' # Water
#' "watConsumBySec", "watWithdrawBySec", "watWithdrawByCrop", "watBioPhysCons", "watIrrWithdrawBasin","watIrrConsBasin",
#' # Socio-economics
#' "gdpPerCapita", "gdp", "gdpGrowthRate", "pop",
#' # Agriculture
#' "agProdbyIrrRfd","agProdBiomass", "agProdForest", "agProdByCrop",
#' # Land use
#' "landIrrRfd", "landAlloc","landAllocByCrop",
#' # Emissions
#' "emissLUC", "emissCO2BySector","emissCO2NonCO2BySectorGWPAR5","emissCO2NonCO2BySectorGTPAR5","emissNonCO2BySectorOrigUnits",
#' "emissNonCO2ByResProdGWPAR5", "emissTotalFFIBySec","emissMethaneBySource",
#' "emissCO2BySectorNonCO2GWPAR5", "emissCO2BySectorNonCO2GWPAR5LUC", "emissTotalBySec","emissCO2BySectorNoBio")
#' @return A list with the scenarios in the gcam database, queries in the queryxml file and a
#' tibble with gcam data formatted for metis charts.
#' @keywords gcam, gcam database, query
#' @export


metis.readgcam <- function(gcamdatabasePath = NULL,
                           gcamdatabaseName = NULL,
                           queryxml = "metisQueries.xml",
                           queryPath = paste(getwd(),"/dataFiles/gcam",sep=""),
                           scenOrigNames = NULL,
                           scenNewNames = NULL,
                           reReadData = T,
                           dataProj = "dataProj.proj",
                           dataProjPath = paste(getwd(), "/outputs", sep = ""),
                           dirOutputs = paste(getwd(), "/outputs", sep = ""),
                           regionsSelect = NULL,
                           queriesSelect="All",
                           paramsSelect="All"
){


  # gcamdatabasePath = NULL
  # gcamdatabaseName = NULL
  # queryxml = "metisQueries.xml"
  # queryPath = paste(getwd(),"/dataFiles/gcam",sep="")
  # scenOrigNames = NULL
  # scenNewNames = NULL
  # reReadData = T
  # dataProj = "dataProj.proj"
  # dataProjPath = paste(getwd(), "/outputs", sep = "")
  # dirOutputs = paste(getwd(), "/outputs", sep = "")
  # regionsSelect = NULL
  # queriesSelect="All"
  # paramsSelect="All"


  #----------------
  # Initialize variables by setting to NULL
  #----------------

  NULL -> vintage -> year -> xLabel -> x -> value -> sector -> scenario -> region -> param -> origX -> origValue ->
    origUnits -> origScen -> origQuery -> classPalette2 -> classPalette1 -> classLabel2 -> classLabel1 -> class2 ->
    class1 -> connx -> aggregate -> Units -> sources -> paramx -> fuel -> technology -> input -> output -> water ->
    landleaf -> ghg -> Convert -> regionsSelectAll->cf1971to2100->gcamCapacityFactor -> . -> GWPAR5 -> tblelecByTechTWh ->
    totalFFINonCO2 -> FracBioFuel -> FracFossilFuel -> TotalLiquids ->
    class_temp -> resource -> subRegAreaSum -> subsector


#---------------------
# Query sets and query select
#---------------------
  querySets <- list('water'=c("water withdrawals by crop",
                              "water withdrawals by water mapping source",
                              "water consumption by water mapping source",
                              "water withdrawals by sector",
                              "water consumption by sector",
                              "biophysical water demand by crop type and land region"),
                    'energy'=c("primary energy consumption by region (direct equivalent) ORDERED SUBSECTORS",
                               "Electricity generation by aggregate technology ORDERED SUBSECTORS",
                               "Final energy by detailed end-use sector and fuel",
                               "total final energy by aggregate sector",
                               "refined liquids production by subsector",
                               "building final energy by fuel",
                               "industry final energy by fuel",
                               "building final energy by subsector",
                               "transport final energy by fuel"),
                    'land'=c("land allocation by crop and water source",
                             "aggregated land allocation",
                             "land allocation by crop"),
                    'emissions'=c("nonCO2 emissions by resource production",
                                  "nonCO2 emissions by sector",
                                  "Land Use Change Emission (future)",
                                  "CO2 emissions by sector (no bio)",
                                  "CO2 emissions by sector"),
                    'ag'=c("Ag Production by Crop Type",
                           "ag production by tech"),
                    'socioecon'=c("GDP MER by region",
                                  "GDP per capita MER by region",
                                  "Population by region"),
                    'transport'=c("transport service output by mode",
                                  "transport service output by tech (new)"))

  # Check if queriesSelect is a querySet or one of the queries
  if(!any(c("All","all") %in% queriesSelect)){
  if(any(queriesSelect %in% names(querySets))){
    queriesSelectx <- as.vector(unlist(querySets[names(querySets) %in% queriesSelect]))
    print(paste("queriesSelect chosen include the following querySets: ",paste(queriesSelect,collapse=", "),".",sep=""))
    print(paste("Which include the following queries: ",paste(queriesSelectx,collapse=", "),".",sep=""))
    #print(paste("Other queries not run include: ",paste(as.vector(unlist(querySets))[!as.vector(unlist(querySets)) %in% queriesSelectx],collapse=", "),".",sep=""))
  }else{
    if(any(queriesSelect %in% as.vector(unlist(querySets)))){
      queriesSelectx <- queriesSelect[queriesSelect %in% as.vector(unlist(querySets))]
      print(paste("queriesSelect chosen include the following queries: ",paste(queriesSelectx,collapse=", "),".",sep=""))
     # print(paste("Other queries not run include: ",paste(as.vector(unlist(querySets))[!as.vector(unlist(querySets)) %in% queriesSelectx],collapse=", "),".",sep=""))
    }else {
      queriesSelectx <-  queriesSelect
      print(paste("None of the queries in queriesSelect are available in metisQueries.xml: ",paste(queriesSelectx,collapse=", "),".",sep=""))
      print(paste("Queries in metisQueries.xml include: ",paste(as.vector(unlist(querySets))[!as.vector(unlist(querySets)) %in% queriesSelectx],collapse=", "),".",sep=""))
    }
  }}else{
    queriesSelectx <- as.vector(unlist(querySets))
  }

#-----------------------------
# Create necessary directories if they dont exist.
#----------------------------
  if (!dir.exists(dirOutputs)){
    dir.create(dirOutputs)}  # Output Directory
  if (!dir.exists(paste(dirOutputs,"/readGCAMTables",sep=""))){
    dir.create(paste(dirOutputs,"/readGCAMTables",sep=""))}  # Output Directory
  if (!dir.exists(paste(dirOutputs, "/readGCAMTables/Tables_gcam", sep = ""))){
    dir.create(paste(dirOutputs, "/readGCAMTables/Tables_gcam", sep = ""))}  # GCAM output directory
  if (!dir.exists(paste(dirOutputs, "/readGCAMTables/Tables_Templates", sep = ""))){
    dir.create(paste(dirOutputs, "/readGCAMTables/Tables_Templates", sep = ""))}  # GCAM output directory
  if (!dir.exists(paste(dirOutputs, "/readGCAMTables/Tables_Local", sep = ""))){
    dir.create(paste(dirOutputs, "/readGCAMTables/Tables_Local", sep = ""))}  # GCAM output directory

  # Check for new scenario names
  if (is.null(scenNewNames)) {
    scenNewNames <- scenOrigNames}

#---------------------------------------------
# Read gcam database or existing dataProj.proj
#--------------------------------------------
  if (!reReadData) {
 # Check for proj file path and folder if incorrect give error
    if(!file.exists(paste(dataProjPath, "/", dataProj, sep = ""))){stop(paste("dataProj file: ", dataProjPath,"/",dataProj," is incorrect or doesn't exist.",sep=""))}

    if (file.exists(paste(dataProjPath, "/", dataProj, sep = ""))) {
      dataProjLoaded <- rgcam::loadProject(paste(dataProjPath, "/", dataProj, sep = ""))
    } else {
      stop(paste("No ", dataProj, " file exists. Please set reReadData=T to create dataProj.proj"))
    }
  } else {

  # Check for query file and folder if incorrect give error
    if(!file.exists(paste(queryPath, "/", queryxml, sep = ""))){stop(paste("query file: ", queryPath,"/",queryxml," is incorrect or doesn't exist.",sep=""))}
    if(file.exists(paste(queryPath, "/subSetQueries.xml", sep = ""))){unlist(paste(queryPath, "/subSetQueries.xml", sep = ""))}

    # Subset the query file if queriwsSelect is not "All"
    if(!any(c("All","all") %in% queriesSelect)){

    xmlFilePath = paste(queryPath, "/", queryxml, sep = "")
    xmlfile <- XML::xmlTreeParse(xmlFilePath)
    xmltop <- XML::xmlRoot(xmlfile)
    top <- XML::xmlNode(XML::xmlName(xmltop))

    for(i in 1:length(xmltop)){
      for(j in 1:length(queriesSelectx)){
        if(any(grepl(gsub("\\(","\\\\(",gsub("\\)","\\\\)",queriesSelectx[j])), as.character(xmltop[[i]]))))
          top <- XML::addChildren(top, xmltop[[i]])
      }
    }
    XML::saveXML(top, file=paste(queryPath, "/subSetQueries.xml", sep = ""))
    } else {
      print(paste("queriesSelect includes 'All' so running all available queries: ",paste(queriesSelectx,collapse=", "),".",sep=""))
      file.copy(from=paste(queryPath, "/", queryxml, sep = ""), to=paste(queryPath, "/subSetQueries.xml", sep = ""))
    }

    if(!file.exists(paste(queryPath, "/subSetQueries.xml", sep = ""))){stop(paste("query file: ", queryPath,"/subSetQueries.xml is incorrect or doesn't exist.",sep=""))}else{
      print(paste("Reading queries from queryFile created: ", queryPath,"/subSetQueries.xml.",sep=""))
    }

    # Check for gcamdatbasePath and gcamdatabasename
    if(is.null(gcamdatabasePath) | is.null(gcamdatabaseName)){stop(paste("GCAM database: ", gcamdatabasePath,"/",gcamdatabaseName," is incorrect or doesn't exist.",sep=""))}
    if(!file.exists(paste(gcamdatabasePath, "/", gcamdatabaseName, sep = ""))){stop(paste("GCAM database: ", gcamdatabasePath,"/",gcamdatabaseName," is incorrect or doesn't exist.",sep=""))}

    if (file.exists(paste(dataProjPath, "/", dataProj, sep = ""))){
      file.remove(paste(dataProjPath, "/", dataProj, sep = ""))}  # Delete old project file


    for (scenario_i in scenOrigNames) {
      dataProj.proj <- rgcam::addScenario(conn = rgcam::localDBConn(gcamdatabasePath, gcamdatabaseName), proj = dataProj,
                                          scenario = scenario_i, queryFile = paste(queryPath, "/subSetQueries.xml", sep = ""))  # Check your queries file
    }
    file.copy(from = paste(getwd(), "/", dataProj, sep = ""), to = dataProjPath, overwrite = T,
              copy.mode = TRUE)
    file.remove(dataProj)
    dataProjLoaded <- rgcam::loadProject(paste(dataProjPath, "/", dataProj, sep = ""))
  }

  # Save list of scenarios and queries
  scenarios <- rgcam::listScenarios(dataProjLoaded); scenarios  # List of Scenarios in GCAM database
  queries <- rgcam::listQueries(dataProjLoaded); queries  # List of Queries in queryxml

  # Get All Regions
  if(length(queries)==0){stop("No queries found. PLease check data.")}
  tbl <- rgcam::getQuery(dataProjLoaded, queries[1])  # Tibble
  regionsAll<-unique(tbl$region)
  if(any(regionsSelect=="All" | regionsSelect=="all" )){regionsSelect<-regionsAll; regionsSelectAll=T}else{
    regionsSelectAll=F
  }

  # Read in paramaters from query file to create formatted table
  datax <- tibble::tibble()

  if(any(queriesSelect=="All")){queriesx <- queries} else{
    if(!any(queriesSelectx %in% queries)){stop("None of the selected queries are available in the data that has been read.
Please check your data if reRead was set to F. Otherwise check the queriesSelect entries and the queryxml file.")} else {
                                                if(length(queriesSelectx[!(queriesSelectx %in% queries)])>0){
                                                  print(paste("Queries not available in queryxml: ", paste(queriesSelectx[!(queriesSelectx %in% queries)],collapse=", "), sep=""))
                                                  print(paste("Running remaining queriesSelect: ",  paste(queriesSelectx[(queriesSelectx %in% queries)],collapse=", "), sep=""))}
                                                queriesx <- queriesSelectx}
  }


  if(any(paramsSelect=="All")){

    paramsSelectx=c(# Energy
                    "energyPrimaryByFuelEJ","energyPrimaryRefLiqProdEJ",
                    "energyFinalConsumBySecEJ","energyFinalByFuelBySectorEJ","energyFinalSubsecByFuelTranspEJ",
                    "energyFinalSubsecByFuelBuildEJ", "energyFinalSubsecByFuelIndusEJ","energyFinalSubsecBySectorBuildEJ",
                    "energyPrimaryByFuelMTOE","energyPrimaryRefLiqProdMTOE",
                    "energyFinalConsumBySecMTOE","energyFinalbyFuelMTOE","energyFinalSubsecByFuelTranspMTOE",
                    "energyFinalSubsecByFuelBuildMTOE", "energyFinalSubsecByFuelIndusMTOE","energyFinalSubsecBySectorBuildMTOE",
                    "energyPrimaryByFuelTWh","energyPrimaryRefLiqProdTWh",
                    "energyFinalConsumBySecTWh","energyFinalbyFuelTWh","energyFinalSubsecByFuelTranspTWh",
                    "energyFinalSubsecByFuelBuildTWh", "energyFinalSubsecByFuelIndusTWh","energyFinalSubsecBySectorBuildTWh",
                    # Electricity
                    "elecByTechTWh","elecCapByFuel","elecFinalBySecTWh","elecFinalByFuelTWh",
                    # Transport
                    "transportPassengerVMTByMode", "transportFreightVMTByMode", "transportPassengerVMTByFuel", "transportFreightVMTByFuel",
                    # Water
                    "watConsumBySec", "watWithdrawBySec", "watWithdrawByCrop", "watBioPhysCons", "watIrrWithdrawBasin","watIrrConsBasin",
                    # Socio-economics
                    "gdpPerCapita", "gdp", "gdpGrowthRate", "pop",
                    # Agriculture
                    "agProdbyIrrRfd","agProdBiomass", "agProdForest", "agProdByCrop",
                    # Land use
                    "landIrrRfd", "landAlloc","landAllocByCrop",
                    # Emissions
                    "emissLUC", "emissCO2BySector","emissCO2NonCO2BySectorGWPAR5","emissCO2NonCO2BySectorGTPAR5","emissNonCO2BySectorOrigUnits",
                    "emissNonCO2ByResProdGWPAR5", "emissTotalFFIBySec","emissMethaneBySource",
                    "emissCO2BySectorNonCO2GWPAR5", "emissCO2BySectorNonCO2GWPAR5LUC", "emissTotalBySec","emissCO2BySectorNoBio")
  }else{paramsSelectx=paramsSelect}

  paramx<-"energyFinalConsumBySecEJ"
  # Total final energy by aggregate end-use sector
  if(paramx %in% paramsSelectx){
    queryx <- "total final energy by aggregate sector"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "energyFinalConsumBySecEJ",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year,
                      scenario = scenNewNames,
                      units = "Final Energy by Sector (EJ)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = sector,
                      classLabel1 = "Sector",
                      classPalette1 = "pal_metis",
                      class2 = "class2",
                      classLabel2 = "classLabel2",
                      classPalette2 = "classPalette2")%>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%
        dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}

  paramx<-"energyFinalSubsecBySectorBuildEJ"
  # Total final energy by aggregate end-use sector
  if(paramx %in% paramsSelectx){
    queryx <- "building final energy by subsector"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "energyFinalSubsecBySectorBuildEJ",
                      sector=gsub("comm\\scooling","Commercial CoolingHeating",sector),
                      sector=gsub("comm\\sheating","Commercial CoolingHeating",sector),
                      sector=gsub("comm\\sothers","Commercial Others",sector),
                      sector=gsub("resid\\scooling","Residential CoolingHeating",sector),
                      sector=gsub("resid\\sheating","Residential CoolingHeating",sector),
                      sector=gsub("resid\\sothers","Residential Others",sector),
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year,
                      scenario = scenNewNames,
                      units = "Building Final Energy By Subsector (EJ)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = sector,
                      classLabel1 = "Sector",
                      classPalette1 = "pal_metis",
                      class2 = subsector,
                      classLabel2 = "classLabel2",
                      classPalette2 = "classPalette2")%>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%
        dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}

  paramx<-"energyFinalByFuelBySectorEJ"
  # Total final energy by aggregate end-use sector
  if(paramx %in% paramsSelectx){
    queryx <- "Final energy by detailed end-use sector and fuel"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(input=dplyr::if_else(input=="biomass","bioenergy",input),
                      sector=gsub("process heat cement","industry",sector),
                      sector=gsub("cement","industry",sector),
                      sector=gsub("industrial energy use","industry",sector),
                      sector=gsub("industrial feedstocks","industry",sector),
                      sector=gsub("N fertilizer","industry",sector),
                      sector = replace(sector, stringr::str_detect(sector, "trn"), "transport"),
                      sector=gsub("comm cooling","buildings",sector),
                      sector=gsub("comm heating","buildings",sector),
                      sector=gsub("comm others","buildings",sector),
                      sector=gsub("resid cooling","buildings",sector),
                      sector=gsub("resid heating","buildings",sector),
                      sector=gsub("resid others","buildings",sector),
                      input=gsub("elect\\_td\\_ind","electricity",input),
                      input=gsub("elect\\_td\\_bld","electricity",input),
                      input=gsub("elect\\_td\\_trn","electricity",input),
                      input=gsub("delivered coal","coal",input),
                      input=gsub("refined liquids enduse","liquids",input),
                      input=gsub("delivered biomass","biomass",input),
                      input=gsub("H2 enduse","hydrogen",input),
                      input=gsub("refined liquids industrial","liquids",input),
                      input=gsub("wholesale gas","gas",input),
                      input=gsub("traditional biomass","biomass",input),
                      input=gsub("delivered gas","gas",input),
                      input=gsub("district heat","Other",input),
                      param = "energyFinalByFuelBySectorEJ",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year,
                      scenario = scenNewNames,
                      units = "Final Energy by Fuel (EJ)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = input,
                      classLabel1 = "Fuel",
                      classPalette1 = "pal_metis",
                      class2 = sector,
                      classLabel2 = "classLabel2",
                      classPalette2 = "classPalette2")%>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}

  paramx<-"energyFinalSubsecByFuelBuildEJ"
  # Total final energy by aggregate end-use sector
  if(paramx %in% paramsSelectx){
    queryx <- "building final energy by fuel"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::rename(sector=input) %>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(sector=gsub("elect_td_bld","electricity",sector),
                      sector=gsub("delivered gas","gas",sector),
                      sector=gsub("delivered biomass","bioenergy",sector),
                      sector=gsub("delivered coal","coal",sector),
                      sector=gsub("refined liquids enduse","liquids",sector),
                      param = "energyFinalSubsecByFuelBuildEJ",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year,
                      scenario = scenNewNames,
                      units = "Building Final Energy by Fuel (EJ)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = sector,
                      classLabel1 = "Fuel",
                      classPalette1 = "pal_metis",
                      class2 = sector,
                      classLabel2 = "classLabel2",
                      classPalette2 = "classPalette2")%>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}

  paramx<-"energyFinalSubsecByFuelIndusEJ"
  # Total final energy by aggregate end-use sector
  if(paramx %in% paramsSelectx){
    queryx <- "industry final energy by fuel"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::rename(sector=input) %>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(sector=gsub("elect_td_ind","electricity",sector),
                      sector=gsub("wholesale gas","gas",sector),
                      sector=gsub("delivered biomass","bioenergy",sector),
                      sector=gsub("delivered coal","coal",sector),
                      sector=gsub("refined liquids industrial","liquids",sector),
                      sector=gsub("H2 enduse","hydrogen",sector),
                      param = "energyFinalSubsecByFuelIndusEJ",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year,
                      scenario = scenNewNames,
                      units = "Industry Final Energy by Fuel (EJ)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = sector,
                      classLabel1 = "Fuel",
                      classPalette1 = "pal_metis",
                      class2 = sector,
                      classLabel2 = "classLabel2",
                      classPalette2 = "classPalette2")%>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}

  paramx<-"elecFinalBySecTWh"
  # Total final energy by aggregate end-use sector
  if(paramx %in% paramsSelectx){
    queryx <- "Final energy by detailed end-use sector and fuel"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::filter(grepl("elect",input))%>%
        dplyr::mutate(sector=gsub("cement","industry",sector),
                      sector=gsub("comm cooling","buildings",sector),
                      sector=gsub("comm heating","buildings",sector),
                      sector=gsub("comm others","buildings",sector),
                      sector=gsub("industrial energy use","industry",sector),
                      sector=gsub("resid\\scooling","buildings",sector),
                      sector=gsub("resid\\sheating","buildings",sector),
                      sector=gsub("resid\\sothers","buildings",sector),
                      sector=gsub("trn\\_freight","transportation",sector),
                      sector=gsub("trn\\_pass\\_road\\_LDV\\_2W","transportation",sector),
                      sector=gsub("trn\\_pass\\_road\\_LDV\\_4W","transportation",sector),
                      sector=gsub("trn\\_pass","transportation",sector)
                      )%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "elecFinalBySecTWh",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year,
                      scenario = scenNewNames,
                      value = value * metis.assumptions()$convEJ2TWh,
                      units = "Final Electricity (TWh)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = sector,
                      classLabel1 = "Sector",
                      classPalette1 = "pal_metis",
                      class2 = input,
                      classLabel2 = "input",
                      classPalette2 = "classPalette2")%>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}

  paramx<-"elecFinalByFuelTWh"
  # Total final energy by aggregate end-use sector
  if(paramx %in% paramsSelectx){
    queryx <- "Final energy by detailed end-use sector and fuel"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::filter(grepl("elect",input))%>%
        dplyr::mutate(input=gsub("elect\\_td\\_ind","electricity",input),
                      input=gsub("elect\\_td\\_bld","electricity",input),
                      input=gsub("elect\\_td\\_trn","electricity",input),
                      input=gsub("delivered\\scoal","coal",input),
                      input=gsub("refined\\sliquids\\senduse","oil",input),
                      input=gsub("delivered\\sbiomass","biomass",input),
                      input=gsub("H2\\senduse","hydrogen",input),
                      input=gsub("refined\\sliquids industrial","oil",input),
                      input=gsub("wholesale\\sgas","gas",input),
                      input=gsub("traditional\\sbiomass","biomass",input),
                      input=gsub("delivered\\sgas","gas",input),
                      input=gsub("district\\sheat","Other",input))%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "elecFinalByFuelTWh",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year,
                      scenario = scenNewNames,
                      value = value * metis.assumptions()$convEJ2TWh,
                      units = "Final Electricity (TWh)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = input,
                      classLabel1 = "Fuel",
                      classPalette1 = "pal_metis",
                      class2 = sector,
                      classLabel2 = "Sector",
                      classPalette2 = "classPalette2")%>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}

  paramx<-"energyPrimaryByFuelEJ"
  # primary energy consumption by region (direct equivalent)
  if(paramx %in% paramsSelectx){
    queryx <- "primary energy consumption by region (direct equivalent) ORDERED SUBSECTORS"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "energyPrimaryByFuelEJ",
                      fuel=gsub("biomass","bioenergy",fuel),
                      fuel=gsub("b biomass","b bioenergy",fuel),
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year,
                      scenario = scenNewNames,
                      units = "Primary Energy Consumption by Fuel (EJ)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = fuel,
                      classLabel1 = "Fuel",
                      classPalette1 = "pal_metis",
                      class2 = "class2",
                      classLabel2 = "classLabel2",
                      classPalette2 = "classPalette2")%>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}

  paramx <- "elecByTechTWh"
  if(paramx %in% paramsSelectx){
    # Electricity generation by aggregate technology
    queryx <- "Electricity generation by aggregate technology ORDERED SUBSECTORS"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "elecByTechTWh",
                      technology=gsub("biomass","bioenergy",technology),
                      technology=gsub("b\\sbiomass","b bioenergy",technology),
                      technology=gsub("g\\sBiomass","g Bioenergy",technology),
                      technology=gsub("h\\sBiomass\\sw\\/CCS","h Bioenergy w/CCS",technology),
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year,
                      scenario = scenNewNames,
                      value = value * metis.assumptions()$convEJ2TWh,
                      units = "Electricity Generation (TWh)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = technology,
                      classLabel1 = "Fuel",
                      classPalette1 = "pal_metis",
                      class2 = "class2",
                      classLabel2 = "classLabel2",
                      classPalette2 = "classPalette2")%>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources,class1,class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
      tblelecByTechTWh<-tbl
    } else {
      if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}

  if(!is.null(tblelecByTechTWh)){
  if(file.exists(paste(getwd(),"/dataFiles/gcam/capacity_factor_by_elec_gen_subsector.csv",sep=""))){
    capfactors <- data.table::fread(file=paste(getwd(),"/dataFiles/gcam/capacity_factor_by_elec_gen_subsector.csv",sep=""),skip=3,encoding="Latin-1")
    paramx <- "elecCapByFuel"
    if(paramx %in% paramsSelectx){
      # Electricity Capacity by Subsector
      queryx <- "Electricity generation by aggregate technology ORDERED SUBSECTORS"
      if (queryx %in% queriesx) {
        tbl <- tblelecByTechTWh  # Tibble
        rm(tblelecByTechTWh)
        if (!is.null(regionsSelect)) {
          tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
        }
        tbl <- tbl %>%
          dplyr::full_join(capfactors, by="class1")%>%
          dplyr::mutate(param = "elecCapByFuel",
                        gcamCapacityFactor=cf1971to2100,
                        value = value*metis.assumptions()$convEJ2GW/metis.assumptions()$convEJ2TWh/gcamCapacityFactor,
                        origValue = value,
                        units = "Electricity Capacity (GW)",
                        origUnits = units) %>%
          dplyr::filter(!is.na(value))%>%
          dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origValue, origUnits, origX)

        datax <- dplyr::bind_rows(datax, tbl)
      } else {
        if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
      }}
  } else {print(paste("Electricity capacity factor file capacity_factor_by_elec_gen_subsector.csv not found. Skipping param elecCapByFuel."))}
  } else {
  if("Electricity generation by aggregate technology ORDERED SUBSECTORS" %in% queriesSelectx){
    print(paste("elecByTechTWh did not run so skipping param elecCapByFuel."))}
    }

  # metis.chart(tbl,xData="x",yData="value",useNewLabels = 0)

  paramx <- "watConsumBySec"
  if(paramx %in% paramsSelectx){
    # water consumption by sector
    queryx <- "water consumption by sector"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "watConsumBySec",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year,
                      scenario = scenNewNames,
                      value = value,
                      units = "Water Consumption (km^3)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = sector,
                      classLabel1 = "Sector",
                      classPalette1 = "pal_metis",
                      class2 = "class2",
                      classLabel2 = "classLabel2",
                      classPalette2 = "classPalette2")%>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}

  paramx<- "watWithdrawBySec"
  if(paramx %in% paramsSelectx){
    # water withdrawals by sector
    queryx <- "water withdrawals by sector"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "watWithdrawBySec",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year,
                      scenario = scenNewNames,
                      value = value,
                      units = "Water Withdrawals (km^3)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = sector,
                      classLabel1 = "Sector",
                      classPalette1 = "pal_metis",
                      class2 = "class2",
                      classLabel2 = "classLabel2",
                      classPalette2 = "classPalette2")%>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}

  paramx <- "watWithdrawByCrop"
  if(paramx %in% paramsSelectx){
    # water withdrawals by sector
    queryx <- "water withdrawals by crop"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::filter(sector!="industry", sector!="mining" , sector!="municipal"
                      , sector!="electricity" , sector!="livestock")%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "watWithdrawByCrop",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year,
                      scenario = scenNewNames,
                      value = value,
                      units = "Water Withdrawals (km^3)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = sector,
                      classLabel1 = "Crop",
                      classPalette1 = "pal_metis",
                      class2 = "class2",
                      classLabel2 = "classLabel2",
                      classPalette2 = "classPalette2")%>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}

  paramx <- "watBioPhysCons"
  if(paramx %in% paramsSelectx){
    # biophysical water demand by crop type and land region
    queryx <- "biophysical water demand by crop type and land region"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "watBioPhysCons",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year,
                      scenario = scenNewNames,
                      value = value,
                      units = "Biophysical Water Consumption (km^3)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = sector,
                      classLabel1 = "Crop",
                      classPalette1 = "pal_16",
                      class2 = "class2",
                      classLabel2 = "classLabel2",
                      classPalette2 = "classPalette2") %>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}

  paramx <- "watIrrWithdrawBasin"
  if(paramx %in% paramsSelectx){
    # water withdrawals by water mapping source
    queryx <- "water withdrawals by water mapping source"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::filter(grepl("_irr_",input))%>%
        dplyr::mutate(input=gsub("water_td_irr_","",input),
                      input=gsub("_W","",input))%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "watIrrWithdrawBasin",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year,
                      scenario = scenNewNames,
                      value = value,
                      units = "Irrigation Water Withdrawal (km^3)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = input,
                      classLabel1 = "Basin",
                      classPalette1 = "pal_16",
                      class2 = "class2",
                      classLabel2 = "classLabel2",
                      classPalette2 = "classPalette2") %>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}


  paramx <- "watIrrConsBasin"
  if(paramx %in% paramsSelectx){
    # water consumption by water mapping source
    queryx <- "water consumption by water mapping source"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::filter(grepl("_irr_",input))%>%
        dplyr::mutate(input=gsub("water_td_irr_","",input),
                      input=gsub("_C","",input))%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "watIrrConsBasin",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year,
                      scenario = scenNewNames,
                      value = value,
                      units = "Irrigation Water Consumption (km^3)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = input,
                      classLabel1 = "Basin",
                      classPalette1 = "pal_16",
                      class2 = "class2",
                      classLabel2 = "classLabel2",
                      classPalette2 = "classPalette2") %>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}

  paramx <- "gdpPerCapita"
  if(paramx  %in% paramsSelectx){
    # GDP MER per Capita MER by region
    queryx <- "GDP per capita MER by region"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "gdpPerCapita",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year,
                      scenario = scenNewNames,
                      value = value,
                      units = "GDP per Capita (Thousand 1990 USD per Person)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = "class1",
                      classLabel1 = "GDP Per Capita",
                      classPalette1 = "pal_16",
                      class2 = "class2",
                      classLabel2 = "classLabel2",
                      classPalette2 = "classPalette2") %>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}

  paramx <- "gdp"
  if(paramx %in% paramsSelectx){
    # GDP MER by region
    queryx <- "GDP MER by region"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "gdp",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
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
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
      tblgdp<-tbl
    } else {
      if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}

  paramx <- "gdpGrowthRate"
  if(paramx %in% paramsSelectx){
    # GDP Growth Rate by region
    queryx <- "GDP Growth Rate (Percent)"
    if ("GDP MER by region" %in% queriesx) {
      tbl <- tblgdp  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::group_by(scenario,region) %>%
        dplyr::mutate(param = "gdpGrowthRate",
                      sources = "Sources",
                      value = ((value/dplyr::lag(value,order_by=x))^(1/5)-1)*100,
                      units = "GDP Growth Rate (Percent)",
                      vintage = paste("Vint_", x, sep = ""),
                      classLabel1 = "GDP growth rate",
                      origQuery = "Calculated",
                      origValue = value,
                      origUnits = units,
                      origX = x) %>%
        dplyr::ungroup() %>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      print(paste("Paramater 'GDP MER by region' not found in database, so
                  cannot calculate" ,queryx, sep = ""))
    }}

  paramx <- "pop"
  if(paramx %in% paramsSelectx){
    # Population
    queryx <- "Population by region"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "pop",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year,
                      scenario = scenNewNames,
                      value = value/1000,
                      units = "Population (Millions)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = "class1",
                      classLabel1 = "Population",
                      classPalette1 = "pal_16",
                      class2 = "class2",
                      classLabel2 = "classLabel2",
                      classPalette2 = "classPalette2") %>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}

  paramx <- "agProdbyIrrRfd"
  if(paramx %in% paramsSelectx){
    # Ag production by tech
    queryx <- "ag production by tech"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::filter(Units=="Mt")%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "agProdbyIrrRfd",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origUnits = Units,
                      origX = year,
                      origValue = value,
                      scenario = scenNewNames,
                      value = value,
                      units = "Ag Production (Mt)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = dplyr::case_when(grepl("IRR",technology)~"irrigation",
                                                grepl("RFD",technology)~"rainfed",
                                                TRUE~"NA"),
                      classLabel1 = "Water Source",
                      classPalette1 = "pal_16",
                      class2 = "class2",
                      classLabel2 = "classLabel2",
                      classPalette2 = "classPalette2") %>%
        dplyr::filter(class1!="NA")%>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}

  paramx <- "agProdBiomass"
  if(paramx %in% paramsSelectx){
    # Ag Production by Crop Type Biomass EJ
    queryx <- "Ag Production by Crop Type"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::filter(Units=="EJ",sector==output)%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "agProdBiomass",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year,
                      scenario = scenNewNames,
                      units = "Biomass Production (Mt)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = sector,
                      classLabel1 = "Crop",
                      classPalette1 = "pal_metis",
                      class2 = "class2",
                      classLabel2 = "classLabel2",
                      classPalette2 = "classPalette2")%>%
        dplyr::select(origScen,origQuery, origValue, origUnits, origX, region, param, scenario,
                      value, units, vintage, x, xLabel, aggregate, class1, classLabel1, classPalette1,
                      class2, classLabel2, classPalette2)%>%dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}

  paramx <- "agProdForest"
  if(paramx %in% paramsSelectx){
    # Ag Production by Crop Type Forest
    queryx <- "Ag Production by Crop Type"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::filter(Units=="billion m3",sector==output)%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "agProdForest",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year,
                      scenario = scenNewNames,
                      value = value,
                      units = "Agricultural Production (billion m3)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = sector,
                      classLabel1 = "Forest",
                      classPalette1 = "pal_metis",
                      class2 = "class2",
                      classLabel2 = "classLabel2",
                      classPalette2 = "classPalette2")%>%
        dplyr::select(origScen,origQuery, origValue, origUnits, origX, region, param, scenario,
                      value, units, vintage, x, xLabel, aggregate, class1, classLabel1, classPalette1,
                      class2, classLabel2, classPalette2)%>%dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}

  paramx <- "agProdByCrop"
  if(paramx %in% paramsSelectx){
    # Ag Production by Crop Type
    queryx <- "Ag Production by Crop Type"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::filter(Units=="Mt",sector==output, sector!="Pasture")%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "agProdByCrop",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year,
                      scenario = scenNewNames,
                      value = value,
                      units = "Agricultural Production (Mt)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = sector,
                      classLabel1 = "Crop",
                      classPalette1 = "pal_metis",
                      class2 = "class2",
                      classLabel2 = "classLabel2",
                      classPalette2 = "classPalette2")%>%
        dplyr::select(origScen,origQuery, origValue, origUnits, origX, region, param, scenario,
                      value, units, vintage, x, xLabel, aggregate, class1, classLabel1, classPalette1,
                      class2, classLabel2, classPalette2)%>%dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}

  paramx <- "landIrrRfd"
  if(paramx %in% paramsSelectx){
    # land allocation by crop and water source
    queryx <- "land allocation by crop and water source"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::filter(!is.na(water))%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "landIrrRfd",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year,
                      scenario = scenNewNames,
                      value = value/1000,
                      units = "Crop Land Allocation (1000 km^2)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = water,
                      classLabel1 = "Water Source",
                      classPalette1 = "pal_metis",
                      class2 = "class2",
                      classLabel2 = "classLabel2",
                      classPalette2 = "classPalette2") %>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}

  paramx <- "landAlloc"
  if(paramx %in% paramsSelectx){
    # aggregated land allocation
    queryx <- "aggregated land allocation"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::mutate(
          class2=landleaf,
          landleaf=gsub("forest\\s\\(managed\\)","forest",landleaf),
          landleaf=gsub("forest\\s\\(unmanaged\\)","forest",landleaf),
          landleaf=gsub("pasture\\s\\(grazed\\)","pasture",landleaf),
          landleaf=gsub("pasture\\s\\(other\\)","pasture",landleaf),
          landleaf=gsub("otherarable","crops",landleaf),
          landleaf=gsub("biomass","naturalOther",landleaf),
          landleaf=gsub("grass","naturalOther",landleaf),
          landleaf=gsub("shrubs","naturalOther",landleaf),
          landleaf=gsub("rock\\sand\\sdesert","naturalOther",landleaf),
          landleaf=gsub("tundra","naturalOther",landleaf))%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "landAlloc",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year,
                      scenario = scenNewNames,
                      value = value,
                      units = "Land Allocation (1000 km^2)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = landleaf,
                      classLabel1 = "Land Type",
                      classPalette1 = "pal_metis",
                      classLabel2 = "classLabel2",
                      classPalette2 = "classPalette2") %>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}

  paramx <- "landAllocByCrop"
  if(paramx %in% paramsSelectx){
    # aggregated land allocation
    queryx <- "land allocation by crop"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::mutate(
          class2=landleaf,
          landleaf=gsub("biomass_grass","Biomass",landleaf),
          landleaf=gsub("biomass_tree","Biomass",landleaf),
          landleaf=gsub("ProtectedUnmanagedForest","Delete",landleaf),
          landleaf=gsub("ProtectedUnmanagedPasture","Delete",landleaf),
          landleaf=gsub("UnmanagedPasture","Delete",landleaf),
          landleaf=gsub("OtherArableLand","Delete",landleaf),
          landleaf=gsub("UnmanagedForest","Delete",landleaf),
          landleaf=gsub("ProtectedGrassland","Delete",landleaf),
          landleaf=gsub("ProtectedShrubland","Delete",landleaf),
          landleaf=gsub("RockIceDesert","Delete",landleaf),
          landleaf=gsub("Shrubland","Delete",landleaf),
          landleaf=gsub("Tundra","Delete",landleaf),
          landleaf=gsub("Pasture","Delete",landleaf),
          landleaf=gsub("Forest","Delete",landleaf),
          landleaf=gsub("Grassland","Delete",landleaf),
          landleaf=gsub("UrbanLand","Delete",landleaf)) %>%
        dplyr::filter(!landleaf %in% c('Delete')) %>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "landAllocByCrop",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year,
                      scenario = scenNewNames,
                      value = value,
                      units = "Land Allocation (1000 km^2)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = landleaf,
                      classLabel1 = "Land Type",
                      classPalette1 = "pal_metis",
                      classLabel2 = "classLabel2",
                      classPalette2 = "classPalette2") %>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}

  paramx <- "emissLUC"
  if(paramx %in% paramsSelectx){
    # Land Use Change Emission (future)
    queryx <- "Land Use Change Emission (future)"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::left_join(metis.assumptions()$convertGgTgMTC,by="Units") %>%
        dplyr::mutate(origValue=value,value=value*Convert*44/12,
                      origUnits=Units,units="Emissions LUC - MegaTonnes of CO2 eq. (MTCO2eq)")%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "emissLUC",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origUnits = Units,
                      origX = year,
                      scenario = scenNewNames,
                      value = value,
                      origValue = origValue,
                      units = "LUC CO2 Emissions (MTCO2 Eq.)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = "class1",
                      classLabel1 = "Land Type",
                      classPalette1 = "pal_metis",
                      class2 = "class2",
                      classLabel2 = "classLabel2",
                      classPalette2 = "classPalette2") %>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value)) %>%
        dplyr::mutate(class1='LUC')
      tblLUEmiss<-tbl
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}

  paramx <- "emissCO2BySector"
  if(paramx %in% paramsSelectx){
    queryx <- "CO2 emissions by sector"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::left_join(metis.assumptions()$convertGgTgMTC,by="Units") %>%
        dplyr::mutate(origValue=value,value=value*Convert*44/12,
                      origUnits=Units,units="CO2 Emissions (MTCO2 eq.)")%>%
        dplyr::mutate(
          class1=sector,
          class2=sector) %>%
        dplyr::mutate(
          class1=gsub("comm\\scooling","Buildings",class1),
          class1=gsub("comm\\scooking","Buildings",class1),
          class1=gsub("comm\\sheating","Buildings",class1),
          class1=gsub("comm\\sothers","Buildings",class1),
          class1=gsub("comm\\sother","Buildings",class1),
          class1=gsub("comm\\shot\\swater","Buildings",class1),
          class1=gsub("elec\\_biomass\\s\\(conv\\)","Electricity",class1),
          class1=gsub("elec\\_biomass\\s\\(IGCC\\)","Electricity",class1),
          class1=gsub("elec\\_biomass\\s\\(conv\\sCCS\\)","Electricity",class1),
          class1=gsub("elec\\_biomass\\s\\(IGCC\\sCCS\\)","Electricity",class1),
          class1=gsub("elec\\_coal\\s\\(conv\\spul\\)","Electricity",class1),
          class1=gsub("elec\\_coal\\s\\(IGCC\\)","Electricity",class1),
          class1=gsub("elec\\_coal\\s\\(conv\\s\\pul\\sCCS\\)","Electricity",class1),
          class1=gsub("elec\\_coal\\s\\(IGCC\\sCCS\\)","Electricity",class1),
          class1=gsub("elec\\_gas\\s\\(IGCC\\sCCS\\)","Electricity",class1),
          class1=gsub("elec\\_gas\\s\\(steam/CT\\)","Electricity",class1),
          class1=gsub("elec\\_gas\\s\\(CC\\sCCS\\)","Electricity",class1),
          class1=gsub("elec\\_gas\\s\\(CC\\)","Electricity",class1),
          class1=gsub("elec\\_refined\\sliquids\\s\\(CC\\)","Electricity",class1),
          class1=gsub("elec\\_refined\\sliquids\\s\\(steam/CT\\)","Electricity",class1),
          class1=gsub("elec\\_refined\\sliquids\\s\\(CC\\sCCS\\)","Electricity",class1),
          class1=dplyr::if_else(class1=="electricity","Electricity",class1),
          class1=gsub("electricity_net_ownuse","Electricity",class1),
          class1=gsub("base\\s\\load\\sgeneration","Electricity",class1),
          class1=gsub("subpeak\\sgeneration","Electricity",class1),
          class1=gsub("peak\\sgeneration","Electricity",class1),
          class1=gsub("intermediate\\sgeneration","Electricity",class1),
          class1=gsub("gas\\spipeline","Refining and Hydrogen Production",class1),
          class1=gsub("gas\\sprocessing","Refining and Hydrogen Production",class1),
          class1=gsub("H2\\scentral\\sproduction","Refining and Hydrogen Production",class1),
          class1=gsub("H2\\sforecourt\\sproduction","Refining and Hydrogen Production",class1),
          class1=gsub("oil\\srefining","Refining and Hydrogen Production",class1),
          class1=gsub("gas\\sto\\sliquids","Refining and Hydrogen Production",class1),
          class1=gsub("coal\\sto\\sliquids","Refining and Hydrogen Production",class1),
          class1=gsub("industrial\\senergy\\suse","Industry",class1),
          class1=gsub("industrial\\sfeedstocks","Industry",class1),
          class1=gsub("industrial\\sprocesses","Industry",class1),
          class1=gsub("urban\\sprocesses","Waste",class1),
          class1=gsub("N\\sfertilizer","Industry",class1),
          class1=gsub("process\\sheat\\scement","Industry",class1),
          class1=gsub("cement","Industry",class1),
          class1=dplyr::if_else(class1=="refining","Refining and Hydrogen Production",class1),
          class1=gsub("regional\\sbiomassOil","Refining and Hydrogen Production",class1),
          class1=gsub("regional\\ssugar\\sfor\\sethanol","Refining and Hydrogen Production",class1),
          class1=gsub("regional\\sbiomass","Refining and Hydrogen Production",class1),
          class1=gsub("regional\\scorn\\sfor\\sethanol","Refining and Hydrogen Production",class1),
          class1=gsub("resid\\scooling","Buildings",class1),
          class1=gsub("resid\\sheating","Buildings",class1),
          class1=gsub("resid\\sothers","Buildings",class1),
          class1=gsub("resid\\sother","Buildings",class1),
          class1=gsub("resid\\shot\\swater","Buildings",class1),
          class1=gsub("resid\\scooking","Buildings",class1),
          class1=gsub("resid\\sclothes\\sdryer","Buildings",class1),
          class1=gsub("district\\sheat","Buildings",class1),
          class1=gsub("trn\\_aviation\\_intl","Transport",class1),
          class1=gsub("trn\\_shipping\\_intl","Transport",class1),
          class1=dplyr::if_else(class1=="trn_freight_road","Transport",class1),
          class1=dplyr::if_else(class1=="trn_freight","Transport",class1),
          class1=dplyr::if_else(class1=="trn_pass","Transport",class1),
          class1=gsub("trn\\_pass\\_road\\_LDV\\_2W","Transport",class1),
          class1=gsub("trn\\_pass\\_road\\_LDV\\_4W","Transport",class1),
          class1=dplyr::if_else(class1=="trn_pass_road","Transport",class1),
          # class1=gsub("trn\\_shipping\\_intl","Transport",class1),
          class1=gsub("transport\\_LDV","Transport",class1),
          class1=gsub("transport\\_bus","Transport",class1),
          class1=dplyr::if_else(class1=="trn_pass","Transport",class1),
          class1=gsub("unconventional\\soil\\sproduction","Refining and Hydrogen Production",class1),
          class1=gsub("conventional\\sgas","Refining and Hydrogen Production",class1),
          class1=gsub("unconventional\\sgas\\sother","Refining and Hydrogen Production",class1),
          class1=gsub("delivered\\sgas","Industry",class1),
          class1=gsub("tight\\sgas","Refining and Hydrogen Production",class1),
          class1=gsub("delivered\\sbiomass","Industry",class1),
          class1=gsub("refined\\sliquids\\senduse","Industry",class1),
          class1=gsub("refined\\sliquids\\sindustrial","Industry",class1),
          class1=gsub("wholesale\\sgas","Industry",class1),
          class1=gsub("natural\\sgas","Refining and Hydrogen Production",class1),
          class1=gsub("biomass\\sliquids","Refining and Hydrogen Production",class1),
          class1=gsub("coalbed\\smethane","Refining and Hydrogen Production",class1),
          class1=gsub("shale\\sgas","Refining and Hydrogen Production",class1),
          class1=dplyr::if_else(class1=="coal","Refining and Hydrogen Production",class1),
          class1=gsub("crude oil","Refining and Hydrogen Production",class1),
          class1=gsub("electricity\\sdomestic\\ssupply","Refining and Hydrogen Production",class1),
          class1=gsub("Beef","Livestock",class1),
          class1=gsub("Dairy","Livestock",class1),
          class1=gsub("FiberCrop","Crops",class1),
          class1=gsub("MiscCrop","Crops",class1),
          class1=gsub("OilCrop","Crops",class1),
          class1=gsub("OtherGrain","Crops",class1),
          class1=gsub("PalmFruit","Crops",class1),
          class1=gsub("Pork","Livestock",class1),
          class1=gsub("Poultry","Livestock",class1),
          class1=gsub("Corn","Crops",class1),
          class1=gsub("Rice","Crops",class1),
          class1=gsub("Root_Tuber","Crops",class1),
          class1=gsub("SheepGoat","Crops",class1),
          class1=gsub("SugarCrop","Crops",class1),
          class1=gsub("UnmanagedLand","Crops",class1),
          class1=gsub("Wheat","Crops",class1),
          class1=gsub("biomass","Crops",class1),
          class1=gsub("FodderGrass","Crops",class1),
          class1=gsub("FodderHerb","Crops",class1),
          class1=dplyr::if_else(class1=="biomass","Crops",class1),
          class1=gsub("backup\\_electricity","Electricity",class1),
          class1=gsub("csp\\_backup","Electricity",class1))%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "emissCO2BySector",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origX = year,
                      scenario = scenNewNames,
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      classLabel1 = "sector",
                      classPalette1 = "pal_metis",
                      classLabel2 = "sectorDetail",
                      classPalette2 = "pal_metis") %>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}


paramx <- "emissCO2BySectorNoBio"
  if(paramx %in% paramsSelectx){
    queryx <- "CO2 emissions by sector (no bio)"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
    #emiss_sector_mapping <- read.csv(CO2mappingFile, skip=1)
    tbl <- tbl %>%
      dplyr::left_join(metis.assumptions()$convertGgTgMTC,by="Units") %>%
      dplyr::mutate(origValue=value,value=value*Convert*44/12,
                    origUnits=Units,units="MegaTonnes of CO2 eq. (MTCO2eq)")%>%
      dplyr::mutate(
        class1=sector,
        class2=sector) %>%
      #dplyr::left_join(emiss_sector_mapping, by=c('class1')) %>%
      #dplyr::mutate(class1=agg_sector) %>%
      #dplyr::select(-agg_sector) %>%
      dplyr::mutate(
        class1=gsub("comm\\scooling","Buildings",class1),
        class1=gsub("comm\\scooking","Buildings",class1),
        class1=gsub("comm\\sheating","Buildings",class1),
        class1=gsub("comm\\sothers","Buildings",class1),
        class1=gsub("comm\\sother","Buildings",class1),
        class1=gsub("comm\\shot\\swater","Buildings",class1),
        class1=gsub("elec\\_biomass\\s\\(conv\\)","Electricity",class1),
        class1=gsub("elec\\_biomass\\s\\(IGCC\\)","Electricity",class1),
        class1=gsub("elec\\_biomass\\s\\(conv\\sCCS\\)","Electricity",class1),
        class1=gsub("elec\\_biomass\\s\\(IGCC\\sCCS\\)","Electricity",class1),
        class1=gsub("elec\\_coal\\s\\(conv\\spul\\)","Electricity",class1),
        class1=gsub("elec\\_coal\\s\\(IGCC\\)","Electricity",class1),
        class1=gsub("elec\\_coal\\s\\(conv\\s\\pul\\sCCS\\)","Electricity",class1),
        class1=gsub("elec\\_coal\\s\\(IGCC\\sCCS\\)","Electricity",class1),
        class1=gsub("elec\\_gas\\s\\(IGCC\\sCCS\\)","Electricity",class1),
        class1=gsub("elec\\_gas\\s\\(steam/CT\\)","Electricity",class1),
        class1=gsub("elec\\_gas\\s\\(CC\\sCCS\\)","Electricity",class1),
        class1=gsub("elec\\_gas\\s\\(CC\\)","Electricity",class1),
        class1=gsub("elec\\_refined\\sliquids\\s\\(CC\\)","Electricity",class1),
        class1=gsub("elec\\_refined\\sliquids\\s\\(steam/CT\\)","Electricity",class1),
        class1=gsub("elec\\_refined\\sliquids\\s\\(CC\\sCCS\\)","Electricity",class1),
        class1=dplyr::if_else(class1=="electricity","Electricity",class1),
        class1=gsub("electricity_net_ownuse","Electricity",class1),
        class1=gsub("base\\s\\load\\sgeneration","Electricity",class1),
        class1=gsub("subpeak\\sgeneration","Electricity",class1),
        class1=gsub("peak\\sgeneration","Electricity",class1),
        class1=gsub("intermediate\\sgeneration","Electricity",class1),
        class1=gsub("gas\\spipeline","Refining and Hydrogen Production",class1),
        class1=gsub("gas\\sprocessing","Refining and Hydrogen Production",class1),
        class1=gsub("H2\\scentral\\sproduction","Refining and Hydrogen Production",class1),
        class1=gsub("H2\\sforecourt\\sproduction","Refining and Hydrogen Production",class1),
        class1=gsub("oil\\srefining","Refining and Hydrogen Production",class1),
        class1=gsub("gas\\sto\\sliquids","Refining and Hydrogen Production",class1),
        class1=gsub("coal\\sto\\sliquids","Refining and Hydrogen Production",class1),
        class1=gsub("industrial\\senergy\\suse","Industry",class1),
        class1=gsub("industrial\\sfeedstocks","Industry",class1),
        class1=gsub("industrial\\sprocesses","Industry",class1),
        class1=gsub("urban\\sprocesses","Waste",class1),
        class1=gsub("N\\sfertilizer","Industry",class1),
        class1=gsub("process\\sheat\\scement","Industry",class1),
        class1=gsub("cement","Industry",class1),
        class1=dplyr::if_else(class1=="refining","Refining and Hydrogen Production",class1),
        class1=gsub("regional\\sbiomassOil","Refining and Hydrogen Production",class1),
        class1=gsub("regional\\ssugar\\sfor\\sethanol","Refining and Hydrogen Production",class1),
        class1=gsub("regional\\sbiomass","Refining and Hydrogen Production",class1),
        class1=gsub("regional\\scorn\\sfor\\sethanol","Refining and Hydrogen Production",class1),
        class1=gsub("resid\\scooling","Buildings",class1),
        class1=gsub("resid\\sheating","Buildings",class1),
        class1=gsub("resid\\sothers","Buildings",class1),
        class1=gsub("resid\\sother","Buildings",class1),
        class1=gsub("resid\\shot\\swater","Buildings",class1),
        class1=gsub("resid\\scooking","Buildings",class1),
        class1=gsub("resid\\sclothes\\sdryer","Buildings",class1),
        class1=gsub("district\\sheat","Buildings",class1),
        class1=gsub("trn\\_aviation\\_intl","Transport",class1),
        class1=gsub("trn\\_shipping\\_intl","Transport",class1),
        class1=dplyr::if_else(class1=="trn_freight_road","Transport",class1),
        class1=dplyr::if_else(class1=="trn_freight","Transport",class1),
        class1=dplyr::if_else(class1=="trn_pass","Transport",class1),
        class1=gsub("trn\\_pass\\_road\\_LDV\\_2W","Transport",class1),
        class1=gsub("trn\\_pass\\_road\\_LDV\\_4W","Transport",class1),
        class1=dplyr::if_else(class1=="trn_pass_road","Transport",class1),
        #class1=gsub("trn\\_shipping\\_intl","Transport",class1),
        class1=gsub("transport\\_LDV","Transport",class1),
        class1=gsub("transport\\_bus","Transport",class1),
        class1=dplyr::if_else(class1=="trn_pass","Transport",class1),
        class1=gsub("unconventional\\soil\\sproduction","Refining and Hydrogen Production",class1),
        class1=gsub("conventional\\sgas","Refining and Hydrogen Production",class1),
        class1=gsub("unconventional\\sgas\\sother","Refining and Hydrogen Production",class1),
        class1=gsub("delivered\\sgas","Industry",class1),
        class1=gsub("tight\\sgas","Refining and Hydrogen Production",class1),
        class1=gsub("delivered\\sbiomass","Industry",class1),
        class1=gsub("refined\\sliquids\\senduse","Industry",class1),
        class1=gsub("refined\\sliquids\\sindustrial","Industry",class1),
        class1=gsub("wholesale\\sgas","Industry",class1),
        class1=gsub("natural\\sgas","Refining and Hydrogen Production",class1),
        class1=gsub("biomass\\sliquids","Refining and Hydrogen Production",class1),
        class1=gsub("coalbed\\smethane","Refining and Hydrogen Production",class1),
        class1=gsub("shale\\sgas","Refining and Hydrogen Production",class1),
        class1=dplyr::if_else(class1=="coal","Refining and Hydrogen Production",class1),
        class1=gsub("crude oil","Refining and Hydrogen Production",class1),
        class1=gsub("electricity\\sdomestic\\ssupply","Refining and Hydrogen Production",class1),
        class1=gsub("Beef","Livestock",class1),
        class1=gsub("Dairy","Livestock",class1),
        class1=gsub("FiberCrop","Crops",class1),
        class1=gsub("MiscCrop","Crops",class1),
        class1=gsub("OilCrop","Crops",class1),
        class1=gsub("OtherGrain","Crops",class1),
        class1=gsub("PalmFruit","Crops",class1),
        class1=gsub("Pork","Livestock",class1),
        class1=gsub("Poultry","Livestock",class1),
        class1=gsub("Corn","Crops",class1),
        class1=gsub("Rice","Crops",class1),
        class1=gsub("Root_Tuber","Crops",class1),
        class1=gsub("SheepGoat","Crops",class1),
        class1=gsub("SugarCrop","Crops",class1),
        class1=gsub("UnmanagedLand","Crops",class1),
        class1=gsub("Wheat","Crops",class1),
        class1=gsub("biomass","Crops",class1),
        class1=gsub("FodderGrass","Crops",class1),
        class1=gsub("FodderHerb","Crops",class1),
        class1=dplyr::if_else(class1=="biomass","Crops",class1),
        class1=gsub("backup\\_electricity","Electricity",class1),
        class1=gsub("csp\\_backup","Electricity",class1))%>%
      dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
      dplyr::mutate(param = "emissCO2BySectorNoBio",
                    sources = "Sources",
                    origScen = scenario,
                    origQuery = queryx,
                    origX = year,
                    scenario = scenNewNames,
                    vintage = paste("Vint_", year, sep = ""),
                    x = year,
                    xLabel = "Year",
                    aggregate = "sum",
                    classLabel1 = "sector",
                    classPalette1 = "pal_metis",
                    classLabel2 = "sectorDetail",
                    classPalette2 = "pal_metis") %>%
      dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                    aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                    origScen, origQuery, origValue, origUnits, origX)%>%
      dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
      dplyr::filter(!is.na(value))
    datax <- dplyr::bind_rows(datax, tbl)}
  }


  paramx <- "emissCO2NonCO2BySectorGWPAR5"
  if(paramx %in% paramsSelectx){
    # GHG emissions (non CO2) by subsector, using AR5 GWP values
    queryx <- "nonCO2 emissions by sector"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      #emiss_sector_mapping <- read.csv(CO2mappingFile, skip=1)
      tbl <- tbl %>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(class1=ghg, class2=sector) %>%
        dplyr::mutate(class1 = dplyr::case_when ((grepl("HFC", class1)) ~ "HFCs",
                                                 (grepl("SF6", class1)) ~ "SF6",
                                                 (grepl("CO2", class1)) ~ "CO2",
                                                 (grepl("N2O", class1)) ~ "N2O",
                                                 (grepl("CH4", class1)) ~ "CH4",
                                                 (grepl("SO2", class1)) ~ "SO2",
                                                 (grepl("NH3", class1)) ~ "NH3",
                                                 (grepl("CF4", class1)) ~ "CF4",
                                                 (grepl("C2F6", class1)) ~ "C2F6",
                                                 TRUE ~ "Other"))%>%
        dplyr::filter(!class1 %in% c('SO2', 'NH3', 'Other')) %>%
        #dplyr::left_join(emiss_sector_mapping, by=c('class2' = 'class1')) %>%
        #dplyr::mutate(class2=agg_sector) %>%
        #dplyr::select(-agg_sector) %>%
        dplyr::mutate(
          class2=gsub("comm\\scooling","Buildings",class2),
          class2=gsub("comm\\scooking","Buildings",class2),
          class2=gsub("comm\\sheating","Buildings",class2),
          class2=gsub("comm\\sothers","Buildings",class2),
          class2=gsub("comm\\sother","Buildings",class2),
          class2=gsub("comm\\shot\\swater","Buildings",class2),
          class2=gsub("elec\\_biomass\\s\\(conv\\)","Electricity",class2),
          class2=gsub("elec\\_biomass\\s\\(IGCC\\)","Electricity",class2),
          class2=gsub("elec\\_biomass\\s\\(conv\\sCCS\\)","Electricity",class2),
          class2=gsub("elec\\_biomass\\s\\(IGCC\\sCCS\\)","Electricity",class2),
          class2=gsub("elec\\_coal\\s\\(conv\\spul\\)","Electricity",class2),
          class2=gsub("elec\\_coal\\s\\(IGCC\\)","Electricity",class2),
          class2=gsub("elec\\_coal\\s\\(conv\\s\\pul\\sCCS\\)","Electricity",class2),
          class2=gsub("elec\\_coal\\s\\(IGCC\\sCCS\\)","Electricity",class2),
          class2=gsub("elec\\_gas\\s\\(IGCC\\sCCS\\)","Electricity",class2),
          class2=gsub("elec\\_gas\\s\\(steam/CT\\)","Electricity",class2),
          class2=gsub("elec\\_gas\\s\\(CC\\sCCS\\)","Electricity",class2),
          class2=gsub("elec\\_gas\\s\\(CC\\)","Electricity",class2),
          class2=gsub("elec\\_refined\\sliquids\\s\\(CC\\)","Electricity",class2),
          class2=gsub("elec\\_refined\\sliquids\\s\\(steam/CT\\)","Electricity",class2),
          class2=gsub("elec\\_refined\\sliquids\\s\\(CC\\sCCS\\)","Electricity",class2),
          class2=dplyr::if_else(class2=="electricity","Electricity",class2),
          class2=gsub("electricity_net_ownuse","Electricity",class2),
          class2=gsub("base\\s\\load\\sgeneration","Electricity",class2),
          class2=gsub("subpeak\\sgeneration","Electricity",class2),
          class2=gsub("peak\\sgeneration","Electricity",class2),
          class2=gsub("intermediate\\sgeneration","Electricity",class2),
          class2=gsub("gas\\spipeline","Refining and Hydrogen Production",class2),
          class2=gsub("gas\\sprocessing","Refining and Hydrogen Production",class2),
          class2=gsub("H2\\scentral\\sproduction","Refining and Hydrogen Production",class2),
          class2=gsub("H2\\sforecourt\\sproduction","Refining and Hydrogen Production",class2),
          class2=gsub("oil\\srefining","Refining and Hydrogen Production",class2),
          class2=gsub("gas\\sto\\sliquids","Refining and Hydrogen Production",class2),
          class2=gsub("coal\\sto\\sliquids","Refining and Hydrogen Production",class2),
          class2=gsub("industrial\\senergy\\suse","Industry",class2),
          class2=gsub("industrial\\sfeedstocks","Industry",class2),
          class2=gsub("industrial\\sprocesses","Industry",class2),
          class2=gsub("urban\\sprocesses","Waste",class2),
          class2=gsub("N\\sfertilizer","Industry",class2),
          class2=gsub("process\\sheat\\scement","Industry",class2),
          class2=gsub("cement","Industry",class2),
          class2=dplyr::if_else(class2=="refining","Refining and Hydrogen Production",class2),
          class2=gsub("regional\\sbiomassOil","Refining and Hydrogen Production",class2),
          class2=gsub("regional\\ssugar\\sfor\\sethanol","Refining and Hydrogen Production",class2),
          class2=gsub("regional\\sbiomass","Refining and Hydrogen Production",class2),
          class2=gsub("regional\\scorn\\sfor\\sethanol","Refining and Hydrogen Production",class2),
          class2=gsub("resid\\scooling","Buildings",class2),
          class2=gsub("resid\\sheating","Buildings",class2),
          class2=gsub("resid\\sothers","Buildings",class2),
          class2=gsub("resid\\sother","Buildings",class2),
          class2=gsub("resid\\shot\\swater","Buildings",class2),
          class2=gsub("resid\\scooking","Buildings",class2),
          class2=gsub("resid\\sclothes\\sdryer","Buildings",class2),
          class2=gsub("district\\sheat","Buildings",class2),
          class2=gsub("trn\\_aviation\\_intl","Transport",class2),
          class2=dplyr::if_else(class2=="trn_freight_road","Transport",class2),
          class2=dplyr::if_else(class2=="trn_freight","Transport",class2),
          class2=dplyr::if_else(class2=="trn_pass","Transport",class2),
          class2=gsub("trn\\_pass\\_road\\_LDV\\_2W","Transport",class2),
          class2=gsub("trn\\_pass\\_road\\_LDV\\_4W","Transport",class2),
          class2=dplyr::if_else(class2=="trn_pass_road","Transport",class2),
          #class2=gsub("trn\\_shipping\\_intl","Transport",class2),
          class2=gsub("transport\\_LDV","Transport",class2),
          class2=gsub("transport\\_bus","Transport",class2),
          class2=dplyr::if_else(class2=="trn_pass","Transport",class2),
          class2=gsub("unconventional\\soil\\sproduction","Refining and Hydrogen Production",class2),
          class2=gsub("conventional\\sgas","Refining and Hydrogen Production",class2),
          class2=gsub("unconventional\\sgas\\sother","Refining and Hydrogen Production",class2),
          class2=gsub("delivered\\sgas","Industry",class2),
          class2=gsub("tight\\sgas","Refining and Hydrogen Production",class2),
          class2=gsub("delivered\\sbiomass","Industry",class2),
          class2=gsub("refined\\sliquids\\senduse","Industry",class2),
          class2=gsub("refined\\sliquids\\sindustrial","Industry",class2),
          class2=gsub("wholesale\\sgas","Industry",class2),
          class2=gsub("natural\\sgas","Refining and Hydrogen Production",class2),
          class2=gsub("biomass\\sliquids","Refining and Hydrogen Production",class2),
          class2=gsub("coalbed\\smethane","Refining and Hydrogen Production",class2),
          class2=gsub("shale\\sgas","Refining and Hydrogen Production",class2),
          class2=dplyr::if_else(class2=="coal","Refining and Hydrogen Production",class2),
          class2=gsub("crude oil","Refining and Hydrogen Production",class2),
          class2=gsub("electricity\\sdomestic\\ssupply","Refining and Hydrogen Production",class2),
          class2=gsub("Beef","Livestock",class2),
          class2=gsub("Dairy","Livestock",class2),
          class2=gsub("FiberCrop","Crops",class2),
          class2=gsub("MiscCrop","Crops",class2),
          class2=gsub("OilCrop","Crops",class2),
          class2=gsub("OtherGrain","Crops",class2),
          class2=gsub("PalmFruit","Crops",class2),
          class2=gsub("Pork","Livestock",class2),
          class2=gsub("Poultry","Livestock",class2),
          class2=gsub("Corn","Crops",class2),
          class2=gsub("Rice","Crops",class2),
          class2=gsub("Root_Tuber","Crops",class2),
          class2=gsub("SheepGoat","Crops",class2),
          class2=gsub("SugarCrop","Crops",class2),
          class2=gsub("UnmanagedLand","Crops",class2),
          class2=gsub("Wheat","Crops",class2),
          class2=gsub("biomass","Crops",class2),
          class2=gsub("FodderGrass","Crops",class2),
          class2=gsub("FodderHerb","Crops",class2),
          class2=dplyr::if_else(class2=="biomass","Crops",class2),
          class2=gsub("backup\\_electricity","Electricity",class2),
          class2=gsub("csp\\_backup","Electricity",class2))%>%
        dplyr::left_join(metis.assumptions()$GWP%>%dplyr::rename(class1=ghg),by="class1")%>%
        dplyr::left_join(metis.assumptions()$convertGgTgMTC,by="Units") %>%
        #dplyr::filter(!class1=='CO2') %>%
        dplyr::mutate(origValue=value,
                      value=value*GWPAR5*Convert,
                      origUnits=Units,
                      origUnits = dplyr::case_when(class1=="Other"~"Units",TRUE~origUnits),
                      units="Emissions GWP - MegaTonnes of CO2 eq. (MTCO2eq)")%>%
        dplyr::mutate(param = "emissCO2NonCO2BySectorGWPAR5",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origX = year,
                      scenario = scenNewNames,
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      classLabel1 = "GHG",
                      classPalette1 = "pal_metis",
                      classLabel2 = "sector",
                      classPalette2 = "pal_metis") %>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}

 paramx <- "emissMethaneBySource"
  if(paramx %in% paramsSelectx){
    # GHG emissions (non CO2) by subsector, using AR5 GWP values
    queryx <- "nonCO2 emissions by sector"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      #emiss_sector_mapping <- read.csv(CO2mappingFile, skip=1)
      tbl <- tbl %>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(class1=sector, class2=ghg) %>%
        dplyr::filter(class2 %in% c('CH4', 'CH4_AGR', 'CH4_AWB')) %>%
        dplyr::mutate(
          class1=gsub("comm\\scooling","Buildings",class1),
          class1=gsub("comm\\scooking","Buildings",class1),
          class1=gsub("comm\\sheating","Buildings",class1),
          class1=gsub("comm\\sothers","Buildings",class1),
          class1=gsub("comm\\sother","Buildings",class1),
          class1=gsub("comm\\shot\\swater","Buildings",class1),
          class1=gsub("elec\\_biomass\\s\\(conv\\)","Electricity",class1),
          class1=gsub("elec\\_biomass\\s\\(IGCC\\)","Electricity",class1),
          class1=gsub("elec\\_biomass\\s\\(conv\\sCCS\\)","Electricity",class1),
          class1=gsub("elec\\_biomass\\s\\(IGCC\\sCCS\\)","Electricity",class1),
          class1=gsub("elec\\_coal\\s\\(conv\\spul\\)","Electricity",class1),
          class1=gsub("elec\\_coal\\s\\(IGCC\\)","Electricity",class1),
          class1=gsub("elec\\_coal\\s\\(conv\\s\\pul\\sCCS\\)","Electricity",class1),
          class1=gsub("elec\\_coal\\s\\(IGCC\\sCCS\\)","Electricity",class1),
          class1=gsub("elec\\_gas\\s\\(IGCC\\sCCS\\)","Electricity",class1),
          class1=gsub("elec\\_gas\\s\\(steam/CT\\)","Electricity",class1),
          class1=gsub("elec\\_gas\\s\\(CC\\sCCS\\)","Electricity",class1),
          class1=gsub("elec\\_gas\\s\\(CC\\)","Electricity",class1),
          class1=gsub("elec\\_refined\\sliquids\\s\\(CC\\)","Electricity",class1),
          class1=gsub("elec\\_refined\\sliquids\\s\\(steam/CT\\)","Electricity",class1),
          class1=gsub("elec\\_refined\\sliquids\\s\\(CC\\sCCS\\)","Electricity",class1),
          class1=dplyr::if_else(class1=="electricity","Electricity",class1),
          class1=gsub("electricity_net_ownuse","Electricity",class1),
          class1=gsub("base\\s\\load\\sgeneration","Electricity",class1),
          class1=gsub("subpeak\\sgeneration","Electricity",class1),
          class1=gsub("peak\\sgeneration","Electricity",class1),
          class1=gsub("intermediate\\sgeneration","Electricity",class1),
          class1=gsub("gas\\spipeline","Refining and Hydrogen Production",class1),
          class1=gsub("gas\\sprocessing","Refining and Hydrogen Production",class1),
          class1=gsub("H2\\scentral\\sproduction","Refining and Hydrogen Production",class1),
          class1=gsub("H2\\sforecourt\\sproduction","Refining and Hydrogen Production",class1),
          class1=gsub("oil\\srefining","Refining and Hydrogen Production",class1),
          class1=gsub("gas\\sto\\sliquids","Refining and Hydrogen Production",class1),
          class1=gsub("coal\\sto\\sliquids","Refining and Hydrogen Production",class1),
          class1=gsub("industrial\\senergy\\suse","Industry",class1),
          class1=gsub("industrial\\sfeedstocks","Industry",class1),
          class1=gsub("industrial\\sprocesses","Industry",class1),
          class1=gsub("urban\\sprocesses","Waste",class1),
          class1=gsub("N\\sfertilizer","Industry",class1),
          class1=gsub("process\\sheat\\scement","Industry",class1),
          class1=gsub("cement","Industry",class1),
          class1=dplyr::if_else(class1=="refining","Refining and Hydrogen Production",class1),
          class1=gsub("regional\\sbiomassOil","Refining and Hydrogen Production",class1),
          class1=gsub("regional\\ssugar\\sfor\\sethanol","Refining and Hydrogen Production",class1),
          class1=gsub("regional\\sbiomass","Refining and Hydrogen Production",class1),
          class1=gsub("regional\\scorn\\sfor\\sethanol","Refining and Hydrogen Production",class1),
          class1=gsub("resid\\scooling","Buildings",class1),
          class1=gsub("resid\\sheating","Buildings",class1),
          class1=gsub("resid\\sothers","Buildings",class1),
          class1=gsub("resid\\sother","Buildings",class1),
          class1=gsub("resid\\shot\\swater","Buildings",class1),
          class1=gsub("resid\\scooking","Buildings",class1),
          class1=gsub("resid\\sclothes\\sdryer","Buildings",class1),
          class1=gsub("district\\sheat","Buildings",class1),
          class1=gsub("trn\\_aviation\\_intl","Transport",class1),
          class1=gsub("trn\\_shipping\\_intl","Transport",class1),
          class1=dplyr::if_else(class1=="trn_freight_road","Transport",class1),
          class1=dplyr::if_else(class1=="trn_freight","Transport",class1),
          class1=dplyr::if_else(class1=="trn_pass","Transport",class1),
          class1=gsub("trn\\_pass\\_road\\_LDV\\_2W","Transport",class1),
          class1=gsub("trn\\_pass\\_road\\_LDV\\_4W","Transport",class1),
          class1=dplyr::if_else(class1=="trn_pass_road","Transport",class1),
          #class1=gsub("trn\\_shipping\\_intl","Transport",class1),
          class1=gsub("transport\\_LDV","Transport",class1),
          class1=gsub("transport\\_bus","Transport",class1),
          class1=dplyr::if_else(class1=="trn_pass","Transport",class1),
          class1=gsub("unconventional\\soil\\sproduction","Refining and Hydrogen Production",class1),
          class1=gsub("conventional\\sgas","Refining and Hydrogen Production",class1),
          class1=gsub("unconventional\\sgas\\sother","Refining and Hydrogen Production",class1),
          class1=gsub("delivered\\sgas","Industry",class1),
          class1=gsub("tight\\sgas","Refining and Hydrogen Production",class1),
          class1=gsub("delivered\\sbiomass","Industry",class1),
          class1=gsub("refined\\sliquids\\senduse","Industry",class1),
          class1=gsub("refined\\sliquids\\sindustrial","Industry",class1),
          class1=gsub("wholesale\\sgas","Industry",class1),
          class1=gsub("natural\\sgas","Refining and Hydrogen Production",class1),
          class1=gsub("biomass\\sliquids","Refining and Hydrogen Production",class1),
          class1=gsub("coalbed\\smethane","Refining and Hydrogen Production",class1),
          class1=gsub("shale\\sgas","Refining and Hydrogen Production",class1),
          class1=dplyr::if_else(class1=="coal","Refining and Hydrogen Production",class1),
          class1=gsub("crude oil","Refining and Hydrogen Production",class1),
          class1=gsub("electricity\\sdomestic\\ssupply","Refining and Hydrogen Production",class1),
          class1=gsub("Beef","Livestock",class1),
          class1=gsub("Dairy","Livestock",class1),
          class1=gsub("FiberCrop","Crops",class1),
          class1=gsub("MiscCrop","Crops",class1),
          class1=gsub("OilCrop","Crops",class1),
          class1=gsub("OtherGrain","Crops",class1),
          class1=gsub("PalmFruit","Crops",class1),
          class1=gsub("Pork","Livestock",class1),
          class1=gsub("Poultry","Livestock",class1),
          class1=gsub("Corn","Crops",class1),
          class1=gsub("Rice","Crops",class1),
          class1=gsub("Root_Tuber","Crops",class1),
          class1=gsub("SheepGoat","Crops",class1),
          class1=gsub("SugarCrop","Crops",class1),
          class1=gsub("UnmanagedLand","Crops",class1),
          class1=gsub("Wheat","Crops",class1),
          class1=gsub("biomass","Crops",class1),
          class1=gsub("FodderGrass","Crops",class1),
          class1=gsub("FodderHerb","Crops",class1),
          class1=dplyr::if_else(class1=="biomass","Crops",class1),
          class1=gsub("backup\\_electricity","Electricity",class1),
          class1=gsub("csp\\_backup","Electricity",class1))%>%
        dplyr::left_join(metis.assumptions()$GWP%>%dplyr::rename(class2=ghg),by="class2")%>%
        dplyr::left_join(metis.assumptions()$convertGgTgMTC,by="Units") %>%
        dplyr::mutate(origValue=value,
                      value=value*GWPAR5*Convert,
                      origUnits=Units,
                      origUnits = dplyr::case_when(class2=="Other"~"Units",TRUE~origUnits),
                      units="Methane Emissions - MegaTonnes of CO2 eq. (MTCO2eq)")%>%
        dplyr::mutate(param = "emissMethaneBySource",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origX = year,
                      scenario = scenNewNames,
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      classLabel1 = "sector",
                      classPalette1 = "pal_metis",
                      classLabel2 = "GHG",
                      classPalette2 = "pal_metis") %>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}

  paramx <- "emissNonCO2ByResProdGWPAR5"
  if(paramx %in% paramsSelectx){
    # GHG emissions by resource production, using AR5 GWP values
    queryx <- "nonCO2 emissions by resource production"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      #emiss_sector_mapping <- read.csv(CO2mappingFile, skip=1)
      tbl <- tbl %>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(class1 = ghg, class2 = resource) %>%
        dplyr::mutate(class1 = dplyr::case_when ((grepl("HFC", class1)) ~ "HFCs",
                                                 (grepl("SF6", class1)) ~ "SF6",
                                                 (grepl("CO2", class1)) ~ "CO2",
                                                 (grepl("N2O", class1)) ~ "N2O",
                                                 (grepl("CH4", class1)) ~ "CH4",
                                                 (grepl("SO2", class1)) ~ "SO2",
                                                 (grepl("NH3", class1)) ~ "NH3",
                                                 (grepl("CF4", class1)) ~ "CF4",
                                                 (grepl("C2F6", class1)) ~ "C2F6",
                                                 TRUE ~ "Other"))%>%
        dplyr::filter(!class1 %in% c('SO2', 'NH3', 'Other')) %>%
        #dplyr::left_join(emiss_sector_mapping, by=c('class2' = 'class1')) %>%
        #dplyr::mutate(class2=agg_sector) %>%
        #dplyr::select(-agg_sector) %>%
        dplyr::mutate(
          class2=gsub("comm\\scooling","Buildings",class2),
          class2=gsub("comm\\scooking","Buildings",class2),
          class2=gsub("comm\\sheating","Buildings",class2),
          class2=gsub("comm\\sothers","Buildings",class2),
          class2=gsub("comm\\sother","Buildings",class2),
          class2=gsub("comm\\shot\\swater","Buildings",class2),
          class2=gsub("elec\\_biomass\\s\\(conv\\)","Electricity",class2),
          class2=gsub("elec\\_biomass\\s\\(IGCC\\)","Electricity",class2),
          class2=gsub("elec\\_biomass\\s\\(conv\\sCCS\\)","Electricity",class2),
          class2=gsub("elec\\_biomass\\s\\(IGCC\\sCCS\\)","Electricity",class2),
          class2=gsub("elec\\_coal\\s\\(conv\\spul\\)","Electricity",class2),
          class2=gsub("elec\\_coal\\s\\(IGCC\\)","Electricity",class2),
          class2=gsub("elec\\_coal\\s\\(conv\\s\\pul\\sCCS\\)","Electricity",class2),
          class2=gsub("elec\\_coal\\s\\(IGCC\\sCCS\\)","Electricity",class2),
          class2=gsub("elec\\_gas\\s\\(IGCC\\sCCS\\)","Electricity",class2),
          class2=gsub("elec\\_gas\\s\\(steam/CT\\)","Electricity",class2),
          class2=gsub("elec\\_gas\\s\\(CC\\sCCS\\)","Electricity",class2),
          class2=gsub("elec\\_gas\\s\\(CC\\)","Electricity",class2),
          class2=gsub("elec\\_refined\\sliquids\\s\\(CC\\)","Electricity",class2),
          class2=gsub("elec\\_refined\\sliquids\\s\\(steam/CT\\)","Electricity",class2),
          class2=gsub("elec\\_refined\\sliquids\\s\\(CC\\sCCS\\)","Electricity",class2),
          class2=dplyr::if_else(class2=="electricity","Electricity",class2),
          class2=gsub("electricity_net_ownuse","Electricity",class2),
          class2=gsub("base\\s\\load\\sgeneration","Electricity",class2),
          class2=gsub("subpeak\\sgeneration","Electricity",class2),
          class2=gsub("peak\\sgeneration","Electricity",class2),
          class2=gsub("intermediate\\sgeneration","Electricity",class2),
          class2=gsub("gas\\spipeline","Refining and Hydrogen Production",class2),
          class2=gsub("gas\\sprocessing","Refining and Hydrogen Production",class2),
          class2=gsub("H2\\scentral\\sproduction","Refining and Hydrogen Production",class2),
          class2=gsub("H2\\sforecourt\\sproduction","Refining and Hydrogen Production",class2),
          class2=gsub("oil\\srefining","Refining and Hydrogen Production",class2),
          class2=gsub("gas\\sto\\sliquids","Refining and Hydrogen Production",class2),
          class2=gsub("coal\\sto\\sliquids","Refining and Hydrogen Production",class2),
          class2=gsub("industrial\\senergy\\suse","Industry",class2),
          class2=gsub("industrial\\sfeedstocks","Industry",class2),
          class2=gsub("industrial\\sprocesses","Industry",class2),
          class2=gsub("urban\\sprocesses","Waste",class2),
          class2=gsub("N\\sfertilizer","Industry",class2),
          class2=gsub("process\\sheat\\scement","Industry",class2),
          class2=gsub("cement","Industry",class2),
          class2=dplyr::if_else(class2=="refining","Refining and Hydrogen Production",class2),
          class2=gsub("regional\\sbiomassOil","Refining and Hydrogen Production",class2),
          class2=gsub("regional\\ssugar\\sfor\\sethanol","Refining and Hydrogen Production",class2),
          class2=gsub("regional\\sbiomass","Refining and Hydrogen Production",class2),
          class2=gsub("regional\\scorn\\sfor\\sethanol","Refining and Hydrogen Production",class2),
          class2=gsub("resid\\scooling","Buildings",class2),
          class2=gsub("resid\\sheating","Buildings",class2),
          class2=gsub("resid\\sothers","Buildings",class2),
          class2=gsub("resid\\sother","Buildings",class2),
          class2=gsub("resid\\shot\\swater","Buildings",class2),
          class2=gsub("resid\\scooking","Buildings",class2),
          class2=gsub("resid\\sclothes\\sdryer","Buildings",class2),
          class2=gsub("district\\sheat","Buildings",class2),
          class2=gsub("trn\\_aviation\\_intl","Transport",class2),
          class2=dplyr::if_else(class2=="trn_freight_road","Transport",class2),
          class2=dplyr::if_else(class2=="trn_freight","Transport",class2),
          class2=dplyr::if_else(class2=="trn_pass","Transport",class2),
          class2=gsub("trn\\_pass\\_road\\_LDV\\_2W","Transport",class2),
          class2=gsub("trn\\_pass\\_road\\_LDV\\_4W","Transport",class2),
          class2=dplyr::if_else(class2=="trn_pass_road","Transport",class2),
          #class2=gsub("trn\\_shipping\\_intl","Transport",class2),
          class2=gsub("transport\\_LDV","Transport",class2),
          class2=gsub("transport\\_bus","Transport",class2),
          class2=dplyr::if_else(class2=="trn_pass","Transport",class2),
          class2=gsub("unconventional\\soil\\sproduction","Refining and Hydrogen Production",class2),
          class2=gsub("conventional\\sgas","Refining and Hydrogen Production",class2),
          class2=gsub("unconventional\\sgas\\sother","Refining and Hydrogen Production",class2),
          class2=gsub("delivered\\sgas","Industry",class2),
          class2=gsub("tight\\sgas","Refining and Hydrogen Production",class2),
          class2=gsub("delivered\\sbiomass","Industry",class2),
          class2=gsub("refined\\sliquids\\senduse","Industry",class2),
          class2=gsub("refined\\sliquids\\sindustrial","Industry",class2),
          class2=gsub("wholesale\\sgas","Industry",class2),
          class2=gsub("natural\\sgas","Refining and Hydrogen Production",class2),
          class2=gsub("biomass\\sliquids","Refining and Hydrogen Production",class2),
          class2=gsub("coalbed\\smethane","Refining and Hydrogen Production",class2),
          class2=gsub("shale\\sgas","Refining and Hydrogen Production",class2),
          class2=dplyr::if_else(class2=="coal","Refining and Hydrogen Production",class2),
          class2=gsub("crude oil","Refining and Hydrogen Production",class2),
          class2=gsub("electricity\\sdomestic\\ssupply","Refining and Hydrogen Production",class2),
          class2=gsub("Beef","Livestock",class2),
          class2=gsub("Dairy","Livestock",class2),
          class2=gsub("FiberCrop","Crops",class2),
          class2=gsub("MiscCrop","Crops",class2),
          class2=gsub("OilCrop","Crops",class2),
          class2=gsub("OtherGrain","Crops",class2),
          class2=gsub("PalmFruit","Crops",class2),
          class2=gsub("Pork","Livestock",class2),
          class2=gsub("Poultry","Livestock",class2),
          class2=gsub("Corn","Crops",class2),
          class2=gsub("Rice","Crops",class2),
          class2=gsub("Root_Tuber","Crops",class2),
          class2=gsub("SheepGoat","Crops",class2),
          class2=gsub("SugarCrop","Crops",class2),
          class2=gsub("UnmanagedLand","Crops",class2),
          class2=gsub("Wheat","Crops",class2),
          class2=gsub("biomass","Crops",class2),
          class2=gsub("FodderGrass","Crops",class2),
          class2=gsub("FodderHerb","Crops",class2),
          class2=dplyr::if_else(class2=="biomass","Crops",class2),
          class2=gsub("backup\\_electricity","Electricity",class2),
          class2=gsub("csp\\_backup","Electricity",class2))%>%
        dplyr::left_join(metis.assumptions()$GWP%>%dplyr::rename(class1=ghg),by="class1")%>%
        dplyr::left_join(metis.assumptions()$convertGgTgMTC,by="Units") %>%
        dplyr::filter(!class1=='CO2') %>%
        dplyr::mutate(origValue=value,
                      value=value*GWPAR5*Convert,
                      origUnits=Units,
                      origUnits = dplyr::case_when(class1=="Other"~"Units",TRUE~origUnits),
                      units="Non-CO2 Emissions by Resource GWP - MegaTonnes of CO2 eq. (MTCO2eq)")%>%
        dplyr::mutate(param = "emissNonCO2ByResProdGWPAR5",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origX = year,
                      scenario = scenNewNames,
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      classLabel1 = "GHG",
                      classPalette1 = "pal_metis",
                      classLabel2 = "sector",
                      classPalette2 = "pal_metis") %>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}

  if(any(c("emissNonCO2ByResProdGWPAR5", "emissCO2NonCO2BySectorGWPAR5") %in% unique(datax$param))){
paramx <- "emissTotalFFIBySec"
  if(paramx %in% paramsSelectx){
    # GHG emissions by resource production, using AR5 GWP values
    totalFFINonCO2 <- datax %>% dplyr::filter(param %in% c("emissNonCO2ByResProdGWPAR5", "emissCO2NonCO2BySectorGWPAR5")) %>%
      dplyr::filter(!class1=='CO2')
    # Rename so that the nonCO2 classes 1 and 2 are consistent with the CO2 classes 1 and 2
    totalFFINonCO2 <- totalFFINonCO2 %>%
      dplyr::mutate(class_temp = class2) %>%
      dplyr::mutate(class2 = class1) %>%
      dplyr::mutate(class1=class_temp) %>%
      dplyr::select(-class_temp)
    totalFFICO2 <- datax %>% dplyr::filter(param %in% c("emissCO2BySectorNoBio"))
    totalFFICO2Eq <- rbind(totalFFICO2, totalFFINonCO2)
    totalFFICO2Eq$param <- 'emissTotalFFIBySec'
    totalFFICO2Eq <- totalFFICO2Eq %>%
      dplyr::mutate(origQuery="comb_origQueries",
                    origUnits="comb_origUnits",
                    units="Emissions Total FFI by Sector - MegaTonnes of CO2 eq. (MTCO2eq)",
                    classLabel1 = "sector",
                    classLabel2 = "subSector",
                    classPalette1 = 'pal_metis')%>%
      dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                    aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                    origScen, origQuery, origValue, origUnits, origX)%>%
      dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
      dplyr::filter(!is.na(value))
    # Take this new parameter and put it in datax (main dataframe for ready-to-plot results)
    datax <- rbind(datax, totalFFICO2Eq)
  }} else {
    if(any(c("nonCO2 emissions by resource production","nonCO2 emissions by sector") %in% queriesSelectx)){
      print(paste("totalFFINonCO2 did not run so skipping param emissTotalBySec",sep=""))}
  }

  if(any(c("emissNonCO2ByResProdGWPAR5", "emissCO2NonCO2BySectorGWPAR5") %in% unique(datax$param))){
  paramx <- "emissTotalBySec"
  if(paramx %in% paramsSelectx){
    # Same as FFI Emiss by Sec, except we are now adding LUC. So really it is the whole emissions picture (or close to it)
    totalFFINonCO2 <- datax %>% dplyr::filter(param %in% c("emissNonCO2ByResProdGWPAR5", "emissCO2NonCO2BySectorGWPAR5")) %>%
      dplyr::filter(!class1=='CO2')
    # Rename so that the nonCO2 classes 1 and 2 are consistent with the CO2 classes 1 and 2
    totalFFINonCO2 <- totalFFINonCO2 %>%
      dplyr::mutate(class_temp = class2) %>%
      dplyr::mutate(class2 = class1) %>%
      dplyr::mutate(class1=class_temp) %>%
      dplyr::select(-class_temp)
    totalFFICO2 <- datax %>% dplyr::filter(param %in% c("emissCO2BySectorNoBio", "emissLUC"))
    totalFFICO2Eq <- rbind(totalFFICO2, totalFFINonCO2)
    totalFFICO2Eq$param <- 'emissTotalBySec'
    totalFFICO2Eq$Class1Palette <- 'pal_metis'
    totalFFICO2Eq <- totalFFICO2Eq %>%
      dplyr::mutate(origQuery="comb_origQueries",
                    origUnits="comb_origUnits",
                    units="Emissions by Sector - MegaTonnes of CO2 eq. (MTCO2eq)",
                    classLabel1 = "sector",
                    classLabel2 = "subSector",
                    classPalette1 = 'pal_metis')%>%
      dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                    aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                    origScen, origQuery, origValue, origUnits, origX)%>%
      dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
      dplyr::filter(!is.na(value))
    # Take this new parameter and put it in datax (main dataframe for ready-to-plot results)
    datax <- rbind(datax, totalFFICO2Eq)
  }} else {
    if(any(c("nonCO2 emissions by resource production","nonCO2 emissions by sector") %in% queriesSelectx)){
    print(paste("totalFFINonCO2 did not run so skipping param emissTotalBySec",sep=""))}
    }

  if(any(c("emissNonCO2ByResProdGWPAR5", "emissCO2NonCO2BySectorGWPAR5") %in% unique(datax$param))){
    paramx <- "emissCO2BySectorNonCO2GWPAR5"
  if(paramx %in% paramsSelectx){
    # GHG emissions by resource production, using AR5 GWP values
    totalFFINonCO2 <- datax %>% dplyr::filter(param %in% c("emissNonCO2ByResProdGWPAR5", "emissCO2NonCO2BySectorGWPAR5")) %>%
      dplyr::filter(!class1=='CO2')
    # Rename so that the nonCO2 classes 1 and 2 are consistent with the CO2 classes 1 and 2
    totalFFICO2 <- datax %>% dplyr::filter(param %in% c("emissCO2BySectorNoBio")) %>% dplyr::mutate(
      class1=dplyr::if_else(class1=="Buildings", "CO2 Buildings", class1),
      class1=dplyr::if_else(class1=="Refining and Hydrogen Production", "CO2 Refining and Hydrogen Production", class1),
      class1=dplyr::if_else(class1=="Industry", "CO2 Industry", class1),
      class1=dplyr::if_else(class1=="Transport", "CO2 Transport", class1),
      class1=dplyr::if_else(class1=="Trn_shipping_intl", "CO2 Transport", class1),
      class1=dplyr::if_else(class1=="Electricity", "CO2 Electricity", class1),
      class1=dplyr::if_else(class1=="Other", "CO2 Other", class1),
      class1=dplyr::if_else(class1=="Waste", "CO2 Waste", class1),
      class1=dplyr::if_else(class1=="LUC", "CO2 LUC", class1)
    )
    totalFFICO2Eq <- rbind(totalFFICO2, totalFFINonCO2)
    totalFFICO2Eq$param <- 'emissCO2BySectorNonCO2GWPAR5'
    totalFFICO2Eq <- totalFFICO2Eq %>%
      dplyr::mutate(origQuery="comb_origQueries",
                    origUnits="comb_origUnits",
                    units="MegaTonnes of CO2 eq. (MTCO2eq)",
                    classLabel1 = "sector",
                    classLabel2 = "subSector",
                    classPalette1 = 'pal_metis')%>%
      dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                    aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                    origScen, origQuery, origValue, origUnits, origX)%>%
      dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
      dplyr::filter(!is.na(value))
    # Take this new parameter and put it in datax (main dataframe for ready-to-plot results)
    datax <- rbind(datax, totalFFICO2Eq)
  }} else {
    if(any(c("nonCO2 emissions by resource production","nonCO2 emissions by sector") %in% queriesSelectx)){
    print(paste("totalFFINonCO2 did not run so skipping param emissTotalBySec",sep=""))}
    }

  if(any(c("emissNonCO2ByResProdGWPAR5", "emissCO2NonCO2BySectorGWPAR5",
           "emissLUC","emissCO2BySectorNoBio") %in% unique(datax$param))){
    paramx <- "emissCO2BySectorNonCO2GWPAR5LUC"
  if(paramx %in% paramsSelectx){

    totalFFICO2 <- datax %>% dplyr::filter(param %in% c("emissCO2BySectorNoBio")) %>% dplyr::mutate(
      class1=dplyr::if_else(class1=="Buildings", "CO2 Buildings", class1),
      class1=dplyr::if_else(class1=="Refining and Hydrogen Production", "CO2 Refining and Hydrogen Production", class1),
      class1=dplyr::if_else(class1=="Industry", "CO2 Industry", class1),
      class1=dplyr::if_else(class1=="Transport", "CO2 Transport", class1),
      class1=dplyr::if_else(class1=="Trn_shipping_intl", "CO2 Transport", class1),
      class1=dplyr::if_else(class1=="Electricity", "CO2 Electricity", class1),
      class1=dplyr::if_else(class1=="Other", "CO2 Other", class1),
      class1=dplyr::if_else(class1=="Waste", "CO2 Waste", class1),
      class1=dplyr::if_else(class1=="LUC", "CO2 LUC", class1)
    )
    # GHG emissions by resource production, using AR5 GWP values
    NonCo2_LUC <- datax %>% dplyr::filter(param %in% c("emissNonCO2ByResProdGWPAR5", "emissCO2NonCO2BySectorGWPAR5",
                                                "emissLUC")) %>%
      dplyr::filter(!class1=='CO2')
    totalCO2Eq <- rbind(totalFFICO2, NonCo2_LUC)

    # Rename so that the nonCO2 classes 1 and 2 are consistent with the CO2 classes 1 and 2
    totalCO2Eq$param <- 'emissCO2BySectorNonCO2GWPAR5LUC'
    totalCO2Eq <- totalCO2Eq %>%
      dplyr::mutate(origQuery="comb_origQueries",
                    origUnits="comb_origUnits",
                    units="MegaTonnes of CO2 eq. (MTCO2eq)",
                    classLabel1 = "sector",
                    classLabel2 = "subSector",
                    classPalette1 = 'pal_metis')%>%
      dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                    aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                    origScen, origQuery, origValue, origUnits, origX)%>%
      dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
      dplyr::filter(!is.na(value))
    # Take this new parameter and put it in datax (main dataframe for ready-to-plot results)
    datax <- rbind(datax, totalCO2Eq)
  }

  } else {
    if(any(c("nonCO2 emissions by resource production","nonCO2 emissions by sector") %in% queriesSelectx)){
      print(paste("totalFFINonCO2 did not run so skipping param emissTotalBySec",sep=""))}
  }

  paramx <- "emissCO2NonCO2BySectorGTPAR5"
  if(paramx %in% paramsSelectx){
    # GHG emissions by subsector
    queryx <- "nonCO2 emissions by sector"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(class1=ghg, class2=sector) %>%
        dplyr::mutate(class1 = dplyr::case_when ((grepl("HFC", class1)) ~ "HFCs",
                                                 (grepl("SF6", class1)) ~ "SF6",
                                                 (grepl("CO2", class1)) ~ "CO2",
                                                 (grepl("N2O", class1)) ~ "N2O",
                                                 (grepl("CH4", class1)) ~ "CH4",
                                                 (grepl("SO2", class1)) ~ "SO2",
                                                 (grepl("NH3", class1)) ~ "NH3",
                                                 (grepl("CF4", class1)) ~ "CF4",
                                                 (grepl("C2F6", class1)) ~ "C2F6",
                                                 TRUE ~ "Other"))%>%
        dplyr::filter(!class1 %in% c('SO2', 'NH3', 'Other')) %>%
        dplyr::mutate(
          class2=gsub("comm\\scooling","Buildings",class2),
          class2=gsub("comm\\scooking","Buildings",class2),
          class2=gsub("comm\\sheating","Buildings",class2),
          class2=gsub("comm\\sothers","Buildings",class2),
          class2=gsub("comm\\sother","Buildings",class2),
          class2=gsub("comm\\shot\\swater","Buildings",class2),
          class2=gsub("elec\\_biomass\\s\\(conv\\)","Electricity",class2),
          class2=gsub("elec\\_biomass\\s\\(IGCC\\)","Electricity",class2),
          class2=gsub("elec\\_biomass\\s\\(conv\\sCCS\\)","Electricity",class2),
          class2=gsub("elec\\_biomass\\s\\(IGCC\\sCCS\\)","Electricity",class2),
          class2=gsub("elec\\_coal\\s\\(conv\\spul\\)","Electricity",class2),
          class2=gsub("elec\\_coal\\s\\(IGCC\\)","Electricity",class2),
          class2=gsub("elec\\_coal\\s\\(conv\\s\\pul\\sCCS\\)","Electricity",class2),
          class2=gsub("elec\\_coal\\s\\(IGCC\\sCCS\\)","Electricity",class2),
          class2=gsub("elec\\_gas\\s\\(IGCC\\sCCS\\)","Electricity",class2),
          class2=gsub("elec\\_gas\\s\\(steam/CT\\)","Electricity",class2),
          class2=gsub("elec\\_gas\\s\\(CC\\sCCS\\)","Electricity",class2),
          class2=gsub("elec\\_gas\\s\\(CC\\)","Electricity",class2),
          class2=gsub("elec\\_refined\\sliquids\\s\\(CC\\)","Electricity",class2),
          class2=gsub("elec\\_refined\\sliquids\\s\\(steam/CT\\)","Electricity",class2),
          class2=gsub("elec\\_refined\\sliquids\\s\\(CC\\sCCS\\)","Electricity",class2),
          class2=dplyr::if_else(class2=="electricity","Electricity",class2),
          class2=gsub("electricity_net_ownuse","Electricity",class2),
          class2=gsub("base\\s\\load\\sgeneration","Electricity",class2),
          class2=gsub("subpeak\\sgeneration","Electricity",class2),
          class2=gsub("peak\\sgeneration","Electricity",class2),
          class2=gsub("intermediate\\sgeneration","Electricity",class2),
          class2=gsub("gas\\spipeline","Refining and Hydrogen Production",class2),
          class2=gsub("gas\\sprocessing","Refining and Hydrogen Production",class2),
          class2=gsub("H2\\scentral\\sproduction","Refining and Hydrogen Production",class2),
          class2=gsub("H2\\sforecourt\\sproduction","Refining and Hydrogen Production",class2),
          class2=gsub("oil\\srefining","Refining and Hydrogen Production",class2),
          class2=gsub("gas\\sto\\sliquids","Refining and Hydrogen Production",class2),
          class2=gsub("coal\\sto\\sliquids","Refining and Hydrogen Production",class2),
          class2=gsub("industrial\\senergy\\suse","Industry",class2),
          class2=gsub("industrial\\sfeedstocks","Industry",class2),
          class2=gsub("industrial\\sprocesses","Industry",class2),
          class2=gsub("urban\\sprocesses","Waste",class2),
          class2=gsub("N\\sfertilizer","Industry",class2),
          class2=gsub("process\\sheat\\scement","Industry",class2),
          class2=gsub("cement","Industry",class2),
          class2=dplyr::if_else(class2=="refining","Refining and Hydrogen Production",class2),
          class2=gsub("regional\\sbiomassOil","Refining and Hydrogen Production",class2),
          class2=gsub("regional\\ssugar\\sfor\\sethanol","Refining and Hydrogen Production",class2),
          class2=gsub("regional\\sbiomass","Refining and Hydrogen Production",class2),
          class2=gsub("regional\\scorn\\sfor\\sethanol","Refining and Hydrogen Production",class2),
          class2=gsub("resid\\scooling","Buildings",class2),
          class2=gsub("resid\\sheating","Buildings",class2),
          class2=gsub("resid\\sothers","Buildings",class2),
          class2=gsub("resid\\sother","Buildings",class2),
          class2=gsub("resid\\shot\\swater","Buildings",class2),
          class2=gsub("resid\\scooking","Buildings",class2),
          class2=gsub("resid\\sclothes\\sdryer","Buildings",class2),
          class2=gsub("district\\sheat","Buildings",class2),
          class2=gsub("trn\\_aviation\\_intl","Transport",class2),
          class2=dplyr::if_else(class2=="trn_freight_road","Transport",class2),
          class2=dplyr::if_else(class2=="trn_freight","Transport",class2),
          class2=dplyr::if_else(class2=="trn_pass","Transport",class2),
          class2=gsub("trn\\_pass\\_road\\_LDV\\_2W","Transport",class2),
          class2=gsub("trn\\_pass\\_road\\_LDV\\_4W","Transport",class2),
          class2=dplyr::if_else(class2=="trn_pass_road","Transport",class2),
          #class2=gsub("trn\\_shipping\\_intl","Transport",class2),
          class2=gsub("transport\\_LDV","Transport",class2),
          class2=gsub("transport\\_bus","Transport",class2),
          class2=dplyr::if_else(class2=="trn_pass","Transport",class2),
          class2=gsub("unconventional\\soil\\sproduction","Refining and Hydrogen Production",class2),
          class2=gsub("conventional\\sgas","Refining and Hydrogen Production",class2),
          class2=gsub("unconventional\\sgas\\sother","Refining and Hydrogen Production",class2),
          class2=gsub("delivered\\sgas","Industry",class2),
          class2=gsub("tight\\sgas","Refining and Hydrogen Production",class2),
          class2=gsub("delivered\\sbiomass","Industry",class2),
          class2=gsub("refined\\sliquids\\senduse","Industry",class2),
          class2=gsub("refined\\sliquids\\sindustrial","Industry",class2),
          class2=gsub("wholesale\\sgas","Industry",class2),
          class2=gsub("natural\\sgas","Refining and Hydrogen Production",class2),
          class2=gsub("biomass\\sliquids","Refining and Hydrogen Production",class2),
          class2=gsub("coalbed\\smethane","Refining and Hydrogen Production",class2),
          class2=gsub("shale\\sgas","Refining and Hydrogen Production",class2),
          class2=dplyr::if_else(class2=="coal","Refining and Hydrogen Production",class2),
          class2=gsub("crude oil","Refining and Hydrogen Production",class2),
          class2=gsub("electricity\\sdomestic\\ssupply","Refining and Hydrogen Production",class2),
          class2=gsub("Beef","Livestock",class2),
          class2=gsub("Dairy","Livestock",class2),
          class2=gsub("FiberCrop","Crops",class2),
          class2=gsub("MiscCrop","Crops",class2),
          class2=gsub("OilCrop","Crops",class2),
          class2=gsub("OtherGrain","Crops",class2),
          class2=gsub("PalmFruit","Crops",class2),
          class2=gsub("Pork","Livestock",class2),
          class2=gsub("Poultry","Livestock",class2),
          class2=gsub("Corn","Crops",class2),
          class2=gsub("Rice","Crops",class2),
          class2=gsub("Root_Tuber","Crops",class2),
          class2=gsub("SheepGoat","Crops",class2),
          class2=gsub("SugarCrop","Crops",class2),
          class2=gsub("UnmanagedLand","Crops",class2),
          class2=gsub("Wheat","Crops",class2),
          class2=gsub("biomass","Crops",class2),
          class2=gsub("FodderGrass","Crops",class2),
          class2=gsub("FodderHerb","Crops",class2),
          class2=dplyr::if_else(class2=="biomass","Crops",class2),
          class2=gsub("backup\\_electricity","Electricity",class2),
          class2=gsub("csp\\_backup","Electricity",class2))%>%
        dplyr::left_join(metis.assumptions()$GWP%>%dplyr::rename(class1=ghg),by="class1")%>%
        dplyr::left_join(metis.assumptions()$convertGgTgMTC,by="Units") %>%
        dplyr::mutate(origValue=value,
                      value=dplyr::case_when(!is.na(GTPAR5) ~ value*GTPAR5*Convert,
                                             TRUE ~  value*GWPAR5*Convert),
                      origUnits=Units,
                      origUnits = dplyr::case_when(class1=="Other"~"Units",TRUE~origUnits),
                      units="Emissions GTP - MegaTonnes of CO2 eq. (MTCO2eq)")%>%
        dplyr::mutate(param = "emissCO2NonCO2BySectorGTPAR5",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origX = year,
                      scenario = scenNewNames,
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      classLabel1 = "GHG",
                      classPalette1 = "pal_metis",
                      classLabel2 = "sector",
                      classPalette2 = "pal_metis") %>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}

  paramx <- "emissNonCO2BySectorOrigUnits"
  if(paramx %in% paramsSelectx){
    # GHG emissions by subsector
    queryx <- "nonCO2 emissions by sector"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::mutate(origValue=value,
                      origUnits=Units,
                      units="Variable Units")%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(class1=ghg, class2=sector) %>%
        dplyr::mutate(class1 = dplyr::case_when ((grepl("HFC", class1)) ~ paste("HFCs",units,sep="_"),
                                                 (grepl("SF6", class1)) ~ paste("SF6",units,sep="_"),
                                                 (grepl("CO2", class1)) ~ paste("CO2",units,sep="_"),
                                                 (grepl("N2O", class1)) ~ paste("N2O",units,sep="_"),
                                                 (grepl("CH4", class1)) ~ paste("CH4",units,sep="_"),
                                                 (grepl("NH3", class1)) ~ paste("NH3",units,sep="_"),
                                                 (grepl("NOx", class1)) ~ paste("NOx",units,sep="_"),
                                                 (grepl("VOC", class1)) ~ paste("VOC",units,sep="_"),
                                                 (grepl("SO2", class1)) ~ paste("SO2",units,sep="_"),
                                                 (grepl("CF4", class1)) ~ "CF4",
                                                 (grepl("C2F6", class1)) ~ "C2F6",
                                                 TRUE ~ "Other"))%>%
        dplyr::filter(!class1 %in% c('SO2', 'NH3', 'Other')) %>%
        dplyr::mutate(
          class2=gsub("comm\\scooling","Buildings",class2),
          class2=gsub("comm\\scooking","Buildings",class2),
          class2=gsub("comm\\sheating","Buildings",class2),
          class2=gsub("comm\\sothers","Buildings",class2),
          class2=gsub("comm\\sother","Buildings",class2),
          class2=gsub("comm\\shot\\swater","Buildings",class2),
          class2=gsub("elec\\_biomass\\s\\(conv\\)","Electricity",class2),
          class2=gsub("elec\\_biomass\\s\\(IGCC\\)","Electricity",class2),
          class2=gsub("elec\\_biomass\\s\\(conv\\sCCS\\)","Electricity",class2),
          class2=gsub("elec\\_biomass\\s\\(IGCC\\sCCS\\)","Electricity",class2),
          class2=gsub("elec\\_coal\\s\\(conv\\spul\\)","Electricity",class2),
          class2=gsub("elec\\_coal\\s\\(IGCC\\)","Electricity",class2),
          class2=gsub("elec\\_coal\\s\\(conv\\s\\pul\\sCCS\\)","Electricity",class2),
          class2=gsub("elec\\_coal\\s\\(IGCC\\sCCS\\)","Electricity",class2),
          class2=gsub("elec\\_gas\\s\\(IGCC\\sCCS\\)","Electricity",class2),
          class2=gsub("elec\\_gas\\s\\(steam/CT\\)","Electricity",class2),
          class2=gsub("elec\\_gas\\s\\(CC\\sCCS\\)","Electricity",class2),
          class2=gsub("elec\\_gas\\s\\(CC\\)","Electricity",class2),
          class2=gsub("elec\\_refined\\sliquids\\s\\(CC\\)","Electricity",class2),
          class2=gsub("elec\\_refined\\sliquids\\s\\(steam/CT\\)","Electricity",class2),
          class2=gsub("elec\\_refined\\sliquids\\s\\(CC\\sCCS\\)","Electricity",class2),
          class2=dplyr::if_else(class2=="electricity","Electricity",class2),
          class2=gsub("electricity_net_ownuse","Electricity",class2),
          class2=gsub("base\\s\\load\\sgeneration","Electricity",class2),
          class2=gsub("subpeak\\sgeneration","Electricity",class2),
          class2=gsub("peak\\sgeneration","Electricity",class2),
          class2=gsub("intermediate\\sgeneration","Electricity",class2),
          class2=gsub("gas\\spipeline","Refining and Hydrogen Production",class2),
          class2=gsub("gas\\sprocessing","Refining and Hydrogen Production",class2),
          class2=gsub("H2\\scentral\\sproduction","Refining and Hydrogen Production",class2),
          class2=gsub("H2\\sforecourt\\sproduction","Refining and Hydrogen Production",class2),
          class2=gsub("oil\\srefining","Refining and Hydrogen Production",class2),
          class2=gsub("gas\\sto\\sliquids","Refining and Hydrogen Production",class2),
          class2=gsub("coal\\sto\\sliquids","Refining and Hydrogen Production",class2),
          class2=gsub("industrial\\senergy\\suse","Industry",class2),
          class2=gsub("industrial\\sfeedstocks","Industry",class2),
          class2=gsub("industrial\\sprocesses","Industry",class2),
          class2=gsub("urban\\sprocesses","Waste",class2),
          class2=gsub("N\\sfertilizer","Industry",class2),
          class2=gsub("process\\sheat\\scement","Industry",class2),
          class2=gsub("cement","Industry",class2),
          class2=dplyr::if_else(class2=="refining","Refining and Hydrogen Production",class2),
          class2=gsub("regional\\sbiomassOil","Refining and Hydrogen Production",class2),
          class2=gsub("regional\\ssugar\\sfor\\sethanol","Refining and Hydrogen Production",class2),
          class2=gsub("regional\\sbiomass","Refining and Hydrogen Production",class2),
          class2=gsub("regional\\scorn\\sfor\\sethanol","Refining and Hydrogen Production",class2),
          class2=gsub("resid\\scooling","Buildings",class2),
          class2=gsub("resid\\sheating","Buildings",class2),
          class2=gsub("resid\\sothers","Buildings",class2),
          class2=gsub("resid\\sother","Buildings",class2),
          class2=gsub("resid\\shot\\swater","Buildings",class2),
          class2=gsub("resid\\scooking","Buildings",class2),
          class2=gsub("resid\\sclothes\\sdryer","Buildings",class2),
          class2=gsub("district\\sheat","Buildings",class2),
          class2=gsub("trn\\_aviation\\_intl","Transport",class2),
          class2=dplyr::if_else(class2=="trn_freight_road","Transport",class2),
          class2=dplyr::if_else(class2=="trn_freight","Transport",class2),
          class2=dplyr::if_else(class2=="trn_pass","Transport",class2),
          class2=gsub("trn\\_pass\\_road\\_LDV\\_2W","Transport",class2),
          class2=gsub("trn\\_pass\\_road\\_LDV\\_4W","Transport",class2),
          class2=dplyr::if_else(class2=="trn_pass_road","Transport",class2),
          #class2=gsub("trn\\_shipping\\_intl","Transport",class2),
          class2=gsub("transport\\_LDV","Transport",class2),
          class2=gsub("transport\\_bus","Transport",class2),
          class2=dplyr::if_else(class2=="trn_pass","Transport",class2),
          class2=gsub("unconventional\\soil\\sproduction","Refining and Hydrogen Production",class2),
          class2=gsub("conventional\\sgas","Refining and Hydrogen Production",class2),
          class2=gsub("unconventional\\sgas\\sother","Refining and Hydrogen Production",class2),
          class2=gsub("delivered\\sgas","Industry",class2),
          class2=gsub("tight\\sgas","Refining and Hydrogen Production",class2),
          class2=gsub("delivered\\sbiomass","Industry",class2),
          class2=gsub("refined\\sliquids\\senduse","Industry",class2),
          class2=gsub("refined\\sliquids\\sindustrial","Industry",class2),
          class2=gsub("wholesale\\sgas","Industry",class2),
          class2=gsub("natural\\sgas","Refining and Hydrogen Production",class2),
          class2=gsub("biomass\\sliquids","Refining and Hydrogen Production",class2),
          class2=gsub("coalbed\\smethane","Refining and Hydrogen Production",class2),
          class2=gsub("shale\\sgas","Refining and Hydrogen Production",class2),
          class2=dplyr::if_else(class2=="coal","Refining and Hydrogen Production",class2),
          class2=gsub("crude oil","Refining and Hydrogen Production",class2),
          class2=gsub("electricity\\sdomestic\\ssupply","Refining and Hydrogen Production",class2),
          class2=gsub("Beef","Livestock",class2),
          class2=gsub("Dairy","Livestock",class2),
          class2=gsub("FiberCrop","Crops",class2),
          class2=gsub("MiscCrop","Crops",class2),
          class2=gsub("OilCrop","Crops",class2),
          class2=gsub("OtherGrain","Crops",class2),
          class2=gsub("PalmFruit","Crops",class2),
          class2=gsub("Pork","Livestock",class2),
          class2=gsub("Poultry","Livestock",class2),
          class2=gsub("Corn","Crops",class2),
          class2=gsub("Rice","Crops",class2),
          class2=gsub("Root_Tuber","Crops",class2),
          class2=gsub("SheepGoat","Crops",class2),
          class2=gsub("SugarCrop","Crops",class2),
          class2=gsub("UnmanagedLand","Crops",class2),
          class2=gsub("Wheat","Crops",class2),
          class2=gsub("biomass","Crops",class2),
          class2=gsub("FodderGrass","Crops",class2),
          class2=gsub("FodderHerb","Crops",class2),
          class2=dplyr::if_else(class2=="biomass","Crops",class2),
          class2=gsub("backup\\_electricity","Electricity",class2),
          class2=gsub("csp\\_backup","Electricity",class2))%>%
        dplyr::mutate(param = "emissNonCO2BySectorOrigUnits",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origX = year,
                      scenario = scenNewNames,
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      classLabel1 = "GHG",
                      classPalette1 = "pal_metis",
                      classLabel2 = "sector",
                      classPalette2 = "pal_metis",
                      origUnits = dplyr::case_when(class1=="Other"~"Units",TRUE~origUnits)) %>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}

  paramx<-"transportPassengerVMTByMode"
  # Total final energy by aggregate end-use sector
  if(paramx %in% paramsSelectx){
    queryx <- "transport service output by mode"
    vmt_array <- c("trn_aviation_intl", "trn_pass", "trn_pass_road", "trn_pass_road_LDV",
                   "trn_pass_road_LDV_2W", "trn_pass_road_LDV_4W")
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::filter(sector %in% vmt_array, !mode %in% c('Cycle', 'Walk', '2W', '4W', 'LDV', 'road')) %>%
        dplyr::mutate(mode=gsub("International\\sAviation","Plane",mode),
                      mode=gsub("Domestic\\sAviation","Plane",mode),
                      mode=gsub("HSR","Plane",mode),
                      mode=gsub("Passenger\\sRail","Rail",mode),
                      mode=gsub("Bus","Bus",mode),
                      mode=gsub("Moped","MotorBike",mode),
                      mode=gsub("Motorcycle\\s[(]50-250cc[)]","MotorBike",mode),
                      mode=gsub("Motorcycle\\s[:(:][:>:]250cc[:):]","MotorBike",mode),
                      mode=gsub("Compact\\sCar","LDV",mode),
                      mode=gsub("Large\\sCar\\sand\\sSUV","LDV",mode),
                      mode=gsub("Mini\\sCar","LDV",mode),
                      mode=gsub("Subcompact\\sCar","LDV",mode),
                      param = "transportPassengerVMTByMode",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year,
                      scenario = scenNewNames,
                      units = "million pass-km",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = mode,
                      classLabel1 = "Mode",
                      classPalette1 = "pal_metis",
                      class2 = sector,
                      classLabel2 = "classLabel2",
                      classPalette2 = "classPalette2")%>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}

  paramx<-"transportFreightVMTByMode"
  # Total final energy by aggregate end-use sector
  if(paramx %in% paramsSelectx){
    queryx <- "transport service output by mode"
    vmt_array <- c("trn_freight", "trn_freight_road")
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::filter(sector %in% vmt_array, !mode %in% c('road')) %>%
        dplyr::mutate(mode=dplyr::if_else(mode=="Domestic Ship","Ship",mode),
                      mode=dplyr::if_else(mode=="Freight Rail","Rail",mode),
                      #mode=dplyr::if_else(mode=="International Ship","Ship",mode),
                      mode=dplyr::if_else(mode=="Truck (6-15t)","Truck",mode),
                      mode=dplyr::if_else(mode=="Truck (0-1t)","Truck",mode),
                      mode=dplyr::if_else(mode=="Truck (>15t)","Truck",mode),
                      param = "transportFreightVMTByMode",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year,
                      scenario = scenNewNames,
                      units = "million ton-km",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = mode,
                      classLabel1 = "Mode",
                      classPalette1 = "pal_metis",
                      class2 = sector,
                      classLabel2 = "classLabel2",
                      classPalette2 = "classPalette2")%>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}

  paramx<-"energyPrimaryRefLiqProdEJ"
  # Freight VMT (services) by fuel
  if(paramx %in% paramsSelectx){
    queryx <- "refined liquids production by subsector"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(subsector=dplyr::if_else(subsector=="biomass liquids","biomass liquids", subsector),
                      subsector=dplyr::if_else(subsector=="coal to liquids","coal to liquids", subsector),
                      subsector=dplyr::if_else(subsector=="gas to liquids","gas to liquids", subsector),
                      subsector=dplyr::if_else(subsector=="oil refining","oil refining", subsector),
                      param = "energyPrimaryRefLiqProdEJ",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year,
                      scenario = scenNewNames,
                      units = "Refined Liquids Production (EJ)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = subsector,
                      classLabel1 = "Liquid",
                      classPalette1 = "pal_metis",
                      class2 = sector,
                      classLabel2 = "Refining",
                      classPalette2 = "classPalette2")%>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))

      # Create table that stores total liquids production in refining
      QueryTbl <- tbl
      TotalLiquidsProdTbl <- QueryTbl %>%
        dplyr::group_by(scenario, region, x) %>%
        dplyr::summarise(TotalLiquids=sum(value))
      # Return to original table, and calculate fraction of total liquids production that is biofuels. QueryTbl can now
      # be applied to VMT by fuel categories (passenger and freight) below.
      FracBioFuel_tbl <- QueryTbl %>%
        dplyr::select(scenario, region, class1, x, value) %>%
        dplyr::left_join(TotalLiquidsProdTbl, by=c('x', 'scenario', 'region')) %>%
        dplyr::filter(class1=='biomass liquids') %>%
        dplyr::mutate(FracBioFuel=value/TotalLiquids) %>%
        dplyr::mutate(FracFossilFuel=1-FracBioFuel) %>%
        dplyr::select(-value, -TotalLiquids) %>%
        dplyr::mutate(class1 = 'liquids')  #only apply fraction to liquids

      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}

  paramx<-"transportPassengerVMTByFuel"
  # Passenger VMT (services) by fuel
  if(paramx %in% paramsSelectx){
    queryx <- "transport service output by tech (new)"
    vmt_array <- c("trn_aviation_intl", "trn_pass", "trn_pass_road", "trn_pass_road_LDV",
                   "trn_pass_road_LDV_2W", "trn_pass_road_LDV_4W")
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::filter(sector %in% vmt_array, !technology %in% c('Cycle', 'Walk', '2W', '4W', 'LDV', 'road')) %>%
        dplyr::mutate(technology=gsub("NG","gas", technology),
                      technology=gsub("FCEV","hydrogen", technology),
                      technology=gsub("BEV","electricity", technology),
                      technology=gsub("Electric","electricity", technology),
                      technology=gsub("Liquids","liquids", technology),
                      technology=gsub("Hybrid Liquids","liquids", technology),
                      technology=gsub("Hybrid liquids","liquids", technology),
                      param = "transportPassengerVMTByFuel",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year,
                      scenario = scenNewNames,
                      units = "million pass-km",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = technology,
                      classLabel1 = "Fuel",
                      classPalette1 = "pal_metis",
                      class2 = subsector,
                      classLabel2 = "subsector",
                      classPalette2 = "classPalette2")
      if("energyPrimaryRefLiqProdEJ" %in% paramsSelectx){
        # Break out biofuels
        tbl <- tbl %>%
          dplyr::left_join(FracBioFuel_tbl, by=c('scenario', 'region', 'x', 'class1')) %>%
          dplyr::mutate(value = dplyr::if_else(class1=='liquids', value*FracBioFuel, value)) %>%
          dplyr::select(-FracBioFuel, -FracFossilFuel) %>%
          dplyr::mutate(class1=dplyr::if_else(class1=='liquids', 'biofuel', class1))
        tbl2 <- tbl %>%
          dplyr::left_join(FracBioFuel_tbl %>% dplyr::mutate(class1='biofuel'), by=c('scenario', 'region', 'x', 'class1')) %>%
          dplyr::filter(class1=='biofuel') %>%
          dplyr::mutate(class1='fossil fuel') %>%
          dplyr::mutate(value=dplyr::if_else(class1=='fossil fuel', (value/FracBioFuel)*(1-FracBioFuel), value)) %>%
          dplyr::select(-FracBioFuel, -FracFossilFuel)
        tbl <- rbind(tbl, tbl2)
      }
      tbl <- tbl %>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}

  paramx<-"transportFreightVMTByFuel"
  # Freight VMT (services) by fuel
  if(paramx %in% paramsSelectx){
    queryx <- "transport service output by tech (new)"
    vmt_array <- c("trn_freight", "trn_freight_road")
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::filter(sector %in% vmt_array, !technology %in% c('road')) %>%
        dplyr::mutate(technology=gsub("NG","gas", technology),
                      technology=gsub("Liquids","liquids", technology),
                      technology=gsub("Electric","electricity", technology),
                      technology=gsub("Coal","coal", technology),
                      param = "transportFreightVMTByFuel",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year,
                      scenario = scenNewNames,
                      units = "million ton-km",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = technology,
                      classLabel1 = "Fuel",
                      classPalette1 = "pal_metis",
                      class2 = subsector,
                      classLabel2 = "subsector",
                      classPalette2 = "classPalette2")
      if("energyPrimaryRefLiqProdEJ" %in% paramsSelectx){
        # Break out biofuels
        tbl <- tbl %>%
          dplyr::left_join(FracBioFuel_tbl, by=c('scenario', 'region', 'x', 'class1')) %>%
          dplyr::mutate(value = dplyr::if_else(class1=='liquids', value*FracBioFuel, value)) %>%
          dplyr::select(-FracBioFuel, -FracFossilFuel) %>%
          dplyr::mutate(class1=dplyr::if_else(class1=='liquids', 'biofuel', class1))
        tbl2 <- tbl %>%
          dplyr::left_join(FracBioFuel_tbl %>% dplyr::mutate(class1='biofuel'), by=c('scenario', 'region', 'x', 'class1')) %>%
          dplyr::filter(class1=='biofuel') %>%
          dplyr::mutate(class1='fossil fuel') %>%
          dplyr::mutate(value=dplyr::if_else(class1=='fossil fuel', (value/FracBioFuel)*(1-FracBioFuel), value)) %>%
          dplyr::select(-FracBioFuel, -FracFossilFuel)
        tbl <- rbind(tbl, tbl2)
      }
      tbl <- tbl %>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}

  paramx<-"energyFinalSubsecByFuelTranspEJ"
  # Total final energy by aggregate end-use sector
  if(paramx %in% paramsSelectx){
    queryx <- "transport final energy by fuel"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::rename(sector=input) %>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(sector=gsub("elect_td_trn","electricity",sector),
                      sector=gsub("delivered gas","gas",sector),
                      sector=gsub("refined liquids enduse","liquids",sector),
                      sector=gsub("H2 enduse","hydrogen",sector),
                      sector=gsub("delivered coal","coal",sector),
                      param = "energyFinalSubsecByFuelTranspEJ",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year,
                      scenario = scenNewNames,
                      units = "Transport Final Energy by Fuel (EJ)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = sector,
                      classLabel1 = "Fuel",
                      classPalette1 = "pal_metis",
                      class2 = sector,
                      classLabel2 = "classLabel2",
                      classPalette2 = "classPalette2")
      if("energyPrimaryRefLiqProdEJ" %in% paramsSelectx){
        # Break out biofuels
        tbl <- tbl %>%
          dplyr::left_join(FracBioFuel_tbl, by=c('scenario', 'region', 'x', 'class1')) %>%
          dplyr::mutate(value = dplyr::if_else(class1=='liquids', value*FracBioFuel, value)) %>%
          dplyr::select(-FracBioFuel, -FracFossilFuel) %>%
          dplyr::mutate(class1=dplyr::if_else(class1=='liquids', 'biofuel', class1))
        tbl2 <- tbl %>%
          dplyr::left_join(FracBioFuel_tbl %>% dplyr::mutate(class1='biofuel'), by=c('scenario', 'region', 'x', 'class1')) %>%
          dplyr::filter(class1=='biofuel') %>%
          dplyr::mutate(class1='fossil fuel') %>%
          dplyr::mutate(value=dplyr::if_else(class1=='fossil fuel', (value/FracBioFuel)*(1-FracBioFuel), value)) %>%
          dplyr::select(-FracBioFuel, -FracFossilFuel)
        tbl <- rbind(tbl, tbl2)
      }
      tbl <- tbl %>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}


  datax<-datax%>%unique()


  # -----------
  # unit Conversions
  # -----------
  dataxEJtoMTOE <- datax %>% dplyr::filter(grepl("(EJ)",units)) %>%
    dplyr::mutate(value=value*metis.assumptions()$convEJ2MTOE,
                  units = gsub("\\(EJ\\)","- Million tons of Oil Equiv. (Mtoe)",units),
                  param = gsub("EJ","MTOE",param))

  dataxEJtoTWh <- datax %>% dplyr::filter(grepl("(EJ)",units)) %>%
    dplyr::mutate(value=value*metis.assumptions()$convEJ2TWh,
                  units = gsub("\\(EJ\\)","(TWh)",units),
                  param = gsub("EJ","TWh",param))

  datax <- dplyr::bind_rows(datax,dataxEJtoMTOE,dataxEJtoTWh)
  datax<-datax%>%unique()

  # metis.chart(tbl,xData="x",yData="value",useNewLabels = 0)

  #---------------------
  # Create Data Template
  #---------------------

  dataTemplate <- datax %>%
    dplyr::mutate(scenario = "Local Data", value = 0, sources="Sources", x=2010) %>%
    dplyr::select(scenario, region, sources, param, units, class1,class2, x, value) %>%
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

  if (file.exists(paste(getwd(),"/dataFiles/mapping/template_Regional_mapping.csv", sep = ""))){
    fullTemplateMapExisting <- data.table::fread(file=paste(getwd(),"/dataFiles/mapping/template_Regional_mapping.csv", sep = ""),encoding="Latin-1")
    fullTemplateMap <- fullTemplateMap %>% dplyr::bind_rows(fullTemplateMapExisting) %>% unique()
  }


  utils::write.csv(fullTemplateMap, file = paste(getwd(),"/dataFiles/mapping/template_Regional_mapping.csv", sep = ""),
                   row.names = F)

  if (is.null(regionsSelect) | regionsSelectAll==T) {
    utils::write.csv(datax, file = paste(dirOutputs, "/readGCAMTables/Tables_gcam/gcamDataTable_AllRegions_", min(range(datax$x)),
                                         "to", max(range(datax$x)), ".csv", sep = ""), row.names = F)
    print(paste("GCAM data table saved to: ", paste(dirOutputs, "/readGCAMTables/Tables_gcam/gcamDataTable_AllRegions.csv", sep = "")))

    utils::write.csv(dataTemplate, file = paste(dirOutputs, "/readGCAMTables/Tables_Templates/template_Regional_AllRegions.csv", sep = ""),
                     row.names = F)
    print(paste("GCAM data template saved to: ", paste(dirOutputs, "/readGCAMTables/Tables_Templates/template_Regional_AllRegions.csv", sep = "")))

  } else {

    if(!all(regionsSelect %in% unique(datax$region))){
      print(paste("Regions not available in data: ", paste(regionsSelect[!(regionsSelect %in% unique(datax$region))],collapse=", "), sep=""))
      print(paste("Running remaining regions: ",  paste(regionsSelect[(regionsSelect %in% unique(datax$region))],collapse=", "), sep=""))
    }

    for (region_i in regionsSelect[(regionsSelect %in% unique(datax$region))]) {

      print(paste("Saving data table for region: ",region_i,"...", sep = ""))
      utils::write.csv(datax %>% dplyr::filter(region == region_i),
                       file = paste(dirOutputs, "/readGCAMTables/Tables_gcam/gcamDataTable_",region_i,".csv", sep = ""),row.names = F)

      # Aggregate across classes
      dataxAggsums<-datax%>%
        dplyr::filter(aggregate=="sum")%>%
        dplyr::select(-tidyselect::contains("class"))%>%
        dplyr::group_by_at(dplyr::vars(-value,-origValue))%>%
        dplyr::summarize_at(c("value"),list(~sum(.,na.rm = T)))
      dataxAggmeans<-datax%>%
        dplyr::filter(aggregate=="mean")%>%
        dplyr::select(-tidyselect::contains("class"))%>%
        dplyr::group_by_at(dplyr::vars(-value,-origValue))%>%
        dplyr::summarize_at(c("value"),list(~mean(.,na.rm = T)))
      dataxAgg<-dplyr::bind_rows(dataxAggsums,dataxAggmeans)%>%dplyr::ungroup()

      utils::write.csv(dataxAgg %>% dplyr::filter(region == region_i),
                       file = paste(dirOutputs, "/readGCAMTables/Tables_gcam/gcamDataTable_",region_i,"_aggClass.csv", sep = ""),row.names = F)

      utils::write.csv(dataTemplate %>% dplyr::filter(region == region_i),
                       file = paste(dirOutputs, "/readGCAMTables/Tables_Templates/template_Regional_",region_i,".csv", sep = ""),row.names = F)
      #utils::write.csv(dataTemplate %>% dplyr::filter(region == region_i),
      #                 file = paste(dirOutputs, "/Tables/Tables_Local/local_Regional_",region_i,".csv", sep = ""),row.names = F)

      print(paste("Table saved to: ",dirOutputs, "/readGCAMTables/Tables_gcam/gcamDataTable_",region_i,"_aggClass.csv", sep = ""))
    }
  }

  return(list(data = datax, dataTemplate = dataTemplate, scenarios = scenarios, queries = queries))

    }
