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
#' @param queriesSelect Default = "All". Vector of queries to read from the queryxml for example
#' c("Total final energy by aggregate end-use sector", "Population by region"). The queries must be
#' availble in the queryxml file. Current list of queries and generated paramaters are:
#' \itemize{
#' \item "Total final energy by aggregate end-use sector". Parameters generated: finalNrgbySec.
#' \item "primary energy consumption by region (direct equivalent)".
#' Parameters generated: primNrgConsumByFuel
#' \item "Electricity generation by aggregate technology". Parameters generated: elecByTech
#' \item "water withdrawals by sector". Parameters generated: watWithdrawBySec
#' \item "water consumption by sector". Parameters generated: watConsumBySec
#' \item "water withdrawals by crop". Parameters generated: watWithdrawByCrop
#' \item "biophysical water demand by crop type and land region". Parameters generated: watBioPhysCons
#' \item "water withdrawals by water mapping source". Parameters generated: irrWatWithBasin
#' \item "water consumption by water mapping source". Parameters generated: irrWatConsBasin
#' \item "GDP per capita MER by region". Where MER is "Market Exchange Rate".
#' Parameters generated: gdpPerCapita.
#' \item "GDP MER by region". Where MER is "Market Exchange Rate".
#' Parameters generated: gdp, gdpGrowthRate
#' \item "Population by region". Parameters generated: pop.
#' \item "ag production by tech". Where technologies signify irrigated or rainfed.
#' Parameters generated: agProdbyIrrRfd
#' \item "Ag Production by Crop Type". Parameters generated: agProdBiomass, agProdForest, agProdByCrop
#' \item "land allocation by crop and water source". Parameters generated: landIrrRfd
#' \item "aggregated land allocation". Parameters generated: aggLandAlloc
#' \item "Land Use Change Emission". Parameters generated: LUCemissFut
#' \item "GHG emissions by subsector". Parameters generated: ghgEmissByGHGGROUPS, ghgEmissionByGHG
#' \item "CO2 emissions by sector". Parameters generated:co2emissionBySector
#' \item "nonCO2 emissions by sector". Parameters generated: nonco2emissionBySectorGWPAR5, nonco2emissionBySectorGTPAR5,nonco2emissionBySectorOrigUnits
#' }
#'
#' @param paramsSelect Default = "All". If desired select a subset of paramaters to analyze from the full list of parameters:
#' c("finalNrgbySec", "primNrgConsumByFuel", "elecByTech",
#' "watConsumBySec", "watWithdrawBySec", "watWithdrawByCrop", "watBioPhysCons", "irrWatWithBasin","irrWatConsBasin",
#' "gdpPerCapita", "gdp", "gdpGrowthRate", "pop", "agProdbyIrrRfd",
#' "agProdBiomass", "agProdForest", "agProdByCrop", "landIrrRfd", "aggLandAlloc",
#' "LUCemiss", "co2emissionBySector","nonco2emissionBySectorGWPAR5,nonco2emissionBySectorGTPAR5","nonco2emissionBySectorOrigUnits")
#' @param RawQueryCSV Optional. A default 'dataProj.proj' is produced if no .Proj file is specified.
#'
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
                           paramsSelect="All",
                           RawQueryCSV=NULL
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
    landleaf -> ghg -> Convert -> regionsSelectAll->cf1971to2100->gcamCapacityFactor -> . -> GWPAR5

  # Create necessary directories if they dont exist.
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
  # Read gcam database or existing dataProj.proj
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

    # Check for gcamdatbasePath and gcamdatabasename
    if(is.null(gcamdatabasePath) | is.null(gcamdatabaseName)){stop(paste("GCAM database: ", gcamdatabasePath,"/",gcamdatabaseName," is incorrect or doesn't exist.",sep=""))}
    if(!file.exists(paste(gcamdatabasePath, "/", gcamdatabaseName, sep = ""))){stop(paste("GCAM database: ", gcamdatabasePath,"/",gcamdatabaseName," is incorrect or doesn't exist.",sep=""))}

    if (file.exists(paste(dataProjPath, "/", dataProj, sep = ""))){
      file.remove(paste(dataProjPath, "/", dataProj, sep = ""))}  # Delete old project file


    for (scenario_i in scenOrigNames) {
      dataProj.proj <- rgcam::addScenario(conn = rgcam::localDBConn(gcamdatabasePath, gcamdatabaseName), proj = dataProj,
                                          scenario = scenario_i, queryFile = paste(queryPath, "/", queryxml, sep = ""))  # Check your queries file
    }
    file.copy(from = paste(getwd(), "/", dataProj, sep = ""), to = dataProjPath, overwrite = T,
              copy.mode = TRUE)
    file.remove(dataProj)
    dataProjLoaded <- rgcam::loadProject(paste(dataProjPath, "/", dataProj, sep = ""))
  }

  # Save list of scenarios and queries
  scenarios <- rgcam::listScenarios(dataProjLoaded)  # List of Scenarios in GCAM database
  queries <- rgcam::listQueries(dataProjLoaded)  # List of Queries in queryxml

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
    if(!all(queriesSelect %in% queries)){stop("No queries are available in queryxml.
                                              Please check your queriesSelect entries or your queryxml")} else {
                                                if(length(queriesSelect[!(queriesSelect %in% queries)])>0){
                                                  print(paste("Queries not available in queryxml: ", paste(queriesSelect[!(queriesSelect %in% queries)],collapse=", "), sep=""))
                                                  print(paste("Running remaining queriesSelect: ",  paste(queriesSelect[(queriesSelect %in% queries)],collapse=", "), sep=""))}
                                                queriesx <- queriesSelect}
  }


  if(any(paramsSelect=="All")){
    paramsSelectx=c("finalNrgbySec", "primNrgConsumByFuel", "elecByTech","elecCapBySubsector",
                    "watConsumBySec", "watWithdrawBySec", "watWithdrawByCrop", "watBioPhysCons", "irrWatWithBasin","irrWatConsBasin",
                    "gdpPerCapita", "gdp", "gdpGrowthRate", "pop", "agProdbyIrrRfd",
                    "agProdBiomass", "agProdForest", "agProdByCrop", "landIrrRfd", "aggLandAlloc",
                    "LUCemiss", "co2emissionBySector","nonco2emissionBySectorGWPAR5","nonco2emissionBySectorGTPAR5","nonco2emissionBySectorOrigUnits",
                    "finalNrgbyFuel","finalElecbySec","finalElecbyFuel",
                    "NonCo2EmissionsByResProdGWPAR5", "TotalFFIEmissBySec",
                    "CO2BySector_NonCO2Gases_GWPAR5", "CO2BySector_NonCO2Gases_GWPAR5_LUC", "TotalEmissBySec",
                    "LandAllocByCrop", "TranspFinalNrgByFuel", "BuildFinalNrgByFuel", "IndFinalNrgByFuel",
                    "MethaneBySource", "PassengerVMTByMode", "FreightVMTByMode", "BuildFinalNrgBySector",
                    "co2emissionBySectorNoBio", "PassengerVMTByFuel", "FreightVMTByFuel", "RefiningByLiq")
  }else{paramsSelectx=paramsSelect}

  paramx<-"finalNrgbySec"
  # Total final energy by aggregate end-use sector
  if(paramx %in% paramsSelectx){
    queryx <- "Total final energy by aggregate end-use sector"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::left_join(dplyr::data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "finalNrgbySec",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year,
                      scenario = scenNewNames,
                      units = "Final Energy (EJ)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = sector,
                      classLabel1 = "Sector",
                      classPalette1 = "pal_nrg",
                      class2 = "class2",
                      classLabel2 = "classLabel2",
                      classPalette2 = "classPalette2")%>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),dplyr::funs(sum(.,na.rm = T)))%>%
        dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
    }}

  paramx<-"BuildFinalNrgBySector"
  # Total final energy by aggregate end-use sector
  if(paramx %in% paramsSelectx){
    queryx <- "building final energy by subsector"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::left_join(dplyr::data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "BuildFinalNrgBySector",
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
                      units = "Final Energy By Sector (EJ)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = sector,
                      classLabel1 = "Sector",
                      classPalette1 = "pal_nrg",
                      class2 = subsector,
                      classLabel2 = "classLabel2",
                      classPalette2 = "classPalette2")%>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),dplyr::funs(sum(.,na.rm = T)))%>%
        dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
    }}

  paramx<-"finalNrgbyFuel"
  # Total final energy by aggregate end-use sector
  if(paramx %in% paramsSelectx){
    queryx <- "Final energy by detailed end-use sector and fuel"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::left_join(dplyr::data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(input=if_else(input=="biomass","bioenergy",input),
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
                      param = "finalNrgbyFuel",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year,
                      scenario = scenNewNames,
                      units = "Final Energy (EJ)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = input,
                      classLabel1 = "Fuel",
                      classPalette1 = "pal_nrg",
                      class2 = sector,
                      classLabel2 = "classLabel2",
                      classPalette2 = "classPalette2")%>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),dplyr::funs(sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
    }}

  paramx<-"BuildFinalNrgByFuel"
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
        dplyr::left_join(dplyr::data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(sector=gsub("elect_td_bld","electricity",sector),
                      sector=gsub("delivered gas","gas",sector),
                      sector=gsub("delivered biomass","bioenergy",sector),
                      sector=gsub("delivered coal","coal",sector),
                      sector=gsub("refined liquids enduse","liquids",sector),
                      param = "BuildFinalNrgByFuel",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year,
                      scenario = scenNewNames,
                      units = "Building Final Energy (EJ)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = sector,
                      classLabel1 = "Fuel",
                      classPalette1 = "pal_nrg",
                      class2 = sector,
                      classLabel2 = "classLabel2",
                      classPalette2 = "classPalette2")%>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),dplyr::funs(sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
    }}

  paramx<-"IndFinalNrgByFuel"
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
        dplyr::left_join(dplyr::data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(sector=gsub("elect_td_ind","electricity",sector),
                      sector=gsub("wholesale gas","gas",sector),
                      sector=gsub("delivered biomass","bioenergy",sector),
                      sector=gsub("delivered coal","coal",sector),
                      sector=gsub("refined liquids industrial","liquids",sector),
                      sector=gsub("H2 enduse","hydrogen",sector),
                      param = "IndFinalNrgByFuel",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year,
                      scenario = scenNewNames,
                      units = "Industry Final Energy (EJ)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = sector,
                      classLabel1 = "Fuel",
                      classPalette1 = "pal_nrg",
                      class2 = sector,
                      classLabel2 = "classLabel2",
                      classPalette2 = "classPalette2")%>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),dplyr::funs(sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
    }}

  paramx<-"finalElecbySec"
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
        dplyr::left_join(dplyr::data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "finalElecbySec",
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
                      classPalette1 = "pal_nrg",
                      class2 = input,
                      classLabel2 = "input",
                      classPalette2 = "classPalette2")%>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),dplyr::funs(sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
    }}

  paramx<-"finalElecbyFuel"
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
        dplyr::left_join(dplyr::data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "finalElecbyFuel",
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
                      classPalette1 = "pal_nrg",
                      class2 = sector,
                      classLabel2 = "Sector",
                      classPalette2 = "classPalette2")%>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),dplyr::funs(sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
    }}

  paramx<-"primNrgConsumByFuel"
  # primary energy consumption by region (direct equivalent)
  if(paramx %in% paramsSelectx){
    queryx <- "primary energy consumption by region (direct equivalent) ORDERED SUBSECTORS"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::left_join(dplyr::data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "primNrgConsumByFuel",
                      fuel=gsub("biomass","bioenergy",fuel),
                      fuel=gsub("b biomass","b bioenergy",fuel),
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year,
                      scenario = scenNewNames,
                      units = "Primary Energy Consumption (EJ)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = fuel,
                      classLabel1 = "Fuel",
                      classPalette1 = "pal_nrg",
                      class2 = "class2",
                      classLabel2 = "classLabel2",
                      classPalette2 = "classPalette2")%>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),dplyr::funs(sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
    }}

  paramx <- "elecByTech"
  if(paramx %in% paramsSelectx){
    # Electricity generation by aggregate technology
    queryx <- "Electricity generation by aggregate technology ORDERED SUBSECTORS"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::left_join(dplyr::data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "elecByTech",
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
                      classPalette1 = "pal_nrg",
                      class2 = "class2",
                      classLabel2 = "classLabel2",
                      classPalette2 = "classPalette2")%>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources,class1,class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),dplyr::funs(sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
      tblElecbyTech<-tbl
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
    }}

  if(file.exists(paste(getwd(),"/dataFiles/gcam/capacity_factor_by_elec_gen_subsector.csv",sep=""))){
    capfactors <- data.table::fread(file=paste(getwd(),"/dataFiles/gcam/capacity_factor_by_elec_gen_subsector.csv",sep=""),skip=3,encoding="Latin-1")
    paramx <- "elecCapBySubsector"
    if(paramx %in% paramsSelectx){
      # Electricity Capacity by Subsector
      queryx <- "Electricity generation by aggregate technology"
      if (queryx %in% queriesx) {
        tbl <- tblElecbyTech  # Tibble
        rm(tblElecbyTech)
        if (!is.null(regionsSelect)) {
          tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
        }
        tbl <- tbl %>%
          dplyr::full_join(capfactors, by="class1")%>%
          dplyr::mutate(param = "elecCapBySubsector",
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
        print(paste("Query '", queryx, "' not found in database", sep = ""))
      }}
  } else {print(paste("Electricity capacity factor file capacity_factor_by_elec_gen_subsector.csv not found. Skipping param elecCapbySubSector."))}


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
        dplyr::left_join(dplyr::data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
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
                      classPalette1 = "pal_wat_dem",
                      class2 = "class2",
                      classLabel2 = "classLabel2",
                      classPalette2 = "classPalette2")%>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),dplyr::funs(sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
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
        dplyr::left_join(dplyr::data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
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
                      classPalette1 = "pal_wat_dem",
                      class2 = "class2",
                      classLabel2 = "classLabel2",
                      classPalette2 = "classPalette2")%>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),dplyr::funs(sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
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
        dplyr::left_join(dplyr::data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
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
                      classPalette1 = "pal_16",
                      class2 = "class2",
                      classLabel2 = "classLabel2",
                      classPalette2 = "classPalette2")%>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),dplyr::funs(sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
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
        dplyr::left_join(dplyr::data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
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
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),dplyr::funs(sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
    }}

  paramx <- "irrWatWithBasin"
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
        dplyr::left_join(dplyr::data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "irrWatWithBasin",
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
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),dplyr::funs(sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
    }}


  paramx <- "irrWatConsBasin"
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
        dplyr::left_join(dplyr::data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "irrWatConsBasin",
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
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),dplyr::funs(sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
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
        dplyr::left_join(dplyr::data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
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
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),dplyr::funs(sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
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
        dplyr::left_join(dplyr::data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
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
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),dplyr::funs(sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
      tblgdp<-tbl
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
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
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),dplyr::funs(sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
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
        dplyr::left_join(dplyr::data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
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
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),dplyr::funs(sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
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
        dplyr::left_join(dplyr::data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
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
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),dplyr::funs(sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
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
        dplyr::left_join(dplyr::data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
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
                      classPalette1 = "pal_ag_type",
                      class2 = "class2",
                      classLabel2 = "classLabel2",
                      classPalette2 = "classPalette2")%>%
        dplyr::select(origScen,origQuery, origValue, origUnits, origX, region, param, scenario,
                      value, units, vintage, x, xLabel, aggregate, class1, classLabel1, classPalette1,
                      class2, classLabel2, classPalette2)%>%dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
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
        dplyr::left_join(dplyr::data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
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
                      classPalette1 = "pal_ag_type",
                      class2 = "class2",
                      classLabel2 = "classLabel2",
                      classPalette2 = "classPalette2")%>%
        dplyr::select(origScen,origQuery, origValue, origUnits, origX, region, param, scenario,
                      value, units, vintage, x, xLabel, aggregate, class1, classLabel1, classPalette1,
                      class2, classLabel2, classPalette2)%>%dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
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
        dplyr::left_join(dplyr::data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
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
                      classPalette1 = "pal_ag_type",
                      class2 = "class2",
                      classLabel2 = "classLabel2",
                      classPalette2 = "classPalette2")%>%
        dplyr::select(origScen,origQuery, origValue, origUnits, origX, region, param, scenario,
                      value, units, vintage, x, xLabel, aggregate, class1, classLabel1, classPalette1,
                      class2, classLabel2, classPalette2)%>%dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
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
        dplyr::left_join(dplyr::data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
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
                      classPalette1 = "pal_16",
                      class2 = "class2",
                      classLabel2 = "classLabel2",
                      classPalette2 = "classPalette2") %>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),dplyr::funs(sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
    }}

  paramx <- "aggLandAlloc"
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
        dplyr::left_join(dplyr::data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "aggLandAlloc",
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
                      classPalette1 = "pal_lu_type",
                      classLabel2 = "classLabel2",
                      classPalette2 = "classPalette2") %>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),dplyr::funs(sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
    }}

  paramx <- "LandAllocByCrop"
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
        dplyr::left_join(dplyr::data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "LandAllocByCrop",
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
                      classPalette1 = "pal_ag_type",
                      classLabel2 = "classLabel2",
                      classPalette2 = "classPalette2") %>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),dplyr::funs(sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
    }}

  paramx <- "LUCemiss"
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
                      origUnits=Units,units="MegaTonnes of CO2 eq. (MTCO2eq)")%>%
        dplyr::left_join(dplyr::data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "LUCemiss",
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
                      classPalette1 = "pal_nrg",
                      class2 = "class2",
                      classLabel2 = "classLabel2",
                      classPalette2 = "classPalette2") %>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),dplyr::funs(sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value)) %>%
        dplyr::mutate(class1='LUC')
      tblLUEmiss<-tbl
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
    }}

  paramx <- "co2emissionBySector"
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
          class1=if_else(class1=="electricity","Electricity",class1),
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
          class1=if_else(class1=="refining","Refining and Hydrogen Production",class1),
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
          class1=if_else(class1=="trn_freight_road","Transport",class1),
          class1=if_else(class1=="trn_freight","Transport",class1),
          class1=if_else(class1=="trn_pass","Transport",class1),
          class1=gsub("trn\\_pass\\_road\\_LDV\\_2W","Transport",class1),
          class1=gsub("trn\\_pass\\_road\\_LDV\\_4W","Transport",class1),
          class1=if_else(class1=="trn_pass_road","Transport",class1),
          # class1=gsub("trn\\_shipping\\_intl","Transport",class1),
          class1=gsub("transport\\_LDV","Transport",class1),
          class1=gsub("transport\\_bus","Transport",class1),
          class1=if_else(class1=="trn_pass","Transport",class1),
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
          class1=if_else(class1=="coal","Refining and Hydrogen Production",class1),
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
          class1=if_else(class1=="biomass","Crops",class1),
          class1=gsub("backup\\_electricity","Electricity",class1),
          class1=gsub("csp\\_backup","Electricity",class1))%>%
        dplyr::left_join(dplyr::data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "co2emissionBySector",
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
                      classPalette1 = "pal_nrg",
                      classLabel2 = "sectorDetail",
                      classPalette2 = "pal_nrg") %>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),dplyr::funs(sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
    }}

  paramx <- "co2emissionBySectorNoBio"
  if(paramx %in% paramsSelectx){
    # CO2 emissions by subsector with bio correctly tracked and zeroed out (e.g., in power sector), using AR5 GWP values
    queryx <- "CO2 emissions by sector (no bio)"
    #tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
    # Read in Queries of CO2Emissions (no bio) from multiple scenarios
    co2Emiss_NoBio <- data.frame(matrix(ncol = 6, nrow = 0))
    colnames(co2Emiss_NoBio) <- c('scenario', 'region', 'sector', 'Units', 'Year', 'value')
    # Read in data from csv sheets, as this query doesn't work with rgcam
    for (scen in scenOrigNames){
      scen_index <- grep(scen, scenOrigNames)
      print(scen_index)
      pol_cO2emissRaw <- read.csv(RawQueryCSV[scen_index], skip=1)
      pol_cO2emissRaw$scenario <- as.character(pol_cO2emissRaw$scenario)
      pol_cO2emissRaw <- pol_cO2emissRaw %>% tidyr::gather(Year, value, `X1990`:`X2095`) %>%
        mutate(scenario=if_else(grepl(scenOrigNames[scen_index], scenario)==TRUE, scenOrigNames[scen_index], scenario))
      pol_cO2emissRaw$Year <- gsub('X', '', pol_cO2emissRaw$Year)
      pol_cO2emissRaw$Year <- as.numeric(pol_cO2emissRaw$Year)
      co2Emiss_NoBio <- rbind(co2Emiss_NoBio, pol_cO2emissRaw)
    }
    co2Emiss_NoBio <- co2Emiss_NoBio %>% rename(year=Year)
    tbl <- co2Emiss_NoBio
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
        class1=if_else(class1=="electricity","Electricity",class1),
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
        class1=if_else(class1=="refining","Refining and Hydrogen Production",class1),
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
        class1=if_else(class1=="trn_freight_road","Transport",class1),
        class1=if_else(class1=="trn_freight","Transport",class1),
        class1=if_else(class1=="trn_pass","Transport",class1),
        class1=gsub("trn\\_pass\\_road\\_LDV\\_2W","Transport",class1),
        class1=gsub("trn\\_pass\\_road\\_LDV\\_4W","Transport",class1),
        class1=if_else(class1=="trn_pass_road","Transport",class1),
        #class1=gsub("trn\\_shipping\\_intl","Transport",class1),
        class1=gsub("transport\\_LDV","Transport",class1),
        class1=gsub("transport\\_bus","Transport",class1),
        class1=if_else(class1=="trn_pass","Transport",class1),
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
        class1=if_else(class1=="coal","Refining and Hydrogen Production",class1),
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
        class1=if_else(class1=="biomass","Crops",class1),
        class1=gsub("backup\\_electricity","Electricity",class1),
        class1=gsub("csp\\_backup","Electricity",class1))%>%
      dplyr::left_join(dplyr::data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
      dplyr::mutate(param = "co2emissionBySectorNoBio",
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
                    classPalette1 = "pal_nrg",
                    classLabel2 = "sectorDetail",
                    classPalette2 = "pal_nrg") %>%
      dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                    aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                    origScen, origQuery, origValue, origUnits, origX)%>%
      dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),dplyr::funs(sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
      dplyr::filter(!is.na(value))
    datax <- dplyr::bind_rows(datax, tbl)}

  paramx <- "nonco2emissionBySectorGWPAR5"
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
        dplyr::left_join(dplyr::data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
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
          class2=if_else(class2=="electricity","Electricity",class2),
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
          class2=if_else(class2=="refining","Refining and Hydrogen Production",class2),
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
          class2=if_else(class2=="trn_freight_road","Transport",class2),
          class2=if_else(class2=="trn_freight","Transport",class2),
          class2=if_else(class2=="trn_pass","Transport",class2),
          class2=gsub("trn\\_pass\\_road\\_LDV\\_2W","Transport",class2),
          class2=gsub("trn\\_pass\\_road\\_LDV\\_4W","Transport",class2),
          class2=if_else(class2=="trn_pass_road","Transport",class2),
          #class2=gsub("trn\\_shipping\\_intl","Transport",class2),
          class2=gsub("transport\\_LDV","Transport",class2),
          class2=gsub("transport\\_bus","Transport",class2),
          class2=if_else(class2=="trn_pass","Transport",class2),
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
          class2=if_else(class2=="coal","Refining and Hydrogen Production",class2),
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
          class2=if_else(class2=="biomass","Crops",class2),
          class2=gsub("backup\\_electricity","Electricity",class2),
          class2=gsub("csp\\_backup","Electricity",class2))%>%
        dplyr::left_join(metis.assumptions()$GWP%>%dplyr::rename(class1=ghg),by="class1")%>%
        dplyr::left_join(metis.assumptions()$convertGgTgMTC,by="Units") %>%
        dplyr::filter(!class1=='CO2') %>%
        dplyr::mutate(origValue=value,
                      value=value*GWPAR5*Convert,
                      origUnits=Units,
                      origUnits = case_when(class1=="Other"~"Units",TRUE~origUnits),
                      units="Non-CO2 Emissions (Mt CO2-Eq)")%>%
        dplyr::mutate(param = "nonco2emissionBySectorGWPAR5",
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
                      classPalette1 = "pal_nrg",
                      classLabel2 = "sector",
                      classPalette2 = "pal_nrg") %>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),dplyr::funs(sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
    }}

 paramx <- "MethaneBySource"
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
        dplyr::left_join(dplyr::data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
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
          class1=if_else(class1=="electricity","Electricity",class1),
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
          class1=if_else(class1=="refining","Refining and Hydrogen Production",class1),
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
          class1=if_else(class1=="trn_freight_road","Transport",class1),
          class1=if_else(class1=="trn_freight","Transport",class1),
          class1=if_else(class1=="trn_pass","Transport",class1),
          class1=gsub("trn\\_pass\\_road\\_LDV\\_2W","Transport",class1),
          class1=gsub("trn\\_pass\\_road\\_LDV\\_4W","Transport",class1),
          class1=if_else(class1=="trn_pass_road","Transport",class1),
          #class1=gsub("trn\\_shipping\\_intl","Transport",class1),
          class1=gsub("transport\\_LDV","Transport",class1),
          class1=gsub("transport\\_bus","Transport",class1),
          class1=if_else(class1=="trn_pass","Transport",class1),
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
          class1=if_else(class1=="coal","Refining and Hydrogen Production",class1),
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
          class1=if_else(class1=="biomass","Crops",class1),
          class1=gsub("backup\\_electricity","Electricity",class1),
          class1=gsub("csp\\_backup","Electricity",class1))%>%
        dplyr::left_join(metis.assumptions()$GWP%>%dplyr::rename(class2=ghg),by="class2")%>%
        dplyr::left_join(metis.assumptions()$convertGgTgMTC,by="Units") %>%
        dplyr::mutate(origValue=value,
                      value=value*GWPAR5*Convert,
                      origUnits=Units,
                      origUnits = case_when(class2=="Other"~"Units",TRUE~origUnits),
                      units="Methane Emissions (Mt CO2-Eq)")%>%
        dplyr::mutate(param = "MethaneBySource",
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
                      classPalette1 = "pal_nrg",
                      classLabel2 = "GHG",
                      classPalette2 = "pal_nrg") %>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),dplyr::funs(sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
    }}

  paramx <- "NonCo2EmissionsByResProdGWPAR5"
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
        dplyr::left_join(dplyr::data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
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
          class2=if_else(class2=="electricity","Electricity",class2),
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
          class2=if_else(class2=="refining","Refining and Hydrogen Production",class2),
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
          class2=if_else(class2=="trn_freight_road","Transport",class2),
          class2=if_else(class2=="trn_freight","Transport",class2),
          class2=if_else(class2=="trn_pass","Transport",class2),
          class2=gsub("trn\\_pass\\_road\\_LDV\\_2W","Transport",class2),
          class2=gsub("trn\\_pass\\_road\\_LDV\\_4W","Transport",class2),
          class2=if_else(class2=="trn_pass_road","Transport",class2),
          #class2=gsub("trn\\_shipping\\_intl","Transport",class2),
          class2=gsub("transport\\_LDV","Transport",class2),
          class2=gsub("transport\\_bus","Transport",class2),
          class2=if_else(class2=="trn_pass","Transport",class2),
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
          class2=if_else(class2=="coal","Refining and Hydrogen Production",class2),
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
          class2=if_else(class2=="biomass","Crops",class2),
          class2=gsub("backup\\_electricity","Electricity",class2),
          class2=gsub("csp\\_backup","Electricity",class2))%>%
        dplyr::left_join(metis.assumptions()$GWP%>%dplyr::rename(class1=ghg),by="class1")%>%
        dplyr::left_join(metis.assumptions()$convertGgTgMTC,by="Units") %>%
        dplyr::filter(!class1=='CO2') %>%
        dplyr::mutate(origValue=value,
                      value=value*GWPAR5*Convert,
                      origUnits=Units,
                      origUnits = case_when(class1=="Other"~"Units",TRUE~origUnits),
                      units="Non-CO2 Emissions (Mt CO2-Eq)")%>%
        dplyr::mutate(param = "NonCo2EmissionsByResProdGWPAR5",
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
                      classPalette1 = "pal_nrg",
                      classLabel2 = "sector",
                      classPalette2 = "pal_nrg") %>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),dplyr::funs(sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
    }}

  paramx <- "TotalFFIEmissBySec"
  if(paramx %in% paramsSelectx){
    # GHG emissions by resource production, using AR5 GWP values
    totalFFINonCO2 <- datax %>% filter(param %in% c("NonCo2EmissionsByResProdGWPAR5", "nonco2emissionBySectorGWPAR5"))
    # Rename so that the nonCO2 classes 1 and 2 are consistent with the CO2 classes 1 and 2
    totalFFINonCO2 <- totalFFINonCO2 %>%
      mutate(class_temp = class2) %>%
      mutate(class2 = class1) %>%
      mutate(class1=class_temp) %>%
      select(-class_temp)
    totalFFICO2 <- datax %>% filter(param %in% c("co2emissionBySectorNoBio"))
    totalFFICO2Eq <- rbind(totalFFICO2, totalFFINonCO2)
    totalFFICO2Eq$param <- 'TotalFFIEmissBySec'
    totalFFICO2Eq <- totalFFICO2Eq %>%
      dplyr::mutate(origQuery="comb_origQueries",
                    origUnits="comb_origUnits",
                    units="MegaTonnes of CO2 eq. (MTCO2eq)",
                    classLabel1 = "sector",
                    classLabel2 = "subSector",
                    classPalette1 = 'pal_nrg')%>%
      dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                    aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                    origScen, origQuery, origValue, origUnits, origX)%>%
      dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),dplyr::funs(sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
      dplyr::filter(!is.na(value))
    # Take this new parameter and put it in datax (main dataframe for ready-to-plot results)
    datax <- rbind(datax, totalFFICO2Eq)
  }

  paramx <- "TotalEmissBySec"
  if(paramx %in% paramsSelectx){
    # Same as FFI Emiss by Sec, except we are now adding LUC. So really it is the whole emissions picture (or close to it)
    totalFFINonCO2 <- datax %>% filter(param %in% c("NonCo2EmissionsByResProdGWPAR5", "nonco2emissionBySectorGWPAR5"))
    # Rename so that the nonCO2 classes 1 and 2 are consistent with the CO2 classes 1 and 2
    totalFFINonCO2 <- totalFFINonCO2 %>%
      mutate(class_temp = class2) %>%
      mutate(class2 = class1) %>%
      mutate(class1=class_temp) %>%
      select(-class_temp)
    totalFFICO2 <- datax %>% filter(param %in% c("co2emissionBySectorNoBio", "LUCemiss"))
    totalFFICO2Eq <- rbind(totalFFICO2, totalFFINonCO2)
    totalFFICO2Eq$param <- 'TotalEmissBySec'
    totalFFICO2Eq$Class1Palette <- 'pal_nrg'
    totalFFICO2Eq <- totalFFICO2Eq %>%
      dplyr::mutate(origQuery="comb_origQueries",
                    origUnits="comb_origUnits",
                    units="MegaTonnes of CO2 eq. (MTCO2eq)",
                    classLabel1 = "sector",
                    classLabel2 = "subSector",
                    classPalette1 = 'pal_nrg')%>%
      dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                    aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                    origScen, origQuery, origValue, origUnits, origX)%>%
      dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),dplyr::funs(sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
      dplyr::filter(!is.na(value))
    # Take this new parameter and put it in datax (main dataframe for ready-to-plot results)
    datax <- rbind(datax, totalFFICO2Eq)
  }

  paramx <- "CO2BySector_NonCO2Gases_GWPAR5"
  if(paramx %in% paramsSelectx){
    # GHG emissions by resource production, using AR5 GWP values
    totalFFINonCO2 <- datax %>% filter(param %in% c("NonCo2EmissionsByResProdGWPAR5", "nonco2emissionBySectorGWPAR5"))
    # Rename so that the nonCO2 classes 1 and 2 are consistent with the CO2 classes 1 and 2
    totalFFICO2 <- datax %>% filter(param %in% c("co2emissionBySectorNoBio")) %>% mutate(
      class1=if_else(class1=="Buildings", "CO2 Buildings", class1),
      class1=if_else(class1=="Refining and Hydrogen Production", "CO2 Refining and Hydrogen Production", class1),
      class1=if_else(class1=="Industry", "CO2 Industry", class1),
      class1=if_else(class1=="Transport", "CO2 Transport", class1),
      class1=if_else(class1=="Electricity", "CO2 Electricity", class1),
      class1=if_else(class1=="Other", "CO2 Other", class1),
      class1=if_else(class1=="Waste", "CO2 Waste", class1),
      class1=if_else(class1=="LUC", "CO2 LUC", class1)
    )
    totalFFICO2Eq <- rbind(totalFFICO2, totalFFINonCO2)
    totalFFICO2Eq$param <- 'CO2BySector_NonCO2Gases_GWPAR5'
    totalFFICO2Eq <- totalFFICO2Eq %>%
      dplyr::mutate(origQuery="comb_origQueries",
                    origUnits="comb_origUnits",
                    units="MegaTonnes of CO2 eq. (MTCO2eq)",
                    classLabel1 = "sector",
                    classLabel2 = "subSector",
                    classPalette1 = 'pal_nrg')%>%
      dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                    aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                    origScen, origQuery, origValue, origUnits, origX)%>%
      dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),dplyr::funs(sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
      dplyr::filter(!is.na(value))
    # Take this new parameter and put it in datax (main dataframe for ready-to-plot results)
    datax <- rbind(datax, totalFFICO2Eq)
  }

  paramx <- "CO2BySector_NonCO2Gases_GWPAR5_LUC"
  if(paramx %in% paramsSelectx){

    totalFFICO2 <- datax %>% filter(param %in% c("co2emissionBySectorNoBio")) %>% mutate(
      class1=if_else(class1=="Buildings", "CO2 Buildings", class1),
      class1=if_else(class1=="Refining and Hydrogen Production", "CO2 Refining and Hydrogen Production", class1),
      class1=if_else(class1=="Industry", "CO2 Industry", class1),
      class1=if_else(class1=="Transport", "CO2 Transport", class1),
      class1=if_else(class1=="Electricity", "CO2 Electricity", class1),
      class1=if_else(class1=="Other", "CO2 Other", class1),
      class1=if_else(class1=="Waste", "CO2 Waste", class1),
      class1=if_else(class1=="LUC", "CO2 LUC", class1)
    )
    # GHG emissions by resource production, using AR5 GWP values
    NonCo2_LUC <- datax %>% filter(param %in% c("NonCo2EmissionsByResProdGWPAR5", "nonco2emissionBySectorGWPAR5",
                                                "LUCemiss"))
    totalCO2Eq <- rbind(totalFFICO2, NonCo2_LUC)

    # Rename so that the nonCO2 classes 1 and 2 are consistent with the CO2 classes 1 and 2
    totalCO2Eq$param <- 'CO2BySector_NonCO2Gases_GWPAR5_LUC'
    totalCO2Eq <- totalCO2Eq %>%
      dplyr::mutate(origQuery="comb_origQueries",
                    origUnits="comb_origUnits",
                    units="MegaTonnes of CO2 eq. (MTCO2eq)",
                    classLabel1 = "sector",
                    classLabel2 = "subSector",
                    classPalette1 = 'pal_nrg')%>%
      dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                    aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                    origScen, origQuery, origValue, origUnits, origX)%>%
      dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),dplyr::funs(sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
      dplyr::filter(!is.na(value))
    # Take this new parameter and put it in datax (main dataframe for ready-to-plot results)
    datax <- rbind(datax, totalCO2Eq)
  }

  paramx <- "nonco2emissionBySectorGTPAR5"
  if(paramx %in% paramsSelectx){
    # GHG emissions by subsector
    queryx <- "nonCO2 emissions by sector"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::left_join(dplyr::data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
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
          class2=if_else(class2=="electricity","Electricity",class2),
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
          class2=if_else(class2=="refining","Refining and Hydrogen Production",class2),
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
          class2=if_else(class2=="trn_freight_road","Transport",class2),
          class2=if_else(class2=="trn_freight","Transport",class2),
          class2=if_else(class2=="trn_pass","Transport",class2),
          class2=gsub("trn\\_pass\\_road\\_LDV\\_2W","Transport",class2),
          class2=gsub("trn\\_pass\\_road\\_LDV\\_4W","Transport",class2),
          class2=if_else(class2=="trn_pass_road","Transport",class2),
          #class2=gsub("trn\\_shipping\\_intl","Transport",class2),
          class2=gsub("transport\\_LDV","Transport",class2),
          class2=gsub("transport\\_bus","Transport",class2),
          class2=if_else(class2=="trn_pass","Transport",class2),
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
          class2=if_else(class2=="coal","Refining and Hydrogen Production",class2),
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
          class2=if_else(class2=="biomass","Crops",class2),
          class2=gsub("backup\\_electricity","Electricity",class2),
          class2=gsub("csp\\_backup","Electricity",class2))%>%
        dplyr::left_join(metis.assumptions()$GWP%>%dplyr::rename(class1=ghg),by="class1")%>%
        dplyr::left_join(metis.assumptions()$convertGgTgMTC,by="Units") %>%
        dplyr::mutate(origValue=value,
                      value=dplyr::case_when(!is.na(GTPAR5) ~ value*GTPAR5*Convert,
                                             TRUE ~  value*GWPAR5*Convert),
                      origUnits=Units,
                      origUnits = case_when(class1=="Other"~"Units",TRUE~origUnits),
                      units="100 yr GTP AR5")%>%
        dplyr::mutate(param = "nonco2emissionBySectorGTPAR5",
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
                      classPalette1 = "pal_nrg",
                      classLabel2 = "sector",
                      classPalette2 = "pal_nrg") %>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),dplyr::funs(sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
    }}

  paramx <- "nonco2emissionBySectorOrigUnits"
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
        dplyr::left_join(dplyr::data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
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
          class2=if_else(class2=="electricity","Electricity",class2),
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
          class2=if_else(class2=="refining","Refining and Hydrogen Production",class2),
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
          class2=if_else(class2=="trn_freight_road","Transport",class2),
          class2=if_else(class2=="trn_freight","Transport",class2),
          class2=if_else(class2=="trn_pass","Transport",class2),
          class2=gsub("trn\\_pass\\_road\\_LDV\\_2W","Transport",class2),
          class2=gsub("trn\\_pass\\_road\\_LDV\\_4W","Transport",class2),
          class2=if_else(class2=="trn_pass_road","Transport",class2),
          #class2=gsub("trn\\_shipping\\_intl","Transport",class2),
          class2=gsub("transport\\_LDV","Transport",class2),
          class2=gsub("transport\\_bus","Transport",class2),
          class2=if_else(class2=="trn_pass","Transport",class2),
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
          class2=if_else(class2=="coal","Refining and Hydrogen Production",class2),
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
          class2=if_else(class2=="biomass","Crops",class2),
          class2=gsub("backup\\_electricity","Electricity",class2),
          class2=gsub("csp\\_backup","Electricity",class2))%>%
        dplyr::mutate(param = "nonco2emissionBySectorOrigUnits",
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
                      classPalette1 = "pal_nrg",
                      classLabel2 = "sector",
                      classPalette2 = "pal_nrg",
                      origUnits = case_when(class1=="Other"~"Units",TRUE~origUnits)) %>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),dplyr::funs(sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
    }}

  paramx<-"PassengerVMTByMode"
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
        dplyr::left_join(dplyr::data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
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
                      param = "PassengerVMTByMode",
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
                      classPalette1 = "pal_nrg",
                      class2 = sector,
                      classLabel2 = "classLabel2",
                      classPalette2 = "classPalette2")%>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),dplyr::funs(sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
    }}

  paramx<-"FreightVMTByMode"
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
        dplyr::left_join(dplyr::data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::filter(sector %in% vmt_array, !mode %in% c('road')) %>%
        dplyr::mutate(mode=if_else(mode=="Domestic Ship","Ship",mode),
                      mode=if_else(mode=="Freight Rail","Rail",mode),
                      #mode=if_else(mode=="International Ship","Ship",mode),
                      mode=if_else(mode=="Truck (6-15t)","Truck",mode),
                      mode=if_else(mode=="Truck (0-1t)","Truck",mode),
                      mode=if_else(mode=="Truck (>15t)","Truck",mode),
                      param = "FreightVMTByMode",
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
                      classPalette1 = "pal_nrg",
                      class2 = sector,
                      classLabel2 = "classLabel2",
                      classPalette2 = "classPalette2")%>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),dplyr::funs(sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
    }}

  paramx<-"RefiningByLiq"
  # Freight VMT (services) by fuel
  if(paramx %in% paramsSelectx){
    queryx <- "refined liquids production by subsector"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::left_join(dplyr::data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(subsector=if_else(subsector=="biomass liquids","biomass liquids", subsector),
                      subsector=if_else(subsector=="coal to liquids","coal to liquids", subsector),
                      subsector=if_else(subsector=="gas to liquids","gas to liquids", subsector),
                      subsector=if_else(subsector=="oil refining","oil refining", subsector),
                      param = "RefiningByLiq",
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
                      classPalette1 = "pal_nrg",
                      class2 = sector,
                      classLabel2 = "Refining",
                      classPalette2 = "classPalette2")%>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),dplyr::funs(sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))

      # Create table that stores total liquids production in refining
      QueryTbl <- tbl
      TotalLiquidsProdTbl <- QueryTbl %>%
        group_by(scenario, region, x) %>%
        summarise(TotalLiquids=sum(value))
      # Return to original table, and calculate fraction of total liquids production that is biofuels. QueryTbl can now
      # be applied to VMT by fuel categories (passenger and freight) below.
      FracBioFuel_tbl <- QueryTbl %>%
        select(scenario, region, class1, x, value) %>%
        left_join(TotalLiquidsProdTbl, by=c('x', 'scenario', 'region')) %>%
        filter(class1=='biomass liquids') %>%
        mutate(FracBioFuel=value/TotalLiquids) %>%
        mutate(FracFossilFuel=1-FracBioFuel) %>%
        select(-value, -TotalLiquids) %>%
        mutate(class1 = 'liquids')  #only apply fraction to liquids

      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
    }}

  paramx<-"PassengerVMTByFuel"
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
        dplyr::left_join(dplyr::data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::filter(sector %in% vmt_array, !technology %in% c('Cycle', 'Walk', '2W', '4W', 'LDV', 'road')) %>%
        dplyr::mutate(technology=gsub("NG","gas", technology),
                      technology=gsub("FCEV","hydrogen", technology),
                      technology=gsub("BEV","electricity", technology),
                      technology=gsub("Electric","electricity", technology),
                      technology=gsub("Liquids","liquids", technology),
                      technology=gsub("Hybrid Liquids","liquids", technology),
                      technology=gsub("Hybrid liquids","liquids", technology),
                      param = "PassengerVMTByFuel",
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
                      classPalette1 = "pal_nrg",
                      class2 = subsector,
                      classLabel2 = "subsector",
                      classPalette2 = "classPalette2")
      if("RefiningByLiq" %in% paramsSelectx){
        # Break out biofuels
        tbl <- tbl %>%
          left_join(FracBioFuel_tbl, by=c('scenario', 'region', 'x', 'class1')) %>%
          mutate(value = if_else(class1=='liquids', value*FracBioFuel, value)) %>%
          select(-FracBioFuel, -FracFossilFuel) %>%
          mutate(class1=if_else(class1=='liquids', 'biofuel', class1))
        tbl2 <- tbl %>%
          left_join(FracBioFuel_tbl %>% mutate(class1='biofuel'), by=c('scenario', 'region', 'x', 'class1')) %>%
          filter(class1=='biofuel') %>%
          mutate(class1='fossil fuel') %>%
          mutate(value=if_else(class1=='fossil fuel', (value/FracBioFuel)*(1-FracBioFuel), value)) %>%
          select(-FracBioFuel, -FracFossilFuel)
        tbl <- rbind(tbl, tbl2)
      }
      tbl <- tbl %>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),dplyr::funs(sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
    }}

  paramx<-"FreightVMTByFuel"
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
        dplyr::left_join(dplyr::data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::filter(sector %in% vmt_array, !technology %in% c('road')) %>%
        dplyr::mutate(technology=gsub("NG","gas", technology),
                      technology=gsub("Liquids","liquids", technology),
                      technology=gsub("Electric","electricity", technology),
                      technology=gsub("Coal","coal", technology),
                      param = "FreightVMTByFuel",
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
                      classPalette1 = "pal_nrg",
                      class2 = subsector,
                      classLabel2 = "subsector",
                      classPalette2 = "classPalette2")
      if("RefiningByLiq" %in% paramsSelectx){
        # Break out biofuels
        tbl <- tbl %>%
          left_join(FracBioFuel_tbl, by=c('scenario', 'region', 'x', 'class1')) %>%
          mutate(value = if_else(class1=='liquids', value*FracBioFuel, value)) %>%
          select(-FracBioFuel, -FracFossilFuel) %>%
          mutate(class1=if_else(class1=='liquids', 'biofuel', class1))
        tbl2 <- tbl %>%
          left_join(FracBioFuel_tbl %>% mutate(class1='biofuel'), by=c('scenario', 'region', 'x', 'class1')) %>%
          filter(class1=='biofuel') %>%
          mutate(class1='fossil fuel') %>%
          mutate(value=if_else(class1=='fossil fuel', (value/FracBioFuel)*(1-FracBioFuel), value)) %>%
          select(-FracBioFuel, -FracFossilFuel)
        tbl <- rbind(tbl, tbl2)
      }
      tbl <- tbl %>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),dplyr::funs(sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
    }}

  paramx<-"TranspFinalNrgByFuel"
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
        dplyr::left_join(dplyr::data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(sector=gsub("elect_td_trn","electricity",sector),
                      sector=gsub("delivered gas","gas",sector),
                      sector=gsub("refined liquids enduse","liquids",sector),
                      sector=gsub("H2 enduse","hydrogen",sector),
                      sector=gsub("delivered coal","coal",sector),
                      param = "TranspFinalNrgByFuel",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year,
                      scenario = scenNewNames,
                      units = "Transport Final Energy (EJ)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = sector,
                      classLabel1 = "Fuel",
                      classPalette1 = "pal_nrg",
                      class2 = sector,
                      classLabel2 = "classLabel2",
                      classPalette2 = "classPalette2")
      if("RefiningByLiq" %in% paramsSelectx){
        # Break out biofuels
        tbl <- tbl %>%
          left_join(FracBioFuel_tbl, by=c('scenario', 'region', 'x', 'class1')) %>%
          mutate(value = if_else(class1=='liquids', value*FracBioFuel, value)) %>%
          select(-FracBioFuel, -FracFossilFuel) %>%
          mutate(class1=if_else(class1=='liquids', 'biofuel', class1))
        tbl2 <- tbl %>%
          left_join(FracBioFuel_tbl %>% mutate(class1='biofuel'), by=c('scenario', 'region', 'x', 'class1')) %>%
          filter(class1=='biofuel') %>%
          mutate(class1='fossil fuel') %>%
          mutate(value=if_else(class1=='fossil fuel', (value/FracBioFuel)*(1-FracBioFuel), value)) %>%
          select(-FracBioFuel, -FracFossilFuel)
        tbl <- rbind(tbl, tbl2)
      }
      tbl <- tbl %>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),dplyr::funs(sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
    }}


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
        dplyr::summarize_at(c("value"),dplyr::funs(sum(.,na.rm = T)))
      dataxAggmeans<-datax%>%
        dplyr::filter(aggregate=="mean")%>%
        dplyr::select(-tidyselect::contains("class"))%>%
        dplyr::group_by_at(dplyr::vars(-value,-origValue))%>%
        dplyr::summarize_at(c("value"),dplyr::funs(mean))
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
