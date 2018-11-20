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
#' \item "CO2 Emissions by enduse". Parameters generated: co2emission, co2emissionByEndUse,
#' \item "GHG emissions by subsector". Parameters generated: ghgEmissByGHGGROUPS, ghgEmissionByGHG
#' }
#'
#' @param paramsSelect Default = "All". If desired select a subset of paramaters to analyze from the full list of parameters:
#' c("finalNrgbySec", "primNrgConsumByFuel", "elecByTech",
#' "watConsumBySec", "watWithdrawBySec", "watWithdrawByCrop", "watBioPhysCons", "irrWatWithBasin","irrWatConsBasin",
#' "gdpPerCapita", "gdp", "gdpGrowthRate", "pop", "agProdbyIrrRfd",
#' "agProdBiomass", "agProdForest", "agProdByCrop", "landIrrRfd", "aggLandAlloc",
#' "LUCemiss", "co2emission", "co2emissionByEndUse", "ghgEmissionByGHG", "ghgEmissByGHGGROUPS")
#'
#' @return A list with the scenarios in the gcam database, queries in the queryxml file and a
#' tibble with gcam data formatted for srn charts.
#' @keywords gcam, gcam database, query
#' @import rgcam tibble dplyr
#' @export

srn.readgcam <- function(gcamdatabasePath, gcamdatabaseName, queryxml = "srnQueries.xml",
                         scenOrigNames, scenNewNames = NULL,
                         reReadData = T, dataProj = "dataProj.proj", dirOutputs = paste(getwd(), "/outputs", sep = ""),
                         regionsSelect = NULL, queriesSelect="All",
                         paramsSelect="All"){

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
    class1 -> connx -> aggregate -> Units -> sources -> paramx -> fuel -> technology -> input -> output -> water ->
    landleaf -> ghg -> Convert

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

    if(any(queriesSelect=="All")){queriesx <- queries} else{
      if(!all(queriesSelect %in% queries)){stop("No queries are available in queryxml.
                               Please check your queriesSelect entries or your queryxml")} else {
      if(length(queriesSelect[!(queriesSelect %in% queries)])>0){
        print(paste("Queries not available in queryxml: ", paste(queriesSelect[!(queriesSelect %in% queries)],collapse=", "), sep=""))
        print(paste("Running remaining queriesSelect: ",  paste(queriesSelect[(queriesSelect %in% queries)],collapse=", "), sep=""))}
      queriesx <- queriesSelect}
    }


    if(any(paramsSelect=="All")){
      paramsSelectx=c("finalNrgbySec", "primNrgConsumByFuel", "elecByTech",
                     "watConsumBySec", "watWithdrawBySec", "watWithdrawByCrop", "watBioPhysCons", "irrWatWithBasin","irrWatConsBasin",
                     "gdpPerCapita", "gdp", "gdpGrowthRate", "pop", "agProdbyIrrRfd",
                     "agProdBiomass", "agProdForest", "agProdByCrop", "landIrrRfd", "aggLandAlloc",
                     "LUCemiss", "co2emission", "co2emissionByEndUse", "ghgEmissionByGHG", "ghgEmissByGHGGROUPS")
    }else{paramsSelectx=paramsSelect}


    # Total final energy by aggregate end-use sector
    if("finalNrgbySec" %in% paramsSelectx){
    queryx <- "Total final energy by aggregate end-use sector"
    if (queryx %in% queriesx) {
        tbl <- getQuery(dataProjLoaded, queryx)  # Tibble
        if (!is.null(regionsSelect)) {
            tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
        }
        tbl <- tbl %>%
          left_join(data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
          mutate(param = "finalNrgbySec",
                 sources = "Sources",
                 origScen = scenario,
                 origQuery = queryx,
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
          dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%
          group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                   aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                   origScen, origQuery, origUnits, origX)%>%summarize_at("value",funs(sum))%>%ungroup()%>%
          mutate(origValue = value)%>%filter(!is.na(value))
        datax <- bind_rows(datax, tbl)
    } else {
        print(paste("Query '", queryx, "' not found in database", sep = ""))
    }}else{print(paste("Param '", paramx, "' not available in current queries", sep = ""))}


    # primary energy consumption by region (direct equivalent)
    if("primNrgConsumByFuel" %in% paramsSelectx){
    queryx <- "primary energy consumption by region (direct equivalent)"
    if (queryx %in% queriesx) {
      tbl <- getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        left_join(data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        mutate(param = "primNrgConsumByFuel",
               sources = "Sources",
               origScen = scenario,
               origQuery = queryx,
               origValue = value,
               origUnits = Units,
               origX = year,
               scenario = scenNewNames,
               value = value * srn.assumptions()$convEJ2TWh,
               units = "Primary Energy Consumption (TWh)",
               vintage = paste("Vint_", year, sep = ""),
               x = year,
               xLabel = "Year",
               aggregate = "sum",
               class1 = fuel,
               classLabel1 = "Fuel",
               classPalette1 = "pal_pri_ene",
               class2 = "class2",
               classLabel2 = "classLabel2",
               classPalette2 = "classPalette2")%>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origUnits, origX)%>%
        group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                 aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                 origScen, origQuery, origUnits, origX)%>%summarize_at("value",funs(sum))%>%ungroup()%>%
        mutate(origValue = value)%>%filter(!is.na(value))
      datax <- bind_rows(datax, tbl)
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
    }}else{print(paste("Param '", paramx, "' not available in current queries", sep = ""))}


    if("elecByTech" %in% paramsSelectx){
    # Electricity generation by aggregate technology
    queryx <- "Electricity generation by aggregate technology"
    if (queryx %in% queriesx) {
      tbl <- getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        left_join(data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        mutate(param = "elecByTech",
               sources = "Sources",
               origScen = scenario,
               origQuery = queryx,
               origValue = value,
               origUnits = Units,
               origX = year,
               scenario = scenNewNames,
               value = value * srn.assumptions()$convEJ2TWh,
               units = "Electricity Generation (TWh)",
               vintage = paste("Vint_", year, sep = ""),
               x = year,
               xLabel = "Year",
               aggregate = "sum",
               class1 = technology,
               classLabel1 = "Fuel",
               classPalette1 = "pal_elec_tech_colors",
               class2 = "class2",
               classLabel2 = "classLabel2",
               classPalette2 = "classPalette2")%>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origUnits, origX)%>%
        group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                 aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                 origScen, origQuery, origUnits, origX)%>%summarize_at("value",funs(sum))%>%ungroup()%>%
        mutate(origValue = value)%>%filter(!is.na(value))
      datax <- bind_rows(datax, tbl)
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
    }}else{print(paste("Param '", paramx, "' not available in current queries", sep = ""))}

    if("watConsumBySec" %in% paramsSelectx){
    # water consumption by sector
    queryx <- "water consumption by sector"
    if (queryx %in% queriesx) {
      tbl <- getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        left_join(data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        mutate(param = "watConsumBySec",
               sources = "Sources",
               origScen = scenario,
               origQuery = queryx,
               origValue = value,
               origUnits = Units,
               origX = year,
               scenario = scenNewNames,
               value = value * srn.assumptions()$convEJ2TWh,
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
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origUnits, origX)%>%
        group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                 aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                 origScen, origQuery, origUnits, origX)%>%summarize_at("value",funs(sum))%>%ungroup()%>%
        mutate(origValue = value)%>%filter(!is.na(value))
      datax <- bind_rows(datax, tbl)
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
    }}else{print(paste("Param '", paramx, "' not available in current queries", sep = ""))}

    if("watWithdrawBySec" %in% paramsSelectx){
    # water withdrawals by sector
    queryx <- "water withdrawals by sector"
    if (queryx %in% queriesx) {
      tbl <- getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        left_join(data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        mutate(param = "watWithdrawBySec",
               sources = "Sources",
               origScen = scenario,
               origQuery = queryx,
               origValue = value,
               origUnits = Units,
               origX = year,
               scenario = scenNewNames,
               value = value * srn.assumptions()$convEJ2TWh,
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
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origUnits, origX)%>%
        group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                 aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                 origScen, origQuery, origUnits, origX)%>%summarize_at("value",funs(sum))%>%ungroup()%>%
        mutate(origValue = value)%>%filter(!is.na(value))
      datax <- bind_rows(datax, tbl)
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
    }}else{print(paste("Param '", paramx, "' not available in current queries", sep = ""))}

    if("watWithdrawByCrop" %in% paramsSelectx){
    # water withdrawals by sector
    queryx <- "water withdrawals by crop"
    if (queryx %in% queriesx) {
      tbl <- getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        filter(sector!="industry", sector!="mining" , sector!="municipal"
               , sector!="electricity" , sector!="livestock")%>%
        left_join(data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        mutate(param = "watWithdrawByCrop",
               sources = "Sources",
               origScen = scenario,
               origQuery = queryx,
               origValue = value,
               origUnits = Units,
               origX = year,
               scenario = scenNewNames,
               value = value * srn.assumptions()$convEJ2TWh,
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
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origUnits, origX)%>%
        group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                 aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                 origScen, origQuery, origUnits, origX)%>%summarize_at("value",funs(sum))%>%ungroup()%>%
        mutate(origValue = value)%>%filter(!is.na(value))
      datax <- bind_rows(datax, tbl)
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
    }}else{print(paste("Param '", paramx, "' not available in current queries", sep = ""))}

    if("watBioPhysCons" %in% paramsSelectx){
    # biophysical water demand by crop type and land region
    queryx <- "biophysical water demand by crop type and land region"
    if (queryx %in% queriesx) {
      tbl <- getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        left_join(data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        mutate(param = "watBioPhysCons",
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
        group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                 aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                 origScen, origQuery, origUnits, origX)%>%summarize_at("value",funs(sum))%>%ungroup()%>%
        mutate(origValue = value)%>%filter(!is.na(value))
      datax <- bind_rows(datax, tbl)
      tblgdp<-tbl
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
    }}else{print(paste("Param '", paramx, "' not available in current queries", sep = ""))}

    if("irrWatWithBasin" %in% paramsSelectx){
    # water withdrawals by water mapping source
    queryx <- "water withdrawals by water mapping source"
    if (queryx %in% queriesx) {
      tbl <- getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        filter(grepl("_irr_",input))%>%
        mutate(input=gsub("water_td_irr_","",input),
               input=gsub("_W","",input))%>%
        left_join(data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        mutate(param = "irrWatWithBasin",
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
        group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                 aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                 origScen, origQuery, origUnits, origX)%>%summarize_at("value",funs(sum))%>%ungroup()%>%
        mutate(origValue = value)%>%filter(!is.na(value))
      datax <- bind_rows(datax, tbl)
      tblgdp<-tbl
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
    }}else{print(paste("Param '", paramx, "' not available in current queries", sep = ""))}


    if("irrWatConsBasin" %in% paramsSelectx){
    # water consumption by water mapping source
    queryx <- "water consumption by water mapping source"
    if (queryx %in% queriesx) {
      tbl <- getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        filter(grepl("_irr_",input))%>%
        mutate(input=gsub("water_td_irr_","",input),
               input=gsub("_C","",input))%>%
        left_join(data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        mutate(param = "irrWatConsBasin",
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
        group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                 aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                 origScen, origQuery, origUnits, origX)%>%summarize_at("value",funs(sum))%>%ungroup()%>%
        mutate(origValue = value)%>%filter(!is.na(value))
      datax <- bind_rows(datax, tbl)
      tblgdp<-tbl
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
    }}else{print(paste("Param '", paramx, "' not available in current queries", sep = ""))}

    if("gdpPerCapita" %in% paramsSelectx){
    # GDP MER per Capita MER by region
    queryx <- "GDP per capita MER by region"
    if (queryx %in% queriesx) {
      tbl <- getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        left_join(data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        mutate(param = "gdpPerCapita",
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
        group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                 aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                 origScen, origQuery, origUnits, origX)%>%summarize_at("value",funs(sum))%>%ungroup()%>%
        mutate(origValue = value)%>%filter(!is.na(value))
      datax <- bind_rows(datax, tbl)
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
    }}else{print(paste("Param '", paramx, "' not available in current queries", sep = ""))}

    if("gdp" %in% paramsSelectx){
    # GDP MER by region
    queryx <- "GDP MER by region"
    if (queryx %in% queriesx) {
      tbl <- getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        left_join(data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        mutate(param = "gdp",
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
        group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                 aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                 origScen, origQuery, origUnits, origX)%>%summarize_at("value",funs(sum))%>%ungroup()%>%
        mutate(origValue = value)%>%filter(!is.na(value))
      datax <- bind_rows(datax, tbl)
      tblgdp<-tbl
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
    }}else{print(paste("Param '", paramx, "' not available in current queries", sep = ""))}

    if("gdpGrowthRate" %in% paramsSelectx){
    # GDP Growth Rate by region
    queryx <- "GDP Growth Rate (Percent)"
    if ("GDP MER by region" %in% queriesx) {
      tbl <- tblgdp  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        mutate(param = "gdpGrowthRate",
               sources = "Sources",
               value = (value-lag(value,order_by=year))*100/lag(value,order_by=x),
               units = "GDP Growth Rate (Percent)",
               vintage = paste("Vint_", year, sep = ""),
               classLabel1 = "GDP growth rate",
               origQuery = "Calculated",
               origValue = value,
               origUnits = units,
               origX = x) %>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                 aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                 origScen, origQuery, origUnits, origX)%>%summarize_at("value",funs(sum))%>%ungroup()%>%
        mutate(origValue = value)%>%filter(!is.na(value))
      datax <- bind_rows(datax, tbl)
    } else {
      print(paste("Paramater 'GDP MER by region' not found in database, so
                  cannot calculate" ,queryx, sep = ""))
    }}else{print(paste("Param '", paramx, "' not available in current queries", sep = ""))}

    if("pop" %in% paramsSelectx){
    # Population
    queryx <- "Population by region"
    if (queryx %in% queriesx) {
      tbl <- getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        left_join(data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        mutate(param = "pop",
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
        group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                 aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                 origScen, origQuery, origUnits, origX)%>%summarize_at("value",funs(sum))%>%ungroup()%>%
        mutate(origValue = value)%>%filter(!is.na(value))
      datax <- bind_rows(datax, tbl)
      tblgdp<-tbl
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
    }}else{print(paste("Param '", paramx, "' not available in current queries", sep = ""))}

    if("agProdbyIrrRfd" %in% paramsSelectx){
    # Ag production by tech
    queryx <- "ag production by tech"
    if (queryx %in% queriesx) {
      tbl <- getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        filter(Units=="Mt")%>%
        left_join(data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        mutate(param = "agProdbyIrrRfd",
               sources = "Sources",
               origScen = scenario,
               origQuery = queryx,
               origUnits = Units,
               origX = year,
               scenario = scenNewNames,
               value = value,
               units = "Ag Production (Mt)",
               vintage = paste("Vint_", year, sep = ""),
               x = year,
               xLabel = "Year",
               aggregate = "sum",
               class1 = case_when(grepl("IRR",technology)~"irrigation",
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
        group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                 aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                 origScen, origQuery, origUnits, origX)%>%summarize_at("value",funs(sum))%>%ungroup()%>%
        mutate(origValue = value)%>%filter(!is.na(value))
      datax <- bind_rows(datax, tbl)
      tblgdp<-tbl
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
    }}else{print(paste("Param '", paramx, "' not available in current queries", sep = ""))}


    if("agProdBiomass" %in% paramsSelectx){
    # Ag Production by Crop Type Biomass EJ
    queryx <- "Ag Production by Crop Type"
    if (queryx %in% queriesx) {
      tbl <- getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        filter(Units=="EJ",sector==output)%>%
        left_join(data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        mutate(param = "agProdBiomass",
               sources = "Sources",
               origScen = scenario,
               origQuery = queryx,
               origValue = value,
               origUnits = Units,
               origX = year,
               scenario = scenNewNames,
               value = value * srn.assumptions()$convEJ2TWh,
               units = "Biomass Production (EJ)",
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
                      class2, classLabel2, classPalette2)%>%filter(!is.na(value))
      datax <- bind_rows(datax, tbl)
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
    }}else{print(paste("Param '", paramx, "' not available in current queries", sep = ""))}


    if("agProdForest" %in% paramsSelectx){
    # Ag Production by Crop Type Forest
    queryx <- "Ag Production by Crop Type"
    if (queryx %in% queriesx) {
      tbl <- getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        filter(Units=="billion m3",sector==output)%>%
        left_join(data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        mutate(param = "agProdForest",
               sources = "Sources",
               origScen = scenario,
               origQuery = queryx,
               origValue = value,
               origUnits = Units,
               origX = year,
               scenario = scenNewNames,
               value = value * srn.assumptions()$convEJ2TWh,
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
                      class2, classLabel2, classPalette2)%>%filter(!is.na(value))
      datax <- bind_rows(datax, tbl)
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
    }}else{print(paste("Param '", paramx, "' not available in current queries", sep = ""))}



    if("agProdByCrop" %in% paramsSelectx){
    # Ag Production by Crop Type
    queryx <- "Ag Production by Crop Type"
    if (queryx %in% queriesx) {
      tbl <- getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        filter(Units=="Mt",sector==output, sector!="Pasture")%>%
        left_join(data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        mutate(param = "agProdByCrop",
               sources = "Sources",
               origScen = scenario,
               origQuery = queryx,
               origValue = value,
               origUnits = Units,
               origX = year,
               scenario = scenNewNames,
               value = value * srn.assumptions()$convEJ2TWh,
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
                      class2, classLabel2, classPalette2)%>%filter(!is.na(value))
      datax <- bind_rows(datax, tbl)
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
    }}else{print(paste("Param '", paramx, "' not available in current queries", sep = ""))}


    if("landIrrRfd" %in% paramsSelectx){
    # land allocation by crop and water source
    queryx <- "land allocation by crop and water source"
    if (queryx %in% queriesx) {
      tbl <- getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::filter(!is.na(water))%>%
        left_join(data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        mutate(param = "landIrrRfd",
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
        group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                 aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                 origScen, origQuery, origUnits, origX)%>%summarize_at("value",funs(sum))%>%ungroup()%>%
        mutate(origValue = value)%>%filter(!is.na(value))
      datax <- bind_rows(datax, tbl)
      tblgdp<-tbl
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
    }}else{print(paste("Param '", paramx, "' not available in current queries", sep = ""))}


    if("aggLandAlloc" %in% paramsSelectx){
    # aggregated land allocation
    queryx <- "aggregated land allocation"
    if (queryx %in% queriesx) {
      tbl <- getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        mutate(landleaf=gsub("forest\\s\\(managed\\)","forest",landleaf),
               landleaf=gsub("forest\\s\\(unmanaged\\)","forest",landleaf),
               landleaf=gsub("pasture\\s\\(grazed\\)","pasture",landleaf),
               landleaf=gsub("pasture\\s\\(other\\)","pasture",landleaf))%>%
        left_join(data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        mutate(param = "aggLandAlloc",
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
               class2 = "class2",
               classLabel2 = "classLabel2",
               classPalette2 = "classPalette2") %>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                 aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                 origScen, origQuery, origUnits, origX)%>%summarize_at("value",funs(sum))%>%ungroup()%>%
        mutate(origValue = value)%>%filter(!is.na(value))
      datax <- bind_rows(datax, tbl)
      tblgdp<-tbl
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
    }}else{print(paste("Param '", paramx, "' not available in current queries", sep = ""))}


    if("LUCemiss" %in% paramsSelectx){
    # Land Use Change Emission (future)
    queryx <- "Land Use Change Emission (future)"
    if (queryx %in% queriesx) {
      tbl <- getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        left_join(data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        mutate(param = "LUCemiss",
               sources = "Sources",
               origScen = scenario,
               origQuery = queryx,
               origUnits = Units,
               origX = year,
               scenario = scenNewNames,
               value = value*(srn.assumptions()$GWP%>%filter(ghg=="CO2")%>%dplyr::select(srn.assumptions()$GWPType))[1,1],
               origValue = value,
               units = "LUC CO2 Emissions (MTCO2 Eq.)",
               vintage = paste("Vint_", year, sep = ""),
               x = year,
               xLabel = "Year",
               aggregate = "sum",
               class1 = "class1",
               classLabel1 = "Land Type",
               classPalette1 = "pal_16",
               class2 = "class2",
               classLabel2 = "classLabel2",
               classPalette2 = "classPalette2") %>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                 aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                 origScen, origQuery, origUnits, origX)%>%summarize_at("value",funs(sum))%>%ungroup()%>%
        mutate(origValue = value)%>%filter(!is.na(value))
      tblLUEmiss<-tbl
      datax <- bind_rows(datax, tbl)
      tblgdp<-tbl
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
    }}else{print(paste("Param '", paramx, "' not available in current queries", sep = ""))}


    if(any(grepl("\\bco2emission\\b",paramsSelectx))){
    # CO2 Emissions
    queryx <- "CO2 Emissions"
    if (queryx %in% queriesx) {
      tbl <- getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        left_join(data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        mutate(param = "co2emission",
               sources = "Sources",
               origScen = scenario,
               origQuery = queryx,
               origUnits = Units,
               origX = year,
               scenario = scenNewNames,
               value = value*(srn.assumptions()$GWP%>%filter(ghg=="CO2")%>%dplyr::select(srn.assumptions()$GWPType))[1,1],
               origValue = value,
               units = "CO2 Emissions by Sector (MTCO2 Eq.)",
               vintage = paste("Vint_", year, sep = ""),
               x = year,
               xLabel = "Year",
               aggregate = "sum",
               class1 = sector,
               classLabel1 = "Type",
               classPalette1 = "pal_16",
               class2 = "class2",
               classLabel2 = "classLabel2",
               classPalette2 = "classPalette2") %>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                 aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                 origScen, origQuery, origUnits, origX)%>%summarize_at("value",funs(sum))%>%ungroup()%>%
        mutate(origValue = value)%>%filter(!is.na(value))
      datax <- bind_rows(datax, tbl)
      tblgdp<-tbl
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
    }}else{print(paste("Param '", paramx, "' not available in current queries", sep = ""))}


    if("co2emissionByEndUse" %in% paramsSelectx){
    # CO2 Emissions by enduse
    queryx <- "CO2 Emissions by enduse"
    if (queryx %in% queriesx) {
      tbl <- getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        left_join(data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        mutate(param = "co2emissionByEndUse",
               sources = "Sources",
               origScen = scenario,
               origQuery = queryx,
               origUnits = Units,
               origX = year,
               scenario = scenNewNames,
               value = value*(srn.assumptions()$GWP%>%filter(ghg=="CO2")%>%dplyr::select(srn.assumptions()$GWPType))[1,1],
               origValue = value,
               units = "CO2 Emission by Enduse (MTCO2 Eq.)",
               vintage = paste("Vint_", year, sep = ""),
               x = year,
               xLabel = "Year",
               aggregate = "sum",
               class1 = sector,
               classLabel1 = "Type",
               classPalette1 = "pal_finalNrg_sec",
               class2 = "class2",
               classLabel2 = "classLabel2",
               classPalette2 = "classPalette2") %>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)
      dfLUCAbs<-tblLUEmiss%>%filter(value<0)%>%mutate(class1="LUC_Absorption")
      dfLUCAbs<-dfLUCAbs%>%group_by_at(vars(-value,-origValue)) %>% summarize(value = sum(value,na.rm=T))%>%
        ungroup()%>%mutate(origValue=value,class1="LUC Absorption")
      dfLUCEmit<-tblLUEmiss%>%filter(value>0)%>%mutate(class1="LUC_Absorption")
      dfLUCEmit<-dfLUCEmit%>%group_by_at(vars(-value,-origValue)) %>% summarize(value = sum(value,na.rm=T))%>%
        ungroup()%>%mutate(origValue=value,class1="LUC Emission")
      dfLUC<-bind_rows(dfLUCAbs,dfLUCEmit);
      dfLUC<-dfLUC%>%mutate(param=unique(tbl$param),
                          classLabel1=unique(tbl$classLabel1),
                          classPalette1=unique(tbl$classPalette1))
      tbl<-bind_rows(tbl,dfLUC)%>%filter(!is.na(value))%>%mutate(units = "CO2 Emission by Enduse (MTCO2 Eq.)")
      datax <- bind_rows(datax, tbl)
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
    }}else{print(paste("Param '", paramx, "' not available in current queries", sep = ""))}


    if("ghgEmissionByGHG" %in% paramsSelectx){
    # GHG emissions by subsector
    queryx <- "GHG emissions by subsector"
    if (queryx %in% queriesx) {
      tbl <- getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        left_join(srn.assumptions()$GWP%>%dplyr::select(ghg,srn.assumptions()$GWPType),by="ghg")%>%
        left_join(srn.assumptions()$convertGgTgMTC,by="Units") %>%
        left_join(data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        mutate(param = "ghgEmissionByGHG",
               sources = "Sources",
               origScen = scenario,
               origQuery = queryx,
               origUnits = Units,
               origX = year,
               scenario = scenNewNames,
               value=value*get(srn.assumptions()$GWPType)*Convert,
               origValue = value,
               units = "GHG Emissions (MTCO2 Eq.)",
               vintage = paste("Vint_", year, sep = ""),
               x = year,
               xLabel = "Year",
               aggregate = "sum",
               class1 = ghg,
               classLabel1 = "GHG",
               classPalette1 = "pal_16",
               class2 = "class2",
               classLabel2 = "classLabel2",
               classPalette2 = "classPalette2") %>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                 aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                 origScen, origQuery, origUnits, origX)%>%summarize_at("value",funs(sum))%>%ungroup()%>%
        mutate(origValue = value)%>%filter(!is.na(value))
      datax <- bind_rows(datax, tbl)
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
    }}else{print(paste("Param '", paramx, "' not available in current queries", sep = ""))}


    if("ghgEmissByGHGGROUPS" %in% paramsSelectx){
    # GHG emissions by subsector
    queryx <- "GHG emissions by subsector"
    if (queryx %in% queriesx) {
      tbl <- getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        left_join(srn.assumptions()$GWP%>%dplyr::select(ghg,srn.assumptions()$GWPType),by="ghg")%>%
        left_join(srn.assumptions()$convertGgTgMTC,by="Units") %>%
        left_join(data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        mutate(param = "ghgEmissByGHGGROUPS",
               sources = "Sources",
               origScen = scenario,
               origQuery = "X",
               origUnits = Units,
               origX = year,
               scenario = scenNewNames,
               value=value*get(srn.assumptions()$GWPType)*Convert,
               origValue = value,
               units = "GHG Emissions by Group (MTCO2 Eq.)",
               vintage = paste("Vint_", year, sep = ""),
               x = year,
               xLabel = "Year",
               aggregate = "sum",
               class1 = ghg,
               classLabel1 = "GHG",
               classPalette1 = "pal_16",
               class2 = "class2",
               classLabel2 = "classLabel2",
               classPalette2 = "classPalette2") %>%
        mutate(class1 = case_when ((grepl("HFC", class1)) ~ "HFCs",
                                   (grepl("SF6", class1)) ~ "SF6",
                                   (grepl("CO2", class1)) ~ "CO2",
                                   (grepl("N2O", class1)) ~ "N2O",
                                   (grepl("CH4", class1)) ~ "CH4",
                                   TRUE ~ "Other"))%>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                 aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                 origScen, origQuery, origUnits, origX)%>%summarize_at("value",funs(sum))%>%ungroup()%>%
        mutate(origValue = value)%>%filter(!is.na(value))
      datax <- bind_rows(datax, tbl)
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
    }}else{print(paste("Param '", paramx, "' not available in current queries", sep = ""))}


   datax<-datax%>%unique()

   # srn.chart(tbl,xData="x",yData="value",useNewLabels = 0)

    #---------------------
    # Create Data Template
    #---------------------

    dataTemplate <- datax %>%
      mutate(scenario = "Local Data", value = 0, sources="Sources", x=2010, vintage="vintage if available") %>%
      dplyr::select(scenario, region, sources, param, units, class1, class2, x, value, vintage) %>%
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
