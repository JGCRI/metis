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
#' tibble with gcam data formatted for metis charts.
#' @keywords gcam, gcam database, query
#' @export

metis.readgcam <- function(gcamdatabasePath,
                           gcamdatabaseName,
                           queryxml = "metisQueries.xml",
                           queryPath = gcamdatabasePath,
                           scenOrigNames,
                           scenNewNames = NULL,
                           reReadData = T,
                           dataProj = "dataProj.proj",
                           dataProjPath = gcamdatabasePath,
                           dirOutputs = paste(getwd(), "/outputs", sep = ""),
                           regionsSelect = NULL, queriesSelect="All",
                           paramsSelect="All"
                           ){


#----------------
# Initialize variables by setting to NULL
#----------------

    NULL -> vintage -> year -> xLabel -> x -> value -> sector -> scenario -> region -> param -> origX -> origValue ->
    origUnits -> origScen -> origQuery -> classPalette2 -> classPalette1 -> classLabel2 -> classLabel1 -> class2 ->
    class1 -> connx -> aggregate -> Units -> sources -> paramx -> fuel -> technology -> input -> output -> water ->
    landleaf -> ghg -> Convert -> regionsSelectAll->cf1971to2100->gcamCapacityFactor

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
        if (file.exists(paste(dataProjPath, "/", dataProj, sep = ""))){
                file.remove(paste(dataProjPath, "/", dataProj, sep = ""))}  # Delete old project file

        # Check for query file and folder if incorrect give error
        if(!file.exists(paste(queryPath, "/", queryxml, sep = ""))){stop(paste("query file: ", queryPath,"/",queryxml," is incorrect or doesn't exist.",sep=""))}

        # Check for gcamdatbasePath and gcamdatabasename
        if(!file.exists(paste(gcamdatabasePath, "/", gcamdatabaseName, sep = ""))){stop(paste("GCAM database: ", gcamdatabasePath,"/",gcamdatabaseName," is incorrect or doesn't exist.",sep=""))}

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
                     "LUCemiss", "co2emission", "co2emissionByEndUse", "ghgEmissionByGHG", "ghgEmissByGHGGROUPS",
                     "finalNrgbySecDet","finalElecbySecDet","finalElecbyServiceDet")
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
                 value = value * metis.assumptions()$convEJ2TWh,
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
          dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origValue, origUnits, origX)%>%
          dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                   aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                   origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(vars("value","origValue"),dplyr::funs(sum))%>%
          dplyr::ungroup()%>%
          dplyr::filter(!is.na(value))
        datax <- dplyr::bind_rows(datax, tbl)
    } else {
        print(paste("Query '", queryx, "' not found in database", sep = ""))
    }}


     paramx<-"finalNrgbySecDet"
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
           dplyr::mutate(param = "finalNrgbySecDet",
                         sources = "Sources",
                         origScen = scenario,
                         origQuery = queryx,
                         origValue = value,
                         origUnits = Units,
                         origX = year,
                         scenario = scenNewNames,
                         value = value * metis.assumptions()$convEJ2TWh,
                         units = "Final Energy (TWh)",
                         vintage = paste("Vint_", year, sep = ""),
                         x = year,
                         xLabel = "Year",
                         aggregate = "sum",
                         class1 = sector,
                         classLabel1 = "Sector",
                         classPalette1 = "pal_16",
                         class2 = input,
                         classLabel2 = "classLabel2",
                         classPalette2 = "classPalette2")%>%
           dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                         aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                         origScen, origQuery, origValue, origUnits, origX)%>%
           dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                           aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                           origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(vars("value","origValue"),dplyr::funs(sum))%>%dplyr::ungroup()%>%
           dplyr::filter(!is.na(value))
         datax <- dplyr::bind_rows(datax, tbl)
       } else {
         print(paste("Query '", queryx, "' not found in database", sep = ""))
       }}

     paramx<-"finalElecbySecDet"
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
           dplyr::mutate(input=gsub("elect\\_td\\_ind","industry",input),
                         input=gsub("elect\\_td\\_bld","building",input),
                         input=gsub("elect\\_td\\_trn","transport",input))%>%
           dplyr::left_join(dplyr::data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
           dplyr::mutate(param = "finalElecbySecDet",
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
                         classLabel1 = "Sector",
                         classPalette1 = "pal_finalNrg_sec",
                         class2 = sector,
                         classLabel2 = "classLabel2",
                         classPalette2 = "classPalette2")%>%
           dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                         aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                         origScen, origQuery, origValue, origUnits, origX)%>%
           dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                           aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                           origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(vars("value","origValue"),dplyr::funs(sum))%>%dplyr::ungroup()%>%
           dplyr::filter(!is.na(value))
         datax <- dplyr::bind_rows(datax, tbl)
       } else {
         print(paste("Query '", queryx, "' not found in database", sep = ""))
       }}


     paramx<-"finalElecbyServiceDet"
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
                         sector=gsub("industrial energy use","industry",sector),
                         sector=gsub("trn_pass","transport",sector),
                         sector=gsub("trn_pass","industry",sector),
                         sector=gsub("trn_pass_road_LDV_2W","industry",sector),
                         sector=gsub("trn_pass_road_LDV_4W","industry",sector))%>%
           dplyr::mutate(input=gsub("elect\\_td\\_ind","industry",input),
                         input=gsub("elect\\_td\\_bld","building",input),
                         input=gsub("elect\\_td\\_trn","transport",input))%>%
           dplyr::left_join(dplyr::data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
           dplyr::mutate(param = "finalElecbyServiceDet",
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
                         classPalette1 = "pal_16",
                         class2 = input,
                         classLabel2 = "classLabel2",
                         classPalette2 = "classPalette2")%>%
           dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                         aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                         origScen, origQuery, origValue, origUnits, origX)%>%
           dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                           aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                           origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(vars("value","origValue"),dplyr::funs(sum))%>%dplyr::ungroup()%>%
           dplyr::filter(!is.na(value))
         datax <- dplyr::bind_rows(datax, tbl)
       } else {
         print(paste("Query '", queryx, "' not found in database", sep = ""))
       }}

    paramx<-"primNrgConsumByFuel"
    # primary energy consumption by region (direct equivalent)
    if(paramx %in% paramsSelectx){
    queryx <- "primary energy consumption by region (direct equivalent)"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::left_join(dplyr::data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "primNrgConsumByFuel",
               sources = "Sources",
               origScen = scenario,
               origQuery = queryx,
               origValue = value,
               origUnits = Units,
               origX = year,
               scenario = scenNewNames,
               value = value * metis.assumptions()$convEJ2TWh,
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
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                 aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                 origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(vars("value","origValue"),dplyr::funs(sum))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
    }}


    paramx <- "elecByTech"
    if(paramx %in% paramsSelectx){
    # Electricity generation by aggregate technology
    queryx <- "Electricity generation by aggregate technology"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::left_join(dplyr::data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "elecByTech",
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
               classPalette1 = "pal_elec_tech_colors",
               class2 = "class2",
               classLabel2 = "classLabel2",
               classPalette2 = "classPalette2")%>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources,class1,class2, x, xLabel, vintage, units,
                 aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                 origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(vars("value","origValue"),dplyr::funs(sum))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
      tblElecbyTech<-tbl
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
    }}

    if(file.exists(paste(getwd(),"/dataFiles/gcam/capacity_factor_by_elec_gen_subsector.csv",sep=""))){
      capfactors <- data.table::fread(file=paste(getwd(),"/dataFiles/gcam/capacity_factor_by_elec_gen_subsector.csv",sep=""),skip=3)
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
               value = value * metis.assumptions()$convEJ2TWh,
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
                 origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(vars("value","origValue"),dplyr::funs(sum))%>%dplyr::ungroup()%>%
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
               value = value * metis.assumptions()$convEJ2TWh,
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
                 origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(vars("value","origValue"),dplyr::funs(sum))%>%dplyr::ungroup()%>%
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
               value = value * metis.assumptions()$convEJ2TWh,
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
                 origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(vars("value","origValue"),dplyr::funs(sum))%>%dplyr::ungroup()%>%
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
                 origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(vars("value","origValue"),dplyr::funs(sum))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
      tblgdp<-tbl
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
                 origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(vars("value","origValue"),dplyr::funs(sum))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
      tblgdp<-tbl
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
                 origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(vars("value","origValue"),dplyr::funs(sum))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
      tblgdp<-tbl
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
                 origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(vars("value","origValue"),dplyr::funs(sum))%>%dplyr::ungroup()%>%
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
                 origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(vars("value","origValue"),dplyr::funs(sum))%>%dplyr::ungroup()%>%
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
        dplyr::mutate(param = "gdpGrowthRate",
               sources = "Sources",
               value = (value-dplyr::lag(value,order_by=x))*100/(5*dplyr::lag(value,order_by=x)),
               units = "GDP Growth Rate (Percent)",
               vintage = paste("Vint_", x, sep = ""),
               classLabel1 = "GDP growth rate",
               origQuery = "Calculated",
               origValue = value,
               origUnits = units,
               origX = x) %>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                 aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                 origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(vars("value","origValue"),dplyr::funs(sum))%>%dplyr::ungroup()%>%
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
                 origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(vars("value","origValue"),dplyr::funs(sum))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
      tblgdp<-tbl
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
                 origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(vars("value","origValue"),dplyr::funs(sum))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
      tblgdp<-tbl
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
               value = value * metis.assumptions()$convEJ2TWh,
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
               value = value * metis.assumptions()$convEJ2TWh,
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
               value = value * metis.assumptions()$convEJ2TWh,
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
                 origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(vars("value","origValue"),dplyr::funs(sum))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
      tblgdp<-tbl
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
                 origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(vars("value","origValue"),dplyr::funs(sum))%>%dplyr::ungroup()%>%
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
        dplyr::left_join(dplyr::data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "LUCemiss",
               sources = "Sources",
               origScen = scenario,
               origQuery = queryx,
               origUnits = Units,
               origX = year,
               scenario = scenNewNames,
               value = value*(metis.assumptions()$GWP%>%dplyr::filter(ghg=="CO2")%>%dplyr::select(metis.assumptions()$GWPType))[1,1],
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
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                 aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                 origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(vars("value","origValue"),dplyr::funs(sum))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      tblLUEmiss<-tbl
      datax <- dplyr::bind_rows(datax, tbl)
      tblgdp<-tbl
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
    }}

    paramx <- "co2emission"
    if(any(grepl("\\bco2emission\\b",paramsSelectx))){
    # CO2 Emissions
    queryx <- "CO2 Emissions"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::left_join(dplyr::data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "co2emission",
               sources = "Sources",
               origScen = scenario,
               origQuery = queryx,
               origUnits = Units,
               origX = year,
               scenario = scenNewNames,
               value = value*(metis.assumptions()$GWP%>%dplyr::filter(ghg=="CO2")%>%dplyr::select(metis.assumptions()$GWPType))[1,1],
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
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                 aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                 origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(vars("value","origValue"),dplyr::funs(sum))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
      tblgdp<-tbl
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
    }}

    paramx <- "co2emissionByEndUse"
    if(paramx %in% paramsSelectx){
    # CO2 Emissions by enduse
    queryx <- "CO2 Emissions by enduse"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::left_join(dplyr::data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "co2emissionByEndUse",
               sources = "Sources",
               origScen = scenario,
               origQuery = queryx,
               origUnits = Units,
               origX = year,
               scenario = scenNewNames,
               value = value*(metis.assumptions()$GWP%>%dplyr::filter(ghg=="CO2")%>%dplyr::select(metis.assumptions()$GWPType))[1,1],
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
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                 aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                 origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(vars("value","origValue"),dplyr::funs(sum))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))

       # Add LU Change Emissions
       tblLUEmiss <- rgcam::getQuery(dataProjLoaded, "Land Use Change Emission (future)")  # Tibble
        if (!is.null(regionsSelect)) {
          tblLUEmiss <- tblLUEmiss %>% dplyr::filter(region %in% regionsSelect)
        }
       tblLUEmiss <- tblLUEmiss %>%
          dplyr::left_join(dplyr::data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
          dplyr::mutate(param = "LUCemiss",
                 sources = "Sources",
                 origScen = scenario,
                 origQuery = queryx,
                 origUnits = Units,
                 origX = year,
                 scenario = scenNewNames,
                 value = value*(metis.assumptions()$GWP%>%dplyr::filter(ghg=="CO2")%>%dplyr::select(metis.assumptions()$GWPType))[1,1],
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
          dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                   aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                   origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(vars("value","origValue"),dplyr::funs(sum))%>%dplyr::ungroup()%>%
          dplyr::filter(!is.na(value))


      dfLUCAbs<-tblLUEmiss%>%dplyr::filter(value<0)%>%dplyr::mutate(class1="LUC_Absorption")
      dfLUCAbs<-dfLUCAbs%>%dplyr::group_by_at(dplyr::vars(-value,-origValue)) %>% dplyr::summarize(value = sum(value,na.rm=T))%>%
        dplyr::ungroup()%>%dplyr::mutate(origValue=value,class1="LUC Absorption")
      dfLUCEmit<-tblLUEmiss%>%dplyr::filter(value>0)%>%dplyr::mutate(class1="LUC_Absorption")
      dfLUCEmit<-dfLUCEmit%>%dplyr::group_by_at(dplyr::vars(-value,-origValue)) %>% dplyr::summarize(value = sum(value,na.rm=T))%>%
        dplyr::ungroup()%>%dplyr::mutate(origValue=value,class1="LUC Emission")
      dfLUC<-dplyr::bind_rows(dfLUCAbs,dfLUCEmit);
      dfLUC<-dfLUC%>%dplyr::mutate(param=unique(tbl$param),
                          classLabel1=unique(tbl$classLabel1),
                          classPalette1=unique(tbl$classPalette1))

      tbl<-dplyr::bind_rows(tbl,dfLUC)%>%dplyr::filter(!is.na(value))%>%dplyr::mutate(units = "CO2 Emission by Enduse (MTCO2 Eq.)")
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
    }}


    paramx<-"ghgEmissionByGHG"
    if(paramx %in% paramsSelectx){
    # GHG emissions by subsector
    queryx <- "GHG emissions by subsector"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::left_join(metis.assumptions()$GWP%>%dplyr::select(ghg,metis.assumptions()$GWPType),by="ghg")%>%
        dplyr::left_join(metis.assumptions()$convertGgTgMTC,by="Units") %>%
        dplyr::left_join(dplyr::data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "ghgEmissionByGHG",
               sources = "Sources",
               origScen = scenario,
               origQuery = queryx,
               origUnits = Units,
               origX = year,
               scenario = scenNewNames,
               value=value*get(metis.assumptions()$GWPType)*Convert,
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
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                 aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                 origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(vars("value","origValue"),dplyr::funs(sum))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      print(paste("Query '", queryx, "' not found in database", sep = ""))
    }}

    paramx <- "ghgEmissByGHGGROUPS"
    if(paramx %in% paramsSelectx){
    # GHG emissions by subsector
    queryx <- "GHG emissions by subsector"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::left_join(metis.assumptions()$GWP%>%dplyr::select(ghg,metis.assumptions()$GWPType),by="ghg")%>%
        dplyr::left_join(metis.assumptions()$convertGgTgMTC,by="Units") %>%
        dplyr::left_join(dplyr::data_frame(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "ghgEmissByGHGGROUPS",
               sources = "Sources",
               origScen = scenario,
               origQuery = "X",
               origUnits = Units,
               origX = year,
               scenario = scenNewNames,
               value=value*get(metis.assumptions()$GWPType)*Convert,
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
        dplyr::mutate(class1 = dplyr::case_when ((grepl("HFC", class1)) ~ "HFCs",
                                   (grepl("SF6", class1)) ~ "SF6",
                                   (grepl("CO2", class1)) ~ "CO2",
                                   (grepl("N2O", class1)) ~ "N2O",
                                   (grepl("CH4", class1)) ~ "CH4",
                                   (grepl("SO2", class1)) ~ "SO2",
                                   (grepl("NH3", class1)) ~ "NH3",
                                   TRUE ~ "Other"))%>%
        dplyr::select(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, param, sources, class1, class2, x, xLabel, vintage, units,
                 aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                 origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(vars("value","origValue"),dplyr::funs(sum))%>%dplyr::ungroup()%>%
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
      dplyr::rename(class=class1)%>%
      dplyr::select(scenario, region, sources, param, units, class, x, value) %>%
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

    if (is.null(regionsSelect) | regionsSelectAll==T) {
        utils::write.csv(datax, file = paste(dirOutputs, "/readGCAMTables/Tables_gcam/gcamDataTable_AllRegions_", min(range(datax$x)),
            "to", max(range(datax$x)), ".csv", sep = ""), row.names = F)
        print(paste("GCAM data table saved to: ", paste(dirOutputs, "/readGCAMTables/Tables_gcam/gcamDataTable_AllRegions_", min(range(datax$x)),
                                                      "to", max(range(datax$x)), ".csv", sep = "")))

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
                             file = paste(dirOutputs, "/readGCAMTables/Tables_gcam/gcamDataTable_",region_i,"_", min(range(datax$x)),
                                          "to", max(range(datax$x)), ".csv", sep = ""),row.names = F)

          # Aggregate across classes
          dataxAggsums<-datax%>%
            dplyr::filter(aggregate=="sum")%>%
            dplyr::select(-tidyselect::contains("class"))%>%
            dplyr::group_by_at(dplyr::vars(-value,-origValue))%>%
            dplyr::summarize_at(c("value"),dplyr::funs(sum))
          dataxAggmeans<-datax%>%
            dplyr::filter(aggregate=="mean")%>%
            dplyr::select(-tidyselect::contains("class"))%>%
            dplyr::group_by_at(dplyr::vars(-value,-origValue))%>%
            dplyr::summarize_at(c("value"),dplyr::funs(mean))
          dataxAgg<-dplyr::bind_rows(dataxAggsums,dataxAggmeans)%>%dplyr::ungroup()

          utils::write.csv(dataxAgg,
                           file = paste(dirOutputs, "/readGCAMTables/Tables_gcam/gcamDataTable_aggClass_",region_i,"_", min(range(datax$x)),
                                        "to", max(range(datax$x)), ".csv", sep = ""),row.names = F)

            utils::write.csv(dataTemplate %>% dplyr::filter(region == region_i),
                             file = paste(dirOutputs, "/readGCAMTables/Tables_Templates/template_Regional_",region_i,".csv", sep = ""),row.names = F)
            #utils::write.csv(dataTemplate %>% dplyr::filter(region == region_i),
            #                 file = paste(dirOutputs, "/Tables/Tables_Local/local_Regional_",region_i,".csv", sep = ""),row.names = F)

            print(paste("Table saved.", sep = ""))
        }
    }

    return(list(data = datax, dataTemplate = dataTemplate, scenarios = scenarios, queries = queries))

}
