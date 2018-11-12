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
#' @param regions The regions to analyze in a vector. Example c('Colombia','Pakistan')
#' @return A list with the scenarios in the gcam database, queries in the queryxml file and a
#' tibble with gcam data formatted for srn charts.
#' @keywords gcam, gcam database, query
#' @import rgcam tibble dplyr
#' @export

srn.readgcam <- function(gcamdatabasePath, gcamdatabaseName, queryxml = "srnQueries.xml",
                         scenOrigNames, scenNewNames = NULL,
                         reReadData = T, dataProj = "dataProj.proj", dirOutputs = paste(getwd(), "/outputs", sep = ""),
                         regions = NULL) {

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
    if (!dir.exists(dirOutputs))
        {
            dir.create(dirOutputs)
        }  # Output Directory
    if (is.null(regions)) {
        if (!dir.exists(paste(dirOutputs, "/AllRegions", sep = "")))
            {
                dir.create(paste(dirOutputs, "/AllRegions", sep = ""))
            }  # GCAM output directory
        if (!dir.exists(paste(dirOutputs, "/AllRegions/regional", sep = "")))
            {
                dir.create(paste(dirOutputs, "/AllRegions/regional", sep = ""))
            }  # GCAM output directory
    } else {
        for (region_i in regions) {
            if (!dir.exists(paste(dirOutputs, "/", region_i, sep = "")))
                {
                  dir.create(paste(dirOutputs, "/", region_i, sep = ""))
                }  # GCAM output directory
            if (!dir.exists(paste(dirOutputs, "/", region_i, "/regional", sep = "")))
                {
                  dir.create(paste(dirOutputs, "/", region_i, "/regional", sep = ""))
                }  # GCAM output directory
        }
    }

    # Check for new scenario names
    if (is.null(scenNewNames)) {
        scenNewNames <- scenOrigNames
    }

    # Read gcam database or existing dataProj.proj
    if (!reReadData) {
        if (file.exists(paste(gcamdatabasePath, "/", dataProj, sep = ""))) {
            dataProjLoaded <- loadProject(paste(gcamdatabasePath, "/", dataProj, sep = ""))
        } else {
            stop(paste("No ", dataProj, " file exists. Please set reReadData=T to create dataProj.proj"))
        }
    } else {
        if (file.exists(dataProj))
            {
                file.remove(dataProj)
            }  # Delete old project file
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


    # Read in paramaters from query file and format for later use
    data <- tibble()

    # Total final energy by aggregate end-use sector
    paramx <- "Total final energy by aggregate end-use sector"
    if (paramx %in% queries) {
        tbl <- getQuery(dataProjLoaded, paramx)  # Tibble
        if (!is.null(regions)) {
            tbl <- tbl %>% dplyr::filter(region %in% regions)
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
        data <- bind_rows(data, tbl)
    } else {
        print(paste("Paramater '", paramx, "' not found in database", sep = ""))
    }

    # GDP MER by region
    paramx <- "GDP MER by region"
    if (paramx %in% queries) {
        tbl <- getQuery(dataProjLoaded, paramx)  # Tibble
        if (!is.null(regions)) {
            tbl <- tbl %>% dplyr::filter(region %in% regions)
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
        data <- bind_rows(data, tbl)
    } else {
        print(paste("Paramater '", paramx, "' not found in database", sep = ""))
    }


    #---------------------
    # Create Data Template
    #---------------------

    dataTemplate <- data %>%
      mutate(scenario = "Local Data", origScen = "Local Data", value = 0, origValue = 0, sources="Sources") %>%
      unique() %>%
      dplyr::select(scenario, region, sources, param, x, xLabel, vintage, class1, class2, units, value, aggregate,
                    classLabel1, classPalette1, classLabel2, classPalette2)

    #---------------------
    # Save Data in CSV
    #---------------------

    if (is.null(regions)) {
        utils::write.csv(data, file = paste(dirOutputs, "/AllRegions/regional/dataTable_", min(range(data$x)),
            "to", max(range(data$x)), ".csv", sep = ""), row.names = F)
        utils::write.csv(dataTemplate, file = paste(dirOutputs, "/AllRegions/regional/dataTableTemplate_", min(range(data$x)),
            "to", max(range(data$x)), ".csv", sep = ""), row.names = F)
    } else {
        for (region_i in regions) {
            utils::write.csv(data %>% dplyr::filter(region == region_i), file = paste(dirOutputs, "/", region_i,
                "/regional/dataTable_",region_i,"_", min(range(data$x)), "to", max(range(data$x)), ".csv", sep = ""),
                row.names = F)
            utils::write.csv(dataTemplate %>% dplyr::filter(region == region_i), file = paste(dirOutputs, "/", region_i,
                "/regional/dataTableTemplate_",region_i,"_", min(range(data$x)), "to", max(range(data$x)), ".csv", sep = ""),
                row.names = F)
        }
    }

    return(list(data = data, dataTemplate = dataTemplate, scenarios = scenarios, queries = queries))


}
