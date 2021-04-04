#' metis.readgcam
#'
#' This function connects to a gcamdatabase and uses a query file to
#' out results into a table ready for plotting.
#' @param dirOutputs Full path to directory for outputs
#' @param folderName Default = NULL
#' @param nameAppend  Default="". Name to append to saved files.
#' @param gcamdatabase Default = NULL. Full path to GCAM database folder.
#' @param queryFile Defualt = NULL. When NULL metis loads pre-saved xml file metis::xmlMetisQueries
#' @param dataProjFile Default = NULL. Optional. A default 'dataProj.proj' is produced if no .Proj file is specified.
#' @param scenOrigNames Default = "All". Original Scenarios names in GCAM database in a string vector.
#' For example c('scenario1','scenario2).
#' @param scenNewNames New Names which may be shorter and more useful for figures etc.
#' Default will use Original Names. For example c('scenario1','scenario2)
#' @param reReadData If TRUE will read the GCAM data base and create a queryData.proj file
#' in the same folder as the GCAM database. If FALSE will load a '.proj' file if a file
#' with full path is provided otherwise it will search for a dataProj.proj file in the existing
#' folder which may have been created from an old run.
#' @param regionsSelect The regions to analyze in a vector. Example c('Colombia','Argentina'). Full list:
#'
#' USA, Africa_Eastern, Africa_Northern, Africa_Southern, Africa_Western, Australia_NZ, Brazil, Canada
#' Central America and Caribbean, Central Asia, China, EU-12, EU-15, Europe_Eastern, Europe_Non_EU,
#' European Free Trade Association, India, Indonesia, Japan, Mexico, Middle East, Pakistan, Russia,
#' South Africa, South America_Northern, South America_Southern, South Asia, South Korea, Southeast Asia,
# Taiwan, Argentina, Colombia, Uruguay)
#' @param paramsSelect Default = "All".
#'
#' Choose "All" or paramSet from "energy", "electricity", "transport",
#' "water" , "socioecon" ,"ag" , "livestock" ,"land"  ,"emissions".
#'
#' Or pick an individual param from the list:
#'
#' # energy
#' "energyPrimaryByFuelEJ","energyPrimaryRefLiqProdEJ",
#' "energyFinalConsumBySecEJ","energyFinalByFuelBySectorEJ","energyFinalSubsecByFuelTranspEJ",
#' "energyFinalSubsecByFuelBuildEJ", "energyFinalSubsecByFuelIndusEJ","energyFinalSubsecBySectorBuildEJ",
#' "energyPrimaryByFuelMTOE","energyPrimaryRefLiqProdMTOE",
#' "energyFinalConsumBySecMTOE","energyFinalbyFuelMTOE","energyFinalSubsecByFuelTranspMTOE",
#' "energyFinalSubsecByFuelBuildMTOE", "energyFinalSubsecByFuelIndusMTOE","energyFinalSubsecBySectorBuildMTOE",
#' "energyPrimaryByFuelTWh","energyPrimaryRefLiqProdTWh",
#' "energyFinalConsumBySecTWh","energyFinalbyFuelTWh","energyFinalSubsecByFuelTranspTWh",
#' "energyFinalSubsecByFuelBuildTWh", "energyFinalSubsecByFuelIndusTWh","energyFinalSubsecBySectorBuildTWh",
#'
#' # electricity
#' "elecByTechTWh","elecCapByFuel","elecFinalBySecTWh","elecFinalByFuelTWh",
#' "elecNewCapCost","elecNewCapGW","elecAnnualRetPrematureCost","elecAnnualRetPrematureGW","elecCumCapCost","elecCumCapGW","elecCumRetPrematureCost","elecCumRetPrematureGW",
#'
#' # transport
#' "transportPassengerVMTByMode", "transportFreightVMTByMode", "transportPassengerVMTByFuel", "transportFreightVMTByFuel",
#'
#' # water
#' "watConsumBySec", "watWithdrawBySec", "watWithdrawByCrop", "watBioPhysCons", "watIrrWithdrawBasin","watIrrConsBasin",
#'
#' # socioecon
#' "gdpPerCapita", "gdp", "gdpGrowthRate", "pop",
#'
#'  # ag
#'  "agProdbyIrrRfd", "agProdBiomass", "agProdForest","agProdByCrop",
#'
#'  # livestock
#' "livestock_MeatDairybyTechMixed","livestock_MeatDairybyTechPastoral","livestock_MeatDairybyTechImports", "livestock_MeatDairybySubsector",
#'
#' # land
#' "landIrrRfd", "landIrrCrop","landRfdCrop", "landAlloc","landAllocByCrop",
#'
#'  # emissions
#' "emissLUC", "emissNonCO2BySectorGWPAR5","emissNonCO2BySectorGTPAR5","emissNonCO2BySectorOrigUnits",
#' "emissNonCO2ByResProdGWPAR5", "emissBySectorGWPAR5FFI","emissMethaneBySourceGWPAR5",
#' "emissByGasGWPAR5FFI", "emissByGasGWPAR5LUC", "emissBySectorGWPAR5LUC",
#' "emissNonCO2ByResProdGTPAR5", "emissBySectorGTPAR5FFI","emissMethaneBySourceGTPAR5",
#' "emissByGasGTPAR5FFI", "emissByGasGTPAR5LUC","emissBySectorGTPAR5LUC",
#' "emissCO2BySectorNoBio"
#' @param saveData Default = "T". Set to F if do not want to save any data to file.
#' @return A list with the scenarios in the gcam database, queries in the queryxml file and a
#' tibble with gcam data formatted for metis charts aggregated to different categories.
#' These include data, dataAggParam, dataAggClass1, dataAggClass2.
#' @keywords gcam, gcam database, query
#' @export


metis.readgcam <- function(gcamdatabase = NULL,
                           queryFile = NULL,
                           dataProjFile = paste(getwd(), "/outputs/dataProj.proj", sep = ""),
                           scenOrigNames = "All",
                           scenNewNames = NULL,
                           reReadData = T,
                           dirOutputs = paste(getwd(), "/outputs", sep = ""),
                           regionsSelect = NULL,
                           paramsSelect="All",
                           folderName=NULL,
                           nameAppend="",
                           saveData = T
){


  # gcamdatabase = NULL
  # queryFile = NULL
  # dataProjFile = paste(getwd(), "/outputs/dataProj.proj", sep = "")
  # scenOrigNames = "All"
  # scenNewNames = NULL
  # reReadData = T
  # dirOutputs = paste(getwd(), "/outputs", sep = "")
  # regionsSelect = NULL
  # paramsSelect="All"
  # folderName=NULL
  # nameAppend=""
  # saveData = T

  #----------------
  # Initialize variables by setting to NULL
  #----------------

  NULL -> vintage -> year -> xLabel -> x -> value -> sector -> scenario -> region -> param -> origX -> origValue ->
    origUnits -> origScen -> origQuery -> classPalette2 -> classPalette1 -> classLabel2 -> classLabel1 -> class2 ->
    class1 -> connx -> aggregate -> Units -> sources -> paramx -> fuel -> technology -> input -> output -> water ->
    landleaf -> ghg -> Convert -> regionsSelectAll->cf1971to2100->gcamCapacityFactor -> . -> GWPAR5 -> tblelecByTechTWh ->
    totalFFINonCO2 -> FracBioFuel -> FracFossilFuel -> TotalLiquids -> agg_tech->
    class_temp -> resource -> subRegAreaSum -> subsector->tblFinalNrgIntlAvShipMod -> 'transportation' ->
    'International Aviation' -> 'International Ship' -> 'International Aviation oil' -> 'a oil' ->
    'International Ship oil' -> 'International Aviation liquids' -> liquids -> 'International Ship liquids'->crop->
    paramsSelectAll -> tblFinalNrgIntlAvShip->datax->group->basin->subRegion->query->subresource


#---------------------
# Params and Queries
#---------------------

  paramQueryMap <- (metis::metis.mappings()$mapParamQuery)%>%dplyr::select(group,param,query)

  # Check if queriesSelect is a querySet or one of the queries
  if(!any(c("all","All","ALL") %in% paramsSelect)){
  if(any(paramsSelect %in% unique(paramQueryMap$group))){
    queriesSelectx <- as.vector(unlist(unique((paramQueryMap%>%dplyr::filter(group %in% paramsSelect))$query)))
    #print(paste("queriesSelect chosen include the following querySets: ",paste(paramsSelect,collapse=", "),".",sep=""))
    #print(paste("Which include the following queries: ",paste(queriesSelectx,collapse=", "),".",sep=""))
    #print(paste("Other queries not run include: ",paste(as.vector(unlist(querySets))[!as.vector(unlist(querySets)) %in% queriesSelectx],collapse=", "),".",sep=""))
  }else{
    if(any(paramsSelect %in% as.vector(unique(paramQueryMap$param)))){
      queriesSelectx<- as.vector(unlist(unique((paramQueryMap%>%dplyr::filter(param %in% paramsSelect))$query)))
      #print(paste("queriesSelect chosen include the following queries: ",paste(queriesSelectx,collapse=", "),".",sep=""))
     # print(paste("Other queries not run include: ",paste(as.vector(unlist(querySets))[!as.vector(unlist(querySets)) %in% queriesSelectx],collapse=", "),".",sep=""))
    }else {
      queriesSelectx <-  NULL
      print(paste("Params in metisQueries.xml include: ",paste(as.vector(unlist(unique(paramQueryMap$param))),collapse=", "),".",sep=""))
      print("")
      print(paste("None of the chosen paramsSelect are available in metis params: ",paste(paramsSelect,collapse=", "),".",sep=""))
      stop("None of the params chosen are available.")
      }
  }}else{
    queriesSelectx <- as.vector(unlist(unique(paramQueryMap$query)))
  }

#-----------------------------
# Create necessary directories if they dont exist.
#----------------------------
  if (!dir.exists(dirOutputs)){
    dir.create(dirOutputs)}  # Output Directory
  if (!dir.exists(paste(dirOutputs, "/", folderName, sep = ""))){
    dir.create(paste(dirOutputs, "/", folderName, sep = ""))}
  if (!dir.exists(paste(dirOutputs, "/", folderName,"/readGCAM",sep=""))){
    dir.create(paste(dirOutputs, "/", folderName,"/readGCAM",sep=""))}  # Output Directory
  if (!dir.exists(paste(dirOutputs, "/", folderName, "/readGCAM/Tables_gcam", sep = ""))){
    dir.create(paste(dirOutputs, "/", folderName, "/readGCAM/Tables_gcam", sep = ""))}  # GCAM output directory

#----------------
# Set file paths
#----------------

  if(is.null(gcamdatabase)){
    gcamdatabasePath = NULL
    gcamdatabaseName = NULL
  }else{
    if(is.character(gcamdatabase)){
      if(dir.exists(gcamdatabase)){
        gcamdatabasePath <- gsub("/$","",gsub("[^/]+$","",gcamdatabase)); gcamdatabasePath
        gcamdatabaseName <- basename(gcamdatabase); gcamdatabaseName
        print(paste("Connecting to GCAM database provided ",gcamdatabase,"...",sep=""))
      }else{stop(paste("The GCAM database path provided dos not exist: ", gcamdatabase, sep=""))}
    }else{
      print(paste("gcamdatabase provided is not a character string to the GCAM database path. Please check your entry."))
    }
  }

  if(is.null(queryFile)){
    XML::saveXML(metis::xmlMetisQueries, file=paste(dirOutputs, "/", folderName,"/readGCAM/metisQueries.xml", sep = ""))
    queryFile <- paste(dirOutputs, "/", folderName,"/readGCAM/metisQueries.xml", sep = "")
    xfun::gsub_file(queryFile,"&apos;","'")
    queryPath <- gsub("[^/]+$","",queryFile)
    queryxml <- basename(queryFile)
  }else{
    if(is.character(queryFile)){
      if(file.exists(queryFile)){
        queryPath <- gsub("[^/]+$","",queryFile)
        queryxml <- basename(queryFile)
        print(paste("Connecting to the queryFile provided ",queryFile,"...",sep=""))
      }else{print(paste("The queryFile path provided dos not exist: ", queryFile, sep=""))}
    }else{
      print(paste("The queryFile path provided is not a character string to the query file. Please check your entry."))
    }
  }


  if(is.null(dataProjFile)){
    dataProj = "dataProj"
    dataProjPath = gsub("//","/",paste(dirOutputs, "/", folderName,"/readGCAM/", sep = ""))
  }else{
    if(is.list(dataProjFile)){
      dataProjPath <- gsub("//","/",paste(dirOutputs, "/", folderName,"/readGCAM/", sep = ""))
      dataProj <- paste("dataProj", sep = "")
    }else{
    if(is.character(dataProjFile)){
      if(grepl("/",dataProjFile)){
        if(file.exists(dataProjFile)){
        dataProjPath <- gsub("[^/]+$","",dataProjFile)
        dataProj <- basename(dataProjFile)
        print(paste("Connecting to the dataProjFile provided ",dataProjFile,"...",sep=""))}else{
          dataProjPath <- gsub("[^/]+$","",dataProjFile)
          dataProj <- basename(dataProjFile)
          print(gsub("//","/",paste("Will save GCAM data to ",dataProjPath,"/",dataProjFile,"...",sep="")))
        }
      }else{
        dataProjPath <- gsub("//","/",paste(dirOutputs, "/", folderName,"/readGCAM/", sep = ""))
        dataProj <- dataProjFile
        print(paste("Will save data to: ", dataProjPath,"/",dataProjFile, sep=""))
      }
    }else{
      print(paste("The dataProjFile path provided is not a character string to the query file. Please check your entry."))
    }
    }
  }


  # Set new scenario names if provided
  if (is.null(scenOrigNames)) {
    scenNewNames <- NULL
    #print("scenOrigNames is NULL so cannot assign scenNewNames.")
    #print("To set new names for scenarios please enter original names in scenOrigNames and then corresponding new names in scenNewNames.")
  } else {
    if(any(c("all","All","ALL") %in% scenOrigNames)){
      scenNewNames <- NULL
      #print("scenOrigNames is All so cannot assign scenNewNames.")
      #print("To set new names for scenarios please enter original names in scenOrigNames and then corresponding new names in scenNewNames.")
    }
    }

#---------------------------------------------
# Read gcam database or existing dataProj.proj
#--------------------------------------------

  # In case user sets reReadData=F and provides a .proj file instead of a gcamdatabase
  if((is.null(gcamdatabasePath) | is.null(gcamdatabaseName)) &
     reReadData==T){
    if(is.list(dataProjFile)){
      reReadData=F
      #print("Setting reReadData to F because no gcamdatabase is provided but a valid dataProjFile provided.")
    }
    if(file.exists(paste(dataProjPath,"/",dataProj,sep=""))){
    reReadData=F
    #print("Setting reReadData to F because no gcamdatabase is provided but a valid dataProjFile provided.")
    }
  }

  if (!reReadData) {
 # Check for proj file path and folder if incorrect give error
    if(!is.list(dataProjFile)){
    if(!file.exists(gsub("//","/",paste(dataProjPath, "/", dataProj, sep = "")))){
      stop(gsub("//","/",paste("dataProj file: ", dataProjPath,"/",dataProj," is incorrect or doesn't exist.",sep="")))}
    }

  # Checking if dataProjFile is preloaded xml metis::xmlMetiQueries
  if(is.list(dataProjFile)){
    dataProjLoaded <- rgcam::loadProject(dataProjFile)
    }else{
   if (file.exists(gsub("//","/",paste(dataProjPath, "/", dataProj, sep = "")))) {
      dataProjLoaded <- rgcam::loadProject(gsub("//","/",paste(dataProjPath, "/", dataProj, sep = "")))
   } else {
     stop(paste("No ", dataProj, " file exists. Please set reReadData=T to create dataProj.proj"))
   }}

      scenarios <- rgcam::listScenarios(dataProjLoaded); scenarios  # List of Scenarios in GCAM database
      queries <- rgcam::listQueries(dataProjLoaded); queries  # List of queries in GCAM database

      # Select Scenarios
      if(is.null(scenOrigNames)){
        scenOrigNames <- scenarios[1]
        print(paste("scenOrigNames set to NULL so using only first scenario: ",scenarios[1],sep=""))
        print(paste("from all available scenarios: ",paste(scenarios,collapse=", "),sep=""))
        print("To run all scenarios please set scenOrigNames to 'All' or")
        print(paste("you can choose a subset of scenarios by setting the scenOrigNames input (eg. scenOrigNames = c('scen1','scen2'))" ,sep=""))
      } else {
        if(any(c("all","All","ALL") %in% scenOrigNames)){
          scenOrigNames <- scenarios
          print(paste("scenOrigNames set to 'All' (Default) so using all available scenarios: ",paste(scenarios,collapse=", "),sep=""))
          print(paste("You can choose a subset of scenarios by setting the scenOrigNames input (eg. scenOrigNames = c('scen1','scen2'))" ,sep=""))
        } else {
          if(any(scenOrigNames %in% scenarios)){
            print(paste("scenOrigNames available in scenarios are :",paste(scenOrigNames[scenOrigNames %in% scenarios],collapse=", "),sep=""))
            if(length(scenOrigNames[!scenOrigNames %in% scenarios])>0){
              print(paste("scenOrigNames not available in scenarios are :",paste(scenOrigNames[!scenOrigNames %in% scenarios],collapse=", "),sep=""))}
            if(length(scenarios[!scenarios %in% scenOrigNames])>0){
              print(paste("Other scenarios not selected are :",paste(scenarios[!scenarios %in% scenOrigNames],collapse=", "),sep=""))}
          } else {
            print(paste("None of the scenOrigNames : ",paste(scenOrigNames,collapse=", "),sep=""))
            print(paste("are in the available scenarios : ",paste(scenarios,collapse=", "),sep=""))
          }
        }
      }

      scenarios <- scenOrigNames # Set scenarios to chosen scenarios

  } else {

  # Check for query file and folder if incorrect give error
    if(!file.exists(gsub("//","/",paste(queryPath, "/", queryxml, sep = "")))){stop(paste("query file: ", queryPath,"/",queryxml," is incorrect or doesn't exist.",sep=""))}
    if(file.exists(gsub("//","/",paste(queryPath, "/subSetQueries.xml", sep = "")))){unlink(gsub("//","/",paste(queryPath, "/subSetQueries.xml", sep = "")))}

    # Subset the query file if queriwsSelect is not "All"
    if(!any(c("All","all","ALL") %in% paramsSelect)){

    xmlFilePath = gsub("//","/",paste(queryPath, "/", queryxml, sep = ""))
    xmlfile <- XML::xmlTreeParse(xmlFilePath)
    xmltop <- XML::xmlRoot(xmlfile)
    top <- XML::xmlNode(XML::xmlName(xmltop))

    for(i in 1:length(xmltop)){
      for(j in 1:length(queriesSelectx)){
        if(any(grepl(gsub("\\(","\\\\(",gsub("\\)","\\\\)",queriesSelectx[j])), as.character(xmltop[[i]]))))
          top <- XML::addChildren(top, xmltop[[i]])
      }
    }
    XML::saveXML(top, file=gsub("//","/",paste(queryPath, "/subSetQueries.xml", sep = "")))
    } else {
      print(paste("paramsSelect includes 'All' so running all available queries: ",paste(queriesSelectx,collapse=", "),".",sep=""))
      file.copy(from=gsub("//","/",paste(queryPath, "/", queryxml, sep = "")),
                to=gsub("//","/",paste(queryPath, "/subSetQueries.xml", sep = "")))
    }

    if(!file.exists(gsub("//","/",paste(queryPath, "/subSetQueries.xml", sep = "")))){
      stop(gsub("//","/",paste("query file: ", queryPath,"/subSetQueries.xml is incorrect or doesn't exist.",sep="")))}else{
        xfun::gsub_file(paste(queryPath, "/subSetQueries.xml", sep = ""),"&apos;","'")
        print(gsub("//","/",paste("Reading queries from queryFile created: ", queryPath,"/subSetQueries.xml.",sep="")))
      }

    # Check for gcamdatbasePath and gcamdatabasename
    if(!is.null(gcamdatabase)){
    if(is.null(gcamdatabasePath) | is.null(gcamdatabaseName)){
      stop(gsub("//","/",paste("GCAM database: ", gcamdatabasePath,"/",gcamdatabaseName," is incorrect or doesn't exist.",sep="")))}
    if(!file.exists(gsub("//","/",paste(gcamdatabasePath, "/", gcamdatabaseName, sep = "")))){
      stop(gsub("//","/",paste("GCAM database: ", gcamdatabasePath,"/",gcamdatabaseName," is incorrect or doesn't exist.",sep="")))}
    }

    # Get names of scenarios in database
    # Save Message from rgcam::localDBConn to a text file and then extract names
    zz <- file(paste(getwd(),"/test.txt",sep=""), open = "wt")
    sink(zz,type="message")
    rgcam::localDBConn(gcamdatabasePath,gcamdatabaseName)
    sink()
    closeAllConnections()
    # Read temp file
    con <- file(paste(getwd(),"/test.txt",sep=""),open = "r")
    first_line <- readLines(con,n=1); first_line
    closeAllConnections()
    if(grepl("error",first_line,ignore.case = T)){stop(paste(first_line))}
    print(first_line)
    if(file.exists(paste(getwd(),"/test.txt",sep=""))){unlink(paste(getwd(),"/test.txt",sep=""))}
    # Extract scenario names from saved line
    s1 <- gsub(".*:","",first_line);s1
    s2 <- gsub(" ","",s1);s2
    scenarios <- as.vector(unlist(strsplit(s2,",")))
    print(paste("All scenarios in data available: ", paste(scenarios,collapse=", "), sep=""))


    if(file.exists(gsub("//","/",paste(dataProjPath, "/", dataProj, sep = "")))){
      unlink(gsub("//","/",paste(dataProjPath, "/", dataProj, sep = "")))}  # Delete old project file

    # Select Scenarios
    if(is.null(scenOrigNames)){
      scenOrigNames <- scenarios[1]
      print(paste("scenOrigNames set to NULL so using only first scenario: ",scenarios[1],sep=""))
      print(paste("from all available scenarios: ",paste(scenarios,collapse=", "),sep=""))
      print("To run all scenarios please set scenOrigNames to 'All'")
    } else {
        if(any(c("all","All","ALL") %in% scenOrigNames)){
          scenOrigNames <- scenarios
          print(paste("scenOrigNames set to 'All' so using all available scenarios: ",paste(scenarios,collapse=", "),sep=""))
        } else {
          if(any(scenOrigNames %in% scenarios)){
            print(paste("scenOrigNames available in scenarios are :",paste(scenOrigNames[scenOrigNames %in% scenarios],collapse=", "),sep=""))
            if(length(scenOrigNames[!scenOrigNames %in% scenarios])>0){
            print(paste("scenOrigNames not available in scenarios are :",paste(scenOrigNames[!scenOrigNames %in% scenarios],collapse=", "),sep=""))}
            if(length(scenarios[!scenarios %in% scenOrigNames])>0){
            print(paste("Other scenarios not selected are :",paste(scenarios[!scenarios %in% scenOrigNames],collapse=", "),sep=""))}
          } else {
            print(paste("None of the scenOrigNames : ",paste(scenOrigNames,collapse=", "),sep=""))
            print(paste("are in the available scenarios : ",paste(scenarios,collapse=", "),sep=""))
            stop("Please check scenOrigNames and rerun.")
          }
        }
    }

    for (scenario_i in scenOrigNames) {
       dataProj.proj <- rgcam::addScenario(conn = rgcam::localDBConn(gcamdatabasePath, gcamdatabaseName), proj = gsub("//","/",paste(dataProjPath, "/", dataProj, sep = "")),
                                           scenario = scenario_i, queryFile = gsub("//","/",paste(queryPath, "/subSetQueries.xml", sep = "")))  # Check your queries file
      }

    dataProjLoaded <- rgcam::loadProject(gsub("//","/",paste(dataProjPath, "/", dataProj, sep = "")))

    # Save list of scenarios and queries
    scenarios <- rgcam::listScenarios(dataProjLoaded); scenarios  # List of Scenarios in GCAM database
    queries <- rgcam::listQueries(dataProjLoaded); queries  # List of queries in GCAM database
  }

  queries <- rgcam::listQueries(dataProjLoaded); queries  # List of Queries in queryxml

  # Set new scenario names if provided
  if (is.null(scenNewNames)) {
    scenNewNames <- scenOrigNames}else{
      scenNewNames <- scenNewNames[1:length(scenOrigNames)]
    }

  # Read in paramaters from query file to create formatted table
 queriesx <- queries

  if(!any(queriesSelectx %in% queries)){stop("None of the selected params are available in the data that has been read.
                                             Please check your data if reRead was set to F. Otherwise check the paramSelect entries and the queryxml file.")}

  paramsSelectAll <- as.vector(unlist(unique(paramQueryMap$param)))


  if(any(c("all","All","ALL") %in% paramsSelect)){
    paramsSelectx <- paramsSelectAll
    } else {
      if(any(paramsSelect %in% as.vector(unique(paramQueryMap$group)))){
        paramsSelectx <- unique((paramQueryMap%>%dplyr::filter(group %in% paramsSelect))$param)
      } else {paramsSelectx=paramsSelect}
    }

  # Check if any of the selected parameters are available in the GCAM data
  if(any(paramsSelectx %in% paramsSelectAll)){

  datax <- tibble::tibble()

  if(T){

  queriesx <- queriesx[queriesx %in% queries]

  paramx<-"energyFinalConsumByIntlShpAvEJ"
  # Total final energy by aggregate end-use sector
  if(paramx %in% paramsSelectx){
    queryx <- "transport final energy by mode and fuel"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% c(regionsSelect))
      }
      tbl <- tbl %>%
        dplyr::filter(mode %in% c("International Aviation", "International Ship"))%>%
        dplyr::filter(scenario %in% scenOrigNames)%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "energyFinalConsumByIntlShpAvEJ",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year, subRegion=region,
                      scenario = scenNewNames,
                      units = "Final Energy Intl. Av and Shp (EJ)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = mode,
                      classLabel1 = "Sector",
                      classPalette1 = "pal_metis",
                      class2 = gsub(" enduse","",input),
                      classLabel2 = "Fuel",
                      classPalette2 = "pal_metis")%>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%
        dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      tblFinalNrgIntlAvShip <- tbl
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}


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
        dplyr::filter(scenario %in% scenOrigNames)%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "energyFinalConsumBySecEJ",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year, subRegion=region,
                      scenario = scenNewNames,
                      units = "Final Energy by Sector (EJ)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = dplyr::case_when(
                        grepl("building|comm|resid",sector)~"building",
                        grepl("industry",sector)~"industry",
                        grepl("transport",sector)~"transport"),
                      classLabel1 = "Sector",
                      classPalette1 = "pal_metis",
                      class2 = sector,
                      classLabel2 = "Subsector",
                      classPalette2 = "pal_metis")%>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%
        dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))

      if(!is.null(tblFinalNrgIntlAvShip)){
      # Separat out Intl. Shipping and Aviation from Transport
      tblTransport <- tbl%>%dplyr::filter(class1=="transportation") %>%
        dplyr::mutate(class2="class2",classLabel2="classLabel2",classPalette2="classPalette2") %>%
        dplyr::select(-origValue)# Subset Transport Sector
      tblFinalNrgIntlAvShipMod <- tblFinalNrgIntlAvShip %>%
        dplyr::mutate(param=unique(tblTransport$param),
                      sources=unique(tblTransport$sources),
                      origQuery=unique(tblTransport$origQuery),
                      origUnits=unique(tblTransport$origUnits),
                      units=unique(tblTransport$units),
                      xLabel=unique(tblTransport$xLabel),
                      aggregate=unique(tblTransport$aggregate),
                      class2=unique(tblTransport$class2),
                      classLabel2=unique(tblTransport$classLabel2),
                      classPalette2=unique(tblTransport$classPalette2),
                      classLabel1=unique(tblTransport$classLabel1),
                      classPalette1=unique(tblTransport$classPalette1))%>%
        dplyr::select(-origValue)# Prepare in intl. transport in correct format
      # Separate out Intl. Shipping and Aviation
      tblSepTransportIntlAvShip <- tblTransport %>%
        dplyr::bind_rows(tblFinalNrgIntlAvShipMod) %>%
        tidyr::spread(key="class1",value="value") %>%
        dplyr::mutate(transportation=transportation-`International Aviation`-`International Ship`)%>%
        dplyr::rename(`transport intl av`=`International Aviation`,
                      `transport intl shp`=`International Ship`) %>%
        tidyr::gather(key="class1",value="value",
                      -scenario, -region, -subRegion, -param, -sources, -class2, -x, -xLabel, -vintage, -units, -aggregate,
                      -classLabel1, -classPalette1, -classLabel2, -classPalette2,
                      -origScen,-origQuery,-origUnits,-origX)%>%
        dplyr::mutate(origValue=value); tblSepTransportIntlAvShip%>%as.data.frame()
      # Rbind Transport, Intl. Shipping and Aviation back to all other Final Energy types
      tblMod<-tbl%>%dplyr::filter(class1!="transportation") %>%
        dplyr::bind_rows(tblSepTransportIntlAvShip) # Remove Transport sector from Original tbl

      } else {
        # print(paste("tblFinalNrgIntlAvShip does not exist so skipping subset of final energy to remove intl. shipping and aviation."))
        tblMod <- tbl
        }

      tblMod <- tblMod %>%
        dplyr::mutate(class2 = dplyr::case_when(grepl("comm|resid|building",class1)~"building",
                                         grepl("industry",class1)~"industry",
                                         grepl("transport",class1)~"transport",
                                         TRUE~class2)) %>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))

      datax <- dplyr::bind_rows(datax, tblMod)
    } else {
      # if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
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
        dplyr::filter(scenario %in% scenOrigNames)%>%
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
                      origX = year, subRegion=region,
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
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%
        dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
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
        dplyr::filter(scenario %in% scenOrigNames)%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::filter(!grepl("trn",input))%>%
        dplyr::mutate(input=dplyr::if_else(input=="biomass","bioenergy",input),
                      sector=gsub("process heat cement","industry",sector),
                      sector=gsub("cement","industry",sector),
                      sector=gsub("industrial energy use","industry",sector),
                      sector=gsub("industrial feedstocks","industry",sector),
                      sector=gsub("N fertilizer","industry",sector),
                      sector=gsub("trn_aviation_intl","trans intl av",sector),
                      sector=gsub("trn_shipping_intl","trans intl shp",sector),
                      sector = replace(sector, stringr::str_detect(sector, "trn"), "transport"),
                      sector=gsub("comm cooling","building",sector),
                      sector=gsub("comm heating","building",sector),
                      sector=gsub("comm others","building",sector),
                      sector=gsub("resid cooling","building",sector),
                      sector=gsub("resid heating","building",sector),
                      sector=gsub("resid others","building",sector),
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
                      origX = year, subRegion=region,
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
        dplyr::mutate(class1=dplyr::case_when(class2=="trans intl av"~paste(class1,"av",sep=" "),
                                             class2=="trans intl shp"~paste(class1,"shp",sep=" "),
                                             TRUE~class1))%>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
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
        dplyr::filter(scenario %in% scenOrigNames)%>%
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
                      origX = year, subRegion=region,
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
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
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
        dplyr::filter(scenario %in% scenOrigNames)%>%
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
                      origX = year, subRegion=region,
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
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}

  paramx<-"elecFinalBySecTWh"
  # Total final energy by aggregate end-use sector
  if(paramx %in% paramsSelectx){
    queryx <- "inputs by tech"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }

      # USA
      tblUSA <- tbl %>%
        dplyr::filter(grepl("domestic supply",input),
                      region %in% metis.assumptions("US52"),
                      scenario %in% scenOrigNames)%>%
        dplyr::mutate(
          class2=sector,
          sector=dplyr::case_when(
          grepl("comm |resid |elect_td_bld",sector,ignore.case = T)~"building",
          grepl("elect_td_trn",sector,ignore.case = T)~"transport",
          grepl("elect_td_ind",sector,ignore.case = T)~"industry",
          TRUE~sector))%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "elecFinalBySecTWh",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year, subRegion=region,
                      scenario = scenNewNames,
                      value = value * metis.assumptions("convEJ2TWh"),
                      units = "Final Electricity by Sector (TWh)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = sector,
                      classLabel1 = "Sector",
                      classPalette1 = "pal_metis",
                      classLabel2 = "subsector",
                      classPalette2 = "classPalette2")%>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))

      # CORE
      tblCORE <- tbl %>%
        dplyr::filter(!region %in% c(metis.assumptions("US52")),
                      scenario %in% scenOrigNames,
                      sector %in% c("elect_td_bld","elect_td_trn","elect_td_ind"))%>%
        dplyr::mutate(
          sector=dplyr::case_when(
            grepl("elect_td_bld",sector,ignore.case = T)~"building",
            grepl("elect_td_trn",sector,ignore.case = T)~"transport",
            grepl("elect_td_ind",sector,ignore.case = T)~"industry",
            TRUE~sector))%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "elecFinalBySecTWh",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year, subRegion=region,
                      scenario = scenNewNames,
                      value = value * metis.assumptions("convEJ2TWh"),
                      units = "Final Electricity by Sector (TWh)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = sector,
                      classLabel1 = "Sector",
                      classPalette1 = "pal_metis",
                      class2 = sector,
                      classLabel2 = "subsector",
                      classPalette2 = "classPalette2")%>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))

      datax <- dplyr::bind_rows(datax, tblUSA,tblCORE)
    } else {
      # if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
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
        dplyr::filter(!grepl("trn",input))%>%
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
        dplyr::filter(scenario %in% scenOrigNames)%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "elecFinalByFuelTWh",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year, subRegion=region,
                      scenario = scenNewNames,
                      value = value * metis.assumptions("convEJ2TWh"),
                      units = "Final Electricity by Fuel (TWh)",
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
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}

  paramx<-"energyPrimaryByFuelEJ"
  # primary energy consumption by region (direct equivalent)
  if(paramx %in% paramsSelectx){
    queryx <- "primary energy consumption by region (direct equivalent)"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::filter(scenario %in% scenOrigNames)%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "energyPrimaryByFuelEJ",
                      fuel=gsub("biomass","bioenergy",fuel),
                      fuel=gsub("b biomass","b bioenergy",fuel),
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year, subRegion=region,
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
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))

      if(!is.null(tblFinalNrgIntlAvShip)){
        # Separat out Intl. Shipping and Aviation refined liquids from Primary Energy Oil
        tblPrimaryOil <- tbl%>%dplyr::filter(class1=="a oil") %>%
          dplyr::mutate(class2="class2",classLabel2="classLabel2",classPalette2="classPalette2") %>%
          dplyr::select(-origValue)# Subset Transport Sector
        tblFinalNrgIntlAvShipMod <- tblFinalNrgIntlAvShip %>%
          dplyr::mutate(param=unique(tblPrimaryOil$param),
                        class1=paste(class1,"oil",sep=" "),
                        sources=unique(tblPrimaryOil$sources),
                        origQuery=unique(tblPrimaryOil$origQuery),
                        origUnits=unique(tblPrimaryOil$origUnits),
                        units=unique(tblPrimaryOil$units),
                        xLabel=unique(tblPrimaryOil$xLabel),
                        aggregate=unique(tblPrimaryOil$aggregate),
                        class2=unique(tblPrimaryOil$class2),
                        classLabel2=unique(tblPrimaryOil$classLabel2),
                        classPalette2=unique(tblPrimaryOil$classPalette2),
                        classLabel1=unique(tblPrimaryOil$classLabel1),
                        classPalette1=unique(tblPrimaryOil$classPalette1))%>%
          dplyr::select(-origValue)# Prepare in intl. transport in correct format
        # Separate out Intl. Shipping and Aviation
        tblSepPrimaryIntlAvShip <- tblPrimaryOil %>%
          dplyr::bind_rows(tblFinalNrgIntlAvShipMod) %>%
          tidyr::spread(key="class1",value="value") %>%
          dplyr::mutate(`a oil`=`a oil` -`International Aviation oil`-`International Ship oil`)%>%
          dplyr::rename(`oil intl av`=`International Aviation oil`,
                        `oil intl shp`=`International Ship oil`)%>%
          tidyr::gather(key="class1",value="value",
                        -scenario, -region, -subRegion, -param, -sources, -class2, -x, -xLabel, -vintage, -units, -aggregate,
                        -classLabel1, -classPalette1, -classLabel2, -classPalette2,
                        -origScen,-origQuery,-origUnits,-origX)%>%
          dplyr::mutate(origValue=value); tblSepPrimaryIntlAvShip%>%as.data.frame()
        # Rbind Transport, Intl. Shipping and Aviation back to all other Final Energy types
        tblMod<-tbl%>%dplyr::filter(class1!="a oil") %>%
          dplyr::bind_rows(tblSepPrimaryIntlAvShip) # Remove Transport sector from Original tbl

      } else {
        print(paste("tblFinalNrgIntlAvShip does not exist so skipping subset of final energy to remove intl. shipping and aviation."))
        tblMod <- tbl
      }

      tblMod <- tblMod %>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))

      datax <- dplyr::bind_rows(datax, tblMod)

    } else {
      # if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}

  paramx <- "elecByTechTWh"
  if(paramx %in% paramsSelectx){
    tbl<-tibble::tibble()
    tblUSA<-tibble::tibble()
    tblUSACogen<-tibble::tibble()
    tblGCAMReg<-tibble::tibble()

    #-------------
    # For GCAM USA no cogen
    #-------------
    queryx <- "elec gen by gen tech USA"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>%
          dplyr::filter(region %in% regionsSelect)
      }
      if (nrow(tbl)>0) {
      tbl <- tbl %>%
        dplyr::filter(region %in% metis.assumptions("US52"))%>%
        dplyr::filter(!sector %in% "industrial energy use")
      }
      tblUSA <- tbl %>%
        dplyr::filter(scenario %in% scenOrigNames)%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "elecByTechTWh",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year, subRegion=region,
                      scenario = scenNewNames,
                      value = value * metis.assumptions("convEJ2TWh"),
                      units = "Electricity Generation by Fuel (TWh)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = subsector,
                      classLabel1 = "Fuel",
                      classPalette1 = "pal_metis",
                      class2 = paste(technology,sector,sep=" "),
                      classLabel2 = "Technology",
                      classPalette2 = "pal_metis")
    }
    #-------------
    # For GCAM USA cogen
    #-------------
    # queryx <- "elec gen by gen tech cogen USA"
    # if (queryx %in% queriesx) {
    #   tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
    #   if (!is.null(regionsSelect)) {
    #     tbl <- tbl %>%
    #       dplyr::filter(region %in% regionsSelect)
    #   }
    #   if (nrow(tbl)>0) {
    #     tbl <- tbl %>%
    #       dplyr::filter(region %in% metis.assumptions("US52"))
    #   }
    #   tblUSACogen <- tbl %>%
    #     dplyr::filter(scenario %in% scenOrigNames)%>%
    #     dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
    #     dplyr::mutate(param = "elecByTechTWh",
    #                   sources = "Sources",
    #                   origScen = scenario,
    #                   origQuery = queryx,
    #                   origValue = value,
    #                   origUnits = Units,
    #                   origX = year, subRegion=region,
    #                   scenario = scenNewNames,
    #                   value = value * metis.assumptions("convEJ2TWh"),
    #                   units = "Electricity Generation by Fuel (TWh)",
    #                   vintage = paste("Vint_", year, sep = ""),
    #                   x = year,
    #                   xLabel = "Year",
    #                   aggregate = "sum",
    #                   class1 = subsector,
    #                   classLabel1 = "Fuel",
    #                   classPalette1 = "pal_metis",
    #                   class2 = technology,
    #                   classLabel2 = "Technology",
    #                   classPalette2 = "pal_metis")
    # }
    tblUSACogen <- tibble::tibble()
      #--------------------
      # GCAM other Regions
      #------------------------
      queryx <- "elec gen by gen tech and cooling tech"
      if (queryx %in% queriesx) {
        tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
        if (!is.null(regionsSelect)) {
          tbl <- tbl %>%
            dplyr::filter(region %in% regionsSelect) %>%
            dplyr::filter(!region %in% metis.assumptions("US52"))
        }
        tblGCAMReg <- tbl %>%
          dplyr::filter(scenario %in% scenOrigNames)%>%
          dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
          dplyr::mutate(param = "elecByTechTWh",
                        sources = "Sources",
                        origScen = scenario,
                        origQuery = queryx,
                        origValue = value,
                        origUnits = Units,
                        origX = year, subRegion=region,
                        scenario = scenNewNames,
                        value = value * metis.assumptions("convEJ2TWh"),
                        units = "Electricity Generation by Fuel (TWh)",
                        vintage = paste("Vint_", year, sep = ""),
                        x = year,
                        xLabel = "Year",
                        aggregate = "sum",
                        class1 = subsector,
                        classLabel1 = "Fuel",
                        classPalette1 = "pal_metis",
                        class2 = technology,
                        classLabel2 = "Technology",
                        classPalette2 = "pal_metis")
      }else {
        # if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
      }

    if(nrow(tblUSA)>0 | nrow(tblUSACogen)>0 | nrow(tblGCAMReg)>0){
      # Combine USA with others
      commonNames = c(names(tblUSA),names(tblUSACogen),names(tblGCAMReg))%>%unique()

      tbl <- tibble::tibble()
      if(nrow(tblUSA)>0){tbl <- tbl %>% dplyr::bind_rows(tblUSA %>%
                                                           dplyr::select(commonNames[commonNames %in% names(tblUSA)]))}
      if(nrow(tblUSACogen)>0){tbl <- tbl %>% dplyr::bind_rows(tblUSACogen %>%
                                                                dplyr::select(commonNames[commonNames %in% names(tblUSACogen)]))}
      if(nrow(tblGCAMReg)>0){tbl <- tbl %>% dplyr::bind_rows(tblGCAMReg %>%
                                                               dplyr::select(commonNames[commonNames %in% names(tblGCAMReg)]))}
    } # Check different tblUSA, tblUSACogen and tblGCAMReg

      if(nrow(tbl)>0){
      tbl <- tbl %>%
        dplyr::mutate(class1=dplyr::case_when(class1=="rooftop_pv"~"solar",
                                              TRUE~class1))%>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::mutate(origQuery="origQuery") %>%
        dplyr::group_by(scenario, region, subRegion,    param, sources,class1,class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX) %>%
       dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%
        dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
      tblelecByTechTWh<-tbl}
    }


  # Capacity Calculation based on exogenous cap factors
  paramx<-"elecCapByFuel"
  # Total final energy by aggregate end-use sector
  if(paramx %in% paramsSelectx){
  if(!is.null(tblelecByTechTWh)){
    capfactors <- metis::data_capfactors
    capfactors
        tbl <- tblelecByTechTWh  # Tibble
        #rm(tblelecByTechTWh)
        if (!is.null(regionsSelect)) {
          tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
        }
        tbl <- tbl %>%
          dplyr::full_join(capfactors, by="class1")%>%
          dplyr::mutate(param = "elecCapByFuel",
                        gcamCapacityFactor=cf1971to2100,
                        value = value*1000/(8760*gcamCapacityFactor),
                        origValue = value,
                        units = "Electricity Capacity by Fuel (GW)",
                        origUnits = units) %>%
          dplyr::filter(!is.na(value))%>%
          dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origValue, origUnits, origX)

        datax <- dplyr::bind_rows(datax, tbl)


  } else {
  if("elec gen by gen tech and cooling tech" %in% queriesSelectx){
    #print(paste("elecByTechTWh did not run so skipping param elecCapByFuel."))
    }
  }}

  # metis.chart(tbl,xData="x",yData="value",useNewLabels = 0)


   paramx <- "watConsumBySec"
  if(paramx %in% paramsSelectx){
    # water consumption by sector
    queryx <- "water consumption by state, sector, basin (includes desal)"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if(nrow(tbl)>0){
        # If GCAM USA then remove "USA" region and use states
        # if(any(metis.assumptions("US52") %in% unique(tbl$region))){
        #   tbl <- tbl %>% dplyr::filter(region!="USA") # Remove region USA and use states instead
        # }
      }
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::filter(scenario %in% scenOrigNames)%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(sector=dplyr::case_when(
          grepl("desalination",technology,ignore.case=T)~"desalination",
          sector=="water_td_an_C"~"animal",
          sector=="water_td_dom_C"~"domestic",
          sector=="water_td_elec_C"~"electric",
          sector=="water_td_ind_C"~"industry",
          sector=="water_td_pri_C"~"primary",
          grepl("_irr_",sector)~"irrigation",
          TRUE~sector)) %>%
        dplyr::mutate(param = "watConsumBySec",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year, subRegion=region,
                      scenario = scenNewNames,
                      value = value,
                      units = "Water Consumption by Sector (km3)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = sector,
                      classLabel1 = "Sector",
                      classPalette1 = "pal_metis",
                      class2 = subsector,
                      classLabel2 = "basin",
                      classPalette2 = "pal_metis")%>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}


  paramx<- "watWithdrawBySec"
  if(paramx %in% paramsSelectx){
    # water consumption by sector
    queryx <- "water withdrawals by state, sector, basin (includes desal)"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      # if(nrow(tbl)>0){
      #   # If GCAM USA then remove "USA" region and use states
      #   if(any(metis.assumptions("US52") %in% unique(tbl$region))){
      #     tbl <- tbl %>% dplyr::filter(region!="USA") # Remove region USA and use states instead
      #   }
      # }
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::filter(scenario %in% scenOrigNames)%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(sector=dplyr::case_when(
          grepl("desalination",technology,ignore.case=T)~"desalination",
          sector=="water_td_an_W"~"livestock",
          sector=="water_td_dom_W"~"municipal",
          sector=="water_td_elec_W"~"electricity",
          sector=="water_td_ind_W"~"industry",
          sector=="water_td_pri_W"~"mining",
          grepl("_irr_",sector)~"agriculture",
          TRUE~sector)) %>%
        dplyr::mutate(param = "watWithdrawBySec",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year, subRegion=region,
                      scenario = scenNewNames,
                      value = value,
                      units = "Water Withdrawal by Sector (km3)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = sector,
                      classLabel1 = "Sector",
                      classPalette1 = "pal_wet",
                      class2 = subsector,
                      classLabel2 = "basin",
                      classPalette2 = "pal_metis")%>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}


  paramx <- "watWithdrawByCrop"
  if(paramx %in% paramsSelectx){
    # water withdrawals by sector
    queryx <- "water withdrawals by crop"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      # Need to add in conveyance losses for USA when running GCAM USA
      # gcamusa.CONVEYANCE_LOSSES <- 0.829937455747218 from constants.R
      if(nrow(tbl)>0){
        if(any(unique(tbl$region) %in% metis.assumptions("US52"))){
          tbl <- tbl %>%
            dplyr::mutate(value = dplyr::case_when(region=="USA"~value/0.829937455747218,
                                            TRUE~value)) %>%
            dplyr::filter(!region %in% metis.assumptions("US52"))
        }
      }
      if (!is.null(regionsSelect)) {
        if(any(regionsSelect %in% metis.assumptions("US52"))){
          tbl <- tbl %>% dplyr::filter(region %in% c(regionsSelect,"USA"))
        } else {
          tbl <- tbl %>% dplyr::filter(region %in% c(regionsSelect))
        }
      }
      tbl <- tbl %>%
        dplyr::filter(sector!="industry", sector!="mining" , sector!="municipal"
                      , sector!="electricity" , sector!="livestock", !grepl("water_td_",sector))%>%
        dplyr::filter(scenario %in% scenOrigNames)%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "watWithdrawByCrop",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year, subRegion=region,
                      scenario = scenNewNames,
                      value = value,
                      units = "Water Withdrawals by Crop (km3)",
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
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
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
        dplyr::filter(scenario %in% scenOrigNames)%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "watBioPhysCons",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year, subRegion=region,
                      scenario = scenNewNames,
                      value = value,
                      units = "Biophysical Water Consumption (km3)",
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
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
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
        dplyr::filter(scenario %in% scenOrigNames)%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "watIrrWithdrawBasin",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year, subRegion=region,
                      scenario = scenNewNames,
                      value = value,
                      units = "Irrigation Water Withdrawal (km3)",
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
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
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
        dplyr::filter(scenario %in% scenOrigNames)%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "watIrrConsBasin",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year, subRegion=region,
                      scenario = scenNewNames,
                      value = value,
                      units = "Irrigation Water Consumption (km3)",
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
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}


  paramx <- "watSupRunoffBasin"
  if(paramx %in% paramsSelectx){
    # water consumption by water mapping source
    queryx <- "Basin level available runoff"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::mutate(subRegion=gsub(" ","_",gsub("\\_.*","",basin)))%>%
        dplyr::select(-basin)%>%
        dplyr::filter(scenario %in% scenOrigNames)%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "watSupRunoffBasin",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year,
                      scenario = scenNewNames,
                      value = value,
                      units = "Runoff (km3)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1="class1",
                      classLabel1 = "runoff",
                      classPalette1 = "pal_wet",
                      class2 = "class2",
                      classLabel2 = "classLabel2",
                      classPalette2 = "pal_wet") %>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}


  paramx <- "waterWithdrawROGW"
  if(paramx %in% paramsSelectx){
    # water consumption by water mapping source
    queryx <- "Water withdrawals by water source (runoff vs. groundwater)"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        if(any(regionsSelect %in% metis.assumptions("US52"))){
          tbl <- tbl %>% dplyr::filter(region %in% c(regionsSelect,"USA"))
        } else {
          tbl <- tbl %>% dplyr::filter(region %in% c(regionsSelect))
        }
      }
      tbl <- tbl %>%
        dplyr::mutate(subRegion=gsub(" ","_",gsub("\\_.*","",resource)))%>%
        dplyr::select(-resource)%>%
        dplyr::filter(scenario %in% scenOrigNames)%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "waterWithdrawROGW",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year,
                      scenario = scenNewNames,
                      value = value,
                      units = "km3",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1=subresource,
                      classLabel1 = "source",
                      classPalette1 = "pal_metis",
                      class2 = "class2",
                      classLabel2 = "classLabel2",
                      classPalette2 = "pal_metis") %>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
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
        dplyr::filter(scenario %in% scenOrigNames)%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "gdpPerCapita",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year, subRegion=region,
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
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
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
        dplyr::filter(scenario %in% scenOrigNames)%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "gdp",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year, subRegion=region,
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
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
      tblgdp<-tbl
    } else {
      # if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
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
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
     # print(paste("Paramater 'GDP MER by region' not found in database, so cannot calculate" ,queryx, sep = ""))
    }}

  paramx <- "livestock_MeatDairybyTechMixed"
  if(paramx %in% paramsSelectx){
    # Population
    queryx <- "meat and dairy production by tech"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::filter(scenario %in% scenOrigNames)%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::filter(subsector=="Mixed")%>% # "Mixed"    "Pastoral" "Imports"
        dplyr::mutate(param = "livestock_MeatDairybyTechMixed",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year, subRegion=region,
                      scenario = scenNewNames,
                      value = value,
                      units = "Livestock Production Mixed Feed (Mt)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = sector,
                      classLabel1 = "sector",
                      classPalette1 = "pal_metis",
                      class2 = technology,
                      classLabel2 = "technology",
                      classPalette2 = "pal_metis") %>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}


  paramx <- "livestock_MeatDairybySubsector"
  if(paramx %in% paramsSelectx){
    # Population
    queryx <- "meat and dairy production by tech"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::filter(scenario %in% scenOrigNames)%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "livestock_MeatDairybySubsector",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year, subRegion=region,
                      scenario = scenNewNames,
                      value = value,
                      units = "Livestock Production (Mt)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = sector,
                      classLabel1 = "sector",
                      classPalette1 = "pal_metis",
                      class2 = subsector,
                      classLabel2 = "subsector",
                      classPalette2 = "pal_metis") %>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}



  paramx <- "livestock_MeatDairybyTechPastoral"
  if(paramx %in% paramsSelectx){
    # Population
    queryx <- "meat and dairy production by tech"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::filter(scenario %in% scenOrigNames)%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::filter(subsector=="Pastoral")%>% # "Mixed"    "Pastoral" "Imports"
        dplyr::mutate(param = "livestock_MeatDairybyTechPastoral",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year, subRegion=region,
                      scenario = scenNewNames,
                      value = value,
                      units = "Livestock Production Pastoral (Mt)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = sector,
                      classLabel1 = "sector",
                      classPalette1 = "pal_metis",
                      class2 = technology,
                      classLabel2 = "technology",
                      classPalette2 = "pal_metis") %>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}

  paramx <- "livestock_MeatDairybyTechImports"
  if(paramx %in% paramsSelectx){
    # Population
    queryx <- "meat and dairy production by tech"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::filter(scenario %in% scenOrigNames)%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::filter(subsector=="Imports")%>% # "Mixed"    "Pastoral" "Imports"
        dplyr::mutate(param = "livestock_MeatDairybyTechImports",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year, subRegion=region,
                      scenario = scenNewNames,
                      value = value,
                      units = "Livestock Production Imported Feed (Mt)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = sector,
                      classLabel1 = "sector",
                      classPalette1 = "pal_metis",
                      class2 = technology,
                      classLabel2 = "technology",
                      classPalette2 = "pal_metis") %>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
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
        dplyr::filter(scenario %in% scenOrigNames)%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "pop",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year, subRegion=region,
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
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}

  paramx <- "agProdbyIrrRfd"
  if(paramx %in% paramsSelectx){
    # Ag production by tech
    queryx <- "ag production by tech"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        if(any(regionsSelect %in% metis.assumptions("US52"))){
          tbl <- tbl %>% dplyr::filter(region %in% c(regionsSelect,"USA"))
        } else {
          tbl <- tbl %>% dplyr::filter(region %in% c(regionsSelect))
        }
      }
      tbl <- tbl %>%
        dplyr::filter(Units=="Mt")%>%
        dplyr::filter(scenario %in% scenOrigNames)%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "agProdbyIrrRfd",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origUnits = Units,
                      origX = year,
                      subRegion=gsub(".*_","",subsector),
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
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}

  paramx <- "agProdBiomass"
  if(paramx %in% paramsSelectx){
    # Ag Production by Crop Type Biomass EJ
    queryx <- "ag production by tech"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        if(any(regionsSelect %in% metis.assumptions("US52"))){
          tbl <- tbl %>% dplyr::filter(region %in% c(regionsSelect,"USA"))
        } else {
          tbl <- tbl %>% dplyr::filter(region %in% c(regionsSelect))
        }
      }
      tbl <- tbl %>%
        dplyr::filter(Units=="EJ",sector==output)%>%
        dplyr::filter(scenario %in% scenOrigNames)%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "agProdBiomass",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year,
                      scenario = scenNewNames,
                      units = "Biomass Production (EJ)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = sector,
                      classLabel1 = "Crop",
                      classPalette1 = "pal_metis",
                      subRegion=gsub(".*_","",subsector),
                      class2 = gsub("_.*RFD","_RFD",technology),
                      class2 = gsub("_.*IRR","_IRR",class2),
                      classLabel2 = "Detail",
                      classPalette2 = "pal_metis")%>%
        dplyr::select(origScen,origQuery, origValue, origUnits, origX, region, subRegion,    param, scenario,
                      value, units, vintage, x, xLabel, aggregate, class1, classLabel1, classPalette1,
                      class2, classLabel2, classPalette2)%>%dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}

  paramx <- "agProdForest"
  if(paramx %in% paramsSelectx){
    # Ag Production by Crop Type Forest
    queryx <- "ag production by tech"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        if(any(regionsSelect %in% metis.assumptions("US52"))){
          tbl <- tbl %>% dplyr::filter(region %in% c(regionsSelect,"USA"))
        } else {
          tbl <- tbl %>% dplyr::filter(region %in% c(regionsSelect))
        }
      }
      tbl <- tbl %>%
        dplyr::filter(Units=="billion m3",sector==output)%>%
        dplyr::filter(scenario %in% scenOrigNames)%>%
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
                      units = "Ag Production (billion m3)",
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
        dplyr::select(origScen,origQuery, origValue, origUnits, origX, region, subRegion,    param, scenario,
                      value, units, vintage, x, xLabel, aggregate, class1, classLabel1, classPalette1,
                      class2, classLabel2, classPalette2)%>%dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}

  paramx <- "agProdByCrop"
  if(paramx %in% paramsSelectx){
    # Ag Production by Crop Type
    queryx <- "ag production by tech"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        if(any(regionsSelect %in% metis.assumptions("US52"))){
          tbl <- tbl %>% dplyr::filter(region %in% c(regionsSelect,"USA"))
        } else {
          tbl <- tbl %>% dplyr::filter(region %in% c(regionsSelect))
        }
      }
      tbl <- tbl %>%
        dplyr::filter(Units=="Mt",sector==output, sector!="Pasture")%>%
        dplyr::filter(scenario %in% scenOrigNames)%>%
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
                      units = "Ag Production (Mt)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = sector,
                      classLabel1 = "Crop",
                      classPalette1 = "pal_metis",
                      subRegion=gsub(".*_","",subsector),
                      class2 = gsub("_.*RFD","_RFD",technology),
                      class2 = gsub("_.*IRR","_IRR",class2),
                      classLabel2 = "Detail",
                      classPalette2 = "pal_metis")%>%
        dplyr::select(origScen,origQuery, origValue, origUnits, origX, region, subRegion,    param, scenario,
                      value, units, vintage, x, xLabel, aggregate, class1, classLabel1, classPalette1,
                      class2, classLabel2, classPalette2)%>%dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
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
        dplyr::filter(scenario %in% scenOrigNames)%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "landIrrRfd",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year, subRegion=region,
                      scenario = scenNewNames,
                      value = value,
                      units = "Irr vs Rfd Land (1000 km2)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = water,
                      classLabel1 = "Water Source",
                      classPalette1 = "pal_metis",
                      class2 = crop,
                      classLabel2 = "crop",
                      classPalette2 = "pal_metis") %>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}

  paramx <- "landIrrCrop"
  if(paramx %in% paramsSelectx){
    # land allocation by crop and water source
    queryx <- "land allocation by crop and water source"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::filter(!is.na(water),water=="IRR")%>%
        dplyr::filter(scenario %in% scenOrigNames)%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "landIrrCrop",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year, subRegion=region,
                      scenario = scenNewNames,
                      value = value,
                      units = "Irr Crop Land (1000 km2)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = crop,
                      classLabel1 = "crop",
                      classPalette1 = "pal_metis",
                      class2 = "class2",
                      classLabel2 = "class2",
                      classPalette2 = "pal_metis") %>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}

  paramx <- "landRfdCrop"
  if(paramx %in% paramsSelectx){
    # land allocation by crop and water source
    queryx <- "land allocation by crop and water source"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::filter(!is.na(water),water=="RFD")%>%
        dplyr::filter(scenario %in% scenOrigNames)%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "landRfdCrop",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year, subRegion=region,
                      scenario = scenNewNames,
                      value = value,
                      units = "Rainfed Crop Land (1000 km2)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = crop,
                      classLabel1 = "crop",
                      classPalette1 = "pal_metis",
                      class2 = "class2",
                      classLabel2 = "class2",
                      classPalette2 = "pal_metis") %>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
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
        dplyr::filter(scenario %in% scenOrigNames)%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "landAlloc",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year, subRegion=region,
                      scenario = scenNewNames,
                      value = value,
                      units = "Land Allocation (1000 km2)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = landleaf,
                      classLabel1 = "Land Type",
                      classPalette1 = "pal_metis",
                      classLabel2 = "classLabel2",
                      classPalette2 = "classPalette2") %>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}

  paramx <- "inputs"
  if(paramx %in% paramsSelectx){
    queryx <- "inputs by tech"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::mutate(
          subRegion=region,
          class1=input,
          class2=sector)%>%
        dplyr::filter(scenario %in% scenOrigNames)%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "inputs",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year,
                      scenario = scenNewNames,
                      value = value,
                      units = Units,
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      classLabel1 = "input",
                      classPalette1 = "pal_metis",
                      classLabel2 = "sector",
                      classPalette2 = "pal_metis") %>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}

  paramx <- "outputs"
  if(paramx %in% paramsSelectx){
    queryx <- "outputs by tech"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::mutate(
          subRegion=region,
          class1=output,
          class2=sector)%>%
        dplyr::filter(scenario %in% scenOrigNames)%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "outputs",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year,
                      scenario = scenNewNames,
                      value = value,
                      units = Units,
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      classLabel1 = "output",
                      classPalette1 = "pal_metis",
                      classLabel2 = "sector",
                      classPalette2 = "pal_metis") %>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}


  paramx <- "landAllocDetail"
  if(paramx %in% paramsSelectx){
    # aggregated land allocation
    queryx <- "detailed land allocation"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::mutate(
          subRegion=gsub("^[^_]*_","",landleaf),
          subRegion=gsub("_.*","",subRegion),
          class1=gsub("_.*","",landleaf),
          class2=gsub("^[^_]*_[^_]*_","",landleaf))%>%
        dplyr::filter(scenario %in% scenOrigNames)%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "landAllocDetail",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year,
                      scenario = scenNewNames,
                      value = value,
                      units = "Land Allocation (1000 km2)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      classLabel1 = "Land Type",
                      classPalette1 = "pal_metis",
                      classLabel2 = "Source",
                      classPalette2 = "pal_metis") %>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
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
        dplyr::filter(scenario %in% scenOrigNames)%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "landAllocByCrop",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year, subRegion=region,
                      scenario = scenNewNames,
                      value = value,
                      units = "Crop Land (1000 km2)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = landleaf,
                      classLabel1 = "Land Type",
                      classPalette1 = "pal_metis",
                      classLabel2 = "classLabel2",
                      classPalette2 = "classPalette2") %>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
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
        dplyr::left_join(metis.assumptions("convertGgTgMTC"),by="Units") %>%
        dplyr::mutate(origValue=value,value=value*Convert*44/12,
                      origUnits=Units,units="Emissions LUC - (MTCO2eq)")%>%
        dplyr::filter(scenario %in% scenOrigNames)%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "emissLUC",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origUnits = Units,
                      origX = year, subRegion=region,
                      scenario = scenNewNames,
                      value = value,
                      origValue = origValue,
                      units = "LUC CO2 Emissions (MTCO2eq)",
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
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value)) %>%
        dplyr::mutate(class1='LUC')
      tblLUEmiss<-tbl
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
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
      dplyr::left_join(metis.assumptions("convertGgTgMTC"),by="Units") %>%
      dplyr::mutate(origValue=value,value=value*Convert*44/12,
                    origUnits=Units,units="CO2 Emissions by Sector (MTCO2eq)")%>%
      dplyr::mutate(
        class1=sector,
        class2=sector) %>%
      #dplyr::left_join(emiss_sector_mapping, by=c('class1')) %>%
      #dplyr::mutate(class1=agg_sector) %>%
      #dplyr::select(-agg_sector) %>%
      dplyr::mutate(
        class1=dplyr::case_when(
          grepl("refining",class1,ignore.case=T)~"refining",
          grepl("regional biomass|regional biomassOil|regional corn for ethanol|biomass" ,class1,ignore.case=T)~"biomass",
          grepl("trn_",class1,ignore.case=T)~"transport",
          grepl("comm |resid ",class1,ignore.case=T)~"building",
          grepl("electricity|elec_|electricity |csp_backup",class1,ignore.case=T)~"electricity",
          grepl("H2",class1,ignore.case=T)~"hydrogen",
          grepl("cement|N fertilizer|industrial|ind ",class1,ignore.case=T)~"industry",
          grepl("gas pipeline|gas processing|unconventional oil production|gas to liquids",class1,ignore.case=T)~"industry",
          grepl("Beef|Dairy|Pork|Poultry",class1,ignore.case=T)~"livestock",
          grepl("FiberCrop|MiscCrop|OilCrop|OtherGrain|PalmFruit|Corn|Rice|Root_Tuber|SheepGoat|SugarCrop|UnmanagedLand|Wheat|FodderGrass|FodderHerb",class1,ignore.case=T)~"crops",
          TRUE~class1))%>%
      dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
      dplyr::mutate(param = "emissCO2BySectorNoBio",
                    sources = "Sources",
                    origScen = scenario,
                    origQuery = queryx,
                    origX = year, subRegion=region,
                    scenario = scenNewNames,
                    vintage = paste("Vint_", year, sep = ""),
                    x = year,
                    xLabel = "Year",
                    aggregate = "sum",
                    classLabel1 = "sector",
                    classPalette1 = "pal_metis",
                    classLabel2 = "sectorDetail",
                    classPalette2 = "pal_metis") %>%
      dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                    aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                    origScen, origQuery, origValue, origUnits, origX)%>%
      dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
      dplyr::filter(!is.na(value))
    datax <- dplyr::bind_rows(datax, tbl)}
  }


  paramx <- "emissNonCO2BySectorGWPAR5"
  if(paramx %in% paramsSelectx){

    # GHG emissions (non CO2) by subsector, using AR5 GWP values
    queryx <- "nonCO2 emissions by sector"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      #emiss_sector_mapping <- read.csv(CO2mappingFile, skip=1)
      tblCore <- tbl %>%
        dplyr::filter(ghg!="CO2")%>%
        dplyr::filter(scenario %in% scenOrigNames)%>%
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
          class2=dplyr::case_when(
            grepl("refining",class2,ignore.case=T)~"refining",
            grepl("regional biomass|regional biomassOil|regional corn for ethanol|biomass" ,class2,ignore.case=T)~"biomass",
            grepl("trn_",class2,ignore.case=T)~"transport",
            grepl("comm |resid ",class2,ignore.case=T)~"building",
            grepl("electricity|elec_|electricity |csp_backup",class2,ignore.case=T)~"electricity",
            grepl("H2",class2,ignore.case=T)~"hydrogen",
            grepl("cement|N fertilizer|industrial|ind ",class2,ignore.case=T)~"industry",
            grepl("gas pipeline|gas processing|unconventional oil production|gas to liquids",class2,ignore.case=T)~"industry",
            grepl("Beef|Dairy|Pork|Poultry",class2,ignore.case=T)~"livestock",
            grepl("FiberCrop|MiscCrop|OilCrop|OtherGrain|PalmFruit|Corn|Rice|Root_Tuber|SheepGoat|SugarCrop|UnmanagedLand|Wheat|FodderGrass|FodderHerb",class2,ignore.case=T)~"crops",
            TRUE~class2))%>%
        dplyr::left_join(metis.assumptions("GWP")%>%dplyr::rename(class1=ghg),by="class1")%>%
        dplyr::left_join(metis.assumptions("convertGgTgMTC"),by="Units") %>%
        #dplyr::filter(!class1=='CO2') %>%
        dplyr::mutate(origValue=value,
                      value=value*GWPAR5*Convert,
                      origUnits=Units,
                      origUnits = dplyr::case_when(class1=="Other"~"Units",TRUE~origUnits),
                      units="GHG Emissions GWPAR5 (MTCO2eq)")%>%
        dplyr::mutate(param = "emissNonCO2BySectorGWPAR5",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origX = year, subRegion=region,
                      scenario = scenNewNames,
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      classLabel1 = "GHG",
                      classPalette1 = "pal_metis",
                      classLabel2 = "sector",
                      classPalette2 = "pal_metis") %>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
    } else {
      tblCore <- tibble::tibble()
      # if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }

    # GHG emissions (non CO2) by subsector, using AR5 GWP values USA
    queryx <- "nonCO2 emissions by sector USA"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      #emiss_sector_mapping <- read.csv(CO2mappingFile, skip=1)
      tblUSA <- tbl %>%
        dplyr::filter(ghg!="CO2")%>%
        dplyr::filter(scenario %in% scenOrigNames)%>%
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
        dplyr::filter(!class1 %in% c('SO2', 'NH3', 'Other'),
                      !(class1 == "SF6" & grepl("grid",region,ignore.case = T)),
                      !(class1 %in% c("CH4","HFCs","N2O") & region=="USA")) %>%
        #dplyr::left_join(emiss_sector_mapping, by=c('class2' = 'class1')) %>%
        #dplyr::mutate(class2=agg_sector) %>%
        #dplyr::select(-agg_sector) %>%
        dplyr::mutate(
          class2=dplyr::case_when(
            grepl("refining",class2,ignore.case=T)~"refining",
            grepl("regional biomass|regional biomassOil|regional corn for ethanol|biomass" ,class2,ignore.case=T)~"biomass",
            grepl("trn_",class2,ignore.case=T)~"transport",
            grepl("comm |resid ",class2,ignore.case=T)~"building",
            grepl("electricity|elec_|electricity |csp_backup",class2,ignore.case=T)~"electricity",
            grepl("H2",class2,ignore.case=T)~"hydrogen",
            grepl("cement|N fertilizer|industrial|ind ",class2,ignore.case=T)~"industry",
            grepl("gas pipeline|gas processing|unconventional oil production|gas to liquids",class2,ignore.case=T)~"industry",
            grepl("Beef|Dairy|Pork|Poultry",class2,ignore.case=T)~"livestock",
            grepl("FiberCrop|MiscCrop|OilCrop|OtherGrain|PalmFruit|Corn|Rice|Root_Tuber|SheepGoat|SugarCrop|UnmanagedLand|Wheat|FodderGrass|FodderHerb",class2,ignore.case=T)~"crops",
            TRUE~class2))%>%
        dplyr::left_join(metis.assumptions("GWP")%>%dplyr::rename(class1=ghg),by="class1")%>%
        dplyr::left_join(metis.assumptions("convertGgTgMTC"),by="Units") %>%
        #dplyr::filter(!class1=='CO2') %>%
        dplyr::mutate(origValue=value,
                      value=value*GWPAR5*Convert,
                      origUnits=Units,
                      origUnits = dplyr::case_when(class1=="Other"~"Units",TRUE~origUnits),
                      units="GHG Emissions GWPAR5 (MTCO2eq)")%>%
        dplyr::mutate(param = "emissNonCO2BySectorGWPAR5",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origX = year, subRegion=region,
                      scenario = scenNewNames,
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      classLabel1 = "GHG",
                      classPalette1 = "pal_metis",
                      classLabel2 = "sector",
                      classPalette2 = "pal_metis") %>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
    } else {
      tblUSA <- tibble::tibble()
      # if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }

    # Combine Core and USA
    if(nrow(tblCore)>0){scenariosCore <- unique(tblCore$scenario)}else{
      scenariosCore <- NULL
    }

    tblUSA <- tblUSA %>%
      dplyr::filter(!scenario %in% scenariosCore)

    datax <- dplyr::bind_rows(datax, tblCore, tblUSA)

  }


  paramx <- "emissNonCO2BySector"
  if(paramx %in% paramsSelectx){

    # GHG emissions (non CO2) by subsector values
    queryx <- "nonCO2 emissions by sector"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      tblCore <- tbl %>%
        dplyr::filter(ghg!="CO2")%>%
        dplyr::filter(scenario %in% scenOrigNames)%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(class1=ghg, class2=sector) %>%
        dplyr::mutate(origValue=value,
                      value=value,
                      origUnits=Units,
                      units=Units)%>%
        dplyr::mutate(param = "emissNonCO2BySector",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origX = year, subRegion=region,
                      scenario = scenNewNames,
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      classLabel1 = "GHG",
                      class1 = paste0(class1," (",units,")"),
                      classPalette1 = "pal_metis",
                      classLabel2 = "sector",
                      classPalette2 = "pal_metis") %>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
    } else {
      tblCore <- tibble::tibble()
      # if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }

    # GHG emissions (non CO2) by subsector values USA
    queryx <- "nonCO2 emissions by sector USA"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      tblUSA <- tbl %>%
        dplyr::filter(ghg!="CO2")%>%
        dplyr::filter(scenario %in% scenOrigNames)%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(class1=ghg, class2=sector) %>%
        dplyr::mutate(origValue=value,
                      value=value,
                      origUnits=Units,
                      units=Units)%>%
        dplyr::mutate(param = "emissNonCO2BySector",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origX = year, subRegion=region,
                      scenario = scenNewNames,
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      classLabel1 = "GHG",
                      classPalette1 = "pal_metis",
                      classLabel2 = "sector",
                      classPalette2 = "pal_metis",
                      class1 = paste0(class1," (",units,")")) %>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
    } else {
      tblUSA <- tibble::tibble()
      # if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }

    # GHG emissions (non CO2) by subsector values USA
    queryx <- "nonCO2 emissions by sector USA nonUS"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      tblUSAnonUS <- tbl %>%
        dplyr::filter(ghg!="CO2")%>%
        dplyr::filter(scenario %in% scenOrigNames)%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(class1=ghg, class2=sector) %>%
        dplyr::mutate(origValue=value,
                      value=value,
                      origUnits=Units,
                      units=Units)%>%
        dplyr::mutate(param = "emissNonCO2BySector",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origX = year, subRegion=region,
                      scenario = scenNewNames,
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      classLabel1 = "GHG",
                      class1 = paste0(class1," (",units,")"),
                      classPalette1 = "pal_metis",
                      classLabel2 = "sector",
                      classPalette2 = "pal_metis") %>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
    } else {
      tblUSAnonUS <- tibble::tibble()
      # if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }

    # Combine Core and USA
    if(nrow(tblCore)>0){scenariosCore <- unique(tblCore$scenario)}else{
      scenariosCore <- NULL
    }

    tblUSA <- tblUSA %>%
      dplyr::filter(!scenario %in% scenariosCore)

    tblUSAnonUS <- tblUSAnonUS %>%
      dplyr::filter(!scenario %in% scenariosCore)

    datax <- dplyr::bind_rows(datax, tblCore, tblUSA, tblUSAnonUS)

  }


  if(any(c("emissCO2BySector","emissCO2CumGlobal2010to2100","emissCO2CumGlobal2010to2100RCP") %in% paramsSelectx)){

    paramx <- "emissCO2BySector"
    # GHG emissions (non CO2) by subsector, using AR5 GWP values
    queryx <- "CO2 emissions by sector"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      # if (!is.null(regionsSelect)) {
      #   tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      # }
      #emiss_sector_mapping <- read.csv(CO2mappingFile, skip=1)
      tbl <- tbl %>%
        dplyr::filter(scenario %in% scenOrigNames)%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(class1=sector, class2="class2") %>%
        dplyr::mutate(origValue=value,
                      origUnits=Units,
                      units="CO2 (MTC)")%>%
        dplyr::mutate(param = "emissCO2BySector",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origX = year, subRegion=region,
                      scenario = scenNewNames,
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      classLabel1 = "sector",
                      classPalette1 = "pal_metis",
                      classLabel2 = "classLabel2",
                      classPalette2 = "pal_metis") %>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)

      # Add Global Emissions
      tblGlobal <- tbl %>%
        dplyr::mutate(region="Global", subRegion="Global") %>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      # For selected region
      if (!is.null(regionsSelect)) {
        tblSelect <- tbl %>% dplyr::filter(region %in% regionsSelect)  %>%
          dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                          aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                          origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
          dplyr::filter(!is.na(value))
      } else {
        tblSelect <- tbl %>%
          dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                          aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                          origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
          dplyr::filter(!is.na(value))}

      tblx <- tblGlobal %>%
        dplyr::bind_rows(tblSelect)

      # Cumulative Global Emissions
      # Expand data to include years
      tblxexpand <- tblx %>%
        dplyr::select(scenario,region,subRegion,units,x,value)%>%
        dplyr::group_by(scenario,region,subRegion,units,x) %>% dplyr::summarize_at(dplyr::vars("value"),list(~sum(.,na.rm = T)))%>%
        tidyr::complete(x=seq(max(2010,min(tblx$x)),max(tblx$x),1),tidyr::nesting(scenario,region,subRegion,units)) %>% dplyr::ungroup()%>%
        dplyr::mutate(value=zoo::na.approx(value,na.rm =FALSE)) %>%
        dplyr::arrange(x) %>%
        dplyr::filter(!is.na(value));  tblxexpand

      tblxexpandcum2010to2100 <- tblxexpand %>%
        filter(x > 2005) %>%
        dplyr::group_by(scenario,region,subRegion,units) %>%
        dplyr::mutate(value=cumsum(value)/1000,
                      units = gsub("MTC","GTC",units),
                      param = "emissCO2CumGlobal2010to2100",
                      sources = "sources",
                      class1 = "class1",
                      class2 = "class2",
                      xLabel = "year",
                      vintage = paste0("vint_",x),
                      aggregate = "sum",
                      classLabel1 = "classLabel1",
                      classPalette1 = "pal_hot",
                      classLabel2 = "classLabel2",
                      classPalette2 = "pal_hot",
                      origScen = scenario,
                      origQuery = queryx,
                      origUnits = units,
                      origX = x,
                      origValue = value); tblxexpandcum2010to2100



      # Comapre CO2 Emissions between core and GCAM-USA Global and the RCP ranges
      # CMIP5 Table SPM.3, pg 27 https://www.ipcc.ch/site/assets/uploads/2018/02/WG1AR5_SPM_FINAL.pdf

      replen <- length(unique(tblxexpand$x))

      rcpranges <- data.frame(scenario=rep("rcpGlobalCumEmissions",replen),
                              class1 = rep("rcp8.5",replen),
                              class2 = rep("max",replen),
                              x = unique(tblxexpand$x),
                              value = rep(1910,replen)) %>%
        dplyr::bind_rows(data.frame(scenario=rep("rcpGlobalCumEmissions",replen),
                                    class1 = rep("rcp8.5",replen),
                                    class2 = rep("min",replen),
                                    x = unique(tblxexpand$x),
                                    value = rep(1415,replen))) %>%
        dplyr::bind_rows(data.frame(scenario=rep("rcpGlobalCumEmissions",replen),
                                    class1 = rep("rcp6.0",replen),
                                    class2 = rep("max",replen),
                                    x = unique(tblxexpand$x),
                                    value = rep(1250,replen))) %>%
        dplyr::bind_rows(data.frame(scenario=rep("rcpGlobalCumEmissions",replen),
                                    class1 = rep("rcp6.0",replen),
                                    class2 = rep("min",replen),
                                    x = unique(tblxexpand$x),
                                    value = rep(840,replen))) %>%
        dplyr::bind_rows(data.frame(scenario=rep("rcpGlobalCumEmissions",replen),
                                    class1 = rep("rcp4.5",replen),
                                    class2 = rep("max",replen),
                                    x = unique(tblxexpand$x),
                                    value = rep(1005,replen))) %>%
        dplyr::bind_rows(data.frame(scenario=rep("rcpGlobalCumEmissions",replen),
                                    class1 = rep("rcp4.5",replen),
                                    class2 = rep("min",replen),
                                    x = unique(tblxexpand$x),
                                    value = rep(595,replen))) %>%
      dplyr::bind_rows(data.frame(scenario=rep("rcpGlobalCumEmissions",replen),
                                  class1 = rep("rcp2.6",replen),
                                  class2 = rep("max",replen),
                                  x = unique(tblxexpand$x),
                                  value = rep(410,replen))) %>%
        dplyr::bind_rows(data.frame(scenario=rep("rcpGlobalCumEmissions",replen),
                                    class1 = rep("rcp2.6",replen),
                                    class2 = rep("min",replen),
                                    x = unique(tblxexpand$x),
                                    value = rep(140,replen))) %>%
        tibble::as_tibble(); rcpranges

      rcpranges <- rcpranges %>%
        dplyr::mutate(units = "CO2 (GTC)",
                      param = "emissCO2CumGlobal2010to2100RCPs",
                      sources = "sources",
                      xLabel = "year",
                      vintage = paste0("vint_",x),
                      aggregate = "sum",
                      classLabel1 = "rcp",
                      classPalette1 = "pal_hot",
                      classLabel2 = "minmax",
                      classPalette2 = "pal_hot",
                      origScen = scenario,
                      origQuery = queryx,
                      origUnits = units,
                      origX = x,
                      origValue = value,
                      region="Global",
                      subRegion="Global") %>%
        unique(); rcpranges



      datax <- dplyr::bind_rows(datax, tblx, tblxexpandcum2010to2100, rcpranges)
    } else {
      # if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }


    }


 paramx <- "emissMethaneBySourceGWPAR5"
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
        dplyr::filter(scenario %in% scenOrigNames)%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(class1=sector, class2=ghg) %>%
        dplyr::filter(class2 %in% c('CH4', 'CH4_AGR', 'CH4_AWB')) %>%
        dplyr::mutate(
          class1=dplyr::case_when(
            grepl("refining",class1,ignore.case=T)~"refining",
            grepl("regional biomass|regional biomassOil|regional corn for ethanol|biomass" ,class1,ignore.case=T)~"biomass",
            grepl("trn_",class1,ignore.case=T)~"transport",
            grepl("comm |resid ",class1,ignore.case=T)~"building",
            grepl("electricity|elec_|electricity |csp_backup",class1,ignore.case=T)~"electricity",
            grepl("H2",class1,ignore.case=T)~"hydrogen",
            grepl("cement|N fertilizer|industrial|ind ",class1,ignore.case=T)~"industry",
            grepl("gas pipeline|gas processing|unconventional oil production|gas to liquids",class1,ignore.case=T)~"industry",
            grepl("Beef|Dairy|Pork|Poultry",class1,ignore.case=T)~"livestock",
            grepl("FiberCrop|MiscCrop|OilCrop|OtherGrain|PalmFruit|Corn|Rice|Root_Tuber|SheepGoat|SugarCrop|UnmanagedLand|Wheat|FodderGrass|FodderHerb",class1,ignore.case=T)~"crops",
            TRUE~class1))%>%
        dplyr::left_join(metis.assumptions("GWP")%>%dplyr::rename(class2=ghg),by="class2")%>%
        dplyr::left_join(metis.assumptions("convertGgTgMTC"),by="Units") %>%
        dplyr::mutate(origValue=value,
                      value=value*GWPAR5*Convert,
                      origUnits=Units,
                      origUnits = dplyr::case_when(class2=="Other"~"Units",TRUE~origUnits),
                      units="Methane Emissions GWPAR5 (MTCO2eq)")%>%
        dplyr::mutate(param = "emissMethaneBySourceGWPAR5",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origX = year, subRegion=region,
                      scenario = scenNewNames,
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      classLabel1 = "sector",
                      classPalette1 = "pal_metis",
                      classLabel2 = "GHG",
                      classPalette2 = "pal_metis") %>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
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
        dplyr::filter(scenario %in% scenOrigNames)%>%
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
          class2=dplyr::case_when(
          grepl("refining",class2,ignore.case=T)~"refining",
          grepl("regional biomass|regional biomassOil|regional corn for ethanol|biomass" ,class2,ignore.case=T)~"biomass",
          grepl("trn_",class2,ignore.case=T)~"transport",
          grepl("comm |resid ",class2,ignore.case=T)~"building",
          grepl("electricity|elec_|electricity |csp_backup",class2,ignore.case=T)~"electricity",
          grepl("H2",class2,ignore.case=T)~"hydrogen",
          grepl("cement|N fertilizer|industrial|ind ",class2,ignore.case=T)~"industry",
          grepl("gas pipeline|gas processing|unconventional oil production|gas to liquids",class2,ignore.case=T)~"industry",
          grepl("Beef|Dairy|Pork|Poultry",class2,ignore.case=T)~"livestock",
          grepl("FiberCrop|MiscCrop|OilCrop|OtherGrain|PalmFruit|Corn|Rice|Root_Tuber|SheepGoat|SugarCrop|UnmanagedLand|Wheat|FodderGrass|FodderHerb",class2,ignore.case=T)~"crops",
          TRUE~class2))%>%
        dplyr::left_join(metis.assumptions("GWP")%>%dplyr::rename(class1=ghg),by="class1")%>%
        dplyr::left_join(metis.assumptions("convertGgTgMTC"),by="Units") %>%
        dplyr::filter(!class1=='CO2') %>%
        dplyr::mutate(origValue=value,
                      value=value*GWPAR5*Convert,
                      origUnits=Units,
                      origUnits = dplyr::case_when(class1=="Other"~"Units",TRUE~origUnits),
                      units="Non-CO2 Emissions by Resource GWPAR5 (MTCO2eq)")%>%
        dplyr::mutate(param = "emissNonCO2ByResProdGWPAR5",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origX = year, subRegion=region,
                      scenario = scenNewNames,
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      classLabel1 = "GHG",
                      classPalette1 = "pal_metis",
                      classLabel2 = "sector",
                      classPalette2 = "pal_metis") %>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}

# Emissions Fossil FUels and Industry (FFI) basically everything but LUC GWP AR5
  if(any(c("emissNonCO2ByResProdGWPAR5", "emissNonCO2BySectorGWPAR5") %in% unique(datax$param))){
paramx <- "emissBySectorGWPAR5FFI"
  if(paramx %in% paramsSelectx){
    # GHG emissions by resource production, using AR5 GWP values
    totalFFINonCO2 <- datax %>% dplyr::filter(param %in% c("emissNonCO2ByResProdGWPAR5", "emissNonCO2BySectorGWPAR5")) %>%
      dplyr::filter(!class1=='CO2')
    # dplyr::rename so that the nonCO2 classes 1 and 2 are consistent with the CO2 classes 1 and 2
    totalFFINonCO2 <- totalFFINonCO2 %>%
      dplyr::mutate(class_temp = class2) %>%
      dplyr::mutate(class2 = class1) %>%
      dplyr::mutate(class1=class_temp) %>%
      dplyr::select(-class_temp)
    totalFFICO2 <- datax %>% dplyr::filter(param %in% c("emissCO2BySectorNoBio")) %>% dplyr::mutate(class2 = "CO2")
    totalFFICO2Eq <- rbind(totalFFICO2, totalFFINonCO2)
    totalFFICO2Eq$param <- 'emissBySectorGWPAR5FFI'
    totalFFICO2Eq <- totalFFICO2Eq %>%
      dplyr::mutate(origQuery="comb_origQueries",
                    origUnits="comb_origUnits",
                    units="GHG Emissions GWPAR5 (MTCO2eq)",
                    classLabel1 = "sector",
                    classLabel2 = "subSector",
                    classPalette1 = 'pal_metis')%>%
      dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                    aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                    origScen, origQuery, origValue, origUnits, origX)%>%
      dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
      dplyr::filter(!is.na(value))
    # Take this new parameter and put it in datax (main dataframe for ready-to-plot results)
    datax <- rbind(datax, totalFFICO2Eq)
  }} else {
    if(any(c("nonCO2 emissions by resource production","nonCO2 emissions by sector") %in% queriesSelectx)){
      #print(paste("totalFFINonCO2 did not run so skipping param emissBySectorGWPAR5FFI",sep=""))
      }
  }

# Emissions with LUC GWP AR5
  if(any(c("emissNonCO2ByResProdGWPAR5", "emissNonCO2BySectorGWPAR5") %in% unique(datax$param))){
  paramx <- "emissBySectorGWPAR5LUC"
  if(paramx %in% paramsSelectx){
    # Same as FFI Emiss by Sec, except we are now adding LUC. So really it is the whole emissions picture (or close to it)
    totalFFINonCO2 <- datax %>% dplyr::filter(param %in% c("emissNonCO2ByResProdGWPAR5", "emissNonCO2BySectorGWPAR5")) %>%
      dplyr::filter(!class1=='CO2')
    # dplyr::rename so that the nonCO2 classes 1 and 2 are consistent with the CO2 classes 1 and 2
    totalFFINonCO2 <- totalFFINonCO2 %>%
      dplyr::mutate(class_temp = class2) %>%
      dplyr::mutate(class2 = class1) %>%
      dplyr::mutate(class1=class_temp) %>%
      dplyr::select(-class_temp)
    totalFFICO2 <- datax %>% dplyr::filter(param %in% c("emissCO2BySectorNoBio", "emissLUC")) %>% dplyr::mutate(class2 = "CO2")
    totalFFICO2Eq <- rbind(totalFFICO2, totalFFINonCO2)
    totalFFICO2Eq$param <- 'emissBySectorGWPAR5LUC'
    totalFFICO2Eq$Class1Palette <- 'pal_metis'
    totalFFICO2Eq <- totalFFICO2Eq %>%
      dplyr::mutate(origQuery="comb_origQueries",
                    origUnits="comb_origUnits",
                    units="GHG Emissions GWPAR5 (MTCO2eq)",
                    classLabel1 = "sector",
                    classLabel2 = "subSector",
                    classPalette1 = 'pal_metis')%>%
      dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                    aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                    origScen, origQuery, origValue, origUnits, origX)%>%
      dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
      dplyr::filter(!is.na(value))
    # Take this new parameter and put it in datax (main dataframe for ready-to-plot results)
    datax <- rbind(datax, totalFFICO2Eq)
  }} else {
    if(any(c("nonCO2 emissions by resource production","nonCO2 emissions by sector") %in% queriesSelectx)){
    #print(paste("totalFFINonCO2 did not run so skipping param emissBySectorGWPAR5LUC",sep=""))
      }
    }

# Total Emissions without LUC GWP Summarized
  if(any(c("emissNonCO2ByResProdGWPAR5", "emissNonCO2BySectorGWPAR5") %in% unique(datax$param))){
    paramx <- "emissByGasGWPAR5FFI"
  if(paramx %in% paramsSelectx){
    # GHG emissions by resource production, using AR5 GWP values
    totalFFINonCO2 <- datax %>% dplyr::filter(param %in% c("emissNonCO2ByResProdGWPAR5", "emissNonCO2BySectorGWPAR5")) %>%
      dplyr::filter(!class1=='CO2')
    # dplyr::rename so that the nonCO2 classes 1 and 2 are consistent with the CO2 classes 1 and 2
    totalFFICO2 <- datax %>% dplyr::filter(param %in% c("emissCO2BySectorNoBio")) %>% dplyr::mutate(class1 = "CO2")
    totalFFICO2Eq <- rbind(totalFFICO2, totalFFINonCO2)
    totalFFICO2Eq$param <- 'emissByGasGWPAR5FFI'
    totalFFICO2Eq <- totalFFICO2Eq %>%
      dplyr::mutate(origQuery="comb_origQueries",
                    origUnits="comb_origUnits",
                    units="GHG Emissions GWPAR5 (MTCO2eq)",
                    classLabel1 = "sector",
                    classLabel2 = "subSector",
                    classPalette1 = 'pal_metis')%>%
      dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                    aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                    origScen, origQuery, origValue, origUnits, origX)%>%
      dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
      dplyr::filter(!is.na(value))
    # Take this new parameter and put it in datax (main dataframe for ready-to-plot results)
    datax <- rbind(datax, totalFFICO2Eq)
  }
    } else {
    if(any(c("nonCO2 emissions by resource production","nonCO2 emissions by sector") %in% queriesSelectx)){
    #print(paste("totalFFINonCO2 did not run so skipping param emissByGasGWPAR5FFI",sep=""))
      }
    }

# Total Emissions with LUC GWP Summarized

  if(any(c("emissNonCO2ByResProdGWPAR5", "emissNonCO2BySectorGWPAR5",
           "emissLUC","emissCO2BySectorNoBio") %in% unique(datax$param))){
    paramx <- "emissByGasGWPAR5LUC"
  if(paramx %in% paramsSelectx){

    totalFFICO2 <- datax %>% dplyr::filter(param %in% c("emissCO2BySectorNoBio")) %>%
      dplyr::mutate(class1=dplyr::if_else(class1=="LUC", "CO2 LUC", "CO2"))
    # GHG emissions by resource production, using AR5 GWP values
    NonCo2_LUC <- datax %>% dplyr::filter(param %in% c("emissNonCO2ByResProdGWPAR5", "emissNonCO2BySectorGWPAR5",
                                                "emissLUC")) %>%
      dplyr::filter(!class1=='CO2') %>%
      dplyr::mutate(class1=dplyr::if_else(class1=="LUC", "CO2 LUC", class1))
    totalCO2Eq <- rbind(totalFFICO2, NonCo2_LUC)

    # dplyr::rename so that the nonCO2 classes 1 and 2 are consistent with the CO2 classes 1 and 2
    totalCO2Eq$param <- 'emissByGasGWPAR5LUC'
    totalCO2Eq <- totalCO2Eq %>%
      dplyr::mutate(origQuery="comb_origQueries",
                    origUnits="comb_origUnits",
                    units="GHG Emissions GWPAR5 (MTCO2eq)",
                    classLabel1 = "sector",
                    classLabel2 = "subSector",
                    classPalette1 = 'pal_metis')%>%
      dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                    aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                    origScen, origQuery, origValue, origUnits, origX)%>%
      dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
      dplyr::filter(!is.na(value))
    # Take this new parameter and put it in datax (main dataframe for ready-to-plot results)
    datax <- rbind(datax, totalCO2Eq)
  }

  } else {
    if(any(c("nonCO2 emissions by resource production","nonCO2 emissions by sector") %in% queriesSelectx)){
      #print(paste("totalFFINonCO2 did not run so skipping param emissByGasGWPAR5LUC",sep=""))
      }
  }


#-----------------------
# AR5 GTP
#----------------------

  paramx <- "emissNonCO2BySectorGTPAR5"
  if(paramx %in% paramsSelectx){
    # GHG emissions by subsector
    queryx <- "nonCO2 emissions by sector"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::filter(ghg!="CO2")%>%
        dplyr::filter(scenario %in% scenOrigNames)%>%
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
          class2=dplyr::case_when(
          grepl("refining",class2,ignore.case=T)~"refining",
          grepl("regional biomass|regional biomassOil|biomass" ,class2,ignore.case=T)~"biomass",
          grepl("regional corn for ethanol" ,class2,ignore.case=T)~"corn for ethanol",
          grepl("trn_",class2,ignore.case=T)~"transport",
          grepl("comm |resid ",class2,ignore.case=T)~"building",
          grepl("electricity|elec_|electricity |csp_backup",class2,ignore.case=T)~"electricity",
          grepl("H2",class2,ignore.case=T)~"hydrogen",
          grepl("cement|N fertilizer|industrial|ind ",class2,ignore.case=T)~"industry",
          grepl("gas pipeline|gas processing|unconventional oil production|gas to liquids",class2,ignore.case=T)~"industry",
          grepl("Beef|Dairy|Pork|Poultry",class2,ignore.case=T)~"livestock",
          grepl("FiberCrop|MiscCrop|OilCrop|OtherGrain|PalmFruit|Corn|Rice|Root_Tuber|SheepGoat|SugarCrop|UnmanagedLand|Wheat|FodderGrass|FodderHerb",class2,ignore.case=T)~"crops",
          TRUE~class2))%>%
        dplyr::left_join(metis.assumptions("GWP")%>%dplyr::rename(class1=ghg),by="class1")%>%
        dplyr::left_join(metis.assumptions("convertGgTgMTC"),by="Units") %>%
        dplyr::mutate(origValue=value,
                      value=dplyr::case_when(!is.na(GTPAR5) ~ value*GTPAR5*Convert,
                                             TRUE ~  value*GWPAR5*Convert),
                      origUnits=Units,
                      origUnits = dplyr::case_when(class1=="Other"~"Units",TRUE~origUnits),
                      units="GHG Emissions GTPAR5 (MTCO2eq)")%>%
        dplyr::mutate(param = "emissNonCO2BySectorGTPAR5",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origX = year, subRegion=region,
                      scenario = scenNewNames,
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      classLabel1 = "GHG",
                      classPalette1 = "pal_metis",
                      classLabel2 = "sector",
                      classPalette2 = "pal_metis") %>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}

  paramx <- "emissMethaneBySourceGTPAR5"
  if(paramx %in% paramsSelectx){
    # GHG emissions (non CO2) by subsector, using AR5 GTP values
    queryx <- "nonCO2 emissions by sector"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      #emiss_sector_mapping <- read.csv(CO2mappingFile, skip=1)
      tbl <- tbl %>%
        dplyr::filter(scenario %in% scenOrigNames)%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(class1=sector, class2=ghg) %>%
        dplyr::filter(class2 %in% c('CH4', 'CH4_AGR', 'CH4_AWB')) %>%
        dplyr::mutate(
          class1=dplyr::case_when(
            grepl("refining",class1,ignore.case=T)~"refining",
            grepl("regional biomass|regional biomassOil|biomass" ,class1,ignore.case=T)~"biomass",
            grepl("regional corn for ethanol" ,class1,ignore.case=T)~"corn for ethanol",
            grepl("trn_",class1,ignore.case=T)~"transport",
            grepl("comm |resid ",class1,ignore.case=T)~"building",
            grepl("electricity|elec_|electricity |csp_backup",class1,ignore.case=T)~"electricity",
            grepl("H2",class1,ignore.case=T)~"hydrogen",
            grepl("cement|N fertilizer|industrial|ind ",class1,ignore.case=T)~"industry",
            grepl("gas pipeline|gas processing|unconventional oil production|gas to liquids",class1,ignore.case=T)~"industry",
            grepl("Beef|Dairy|Pork|Poultry",class1,ignore.case=T)~"livestock",
            grepl("FiberCrop|MiscCrop|OilCrop|OtherGrain|PalmFruit|Corn|Rice|Root_Tuber|SheepGoat|SugarCrop|UnmanagedLand|Wheat|FodderGrass|FodderHerb",class1,ignore.case=T)~"crops",
            TRUE~class1))%>%
        dplyr::left_join(metis.assumptions("GWP")%>%dplyr::rename(class2=ghg),by="class2")%>%
        dplyr::left_join(metis.assumptions("convertGgTgMTC"),by="Units") %>%
        dplyr::mutate(origValue=value,
                      value=dplyr::case_when(!is.na(GTPAR5) ~ value*GTPAR5*Convert,
                                             TRUE ~  value*GWPAR5*Convert),
                      origUnits=Units,
                      origUnits = dplyr::case_when(class2=="Other"~"Units",TRUE~origUnits),
                      units="Methane Emissions GTPAR5 (MTCO2eq)")%>%
        dplyr::mutate(param = "emissMethaneBySourceGTPAR5",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origX = year, subRegion=region,
                      scenario = scenNewNames,
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      classLabel1 = "sector",
                      classPalette1 = "pal_metis",
                      classLabel2 = "GHG",
                      classPalette2 = "pal_metis") %>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}

  paramx <- "emissNonCO2ByResProdGTPAR5"
  if(paramx %in% paramsSelectx){
    # GHG emissions by resource production, using AR5 GTP values
    queryx <- "nonCO2 emissions by resource production"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      #emiss_sector_mapping <- read.csv(CO2mappingFile, skip=1)
      tbl <- tbl %>%
        dplyr::filter(scenario %in% scenOrigNames)%>%
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
          class2=dplyr::case_when(
          grepl("refining",class2,ignore.case=T)~"refining",
          grepl("regional biomass|regional biomassOil|biomass" ,class2,ignore.case=T)~"biomass",
          grepl("regional corn for ethanol" ,class2,ignore.case=T)~"corn for ethanol",
          grepl("trn_",class2,ignore.case=T)~"transport",
          grepl("comm |resid ",class2,ignore.case=T)~"building",
          grepl("electricity|elec_|electricity |csp_backup",class2,ignore.case=T)~"electricity",
          grepl("H2",class2,ignore.case=T)~"hydrogen",
          grepl("cement|N fertilizer|industrial|ind ",class2,ignore.case=T)~"industry",
          grepl("gas pipeline|gas processing|unconventional oil production|gas to liquids",class2,ignore.case=T)~"industry",
          grepl("Beef|Dairy|Pork|Poultry",class2,ignore.case=T)~"livestock",
          grepl("FiberCrop|MiscCrop|OilCrop|OtherGrain|PalmFruit|Corn|Rice|Root_Tuber|SheepGoat|SugarCrop|UnmanagedLand|Wheat|FodderGrass|FodderHerb",class2,ignore.case=T)~"crops",
          TRUE~class2))%>%
        dplyr::left_join(metis.assumptions("GWP")%>%dplyr::rename(class1=ghg),by="class1")%>%
        dplyr::left_join(metis.assumptions("convertGgTgMTC"),by="Units") %>%
        dplyr::filter(!class1=='CO2') %>%
        dplyr::mutate(origValue=value,
                      value=dplyr::case_when(!is.na(GTPAR5) ~ value*GTPAR5*Convert,
                                             TRUE ~  value*GWPAR5*Convert),
                      origUnits=Units,
                      origUnits = dplyr::case_when(class1=="Other"~"Units",TRUE~origUnits),
                      units="Non-CO2 Emissions by Resource GTPAR5 (MTCO2eq)")%>%
        dplyr::mutate(param = "emissNonCO2ByResProdGTPAR5",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origX = year, subRegion=region,
                      scenario = scenNewNames,
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      classLabel1 = "GHG",
                      classPalette1 = "pal_metis",
                      classLabel2 = "sector",
                      classPalette2 = "pal_metis") %>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}

  # Emissions Fossil FUels and Industry (FFI) basically everything but LUC GTP AR5
  if(any(c("emissNonCO2ByResProdGTPAR5", "emissNonCO2BySectorGTPAR5") %in% unique(datax$param))){
    paramx <- "emissBySectorGTPAR5FFI"
    if(paramx %in% paramsSelectx){
      # GHG emissions by resource production, using AR5 GTP values
      totalFFINonCO2 <- datax %>% dplyr::filter(param %in% c("emissNonCO2ByResProdGTPAR5", "emissNonCO2BySectorGTPAR5")) %>%
        dplyr::filter(!class1=='CO2')
      # dplyr::rename so that the nonCO2 classes 1 and 2 are consistent with the CO2 classes 1 and 2
      totalFFINonCO2 <- totalFFINonCO2 %>%
        dplyr::mutate(class_temp = class2) %>%
        dplyr::mutate(class2 = class1) %>%
        dplyr::mutate(class1=class_temp) %>%
        dplyr::select(-class_temp)
      totalFFICO2 <- datax %>% dplyr::filter(param %in% c("emissCO2BySectorNoBio"))%>%dplyr::mutate(class2="CO2")
      totalFFICO2Eq <- rbind(totalFFICO2, totalFFINonCO2)
      totalFFICO2Eq$param <- 'emissBySectorGTPAR5FFI'
      totalFFICO2Eq <- totalFFICO2Eq %>%
        dplyr::mutate(origQuery="comb_origQueries",
                      origUnits="comb_origUnits",
                      units="GHG Emissions GTPAR5 (MTCO2eq)",
                      classLabel1 = "sector",
                      classLabel2 = "subSector",
                      classPalette1 = 'pal_metis')%>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      # Take this new parameter and put it in datax (main dataframe for ready-to-plot results)
      datax <- rbind(datax, totalFFICO2Eq)
    }} else {
      if(any(c("nonCO2 emissions by resource production","nonCO2 emissions by sector") %in% queriesSelectx)){
        #print(paste("totalFFINonCO2 did not run so skipping paramemissBySectorGTPAR5FFI",sep=""))
        }
    }

  # Emissions LUC basically GTP AR5
  if(any(c("emissNonCO2ByResProdGTPAR5", "emissNonCO2BySectorGTPAR5") %in% unique(datax$param))){
    paramx <- "emissBySectorGTPAR5LUC"
    if(paramx %in% paramsSelectx){
      # Same as FFI Emiss by Sec, except we are now adding LUC. So really it is the whole emissions picture (or close to it)
      totalFFINonCO2 <- datax %>% dplyr::filter(param %in% c("emissNonCO2ByResProdGTPAR5", "emissNonCO2BySectorGTPAR5")) %>%
        dplyr::filter(!class1=='CO2')
      # dplyr::rename so that the nonCO2 classes 1 and 2 are consistent with the CO2 classes 1 and 2
      totalFFINonCO2 <- totalFFINonCO2 %>%
        dplyr::mutate(class_temp = class2) %>%
        dplyr::mutate(class2 = class1) %>%
        dplyr::mutate(class1=class_temp) %>%
        dplyr::select(-class_temp)
      totalFFICO2 <- datax %>% dplyr::filter(param %in% c("emissCO2BySectorNoBio", "emissLUC"))%>%dplyr::mutate(class2="CO2")
      totalFFICO2Eq <- rbind(totalFFICO2, totalFFINonCO2)
      totalFFICO2Eq$param <- 'emissBySectorGTPAR5LUC'
      totalFFICO2Eq$Class1Palette <- 'pal_metis'
      totalFFICO2Eq <- totalFFICO2Eq %>%
        dplyr::mutate(origQuery="comb_origQueries",
                      origUnits="comb_origUnits",
                      units="GHG Emissions GTPAR5 (MTCO2eq)",
                      classLabel1 = "sector",
                      classLabel2 = "subSector",
                      classPalette1 = 'pal_metis')%>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      # Take this new parameter and put it in datax (main dataframe for ready-to-plot results)
      datax <- rbind(datax, totalFFICO2Eq)
    }} else {
      if(any(c("nonCO2 emissions by resource production","nonCO2 emissions by sector") %in% queriesSelectx)){
        #print(paste("totalFFINonCO2 did not run so skipping param emissBySectorGTPAR5LUC",sep=""))
        }
    }

  # Total Emissions without LUC GTP Summarized
  if(any(c("emissNonCO2ByResProdGTPAR5", "emissNonCO2BySectorGTPAR5") %in% unique(datax$param))){
    paramx <- "emissByGasGTPAR5FFI"
    if(paramx %in% paramsSelectx){
      # GHG emissions by resource production, using AR5 GTP values
      totalFFINonCO2 <- datax %>% dplyr::filter(param %in% c("emissNonCO2ByResProdGTPAR5", "emissNonCO2BySectorGTPAR5")) %>%
        dplyr::filter(!class1=='CO2')
      # dplyr::rename so that the nonCO2 classes 1 and 2 are consistent with the CO2 classes 1 and 2
      totalFFICO2 <- datax %>% dplyr::filter(param %in% c("emissCO2BySectorNoBio")) %>%dplyr::mutate(class1="CO2")
      totalFFICO2Eq <- rbind(totalFFICO2, totalFFINonCO2)
      totalFFICO2Eq$param <- 'emissByGasGTPAR5FFI'
      totalFFICO2Eq <- totalFFICO2Eq %>%
        dplyr::mutate(origQuery="comb_origQueries",
                      origUnits="comb_origUnits",
                      units="GHG Emissions GTPAR5 (MTCO2eq)",
                      classLabel1 = "sector",
                      classLabel2 = "subSector",
                      classPalette1 = 'pal_metis')%>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      # Take this new parameter and put it in datax (main dataframe for ready-to-plot results)
      datax <- rbind(datax, totalFFICO2Eq)
    }
  } else {
    if(any(c("nonCO2 emissions by resource production","nonCO2 emissions by sector") %in% queriesSelectx)){
      #print(paste("totalFFINonCO2 did not run so skipping param emissByGasGTPAR5FFI",sep=""))
      }
  }

  # Total Emissions with LUC GTP Summarized

  if(any(c("emissNonCO2ByResProdGTPAR5", "emissNonCO2BySectorGTPAR5",
           "emissLUC","emissCO2BySectorNoBio") %in% unique(datax$param))){
    paramx <- "emissByGasGTPAR5LUC"
    if(paramx %in% paramsSelectx){

      totalFFICO2 <- datax %>% dplyr::filter(param %in% c("emissCO2BySectorNoBio"))%>%dplyr::mutate(class1="CO2")
      # GHG emissions by resource production, using AR5 GTP values
      NonCo2_LUC <- datax %>% dplyr::filter(param %in% c("emissNonCO2ByResProdGTPAR5", "emissNonCO2BySectorGTPAR5",
                                                         "emissLUC")) %>%
        dplyr::filter(!class1=='CO2')%>%dplyr::mutate(class1=dplyr::if_else(class1=="LUC", "CO2 LUC", class1))
      totalCO2Eq <- rbind(totalFFICO2, NonCo2_LUC)

      # dplyr::rename so that the nonCO2 classes 1 and 2 are consistent with the CO2 classes 1 and 2
      totalCO2Eq$param <- 'emissByGasGTPAR5LUC'
      totalCO2Eq <- totalCO2Eq %>%
        dplyr::mutate(origQuery="comb_origQueries",
                      origUnits="comb_origUnits",
                      units="GHG Emissions GTPAR5 (MTCO2eq)",
                      classLabel1 = "sector",
                      classLabel2 = "subSector",
                      classPalette1 = 'pal_metis')%>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      # Take this new parameter and put it in datax (main dataframe for ready-to-plot results)
      datax <- rbind(datax, totalCO2Eq)
    }

  } else {
    if(any(c("nonCO2 emissions by resource production","nonCO2 emissions by sector") %in% queriesSelectx)){
      #print(paste("totalFFINonCO2 did not run so skipping param emissByGasGTPAR5LUC",sep=""))
      }
  }




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
        dplyr::filter(ghg!="CO2")%>%
        dplyr::mutate(origValue=value,
                      origUnits=Units,
                      units="Variable Units")%>%
        dplyr::filter(scenario %in% scenOrigNames)%>%
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
          class2=dplyr::case_when(
          grepl("refining",class2,ignore.case=T)~"refining",
          grepl("regional biomass|regional biomassOil|biomass" ,class2,ignore.case=T)~"biomass",
          grepl("regional corn for ethanol" ,class2,ignore.case=T)~"corn for ethanol",
          grepl("trn_",class2,ignore.case=T)~"transport",
          grepl("comm |resid ",class2,ignore.case=T)~"building",
          grepl("electricity|elec_|electricity |csp_backup",class2,ignore.case=T)~"electricity",
          grepl("H2",class2,ignore.case=T)~"hydrogen",
          grepl("cement|N fertilizer|industrial|ind ",class2,ignore.case=T)~"industry",
          grepl("gas pipeline|gas processing|unconventional oil production|gas to liquids",class2,ignore.case=T)~"industry",
          grepl("Beef|Dairy|Pork|Poultry",class2,ignore.case=T)~"livestock",
          grepl("FiberCrop|MiscCrop|OilCrop|OtherGrain|PalmFruit|Corn|Rice|Root_Tuber|SheepGoat|SugarCrop|UnmanagedLand|Wheat|FodderGrass|FodderHerb",class2,ignore.case=T)~"crops",
          TRUE~class2))%>%
        dplyr::mutate(param = "emissNonCO2BySectorOrigUnits",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origX = year, subRegion=region,
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
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
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
        dplyr::filter(scenario %in% scenOrigNames)%>%
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
                      origX = year, subRegion=region,
                      scenario = scenNewNames,
                      units = "Pasenger (million pass-km)",
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
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
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
        dplyr::filter(scenario %in% scenOrigNames)%>%
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
                      origX = year, subRegion=region,
                      scenario = scenNewNames,
                      units = "Freight (million ton-km)",
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
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
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
        dplyr::filter(scenario %in% scenOrigNames)%>%
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
                      origX = year, subRegion=region,
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
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))

      # Create table that stores total liquids production in refining
      QueryTbl <- tbl
      TotalLiquidsProdTbl <- QueryTbl %>%
        dplyr::group_by(scenario, region, subRegion,    x) %>%
        dplyr::summarise(TotalLiquids=sum(value))
      # Return to original table, and calculate fraction of total liquids production that is biofuels. QueryTbl can now
      # be applied to VMT by fuel categories (passenger and freight) below.
      FracBioFuel_tbl <- QueryTbl %>%
        dplyr::select(scenario, region, subRegion,    class1, x, value) %>%
        dplyr::left_join(TotalLiquidsProdTbl, by=c('x', 'scenario','subRegion', 'region')) %>%
        dplyr::filter(class1=='biomass liquids') %>%
        dplyr::mutate(FracBioFuel=value/TotalLiquids) %>%
        dplyr::mutate(FracFossilFuel=1-FracBioFuel) %>%
        dplyr::select(-value, -TotalLiquids) %>%
        dplyr::mutate(class1 = 'liquids')  #only apply fraction to liquids

      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
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
        dplyr::filter(scenario %in% scenOrigNames)%>%
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
                      origX = year, subRegion=region,
                      scenario = scenNewNames,
                      units = "Passenger (million pass-km)",
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
          dplyr::left_join(FracBioFuel_tbl, by=c('scenario', 'region','subRegion', 'x', 'class1')) %>%
          dplyr::mutate(value = dplyr::if_else(class1=='liquids', value*FracBioFuel, value)) %>%
          dplyr::select(-FracBioFuel, -FracFossilFuel) %>%
          dplyr::mutate(class1=dplyr::if_else(class1=='liquids', 'biofuel', class1))
        tbl2 <- tbl %>%
          dplyr::left_join(FracBioFuel_tbl %>% dplyr::mutate(class1='biofuel'), by=c('scenario', 'region', 'subRegion','x', 'class1')) %>%
          dplyr::filter(class1=='biofuel') %>%
          dplyr::mutate(class1='fossil fuel') %>%
          dplyr::mutate(value=dplyr::if_else(class1=='fossil fuel', (value/FracBioFuel)*(1-FracBioFuel), value)) %>%
          dplyr::select(-FracBioFuel, -FracFossilFuel)
        tbl <- rbind(tbl, tbl2)
      }
      tbl <- tbl %>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
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
        dplyr::filter(scenario %in% scenOrigNames)%>%
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
                      origX = year, subRegion=region,
                      scenario = scenNewNames,
                      units = "Freight (million ton-km)",
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
          dplyr::left_join(FracBioFuel_tbl, by=c('scenario', 'region','subRegion', 'x', 'class1')) %>%
          dplyr::mutate(value = dplyr::if_else(class1=='liquids', value*FracBioFuel, value)) %>%
          dplyr::select(-FracBioFuel, -FracFossilFuel) %>%
          dplyr::mutate(class1=dplyr::if_else(class1=='liquids', 'biofuel', class1))
        tbl2 <- tbl %>%
          dplyr::left_join(FracBioFuel_tbl %>% dplyr::mutate(class1='biofuel'), by=c('scenario', 'region','subRegion', 'x', 'class1')) %>%
          dplyr::filter(class1=='biofuel') %>%
          dplyr::mutate(class1='fossil fuel') %>%
          dplyr::mutate(value=dplyr::if_else(class1=='fossil fuel', (value/FracBioFuel)*(1-FracBioFuel), value)) %>%
          dplyr::select(-FracBioFuel, -FracFossilFuel)
        tbl <- rbind(tbl, tbl2)
      }
      tbl <- tbl %>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
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
        dplyr::filter(scenario %in% scenOrigNames)%>%
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
                      origX = year, subRegion=region,
                      scenario = scenNewNames,
                      units = "Transport Final Energy by Fuel (EJ)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = sector,
                      classLabel1 = "Fuel",
                      classPalette1 = "pal_metis",
                      class2 = "class2",
                      classLabel2 = "classLabel2",
                      classPalette2 = "classPalette2")
      # if("energyPrimaryRefLiqProdEJ" %in% unique(datax$param)){
      #   # Break out biofuels
      #   tbl <- tbl %>%
      #     dplyr::left_join(FracBioFuel_tbl, by=c('scenario', 'region', 'x', 'class1')) %>%
      #     dplyr::mutate(value = dplyr::if_else(class1=='liquids', value*FracBioFuel, value)) %>%
      #     dplyr::select(-FracBioFuel, -FracFossilFuel) %>%
      #     dplyr::mutate(class1=dplyr::if_else(class1=='liquids', 'biofuel', class1))
      #   tbl2 <- tbl %>%
      #     dplyr::left_join(FracBioFuel_tbl %>% dplyr::mutate(class1='biofuel'), by=c('scenario', 'region', 'x', 'class1')) %>%
      #     dplyr::filter(class1=='biofuel') %>%
      #     dplyr::mutate(class1='fossil fuel liquids') %>%
      #     dplyr::mutate(value=dplyr::if_else(class1=='fossil fuel liquids', (value/FracBioFuel)*(1-FracBioFuel), value)) %>%
      #     dplyr::select(-FracBioFuel, -FracFossilFuel)
      #   tbl <- rbind(tbl, tbl2)
      # }
      tbl <- tbl %>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))

      if(!is.null(tblFinalNrgIntlAvShip)){
        # Separat out Intl. Shipping and Aviation refined liquids from Primary Energy Oil
        tblTransportFinalOil <- tbl%>%dplyr::filter(class1=="liquids") %>%
          dplyr::mutate(class2="class2",classLabel2="classLabel2",classPalette2="classPalette2") %>%
          dplyr::select(-origValue)# Subset Transport Sector
        tblFinalNrgIntlAvShipMod <- tblFinalNrgIntlAvShip %>%
          dplyr::mutate(param=unique(tblTransportFinalOil$param),
                        class1=paste(class1,"liquids",sep=" "),
                        sources=unique(tblTransportFinalOil$sources),
                        origQuery=unique(tblTransportFinalOil$origQuery),
                        origUnits=unique(tblTransportFinalOil$origUnits),
                        units=unique(tblTransportFinalOil$units),
                        xLabel=unique(tblTransportFinalOil$xLabel),
                        aggregate=unique(tblTransportFinalOil$aggregate),
                        class2=unique(tblTransportFinalOil$class2),
                        classLabel2=unique(tblTransportFinalOil$classLabel2),
                        classPalette2=unique(tblTransportFinalOil$classPalette2),
                        classLabel1=unique(tblTransportFinalOil$classLabel1),
                        classPalette1=unique(tblTransportFinalOil$classPalette1))%>%
          dplyr::select(-origValue)# Prepare in intl. transport in correct format
        # Separate out Intl. Shipping and Aviation
        tblSepTransportFinalIntlAvShip <- tblTransportFinalOil %>%
          dplyr::bind_rows(tblFinalNrgIntlAvShipMod) %>%
          tidyr::spread(key="class1",value="value") %>%
          dplyr::mutate(`liquids`=`liquids` -`International Aviation liquids`-`International Ship liquids`)%>%
          dplyr::rename(`liquids intl av`=`International Aviation liquids`,
                        `liquids intl shp`=`International Ship liquids`) %>%
          tidyr::gather(key="class1",value="value",
                        -scenario, -region, -subRegion, -param, -sources, -class2, -x, -xLabel, -vintage, -units, -aggregate,
                        -classLabel1, -classPalette1, -classLabel2, -classPalette2,
                        -origScen,-origQuery,-origUnits,-origX)%>%
          dplyr::mutate(origValue=value); tblSepTransportFinalIntlAvShip%>%as.data.frame()
        # Rbind Transport, Intl. Shipping and Aviation back to all other Final Energy types
        tblMod<-tbl%>%dplyr::filter(class1!="liquids") %>%
          dplyr::bind_rows(tblSepTransportFinalIntlAvShip) # Remove Transport sector from Original tbl

      } else {
        print(paste("tblFinalNrgIntlAvShip does not exist so skipping subset of final energy to remove intl. shipping and aviation."))
        tblMod <- tbl
      }

      tblMod <- tblMod %>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))

      datax <- dplyr::bind_rows(datax, tblMod)
    } else {
      # if(queryx %in% queriesSelectx){print(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}

  } # Close datax assignments


  if(nrow(datax)>0){

  datax<-datax%>%unique()


  # -----------
  # unit Conversions
  # -----------
  dataxEJtoMTOE <- datax %>% dplyr::filter(grepl("\\(EJ\\)",units)) %>%
    dplyr::mutate(value=value*metis.assumptions("convEJ2MTOE"),
                  units = gsub("\\(EJ\\)","(Mtoe)",units),
                  param = gsub("EJ","MTOE",param)); dataxEJtoMTOE

  dataxEJtoTWh <- datax %>% dplyr::filter(grepl("\\(EJ\\)",units)) %>%
    dplyr::mutate(value=value*metis.assumptions("convEJ2TWh"),
                  units = gsub("\\(EJ\\)","(TWh)",units),
                  param = gsub("EJ","TWh",param))

  datax <- dplyr::bind_rows(datax,dataxEJtoMTOE,dataxEJtoTWh)
  # datax<-datax %>%
  #   dplyr::mutate(region=gsub("-","_",region), # To make consistent with maps (see ./metis/extras/metis.saveDataFile.R)
  #                 subRegion=gsub("-","_",subRegion))%>% # To make consistent with maps (see ./metis/extras/metis.saveDataFile.R)
  #   dplyr::filter(param %in% paramsSelectx) %>%
  #   unique()


  # Remove repeated USA regions
  for(scenario_i in unique(datax$scenario)){
  for(param_i in unique(datax$param)){

    regions_i <-(datax %>% dplyr::filter(param==param_i,scenario==scenario_i))$region%>%unique(); regions_i

    if(any(grepl("USA",regions_i)) & length(regions_i[regions_i %in% metis::metis.assumptions("US52")])>1){
      datax <- datax %>%
        dplyr::mutate(region = dplyr::case_when(param==param_i &
                                           scenario==scenario_i &
                                           (region=="USA" & !param %in% c("emissNonCO2BySectorGWPAR5",
                                                                          "emissNonCO2BySector",
                                                                          "emissCO2BySector",
                                                                          "emissCO2CumGlobal2010to2100",
                                                                          "emissCO2CumGlobal2010to2100RCP"))~ "RegionRemove",
                                         TRUE ~ region)) %>%
        dplyr::filter(region != "RegionRemove")
    }

    subRegions_i <-(datax %>% dplyr::filter(param==param_i,scenario==scenario_i))$subRegion%>%unique(); subRegions_i

    if(any(grepl("USA",subRegions_i)) & length(subRegions_i[subRegions_i %in% metis::metis.assumptions("US52")])>1){
      datax <- datax %>%
        dplyr::mutate(subRegion = dplyr::case_when(param==param_i &
                                           scenario==scenario_i &
                                           (subRegion=="USA" & !param %in% c("emissNonCO2BySectorGWPAR5",
                                                                             "emissNonCO2BySector",
                                                                             "emissCO2BySector",
                                                                             "emissCO2CumGlobal2010to2100",
                                                                             "emissCO2CumGlobal2010to2100RCP"))~ "subRegionRemove",
                                         TRUE ~ subRegion)) %>%
                        dplyr::filter(region != "RegionRemove")
    }
  }
  }

  # datax$scenario%>%unique()
  # datax$param%>%unique()

  # metis.chart(tbl,xData="x",yData="value",useNewLabels = 0)

  # Check
  # unique(datax$param)%>%sort();unique(datax$scenario)%>%sort()
  # datax%>%as.data.frame()%>%dplyr::select(scenario,class1,class2,x,param,value)%>%
  # dplyr::filter(x %in% c(2010:2050),param=="elecNewCapGW",scenario=="GCAMRef")%>%
  # dplyr::group_by(scenario,x)%>%dplyr::summarize(sum=sum(value))

  #---------------------
  # Save Data in CSV
  #---------------------

  datax <- datax  %>% unique()

  if(!all(regionsSelect %in% unique(datax$region))){
    print(paste("Regions not available in data: ", paste(regionsSelect[!(regionsSelect %in% unique(datax$region))],collapse=", "), sep=""))
    print(paste("Running remaining regions: ",  paste(regionsSelect[(regionsSelect %in% unique(datax$region))],collapse=", "), sep=""))
  }


  if(saveData){
     # All Data
    utils::write.csv(datax, file = paste(dirOutputs, "/", folderName, "/readGCAM/Tables_gcam/gcamDataTable",nameAppend, ".csv", sep = ""), row.names = F)
    print(paste("GCAM data table saved to: ",
                gsub("//","/",paste(dirOutputs, "/", folderName, "/readGCAM/Tables_gcam/gcamDataTable",nameAppend,".csv", sep = ""))))
     }

    # Aggregate across Class 2
    dataxAggsums<-datax%>%
      dplyr::filter(aggregate=="sum")%>%
      dplyr::select(-c(class1,classLabel1,classPalette1))%>%
      dplyr::group_by_at(dplyr::vars(-value,-origValue))%>%
      dplyr::summarize_at(c("value"),list(~sum(.,na.rm = T)))
    dataxAggmeans<-datax%>%
      dplyr::filter(aggregate=="mean")%>%
      dplyr::select(-c(class1,classLabel1,classPalette1))%>%
      dplyr::group_by_at(dplyr::vars(-value,-origValue))%>%
      dplyr::summarize_at(c("value"),list(~mean(.,na.rm = T)))
    dataxAggClass<-dplyr::bind_rows(dataxAggsums,dataxAggmeans)%>%dplyr::ungroup()

    dataAggClass2 = dataxAggClass %>% dplyr::rename(class=class2,classLabel=classLabel2,classPalette=classPalette2) %>% unique()

    if(saveData){
    utils::write.csv(dataxAggClass %>% dplyr::rename(class=class2,classLabel=classLabel2,classPalette=classPalette2),
                     file = gsub("//","/",paste(dirOutputs, "/", folderName,
                                                "/readGCAM/Tables_gcam/gcamDataTable_aggClass2",
                                                nameAppend,".csv", sep = "")),row.names = F)

    print(paste("GCAM data aggregated to class 2 saved to: ",gsub("//","/",paste(dirOutputs, "/", folderName,
                              "/readGCAM/Tables_gcam/gcamDataTable_aggClass2",
                              nameAppend,".csv", sep = "")),sep=""))

}

    # Aggregate across Class 1
    dataxAggsums<-datax%>%
      dplyr::filter(aggregate=="sum")%>%
      dplyr::select(-c(class2,classLabel2,classPalette2))%>%
      dplyr::group_by_at(dplyr::vars(-value,-origValue))%>%
      dplyr::summarize_at(c("value"),list(~sum(.,na.rm = T)))
    dataxAggmeans<-datax%>%
      dplyr::filter(aggregate=="mean")%>%
      dplyr::select(-c(class2,classLabel2,classPalette2))%>%
      dplyr::group_by_at(dplyr::vars(-value,-origValue))%>%
      dplyr::summarize_at(c("value"),list(~mean(.,na.rm = T)))
    dataxAggClass<-dplyr::bind_rows(dataxAggsums,dataxAggmeans)%>%dplyr::ungroup()

    dataAggClass1 = dataxAggClass  %>% dplyr::rename(class=class1,classLabel=classLabel1,classPalette=classPalette1) %>% unique()

    if(saveData){

    utils::write.csv(dataxAggClass  %>% dplyr::rename(class=class1,classLabel=classLabel1,classPalette=classPalette1),
                     file = gsub("//","/",paste(dirOutputs, "/", folderName,
                                                "/readGCAM/Tables_gcam/gcamDataTable_aggClass1",
                                                nameAppend,".csv", sep = "")),row.names = F)

    print(paste("GCAM data aggregated to class 1 saved to: ",gsub("//","/",paste(dirOutputs, "/", folderName,
                                                                                   "/readGCAM/Tables_gcam/gcamDataTable_aggClass1",
                                                                                   nameAppend,".csv", sep = "")),sep=""))

  }

    # Aggregate across Param
    dataxAggsums<-datax%>%
      dplyr::filter(aggregate=="sum")%>%
      dplyr::select(-c(class2,classLabel2,classPalette2,class1,classLabel1))%>%
      dplyr::group_by_at(dplyr::vars(-value,-origValue))%>%
      dplyr::summarize_at(c("value"),list(~sum(.,na.rm = T)))
    dataxAggmeans<-datax%>%
      dplyr::filter(aggregate=="mean")%>%
      dplyr::select(-c(class2,classLabel2,classPalette2,class1,classLabel1))%>%
      dplyr::group_by_at(dplyr::vars(-value,-origValue))%>%
      dplyr::summarize_at(c("value"),list(~mean(.,na.rm = T)))
    dataxAggClass<-dplyr::bind_rows(dataxAggsums,dataxAggmeans)%>%dplyr::ungroup()

    dataAggParam = dataxAggClass %>% dplyr::rename(classPalette=classPalette1) %>% unique()

    if(saveData){
    utils::write.csv(dataxAggClass %>% dplyr::rename(classPalette=classPalette1),
                     file = gsub("//","/",paste(dirOutputs, "/", folderName,
                                                "/readGCAM/Tables_gcam/gcamDataTable_aggParam",
                                                nameAppend,".csv", sep = "")),row.names = F)


    print(paste("GCAM data aggregated to param saved to: ",gsub("//","/",paste(dirOutputs, "/", folderName,
                                                                                   "/readGCAM/Tables_gcam/gcamDataTable_aggParam",
                                                                                   nameAppend,".csv", sep = "")),sep=""))
    }

  }else{print("No data for any of the regions, params or queries selected")} # Close datax nrow check

  }else{ # CLose Param Check
    print(paste("None of the parameters in paramsSelect: ", paste(paramsSelect,collapse=",")," are available."))}

  print("Outputs returned as list containing data, scenarios and queries.")
  print("For example if df <- metis.readgcam(dataProjFile = metis::exampleGCAMproj)")
  print("Then you can view the outputs as df$data, df$dataAggClass1, df$dataAggClass2, df$dataAggParam, df$scenarios, df$queries.")
  print(gsub("//","/",paste("All outputs in : ",dirOutputs, "/", folderName, "/readGCAM/",sep="")))
  print("metis.readgcam run completed.")

  return(list(data = datax,
              dataAggClass1 = dataAggClass1,
              dataAggClass2 = dataAggClass2,
              dataAggParam = dataAggParam,
              scenarios = scenarios,
              queries = queries))

    }
