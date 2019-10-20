#' metis.chartsProcess
#'
#' This function produces charts given any number of tables in the metis format.
#' The metis.chart() function produces charts for each region nd scenario.
#' If there are more than one scenario then the function also produces a folder for diffplots.
#' The input tables should be .csv files with the following columns:
#' scenario, region, sources, param, x, xLabel, vintage, class1, class2, units, value, aggregate,
#' classLabel1,classPalette1,classLabel2,classPalette2. Running the metis.readgcam automatically produces
#' An empty template with these columns for the relevant parameters. Each column is defined below:
#'
#' List of Assumptions
#' \itemize{
#' \item scenario: The name of the new data scenario
#' \item region: The region for the data
#' \item sources: Sources for the data
#' \item param: Name of the parameter
#' \item x: The x axis variable values
#' \item xLabel: X axis Label
#' \item vintage: Vintages if any. If not relevant then just enter "Vintage"
#' \item class1: Classes or types (eg. if param is water_demands then the classes may be Industry, Agriculture etc.)
#' \item class2: A second category of classes if exists.
#' \item units: Units for the parameter. These are used as the y axis label.
#' \item value: The parameter value.
#' \item aggregate: Either "sum" or "mean". This paramater is used to determine how to aggregate across regions or scenarios.
#' \item classLabel1: If class1 exists then this will be legend Label. If it doesnt exist enter "classLabel1"
#' \item classPalette1: An R or metis.colors() palette. Can leave the default as "pal_16".
#' \item classLabel2: If class2 exists then this will be legend Label. If it doesnt exist enter "classLabel2"
#' \item classPalette2: An R or metis.colors() palette. Can leave the default as "pal_16".
#' }
#' @param dataTables Vector of strings with full path to datatables to be read in.
#' Example c("D:/metis/outputs/Colombia/dataTable_Colombia_1975to2100.csv",
#' "D:/metis/outputs/Colombia/dataTableLocal_Colombia_1975to2100.csv").
#' Where "dataTableLocal_Colombia_1975to2100.csv" is the new datafile created based on
#' "dataTableTemplate_Colombia_1975to2100.csv" and contains new local data.
#' @param rTable If a table is created directly in R as a data.frame or tibble it can entered here.
#' @param dirOutputs Full path to directory for outputs. Default is paste(getwd(),"/outputs",sep="")
#' @param pdfpng Choose the format for outputs. Either "pdf", "png" or "both. Default is "png"
#' @param scenRef The reference scenario to compare against. Default will pick first scenario from
#' list f all scenarios
#' @param xData Default "x"
#' @param yData Default "value"
#' @param xLabel Default "xLabel"
#' @param yLabel Default "units"
#' @param class Default "class"
#' @param aggregate Default "sum"
#' @param classPalette Default "pal_metis" from metis.colors()$pal_metis
#' @param regionCompare Default =1. To turn of regional comparison make 0.
#' @param regionCompareOnly Default 0. If set to 1, will only run comparison plots and not individual
#' @param scenarioCompareOnly Default 0. If set to 1, will only run comparison plots and not individual
#' @param useNewLabels Default 0
#' @param sizeBarLines Default 0.5
#' @param sizeLines Default 1.5
#' @param xCompare Choose the years to compare scenarios for xScenSelectYears plot. Default is
#' c("2015","2030","2050","2100")
#' @param paramsSelect Default = "All". If desired dplyr::select a subset of paramaters to analyze from the full list of parameters:
#' c(# energy
#' "energyPrimaryByFuelEJ","energyPrimaryRefLiqProdEJ",
#' "energyFinalConsumBySecEJ","energyFinalByFuelBySectorEJ","energyFinalSubsecByFuelTranspEJ",
#' "energyFinalSubsecByFuelBuildEJ", "energyFinalSubsecByFuelIndusEJ","energyFinalSubsecBySectorBuildEJ",
#' "energyPrimaryByFuelMTOE","energyPrimaryRefLiqProdMTOE",
#' "energyFinalConsumBySecMTOE","energyFinalbyFuelMTOE","energyFinalSubsecByFuelTranspMTOE",
#' "energyFinalSubsecByFuelBuildMTOE", "energyFinalSubsecByFuelIndusMTOE","energyFinalSubsecBySectorBuildMTOE",
#' "energyPrimaryByFuelTWh","energyPrimaryRefLiqProdTWh",
#' "energyFinalConsumBySecTWh","energyFinalbyFuelTWh","energyFinalSubsecByFuelTranspTWh",
#' "energyFinalSubsecByFuelBuildTWh", "energyFinalSubsecByFuelIndusTWh","energyFinalSubsecBySectorBuildTWh",
#' # electricity
#' "elecByTechTWh","elecCapByFuel","elecFinalBySecTWh","elecFinalByFuelTWh",
#' # transport
#' "transportPassengerVMTByMode", "transportFreightVMTByMode", "transportPassengerVMTByFuel", "transportFreightVMTByFuel",
#' # water
#' "watConsumBySec", "watWithdrawBySec", "watWithdrawByCrop", "watBioPhysCons", "watIrrWithdrawBasin","watIrrConsBasin",
#' # socioecon
#' "gdpPerCapita", "gdp", "gdpGrowthRate", "pop",
#' # ag
#' "agProdbyIrrRfd","agProdBiomass", "agProdForest", "agProdByCrop",
#' # land
#' "landIrrRfd", "landAlloc","landAllocByCrop",
#' # emissions
#' "emissLUC", "emissCO2BySector","emissCO2NonCO2BySectorGWPAR5","emissCO2NonCO2BySectorGTPAR5","emissNonCO2BySectorOrigUnits",
#' "emissNonCO2ByResProdGWPAR5", "emissTotalFFIBySec","emissMethaneBySource",
#' "emissCO2BySectorNonCO2GWPAR5", "emissCO2BySectorNonCO2GWPAR5LUC", "emissTotalBySec","emissCO2BySectorNoBio")
#' @param regionsSelect Default = "All". Select regions to create charts for.
#' @param scensSelect Default = "All". Select regions to create charts for.
#' @param xRange Default "All". Range of x values eg. c(2001:2005)
#' @param folderName Default ="folderNameDefault"
#' @param nameAppend Default =""
#' @param colOrder1 Default = NULL,
#' @param colOrderName1 Default = NULL,
#' @param colOrder2 Default = NULL,
#' @param colOrderName2 Default = NULL,
#' @param scaleRange Default NULL. Dataframe with columns param, maxScale, minScale to indicate maximum and minumum values for a parameter scale.
#' @param xScenCompFacetLabelSize Default = 2
#' @param legendPosition Default="right" ("none", "left", "right", "bottom", "top", or two-element numeric vector)
#' @param figWidth Default = 13
#' @param figHeight Default = 9
#' @param multiplotOn Default=F,
#' @keywords charts, diffplots
#' @return Produces charts in output folder and also returns combined table in metis format.
#' @export
#' @importFrom "rlang" ":="

metis.chartsProcess <- function(dataTables=NULL,rTable=NULL,scenRef=NULL,
                       dirOutputs=paste(getwd(),"/outputs",sep=""),
                       pdfpng="png",
                       xRange="All",
                       xCompare=c("2015","2030","2050","2100"),
                       paramsSelect="All",
                       regionsSelect="All",
                       xData="x",yData="value",xLabel="xLabel",yLabel="units",
                       aggregate="sum",class="class", classPalette="pal_metis",
                       regionCompare=1,
                       regionCompareOnly=0,scenarioCompareOnly=0,useNewLabels=0,
                       sizeBarLines=0,sizeLines=1.5,
                       folderName="folderNameDefault",
                       nameAppend="",
                       scensSelect="All",
                       colOrder1 = NULL,
                       colOrderName1 = NULL,
                       colOrder2 = NULL,
                       colOrderName2 = NULL,
                       scaleRange = NULL,
                       xScenCompFacetLabelSize = 35,
                       legendPosition="right",
                       figWidth=13,
                       figHeight=9,
                       multiplotOn=F,
                       mp = list(paramSet=list(c("energy"),
                                                c("water")),
                                  param=list(c("elecCapByFuel","elecByTechTWh"),
                                             c("watWithdrawBySec","watConsumBySec")),
                                  nColMax=list(c(3),
                                               c(3)))) {


  # regionCompare=1
  # dataTables=NULL
  # rTable=NULL
  # scenRef=NULL
  # dirOutputs=paste(getwd(),"/outputs",sep="")
  # pdfpng="png"
  # xRange="All"
  # xCompare=c("2015","2030","2050","2100")
  # paramsSelect="All"
  # regionsSelect="All"
  # xData="x"
  # yData="value"
  # xLabel="xLabel"
  # yLabel="units"
  # aggregate="sum"
  # class="class"
  # classPalette="pal_metis"
  # regionCompareOnly=0
  # useNewLabels=0
  # sizeBarLines=0
  # sizeLines=1.5
  # nameAppend=""
  # scensSelect="All"
  # colOrder1 = NULL
  # colOrderName1 = NULL
  # colOrder2 = NULL
  # colOrderName2 = NULL
  # scaleRange = NULL
  # xScenCompFacetLabelSize = 35

#------------------
# Initialize variables to remove binding errors
# -----------------

  NULL->scenario->value->x->region->param->origValue->origScen->origQuery->year->
  origUnits->origX->sources->vintage->class1->classLabel1->classPalette1->yMax_i->yMin_i->
  class2->classLabel2->classPalette2->i->j->k->figWMult->classLabel1x ->classLabel2x-> classPalette1x-> classPalette2x->
  nScen

  aggregate_i <- aggregate

#------------------
# Function for adding any missing columns if needed
# -----------------


addMissing<-function(data){
  if(!any(grepl("\\<origScen\\>",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(origScen="scenario")}else{
    data <- data %>% dplyr::rename(!!"origScen" := (names(data)[grepl("\\<origScen\\>",names(data),ignore.case = T)])[1])
    data<-data%>%dplyr::mutate(origScen=dplyr::case_when(is.na(origScen)~"scenario",TRUE~origScen))}
  if(!any(grepl("\\<scenario\\>",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(scenario=origScen)}else{
    data <- data %>% dplyr::rename(!!"scenario" := (names(data)[grepl("\\<scenario\\>",names(data),ignore.case = T)])[1])
    data<-data%>%dplyr::mutate(scenario=dplyr::case_when(is.na(scenario)~origScen,TRUE~scenario))}
  if(!any(grepl("\\<scenarios\\>",names(data),ignore.case = T))){}else{
    data <- data %>% dplyr::rename(!!"scenario" := (names(data)[grepl("\\<scenarios\\>",names(data),ignore.case = T)])[1])
    data<-data%>%dplyr::mutate(scenario=dplyr::case_when(is.na(scenario)~origScen,TRUE~scenario))}
  if(!any(grepl("\\<region\\>",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(region="region")}else{
    data <- data %>% dplyr::rename(!!"region" := (names(data)[grepl("\\<region\\>",names(data),ignore.case = T)])[1])
    data<-data%>%dplyr::mutate(region=dplyr::case_when(is.na(region)~"region",TRUE~region))}
  if(!any(grepl("\\<regions\\>",names(data),ignore.case = T))){}else{
    data <- data %>% dplyr::rename(!!"region" := (names(data)[grepl("\\<regions\\>",names(data),ignore.case = T)])[1])
    data<-data%>%dplyr::mutate(region=dplyr::case_when(is.na(region)~"region",TRUE~region))}
  if(!any(grepl("\\<param\\>",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(param="param")}else{
    data <- data %>% dplyr::rename(!!"param" := (names(data)[grepl("\\<param\\>",names(data),ignore.case = T)])[1])
    data<-data%>%dplyr::mutate(param=dplyr::case_when(is.na(param)~"param",TRUE~param))}
  if(!any(grepl("\\<params\\>",names(data),ignore.case = T))){}else{
    data <- data %>% dplyr::rename(!!"param" := (names(data)[grepl("\\<params\\>",names(data),ignore.case = T)])[1])
    data<-data%>%dplyr::mutate(param=dplyr::case_when(is.na(param)~"param",TRUE~param))}
  if(!any(grepl("\\<value\\>",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(value=get(yData))}else{
    data <- data %>% dplyr::rename(!!"value" := (names(data)[grepl("\\<value\\>",names(data),ignore.case = T)])[1])
    data$value = as.numeric(data$value)
    data<-data%>%dplyr::mutate(value=dplyr::case_when(is.na(value)~0,TRUE~value))}
  if(!any(grepl("\\<values\\>",names(data),ignore.case = T))){}else{
    data <- data %>% dplyr::rename(!!"value" := (names(data)[grepl("\\<values\\>",names(data),ignore.case = T)])[1])
    data$value = as.numeric(data$value)
    data<-data%>%dplyr::mutate(value=dplyr::case_when(is.na(value)~0,TRUE~value))}
  if(!any(grepl("\\<origvalue\\>",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(origValue=value)}else{
    data <- data %>% dplyr::rename(!!"origValue" := (names(data)[grepl("\\<origvalue\\>",names(data),ignore.case = T)])[1])
    data<-data%>%dplyr::mutate(origValue=dplyr::case_when(is.na(origValue)~value,TRUE~origValue))}
  if(!any(grepl("\\<unit\\>",names(data),ignore.case = T))){}else{
    data <- data %>% dplyr::rename(!!"units" := (names(data)[grepl("\\<unit\\>",names(data),ignore.case = T)])[1])
    data<-data%>%dplyr::mutate(units=dplyr::case_when(is.na(units)~"units",TRUE~units))}
  if(!any(grepl("\\<units\\>",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(units="units")}else{
    data <- data %>% dplyr::rename(!!"units" := (names(data)[grepl("\\<units\\>",names(data),ignore.case = T)])[1])
    data<-data%>%dplyr::mutate(units=dplyr::case_when(is.na(units)~"units",TRUE~units))}
  if(!"x"%in%names(data)){if("year"%in%names(data)){
    data<-data%>%dplyr::mutate(x=year)}else{data<-data%>%dplyr::mutate(x="x")}}
  if(!any(grepl("\\<vintage\\>",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(vintage = paste("Vint_", x, sep = ""))}else{
    data <- data %>% dplyr::rename(!!"vintage" := (names(data)[grepl("\\<vintage\\>",names(data),ignore.case = T)])[1])
    data<-data%>%dplyr::mutate(vintage=dplyr::case_when(is.na(vintage)~paste("Vint_", x, sep = ""),TRUE~vintage))}
  if(!any(grepl("\\<xlabel\\>",names(data),ignore.case = T))){
    if(is.null(xLabel)){data<-data%>%dplyr::mutate(xLabel="xLabel")}else{
      data<-data%>%dplyr::mutate(xLabel=xLabel)}}else{
        data <- data %>% dplyr::rename(!!"xLabel" := (names(data)[grepl("\\<xlabel\\>",names(data),ignore.case = T)])[1])
        data<-data%>%dplyr::mutate(xLabel=dplyr::case_when(is.na(xLabel)~"xLabel",TRUE~xLabel))}
  if(!any(grepl("\\<aggregate\\>",names(data),ignore.case = T))){
    if(is.null(aggregate)){data<-data%>%dplyr::mutate(aggregate=aggregate_i)}else{
      data<-data%>%dplyr::mutate(aggregate=aggregate_i)}
  }else{
    data <- data %>% dplyr::rename(!!"aggregate" := (names(data)[grepl("\\<aggregate\\>",names(data),ignore.case = T)])[1])
    data<-data%>%dplyr::mutate(aggregate=dplyr::case_when(is.na(aggregate)~aggregate_i,
                                                         TRUE~aggregate))}
  if(!any(grepl("\\<class1\\>",names(data),ignore.case = T))){
    if(!any(grepl("\\<class\\>",names(data),ignore.case = T))){
    data<-data%>%dplyr::rename(class1="class1")}else{data<-data%>%dplyr::mutate(class1=class)}}else{
      data <- data %>% dplyr::rename(!!"class1" := (names(data)[grepl("\\<class1\\>",names(data),ignore.case = T)])[1])
      data<-data%>%dplyr::mutate(class1=dplyr::case_when(is.na(class1)~"class1",TRUE~class1))}
  if(!any(grepl("\\<classlabel1\\>",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(classLabel1="classLabel1")}else{
    data <- data %>% dplyr::rename(!!"classLabel1" := (names(data)[grepl("\\<classlabel1\\>",names(data),ignore.case = T)])[1])
    data<-data%>%dplyr::mutate(classLabel1=dplyr::case_when(is.na(classLabel1)~"classLabel1",TRUE~classLabel1))}
  if(!any(grepl("\\<classpalette1\\>",names(data),ignore.case = T))){ if(is.null(classPalette)){data<-data%>%dplyr::mutate(classPalette1="pal_metis")}else{
    data<-data%>%dplyr::mutate(classPalette1=classPalette)}}else{
      data <- data %>% dplyr::rename(!!"classPalette1" := (names(data)[grepl("\\<classpalette1\\>",names(data),ignore.case = T)])[1])
      data<-data%>%dplyr::mutate(classPalette1=dplyr::case_when(is.na(classPalette1)~classPalette,TRUE~classPalette1))}
  if(!any(grepl("\\<class2\\>",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(class2="class2")}else{
    data <- data %>% dplyr::rename(!!"class2" := (names(data)[grepl("\\<class2\\>",names(data),ignore.case = T)])[1])
    data<-data%>%dplyr::mutate(class2=dplyr::case_when(is.na(class2)~"class2",TRUE~class2))}
  if(!any(grepl("\\<classlabel2\\>",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(classLabel2="classLabel2")}else{
    data <- data %>% dplyr::rename(!!"classLabel2" := (names(data)[grepl("\\<classlabel2\\>",names(data),ignore.case = T)])[1])
    data<-data%>%dplyr::mutate(classLabel2=dplyr::case_when(is.na(classLabel2)~"classLabel2",TRUE~classLabel2))}
  if(!any(grepl("\\<classpalette2\\>",names(data),ignore.case = T))){ if(is.null(classPalette)){data<-data%>%dplyr::mutate(classPalette2="pal_metis")}else{
    data<-data%>%dplyr::mutate(classPalette2=classPalette)}}else{
      data <- data %>% dplyr::rename(!!"classPalette2" := (names(data)[grepl("\\<classpalette2\\>",names(data),ignore.case = T)])[1])
      data<-data%>%dplyr::mutate(classPalette2=dplyr::case_when(is.na(classPalette2)~classPalette,TRUE~classPalette2))}
  if(!any(grepl("\\<origscen\\>",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(origScen="origScen")}else{
    data <- data %>% dplyr::rename(!!"origScen" := (names(data)[grepl("\\<origscen\\>",names(data),ignore.case = T)])[1])
    data<-data%>%dplyr::mutate(origScen=dplyr::case_when(is.na(origScen)~"origScen",TRUE~origScen))}
  if(!any(grepl("\\<origquery\\>",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(origQuery="origQuery")}else{
    data <- data %>% dplyr::rename(!!"origQuery" := (names(data)[grepl("\\<origquery\\>",names(data),ignore.case = T)])[1])
    data<-data%>%dplyr::mutate(origQuery=dplyr::case_when(is.na(origQuery)~"origQuery",TRUE~origQuery))}
  if(!any(grepl("\\<origunit\\>",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(origUnits="origUnits")}else{
    data <- data %>% dplyr::rename(!!"origUnits" := (names(data)[grepl("\\<origunit\\>",names(data),ignore.case = T)])[1])
    data<-data%>%dplyr::mutate(origUnits=dplyr::case_when(is.na(origUnits)~"origUnits",TRUE~origUnits))}
  if(!any(grepl("\\<origx\\>",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(origX="origX")}else{
    data <- data %>% dplyr::rename(!!"origX" := (names(data)[grepl("\\<origx\\>",names(data),ignore.case = T)])[1])
    data<-data%>%dplyr::mutate(origX=dplyr::case_when(is.na(origX)~"origX",TRUE~origUnits))}
  if(!any(grepl("\\<source\\>",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(sources="sources")}else{
    data <- data %>% dplyr::rename(!!"sources" := (names(data)[grepl("\\<source\\>",names(data),ignore.case = T)])[1])
    data<-data%>%dplyr::mutate(sources=dplyr::case_when(is.na(sources)~"sources",TRUE~sources))}
  return(data)
}


#------------------
# Read in tables (Either csv tables (dataTables) or R data (rTables))
#------------------

tbl<-tibble::tibble()

if(is.null(dataTables) & is.null(rTable)){
  stop ("No dataTable or rTables have been provided.")
}

# Read in csv (dataTables)
#------------------------
if(!is.null(dataTables)){

for(i in dataTables){
  if(file.exists(i)){
  tblNew<-utils::read.csv(paste(i), stringsAsFactors = F, encoding="Latin-1")%>%tibble::as_tibble()
  if("class"%in%names(tblNew) & !"class1"%in%names(tblNew)){tblNew<-tblNew%>%dplyr::mutate(class1=class)}

  # Join relevant colors and classes using the mapping file if it exists
  if(file.exists(paste(getwd(),"/dataFiles/mapping/template_Regional_mapping.csv", sep = ""))){
    map<-utils::read.csv(paste(getwd(),"/dataFiles/mapping/template_Regional_mapping.csv", sep = ""), stringsAsFactors = F, encoding="Latin-1")%>%tibble::as_tibble()
    for(missing_i in c( "classLabel1","classPalette1","classLabel2","classPalette2")){
      if(!missing_i %in% names(tblNew))
    tblNew<-tblNew%>%dplyr::left_join(map%>%dplyr::select(param,missing_i)%>%dplyr::distinct(),by=c("param"))}}

   tbl<-dplyr::bind_rows(tbl,tblNew)
   } else {stop(paste(i," does not exist. Please run metis.readgcam.R to generate corresponding table."))}
}

# Add missing columns
  tbl<-addMissing(tbl)
  if("class" %in% names(tbl)){tbl<-tbl%>%dplyr::select(-class)}
}

# Read in R data (rTable)
#------------------------
if(!is.null(rTable)){
  rTable<-addMissing(rTable)
  rTable$origScen=as.character(rTable$origScen)
  rTable$origQuery=as.character(rTable$origQuery)
  rTable$origUnits=as.character(rTable$origUnits)
  rTable$origX=as.character(rTable$origX)
tbl<-dplyr::bind_rows(tbl,rTable)
}


#----------------
# Check scaleRange
#---------------

  scaleRange[is.na(scaleRange)]<-NA_real_
  scaleRange[scaleRange=="NA"]<-NA_real_
  if(!all(c("param","maxScale","minScale") %in% names(scaleRange))){
    paste("Incorrect column names for scaleRange: ",names(scaleRange),". Should include param, maxScale, minscale.")
    paste("Setting scaleRange to NULL.")
    scaleRange=NULL
  }


#------------------------
# Subset Data
#------------------------

if(any(paramsSelect!="All")){
  if(all(paramsSelect %in% unique(tbl$param))){
    print(paste("Running paramaters: ",  paste(paramsSelect[(paramsSelect %in% unique(tbl$param))],collapse=", "), sep=""))
    tbl<-tbl%>%dplyr::filter(param %in% paramsSelect[(paramsSelect %in% unique(tbl$param))])
  }else{
    print(paste("Parameters not available in data: ", paste(paramsSelect[!(paramsSelect %in% unique(tbl$param))],collapse=", "), sep=""))
    print(paste("Running remaining paramaters: ",  paste(paramsSelect[(paramsSelect %in% unique(tbl$param))],collapse=", "), sep=""))
    tbl<-tbl%>%dplyr::filter(param %in% paramsSelect[(paramsSelect %in% unique(tbl$param))])
  }
}else{print("One or more items in paramsSelect is 'All' so running analysis for all params:")
      print(paste(unique(tbl$param),collapse=", "))}

if(any(regionsSelect!="All")){
  if(all(regionsSelect %in% unique(tbl$region))){
    print(paste("Running regions: ",  paste(regionsSelect[(regionsSelect %in% unique(tbl$region))],collapse=", "), sep=""))
    tbl<-tbl%>%dplyr::filter(region %in% regionsSelect[(regionsSelect %in% unique(tbl$region))])
  }else{
    print(paste("Regions not available in data: ", paste(regionsSelect[!(regionsSelect %in% unique(tbl$region))],collapse=", "), sep=""))
    print(paste("Running remaining regions: ",  paste(regionsSelect[(regionsSelect %in% unique(tbl$region))],collapse=", "), sep=""))
    tbl<-tbl%>%dplyr::filter(region %in% regionsSelect[(regionsSelect %in% unique(tbl$region))])
  }
}else{print("One or more items in regionsSelect is 'All' so running analysis for all regions.")
       print(paste(unique(tbl$region),collapse=", "))}

if(any(scensSelect!="All")){
  if(all(scensSelect %in% unique(tbl$scenario))){
    print(paste("Running scenarios: ",  paste(scensSelect[(scensSelect %in% unique(tbl$scenario))],collapse=", "), sep=""))
    tbl<-tbl%>%dplyr::filter(scenario %in% scensSelect[(scensSelect %in% unique(tbl$scenario))])
  }else{
    print(paste("Scenarios not available in data: ", paste(scensSelect[!(scensSelect %in% unique(tbl$scenario))],collapse=", "), sep=""))
    print(paste("Running remaining scenarios: ",  paste(scensSelect[(scensSelect %in% unique(tbl$scenario))],collapse=", "), sep=""))
    tbl<-tbl%>%dplyr::filter(scenario %in% scensSelect[(scensSelect %in% unique(tbl$scenario))])
  }
}else{print("One or more items in scensSelect is 'All' so running analysis for all scenarios.")
       print(paste(unique(tbl$scenario),collapse=", "))}



if(any(xRange!="All")){if(is.numeric(tbl$x)){tbl<-tbl%>%dplyr::filter(x %in% xRange)
                       print(paste("Running analysis for chosen years: ",paste(xRange,collapse=", ")))}}else{
                         print("One or more items in xRange is 'All' so running analysis for all years.")
                         print(paste(unique(tbl$x),collapse=", "))
                       }


if(any(is.na(unique(tbl$scenario)))){stop("NA scenario not valid. Please check your input scenarios.")}


#------------------
# Create Folders if needed
#------------------
if (!dir.exists(dirOutputs)){
  dir.create(dirOutputs)}
if (!dir.exists(paste(dirOutputs, "/Charts", sep = ""))){
  dir.create(paste(dirOutputs, "/Charts", sep = ""))}
  if (!dir.exists(paste(dirOutputs, "/Charts/",folderName, sep = ""))){
    dir.create(paste(dirOutputs, "/Charts/",folderName, sep = ""))}


if(length(unique(tbl$region))>1){
  if (!dir.exists(paste(dirOutputs, "/Charts/",folderName,"/compareRegions", sep = ""))){
    dir.create(paste(dirOutputs, "/Charts/",folderName,"/compareRegions", sep = ""))}
  if (!dir.exists(paste(dirOutputs, "/Charts/",folderName,"/compareRegions/",gsub(" ","",paste(unique(unique(tbl$region)),collapse="")),"compareScen", sep = ""))){
    dir.create(paste(dirOutputs, "/Charts/",folderName,"/compareRegions/",gsub(" ","",paste(unique(unique(tbl$region)),collapse="")),"compareScen", sep = ""))}
  for (j in unique(tbl$scenario)) {
    if (!dir.exists(paste(dirOutputs, "/Charts/",folderName,"/compareRegions/",gsub(" ","",paste(unique(unique(tbl$region)),collapse="")),j,sep = "")))
    {dir.create(paste(dirOutputs, "/Charts/",folderName,"/compareRegions/",gsub(" ","",paste(unique(unique(tbl$region)),collapse="")),j,sep = ""))}
  }
} # If length(unique(tbl$region))>1

if(regionCompareOnly!=1){
  for (i in unique(tbl$region)){
    tbl_r<-tbl%>%dplyr::filter(region==i)
    if (!dir.exists(paste(dirOutputs, "/Charts/",folderName,"/", i, sep = ""))){
      dir.create(paste(dirOutputs, "/Charts/",folderName,"/", i, sep = ""))}
    if (!dir.exists(paste(dirOutputs, "/Charts/",folderName,"/", i,"/multiPlot", sep = ""))){
      dir.create(paste(dirOutputs, "/Charts/",folderName,"/", i,"/multiPlot", sep = ""))}
    if(length(unique(tbl_r$scenario))>1){
      if (!dir.exists(paste(dirOutputs, "/Charts/",folderName,"/", i, "/compareScen",sep = ""))){
        dir.create(paste(dirOutputs, "/Charts/",folderName,"/", i, "/compareScen",sep = ""))}}
    for (j in unique(tbl_r$scenario)) {
      if (!dir.exists(paste(dirOutputs, "/Charts/",folderName,"/", i,"/", j,sep = "")))
      {dir.create(paste(dirOutputs, "/Charts/",folderName,"/", i,"/", j,sep = ""))}
    }
  }
} # Close if(regionCompareOnly!=1)


#------------------
# Tables
#------------------
  tbl <- tbl %>% unique()

# Aggregate across classes
tblAggsums<-tbl%>%
  dplyr::mutate(scenario=as.character(scenario))%>%
  dplyr::filter(aggregate=="sum")%>%
  dplyr::select(scenario,region,param,units,x,value)%>%
  dplyr::group_by_at(dplyr::vars(-value))%>%
  dplyr::summarize_at(c("value"),list(~sum(.)))
tblAggmeans<-tbl%>%
  dplyr::mutate(scenario=as.character(scenario))%>%
  dplyr::filter(aggregate=="mean")%>%
  dplyr::select(scenario,region,param,units,x, value)%>%
  dplyr::group_by_at(dplyr::vars(-value))%>%
  dplyr::summarize_at(c("value"),list(~mean(.)))
tblAgg<-dplyr::bind_rows(tblAggsums,tblAggmeans)%>%dplyr::ungroup()

tblAggClass1sums<-tbl%>%
  dplyr::mutate(scenario=as.character(scenario))%>%
  dplyr::filter(aggregate=="sum")%>%
  dplyr::select(scenario,region,param,units,x,value,class1)%>%
  dplyr::group_by_at(dplyr::vars(-value))%>%
  dplyr::summarize_at(c("value"),list(~sum(.)))
tblAggClass1means<-tbl%>%
  dplyr::mutate(scenario=as.character(scenario))%>%
  dplyr::filter(aggregate=="mean")%>%
  dplyr::select(scenario,region,param,units,x, value, class1)%>%
  dplyr::group_by_at(dplyr::vars(-value))%>%
  dplyr::summarize_at(c("value"),list(~mean(.)))
tblAggClass1<-dplyr::bind_rows(tblAggClass1sums,tblAggClass1means)%>%dplyr::ungroup()

# Check
# tbl%>%as.data.frame()%>%select(scenario,class1,x,param,value)%>%
# filter(x %in% c(2010,2015),param=="energyFinalConsumBySecMTOE",scenario=="GCAMRef")%>%
# group_by(scenario,x)%>%summarize(sum=sum(value/metis.assumptions()$convEJ2MTOE))


tblAggClass2sums<-tbl%>%
  dplyr::mutate(scenario=as.character(scenario))%>%
  dplyr::filter(aggregate=="sum")%>%
  dplyr::select(scenario,region,param,units,x,value,class2)%>%
  dplyr::group_by_at(dplyr::vars(-value))%>%
  dplyr::summarize_at(c("value"),list(~sum(.)))
tblAggClass2means<-tbl%>%
  dplyr::mutate(scenario=as.character(scenario))%>%
  dplyr::filter(aggregate=="mean")%>%
  dplyr::select(scenario,region,param,units,x, value, class2)%>%
  dplyr::group_by_at(dplyr::vars(-value))%>%
  dplyr::summarize_at(c("value"),list(~mean(.)))
tblAggClass2<-dplyr::bind_rows(tblAggClass2sums,tblAggClass2means)%>%dplyr::ungroup()


for(i in unique(tbl$region)){
  utils::write.csv(tbl%>%
                     dplyr::filter(region == i)%>%
                     dplyr::select(scenario,region,param,units, class1, class2, x, value, vintage)%>%
                     tidyr::spread(scenario,yData),
                   file = paste(dirOutputs, "/Charts/",folderName,"/Tables_regional_",i,".csv", sep = ""),row.names = F)

  utils::write.csv(tblAgg%>%
                     dplyr::filter(region == i)%>%
                     tidyr::spread(scenario,yData),
                   file = paste(dirOutputs, "/Charts/",folderName,"/Tables_regional_",i,"_aggParam.csv", sep = ""),row.names = F)

  utils::write.csv(tblAggClass1%>%
                     dplyr::filter(region == i)%>%
                     tidyr::spread(scenario,yData),
                   file = paste(dirOutputs, "/Charts/",folderName,"/Tables_regional_",i,"_aggClass1.csv", sep = ""),row.names = F)

  utils::write.csv(tblAggClass2%>%
                     dplyr::filter(region == i)%>%
                     tidyr::spread(scenario,yData),
                   file = paste(dirOutputs, "/Charts/",folderName,"/Tables_regional_",i,"_aggClass2.csv", sep = ""),row.names = F)


}

utils::write.csv(tbl%>%
                   dplyr::select(scenario, region, param, units, class1, class2, x, value, vintage)%>%
                   tidyr::spread(scenario,yData),
                 file = paste(dirOutputs, "/Charts/",folderName,"/Tables_regional_allRegions.csv", sep = ""),row.names = F)

utils::write.csv(tblAgg%>%
                   tidyr::spread(scenario,yData),
                 file = paste(dirOutputs, "/Charts/",folderName,"/Tables_regional_allRegions_aggParam.csv", sep = ""),row.names = F)

utils::write.csv(tblAggClass1%>%
                   tidyr::spread(scenario,yData),
                 file = paste(dirOutputs, "/Charts/",folderName,"/Tables_regional_allRegions_aggClass1.csv", sep = ""),row.names = F)

utils::write.csv(tblAggClass2%>%
                   tidyr::spread(scenario,yData),
                 file = paste(dirOutputs, "/Charts/",folderName,"/Tables_regional_allRegions_aggClass2.csv", sep = ""),row.names = F)



#------------------
# Create Charts for Regional Comparison
#------------------

if(regionCompare==1){
if(length(unique(tbl$region))>1){

  for(j in unique(tbl$scenario)){
    for(k in unique(tbl$param)){

      tbl_sp<-tbl%>%dplyr::filter(scenario==j,
                                   param==k)


    if(k %in% unique(scaleRange$param)){
      yMax_i = scaleRange %>% dplyr::filter(param==k) %>% unique()
      if(nrow(yMax_i)==1){yMax_i = yMax_i$maxScale}else{
        yMax_i=NULL
        print("number of rows for scaleRange maxScale for param ", k," incorrect. Setting yMax to NULL")}

      yMin_i = scaleRange %>% dplyr::filter(param==k) %>% unique()
      if(nrow(yMin_i)==1){yMin_i = yMin_i$minScale}else{
        yMin_i=NULL
        print("number of rows for scaleRange minScale for param ", k," incorrect. Setting yMin to NULL")}
    }else{yMax_i=NULL;yMin_i=NULL}

      if(nrow(tbl_sp)>0){

        if(length(unique(tbl_sp$class1))>1){if(grepl("right|left",legendPosition,ignore.case = T)){figWMult=1.3}else{figWMult=1}}else{figWMult=1}

        # Aggregated Class 1
        # Aggregate across classes
        tblAggsums<-tbl_sp%>%
          dplyr::mutate(scenario=as.character(scenario))%>%
          dplyr::filter(aggregate=="sum")%>%
          dplyr::select(-class2,-classLabel2,-classPalette2,-origValue,-origScen,-origQuery,-origUnits,-origX,-vintage)%>%
          dplyr::group_by_at(dplyr::vars(-value))%>%
          dplyr::summarize_at(c("value"),list(~sum(.)))
        tblAggmeans<-tbl_sp%>%
          dplyr::mutate(scenario=as.character(scenario))%>%
          dplyr::filter(aggregate=="mean")%>%
          dplyr::select(-class2,-classLabel2,-classPalette2,-origValue,-origScen,-origQuery,-origUnits,-origX,-vintage)%>%
          dplyr::group_by_at(dplyr::vars(-value))%>%
          dplyr::summarize_at(c("value"),list(~mean(.)))
        tbl_spC1<-dplyr::bind_rows(tblAggsums,tblAggmeans)%>%dplyr::ungroup()


        # Bar Chart
       metis.chart(tbl_spC1, xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel, yMax=yMax_i, yMin=yMin_i,
                    sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar",facet_columns="region",facet_rows=NULL,
          dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/compareRegions/",gsub(" ","",paste(unique(unique(tbl$region)),collapse="")),j,sep = ""),
          fileName = paste(k,"_figBar_",j,"_compareRegions",nameAppend,sep=""),
          figWidth = figWidth*max((length(unique(tbl_sp$region))/2),1)*figWMult,
          figHeight = figHeight*max(1,ceiling(length(unique(tbl_sp$region))/4)*0.75),pdfpng=pdfpng, legendPosition=legendPosition, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
        )



        # Line Chart
        metis.chart(tbl_spC1,xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel, yMax=yMax_i, yMin=yMin_i,
                    sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "line",facet_columns="region",facet_rows=NULL,
          dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/compareRegions/",gsub(" ","",paste(unique(unique(tbl$region)),collapse="")),j,sep = ""),
          fileName = paste(k,"_figLines_",j,"_compareRegions",nameAppend,sep=""),
          figWidth = figWidth*max((length(unique(tbl_sp$region))/2),1)*figWMult,
          figHeight = figHeight*max(1,ceiling(length(unique(tbl_sp$region))/4)*0.75),pdfpng=pdfpng, legendPosition=legendPosition, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
        )

        # If class 2 available
        if(length(unique(tbl_sp$class2))>1){

          # Bar Chart
          metis.chart(tbl_sp, xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel, yMax=yMax_i, yMin=yMin_i,
                      sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar",facet_columns="region",facet_rows="class2",
                      dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/compareRegions/",gsub(" ","",paste(unique(unique(tbl$region)),collapse="")),j,sep = ""),
                      fileName = paste(k,"_figBar_",j,"_compareRegionsClass2",nameAppend,sep=""),
                      figWidth = figWidth*max((length(unique(tbl_sp$region))/2),1)*figWMult,
                      figHeight = figHeight*max((length(unique(tbl_sp$class2))/2),1),pdfpng=pdfpng, legendPosition=legendPosition, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
          )

          # Line Chart
          metis.chart(tbl_sp,xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel, yMax=yMax_i, yMin=yMin_i,
                      sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "line",facet_columns="region",facet_rows="class2",
                      dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/compareRegions/",gsub(" ","",paste(unique(unique(tbl$region)),collapse="")),j,sep = ""),
                      fileName = paste(k,"_figLines_",j,"_compareRegionsClass2",nameAppend,sep=""),
                      figWidth = figWidth*max((length(unique(tbl_sp$region))/2),1)*figWMult,
                      figHeight = figHeight*max((length(unique(tbl_sp$class2))/2),1),pdfpng=pdfpng, legendPosition=legendPosition, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
          )


        }

      } # Close if(nrow(tbl_sp)>0)

    } # close loop for param
  } # close loop for scenario


#------------------
# Compare Scenarios for each region
#------------------

if(length(unique(tbl$scenario))>1){


    for(j in unique(tbl$param)){

      tbl_p<-tbl%>%dplyr::filter(param==j)

      if(j %in% unique(scaleRange$param)){
        yMax_i = scaleRange %>% dplyr::filter(param==j) %>% unique()
        if(nrow(yMax_i)==1){yMax_i = yMax_i$maxScale}else{
          yMax_i=NULL
          print("number of rows for scaleRange maxScale for param ", k," incorrect. Setting yMax to NULL")}

        yMin_i = scaleRange %>% dplyr::filter(param==j) %>% unique()
        if(nrow(yMin_i)==1){yMin_i = yMin_i$minScale}else{
          yMin_i=NULL
          print("number of rows for scaleRange minScale for param ", k," incorrect. Setting yMin to NULL")}
      }else{yMax_i=NULL;yMin_i=NULL}



      if(length(unique((tbl_p%>%dplyr::filter(value>0))$scenario))>1){

      if(nrow(tbl_p)>0){

        if(length(unique(tbl_p$class1))>1){if(grepl("right|left",legendPosition,ignore.case = T)){figWMult=1.3}else{figWMult=1}}else{figWMult=1}

        # Aggregated Class 1
        # Aggregate across classes
        tblAggsums<-tbl_p%>%
          dplyr::mutate(scenario=as.character(scenario))%>%
          dplyr::filter(aggregate=="sum")%>%
          dplyr::select(-class2,-classLabel2,-classPalette2,-origValue,-origScen,-origQuery,-origUnits,-origX,-vintage)%>%
          dplyr::group_by_at(dplyr::vars(-value))%>%
          dplyr::summarize_at(c("value"),list(~sum(.)))
        tblAggmeans<-tbl_p%>%
          dplyr::mutate(scenario=as.character(scenario))%>%
          dplyr::filter(aggregate=="mean")%>%
          dplyr::select(-class2,-classLabel2,-classPalette2,-origValue,-origScen,-origQuery,-origUnits,-origX,-vintage)%>%
          dplyr::group_by_at(dplyr::vars(-value))%>%
          dplyr::summarize_at(c("value"),list(~mean(.)))
        tbl_pC1<-dplyr::bind_rows(tblAggsums,tblAggmeans)%>%dplyr::ungroup()

        # Bar Chart
        metis.chart(tbl_pC1, xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel, yMax=yMax_i, yMin=yMin_i,
                    sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar",facet_columns="scenario",facet_rows="region",
          dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/compareRegions/",gsub(" ","",paste(unique(unique(tbl$region)),collapse="")),"compareScen", sep = ""),
          fileName = paste(j,"_figBar_compareScenRegions",nameAppend,sep=""),
          figWidth = figWidth*max((length(unique(tbl_p$scenario))/2),1)*figWMult,
          figHeight = figHeight*max((length(unique(tbl_p$region))/2),1),pdfpng=pdfpng, legendPosition=legendPosition, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
        )

        # Line Chart
        metis.chart(tbl_pC1,xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel, yMax=yMax_i, yMin=yMin_i,
                    sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "line",facet_columns="scenario",facet_rows="region",
          dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/compareRegions/",gsub(" ","",paste(unique(unique(tbl$region)),collapse="")),"compareScen", sep = ""),
          fileName = paste(j,"_figLine_compareScenRegions",nameAppend,sep=""),
          figWidth = figWidth*max((length(unique(tbl_p$scenario))/2),1)*figWMult,
          figHeight = figHeight*max((length(unique(tbl_p$region))/2),1),pdfpng=pdfpng, legendPosition=legendPosition, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
        )

        #-------------------------
        # Plot with Scenarios on X for Chosen Years
        #------------------------

        if(any(!xCompare %in% unique(tbl_p[[xData]]))){
          print(paste("xCompare not available in data: ", paste(xCompare[!(xCompare %in% unique(tbl_p[[xData]]))],collapse=", "), sep=""))
          print(paste("Comparing for only: ",  paste(xCompare[(xCompare %in% unique(tbl_p[[xData]]))],collapse=", "), sep=""))
          tbl_py <- tbl_p%>%dplyr::filter(x %in% xCompare)}else{
            print(paste("Comparing for only: ",  paste(xCompare,collapse=", "), sep=""))
            tbl_py <- tbl_p%>%dplyr::filter(x %in% xCompare)
          }

        if(length(unique(tbl_py$class1))>1){if(grepl("right|left",legendPosition,ignore.case = T)){figWMult=1.3}else{figWMult=1}}else{figWMult=1}

        # Aggregated Class 1
        # Aggregate across classes
        tblAggsums<-tbl_py%>%
          dplyr::mutate(scenario=as.character(scenario))%>%
          dplyr::filter(aggregate=="sum")%>%
          dplyr::select(-class2,-classLabel2,-classPalette2,-origValue,-origScen,-origQuery,-origUnits,-origX,-vintage)%>%
          dplyr::group_by_at(dplyr::vars(-value))%>%
          dplyr::summarize_at(c("value"),list(~sum(.)))
        tblAggmeans<-tbl_py%>%
          dplyr::mutate(scenario=as.character(scenario))%>%
          dplyr::filter(aggregate=="mean")%>%
          dplyr::select(-class2,-classLabel2,-classPalette2,-origValue,-origScen,-origQuery,-origUnits,-origX,-vintage)%>%
          dplyr::group_by_at(dplyr::vars(-value))%>%
          dplyr::summarize_at(c("value"),list(~mean(.)))
        tbl_pyC1<-dplyr::bind_rows(tblAggsums,tblAggmeans)%>%dplyr::ungroup()



        # Bar Chart

        metis.chart(tbl_pyC1, xData ="scenario", yData=yData,xLabel=xLabel,yLabel=yLabel,facetLabelSize = xScenCompFacetLabelSize,
                    sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar", facet_columns = xData, facet_rows="region",
          dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/compareRegions/",gsub(" ","",paste(unique(unique(tbl$region)),collapse="")),"compareScen", sep = ""),
          fileName = paste(j,"_figBar_compareScenRegion_xScenSelectYears",nameAppend,sep=""),
          figWidth = figWidth*max((length(unique(tbl_py$x)[unique(tbl_py$x) %in% xCompare])/3),1)*figWMult,
          figHeight = figHeight*max((length(unique(tbl_py$region))/2),1),pdfpng=pdfpng, legendPosition=legendPosition, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
        )


        #-------------------------
        # Aggregate and Plot Dodged/OverLapping Plots
        #------------------------

        # Aggregate across classes
        tbl_pAggsums<-tbl_p%>%
          dplyr::mutate(scenario=as.character(scenario))%>%
          dplyr::filter(aggregate=="sum")%>%
          dplyr::select(-tidyselect::contains("class"),-origValue,-origScen,-origQuery,-origUnits,-origX,-vintage)%>%
          dplyr::group_by_at(dplyr::vars(-yData))%>%
          dplyr::summarize_at(c(yData),list(~sum(.)))
        tbl_pAggmeans<-tbl_p%>%
          dplyr::mutate(scenario=as.character(scenario))%>%
          dplyr::filter(aggregate=="mean")%>%
          dplyr::select(-tidyselect::contains("class"),-origValue,-origScen,-origQuery,-origUnits,-origX,-vintage)%>%
          dplyr::group_by_at(dplyr::vars(-yData))%>%
          dplyr::summarize_at(c(yData),list(~mean(.)))
        tbl_pAgg<-dplyr::bind_rows(tbl_pAggsums,tbl_pAggmeans)%>%dplyr::ungroup()


        if(nrow(tbl_pAgg)>0){

          # Bar Chart Dodged
          metis.chart(tbl_pAgg, xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel, yMax=yMax_i, yMin=yMin_i, sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar",
                      class ="scenario", position ="dodge", classPalette = classPalette,
                      facet_columns="region",facet_rows=NULL,
            dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/compareRegions/",gsub(" ","",paste(unique(unique(tbl$region)),collapse="")),"compareScen", sep = ""),
            fileName = paste(j,"_figBarDodged_compareScenRegion",nameAppend,sep=""),
            figWidth = figWidth*figWMult,pdfpng=pdfpng, legendPosition=legendPosition, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
          )

          # Line Chart Overlapped
          metis.chart(tbl_pAgg,xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel, yMax=yMax_i, yMin=yMin_i,
                      sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "line",class ="scenario", classPalette = classPalette,
                      facet_columns="region",facet_rows=NULL,
            dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/compareRegions/",gsub(" ","",paste(unique(unique(tbl$region)),collapse="")),"compareScen", sep = ""),
            fileName = paste(j,"_figLineOverlap_compareScenRegion",nameAppend,sep=""),
            figWidth = figWidth*figWMult,pdfpng=pdfpng, legendPosition=legendPosition, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
          )
        }

        #-------------------------
        # Diff Plots
        #------------------------

        if(is.null(scenRef)){
          print(paste("No reference scenario provided",sep=""))
          print(paste("Using ",unique(tbl_p$scenario)[1]," as reference",sep=""))
          scenRef_i = unique(tbl_p$scenario)[1]}else{
            if(!scenRef %in% unique(tbl_p$scenario)){
              print(paste("scenario ",scenRef," not in scenarios",sep=""))
              print(paste("Using ",unique(tbl_p$scenario)[1]," as reference",sep=""))
              scenRef_i = unique(tbl_p$scenario)[1]}else{
                scenRef_i <- scenRef}
          } # Check if Ref Scenario Chosen

        # Calculate Diff Values
        tbl_pd<-tbl_p%>%
          dplyr::filter(scenario==scenRef_i)%>%
          dplyr::select(-origScen,-origQuery,-origValue,-origUnits,-origX,-sources)
        if(!yData %in% names(tbl_p)){tbl_pd<-tbl_pd%>%dplyr::select(-dplyr::one_of(c(yData)))}

        for (k in unique(tbl_p$scenario)[unique(tbl_p$scenario)!=scenRef_i]){
          tbl_temp <- tbl_p%>%
            dplyr::filter(scenario %in% c(scenRef_i,k))%>%
            dplyr::select(-origScen,-origQuery,-origValue,-origUnits,-origX,-sources)
          if(!yData %in% names(tbl_temp)){tbl_temp<-tbl_temp%>%dplyr::select(-dplyr::one_of(c(yData)))}
          tbl_temp <- tbl_temp%>%
            tidyr::spread(scenario,yData)

          tbl_temp[is.na(tbl_temp)] <- 0

          tbl_temp <- tbl_temp %>%
            dplyr::mutate(!!paste(k,"_diff",sep=""):=get(k)-get(scenRef_i))%>%
            dplyr::select(-dplyr::one_of(c(k,scenRef_i)))
          tbl_temp<-tbl_temp%>%
            tidyr::gather(key=scenario,value=!!yData,
                          -c(names(tbl_temp)[!names(tbl_temp) %in% paste(k,"_diff",sep="")]))
          tbl_pd<-dplyr::bind_rows(tbl_pd,tbl_temp)
        }

        tbl_pd <-tbl_pd %>%
          dplyr::mutate(scenario=factor(scenario,
                                 levels=c(scenRef_i,
                                          unique(tbl_pd$scenario)[unique(tbl_pd$scenario)!=scenRef_i])))

        # Keep ref scenario
        tbl_pdx <- tbl_pd
        tbl_pdx<-droplevels(tbl_pdx)

        if(length(unique(tbl_pdx$class1))>1){if(grepl("right|left",legendPosition,ignore.case = T)){figWMult=1.3}else{figWMult=1}}else{figWMult=1}

        # Aggregated Class 1
        # Aggregate across classes
        tblAggsums<-tbl_pdx%>%
          dplyr::mutate(scenario=as.character(scenario))%>%
          dplyr::filter(aggregate=="sum")%>%
          dplyr::select(-class2,-classLabel2,-classPalette2)%>%
          dplyr::group_by_at(dplyr::vars(-value))%>%
          dplyr::summarize_at(c("value"),list(~sum(.)))
        tblAggmeans<-tbl_pdx%>%
          dplyr::mutate(scenario=as.character(scenario))%>%
          dplyr::filter(aggregate=="mean")%>%
          dplyr::select(-class2,-classLabel2,-classPalette2)%>%
          dplyr::group_by_at(dplyr::vars(-value))%>%
          dplyr::summarize_at(c("value"),list(~mean(.)))
        tbl_pdC1<-dplyr::bind_rows(tblAggsums,tblAggmeans)%>%dplyr::ungroup()

        # Bar Chart
        metis.chart(tbl_pdC1, xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel, yMax=yMax_i, yMin=yMin_i,
                    sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar", facet_rows="region", facet_columns="scenario",
                    dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/compareRegions/",gsub(" ","",paste(unique(unique(tbl$region)),collapse="")),"compareScen", sep = ""),
                    fileName = paste(j,"_figBarDiff_compareScenRegionREF",nameAppend,sep=""),
                    figWidth = figWidth*max((length(unique(tbl_pd$scenario))/2),1)*figWMult,forceFacets = T,
                    figHeight = figHeight*max((length(unique(tbl_pd$region))/2),1),pdfpng=pdfpng, legendPosition=legendPosition, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
        )

        # Line Chart
        metis.chart(tbl_pdC1, xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel, yMax=yMax_i, yMin=yMin_i,
                    sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "line", facet_rows="region", facet_columns="scenario",
                    dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/compareRegions/",gsub(" ","",paste(unique(unique(tbl$region)),collapse="")),"compareScen", sep = ""),
                    fileName = paste(j,"_figLineDiff_compareScenRegionREF",nameAppend,sep=""),
                    figWidth = figWidth*max((length(unique(tbl_pd$scenario))/2),1)*figWMult,forceFacets = T,
                    figHeight = figHeight*max((length(unique(tbl_pd$region))/2),1),pdfpng=pdfpng, legendPosition=legendPosition, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
        )

        # Keep ref scenario
        tbl_pdx <- tbl_pd
        tbl_pdx<-droplevels(tbl_pdx)

        if(length(unique(tbl_pdx$class1))>1){if(grepl("right|left",legendPosition,ignore.case = T)){figWMult=1.3}else{figWMult=1}}else{figWMult=1}

        # Aggregated Class 1
        # Aggregate across classes
        tblAggsums<-tbl_pdx%>%
          dplyr::mutate(scenario=as.character(scenario))%>%
          dplyr::filter(aggregate=="sum")%>%
          dplyr::select(-class2,-classLabel2,-classPalette2)%>%
          dplyr::group_by_at(dplyr::vars(-value))%>%
          dplyr::summarize_at(c("value"),list(~sum(.)))
        tblAggmeans<-tbl_pdx%>%
          dplyr::mutate(scenario=as.character(scenario))%>%
          dplyr::filter(aggregate=="mean")%>%
          dplyr::select(-class2,-classLabel2,-classPalette2)%>%
          dplyr::group_by_at(dplyr::vars(-value))%>%
          dplyr::summarize_at(c("value"),list(~mean(.)))
        tbl_pdC1<-dplyr::bind_rows(tblAggsums,tblAggmeans)%>%dplyr::ungroup()

        # Bar Chart
        metis.chart(tbl_pdC1, xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel, yMax=yMax_i, yMin=yMin_i,
                    sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar", facet_rows="region", facet_columns="scenario",
                    dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/compareRegions/",gsub(" ","",paste(unique(unique(tbl$region)),collapse="")),"compareScen", sep = ""),
                    fileName = paste(j,"_figBarDiff_compareScenRegionREF",nameAppend,sep=""),
                    figWidth = figWidth*max((length(unique(tbl_pd$scenario))/2),1)*figWMult,forceFacets = T,
                    figHeight = figHeight*max((length(unique(tbl_pd$region))/2),1),pdfpng=pdfpng, legendPosition=legendPosition, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
        )

        # Line Chart
        metis.chart(tbl_pdC1, xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel, yMax=yMax_i, yMin=yMin_i,
                    sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "line", facet_rows="region", facet_columns="scenario",
                    dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/compareRegions/",gsub(" ","",paste(unique(unique(tbl$region)),collapse="")),"compareScen", sep = ""),
                    fileName = paste(j,"_figLineDiff_compareScenRegionREF",nameAppend,sep=""),
                    figWidth = figWidth*max((length(unique(tbl_pd$scenario))/2),1)*figWMult,forceFacets = T,
                    figHeight = figHeight*max((length(unique(tbl_pd$region))/2),1),pdfpng=pdfpng, legendPosition=legendPosition, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
        )


        # Drop the ref scenario
        tbl_pdx <- tbl_pd %>%
          dplyr::filter(scenario!=scenRef_i)
        tbl_pdx<-droplevels(tbl_pdx)

        if(length(unique(tbl_pdx$class1))>1){if(grepl("right|left",legendPosition,ignore.case = T)){figWMult=1.3}else{figWMult=1}}else{figWMult=1}

        # Aggregated Class 1
        # Aggregate across classes
        tblAggsums<-tbl_pdx%>%
          dplyr::mutate(scenario=as.character(scenario))%>%
          dplyr::filter(aggregate=="sum")%>%
          dplyr::select(-class2,-classLabel2,-classPalette2)%>%
          dplyr::group_by_at(dplyr::vars(-value))%>%
          dplyr::summarize_at(c("value"),list(~sum(.)))
        tblAggmeans<-tbl_pdx%>%
          dplyr::mutate(scenario=as.character(scenario))%>%
          dplyr::filter(aggregate=="mean")%>%
          dplyr::select(-class2,-classLabel2,-classPalette2)%>%
          dplyr::group_by_at(dplyr::vars(-value))%>%
          dplyr::summarize_at(c("value"),list(~mean(.)))
        tbl_pdC1<-dplyr::bind_rows(tblAggsums,tblAggmeans)%>%dplyr::ungroup()

        # Bar Chart
        metis.chart(tbl_pdC1, xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel, yMax=yMax_i, yMin=yMin_i,
                    sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar", facet_rows="region", facet_columns="scenario",
          dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/compareRegions/",gsub(" ","",paste(unique(unique(tbl$region)),collapse="")),"compareScen", sep = ""),
          fileName = paste(j,"_figBarDiff_compareScenRegion",nameAppend,sep=""),
          figWidth = figWidth*((length(unique(tbl_pd$scenario)))/((length(unique(tbl_pd$scenario)))+1))*max((length(unique(tbl_pd$scenario))/2),1)*figWMult,forceFacets = T,
          figHeight = figHeight*max((length(unique(tbl_pd$region))/2),1),pdfpng=pdfpng, legendPosition=legendPosition, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
        )

        # Line Chart
        metis.chart(tbl_pdC1, xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel, yMax=yMax_i, yMin=yMin_i,
                    sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "line", facet_rows="region", facet_columns="scenario",
          dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/compareRegions/",gsub(" ","",paste(unique(unique(tbl$region)),collapse="")),"compareScen", sep = ""),
          fileName = paste(j,"_figLineDiff_compareScenRegion",nameAppend,sep=""),
          figWidth = figWidth*((length(unique(tbl_pd$scenario)))/((length(unique(tbl_pd$scenario)))+1))*max((length(unique(tbl_pd$scenario))/2),1)*figWMult,forceFacets = T,
          figHeight = figHeight*max((length(unique(tbl_pd$region))/2),1),pdfpng=pdfpng, legendPosition=legendPosition, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
        )

        #-------------------------
        # Aggregate and Plot Dodged/OverLapping Plots
        #------------------------

        # Aggregate across classes
        tbl_pdAggsums<-tbl_pdC1%>%
          dplyr::mutate(scenario=as.character(scenario))%>%
          dplyr::filter(aggregate=="sum")%>%
          dplyr::select(-tidyselect::contains("class"))%>%
          dplyr::group_by_at(dplyr::vars(-yData))%>%
          dplyr::summarize_at(c(yData),list(~sum(.)))
        tbl_pdAggmeans<-tbl_pdC1%>%
          dplyr::mutate(scenario=as.character(scenario))%>%
          dplyr::filter(aggregate=="mean")%>%
          dplyr::select(-tidyselect::contains("class"))%>%
          dplyr::group_by_at(dplyr::vars(-yData))%>%
          dplyr::summarize_at(c(yData),list(~mean(.)))
        tbl_pdAgg<-dplyr::bind_rows(tbl_pdAggsums,tbl_pdAggmeans)%>%dplyr::ungroup()


        if(nrow(tbl_pdAgg)>0){

          # Bar Chart Dodged
          metis.chart(tbl_pdAgg, xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel, sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar",
                      class ="scenario", position ="dodge", classPalette = classPalette,
                      facet_rows="region",facet_columns = "scenario",
                      dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/compareRegions/",gsub(" ","",paste(unique(unique(tbl$region)),collapse="")),"compareScen", sep = ""),
                      fileName = paste(j,"_figBarDodgedDiff_compareScenRegion",nameAppend,sep=""),forceFacets = T,
                      pdfpng=pdfpng, legendPosition=legendPosition, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
          )

          # Line Chart Overlapped
          metis.chart(tbl_pdAgg,xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel,
                      sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "line",class ="scenario", classPalette = classPalette,
                      facet_rows="region",facet_columns = "scenario",
                      dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/compareRegions/",gsub(" ","",paste(unique(unique(tbl$region)),collapse="")),"compareScen", sep = ""),
                      fileName = paste(j,"_figLineOverlapDiff_compareScenRegion",nameAppend,sep=""),forceFacets = T,
                      pdfpng=pdfpng, legendPosition=legendPosition, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
          )
        }


        # Calculate Percentage Diff Values
        tbl_pd<-tbl_p%>%
          dplyr::filter(scenario==scenRef_i)%>%
          dplyr::select(-origScen,-origQuery,-origValue,-origUnits,-origX,-sources)
        if(!yData %in% names(tbl_p)){tbl_pd<-tbl_pd%>%dplyr::select(-dplyr::one_of(c(yData)))}

        for (k in unique(tbl_p$scenario)[unique(tbl_p$scenario)!=scenRef_i]){
          tbl_temp <- tbl_p%>%
            dplyr::filter(scenario %in% c(scenRef_i,k))%>%
            dplyr::select(-origScen,-origQuery,-origValue,-origUnits,-origX,-sources)
          if(!yData %in% names(tbl_temp)){tbl_temp<-tbl_temp%>%dplyr::select(-dplyr::one_of(c(yData)))}
          tbl_temp <- tbl_temp%>%
            tidyr::spread(scenario,yData)

          tbl_temp[is.na(tbl_temp)] <- 0

          tbl_temp <- tbl_temp %>%
            dplyr::mutate(!!paste(k,"_diff_percent",sep=""):=(get(k)-get(scenRef_i))*100/get(scenRef_i))%>%
            dplyr::select(-dplyr::one_of(c(k,scenRef_i)))
          tbl_temp<-tbl_temp%>%
            tidyr::gather(key=scenario,value=!!yData,
                          -c(names(tbl_temp)[!names(tbl_temp) %in% paste(k,"_diff_percent",sep="")]))
          tbl_pd<-dplyr::bind_rows(tbl_pd,tbl_temp)
        }

        tbl_pd <-tbl_pd %>%
          dplyr::mutate(scenario=factor(scenario,
                                        levels=c(scenRef_i,
                                                 unique(tbl_pd$scenario)[unique(tbl_pd$scenario)!=scenRef_i])))

        # Keep ref scenario
        tbl_pdx <- tbl_pd
        tbl_pdx<-droplevels(tbl_pdx)

        if(length(unique(tbl_pdx$class1))>1){if(grepl("right|left",legendPosition,ignore.case = T)){figWMult=1.3}else{figWMult=1}}else{figWMult=1}

        # Aggregated Class 1
        # Aggregate across classes
        tblAggsums<-tbl_pdx%>%
          dplyr::mutate(scenario=as.character(scenario))%>%
          dplyr::filter(aggregate=="sum")%>%
          dplyr::select(-class2,-classLabel2,-classPalette2)%>%
          dplyr::group_by_at(dplyr::vars(-value))%>%
          dplyr::summarize_at(c("value"),list(~sum(.)))
        tblAggmeans<-tbl_pdx%>%
          dplyr::mutate(scenario=as.character(scenario))%>%
          dplyr::filter(aggregate=="mean")%>%
          dplyr::select(-class2,-classLabel2,-classPalette2)%>%
          dplyr::group_by_at(dplyr::vars(-value))%>%
          dplyr::summarize_at(c("value"),list(~mean(.)))
        tbl_pdC1<-dplyr::bind_rows(tblAggsums,tblAggmeans)%>%dplyr::ungroup()


        # Drop the ref scenario
        tbl_pdx <- tbl_pd %>%
          dplyr::filter(scenario!=scenRef_i)
        tbl_pdx<-droplevels(tbl_pdx)

        if(length(unique(tbl_pdx$class1))>1){if(grepl("right|left",legendPosition,ignore.case = T)){figWMult=1.3}else{figWMult=1}}else{figWMult=1}

        # Aggregated Class 1
        # Aggregate across classes
        tblAggsums<-tbl_pdx%>%
          dplyr::mutate(scenario=as.character(scenario))%>%
          dplyr::filter(aggregate=="sum")%>%
          dplyr::select(-class2,-classLabel2,-classPalette2)%>%
          dplyr::group_by_at(dplyr::vars(-value))%>%
          dplyr::summarize_at(c("value"),list(~sum(.)))
        tblAggmeans<-tbl_pdx%>%
          dplyr::mutate(scenario=as.character(scenario))%>%
          dplyr::filter(aggregate=="mean")%>%
          dplyr::select(-class2,-classLabel2,-classPalette2)%>%
          dplyr::group_by_at(dplyr::vars(-value))%>%
          dplyr::summarize_at(c("value"),list(~mean(.)))
        tbl_pdC1<-dplyr::bind_rows(tblAggsums,tblAggmeans)%>%dplyr::ungroup()


        # Bar Chart
        metis.chart(tbl_pdC1%>%dplyr::mutate(units="~Percent"), xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel,
                    sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar", facet_rows="region", facet_columns="scenario",
                    dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/compareRegions/",gsub(" ","",paste(unique(unique(tbl$region)),collapse="")),"compareScen", sep = ""),
                    fileName = paste(j,"_figBarDiffPrcnt_compareScenRegion",nameAppend,sep=""),
                    figWidth = figWidth*((length(unique(tbl_pd$scenario)))/((length(unique(tbl_pd$scenario)))+1))*max((length(unique(tbl_pd$scenario))/2),1)*figWMult,forceFacets = T,
                    figHeight = figHeight*max((length(unique(tbl_pd$region))/2),1),pdfpng=pdfpng, legendPosition=legendPosition, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
        )

        # Line Chart
        metis.chart(tbl_pdC1%>%dplyr::mutate(units="~Percent"), xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel,
                    sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "line", facet_rows="region", facet_columns="scenario",
                    dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/compareRegions/",gsub(" ","",paste(unique(unique(tbl$region)),collapse="")),"compareScen", sep = ""),
                    fileName = paste(j,"_figLineDiffPrcnt_compareScenRegion",nameAppend,sep=""),
                    figWidth = figWidth*((length(unique(tbl_pd$scenario)))/((length(unique(tbl_pd$scenario)))+1))*max((length(unique(tbl_pd$scenario))/2),1)*figWMult,forceFacets = T,
                    figHeight = figHeight*max((length(unique(tbl_pd$region))/2),1),pdfpng=pdfpng, legendPosition=legendPosition, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
        )
        #-------------------------
        # Aggregate and Plot Dodged/OverLapping Plots
        #------------------------


        # Aggregate across classes
        tblAggsums<-tbl_p%>%
          dplyr::select(-origScen,-origQuery,-origValue,-origUnits,-origX,-sources,-tidyselect::contains("class"))%>%
          dplyr::mutate(scenario=as.character(scenario))%>%
          dplyr::filter(aggregate=="sum")%>%
          dplyr::group_by_at(dplyr::vars(-value))%>%
          dplyr::summarize_at(c("value"),list(~sum(.)))
        tblAggmeans<-tbl_p%>%
          dplyr::select(-origScen,-origQuery,-origValue,-origUnits,-origX,-sources,-tidyselect::contains("class"))%>%
          dplyr::mutate(scenario=as.character(scenario))%>%
          dplyr::filter(aggregate=="mean")%>%
          dplyr::group_by_at(dplyr::vars(-value))%>%
          dplyr::summarize_at(c("value"),list(~mean(.)))
        tbl_pdx<-dplyr::bind_rows(tblAggsums,tblAggmeans)%>%dplyr::ungroup()


        # Calculate Percentage Diff Values
        tbl_pd<-tbl_pdx%>%
          dplyr::filter(scenario==scenRef_i)
          if(!yData %in% names(tbl_p)){tbl_pd<-tbl_pd%>%dplyr::select(-dplyr::one_of(c(yData)))}

        for (k in unique(tbl_p$scenario)[unique(tbl_p$scenario)!=scenRef_i]){
          tbl_temp <- tbl_pdx%>%
            dplyr::filter(scenario %in% c(scenRef_i,k))
            if(!yData %in% names(tbl_temp)){tbl_temp<-tbl_temp%>%dplyr::select(-dplyr::one_of(c(yData)))}
          tbl_temp <- tbl_temp%>%
            tidyr::spread(scenario,yData)

          tbl_temp[is.na(tbl_temp)] <- 0

          tbl_temp <- tbl_temp %>%
            dplyr::mutate(!!paste(k,"_diff_percent",sep=""):=(get(k)-get(scenRef_i))*100/get(scenRef_i))%>%
            dplyr::select(-dplyr::one_of(c(k,scenRef_i)))
          tbl_temp<-tbl_temp%>%
            tidyr::gather(key=scenario,value=!!yData,
                          -c(names(tbl_temp)[!names(tbl_temp) %in% paste(k,"_diff_percent",sep="")]))
          tbl_pd<-dplyr::bind_rows(tbl_pd,tbl_temp)
        }

        tbl_pd <-tbl_pd %>%
          dplyr::mutate(scenario=factor(scenario,
                                        levels=c(scenRef_i,
                                                 unique(tbl_pd$scenario)[unique(tbl_pd$scenario)!=scenRef_i])))

        # Keep ref scenario
        tbl_pdx <- tbl_pd %>% dplyr::ungroup()
        tbl_pdx<-droplevels(tbl_pdx)

        # Aggregated Class 1
        # Aggregate across classes
        tblAggsums<-tbl_pdx%>%
          dplyr::mutate(scenario=as.character(scenario))%>%
          dplyr::filter(aggregate=="sum")%>%
          dplyr::group_by_at(dplyr::vars(-value))%>%
          dplyr::summarize_at(c("value"),list(~sum(.)))
        tblAggmeans<-tbl_pdx%>%
          dplyr::mutate(scenario=as.character(scenario))%>%
          dplyr::filter(aggregate=="mean")%>%
          dplyr::group_by_at(dplyr::vars(-value))%>%
          dplyr::summarize_at(c("value"),list(~mean(.)))
        tbl_pdC1<-dplyr::bind_rows(tblAggsums,tblAggmeans)%>%dplyr::ungroup()


        if(nrow(tbl_pdC1)>0){

          # Bar Chart Dodged
          metis.chart(tbl_pdC1%>%dplyr::mutate(units="~Percent"), xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel,sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar",
                      class ="scenario", position ="dodge", classPalette = classPalette,
                      facet_rows="region",facet_columns="scenario",
                      dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/compareRegions/",gsub(" ","",paste(unique(unique(tbl$region)),collapse="")),"compareScen", sep = ""),
                      fileName = paste(j,"_figBarDodgedDiffPrcnt_compareScenRegionREF",nameAppend,sep=""),forceFacets = T,
                      pdfpng=pdfpng, legendPosition=legendPosition, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
          )

          # Line Chart Overlapped
          metis.chart(tbl_pdC1%>%dplyr::mutate(units="~Percent"),xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel,
                      sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "line",class ="scenario", classPalette = classPalette,
                      facet_rows="region",facet_columns="scenario",
                      dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/compareRegions/",gsub(" ","",paste(unique(unique(tbl$region)),collapse="")),"compareScen", sep = ""),
                      fileName = paste(j,"_figLineOverlapDiffPrcnt_compareScenRegionREF",nameAppend,sep=""),forceFacets = T,
                      pdfpng=pdfpng, legendPosition=legendPosition, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
          )

        # Drop the ref scenario
        tbl_pdx <- tbl_pd %>%
          dplyr::filter(scenario!=scenRef_i)%>%dplyr::ungroup()
        tbl_pdx<-droplevels(tbl_pdx)

        # Aggregated Class 1
        # Aggregate across classes
        tblAggsums<-tbl_pdx%>%
          dplyr::mutate(scenario=as.character(scenario))%>%
          dplyr::filter(aggregate=="sum")%>%
          dplyr::group_by_at(dplyr::vars(-value))%>%
          dplyr::summarize_at(c("value"),list(~sum(.)))
        tblAggmeans<-tbl_pdx%>%
          dplyr::mutate(scenario=as.character(scenario))%>%
          dplyr::filter(aggregate=="mean")%>%
          dplyr::group_by_at(dplyr::vars(-value))%>%
          dplyr::summarize_at(c("value"),list(~mean(.)))
        tbl_pdC1<-dplyr::bind_rows(tblAggsums,tblAggmeans)%>%dplyr::ungroup()


        if(nrow(tbl_pdC1)>0){

          # Bar Chart Dodged
          metis.chart(tbl_pdC1%>%dplyr::mutate(units="~Percent"), xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel,sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar",
                      class ="scenario", position ="dodge", classPalette = classPalette,
                      facet_rows="region",facet_columns="scenario",
                      dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/compareRegions/",gsub(" ","",paste(unique(unique(tbl$region)),collapse="")),"compareScen", sep = ""),
                      fileName = paste(j,"_figBarDodgedDiffPrcnt_compareScenRegion",nameAppend,sep=""),forceFacets = T,
                      pdfpng=pdfpng, legendPosition=legendPosition, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
          )

          # Line Chart Overlapped
          metis.chart(tbl_pdC1%>%dplyr::mutate(units="~Percent"),xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel,
                      sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "line",class ="scenario", classPalette = classPalette,
                      facet_rows="region",facet_columns="scenario",
                      dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/compareRegions/",gsub(" ","",paste(unique(unique(tbl$region)),collapse="")),"compareScen", sep = ""),
                      fileName = paste(j,"_figLineOverlapDiffPrcnt_compareScenRegion",nameAppend,sep=""),forceFacets = T,
                      pdfpng=pdfpng, legendPosition=legendPosition, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
          )
        }

        } # Close if(nrow(tbl_rsp)>0)
     } # Close loop if(length(unique(tbl$scenario))>1) to see if multiple scenarios for chosen param
    } # close loop for param
} # Close if multiple scenarios available

} # if length(unique(tbl$region))>1
}
} # Close loop for regionCompare

if(regionCompareOnly!=1){

#------------------
# Create Charts for Each Region & Each Scenario
#------------------

  mpParamPlots<-tibble::tibble()

for(i in unique(tbl$region)){
if(scenarioCompareOnly!=1){
  for(j in unique(tbl$scenario)){
    for(k in unique(tbl$param)){

    tbl_rsp<-tbl%>%dplyr::filter(region==i,
                          scenario==j,
                          param==k)

    if(k %in% unique(scaleRange$param)){
      yMax_i = scaleRange %>% dplyr::filter(param==k) %>% unique()
      if(nrow(yMax_i)==1){yMax_i = yMax_i$maxScale}else{
        yMax_i=NULL
        print("number of rows for scaleRange maxScale for param ", k," incorrect. Setting yMax to NULL")}

      yMin_i = scaleRange %>% dplyr::filter(param==k) %>% unique()
      if(nrow(yMin_i)==1){yMin_i = yMin_i$minScale}else{
        yMin_i=NULL
        print("number of rows for scaleRange minScale for param ", k," incorrect. Setting yMin to NULL")}
    }else{yMax_i=NULL;yMin_i=NULL}


    if(nrow(tbl_rsp)>0){

      if(length(unique(tbl_rsp$class1))>1){if(grepl("right|left",legendPosition,ignore.case = T)){figWMult=1.3}else{figWMult=1}}else{figWMult=1}

      # Aggregated Class 1
      # Aggregate across classes
      tblAggsums<-tbl_rsp%>%
        dplyr::mutate(scenario=as.character(scenario))%>%
        dplyr::filter(aggregate=="sum")%>%
        dplyr::select(-class2,-classLabel2,-classPalette2,-origValue,-origScen,-origQuery,-origUnits,-origX,-vintage)%>%
        dplyr::group_by_at(dplyr::vars(-value))%>%
        dplyr::summarize_at(c("value"),list(~sum(.)))
      tblAggmeans<-tbl_rsp%>%
        dplyr::mutate(scenario=as.character(scenario))%>%
        dplyr::filter(aggregate=="mean")%>%
        dplyr::select(-class2,-classLabel2,-classPalette2,-origValue,-origScen,-origQuery,-origUnits,-origX,-vintage)%>%
        dplyr::group_by_at(dplyr::vars(-value))%>%
        dplyr::summarize_at(c("value"),list(~mean(.)))
      tbl_rspC1<-dplyr::bind_rows(tblAggsums,tblAggmeans)%>%dplyr::ungroup()


      metis.chart(tbl_rspC1, xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel, yMax=yMax_i, yMin=yMin_i,sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar",
                  dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i, "/", j,sep = ""),
                  fileName = paste(k,"_figBar_",i,"_",j,nameAppend,sep=""),pdfpng=pdfpng, legendPosition=legendPosition, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
      )

    # Bar Chart
    metis.chart(tbl_rspC1%>%dplyr::mutate(label=units), xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel, yMax=yMax_i, yMin=yMin_i,sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar",
    dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i, "/", j,sep = ""),printFig = F,
    forceFacets = T, facet_columns="label",
    fileName = paste(k,"_figBar_",i,"_",j,nameAppend,sep=""),pdfpng=pdfpng, legendPosition=legendPosition, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
    )->mpx

    if(k %in% unlist(mp$param)){
      assign(paste("mp_Bar_",k,sep=""),mpx)
      mpParamPlots <- mpParamPlots %>%
        dplyr::bind_rows(tibble::tibble(param=c(k),
                                        plot=c(paste("mp_Bar_",k,sep="")),
                                        scen=c(rep(j,1)))); mpParamPlots
    }


    # data=tbl_rspC1; xData=xData;yData=yData;xLabel=xLabel;yLabel=yLabel;sizeBarLines=sizeBarLines;useNewLabels=useNewLabels;sizeLines=sizeLines; chartType = "bar";
    # dirOutputs = paste(dirOutputs, "/Charts/", i, "/", j,sep = "");
    # fileName = paste(k,"_figBar_",i,"_",j,nameAppend,sep="");pdfpng=pdfpng, legendPosition=legendPosition, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2

    metis.chart(tbl_rspC1,xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel, yMax=yMax_i, yMin=yMin_i,sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "line",
                dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i, "/", j,sep = ""),
                fileName = paste(k,"_figLine_",i,"_",j,nameAppend,sep=""),pdfpng=pdfpng, legendPosition=legendPosition, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
    )

    # Line Chart
    metis.chart(tbl_rspC1%>%dplyr::mutate(label=units),xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel, yMax=yMax_i, yMin=yMin_i,sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "line",
      dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i, "/", j,sep = ""),printFig = F,
      forceFacets = T, facet_columns="label",
      fileName = paste(k,"_figLine_",i,"_",j,nameAppend,sep=""),pdfpng=pdfpng, legendPosition=legendPosition, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
    )->mpx



    if(k %in% unlist(mp$param)){
      assign(paste("mp_Line_",k,sep=""),mpx)
      mpParamPlots <- mpParamPlots %>%
        dplyr::bind_rows(tibble::tibble(param=c(k),
                                        plot=c(paste("mp_Line_",k,sep="")),
                                        scen=c(rep(j,1)))); mpParamPlots
    }


    # Class 2 Charts
    if(length(unique(tbl_rsp$class2))>1){

      # Bar Chart
      metis.chart(tbl_rsp, xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel, yMax=yMax_i, yMin=yMin_i,sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar",
                  facet_columns = "class2", dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i, "/", j,sep = ""),
                  fileName = paste(k,"_figBar_",i,"_Class2_",j,nameAppend,sep="")
      )

      # Line Chart
      metis.chart(tbl_rsp,xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel, yMax=yMax_i, yMin=yMin_i,sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "line",
                  facet_columns = "class2", dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i, "/", j,sep = ""),
                  fileName = paste(k,"_figLine_",i,"_Class2_",j,nameAppend,sep="")
      )
    }

    } # Close if(nrow(tbl_rsp)>0)

} # close loop for param


    if(multiplotOn){
    #-------------------------------------
    # Multiplots per Region and chosen parameters
    #------------------------------------

    mpParamPlots # Multiplots recorded
    # Prepare MultiPlots for Each region across scenarios and params
    mpdf <- tibble::tibble();
    for(paramSet_i in 1:length(unique(unlist(mp$paramSet)))){
      for(param_i in 1:length(unique(unlist(mp$param)))){
        mpdf<-mpdf %>% dplyr::bind_rows(tibble::tibble(paramSet=mp$paramSet[[paramSet_i]],
                                                       param=mp$param[[paramSet_i]][param_i],
                                        nColMax = mp$nColMax[[paramSet_i]])) %>%
          dplyr::filter(!is.na(param))%>%unique()
      }};mpdf

    # Subset to available plots
    print(paste("Params selected but not available for multiplot are:"))
    print(paste(unique(mpdf$param)[!unique(mpdf$param) %in% unique(unlist(mpParamPlots$param))],collapse=", "))
    print(paste("Params available for multiplot are:"))
    print(paste(unique(mpdf$param)[unique(mpdf$param) %in% unique(unlist(mpParamPlots$param))],collapse=", "))

    mpdf <- mpdf %>% dplyr::filter(param %in% unique(unlist(mpParamPlots$param))); mpdf


    if(nrow(mpdf)>0){

      # Left_join paramSets with the paramPlot list
      mpParamPlotsx <- mpParamPlots %>%
        dplyr::left_join(mpdf, by="param")%>%
        unique(); mpParamPlotsx

      multiPlotFigs<-tibble::tibble()
      paramSetCurrent=mpdf[1,]$paramSet
      for(paramSet_i in unique(mpdf$paramSet)){

        mpParamPlots_i <- mpParamPlotsx %>%
          dplyr::filter(paramSet==paramSet_i)%>%unique();mpParamPlots_i
        plotSet_i <-unique(mpParamPlots_i$plot);plotSet_i

        # Plot diff 1 Scale
        multiplotlist_i<-list();
        plotSet_ix <- unique(plotSet_i[grepl("_Bar_",plotSet_i)])
        for(multiplot_i in 1:length(plotSet_ix)){
          multiplotlist_i[[multiplot_i]] <-get(plotSet_ix[multiplot_i])+ylab(NULL)
        };multiplotlist_i

        labels1 <-paste(letters[seq(from=1,to=length(multiplotlist_i),by=1)],")",sep=""); labels1

        # MultiPlots 1: Ref,diffs1scale,diffsPrcnt,leg
        #ncol
        if(length(plotSet_ix)>unique(mpParamPlots_i$nColMax)){ncol_i=unique(mpParamPlots_i$nColMax)}else{ncol_i=length(plotSet_ix)}; ncol_i
        if((length(plotSet_ix)/ncol_i)<1){nrow_i=1}else{nrow_i=round(length(plotSet_ix)/ncol_i)};nrow_i
        figure <- ggpubr::ggarrange(plotlist=multiplotlist_i,
                          ncol = ncol_i,nrow=nrow_i,
                         #labels="auto",
                         labels = labels1,
                         font.label=list(size=15*ncol_i*nrow_i*1),
                         hjust=-0.5,vjust=1.5,
                         legend="right"); figure

      metis.printPdfPng(figure=figure,
                          dir=paste(dirOutputs, "/Charts/",folderName,"/", i,"/multiPlot", sep = ""),
                          filename=paste(paramSet_i,"_",i,"_",j,"_mplot_Bar",sep=""),
                          figWidth=figWidth*ncol_i,
                          figHeight=figHeight*nrow_i,
                          pdfpng="png")

      # Plot diff 1 Scale
      multiplotlist_i<-list();
      plotSet_ix <- unique(plotSet_i[grepl("_Line_",plotSet_i)])
      for(multiplot_i in 1:length(plotSet_ix)){
        multiplotlist_i[[multiplot_i]] <-get(plotSet_ix[multiplot_i])
      };multiplotlist_i

      labels1 <-paste(letters[seq(from=1,to=length(multiplotlist_i),by=1)],")",sep=""); labels1

      # MultiPlots 1: Ref,diffs1scale,diffsPrcnt,leg
      #ncol
      if(length(plotSet_ix)>unique(mpParamPlots_i$nColMax)){ncol_i=unique(mpParamPlots_i$nColMax)}else{ncol_i=length(plotSet_ix)}; ncol_i
      if((length(plotSet_ix)/ncol_i)<1){nrow_i=1}else{nrow_i=round(length(plotSet_ix)/ncol_i)};nrow_i
      figure <- ggpubr::ggarrange(plotlist=multiplotlist_i,
                          ncol = ncol_i,nrow=nrow_i,
                          #labels="auto",
                          labels = labels1,
                          font.label=list(size=15*ncol_i*nrow_i*1),
                          hjust=-0.5,vjust=1.5,
                          legend="right"); figure

      metis.printPdfPng(figure=figure,
                        dir=paste(dirOutputs, "/Charts/",folderName,"/", i,"/multiPlot", sep = ""),
                        filename=paste(paramSet_i,"_",i,"_",j,"_mplot_Line",sep=""),
                        figWidth=figWidth*ncol_i,
                        figHeight=figHeight*nrow_i,
                        pdfpng="png")

      }
    }
}

  } # Close if statement for compareScenariosOnly
} # close loop for scenario
} # close loop for region

#------------------
# Compare Scenarios for each region
#------------------

if(length(unique(tbl$scenario))>1){

  mpParamPlots<-tibble::tibble()

for(i in unique(tbl$region)){
    for(j in unique(tbl$param)){

      tbl_rp<-tbl%>%dplyr::filter(region==i,
                                   param==j)

      if(j %in% unique(scaleRange$param)){
      yMax_i = scaleRange %>% dplyr::filter(param==j) %>% unique()
      if(nrow(yMax_i)==1){yMax_i = yMax_i$maxScale}else{
        yMax_i=NULL
        print("number of rows for scaleRange maxScale for param ", k," incorrect. Setting yMax to NULL")}

      yMin_i = scaleRange %>% dplyr::filter(param==j) %>% unique()
      if(nrow(yMin_i)==1){yMin_i = yMin_i$minScale}else{
        yMin_i=NULL
        print("number of rows for scaleRange minScale for param ", k," incorrect. Setting yMin to NULL")}
      }else{yMax_i=NULL;yMin_i=NULL}


      if(is.null(scenRef)){
        print(paste("No reference scenario provided",sep=""))
        print(paste("Using ",unique(tbl_rp$scenario)[1]," as reference",sep=""))
        scenRef_i = unique(tbl_rp$scenario)[1]}else{
          if(!scenRef %in% unique(tbl_rp$scenario)){
            print(paste("scenario ",scenRef," not in scenarios",sep=""))
            print(paste("Using ",unique(tbl_rp$scenario)[1]," as reference",sep=""))
            scenRef_i = unique(tbl_rp$scenario)[1]}else{
              scenRef_i <- scenRef}
        } # Check if Ref Scenario Chosen

      # Check for Different yLabels
      for (scen_i in unique(tbl_rp$scenario)){
        if(yLabel!="yLabel"){
        if(unique(tbl_rp%>%dplyr::filter(scenario==scenRef_i)%>%dplyr::select(yLabel))[1,1] !=
           unique(tbl_rp%>%dplyr::filter(scenario==scen_i)%>%dplyr::select(yLabel))[1,1]){
          print(paste("Warning. ",yLabel," in reference scenario '", scenRef_i, "': '",
                      unique(tbl_rp%>%dplyr::filter(scenario==scenRef_i)%>%dplyr::select(yLabel))[1,1],
                "' are not the same as ",yLabel," in diff scenario '", scen_i, "': '",
                unique(tbl_rp%>%dplyr::filter(scenario==scen_i)%>%dplyr::select(yLabel))[1,1],
                "'. Setting ",yLabel," to Ref Scenario Units.",
                sep=""))

          tbl_rp <- tbl_rp %>% dplyr::mutate(!!yLabel:=as.character(unique(tbl_rp%>%dplyr::filter(scenario==scenRef_i)%>%dplyr::select(yLabel))[1,1]))
          }}}


      if(length(unique((tbl_rp%>%dplyr::filter(value>0))$scenario))>1){

      if(nrow(tbl_rp)>0){

        if(length(unique(tbl_rp$class1))>1){if(grepl("right|left",legendPosition,ignore.case = T)){figWMult=1.3}else{figWMult=1}}else{figWMult=1}

        # Aggregated Class 1
        # Aggregate across classes
        tblAggsums<-tbl_rp%>%
          dplyr::mutate(scenario=as.character(scenario))%>%
          dplyr::filter(aggregate=="sum")%>%
          dplyr::select(-class2,-classLabel2,-classPalette2,-origValue,-origScen,-origQuery,-origUnits,-origX,-vintage)%>%
          dplyr::group_by_at(dplyr::vars(-value))%>%
          dplyr::summarize_at(c("value"),list(~sum(.)))
        tblAggmeans<-tbl_rp%>%
          dplyr::mutate(scenario=as.character(scenario))%>%
          dplyr::filter(aggregate=="mean")%>%
          dplyr::select(-class2,-classLabel2,-classPalette2,-origValue,-origScen,-origQuery,-origUnits,-origX,-vintage)%>%
          dplyr::group_by_at(dplyr::vars(-value))%>%
          dplyr::summarize_at(c("value"),list(~mean(.)))
        tbl_rpC1<-dplyr::bind_rows(tblAggsums,tblAggmeans)%>%dplyr::ungroup()


      # Bar Chart
      metis.chart(tbl_rpC1, xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel, yMax=yMax_i, yMin=yMin_i, sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar",
        dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i,"/compareScen",sep = ""),
        facet_columns="scenario",
        fileName = paste(j,"_figBar_",i,"_compareScen",nameAppend,sep=""),
        figWidth = figWidth*max((length(unique(tbl_rp$scenario))/2),1)*figWMult,
        figHeight = figHeight*max((length(unique(tbl_rp$region))/2),1),pdfpng=pdfpng, legendPosition=legendPosition, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
      )


      # data=tbl_rpC1; xData=xData;yData=yData;xLabel=xLabel;yLabel=yLabel; sizeBarLines=sizeBarLines;useNewLabels=useNewLabels;sizeLines=sizeLines; chartType = "bar";
      # dirOutputs = paste(dirOutputs, "/Charts/", i,"/compareScen",sep = "")
      # fileName = paste(j,"_figBar_",i,"_compareScen",nameAppend,sep="")
      # figWidth = figWidth*max((length(unique(tbl_rp$scenario))/2),1)*figWMult
      # figHeight = figHeight*max((length(unique(tbl_rp$region))/2),1)
      # pdfpng=pdfpng; legendPosition=legendPosition, colOrder1 = colOrder1; colOrderName1 = colOrderName1; colOrder2 = colOrder2; colOrderName2 = colOrderName2

      # Line Chart
      metis.chart(tbl_rpC1,xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel, yMax=yMax_i, yMin=yMin_i, sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "line",
        dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i,"/compareScen",sep = ""),
        facet_columns="scenario",
        fileName = paste(j,"_figLine_",i,"_compareScen",nameAppend,sep=""),
        figWidth = figWidth*max((length(unique(tbl_rp$scenario))/2),1)*figWMult,
        figHeight = figHeight*max((length(unique(tbl_rp$region))/2),1),pdfpng=pdfpng, legendPosition=legendPosition, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
      )->lineCompScen



#-------------------------
# Plot with Scenarios on X for Chosen Years
#------------------------

      if(!all(xCompare %in% unique(tbl_rp[[xData]]))){
      print(paste("xCompare not available in data: ", paste(xCompare[!(xCompare %in% unique(tbl_rp[[xData]]))],collapse=", "), sep=""))
      print(paste("Comparing for only: ",  paste(xCompare[(xCompare %in% unique(tbl_rp[[xData]]))],collapse=", "), sep=""))
      tbl_rpy <- tbl_rp%>%dplyr::filter(x %in% xCompare)}else{
        print(paste("Comparing for only: ",  paste(xCompare,collapse=", "), sep=""))
        tbl_rpy <- tbl_rp%>%dplyr::filter(x %in% xCompare)
      }

      if(length(unique(tbl_rpy$class1))>1){if(grepl("right|left",legendPosition,ignore.case = T)){figWMult=1.3}else{figWMult=1}}else{figWMult=1}

      # Aggregated Class 1
      # Aggregate across classes
      tblAggsums<-tbl_rpy%>%
        dplyr::mutate(scenario=as.character(scenario))%>%
        dplyr::filter(aggregate=="sum")%>%
        dplyr::select(-class2,-classLabel2,-classPalette2,-origValue,-origScen,-origQuery,-origUnits,-origX,-vintage)%>%
        dplyr::group_by_at(dplyr::vars(-value))%>%
        dplyr::summarize_at(c("value"),list(~sum(.)))
      tblAggmeans<-tbl_rpy%>%
        dplyr::mutate(scenario=as.character(scenario))%>%
        dplyr::filter(aggregate=="mean")%>%
        dplyr::select(-class2,-classLabel2,-classPalette2,-origValue,-origScen,-origQuery,-origUnits,-origX,-vintage)%>%
        dplyr::group_by_at(dplyr::vars(-value))%>%
        dplyr::summarize_at(c("value"),list(~mean(.)))
      tbl_rpyC1<-dplyr::bind_rows(tblAggsums,tblAggmeans)%>%dplyr::ungroup()


      # Bar Chart
      metis.chart(tbl_rpyC1, xData ="scenario", yData=yData,xLabel=xLabel,yLabel=yLabel, facetLabelSize = xScenCompFacetLabelSize,
                  sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar", facet_columns = xData,
       dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i,"/compareScen",sep = ""),
        fileName = paste(j,"_figBar_",i,"_compareScen_xScenSelectYears",nameAppend,sep=""),
        figWidth = figWidth*max((length(unique(tbl_rpy$x)[unique(tbl_rpy$x) %in% xCompare])/3),1)*figWMult,
        figHeight = figHeight*max((length(unique(tbl_rpy$region))/2),1),pdfpng=pdfpng, legendPosition=legendPosition, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
      )


#-------------------------
# Aggregate and Plot Dodged/OverLapping Plots
#------------------------

      # Aggregate across classes
      tbl_rpAggsums<-tbl_rp%>%
        dplyr::mutate(scenario=as.character(scenario))%>%
        dplyr::filter(aggregate=="sum")%>%
        dplyr::select(-tidyselect::contains("class"),-origValue,-origScen,-origQuery,-origUnits,-origX,-vintage)%>%
        dplyr::group_by_at(dplyr::vars(-yData))%>%
        dplyr::summarize_at(c(yData),list(~sum(.)))
      tbl_rpAggmeans<-tbl_rp%>%
        dplyr::mutate(scenario=as.character(scenario))%>%
        dplyr::filter(aggregate=="mean")%>%
        dplyr::select(-tidyselect::contains("class"),-origValue,-origScen,-origQuery,-origUnits,-origX,-vintage)%>%
        dplyr::group_by_at(dplyr::vars(-yData))%>%
        dplyr::summarize_at(c(yData),list(~mean(.)))
      tbl_rpAgg<-dplyr::bind_rows(tbl_rpAggsums,tbl_rpAggmeans)%>%dplyr::ungroup()


      if(nrow(tbl_rpAgg)>0){

        if(length(unique(tbl_rpAgg$class1))>1){if(grepl("right|left",legendPosition,ignore.case = T)){figWMult=1.3}else{figWMult=1}}else{figWMult=1}

      # Bar Chart Dodged
      metis.chart(tbl_rpAgg, xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel, yMax=yMax_i, yMin=yMin_i, sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar", facet_columns=NULL,
                  class ="scenario", position ="dodge", classPalette = classPalette,
        dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i,"/compareScen",sep = ""),
        fileName = paste(j,"_figBarDodged_",i,"_compareScen_",nameAppend,sep=""),
        figWidth = figWidth*figWMult,pdfpng=pdfpng, legendPosition=legendPosition, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
      )

      metis.chart(tbl_rpAgg,xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel, yMax=yMax_i, yMin=yMin_i, sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "line", facet_columns=NULL,
                  class ="scenario", classPalette = classPalette,
                  dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i,"/compareScen",sep = ""),
                  fileName = paste(j,"_figLineOverlap_",i,"_compareScen",nameAppend,sep=""),figWidth = figWidth*figWMult,
                  pdfpng=pdfpng, legendPosition=legendPosition, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
      )

      # Line Chart Overlapped
      metis.chart(tbl_rpAgg%>%dplyr::mutate(label=paste(units)),xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel, yMax=yMax_i, yMin=yMin_i, sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "line",
                  class ="scenario", classPalette = classPalette,
        dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i,"/compareScen",sep = ""),
        fileName = paste(j,"_figLineOverlap_",i,"_compareScen",nameAppend,sep=""),figWidth = figWidth*figWMult,
        printFig = F, forceFacets = T, facet_columns="label",
        pdfpng=pdfpng, legendPosition=legendPosition, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
      )->mpx

      if(j %in% unlist(mp$param)){
        mpnScen<-length(unique(tbl_rpAgg$scenario))
        assign(paste("mp_lineSumAbs_",j,sep=""),mpx)
        mpParamPlots <- mpParamPlots %>%
          dplyr::bind_rows(tibble::tibble(param=c(j),
                                          plot=c(paste("mp_lineSumAbs_",j,sep="")),
                                          nScen=c(rep(mpnScen,1)))); mpParamPlots
      }

      }

#-------------------------
# Diff Plots
#------------------------


      # Aggregate across classes
      tbl_rpAggsums<-tbl_rp%>%
        dplyr::mutate(scenario=as.character(scenario))%>%
        dplyr::filter(aggregate=="sum")%>%
        dplyr::select(-class2,-classLabel2,-classPalette2,-classLabel1,-classPalette1,-origValue,-origScen,-origQuery,-origUnits,-origX,-vintage,-xLabel)%>%
        dplyr::group_by_at(dplyr::vars(-yData))%>%
        dplyr::summarize_at(c(yData),list(~sum(.)))
      tbl_rpAggmeans<-tbl_rp%>%
        dplyr::mutate(scenario=as.character(scenario))%>%
        dplyr::filter(aggregate=="mean")%>%
        dplyr::select(-class2,-classLabel2,-classPalette2,-classLabel1,-classPalette1,-origValue,-origScen,-origQuery,-origUnits,-origX,-vintage, -xLabel)%>%
        dplyr::group_by_at(dplyr::vars(-yData))%>%
        dplyr::summarize_at(c(yData),list(~mean(.)))
      tbl_rpAgg<-dplyr::bind_rows(tbl_rpAggsums,tbl_rpAggmeans)%>%dplyr::ungroup()


      # Calculate Diff Values
      tbl_rpd<-tbl_rpAgg%>%
        dplyr::filter(scenario==scenRef_i)
      if(!yData %in% names(tbl_rpAgg)){tbl_rpd<-tbl_rpd%>%dplyr::select(-dplyr::one_of(c(yData)))}

      tbl_rpd_fixedCols <- tbl_rp %>%
        dplyr::filter(scenario==scenRef_i) %>%
        dplyr::select("param","xLabel","classLabel1","classPalette1","classLabel2","classPalette2") %>% unique()

      for (k in unique(tbl_rpAgg$scenario)[unique(tbl_rpAgg$scenario)!=scenRef_i]){
        tbl_temp <- tbl_rpAgg%>%
          dplyr::filter(scenario %in% c(scenRef_i,k))
        if(!yData %in% names(tbl_temp)){tbl_temp<-tbl_temp%>%dplyr::select(-dplyr::one_of(c(yData)))}
        tbl_temp <- tbl_temp%>%
          tidyr::spread(scenario,yData)

        tbl_temp[is.na(tbl_temp)] <- 0

        tbl_temp <- tbl_temp %>%
          dplyr::mutate(!!paste(k,"_diff",sep=""):=get(k)-get(scenRef_i))%>%
          dplyr::select(-dplyr::one_of(c(k,scenRef_i)))
        tbl_temp<-tbl_temp%>%
          tidyr::gather(key=scenario,value=!!yData,
                        -c(names(tbl_temp)[!names(tbl_temp) %in% paste(k,"_diff",sep="")]))
        tbl_rpd<-dplyr::bind_rows(tbl_rpd,tbl_temp)
      }


      # Join relevant colors and classes using the mapping file if it exists
      for(missing_i in c( "xLabel","classLabel1","classPalette1","classLabel2","classPalette2")){
          if(!missing_i %in% names( tbl_rpd))
            tbl_rpd<- tbl_rpd%>%dplyr::left_join(tbl_rpd_fixedCols%>%dplyr::select(param,missing_i)%>%dplyr::distinct(),by=c("param"))}

      tbl_rpd <-tbl_rpd %>%
        dplyr::mutate(scenario=factor(scenario,
                               levels=c(scenRef_i,
                                        unique(tbl_rpd$scenario)[unique(tbl_rpd$scenario)!=scenRef_i])))


      # Keep ref scenario
      tbl_rpdx <- tbl_rpd %>% dplyr::ungroup()
      tbl_rpdx<-droplevels(tbl_rpdx)

      if(length(unique(tbl_rpdx$class1))>1){if(grepl("right|left",legendPosition,ignore.case = T)){figWMult=1.3}else{figWMult=1}}else{figWMult=1}

      # Aggregated Class 1
      # Aggregate across classes
      tblAggsums<-tbl_rpdx%>%
        dplyr::mutate(scenario=as.character(scenario))%>%
        dplyr::filter(aggregate=="sum")%>%
        dplyr::select(-classLabel2,-classPalette2)%>%
        dplyr::group_by_at(dplyr::vars(-value))%>%
        dplyr::summarize_at(c("value"),list(~sum(.)))
      tblAggmeans<-tbl_rpdx%>%
        dplyr::mutate(scenario=as.character(scenario))%>%
        dplyr::filter(aggregate=="mean")%>%
        dplyr::select(-classLabel2,-classPalette2)%>%
        dplyr::group_by_at(dplyr::vars(-value))%>%
        dplyr::summarize_at(c("value"),list(~mean(.)))
      tbl_rpdC1<-dplyr::bind_rows(tblAggsums,tblAggmeans)%>%dplyr::ungroup()


      # Bar Chart
      metis.chart(tbl_rpdC1, xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel, sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar",
                  dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i,"/compareScen",sep = ""),
                  facet_columns="scenario",
                  fileName = paste(j,"_figBarDiff_",i,"_compareScen1Scale",nameAppend,sep=""),
                  figWidth = figWidth*max((length(unique(tbl_rpd$scenario))/2),1)*figWMult,forceFacets = T,
                  figHeight = figHeight*max((length(unique(tbl_rpd$region))/2),1),pdfpng=pdfpng, legendPosition=legendPosition, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
      )->mpx

      metis.chart(tbl_rpdC1%>%dplyr::filter(scenario %in% scenRef_i),printFig=F, xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel, sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar",
                  dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i,"/compareScen",sep = ""),
                  facet_columns="scenario",
                  fileName = paste(j,"_figBarDiff_",i,"_compareScen1Scale",nameAppend,sep=""),
                  figWidth = figWidth*max((length(unique(tbl_rpd$scenario))/2),1)*figWMult,forceFacets = T,
                  figHeight = figHeight*max((length(unique(tbl_rpd$region))/2),1),pdfpng=pdfpng, legendPosition=legendPosition, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
      )->mpxR


      if(j %in% unlist(mp$param)){
        assign(paste("mp_barDiffAbs1Scale_",j,sep=""),mpx)
        assign(paste("mp_barDiffAbs1ScaleRef_",j,sep=""),mpxR)
        mpnScen<-length(unique(tbl_rpdC1$scenario))
        mpParamPlots <- mpParamPlots %>%
          dplyr::bind_rows(tibble::tibble(param=c(j,j),
                                          plot=c(paste("mp_barDiffAbs1Scale_",j,sep=""),
                                                 paste("mp_barDiffAbs1ScaleRef_",j,sep="")),
                                          nScen=c(rep(mpnScen,2)))); mpParamPlots
      }

      # Line Chart
      metis.chart(tbl_rpdC1, xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel, sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "line",
                  dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i,"/compareScen",sep = ""),
                  facet_columns="scenario",
                  fileName = paste(j,"_figLineDiff_",i,"_compareScen1Scale",nameAppend,sep=""),
                  figWidth = figWidth*max((length(unique(tbl_rpd$scenario))/2),1)*figWMult,forceFacets = T,
                  figHeight = figHeight*max((length(unique(tbl_rpd$region))/2),1),pdfpng=pdfpng, legendPosition=legendPosition, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
      )

      # Drop the ref scenario
      tbl_rpdx <- tbl_rpd %>%
        dplyr::filter(scenario!=scenRef_i)
      tbl_rpdx<-droplevels(tbl_rpdx)

      if(length(unique(tbl_rpdx$class1))>1){if(grepl("right|left",legendPosition,ignore.case = T)){figWMult=1.3}else{figWMult=1}}else{figWMult=1}

      # Aggregated Class 1
      # Aggregate across classes
      tblAggsums<-tbl_rpdx%>%
        dplyr::mutate(scenario=as.character(scenario))%>%
        dplyr::filter(aggregate=="sum")%>%
        dplyr::select(-classLabel2,-classPalette2)%>%
        dplyr::group_by_at(dplyr::vars(-value))%>%
        dplyr::summarize_at(c("value"),list(~sum(.)))
      tblAggmeans<-tbl_rpdx%>%
        dplyr::mutate(scenario=as.character(scenario))%>%
        dplyr::filter(aggregate=="mean")%>%
        dplyr::select(-classLabel2,-classPalette2)%>%
        dplyr::group_by_at(dplyr::vars(-value))%>%
        dplyr::summarize_at(c("value"),list(~mean(.)))
      tbl_rpdC1<-dplyr::bind_rows(tblAggsums,tblAggmeans)%>%dplyr::ungroup()


      # Bar Chart
      metis.chart(tbl_rpdC1, xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel, sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar",
        dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i,"/compareScen",sep = ""),
        facet_columns="scenario",
        fileName = paste(j,"_figBarDiff_",i,"_compareScen",nameAppend,sep=""),
        figWidth = figWidth*((length(unique(tbl_rpd$scenario)))/((length(unique(tbl_rpd$scenario)))+1))*max((length(unique(tbl_rpd$scenario))/2),1)*figWMult,forceFacets = T,
        figHeight = figHeight*max((length(unique(tbl_rpd$region))/2),1),pdfpng=pdfpng, legendPosition=legendPosition, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
      )->mpx

      if(j %in% unlist(mp$param)){
        mpnScen<-length(unique(tbl_rpdC1$scenario))
        assign(paste("mp_barDiffAbs_",j,sep=""),mpx)
        mpParamPlots <- mpParamPlots %>%
          dplyr::bind_rows(tibble::tibble(param=c(j),
                                          plot=c(paste("mp_barDiffAbs_",j,sep="")),
                           nScen=c(rep(mpnScen,1)))); mpParamPlots
      }

      # Line Chart
     metis.chart(tbl_rpdC1, xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel, sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "line",
        dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i,"/compareScen",sep = ""),
        facet_columns="scenario",
        fileName = paste(j,"_figLineDiff_",i,"_compareScen",nameAppend,sep=""),
        figWidth = figWidth*((length(unique(tbl_rpd$scenario)))/((length(unique(tbl_rpd$scenario)))+1))*max((length(unique(tbl_rpd$scenario))/2),1)*figWMult,forceFacets = T,
        figHeight = figHeight*max((length(unique(tbl_rpd$region))/2),1),pdfpng=pdfpng, legendPosition=legendPosition, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
      )


     #-------------------------
     # Aggregate and Plot Dodged/OverLapping Plots
     #------------------------

     # Aggregate across classes
     tbl_rpdAggsums<-tbl_rpdC1%>%
       dplyr::mutate(scenario=as.character(scenario))%>%
       dplyr::filter(aggregate=="sum")%>%
       dplyr::select(-tidyselect::contains("class"))%>%
       dplyr::group_by_at(dplyr::vars(-yData))%>%
       dplyr::summarize_at(c(yData),list(~sum(.)))
     tbl_rpdAggmeans<-tbl_rpdC1%>%
       dplyr::mutate(scenario=as.character(scenario))%>%
       dplyr::filter(aggregate=="mean")%>%
       dplyr::select(-tidyselect::contains("class"))%>%
       dplyr::group_by_at(dplyr::vars(-yData))%>%
       dplyr::summarize_at(c(yData),list(~mean(.)))
     tbl_rpdAgg<-dplyr::bind_rows(tbl_rpdAggsums,tbl_rpdAggmeans)%>%dplyr::ungroup()


     if(nrow(tbl_rpdAgg)>0){

       if(length(unique(tbl_rpdAgg$class1))>1){if(grepl("right|left",legendPosition,ignore.case = T)){figWMult=1.3}else{figWMult=1}}else{figWMult=1}

       # Bar Chart Dodged
       metis.chart(tbl_rpdAgg, xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel, sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar", facet_columns=NULL,
                   class ="scenario", position ="dodge", classPalette = classPalette,
                   dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i,"/compareScen",sep = ""),
                   fileName = paste(j,"_figBarDodgedDiff_",i,"_compareScen_",nameAppend,sep=""), forceFacets = T,
                   pdfpng=pdfpng, legendPosition=legendPosition, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
       )

       # Line Chart Overlapped
       metis.chart(tbl_rpdAgg,xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel,sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "line", facet_columns=NULL,
                   class ="scenario", classPalette = classPalette,
                   dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i,"/compareScen",sep = ""),
                   fileName = paste(j,"_figLineOverlapDiff_",i,"_compareScen",nameAppend,sep=""),
                   forceFacets = T,
                   pdfpng=pdfpng, legendPosition=legendPosition, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
       )

       # Line Chart Overlapped
       metis.chart(tbl_rpdAgg%>%dplyr::mutate(label="Absolute Difference"),xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel,sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "line",
                   class ="scenario", classPalette = classPalette,
                   dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i,"/compareScen",sep = ""),
                   fileName = paste(j,"_figLineOverlapDiff_",i,"_compareScen",nameAppend,sep=""),
                   forceFacets = T,printFig = F,facet_columns="label",
                   pdfpng=pdfpng, legendPosition=legendPosition, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
       )->mpx

       if(j %in% unlist(mp$param)){
         mpnScen<-length(unique(tbl_rpdAgg$scenario))
         assign(paste("mp_lineSumDiff_",j,sep=""),mpx)
         mpParamPlots <- mpParamPlots %>%
           dplyr::bind_rows(tibble::tibble(param=c(j),
                                           plot=c(paste("mp_lineSumDiff_",j,sep="")),
                                           nScen=c(rep(mpnScen,1)))); mpParamPlots
       }
     }

     #--------------------
     # Percent Diff Plots
     #---------------------


     # Aggregate across classes
     tbl_rpAggsums<-tbl_rp%>%
       dplyr::mutate(scenario=as.character(scenario))%>%
       dplyr::filter(aggregate=="sum")%>%
       dplyr::select(-class2,-classLabel2,-classPalette2,-classLabel1,-classPalette1,-origValue,-origScen,-origQuery,-origUnits,-origX,-vintage,-xLabel)%>%
       dplyr::group_by_at(dplyr::vars(-yData))%>%
       dplyr::summarize_at(c(yData),list(~sum(.)))
     tbl_rpAggmeans<-tbl_rp%>%
       dplyr::mutate(scenario=as.character(scenario))%>%
       dplyr::filter(aggregate=="mean")%>%
       dplyr::select(-class2,-classLabel2,-classPalette2,-classLabel1,-classPalette1,-origValue,-origScen,-origQuery,-origUnits,-origX,-vintage, -xLabel)%>%
       dplyr::group_by_at(dplyr::vars(-yData))%>%
       dplyr::summarize_at(c(yData),list(~mean(.)))
     tbl_rpAgg<-dplyr::bind_rows(tbl_rpAggsums,tbl_rpAggmeans)%>%dplyr::ungroup()


     # Calculate Diff Values
     tbl_rpd<-tbl_rpAgg%>%
       dplyr::filter(scenario==scenRef_i)
     if(!yData %in% names(tbl_rpAgg)){tbl_rpd<-tbl_rpd%>%dplyr::select(-dplyr::one_of(c(yData)))}

     tbl_rpd_fixedCols <- tbl_rp %>%
       dplyr::filter(scenario==scenRef_i) %>%
       dplyr::select("param","xLabel","classLabel1","classPalette1","classLabel2","classPalette2") %>% unique()

     for (k in unique(tbl_rpAgg$scenario)[unique(tbl_rpAgg$scenario)!=scenRef_i]){
       tbl_temp <- tbl_rpAgg%>%
         dplyr::filter(scenario %in% c(scenRef_i,k))
       if(!yData %in% names(tbl_temp)){tbl_temp<-tbl_temp%>%dplyr::select(-dplyr::one_of(c(yData)))}
       tbl_temp <- tbl_temp%>%
         tidyr::spread(scenario,yData)

       tbl_temp[is.na(tbl_temp)] <- 0

       tbl_temp <- tbl_temp %>%
         dplyr::mutate(!!paste(k,"_diff_prcnt",sep=""):=(get(k)-get(scenRef_i))*100/get(scenRef_i))%>%
         dplyr::select(-dplyr::one_of(c(k,scenRef_i)))
       tbl_temp<-tbl_temp%>%
         tidyr::gather(key=scenario,value=!!yData,
                       -c(names(tbl_temp)[!names(tbl_temp) %in% paste(k,"_diff_prcnt",sep="")]))
       tbl_rpd<-dplyr::bind_rows(tbl_rpd,tbl_temp)
     }


     # Join relevant colors and classes using the mapping file if it exists
     for(missing_i in c( "xLabel","classLabel1","classPalette1","classLabel2","classPalette2")){
       if(!missing_i %in% names( tbl_rpd))
         tbl_rpd<- tbl_rpd%>%dplyr::left_join(tbl_rpd_fixedCols%>%dplyr::select(param,missing_i)%>%dplyr::distinct(),by=c("param"))}

     tbl_rpd <-tbl_rpd %>%
       dplyr::mutate(scenario=factor(scenario,
                                     levels=c(scenRef_i,
                                              unique(tbl_rpd$scenario)[unique(tbl_rpd$scenario)!=scenRef_i])))


     # Keep ref scenario
     tbl_rpdx <- tbl_rpd %>% dplyr::ungroup()
     tbl_rpdx<-droplevels(tbl_rpdx)

     if(length(unique(tbl_rpdx$class1))>1){if(grepl("right|left",legendPosition,ignore.case = T)){figWMult=1.3}else{figWMult=1}}else{figWMult=1}

     # Aggregated Class 1
     # Aggregate across classes
     tblAggsums<-tbl_rpdx%>%
       dplyr::mutate(scenario=as.character(scenario))%>%
       dplyr::filter(aggregate=="sum")%>%
       dplyr::select(-classLabel2,-classPalette2)%>%
       dplyr::group_by_at(dplyr::vars(-value))%>%
       dplyr::summarize_at(c("value"),list(~sum(.)))
     tblAggmeans<-tbl_rpdx%>%
       dplyr::mutate(scenario=as.character(scenario))%>%
       dplyr::filter(aggregate=="mean")%>%
       dplyr::select(-classLabel2,-classPalette2)%>%
       dplyr::group_by_at(dplyr::vars(-value))%>%
       dplyr::summarize_at(c("value"),list(~mean(.)))
     tbl_rpdC1<-dplyr::bind_rows(tblAggsums,tblAggmeans)%>%dplyr::ungroup()


     # Bar Chart
     metis.chart(tbl_rpdC1, xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel, sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar",
                 dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i,"/compareScen",sep = ""),
                 facet_columns="scenario",
                 fileName = paste(j,"_figBarDiffPrcnt_",i,"_compareScen1Scale",nameAppend,sep=""),
                 figWidth = figWidth*max((length(unique(tbl_rpd$scenario))/2),1)*figWMult,forceFacets = T,
                 figHeight = figHeight*max((length(unique(tbl_rpd$region))/2),1),pdfpng=pdfpng, legendPosition=legendPosition, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
     )

     # Line Chart
     metis.chart(tbl_rpdC1, xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel, sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "line",
                 dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i,"/compareScen",sep = ""),
                 facet_columns="scenario",
                 fileName = paste(j,"_figLineDiffPrcnt_",i,"_compareScen1Scale",nameAppend,sep=""),
                 figWidth = figWidth*max((length(unique(tbl_rpd$scenario))/2),1)*figWMult,forceFacets = T,
                 figHeight = figHeight*max((length(unique(tbl_rpd$region))/2),1),pdfpng=pdfpng, legendPosition=legendPosition, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
     )


     # Drop the ref scenario
     tbl_rpdx <- tbl_rpd %>%
       dplyr::filter(scenario!=scenRef_i)
     tbl_rpdx<-droplevels(tbl_rpdx)

     if(length(unique(tbl_rpdx$class1))>1){if(grepl("right|left",legendPosition,ignore.case = T)){figWMult=1.3}else{figWMult=1}}else{figWMult=1}

     # Aggregated Class 1
     # Aggregate across classes
     tblAggsums<-tbl_rpdx%>%
       dplyr::mutate(scenario=as.character(scenario))%>%
       dplyr::filter(aggregate=="sum")%>%
       dplyr::select(-classLabel2,-classPalette2)%>%
       dplyr::group_by_at(dplyr::vars(-value))%>%
       dplyr::summarize_at(c("value"),list(~sum(.)))
     tblAggmeans<-tbl_rpdx%>%
       dplyr::mutate(scenario=as.character(scenario))%>%
       dplyr::filter(aggregate=="mean")%>%
       dplyr::select(-classLabel2,-classPalette2)%>%
       dplyr::group_by_at(dplyr::vars(-value))%>%
       dplyr::summarize_at(c("value"),list(~mean(.)))
     tbl_rpdC1<-dplyr::bind_rows(tblAggsums,tblAggmeans)%>%dplyr::ungroup()


     # Bar Chart
     metis.chart(tbl_rpdC1, xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel, sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar",
                 dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i,"/compareScen",sep = ""),
                 facet_columns="scenario",
                 fileName = paste(j,"_figBarDiffPrcnt_",i,"_compareScen",nameAppend,sep=""),
                 figWidth = figWidth*((length(unique(tbl_rpd$scenario)))/((length(unique(tbl_rpd$scenario)))+1))*max((length(unique(tbl_rpd$scenario))/2),1)*figWMult,forceFacets = T,
                 figHeight = figHeight*max((length(unique(tbl_rpd$region))/2),1),pdfpng=pdfpng, legendPosition=legendPosition, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
     )

     # Line Chart
     metis.chart(tbl_rpdC1, xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel, sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "line",
                 dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i,"/compareScen",sep = ""),
                 facet_columns="scenario",
                 fileName = paste(j,"_figLineDiffPrcnt_",i,"_compareScen",nameAppend,sep=""),
                 figWidth = figWidth*((length(unique(tbl_rpd$scenario)))/((length(unique(tbl_rpd$scenario)))+1))*max((length(unique(tbl_rpd$scenario))/2),1)*figWMult,forceFacets = T,
                 figHeight = figHeight*max((length(unique(tbl_rpd$region))/2),1),pdfpng=pdfpng, legendPosition=legendPosition, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
     )->mpx

     if(j %in% unlist(mp$param)){
       mpnScen<-length(unique(tbl_rpdC1$scenario))
       assign(paste("mp_lineDiffPrcnt_",j,sep=""),mpx)
       mpParamPlots <- mpParamPlots %>%
         dplyr::bind_rows(tibble::tibble(param=c(j),
                                         plot=c(paste("mp_lineDiffPrcnt_",j,sep="")),
                          nScen=c(rep(mpnScen,1)))); mpParamPlots
     }


     #-------------------------
     # Aggregate and Plot Dodged/OverLapping Plots
     #------------------------

     # Aggregate across classes
     tbl_rpAggsums<-tbl_rp%>%
       dplyr::mutate(scenario=as.character(scenario))%>%
       dplyr::filter(aggregate=="sum")%>%
       dplyr::select(-tidyselect::contains("class"),-origValue,-origScen,-origQuery,-origUnits,-origX,-vintage,-xLabel)%>%
       dplyr::group_by_at(dplyr::vars(-yData))%>%
       dplyr::summarize_at(c(yData),list(~sum(.)))
     tbl_rpAggmeans<-tbl_rp%>%
       dplyr::mutate(scenario=as.character(scenario))%>%
       dplyr::filter(aggregate=="mean")%>%
       dplyr::select(-tidyselect::contains("class"),-origValue,-origScen,-origQuery,-origUnits,-origX,-vintage, -xLabel)%>%
       dplyr::group_by_at(dplyr::vars(-yData))%>%
       dplyr::summarize_at(c(yData),list(~mean(.)))
     tbl_rpAgg<-dplyr::bind_rows(tbl_rpAggsums,tbl_rpAggmeans)%>%dplyr::ungroup()


     # Calculate Diff Values
     tbl_rpd<-tbl_rpAgg%>%
       dplyr::filter(scenario==scenRef_i)
     if(!yData %in% names(tbl_rpAgg)){tbl_rpd<-tbl_rpd%>%dplyr::select(-dplyr::one_of(c(yData)))}

     tbl_rpd_fixedCols <- tbl_rp %>%
       dplyr::filter(scenario==scenRef_i) %>%
       dplyr::select("param","xLabel","classLabel1","classPalette1","classLabel2","classPalette2") %>% unique()

     for (k in unique(tbl_rpAgg$scenario)[unique(tbl_rpAgg$scenario)!=scenRef_i]){
       tbl_temp <- tbl_rpAgg%>%
         dplyr::filter(scenario %in% c(scenRef_i,k))
       if(!yData %in% names(tbl_temp)){tbl_temp<-tbl_temp%>%dplyr::select(-dplyr::one_of(c(yData)))}
       tbl_temp <- tbl_temp%>%
         tidyr::spread(scenario,yData)

       tbl_temp[is.na(tbl_temp)] <- 0

       tbl_temp <- tbl_temp %>%
         dplyr::mutate(!!paste(k,"_diff_prcnt",sep=""):=(get(k)-get(scenRef_i))*100/get(scenRef_i))%>%
         dplyr::select(-dplyr::one_of(c(k,scenRef_i)))
       tbl_temp<-tbl_temp%>%
         tidyr::gather(key=scenario,value=!!yData,
                       -c(names(tbl_temp)[!names(tbl_temp) %in% paste(k,"_diff_prcnt",sep="")]))
       tbl_rpd<-dplyr::bind_rows(tbl_rpd,tbl_temp)
     }


     # Join relevant colors and classes using the mapping file if it exists
     for(missing_i in c( "xLabel","classLabel1","classPalette1","classLabel2","classPalette2")){
       if(!missing_i %in% names( tbl_rpd))
         tbl_rpd<- tbl_rpd%>%dplyr::left_join(tbl_rpd_fixedCols%>%dplyr::select(param,missing_i)%>%dplyr::distinct(),by=c("param"))}

     tbl_rpd <-tbl_rpd %>%
       dplyr::mutate(scenario=factor(scenario,
                                     levels=c(scenRef_i,
                                              unique(tbl_rpd$scenario)[unique(tbl_rpd$scenario)!=scenRef_i])))

     # Keep ref scenario
     tbl_rpdx <- tbl_rpd %>% dplyr::ungroup()
     tbl_rpdx<-droplevels(tbl_rpdx)

     if(length(unique(tbl_rpdx$class1))>1){if(grepl("right|left",legendPosition,ignore.case = T)){figWMult=1.3}else{figWMult=1}}else{figWMult=1}

     # Aggregated Class 1
     # Aggregate across classes
     tblAggsums<-tbl_rpdx%>%
       dplyr::mutate(scenario=as.character(scenario))%>%
       dplyr::filter(aggregate=="sum")%>%
       dplyr::group_by_at(dplyr::vars(-value))%>%
       dplyr::summarize_at(c("value"),list(~sum(.)))
     tblAggmeans<-tbl_rpdx%>%
       dplyr::mutate(scenario=as.character(scenario))%>%
       dplyr::filter(aggregate=="mean")%>%
       dplyr::group_by_at(dplyr::vars(-value))%>%
       dplyr::summarize_at(c("value"),list(~mean(.)))
     tbl_rpdC1<-dplyr::bind_rows(tblAggsums,tblAggmeans)%>%dplyr::ungroup()

     if(nrow(tbl_rpdC1)>0){

       if(grepl("right|left",legendPosition,ignore.case = T)){figWMult=1.3}else{figWMult=1}

       # Bar Chart Dodged
       metis.chart(tbl_rpdC1, xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel, sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar", facet_columns=NULL,
                   class ="scenario", position ="dodge", classPalette = classPalette,
                   dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i,"/compareScen",sep = ""),
                   fileName = paste(j,"_figBarDodgedDiffPrcnt_",i,"_compareScen1Scale",nameAppend,sep=""),forceFacets = T,
                   pdfpng=pdfpng, legendPosition=legendPosition, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
       )

       # Line Chart Overlapped
       metis.chart(tbl_rpdC1,xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel, sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "line", facet_columns=NULL,
                   class ="scenario", classPalette = classPalette,
                   dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i,"/compareScen",sep = ""),
                   fileName = paste(j,"_figLineOverlapDiffPrcnt_",i,"_compareScen1Scale",nameAppend,sep=""),
                   forceFacets = T,
                   pdfpng=pdfpng, legendPosition=legendPosition, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
       )
       }

       }

     # Drop the ref scenario
     tbl_rpdx <- tbl_rpd %>%
       dplyr::filter(scenario!=scenRef_i)
     tbl_rpdx<-droplevels(tbl_rpdx)

     if(length(unique(tbl_rpdx$class1))>1){if(grepl("right|left",legendPosition,ignore.case = T)){figWMult=1.3}else{figWMult=1}}else{figWMult=1}

     # Aggregated Class 1
     # Aggregate across classes
     tblAggsums<-tbl_rpdx%>%
       dplyr::mutate(scenario=as.character(scenario))%>%
       dplyr::filter(aggregate=="sum")%>%
       dplyr::group_by_at(dplyr::vars(-value))%>%
       dplyr::summarize_at(c("value"),list(~sum(.)))
     tblAggmeans<-tbl_rpdx%>%
       dplyr::mutate(scenario=as.character(scenario))%>%
       dplyr::filter(aggregate=="mean")%>%
       dplyr::group_by_at(dplyr::vars(-value))%>%
       dplyr::summarize_at(c("value"),list(~mean(.)))
     tbl_rpdC1<-dplyr::bind_rows(tblAggsums,tblAggmeans)%>%dplyr::ungroup()

     if(nrow(tbl_rpdC1)>0){

       if(grepl("right|left",legendPosition,ignore.case = T)){figWMult=1.3}else{figWMult=1}

       # Bar Chart Dodged
       metis.chart(tbl_rpdC1, xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel, sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar", facet_columns=NULL,
                   class ="scenario", position ="dodge", classPalette = classPalette,
                   dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i,"/compareScen",sep = ""),
                   fileName = paste(j,"_figBarDodgedDiffPrcnt_",i,"_compareScen_",nameAppend,sep=""),forceFacets = T,
                   pdfpng=pdfpng, legendPosition=legendPosition, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
       )

       # Line Chart Overlapped
       metis.chart(tbl_rpdC1,xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel, sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "line",
                   class ="scenario", classPalette = classPalette,
                   dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i,"/compareScen",sep = ""),
                   fileName = paste(j,"_figLineOverlapDiffPrcnt_",i,"_compareScen",nameAppend,sep=""),
                   forceFacets = T,facet_columns=NULL,
                   pdfpng=pdfpng, legendPosition=legendPosition, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
       )

       # Line Chart Overlapped
       metis.chart(tbl_rpdC1%>%dplyr::mutate(label="Percent Difference (%)"),xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel, sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "line",
                   class ="scenario", classPalette = classPalette,
                   dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i,"/compareScen",sep = ""),
                   fileName = paste(j,"_figLineOverlapDiffPrcnt_",i,"_compareScen",nameAppend,sep=""),
                   forceFacets = T,facet_columns="label",printFig = F,
                   pdfpng=pdfpng, legendPosition=legendPosition, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
       )->mpx

       if(j %in% unlist(mp$param)){
         mpnScen<-length(unique(tbl_rpdC1$scenario))
         assign(paste("mp_lineSumDiffPrcnt_",j,sep=""),mpx)
         mpParamPlots <- mpParamPlots %>%
           dplyr::bind_rows(tibble::tibble(param=c(j),
                                           plot=c(paste("mp_lineSumDiffPrcnt_",j,sep="")),
                                           nScen=c(rep(mpnScen,1)))); mpParamPlots
     }

      } # Close if(nrow(tbl_rsp)>0)
      } # if(length(unique(tbl$scenario))>1){ to check if chosen param exists for comparison
      } # close loop for param


  #-------------------------------------
  # Multiplots per Region and chosen parameters
  #------------------------------------
  if(multiplotOn){
  mpParamPlots # Multiplots recorded
  # Prepare MultiPlots for Each region across scenarios and params
  mpdf <- tibble::tibble();
  for(paramSet_i in 1:length(unique(unlist(mp$paramSet)))){
    for(param_i in 1:length(unique(unlist(mp$param)))){
    mpdf<-mpdf %>% dplyr::bind_rows(tibble::tibble(paramSet=mp$paramSet[[paramSet_i]],
                                                   param=mp$param[[paramSet_i]][param_i])) %>%
      dplyr::filter(!is.na(param))%>%unique()
  }};mpdf

  # Subset to available plots
  print(paste("Params selected but not available for multiplot are:"))
  print(paste(unique(mpdf$param)[!unique(mpdf$param) %in% unique(unlist(mpParamPlots$param))],collapse=", "))
  print(paste("Params available for multiplot are:"))
  print(paste(unique(mpdf$param)[unique(mpdf$param) %in% unique(unlist(mpParamPlots$param))],collapse=", "))

  mpdf <- mpdf %>% dplyr::filter(param %in% unique(unlist(mpParamPlots$param))); mpdf


  if(nrow(mpdf)>0){

  # Left_join paramSets with the paramPlot list
  mpParamPlotsx <- mpParamPlots %>%
    dplyr::left_join(mpdf, by="param")%>%
    unique(); mpParamPlotsx

  multiPlotFigs<-tibble::tibble()
  countMultiLabel=0;
  paramSetCurrent=mpdf[1,]$paramSet
  for(row_i in 1:nrow(mpdf)){

    if(row_i>1){
    if(mpdf[row_i,]$paramSet!=mpdf[row_i-1,]$paramSet){
      countMultiLabel=0
    }else{countMultiLabel=countMultiLabel+1}}

    mpParamPlots_i <- mpParamPlotsx %>%
      dplyr::filter(paramSet==mpdf[row_i,]$paramSet,param==mpdf[row_i,]$param)%>%unique();mpParamPlots_i
    plotSet_i <-unique(mpParamPlots_i$plot);plotSet_i
    pBRef<-get(plotSet_i[grepl("barDiffAbs1ScaleRef_",plotSet_i)]);pBRef
    pBDiff1S<-get(plotSet_i[grepl("barDiffAbs1Scale_",plotSet_i)]);pBDiff1S
    pBDiffMS<-get(plotSet_i[grepl("barDiffAbs_",plotSet_i)])+ylab(NULL);pBDiffMS
    pLDiffPrcnt<-get(plotSet_i[grepl("lineDiffPrcnt_",plotSet_i)])+ylab(NULL);pLDiffPrcnt
    pBLeg<-ggpubr::as_ggplot(ggpubr::get_legend(pBRef+theme(legend.position = "right")));pBLeg


    labels1 <-paste(letters[seq(from=(countMultiLabel*2+1),to=(countMultiLabel*2+2),by=1)],")",sep=""); labels1
    labels2 <-paste(letters[seq(from=(countMultiLabel*3+1),to=(countMultiLabel*3+3),by=1)],")",sep=""); labels2
    nScen_i=unique(mpParamPlots_i$nScen); nScen

    # MultiPlots 1: Ref,diffs1scale,diffsPrcnt,leg
    assign(paste("fig1Scale_",mpdf[row_i,]$paramSet,"_",mpdf[row_i,]$param,sep=""),
           ggpubr::ggarrange(pBDiff1S,pLDiffPrcnt,pBLeg,
                        ncol = 3,nrow=1,
                        widths=c((nScen_i+nScen_i-1)/nScen_i,1,0.25),
                        #labels="auto",
                        labels = labels1,
                        font.label=list(size=15*((nScen_i+nScen_i-1)/nScen_i+1.25)),
                        hjust=-0.5,vjust=1,
                        legend="none"))
    # MultiPlots 2: Ref,diffs1scale,diffsPrcnt,leg
    assign(paste("figMScale_",mpdf[row_i,]$paramSet,"_",mpdf[row_i,]$param,sep=""),
           ggpubr::ggarrange(pBRef,pBDiffMS,pLDiffPrcnt,pBLeg,
                     ncol = 4,nrow=1,
                     widths=c((nScen_i-1)/nScen_i,1,1,0.25),
                     #labels="auto",
                     labels = labels2,
                     font.label=list(size=15*((nScen_i-1)/nScen_i+2.25)),
                     hjust=-0.5,vjust=1,
                     legend="none"))

    multiPlotFigs <- multiPlotFigs %>%
      dplyr::bind_rows(tibble::tibble(paramSet=mpdf[row_i,]$paramSet,
                                  multiPlot=paste("fig1Scale_",mpdf[row_i,]$paramSet,"_",mpdf[row_i,]$param,sep=""),
                                  nScen=nScen_i)) %>%
      dplyr::bind_rows(tibble::tibble(paramSet=mpdf[row_i,]$paramSet,
                                  multiPlot=paste("figMScale_",mpdf[row_i,]$paramSet,"_",mpdf[row_i,]$param,sep=""),
                                  nScen=nScen_i));

    };multiPlotFigs;

for(paramSet_i in unique(multiPlotFigs$paramSet)){

  multiPlotFigs_i = multiPlotFigs %>% dplyr::filter(paramSet==paramSet_i);multiPlotFigs_i
  plotRows_i = length(unique(multiPlotFigs_i$multiPlot)); plotRows_i

  # # Plot diff 1 Scale
  # multiplotlist_i<-list()
  # multiPlotFigs_ix <- multiPlotFigs_i%>%dplyr::filter(grepl("fig1Scale",multiPlot))
  # for(multiplot_i in 1:length(unique(multiPlotFigs_ix $multiPlot))){
  #   multiplotlist_i[[multiplot_i]] <-get(unique(multiPlotFigs_ix $multiPlot)[multiplot_i])
  # };multiplotlist_i
  # figure <- ggpubr::ggarrange(plotlist=multiplotlist_i,ncol=1,nrow=plotRows_i); figure
  # # Plot Combined Figure for the Param Set
  # metis.printPdfPng(figure=figure,
  #                     dir=paste(dirOutputs, "/Charts/",folderName,"/", i,"/multiPlot", sep = ""),
  #                     filename=paste(paramSet_i,"_",i,"_mplot_diff1Scale",sep=""),
  #                     figWidth=figWidth*((nScen_i+nScen_i-1)/nScen_i+1.25),
  #                     figHeight=figHeight*plotRows_i,
  #                     pdfpng="png")

  # Plot diff Multi Scale
  multiPlotFigs_ix <- multiPlotFigs_i%>%dplyr::filter(grepl("figMScale",multiPlot))
  for(multiplot_i in 1:length(unique(multiPlotFigs_ix $multiPlot))){
    multiplotlist_i[[multiplot_i]] <-get(unique(multiPlotFigs_ix $multiPlot)[multiplot_i])
  };multiplotlist_i
  figure <- ggpubr::ggarrange(plotlist=multiplotlist_i,ncol=1,nrow=plotRows_i); figure
  # Plot Combined Figure for the Param Set
  metis.printPdfPng(figure=figure,
                    dir=paste(dirOutputs, "/Charts/",folderName,"/", i,"/multiPlot", sep = ""),
                    filename=paste(paramSet_i,"_",i,"_mplot_diffMultiScale",sep=""),
                    figWidth=figWidth*((nScen-1)/nScen+2.25),
                    figHeight=figHeight*plotRows_i,
                    pdfpng="png")

}
} # Close if mpdf rows >0


  # Summary Multiplots for Across Scenarios

  #-------------------------------------
  # Multiplots per Region and chosen parameters
  #------------------------------------

  mpParamPlots%>%as.data.frame() # Multiplots recorded
  # Prepare MultiPlots for Each region across scenarios and params
  mpdf <- tibble::tibble();
  for(paramSet_i in 1:length(unique(unlist(mp$paramSet)))){
    for(param_i in 1:length(unique(unlist(mp$param)))){
      mpdf<-mpdf %>% dplyr::bind_rows(tibble::tibble(paramSet=mp$paramSet[[paramSet_i]],
                                                     param=mp$param[[paramSet_i]][param_i],
                                                     nColMax = mp$nColMax[[paramSet_i]])) %>%
        dplyr::filter(!is.na(param))%>%unique()
    }};mpdf

  # Subset to available plots
  print(paste("Params selected but not available for multiplot are:"))
  print(paste(unique(mpdf$param)[!unique(mpdf$param) %in% unique(unlist(mpParamPlots$param))],collapse=", "))
  print(paste("Params available for multiplot are:"))
  print(paste(unique(mpdf$param)[unique(mpdf$param) %in% unique(unlist(mpParamPlots$param))],collapse=", "))

  mpdf <- mpdf %>% dplyr::filter(param %in% unique(unlist(mpParamPlots$param))); mpdf


  if(nrow(mpdf)>0){

    # Left_join paramSets with the paramPlot list
    mpParamPlotsx <- mpParamPlots %>%
      dplyr::left_join(mpdf, by="param")%>%
      unique(); mpParamPlotsx

    multiPlotFigs<-tibble::tibble()
    paramSetCurrent=mpdf[1,]$paramSet
    for(paramSet_i in unique(mpdf$paramSet)){

      mpParamPlots_i <- mpParamPlotsx %>%
        dplyr::filter(paramSet==paramSet_i)%>%unique();mpParamPlots_i
      plotSet_i <-unique(mpParamPlots_i$plot);plotSet_i

      # Plot diff 1 Scale
      multiplotlist_i<-list();
      plotSet_ix <- unique(plotSet_i[grepl("_lineSum",plotSet_i)]);plotSet_ix
      for(multiplot_i in 1:length(plotSet_ix)){
        if(grepl("lineSumAbs",plotSet_ix[multiplot_i])){
          multiplotlist_i[[multiplot_i]] <-get(plotSet_ix[multiplot_i])+ylab(NULL)
        }else{
        multiplotlist_i[[multiplot_i]] <-get(plotSet_ix[multiplot_i])+ylab(NULL)
        }
      };multiplotlist_i

      labels1 <-paste(letters[seq(from=1,to=length(multiplotlist_i),by=1)],")",sep=""); labels1

      legRows = max(round(mpParamPlots_i[1,]$nScen/3),1);legRows
      mp_LineLeg <- ggpubr::as_ggplot(ggpubr::get_legend(get(plotSet_ix[1])+
                                                   theme(legend.position = "bottom")+
                                                   guides(colour = guide_legend(nrow = legRows,title="Scenario"),
                                                            shape = guide_legend(nrow=legRows,title="Scenario"))));mp_LineLeg
      # MultiPlots 1: Ref,diffs1scale,diffsPrcnt,leg
      #ncol
      ncol_i=3;
      if((length(plotSet_ix)/ncol_i)<1){nrow_i=1}else{nrow_i=round(length(plotSet_ix)/ncol_i)};nrow_i
      figure <- ggpubr::ggarrange(ggpubr::ggarrange(plotlist=multiplotlist_i,
                                    labels = labels1,
                                    font.label=list(size=4*ncol_i*(nrow_i)*1),
                                    hjust=0,vjust=1,
                                    ncol = ncol_i,nrow=nrow_i,
                                    legend="none"),
                          mp_LineLeg, ncol=1,nrow=2,
                          heights=c(1,(1/(nrow_i*3))),legend="none"); figure

      metis.printPdfPng(figure=figure,
                        dir=paste(dirOutputs, "/Charts/",folderName,"/", i,"/multiPlot", sep = ""),
                        filename=paste(paramSet_i,"_",i,"_",j,"_mplot_SumLineDiffPrcnt",sep=""),
                        figWidth=figWidth*ncol_i*0.6,
                        figHeight=figHeight*(nrow_i+0.3)*0.75,
                        pdfpng="png")


      # Only Abs
      #----------------------------------

      # Plot diff 1 Scale
      multiplotlist_i<-list();
      plotSet_ix <- unique(plotSet_i[grepl("_lineSumAbs_",plotSet_i)]);plotSet_ix
      for(multiplot_i in 1:length(plotSet_ix)){
        if(grepl("lineSumAbs",plotSet_ix[multiplot_i])){
          multiplotlist_i[[multiplot_i]] <-get(plotSet_ix[multiplot_i])+ylab(NULL)
        }else{
          multiplotlist_i[[multiplot_i]] <-get(plotSet_ix[multiplot_i])+ylab(NULL)
        }
      };multiplotlist_i

      labels1 <-paste(letters[seq(from=1,to=length(multiplotlist_i),by=1)],")",sep=""); labels1

      legRows = max(round(mpParamPlots_i[1,]$nScen/3),1);legRows
      mp_LineLeg <- ggpubr::as_ggplot(ggpubr::get_legend(get(plotSet_ix[1])+
                                                   theme(legend.position = "bottom")+
                                                   guides(colour = guide_legend(nrow = legRows,title="Scenario"),
                                                          shape = guide_legend(nrow=legRows,title="Scenario"))));mp_LineLeg
      # MultiPlots 1: Ref,diffs1scale,diffsPrcnt,leg
      #ncol
      if(round(length(plotSet_ix)/4)<1){ncol_i=length(plotSet_ix)}else{ncol_i=4};ncol_i
      if((length(plotSet_ix)/ncol_i)<1){nrow_i=1}else{nrow_i=round(length(plotSet_ix)/ncol_i)};nrow_i
      figure <- ggpubr::ggarrange(ggpubr::ggarrange(plotlist=multiplotlist_i,
                                    labels = labels1,
                                    font.label=list(size=14*ncol_i*(nrow_i)*1),
                                    hjust=0,vjust=1,
                                    ncol = ncol_i,nrow=nrow_i,
                                    legend="none"),
                          mp_LineLeg, ncol=1,nrow=2,
                          heights=c(1,(1/(nrow_i*3))),legend="none"); figure

      metis.printPdfPng(figure=figure,
                        dir=paste(dirOutputs, "/Charts/",folderName,"/", i,"/multiPlot", sep = ""),
                        filename=paste(paramSet_i,"_",i,"_",j,"_mplot_SumLineAbs",sep=""),
                        figWidth=figWidth*ncol_i*0.6,
                        figHeight=figHeight*(nrow_i+0.3)*0.75,
                        pdfpng="png")


    }
  }
}
    } # close loop for region
  } # Close if multiple scenarios available
} # Close if(regionCompareOnly==1)


  return(tbl)

  } # Close Function
