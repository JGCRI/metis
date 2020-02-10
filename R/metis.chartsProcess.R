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
#' @param pointsOn Default =T
#' @param multiPlotOn Default=F,
#' @param multiPlotFigsOnly Default = F. Plot only multiplot figs. Much faster.
#' @param multiPlotFigLabels Default = T. AutoPlot Labels
#' @param multiPlotAllYears Default=F
#' @param mp This is a list with the paramSet and related params plus the max number of rows desired for summary plots.
#' @param facetCols Default=3
#' @param facetLabelSize Default =24
#' @param diffText Default = "_diff",
#' @param diffPrcntText Default = "_diff_prcnt"
#' @param theme_custom Default = NULL, "theme_gray","theme_bw","theme_linedraw","theme_light", "theme_minimal","theme_classic","theme_void","theme_dark"
#' @param panelBGcolor Default = NULL,
#' @param plotBGcolor Default = "transparent",
#' @param legendBGcolor Default = NULL
#' @param facetBGColor Default ="grey30",
#' @param facetLabelColor Default = "white",
#' @param facetBorderColor Default ="black",
#' @param xOrder Default = NULL,
#' @param yMinDefault Default = NULL,
#' @param yMaxDefault Default = NULL
#' @param yMaxDiffAbsDefault Default = NULL,
#' @param yMinDiffAbsDefault Default = NULL
#' @param yMaxDiffPrcntDefault Default = NULL,
#' @param yMinDiffPrcntDefault Default = NULL
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
                       facetBGColor="grey30",
                       facetBorderColor="black",
                       facetLabelColor = "white",
                       facetLabelSize=24,
                       nameAppend="",
                       scensSelect="All",
                       xOrder = NULL,
                       colOrder1 = NULL,
                       colOrderName1 = NULL,
                       colOrder2 = NULL,
                       colOrderName2 = NULL,
                       scaleRange = NULL,
                       xScenCompFacetLabelSize = 35,
                       legendPosition="right",
                       figWidth=13,
                       figHeight=9,
                       pointsOn=T,
                       multiPlotOn=F,
                       multiPlotFigsOnly=F,
                       multiPlotFigLabels=T,
                       multiPlotAllYears=F,
                       mp = list(paramSet=list(c("energy"),
                                                c("water")),
                                  param=list(c("elecCapByFuel","elecByTechTWh"),
                                             c("watWithdrawBySec","watConsumBySec")),
                                  nColMax=list(c(3),
                                               c(3))),
                       facetCols=3,
                       diffText = "_diff",
                       diffPrcntText = "_diff_prcnt",
                       theme_custom=NULL,
                       panelBGcolor = NULL,
                       plotBGcolor = "transparent",
                       legendBGcolor = NULL,
                       yMinDefault = 0,
                       yMaxDefault = NULL,
                       yMaxDiffAbsDefault = NULL,
                       yMinDiffAbsDefault  = NULL,
                       yMaxDiffPrcntDefault = NULL,
                       yMinDiffPrcntDefault = NULL) {


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
  # xOrder = NULL
  # colOrder1 = NULL
  # colOrderName1 = NULL
  # colOrder2 = NULL
  # colOrderName2 = NULL
  # scaleRange = NULL
  # xScenCompFacetLabelSize = 35
  # legendPosition="right"
  # figWidth=13
  # figHeight=9
  # multiPlotOn=F
  # multiPlotAllYears=F
  # mp = list(paramSet=list(c("energy"),
  #                         c("water")),
  #           param=list(c("elecCapByFuel","elecByTechTWh"),
  #                      c("watWithdrawBySec","watConsumBySec")),
  #           nColMax=list(c(3),
  #                        c(3)))
  #  facetCols=3
  #  diffText = "_diff"
  #  diffPrcntText = "_diff_prcnt"
  #  theme_custom=NULL
  #  panelBGcolor = NULL
  #  plotBGcolor = "transparent"
  #  legendBGcolor = NULL
  # yMinDefault = NULL
  # yMaxDefault = NULL
  # yMaxDiffAbsDefault = NULL
  # yMinDiffAbsDefault  = NULL
  # yMaxDiffPrcntDefault = NULL
  # yMinDiffPrcntDefault = NULL

#------------------
# Initialize variables to remove binding errors
# -----------------

  NULL->scenario->value->x->region->param->origValue->origScen->origQuery->year->
  origUnits->origX->sources->vintage->class1->classLabel1->classPalette1->yMax_i->yMin_i->
  class2->classLabel2->classPalette2->i->j->k->figWMult->classLabel1x ->classLabel2x-> classPalette1x-> classPalette2x->
  nScen->paramSet->multiPlot->plot->scen->param->lastrow

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
  if(!"x"%in%names(data)){
    if("year"%in%names(data)){
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

  scaleRangeDefault <- tibble::tibble(param=unique(tbl$param)) %>%
    dplyr::mutate(maxScale = NA_real_,
                  minScale = NA_real_,
                  maxDiffAbs = NA_real_,
                  minDiffAbs = NA_real_,
                  maxDiffPrcnt = NA_real_,
                  minDiffPrcnt = NA_real_)

  if(!is.null(scaleRange)){

  # Check for incorrect col names
    if(any(!names(scaleRange) %in% names(scaleRangeDefault))){
      paste("Incorrect column names for scaleRange: ",names(scaleRange)[!names(scaleRange) %in% names(scaleRangeDefault)],
            ". Can only be param, minScale, maxscale,minDiffAbs, maxDiffAbs, minDiffPrcnt, maxDiffPrcnt.")
    }

    # Add missing column names to scaleRange
    scaleRange = scaleRange %>%
      dplyr::left_join(scaleRangeDefault %>%
                         dplyr::select(param,names(scaleRangeDefault)[!names(scaleRangeDefault) %in% names(scaleRange)])) %>%
      dplyr::bind_rows(scaleRangeDefault %>% filter(!param %in% unique(scaleRange$param)))

  scaleRange[is.na(scaleRange)]<-NA_real_
  scaleRange[scaleRange=="NA"]<-NA_real_
} else {
    scaleRange = scaleRangeDefault
  }


#------------------------
# Check multiplot Param
#------------------------

if(!is.null(mp)){
  if(length(mp$paramSet)!=length(mp$param)){stop("Error mp paramSet and param should have the same number of elements.")}
  if(length(mp$paramSet)!=length(mp$nColMax)){stop("Error mp paramSet and param should have the same number of elements.")}
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


if(xData %in% names(tbl)){
  tbl$x <- tbl[[xData]]
}

if(any(xRange!="All")){if(is.numeric(tbl$x)){tbl<-tbl%>%dplyr::filter(x %in% xRange)
                       print(paste("Running analysis for chosen x: ",paste(xRange,collapse=", ")))}}else{
                         print("One or more items in xRange is 'All' so running analysis for all x.")
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

      NULL -> yMax_i -> yMin_i
      yMax_i = scaleRange %>% dplyr::filter(param==k) %>% unique()
      if(!is.na(yMax_i$maxScale)){yMax_i = yMax_i$maxScale}else{
        yMax_i=NULL
        print(paste("No scaleRange maxScale for param ", k," provided. Setting yMax to NULL",sep=""))}

      yMin_i = scaleRange %>% dplyr::filter(param==k) %>% unique()
      if(!is.na(yMin_i$maxScale)){yMin_i = yMin_i$minScale}else{
        yMin_i=NULL
        print(paste("No scaleRange minScale for param ", k," provided. Setting yMax to NULL",sep=""))}

      if(!is.null(yMaxDefault) & is.null(yMax_i)){yMax_i=max(yMaxDefault,max(tbl_sp$value))}
      if(!is.null(yMinDefault) & is.null(yMin_i)){yMin_i=min(yMinDefault,min(tbl_sp$value))}


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
       if(multiPlotFigsOnly==F){
       metis.chart(tbl_spC1, xData=xData, pointsOn=pointsOn, theme_custom=theme_custom,  panelBGcolor=panelBGcolor, plotBGcolor=plotBGcolor,
                   legendBGcolor=legendBGcolor,yData=yData,xLabel=xLabel,yLabel=yLabel,facetLabelSize = facetLabelSize, facetBorderColor=facetBorderColor,
                   facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor, yMax=yMax_i, yMin=yMin_i,
                    sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar",facet_columns="region",facet_rows=NULL,
          dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/compareRegions/",gsub(" ","",paste(unique(unique(tbl$region)),collapse="")),j,sep = ""),
          fileName = paste(k,"_figBar_",j,"_compareRegions",nameAppend,sep=""),
          figWidth = figWidth*max((length(unique(tbl_sp$region))/2),1)*figWMult,
          figHeight = figHeight*max(1,ceiling(length(unique(tbl_sp$region))/4)*0.75),pdfpng=pdfpng, legendPosition=legendPosition, xOrder = xOrder, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
        )}



        # Line Chart
        if(multiPlotFigsOnly==F){
        metis.chart(tbl_spC1,xData=xData, pointsOn=pointsOn, theme_custom=theme_custom,  panelBGcolor=panelBGcolor, plotBGcolor=plotBGcolor,
                    legendBGcolor=legendBGcolor,yData=yData,xLabel=xLabel,yLabel=yLabel,facetLabelSize = facetLabelSize, facetBorderColor=facetBorderColor,
                    facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor, yMax=yMax_i, yMin=yMin_i,
                    sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "line",facet_columns="region",facet_rows=NULL,
          dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/compareRegions/",gsub(" ","",paste(unique(unique(tbl$region)),collapse="")),j,sep = ""),
          fileName = paste(k,"_figLines_",j,"_compareRegions",nameAppend,sep=""),
          figWidth = figWidth*max((length(unique(tbl_sp$region))/2),1)*figWMult,
          figHeight = figHeight*max(1,ceiling(length(unique(tbl_sp$region))/4)*0.75),pdfpng=pdfpng, legendPosition=legendPosition, xOrder = xOrder, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
        )}

        # If class 2 available
        if(length(unique(tbl_sp$class2))>1){

          # Bar Chart
          if(multiPlotFigsOnly==F){
          metis.chart(tbl_sp, xData=xData, pointsOn=pointsOn, theme_custom=theme_custom,  panelBGcolor=panelBGcolor, plotBGcolor=plotBGcolor,
                      legendBGcolor=legendBGcolor,yData=yData,xLabel=xLabel,yLabel=yLabel,facetLabelSize = facetLabelSize, facetBorderColor=facetBorderColor,
                      facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor, yMax=yMax_i, yMin=yMin_i,
                      sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar",facet_columns="region",facet_rows="class2",
                      dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/compareRegions/",gsub(" ","",paste(unique(unique(tbl$region)),collapse="")),j,sep = ""),
                      fileName = paste(k,"_figBar_",j,"_compareRegionsClass2",nameAppend,sep=""),
                      figWidth = figWidth*max((length(unique(tbl_sp$region))/2),1)*figWMult,
                      figHeight = figHeight*max((length(unique(tbl_sp$class2))/2),1),pdfpng=pdfpng, legendPosition=legendPosition, xOrder = xOrder, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
          )}

          # Line Chart
          if(multiPlotFigsOnly==F){
          metis.chart(tbl_sp,xData=xData, pointsOn=pointsOn, theme_custom=theme_custom,  panelBGcolor=panelBGcolor, plotBGcolor=plotBGcolor,
                      legendBGcolor=legendBGcolor,yData=yData,xLabel=xLabel,yLabel=yLabel,facetLabelSize = facetLabelSize, facetBorderColor=facetBorderColor,
                      facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor, yMax=yMax_i, yMin=yMin_i,
                      sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "line",facet_columns="region",facet_rows="class2",
                      dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/compareRegions/",gsub(" ","",paste(unique(unique(tbl$region)),collapse="")),j,sep = ""),
                      fileName = paste(k,"_figLines_",j,"_compareRegionsClass2",nameAppend,sep=""),
                      figWidth = figWidth*max((length(unique(tbl_sp$region))/2),1)*figWMult,
                      figHeight = figHeight*max((length(unique(tbl_sp$class2))/2),1),pdfpng=pdfpng, legendPosition=legendPosition, xOrder = xOrder, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
          )}


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

      NULL -> yMax_i -> yMin_i
      yMax_i = scaleRange %>% dplyr::filter(param==j) %>% unique()
      if(!is.na(yMax_i$maxScale)){yMax_i = yMax_i$maxScale}else{
        yMax_i=NULL
        print(paste("No scaleRange maxScale for param ", j," provided. Setting yMax to NULL",sep=""))}

      yMin_i = scaleRange %>% dplyr::filter(param==j) %>% unique()
      if(!is.na(yMin_i$maxScale)){yMin_i = yMin_i$minScale}else{
        yMin_i=NULL
        print(paste("No scaleRange minScale for param ", j," provided. Setting yMax to NULL",sep=""))}

      if(!is.null(yMaxDefault) & is.null(yMax_i)){yMax_i=max(yMaxDefault,max(tbl_p$value))}
      if(!is.null(yMinDefault) & is.null(yMin_i)){yMin_i=min(yMinDefault,min(tbl_p$value))}


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
        if(multiPlotFigsOnly==F){
        metis.chart(tbl_pC1, xData=xData, pointsOn=pointsOn, theme_custom=theme_custom,  panelBGcolor=panelBGcolor, plotBGcolor=plotBGcolor,
                    legendBGcolor=legendBGcolor,yData=yData,xLabel=xLabel,yLabel=yLabel,facetLabelSize = facetLabelSize, facetBorderColor=facetBorderColor,
                    facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor, yMax=yMax_i, yMin=yMin_i,
                    sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar",facet_columns="scenario",facet_rows="region",
          dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/compareRegions/",gsub(" ","",paste(unique(unique(tbl$region)),collapse="")),"compareScen", sep = ""),
          fileName = paste(j,"_figBar_compareScenRegions",nameAppend,sep=""),
          figWidth = figWidth*max((length(unique(tbl_p$scenario))/2),1)*figWMult,
          figHeight = figHeight*max((length(unique(tbl_p$region))/2),1),pdfpng=pdfpng, legendPosition=legendPosition, xOrder = xOrder, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
        )}

        # Line Chart
        if(multiPlotFigsOnly==F){
        metis.chart(tbl_pC1,xData=xData, pointsOn=pointsOn, theme_custom=theme_custom,  panelBGcolor=panelBGcolor, plotBGcolor=plotBGcolor,
                    legendBGcolor=legendBGcolor,yData=yData,xLabel=xLabel,yLabel=yLabel,facetLabelSize = facetLabelSize, facetBorderColor=facetBorderColor,
                    facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor, yMax=yMax_i, yMin=yMin_i,
                    sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "line",facet_columns="scenario",facet_rows="region",
          dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/compareRegions/",gsub(" ","",paste(unique(unique(tbl$region)),collapse="")),"compareScen", sep = ""),
          fileName = paste(j,"_figLine_compareScenRegions",nameAppend,sep=""),
          figWidth = figWidth*max((length(unique(tbl_p$scenario))/2),1)*figWMult,
          figHeight = figHeight*max((length(unique(tbl_p$region))/2),1),pdfpng=pdfpng, legendPosition=legendPosition, xOrder = xOrder, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
        )}

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
        if(multiPlotFigsOnly==F){
        metis.chart(tbl_pyC1, xData ="scenario", yData=yData,xLabel=xLabel,yLabel=yLabel,facetLabelSize = xScenCompFacetLabelSize,
                    sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar", facet_columns = xData, facet_rows="region",
                    yMax=yMax_i, yMin=yMin_i,
          dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/compareRegions/",gsub(" ","",paste(unique(unique(tbl$region)),collapse="")),"compareScen", sep = ""),
          fileName = paste(j,"_figBar_compareScenRegion_xScenSelectYears",nameAppend,sep=""),
          figWidth = figWidth*max((length(unique(tbl_py$x)[unique(tbl_py$x) %in% xCompare])/3),1)*figWMult,
          figHeight = figHeight*max((length(unique(tbl_py$region))/2),1),pdfpng=pdfpng, legendPosition=legendPosition, xOrder = xOrder, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
        )}


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
          if(multiPlotFigsOnly==F){
          metis.chart(tbl_pAgg, xData=xData, pointsOn=pointsOn, theme_custom=theme_custom,  panelBGcolor=panelBGcolor, plotBGcolor=plotBGcolor,
                      legendBGcolor=legendBGcolor,yData=yData,xLabel=xLabel,yLabel=yLabel,facetLabelSize = facetLabelSize, facetBorderColor=facetBorderColor,
                      facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor, yMax=yMax_i, yMin=yMin_i, sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar",
                      class ="scenario", position ="dodge", classPalette = classPalette,
                      facet_columns="region",facet_rows=NULL,
            dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/compareRegions/",gsub(" ","",paste(unique(unique(tbl$region)),collapse="")),"compareScen", sep = ""),
            fileName = paste(j,"_figBarDodged_compareScenRegion",nameAppend,sep=""), paletteRev=F,
            figWidth = figWidth*figWMult,pdfpng=pdfpng, legendPosition=legendPosition, xOrder = xOrder, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
          )}

          # Line Chart Overlapped
          if(multiPlotFigsOnly==F){
          metis.chart(tbl_pAgg,xData=xData, pointsOn=pointsOn, theme_custom=theme_custom,  panelBGcolor=panelBGcolor, plotBGcolor=plotBGcolor,
                      legendBGcolor=legendBGcolor,yData=yData,xLabel=xLabel,yLabel=yLabel,facetLabelSize = facetLabelSize, facetBorderColor=facetBorderColor,
                      facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor, yMax=yMax_i, yMin=yMin_i,
                      sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "line",class ="scenario", classPalette = classPalette,
                      facet_columns="region",facet_rows=NULL,paletteRev = F,
            dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/compareRegions/",gsub(" ","",paste(unique(unique(tbl$region)),collapse="")),"compareScen", sep = ""),
            fileName = paste(j,"_figLineOverlap_compareScenRegion",nameAppend,sep=""),
            figWidth = figWidth*figWMult,pdfpng=pdfpng, legendPosition=legendPosition, xOrder = xOrder, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
          )}
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
            dplyr::mutate(!!paste(k,diffText,sep=""):=get(k)-get(scenRef_i))%>%
            dplyr::select(-dplyr::one_of(c(k,scenRef_i)))
          tbl_temp<-tbl_temp%>%
            tidyr::gather(key=scenario,value=!!yData,
                          -c(names(tbl_temp)[!names(tbl_temp) %in% paste(k,diffText,sep="")]))
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
        if(multiPlotFigsOnly==F){
        metis.chart(tbl_pdC1, xData=xData, pointsOn=pointsOn, theme_custom=theme_custom,  panelBGcolor=panelBGcolor, plotBGcolor=plotBGcolor,
                    legendBGcolor=legendBGcolor,yData=yData,xLabel=xLabel,yLabel=yLabel,facetLabelSize = facetLabelSize, facetBorderColor=facetBorderColor,
                    facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor, yMax=yMax_i, yMin=yMin_i,
                    sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar", facet_rows="region", facet_columns="scenario",
                    dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/compareRegions/",gsub(" ","",paste(unique(unique(tbl$region)),collapse="")),"compareScen", sep = ""),
                    fileName = paste(j,"_figBarDiff_compareScenRegionREF",nameAppend,sep=""),
                    figWidth = figWidth*max((length(unique(tbl_pd$scenario))/2),1)*figWMult,forceFacets = T,
                    figHeight = figHeight*max((length(unique(tbl_pd$region))/2),1),pdfpng=pdfpng, legendPosition=legendPosition,
                    xOrder = xOrder, colOrder1 = c(colOrder1[1],c(colOrder1[1],paste(colOrder1,diffText,sep=""))),colOrderName1 = colOrderName1,
                    colOrder2 = c(colOrder2[1],c(colOrder2[1],paste(colOrder2,diffText,sep=""))), colOrderName2 = colOrderName2
        )}

        # Line Chart
        if(multiPlotFigsOnly==F){
        metis.chart(tbl_pdC1, xData=xData, pointsOn=pointsOn, theme_custom=theme_custom,  panelBGcolor=panelBGcolor, plotBGcolor=plotBGcolor,
                    legendBGcolor=legendBGcolor,yData=yData,xLabel=xLabel,yLabel=yLabel,facetLabelSize = facetLabelSize, facetBorderColor=facetBorderColor,
                    facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor, yMax=yMax_i, yMin=yMin_i,
                    sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "line", facet_rows="region", facet_columns="scenario",
                    dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/compareRegions/",gsub(" ","",paste(unique(unique(tbl$region)),collapse="")),"compareScen", sep = ""),
                    fileName = paste(j,"_figLineDiff_compareScenRegionREF",nameAppend,sep=""),
                    figWidth = figWidth*max((length(unique(tbl_pd$scenario))/2),1)*figWMult,forceFacets = T,
                    figHeight = figHeight*max((length(unique(tbl_pd$region))/2),1),pdfpng=pdfpng, legendPosition=legendPosition,
                    xOrder = xOrder, colOrder1 = c(colOrder1[1],paste(colOrder1,diffText,sep="")),colOrderName1 = colOrderName1,
                    colOrder2 = c(colOrder2[1],paste(colOrder2,diffText,sep="")), colOrderName2 = colOrderName2
        )}

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
        if(multiPlotFigsOnly==F){
        metis.chart(tbl_pdC1, xData=xData, pointsOn=pointsOn, theme_custom=theme_custom,  panelBGcolor=panelBGcolor, plotBGcolor=plotBGcolor,
                    legendBGcolor=legendBGcolor,yData=yData,xLabel=xLabel,yLabel=yLabel,facetLabelSize = facetLabelSize, facetBorderColor=facetBorderColor,
                    facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor, yMax=yMax_i, yMin=yMin_i,
                    sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar", facet_rows="region", facet_columns="scenario",
                    dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/compareRegions/",gsub(" ","",paste(unique(unique(tbl$region)),collapse="")),"compareScen", sep = ""),
                    fileName = paste(j,"_figBarDiff_compareScenRegionREF",nameAppend,sep=""),
                    figWidth = figWidth*max((length(unique(tbl_pd$scenario))/2),1)*figWMult,forceFacets = T,
                    figHeight = figHeight*max((length(unique(tbl_pd$region))/2),1),pdfpng=pdfpng, legendPosition=legendPosition,
                    xOrder = xOrder, colOrder1 = c(colOrder1[1],paste(colOrder1,diffText,sep="")),colOrderName1 = colOrderName1,
                    colOrder2 = c(colOrder2[1],paste(colOrder2,diffText,sep="")), colOrderName2 = colOrderName2
        )}

        # Line Chart
        if(multiPlotFigsOnly==F){
        metis.chart(tbl_pdC1, xData=xData, pointsOn=pointsOn, theme_custom=theme_custom,  panelBGcolor=panelBGcolor, plotBGcolor=plotBGcolor,
                    legendBGcolor=legendBGcolor,yData=yData,xLabel=xLabel,yLabel=yLabel,facetLabelSize = facetLabelSize, facetBorderColor=facetBorderColor,
                    facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor, yMax=yMax_i, yMin=yMin_i,
                    sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "line", facet_rows="region", facet_columns="scenario",
                    dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/compareRegions/",gsub(" ","",paste(unique(unique(tbl$region)),collapse="")),"compareScen", sep = ""),
                    fileName = paste(j,"_figLineDiff_compareScenRegionREF",nameAppend,sep=""),
                    figWidth = figWidth*max((length(unique(tbl_pd$scenario))/2),1)*figWMult,forceFacets = T,
                    figHeight = figHeight*max((length(unique(tbl_pd$region))/2),1),pdfpng=pdfpng, legendPosition=legendPosition,
                    xOrder = xOrder, colOrder1 = c(colOrder1[1],paste(colOrder1,diffText,sep="")),colOrderName1 = colOrderName1,
                    colOrder2 = c(colOrder2[1],paste(colOrder2,diffText,sep="")), colOrderName2 = colOrderName2
        )}


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

        # Set Scale Ranges for Diffs
        NULL -> yMaxDiffAbs_i -> yMinDiffAbs_i
        yMaxDiffAbs_i = scaleRange %>% dplyr::filter(param==j) %>% unique()
        if(!is.na(yMaxDiffAbs_i$maxDiffAbs)){yMaxDiffAbs_i = yMaxDiffAbs_i$maxDiffAbs}else{
          yMaxDiffAbs_i=NULL
          print(paste("No scaleRange maxScale for param ", j," provided. Setting yMaxDiffAbs to NULL",sep=""))}

        yMinDiffAbs_i = scaleRange %>% dplyr::filter(param==j) %>% unique()
        if(!is.na(yMinDiffAbs_i$minDiffAbs)){yMinDiffAbs_i = yMinDiffAbs_i$minDiffAbs}else{
          yMinDiffAbs_i=NULL
          print(paste("No scaleRange minScale for param ", j," provided. Setting yMaxDiffAbs to NULL",sep=""))}

        if(!is.null(yMaxDiffAbsDefault) & is.null(yMaxDiffAbs_i)){yMaxDiffAbs_i=max(yMaxDiffAbsDefault,max(tbl_p$value))}
        if(!is.null(yMinDiffAbsDefault) & is.null(yMinDiffAbs_i)){yMinDiffAbs_i=min(yMinDiffAbsDefault,min(tbl_p$value))}

        # Bar Chart
        if(multiPlotFigsOnly==F){
        metis.chart(tbl_pdC1, xData=xData, pointsOn=pointsOn, theme_custom=theme_custom,  panelBGcolor=panelBGcolor, plotBGcolor=plotBGcolor,
                    legendBGcolor=legendBGcolor,yData=yData,xLabel=xLabel,yLabel=yLabel,facetLabelSize = facetLabelSize, facetBorderColor=facetBorderColor,
                    facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor, yMax=yMaxDiffAbs_i, yMin=yMinDiffAbs_i,
                    sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar", facet_rows="region", facet_columns="scenario",
          dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/compareRegions/",gsub(" ","",paste(unique(unique(tbl$region)),collapse="")),"compareScen", sep = ""),
          fileName = paste(j,"_figBarDiff_compareScenRegion",nameAppend,sep=""),
          figWidth = figWidth*((length(unique(tbl_pd$scenario)))/((length(unique(tbl_pd$scenario)))+1))*max((length(unique(tbl_pd$scenario))/2),1)*figWMult,forceFacets = T,
          figHeight = figHeight*max((length(unique(tbl_pd$region))/2),1),pdfpng=pdfpng, legendPosition=legendPosition,
          xOrder = xOrder, colOrder1 = c(colOrder1[1],paste(colOrder1,diffText,sep="")),colOrderName1 = colOrderName1,
          colOrder2 = c(colOrder2[1],paste(colOrder2,diffText,sep="")), colOrderName2 = colOrderName2
        )}

        # Line Chart
        if(multiPlotFigsOnly==F){
        metis.chart(tbl_pdC1, xData=xData, pointsOn=pointsOn, theme_custom=theme_custom,  panelBGcolor=panelBGcolor, plotBGcolor=plotBGcolor,
                    legendBGcolor=legendBGcolor,yData=yData,xLabel=xLabel,yLabel=yLabel,facetLabelSize = facetLabelSize, facetBorderColor=facetBorderColor,
                    facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor,  yMax=yMaxDiffAbs_i, yMin=yMinDiffAbs_i,
                    sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "line", facet_rows="region", facet_columns="scenario",
          dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/compareRegions/",gsub(" ","",paste(unique(unique(tbl$region)),collapse="")),"compareScen", sep = ""),
          fileName = paste(j,"_figLineDiff_compareScenRegion",nameAppend,sep=""),
          figWidth = figWidth*((length(unique(tbl_pd$scenario)))/((length(unique(tbl_pd$scenario)))+1))*max((length(unique(tbl_pd$scenario))/2),1)*figWMult,forceFacets = T,
          figHeight = figHeight*max((length(unique(tbl_pd$region))/2),1),pdfpng=pdfpng, legendPosition=legendPosition,
          xOrder = xOrder, colOrder1 = c(colOrder1[1],paste(colOrder1,diffText,sep="")),colOrderName1 = colOrderName1,
          colOrder2 = c(colOrder2[1],paste(colOrder2,diffText,sep="")), colOrderName2 = colOrderName2
        )}

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
          if(multiPlotFigsOnly==F){
            if("classPalette" %in% names(tbl_pdAgg)){classPalettex=unique(tbl_pdAgg$classPalette)}else{classPalettex="pal_Basic"}
          metis.chart(tbl_pdAgg, xData=xData, pointsOn=pointsOn, theme_custom=theme_custom,  panelBGcolor=panelBGcolor, plotBGcolor=plotBGcolor,
                      legendBGcolor=legendBGcolor,yData=yData,xLabel=xLabel,yLabel=yLabel,facetLabelSize = facetLabelSize, facetBorderColor=facetBorderColor,
                      facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor, sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines,
                      chartType = "bar", yMax=yMaxDiffAbs_i, yMin=yMinDiffAbs_i,
                      class ="scenario", position ="dodge", figWidth=figWidth, figHeight=figHeight,
                      classPalette = metis.colors()[[classPalettex]][!metis.colors()[[classPalettex]] %in% metis.colors()[[classPalettex]][1]],
                      facet_rows="region",facet_columns = "scenario",paletteRev=F,
                      dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/compareRegions/",gsub(" ","",paste(unique(unique(tbl$region)),collapse="")),"compareScen", sep = ""),
                      fileName = paste(j,"_figBarDodgedDiff_compareScenRegion",nameAppend,sep=""),forceFacets = T,
                      pdfpng=pdfpng, legendPosition=legendPosition, xOrder = xOrder, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
          )}

          # Line Chart Overlapped
          if(multiPlotFigsOnly==F){
            if("classPalette" %in% names(tbl_pdAgg)){classPalettex=unique(tbl_pdAgg$classPalette)}else{classPalettex="pal_Basic"}
          metis.chart(tbl_pdAgg,xData=xData, pointsOn=pointsOn, theme_custom=theme_custom,  panelBGcolor=panelBGcolor, plotBGcolor=plotBGcolor,
                      legendBGcolor=legendBGcolor,yData=yData,xLabel=xLabel,yLabel=yLabel,facetLabelSize = facetLabelSize, facetBorderColor=facetBorderColor,
                      facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor, yMax=yMaxDiffAbs_i, yMin=yMinDiffAbs_i,
                      sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "line",class ="scenario",
                      classPalette = metis.colors()[[classPalettex]][!metis.colors()[[classPalettex]] %in% metis.colors()[[classPalettex]][1]],
                      facet_rows="region",facet_columns = "scenario",paletteRev=F,figWidth=figWidth, figHeight=figHeight,
                      dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/compareRegions/",gsub(" ","",paste(unique(unique(tbl$region)),collapse="")),"compareScen", sep = ""),
                      fileName = paste(j,"_figLineOverlapDiff_compareScenRegion",nameAppend,sep=""),forceFacets = T,
                      pdfpng=pdfpng, legendPosition=legendPosition, xOrder = xOrder, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
          )}
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

        # Set Scale Ranges for Diffs
        NULL -> yMaxDiffPrcnt_i -> yMinDiffPrcnt_i
        yMaxDiffPrcnt_i = scaleRange %>% dplyr::filter(param==j) %>% unique()
        if(!is.na(yMaxDiffPrcnt_i$maxDiffPrcnt)){yMaxDiffPrcnt_i = yMaxDiffPrcnt_i$maxDiffPrcnt}else{
          yMaxDiffPrcnt_i=NULL
          print(paste("No scaleRange maxScale for param ", j," provided. Setting yMaxDiffPrcnt to NULL",sep=""))}

        yMinDiffPrcnt_i = scaleRange %>% dplyr::filter(param==j) %>% unique()
        if(!is.na(yMinDiffPrcnt_i$minDiffPrcnt)){yMinDiffPrcnt_i = yMinDiffPrcnt_i$minDiffPrcnt}else{
          yMinDiffPrcnt_i=NULL
          print(paste("No scaleRange minScale for param ", j," provided. Setting yMaxDiffPrcnt to NULL",sep=""))}

        if(!is.null(yMaxDiffPrcntDefault) & is.null(yMaxDiffPrcnt_i)){yMaxDiffPrcnt_i=max(yMaxDiffPrcntDefault,max(tbl_pdC1$value))}
        if(!is.null(yMinDiffPrcntDefault) & is.null(yMinDiffPrcnt_i)){yMinDiffPrcnt_i=min(yMinDiffPrcntDefault,min(tbl_pdC1$value))}

        # Bar Chart
        if(multiPlotFigsOnly==F){
        metis.chart(tbl_pdC1%>%dplyr::mutate(units="~Percent"), xData=xData, pointsOn=pointsOn, theme_custom=theme_custom,  panelBGcolor=panelBGcolor,
                    plotBGcolor=plotBGcolor, legendBGcolor=legendBGcolor,yData=yData,xLabel=xLabel,yLabel=yLabel,facetLabelSize = facetLabelSize,
                    facetBorderColor=facetBorderColor,  facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor,yMax=yMaxDiffPrcnt_i, yMin=yMinDiffPrcnt_i,
                    sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar", facet_rows="region", facet_columns="scenario",
                    dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/compareRegions/",gsub(" ","",paste(unique(unique(tbl$region)),collapse="")),"compareScen", sep = ""),
                    fileName = paste(j,"_figBarDiffPrcnt_compareScenRegion",nameAppend,sep=""),
                    figWidth = figWidth*((length(unique(tbl_pd$scenario)))/((length(unique(tbl_pd$scenario)))+1))*max((length(unique(tbl_pd$scenario))/2),1)*figWMult,forceFacets = T,
                    figHeight = figHeight*max((length(unique(tbl_pd$region))/2),1),pdfpng=pdfpng, legendPosition=legendPosition,
                    xOrder = xOrder, colOrder1 = c(colOrder1[1],paste(colOrder1,diffPrcntText,sep="")),colOrderName1 = colOrderName1,
                    colOrder2 = c(colOrder2[1],paste(colOrder2,diffPrcntText,sep="")), colOrderName2 = colOrderName2
        )}

        # Line Chart
        if(multiPlotFigsOnly==F){
        metis.chart(tbl_pdC1%>%dplyr::mutate(units="~Percent"), xData=xData, pointsOn=pointsOn, theme_custom=theme_custom,  panelBGcolor=panelBGcolor,
                    plotBGcolor=plotBGcolor, legendBGcolor=legendBGcolor,yData=yData,xLabel=xLabel,yLabel=yLabel,facetLabelSize = facetLabelSize,
                    facetBorderColor=facetBorderColor,  facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor,yMax=yMaxDiffPrcnt_i, yMin=yMinDiffPrcnt_i,
                    sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "line", facet_rows="region", facet_columns="scenario",
                    dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/compareRegions/",gsub(" ","",paste(unique(unique(tbl$region)),collapse="")),"compareScen", sep = ""),
                    fileName = paste(j,"_figLineDiffPrcnt_compareScenRegion",nameAppend,sep=""),
                    figWidth = figWidth*((length(unique(tbl_pd$scenario)))/((length(unique(tbl_pd$scenario)))+1))*max((length(unique(tbl_pd$scenario))/2),1)*figWMult,forceFacets = T,
                    figHeight = figHeight*max((length(unique(tbl_pd$region))/2),1),pdfpng=pdfpng, legendPosition=legendPosition,
                    xOrder = xOrder, colOrder1 = c(colOrder1[1],paste(colOrder1,diffPrcntText,sep="")),colOrderName1 = colOrderName1,
                    colOrder2 = c(colOrder2[1],paste(colOrder2,diffPrcntText,sep="")), colOrderName2 = colOrderName2
        )}
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
          if(multiPlotFigsOnly==F){
            if("classPalette" %in% names(tbl_pdC1)){classPalettex=unique(tbl_pdC1$classPalette)}else{classPalettex="pal_Basic"}
          metis.chart(tbl_pdC1%>%dplyr::mutate(units="~Percent"), xData=xData, pointsOn=pointsOn, theme_custom=theme_custom,
                      panelBGcolor=panelBGcolor, plotBGcolor=plotBGcolor,
                      legendBGcolor=legendBGcolor,yData=yData,xLabel=xLabel,yLabel=yLabel,facetLabelSize = facetLabelSize,
                      facetBorderColor=facetBorderColor,  facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor,
                      sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,
                      sizeLines=sizeLines, chartType = "bar",
                      class ="scenario", position ="dodge",
                      classPalette = metis.colors()[[classPalettex]][!metis.colors()[[classPalettex]] %in% metis.colors()[[classPalettex]][1]],
                      facet_rows="region",facet_columns="scenario",figWidth=figWidth, figHeight=figHeight,
                      dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/compareRegions/",gsub(" ","",paste(unique(unique(tbl$region)),collapse="")),"compareScen", sep = ""),
                      fileName = paste(j,"_figBarDodgedDiffPrcnt_compareScenRegionREF",nameAppend,sep=""),forceFacets = T,
                      pdfpng=pdfpng, legendPosition=legendPosition,paletteRev=F,
                      xOrder = xOrder, colOrder1 = c(colOrder1[1],paste(colOrder1,diffPrcntText,sep="")),colOrderName1 = colOrderName1,
                      colOrder2 = c(colOrder2[1],paste(colOrder2,diffPrcntText,sep="")), colOrderName2 = colOrderName2
          )}

          # Line Chart Overlapped
          if(multiPlotFigsOnly==F){
            if("classPalette" %in% names(tbl_pdC1)){classPalettex=unique(tbl_pdC1$classPalette)}else{classPalettex="pal_Basic"}
          metis.chart(tbl_pdC1%>%dplyr::mutate(units="~Percent"),xData=xData, pointsOn=pointsOn, theme_custom=theme_custom,  panelBGcolor=panelBGcolor,
                      plotBGcolor=plotBGcolor, legendBGcolor=legendBGcolor,yData=yData,xLabel=xLabel,yLabel=yLabel,facetLabelSize = facetLabelSize,
                      facetBorderColor=facetBorderColor,  facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor,
                      sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "line",class ="scenario",
                      classPalette = metis.colors()[[classPalettex]][!metis.colors()[[classPalettex]] %in% metis.colors()[[classPalettex]][1]],
                      facet_rows="region",facet_columns="scenario",figWidth=figWidth, figHeight=figHeight,
                      dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/compareRegions/",gsub(" ","",paste(unique(unique(tbl$region)),collapse="")),"compareScen", sep = ""),
                      fileName = paste(j,"_figLineOverlapDiffPrcnt_compareScenRegionREF",nameAppend,sep=""),forceFacets = T,
                      pdfpng=pdfpng, legendPosition=legendPosition,paletteRev=F,
                      xOrder = xOrder, colOrder1 = c(colOrder1[1],paste(colOrder1,diffPrcntText,sep="")),colOrderName1 = colOrderName1,
                      colOrder2 = c(colOrder2[1],paste(colOrder2,diffPrcntText,sep="")), colOrderName2 = colOrderName2
          )}

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
          if(multiPlotFigsOnly==F){
            if("classPalette" %in% names(tbl_pdC1)){classPalettex=unique(tbl_pdC1$classPalette)}else{classPalettex="pal_Basic"}
          metis.chart(tbl_pdC1%>%dplyr::mutate(units="~Percent"), xData=xData, pointsOn=pointsOn, theme_custom=theme_custom,  panelBGcolor=panelBGcolor,
                      plotBGcolor=plotBGcolor, legendBGcolor=legendBGcolor,yData=yData,xLabel=xLabel,yLabel=yLabel,facetLabelSize = facetLabelSize,
                      facetBorderColor=facetBorderColor,  facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor,sizeBarLines=sizeBarLines,
                      useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar",
                      class ="scenario", position ="dodge",yMax=yMaxDiffPrcnt_i, yMin=yMinDiffPrcnt_i,
                      classPalette = metis.colors()[[classPalettex]][!metis.colors()[[classPalettex]] %in% metis.colors()[[classPalettex]][1]],
                      facet_rows="region",facet_columns="scenario",figWidth=figWidth, figHeight=figHeight,
                      dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/compareRegions/",gsub(" ","",paste(unique(unique(tbl$region)),collapse="")),"compareScen", sep = ""),
                      fileName = paste(j,"_figBarDodgedDiffPrcnt_compareScenRegion",nameAppend,sep=""),forceFacets = T,
                      pdfpng=pdfpng, legendPosition=legendPosition,paletteRev=F,
                      xOrder = xOrder, colOrder1 = c(colOrder1[1],paste(colOrder1,diffPrcntText,sep="")),colOrderName1 = colOrderName1,
                      colOrder2 = c(colOrder2[1],paste(colOrder2,diffPrcntText,sep="")), colOrderName2 = colOrderName2
          )}

          # Line Chart Overlapped
          if(multiPlotFigsOnly==F){
            if("classPalette" %in% names(tbl_pdC1)){classPalettex=unique(tbl_pdC1$classPalette)}else{classPalettex="pal_Basic"}
          metis.chart(tbl_pdC1%>%dplyr::mutate(units="~Percent"),xData=xData, pointsOn=pointsOn, theme_custom=theme_custom,  panelBGcolor=panelBGcolor,
                      plotBGcolor=plotBGcolor, legendBGcolor=legendBGcolor,yData=yData,xLabel=xLabel,yLabel=yLabel,facetLabelSize = facetLabelSize,
                      facetBorderColor=facetBorderColor,  facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor,
                      sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "line",class ="scenario",
                      classPalette = metis.colors()[[classPalettex]][!metis.colors()[[classPalettex]] %in% metis.colors()[[classPalettex]][1]],
                      facet_rows="region",facet_columns="scenario",figWidth=figWidth, figHeight=figHeight,yMax=yMaxDiffPrcnt_i, yMin=yMinDiffPrcnt_i,
                      dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/compareRegions/",gsub(" ","",paste(unique(unique(tbl$region)),collapse="")),"compareScen", sep = ""),
                      fileName = paste(j,"_figLineOverlapDiffPrcnt_compareScenRegion",nameAppend,sep=""),forceFacets = T,
                      pdfpng=pdfpng, legendPosition=legendPosition,paletteRev=F,
                      xOrder = xOrder, colOrder1 = c(colOrder1[1],paste(colOrder1,diffPrcntText,sep="")),colOrderName1 = colOrderName1,
                      colOrder2 = c(colOrder2[1],paste(colOrder2,diffPrcntText,sep="")), colOrderName2 = colOrderName2
          )}
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

      NULL -> yMax_i -> yMin_i
      yMax_i = scaleRange %>% dplyr::filter(param==k) %>% unique()
      if(!is.na(yMax_i$maxScale)){yMax_i = yMax_i$maxScale}else{
        yMax_i=NULL
        print(paste("No scaleRange maxScale for param ", k," provided. Setting yMax to NULL",sep=""))}

      yMin_i = scaleRange %>% dplyr::filter(param==k) %>% unique()
      if(!is.na(yMin_i$maxScale)){yMin_i = yMin_i$minScale}else{
        yMin_i=NULL
        print(paste("No scaleRange minScale for param ", k," provided. Setting yMax to NULL",sep=""))}

      if(!is.null(yMaxDefault) & is.null(yMax_i)){yMax_i=max(yMaxDefault,max(tbl_rsp$value))}
      if(!is.null(yMinDefault) & is.null(yMin_i)){yMin_i=min(yMinDefault,min(tbl_rsp$value))}


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

      if(multiPlotFigsOnly==F){
      metis.chart(tbl_rspC1, xData=xData, pointsOn=pointsOn, theme_custom=theme_custom,  panelBGcolor=panelBGcolor, plotBGcolor=plotBGcolor,
                  legendBGcolor=legendBGcolor,yData=yData,xLabel=xLabel,yLabel=yLabel,facetLabelSize = facetLabelSize, facetBorderColor=facetBorderColor,
                  facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor, yMax=yMax_i, yMin=yMin_i,sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,
                  sizeLines=sizeLines, chartType = "bar",
                  dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i, "/", j,sep = ""),figWidth=figWidth, figHeight=figHeight,
                  fileName = paste(k,"_figBar_",i,"_",j,nameAppend,sep=""),pdfpng=pdfpng, legendPosition=legendPosition, xOrder = xOrder, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
      )}

    # Bar Chart
    metis.chart(tbl_rspC1%>%dplyr::mutate(label=units), xData=xData, pointsOn=pointsOn, theme_custom=theme_custom,
                panelBGcolor=panelBGcolor, plotBGcolor=plotBGcolor, legendBGcolor=legendBGcolor,yData=yData,xLabel=xLabel,yLabel=yLabel,
                facetLabelSize = facetLabelSize, facetBorderColor=facetBorderColor,  facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor,
                yMax=yMax_i, yMin=yMin_i,sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar",
    dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i, "/", j,sep = ""),printFig = F,
    forceFacets = T, facet_columns="label",figWidth=figWidth, figHeight=figHeight,
    fileName = paste(k,"_figBar_",i,"_",j,nameAppend,sep=""),pdfpng=pdfpng, legendPosition=legendPosition, xOrder = xOrder, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
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
    # fileName = paste(k,"_figBar_",i,"_",j,nameAppend,sep="");pdfpng=pdfpng, legendPosition=legendPosition, xOrder = xOrder, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
    if(multiPlotFigsOnly==F){
    metis.chart(tbl_rspC1,xData=xData, pointsOn=pointsOn, theme_custom=theme_custom,  panelBGcolor=panelBGcolor, plotBGcolor=plotBGcolor,
                legendBGcolor=legendBGcolor,yData=yData,xLabel=xLabel,yLabel=yLabel,facetLabelSize = facetLabelSize, facetBorderColor=facetBorderColor,
                facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor, yMax=yMax_i, yMin=yMin_i,sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,
                sizeLines=sizeLines, chartType = "line",
                dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i, "/", j,sep = ""),figWidth=figWidth, figHeight=figHeight,
                fileName = paste(k,"_figLine_",i,"_",j,nameAppend,sep=""),pdfpng=pdfpng, legendPosition=legendPosition, xOrder = xOrder, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
    )}

    # Line Chart
    metis.chart(tbl_rspC1%>%dplyr::mutate(label=units),xData=xData, pointsOn=pointsOn, theme_custom=theme_custom,
                panelBGcolor=panelBGcolor, plotBGcolor=plotBGcolor, legendBGcolor=legendBGcolor,yData=yData,xLabel=xLabel,yLabel=yLabel,
                facetLabelSize = facetLabelSize, facetBorderColor=facetBorderColor,  facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor,
                yMax=yMax_i, yMin=yMin_i,sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "line",
      dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i, "/", j,sep = ""),printFig = F,
      forceFacets = T, facet_columns="label",figWidth=figWidth, figHeight=figHeight,
      fileName = paste(k,"_figLine_",i,"_",j,nameAppend,sep=""),pdfpng=pdfpng, legendPosition=legendPosition, xOrder = xOrder, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
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
      if(multiPlotFigsOnly==F){
      metis.chart(tbl_rsp, xData=xData, pointsOn=pointsOn, theme_custom=theme_custom,  panelBGcolor=panelBGcolor,
                  plotBGcolor=plotBGcolor, legendBGcolor=legendBGcolor,yData=yData,xLabel=xLabel,yLabel=yLabel,
                  facetLabelSize = facetLabelSize, facetBorderColor=facetBorderColor,  facetBGColor=facetBGColor,
                  facetLabelColor=facetLabelColor, yMax=yMax_i, yMin=yMin_i,sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,
                  sizeLines=sizeLines, chartType = "bar",
                  facet_columns = "class2", dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i, "/", j,sep = ""),figWidth=figWidth, figHeight=figHeight,
                  fileName = paste(k,"_figBar_",i,"_Class2_",j,nameAppend,sep="")
      )}

      # Line Chart
      if(multiPlotFigsOnly==F){
      metis.chart(tbl_rsp,xData=xData, pointsOn=pointsOn, theme_custom=theme_custom,  panelBGcolor=panelBGcolor,
                  plotBGcolor=plotBGcolor, legendBGcolor=legendBGcolor,yData=yData,xLabel=xLabel,yLabel=yLabel,facetLabelSize = facetLabelSize,
                  facetBorderColor=facetBorderColor,  facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor, yMax=yMax_i, yMin=yMin_i,
                  sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "line",
                  facet_columns = "class2", dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i, "/", j,sep = ""),figWidth=figWidth, figHeight=figHeight,
                  fileName = paste(k,"_figLine_",i,"_Class2_",j,nameAppend,sep="")
      )}
    }

    } # Close if(nrow(tbl_rsp)>0)

} # close loop for param


    if(multiPlotOn){
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
    print(paste("Params selected but not available for multiPlot are:"))
    print(paste(unique(mpdf$param)[!unique(mpdf$param) %in% unique(unlist(mpParamPlots$param))],collapse=", "))
    print(paste("Params available for multiPlot are:"))
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
          dplyr::filter(paramSet==paramSet_i)%>%unique();
        mpParamPlots_i <- mpParamPlots_i%>%
          dplyr::arrange(match(param,(mpdf%>%dplyr::filter(paramSet==paramSet_i))$param));mpParamPlots_i

        plotSet_i <-unique(mpParamPlots_i$plot);plotSet_i

        # Plot diff 1 Scale
        multiPlotlist_i<-list();
        plotSet_ix <- unique(plotSet_i[grepl("_Bar_",plotSet_i)])
        for(multiPlot_i in 1:length(plotSet_ix)){
          if(multiPlotFigLabels==F){
          multiPlotlist_i[[multiPlot_i]] <-get(plotSet_ix[multiPlot_i])+ggplot2::ylab(NULL)+ggplot2::theme(plot.margin=ggplot2::margin(l=15,r=15,b=30,unit="pt"))}else{
            multiPlotlist_i[[multiPlot_i]] <-get(plotSet_ix[multiPlot_i])+ggplot2::ylab(NULL)+ggplot2::theme(plot.margin=ggplot2::margin(l=65,r=20,b=30,unit="pt"))
          }
        };multiPlotlist_i

        if(multiPlotFigLabels==F){labels1=NULL}else{
        labels1 <-paste(letters[seq(from=1,to=length(multiPlotlist_i),by=1)],")",sep="")}; labels1

        # MultiPlots 1: Ref,diffs1scale,diffsPrcnt,leg
        #ncol
        if(length(plotSet_ix)>unique(mpParamPlots_i$nColMax)){ncol_i=unique(mpParamPlots_i$nColMax)}else{ncol_i=length(plotSet_ix)}; ncol_i
        if((length(plotSet_ix)/ncol_i)<1){nrow_i=1}else{nrow_i=ceiling(length(plotSet_ix)/ncol_i)};nrow_i
        figure<-egg::ggarrange(plots=multiPlotlist_i,ncol = ncol_i,nrow=nrow_i,
                       labels = labels1,
                       label.args = list(gp = grid::gpar(hjust = 0, vjust=0, cex=1*ncol_i*nrow_i))); figure

      metis.printPdfPng(figure=figure,
                          dir=paste(dirOutputs, "/Charts/",folderName,"/", i,"/multiPlot", sep = ""),
                          filename=paste(paramSet_i,"_",i,"_",j,"_mplot_Bar",sep=""),
                          figWidth=figWidth*ncol_i,
                          figHeight=figHeight*nrow_i,
                          pdfpng="png")

      # Plot diff 1 Scale
      multiPlotlist_i<-list();
      plotSet_ix <- unique(plotSet_i[grepl("_Line_",plotSet_i)])
      for(multiPlot_i in 1:length(plotSet_ix)){
        if(multiPlotFigLabels==F){
          multiPlotlist_i[[multiPlot_i]] <-get(plotSet_ix[multiPlot_i])+ggplot2::ylab(NULL)+ggplot2::theme(plot.margin=ggplot2::margin(l=15,r=15,b=30,unit="pt"))}else{
            multiPlotlist_i[[multiPlot_i]] <-get(plotSet_ix[multiPlot_i])+ggplot2::ylab(NULL)+ggplot2::theme(plot.margin=ggplot2::margin(l=65,r=20,b=30,unit="pt"))
          }
      };multiPlotlist_i

      if(multiPlotFigLabels==F){labels1=NULL}else{
      labels1 <-paste(letters[seq(from=1,to=length(multiPlotlist_i),by=1)],")",sep="")}; labels1

      # MultiPlots 1: Ref,diffs1scale,diffsPrcnt,leg
      #ncol
      if(length(plotSet_ix)>unique(mpParamPlots_i$nColMax)){ncol_i=unique(mpParamPlots_i$nColMax)}else{ncol_i=length(plotSet_ix)}; ncol_i
      if((length(plotSet_ix)/ncol_i)<1){nrow_i=1}else{nrow_i=ceiling(length(plotSet_ix)/ncol_i)};nrow_i
      figure<-egg::ggarrange(plots=multiPlotlist_i,ncol = ncol_i,nrow=nrow_i,
                             labels = labels1,
                             label.args = list(gp = grid::gpar(hjust = 0, vjust=0, cex=1*ncol_i*nrow_i))); figure

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

      NULL -> yMin_i -> yMax_i
      yMax_i = scaleRange %>% dplyr::filter(param==j) %>% unique()
      if(!is.na(yMax_i$maxScale)){yMax_i = yMax_i$maxScale}else{
        yMax_i=NULL
        print(paste("No scaleRange maxScale for param ", j," provided. Setting yMax to NULL",sep=""))}

      yMin_i = scaleRange %>% dplyr::filter(param==j) %>% unique()
      if(!is.na(yMin_i$maxScale)){yMin_i = yMin_i$minScale}else{
        yMin_i=NULL
        print(paste("No scaleRange minScale for param ", j," provided. Setting yMax to NULL",sep=""))}

      if(!is.null(yMaxDefault) & is.null(yMax_i)){yMax_i=max(yMaxDefault,max(tbl_rp$value))}
      if(!is.null(yMinDefault) & is.null(yMin_i)){yMin_i=min(yMinDefault,min(tbl_rp$value))}



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
        if(multiPlotFigsOnly==F){
      metis.chart(tbl_rpC1, xData=xData, pointsOn=pointsOn, theme_custom=theme_custom,  panelBGcolor=panelBGcolor,
                  plotBGcolor=plotBGcolor, legendBGcolor=legendBGcolor,yData=yData,xLabel=xLabel,yLabel=yLabel,facetLabelSize = facetLabelSize,
                  facetBorderColor=facetBorderColor,  facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor, yMax=yMax_i, yMin=yMin_i,
                  sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar",
        dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i,"/compareScen",sep = ""),
        facet_columns="scenario",
        fileName = paste(j,"_figBar_",i,"_compareScen",nameAppend,sep=""),
        figWidth = figWidth*max((length(unique(tbl_rp$scenario))/2),1)*figWMult,
        figHeight = figHeight*max((length(unique(tbl_rp$region))/2),1),pdfpng=pdfpng, legendPosition=legendPosition, xOrder = xOrder, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
      )}


      # data=tbl_rpC1; xData=xData;yData=yData;xLabel=xLabel;yLabel=yLabel; sizeBarLines=sizeBarLines;useNewLabels=useNewLabels;sizeLines=sizeLines; chartType = "bar";
      # dirOutputs = paste(dirOutputs, "/Charts/", i,"/compareScen",sep = "")
      # fileName = paste(j,"_figBar_",i,"_compareScen",nameAppend,sep="")
      # figWidth = figWidth*max((length(unique(tbl_rp$scenario))/2),1)*figWMult
      # figHeight = figHeight*max((length(unique(tbl_rp$region))/2),1)
      # pdfpng=pdfpng; legendPosition=legendPosition, xOrder = xOrder, colOrder1 = colOrder1; colOrderName1 = colOrderName1; colOrder2 = colOrder2; colOrderName2 = colOrderName2

      # Line Chart
        if(multiPlotFigsOnly==F){
      metis.chart(tbl_rpC1,xData=xData, pointsOn=pointsOn, theme_custom=theme_custom,  panelBGcolor=panelBGcolor, plotBGcolor=plotBGcolor,
                  legendBGcolor=legendBGcolor,yData=yData,xLabel=xLabel,yLabel=yLabel,facetLabelSize = facetLabelSize, facetBorderColor=facetBorderColor,
                  facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor, yMax=yMax_i, yMin=yMin_i,
                  sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "line",
        dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i,"/compareScen",sep = ""),
        facet_columns="scenario",
        fileName = paste(j,"_figLine_",i,"_compareScen",nameAppend,sep=""),
        figWidth = figWidth*max((length(unique(tbl_rp$scenario))/2),1)*figWMult,
        figHeight = figHeight*max((length(unique(tbl_rp$region))/2),1),pdfpng=pdfpng, legendPosition=legendPosition, xOrder = xOrder, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
      )}



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

      NULL -> yMin_i -> yMax_i
      yMax_i = scaleRange %>% dplyr::filter(param==j) %>% unique()
      if(!is.na(yMax_i$maxScale)){yMax_i = yMax_i$maxScale}else{
        yMax_i=NULL
        print(paste("No scaleRange maxScale for param ", j," provided. Setting yMax to NULL",sep=""))}

      yMin_i = scaleRange %>% dplyr::filter(param==j) %>% unique()
      if(!is.na(yMin_i$maxScale)){yMin_i = yMin_i$minScale}else{
        yMin_i=NULL
        print(paste("No scaleRange minScale for param ", j," provided. Setting yMax to NULL",sep=""))}

      if(!is.null(yMaxDefault) & is.null(yMax_i)){yMax_i=max(yMaxDefault,max(tbl_rpyC1$value))}
      if(!is.null(yMinDefault) & is.null(yMin_i)){yMin_i=min(yMinDefault,min(tbl_rpyC1$value))}



      # Bar Chart
      if(multiPlotFigsOnly==F){
      metis.chart(tbl_rpyC1, xData ="scenario", yData=yData,xLabel=xLabel,yLabel=yLabel, facetLabelSize = xScenCompFacetLabelSize,
                  sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar", facet_columns = xData,
       dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i,"/compareScen",sep = ""),yMax=yMax_i, yMin=yMin_i,
        fileName = paste(j,"_figBar_",i,"_compareScen_xScenSelectYears",nameAppend,sep=""),
        figWidth = figWidth*max((length(unique(tbl_rpy$x)[unique(tbl_rpy$x) %in% xCompare])/3),1)*figWMult,
        figHeight = figHeight*max((length(unique(tbl_rpy$region))/2),1),pdfpng=pdfpng, legendPosition=legendPosition, xOrder = xOrder,
       colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
      )}


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

      NULL -> yMin_i -> yMax_i
      yMax_i = scaleRange %>% dplyr::filter(param==j) %>% unique()
      if(!is.na(yMax_i$maxScale)){yMax_i = yMax_i$maxScale}else{
        yMax_i=NULL
        print(paste("No scaleRange maxScale for param ", j," provided. Setting yMax to NULL",sep=""))}

      yMin_i = scaleRange %>% dplyr::filter(param==j) %>% unique()
      if(!is.na(yMin_i$maxScale)){yMin_i = yMin_i$minScale}else{
        yMin_i=NULL
        print(paste("No scaleRange minScale for param ", j," provided. Setting yMax to NULL",sep=""))}

      if(!is.null(yMaxDefault) & is.null(yMax_i)){yMax_i=max(yMaxDefault,max(tbl_rpAgg$value))}
      if(!is.null(yMinDefault) & is.null(yMin_i)){yMin_i=min(yMinDefault,min(tbl_rpAgg$value))}



      if(nrow(tbl_rpAgg)>0){

        if(length(unique(tbl_rpAgg$class1))>1){if(grepl("right|left",legendPosition,ignore.case = T)){figWMult=1.3}else{figWMult=1}}else{figWMult=1}

      # Bar Chart Dodged
        if(multiPlotFigsOnly==F){
      metis.chart(tbl_rpAgg, xData=xData, pointsOn=pointsOn, theme_custom=theme_custom,  panelBGcolor=panelBGcolor,
                  plotBGcolor=plotBGcolor, legendBGcolor=legendBGcolor,yData=yData,xLabel=xLabel,yLabel=yLabel,facetLabelSize = facetLabelSize,
                  facetBorderColor=facetBorderColor,  facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor, yMax=yMax_i, yMin=yMin_i,
                  sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar", facet_columns=NULL,
                  class ="scenario", position ="dodge", classPalette = classPalette,
        dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i,"/compareScen",sep = ""),
        fileName = paste(j,"_figBarDodged_",i,"_compareScen_",nameAppend,sep=""), paletteRev=F,figHeight=figHeight,
        figWidth = figWidth*figWMult,pdfpng=pdfpng, legendPosition=legendPosition, xOrder = xOrder, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
      )}

        if(multiPlotFigsOnly==F){
      if("classPalette" %in% names(tbl_rpAgg)){classPalettex=unique(tbl_rpAgg$classPalette)}else{classPalettex="pal_Basic"}
      metis.chart(tbl_rpAgg,xData=xData, pointsOn=pointsOn, theme_custom=theme_custom,  panelBGcolor=panelBGcolor, plotBGcolor=plotBGcolor,
                  legendBGcolor=legendBGcolor,yData=yData,xLabel=xLabel,yLabel=yLabel,facetLabelSize = facetLabelSize, facetBorderColor=facetBorderColor,
                  facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor, yMax=yMax_i, yMin=yMin_i, sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,
                  sizeLines=sizeLines, chartType = "line", facet_columns=NULL,
                  class ="scenario", classPalette = classPalette, figHeight=figHeight,
                  dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i,"/compareScen",sep = ""),
                  fileName = paste(j,"_figLineOverlap_",i,"_compareScen",nameAppend,sep=""),figWidth = figWidth*figWMult, paletteRev = F,
                  pdfpng=pdfpng, legendPosition=legendPosition, xOrder = xOrder, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
      )}

      # Line Chart Overlapped
      if("classPalette" %in% names(tbl_rpAgg)){classPalettex=unique(tbl_rpAgg$classPalette)}else{classPalettex="pal_Basic"}
      metis.chart(tbl_rpAgg%>%dplyr::mutate(label=paste(units)),xData=xData, pointsOn=pointsOn, theme_custom=theme_custom,
                  panelBGcolor=panelBGcolor, plotBGcolor=plotBGcolor, legendBGcolor=legendBGcolor,yData=yData,xLabel=xLabel,yLabel=yLabel,
                  facetLabelSize = facetLabelSize, facetBorderColor=facetBorderColor,  facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor,
                  yMax=yMax_i, yMin=yMin_i, sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "line",
                  class ="scenario", classPalette = classPalettex,
        dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i,"/compareScen",sep = ""),
        fileName = paste(j,"_figLineOverlap_",i,"_compareScen",nameAppend,sep=""),figWidth = figWidth*figWMult, figHeight=figHeight,
        printFig = F, forceFacets = T, facet_columns="label",
        pdfpng=pdfpng, legendPosition=legendPosition,
        xOrder = xOrder, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2, paletteRev = F
      )->mpx

      # data=tbl_rpAgg%>%dplyr::mutate(label=paste(units))
      # xData=xData
      # pointsOn=pointsOn
      # yData=yData
      # xLabel=xLabel
      # yLabel=yLabel
      # facetLabelSize = facetLabelSize
      # yMax=yMax_i
      # yMin=yMin_i
      # sizeBarLines=sizeBarLines
      # useNewLabels=useNewLabels
      # sizeLines=sizeLines
      # chartType = "line"
      # class ="scenario"
      # classPalette = classPalettex
      # dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i,"/compareScen",sep = "")
      # fileName = paste(j,"_figLineOverlap_",i,"_compareScen",nameAppend,sep="")
      # figWidth = figWidth*figWMult
      # printFig = F
      # forceFacets = T
      # facet_columns="label"
      # pdfpng=pdfpng
      # legendPosition=legendPosition
      # xOrder = xOrder, colOrder1 = colOrder1
      # colOrderName1 = colOrderName1
      # colOrder2 = colOrder2
      # colOrderName2 = colOrderName2

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
          dplyr::mutate(!!paste(k,diffText,sep=""):=get(k)-get(scenRef_i))%>%
          dplyr::select(-dplyr::one_of(c(k,scenRef_i)))
        tbl_temp<-tbl_temp%>%
          tidyr::gather(key=scenario,value=!!yData,
                        -c(names(tbl_temp)[!names(tbl_temp) %in% paste(k,diffText,sep="")]))
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


      NULL -> yMin_i -> yMax_i
      yMax_i = scaleRange %>% dplyr::filter(param==j) %>% unique()
      if(!is.na(yMax_i$maxScale)){yMax_i = yMax_i$maxScale}else{
        yMax_i=NULL
        print(paste("No scaleRange maxScale for param ", j," provided. Setting yMax to NULL",sep=""))}

      yMin_i = scaleRange %>% dplyr::filter(param==j) %>% unique()
      if(!is.na(yMin_i$maxScale)){yMin_i = yMin_i$minScale}else{
        yMin_i=NULL
        print(paste("No scaleRange minScale for param ", j," provided. Setting yMax to NULL",sep=""))}

      if(!is.null(yMaxDefault) & is.null(yMax_i)){yMax_i=max(yMaxDefault,max(tbl_rpdC1$value))}
      if(!is.null(yMinDefault) & is.null(yMin_i)){yMin_i=min(yMinDefault,min(tbl_rpdC1$value))}

      # Bar Chart
      metis.chart(tbl_rpdC1, xData=xData, pointsOn=pointsOn, theme_custom=theme_custom,  panelBGcolor=panelBGcolor, plotBGcolor=plotBGcolor,
                  legendBGcolor=legendBGcolor,yData=yData,xLabel=xLabel,yLabel=yLabel,facetLabelSize = facetLabelSize, facetBorderColor=facetBorderColor,
                  facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor, sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines,
                  chartType = "bar",yMax=yMax_i, yMin=yMin_i,
                  dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i,"/compareScen",sep = ""),
                  facet_columns="scenario",
                  fileName = paste(j,"_figBarDiff_",i,"_compareScen1Scale",nameAppend,sep=""),
                  figWidth = figWidth*max((length(unique(tbl_rpd$scenario))/2),1)*figWMult,forceFacets = T,
                  figHeight = figHeight*max((length(unique(tbl_rpd$region))/2),1),pdfpng=pdfpng, legendPosition=legendPosition,
                  xOrder = xOrder, colOrder1 = c(colOrder1[1],paste(colOrder1,diffText,sep="")),colOrderName1 = colOrderName1,
                  colOrder2 = c(colOrder2[1],paste(colOrder2,diffText,sep="")), colOrderName2 = colOrderName2
      )->mpx

      metis.chart(tbl_rpdC1%>%dplyr::filter(scenario %in% scenRef_i),printFig=F, xData=xData, pointsOn=pointsOn, theme_custom=theme_custom,
                  panelBGcolor=panelBGcolor, plotBGcolor=plotBGcolor, legendBGcolor=legendBGcolor,yData=yData,xLabel=xLabel,yLabel=yLabel,
                  facetLabelSize = facetLabelSize, facetBorderColor=facetBorderColor,  facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor,
                  sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar",
                  dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i,"/compareScen",sep = ""),
                  facet_columns="scenario",yMax=yMax_i, yMin=yMin_i,
                  fileName = paste(j,"_figBarDiff_",i,"_compareScen1Scale",nameAppend,sep=""),
                  figWidth = figWidth*max((length(unique(tbl_rpd$scenario))/2),1)*figWMult,forceFacets = T,
                  figHeight = figHeight*max((length(unique(tbl_rpd$region))/2),1),pdfpng=pdfpng, legendPosition=legendPosition,
                  xOrder = xOrder, colOrder1 = c(colOrder1[1],paste(colOrder1,diffText,sep="")),colOrderName1 = colOrderName1,
                  colOrder2 = c(colOrder2[1],paste(colOrder2,diffText,sep="")), colOrderName2 = colOrderName2
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
      if(multiPlotFigsOnly==F){
      metis.chart(tbl_rpdC1, xData=xData, pointsOn=pointsOn, theme_custom=theme_custom,  panelBGcolor=panelBGcolor, plotBGcolor=plotBGcolor,
                  legendBGcolor=legendBGcolor,yData=yData,xLabel=xLabel,yLabel=yLabel,facetLabelSize = facetLabelSize, facetBorderColor=facetBorderColor,
                  facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor, sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines,
                  chartType = "line",yMax=yMax_i, yMin=yMin_i,
                  dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i,"/compareScen",sep = ""),
                  facet_columns="scenario",
                  fileName = paste(j,"_figLineDiff_",i,"_compareScen1Scale",nameAppend,sep=""),
                  figWidth = figWidth*max((length(unique(tbl_rpd$scenario))/2),1)*figWMult,forceFacets = T,
                  figHeight = figHeight*max((length(unique(tbl_rpd$region))/2),1),pdfpng=pdfpng, legendPosition=legendPosition,
                  xOrder = xOrder, colOrder1 = c(colOrder1[1],paste(colOrder1,diffText,sep="")),colOrderName1 = colOrderName1,
                  colOrder2 = c(colOrder2[1],paste(colOrder2,diffText,sep="")), colOrderName2 = colOrderName2
      )}

      # dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i,"/compareScen",sep = "");
      # fileName = paste(j,"_figLineDiff_",i,"_compareScen1Scale",nameAppend,sep="");
      # data=tbl_rpdC1; xData=xData; pointsOn=pointsOn;yData=yData;xLabel=xLabel;yLabel=yLabel;facetLabelSize = facetLabelSize; sizeBarLines=sizeBarLines;useNewLabels=useNewLabels;sizeLines=sizeLines; chartType = "line";
      # facet_columns="scenario";
      # figWidth = figWidth*max((length(unique(tbl_rpd$scenario))/2),1)*figWMult;forceFacets = T;
      # figHeight = figHeight*max((length(unique(tbl_rpd$region))/2),1);pdfpng=pdfpng; legendPosition=legendPosition;
      # xOrder = xOrder, colOrder1 = c(colOrder1[1],paste(colOrder1,diffText,sep=""));colOrderName1 = colOrderName1;
      # colOrder2 = c(colOrder2[1],paste(colOrder2,diffText,sep="")); colOrderName2 = colOrderName2

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

      # Set Scale Ranges for Diffs
      NULL -> yMaxDiffAbs_i -> yMinDiffAbs_i
      yMaxDiffAbs_i = scaleRange %>% dplyr::filter(param==j) %>% unique()
      if(!is.na(yMaxDiffAbs_i$maxDiffAbs)){yMaxDiffAbs_i = yMaxDiffAbs_i$maxDiffAbs}else{
        yMaxDiffAbs_i=NULL
        print(paste("No scaleRange maxScale for param ", j," provided. Setting yMaxDiffAbs to NULL",sep=""))}

      yMinDiffAbs_i = scaleRange %>% dplyr::filter(param==j) %>% unique()
      if(!is.na(yMinDiffAbs_i$minDiffAbs)){yMinDiffAbs_i = yMinDiffAbs_i$minDiffAbs}else{
        yMinDiffAbs_i=NULL
        print(paste("No scaleRange minScale for param ", j," provided. Setting yMaxDiffAbs to NULL",sep=""))}

      if(!is.null(yMaxDiffAbsDefault) & is.null(yMaxDiffAbs_i)){yMaxDiffAbs_i=max(yMaxDiffAbsDefault,max(tbl_rpdC1$value))}
      if(!is.null(yMinDiffAbsDefault) & is.null(yMinDiffAbs_i)){yMinDiffAbs_i=min(yMinDiffAbsDefault,min(tbl_rpdC1$value))}


      # Bar Chart
      metis.chart(tbl_rpdC1, xData=xData, pointsOn=pointsOn, theme_custom=theme_custom,  panelBGcolor=panelBGcolor,
                  plotBGcolor=plotBGcolor, legendBGcolor=legendBGcolor,yData=yData,xLabel=xLabel,yLabel=yLabel,facetLabelSize = facetLabelSize,
                  facetBorderColor=facetBorderColor,  facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor, sizeBarLines=sizeBarLines,
                  useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar",yMax=yMaxDiffAbs_i, yMin=yMinDiffAbs_i,
        dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i,"/compareScen",sep = ""),
        facet_columns="scenario",
        fileName = paste(j,"_figBarDiff_",i,"_compareScen",nameAppend,sep=""),
        figWidth = figWidth*((length(unique(tbl_rpd$scenario)))/((length(unique(tbl_rpd$scenario)))+1))*max((length(unique(tbl_rpd$scenario))/2),1)*figWMult,forceFacets = T,
        figHeight = figHeight*max((length(unique(tbl_rpd$region))/2),1),pdfpng=pdfpng, legendPosition=legendPosition,
        xOrder = xOrder, colOrder1 = c(colOrder1[1],paste(colOrder1,diffText,sep="")),colOrderName1 = colOrderName1,
        colOrder2 = c(colOrder2[1],paste(colOrder2,diffText,sep="")), colOrderName2 = colOrderName2
      )->mpx

      # data=tbl_rpdC1; xData=xData; pointsOn=pointsOn; theme_custom=theme_custom;  panelBGcolor=panelBGcolor;
      # plotBGcolor=plotBGcolor; legendBGcolor=legendBGcolor;yData=yData;xLabel=xLabel;yLabel=yLabel;facetLabelSize = facetLabelSize;
      # facetBorderColor=facetBorderColor;  facetBGColor=facetBGColor;  facetLabelColor=facetLabelColor; sizeBarLines=sizeBarLines;
      # useNewLabels=useNewLabels
      # sizeLines=sizeLines
      # chartType = "bar"
      # yMax=yMaxDiffAbs_i
      # yMin=yMinDiffAbs_i
      # dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i,"/compareScen",sep = "")
      # facet_columns="scenario"
      # fileName = paste(j,"_figBarDiff_",i,"_compareScen",nameAppend,sep="")
      # figWidth = figWidth*((length(unique(tbl_rpd$scenario)))/((length(unique(tbl_rpd$scenario)))+1))*max((length(unique(tbl_rpd$scenario))/2),1)*figWMult
      # forceFacets = T
      # figHeight = figHeight*max((length(unique(tbl_rpd$region))/2),1)
      # pdfpng=pdfpng
      # legendPosition=legendPosition
      # xOrder = xOrder
      # colOrder1 = c(colOrder1[1],paste(colOrder1,diffText,sep=""))
      # colOrderName1 = colOrderName1
      # colOrder2 = c(colOrder2[1],paste(colOrder2,diffText,sep=""))
      # colOrderName2 = colOrderName2


      if(j %in% unlist(mp$param)){
        mpnScen<-length(unique(tbl_rpdC1$scenario))
        assign(paste("mp_barDiffAbs_",j,sep=""),mpx)
        mpParamPlots <- mpParamPlots %>%
          dplyr::bind_rows(tibble::tibble(param=c(j),
                                          plot=c(paste("mp_barDiffAbs_",j,sep="")),
                           nScen=c(rep(mpnScen,1)))); mpParamPlots
      }

      # Line Chart
      if(multiPlotFigsOnly==F){
     metis.chart(tbl_rpdC1, xData=xData, pointsOn=pointsOn, theme_custom=theme_custom,  panelBGcolor=panelBGcolor,
                 plotBGcolor=plotBGcolor, legendBGcolor=legendBGcolor,yData=yData,xLabel=xLabel,yLabel=yLabel,facetLabelSize = facetLabelSize,
                 facetBorderColor=facetBorderColor,  facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor, sizeBarLines=sizeBarLines,
                 useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "line",yMax=yMaxDiffAbs_i, yMin=yMinDiffAbs_i,
        dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i,"/compareScen",sep = ""),
        facet_columns="scenario",
        fileName = paste(j,"_figLineDiff_",i,"_compareScen",nameAppend,sep=""),
        figWidth = figWidth*((length(unique(tbl_rpd$scenario)))/((length(unique(tbl_rpd$scenario)))+1))*max((length(unique(tbl_rpd$scenario))/2),1)*figWMult,forceFacets = T,
        figHeight = figHeight*max((length(unique(tbl_rpd$region))/2),1),pdfpng=pdfpng, legendPosition=legendPosition,
        xOrder = xOrder, colOrder1 = c(colOrder1[1],paste(colOrder1,diffText,sep="")),colOrderName1 = colOrderName1,
        colOrder2 = c(colOrder2[1],paste(colOrder2,diffText,sep="")), colOrderName2 = colOrderName2
      )}


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
       if(multiPlotFigsOnly==F){
       if("classPalette" %in% names(tbl_rpdAgg)){classPalettex=unique(tbl_rpdAgg$classPalette)}else{classPalettex="pal_Basic"}
       metis.chart(tbl_rpdAgg, xData=xData, pointsOn=pointsOn, theme_custom=theme_custom,  panelBGcolor=panelBGcolor, plotBGcolor=plotBGcolor,
                   legendBGcolor=legendBGcolor,yData=yData,xLabel=xLabel,yLabel=yLabel,facetLabelSize = facetLabelSize, facetBorderColor=facetBorderColor,
                   facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor, sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines,
                   chartType = "bar", facet_columns=NULL,yMax=yMaxDiffAbs_i, yMin=yMinDiffAbs_i,
                   class ="scenario", position ="dodge",
                   classPalette = metis.colors()[[classPalettex]][!metis.colors()[[classPalettex]] %in% metis.colors()[[classPalettex]][1]],
                   dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i,"/compareScen",sep = ""),
                   fileName = paste(j,"_figBarDodgedDiff_",i,"_compareScen_",nameAppend,sep=""), forceFacets = T,
                   pdfpng=pdfpng, legendPosition=legendPosition,paletteRev=F,figWidth=figWidth, figHeight=figHeight,
                   xOrder = xOrder, colOrder1 = c(colOrder1[1],paste(colOrder1,diffText,sep="")),colOrderName1 = colOrderName1,
                   colOrder2 = c(colOrder2[1],paste(colOrder2,diffText,sep="")), colOrderName2 = colOrderName2
       )}

       # Line Chart Overlapped
       if(multiPlotFigsOnly==F){
         if("classPalette" %in% names(tbl_rpdAgg)){classPalettex=unique(tbl_rpdAgg$classPalette)}else{classPalettex="pal_Basic"}
       metis.chart(tbl_rpdAgg,xData=xData, pointsOn=pointsOn, theme_custom=theme_custom,  panelBGcolor=panelBGcolor, plotBGcolor=plotBGcolor,
                   legendBGcolor=legendBGcolor,yData=yData,xLabel=xLabel,yLabel=yLabel,facetLabelSize = facetLabelSize, facetBorderColor=facetBorderColor,
                   facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor,sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines,
                   chartType = "line", facet_columns=NULL,yMax=yMaxDiffAbs_i, yMin=yMinDiffAbs_i,
                   class ="scenario",
                   classPalette = metis.colors()[[classPalettex]][!metis.colors()[[classPalettex]] %in% metis.colors()[[classPalettex]][1]],
                   dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i,"/compareScen",sep = ""),
                   fileName = paste(j,"_figLineOverlapDiff_",i,"_compareScen",nameAppend,sep=""),
                   forceFacets = T,figWidth=figWidth, figHeight=figHeight,
                   pdfpng=pdfpng, legendPosition=legendPosition,paletteRev=F,
                   xOrder = xOrder, colOrder1 = c(colOrder1[1],paste(colOrder1,diffText,sep="")),colOrderName1 = colOrderName1,
                   colOrder2 = c(colOrder2[1],paste(colOrder2,diffText,sep="")), colOrderName2 = colOrderName2
       )}

       # Line Chart Overlapped
       if("classPalette" %in% names(tbl_rpdAgg)){classPalettex=unique(tbl_rpdAgg$classPalette)}else{classPalettex="pal_Basic"}
       metis.chart(tbl_rpdAgg%>%dplyr::mutate(label="Absolute Difference"),xData=xData, pointsOn=pointsOn, theme_custom=theme_custom,
                   panelBGcolor=panelBGcolor, plotBGcolor=plotBGcolor, legendBGcolor=legendBGcolor,yData=yData,xLabel=xLabel,yLabel=yLabel,
                   facetLabelSize = facetLabelSize, facetBorderColor=facetBorderColor,  facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor,
                   sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "line",yMax=yMaxDiffAbs_i, yMin=yMinDiffAbs_i,
                   class ="scenario",
                   classPalette =  metis.colors()[[classPalettex]][!metis.colors()[[classPalettex]] %in% metis.colors()[[classPalettex]][1]],
                   dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i,"/compareScen",sep = ""),
                   fileName = paste(j,"_figLineOverlapDiff_",i,"_compareScen",nameAppend,sep=""),
                   forceFacets = T,printFig = F,facet_columns="label",figWidth=figWidth, figHeight=figHeight,
                   pdfpng=pdfpng, legendPosition=legendPosition,paletteRev=F,
                   xOrder = xOrder, colOrder1 = c(colOrder1[1],paste(colOrder1,diffText,sep="")),colOrderName1 = colOrderName1,
                   colOrder2 = c(colOrder2[1],paste(colOrder2,diffText,sep="")), colOrderName2 = colOrderName2
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
         dplyr::mutate(!!paste(k,diffPrcntText,sep=""):=(get(k)-get(scenRef_i))*100/get(scenRef_i))%>%
         dplyr::select(-dplyr::one_of(c(k,scenRef_i)))
       tbl_temp<-tbl_temp%>%
         tidyr::gather(key=scenario,value=!!yData,
                       -c(names(tbl_temp)[!names(tbl_temp) %in% paste(k,diffPrcntText,sep="")]))
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
     if(multiPlotFigsOnly==F){
     metis.chart(tbl_rpdC1, xData=xData, pointsOn=pointsOn, theme_custom=theme_custom,  panelBGcolor=panelBGcolor, plotBGcolor=plotBGcolor,
                 legendBGcolor=legendBGcolor,yData=yData,xLabel=xLabel,yLabel=yLabel,facetLabelSize = facetLabelSize, facetBorderColor=facetBorderColor,
                 facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor, sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines,
                 chartType = "bar",
                 dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i,"/compareScen",sep = ""),
                 facet_columns="scenario",
                 fileName = paste(j,"_figBarDiffPrcnt_",i,"_compareScen1Scale",nameAppend,sep=""),
                 figWidth = figWidth*max((length(unique(tbl_rpd$scenario))/2),1)*figWMult,forceFacets = T,
                 figHeight = figHeight*max((length(unique(tbl_rpd$region))/2),1),pdfpng=pdfpng, legendPosition=legendPosition,
                 xOrder = xOrder, colOrder1 = c(colOrder1[1],paste(colOrder1,diffPrcntText,sep="")),colOrderName1 = colOrderName1,
                 colOrder2 = c(colOrder2[1],paste(colOrder2,diffPrcntText,sep="")), colOrderName2 = colOrderName2
     )}

     # Line Chart
     if(multiPlotFigsOnly==F){
     metis.chart(tbl_rpdC1, xData=xData, pointsOn=pointsOn, theme_custom=theme_custom,  panelBGcolor=panelBGcolor, plotBGcolor=plotBGcolor,
                 legendBGcolor=legendBGcolor,yData=yData,xLabel=xLabel,yLabel=yLabel,facetLabelSize = facetLabelSize, facetBorderColor=facetBorderColor,
                 facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor, sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines,
                 chartType = "line",
                 dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i,"/compareScen",sep = ""),
                 facet_columns="scenario",
                 fileName = paste(j,"_figLineDiffPrcnt_",i,"_compareScen1Scale",nameAppend,sep=""),
                 figWidth = figWidth*max((length(unique(tbl_rpd$scenario))/2),1)*figWMult,forceFacets = T,
                 figHeight = figHeight*max((length(unique(tbl_rpd$region))/2),1),pdfpng=pdfpng, legendPosition=legendPosition,
                 xOrder = xOrder, colOrder1 = c(colOrder1[1],paste(colOrder1,diffPrcntText,sep="")),colOrderName1 = colOrderName1,
                 colOrder2 = c(colOrder2[1],paste(colOrder2,diffPrcntText,sep="")), colOrderName2 = colOrderName2
     )}


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

     # Set Scale Ranges for Diffs
     NULL -> yMaxDiffPrcnt_i -> yMinDiffPrcnt_i
     yMaxDiffPrcnt_i = scaleRange %>% dplyr::filter(param==j) %>% unique()
     if(!is.na(yMaxDiffPrcnt_i$maxDiffPrcnt)){yMaxDiffPrcnt_i = yMaxDiffPrcnt_i$maxDiffPrcnt}else{
       yMaxDiffPrcnt_i=NULL
       print(paste("No scaleRange maxScale for param ", j," provided. Setting yMaxDiffPrcnt to NULL",sep=""))}

     yMinDiffPrcnt_i = scaleRange %>% dplyr::filter(param==j) %>% unique()
     if(!is.na(yMinDiffPrcnt_i$minDiffPrcnt)){yMinDiffPrcnt_i = yMinDiffPrcnt_i$minDiffPrcnt}else{
       yMinDiffPrcnt_i=NULL
       print(paste("No scaleRange minScale for param ", j," provided. Setting yMaxDiffPrcnt to NULL",sep=""))}

     if(!is.null(yMaxDiffPrcntDefault) & is.null(yMaxDiffPrcnt_i)){yMaxDiffPrcnt_i=max(yMaxDiffPrcntDefault,max(tbl_rpdC1$value))}
     if(!is.null(yMinDiffPrcntDefault) & is.null(yMinDiffPrcnt_i)){yMinDiffPrcnt_i=min(yMinDiffPrcntDefault,min(tbl_rpdC1$value))}


     # Bar Chart
     if(multiPlotFigsOnly==F){
     metis.chart(tbl_rpdC1, xData=xData, pointsOn=pointsOn, theme_custom=theme_custom,  panelBGcolor=panelBGcolor,
                 plotBGcolor=plotBGcolor, legendBGcolor=legendBGcolor,yData=yData,xLabel=xLabel,yLabel=yLabel,
                 facetLabelSize = facetLabelSize, facetBorderColor=facetBorderColor,  facetBGColor=facetBGColor,
                 facetLabelColor=facetLabelColor, sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar",
                 dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i,"/compareScen",sep = ""),
                 facet_columns="scenario",yMax=yMaxDiffPrcnt_i, yMin=yMinDiffPrcnt_i,
                 fileName = paste(j,"_figBarDiffPrcnt_",i,"_compareScen",nameAppend,sep=""),
                 figWidth = figWidth*((length(unique(tbl_rpd$scenario)))/((length(unique(tbl_rpd$scenario)))+1))*max((length(unique(tbl_rpd$scenario))/2),1)*figWMult,forceFacets = T,
                 figHeight = figHeight*max((length(unique(tbl_rpd$region))/2),1),pdfpng=pdfpng, legendPosition=legendPosition,
                 xOrder = xOrder, colOrder1 = c(colOrder1[1],paste(colOrder1,diffPrcntText,sep="")),colOrderName1 = colOrderName1,
                 colOrder2 = c(colOrder2[1],paste(colOrder2,diffPrcntText,sep="")), colOrderName2 = colOrderName2
     )}

     # Line Chart
     metis.chart(tbl_rpdC1, xData=xData, pointsOn=pointsOn, theme_custom=theme_custom,  panelBGcolor=panelBGcolor,
                 plotBGcolor=plotBGcolor, legendBGcolor=legendBGcolor,yData=yData,xLabel=xLabel,yLabel=yLabel,
                 facetLabelSize = facetLabelSize, facetBorderColor=facetBorderColor,  facetBGColor=facetBGColor,
                 facetLabelColor=facetLabelColor, sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "line",
                 dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i,"/compareScen",sep = ""),
                 facet_columns="scenario",yMax=yMaxDiffPrcnt_i, yMin=yMinDiffPrcnt_i,
                 fileName = paste(j,"_figLineDiffPrcnt_",i,"_compareScen",nameAppend,sep=""),
                 figWidth = figWidth*((length(unique(tbl_rpd$scenario)))/((length(unique(tbl_rpd$scenario)))+1))*max((length(unique(tbl_rpd$scenario))/2),1)*figWMult,forceFacets = T,
                 figHeight = figHeight*max((length(unique(tbl_rpd$region))/2),1),pdfpng=pdfpng, legendPosition=legendPosition,
                 xOrder = xOrder, colOrder1 = c(colOrder1[1],paste(colOrder1,diffPrcntText,sep="")),colOrderName1 = colOrderName1,
                 colOrder2 = c(colOrder2[1],paste(colOrder2,diffPrcntText,sep="")), colOrderName2 = colOrderName2
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
         dplyr::mutate(!!paste(k,diffPrcntText,sep=""):=(get(k)-get(scenRef_i))*100/get(scenRef_i))%>%
         dplyr::select(-dplyr::one_of(c(k,scenRef_i)))
       tbl_temp<-tbl_temp%>%
         tidyr::gather(key=scenario,value=!!yData,
                       -c(names(tbl_temp)[!names(tbl_temp) %in% paste(k,diffPrcntText,sep="")]))
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
       if(multiPlotFigsOnly==F){
         if("classPalette" %in% names(tbl_rpdC1)){classPalettex=unique(tbl_rpdC1$classPalette)}else{classPalettex="pal_Basic"}
       metis.chart(tbl_rpdC1, xData=xData, pointsOn=pointsOn, theme_custom=theme_custom,  panelBGcolor=panelBGcolor, plotBGcolor=plotBGcolor,
                   legendBGcolor=legendBGcolor,yData=yData,xLabel=xLabel,yLabel=yLabel,facetLabelSize = facetLabelSize, facetBorderColor=facetBorderColor,
                   facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor, sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines,
                   chartType = "bar", facet_columns=NULL,
                   class ="scenario", position ="dodge",
                   classPalette = metis.colors()[[classPalettex]][!metis.colors()[[classPalettex]] %in% metis.colors()[[classPalettex]][1]],
                   dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i,"/compareScen",sep = ""),
                   fileName = paste(j,"_figBarDodgedDiffPrcnt_",i,"_compareScen1Scale",nameAppend,sep=""),forceFacets = T,
                   pdfpng=pdfpng, legendPosition=legendPosition,paletteRev=F,figWidth=figWidth, figHeight=figHeight,
                   xOrder = xOrder, colOrder1 = c(colOrder1[1],paste(colOrder1,diffPrcntText,sep="")),colOrderName1 = colOrderName1,
                   colOrder2 = c(colOrder2[1],paste(colOrder2,diffPrcntText,sep="")), colOrderName2 = colOrderName2
       )}

       # Line Chart Overlapped
       if(multiPlotFigsOnly==F){
         if("classPalette" %in% names(tbl_rpdC1)){classPalettex=unique(tbl_rpdC1$classPalette)}else{classPalettex="pal_Basic"}
       metis.chart(tbl_rpdC1,xData=xData, pointsOn=pointsOn, theme_custom=theme_custom,  panelBGcolor=panelBGcolor, plotBGcolor=plotBGcolor,
                   legendBGcolor=legendBGcolor,yData=yData,xLabel=xLabel,yLabel=yLabel,facetLabelSize = facetLabelSize, facetBorderColor=facetBorderColor,
                   facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor, sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines,
                   chartType = "line", facet_columns=NULL,
                   class ="scenario",
                   classPalette = metis.colors()[[classPalettex]][!metis.colors()[[classPalettex]] %in% metis.colors()[[classPalettex]][1]],
                   dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i,"/compareScen",sep = ""),
                   fileName = paste(j,"_figLineOverlapDiffPrcnt_",i,"_compareScen1Scale",nameAppend,sep=""),
                   forceFacets = T,figWidth=figWidth, figHeight=figHeight,
                   pdfpng=pdfpng, legendPosition=legendPosition,paletteRev=F,
                   xOrder = xOrder, colOrder1 = c(colOrder1[1],paste(colOrder1,diffPrcntText,sep="")),colOrderName1 = colOrderName1,
                   colOrder2 = c(colOrder2[1],paste(colOrder2,diffPrcntText,sep="")), colOrderName2 = colOrderName2
       )}
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

     # Set Scale Ranges for Diffs
     NULL -> yMaxDiffPrcnt_i -> yMinDiffPrcnt_i
     yMaxDiffPrcnt_i = scaleRange %>% dplyr::filter(param==j) %>% unique()
     if(!is.na(yMaxDiffPrcnt_i$maxDiffPrcnt)){yMaxDiffPrcnt_i = yMaxDiffPrcnt_i$maxDiffPrcnt}else{
       yMaxDiffPrcnt_i=NULL
       print(paste("No scaleRange maxScale for param ", j," provided. Setting yMaxDiffPrcnt to NULL",sep=""))}

     yMinDiffPrcnt_i = scaleRange %>% dplyr::filter(param==j) %>% unique()
     if(!is.na(yMinDiffPrcnt_i$minDiffPrcnt)){yMinDiffPrcnt_i = yMinDiffPrcnt_i$minDiffPrcnt}else{
       yMinDiffPrcnt_i=NULL
       print(paste("No scaleRange minScale for param ", j," provided. Setting yMaxDiffPrcnt to NULL",sep=""))}

     if(!is.null(yMaxDiffPrcntDefault) & is.null(yMaxDiffPrcnt_i)){yMaxDiffPrcnt_i=max(yMaxDiffPrcntDefault,max(tbl_rpdC1$value))}
     if(!is.null(yMinDiffPrcntDefault) & is.null(yMinDiffPrcnt_i)){yMinDiffPrcnt_i=min(yMinDiffPrcntDefault,min(tbl_rpdC1$value))}


     if(nrow(tbl_rpdC1)>0){

       if(grepl("right|left",legendPosition,ignore.case = T)){figWMult=1.3}else{figWMult=1}

       # Bar Chart Dodged
       if(multiPlotFigsOnly==F){
         if("classPalette" %in% names(tbl_rpdC1)){classPalettex=unique(tbl_rpdC1$classPalette)}else{classPalettex="pal_Basic"}
       metis.chart(tbl_rpdC1, xData=xData, pointsOn=pointsOn, theme_custom=theme_custom,  panelBGcolor=panelBGcolor,
                   plotBGcolor=plotBGcolor, legendBGcolor=legendBGcolor,yData=yData,xLabel=xLabel,yLabel=yLabel,
                   facetLabelSize = facetLabelSize, facetBorderColor=facetBorderColor,  facetBGColor=facetBGColor,
                   facetLabelColor=facetLabelColor, sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines,
                   chartType = "bar", facet_columns=NULL,
                   class ="scenario", position ="dodge",yMax=yMaxDiffPrcnt_i, yMin=yMinDiffPrcnt_i,
                   classPalette = metis.colors()[[classPalettex]][!metis.colors()[[classPalettex]] %in% metis.colors()[[classPalettex]][1]],
                   dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i,"/compareScen",sep = ""),
                   fileName = paste(j,"_figBarDodgedDiffPrcnt_",i,"_compareScen_",nameAppend,sep=""),forceFacets = T,
                   pdfpng=pdfpng, legendPosition=legendPosition,paletteRev=F,figWidth=figWidth, figHeight=figHeight,
                   xOrder = xOrder, colOrder1 = c(colOrder1[1],paste(colOrder1,diffPrcntText,sep="")),colOrderName1 = colOrderName1,
                   colOrder2 = c(colOrder2[1],paste(colOrder2,diffPrcntText,sep="")), colOrderName2 = colOrderName2
       )}

       # Line Chart Overlapped
       if(multiPlotFigsOnly==F){
         if("classPalette" %in% names(tbl_rpdC1)){classPalettex=unique(tbl_rpdC1$classPalette)}else{classPalettex="pal_Basic"}
       metis.chart(tbl_rpdC1,xData=xData, pointsOn=pointsOn, theme_custom=theme_custom,  panelBGcolor=panelBGcolor,
                   plotBGcolor=plotBGcolor, legendBGcolor=legendBGcolor,yData=yData,xLabel=xLabel,yLabel=yLabel,
                   facetLabelSize = facetLabelSize, facetBorderColor=facetBorderColor,  facetBGColor=facetBGColor,
                   facetLabelColor=facetLabelColor, sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "line",
                   class ="scenario",yMax=yMaxDiffPrcnt_i, yMin=yMinDiffPrcnt_i,
                   classPalette = metis.colors()[[classPalettex]][!metis.colors()[[classPalettex]] %in% metis.colors()[[classPalettex]][1]],
                   dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i,"/compareScen",sep = ""),
                   fileName = paste(j,"_figLineOverlapDiffPrcnt_",i,"_compareScen",nameAppend,sep=""),
                   forceFacets = T,facet_columns=NULL,figWidth=figWidth, figHeight=figHeight,
                   pdfpng=pdfpng, legendPosition=legendPosition,paletteRev=F,
                   xOrder = xOrder, colOrder1 = c(colOrder1[1],paste(colOrder1,diffPrcntText,sep="")),colOrderName1 = colOrderName1,
                   colOrder2 = c(colOrder2[1],paste(colOrder2,diffPrcntText,sep="")), colOrderName2 = colOrderName2
       )}

       # Line Chart Overlapped
       if("classPalette" %in% names(tbl_rpdC1)){classPalettex=unique(tbl_rpdC1$classPalette)}else{classPalettex="pal_Basic"}
       metis.chart(tbl_rpdC1%>%dplyr::mutate(label="Percent Difference (%)"),xData=xData, pointsOn=pointsOn,
                   theme_custom=theme_custom,  panelBGcolor=panelBGcolor, plotBGcolor=plotBGcolor, legendBGcolor=legendBGcolor,
                   yData=yData,xLabel=xLabel,yLabel=yLabel,facetLabelSize = facetLabelSize, facetBorderColor=facetBorderColor,
                   facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor, sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,
                   sizeLines=sizeLines, chartType = "line",yMax=yMaxDiffPrcnt_i, yMin=yMinDiffPrcnt_i,
                   class ="scenario",
                   classPalette = metis.colors()[[classPalettex]][!metis.colors()[[classPalettex]] %in% metis.colors()[[classPalettex]][1]],
                   dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i,"/compareScen",sep = ""),
                   fileName = paste(j,"_figLineOverlapDiffPrcnt_",i,"_compareScen",nameAppend,sep=""),
                   forceFacets = T,facet_columns="label",printFig = F,figWidth=figWidth, figHeight=figHeight,
                   pdfpng=pdfpng, legendPosition=legendPosition,paletteRev=F,
                   xOrder = xOrder, colOrder1 = c(colOrder1[1],paste(colOrder1,diffPrcntText,sep="")),colOrderName1 = colOrderName1,
                   colOrder2 = c(colOrder2[1],paste(colOrder2,diffPrcntText,sep="")), colOrderName2 = colOrderName2
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
  if(multiPlotOn){
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
  print(paste("Params selected but not available for multiPlot are:"))
  print(paste(unique(mpdf$param)[!unique(mpdf$param) %in% unique(unlist(mpParamPlots$param))],collapse=", "))
  print(paste("Params available for multiPlot are:"))
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

  lastrows <- tibble::tibble()
  for(ps_i in unique(mpdf$paramSet)){
    pn_i<-length(unique((mpdf%>%dplyr::filter(paramSet==ps_i))$param))
    lastrows <- lastrows%>%
      dplyr::bind_rows(tibble::tibble(paramSet=c(ps_i),lastrow=pn_i))%>%
      dplyr::mutate(lastrowCum=cumsum(lastrow))
  };lastrows

  for(row_i in 1:nrow(mpdf)){

    if(row_i>1){
    if(mpdf[row_i,]$paramSet!=mpdf[row_i-1,]$paramSet){
      countMultiLabel=0
    }else{countMultiLabel=countMultiLabel+1}}

    mpParamPlots_i <- mpParamPlotsx %>%
      dplyr::filter(paramSet==mpdf[row_i,]$paramSet,param==mpdf[row_i,]$param)%>%unique();mpParamPlots_i

    mpParamPlots_i <- mpParamPlots_i%>%
      dplyr::arrange(match(param,(mpdf%>%dplyr::filter(paramSet==paramSet_i))$param));mpParamPlots_i

    plotSet_i <-unique(mpParamPlots_i$plot);plotSet_i

    if(multiPlotAllYears==T){
      if(multiPlotFigLabels==F){
      pBRef<-get(plotSet_i[grepl("barDiffAbs1ScaleRef_",plotSet_i)])+ggplot2::theme(plot.margin=ggplot2::margin(l=15, r=15,b=30,unit="pt"),legend.position="none")
      pBDiff1S<-get(plotSet_i[grepl("barDiffAbs1Scale_",plotSet_i)])+ggplot2::theme(plot.margin=ggplot2::margin(l=15, r=15,b=30,unit="pt"),legend.position="none")
      pBDiffMS<-get(plotSet_i[grepl("barDiffAbs_",plotSet_i)])+ggplot2::ylab(NULL)+ggplot2::theme(plot.margin=ggplot2::margin(l=15, r=15,b=30,unit="pt"),legend.position="none")
      pLDiffPrcnt<-get(plotSet_i[grepl("lineDiffPrcnt_",plotSet_i)])+ggplot2::ylab(NULL)+ggplot2::theme(plot.margin=ggplot2::margin(l=15, r=15,b=30,unit="pt"),legend.position="none")
      }else{
        pBRef<-get(plotSet_i[grepl("barDiffAbs1ScaleRef_",plotSet_i)])+ggplot2::theme(plot.margin=ggplot2::margin(l=65,r=20,b=30,unit="pt"),legend.position="none")
        pBDiff1S<-get(plotSet_i[grepl("barDiffAbs1Scale_",plotSet_i)])+ggplot2::theme(plot.margin=ggplot2::margin(l=65,r=20,b=30,unit="pt"),legend.position="none")
        pBDiffMS<-get(plotSet_i[grepl("barDiffAbs_",plotSet_i)])+ggplot2::ylab(NULL)+ggplot2::theme(plot.margin=ggplot2::margin(l=65,r=20,b=30,unit="pt"),legend.position="none")
        pLDiffPrcnt<-get(plotSet_i[grepl("lineDiffPrcnt_",plotSet_i)])+ggplot2::ylab(NULL)+ggplot2::theme(plot.margin=ggplot2::margin(l=65,r=20,b=30,unit="pt"),legend.position="none")
      }
      g <- ggplot2::ggplot_build(pBRef);
      pFilln<-unique(g$data[[1]]["fill"]);pFilln
      pColn<-unique(g$data[[1]]["colour"]);pColn
      if(nrow(pFilln)>1|nrow(pColn)>1){
        pBLeg<-ggpubr::as_ggplot(ggpubr::get_legend(pBRef+ggplot2::theme(legend.position = "right")))}else{
          pBLeg<-ggplot2::ggplot() + ggplot2::theme_void()
        }
      pBRef;pBDiff1S;pBDiffMS;pLDiffPrcnt;pBLeg
    }else{
      if(!row_i %in% unique(lastrows$lastrowCum)){
        if(multiPlotFigLabels==F){
          pBRef<-get(plotSet_i[grepl("barDiffAbs1ScaleRef_",plotSet_i)])+
            ggplot2::theme(plot.margin=ggplot2::margin(l=15, r=15,b=30,unit="pt"),
                  axis.title.x=ggplot2::element_blank(),
                  axis.text.x=ggplot2::element_blank(),
                  axis.ticks.x=ggplot2::element_blank(),legend.position="none")
          pBDiff1S<-get(plotSet_i[grepl("barDiffAbs1Scale_",plotSet_i)])+
            ggplot2::theme(plot.margin=ggplot2::margin(l=15, r=15,b=30,unit="pt"),
                  axis.title.x=ggplot2::element_blank(),
                  axis.text.x=ggplot2::element_blank(),
                  axis.ticks.x=ggplot2::element_blank(),legend.position="none")
          pBDiffMS<-get(plotSet_i[grepl("barDiffAbs_",plotSet_i)])+ggplot2::ylab(NULL)+
            ggplot2::theme(plot.margin=ggplot2::margin(l=15, r=15,b=30,unit="pt"),
                  axis.title.x=ggplot2::element_blank(),
                  axis.text.x=ggplot2::element_blank(),
                  axis.ticks.x=ggplot2::element_blank(),legend.position="none")
          pLDiffPrcnt<-get(plotSet_i[grepl("lineDiffPrcnt_",plotSet_i)])+ggplot2::ylab(NULL)+
            ggplot2::theme(plot.margin=ggplot2::margin(l=15, r=15,b=30,unit="pt"),
                  axis.title.x=ggplot2::element_blank(),
                  axis.text.x=ggplot2::element_blank(),
                  axis.ticks.x=ggplot2::element_blank(),legend.position="none")
        }else{
          pBRef<-get(plotSet_i[grepl("barDiffAbs1ScaleRef_",plotSet_i)])+
            ggplot2::theme(plot.margin=ggplot2::margin(l=65,r=20,b=30,unit="pt"),
                  axis.title.x=ggplot2::element_blank(),
                  axis.text.x=ggplot2::element_blank(),
                  axis.ticks.x=ggplot2::element_blank(),legend.position="none")
          pBDiff1S<-get(plotSet_i[grepl("barDiffAbs1Scale_",plotSet_i)])+
            ggplot2::theme(plot.margin=ggplot2::margin(l=65,r=20,b=30,unit="pt"),
                  axis.title.x=ggplot2::element_blank(),
                  axis.text.x=ggplot2::element_blank(),
                  axis.ticks.x=ggplot2::element_blank(),legend.position="none")
          pBDiffMS<-get(plotSet_i[grepl("barDiffAbs_",plotSet_i)])+ggplot2::ylab(NULL)+
            ggplot2::theme(plot.margin=ggplot2::margin(l=65,r=20,b=30,unit="pt"),
                  axis.title.x=ggplot2::element_blank(),
                  axis.text.x=ggplot2::element_blank(),
                  axis.ticks.x=ggplot2::element_blank(),legend.position="none")
          pLDiffPrcnt<-get(plotSet_i[grepl("lineDiffPrcnt_",plotSet_i)])+ggplot2::ylab(NULL)+
            ggplot2::theme(plot.margin=ggplot2::margin(l=65,r=20,b=30,unit="pt"),
                  axis.title.x=ggplot2::element_blank(),
                  axis.text.x=ggplot2::element_blank(),
                  axis.ticks.x=ggplot2::element_blank(),legend.position="none")
        }
        g <- ggplot2::ggplot_build(pBRef);
        pFilln<-unique(g$data[[1]]["fill"]);pFilln
        pColn<-unique(g$data[[1]]["colour"]);pColn
        if(nrow(pFilln)>1|nrow(pColn)>1){
          pBLeg<-ggpubr::as_ggplot(ggpubr::get_legend(pBRef+ggplot2::theme(legend.position = "right")))}else{
            pBLeg<-ggplot2::ggplot() + ggplot2::theme_void()
          }
        pBRef;pBDiff1S;pBDiffMS;pLDiffPrcnt;pBLeg
      }else{
        if(multiPlotFigLabels==F){
          pBRef<-get(plotSet_i[grepl("barDiffAbs1ScaleRef_",plotSet_i)])+ggplot2::theme(plot.margin=ggplot2::margin(l=15, r=15,b=30,unit="pt"),legend.position="none")
          pBDiff1S<-get(plotSet_i[grepl("barDiffAbs1Scale_",plotSet_i)])+ggplot2::theme(plot.margin=ggplot2::margin(l=15, r=15,b=30,unit="pt"),legend.position="none")
          pBDiffMS<-get(plotSet_i[grepl("barDiffAbs_",plotSet_i)])+ggplot2::ylab(NULL)+ggplot2::theme(plot.margin=ggplot2::margin(l=15, r=15,b=30,unit="pt"),legend.position="none")
          pLDiffPrcnt<-get(plotSet_i[grepl("lineDiffPrcnt_",plotSet_i)])+ggplot2::ylab(NULL)+ggplot2::theme(plot.margin=ggplot2::margin(l=15, r=15,b=30,unit="pt"),legend.position="none")
        }else{
          pBRef<-get(plotSet_i[grepl("barDiffAbs1ScaleRef_",plotSet_i)])+ggplot2::theme(plot.margin=ggplot2::margin(l=65,r=20,b=30,unit="pt"),legend.position="none")
          pBDiff1S<-get(plotSet_i[grepl("barDiffAbs1Scale_",plotSet_i)])+ggplot2::theme(plot.margin=ggplot2::margin(l=65,r=20,b=30,unit="pt"),legend.position="none")
          pBDiffMS<-get(plotSet_i[grepl("barDiffAbs_",plotSet_i)])+ggplot2::ylab(NULL)+ggplot2::theme(plot.margin=ggplot2::margin(l=65,r=20,b=30,unit="pt"),legend.position="none")
          pLDiffPrcnt<-get(plotSet_i[grepl("lineDiffPrcnt_",plotSet_i)])+ggplot2::ylab(NULL)+ggplot2::theme(plot.margin=ggplot2::margin(l=65,r=20,b=30,unit="pt"),legend.position="none")
        }
        g <- ggplot2::ggplot_build(pBRef);
        pFilln<-unique(g$data[[1]]["fill"]);pFilln
        pColn<-unique(g$data[[1]]["colour"]);pColn
        if(nrow(pFilln)>1|nrow(pColn)>1){
        pBLeg<-ggpubr::as_ggplot(ggpubr::get_legend(pBRef+ggplot2::theme(legend.position = "right")))}else{
          pBLeg<-ggplot2::ggplot() + ggplot2::theme_void()
        }

        pBRef;pBDiff1S;pBDiffMS;pLDiffPrcnt;pBLeg
      }}

    if(multiPlotFigLabels==F){labels1=NULL}else{
    labels1 <-c(paste(letters[seq(from=(countMultiLabel*2+1),to=(countMultiLabel*2+2),by=1)],")",sep=""),"")}; labels1
    if(multiPlotFigLabels==F){labels2=NULL}else{
    labels2 <-c(paste(letters[seq(from=(countMultiLabel*3+1),to=(countMultiLabel*3+3),by=1)],")",sep=""),"")}; labels2

    # nScen_i=max(unique((mpParamPlots_i%>%dplyr::filter(grepl("1Scale_",plot)))$nScen)); nScen_i
    #
    # # MultiPlots 1: Ref,diffs1scale,diffsPrcnt,leg
    # assign(paste("fig1Scale_",mpdf[row_i,]$paramSet,"_",mpdf[row_i,]$param,sep=""),
    #        ggpubr::ggarrange(pBDiff1S,pLDiffPrcnt,pBLeg,
    #                     ncol = 3,nrow=1,
    #                     widths=c((nScen_i+nScen_i-1)/nScen_i,1,0.25),
    #                     #labels="auto",
    #                     labels = labels1,
    #                     font.label=list(size=15*((nScen_i+nScen_i-1)/nScen_i+1.25)),
    #                     hjust=-0.5,vjust=1,
    #                     legend="none"))

    nScen_i=max(max(unique((mpParamPlots_i%>%dplyr::filter(grepl("DiffAbs_",plot)))$nScen)),1); nScen_i

    # MultiPlots 2: Ref,diffs1scale,diffsPrcnt,leg
    assign(paste("figMScale_",mpdf[row_i,]$paramSet,"_",mpdf[row_i,]$param,sep=""),
           egg::ggarrange(pBRef,pBDiffMS,pLDiffPrcnt,pBLeg,ncol = 4,nrow=1,
                          labels = labels2,widths=c(1,nScen_i,nScen_i,1),
                          label.args = list(gp = grid::gpar(hjust = 0, vjust=0, cex=1*(nScen_i+1.25)))))

    # MultiPlots 2: Ref,diffsMscale,leg
    assign(paste("figMScaleDiffAbs_",mpdf[row_i,]$paramSet,"_",mpdf[row_i,]$param,sep=""),
           egg::ggarrange(pBRef,pBDiffMS,pBLeg,ncol = 3,nrow=1,
                          labels = labels1,widths=c(1,nScen_i,1),
                          label.args = list(gp = grid::gpar(hjust = 0, vjust=0, cex=1*(nScen_i+1.25)))))

    # MultiPlots 2: Ref,diffsMscale,leg
    assign(paste("figMScaleDiffPrcnt_",mpdf[row_i,]$paramSet,"_",mpdf[row_i,]$param,sep=""),
           egg::ggarrange(pBRef,pLDiffPrcnt,pBLeg,ncol = 3,nrow=1,
                          labels = labels1,widths=c(1,nScen_i,1),
                          label.args = list(gp = grid::gpar(hjust = 0, vjust=0, cex=1*(nScen_i+1.25)))))

    multiPlotFigs <- multiPlotFigs %>%
      dplyr::bind_rows(tibble::tibble(paramSet=mpdf[row_i,]$paramSet,
                                      multiPlot=paste("fig1Scale_",mpdf[row_i,]$paramSet,"_",mpdf[row_i,]$param,sep=""),
                                      nScen=nScen_i)) %>%
      dplyr::bind_rows(tibble::tibble(paramSet=mpdf[row_i,]$paramSet,
                                      multiPlot=paste("figMScale_",mpdf[row_i,]$paramSet,"_",mpdf[row_i,]$param,sep=""),
                                      nScen=nScen_i))%>%
      dplyr::bind_rows(tibble::tibble(paramSet=mpdf[row_i,]$paramSet,
                                      multiPlot=paste("figMScaleDiffAbs_",mpdf[row_i,]$paramSet,"_",mpdf[row_i,]$param,sep=""),
                                      nScen=nScen_i))%>%
      dplyr::bind_rows(tibble::tibble(paramSet=mpdf[row_i,]$paramSet,
                                      multiPlot=paste("figMScaleDiffPrcnt_",mpdf[row_i,]$paramSet,"_",mpdf[row_i,]$param,sep=""),
                                      nScen=nScen_i))
    };multiPlotFigs;

for(paramSet_i in unique(multiPlotFigs$paramSet)){

  multiPlotFigs_i = multiPlotFigs %>% dplyr::filter(paramSet==paramSet_i);multiPlotFigs_i
 # plotRows_i = length(unique(multiPlotFigs_i$multiPlot)); plotRows_i

  # # Plot diff 1 Scale
  # multiPlotlist_i<-list()
  # multiPlotFigs_ix <- multiPlotFigs_i%>%dplyr::filter(grepl("fig1Scale",multiPlot))
  # for(multiPlot_i in 1:length(unique(multiPlotFigs_ix$multiPlot))){
  #   multiPlotlist_i[[multiPlot_i]] <-get(unique(multiPlotFigs_ix$multiPlot)[multiPlot_i])
  # };multiPlotlist_i
  # figure <- ggpubr::ggarrange(plotlist=multiPlotlist_i,ncol=1,nrow=length(multiPlotlist_i)); figure
  # # Plot Combined Figure for the Param Set
  # metis.printPdfPng(figure=figure,
  #                     dir=paste(dirOutputs, "/Charts/",folderName,"/", i,"/multiPlot", sep = ""),
  #                     filename=paste(paramSet_i,"_",i,"_mplot_diff1Scale",sep=""),
  #                     figWidth=figWidth*((nScen_i+nScen_i-1)/nScen_i+1.25),
  #                     figHeight=figHeight*length(multiPlotlist_i),
  #                     pdfpng="png")

  # Plot diff Multi Scale
  multiPlotFigs_ix <- multiPlotFigs_i%>%dplyr::filter(grepl("figMScale_",multiPlot))
  multiPlotlist_i<-NULL
  for(multiPlot_i in 1:length(unique(multiPlotFigs_ix$multiPlot))){
      multiPlotlist_i[[multiPlot_i]] <-get(unique(multiPlotFigs_ix$multiPlot)[multiPlot_i])
  };multiPlotlist_i
  figure <- ggpubr::ggarrange(plotlist=multiPlotlist_i,ncol=1,nrow=length(multiPlotlist_i)); figure
  # Plot Combined Figure for the Param Set
  metis.printPdfPng(figure=figure,
                    dir=paste(dirOutputs, "/Charts/",folderName,"/", i,"/multiPlot", sep = ""),
                    filename=paste(paramSet_i,"_",i,"_mplot_diffMultiScale",sep=""),
                    figWidth=figWidth*((nScen_i-1)/nScen_i+2.25)*max(1,nScen_i/3),
                    figHeight=figHeight*length(multiPlotlist_i),
                    pdfpng="png")

  # Plot diff Multi Scale Abs
  multiPlotFigs_ix <- multiPlotFigs_i%>%dplyr::filter(grepl("figMScaleDiffAbs_",multiPlot))
  multiPlotlist_i<-NULL
  for(multiPlot_i in 1:length(unique(multiPlotFigs_ix$multiPlot))){
    multiPlotlist_i[[multiPlot_i]] <-get(unique(multiPlotFigs_ix$multiPlot)[multiPlot_i])
  };multiPlotlist_i
  figure <- ggpubr::ggarrange(plotlist=multiPlotlist_i,ncol=1,nrow=length(multiPlotlist_i)); figure
  # Plot Combined Figure for the Param Set
  metis.printPdfPng(figure=figure,
                    dir=paste(dirOutputs, "/Charts/",folderName,"/", i,"/multiPlot", sep = ""),
                    filename=paste(paramSet_i,"_",i,"_mplot_diffMultiScaleAbs",sep=""),
                    figWidth=figWidth*((nScen_i-1)/nScen_i+2.25),
                    figHeight=figHeight*length(multiPlotlist_i),
                    pdfpng="png")

  # Plot diff Multi Scale Abs
  multiPlotFigs_ix <- multiPlotFigs_i%>%dplyr::filter(grepl("figMScaleDiffPrcnt_",multiPlot))
  multiPlotlist_i<-NULL
  for(multiPlot_i in 1:length(unique(multiPlotFigs_ix$multiPlot))){
    multiPlotlist_i[[multiPlot_i]] <-get(unique(multiPlotFigs_ix$multiPlot)[multiPlot_i])
  };multiPlotlist_i
  figure <- ggpubr::ggarrange(plotlist=multiPlotlist_i,ncol=1,nrow=length(multiPlotlist_i)); figure
  # Plot Combined Figure for the Param Set
  metis.printPdfPng(figure=figure,
                    dir=paste(dirOutputs, "/Charts/",folderName,"/", i,"/multiPlot", sep = ""),
                    filename=paste(paramSet_i,"_",i,"_mplot_diffMultiScalePrcnt",sep=""),
                    figWidth=figWidth*((nScen_i-1)/nScen_i+2.25),
                    figHeight=figHeight*length(multiPlotlist_i),
                    pdfpng="png")

}
} # Close if mpdf rows >0


  # Summary Multiplots for Across Scenarios

  #-------------------------------------
  # Multiplots per Region and chosen parameters
  #------------------------------------
if(T){
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
  print(paste("Params selected but not available for multiPlot are:"))
  print(paste(unique(mpdf$param)[!unique(mpdf$param) %in% unique(unlist(mpParamPlots$param))],collapse=", "))
  print(paste("Params available for multiPlot are:"))
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
      mpParamPlots_i<-mpParamPlots_i%>% dplyr::arrange(match(param,unique(mpdf$param)));mpParamPlots_i
      plotSet_i <-unique(mpParamPlots_i$plot);plotSet_i


      # Plot diff Abs and Prcnt
      multiPlotlist_i<-list();
      plotSet_ix <- unique(plotSet_i[grepl("_lineSum",plotSet_i)]);plotSet_ix
      for(multiPlot_i in 1:length(plotSet_ix)){
        if(multiPlotFigLabels==F){
          multiPlotlist_i[[multiPlot_i]] <-get(plotSet_ix[multiPlot_i])+ggplot2::ylab(NULL)+ggplot2::theme(plot.margin=ggplot2::margin(l=15,r=15,b=30,unit="pt"),
                                                                                                    legend.position="none")}else{
            multiPlotlist_i[[multiPlot_i]] <-get(plotSet_ix[multiPlot_i])+ggplot2::ylab(NULL)+ggplot2::theme(plot.margin=ggplot2::margin(l=65,r=20,b=30,unit="pt"),
                                                                                                    legend.position="none")
          }
      };multiPlotlist_i

      if(multiPlotFigLabels==F){labels1=NULL}else{
      labels1 <-paste(letters[seq(from=1,to=length(multiPlotlist_i),by=1)],")",sep="")}; labels1

      legRows = max(round(mpParamPlots_i[1,]$nScen/3),1);legRows
      mp_LineLeg <- ggpubr::as_ggplot(ggpubr::get_legend(get(plotSet_ix[1])+
                                                   ggplot2::theme(legend.position = "bottom")+
                                                   ggplot2::guides(colour = ggplot2::guide_legend(nrow = legRows, byrow=T,title="Scenario"),
                                                            shape = ggplot2::guide_legend(nrow=legRows,title="Scenario"))));mp_LineLeg
      # MultiPlots 1: Ref,diffs1scale,diffsPrcnt,leg
      #ncol
      ncol_i=3;
      if((length(plotSet_ix)/ncol_i)<1){nrow_i=1}else{nrow_i=ceiling(length(plotSet_ix)/ncol_i)};nrow_i
      figure<-egg::ggarrange(ggplotify::as.ggplot(egg::ggarrange(plots=multiPlotlist_i,ncol = ncol_i,nrow=nrow_i,
                                                                 labels = labels1,
                                                                 label.args = list(gp = grid::gpar(hjust = 0, vjust=0,  cex=1/3*ncol_i*nrow_i)))),
                             mp_LineLeg, ncol=1,nrow=2, heights=c(nrow_i*4,1)); figure

      metis.printPdfPng(figure=figure,
                        dir=paste(dirOutputs, "/Charts/",folderName,"/", i,"/multiPlot", sep = ""),
                        filename=paste(paramSet_i,"_",i,"_mplot_SumLineDiffAbsPrcnt",sep=""),
                        figWidth=figWidth*max(ncol_i,1.5)*0.6,
                        figHeight=figHeight*(max(nrow_i,1.5)+0.3)*0.75,
                        pdfpng="png")

      # Plot diff Abs
      multiPlotlist_i<-list();
      plotSet_ix <- unique(plotSet_i[grepl("_lineSumDiff_|_lineSumAbs_",plotSet_i)]);plotSet_ix
      for(multiPlot_i in 1:length(plotSet_ix)){
        if(multiPlotFigLabels==F){
          multiPlotlist_i[[multiPlot_i]] <-get(plotSet_ix[multiPlot_i])+ggplot2::ylab(NULL)+ggplot2::theme(plot.margin=ggplot2::margin(l=15,r=15,b=30,unit="pt"),
                                                                                                  legend.position="none")}else{
           multiPlotlist_i[[multiPlot_i]] <-get(plotSet_ix[multiPlot_i])+ggplot2::ylab(NULL)+ggplot2::theme(plot.margin=ggplot2::margin(l=65,r=20,b=30,unit="pt"),
                                                                                                   legend.position="none")
                                                                                              }
      };multiPlotlist_i

      if(multiPlotFigLabels==F){labels1=NULL}else{
        labels1 <-paste(letters[seq(from=1,to=length(multiPlotlist_i),by=1)],")",sep="")}; labels1

      legRows = max(round(mpParamPlots_i[1,]$nScen/3),1);legRows
      mp_LineLeg <- ggpubr::as_ggplot(ggpubr::get_legend(get(plotSet_ix[1])+
                                                           ggplot2::theme(legend.position = "bottom")+
                                                           ggplot2::guides(colour = ggplot2::guide_legend(nrow = legRows, byrow=T,title="Scenario"),
                                                                           shape = ggplot2::guide_legend(nrow=legRows,title="Scenario"))));mp_LineLeg
      # MultiPlots 1: Ref,diffs1scale,diffsPrcnt,leg
      #ncol
      ncol_i=2;
      if((length(plotSet_ix)/ncol_i)<1){nrow_i=1}else{nrow_i=ceiling(length(plotSet_ix)/ncol_i)};nrow_i
      figure<-egg::ggarrange(ggplotify::as.ggplot(egg::ggarrange(plots=multiPlotlist_i,ncol = ncol_i,nrow=nrow_i,
                             labels = labels1,
                             label.args = list(gp = grid::gpar(hjust = 0, vjust=0,  cex=0.5*ncol_i*nrow_i)))),
                           mp_LineLeg, ncol=1,nrow=2, heights=c(nrow_i*4,1)); figure

      metis.printPdfPng(figure=figure,
                        dir=paste(dirOutputs, "/Charts/",folderName,"/", i,"/multiPlot", sep = ""),
                        filename=paste(paramSet_i,"_",i,"_mplot_SumLineDiffAbs",sep=""),
                        figWidth=figWidth*max(ncol_i,1.5)*0.6,
                        figHeight=figHeight*(max(nrow_i,1.5)+0.3)*0.75,
                        pdfpng="png")

      # Plot diff Abs
      multiPlotlist_i<-list();
      plotSet_ix <- unique(plotSet_i[grepl("_lineSumDiffPrcnt_|_lineSumAbs_",plotSet_i)]);plotSet_ix
      for(multiPlot_i in 1:length(plotSet_ix)){
        if(multiPlotFigLabels==F){
          multiPlotlist_i[[multiPlot_i]] <-get(plotSet_ix[multiPlot_i])+ggplot2::ylab(NULL)+ggplot2::theme(plot.margin=ggplot2::margin(l=15,r=15,b=30,unit="pt"),
                                                                                                  legend.position="none")}else{
          multiPlotlist_i[[multiPlot_i]] <-get(plotSet_ix[multiPlot_i])+ggplot2::ylab(NULL)+ggplot2::theme(plot.margin=ggplot2::margin(l=65,r=20,b=30,unit="pt"),
                                                                                                legend.position="none")
                                                                                                  }
      };multiPlotlist_i

      if(multiPlotFigLabels==F){labels1=NULL}else{
        labels1 <-paste(letters[seq(from=1,to=length(multiPlotlist_i),by=1)],")",sep="")}; labels1

      legRows = max(round(mpParamPlots_i[1,]$nScen/3),1);legRows
      mp_LineLeg <- ggpubr::as_ggplot(ggpubr::get_legend(get(plotSet_ix[1])+
                                                           ggplot2::theme(legend.position = "bottom")+
                                                           ggplot2::guides(colour = ggplot2::guide_legend(nrow = legRows, byrow=T,title="Scenario"),
                                                                           shape = ggplot2::guide_legend(nrow=legRows,title="Scenario"))));mp_LineLeg
      # MultiPlots 1: Ref,diffs1scale,diffsPrcnt,leg
      #ncol
      ncol_i=2;
      if((length(plotSet_ix)/ncol_i)<1){nrow_i=1}else{nrow_i=ceiling(length(plotSet_ix)/ncol_i)};nrow_i
      figure<-egg::ggarrange(ggplotify::as.ggplot(egg::ggarrange(plots=multiPlotlist_i,ncol = ncol_i,nrow=nrow_i,
                                                                 labels = labels1,
                                                                 label.args = list(gp = grid::gpar(hjust = 0, vjust=0,  cex=0.5*ncol_i*nrow_i)))),
                             mp_LineLeg, ncol=1,nrow=2, heights=c(nrow_i*4,1)); figure


      metis.printPdfPng(figure=figure,
                        dir=paste(dirOutputs, "/Charts/",folderName,"/", i,"/multiPlot", sep = ""),
                        filename=paste(paramSet_i,"_",i,"_mplot_SumLineDiffPrcnt",sep=""),
                        figWidth=figWidth*max(ncol_i,1.5)*0.6,
                        figHeight=figHeight*(max(nrow_i,1.5)+0.3)*0.75,
                        pdfpng="png")


      # Only Abs
      #----------------------------------

      # Plot diff 1 Scale
      multiPlotlist_i<-list();
      plotSet_ix <- unique(plotSet_i[grepl("_lineSumAbs_",plotSet_i)]);plotSet_ix
      for(multiPlot_i in 1:length(plotSet_ix)){
        if(multiPlotFigLabels==F){
          multiPlotlist_i[[multiPlot_i]] <-get(plotSet_ix[multiPlot_i])+ggplot2::ylab(NULL)+ggplot2::theme(plot.margin=ggplot2::margin(l=15,r=15,b=30,unit="pt"),
                                                                                                  legend.position="none")}else{
          multiPlotlist_i[[multiPlot_i]] <-get(plotSet_ix[multiPlot_i])+ggplot2::ylab(NULL)+ggplot2::theme(plot.margin=ggplot2::margin(l=65,r=20,b=30,unit="pt"),
                                                                                                           legend.position="none")
                                                                                                  }
      };multiPlotlist_i

      if(multiPlotFigLabels==F){labels1=NULL}else{
      labels1 <-paste(letters[seq(from=1,to=length(multiPlotlist_i),by=1)],")",sep="")}; labels1

      legRows = max(round(mpParamPlots_i[1,]$nScen/3),1);legRows
      mp_LineLeg <- ggpubr::as_ggplot(ggpubr::get_legend(get(plotSet_ix[1])+
                                                   ggplot2::theme(legend.position = "bottom")+
                                                   ggplot2::guides(colour = ggplot2::guide_legend(nrow = legRows, byrow=T,title="Scenario"),
                                                          shape = ggplot2::guide_legend(nrow=legRows,title="Scenario"))));mp_LineLeg
      # MultiPlots 1: Ref,diffs1scale,diffsPrcnt,leg
      #ncol
      if(round(length(plotSet_ix)/facetCols)<=1){ncol_i=length(plotSet_ix)}else{ncol_i=facetCols};ncol_i
      if((length(plotSet_ix)/ncol_i)<1){nrow_i=1}else{nrow_i=ceiling(length(plotSet_ix)/ncol_i)};nrow_i
      figure<-egg::ggarrange(ggplotify::as.ggplot(egg::ggarrange(plots=multiPlotlist_i,ncol = ncol_i,nrow=nrow_i,
                                                                 labels = labels1,
                                                                 label.args = list(gp = grid::gpar(hjust = 0, vjust=0, cex=1*ncol_i*nrow_i)))),
                             mp_LineLeg, ncol=1,nrow=2, heights=c(nrow_i*4,1)); figure


      metis.printPdfPng(figure=figure,
                        dir=paste(dirOutputs, "/Charts/",folderName,"/", i,"/multiPlot", sep = ""),
                        filename=paste(paramSet_i,"_",i,"_mplot_SumLineAbs",sep=""),
                        figWidth=figWidth*max(ncol_i,3)*0.5,
                        figHeight=figHeight*(max(nrow_i))*1,
                        pdfpng="png")


    }
  }
  }
}
    } # close loop for region
  } # Close if multiple scenarios available
} # Close if(regionCompareOnly==1)


  return(tbl)

  } # Close Function

