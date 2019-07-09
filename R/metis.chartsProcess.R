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
#' @param classPalette Default "pal_Basic" from metis.colors()$pal_Basic
#' @param regionCompareOnly Default 0. If set to 1, will only run comparison plots and not individual
#' @param scenarioCompareOnly Default 0. If set to 1, will only run comparison plots and not individual
#' @param useNewLabels Default 0
#' @param sizeBarLines Default 0.5
#' @param sizeLines Default 1.5
#' @param xCompare Choose the years to compare scenarios for xScenSelectYears plot. Default is
#' c("2015","2030","2050","2100")
#' @param paramsSelect Default = "All". Select the paramaters to analyze from the the tables provided.
#' Full list of parameters:
#' c("finalNrgbySec", "primNrgConsumByFuel", "elecByTech",
#' "watConsumBySec", "watWithdrawBySec", "watWithdrawByCrop", "watBioPhysCons", "irrWatWithBasin","irrWatConsBasin",
#' "gdpPerCapita", "gdp", "gdpGrowthRate", "pop", "agProdbyIrrRfd",
#' "agProdBiomass", "agProdForest", "agProdByCrop", "landIrrRfd", "aggLandAlloc",
#' "LUCemiss", "co2emission", "co2emissionByEndUse", "ghgEmissionByGHG", "ghgEmissByGHGGROUPS")
#' @param regionsSelect Default = "All". Select regions to create charts for.
#' @param scensSelect Default = "All". Select regions to create charts for.
#' @param xRange Default "All". Range of x values eg. c(2001:2005)
#' @param folderName Default ="analysis"
#' @param nameAppend Default =""
#' @param colOrder1 Default = NULL,
#' @param colOrderName1 Default = NULL,
#' @param colOrder2 Default = NULL,
#' @param colOrderName2 Default = NULL,
#' @keywords charts, diffplots
#' @return Produces charts in output folder and also returns combined table in metis format.
#' @export
#' @importFrom "rlang" ":="

metis.chartsProcess <- function(dataTables=NULL,rTable=NULL,scenRef=NULL,
                       dirOutputs=paste(getwd(),"/outputs",sep=""),pdfpng="png",
                       xRange="All",
                       xCompare=c("2015","2030","2050","2100"),
                       paramsSelect="All",
                       regionsSelect="All",
                       xData="x",yData="value",xLabel="xLabel",yLabel="units",
                       aggregate="sum",class="class", classPalette="pal_Basic",
                       regionCompareOnly=0,scenarioCompareOnly=0,useNewLabels=0,
                       sizeBarLines=0,sizeLines=1.5,
                       folderName="analysis",
                       nameAppend="",
                       scensSelect="All",
                       colOrder1 = NULL,
                       colOrderName1 = NULL,
                       colOrder2 = NULL,
                       colOrderName2 = NULL) {


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
  # classPalette="pal_Basic"
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

#------------------
# Initialize variables to remove binding errors
# -----------------

  NULL->scenario->value->x->region->param->origValue->origScen->origQuery->
  origUnits->origX->sources->vintage->class1->classLabel1->classPalette1->
  class2->classLabel2->classPalette2->i->j->k->figWMult->classLabel1x ->classLabel2x-> classPalette1x-> classPalette2x

  aggregate_i <- aggregate

#------------------
# Function for adding any missing columns if needed
# -----------------


addMissing<-function(data){
  if(!any(grepl("\\<scenario\\>",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(scenario="scenario")}else{
    data <- data %>% dplyr::rename(!!"scenario" := (names(data)[grepl("\\<scenario\\>",names(data),ignore.case = T)])[1])
    data<-data%>%dplyr::mutate(scenario=dplyr::case_when(is.na(scenario)~"scenario",TRUE~scenario))}
  if(!any(grepl("\\<scenarios\\>",names(data),ignore.case = T))){}else{
    data <- data %>% dplyr::rename(!!"scenario" := (names(data)[grepl("\\<scenarios\\>",names(data),ignore.case = T)])[1])
    data<-data%>%dplyr::mutate(scenario=dplyr::case_when(is.na(scenario)~"scenario",TRUE~scenario))}
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
  if(!"x"%in%names(data)){data<-data%>%dplyr::mutate(x="x")}
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
    data<-data%>%dplyr::rename(class1=class)}else{data<-data%>%dplyr::mutate(class1="class1")}}else{
      data <- data %>% dplyr::rename(!!"class1" := (names(data)[grepl("\\<class1\\>",names(data),ignore.case = T)])[1])
      data<-data%>%dplyr::mutate(class1=dplyr::case_when(is.na(class1)~"class1",TRUE~class1))}
  if(!any(grepl("\\<classlabel1\\>",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(classLabel1="classLabel1")}else{
    data <- data %>% dplyr::rename(!!"classLabel1" := (names(data)[grepl("\\<classlabel1\\>",names(data),ignore.case = T)])[1])
    data<-data%>%dplyr::mutate(classLabel1=dplyr::case_when(is.na(classLabel1)~"classLabel1",TRUE~classLabel1))}
  if(!any(grepl("\\<classpalette1\\>",names(data),ignore.case = T))){ if(is.null(classPalette)){data<-data%>%dplyr::mutate(classPalette1="pal_Basic")}else{
    data<-data%>%dplyr::mutate(classPalette1=classPalette)}}else{
      data <- data %>% dplyr::rename(!!"classPalette1" := (names(data)[grepl("\\<classpalette1\\>",names(data),ignore.case = T)])[1])
      data<-data%>%dplyr::mutate(classPalette1=dplyr::case_when(is.na(classPalette1)~classPalette,TRUE~classPalette1))}
  if(!any(grepl("\\<class2\\>",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(class2="class2")}else{
    data <- data %>% dplyr::rename(!!"class2" := (names(data)[grepl("\\<class2\\>",names(data),ignore.case = T)])[1])
    data<-data%>%dplyr::mutate(class2=dplyr::case_when(is.na(class2)~"class2",TRUE~class2))}
  if(!any(grepl("\\<classlabel2\\>",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(classLabel2="classLabel2")}else{
    data <- data %>% dplyr::rename(!!"classLabel2" := (names(data)[grepl("\\<classlabel2\\>",names(data),ignore.case = T)])[1])
    data<-data%>%dplyr::mutate(classLabel2=dplyr::case_when(is.na(classLabel2)~"classLabel2",TRUE~classLabel2))}
  if(!any(grepl("\\<classpalette2\\>",names(data),ignore.case = T))){ if(is.null(classPalette)){data<-data%>%dplyr::mutate(classPalette2="pal_Basic")}else{
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
  tblNew<-utils::read.csv(paste(i), stringsAsFactors = F)%>%tibble::as.tibble()
  if("class"%in%names(tblNew) & !"class1"%in%names(tblNew)){tblNew<-tblNew%>%dplyr::mutate(class1=class)}

  # Join relevant colors and classes using the mapping file if it exists
  if(file.exists(paste(getwd(),"/dataFiles/mapping/template_Regional_mapping.csv", sep = ""))){
    map<-utils::read.csv(paste(getwd(),"/dataFiles/mapping/template_Regional_mapping.csv", sep = ""), stringsAsFactors = F)%>%tibble::as.tibble()
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

# Aggregate across classes
tblAggsums<-tbl%>%
  dplyr::filter(aggregate=="sum")%>%
  dplyr::select(scenario,region,param,units,x,value)%>%
  dplyr::group_by_at(dplyr::vars(-value))%>%
  dplyr::summarize_at(c("value"),dplyr::funs(sum))
tblAggmeans<-tbl%>%
  dplyr::filter(aggregate=="mean")%>%
  dplyr::select(scenario,region,param,units,x, value)%>%
  dplyr::group_by_at(dplyr::vars(-value))%>%
  dplyr::summarize_at(c("value"),dplyr::funs(mean))
tblAgg<-dplyr::bind_rows(tblAggsums,tblAggmeans)%>%dplyr::ungroup()

tblAggClass1sums<-tbl%>%
  dplyr::filter(aggregate=="sum")%>%
  dplyr::select(scenario,region,param,units,x,value,class1)%>%
  dplyr::group_by_at(dplyr::vars(-value))%>%
  dplyr::summarize_at(c("value"),dplyr::funs(sum))
tblAggClass1means<-tbl%>%
  dplyr::filter(aggregate=="mean")%>%
  dplyr::select(scenario,region,param,units,x, value, class1)%>%
  dplyr::group_by_at(dplyr::vars(-value))%>%
  dplyr::summarize_at(c("value"),dplyr::funs(mean))
tblAggClass1<-dplyr::bind_rows(tblAggClass1sums,tblAggClass1means)%>%dplyr::ungroup()

tblAggClass2sums<-tbl%>%
  dplyr::filter(aggregate=="sum")%>%
  dplyr::select(scenario,region,param,units,x,value,class2)%>%
  dplyr::group_by_at(dplyr::vars(-value))%>%
  dplyr::summarize_at(c("value"),dplyr::funs(sum))
tblAggClass2means<-tbl%>%
  dplyr::filter(aggregate=="mean")%>%
  dplyr::select(scenario,region,param,units,x, value, class2)%>%
  dplyr::group_by_at(dplyr::vars(-value))%>%
  dplyr::summarize_at(c("value"),dplyr::funs(mean))
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

if(length(unique(tbl$region))>1){

  for(j in unique(tbl$scenario)){
    for(k in unique(tbl$param)){

      tbl_sp<-tbl%>%dplyr::filter(scenario==j,
                                   param==k)

      if(nrow(tbl_sp)>0){

        if(length(unique(tbl_sp$class1))>1){figWMult=1.3}else{figWMult=1}

        # Aggregated Class 1
        # Aggregate across classes
        tblAggsums<-tbl_sp%>%
          dplyr::filter(aggregate=="sum")%>%
          dplyr::select(-class2,-classLabel2,-classPalette2,-origValue,-origScen,-origQuery,-origUnits,-origX,-vintage)%>%
          dplyr::group_by_at(dplyr::vars(-value))%>%
          dplyr::summarize_at(c("value"),dplyr::funs(sum))
        tblAggmeans<-tbl_sp%>%
          dplyr::filter(aggregate=="mean")%>%
          dplyr::select(-class2,-classLabel2,-classPalette2,-origValue,-origScen,-origQuery,-origUnits,-origX,-vintage)%>%
          dplyr::group_by_at(dplyr::vars(-value))%>%
          dplyr::summarize_at(c("value"),dplyr::funs(mean))
        tbl_spC1<-dplyr::bind_rows(tblAggsums,tblAggmeans)%>%dplyr::ungroup()


        # Bar Chart
       metis.chart(tbl_spC1, xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel,
                    sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar",facet_columns="region",facet_rows=NULL,
          dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/compareRegions/",gsub(" ","",paste(unique(unique(tbl$region)),collapse="")),j,sep = ""),
          fileName = paste(k,"_figBar_",j,"_compareRegions",nameAppend,sep=""),
          figWidth = 13*max((length(unique(tbl_sp$region))/2),1)*figWMult,pdfpng=pdfpng, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
        )

        # Line Chart
        metis.chart(tbl_spC1,xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel,
                    sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "line",facet_columns="region",facet_rows=NULL,
          dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/compareRegions/",gsub(" ","",paste(unique(unique(tbl$region)),collapse="")),j,sep = ""),
          fileName = paste(k,"_figLines_",j,"_compareRegions",nameAppend,sep=""),
          figWidth = 13*max((length(unique(tbl_sp$region))/2),1)*figWMult,pdfpng=pdfpng, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
        )

        # If class 2 available
        if(length(unique(tbl_sp$class2))>1){

          # Bar Chart
          metis.chart(tbl_sp, xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel,
                      sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar",facet_columns="region",facet_rows="class2",
                      dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/compareRegions/",gsub(" ","",paste(unique(unique(tbl$region)),collapse="")),j,sep = ""),
                      fileName = paste(k,"_figBar_",j,"_compareRegionsClass2",nameAppend,sep=""),
                      figWidth = 13*max((length(unique(tbl_sp$region))/2),1)*figWMult,
                      figHeight = 9*max((length(unique(tbl_sp$class2))/2),1),pdfpng=pdfpng, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
          )

          # Line Chart
          metis.chart(tbl_sp,xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel,
                      sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "line",facet_columns="region",facet_rows="class2",
                      dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/compareRegions/",gsub(" ","",paste(unique(unique(tbl$region)),collapse="")),j,sep = ""),
                      fileName = paste(k,"_figLines_",j,"_compareRegionsClass2",nameAppend,sep=""),
                      figWidth = 13*max((length(unique(tbl_sp$region))/2),1)*figWMult,
                      figHeight = 9*max((length(unique(tbl_sp$class2))/2),1),pdfpng=pdfpng, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
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

      if(length(unique((tbl_p%>%dplyr::filter(value>0))$scenario))>1){

      if(nrow(tbl_p)>0){

        if(length(unique(tbl_p$class1))>1){figWMult=1.3}else{figWMult=1}

        # Aggregated Class 1
        # Aggregate across classes
        tblAggsums<-tbl_p%>%
          dplyr::filter(aggregate=="sum")%>%
          dplyr::select(-class2,-classLabel2,-classPalette2,-origValue,-origScen,-origQuery,-origUnits,-origX,-vintage)%>%
          dplyr::group_by_at(dplyr::vars(-value))%>%
          dplyr::summarize_at(c("value"),dplyr::funs(sum))
        tblAggmeans<-tbl_p%>%
          dplyr::filter(aggregate=="mean")%>%
          dplyr::select(-class2,-classLabel2,-classPalette2,-origValue,-origScen,-origQuery,-origUnits,-origX,-vintage)%>%
          dplyr::group_by_at(dplyr::vars(-value))%>%
          dplyr::summarize_at(c("value"),dplyr::funs(mean))
        tbl_pC1<-dplyr::bind_rows(tblAggsums,tblAggmeans)%>%dplyr::ungroup()

        # Bar Chart
        metis.chart(tbl_pC1, xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel,
                    sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar",facet_columns="scenario",facet_rows="region",
          dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/compareRegions/",gsub(" ","",paste(unique(unique(tbl$region)),collapse="")),"compareScen", sep = ""),
          fileName = paste(j,"_figBar_compareScenRegions",nameAppend,sep=""),
          figWidth = 13*max((length(unique(tbl_p$scenario))/2),1)*figWMult,
          figHeight = 9*max((length(unique(tbl_p$region))/2),1),pdfpng=pdfpng, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
        )

        # Line Chart
        metis.chart(tbl_pC1,xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel,
                    sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "line",facet_columns="scenario",facet_rows="region",
          dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/compareRegions/",gsub(" ","",paste(unique(unique(tbl$region)),collapse="")),"compareScen", sep = ""),
          fileName = paste(j,"_figLine_compareScenRegions",nameAppend,sep=""),
          figWidth = 13*max((length(unique(tbl_p$scenario))/2),1)*figWMult,
          figHeight = 9*max((length(unique(tbl_p$region))/2),1),pdfpng=pdfpng, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
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

        if(length(unique(tbl_py$class1))>1){figWMult=1.3}else{figWMult=1}

        # Aggregated Class 1
        # Aggregate across classes
        tblAggsums<-tbl_py%>%
          dplyr::filter(aggregate=="sum")%>%
          dplyr::select(-class2,-classLabel2,-classPalette2,-origValue,-origScen,-origQuery,-origUnits,-origX,-vintage)%>%
          dplyr::group_by_at(dplyr::vars(-value))%>%
          dplyr::summarize_at(c("value"),dplyr::funs(sum))
        tblAggmeans<-tbl_py%>%
          dplyr::filter(aggregate=="mean")%>%
          dplyr::select(-class2,-classLabel2,-classPalette2,-origValue,-origScen,-origQuery,-origUnits,-origX,-vintage)%>%
          dplyr::group_by_at(dplyr::vars(-value))%>%
          dplyr::summarize_at(c("value"),dplyr::funs(mean))
        tbl_pyC1<-dplyr::bind_rows(tblAggsums,tblAggmeans)%>%dplyr::ungroup()



        # Bar Chart
        metis.chart(tbl_pyC1, xData ="scenario", yData=yData,xLabel=xLabel,yLabel=yLabel,
                    sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar", facet_columns = xData, facet_rows="region",
          dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/compareRegions/",gsub(" ","",paste(unique(unique(tbl$region)),collapse="")),"compareScen", sep = ""),
          fileName = paste(j,"_figBar_compareScenRegion_xScenSelectYears",nameAppend,sep=""),
          figWidth = 13*max((length(unique(tbl_py$x)[unique(tbl_py$x) %in% xCompare])/3),1)*figWMult,
          figHeight = 9*max((length(unique(tbl_py$region))/2),1),pdfpng=pdfpng, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
        )


        #-------------------------
        # Aggregate and Plot Dodged/OverLapping Plots
        #------------------------

        # Aggregate across classes
        tbl_pAggsums<-tbl_p%>%
          dplyr::filter(aggregate=="sum")%>%
          dplyr::select(-tidyselect::contains("class"),-origValue,-origScen,-origQuery,-origUnits,-origX,-vintage)%>%
          dplyr::group_by_at(dplyr::vars(-yData))%>%
          dplyr::summarize_at(c(yData),dplyr::funs(sum))
        tbl_pAggmeans<-tbl_p%>%
          dplyr::filter(aggregate=="mean")%>%
          dplyr::select(-tidyselect::contains("class"),-origValue,-origScen,-origQuery,-origUnits,-origX,-vintage)%>%
          dplyr::group_by_at(dplyr::vars(-yData))%>%
          dplyr::summarize_at(c(yData),dplyr::funs(mean))
        tbl_pAgg<-dplyr::bind_rows(tbl_pAggsums,tbl_pAggmeans)%>%dplyr::ungroup()


        if(nrow(tbl_pAgg)>0){

          # Bar Chart Dodged
          metis.chart(tbl_pAgg, xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel, sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar",
                      class ="scenario", position ="dodge", classPalette = classPalette,
                      facet_columns="region",facet_rows=NULL,
            dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/compareRegions/",gsub(" ","",paste(unique(unique(tbl$region)),collapse="")),"compareScen", sep = ""),
            fileName = paste(j,"_figBarDodged_compareScenRegion",nameAppend,sep=""),
            figWidth = 13*max((length(unique(tbl_pAgg$region))/2),1),pdfpng=pdfpng, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
          )

          # Line Chart Overlapped
          metis.chart(tbl_pAgg,xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel,
                      sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "line",class ="scenario", classPalette = classPalette,
                      facet_columns="region",facet_rows=NULL,
            dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/compareRegions/",gsub(" ","",paste(unique(unique(tbl$region)),collapse="")),"compareScen", sep = ""),
            fileName = paste(j,"_figLineOverlap_compareScenRegion",nameAppend,sep=""),
            figWidth = 13*max((length(unique(tbl_pAgg$region))/2),1),pdfpng=pdfpng, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
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
            tidyr::spread(scenario,yData)%>%
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

        if(length(unique(tbl_pd$class1))>1){figWMult=1.3}else{figWMult=1}

        # Aggregated Class 1
        # Aggregate across classes
        tblAggsums<-tbl_pd%>%
          dplyr::filter(aggregate=="sum")%>%
          dplyr::select(-class2,-classLabel2,-classPalette2)%>%
          dplyr::group_by_at(dplyr::vars(-value))%>%
          dplyr::summarize_at(c("value"),dplyr::funs(sum))
        tblAggmeans<-tbl_pd%>%
          dplyr::filter(aggregate=="mean")%>%
          dplyr::select(-class2,-classLabel2,-classPalette2)%>%
          dplyr::group_by_at(dplyr::vars(-value))%>%
          dplyr::summarize_at(c("value"),dplyr::funs(mean))
        tbl_pdC1<-dplyr::bind_rows(tblAggsums,tblAggmeans)%>%dplyr::ungroup()



        # Bar Chart
        metis.chart(tbl_pdC1, xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel,
                    sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar", facet_rows="region", facet_columns="scenario",
          dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/compareRegions/",gsub(" ","",paste(unique(unique(tbl$region)),collapse="")),"compareScen", sep = ""),
          fileName = paste(j,"_figBarDiff_compareScenRegion",nameAppend,sep=""),
          figWidth = 13*max((length(unique(tbl_pd$scenario))/2),1)*figWMult,
          figHeight = 9*max((length(unique(tbl_pd$region))/2),1),pdfpng=pdfpng, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
        )

        # Line Chart
        metis.chart(tbl_pdC1, xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel,
                    sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "line", facet_rows="region", facet_columns="scenario",
          dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/compareRegions/",gsub(" ","",paste(unique(unique(tbl$region)),collapse="")),"compareScen", sep = ""),
          fileName = paste(j,"_figLineDiff_compareScenRegion",nameAppend,sep=""),
          figWidth = 13*max((length(unique(tbl_pd$scenario))/2),1)*figWMult,
          figHeight = 9*max((length(unique(tbl_pd$region))/2),1),pdfpng=pdfpng, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
        )


        } # Close if(nrow(tbl_rsp)>0)
     } # Close loop if(length(unique(tbl$scenario))>1) to see if multiple scenarios for chosen param
    } # close loop for param
} # Close if multiple scenarios available

} # if length(unique(tbl$region))>1

if(regionCompareOnly!=1){

#------------------
# Create Charts for Each Region & Each Scenario
#------------------

for(i in unique(tbl$region)){
if(scenarioCompareOnly!=1){
  for(j in unique(tbl$scenario)){
    for(k in unique(tbl$param)){

    tbl_rsp<-tbl%>%dplyr::filter(region==i,
                          scenario==j,
                          param==k)

    if(nrow(tbl_rsp)>0){

      if(length(unique(tbl_rsp$class1))>1){figWMult=1.3}else{figWMult=1}

      # Aggregated Class 1
      # Aggregate across classes
      tblAggsums<-tbl_rsp%>%
        dplyr::filter(aggregate=="sum")%>%
        dplyr::select(-class2,-classLabel2,-classPalette2,-origValue,-origScen,-origQuery,-origUnits,-origX,-vintage)%>%
        dplyr::group_by_at(dplyr::vars(-value))%>%
        dplyr::summarize_at(c("value"),dplyr::funs(sum))
      tblAggmeans<-tbl_rsp%>%
        dplyr::filter(aggregate=="mean")%>%
        dplyr::select(-class2,-classLabel2,-classPalette2,-origValue,-origScen,-origQuery,-origUnits,-origX,-vintage)%>%
        dplyr::group_by_at(dplyr::vars(-value))%>%
        dplyr::summarize_at(c("value"),dplyr::funs(mean))
      tbl_rspC1<-dplyr::bind_rows(tblAggsums,tblAggmeans)%>%dplyr::ungroup()


    # Bar Chart
    metis.chart(tbl_rspC1, xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel,sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar",
    dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i, "/", j,sep = ""),
    fileName = paste(k,"_figBar_",i,"_",j,nameAppend,sep=""),pdfpng=pdfpng, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
    )

    # data=tbl_rspC1; xData=xData;yData=yData;xLabel=xLabel;yLabel=yLabel;sizeBarLines=sizeBarLines;useNewLabels=useNewLabels;sizeLines=sizeLines; chartType = "bar";
    # dirOutputs = paste(dirOutputs, "/Charts/", i, "/", j,sep = "");
    # fileName = paste(k,"_figBar_",i,"_",j,nameAppend,sep="");pdfpng=pdfpng, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2

    # Line Chart
    metis.chart(tbl_rspC1,xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel,sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "line",
      dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i, "/", j,sep = ""),
      fileName = paste(k,"_figLine_",i,"_",j,nameAppend,sep=""),pdfpng=pdfpng, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
    )

    # Class 2 Charts
    if(length(unique(tbl_rsp$class2))>1){

      # Bar Chart
      metis.chart(tbl_rsp, xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel,sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar",
                  facet_columns = "class2", dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i, "/", j,sep = ""),
                  fileName = paste(k,"_figBar_",i,"_Class2_",j,nameAppend,sep="")
      )

      # Line Chart
      metis.chart(tbl_rsp,xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel,sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "line",
                  facet_columns = "class2", dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i, "/", j,sep = ""),
                  fileName = paste(k,"_figLine_",i,"_Class2_",j,nameAppend,sep="")
      )
    }




    } # Close if(nrow(tbl_rsp)>0)

} # close loop for param
  } # Close if statement for compareScenariosOnly
} # close loop for scenario
} # close loop for region

#------------------
# Compare Scenarios for each region
#------------------

if(length(unique(tbl$scenario))>1){


for(i in unique(tbl$region)){
    for(j in unique(tbl$param)){

      tbl_rp<-tbl%>%dplyr::filter(region==i,
                                   param==j)

      if(length(unique((tbl_rp%>%dplyr::filter(value>0))$scenario))>1){

      if(nrow(tbl_rp)>0){

        if(length(unique(tbl_rp$class1))>1){figWMult=1.3}else{figWMult=1}

        # Aggregated Class 1
        # Aggregate across classes
        tblAggsums<-tbl_rp%>%
          dplyr::filter(aggregate=="sum")%>%
          dplyr::select(-class2,-classLabel2,-classPalette2,-origValue,-origScen,-origQuery,-origUnits,-origX,-vintage)%>%
          dplyr::group_by_at(dplyr::vars(-value))%>%
          dplyr::summarize_at(c("value"),dplyr::funs(sum))
        tblAggmeans<-tbl_rp%>%
          dplyr::filter(aggregate=="mean")%>%
          dplyr::select(-class2,-classLabel2,-classPalette2,-origValue,-origScen,-origQuery,-origUnits,-origX,-vintage)%>%
          dplyr::group_by_at(dplyr::vars(-value))%>%
          dplyr::summarize_at(c("value"),dplyr::funs(mean))
        tbl_rpC1<-dplyr::bind_rows(tblAggsums,tblAggmeans)%>%dplyr::ungroup()


      # Bar Chart
      metis.chart(tbl_rpC1, xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel, sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar",
        dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i,"/compareScen",sep = ""),
        facet_columns="scenario",
        fileName = paste(j,"_figBar_",i,"_compareScen",nameAppend,sep=""),
        figWidth = 13*max((length(unique(tbl_rp$scenario))/2),1)*figWMult,
        figHeight = 9*max((length(unique(tbl_rp$region))/2),1),pdfpng=pdfpng, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
      )

      # data=a; xData=xData;yData=yData;xLabel=xLabel;yLabel=yLabel; sizeBarLines=sizeBarLines;useNewLabels=useNewLabels;sizeLines=sizeLines; chartType = "bar";
      # dirOutputs = paste(dirOutputs, "/Charts/", i,"/compareScen",sep = "")
      # fileName = paste(j,"_figBar_",i,"_compareScen",nameAppend,sep="")
      # figWidth = 13*max((length(unique(tbl_rp$scenario))/2),1)*figWMult
      # figHeight = 9*max((length(unique(tbl_rp$region))/2),1)
      # pdfpng=pdfpng, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2

      # Line Chart
      metis.chart(tbl_rpC1,xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel, sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "line",
        dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i,"/compareScen",sep = ""),
        facet_columns="scenario",
        fileName = paste(j,"_figLine_",i,"_compareScen",nameAppend,sep=""),
        figWidth = 13*max((length(unique(tbl_rp$scenario))/2),1)*figWMult,
        figHeight = 9*max((length(unique(tbl_rp$region))/2),1),pdfpng=pdfpng, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
      )

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

      if(length(unique(tbl_rpy$class1))>1){figWMult=1.3}else{figWMult=1}

      # Aggregated Class 1
      # Aggregate across classes
      tblAggsums<-tbl_rpy%>%
        dplyr::filter(aggregate=="sum")%>%
        dplyr::select(-class2,-classLabel2,-classPalette2,-origValue,-origScen,-origQuery,-origUnits,-origX,-vintage)%>%
        dplyr::group_by_at(dplyr::vars(-value))%>%
        dplyr::summarize_at(c("value"),dplyr::funs(sum))
      tblAggmeans<-tbl_rpy%>%
        dplyr::filter(aggregate=="mean")%>%
        dplyr::select(-class2,-classLabel2,-classPalette2,-origValue,-origScen,-origQuery,-origUnits,-origX,-vintage)%>%
        dplyr::group_by_at(dplyr::vars(-value))%>%
        dplyr::summarize_at(c("value"),dplyr::funs(mean))
      tbl_rpyC1<-dplyr::bind_rows(tblAggsums,tblAggmeans)%>%dplyr::ungroup()


      # Bar Chart
      metis.chart(tbl_rpyC1, xData ="scenario", yData=yData,xLabel=xLabel,yLabel=yLabel, sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar", facet_columns = xData,
        dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i,"/compareScen",sep = ""),
        fileName = paste(j,"_figBar_",i,"_compareScen_xScenSelectYears",nameAppend,sep=""),
        figWidth = 13*max((length(unique(tbl_rpy$x)[unique(tbl_rpy$x) %in% xCompare])/3),1)*figWMult,
        figHeight = 9*max((length(unique(tbl_rpy$region))/2),1),pdfpng=pdfpng, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
      )


#-------------------------
# Aggregate and Plot Dodged/OverLapping Plots
#------------------------

      # Aggregate across classes
      tbl_rpAggsums<-tbl_rp%>%
        dplyr::filter(aggregate=="sum")%>%
        dplyr::select(-tidyselect::contains("class"),-origValue,-origScen,-origQuery,-origUnits,-origX,-vintage)%>%
        dplyr::group_by_at(dplyr::vars(-yData))%>%
        dplyr::summarize_at(c(yData),dplyr::funs(sum))
      tbl_rpAggmeans<-tbl_rp%>%
        dplyr::filter(aggregate=="mean")%>%
        dplyr::select(-tidyselect::contains("class"),-origValue,-origScen,-origQuery,-origUnits,-origX,-vintage)%>%
        dplyr::group_by_at(dplyr::vars(-yData))%>%
        dplyr::summarize_at(c(yData),dplyr::funs(mean))
      tbl_rpAgg<-dplyr::bind_rows(tbl_rpAggsums,tbl_rpAggmeans)%>%dplyr::ungroup()


      if(nrow(tbl_rpAgg)>0){

        if(length(unique(tbl_rpAgg$class1))>1){figWMult=1.3}else{figWMult=1}

      # Bar Chart Dodged
      metis.chart(tbl_rpAgg, xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel, sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar", facet_columns=NULL,
                  class ="scenario", position ="dodge", classPalette = classPalette,
        dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i,"/compareScen",sep = ""),
        fileName = paste(j,"_figBarDodged_",i,"_compareScen_",nameAppend,sep=""),
        figWidth = 13*max((length(unique(tbl_rpAgg$scenario))/2),1)*figWMult,pdfpng=pdfpng, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
      )

      # Line Chart Overlapped
      metis.chart(tbl_rpAgg,xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel, sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "line", facet_columns=NULL,
                  class ="scenario", classPalette = classPalette,
        dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i,"/compareScen",sep = ""),
        fileName = paste(j,"_figLineOverlap_",i,"_compareScen",nameAppend,sep=""),
        pdfpng=pdfpng, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
      )
      }

#-------------------------
# Diff Plots
#------------------------

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

      # Calculate Diff Values
      tbl_rpd<-tbl_rp%>%
        dplyr::filter(scenario==scenRef_i)%>%
        dplyr::select(-origScen,-origQuery,-origValue,-origUnits,-origX,-sources)
      if(!yData %in% names(tbl_rp)){tbl_rpd<-tbl_rpd%>%dplyr::select(-dplyr::one_of(c(yData)))}

      tbl_rpd_fixedCols <- tbl_rpd %>% dplyr::select("xLabel","classLabel1","classPalette1","classLabel2","classPalette2") %>% unique()

      for (k in unique(tbl_rp$scenario)[unique(tbl_rp$scenario)!=scenRef_i]){
        tbl_temp <- tbl_rp%>%
          dplyr::filter(scenario %in% c(scenRef_i,k))%>%
          dplyr::select(-origScen,-origQuery,-origValue,-origUnits,-origX,-sources,
                        -xLabel,classLabel1,classPalette1,classLabel2,classPalette2)
        if(!yData %in% names(tbl_temp)){tbl_temp<-tbl_temp%>%dplyr::select(-dplyr::one_of(c(yData)))}
        tbl_temp <- tbl_temp%>%
          tidyr::spread(scenario,yData)%>%
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

      if(length(unique(tbl_rpd$class1))>1){figWMult=1.3}else{figWMult=1}

      # Aggregated Class 1
      # Aggregate across classes
      tblAggsums<-tbl_rpd%>%
        dplyr::filter(aggregate=="sum")%>%
        dplyr::select(-class2,-classLabel2,-classPalette2)%>%
        dplyr::group_by_at(dplyr::vars(-value))%>%
        dplyr::summarize_at(c("value"),dplyr::funs(sum))
      tblAggmeans<-tbl_rpd%>%
        dplyr::filter(aggregate=="mean")%>%
        dplyr::select(-class2,-classLabel2,-classPalette2)%>%
        dplyr::group_by_at(dplyr::vars(-value))%>%
        dplyr::summarize_at(c("value"),dplyr::funs(mean))
      tbl_rpdC1<-dplyr::bind_rows(tblAggsums,tblAggmeans)%>%dplyr::ungroup()


      # Bar Chart
      metis.chart(tbl_rpdC1, xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel, sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar",
        dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i,"/compareScen",sep = ""),
        facet_columns="scenario",
        fileName = paste(j,"_figBarDiff_",i,"_compareScen",nameAppend,sep=""),
        figWidth = 13*max((length(unique(tbl_rpd$scenario))/2),1)*figWMult,
        figHeight = 9*max((length(unique(tbl_rpd$region))/2),1),pdfpng=pdfpng, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
      )

      # data=tbl_rpdC1; xData=xData;yData=yData;xLabel=xLabel;yLabel=yLabel; sizeBarLines=sizeBarLines;useNewLabels=useNewLabels;sizeLines=sizeLines; chartType = "bar";
      # dirOutputs = paste(dirOutputs, "/Charts/", i,"/compareScen",sep = "")
      # fileName = paste(j,"_figBarDiff_",i,"_compareScen",nameAppend,sep="")
      # figWidth = 13*max((length(unique(tbl_rpd$scenario))/2),1)*figWMult
      # figHeight = 9*max((length(unique(tbl_rpd$region))/2),1)
      # pdfpng=pdfpng; colOrder1 = colOrder1;colOrderName1 = colOrderName1;colOrder2 = colOrder2; colOrderName2 = colOrderName2


      # Line Chart
     metis.chart(tbl_rpdC1, xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel,sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "line",
        dirOutputs = paste(dirOutputs, "/Charts/",folderName,"/", i,"/compareScen",sep = ""),
        facet_columns="scenario",
        fileName = paste(j,"_figLineDiff_",i,"_compareScen",nameAppend,sep=""),
        figWidth = 13*max((length(unique(tbl_rpd$scenario))/2),1)*figWMult,
        figHeight = 9*max((length(unique(tbl_rpd$region))/2),1),pdfpng=pdfpng, colOrder1 = colOrder1,colOrderName1 = colOrderName1,colOrder2 = colOrder2, colOrderName2 = colOrderName2
      )

      } # Close if(nrow(tbl_rsp)>0)
      } # if(length(unique(tbl$scenario))>1){ to check if chosen param exists for comparison
      } # close loop for param
    } # close loop for region
  } # Close if multiple scenarios available
} # Close if(regionCompareOnly==1)


  return(tbl)

  } # Close Function
