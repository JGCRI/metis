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
#' @param xRange Default "All". Range of x values eg. c(2001:2005)
#' @param nameAppend Default =""
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
                       regionCompareOnly=1,useNewLabels=0,
                       sizeBarLines=0,sizeLines=1.5,
                       nameAppend="") {

#------------------
# Load required Libraries
# -----------------
  requireNamespace("tibble",quietly = T)
  requireNamespace("dplyr",quietly = T)
  requireNamespace("utils",quietly = T)
  requireNamespace("tidyr",quietly = T)
  requireNamespace("rlang",quietly = T)
  requireNamespace("magrittr",quietly = T)

#------------------
# Initialize variables to remove binding errors
# -----------------

  NULL->scenario->value->x->region->param->origValue->origScen->origQuery->
  origUnits->origX->sources->vintage->class1->classLabel1->classPalette1->
  class2->classLabel2->classPalette2->i->j->k->figWmult

#------------------
# Function for adding any missing columns if needed
# -----------------

addMissing<-function(data){
  if(!"scenario"%in%names(data)){data<-data%>%dplyr::mutate(scenario="scenario")}
  if(!"region"%in%names(data)){data<-data%>%dplyr::mutate(region="region")}
  if(!"param"%in%names(data)){data<-data%>%dplyr::mutate(param="param")}
  if(!"value"%in%names(data)){data<-data%>%dplyr::mutate(value=get(yData))}
  if(!"origValue"%in%names(data)){data<-data%>%dplyr::mutate(origValue=value)}
  if(!"units"%in%names(data)){data<-data%>%dplyr::mutate(units="units")}
  if(!"x"%in%names(data)){data<-data%>%dplyr::mutate(x="x")}
  if(!"vintage" %in% names(data)){data<-data%>%dplyr::mutate(vintage = paste("Vint_", x, sep = ""))}
  if(!"xLabel"%in%names(data)){
    if(is.null(xLabel)){data<-data%>%dplyr::mutate(xLabel="xLabel")}else{
      data<-data%>%dplyr::mutate(xLabel=xLabel)}}
  if(!"aggregate"%in%names(data)){ if(is.null(aggregate)){data<-data%>%dplyr::mutate(aggregate="aggregate")}else{
    data<-data%>%dplyr::mutate(aggregate=aggregate)}}
  if(!"class1"%in%names(data)){
    if("class"%in%names(data)){
    data<-data%>%dplyr::rename(class1=class)}else
      {data<-data%>%dplyr::mutate(class1="class1")}}
  if(!"classLabel1"%in%names(data)){ if(is.null(classPalette)){data<-data%>%dplyr::mutate(classLabel1="classLabel1")}
  if(!"classPalette1"%in%names(data)){data<-data%>%dplyr::mutate(classPalette1="pal_Basic")}else{
    data<-data%>%dplyr::mutate(classPalette1=classPalette)}}
  if(!"class2"%in%names(data)){data<-data%>%dplyr::mutate(class2="class2")}
  if(!"classLabel2"%in%names(data)){data<-data%>%dplyr::mutate(classLabel2="classLabel2")}
  if(!"classPalette2"%in%names(data)){ if(is.null(classPalette)){data<-data%>%dplyr::mutate(classPalette2="pal_Basic")}else{
    data<-data%>%dplyr::mutate(classPalette2=classPalette)}}
  if(!"origScen"%in%names(data)){data<-data%>%dplyr::mutate(origScen="origScen")}
  if(!"origQuery"%in%names(data)){data<-data%>%dplyr::mutate(origQuery="origQuery")}
  if(!"origUnits"%in%names(data)){data<-data%>%dplyr::mutate(origUnits="origUnits")}
  if(!"origX"%in%names(data)){data<-data%>%dplyr::mutate(origX="origX")}
  if(!"sources"%in%names(data)){data<-data%>%dplyr::mutate(sources="sources")}
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
   tbl<-dplyr::bind_rows(tbl,tblNew)
  } else {stop(paste(i," does not exist"))}
}

# Join relevant colors and classes using the mapping file if it exists
if(file.exists(paste(getwd(),"/dataFiles/mapping/template_Regional_mapping.csv", sep = ""))){
  map<-utils::read.csv(paste(getwd(),"/dataFiles/mapping/template_Regional_mapping.csv", sep = ""), stringsAsFactors = F)%>%tibble::as.tibble()
  tbl<-tbl%>%dplyr::left_join(map%>%dplyr::select(-class1,-class2),by=c("param","units"))
  }

# Add missing columns
  tbl<-addMissing(tbl)
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

tbl<-tbl%>%unique()%>%dplyr::filter(region %in% regionsSelect)
if(any(xRange!="All")){if(is.numeric(tbl$x)){tbl<-tbl%>%dplyr::filter(x %in% xRange)}}


#------------------
# Create Folders if needed
#------------------
if (!dir.exists(dirOutputs)){
  dir.create(dirOutputs)}
if (!dir.exists(paste(dirOutputs, "/Charts", sep = ""))){
  dir.create(paste(dirOutputs, "/Charts", sep = ""))}


if(length(unique(tbl$region))>1){
  if (!dir.exists(paste(dirOutputs, "/Charts/compareRegions", sep = ""))){
    dir.create(paste(dirOutputs, "/Charts/compareRegions", sep = ""))}
  if (!dir.exists(paste(dirOutputs, "/Charts/compareRegions/compareScen", sep = ""))){
    dir.create(paste(dirOutputs, "/Charts/compareRegions/compareScen", sep = ""))}
  for (j in unique(tbl$scenario)) {
    if (!dir.exists(paste(dirOutputs, "/Charts/compareRegions","/", j,sep = "")))
    {dir.create(paste(dirOutputs, "/Charts/compareRegions","/", j,sep = ""))}
  }
} # If length(unique(tbl$region))>1

if(regionCompareOnly!=1){
  for (i in unique(tbl$region)){
    tbl_r<-tbl%>%dplyr::filter(region==i)
    if (!dir.exists(paste(dirOutputs, "/Charts/", i, sep = ""))){
      dir.create(paste(dirOutputs, "/Charts/", i, sep = ""))}
    if(length(unique(tbl_r$scenario))>1){
      if (!dir.exists(paste(dirOutputs, "/Charts/", i, "/compareScen",sep = ""))){
        dir.create(paste(dirOutputs, "/Charts/", i, "/compareScen",sep = ""))}}
    for (j in unique(tbl_r$scenario)) {
      if (!dir.exists(paste(dirOutputs, "/Charts/", i,"/", j,sep = "")))
      {dir.create(paste(dirOutputs, "/Charts/", i,"/", j,sep = ""))}
    }
  }
} # Close if(regionCompareOnly!=1)


#------------------
# Tables
#------------------

# Aggregate across classes
tblAggsums<-tbl%>%
  dplyr::filter(aggregate=="sum")%>%
  dplyr::select(-tidyselect::contains("class"))%>%
  dplyr::select(scenario,region,param,units,x, value, vintage)%>%
  dplyr::group_by_at(dplyr::vars(-value))%>%
  dplyr::summarize_at(c("value"),dplyr::funs(sum))
tblAggmeans<-tbl%>%
  dplyr::filter(aggregate=="mean")%>%
  dplyr::select(-tidyselect::contains("class"))%>%
  dplyr::select(scenario,region,param,units,x, value, vintage)%>%
  dplyr::group_by_at(dplyr::vars(-value))%>%
  dplyr::summarize_at(c("value"),dplyr::funs(mean))
tblAgg<-dplyr::bind_rows(tblAggsums,tblAggmeans)%>%dplyr::ungroup()


for(i in unique(tbl$region)){
  utils::write.csv(tbl%>%
                     dplyr::filter(region == i)%>%
                     dplyr::select(scenario,region,param,units, class1, class2, x, value, vintage)%>%
                     tidyr::spread(scenario,yData),
                   file = paste(dirOutputs, "/Charts/Tables_regional_",i,".csv", sep = ""),row.names = F)

  utils::write.csv(tblAgg%>%
                     dplyr::filter(region == i)%>%
                     tidyr::spread(scenario,yData),
                   file = paste(dirOutputs, "/Charts/Tables_regional_",i,"_aggClass.csv", sep = ""),row.names = F)
}

utils::write.csv(tbl%>%
                   dplyr::select(scenario, region, units, class1, class2, x, value, vintage)%>%
                   tidyr::spread(scenario,yData),
                 file = paste(dirOutputs, "/Charts/Tables_regional_allRegions.csv", sep = ""),row.names = F)

utils::write.csv(tblAgg%>%
                   tidyr::spread(scenario,yData),
                 file = paste(dirOutputs, "/Charts/Tables_regional_allRegions_aggClass.csv", sep = ""),row.names = F)


#------------------------
# Print which parameters and regions if selected are available
#------------------------

if(any(paramsSelect!="All")){
  if(all(paramsSelect %in% unique(tbl$param))){
    print(paste("Running paramaters: ",  paste(paramsSelect[(paramsSelect %in% unique(tbl$param))],collapse=", "), sep=""))
  }else{
    print(paste("Parameters not available in data: ", paste(paramsSelect[!(paramsSelect %in% unique(tbl$param))],collapse=", "), sep=""))
    print(paste("Running remaining paramaters: ",  paste(paramsSelect[(paramsSelect %in% unique(tbl$param))],collapse=", "), sep=""))
  tbl<-tbl%>%dplyr::filter(param %in% paramsSelect[(paramsSelect %in% unique(tbl$param))])
  }
}

if(any(regionsSelect!="All")){
  if(all(regionsSelect %in% unique(tbl$region))){
    print(paste("Running regions: ",  paste(regionsSelect[(regionsSelect %in% unique(tbl$region))],collapse=", "), sep=""))
  }else{
    print(paste("Regions not available in data: ", paste(regionsSelect[!(regionsSelect %in% unique(tbl$region))],collapse=", "), sep=""))
    print(paste("Running remaining regions: ",  paste(regionsSelect[(regionsSelect %in% unique(tbl$region))],collapse=", "), sep=""))
tbl<-tbl%>%dplyr::filter(region %in% regionsSelect[(regionsSelect %in% unique(tbl$region))])
}
}


#------------------
# Create Charts for Regional Comparison
#------------------

if(length(unique(tbl$region))>1){

  for(j in unique(tbl$scenario)){
    for(k in unique(tbl$param)){

      tbl_sp<-tbl%>%dplyr::filter(scenario==j,
                                   param==k)

      if(nrow(tbl_sp)>0){

        if(length(unique(tbl_sp$class1))>1){figWMult=1.3}else{figWmult=1}

        # Bar Chart
       metis.chart(tbl_sp, xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel,
                    sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar",facet_columns="region",facet_rows="none",
          dirOutputs = paste(dirOutputs, "/Charts/compareRegions","/", j,sep = ""),
          fileName = paste(k,"_figBar_",j,"_compareRegions",nameAppend,sep=""),
          figWidth = 13*max((length(unique(tbl_sp$region))/2),1)*figWMult,pdfpng=pdfpng
        )

        # Line Chart
        metis.chart(tbl_sp,xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel,
                    sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "line",facet_columns="region",facet_rows="none",
          dirOutputs = paste(dirOutputs, "/Charts/compareRegions","/", j,sep = ""),
          fileName = paste(k,"_figLines_",j,"_compareRegions",nameAppend,sep=""),
          figWidth = 13*max((length(unique(tbl_sp$region))/2),1)*figWMult,pdfpng=pdfpng
        )

      } # Close if(nrow(tbl_sp)>0)

    } # close loop for param
  } # close loop for scenario


#------------------
# Compare Scenarios for each region
#------------------

if(length(unique(tbl$scenario))>1){


    for(j in unique(tbl$param)){

      tbl_p<-tbl%>%dplyr::filter(param==j)

      if(nrow(tbl_p)>0){

        if(length(unique(tbl_p$class1))>1){figWMult=1.3}else{figWmult=1}

        # Bar Chart
        metis.chart(tbl_p, xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel,
                    sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar",facet_columns="scenario",facet_rows="region",
          dirOutputs = paste(dirOutputs, "/Charts/compareRegions/compareScen", sep = ""),
          fileName = paste(j,"_figBar_compareScenRegions",nameAppend,sep=""),
          figWidth = 13*max((length(unique(tbl_p$scenario))/2),1)*figWMult,
          figHeight = 9*max((length(unique(tbl_p$region))/2),1),pdfpng=pdfpng
        )

        # Line Chart
        metis.chart(tbl_p,xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel,
                    sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "line",facet_columns="scenario",facet_rows="region",
          dirOutputs = paste(dirOutputs, "/Charts/compareRegions/compareScen", sep = ""),
          fileName = paste(j,"_figLine_compareScenRegions",nameAppend,sep=""),
          figWidth = 13*max((length(unique(tbl_p$scenario))/2),1)*figWMult,
          figHeight = 9*max((length(unique(tbl_p$region))/2),1),pdfpng=pdfpng
        )

        #-------------------------
        # Plot with Scenarios on X for Chosen Years
        #------------------------

        if(any(!xCompare %in% unique(tbl_p[[xData]]))){
          print(paste("xCompare not available in data: ", paste(xCompare[!(xCompare %in% unique(tbl_p[[xData]]))],collapse=", "), sep=""))
          print(paste("Comparing for only: ",  paste(xCompare[(xCompare %in% unique(tbl_p[[xData]]))],collapse=", "), sep=""))
          tbl_py <- tbl_p%>%dplyr::filter(x %in% xCompare)}else{
            if(length(unique(tbl_p[[xData]]))<5){
              tbl_py <- tbl_p}else{
                xCompare<-c(unique(tbl_p[[xData]])[1],
                            unique(tbl_p[[xData]])[round(length(unique(tbl_p[[xData]]))/2)],
                            utils::tail(unique(tbl_p[[xData]]),n=1)
                )
                tbl_py <- tbl_p%>%dplyr::filter(x %in% xCompare)
              }
          }

        if(length(unique(tbl_py$class1))>1){figWMult=1.3}else{figWmult=1}

        # Bar Chart
        metis.chart(tbl_py, xData ="scenario", yData=yData,xLabel=xLabel,yLabel=yLabel,
                    sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar", facet_columns = xData, facet_rows="region",
          dirOutputs = paste(dirOutputs, "/Charts/compareRegions/compareScen", sep = ""),
          fileName = paste(j,"_figBar_compareScenRegion_xScenSelectYears",nameAppend,sep=""),
          figWidth = 13*max((length(unique(tbl_py$x)[unique(tbl_py$x) %in% xCompare])/3),1)*figWMult,
          figHeight = 9*max((length(unique(tbl_py$region))/2),1),pdfpng=pdfpng
        )


        #-------------------------
        # Aggregate and Plot Dodged/OverLapping Plots
        #------------------------

        # Aggregate across classes
        tbl_pAggsums<-tbl_p%>%
          dplyr::filter(aggregate=="sum")%>%
          dplyr::select(-tidyselect::contains(class))%>%
          dplyr::group_by_at(dplyr::vars(-yData,-origValue))%>%
          dplyr::summarize_at(c(yData),dplyr::funs(sum))
        tbl_pAggmeans<-tbl_p%>%
          dplyr::filter(aggregate=="mean")%>%
          dplyr::select(-tidyselect::contains(class))%>%
          dplyr::group_by_at(dplyr::vars(-yData,-origValue))%>%
          dplyr::summarize_at(c(yData),dplyr::funs(mean))
        tbl_pAgg<-dplyr::bind_rows(tbl_pAggsums,tbl_pAggmeans)%>%dplyr::ungroup()


        if(nrow(tbl_pAgg)>0){

          # Bar Chart Dodged
          metis.chart(tbl_pAgg, xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel, sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar",
                      class ="scenario", position ="dodge", classPalette = classPalette,
                      facet_columns="region",facet_rows="none",
            dirOutputs = paste(dirOutputs, "/Charts/compareRegions/compareScen", sep = ""),
            fileName = paste(j,"_figBarDodged_compareScenRegion",nameAppend,sep=""),
            figWidth = 13*max((length(unique(tbl_pAgg$region))/2),1),pdfpng=pdfpng
          )

          # Line Chart Overlapped
          metis.chart(tbl_pAgg,xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel,
                      sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "line",class ="scenario", classPalette = classPalette,
                      facet_columns="region",facet_rows="none",
            dirOutputs = paste(dirOutputs, "/Charts/compareRegions/compareScen", sep = ""),
            fileName = paste(j,"_figLineOverlap_compareScenRegion",nameAppend,sep=""),
            figWidth = 13*max((length(unique(tbl_pAgg$region))/2),1),pdfpng=pdfpng
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
        if(!yData %in% names(tbl_p)){tbl_pd<-tbl_pd%>%dplyr::select(-yData)}

        for (k in unique(tbl_p$scenario)[unique(tbl_p$scenario)!=scenRef_i]){
          tbl_temp <- tbl_p%>%
            dplyr::filter(scenario %in% c(scenRef_i,k))%>%
            dplyr::select(-origScen,-origQuery,-origValue,-origUnits,-origX,-sources)
          if(!yData %in% names(tbl_temp)){tbl_temp<-tbl_temp%>%dplyr::select(-yData)}
          tbl_temp <- tbl_temp%>%
            tidyr::spread(scenario,yData)%>%
            dplyr::mutate(!!paste(k,"_diff",sep=""):=get(k)-get(scenRef_i))%>%
            dplyr::select(-k,-scenRef_i)
          tbl_temp<-tbl_temp%>%
            tidyr::gather(key=scenario,value=!!yData,
                          -c(names(tbl_temp)[!names(tbl_temp) %in% paste(k,"_diff",sep="")]))
          tbl_pd<-dplyr::bind_rows(tbl_pd,tbl_temp)
        }

        tbl_pd <-tbl_pd %>%
          dplyr::mutate(scenario=factor(scenario,
                                 levels=c(scenRef_i,
                                          unique(tbl_pd$scenario)[unique(tbl_pd$scenario)!=scenRef_i])))

        if(length(unique(tbl_pd$class1))>1){figWMult=1.3}else{figWmult=1}

        # Bar Chart
        metis.chart(tbl_pd, xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel,
                    sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar", facet_rows="region",
          dirOutputs = paste(dirOutputs, "/Charts/compareRegions/compareScen", sep = ""),
          fileName = paste(j,"_figBarDiff_compareScenRegion",nameAppend,sep=""),
          figWidth = 13*max((length(unique(tbl_pd$scenario))/2),1)*figWMult,
          figHeight = 9*max((length(unique(tbl_pd$region))/2),1),pdfpng=pdfpng
        )

        # Line Chart
        metis.chart(tbl_pd, xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel,
                    sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "line", facet_rows="region",
          dirOutputs = paste(dirOutputs, "/Charts/compareRegions/compareScen", sep = ""),
          fileName = paste(j,"_figLineDiff_compareScenRegion",nameAppend,sep=""),
          figWidth = 13*max((length(unique(tbl_pd$scenario))/2),1)*figWMult,
          figHeight = 9*max((length(unique(tbl_pd$region))/2),1),pdfpng=pdfpng
        )


        } # Close if(nrow(tbl_rsp)>0)
    } # close loop for param
} # Close if multiple scenarios available

} # if length(unique(tbl$region))>1

if(regionCompareOnly!=1){

#------------------
# Create Charts for Each Region & Each Scenario
#------------------

for(i in unique(tbl$region)){
  for(j in unique(tbl$scenario)){
    for(k in unique(tbl$param)){

    tbl_rsp<-tbl%>%dplyr::filter(region==i,
                          scenario==j,
                          param==k)

    if(nrow(tbl_rsp)>0){

      if(length(unique(tbl_rsp$class1))>1){figWMult=1.3}else{figWmult=1}

    # Bar Chart
    metis.chart(tbl_rsp, xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel,sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar",
    dirOutputs = paste(dirOutputs, "/Charts/", i, "/", j,sep = ""),
    fileName = paste(k,"_figBar_",i,"_",j,nameAppend,sep=""),pdfpng=pdfpng
    )

    # Line Chart
    metis.chart(tbl_rsp,xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel,sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "line",
      dirOutputs = paste(dirOutputs, "/Charts/", i, "/", j,sep = ""),
      fileName = paste(k,"_figLine_",i,"_",j,nameAppend,sep=""),pdfpng=pdfpng
    )

    } # Close if(nrow(tbl_rsp)>0)

} # close loop for param
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

      if(nrow(tbl_rp)>0){

        if(length(unique(tbl_rp$class1))>1){figWMult=1.3}else{figWmult=1}

      # Bar Chart
      metis.chart(tbl_rp, xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel, sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar",
        dirOutputs = paste(dirOutputs, "/Charts/", i,"/compareScen",sep = ""),
        fileName = paste(j,"_figBar_",i,"_compareScen",nameAppend,sep=""),
        figWidth = 13*max((length(unique(tbl_rp$scenario))/2),1)*figWMult,
        figHeight = 9*max((length(unique(tbl_rp$region))/2),1),pdfpng=pdfpng
      )

      # Line Chart
      metis.chart(tbl_rp,xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel, sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "line",
        dirOutputs = paste(dirOutputs, "/Charts/", i,"/compareScen",sep = ""),
        fileName = paste(j,"_figLine_",i,"_compareScen",nameAppend,sep=""),
        figWidth = 13*max((length(unique(tbl_rp$scenario))/2),1)*figWMult,
        figHeight = 9*max((length(unique(tbl_rp$region))/2),1),pdfpng=pdfpng
      )

#-------------------------
# Plot with Scenarios on X for Chosen Years
#------------------------

      if(any(!xCompare %in% unique(tbl_rp[[xData]]))){
      print(paste("xCompare not available in data: ", paste(xCompare[!(xCompare %in% unique(tbl_rp[[xData]]))],collapse=", "), sep=""))
      print(paste("Comparing for only: ",  paste(xCompare[(xCompare %in% unique(tbl_rp[[xData]]))],collapse=", "), sep=""))
      tbl_rpy <- tbl_rp%>%dplyr::filter(x %in% xCompare)}else{
        if(length(unique(tbl_rp[[xData]]))<5){
          tbl_rpy <- tbl_rp}else{
        xCompare<-c(unique(tbl_rp[[xData]])[1],
                        unique(tbl_rp[[xData]])[round(length(unique(tbl_rp[[xData]]))/2)],
                        utils::tail(unique(tbl_rp[[xData]]),n=1)
                        )
        tbl_rpy <- tbl_rp%>%dplyr::filter(x %in% xCompare)
        }
      }

      if(length(unique(tbl_rpy$class1))>1){figWMult=1.3}else{figWmult=1}

      # Bar Chart
      metis.chart(tbl_rpy, xData ="scenario", yData=yData,xLabel=xLabel,yLabel=yLabel, sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar", facet_columns = xData,
        dirOutputs = paste(dirOutputs, "/Charts/", i,"/compareScen",sep = ""),
        fileName = paste(j,"_figBar_",i,"_compareScen_xScenSelectYears",nameAppend,sep=""),
        figWidth = 13*max((length(unique(tbl_rpy$x)[unique(tbl_rpy$x) %in% xCompare])/3),1)*figWMult,
        figHeight = 9*max((length(unique(tbl_rpy$region))/2),1),pdfpng=pdfpng
      )


#-------------------------
# Aggregate and Plot Dodged/OverLapping Plots
#------------------------

      # Aggregate across classes
      tbl_rpAggsums<-tbl_rp%>%
        dplyr::filter(aggregate=="sum")%>%
        dplyr::select(-tidyselect::contains(class))%>%
        dplyr::group_by_at(dplyr::vars(-yData,-origValue))%>%
        dplyr::summarize_at(c(yData),dplyr::funs(sum))
      tbl_rpAggmeans<-tbl_rp%>%
        dplyr::filter(aggregate=="mean")%>%
        dplyr::select(-tidyselect::contains(class))%>%
        dplyr::group_by_at(dplyr::vars(-yData,-origValue))%>%
        dplyr::summarize_at(c(yData),dplyr::funs(mean))
      tbl_rpAgg<-dplyr::bind_rows(tbl_rpAggsums,tbl_rpAggmeans)%>%dplyr::ungroup()


      if(nrow(tbl_rpAgg)>0){

        if(length(unique(tbl_rpAgg$class1))>1){figWMult=1.3}else{figWmult=1}

      # Bar Chart Dodged
      metis.chart(tbl_rpAgg, xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel, sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar", facet_columns="none",
                  class ="scenario", position ="dodge", classPalette = classPalette,
        dirOutputs = paste(dirOutputs, "/Charts/", i,"/compareScen",sep = ""),
        fileName = paste(j,"_figBarDodged_",i,"_compareScen_",nameAppend,sep=""),
        figWidth = 13*max((length(unique(tbl_rpAgg$scenario))/2),1)*figWMult,
        figHeight = 9*max((length(unique(tbl_rpAgg$region))/2),1),pdfpng=pdfpng
      )

      # Line Chart Overlapped
      metis.chart(tbl_rpAgg,xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel, sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "line", facet_columns="none",
                  class ="scenario", classPalette = classPalette,
        dirOutputs = paste(dirOutputs, "/Charts/", i,"/compareScen",sep = ""),
        fileName = paste(j,"_figLineOverlap_",i,"_compareScen",nameAppend,sep=""),
        figWidth = 13*max((length(unique(tbl_rpAgg$scenario))/2),1)*figWMult,
        figHeight = 9*max((length(unique(tbl_rpAgg$region))/2),1),pdfpng=pdfpng
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
      if(!yData %in% names(tbl_rp)){tbl_rpd<-tbl_rpd%>%dplyr::select(-yData)}

      for (k in unique(tbl_rp$scenario)[unique(tbl_rp$scenario)!=scenRef_i]){
        tbl_temp <- tbl_rp%>%
          dplyr::filter(scenario %in% c(scenRef_i,k))%>%
          dplyr::select(-origScen,-origQuery,-origValue,-origUnits,-origX,-sources)
        if(!yData %in% names(tbl_temp)){tbl_temp<-tbl_temp%>%dplyr::select(-yData)}
        tbl_temp <- tbl_temp%>%
          tidyr::spread(scenario,yData)%>%
          dplyr::mutate(!!paste(k,"_diff",sep=""):=get(k)-get(scenRef_i))%>%
          dplyr::select(-k,-scenRef_i)
        tbl_temp<-tbl_temp%>%
          tidyr::gather(key=scenario,value=!!yData,
                        -c(names(tbl_temp)[!names(tbl_temp) %in% paste(k,"_diff",sep="")]))
        tbl_rpd<-dplyr::bind_rows(tbl_rpd,tbl_temp)
      }

      tbl_rpd <-tbl_rpd %>%
        dplyr::mutate(scenario=factor(scenario,
                               levels=c(scenRef_i,
                                        unique(tbl_rpd$scenario)[unique(tbl_rpd$scenario)!=scenRef_i])))

      if(length(unique(tbl_rpd$class1))>1){figWMult=1.3}else{figWmult=1}

      # Bar Chart
      metis.chart(tbl_rpd, xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel, sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "bar",
        dirOutputs = paste(dirOutputs, "/Charts/", i,"/compareScen",sep = ""),
        fileName = paste(j,"_figBarDiff_",i,"_compareScen",nameAppend,sep=""),
        figWidth = 13*max((length(unique(tbl_rpd$scenario))/2),1)*figWMult,
        figHeight = 9*max((length(unique(tbl_rpd$region))/2),1),pdfpng=pdfpng
      )

      # Line Chart
     metis.chart(tbl_rpd, xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel,sizeBarLines=sizeBarLines,useNewLabels=useNewLabels,sizeLines=sizeLines, chartType = "line",
        dirOutputs = paste(dirOutputs, "/Charts/", i,"/compareScen",sep = ""),
        fileName = paste(j,"_figLineDiff_",i,"_compareScen",nameAppend,sep=""),
        figWidth = 13*max((length(unique(tbl_rpd$scenario))/2),1)*figWMult,
        figHeight = 9*max((length(unique(tbl_rpd$region))/2),1),pdfpng=pdfpng
      )

      } # Close if(nrow(tbl_rsp)>0)

      } # close loop for param
    } # close loop for region
  } # Close if multiple scenarios available
} # Close if(regionCompareOnly==1)


  return(tbl)

  } # Close Function
