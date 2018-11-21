#' srn.chartsProcess
#'
#' This function produces charts given any number of tables in the srn format.
#' The srn.chart() function produces charts for each region nd scenario.
#' If there are more than one scenario then the function also produces a folder for diffplots.
#' The input tables should be .csv files with the following columns:
#' scenario, region, sources, param, x, xLabel, vintage, class1, class2, units, value, aggregate,
#' classLabel1,classPalette1,classLabel2,classPalette2. Running the srn.readgcam automatically produces
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
#' \item classPalette1: An R or srn.colors() palette. Can leave the default as "pal_16".
#' \item classLabel2: If class2 exists then this will be legend Label. If it doesnt exist enter "classLabel2"
#' \item classPalette2: An R or srn.colors() palette. Can leave the default as "pal_16".
#' }
#' @param dataTables Vector of strings with full path to datatables to be read in.
#' Example c("D:/srn/outputs/Colombia/regional/dataTable_Colombia_1975to2100.csv",
#' "D:/srn/outputs/Colombia/regional/dataTableLocal_Colombia_1975to2100.csv").
#' Where "dataTableLocal_Colombia_1975to2100.csv" is the new datafile created based on
#' "dataTableTemplate_Colombia_1975to2100.csv" and contains new local data.
#' @param rTable If a table is created directly in R as a data.frame or tibble it can entered here.
#' @param dirOutputs Full path to directory for outputs
#' @param pdfpng Choose the format for outputs. Either "pdf", "png" or "both. Default is "png"
#' @param scenRef The reference scenario to compare against. Default will pick first scenario from
#' list f all scenarios
#' @param xData Default "x"
#' @param yData Default "value"
#' @param xLabel Default "xLabel"
#' @param yLabel Default "units"
#' @param class Default "class"
#' @param aggregate Default "sum"
#' @param classPalette Default "pal_Basic" from srn.colors()$pal_Basic
#' @param regionCompareOnly Default 0. If set to 1, will only run comparison plots and not individual
#' @param useNewLabels Default 0
#' @param sizeBarLines Default 0.5
#' @param sizeLines Default 1.5
#' regions
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
#' @keywords charts, diffplots
#' @return Produces charts in output folder and also returns combined table in srn format.
#' @export
#' @import dplyr tibble utils tidyr rlang

srn.chartsProcess <- function(dataTables=NULL,rTable=NULL,scenRef=NULL,
                       dirOutputs=paste(getwd(),"/outputs",sep=""),pdfpng="png",
                       xCompare=c("2015","2030","2050","2100"),
                       paramsSelect="All",
                       regionsSelect="All",
                       xData="x",yData="value",xLabel="xLabel",yLabel="units",
                       aggregate="sum",class="class", classPalette="pal_Basic",
                       regionCompareOnly=0,useNewLabels=0,
                       sizeBarLines=0.5,sizeLines=1.5) {

#------------------
# Load required Libraries
# -----------------
  requireNamespace("tibble",quietly = T)
  requireNamespace("dplyr",quietly = T)
  requireNamespace("utils",quietly = T)

#------------------
# Initialize variables to remove binding errors
# -----------------

  NULL->scenario->value->x->region->param->origValue->origScen->origQuery->
  origUnits->origX->sources->vintage->class1->classLabel1->classPalette1->
  class2->classLabel2->classPalette2->i->j->k

#------------------
# Function for adding any missing columns if needed
# -----------------

addMissing<-function(data){
  if(!"scenario"%in%names(data)){data<-data%>%mutate(scenario="scenario")}
  if(!"region"%in%names(data)){data<-data%>%mutate(region="region")}
  if(!"param"%in%names(data)){data<-data%>%mutate(param="param")}
  if(!"value"%in%names(data)){data<-data%>%mutate(value=get(yData))}
  if(!"origValue"%in%names(data)){data<-data%>%mutate(origValue=value)}
  if(!"units"%in%names(data)){data<-data%>%mutate(units="units")}
  if(!"vintage"%in%names(data)){data<-data%>%mutate(vintage="vintage")}
  if(!"x"%in%names(data)){data<-data%>%mutate(x="x")}
  if(!"xLabel"%in%names(data)){data<-data%>%mutate(xLabel=xLabel)}
  if(!"aggregate"%in%names(data)){data<-data%>%mutate(aggregate=aggregate)}
  if(!"class1"%in%names(data)){data<-data%>%mutate(class1="class1")}
  if(!"classLabel1"%in%names(data)){data<-data%>%mutate(classLabel1="classLabel1")}
  if(!"classPalette1"%in%names(data)){data<-data%>%mutate(classPalette1=classPalette)}
  if(!"class2"%in%names(data)){data<-data%>%mutate(class2="class2")}
  if(!"classLabel2"%in%names(data)){data<-data%>%mutate(classLabel2="classLabel2")}
  if(!"classPalette2"%in%names(data)){data<-data%>%mutate(classPalette2=classPalette)}
  if(!"origScen"%in%names(data)){data<-data%>%mutate(origScen="origScen")}
  if(!"origQuery"%in%names(data)){data<-data%>%mutate(origQuery="origQuery")}
  if(!"origUnits"%in%names(data)){data<-data%>%mutate(origUnits="origUnits")}
  if(!"origX"%in%names(data)){data<-data%>%mutate(origX="origX")}
  if(!"sources"%in%names(data)){data<-data%>%mutate(sources="sources")}
  return(data)
}


#------------------
# Read in tables (Either csv tables (dataTables) or R data (rTables))
#------------------

tbl<-tibble()

if(is.null(dataTables) & is.null(rTable)){
  stop ("No dataTable or rTables have been provided.")
}

# Read in csv (dataTables)
#------------------------
if(!is.null(dataTables)){

for(i in dataTables){
  if(file.exists(i)){
  tblNew<-read.csv(paste(i), stringsAsFactors = F)%>%as.tibble
  if(length(unique(tblNew$vintage))<2){tblNew<-tblNew%>%mutate(vintage = paste("Vint_", x, sep = ""))}
  tbl<-bind_rows(tbl,tblNew)
  } else {stop(paste(i," does not exist"))}
}

# Join relevant colors and classes using the mapping file if it exists
if(file.exists(paste(getwd(),"/dataFiles/mapping/template_Regional_mapping.csv", sep = ""))){
  map<-read.csv(paste(getwd(),"/dataFiles/mapping/template_Regional_mapping.csv", sep = ""), stringsAsFactors = F)%>%as.tibble
  tbl<-tbl%>%left_join(map%>%dplyr::select(-class1,-class2),by=c("param","units"))
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
tbl<-bind_rows(tbl,rTable)
}

tbl<-tbl%>%unique()


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

if(regionCompareOnly==0){
  for (i in unique(tbl$region)){
    tbl_r<-tbl%>%filter(region==i)
    if (!dir.exists(paste(dirOutputs, "/Charts/", i, sep = ""))){
      dir.create(paste(dirOutputs, "/Charts/", i, sep = ""))}
    if (!dir.exists(paste(dirOutputs, "/Charts/", i, "/regional", sep = ""))){
      dir.create(paste(dirOutputs, "/Charts/", i, "/regional", sep = ""))}
    if(length(unique(tbl_r$scenario))>1){
      if (!dir.exists(paste(dirOutputs, "/Charts/", i, "/regional/compareScen",sep = ""))){
        dir.create(paste(dirOutputs, "/Charts/", i, "/regional/compareScen",sep = ""))}}
    for (j in unique(tbl_r$scenario)) {
      if (!dir.exists(paste(dirOutputs, "/Charts/", i, "/regional","/", j,sep = "")))
      {dir.create(paste(dirOutputs, "/Charts/", i, "/regional","/", j,sep = ""))}
    }
  }
} # Close if(regionCompareOnly==0)


#------------------
# Tables
#------------------

for(i in unique(tbl$region)){
  utils::write.csv(tbl%>%
                     dplyr::filter(region == i)%>%
                     dplyr::select(scenario,region,param,units, class1, class2, x, value, vintage)%>%
                     tidyr::spread(scenario,yData),
                   file = paste(dirOutputs, "/Charts/Tables_regional_",i,".csv", sep = ""),row.names = F)
}

utils::write.csv(tbl%>%
                   dplyr::select(scenario, region, units, class1, class2, x, value, vintage)%>%
                   tidyr::spread(scenario,yData),
                 file = paste(dirOutputs, "/Charts/Tables_regional_allRegions.csv", sep = ""),row.names = F)


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

        # Bar Chart
        srn.printPdfPng(
          srn.chart(tbl_sp, xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel,
                    sizeBarLines=sizeBarLines,sizeLines=sizeLines, chartType = "bar",facet_columns="region",facet_rows="none"),
          dir = paste(dirOutputs, "/Charts/compareRegions","/", j,sep = ""),
          filename = paste(k,"_figBar_",j,"_compareRegions",sep=""),
          figWidth = 13*max((length(unique(tbl_sp$region))/2),1)
        )

        # Line Chart
        srn.printPdfPng(
          srn.chart(tbl_sp,xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel,
                    sizeBarLines=sizeBarLines,sizeLines=sizeLines, chartType = "line",facet_columns="region",facet_rows="none"),
          dir = paste(dirOutputs, "/Charts/compareRegions","/", j,sep = ""),
          filename = paste(k,"_figLines_",j,"_compareRegions",sep=""),
          figWidth = 13*max((length(unique(tbl_sp$region))/2),1)
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

        # Bar Chart
        srn.printPdfPng(
          srn.chart(tbl_p, xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel,
                    sizeBarLines=sizeBarLines,sizeLines=sizeLines, chartType = "bar",facet_columns="scenario",facet_rows="region"),
          dir = paste(dirOutputs, "/Charts/compareRegions/compareScen", sep = ""),
          filename = paste(j,"_figBar_compareScenRegions",sep=""),
          figWidth = 13*max((length(unique(tbl_p$scenario))/2),1),
          figHeight = 9*max((length(unique(tbl_p$region))/2),1)
        )

        # Line Chart
        srn.printPdfPng(
          srn.chart(tbl_p,xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel,
                    sizeBarLines=sizeBarLines,sizeLines=sizeLines, chartType = "line",facet_columns="scenario",facet_rows="region"),
          dir = paste(dirOutputs, "/Charts/compareRegions/compareScen", sep = ""),
          filename = paste(j,"_figLine_compareScenRegions",sep=""),
          figWidth = 13*max((length(unique(tbl_p$scenario))/2),1),
          figHeight = 9*max((length(unique(tbl_p$region))/2),1)
        )

        #-------------------------
        # Plot with Scenarios on X for Chosen Years
        #------------------------

        if(any(!xCompare %in% unique(tbl_p[[xData]]))){
          print(paste("xCompare not available in data: ", paste(xCompare[!(xCompare %in% unique(tbl_p[[xData]]))],collapse=", "), sep=""))
          print(paste("Comparing for only: ",  paste(xCompare[(xCompare %in% unique(tbl_p[[xData]]))],collapse=", "), sep=""))
          tbl_py <- tbl_p%>%filter(x %in% xCompare)}else{
            if(length(unique(tbl_p[[xData]]))<5){
              tbl_py <- tbl_p}else{
                xCompare<-c(unique(tbl_p[[xData]])[1],
                            unique(tbl_p[[xData]])[round(length(unique(tbl_p[[xData]]))/2)],
                            tail(unique(tbl_p[[xData]]),n=1)
                )
                tbl_py <- tbl_p%>%filter(x %in% xCompare)
              }
          }

        # Bar Chart
        srn.printPdfPng(
          srn.chart(tbl_py, xData ="scenario", yData=yData,xLabel=xLabel,yLabel=yLabel,
                    sizeBarLines=sizeBarLines,sizeLines=sizeLines, chartType = "bar", facet_columns = xData, facet_rows="region"),
          dir = paste(dirOutputs, "/Charts/compareRegions/compareScen", sep = ""),
          filename = paste(j,"_figBar_compareScenRegion_xScenSelectYears",sep=""),
          figWidth = 13*max((length(unique(tbl_py$x)[unique(tbl_py$x) %in% xCompare])/3),1),
          figHeight = 9*max((length(unique(tbl_py$region))/2),1)
        )


        #-------------------------
        # Aggregate and Plot Dodged/OverLapping Plots
        #------------------------

        # Aggregate across classes
        tbl_pAggsums<-tbl_p%>%
          dplyr::filter(aggregate=="sum")%>%
          dplyr::select(-contains(class))%>%
          group_by_at(vars(-yData,-origValue))%>%
          summarize_at(c(yData),funs(sum))
        tbl_pAggmeans<-tbl_p%>%
          dplyr::filter(aggregate=="mean")%>%
          dplyr::select(-contains(class))%>%
          group_by_at(vars(-yData,-origValue))%>%
          summarize_at(c(yData),funs(mean))
        tbl_pAgg<-bind_rows(tbl_pAggsums,tbl_pAggmeans)%>%ungroup()


        if(nrow(tbl_pAgg)>0){

          # Bar Chart Dodged
          srn.printPdfPng(
            srn.chart(tbl_pAgg, xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel, sizeBarLines=sizeBarLines,sizeLines=sizeLines, chartType = "bar",
                      class ="scenario", position ="dodge", classPalette = classPalette,
                      facet_columns="region",facet_rows="none"),
            dir = paste(dirOutputs, "/Charts/compareRegions/compareScen", sep = ""),
            filename = paste(j,"_figBarDodged_compareScenRegion",sep=""),
            figWidth = 13*max((length(unique(tbl_pAgg$scenario))/2),1),
            figHeight = 9*max((length(unique(tbl_pAgg$region))/2),1)
          )

          # Line Chart Overlapped
          srn.printPdfPng(
            srn.chart(tbl_pAgg,xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel,
                      sizeBarLines=sizeBarLines,sizeLines=sizeLines, chartType = "line",class ="scenario", classPalette = classPalette,
                      facet_columns="region",facet_rows="none"),
            dir = paste(dirOutputs, "/Charts/compareRegions/compareScen", sep = ""),
            filename = paste(j,"_figLineOverlap_compareScenRegion",sep=""),
            figWidth = 13*max((length(unique(tbl_pAgg$scenario))/2),1),
            figHeight = 9*max((length(unique(tbl_pAgg$region))/2),1)
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
          filter(scenario==scenRef_i)%>%
          dplyr::select(-origScen,-origQuery,-origValue,-origUnits,-origX,-sources)
        if(!yData %in% names(tbl_p)){tbl_pd<-tbl_pd%>%dplyr::select(-yData)}

        for (k in unique(tbl_p$scenario)[unique(tbl_p$scenario)!=scenRef_i]){
          tbl_temp <- tbl_p%>%
            dplyr::filter(scenario %in% c(scenRef_i,k))%>%
            dplyr::select(-origScen,-origQuery,-origValue,-origUnits,-origX,-sources)
          if(!yData %in% names(tbl_temp)){tbl_temp<-tbl_temp%>%dplyr::select(-yData)}
          tbl_temp <- tbl_temp%>%
            tidyr::spread(scenario,yData)%>%
            mutate(!!paste(k,"_diff",sep=""):=get(k)-get(scenRef_i))%>%
            dplyr::select(-k,-scenRef_i)
          tbl_temp<-tbl_temp%>%
            tidyr::gather(key=scenario,value=!!yData,
                          -c(names(tbl_temp)[!names(tbl_temp) %in% paste(k,"_diff",sep="")]))
          tbl_pd<-bind_rows(tbl_pd,tbl_temp)
        }

        tbl_pd <-tbl_pd %>%
          mutate(scenario=factor(scenario,
                                 levels=c(scenRef_i,
                                          unique(tbl_pd$scenario)[unique(tbl_pd$scenario)!=scenRef_i])))

        # Bar Chart
        srn.printPdfPng(
          srn.chart(tbl_pd, xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel,
                    sizeBarLines=sizeBarLines,sizeLines=sizeLines, chartType = "bar", facet_rows="region"),
          dir = paste(dirOutputs, "/Charts/compareRegions/compareScen", sep = ""),
          filename = paste(j,"_figBarDiff_compareScenRegion",sep=""),
          figWidth = 13*max((length(unique(tbl_pd$scenario))/2),1),
          figHeight = 9*max((length(unique(tbl_pd$region))/2),1)
        )

        # Line Chart
        srn.printPdfPng(
          srn.chart(tbl_pd, xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel,
                    sizeBarLines=sizeBarLines,sizeLines=sizeLines, chartType = "line", facet_rows="region"),
          dir = paste(dirOutputs, "/Charts/compareRegions/compareScen", sep = ""),
          filename = paste(j,"_figLineDiff_compareScenRegion",sep=""),
          figWidth = 13*max((length(unique(tbl_pd$scenario))/2),1),
          figHeight = 9*max((length(unique(tbl_pd$region))/2),1)
        )


        } # Close if(nrow(tbl_rsp)>0)
    } # close loop for param
} # Close if multiple scenarios available

} # if length(unique(tbl$region))>1

if(regionCompareOnly==0){

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

    # Bar Chart
    srn.printPdfPng(
    srn.chart(tbl_rsp, xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel,sizeBarLines=sizeBarLines,sizeLines=sizeLines, chartType = "bar"),
    dir = paste(dirOutputs, "/Charts/", i, "/regional","/", j,sep = ""),
    filename = paste(k,"_figBar_",i,"_",j,sep="")
    )

    # Line Chart
    srn.printPdfPng(
      srn.chart(tbl_rsp,xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel,sizeBarLines=sizeBarLines,sizeLines=sizeLines, chartType = "line"),
      dir = paste(dirOutputs, "/Charts/", i, "/regional","/", j,sep = ""),
      filename = paste(k,"_figLine_",i,"_",j,sep="")
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

      # Bar Chart
      srn.printPdfPng(
        srn.chart(tbl_rp, xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel, sizeBarLines=sizeBarLines,sizeLines=sizeLines, chartType = "bar"),
        dir = paste(dirOutputs, "/Charts/", i,"/regional/compareScen",sep = ""),
        filename = paste(j,"_figBar_",i,"_compareScen",sep=""),
        figWidth = 13*max((length(unique(tbl_rp$scenario))/2),1),
        figHeight = 9*max((length(unique(tbl_rp$region))/2),1)
      )

      # Line Chart
      srn.printPdfPng(
        srn.chart(tbl_rp,xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel, sizeBarLines=sizeBarLines,sizeLines=sizeLines, chartType = "line"),
        dir = paste(dirOutputs, "/Charts/", i,"/regional/compareScen",sep = ""),
        filename = paste(j,"_figLine_",i,"_compareScen",sep=""),
        figWidth = 13*max((length(unique(tbl_rp$scenario))/2),1),
        figHeight = 9*max((length(unique(tbl_rp$region))/2),1)
      )

#-------------------------
# Plot with Scenarios on X for Chosen Years
#------------------------

      if(any(!xCompare %in% unique(tbl_rp[[xData]]))){
      print(paste("xCompare not available in data: ", paste(xCompare[!(xCompare %in% unique(tbl_rp[[xData]]))],collapse=", "), sep=""))
      print(paste("Comparing for only: ",  paste(xCompare[(xCompare %in% unique(tbl_rp[[xData]]))],collapse=", "), sep=""))
      tbl_rpy <- tbl_rp%>%filter(x %in% xCompare)}else{
        if(length(unique(tbl_rp[[xData]]))<5){
          tbl_rpy <- tbl_rp}else{
        xCompare<-c(unique(tbl_rp[[xData]])[1],
                        unique(tbl_rp[[xData]])[round(length(unique(tbl_rp[[xData]]))/2)],
                        tail(unique(tbl_rp[[xData]]),n=1)
                        )
        tbl_rpy <- tbl_rp%>%filter(x %in% xCompare)
        }
      }

      # Bar Chart
      srn.printPdfPng(
        srn.chart(tbl_rpy, xData ="scenario", yData=yData,xLabel=xLabel,yLabel=yLabel, sizeBarLines=sizeBarLines,sizeLines=sizeLines, chartType = "bar", facet_columns = xData),
        dir = paste(dirOutputs, "/Charts/", i,"/regional/compareScen",sep = ""),
        filename = paste(j,"_figBar_",i,"_compareScen_xScenSelectYears",sep=""),
        figWidth = 13*max((length(unique(tbl_rpy$x)[unique(tbl_py$x) %in% xCompare])/3),1),
        figHeight = 9*max((length(unique(tbl_rpy$region))/2),1)
      )


#-------------------------
# Aggregate and Plot Dodged/OverLapping Plots
#------------------------

      # Aggregate across classes
      tbl_rpAggsums<-tbl_rp%>%
        dplyr::filter(aggregate=="sum")%>%
        dplyr::select(-contains(class))%>%
        group_by_at(vars(-yData,-origValue))%>%
        summarize_at(c(yData),funs(sum))
      tbl_rpAggmeans<-tbl_rp%>%
        dplyr::filter(aggregate=="mean")%>%
        dplyr::select(-contains(class))%>%
        group_by_at(vars(-yData,-origValue))%>%
        summarize_at(c(yData),funs(mean))
      tbl_rpAgg<-bind_rows(tbl_rpAggsums,tbl_rpAggmeans)%>%ungroup()


      if(nrow(tbl_rpAgg)>0){

      # Bar Chart Dodged
      srn.printPdfPng(
        srn.chart(tbl_rpAgg, xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel, sizeBarLines=sizeBarLines,sizeLines=sizeLines, chartType = "bar", facet_columns="none",
                  class ="scenario", position ="dodge", classPalette = classPalette),
        dir = paste(dirOutputs, "/Charts/", i,"/regional/compareScen",sep = ""),
        filename = paste(j,"_figBarDodged_",i,"_compareScen_",sep=""),
        figWidth = 13*max((length(unique(tbl_rpAgg$scenario))/2),1),
        figHeight = 9*max((length(unique(tbl_rpAgg$region))/2),1)
      )

      # Line Chart Overlapped
      srn.printPdfPng(
        srn.chart(tbl_rpAgg,xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel, sizeBarLines=sizeBarLines,sizeLines=sizeLines, chartType = "line", facet_columns="none",
                  class ="scenario", classPalette = classPalette),
        dir = paste(dirOutputs, "/Charts/", i,"/regional/compareScen",sep = ""),
        filename = paste(j,"_figLineOverlap_",i,"_compareScen",sep=""),
        figWidth = 13*max(length(unique(tbl_rpAgg$scenario)),2)/2,
        figHeight = 9*max(length(unique(tbl_rpAgg$region)),2)/2
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
        filter(scenario==scenRef_i)%>%
        dplyr::select(-origScen,-origQuery,-origValue,-origUnits,-origX,-sources)
      if(!yData %in% names(tbl_rp)){tbl_rpd<-tbl_rpd%>%dplyr::select(-yData)}

      for (k in unique(tbl_rp$scenario)[unique(tbl_rp$scenario)!=scenRef_i]){
        tbl_temp <- tbl_rp%>%
          dplyr::filter(scenario %in% c(scenRef_i,k))%>%
          dplyr::select(-origScen,-origQuery,-origValue,-origUnits,-origX,-sources)
        if(!yData %in% names(tbl_temp)){tbl_temp<-tbl_temp%>%dplyr::select(-yData)}
        tbl_temp <- tbl_temp%>%
          tidyr::spread(scenario,yData)%>%
          mutate(!!paste(k,"_diff",sep=""):=get(k)-get(scenRef_i))%>%
          dplyr::select(-k,-scenRef_i)
        tbl_temp<-tbl_temp%>%
          tidyr::gather(key=scenario,value=!!yData,
                        -c(names(tbl_temp)[!names(tbl_temp) %in% paste(k,"_diff",sep="")]))
        tbl_rpd<-bind_rows(tbl_rpd,tbl_temp)
      }

      tbl_rpd <-tbl_rpd %>%
        mutate(scenario=factor(scenario,
                               levels=c(scenRef_i,
                                        unique(tbl_rpd$scenario)[unique(tbl_rpd$scenario)!=scenRef_i])))

      # Bar Chart
      srn.printPdfPng(
        srn.chart(tbl_rpd, xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel, sizeBarLines=sizeBarLines,sizeLines=sizeLines, chartType = "bar"),
        dir = paste(dirOutputs, "/Charts/", i,"/regional/compareScen",sep = ""),
        filename = paste(j,"_figBarDiff_",i,"_compareScen",sep=""),
        figWidth = 13*max(length(unique(tbl_rpd$scenario)),2)/2,
        figHeight = 9*max(length(unique(tbl_rpd$region)),2)/2
      )

      # Line Chart
      srn.printPdfPng(
        srn.chart(tbl_rpd, xData=xData,yData=yData,xLabel=xLabel,yLabel=yLabel,sizeBarLines=sizeBarLines,sizeLines=sizeLines, chartType = "line"),
        dir = paste(dirOutputs, "/Charts/", i,"/regional/compareScen",sep = ""),
        filename = paste(j,"_figLineDiff_",i,"_compareScen",sep=""),
        figWidth = 13*max(length(unique(tbl_rpd$scenario)),2)/2,
        figHeight = 9*max(length(unique(tbl_rpd$region)),2)/2
      )

      } # Close if(nrow(tbl_rsp)>0)

      } # close loop for param
    } # close loop for region
  } # Close if multiple scenarios available
} # Close if(regionCompareOnly==0)


  return(tbl)

  } # Close Function
