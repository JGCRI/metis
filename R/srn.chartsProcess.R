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
#' @param yearsCompare Choose the years to compare scenarios for xScenSelectYears plot. Default is
#' c("2015","2030","2050","2100")
#' @param paramsSelect Default = "All". Select the paramaters to analyze from the the tables provided.
#' The parameters corresponding to the different gcam query files are as follows:
#' \itemize{
#' \item "Query" : "Param"
#' \item "Total final energy by aggregate end-use sector" : "finalNrgbySec"
#' \item "GDP per capita MER by region" : "gdp"
#' \item "GDP MER by region" : "gdp"
#' \item "GDP Growth Rate (Percent)" : Calculated based on the GDP MER by region.
#' \item "Population by region"
#' \item "ag production by tech" : Where technologies signify irrigated or rainfed
#' }
#' @param regionsSelect Default = "All". Select regions to create charts for.
#' @keywords charts, diffplots
#' @return Produces charts in output folder and also returns combined table in srn format.
#' @export
#' @import dplyr tibble utils tidyr rlang

srn.chartsProcess <- function(dataTables=NULL,rTable=NULL,scenRef=NULL,
                       dirOutputs=paste(getwd(),"/outputs",sep=""),pdfpng="png",
                       yearsCompare=c("2015","2030","2050","2100"),
                       paramsSelect="All",
                       regionsSelect="All") {

#------------------
# Load required Libraries
# -----------------
  requireNamespace("tibble",quietly = T)
  requireNamespace("dplyr",quietly = T)
  requireNamespace("utils",quietly = T)

#------------------
# Initialize variables to remove binding errors
# -----------------

  NULL->scenario->value->x->region->param->aggregate->origValue->origScen->origQuery->
  origUnits->origX->sources->vintage->xLabel->class1->classLabel1->classPalette1->
  class2->classLabel2->classPalette2->i->j->k

#------------------
# Read in tables
#------------------

tbl<-tibble()

if(is.null(dataTables) & is.null(rTable)){
  stop ("No dataTable or rTables have been provided.")
}

if(!is.null(dataTables)){

for(i in dataTables){
  if(file.exists(i)){
  # Check if any new data tables using the template have been added. The template has
  # fewer number of columns since some columns are only used to track processed gcam data.
  # These missing columns are added in order to have consistent rows with gcamdata tables.
  tblNew<-read.csv(paste(i), stringsAsFactors = F)%>%as.tibble
  if(ncol(tblNew)==16){
    tblNew<-tblNew%>%
      mutate(origScen=scenario,
             origQuery="Query",
             origValue=value,
             origUnits=units,
             x=as.numeric(x),
             origX=x)
  if(length(unique(tblNew$vintage))<2){tblNew<-tblNew%>%mutate(vintage = paste("Vint_", x, sep = ""))}
  } else {stop(paste("Table ",i," format is not correct. The table should have the following column names:
                scenario, region, sources, param, class1, class2, units, x,
                value, vintage, xLabel, aggregate, classLabel1, classPalette1, classLabel2, classPalette2",sep=""))}
  tbl<-bind_rows(tbl,tblNew)
  } else {stop(paste(i," does not exist"))}
}

}

if(!is.null(rTable)){
tbl<-bind_rows(tbl,rTable)
}

tbl<-tbl%>%unique()

if(any(paramsSelect!="All")){
  if(!all(paramsSelect %in% unique(tbl$param))){
    print(paste("Parameters not available in data: ", paste(paramsSelect[!(paramsSelect %in% unique(tbl$param))],collapse=", "), sep=""))
    print(paste("Running remaining paramaters: ",  paste(paramsSelect[(paramsSelect %in% unique(tbl$param))],collapse=", "), sep=""))
    }
  tbl<-tbl%>%dplyr::filter(param %in% paramsSelect[(paramsSelect %in% unique(tbl$param))])
}


if(any(regionsSelect!="All")){
  if(!all(regionsSelect %in% unique(tbl$region))){
    print(paste("Regions not available in data: ", paste(regionsSelect[!(regionsSelect %in% unique(tbl$region))],collapse=", "), sep=""))
    print(paste("Running remaining regions: ",  paste(regionsSelect[(regionsSelect %in% unique(tbl$region))],collapse=", "), sep=""))
}
tbl<-tbl%>%dplyr::filter(region %in% regionsSelect[(regionsSelect %in% unique(tbl$region))])
}

#------------------
# Create Folders if needed
#------------------
if (!dir.exists(dirOutputs)){
  dir.create(dirOutputs)}
for (i in unique(tbl$region)){
    if (!dir.exists(paste(dirOutputs, "/", i, sep = ""))){
      dir.create(paste(dirOutputs, "/", i, sep = ""))}
    if (!dir.exists(paste(dirOutputs, "/", i, "/regional", sep = ""))){
      dir.create(paste(dirOutputs, "/", i, "/regional", sep = ""))}
  if(length(unique(tbl$scenario))>1){
  if (!dir.exists(paste(dirOutputs, "/", i, "/regional/compareScen",sep = ""))){
    dir.create(paste(dirOutputs, "/", i, "/regional/compareScen",sep = ""))}}
  for (j in unique(tbl$scenario)) {
    if (!dir.exists(paste(dirOutputs, "/", i, "/regional","/", j,sep = "")))
    {dir.create(paste(dirOutputs, "/", i, "/regional","/", j,sep = ""))}
  }
}


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
    srn.chart(tbl_rsp, chartType = "bar"),
    dir = paste(dirOutputs, "/", i, "/regional","/", j,sep = ""),
    filename = paste(k,"_figBar_",i,"_",j,sep="")
    )

    # Line Chart
    srn.printPdfPng(
      srn.chart(tbl_rsp,chartType = "line"),
      dir = paste(dirOutputs, "/", i, "/regional","/", j,sep = ""),
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
        srn.chart(tbl_rp, chartType = "bar"),
        dir = paste(dirOutputs, "/", i,"/regional/compareScen",sep = ""),
        filename = paste(j,"_figBar_",i,"_compareScen",sep=""),
        figWidth = 13*length(unique(tbl_rp$scenario))/2
      )

      # Line Chart
      srn.printPdfPng(
        srn.chart(tbl_rp,chartType = "line"),
        dir = paste(dirOutputs, "/", i,"/regional/compareScen",sep = ""),
        filename = paste(j,"_figLine_",i,"_compareScen",sep=""),
        figWidth = 13*length(unique(tbl_rp$scenario))/2
      )

#-------------------------
# Plot with Scenarios on X for Chosen Years
#------------------------

      tbl_rpy <- tbl_rp%>%filter(x %in% yearsCompare)

      # Bar Chart
      srn.printPdfPng(
        srn.chart(tbl_rpy, chartType = "bar", facet_columns = "x", xData ="scenario"),
        dir = paste(dirOutputs, "/", i,"/regional/compareScen",sep = ""),
        filename = paste(j,"_figBar_",i,"_compareScen_xScenSelectYears",sep=""),
        figWidth = 13*length(unique(tbl_rp$scenario))/2
      )


#-------------------------
# Aggregate and Plot Dodged/OverLapping Plots
#------------------------

      # Aggregate across classes
      tbl_rpAggsums<-tbl_rp%>%
        dplyr::filter(aggregate=="sum")%>%
        dplyr::select(-contains("class"))%>%
        group_by_at(vars(-value,-origValue))%>%
        summarize_at(c("value"),funs(sum))
      tbl_rpAggmeans<-tbl_rp%>%
        dplyr::filter(aggregate=="mean")%>%
        dplyr::select(-contains("class"))%>%
        group_by_at(vars(-value,-origValue))%>%
        summarize_at(c("value"),funs(mean))
      tbl_rpAgg<-bind_rows(tbl_rpAggsums,tbl_rpAggmeans)%>%ungroup()


      # Bar Chart Dodged
      srn.printPdfPng(
        srn.chart(tbl_rpAgg, chartType = "bar", facet_columns="none",
                  class ="scenario", position ="dodge", classPalette = "pal_Basic"),
        dir = paste(dirOutputs, "/", i,"/regional/compareScen",sep = ""),
        filename = paste(j,"_figBarDodged_",i,"_compareScen_",sep="")
      )

      # Line Chart Overlapped
      srn.printPdfPng(
        srn.chart(tbl_rpAgg, chartType = "line", facet_columns="none",
                  class ="scenario", classPalette = "pal_Basic"),
        dir = paste(dirOutputs, "/", i,"/regional/compareScen",sep = ""),
        filename = paste(j,"_figLineOverlap_",i,"_compareScen",sep="")
      )

#-------------------------
# Diff Plots
#------------------------

      if(is.null(scenRef)){scenRef_i = unique(tbl_rp$scenario)[1]}else{
        scenRef_i <- scenRef
      } # Check if Ref Scenario Chosen

      # Calculate Diff Values
      tbl_rpd<-tbl_rp%>%
        filter(scenario==scenRef_i)%>%
        dplyr::select(-origScen,-origQuery,-origValue,-origUnits,-origX,-sources)

      for (k in unique(tbl_rp$scenario)[unique(tbl_rp$scenario)!=scenRef_i]){
        tbl_temp <- tbl_rp%>%
          dplyr::filter(scenario %in% c(scenRef_i,k))%>%
          dplyr::select(-origScen,-origQuery,-origValue,-origUnits,-origX,-sources)%>%
          tidyr::spread(scenario,value)%>%
          mutate(!!paste(k,"_diff",sep=""):=get(k)-get(scenRef_i))%>%
          dplyr::select(-k,-scenRef_i)%>%
          #dplyr::rename(!!paste(k):=!!paste(k,"_diff",sep=""))%>%
          tidyr::gather(key=scenario,value=value,
                        -region,-param,-units,-vintage,-x,-xLabel,-aggregate,
                        -class1,-classLabel1,-classPalette1,-class2,-classLabel2,
                        -classPalette2)
        tbl_rpd<-bind_rows(tbl_rpd,tbl_temp)
      }

      tbl_rpd <-tbl_rpd %>%
        mutate(scenario=factor(scenario,
                               levels=c(scenRef_i,
                                        unique(tbl_rpd$scenario)[unique(tbl_rpd$scenario)!=scenRef_i])))

      # Bar Chart
      srn.printPdfPng(
        srn.chart(tbl_rpd, chartType = "bar"),
        dir = paste(dirOutputs, "/", i,"/regional/compareScen",sep = ""),
        filename = paste(j,"_figBarDiff_",i,"_compareScen",sep=""),
        figWidth = 13*length(unique(tbl_rpd$scenario))/2
      )

      # Line Chart
      srn.printPdfPng(
        srn.chart(tbl_rpd,chartType = "line"),
        dir = paste(dirOutputs, "/", i,"/regional/compareScen",sep = ""),
        filename = paste(j,"_figLineDiff_",i,"_compareScen",sep=""),
        figWidth = 13*length(unique(tbl_rpd$scenario))/2
      )

      } # Close if(nrow(tbl_rsp)>0)

      } # close loop for param
    } # close loop for region
  } # Close if multiple scenarios available

  return(tbl)

  } # Close Function
