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
#' @param dirOutputs Full path to directory for outputs
#' @param pdfpng Choose the format for outputs. Either "pdf", "png" or "both. Default is "png"
#' @keywords charts, diffplots
#' @return Produces charts in output folder and also returns combined table in srn format.
#' @export
#' @import dplyr tibble

srn.chartsProcess <- function(dataTables,scenRef=NULL,
                       dirOutputs=paste(getwd(),"/outputs",sep=""),pdfpng="png") {

#------------------
# Load required Libraries
# -----------------
  library(tibble,quietly = T)
  library(dplyr,quietly = T)


#------------------
# Read in tables
#------------------

tbl<-tibble()

for(table_i in dataTables){
  if(file.exists(table_i)){
  # Check if any new data tables using the template have been added. The template has
  # fewer number of columns since some columns are only used to track processed gcam data.
  # These missing columns are added in order to have consistent rows with gcamdata tables.
  tblNew<-read.csv(paste(table_i), stringsAsFactors = F)%>%as.tibble
  if(length(names(tblNew))<21){
    tblNew<-tblNew%>%
      mutate(origScen=scenario,
             origQuery="Query",
             origValue=value,
             origUnits=units,
             origX=x)}
  tbl<-bind_rows(tbl,tblNew)
  } else {stop(paste(table_i," does not exist"))}
}


#------------------
# Create Folders if needed
#------------------
if (!dir.exists(dirOutputs)){
  dir.create(dirOutputs)}
for (region_i in unique(tbl$region)){
    if (!dir.exists(paste(dirOutputs, "/", region_i, sep = ""))){
      dir.create(paste(dirOutputs, "/", region_i, sep = ""))}
    if (!dir.exists(paste(dirOutputs, "/", region_i, "/regional", sep = ""))){
      dir.create(paste(dirOutputs, "/", region_i, "/regional", sep = ""))}
  if(length(unique(tbl$scenario))>1){
  if (!dir.exists(paste(dirOutputs, "/", region_i, "/regional/compareScen",sep = ""))){
    dir.create(paste(dirOutputs, "/", region_i, "/regional/compareScen",sep = ""))}}
  for (scenario_i in unique(tbl$scenario)) {
    if (!dir.exists(paste(dirOutputs, "/", region_i, "/regional","/", scenario_i,sep = "")))
    {dir.create(paste(dirOutputs, "/", region_i, "/regional","/", scenario_i,sep = ""))}
  }
}


#------------------
# Create Charts for Each Region & Each Scenario
#------------------

for(region_i in unique(tbl$region)){
  for(scenario_i in unique(tbl$scenario)){
    for(param_i in unique(tbl$param)){

    tbl_rsp<-tbl%>%dplyr::filter(region==region_i,
                          scenario==scenario_i,
                          param==param_i)

    # Bar Chart
    srn.printPdfPng(
    srn.chart(tbl_rsp, chartType = "bar"),
    dir = paste(dirOutputs, "/", region_i, "/regional","/", scenario_i,sep = ""),
    filename = paste("figBar_",region_i,"_",scenario_i,"_",param_i,sep="")
    )

    # Line Chart
    srn.printPdfPng(
      srn.chart(tbl_rsp,chartType = "line"),
      dir = paste(dirOutputs, "/", region_i, "/regional","/", scenario_i,sep = ""),
      filename = paste("figLine_",region_i,"_",scenario_i,"_",param_i,sep="")
    )

} # close loop for param
} # close loop for scenario
} # close loop for region


#------------------
# Compare Scenarios for each region
#------------------

if(length(unique(tbl$scenario))>1){


for(region_i in unique(tbl$region)){
    for(param_i in unique(tbl$param)){

      tbl_rp<-tbl%>%dplyr::filter(region==region_i,
                                   param==param_i)

      # Bar Chart
      srn.printPdfPng(
        srn.chart(tbl_rp, chartType = "bar"),
        dir = paste(dirOutputs, "/", region_i,"/regional/compareScen",sep = ""),
        filename = paste("figBar_",region_i,"_compareScen_",param_i,sep=""),
        figWidth = 13*length(unique(tbl_rp$scenario))/2
      )

      # Line Chart
      srn.printPdfPng(
        srn.chart(tbl_rp,chartType = "line"),
        dir = paste(dirOutputs, "/", region_i,"/regional/compareScen",sep = ""),
        filename = paste("figLine_",region_i,"_compareScen_",param_i,sep=""),
        figWidth = 13*length(unique(tbl_rp$scenario))/2
      )

      # Aggregate across classes
      tbl_rpAggsums<-tbl_rp%>%
        dplyr::filter(aggregate=="sum")%>%
        dplyr::select(-contains(class))%>%
        group_by_at(vars(-value,-origValue))%>%
        summarize_at(c("value"),funs(sum))
      tbl_rpAggmeans<-tbl_rp%>%
        dplyr::filter(aggregate=="mean")%>%
        dplyr::select(-contains(class))%>%
        group_by_at(vars(-value,-origValue))%>%
        summarize_at(c("value"),funs(mean))
      tbl_rpAgg<-bind_rows(tbl_rpAggsums,tbl_rpAggmeans)%>%ungroup()


      # Bar Chart
      srn.printPdfPng(
        srn.chart(tbl_rpAgg, chartType = "bar", facet_columns="none",
                  class ="scenario", position ="dodge", classPalette = "pal_Basic"),
        dir = paste(dirOutputs, "/", region_i,"/regional/compareScen",sep = ""),
        filename = paste("figBarDodged_",region_i,"_compareScen_",param_i,sep="")
      )

      # Line Chart
      srn.printPdfPng(
        srn.chart(tbl_rpAgg, chartType = "line", facet_columns="none",
                  class ="scenario", classPalette = "pal_Basic"),
        dir = paste(dirOutputs, "/", region_i,"/regional/compareScen",sep = ""),
        filename = paste("figLineOverlap_",region_i,"_compareScen_",param_i,sep="")
      )

#-------------------------
# Creating Diff Plots
#------------------------

      if(is.null(scenRef)){scenRef = unique(tbl_rp$scenario)[1]} # Check if Ref Scenario Chosen

      # Calculate Diff Values
      tbl_rpd<-tbl_rp%>%
        filter(scenario==scenRef)%>%
        dplyr::select(-origScen,-origQuery,-origValue,-origUnits,-origX,-sources)

      for (scenario_i in unique(tbl_rp$scenario)[unique(tbl_rp$scenario)!=scenRef]){
        tbl_temp <- tbl_rp%>%
          dplyr::filter(scenario %in% c(scenRef,scenario_i))%>%
          dplyr::select(-origScen,-origQuery,-origValue,-origUnits,-origX,-sources)%>%
          tidyr::spread(scenario,value)%>%
          mutate(!!paste(scenario_i,"_diff",sep=""):=get(scenario_i)-get(scenRef))%>%
          dplyr::select(-scenario_i,-scenRef)%>%
          dplyr::rename(!!paste(scenario_i):=!!paste(scenario_i,"_diff",sep=""))%>%
          tidyr::gather(key=scenario,value=value,
                        -region,-param,-units,-vintage,-x,-xLabel,-aggregate,
                        -class1,-classLabel1,-classPalette1,-class2,-classLabel2,
                        -classPalette2)
        tbl_rpd<-bind_rows(tbl_rpd,tbl_temp)
      }


      # Bar Chart
      srn.printPdfPng(
        srn.chart(tbl_rpd, chartType = "bar"),
        dir = paste(dirOutputs, "/", region_i,"/regional/compareScen",sep = ""),
        filename = paste("figBarDiff_",region_i,"_compareScen_",param_i,sep=""),
        figWidth = 13*length(unique(tbl_rpd$scenario))/2
      )

      # Line Chart
      srn.printPdfPng(
        srn.chart(tbl_rpd,chartType = "line"),
        dir = paste(dirOutputs, "/", region_i,"/regional/compareScen",sep = ""),
        filename = paste("figLineDiff_",region_i,"_compareScen_",param_i,sep=""),
        figWidth = 13*length(unique(tbl_rpd$scenario))/2
      )


    } # close loop for param
  } # close loop for region
} # Close if multiple scenarios available

return(tbl)

} # Close Function
