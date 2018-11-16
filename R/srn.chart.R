#' srn.chart
#'
#' This function produce different kinds of charts for the srn package.
#' iIt requires a table in the SRN format. Each figure is accompanied with a csv table.
#'
#' @param data data table for charting
#' @param chartType Type of chart: "bar" or "line"
#' @param position Position in bar charts. "identity", "stack" or "dodge"
#' @param xData Default "x"
#' @param yData Default "value"
#' @param class Default "class1"
#' @param group Default "scenario"
#' @param classPalette Default "classPalette1"
#' @param classLabel Default "classLabel1"
#' @param xLabel Default "xLabel"
#' @param yLabel Default "units"
#' @param facet_rows Default "region"
#' @param facet_columns Default "scenario"
#' @param scales Default "fixed"
#' @param useNewLabels Default 0
#' @param units Default "units"
#' @param xBreaksMaj Default 10
#' @param xBreaksMin Default 5
#' @param yBreaksMajn Default 5
#' @param yBreaksMinn Default 10
#' @param sizeBarLines Default 0.5
#' @param sizeLines Default 1.5
#' @param ncolrow Number of columns or Rows for Faceted plots
#' @keywords charts, diffplots
#' @return Returns the formatted data used to produce chart
#' @export
#' @examples
#' # Examples below show the default chart with minimum information
#' # and then adding progressively more details.
#'
#' library(tibble)
#' tbl <- tribble (
#' ~x,     ~value,
#' 2010,   15,
#' 2020,   20,
#' 2030,   30
#' )
#' srn.chart(data=tbl,xData="x",yData="value",chartType = "line")
#' srn.chart(data=tbl,xData="x",yData="value",chartType = "bar")
#' @import tools ggplot2 scales dplyr tibble


srn.chart<-function(data,
                         chartType="bar",position="stack",
                         xData="x",yData="value",class="class1",group="scenario",
                         classPalette="classPalette1",classLabel="classLabel1",
                         xLabel="xLabel",yLabel="yLabel",
                         facet_rows="region",facet_columns="scenario",ncolrow=4,
                         scales="fixed",
                         useNewLabels=0,units="units",
                         xBreaksMaj=10, xBreaksMin=5,
                         yBreaksMajn=5, yBreaksMinn=10,
                         sizeBarLines=0.5,sizeLines=1.5){

#----------------
# Load Libraries
#---------------

  requireNamespace("tools",quietly = T)
  requireNamespace("ggplot2",quietly = T)
  requireNamespace("scales",quietly = T)
  requireNamespace("dplyr",quietly = T)
  requireNamespace("tibble",quietly = T)

#------------------
# Initialize variables to remove binding errors if needed
# -----------------

#------------------------------------------
# Format data to include any missing columns
#------------------------------------------

# At the very least data, x and y are needed.
if(length(names(data))<2){stop("Need to provide a data object with at least x and y.
                               srn.chart(data = userData,xData ='x',yData ='y'")}

if(!"units"%in%names(data)){data<-data%>%mutate(units="units")}
if(!"classPalette1"%in%names(data)){data<-data%>%mutate(classPalette1="pal_16")}
if(!"class1"%in%names(data)){data<-data%>%mutate(class1="class1")}
if(!"scenario"%in%names(data)){data<-data%>%mutate(scenario="scenario")}

  l1 <- data
  l1<-l1%>%mutate(units=gsub(" ","~",units))
  if(length(classPalette)>1){
    paletteX<-classPalette}else{
  if(classPalette %in% names(l1)){
  paletteX<-srn.colors()[[unique(l1[[classPalette]])]]}else{
    if(classPalette %in% names(srn.colors())){
    paletteX<-srn.colors()[[classPalette]]} else {
      paletteX<-classPalette
    }
  }
  }

  if(useNewLabels==1){
    if(!is.null(names(paletteX))){
      names(paletteX)<-toTitleCase(sub("\\b[a-zA-Z0-9]{1} \\b", "",names(paletteX)))}
    l1[[class]]<-toTitleCase(sub("\\b[a-zA-Z0-9]{1} \\b", "",l1[[class]]))
  }

  p <- ggplot(l1,aes(x=get(xData),y=get(yData),group=get(group))) +
       srn.chartsThemeLight()

  # Chart Type
  if(chartType=="bar"){
  p <- p + geom_bar(aes(fill=get(class)),size=sizeBarLines,color="black", stat="identity",position=position) +
           scale_fill_manual(values=paletteX) + guides(color=F)
  if(!grepl("class",class)){
    p = p + guides(fill = guide_legend(title=toTitleCase(paste(class,sep=""))))}else{
      if(length(unique(l1[[class]]))<2){
        p = p + theme(legend.position="none")
      }else{
        p = p + guides(fill = guide_legend(title=unique(l1[[classLabel]])))
      }
    }
  }

  if(chartType=="line"){
  p <- p +  geom_line(aes(color=get(class),group=get(class)),size=sizeLines, stat="identity",position="identity") +
            scale_color_manual(values=paletteX)
  if(!grepl("class",class)){
    p = p + guides(color = guide_legend(title=toTitleCase(paste(class,sep=""))))}else{
      if(length(unique(l1[[class]]))<2){
        p = p + theme(legend.position="none")
      }else{
    p = p + guides(color = guide_legend(title=unique(l1[[classLabel]])))
    }
    }
  }

if(!xLabel%in%names(l1)){
  if(xLabel!="xLabel"){p<-p+xlab(xLabel)}else{
    p<-p+xlab(NULL)}}else{
      if(xLabel!="xLabel"){p<-p+xlab(unique(l1[[xLabel]]))}else{
        p<-p+xlab(NULL)}}
if(!yLabel%in%names(l1)){p<-p+ylab(yLabel)}else{p<-p+ylab(eval(parse(text=paste(unique(l1[[yLabel]]),collapse="~"))))}
  p <- p + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
       scale_y_continuous(breaks = pretty_breaks(n = yBreaksMajn), minor_breaks = waiver())

if(is.numeric(l1[[xData]])){p<- p + scale_x_continuous (breaks=(seq(min(range(l1[[xData]])),max(range(l1[[xData]])),by=xBreaksMaj)),
                               minor_breaks=(seq(min(range(l1[[xData]])),max(range(l1[[xData]])),by=xBreaksMin)),
                               expand=c(0,xBreaksMaj/2))}

  # Faceting
  if(length(unique(l1[[facet_columns]])) > 1 & length(unique(l1[[facet_rows]])) > 1){
    p <- p + facet_grid(get(facet_rows)~get(facet_columns),scales=scales)
  }else{

    if(length(unique(l1[[facet_columns]])) > 1 & length(unique(l1[[facet_rows]])) < 2){
      p <- p + facet_wrap(facet_columns,ncol=ncolrow,scales = scales)
    }

    if(length(unique(l1[[facet_columns]])) < 2 & length(unique(l1[[facet_rows]])) > 1){
        p <- p + facet_wrap(facet_rows,nrow=ncolrow,scales = scales)
      }
    }

  return(p)
}


