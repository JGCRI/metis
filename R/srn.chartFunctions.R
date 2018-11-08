#' srn.chartFunctions
#'
#' This file contains several functions used to produce different kinds of charts
#' in the srn package. Each figure is accompanied with a csv table.
#'
#' List of Functions in this script:
#' \itemize{
#' \item scenario: The name of the new data scenario
#' }
#' @param srnFormattedTable
#' @param x Default "x"
#' @param y Default "value"
#' @param class Default "class1"
#' @param group Default "scenario"
#' @param classPalette Default "classPalette1"
#' @param classLabel Default "classLabel1"
#' @param xLabel Default "xLabel"
#' @param facet_rows Default "region"
#' @param facet_columns Default "scenario"
#' @param ncol Default 4
#' @param scales Default "fixed"
#' @param useNewLabels Default 1
#' @param units Default "units"
#' @param xBreaksMaj Default 10
#' @param  xBreaksMin Default 5
#' @param yBreaksMajn Default 5
#' @param  yBreaksMinn Default 10
#' @keywords charts, diffplots
#' @return Returns the formatted data used to produce chart
#' @export
#' @examples
#' @import tools ggplot2 scales
#' library(srn)


#----------------
# Bar plots
#---------------

srn.chart<-function(srnFormattedTable,
                         chartType="bar",position="stack",
                         x="x",y="value",class="class1",group="scenario",
                         classPalette="classPalette1",classLabel="classLabel1",
                         xLabel="xLabel",
                         facet_rows="region",facet_columns="scenario",ncolrow=4,scales="fixed",
                         useNewLabels=1,units="units",
                         xBreaksMaj=10, xBreaksMin=5,
                         yBreaksMajn=5, yBreaksMinn=10,
                         sizeBarLines=0.5,sizeLines=1.5){

  l1 <- srnFormattedTable
  l1[[units]] = gsub(" ","~",l1[[units]])
  paletteX<-srn.colors()[[unique(l1[[classPalette]])]];
  if(useNewLabels==1){
    if(!is.null(names(paletteX))){
      names(paletteX)<-toTitleCase(sub("\\b[a-zA-Z0-9]{1} \\b", "",names(paletteX)))}
    l1[[class]]<-toTitleCase(sub("\\b[a-zA-Z0-9]{1} \\b", "",l1[[class]]))
  }

  p <- ggplot(l1,aes(x=x,y=value,group=get(group))) +
       srn.chartsThemeLight()

  # Chart Type
  if(chartType=="bar"){
  p <- p + geom_bar(aes(fill=get(class)),size=sizeBarLines,color="black", stat="identity",position=position) +
           scale_fill_manual(values=paletteX) + guides(color=F)
  if(!grepl("class",class)){
    p = p + guides(fill = guide_legend(title=toTitleCase(paste(class,sep=""))))}else{
      if(length(unique(l1[[classLabel]]))<2){
        p = p + theme(legend.position="none")
      }else{
        p = p + guides(fill = guide_legend(title=unique(l1[[classLabel]])))
      }
    }
  }

  if(chartType=="line"){
  p <- p +  geom_line(aes(color=get(class)),size=sizeLines, stat="identity",position="identity") +
            scale_color_manual(values=paletteX)
  if(!grepl("class",class)){
    p = p + guides(color = guide_legend(title=toTitleCase(paste(class,sep=""))))}else{
      if(length(unique(l1[[classLabel]]))<2){
        p = p + theme(legend.position="none")
      }else{
    p = p + guides(color = guide_legend(title=unique(l1[[classLabel]])))
    }
    }
  }

  p <- p +
       xlab(unique(l1[[xLabel]])) + ylab(eval(parse(text=paste(unique(l1[[units]]),collapse="~")))) +
       theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
       scale_x_continuous (breaks=(seq(min(range(l1[[x]])),max(range(l1[[x]])),by=xBreaksMaj)),
                               minor_breaks=(seq(min(range(l1[[x]])),max(range(l1[[x]])),by=xBreaksMin)),
                               expand=c(0,xBreaksMaj/2)) +
       scale_y_continuous(breaks = pretty_breaks(n = yBreaksMajn), minor_breaks = waiver())

  # Faceting
  if(length(unique(l1[[facet_columns]])) > 1 & length(unique(l1[[facet_rows]])) > 1){
    p <- p + facet_grid(get(facet_rows)~get(facet_columns),scales=scales)
  }else{

    if(length(unique(l1[[facet_columns]])) > 1 & length(unique(l1[[facet_rows]])) == 0){
      p <- p + facet_wrap(get(facet_columns)~.,ncol=ncolrow,scales = scales)
    }

    if(length(unique(l1[[facet_columns]])) == 0 & length(unique(l1[[facet_rows]])) > 1){
        p <- p + facet_wrap(get(facet_rows)~.,nrow=ncolrow,scales = scales)
      }
    }

  return(p)
}


