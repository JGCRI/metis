#' metis.chart
#'
#' This function produce different kinds of charts for the metis package.
#' iIt requires a table in the Metis format. Each figure is accompanied with a csv table.
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
#' @param facetBGColor Default ="grey30",
#' @param facetLabelColor Default= "white",
#' @param facetLabelSize Default =1.5,
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
#' @param printFig Default = T,
#' @param fileName Default = "map",
#' @param dirOutputs Default = paste(getwd(),"/outputs",sep Default = "")
#' @param figWidth Default = 9,
#' @param figHeight Default = 7,
#' @param pdfpng Default = "png",
#' @param labelTextSize = 4,
#' @param sectorToOrder Default = NULL,
#' @param sectorFromOrder Default = NULL,
#' @param removeCols Default = NULL,
#' @param bubbleSize Default = 10,
#' @param sankeyAxis1 Default = NULL,
#' @param sankeyAxis2 Default = NULL,
#' @param sankeyAxis1Label Default = "axis1Label",
#' @param sankeyAxis2Label Default = "axis2Label",
#' @param dataNorm Default = NULL,
#' @param sankeyGroupColor Default = NULL,
#' @param sankeyLabelsOn Default =1
#' @param colOrder1 Default = NULL,
#' @param colOrderName1 Default = NULL,
#' @param colOrder2 Default = NULL,
#' @param colOrderName2 Default = NULL,
#' @keywords charts, diffplots
#' @return Returns the formatted data used to produce chart
#' @import ggplot2
#' @export
#' @examples
#' # Examples below show the default chart with minimum information
#' # and then adding progressively more details.
#'
#' library(tibble)
#' library(dplyr)
#' library(ggplot2)
#' tbl <- tribble (
#' ~x,     ~value,
#' 2010,   15,
#' 2020,   20,
#' 2030,   30
#' )
#' metis.chart(data=tbl,xData="x",yData="value",chartType = "line")
#' metis.chart(data=tbl,xData="x",yData="value",chartType = "bar")


metis.chart<-function(data,
                      dataNorm=NULL,
                         chartType="bar",position="stack",
                         xData="x",yData="value",class="class1",group="scenario",
                         classPalette="classPalette1",classLabel="classLabel1",
                         xLabel="xLabel",yLabel="yLabel",
                         facet_rows="region",facet_columns="scenario",ncolrow=4,
                         facetBGColor="grey30",
                         facetLabelColor = "white",
                         facetLabelSize=1.5,
                         scales="fixed",
                         useNewLabels=0,units="units",
                         xBreaksMaj=10, xBreaksMin=5,
                         yBreaksMajn=5, yBreaksMinn=10,
                         sizeBarLines=0.5,sizeLines=1.5,
                         labelTextSize = 4,
                         sectorToOrder=NULL,
                         sectorFromOrder=NULL,
                         removeCols=NULL,
                         bubbleSize=10,
                         sankeyAxis1=NULL,
                         sankeyAxis2=NULL,
                         sankeyAxis1Label="axis1Label",
                         sankeyAxis2Label="axis2Label",
                         sankeyGroupColor=NULL,
                         printFig = T,
                         fileName = "chart",
                         dirOutputs=paste(getwd(),"/outputs",sep=""),
                         figWidth=13,
                         figHeight=9,
                         pdfpng="png",
                         sankeyLabelsOn=1,
                         colOrder1 = NULL,
                         colOrderName1 = NULL,
                         colOrder2 = NULL,
                         colOrderName2 = NULL)
                        {

  # dataNorm=NULL
  # chartType="bar"
  # position="stack"
  # xData="x"
  # yData="value"
  # class="class1"
  # group="scenario"
  # classPalette="classPalette1"
  # classLabel="classLabel1"
  # xLabel="xLabel"
  # yLabel="yLabel"
  # facet_rows="region"
  # facet_columns="scenario"
  # facetBGColor="grey30"
  # facetLabelColor = "white"
  # facetLabelSize=1.5
  # ncolrow=4
  # scales="fixed"
  # useNewLabels=0
  # units="units"
  # xBreaksMaj=10
  # xBreaksMin=5
  # yBreaksMajn=5
  # yBreaksMinn=10
  # sizeBarLines=0.5
  # sizeLines=1.5
  # printFig = T
  # fileName = "chart"
  # dirOutputs=paste(getwd(),"/outputs",sep="")
  # figWidth=13
  # figHeight=9
  # pdfpng="png"
  # labelTextSize = 4
  # sectorToOrder=NULL
  # sectorFromOrder=NULL
  # removeCols=NULL
  # bubbleSize=10
  # sankeyAxis1=NULL
  # sankeyAxis2=NULL
  # sankeyAxis1Label="axis1Label"
  # sankeyAxis2Label="axis2Label"
  # colOrder1 = NULL
  # colOrderName1 = NULL
  # colOrder2 = NULL
  # colOrderName2 = NULL

#------------------
# Initialize variables to remove binding errors if needed
# -----------------

NULL -> value -> tempName -> sankeyHjustCheck

#------------------------------------------
# Format data to include any missing columns
#------------------------------------------

# At the very least data, x and y are needed.
if(length(names(data))<2){stop("Need to provide a data object with at least x and y.
                               metis.chart(data = userData,xData ='x',yData ='y'")}

if(!"units"%in%names(data)){data<-data%>%dplyr::mutate(units="units")}
if(!"classPalette1"%in%names(data)){data<-data%>%dplyr::mutate(classPalette1="pal_16")}
if(!"class1"%in%names(data)){data<-data%>%dplyr::mutate(class1="class1")}
if(!"scenario"%in%names(data)){data<-data%>%dplyr::mutate(scenario="scenario")}

  l1 <- data
  l1<-l1%>%dplyr::mutate(units=gsub(" ","~",units))
  if(length(classPalette)>1){
    paletteX<-classPalette}else{
  if(classPalette %in% names(l1)){
  paletteX<-metis.colors()[[unique(l1[[classPalette]])]]}else{
    if(classPalette %in% names(metis.colors())){
    paletteX<-metis.colors()[[classPalette]]

    if(chartType=="sankey"){

      paletteX_df <- paletteX %>% as.data.frame() %>%
        dplyr::rename(value=".") %>%
        dplyr::mutate(tempName = as.character(names(paletteX)),
                      value=as.character(value)); paletteX_df %>% tibble::as_tibble()

     paletteDF_temp <- data.frame(tempName=unique(l1[[sankeyGroupColor]])) %>%
       dplyr::mutate(tempName = as.character(tempName)) %>%
         dplyr::left_join(paletteX_df); paletteDF_temp

     paletteX <- as.vector(c(paletteDF_temp$value,metis.colors()$pal_16))
     names(paletteX) <-c(paletteDF_temp$tempName);paletteX

    }

    } else {
      paletteX<-classPalette
    }
  }
  }

  l1[[class]]<-factor(l1[[class]],levels=unique(l1[[class]]))

  if(!is.null(names(paletteX))){
    if(!all(levels(l1[[class]]) %in% names(paletteX))){
  add_colors<-(metis.colors()$pal_Basic)[1:length(levels(l1[[class]])[!levels(l1[[class]]) %in% names(paletteX)])]
  names(add_colors)<-levels(l1[[class]])[!levels(l1[[class]]) %in% names(paletteX)]
  paletteX<-c(paletteX,add_colors)}}

  if(useNewLabels==1){
    if(!is.null(names(paletteX))){
      names(paletteX)<-tools::toTitleCase(sub("\\b[a-zA-Z0-9]{1} \\b", "",names(paletteX)))
      paletteX <- paletteX[rev(order(names(paletteX)))]}
    l1[[class]]<-tools::toTitleCase(sub("\\b[a-zA-Z0-9]{1} \\b", "",l1[[class]]))
    l1<-l1%>%dplyr::arrange(!! rlang::sym(class))
  }

  # Set column order
  if(!is.null(colOrder1) & !is.null(colOrderName1)){
    if(!colOrderName1 %in% names(l1)){print("colOrderName1 provided not in dataframe names. Ignoring.")}
    colOrderSet <- c(colOrder1[ colOrder1 %in% unique(as.character(l1[[colOrderName1]]))],
                     unique(as.character(l1[[colOrderName1]]))[!unique(as.character(l1[[colOrderName1]])) %in% colOrder1]); colOrderSet
    if(length(colOrderSet)>0){
      l1 <- l1 %>% dplyr::mutate(!!as.name(colOrderName1) := factor(!!as.name(colOrderName1), levels = colOrderSet))
    }
  }

  if(!is.null(colOrder2) & !is.null(colOrderName2)){
    if(!colOrderName2 %in% names(l1)){print("colOrderName2 provided not in dataframe names. Ignoring.")}
    colOrderSet <- c(colOrder2[ colOrder2 %in% unique(as.character(l1[[colOrderName2]]))],
                     unique(as.character(l1[[colOrderName2]]))[!unique(as.character(l1[[colOrderName2]])) %in% colOrder2]); colOrderSet
    if(length(colOrderSet)>0){
      l1 <- l1 %>% dplyr::mutate(!!as.name(colOrderName2) := factor(!!as.name(colOrderName2), levels = colOrderSet))
    }
  }




  p <- ggplot(l1,aes(x=get(xData),y=get(yData),group=get(group))) +
      theme_bw() +
      theme(
        text =                element_text(family = NULL, face = "plain",colour = "black", size = 24,
                                           hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9)
        , axis.text.x =       element_text(size=24)
        , axis.text.y =       element_text(size=24)
        ,axis.title.x =       element_text(vjust = -1, margin=margin(t=1,unit="line"))
        ,axis.title.y =       element_text(angle = 90, vjust = 2, margin=margin(r=1,unit="line"))
        ,legend.key =         element_blank()
        ,legend.key.size =    unit(1.5, 'lines')
        ,legend.text =        element_text(size = rel(1.0), colour = "black")
        ,legend.title =       element_text(size = rel(1.2), face = NULL, hjust = 0, colour = "black")
        #,strip.background =   element_rect(fill = NA, colour = "black")
        ,plot.margin =        unit(c(1, 1, 1, 1), "lines")
        ,plot.title=          element_text(face="bold", hjust=0,size=20,margin = margin(b=20))
      )

  if(chartType=="sankey"){
  p <- ggplot(l1, aes(y = get(yData), axis1 = get(sankeyAxis1), axis2 = get(sankeyAxis2), group=get(facet_columns)))
  }

  # Chart Type
  if(chartType=="sankey"){

    # Calculate number of facets. If both are one then
    nFacets = ifelse(length(unique(l1[[facet_columns]]))>0 &
                       length(unique(l1[[facet_rows]]))>0,
                     length(unique(l1[[facet_columns]])) + length(unique(l1[[facet_rows]]))-1,
                     length(unique(l1[[facet_columns]])) + length(unique(l1[[facet_rows]]))); nFacets


    # https://stackoverflow.com/questions/56113973/variable-align-ggrepel-text-labels-in-faceted-alluvial-plot


    p <- p + theme_bw() +
      ggalluvial::geom_alluvium(aes(fill = get(class)), width = 1/12, color="grey10", alpha=0.7, na.rm=F) +
      ggalluvial::geom_stratum(width = 1/12, fill = "grey70", color = "grey10", alpha=1, na.rm=F) +
      scale_x_discrete(limits = c(sankeyAxis1Label, sankeyAxis2Label), expand = c(1,0.1),drop=F) +
      ggrepel::geom_text_repel(stat = "stratum", label.strata = TRUE, direction="y", size=4,segment.color = 'grey50') +
      scale_fill_manual(values=paletteX, name="From",na.translate=F, drop=F) +
      coord_cartesian(clip = "off")+
      theme(aspect.ratio = 0.5) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size = 15),
            strip.text  = element_text(size = 18),
            #strip.background = element_blank(),
            axis.title.x = element_text(size = 15),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            panel.border = element_blank()
            )+
      ggtitle(fileName);



    if(!grepl("class",class)){
      p = p + guides(fill = guide_legend(title=tools::toTitleCase(paste(class,sep="")),reverse = T))}else{
        if(length(unique(l1[[class]]))<2){
          p = p + theme(legend.position="none") + scale_fill_manual(values="firebrick")
        }else{
          p = p + guides(fill = guide_legend(title=unique(l1[[classLabel]]),reverse = T))
        }
      }

  }

  # Chart Type
  if(chartType=="bubble"){

    if(is.null(dataNorm)){dataNorm=l1}

    p <- p + theme_bw() +
      labs(title=fileName) +
      geom_point(data=dataNorm%>%dplyr::filter(!(!!as.name(xData) %in% removeCols)),aes(col=value, size=value)) +
      scale_color_gradient(low = "white", high = "indianred1", guide="none") +
      geom_text(aes(label=round(value,2)),col="black", size=3) +
      coord_fixed(ratio = 1) +
      scale_x_discrete(limits = sectorToOrder, expand = c(0.1,0.1)) +
      scale_y_discrete(limits = rev(sectorFromOrder), expand = c(0.1,0.1)) +
      scale_size_continuous(range = c(1,bubbleSize), guide="none") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 15),
            axis.text.y = element_text(size = 15),
            strip.text  = element_text(size = 15),
            axis.title = element_text(size = 15)) +
      theme(aspect.ratio = ifelse(length(unique(l1[[yData]]))/length(unique(l1[[xData]])) > 1, 1,
            ifelse(length(unique(l1[[yData]]))/length(unique(l1[[xData]])) < 0.05, 0.05, length(unique(l1[[yData]]))/length(unique(l1[[xData]])))))

    if(!grepl("class",class)){
      p = p + guides(fill = guide_legend(title=tools::toTitleCase(paste(class,sep="")),reverse = T))}else{
        if(length(unique(l1[[class]]))<2){
          p = p + theme(legend.position="none") + scale_fill_manual(values="firebrick")
        }else{
          p = p + guides(fill = guide_legend(title=unique(l1[[classLabel]]),reverse = T))
        }
      }

  }



  if(chartType=="bar"){
  p <- p + geom_bar(aes(fill=get(class)),size=sizeBarLines,color="black", stat="identity",position=position) +
           scale_fill_manual(values=paletteX) + guides(color=F)
  if(!grepl("class",class)){
    p = p + guides(fill = guide_legend(title=tools::toTitleCase(paste(class,sep="")),reverse = T))}else{
      if(length(unique(l1[[class]]))<2){
        p = p + theme(legend.position="none") + scale_fill_manual(values="firebrick")
      }else{
        p = p + guides(fill = guide_legend(title=unique(l1[[classLabel]]),reverse = T))
      }
    }
  }

  if(chartType=="line"){
  p <- p +  geom_line(aes(color=get(class),group=get(class)),size=sizeLines, stat="identity",position="identity") +
            scale_color_manual(values=paletteX) +
            geom_point(aes(shape = get(class), color=get(class)),size = 4 )
  if(!grepl("class",class)){
    p = p + guides(color = guide_legend(title=tools::toTitleCase(paste(class,sep=""))),
                   shape = guide_legend(title=tools::toTitleCase(paste(class,sep=""))))}else{
      if(length(unique(l1[[class]]))<2){
        p = p + theme(legend.position="none") + scale_color_manual(values="firebrick")
      }else{
    p = p + guides(color = guide_legend(title=unique(l1[[classLabel]])),
                   shape = guide_legend(title=unique(l1[[classLabel]])))
    }
    }
  }


if(!xLabel%in%names(l1)){
  if(xLabel!="xLabel"){p<-p+xlab(xLabel)}else{
    p<-p+xlab(NULL)}}else{
      if(xLabel!="xLabel"){p<-p+xlab(unique(l1[[xLabel]]))}else{
        p<-p+xlab(NULL)}}
if(!yLabel%in%names(l1)){p<-p+ylab(yLabel)}else{p<-p+ylab(eval(parse(text=paste(unique(l1[[yLabel]]),collapse="~"))))}

  if(!chartType %in% c("bubble","sankey")){
  p <- p + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
       scale_y_continuous(breaks = scales::pretty_breaks(n = yBreaksMajn), minor_breaks = waiver())}

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


  # General Themes
  p <- p + theme(strip.background = element_rect(fill = facetBGColor, colour = 'black'),
                 strip.text = element_text(colour = facetLabelColor))

  if(printFig!=F){
    fname<-paste(fileName,sep="")
    if(!dir.exists(dirOutputs)){
      print(paste("dirOutputs provided: ",dirOutputs," does not exist. Saving to: ", paste(getwd(),"/outputsTemp",sep=""),sep=""))
      if (!dir.exists(paste(getwd(),"/outputsTemp",sep="")))
      {dir.create(paste(getwd(),"/outputsTemp",sep=""))}
      dirOutputs=paste(getwd(),"/outputsTemp",sep="")
      }


        if(chartType=="sankey"){

          if(sankeyLabelsOn==1){
          # This is the plot under the hood. Find the element with ggrepel
          gg_guts <- ggplot_build(p); gg_guts$data
          # The geom_text_repel layer is the 3rd one
          # Adjust the hjust param for the plot by axis
          if(any(grepl("hjust",names(gg_guts$data[[3]])))){

          # labelsFrom <- (gg_guts$data[[3]] %>% dplyr::filter(x==min(gg_guts$data[[3]]$x)))$label; labelsFrom
          # labelsFrom <- gsub("\\s", " ", format(labelsFrom, width=max(nchar(labelsFrom)), justify="right")); labelsFrom; nchar(labelsFrom)
          # labelsTo <- (gg_guts$data[[3]] %>% dplyr::filter(x==max(gg_guts$data[[3]]$x)))$label; labelsTo
          # labelsTo <- gsub("\\s", " ", format(labelsTo, width=1*max(nchar(labelsTo)), justify="left")); labelsTo; nchar(labelsTo)
          # labelsNew <- c(labelsFrom,labelsTo); labelsNew; nchar(labelsNew)
          # gg_guts$data[[3]]$label <- labelsNew

          gg_guts$data[[3]]$label <- gsub("\\s", " ", format(gg_guts$data[[3]]$label, width=max(nchar(gg_guts$data[[3]]$label)), justify="left"));

          gg_guts$data[[3]] <-
            gg_guts$data[[3]] %>%
            dplyr::mutate(hjust = dplyr::case_when(x == min(gg_guts$data[[3]]$x) ~ 1.3,
                                     x == max(gg_guts$data[[3]]$x) ~ -0.3,
                                     TRUE ~ 0)); gg_guts$data[[3]]; nchar(gg_guts$data[[3]]$label)
          # once you've made your adjustments, you can plot it again
          if(pdfpng=='pdf'){grDevices::pdf(paste(dirOutputs,"/",fname,".pdf",sep=""),width=figWidth,height=figHeight)
            grid::grid.newpage();grid::grid.draw(ggplot_gtable(gg_guts))
            grDevices::dev.off()}
          if(pdfpng=='png'){grDevices::png(paste(dirOutputs,"/",fname,".png",sep=""),width=figWidth,height=figHeight, units="in",res=300)
            grid::grid.newpage();grid::grid.draw(ggplot_gtable(gg_guts))
            grDevices::dev.off()}
          if(pdfpng=='both'){
            grDevices::pdf(paste(dirOutputs,"/",fname,".pdf",sep=""),width=figWidth,height=figHeight)
            grid::grid.newpage();grid::grid.draw(ggplot_gtable(gg_guts))
            grDevices::dev.off()
            grDevices::png(paste(dirOutputs,"/",fname,".png",sep=""),width=figWidth,height=figHeight, units="in",res=300)
            grid::grid.newpage();grid::grid.draw(ggplot_gtable(gg_guts))
            grDevices::dev.off()
          }

          sankeyHjustCheck <- "Adjusted"

          }
          } else { p$layers[[3]] <- NULL }
        }


        if(is.null(sankeyHjustCheck)){
        metis.printPdfPng(figure=p,
                        dir=dirOutputs,
                        filename=fname,
                        figWidth=figWidth,
                        figHeight=figHeight,
                        pdfpng=pdfpng)
          }

        print(paste("Figure saved as: ",fileName,".",pdfpng," in folder: ", paste(dirOutputs,sep=""),sep=""))
      }else{print("printFig set to F so no figure will be saved.")}


  return(p)
}


