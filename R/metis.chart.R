#' metis.chart
#'
#' This function produce different kinds of charts for the metis package.
#' iIt requires a table in the Metis format. Each figure is accompanied with a csv table.
#'
#' @param data Data table for charting
#' @param chartType Type of chart: "bar", "line", "bubble", "sankey"
#' @param position Position in bar charts. "identity", "stack" or "dodge"
#' @param xData X axis data variable (dataframe or table column name). Default "x".
#' @param yData Y axis data variable (dataframe or table column name).Default "value"
#' @param class Class data variable (dataframe or table column name).Default "class1"
#' @param group Group (dataframe or table column name).Default "scenario"
#' @param classPalette Color palette to use for multiple classes. Must be a color palette eg. c("red","blue","green") or a metis.colors() palette eg. metis.colors()$pal_Basic. Default "classPalette1"
#' @param color A single color name for single class charts. Default NULL
#' @param classLabel Label to be used for legend title. Default "classLabel1"
#' @param xLabel X axis title. Default "xLabel"
#' @param yLabel Y axis title. Default "units"
#' @param facet_rows Data variable to be used for facet rows (dataframe or table column name).Default "region"
#' @param facet_columns Data variable to be used for facet columns (dataframe or table column name).Default "scenario"
#' @param facetBGColor Facet background color. Default ="grey30",
#' @param facetBorderColor Default = "black"
#' @param facetLabelColor Facet title text color. Default= "white",
#' @param facetLabelSize Facet title text size. Default =1.5,
#' @param scales Fixed or free scales for multiple sankey plots. Default "fixed"
#' @param useNewLabels "1" or "0". Converts labels to title-case.Default 0
#' @param units Data units. Default "units"
#' @param xBreaksMaj X axis major breaks. Default 10
#' @param xBreaksMin X axis minor breaks. Default 5
#' @param yBreaksMajn Y axis major breaks. Default 5
#' @param yBreaksMinn Y axis minor breaks. Default 10
#' @param yMax Y axis max value
#' @param yMin Y axis min value
#' @param sizeBarLines Bar plot line size. Default 0.5
#' @param sizeLines Line plot line size. Default 1.5
#' @param ncolrow Number of columns or Rows for Faceted plots.
#' @param printFig Whether plot should be printed or not. Default = T,
#' @param fileName File name for plot to be saved. Default = "chart",
#' @param dirOutputs Output directory to save figure. Default = paste(getwd(),"/outputs/Charts",sep Default = "")
#' @param folderName Foldername within output directory. Default=NULL,
#' @param figWidth Figure width. Default = 9,
#' @param figHeight Figure height. Default = 7,
#' @param pdfpng Whether to save plot as pdf or png. Choice between "pdf" or "png". Default = "png",
#' @param sectorToOrder Order of "to" column variables in bubble plots. Default = NULL,
#' @param sectorFromOrder Order of "from" column variables in bubble plots. Default = NULL,
#' @param removeCols Option to remove certain columns from bubble plots. Default = NULL,
#' @param bubbleSize Bubble plot bubble size. Default = 10,
#' @param dataNorm Normalized data to plot under actual data in bubble plots. Default = NULL,
#' @param sankeyAxis1 Sankey axis 1 data variable (dataframe or table column name). Default = NULL,
#' @param sankeyAxis2 Sankey axis 2 data variable (dataframe or table column name).Default = NULL,
#' @param sankeyAxis1Label Sankey axis 1 title data variable (dataframe or table column name).Default = "axis1Label",
#' @param sankeyAxis2Label Sankey axis 2 title variable (dataframe or table column name).Default = "axis2Label",
#' @param sankeyGroupColor Which axis variables will be used to color flow paths (One of the sankey axis). Default = NULL,
#' @param sankeyLabelsOn Turn on labels for sankey stratum categories. "1" or "0". Default =1
#' @param colOrder1 Order for sankey column 1. Default = NULL,
#' @param colOrderName1 Column name with sankey variables for column order 1. Default = NULL,
#' @param colOrder2 Order for sankey column 1. Default = NULL,
#' @param colOrderName2 Column name with sankey variables for column order 1. Default = NULL,
#' @param title Figure title. Default = NULL
#' @param pointsOn Include points on lines. Default = 1
#' @param pointsSize ISize of points on line. Default = 4
#' @param paletteRev Default =T
#' @param forceFacets Default =F. When you have one facet only and want to show that.
#' @param legendPosition Default ="right"
#' @param theme_custom Default = NULL, "theme_gray","theme_bw","theme_linedraw","theme_light", "theme_minimal","theme_classic","theme_void","theme_dark"
#' @param panelBGcolor Default = "white", Can also be "transparent"
#' @param plotBGcolor Default = "transparent", Can also be "transparent"
#' @param legendBGcolor Default = "white", Can also be "transparent"
#' @param xOrder Default = NULL,
#' @keywords charts, diffplots, bubble, sankey.
#' @return Returns the formatted data used to produce chart
#' @import ggplot2
#' @export

metis.chart<-function(data,
                      dataNorm=NULL,
                         chartType="bar",position="stack",
                         xData="x",yData="value",class="class1",group="scenario",
                         classPalette="classPalette1",classLabel="classLabel1",
                         color = NULL,
                         xLabel="xLabel",yLabel="yLabel",
                         facet_rows=NULL,facet_columns=NULL,ncolrow=6,
                         facetBGColor="grey30",
                         facetBorderColor="black",
                         facetLabelColor = "white",
                         facetLabelSize=24,
                         scales="fixed",
                         useNewLabels=0,units="units",
                         xBreaksMaj=10, xBreaksMin=5,
                         yBreaksMajn=5, yBreaksMinn=10,
                         sizeBarLines=0.5,sizeLines=1.5,
                         yMax=NULL,
                         yMin=NULL,
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
                         title = NULL,
                         dirOutputs=NULL,
                         folderName=NULL,
                         figWidth=13,
                         figHeight=9,
                         pdfpng="png",
                         sankeyLabelsOn=1,
                         xOrder = NULL,
                         colOrder1 = NULL,
                         colOrderName1 = NULL,
                         colOrder2 = NULL,
                         colOrderName2 = NULL,
                         pointsOn = 1,
                         pointsSize = 4,
                         paletteRev=T,
                         forceFacets=F,
                         legendPosition="right",
                         theme_custom = NULL,
                         panelBGcolor = NULL,
                         plotBGcolor = "transparent",
                         legendBGcolor = NULL)
                        {

  # color=NULL
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
  # yMax=NULL
  # yMin=NULL
  # facet_rows="NULL"
  # facet_columns="NULL"
  # facetBGColor="grey30"
  # facetLabelColor = "white"
  # facetLabelSize=1.5
  # ncolrow=6
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
  # title = NULL
  # dirOutputs=NULL
  # folderName=NULL
  # figWidth=13
  # figHeight=9
  # pdfpng="png"
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
  # title = NULL
  # sankeyLabelsOn=1
  # pointsOn = 1
  # pointsSize = 4
  # paletteRev=T
  # forceFacets=F
  # legendPosition="right"
  # theme_custom = NULL
  # panelBGcolor = NULL
  # plotBGcolor = "transparent"
  # legendBGcolor = NULL
  # xOrder = NULL

#------------------
# Initialize variables to remove binding errors if needed
# -----------------

NULL -> value -> tempName -> sankeyHjustCheck -> value1 -> levelsOn -> theme_customX
requireNamespace("ggalluvial", quietly = TRUE)
StatStratum <- ggalluvial::StatStratum # This is done so that ggplot2 recognizes stat stratum


#------------------
# Create Directories
# -----------------

if(printFig!=F){
if(!is.null(dirOutputs)){
  if(grepl("/",dirOutputs)){dirOutputs = dirOutputs}else{
  dirOutputs = paste(getwd(),"/",gsub(paste(getwd(),"/",sep=""),"",dirOutputs),sep="")}
  if (!dir.exists(paste(dirOutputs,sep=""))){dir.create(paste(dirOutputs,sep=""))}
  if(!is.null(folderName)){
    if (!dir.exists(paste(dirOutputs,"/",folderName,sep=""))){dir.create(paste(dirOutputs,"/",folderName,sep=""))}
    if(dirOutputs==paste(dirOutputs,sep="")){dirOutputs=paste(dirOutputs,"/",folderName,sep="")}
  }
}else{
  if(!is.null(folderName)){
    dirOutputs=paste(getwd(),"/",folderName,sep="")
    }else{
      dirOutputs=paste(getwd(),sep="")
      }
if(!is.null(folderName)){
  if (!dir.exists(paste(getwd(),"/",folderName,sep=""))){dir.create(paste(getwd(),"/",folderName,sep=""))}
 }
}
}



#------------------------------------------
# Format data to include any missing columns
#------------------------------------------

# At the very least data, x and y are needed.
if(length(names(data))<2){stop("Need to provide a data object with at least x and y.
                               metis.chart(data = userData,xData ='x',yData ='y'")}

if(!"units"%in%names(data)){data<-data%>%dplyr::mutate(units="units")}
if(!"classPalette1"%in%names(data)){data<-data%>%dplyr::mutate(classPalette1="pal_16")}
if("class"%in%names(data)){data<-data%>%dplyr::mutate(class="class1")}
if(!"class1"%in%names(data)){data<-data%>%dplyr::mutate(class1="class1")}
if(!"scenario"%in%names(data)){data<-data%>%dplyr::mutate(scenario="scenario")}

  l1 <- data
  l1<-l1%>%dplyr::mutate(units=dplyr::case_when(!grepl(" ",units)~paste("~",units),
                                         TRUE~units),
                         units=gsub(" ","~",units))

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

     namesAvail = unique(l1[[sankeyGroupColor]])[unique(l1[[sankeyGroupColor]]) %in% unique(paletteX_df$tempName)]; namesAvail
     namesUnAvail = unique(l1[[sankeyGroupColor]])[!unique(l1[[sankeyGroupColor]]) %in% unique(paletteX_df$tempName)]; namesUnAvail

     paletteDF_temp <- data.frame(tempName=namesAvail) %>%
       dplyr::mutate(tempName = as.character(tempName)) %>%
         dplyr::left_join(paletteX_df); paletteDF_temp

     paletteX <- as.vector(c(paletteDF_temp$value,metis.colors()$pal_Basic))[1:(length(namesAvail)+length(namesUnAvail))]; paletteX
     names(paletteX) <-c(paletteDF_temp$tempName,namesUnAvail);paletteX

    }

    } else {
      paletteX<-classPalette
    }
  }
  }

  if(class(l1[[class]])=="factor"){l1[[class]]<-factor(l1[[class]],levels=levels(l1[[class]])); levelsOn=T}else{
  l1[[class]]<-factor(l1[[class]],levels=unique(l1[[class]])); levelsOn=F}

  if(!is.null(names(paletteX))){
    if(!all(levels(l1[[class]]) %in% names(paletteX))){
  add_colors<-paletteX[1:length(levels(l1[[class]])[!levels(l1[[class]]) %in% names(paletteX)])]; add_colors
  names(add_colors)<-levels(l1[[class]])[!levels(l1[[class]]) %in% names(paletteX)]; add_colors
  paletteX<-c(paletteX,add_colors)}}

  if(class(l1[[class]])=="factor"){
    if(all(levels(l1[[class]]) %in% names(paletteX))){
  paletteX<-paletteX[levels(l1[[class]])]}}

  if(useNewLabels==1){
    if(!is.null(names(paletteX))){
      names(paletteX)<-tools::toTitleCase(sub("\\b[a-zA-Z0-9]{1} \\b", "",names(paletteX)))
      if(paletteRev==T){paletteX <- paletteX[rev(order(names(paletteX)))]}}
    l1[[class]]<-tools::toTitleCase(sub("\\b[a-zA-Z0-9]{1} \\b", "",l1[[class]]))
    l1<-l1%>%dplyr::arrange(!! rlang::sym(class))
    if(levelsOn){l1[[class]]<-factor(l1[[class]],levels=names(paletteX))}
  }

  if(all(names(paletteX) %in% colOrder1)){
    names(paletteX) <- colOrder1[colOrder1 %in% names(paletteX)]; paletteX
  }

  if(all(names(paletteX) %in% colOrder2)){
    names(paletteX) <- colOrder2[colOrder2 %in% names(paletteX)]; paletteX
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

  if(!is.null(xOrder)){
    xOrderx = xOrder[xOrder %in% unique(l1[[xData]])]
    l1 <- l1 %>% dplyr::mutate(!!as.name(xData) := factor(!!as.name(xData), levels = xOrderx))
  }





  if(chartType=="sankey"){
    p <- ggplot(l1, aes(y = get(yData), axis1 = get(sankeyAxis1), axis2 = get(sankeyAxis2), group=get(group)))
  } else{
  p <- ggplot(l1,aes(x=get(xData),y=get(yData),group=get(group))) +
      ggtitle(title) +
      theme_bw()

  # Custom Theme
  if(!is.null(theme_custom)){
    if(any(theme_custom %in% c("theme_gray","theme_bw","theme_linedraw","theme_light",
                               "theme_minimal","theme_classic","theme_void","theme_dark"))){
      if(theme_custom == "theme_gray"){theme_customX <- ggplot2::theme_gray()}
      if(theme_custom == "theme_bw"){theme_customX <-  ggplot2::theme_bw()}
      if(theme_custom == "theme_linedraw"){theme_customX <-  ggplot2::theme_linedraw()}
      if(theme_custom == "theme_light"){theme_customX <-  ggplot2::theme_light()}
      if(theme_custom == "theme_minimal"){theme_customX <-  ggplot2::theme_minimal()}
      if(theme_custom == "theme_classic"){theme_customX <-  ggplot2::theme_classic()}
      if(theme_custom == "theme_void"){theme_customX <-  ggplot2::theme_void()}
      if(theme_custom == "theme_dark"){theme_customX <-  ggplot2::theme_dark()}
      p <- p + theme_customX
    }else{print("theme_gg provided not available. No theme applied")}
  }

  if(!is.null(panelBGcolor)|!is.null(plotBGcolor)){
    if(is.null(legendBGcolor)){legendBGcolor<-plotBGcolor}
  p <- p +
    theme(panel.background =   element_rect(fill = panelBGcolor),
          plot.background =    element_rect(fill = plotBGcolor),
          legend.background = element_rect(fill = legendBGcolor))}

  p <- p +
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

  }



  # Calculate number of facets. If both are one then
  if(is.null(facet_columns) & is.null(facet_rows)){
    nFacets = 1
  } else {

    if(!is.null(facet_columns) & !is.null(facet_rows)){
      nFacets = length(unique(l1[[facet_columns]]))*length(unique(l1[[facet_rows]]))}

    if(!is.null(facet_columns) & is.null(facet_rows)){
      nFacets = length(unique(l1[[facet_columns]]))}

    if(is.null(facet_columns) & !is.null(facet_rows)){
      nFacets = length(unique(l1[[facet_rows]]))}

  }

  # Chart Type
  if(chartType=="sankey"){

    # https://stackoverflow.com/questions/56113973/variable-align-ggrepel-text-labels-in-faceted-alluvial-plot


    p <- p + theme_bw() +
      ggalluvial::geom_alluvium(aes(fill = get(class)), width = 1/12, color="grey10", alpha=0.7, na.rm=F) +
      ggalluvial::geom_stratum(width = 1/12, fill = "grey70", color = "grey10", alpha=1, na.rm=F) +
      scale_x_discrete(limits = c(sankeyAxis1Label, sankeyAxis2Label), expand = c(1,0.1),drop=F) +
      ggrepel::geom_text_repel(force=1,stat = "stratum", label.strata = TRUE, direction="y", size=4,segment.color = 'grey50') +
      scale_fill_manual(values=paletteX, name="From",na.translate=F, drop=F) +
      coord_cartesian(clip = "off")+
      theme(aspect.ratio = 0.5) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size = 24),
            strip.text  = element_text(size = 24),
            #strip.background = element_blank(),
            axis.title.x = element_text(size = 24),
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
      if(paletteRev==T){p = p + guides(fill = guide_legend(title=tools::toTitleCase(paste(class,sep="")),reverse = T))}else{
        p = p + guides(fill = guide_legend(title=tools::toTitleCase(paste(class,sep="")),reverse = F))+ theme(legend.position=legendPosition)
      }}else{
        if(length(unique(l1[[class]]))<2){
          if(!is.null(color)){value1=color}else{value1="firebrick"}
          p = p + theme(legend.position="none") + scale_fill_manual(values=value1)
        }else{
          if(paletteRev==T){p = p + guides(fill = guide_legend(title=unique(l1[[classLabel]]),reverse = T))}else{
            p = p + guides(fill = guide_legend(title=unique(l1[[classLabel]]),reverse = F))+ theme(legend.position=legendPosition)
          }
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
      geom_text(aes(label=round(value,2)),col="black", size = 5*max(1,nFacets/1.5) ) +
      coord_fixed(ratio = 1) +
      scale_x_discrete(limits = sectorToOrder, expand = c(0.1,0.1)) +
      scale_y_discrete(limits = rev(sectorFromOrder), expand = c(0.1,0.1)) +
      scale_size_continuous(range = c(1,bubbleSize*nFacets), guide="none") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 24),
            axis.text.y = element_text(size = 24),
            strip.text  = element_text(size = 24),
            axis.title = element_text(size = 24)) +
      theme(aspect.ratio = ifelse(length(unique(l1[[yData]]))/length(unique(l1[[xData]])) > 1, 1,
            ifelse(length(unique(l1[[yData]]))/length(unique(l1[[xData]])) < 0.05, 0.05, length(unique(l1[[yData]]))/length(unique(l1[[xData]])))))

    if(!grepl("class",class)){
      if(paletteRev==T){p = p + guides(fill = guide_legend(title=tools::toTitleCase(paste(class,sep="")),reverse = T))}else{
        p = p + guides(fill = guide_legend(title=tools::toTitleCase(paste(class,sep="")),reverse = F))+ theme(legend.position=legendPosition)
      }}else{
        if(length(unique(l1[[class]]))<2){
          if(!is.null(color)){value1=color}else{value1="firebrick"}
          p = p + theme(legend.position="none") + scale_fill_manual(values=value1)
        }else{
          if(paletteRev==T){p = p + guides(fill = guide_legend(title=unique(l1[[classLabel]]),reverse = T))}else{
            p = p + guides(fill = guide_legend(title=unique(l1[[classLabel]]),reverse = F))+ theme(legend.position=legendPosition)
          }
        }
      }

  }



  if(chartType=="bar"){
  p <- p + geom_bar(aes(fill=get(class)),size=sizeBarLines,color="black", stat="identity",position=position) +
           scale_fill_manual(values=paletteX) + guides(color=F)+ theme(legend.position=legendPosition)
  if(!grepl("class",class)){
    if(paletteRev==T){p = p + guides(fill = guide_legend(title=tools::toTitleCase(paste(class,sep="")),reverse = T))}else{
      p = p + guides(fill = guide_legend(title=tools::toTitleCase(paste(class,sep="")),reverse = F))+ theme(legend.position=legendPosition)
    }}
  else{
      if(length(unique(l1[[class]]))<2){
        if(!is.null(color)){value1=color}else{value1="firebrick"}
        p = p + theme(legend.position="none") + scale_fill_manual(values=value1)
      }else{
        if(paletteRev==T){p = p + guides(fill = guide_legend(title=unique(l1[[classLabel]]),reverse = T))}else{
          p = p + guides(fill = guide_legend(title=unique(l1[[classLabel]]),reverse = F))+ theme(legend.position=legendPosition)
        }
      }
    }
  }

  if(chartType=="line"){
  p <- p +  geom_line(aes(color=get(class),group=get(class)),size=sizeLines, stat="identity",position="identity") +
            scale_color_manual(values=paletteX)+ theme(legend.position=legendPosition)
  if(pointsOn==1){ p = p + geom_point(aes(shape = get(class), color=get(class)),size = pointsSize )}
  if(!grepl("class",class)){
    p = p + guides(color = guide_legend(title=tools::toTitleCase(paste(class,sep=""))))
    if(pointsOn==1){  p = p + guides(shape = guide_legend(title=tools::toTitleCase(paste(class,sep=""))))}}else{
      if(length(unique(l1[[class]]))<2){
        if(!is.null(color)){value1=color}else{value1="firebrick"}
        p = p + theme(legend.position="none") + scale_color_manual(values=value1)
      }else{
    p = p + guides(color = guide_legend(title=unique(l1[[classLabel]])))
    if(pointsOn==1){p = p + guides(shape = guide_legend(title=unique(l1[[classLabel]]))) }
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
       scale_y_continuous(breaks = scales::pretty_breaks(n = yBreaksMajn), minor_breaks = waiver())

  # Scale Range
  p = p +  ggplot2::expand_limits(y=c(yMin, yMax))

  # General Themes
  p <- p + theme(strip.text = element_text(size=facetLabelSize))
  }

if(is.numeric(l1[[xData]])){p<- p + scale_x_continuous (breaks=(seq(min(range(l1[[xData]])),max(range(l1[[xData]])),by=xBreaksMaj)),
                               minor_breaks=(seq(min(range(l1[[xData]])),max(range(l1[[xData]])),by=xBreaksMin)),
                               expand=c(0,xBreaksMaj/2))}


  # Faceting
  if(is.null(facet_columns) & is.null(facet_rows)){
    p <- p } else {

  if(!is.null(facet_rows) & !is.null(facet_columns)){

    if(forceFacets==T){p <- p + facet_grid(rows=vars(!!as.name(facet_rows)), cols=vars(!!as.name(facet_columns)),scales=scales)}else{

    if(length(unique(l1[[facet_rows]]))>1 & length(unique(l1[[facet_columns]]))>1){
    p <- p + facet_grid(rows=vars(!!as.name(facet_rows)), cols=vars(!!as.name(facet_columns)),scales=scales)} else {

      if(length(unique(l1[[facet_rows]]))>1 & !length(unique(l1[[facet_columns]]))>1){
        p <- p + facet_wrap(facet_rows,nrow=ncolrow,scales = scales)
        if(is.null(title)){p <- p + ggtitle(paste(unique(l1[[facet_columns]])," ",fileName,sep=""))}else{
          if(title=="Off"|title=="off"|title=="OFF"){p<-p+ggtitle(NULL)}else{
          p <- p + ggtitle(paste(unique(l1[[facet_columns]])," ",fileName,sep=""))
        }}} else {

    if(!length(unique(l1[[facet_rows]]))>1 & length(unique(l1[[facet_columns]]))>1){
      p <- p + facet_wrap(facet_columns,ncol=ncolrow,scales = scales)
      if(is.null(title)){p <- p + ggtitle(paste(unique(l1[[facet_rows]])," ",fileName,sep=""))}else{
        if(title=="Off"|title=="off"|title=="OFF"){p<-p+ggtitle(NULL)}else{
        p <- p + ggtitle(paste(unique(l1[[facet_rows]])," ",fileName,sep=""))}}
      } else {
        if(is.null(title)){p <- p + ggtitle(paste(unique(l1[[facet_columns]])," ",unique(l1[[facet_rows]])," ",fileName,sep=""))}else{
          if(title=="Off"|title=="off"|title=="OFF"){p<-p+ggtitle(NULL)}else{
          p <- p + ggtitle(paste(unique(l1[[facet_columns]])," ",unique(l1[[facet_rows]])," ",fileName,sep=""))}}
          }

          }}}} else {

    if(is.null(facet_rows) & !is.null(facet_columns)){
      p <- p + facet_wrap(facet_columns,ncol=ncolrow,scales = scales)
      }

    if(!is.null(facet_rows) & is.null(facet_columns)){
        p <- p + facet_wrap(facet_rows,nrow=ncolrow,scales = scales)}}
      }


  # General Themes
  p <- p + theme(strip.background = element_rect(fill = facetBGColor, colour = facetBorderColor),
                 strip.text = element_text(colour = facetLabelColor))

  if(printFig!=F){
    fname<-paste(fileName,sep="")
    if(!dir.exists(dirOutputs)){
      print(paste("dirOutputs provided: ",dirOutputs," does not exist. Saving to: ", paste(getwd(),"/outputs/Charts/Temp",sep=""),sep=""))
      if (!dir.exists(paste(getwd(),"/outputs/Charts/Temp",sep=""))){dir.create(paste(getwd(),"/outputs/Charts/Temp",sep=""))}
      dirOutputs=paste(getwd(),"/outputs/Charts/Temp",sep="")
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

          gg_guts$data[[3]]$label <- gsub("\\s", " ", format(gg_guts$data[[3]]$label, width=max(nchar(gg_guts$data[[3]]$label))+2, justify="left"));

          gg_guts$data[[3]] <-
            gg_guts$data[[3]] %>%
            dplyr::mutate(hjust = dplyr::case_when(x == min(gg_guts$data[[3]]$x) ~ 1.3,
                                                   x == max(gg_guts$data[[3]]$x) ~ -0.3,
                                                   TRUE ~ 0)); gg_guts$data[[3]]; nchar(gg_guts$data[[3]]$label)

          grid::grid.draw(ggplot_gtable(gg_guts))

          # once you've made your adjustments, you can plot it again
          if(pdfpng=='pdf'){grDevices::pdf(paste(dirOutputs,"/",fname,".pdf",sep=""),width=figWidth,height=figHeight)
            grid::grid.newpage();grid::grid.draw(ggplot_gtable(gg_guts))
            grDevices::dev.off()}
          if(pdfpng=='png'){grDevices::png(paste(dirOutputs,"/",fname,".png",sep=""),width=figWidth,height=figHeight, units="in",res=300)
            grid::grid.newpage();grid::grid.draw(ggplot_gtable(gg_guts))
            grDevices::dev.off()
            fnameTempImage=paste(dirOutputs,"/",fname,".png",sep="")
            tempImage<-magick::image_read(fnameTempImage)
            croppedImage<-magick::image_trim(tempImage,fuzz=0);
            magick::image_write(croppedImage,fnameTempImage)}
          if(pdfpng=='both'){
            grDevices::pdf(paste(dirOutputs,"/",fname,".pdf",sep=""),width=figWidth,height=figHeight)
            grid::grid.newpage();grid::grid.draw(ggplot_gtable(gg_guts))
            grDevices::dev.off()
            grDevices::png(paste(dirOutputs,"/",fname,".png",sep=""),width=figWidth,height=figHeight, units="in",res=300)
            grid::grid.newpage();grid::grid.draw(ggplot_gtable(gg_guts))
            grDevices::dev.off()
            fnameTempImage=paste(dirOutputs,"/",fname,".png",sep="")
            tempImage<-magick::image_read(fnameTempImage)
            croppedImage<-magick::image_trim(tempImage,fuzz=0);
            magick::image_write(croppedImage,fnameTempImage)
          }

          sankeyHjustCheck <- "Adjusted"

          }
          } else { p$layers[[3]] <- NULL }
        }


        if(is.null(sankeyHjustCheck)){
          if(any("transparent" %in% c(plotBGcolor,panelBGcolor))){
        metis.printPdfPng(figure=p,
                        dir=dirOutputs,
                        filename=fname,
                        figWidth=figWidth,
                        figHeight=figHeight,
                        pdfpng=pdfpng,
                        transparent=T)
          }else{
            metis.printPdfPng(figure=p,
                              dir=dirOutputs,
                              filename=fname,
                              figWidth=figWidth,
                              figHeight=figHeight,
                              pdfpng=pdfpng)
          }
          }

        #print(paste("Figure saved as: ",fileName,".",pdfpng," in folder: ", paste(dirOutputs,sep=""),sep=""))
      }else{print("printFig set to F so no figure will be saved.")}


  return(p)
}


