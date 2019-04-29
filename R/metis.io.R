#' metis.io
#'
#' This function prepares gridded data for use with domestic metis modules.
#' @param dirOutputs Default =paste(getwd(),"/outputs",sep=""),
#' @param ioTable0 Initial ioTable. Must have columns: supplySector,total,export and cap. Each supply sector should also have imports. Default = NULL,
#' @param useIntensity Boolean to use given intensity or not. Default is set to 0.
#' @param A0 Intensity matrix. Default Null.
#' @param nameAppend Modified intensity matrix. Default =NULL,
#' @return A table with data by polygon ID for each shapefile provided
#' @keywords gcam, gcam database, query
#' @export


metis.io<-function(ioTable0 = NULL,
                   useIntensity = 0,
                   A0 = NULL,
                   dirOutputs=paste(getwd(),"/outputs",sep=""),
                   nameAppend=""
                        ){

  # ioTable0 = NULL
  # useIntensity = 0
  # A0 = NULL
  # dirOutputs=paste(getwd(),"/outputs",sep="")
  # nameAppend=""

#----------------
# Key Assumptions:
#----------------


#------------------
# Create Folders if needed
#------------------
if (!dir.exists(dirOutputs)){
  dir.create(dirOutputs)}
if (!dir.exists(paste(dirOutputs, "/IO", sep = ""))){
  dir.create(paste(dirOutputs, "/IO", sep = ""))}
if (!dir.exists(paste(dirOutputs, "/IO/CombSubReg", sep = ""))){
  dir.create(paste(dirOutputs, "/IO/CombSubReg", sep = ""))}

dir<-paste(dirOutputs, "/IO/CombSubReg", sep = "")



#----------------
# Initialize variables by setting to NULL
#----------------

NULL -> ioTable -> A


#------------------
# Small Internal Functions
# -----------------

addMissing<-function(data){
  if(!"scenario"%in%names(data)){data<-data%>%dplyr::mutate(scenario="scenario")}
  if(!"x"%in%names(data)){if("year"%in%names(data)){data<-data%>%dplyr::mutate(x=year)%>%dplyr::select(-year)}else{
    if("Year"%in%names(data)){data<-data%>%dplyr::mutate(x=Year)%>%dplyr::select(-Year)}else{
      if("YEAR"%in%names(data)){data<-data%>%dplyr::mutate(x=YEAR)%>%dplyr::select(-YEAR)}else{
      data<-data%>%dplyr::mutate(x=000)}}}}
  if(!"region"%in%names(data)){data<-data%>%dplyr::mutate(region="region")}
  if(!"subRegion"%in%names(data)){data<-data%>%dplyr::mutate(subRegion="subRegion")}
  if(!"unit"%in%names(data)){data<-data%>%dplyr::mutate(unit="unit")}
  if(!"param"%in%names(data)){data<-data%>%dplyr::mutate(param="param")}
  return(data)
}

addedColumns <- names(addMissing(data.frame())); addedColumns

# ------------------
# Check Columns
# ------------------

if(!is.null(ioTable0)){ioTable <- addMissing(ioTable0)}
if(!is.null(A0)){ A <- addMissing(A0)}

if(!is.null(ioTable)){

  if(!any(grepl("supplySector",colnames(ioTable)))){
    stop(print(paste("Column names in ioTable0: ",paste(colnames(ioTable),collapse=", ")," must include 'supplySector'.",sep="")))}
  for (column_i in c("export","adjustedDemand","cap","capNew","capOrig","surplus")){
  if(!any(grepl(column_i,colnames(ioTable)))){
    print(paste("Column names in ioTable0: ",paste(colnames(ioTable),collapse=", ")," must include ",column_i,".",sep=""))
    ioTable <- ioTable %>%
      mutate(!!column_i := 0)}}
  if(!any(grepl("total",colnames(ioTable),ignore.case = T))){
    ioTable <- ioTable %>%
    dplyr::mutate(total=rowSums(select(.,-c(nonFlowColsAll[nonFlowColsAll!="total"])),na.rm=T))
    } else {names(ioTable)[names(ioTable) %in% c("total","Total","TOTAL")]<-"total"}
  sectors <- ioTable %>% dplyr::select(supplySector) %>% unique()
  subRegions <- unique(ioTable$subRegion)
  years <- unique(ioTable$x)
  scenarios <- unique(ioTable$scenario)
}

if(!is.null(A)){
  if(!any(grepl("supplySector",colnames(ioTable)))){
    stop(print(paste("Column names in A0: ",paste(colnames(ioTable),collapse=", ")," must include 'supplySector'.",sep="")))}
}

if(!is.null(A) & !is.null(ioTable)) {
  if(!any(unique(ioTable$supplySector) %in% unique(A$supplySector))){
    stop(print(paste("Column names in A0: ",paste(colnames(ioTable),collapse=", ")," must include 'supplySector'.",sep="")))}
  }

ioTable
A

nonFlowCols <- c("total","cap","capNew","capOrig","surplus"); nonFlowCols
nonFlowColsAll <- c("supplySector",nonFlowCols,addedColumns); nonFlowColsAll


#------------------------------------
# For each year, suregion and scenario
#------------------------------------

tibble::tibble()->A_Output -> L_Output-> ioTbl_Output

for(scenario_i in scenarios){
  for(subRegion_i in subRegions){
    for(year_i in years){


      print(paste("Solving for scenario: ", scenario_i, ", year:", year_i, " and sub-region:", subRegion_i," ...",sep=""))


      # Subset Data (D0i,X0i,A0i,Z0i,Cap0i,Import0i,Export0i)
      NULL -> A0i -> ioTable0i

      if(!is.null(A)){A0i<-A %>% dplyr::filter(scenario==scenario_i,subRegion==subRegion_i,x==year_i)}
      if(!is.null(ioTable)){ioTable0i<-ioTable %>% dplyr::filter(scenario==scenario_i,subRegion==subRegion_i,x==year_i)}



#----------------------
# Check and fix Totals
# If provided total is less than calculated total then adjust total to new value
# If provided total is more than calcualted total then create a new category called adjustedDemand equal to the difference.
#-------------------


ioTable_adjustTotal <- ioTable0i %>%
  dplyr::mutate(otherTotal=rowSums(ioTable0i %>% select(-nonFlowColsAll),na.rm=T),
                total = case_when(total < otherTotal ~ otherTotal,TRUE ~ total),
                adjustedDemand = case_when(total > otherTotal ~ total - otherTotal,TRUE ~ 0)) %>%
  dplyr::select(-otherTotal)

ioTable_adjustTotal

#----------------------
# Adjust for Capacity and New Capacity
#-------------------

ioTable_adjustCap <- ioTable_adjustTotal %>%
  dplyr::mutate(capNew = case_when(total > cap ~ (total-cap), TRUE~0),
                cap = case_when(total > cap ~ cap + capNew, TRUE~cap),
                capOrig = cap - capNew)

ioTable_adjustCap %>% as.data.frame()

#--------------------------
# Use Intensity if specified
#--------------------------

if(useIntensity==1){
  if(!is.null(A0i)){

  print("Using A0 intensity matrix provided to recaclulate Z and new totals.")

  Ai <- as.matrix(A0i%>%dplyr::select(c(A0i$supplySector))); Ai
  Li <- solve(diag(nrow(Ai))-Ai); Li

  # Calculate nonNexusTotals
  colsRemove <- names(ioTable_adjustCap)[names(ioTable_adjustCap) %in% c(nonFlowCols,addedColumns,unique(ioTable_adjustCap$supplySector))]
  Di <- ioTable_adjustCap %>%
    dplyr::select(-colsRemove) %>%
    dplyr::mutate(otherTotal=rowSums(select(.,-supplySector),na.rm=T)); Di

  # Calculate New Total based on the intensities and existing other demands
  Xi <- tibble::as_tibble(Li %*% as.matrix(Di$otherTotal))
  Xi <- dplyr::bind_cols(Di%>%dplyr::select(supplySector),Xi)
  names(Xi)<-c("supplySector","total"); Xi

  # Calculate nexus flows based on the intensity provided
  Zi <- Ai %*% diag(as.vector(t(as.matrix(Xi$total))))
  Zi <- dplyr::bind_cols(Xi%>%dplyr::select(supplySector),tibble::as_tibble(Zi))
  names(Zi)<-c("supplySector",Xi$supplySector);
  Zi <- Zi %>%
  dplyr::mutate(nexusTotal=rowSums(Zi %>% select(-supplySector),na.rm=T)); Zi

  # Join nexus and other demands to get totals and recalculate new Cap needed
  ZiCols <- names(Zi%>%dplyr::select(-supplySector))[names(Zi%>%dplyr::select(-supplySector)) %in% names(ioTable_adjustCap)]
  DiCols <- names(Di%>%dplyr::select(-supplySector))[names(Di%>%dplyr::select(-supplySector)) %in% names(ioTable_adjustCap)]

  ioTable_adjustIntensity <- ioTable_adjustCap %>%
    dplyr::select(-ZiCols,-DiCols) %>%
    dplyr::left_join(Zi) %>%
    dplyr::left_join(Di) %>%
    dplyr::mutate(total = nexusTotal + otherTotal,
                  capNew = case_when(total > cap ~ (total-cap), TRUE~0),
                  cap = case_when(total > cap ~ cap + capNew, TRUE~cap),
                  capOrig = cap - capNew) %>%
    dplyr::select(-otherTotal,-nexusTotal)

} else {print("No A intensity matrix provided. Skipping calculation using A.")}

} else {
  ioTable_adjustIntensity <- ioTable_adjustCap
}

ioTable_adjustIntensity %>% as.data.frame()

#--------------------------
# Fill Out missing Nexus Sectors into ioTable
#--------------------------

ioTable_fillNexus <- ioTable_adjustIntensity

 for(nexus_sector_i in ioTable_fillNexus$supplySector[!ioTable_fillNexus$supplySector %in% names(ioTable_fillNexus)]){
   ioTable_fillNexus <- ioTable_fillNexus %>%
    mutate(!!nexus_sector_i := 0)
 }

ioTable_fillNexus

#--------------------------
# Final IO Table Outputs
#--------------------------

ioTable_complete<-ioTable_fillNexus %>%
  dplyr::mutate(surplus = cap-total)

Zorg = ioTable_complete %>%
  dplyr::select(supplySector,unique(ioTable_complete$supplySector)); Zorg
Xorg = ioTable_complete %>%
  dplyr::select(supplySector,total); Xorg
Aorg <-as.matrix(Zorg%>%dplyr::select(c(unique(Zorg$supplySector)))) %*% as.matrix(Xorg$total^-1*diag(nrow(Zorg)));Aorg
Lorg <-solve(diag(nrow(Aorg))-Aorg);
Aorg <- tibble::as_tibble(Aorg); Lorg <- tibble::as_tibble(Lorg)
names(Aorg) <- names(Lorg) <- unique(Zorg$supplySector)
Aorg <- Aorg %>% dplyr::bind_cols(Zorg %>% dplyr::select(supplySector)) %>%
  dplyr::select(c("supplySector",sort(unique(Zorg$supplySector)))) %>% dplyr::arrange(supplySector) ;Aorg
Lorg <- Lorg %>% dplyr::bind_cols(Zorg %>% dplyr::select(supplySector));Lorg

colOrder <- c("supplySector",sort(ioTable_complete$supplySector),
              names(ioTable_complete)[!names(ioTable_complete) %in% c(ioTable_complete$supplySector,
                                                                      "export",nonFlowColsAll)],
              "export",nonFlowColsAll[nonFlowColsAll!="supplySector"]); colOrder

ioTable_complete <- ioTable_complete %>% dplyr::select(colOrder) %>% arrange(supplySector);
ioTable_complete %>%as.data.frame()

A_Output = A_Output %>% dplyr::bind_rows(Aorg %>% tibble::as_tibble() %>% dplyr::mutate(scenario=scenario_i,x=year_i,subRegion=subRegion_i)); A_Output
L_Output = L_Output %>% dplyr::bind_rows(Lorg %>% tibble::as_tibble() %>% dplyr::mutate(scenario=scenario_i,x=year_i,subRegion=subRegion_i)); L_Output
ioTbl_Output = ioTbl_Output %>% dplyr::bind_rows(ioTable_complete %>% tibble::as_tibble() %>% dplyr::mutate(scenario=scenario_i,x=year_i,subRegion=subRegion_i)); ioTbl_Output


    } # Close loop scenario
  } # close loop subRegion
} # close loop year

    sol_list<-list(A_Output=A_Output,
                   L_Output=L_Output,
                   ioTbl_Output=ioTbl_Output)

  sol_list<-sol_list[sapply(sol_list, function(x) dim(x)[1]) > 0]


  #-----------------------
  # IO Visualization (Eventually Move this to separate function?)
  #-----------------------


  # Print Figure Function
  printf <- function(printFig=T,
                     fileName="file",
                     dir,
                     figure,
                     figWidth=13,
                     figHeight=9,
                     pdfpng="png"){
    if(printFig!=F){
    fname<-paste(fileName,sep="")
    if(!dir.exists(dir)){
      print(paste("directory provided: ",dir," does not exist. Saving to: ", getwd(),sep=""))
      dir=getwd()}else{
        metis.printPdfPng(figure=figure,
                          dir=dir,
                          filename=fname,
                          figWidth=figWidth,
                          figHeight=figHeight,
                          pdfpng=pdfpng)

        print(paste("Figure saved as: ",fileName,".",pdfpng," in folder: ", paste(dirOutputs,sep=""),sep=""))
      }}else{print("printFig set to F so no figure will be saved.")}
  }



  for (scenario_i in scenarios){

    #---------------------
    # sol_Output
    #---------------------


  # A Intensity Matrix

  A_mat <- sol_list$A_Output %>%
    dplyr::filter(scenario==scenario_i); A_mat

  A_matx <- A_mat %>%
    tidyr::gather(-c("supplySector",addedColumns[addedColumns %in% names(A_mat)]),key="sectorTo",value="value") %>%
    dplyr::rename (sectorFrom=supplySector) %>%
    dplyr::arrange(sectorFrom) %>%
    dplyr::filter(!is.nan(value),!is.infinite(value),value!=0, !is.na(value)); A_matx

  sectorFromOrder <- sort(unique(A_matx$sectorFrom)); sectorFromOrder
  sectorToOrder <-  sectorFromOrder; sectorToOrder


  # A Intensity Matrix

  if(nrow(A_matx)>0){

  fname = paste("A_",scenario_i,nameAppend,sep="")
  ga <- ggplot(A_matx, aes(y = sectorFrom, x = sectorTo)) +
    theme_bw() +
    labs(title=fname) +
    geom_point(data=A_matx,aes(col=value, size=value)) +
    scale_color_gradient(low = "white", high = "indianred1", guide="none") +
    geom_text(aes(label=round(value,2)),col="black", size=5) +
    coord_fixed(ratio = 1) +
    scale_x_discrete(limits = sectorToOrder, expand = c(0.1,0.1)) +
    scale_y_discrete(limits = rev(sectorFromOrder), expand = c(0.1,0.1)) +
    scale_size_continuous(range = c(1,20), guide="none") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 15),
          axis.text.y = element_text(size = 15),
          strip.text  = element_text(size = 15),
          axis.title = element_text(size = 15))+
    facet_grid(x~subRegion); ga

  printf(figure=ga,fileName = fname, dir=dir)

  }
  # ioTable normalized

  sol<- sol_list$ioTbl_Output %>%
    dplyr::filter(scenario==scenario_i); sol


  df_Mn<-sol %>%
    dplyr::mutate(totalTemp=total) %>% # to move total to end to include columns on right side of total in the normalization
    dplyr::mutate_at(vars(-c("supplySector",addedColumns[addedColumns %in% names(sol)])),dplyr::funs(./totalTemp)) %>%
    dplyr::select(-totalTemp); df_Mn

  solx <- sol %>%
    tidyr::gather(-c("supplySector",addedColumns[addedColumns %in% names(df_Mn)]),key="sectorTo",value="value") %>%
    dplyr::rename (sectorFrom=supplySector) %>%
    dplyr::arrange(sectorFrom)  %>%
    dplyr::filter(!is.nan(value),value!=0, !is.na(value));solx


  df_Mnx <- df_Mn %>%
    tidyr::gather(-c("supplySector",addedColumns[addedColumns %in% names(df_Mn)]),key="sectorTo",value="value") %>%
    dplyr::rename (sectorFrom=supplySector) %>%
    dplyr::arrange(sectorFrom) %>%
    dplyr::filter(!is.nan(value),!is.infinite(value),value!=0, !is.na(value)); df_Mnx

  sectorFromOrder <- sort(unique(df_Mnx$sectorFrom)); sectorFromOrder

  sectorToOrder <-
    c(sort(unique(df_Mnx$sectorTo)[unique(df_Mnx$sectorTo) %in% unique(sol$supplySector)]),
      sort(unique(df_Mnx$sectorTo)[!unique(c(df_Mnx$sectorTo)) %in% c(unique(sol$supplySector),"export",nonFlowCols)]),
      "export",nonFlowCols); sectorToOrder


  # ioTable normalized bubbles

  if(nrow(df_Mnx)>0){

  fname = paste("ioNORM_",scenario_i,nameAppend,sep="")
  ga <- ggplot(df_Mnx, aes(y = sectorFrom, x = sectorTo)) +
    theme_bw() +
    labs(title=fname) +
    geom_point(data=df_Mnx%>%dplyr::filter(!sectorTo %in% nonFlowCols),aes(col=value, size=value)) +
    scale_color_gradient(low = "white", high = "indianred1", guide="none") +
    geom_text(aes(label=round(value,2)),col="black", size=5) +
    coord_fixed(ratio = 1) +
    scale_x_discrete(limits = sectorToOrder, expand = c(0.1,0.1)) +
    scale_y_discrete(limits = rev(sectorFromOrder), expand = c(0.1,0.1)) +
    scale_size_continuous(range = c(1,20), guide="none") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 15),
          axis.text.y = element_text(size = 15),
          strip.text  = element_text(size = 15),
          axis.title = element_text(size = 15))+
    facet_grid(x~subRegion); ga

  printf(figure=ga,fileName = fname, dir=dir)
  }

  # ioTable absolute bubbles

  if(nrow(solx)>0){

  fname = paste("ioABS_",scenario_i,nameAppend,sep="")
  ga <- ggplot(solx, aes(y = sectorFrom, x = sectorTo)) +
    theme_bw() +
    labs(title=fname) +
    geom_point(data=df_Mnx%>%dplyr::filter(!sectorTo %in% nonFlowCols),aes(col=value, size=value)) +
    scale_color_gradient(low = "white", high = "indianred1", guide="none") +
    geom_text(aes(label=round(value,2)),col="black", size=5) +
    coord_fixed(ratio = 1) +
    scale_x_discrete(limits = sectorToOrder, expand = c(0.1,0.1)) +
    scale_y_discrete(limits = rev(sectorFromOrder), expand = c(0.1,0.1)) +
    scale_size_continuous(range = c(1,20), guide="none") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 15),
          axis.text.y = element_text(size = 15),
          strip.text  = element_text(size = 15),
          axis.title = element_text(size = 15))+
    facet_grid(x~subRegion); ga

  printf(figure=ga,fileName = fname, dir=dir)

  }


  #-----------------
  # Sankey
  #--------------

  df <- sol %>%
    dplyr::select(-nonFlowCols); df

  dfx <- df %>%
    tidyr::gather(-c("supplySector",addedColumns[addedColumns %in% names(df_Mn)]),key="sectorTo",value="value") %>%
    dplyr::rename (sectorFrom=supplySector) %>%
    dplyr::filter(value>0) %>%
    unique() %>%
    dplyr::arrange(sectorFrom); dfx

  sectorFromOrderSankey <- sectorFromOrder[sectorFromOrder %in% unique(dfx$sectorFrom)]; sectorFromOrderSankey
  sectorToOrderSankey <- sectorToOrder[sectorToOrder %in% unique(dfx$sectorTo)]; sectorToOrderSankey

  dfx$sectorFrom <- factor( as.character(dfx$sectorFrom), levels=sectorFromOrderSankey )
  dfx$sectorTo <- factor( as.character(dfx$sectorTo), levels=sectorToOrderSankey )

  if(all(unique(dfx$sectorFrom) %in% names(metis.colors()$pal_sankey))){
    fillcolors = metis.colors()$pal_sankey} else {
      fillcolors = metis.colors()$pal_Basic
    }; fillcolors


  if(nrow(dfx)>0){

  fname = paste("sankeyAll_",scenario_i,nameAppend,sep="")
  g<-ggplot(as.data.frame(dfx%>%dplyr::filter(value!=0)),
            aes(y = value, axis1 = sectorFrom, axis2 = sectorTo, group=subRegion)) +
    theme_bw() +
    ggalluvial::geom_alluvium(aes(fill = sectorFrom), width = 1/12, color="black", alpha=0.6) +
    ggalluvial::geom_stratum(width = 1/12, fill = "grey30", color = "grey", alpha=1) +
    geom_label(stat = "stratum", label.strata = TRUE) +
    scale_x_discrete(limits = c("From", "To"), expand = c(.05, .05)) +
    #scale_fill_brewer(type = "qual", palette = "Set1", name="From") +
    scale_fill_manual(values=fillcolors, name="From") +
    facet_grid(x~subRegion) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 15),
          axis.text.y = element_text(size = 15),
          strip.text  = element_text(size = 15),
          axis.title = element_text(size = 15))+
    ggtitle(fname);g


  printf(figure=g,fileName = fname, dir=dir)

  }


  #-------- Aggregated Demands

  solFlows <- sol %>%
    dplyr::select(!!c("supplySector",names(sol)[names(sol) %in% c("total",addedColumns)]))
  df <- solFlows;df

  dfx <- df %>%
    tidyr::gather(-c("supplySector",addedColumns[addedColumns %in% names(df_Mn)]),key="sectorTo",value="value") %>%
    dplyr::rename (sectorFrom=supplySector) %>%
    dplyr::filter(value>0) %>%
    unique() %>%
    dplyr::arrange(sectorFrom); dfx

  sectorFromOrderSankey <- sectorFromOrder[sectorFromOrder %in% unique(dfx$sectorFrom)]; sectorFromOrderSankey
  sectorToOrderSankey <- sectorToOrder[sectorToOrder %in% unique(dfx$sectorTo)]; sectorToOrderSankey

  dfx$sectorFrom <- factor( as.character(dfx$sectorFrom), levels=sectorFromOrderSankey )
  dfx$sectorTo <- factor( as.character(dfx$sectorTo), levels=sectorToOrderSankey )

  if(all(unique(dfx$sectorFrom) %in% names(metis.colors()$pal_sankey))){
    fillcolors = metis.colors()$pal_sankey} else {
      fillcolors = metis.colors()$pal_Basic
    }; fillcolors

  if(nrow(dfx)>0){

  fname = paste("sankeyAgg_",scenario_i,nameAppend,sep="")
  g<-ggplot(as.data.frame(dfx%>%dplyr::filter(value!=0)),
            aes(y = value, axis1 = sectorFrom, axis2 = sectorTo, group=subRegion)) +
    theme_bw() +
    ggalluvial::geom_alluvium(aes(fill = sectorFrom), width = 1/12, color="black", alpha=0.6) +
    ggalluvial::geom_stratum(width = 1/12, fill = "grey30", color = "grey", alpha=1) +
    geom_label(stat = "stratum", label.strata = TRUE) +
    scale_x_discrete(limits = c("From", "To"), expand = c(.05, .05)) +
    #scale_fill_brewer(type = "qual", palette = "Set1", name="From") +
    scale_fill_manual(values=fillcolors, name="From") +
    facet_grid(x~subRegion) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 15),
          axis.text.y = element_text(size = 15),
          strip.text  = element_text(size = 15),
          axis.title = element_text(size = 15))+
    ggtitle(fname);g


  printf(figure=g,fileName = fname, dir=dir)
  }


  } # For Scenario i

  return(sol_list)

} # Close Function

