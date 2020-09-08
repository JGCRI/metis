#' metis.mapsProcess
#'
#' This function produce different kinds of maps for the metis package.
#' Each figure is accompanied with a csv table.
#'
#' @keywords charts, diffplots
#' @return Returns the formatted data used to produce chart
#' @param polygonTable Default = NULL,
#' @param gridTable Default = NULL,
#' @param dirOutputs Default = paste(getwd(),"/outputs",sep=""),
#' @param folderName Default ="folderNameDefault",
#' @param xRange Default ="All",
#' @param labels Default = F,
#' @param labelsSize Default = 1.2,
#' @param boundaryRegionsSelect Default = NULL,
#' @param subRegShape Default = NULL,
#' @param subRegShpFolder Default = paste(getwd(),"/dataFiles/gis/admin_gadm36",sep=""),
#' @param subRegShpFile Default = paste("gadm36_1",sep=""),
#' @param subRegCol Default ="subRegion",
#' @param subRegType Default =NULL
#' @param nameAppend Default =""
#' @param legendOutsideSingle Default =F, Single plots by default have legends inside. This can be moved out if wanted.
#' @param legendOutsidePosition Default = NULL, # "right","left","top","bottom", "center"
#' @param legendPosition Default = NULL, # c("RIGHT','top') - RIGHT LEFT TOP BOTTOM
#' @param legendFixedBreaks Default = "5",
#' @param animateOn Default = T,
#' @param fps Default = 1,
#' @param legendTitleSizeO Default = 2,
#' @param legendTextSizeO Default =1,
#' @param legendTitleSizeI Default = 1,
#' @param legendTextSizeI Default =0.5,
#' @param extension Default =F,
#' @param boundaryRegShape Default = NULL,
#' @param boundaryRegShpFolder Default= NULL . Suggested paste(getwd(),"/dataFiles/gis/naturalEarth",sep  Default="")
#' @param boundaryRegShpFile Default=NULL . Suggested paste("ne_10m_admin_0_countries",sep  Default=""),
#' @param boundaryRegCol Default=NULL. Suggested "NAME_0",
#' @param extendedFillColor Default ="grey75",
#' @param extendedBGColor Default ="lightblue1",
#' @param extendedHighLightColor Default ="cornsilk1",
#' @param extendedLabels Default = T
#' @param extendedLabelsColor Default ="grey30",
#' @param extdendedLabelSize Default =0.7,
#' @param extendedShape Default =NULL,
#' @param extendedShapeCol Default =NULL,
#' @param expandPercent Default =3
#' @param paramsSelect Default ="All"
#' @param projX Default = projX="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#' @param figWidth Default =9
#' @param figHeight Default =7
#' @param scenRef Reference Scenario. Default = NULL
#' @param scenDiff Scenarios to Diff. Default = NULL
#' @param scaleRange Default NULL. A vector with c(max,min) (Applied to all params) or a dataframe with cols param, max, min
#' @param scaleRangeDiffAbs Default =NULL, A vector with c(max,min) (Applied to all params) or a dataframe with cols param, max, min
#' @param scaleRangeDiffPrcnt Default =NULL, A vector with c(max,min) (Applied to all params) or a dataframe with cols param, max, min
#' @param xRef Reference year. Default = NULL
#' @param xDiff years to Diff. Default = NULL
#' @param scaleRangeDiffxAbs Default =NULL, A vector with c(max,min) (Applied to all params) or a dataframe with cols param, max, min
#' @param scaleRangeDiffxPrcnt Default =NULL, A vector with c(max,min) (Applied to all params) or a dataframe with cols param, max, min
#' @param multifacetsOn Default = F,
#' @param multiFacetCols Default ="multiFacetRow",
#' @param multiFacetRows Default ="multiFacetCol",
#' @param legendOutsideMulti Default = NULL,
#' @param legendPositionMulti Default = NULL,
#' @param legendTitleSizeMulti Default = NULL,
#' @param legendTextSizeAnim Default = NULL,
#' @param legendTextSizeMulti Default = NULL,
#' @param refMultiA Default = NULL , eg. "gfdl-esm2m"
#' @param refMultiB Default = NULL , eg. "rcp2p6"
#' @param chosenRefMeanYears Default=NULL
#' @param mapTitleSize Default=0.5
#' @param facetLabelSize Default =2.5
#' @param facetBGColor Default = NA
#' @param facetLabelColor Default = Black
#' @param facetLabelSizeMultiAB Default =1
#' @param facetLabelBorderLwd Default=NA_real_,
#' @param numeric2Cat_list Default=NULL,
#' @param frameShow Default = T. Whether to plot frame around maps and facets.
#' @param pdfpng Save IO figures as pdf or png. Type=String. Options: 'pdf' or 'png'. Default = 'png'
#' @param fillcolorNA Default="gray",
#' @param fillshowNA Default=NA,
#' @param fillcolorNULL Default="gray"
#' @param legendSingleColorOn Default=F,
#' @param legendSingleValue Default =NULL,
#' @param legendSingleColor Default="white"
#' @param facetCols Default=4
#' @param mapTitleOn Default=T
#' @param legendDigitsOverride Default=NULL
#' @param innerMargins Default =c(0,0,0,0) # bottom, left, top, right
#' @param classPalette Default = NULL
#' @param classPaletteDiff Default = "pal_div_BrGn"
#' @param cropToBoundary Default = T
#' @return A list with the gridTbl and shapeTbl used to plot the data if any.
#' @export


metis.mapsProcess<-function(polygonTable=NULL,
                           gridTable=NULL,
                           dirOutputs=paste(getwd(),"/outputs",sep=""),
                           folderName="",
                           xRange="All",
                           labels=F,
                           labelsSize=1.0,
                           subRegShape=NULL,
                           subRegShpFolder=NULL,
                           subRegShpFile=NULL,
                           subRegCol="subRegion",
                           subRegType =NULL,
                           nameAppend="",
                           legendOutsideSingle=T,
                           legendOutsidePosition=NULL,
                           legendPosition=NULL,
                           legendFixedBreaks=5,
                           legendTitleSizeO=1.0,
                           legendTextSizeO=0.6,
                           legendTitleSizeI=1.0,
                           legendTextSizeI=0.6,
                           animateOn=T,
                           fps=1,
                           extension=F,
                           boundaryRegShape=NULL,
                           boundaryRegShpFolder=NULL,
                           boundaryRegShpFile=NULL,
                           boundaryRegCol="subRegion",
                           boundaryRegionsSelect=NULL,
                           cropToBoundary=F,
                           extendedLabels =F,
                           extendedFillColor="grey75",
                           extendedBGColor="lightblue1",
                           extendedHighLightColor="cornsilk1",
                           extendedLabelsColor="grey30",
                           extdendedLabelSize=0.4,
                           extendedShape=NULL,
                           extendedShapeCol="subRegion",
                           expandPercent=3,
                           projX="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
                           figWidth=6,
                           figHeight=7,
                           scenRef=NULL,
                           scenDiff=NULL,
                           scaleRange=NULL,
                           scaleRangeDiffAbs=NULL,
                           scaleRangeDiffPrcnt=NULL,
                           xRef=NULL,
                           xDiff=NULL,
                           scaleRangeDiffxAbs=NULL,
                           scaleRangeDiffxPrcnt=NULL,
                           paramsSelect="All",
                           multifacetsOn=F,
                           multiFacetCols="multiFacetCol",
                           multiFacetRows="multiFacetRow",
                           legendOutsideMulti=T,
                           legendPositionMulti=NULL,
                           legendTitleSizeMulti=NULL,
                           legendTextSizeAnim=NULL,
                           legendTextSizeMulti=NULL,
                           refMultiA=NULL,
                           refMultiB=NULL,
                           chosenRefMeanYears=NULL,
                           mapTitleSize=0.5,
                           facetBGColor=NA,
                           facetLabelColor ="black",
                           facetLabelSize=1.0,
                           facetLabelSizeMultiAB=1,
                           facetLabelBorderLwd =NA_real_,
                           numeric2Cat_list=NULL,
                           frameShow = F,
                           pdfpng = 'png',
                           fillcolorNA="gray",
                           fillshowNA=NA,
                           fillcolorNULL="gray",
                           legendSingleColorOn=NULL,
                           legendSingleValue=NULL,
                           legendSingleColor="white",
                           facetCols=4,
                           mapTitleOn=T,
                           innerMargins=c(0,0,0,0), # bottom, left, top, right
                           legendDigitsOverride=NULL,
                           classPalette = NULL,
                           classPaletteDiff = "pal_div_BluRd"
                           ){

  # polygonTable=NULL
  # gridTable=NULL
  # dirOutputs=paste(getwd(),"/outputs",sep="")
  # folderName=""
  # xRange="All"
  # labels=F
  # labelsSize=1.2
  # subRegShape=NULL
  # subRegShpFolder=NULL
  # subRegShpFile=NULL
  # subRegCol="subRegion"
  # nameAppend=""
  # legendOutsideSingle=T
  # legendOutsidePosition=NULL
  # legendPosition=NULL
  # legendFixedBreaks=5
  # legendTitleSizeO=2
  # legendTextSizeO=1
  # legendTitleSizeI=1.5
  # legendTextSizeI=1
  # animateOn=T
  # fps=1
  # scenRef=NULL
  # scenDiff=NULL
  # xRef=NULL
  # xDiff=NULL
  # scaleRangeDiffxAbs=NULL
  # scaleRangeDiffxPrcnt=NULL
  # extension=F
  # boundaryRegShape=NULL
  # boundaryRegShpFolder=NULL
  # boundaryRegShpFile=NULL
  # boundaryRegCol="subRegion"
  # boundaryRegionsSelect=NULL
  # extendedLabels =T
  # extendedFillColor="grey75"
  # extendedBGColor="lightblue1"
  # extendedHighLightColor="cornsilk1"
  # extendedLabelsColor="grey30"
  # extdendedLabelSize=0.7
  # extendedShape=NULL
  # extendedShapeCol=NULL
  # expandPercent=3
  # projX="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  # figWidth=6
  # figHeight=7
  # scaleRange=NULL
  # scaleRangeDiffAbs=NULL
  # scaleRangeDiffPrcnt=NULL
  # paramsSelect="All"
  # multifacetsOn=F
  # multiFacetCols="multiFacetCol"
  # multiFacetRows="multiFacetRow"
  # legendOutsideMulti=T
  # legendPositionMulti=NULL
  # legendTitleSizeMulti=NULL
  # legendTextSizeAnim=NULL
  # legendTextSizeMulti=NULL
  # refMultiA=NULL
  # refMultiB=NULL
  # chosenRefMeanYears=NULL
  # mapTitleSize=0.5
  # facetBGColor=NA
  # facetLabelColor ="black"
  # facetLabelSize=1.5
  # facetLabelSizeMultiAB=1
  # facetLabelBorderLwd =NA_real_
  # numeric2Cat_list=NULL
  # frameShow = F
  # pdfpng = 'png'
  # fillcolorNA="gray"
  # fillshowNA=NA
  # fillcolorNULL="gray"
  # legendSingleColorOn=NULL
  # legendSingleValue=NULL
  # legendSingleColor="white"
  # facetCols=4
  # mapTitleOn=T
  # innerMargins=c(0,0,0,0) # bottom, left, top, right
  # legendDigitsOverride=NULL
  # classPalette = NULL
  # classPaletteDiff = "pal_div_BrGn"
  # cropToBoundary=F
  # subRegType = NULL

  print("Starting metis.mapsProcess...")

  #------------------
  # Initialize variables
  # -----------------

  if(T){
  NULL->lat->lon->param->region->scenario->subRegion->value ->
    x->year->gridID->underLayer->maxScale->minScale->
    valueDiff->rowid->catParam->include->Var1->Var2->Var3->maxX->minX->shapeTblScenMultiABRef->
    shapeTblDiff -> gridTblDiff -> shapeTblDiffx -> gridTblDiffx -> shapeTblMultiOrig->countCheck->
      multiFacetCol -> multiFacetRow->classPaletteOrig->
      xLabel->vintage->aggregate->query->subRegNotInShape ->gridTblOrig -> shapeTblOrig -> subRegionAlt -> subRegion1 ->
      paramsGrid -> paramsShape -> scaleRange_i -> boundaryRegShapeLimits

  expandPercent_i=expandPercent

  tibble::tibble() -> gridTblReturn -> shapeTblReturn

  classPaletteOrig <- classPalette
  subRegShapeOrig <- subRegShape
  subRegShpFileOrig <- subRegShpFile
  subRegShpFolderOrig <- subRegShpFolder
  boundaryRegShapeOrig <- boundaryRegShape
  boundaryRegShpFileOrig <- boundaryRegShpFile
  boundaryRegShpFolderOrig <- boundaryRegShpFolder
  animateOnOrig <- animateOn
  legendSingleColorOnOrig <- legendSingleColorOn
  xRangeOrig = xRange
  if(is.null(legendSingleColorOnOrig)){legendSingleColorOn=F}


  dirOutputsX <- dirOutputs;

  # Set legend size based on where legend is placed
  if(legendOutsideSingle==T){legendTitleSizeS=legendTitleSizeO;legendTextSizeS=legendTextSizeO;legendPositionS=NULL}
  if(legendOutsideSingle==F){legendTitleSizeS=legendTitleSizeI;legendTextSizeS=legendTextSizeI;legendPositionS=legendPosition}
  if(is.null(legendPositionMulti)){legendPositionMulti=legendPosition}
  if(legendOutsideMulti==T){if(is.null(legendTitleSizeMulti)){legendTitleSizeMulti=legendTitleSizeO;legendTextSizeMulti=legendTextSizeO;legendPositionMulti=NULL}}
  if(legendOutsideMulti==F){if(is.null(legendTitleSizeMulti)){legendTitleSizeMulti=legendTitleSizeI;legendTextSizeMulti=legendTextSizeI;legendPositionMulti=legendPosition}}
}


  if(!is.null(subRegShapeOrig)){
    if(is.null(subRegType)){
      if(class(subRegShapeOrig)=="SpatialPolygonsDataFrame"){
        if(any("subRegionType" %in% names(subRegShapeOrig@data))){
          subRegType = unique(subRegShapeOrig@data$subRegionType)
        }
      }
    }
  }; subRegType

  #------------------
  # Function for adding any missing columns if needed
  # -----------------

  if(T){

    addMissingScale<-function(data){
      data <- data %>% dplyr::ungroup()
      if(!any(grepl("\\<param\\>",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(param="param")}else{
        data <- data %>% dplyr::rename(!!"param" := (names(data)[grepl("\\<param\\>",names(data),ignore.case = T)])[1])
        data<-data%>%dplyr::mutate(param=as.character(param),param=dplyr::case_when(is.na(param)~"param",TRUE~param))}
      if(!any(grepl("\\<params\\>",names(data),ignore.case = T))){}else{
        data <- data %>% dplyr::rename(!!"param" := (names(data)[grepl("\\<params\\>",names(data),ignore.case = T)])[1])
        data<-data%>%dplyr::mutate(param=as.character(param),param=dplyr::case_when(is.na(param)~"params",TRUE~param))}
      if(!any(grepl("max",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(maxScale=NA_real_)}else{
        data <- data %>% dplyr::rename(!!"maxScale" := (names(data)[grepl("max",names(data),ignore.case = T)])[1])
        data<-data %>%dplyr::mutate(maxScale=as.numeric(maxScale))
        data<-data %>%dplyr::mutate(maxScale=dplyr::case_when(is.na(maxScale)~NA_real_,TRUE~maxScale))}
      if(!any(grepl("min",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(minScale=NA_real_)}else{
        data <- data %>% dplyr::rename(!!"minScale" := (names(data)[grepl("min",names(data),ignore.case = T)])[1])
        data<-data %>%dplyr::mutate(minScale=as.numeric(minScale))
        data<-data %>%dplyr::mutate(minScale=dplyr::case_when(is.na(minScale)~NA_real_,TRUE~minScale))}
      data = data %>% dplyr::select(param,maxScale,minScale)
       return(data)
    }

  addMissing<-function(data){
    if(!any(grepl("\\<scenario\\>",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(scenario="scenario")}else{
      data <- data %>% dplyr::rename(!!"scenario" := (names(data)[grepl("\\<scenario\\>",names(data),ignore.case = T)])[1])
      data<-data%>%dplyr::mutate(scenario=as.character(scenario),scenario=dplyr::case_when(is.na(scenario)~"scenario",TRUE~scenario))}
    if(!any(grepl("\\<scenarios\\>",names(data),ignore.case = T))){}else{
      data <- data %>% dplyr::rename(!!"scenario" := (names(data)[grepl("\\<scenarios\\>",names(data),ignore.case = T)])[1])
      data<-data%>%dplyr::mutate(scenario=as.character(scenario),scenario=dplyr::case_when(is.na(scenario)~"scenario",TRUE~scenario))}
    if(!"x"%in%names(data)){if("year"%in%names(data)){
      data<-data%>%dplyr::mutate(x=year)}else{data<-data%>%dplyr::mutate(x="x")}}
    if(any(grepl("\\<subregion\\>",names(data),ignore.case = T))){
      data <- data %>% dplyr::rename(!!"subRegion" := (names(data)[grepl("\\<subregion\\>",names(data),ignore.case = T)])[1])}
    if(!any(grepl("\\<subregtype\\>",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(subRegType="subRegType")}else{
      data <- data %>% dplyr::rename(!!"subRegType" := (names(data)[grepl("\\<subregtype\\>",names(data),ignore.case = T)])[1])
      data<-data%>%dplyr::mutate(subRegType=as.character(subRegType),subRegType=dplyr::case_when(is.na(subRegType)~"subRegType",TRUE~subRegType))}
    if(!any(grepl("\\<unit\\>",names(data),ignore.case = T))){}else{
      data <- data %>% dplyr::rename(!!"units" := (names(data)[grepl("\\<unit\\>",names(data),ignore.case = T)])[1])
      data<-data%>%dplyr::mutate(units=as.character(units),units=dplyr::case_when(is.na(units)~"units",TRUE~units))}
    if(!any(grepl("\\<units\\>",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(units="units")}else{
      data <- data %>% dplyr::rename(!!"units" := (names(data)[grepl("\\<units\\>",names(data),ignore.case = T)])[1])
      data<-data%>%dplyr::mutate(units=as.character(units),units=dplyr::case_when(is.na(units)~"units",TRUE~units))}
    if(!any(grepl("\\<region\\>",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(region="region")}else{
      data <- data %>% dplyr::rename(!!"region" := (names(data)[grepl("\\<region\\>",names(data),ignore.case = T)])[1])
      data<-data%>%dplyr::mutate(region=as.character(region),region=dplyr::case_when(is.na(region)~"region",TRUE~region))}
    if(!any(grepl("\\<class\\>",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(class="class")}else{
      data <- data %>% dplyr::rename(!!"class" := (names(data)[grepl("\\<class\\>",names(data),ignore.case = T)])[1])
      data<-data%>%dplyr::mutate(class=as.character(class),class=dplyr::case_when(is.na(class)~"class",TRUE~class))}
    if(!any(grepl("\\<regions\\>",names(data),ignore.case = T))){}else{
      data <- data %>% dplyr::rename(!!"region" := (names(data)[grepl("\\<regions\\>",names(data),ignore.case = T)])[1])
      data<-data%>%dplyr::mutate(region=as.character(region),region=dplyr::case_when(is.na(region)~"region",TRUE~region))}
    if(!any(grepl("\\<classpalette\\>",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(classPalette="pal_hot")}else{
      data <- data %>% dplyr::rename(!!"classPalette" := (names(data)[grepl("\\<classpalette\\>",names(data),ignore.case = T)])[1])
      data<-data%>%dplyr::mutate(classPalette=as.character(classPalette),classPalette=dplyr::case_when(is.na(classPalette)~"pal_hot",TRUE~classPalette))}
    if(!any(grepl("\\<param\\>",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(param="param")}else{
      data <- data %>% dplyr::rename(!!"param" := (names(data)[grepl("\\<param\\>",names(data),ignore.case = T)])[1])
      data<-data%>%dplyr::mutate(param=as.character(param),param=dplyr::case_when(is.na(param)~"param",TRUE~param))}
    if(!any(grepl("\\<params\\>",names(data),ignore.case = T))){}else{
      data <- data %>% dplyr::rename(!!"param" := (names(data)[grepl("\\<params\\>",names(data),ignore.case = T)])[1])
      data<-data%>%dplyr::mutate(param=as.character(param),param=dplyr::case_when(is.na(param)~"params",TRUE~param))}
    if(!any(grepl("\\<multiFacetCol\\>",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(multiFacetCol="multiFacetCol")}else{
      data <- data %>% dplyr::rename(!!"multiFacetCol" := (names(data)[grepl("\\<multiFacetCol\\>",names(data),ignore.case = T)])[1])
      data<-data %>%dplyr::mutate(multiFacetCol=dplyr::case_when(is.na(multiFacetCol)~"multiFacetCol",TRUE~multiFacetCol))}
    if(!any(grepl("\\<multiFacetRow\\>",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(multiFacetRow="multiFacetRow")}else{
      data <- data %>% dplyr::rename(!!"multiFacetRow" := (names(data)[grepl("\\<multiFacetRow\\>",names(data),ignore.case = T)])[1])
      data<-data%>%dplyr::mutate(multiFacetRow=dplyr::case_when(is.na(multiFacetRow)~"multiFacetRow",TRUE~multiFacetRow))}
    return(data)
  }



  if(is.null(gridTable) & is.null(polygonTable)){
    stop ("Both gridTable and polygonTable are Null. Need to provide atleast one of the two.")
  }


  } # Close custom functions

  #------------------
  # Create Folders
  #------------------

  if(T){

  mapFolder <- gsub(" ","",paste("Maps",nameAppend,sep=""))

  if (!dir.exists(dirOutputsX)){dir.create(dirOutputsX)}
  if (!dir.exists(paste(dirOutputsX,"/",folderName, sep = ""))){
    dir.create(paste(dirOutputsX,"/",folderName, sep = ""))}

  if (!dir.exists(paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",sep = ""))){
    dir.create(paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",sep = ""))}
  } # Close create folders

  #------------------
  # Read in grid Tables (Either csv tables or an R Table)
  #------------------

  if(T){

  gridTbl<-tibble::tibble()


  if(!is.null(gridTable)){

    if(all(!class(gridTable) %in% c("tbl_df","tbl","data.frame"))){

      for(grid_i in gridTable){
        if(file.exists(grid_i)){
          gridTblNew<-data.table::fread(paste(grid_i),encoding="Latin-1")%>%tibble::as_tibble()
          gridTbl<-dplyr::bind_rows(gridTbl,gridTblNew)
          rm(gridTblNew)
        } else {stop(paste(grid_i," does not exist"))}
      }

      # Join relevant colors and classes using the mapping file if it exists
      if(!"classPalette" %in% names(gridTbl)){
        if(file.exists(paste(getwd(),"/dataFiles/mapping/template_subRegional_mapping.csv", sep = ""))){
          map<-data.table::fread(paste(getwd(),"/dataFiles/mapping/template_subRegional_mapping.csv", sep = ""),encoding="Latin-1")%>%tibble::as_tibble()
          gridTbl<-gridTbl%>%dplyr::left_join(map,by=c("param","units","class"))
        }}

    }else{gridTbl<-gridTable}

  }else{gridTbl=gridTable}


  if(!is.null(gridTbl)){
  if(nrow(gridTbl)>0){

    # Add missing columns
    gridTbl<-addMissing(gridTbl)

    if(!"value" %in% names(gridTbl)){stop("'value' column not present in grid data provided. Need to have values. Check data.",sep="")}
    if(!"lat" %in% names(gridTbl)){stop("'lat' column not present in grid data provided. Need to have lat. Check data.",sep="")}
    if(!"lon" %in% names(gridTbl)){stop("'lon' column not present in grid data provided. Need to have lat. Check data.",sep="")}

    if(min(gridTbl$value,na.rm=T)<0 & max(gridTbl$value,na.rm=T)>0){

      gridTbl <- gridTbl %>% dplyr::mutate(classPalette=dplyr::case_when(is.na(classPalette)~"pal_div_BlRd",
                                                                           classPalette=="pal_metis"~"pal_div_BlRd",
                                                                           classPalette=="pal_16"~"pal_div_BlRd",
                                                                           TRUE~classPalette),
                                             subRegion=as.character(subRegion))


      if(is.null(classPaletteOrig)){gridTbl <- gridTbl %>% dplyr::mutate(classPalette=classPaletteDiff)}

    } else {

      gridTbl <- gridTbl %>% dplyr::mutate(classPalette=dplyr::case_when(is.na(classPalette)~"pal_hot",
                                                                           classPalette=="pal_metis"~"pal_hot",
                                                                           classPalette=="pal_16"~"pal_hot",
                                                                           TRUE~classPalette),
                                             subRegion=as.character(subRegion))
    }

  #Set classPalette based on metis.mappings()$mapParamQuery
  gridTbl <- gridTbl %>%
    dplyr::left_join(metis.mappings()$mapParamQuery, by=c("param")) %>%
    dplyr::mutate(classPalette=dplyr::case_when(!is.na(mapPalette)~mapPalette,
                                         TRUE~classPalette))

    # Set classPalette if given
  if(!is.null(classPaletteOrig)){
    gridTbl <- gridTbl %>% dplyr::mutate(classPalette = classPaletteOrig)
  }

  }}

  } # Close read in Grid Tables

  #------------------
  # Read in shape Tables (Either csv tables or R table
  #------------------

  if(T){

  shapeTbl<-tibble::tibble()

  if(!is.null(polygonTable)){

    if(all(!class(polygonTable) %in% c("tbl_df","tbl","data.frame"))){
      if(class(polygonTable)!="character"){stop("polygonTable neither .csv file path nor dataframe or tibble")}
      for(i in polygonTable){
        if(file.exists(i)){
          shapeTblNew<-data.table::fread(paste(i),encoding="Latin-1")%>%tibble::as_tibble()

          # Join relevant colors and classes using the mapping file if it exists
          if(!"classPalette" %in% names(shapeTblNew)){
            if(file.exists(paste(getwd(),"/dataFiles/mapping/template_subRegional_mapping.csv", sep = ""))){
              map<-data.table::fread(paste(getwd(),"/dataFiles/mapping/template_subRegional_mapping.csv", sep = ""),encoding="Latin-1")%>%tibble::as_tibble()
              shapeTblNew<-shapeTblNew%>%dplyr::left_join(map,by=c("param","units","class"))
            }else{"subregional mapping not found. Using defaults."}}

          shapeTbl<-dplyr::bind_rows(shapeTbl,shapeTblNew)

          if(!"subRegion" %in% names(shapeTbl)){stop(paste("Column subRegion not present in polygonTable ",i,sep=""))}

        } else {stop(paste(i," does not exist"))}
      }

    }else{shapeTbl<-polygonTable}}


  if(!is.null(shapeTbl)){
    if(nrow(shapeTbl)>0){

      # Add missing columns
      shapeTbl<-addMissing(shapeTbl)

      if(!"value" %in% names(shapeTbl)){stop("'value' column not present in polygon data provided. Need to have values. Check data.",sep="")}

      if(min(shapeTbl$value,na.rm=T)<0 & max(shapeTbl$value,na.rm=T)>0){

        shapeTbl <- shapeTbl %>% dplyr::mutate(classPalette=dplyr::case_when(is.na(classPalette)~classPaletteDiff,
                                                                             classPalette=="pal_metis"~classPaletteDiff,
                                                                             classPalette=="pal_16"~classPaletteDiff,
                                                                             TRUE~classPalette),
                                               subRegion=as.character(subRegion))

        if(is.null(classPaletteOrig)){shapeTbl <- shapeTbl %>% dplyr::mutate(classPalette=classPaletteDiff)}

      } else {

      shapeTbl <- shapeTbl %>% dplyr::mutate(classPalette=dplyr::case_when(is.na(classPalette)~"pal_hot",
                                                                         classPalette=="pal_metis"~"pal_hot",
                                                                         classPalette=="pal_16"~"pal_hot",
                                                                         TRUE~classPalette),
                                            !!subRegCol:=as.character(get(subRegCol)))
      }


      #Set classPalette based on metis.mappings()$mapParamQuery
      shapeTbl <- shapeTbl %>%
        dplyr::left_join(metis.mappings()$mapParamQuery, by=c("param")) %>%
        dplyr::mutate(classPalette=dplyr::case_when(!is.na(mapPalette)~mapPalette,
                                             TRUE~classPalette))

    # Set classPalette if given
      #if(!is.null(classPaletteOrig)){shapeTbl <- shapeTbl %>% dplyr::mutate(classPalette = classPaletteOrig)}
  }}

  } # Read in SHape Tables


  #------------------
  # Subset Data
  #------------------

  if(T){

  if(!is.null(shapeTbl)){
    if(nrow(shapeTbl)>0){

      # dplyr::mutate shapeTbl data from GCAM to match shapefile subRegions
        shapeTbl <- shapeTbl %>%
          dplyr::mutate(!!subRegCol:=gsub("-","_",get(subRegCol)),
                        !!subRegCol:=gsub("_Basin","",get(subRegCol)))

    if(any(paramsSelect!="All")){
      if(any(paramsSelect %in% unique(shapeTbl$param))){
        shapeTbl<-shapeTbl%>%dplyr::filter(param %in% paramsSelect);
        print(paste("Subset shapeTbl param to paramsSelect: ",paramsSelect,sep=""))}else{
          print(paste("None of the paramsSelect: ",paste(paramsSelect,collapse=", ")," are present in shapeTbl params. Skipping subset.",sep=""))
        }}

      if(!is.null(chosenRefMeanYears)){
        if(!all(chosenRefMeanYears) %in% unique(shapeTbl$x)){print("Not all chosenRefMeanYears were available in data. Only selecting available years: ")
          print(paste(unique(shapeTbl$x)[unique(shapeTbl$x) %in% chosenRefMeanYears], collapse=", "))
          chosenRefMeanYears = unique(shapeTbl$x)[unique(shapeTbl$x) %in% chosenRefMeanYears]
          }
      }

      if(multifacetsOn){
      if(!is.null(chosenRefMeanYears)){
        shapeTblScenMultiABRef <- shapeTbl %>% dplyr::filter(x %in% chosenRefMeanYears)
        print(paste("Subset shapeTblScenMultiABRef x to chosenRefMeanYears: ",paste(chosenRefMeanYears,collapse=", "),sep=""))
      }else{shapeTblScenMultiABRef <-  shapeTbl}
        shapeTblScenMultiABRef <- droplevels(shapeTblScenMultiABRef)
        shapeTblMultiOrig <- shapeTbl
        }

      if(any(xRangeOrig!="All")){shapeTbl<-shapeTbl%>%dplyr::filter(x %in% xRangeOrig);
      print(paste("Subset shapeTbl x to xRange: ",paste(xRangeOrig,collapse=", "),sep=""))}

    shapeTbl<-droplevels(shapeTbl)

    }
  }

  if(!is.null(gridTbl)){
    if(any(xRangeOrig!="All")){gridTbl<-gridTbl%>%dplyr::filter(x %in% xRangeOrig);
    print(paste("Subset gridTbl x to xRange: ",xRangeOrig,sep=""))}
    if(any(paramsSelect!="All")){
      if(any(paramsSelect %in% unique(gridTbl$param))){
        gridTbl<-gridTbl%>%dplyr::filter(param %in% paramsSelect);
        print(paste("Subset gridTbl param to paramsSelect: ",paramsSelect,sep=""))}else{
          print(paste("None of the paramsSelect: ",paste(paramsSelect,collapse=", ")," are present in gridTbl params. Skipping subset.",sep=""))
        }
    }

    gridTbl<-droplevels(gridTbl)
  }


  # Remove NA's & Keep only Unique Values
  if(!is.null(gridTbl)){
    if(nrow(gridTbl)>0){
    #print("Removing NA's and keeping only unique values in gridTbl...")
    gridTbl<-gridTbl%>%dplyr::filter(!is.na(value))%>%dplyr::mutate(value = signif(value,10))%>%dplyr::ungroup()%>%dplyr::distinct()
    #print("Complete.")
    }
  }
  if(!is.null(shapeTbl)){
    if(nrow(shapeTbl)>0){
    #print("Removing NA's and keeping only unique values in shapeTbl...")
    shapeTbl<-shapeTbl%>%
      dplyr::filter(!is.na(value))%>%
      dplyr::filter(!is.na(subRegion))%>%
      dplyr::mutate(value = signif(value,10))%>%
      dplyr::ungroup()%>%
      dplyr::distinct()
    #print("Complete.")
    }
  }
  } # Subset data

  #------------------
  # Compare Scenarios
  #------------------

  if(T){
  # Get Params and Scenarios
  if(!is.null(shapeTbl) & !is.null(gridTbl)){
    if(nrow(shapeTbl)>0 & nrow(gridTbl)>0){
      params <- unique(c(unique(shapeTbl$param),unique(gridTbl$param)))
      scenarios <- unique(c(unique(shapeTbl$scenario),unique(gridTbl$scenario)))
    }
  }else{
    if(!is.null(shapeTbl)){
      if(nrow(shapeTbl)>0){
        params <- unique(c(unique(shapeTbl$param)))
        scenarios <- unique(c(unique(shapeTbl$scenario)))
      }
    } else {
      if(!is.null(gridTbl)){
        if(nrow(gridTbl)>0){
          params <- unique(c(unique(gridTbl$param)))
          scenarios <- unique(c(unique(gridTbl$scenario)))
        }
      }
    }
  }

    if(!is.null(scenRef)){

      if(!any(scenRef %in% scenarios)){
        print(paste("scenRef chosen: ", scenRef, " is not in any of the available scenarios: ",sep=""))
        print(paste(scenarios,collapse=", "))
        print(paste("Setting scenRef to first scenario: ", scenarios[1],".",sep=""))
        scenRef <- scenarios[1]
      }


      if(is.null(scenDiff)){
        scenDiff <- scenarios[!scenarios %in% scenRef]
        print(paste("Running difference against all available scenarios:",sep=""))
        print(paste(scenDiff,collapse=", "))
      }else{
        if(!any(scenDiff %in% scenarios)){
          print(paste("None of the scenDiff are in any of the available scenarios: "))
          print(paste(scenarios[!scenarios %in% scenRef],collapse=", "))
          print(paste("Skipping Diff.",sep=""))
        }
      }


      shapeTblDiff <- tibble::tibble()
      gridTblDiff <- tibble::tibble()

      for(i in 1:length(params)){

        NULL -> param_i -> scenRef_i -> scenDiff_i

        param_i <- params[i]
        scenRef_i <- scenRef
        scenDiff_i <- scenDiff

        if(!is.null(param_i) & !is.null(scenRef_i) & !is.null(scenDiff_i)){

    # Compare Gridded Data
    if(!is.null(gridTbl)){

      gridTblDiffa <- gridTbl %>% dplyr::filter(param==param_i & (scenario %in% c(scenRef_i,scenDiff_i)))

      if(length(unique(gridTblDiffa$scenario))>1){


        if(scenRef_i %in% unique(gridTblDiffa$scenario)){
          print(paste("Ref scenario chosen for param: ", param_i, " is ", paste(scenDiff_i,collapse=", "),sep=""))
        if(any(scenDiff_i %in% unique(gridTblDiffa$scenario))){
          print(paste("Diff scenarios chosen for param: ", param_i, " are ",
                      paste(scenDiff_i[scenDiff_i %in% unique(gridTblDiffa$scenario)],collapse=", "),sep=""))}


        scenDiff_i <- scenDiff_i[scenDiff_i %in% unique(gridTblDiffa$scenario)]

       # Calculate Diff Values

      gridTblDiffb<-gridTblDiffa%>%dplyr::filter(param==param_i, scenario %in% c(scenRef_i,scenDiff_i))%>%
        dplyr::select(lat,lon,subRegType,param,x,xLabel,vintage,units,aggregate,classPalette,class,scenario,value)%>%
        tidyr::spread(scenario,value)


      for (scenario_i in unique(gridTblDiffa$scenario)[unique(gridTblDiffa$scenario) %in% scenDiff_i]){
        tbl_temp1 <-gridTblDiffb%>%
          dplyr::mutate(!!paste("DiffAbs_",scenario_i,"_",scenRef_i,sep=""):=get(scenario_i)-get(scenRef_i),
                        classPalette=classPaletteDiff)%>%
          dplyr::select(-dplyr::one_of(as.vector(unique(gridTblDiffa$scenario))))
        tbl_temp1<-tbl_temp1%>%
          tidyr::gather(key=scenario,value=value,
                        -c(names(tbl_temp1)[!names(tbl_temp1) %in% paste("DiffAbs_",scenario_i,"_",scenRef_i,sep="")]))%>%
          dplyr::filter(!is.na(value))

        tbl_temp2 <-gridTblDiffb%>%
          dplyr::mutate(!!paste("DiffPrcnt_",scenario_i,"_",scenRef_i,sep=""):=((get(scenario_i)-get(scenRef_i))*100/get(scenRef_i)),
                        classPalette=classPaletteDiff)%>%
          dplyr::select(-dplyr::one_of(as.vector(unique(gridTblDiffa$scenario))))
        tbl_temp2<-tbl_temp2%>%
          tidyr::gather(key=scenario,value=value,
                        -c(names(tbl_temp2)[!names(tbl_temp2) %in% paste("DiffPrcnt_",scenario_i,"_",scenRef_i,sep="")]))%>%
          dplyr::filter(!is.na(value))

        gridTblDiff<-dplyr::bind_rows(gridTblDiff,tbl_temp1,tbl_temp2)
      }
    }}
  }


  # Compare Shape Data

  if(!is.null(shapeTbl) & nrow(shapeTbl)>0){

      shapeTblDiffa <- shapeTbl %>% dplyr::filter(param==param_i & (scenario %in% c(scenRef_i,scenDiff_i)));shapeTblDiffa

      if(length(unique(shapeTblDiffa$scenario))>1){
      # Calculate Diff Values

        if(scenRef_i %in% unique(shapeTblDiffa$scenario)){
          print(paste("Ref scenario chosen for param: ", param_i, " is ", paste(scenRef_i,collapse=", "),sep=""))
        if(any(scenDiff_i %in% unique(shapeTblDiffa$scenario))){
          print(paste("Diff scenarios chosen for param: ", param_i, " are ",
                      paste(scenDiff_i[scenDiff_i %in% unique(shapeTblDiffa$scenario)],collapse=", "),sep=""))}

        scenDiff_i <- scenDiff_i[scenDiff_i %in% unique(shapeTblDiffa$scenario)]

      shapeTblDiffb<-shapeTbl%>%dplyr::filter(param==param_i, scenario %in% c(scenRef_i,scenDiff_i))%>%
        dplyr::select(region,subRegion,subRegType,param,x,xLabel,vintage,units,aggregate,classPalette,class,scenario,value)%>%
        tidyr::spread(scenario,value);shapeTblDiffb%>%as.data.frame(); names(shapeTblDiffb)

      for (scenario_i in unique(shapeTbl$scenario)[(unique(shapeTbl$scenario) %in% scenDiff_i)]){
        tbl_temp1 <-shapeTblDiffb%>%
          dplyr::mutate(!!paste("DiffAbs_",scenario_i,"_",scenRef_i,sep=""):=get(scenario_i)-get(scenRef_i),
                        classPalette=classPaletteDiff)%>%
          dplyr::select(-dplyr::one_of(as.vector(unique(shapeTblDiffa$scenario)[unique(shapeTblDiffa$scenario) %in% c(scenRef_i,scenDiff_i)])))
        tbl_temp1<-tbl_temp1%>%
          tidyr::gather(key=scenario,value=value,
                        -c(names(tbl_temp1)[!names(tbl_temp1) %in% paste("DiffAbs_",scenario_i,"_",scenRef_i,sep="")]))%>%
          dplyr::filter(!is.na(value))

        tbl_temp2 <-shapeTblDiffb%>%
          dplyr::mutate(!!paste("DiffPrcnt_",scenario_i,"_",scenRef_i,sep=""):=((get(scenario_i)-get(scenRef_i))*100/get(scenRef_i)),
                        classPalette=classPaletteDiff)%>%
          dplyr::select(-dplyr::one_of(as.vector(unique(shapeTblDiffa$scenario)[unique(shapeTblDiffa$scenario) %in% c(scenRef_i,scenDiff_i)])))
        tbl_temp2<-tbl_temp2%>%
          tidyr::gather(key=scenario,value=value,
                        -c(names(tbl_temp2)[!names(tbl_temp2) %in% paste("DiffPrcnt_",scenario_i,"_",scenRef_i,sep="")]))%>%
          dplyr::filter(!is.na(value))

        shapeTblDiff<-dplyr::bind_rows(shapeTblDiff,tbl_temp1,tbl_temp2)
      }
        }
      }
  }
        }
      }
      }

    if(!is.null(xRef)){

      if(!any(xRangeOrig=="All")){
        if(any(class(xRangeOrig)!="numeric")){
          paste("xRange provided is not numeric setting to All.")
          xRange = unique(shapeTbl$x)
        }else{
          xRange = unique(shapeTbl$x)
        }
      }else{
        xRange = unique(shapeTbl$x)
      }

      if(!any(xRef %in% xRange)){
        print(paste("xRef chosen: ", xRef, " is not in any of the available xRange values: ",sep=""))
        print(paste(xRange,collapse=", "))
        print(paste("Setting xRef to first xRange value: ", xRange[1],".",sep=""))
        xRef <- xRange[1]
      }


      if(is.null(xDiff)){
        xDiff <- xRange[!xRange %in% xRef]
        print(paste("Running difference against all available xRange:",sep=""))
        print(paste(xDiff,collapse=", "))
      }else{
        if(!any(xDiff %in% xRange)){
          print(paste("None of the xDiff are in any of the available scenarios: "))
          print(paste(xRange[!xRange %in% xRef],collapse=", "))
          print(paste("Skipping x Range Diff.",sep=""))
        }
      }

      shapeTblDiffx <- tibble::tibble()
      gridTblDiffx <- tibble::tibble()

      for(i in 1:length(params)){

        NULL -> param_i -> xRef_i -> xDiff_i

        param_i <- params[i]
        xRef_i <- xRef
        xDiff_i <- xDiff

        if(!is.null(param_i) & !is.null(xRef_i) & !is.null(xDiff_i)){

          # Compare Shape Data

          if(!is.null(shapeTbl) & nrow(shapeTbl)>0){

            shapeTblDiffa <- shapeTbl %>% dplyr::filter(param==param_i & (x %in% c(xRef_i,xDiff_i)));shapeTblDiffa

            if(nrow(shapeTblDiffa)>0){
              # Calculate Diff Values

              if(xRef_i %in% unique(shapeTblDiffa$x)){
                print(paste("Ref x chosen for param: ", param_i, " is ", paste(xRef_i,collapse=", "),sep=""))
              if(any(xDiff_i %in% unique(shapeTblDiffa$x))){
                print(paste("Diff x chosen for param: ", param_i, " are ",
                            paste(xDiff_i[xDiff_i %in% unique(shapeTblDiffa$x)],collapse=", "),sep=""))}

              xDiff_i <- xDiff_i[xDiff_i %in% unique(shapeTblDiffa$x)]

              colsx <- c("region","subRegion","subRegType","param","x","xLabel","units","aggregate","classPalette","class","scenario","value")
              colsx1 <- names(shapeTbl)[names(shapeTbl) %in% colsx]; colsx1
              colsx2 <- colsx1[!colsx1 %in% "value"]; colsx2
              shapeTblDiffb<-shapeTbl%>%dplyr::filter(param==param_i, x %in% c(xRef_i,xDiff_i))%>%
                dplyr::select(dplyr::all_of(colsx1))%>%
                dplyr::group_by_at(colsx2)%>%
                dplyr::summarize(value=sum(value,na.rm=T))%>%
                tidyr::spread(x,value);shapeTblDiffb%>%as.data.frame(); names(shapeTblDiffb)

              for (scen_i in scenarios){
              for (x_i in unique(shapeTbl$x)[(unique(shapeTbl$x) %in% xDiff_i)]){
                tbl_temp1 <-shapeTblDiffb%>%
                  dplyr::filter(scenario==scen_i)%>%
                  dplyr::mutate(!!paste(scen_i,"_DiffxAbs_",xRef_i,sep=""):=(!!as.name(x_i)-!!as.name(xRef_i)),
                                classPalette=classPaletteDiff)%>%
                  dplyr::select(-!!as.name(xDiff_i),-!!as.name(xRef_i))
                tbl_temp1<-tbl_temp1%>%
                  tidyr::gather(key=scenario,value=value,
                                -c(names(tbl_temp1)[!names(tbl_temp1) %in% paste(scen_i,"_DiffxAbs_",xRef_i,sep="")]))%>%
                  dplyr::filter(!is.na(value))%>%
                  dplyr::mutate(x=x_i)

                tbl_temp2 <-shapeTblDiffb%>%
                  dplyr::filter(scenario==scen_i)%>%
                  dplyr::mutate(!!paste(scen_i,"_DiffxPrcnt_",xRef_i,sep=""):=((!!as.name(x_i)-!!as.name(xRef_i))*100/!!as.name(x_i)),
                                classPalette=classPaletteDiff)%>%
                  dplyr::select(-!!as.name(xDiff_i),-!!as.name(xRef_i))
                tbl_temp2<-tbl_temp2%>%
                  tidyr::gather(key=scenario,value=value,
                                -c(names(tbl_temp2)[!names(tbl_temp2) %in% paste(scen_i,"_DiffxPrcnt_",xRef_i,sep="")]))%>%
                  dplyr::filter(!is.na(value))%>%
                  dplyr::mutate(x=x_i)

                shapeTblDiffx<-dplyr::bind_rows(shapeTblDiffx,tbl_temp1,tbl_temp2)
              }
            }# Close Scenario
            }}
          }
        }
      }
    }

  shapeTbl <- shapeTbl %>% dplyr::bind_rows(shapeTblDiffx)%>% dplyr::bind_rows(shapeTblDiff) %>% dplyr::ungroup() %>% dplyr::distinct();
  gridTbl <- gridTbl %>% dplyr::bind_rows(gridTblDiffx)%>% dplyr::bind_rows(gridTblDiff) %>% dplyr::ungroup() %>% dplyr::distinct();

} # Compare Scenarios


  #------------------
  # Assign MultiFacet Columns
  #------------------

  if(T){


  if(!is.null(shapeTbl)){
    if(nrow(shapeTbl)>0){
    shapeTbl <- shapeTbl %>%
      dplyr::mutate(multiFacetCol=dplyr::case_when(multiFacetCol!="multiFacetCol"~multiFacetCol,
                    TRUE~"MultiANone"),
                    multiFacetRow=dplyr::case_when(multiFacetRow!="multiFacetRow"~multiFacetRow,
                                            TRUE~"MultiBNone"))
    shapeTbl<-shapeTbl%>%dplyr::mutate(!!subRegCol:=as.character(get(subRegCol)))
    shapeTbl<-droplevels(shapeTbl)
    }
  }


  if(!is.null(shapeTblScenMultiABRef)){
    if(nrow(shapeTblScenMultiABRef)>0){
      shapeTblScenMultiABRef <- shapeTblScenMultiABRef %>%
        dplyr::mutate(multiFacetCol=dplyr::case_when(multiFacetCols!="multiFacetCol"~!!as.name(multiFacetCols),
                                              TRUE~"MultiANone"),
                      multiFacetRow=dplyr::case_when(multiFacetRows!="multiFacetRow"~!!as.name(multiFacetRows),
                                              TRUE~"MultiBNone"))
      shapeTblScenMultiABRef<-shapeTblScenMultiABRef%>%dplyr::mutate(!!subRegCol:=as.character(get(subRegCol)))
      shapeTblScenMultiABRef<-droplevels(shapeTblScenMultiABRef)
    }
  }


  if(!is.null(shapeTblMultiOrig)){
    if(nrow(shapeTblMultiOrig)>0){
      shapeTblMultiOrig <- shapeTblMultiOrig %>%
        dplyr::mutate(multiFacetCol=dplyr::case_when(multiFacetCols!="multiFacetCol"~!!as.name(multiFacetCols),
                                              TRUE~"MultiANone"),
                      multiFacetRow=dplyr::case_when(multiFacetRows!="multiFacetRow"~!!as.name(multiFacetRows),
                                              TRUE~"MultiBNone"))
      shapeTblMultiOrig<-shapeTblMultiOrig%>%dplyr::mutate(!!subRegCol:=as.character(get(subRegCol)))
      shapeTblMultiOrig<-droplevels(shapeTblMultiOrig)
    }
  }

  if(!is.null(gridTbl)){
    if(nrow(gridTbl)>0){
      gridTbl <- gridTbl %>%
        dplyr::mutate(multiFacetCol=dplyr::case_when(multiFacetCols!="multiFacetCol"~!!as.name(multiFacetCols),
                                              TRUE~"MultiANone"),
                      multiFacetRow=dplyr::case_when(multiFacetRows!="multiFacetRow"~!!as.name(multiFacetRows),
                                              TRUE~"MultiBNone"))
    gridTbl<-droplevels(gridTbl)
    }
  }

  } # Assign MultiFacet Columns


  #----------------
  # Check scaleRanges
  #---------------

  if(T){

    # Get list of params in grid or shape data
    if(!is.null(gridTbl)){if(nrow(gridTbl)>0){paramsGrid <- unique(gridTbl$param)}}
    if(!is.null(shapeTbl)){if(nrow(shapeTbl)>0){paramsShape <- unique(shapeTbl$param)}}
    paramsRange <- unique(c(paramsGrid,paramsShape)); paramsRange


    if(!is.null(scaleRange)){
      # Scale Range
      scaleRange[is.na(scaleRange)]<-NA_real_
      scaleRange[scaleRange=="NA"]<-NA_real_
      # If scale range is a vector of two numbers set as limits for all params
      if(is.numeric(scaleRange) & length(scaleRange)==2){
        scaleRange = data.frame(param=paramsRange,maxScale=max(scaleRange),minScale=min(scaleRange))
      } else {
        # Else format the scaleRange data frame as needed
        if(!is.null(nrow(scaleRange))){
          scaleRange = addMissingScale(scaleRange)
          if(!any(unique(scaleRange$param) %in% paramsRange)){
            print(paste("None of the params in scaleRange: ",
                        paste(unique(scaleRange$param),collapse=", "),sep=""))
            print("are present in the data params:")
            print(paste(paramsRange,collapse=", "))
            print("Setting scaleRange to NULL")
            scaleRange=NULL
          }
        }else{scaleRange=NULL}
      }
    }

      # Scale Range Diff Abs
    if(!is.null(scaleRangeDiffAbs)){

      scaleRangeDiffAbs[is.na(scaleRangeDiffAbs)]<-NA_real_
      scaleRangeDiffAbs[scaleRangeDiffAbs=="NA"]<-NA_real_
      # If scale range is a vector of two numbers set as limits for all params
      if(is.numeric(scaleRangeDiffAbs) & length(scaleRangeDiffAbs)==2){
        scaleRangeDiffAbs = data.frame(param=paramsRange,maxScale=max(scaleRangeDiffAbs),minScale=min(scaleRangeDiffAbs))
      } else {
        if(!is.null(nrow(scaleRangeDiffAbs))){
          scaleRangeDiffAbs = addMissingScale(scaleRangeDiffAbs)
          if(!any(unique(scaleRangeDiffAbs$param) %in% paramsRange)){
            print(paste("None of the params in scaleRangeDiffAbs: ",
                        paste(unique(scaleRangeDiffAbs$param),collapse=", "),sep=""))
            print("are present in the data params:")
            print(paste(paramsRange,collapse=", "))
            print("Setting scaleRangeDiffAbs to NULL")
            scaleRangeDiffAbs=NULL
          }
        }else{scaleRangeDiffAbs=NULL}
      }
    }

    if(!is.null(scaleRangeDiffPrcnt)){
      # Scale Range Diff Prcnt
      scaleRangeDiffPrcnt[is.na(scaleRangeDiffPrcnt)]<-NA_real_
      scaleRangeDiffPrcnt[scaleRangeDiffPrcnt=="NA"]<-NA_real_
      # If scale range is a vector of two numbers set as limits for all params
      if(is.numeric(scaleRangeDiffPrcnt) & length(scaleRangeDiffPrcnt)==2){
        scaleRangeDiffPrcnt = data.frame(param=paramsRange,maxScale=max(scaleRangeDiffPrcnt),minScale=min(scaleRangeDiffPrcnt))
      } else {
        # Else format the scaleRangeDiffPrcnt data frame as needed
        if(!is.null(nrow(scaleRangeDiffPrcnt))){
          scaleRangeDiffPrcnt = addMissingScale(scaleRangeDiffPrcnt)
          if(!any(unique(scaleRangeDiffPrcnt$param) %in% paramsRange)){
            print(paste("None of the params in scaleRangeDiffPrcnt: ",
                        paste(unique(scaleRangeDiffPrcnt$param),collapse=", "),sep=""))
            print("are present in the data params:")
            print(paste(paramsRange,collapse=", "))
            print("Setting scaleRangeDiffPrcnt to NULL")
            scaleRangeDiffPrcnt=NULL
            }
        }else{scaleRangeDiffPrcnt=NULL}
      }
  }

    # Scale Range Diff Abs
    if(!is.null(scaleRangeDiffxAbs)){

      scaleRangeDiffxAbs[is.na(scaleRangeDiffxAbs)]<-NA_real_
      scaleRangeDiffxAbs[scaleRangeDiffxAbs=="NA"]<-NA_real_
      # If scale range is a vector of two numbers set as limits for all params
      if(is.numeric(scaleRangeDiffxAbs) & length(scaleRangeDiffxAbs)==2){
        scaleRangeDiffxAbs = data.frame(param=paramsRange,maxScale=max(scaleRangeDiffxAbs),minScale=min(scaleRangeDiffxAbs))
      } else {
        if(!is.null(nrow(scaleRangeDiffxAbs))){
          scaleRangeDiffxAbs = addMissingScale(scaleRangeDiffxAbs)
          if(!any(unique(scaleRangeDiffxAbs$param) %in% paramsRange)){
            print(paste("None of the params in scaleRangeDiffxAbs: ",
                        paste(unique(scaleRangeDiffxAbs$param),collapse=", "),sep=""))
            print("are present in the data params:")
            print(paste(paramsRange,collapse=", "))
            print("Setting scaleRangeDiffxAbs to NULL")
            scaleRangeDiffxAbs=NULL
          }
        }else{scaleRangeDiffxAbs=NULL}
      }
    }

    if(!is.null(scaleRangeDiffxPrcnt)){
      # Scale Range Diff Prcnt
      scaleRangeDiffxPrcnt[is.na(scaleRangeDiffxPrcnt)]<-NA_real_
      scaleRangeDiffxPrcnt[scaleRangeDiffxPrcnt=="NA"]<-NA_real_
      # If scale range is a vector of two numbers set as limits for all params
      if(is.numeric(scaleRangeDiffxPrcnt) & length(scaleRangeDiffxPrcnt)==2){
        scaleRangeDiffxPrcnt = data.frame(param=paramsRange,maxScale=max(scaleRangeDiffxPrcnt),minScale=min(scaleRangeDiffxPrcnt))
      } else {
        # Else format the scaleRangeDiffxPrcnt data frame as needed
        if(!is.null(nrow(scaleRangeDiffxPrcnt))){
          scaleRangeDiffxPrcnt = addMissingScale(scaleRangeDiffxPrcnt)
          if(!any(unique(scaleRangeDiffxPrcnt$param) %in% paramsRange)){
            print(paste("None of the params in scaleRangeDiffxPrcnt: ",
                        paste(unique(scaleRangeDiffxPrcnt$param),collapse=", "),sep=""))
            print("are present in the data params:")
            print(paste(paramsRange,collapse=", "))
            print("Setting scaleRangeDiffxPrcnt to NULL")
            scaleRangeDiffxPrcnt=NULL
          }
        }else{scaleRangeDiffxPrcnt=NULL}
      }
    }

    }# Close Check Scale Range


  #--------------------
  # Crop To Boundary
  #--------------------

  if(cropToBoundary){
    if(is.null(boundaryRegionsSelect)){
      if(!is.null(shapeTbl)){
        if(nrow(shapeTbl)>0){
          boundaryRegionsSelect <- unique(shapeTbl$subRegion)
        }
      }
    }
  }

  #------------------
  # Rename SubRegions and SubRegtype
  #------------------

  if(T){
    if(!is.null(shapeTbl)){
      if(nrow(shapeTbl)>0){
        if(!is.null(subRegCol)){
        shapeTbl <- shapeTbl %>% dplyr::rename(subRegion=!!as.name(subRegCol))
        }
        if(!is.null(subRegType)){
          shapeTbl <- shapeTbl %>% dplyr::mutate(subRegType=!!subRegType)
        }

      }
    }
  }


  # -------------------
  # Create Raster Plots
  # -------------------

  if(T){

  if(!is.null(gridTbl)){
    if(nrow(gridTbl)>0){

      gridTblOrig <- gridTbl

      if(!length(unique(gridTblOrig$x))>1){animateOn=F}


      for (scenario_i in unique(gridTblOrig$scenario)){
        for (param_i in unique(gridTblOrig$param)){

          if(nrow(gridTblOrig%>%dplyr::filter(param==param_i,scenario==scenario_i))>0){

          gridTbl <- gridTblOrig%>%dplyr::filter(param==param_i,scenario==scenario_i)

          #------------------
          # Create Grid Table Folders If Needed
          #------------------
          if(T){

                if (!dir.exists(paste(dirOutputsX,"/",folderName, "/",mapFolder,"/raster",sep = ""))){
                  dir.create(paste(dirOutputsX,"/",folderName, "/",mapFolder,"/raster",sep = ""))}

                  if (!dir.exists(paste(dirOutputsX,"/",folderName, "/",mapFolder,"/raster/",param_i,sep = ""))){
                    dir.create(paste(dirOutputsX,"/",folderName, "/",mapFolder,"/raster/",param_i,sep = ""))}

                  if(multifacetsOn==T){
                    if (!dir.exists(paste(dirOutputsX,"/",folderName, "/",mapFolder,"/raster/",param_i,"/compareMultiFacets",sep = ""))){
                      dir.create(paste(dirOutputsX,"/",folderName, "/",mapFolder,"/raster/",param_i,"/compareMultiFacets",sep = ""))}}

                    if (!dir.exists(paste(dirOutputsX,"/",folderName, "/",mapFolder,"/raster/",param_i,"/",scenario_i,sep = ""))){
                      dir.create(paste(dirOutputsX, "/",folderName, "/",mapFolder,"/raster/",param_i,"/",scenario_i,sep = ""))}

                    if (!dir.exists(paste(dirOutputsX,"/",folderName, "/",mapFolder,"/raster/",param_i,"/",scenario_i,"/byYear",sep = ""))){
                      dir.create(paste(dirOutputsX, "/",folderName, "/",mapFolder,"/raster/",param_i,"/",scenario_i,"/byYear",sep = ""))}
          } # Create grid table folder if needed


          #------------------
          # Read in shape files
          #------------------

            if(!is.null(shapeTbl) & nrow(shapeTbl)>0){

              subRegShape <- NULL

              if(all(is.null(subRegShapeOrig) & is.null(subRegShpFileOrig))){

                runSection = T
                if(is.null(subRegShape)){
                  print(paste("For the selected param: ", param_i," and scenario: ", scenario_i,sep=""))
                  print("None of the pre-loaded shapefiles contain all the subRgions specified in a single shapefile.")
                  print(paste("subRegions missing from shapefile: ",
                              paste(unique((shapeTbl%>%dplyr::filter(param==param_i,scenario==scenario_i))$subRegion)[
                                !unique((shapeTbl%>%dplyr::filter(param==param_i,scenario==scenario_i))$subRegion) %in%
                                  unique(subRegShape@data$subRegion)
                              ],collapse=", "),sep=""))
                  print(paste("Skipping map for param: ", param_i," and scenario: ", scenario_i,sep=""))
                  print("Please load a subRegShape directly for your region of interest.")
                  runSection = F
                }

              } else {
                if(nrow(shapeTbl%>%dplyr::filter(param==param_i,scenario==scenario_i))>0){
                  subRegType_ix <- unique((shapeTbl%>%dplyr::filter(param==param_i,scenario==scenario_i))$subRegType)
                }else{subRegType_ix="subRegion"}
                subRegShape <- subRegShapeOrig
              }

              if(runSection){

                if(is.null(boundaryRegShapeOrig)){
                  if(!is.null(boundaryRegShpFolderOrig) & !is.null(boundaryRegShpFileOrig)){
                    if(!dir.exists(boundaryRegShpFolderOrig)){
                      stop("Shapefile folder: ", boundaryRegShpFolderOrig ," is incorrect or doesn't exist.",sep="")}
                    if(!file.exists(paste(boundaryRegShpFolderOrig,"/",boundaryRegShpFileOrig,".shp",sep=""))){
                      stop("Shape file: ", paste(boundaryRegShpFolderOrig,"/",boundaryRegShpFileOrig,".shp",sep="")," is incorrect or doesn't exist.",sep="")}
                    boundaryRegShape=rgdal::readOGR(dsn=boundaryRegShpFolderOrig,layer=boundaryRegShpFileOrig,use_iconv=T,encoding='UTF-8')
                    print(paste("Sub Reg Shape : ",boundaryRegShpFolderOrig,"/",boundaryRegShpFileOrig,".shp",sep=""))
                    print(raster::head(boundaryRegShape))
                  } else {
                    # If only boundary regionsSelect have been chosen then try and find a shapefile with those regions
                    if(!is.null(boundaryRegionsSelect)){
                      mapFound <- metis.mapFind(data.frame(subRegion=boundaryRegionsSelect))
                      boundaryRegShape <- mapFound$subRegShapeFound
                      boundaryRegionsSelect <- unique(mapFound$dataTblFound$subRegion)
                    }else{
                      mapFound <- metis.mapFind(data.frame(subRegion=unique(shapeTbl$subRegion)))
                      boundaryRegShape <- mapFound$subRegShapeFound
                    }
                  }
                }


            if(is.null(subRegShape)){
              if(!is.null(subRegShpFolderOrig) & !is.null(subRegShpFileOrig)){
                if(!dir.exists(subRegShpFolderOrig)){
                  stop("Shapefile folder: ", subRegShpFolderOrig ," is incorrect or doesn't exist.",sep="")}
                if(!file.exists(paste(subRegShpFolderOrig,"/",subRegShpFileOrig,".shp",sep=""))){
                  stop("Shape file: ", paste(subRegShpFolderOrig,"/",subRegShpFileOrig,".shp",sep="")," is incorrect or doesn't exist.",sep="")}
                subRegShape=rgdal::readOGR(dsn=subRegShpFolderOrig,layer=subRegShpFileOrig,use_iconv=T,encoding='UTF-8')
                print(paste("Sub Reg Shape : ",subRegShpFolderOrig,"/",subRegShpFileOrig,".shp",sep=""))
                print(raster::head(subRegShape))
              } # if(!is.null(subRegShpFolder) & !is.null(subRegShpFile)){
            }


            if(is.null(subRegShape)){
              stop("No valid subregional shape file available")}

            if(!subRegCol %in% names(subRegShape)){stop(paste("SubRegCol: ",subRegCol," not present in subRegShape",sep=""))}

            if(!is.null(subRegShape)){
              subRegShape@data[[subRegCol]] <- as.character(subRegShape@data[[subRegCol]])
              if(!all(unique((shapeTbl%>%dplyr::filter(param==param_i,scenario==scenario_i))$subRegion) %in% unique(subRegShape@data[[subRegCol]]))){
                print(paste("Removing subRegions not present in shapefile from datatable: ",
                            paste(unique((shapeTbl%>%dplyr::filter(param==param_i,scenario==scenario_i))$subRegion)[!unique((shapeTbl%>%dplyr::filter(param==param_i,scenario==scenario_i))$subRegion) %in% unique(subRegShape@data[[subRegCol]])],collapse=", "),sep=""))
                shapeTbl <- shapeTbl%>%dplyr::filter(param==param_i,scenario==scenario_i)%>% dplyr::filter(subRegion %in% unique(subRegShape@data[[subRegCol]]))
                print(paste("Remaining subRegions in dataTable are: ",paste(unique(shapeTbl$subRegion),collapse=", "),sep=""))}
            }

            if(!any(unique((shapeTbl%>%dplyr::filter(param==param_i,scenario==scenario_i))$subRegion) %in% unique(subRegShape@data[[subRegCol]]))){
              print(paste("None of the subRegions in data provided for param: ", param_i,"and scenario: ",scenario_i,
                          " match subRegions in the shapefile provided or available.",sep=""))
            }

            subRegShape@data<-subRegShape@data%>%dplyr::mutate(subRegion=get(subRegCol))

            #----------------
            # Create Boundary and subRegional shapefiles
            #---------------
            if(!is.null(boundaryRegShape) & !is.null(subRegShape)){
              if(!is.null(boundaryRegionsSelect)){
                if(any(boundaryRegionsSelect %in% unique(boundaryRegShape@data[[boundaryRegCol]]))){
                  boundaryRegShapeLimits <- boundaryRegShape[boundaryRegShape@data[[boundaryRegCol]] %in% boundaryRegionsSelect,]
                  boundaryRegShapeLimits@data <- droplevels(boundaryRegShapeLimits@data)
                  bbox1<-as.data.frame(sp::bbox(boundaryRegShapeLimits))
                  bbox1$min;bbox1$max
                  rangeX<-abs(range(bbox1$min[1],bbox1$max[1])[2]-range(bbox1$min[1],bbox1$max[1])[1])
                  rangeY<-abs(range(bbox1$min[2],bbox1$max[2])[2]-range(bbox1$min[2],bbox1$max[2])[1])
                  bbox1$min[1]<-min(180,max(-180,(-rangeX*expandPercent/100)+bbox1$min[1]));
                  bbox1$min[2]<-min(90,max(-90,(-rangeY*expandPercent/100)+bbox1$min[2]));
                  bbox1$max[1]<-max(-180,min(180,(rangeX*expandPercent/100)+bbox1$max[1]));
                  bbox1$max[2]<-max(-90,min(90,(rangeY*expandPercent/100)+bbox1$max[2]));
                  bbox1$min;bbox1$max;
                  bbox1<-methods::as(raster::extent(as.vector(t(bbox1))), "SpatialPolygons")
                  sp::proj4string(bbox1)<-sp::CRS(projX) # ASSIGN COORDINATE SYSTEM
                  boundaryRegShape <- sp::spTransform(boundaryRegShape,sp::CRS(projX))
                  boundaryRegShape<-raster::crop(boundaryRegShape, bbox1)
                  boundaryRegShape@bbox <- bbox1@bbox
                  boundaryRegShape@data <- droplevels(boundaryRegShape@data)
                  print("Map cropped to regions with data. To plot full map extent set crop2Boundary = F.")
                  subRegShape <- sp::spTransform(subRegShape,raster::crs(boundaryRegShape))
                  subRegShape <- raster::crop(subRegShape,boundaryRegShape)
                  print("Scale will still include all data from original subRegShape extents")
                  expandPercent_i = 0 # Preventing extension for doubling expansion
                } else {
                  print(paste("boundaryRegionsSelect chosen are not available in the boundaryRegShapeFile.",paste(boundaryRegionsSelect,collapse=", "),sep=""))}
              }else{
                boundaryRegShape <- sp::spTransform(boundaryRegShape,raster::crs(subRegShape))
                subRegShape <- raster::crop(subRegShape,boundaryRegShape)
                subRegShape@data <- droplevels(subRegShape@data)
              }
            }

            shape<-subRegShape

            if(!subRegCol %in% names(shape)){stop(paste("SubRegCol: ",subRegCol," not present in shape",sep=""))}

            shape@data<-shape@data%>%dplyr::mutate(subRegion=get(subRegCol), subRegion=as.character(subRegion))


            #----------------
            # Create Boundary Extension
            #---------------

            bgColorChosen="white"


            if(extension==T){

              frameShow=T;facetLabelBorderLwd=0.1;facetBGColor="grey30";facetLabelColor = "white"

              if(is.null(extendedShape)){

                extendedBoundary <- metis::mapCountries
                bbox1<-as.data.frame(sp::bbox(shape))
                extendedShapeCol<-subRegCol

                if(!is.null(bbox1)){
                  bbox1$min;bbox1$max
                  rangeX<-abs(range(bbox1$min[1],bbox1$max[1])[2]-range(bbox1$min[1],bbox1$max[1])[1])
                  rangeY<-abs(range(bbox1$min[2],bbox1$max[2])[2]-range(bbox1$min[2],bbox1$max[2])[1])
                  bbox1$min[1]<-min(180,max(-180,(-rangeX*expandPercent_i/100)+bbox1$min[1]));
                  bbox1$min[2]<-min(90,max(-90,(-rangeY*expandPercent_i/100)+bbox1$min[2]));
                  bbox1$max[1]<-max(-180,min(180,(rangeX*expandPercent_i/100)+bbox1$max[1]));
                  bbox1$max[2]<-max(-90,min(90,(rangeY*expandPercent_i/100)+bbox1$max[2]));
                  bbox1$min;bbox1$max;
                  bbox1<-methods::as(raster::extent(as.vector(t(bbox1))), "SpatialPolygons")
                  sp::proj4string(bbox1)<-sp::CRS(projX) # ASSIGN COORDINATE SYSTEM
                  boundaryRegShape <- sp::spTransform(boundaryRegShape,sp::CRS(projX))
                  print("Creating extended boundary...")
                  extendedShape<-raster::crop(extendedBoundary, bbox1)
                  extendedShape@bbox <- bbox1@bbox
                  if(!is.null(boundaryRegShape)){extendedShape<-raster::crop(extendedShape, boundaryRegShape)}
                  extendedBGColor="lightblue1"
                }else{print("No extended boundary.")}
              }

              if(!is.null(extendedShape)){
                if(extendedShapeCol %in% names(extendedShape)){
                  underLayer<-metis.map(facetsOn=F,innerMargins=innerMargins, legendDigitsOverride=legendDigitsOverride,facetLabelSize=facetLabelSize,mapTitleOn=mapTitleOn, facetCols=facetCols,fillcolorNA=fillcolorNA,fillshowNA=fillshowNA,fillcolorNULL=fillcolorNULL, dataPolygon=extendedShape, printFig=F,labelsAutoPlace = F,
                                        fillColumn = extendedShapeCol,labels=extendedLabels, fillPalette = extendedFillColor,legendShow=F,
                                        bgColor = extendedBGColor, frameShow=T,facetLabelBorderLwd=facetLabelBorderLwd,labelsSize=extdendedLabelSize, labelsColor=extendedLabelsColor,
                                        figWidth=figWidth,figHeight=figHeight, pdfpng = pdfpng)
                  bgColorChosen= extendedBGColor
                }
              }
            }
          } # Close shapefiles


          # Crop gridTbl data to shape
          if(!is.null(gridTbl) & !is.null(shape)){
            if(nrow(gridTbl)>0){

              gridTbl <- metis.gridByPoly(gridTable = gridTbl,shape=shape,colName=subRegCol)

            }}


          if(nrow(gridTbl%>%dplyr::filter(scenario==scenario_i,param==param_i))>0){

            #-------------------
            # Save Map related Data Table
            #-------------------

            if(nrow(gridTbl %>% dplyr::filter(scenario==scenario_i,param==param_i))>0){
              data.table::fwrite(gridTbl %>% dplyr::filter(scenario==scenario_i,param==param_i)%>%
                                   dplyr::select(scenario,lat,lon,param,class,x,value,units),
                                 paste(dirOutputsX,"/",folderName, "/",mapFolder,"/raster/",param_i,"/", scenario_i,
                                       "/","map_","raster_",param_i,"_",scenario_i,nameAppend,".csv",sep = ""))
              print(paste("Map data table written to ",dirOutputsX,"/",folderName, "/",mapFolder,"/raster/",param_i,"/", scenario_i,
                          "/","map_","raster_",param_i,"_",scenario_i,nameAppend,".csv",sep = ""))
            }




            animScaleGrid<-(gridTbl %>% dplyr::filter(scenario==scenario_i,param==param_i) %>%
                              dplyr::filter(!is.na(value),!is.infinite(value),!is.nan(value)))$value

            # Choose correct scaleRange
            scaleRange_i=scaleRange
            if(grepl("DiffPrcnt",scenario_i)){
              scaleRange_i=scaleRangeDiffPrcnt
              gridTbl <- gridTbl %>% dplyr::mutate(units="Percent")
              if(is.null(legendSingleColorOnOrig)){legendSingleColorOn=T}else{legendSingleColorOn=legendSingleColorOnOrig}
            }
            if(grepl("DiffAbs",scenario_i)){
              scaleRange_i=scaleRangeDiffAbs
              if(is.null(legendSingleColorOnOrig)){legendSingleColorOn=T}else{legendSingleColorOn=legendSingleColorOnOrig}
            }
           if(grepl("DiffxPrcnt",scenario_i)){
              scaleRange_i=scaleRangeDiffxPrcnt
              gridTbl <- gridTbl %>% dplyr::mutate(units="Percent")
              if(is.null(legendSingleColorOnOrig)){legendSingleColorOn=T}else{legendSingleColorOn=legendSingleColorOnOrig}
            }
            if(grepl("DiffxAbs",scenario_i)){
              scaleRange_i=scaleRangeDiffxAbs
              if(is.null(legendSingleColorOnOrig)){legendSingleColorOn=T}else{legendSingleColorOn=legendSingleColorOnOrig}
            }

            if(is.null(legendSingleColorOnOrig)){
              if(min(range(gridTbl$value))<0 & max(range(gridTbl$value))>0){
                legendSingleColorOn=T}}else{legendSingleColorOn=legendSingleColorOnOrig}

            if(!is.null(scaleRange_i)){
               if(any(param_i %in% unique(scaleRange_i$param))){
              if(max(animScaleGrid) < (scaleRange_i %>% dplyr::filter(param==param_i))$maxScale){
                animScaleGrid<-c(animScaleGrid,(scaleRange_i %>% dplyr::filter(param==param_i))$maxScale)} else {
                  animScaleGrid <- c((scaleRange_i %>% dplyr::filter(param==param_i))$maxScale,
                    animScaleGrid[animScaleGrid<(scaleRange_i %>% dplyr::filter(param==param_i))$maxScale])
                }
              if(min(animScaleGrid) > (scaleRange_i %>% dplyr::filter(param==param_i))$minScale){
                animScaleGrid<-c(animScaleGrid,(scaleRange_i %>% dplyr::filter(param==param_i))$minScale)} else {
                  animScaleGrid <-  c((scaleRange_i %>% dplyr::filter(param==param_i))$minScale,
                                      animScaleGrid[animScaleGrid>(scaleRange_i %>% dplyr::filter(param==param_i))$minScale])
                }
              }
            }
            animPrettyBreaksGrid<-scales::pretty_breaks(n=legendFixedBreaks)(animScaleGrid); animPrettyBreaksGrid
            animKmeanBreaksGrid<-sort(as.vector((stats::kmeans(animScaleGrid,
                                                               centers=max(1,min(length(unique(animScaleGrid))-1,(legendFixedBreaks-1)))))$centers[,1]));animKmeanBreaksGrid
            if(!min(animScaleGrid) %in% animKmeanBreaksGrid){
              animKmeanBreaksGrid <- sort(c(min(animScaleGrid),animKmeanBreaksGrid))}
            if(!max(animScaleGrid) %in% animKmeanBreaksGrid){
              animKmeanBreaksGrid <- sort(c(animKmeanBreaksGrid,max(animScaleGrid)))};animKmeanBreaksGrid


            if((max(range(animScaleGrid))-min(range(animScaleGrid)))<1E-10 &
               (max(range(animScaleGrid))-min(range(animScaleGrid)))>-1E-10){animScaleGridRange=min(animScaleGrid)}else{
                 animScaleGridRange=range(animScaleGrid)
               }

            if(abs(min(animScaleGridRange,na.rm = T))==abs(max(animScaleGridRange,na.rm = T))){animScaleGridRange=abs(min(animScaleGridRange,na.rm = T))}
            if(mean(animScaleGridRange,na.rm = T)<0.01 & mean(animScaleGridRange,na.rm = T)>(-0.01)){animLegendDigits<-5}else{
              if(mean(animScaleGridRange,na.rm = T)<0.1 & mean(animScaleGridRange,na.rm = T)>(-0.1)){animLegendDigits<-4}else{
                if(mean(animScaleGridRange,na.rm = T)<1 & mean(animScaleGridRange,na.rm = T)>(-1)){animLegendDigits<-3}else{
                  if(mean(animScaleGridRange,na.rm = T)<10 & mean(animScaleGridRange,na.rm = T)>(-10)){animLegendDigits<-2}else{animLegendDigits<-1}}}}

            # Figure 1 : each param: If class > 1 { (Map x Class) x Selected Years}

            gridTblx<-gridTbl%>%dplyr::filter(scenario==scenario_i,param==param_i)

            for (x_i in unique(gridTblx$x)){

              datax<-gridTblx%>%dplyr::filter(x==x_i)
              if(nrow(datax)>0){
                legendTitle<-unique(datax$units)
                if(!is.null(classPaletteOrig)){fillPalette<-classPaletteOrig}else{
                fillPalette<-as.character(unique(datax$classPalette))}

                datax<-datax%>%dplyr::select(lat,lon,class,value) %>%
                  dplyr::distinct(lat,lon,class,.keep_all = TRUE) %>%
                  tidyr::spread(key=class,value=value)

                rasterx<-sp::SpatialPointsDataFrame(sp::SpatialPoints(coords=(cbind(datax$lon,datax$lat))),data=datax)
                sp::proj4string(rasterx)<-sp::proj4string(shape)
                sp::gridded(rasterx)<-T

                mapx<-rasterx
                mapx@data<-mapx@data%>%dplyr::select(-lat,-lon)


                if("subRegion" %in% names(mapx@data)){countCheck=2}else{countCheck=1}
                if(length(names(mapx@data))==countCheck){
                  legendOutsideAnimated=legendOutsideSingle
                  legendTitleAnimated=legendTitle
                  panelLabelAnimated=paste(x_i)
                  legendAnimatedPosition=legendPositionS
                  legendTitleSizeAnim = legendTitleSizeS
                  legendTextSizeAnim = legendTextSizeS
                  legendBreaksAnim = animKmeanBreaksGrid
                  legendStyleAnim="fixed"}else{
                    legendStyleAnim="fixed"
                    legendBreaksAnim = animKmeanBreaksGrid
                    legendOutsideAnimated=legendOutsideSingle
                    legendTitleAnimated=paste(x_i,"\n",legendTitle,sep="")
                    panelLabelAnimated=NULL
                    legendAnimatedPosition=legendPositionS
                    legendTitleSizeAnim = legendTitleSizeS
                    legendTextSizeAnim = legendTextSizeS
                  }

                metis.map(facetsOn=T,innerMargins=innerMargins, legendDigitsOverride=legendDigitsOverride,facetLabelSize=facetLabelSize,mapTitleOn=mapTitleOn, facetCols=facetCols,numeric2Cat_list=numeric2Cat_list, catParam=param_i, underLayer=underLayer,
                          panelLabel=panelLabelAnimated,
                          dataPolygon=shape,
                          dataGrid=mapx,
                          fillColumn = names(mapx@data),
                          mapTitle=paste(param_i," ",scenario_i,sep="") , legendShow = T,
                          legendOutside = legendOutsideAnimated,
                          facetFreeScale = F,
                          frameShow = frameShow, facetLabelBorderLwd=facetLabelBorderLwd, facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor ,
                          legendSingleColorOn=legendSingleColorOn,legendSingleValue=legendSingleValue,legendSingleColor=legendSingleColor,
                          labels=labels,fillcolorNA=fillcolorNA,fillshowNA=fillshowNA,fillcolorNULL=fillcolorNULL,
                          labelsSize = labelsSize,
                          legendTitle =legendTitleAnimated,
                          legendTitleSize = legendTitleSizeAnim,legendTextSize = legendTextSizeAnim,
                          legendStyle=legendStyleAnim,
                          legendBreaks = legendBreaksAnim,
                          legendFixedBreaks=legendFixedBreaks,
                          legendDigits = animLegendDigits,
                          legendOutsidePosition = legendOutsidePosition,
                          legendPosition = legendAnimatedPosition,
                          fillPalette = fillPalette,
                          bgColor = bgColorChosen,
                          figWidth=figWidth,
                          figHeight=figHeight,
                          pdfpng = pdfpng,
                          fileName = paste("map_","raster_",param_i,"_",x_i,"_",scenario_i,nameAppend,"_KMEANS",sep=""),
                          dirOutputs = paste(dirOutputsX,"/",folderName, "/",mapFolder,"/raster/",param_i,"/", scenario_i,"/byYear",sep = ""))

                # numeric2Cat_list=numeric2Cat_list
                # catParam=param_i
                # underLayer=underLayer
                # panelLabel=panelLabelAnimated
                # dataPolygon=shape
                # dataGrid=mapx
                # fillColumn = names(mapx@data)
                # mapTitle=paste(param_i," ",scenario_i,sep="") , legendShow = T
                # legendOutside = legendOutsideAnimated
                # facetFreeScale = F
                # frameShow = frameShow
                # labels=labels
                # labelsSize = labelsSize
                # legendTitle =legendTitleAnimated
                # legendTitleSize = legendTitleSizeAnim
                # legendTextSize = legendTextSizeAnim
                # legendStyle=legendStyleAnim
                # legendBreaks = legendBreaksAnim
                # legendFixedBreaks=legendFixedBreaks
                # legendDigits = animLegendDigits
                # legendOutsidePosition = legendOutsidePosition
                # legendPosition = legendAnimatedPosition
                # fillPalette = fillPalette
                # bgColor = bgColorChosen
                # figWidth=figWidth
                # figHeight=figHeight
                # fileName = paste("map_","raster_",param_i,"_",x_i,"_",scenario_i,nameAppend,"_KMEANS",sep="")
                # dirOutputs = paste(dirOutputsX,"/",folderName, "/",mapFolder,"/raster/",param_i,"/", scenario_i,"/byYear",sep = "")

                if("subRegion" %in% names(mapx@data)){countCheck=2}else{countCheck=1}
                if(length(names(mapx@data))==countCheck){
                  legendBreaksAnim = animPrettyBreaksGrid
                  legendStyleAnim="fixed"}else{
                    legendStyleAnim="fixed"
                    legendBreaksAnim = animPrettyBreaksGrid
                  }

                metis.map(facetsOn=T,innerMargins=innerMargins, legendDigitsOverride=legendDigitsOverride,facetLabelSize=facetLabelSize,mapTitleOn=mapTitleOn, facetCols=facetCols,numeric2Cat_list=numeric2Cat_list, catParam=param_i, panelLabel=panelLabelAnimated,
                          underLayer=underLayer,  dataPolygon=shape,
                          dataGrid=mapx,
                          fillColumn = names(mapx@data),
                          mapTitle=paste(param_i," ",scenario_i,sep="") , legendShow = T,
                          legendOutside = legendOutsideAnimated,
                          facetFreeScale = F,
                          frameShow = frameShow, facetLabelBorderLwd=facetLabelBorderLwd, facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor ,
                          legendSingleColorOn=legendSingleColorOn,legendSingleValue=legendSingleValue,legendSingleColor=legendSingleColor,
                          labels=labels,fillcolorNA=fillcolorNA,fillshowNA=fillshowNA,fillcolorNULL=fillcolorNULL,
                          labelsSize = labelsSize,
                          legendTitle =legendTitleAnimated,
                          legendTitleSize = legendTitleSizeAnim,legendTextSize = legendTextSizeAnim,
                          legendStyle=legendStyleAnim,
                          legendBreaks = animPrettyBreaksGrid,
                          legendFixedBreaks=legendFixedBreaks,
                          legendDigits = animLegendDigits,
                          legendOutsidePosition = legendOutsidePosition,
                          legendPosition = legendAnimatedPosition,
                          fillPalette = fillPalette,
                          bgColor = bgColorChosen,
                          figWidth=figWidth,
                          figHeight=figHeight,
                          pdfpng = pdfpng,
                          fileName = paste("map_","raster_",param_i,"_",x_i,"_",scenario_i,nameAppend,"_PRETTY",sep=""),
                          dirOutputs = paste(dirOutputsX,"/",folderName, "/",mapFolder,"/raster/",param_i,"/", scenario_i,"/byYear",sep = ""))

                if("subRegion" %in% names(mapx@data)){countCheck=2}else{countCheck=1}
                if(length(names(mapx@data))==countCheck){
                  legendOutsideAnimated=legendOutsideSingle
                  legendTitleSizeAnim = legendTitleSizeS
                  legendTextSizeAnim = legendTextSizeS}else{
                     legendOutsideAnimated=F
                    legendAnimatedPosition=legendPosition
                    legendTitleSizeAnim = legendTitleSizeI
                    legendTextSizeAnim = legendTextSizeI
                  }

                metis.map(facetsOn=T,innerMargins=innerMargins, legendDigitsOverride=legendDigitsOverride,facetLabelSize=facetLabelSize,mapTitleOn=mapTitleOn, facetCols=facetCols,numeric2Cat_list=numeric2Cat_list, catParam=param_i, panelLabel=panelLabelAnimated,
                          underLayer=underLayer,  dataPolygon=shape,
                          dataGrid=mapx,
                          fillColumn = names(mapx@data),
                          mapTitle=paste(param_i," ",scenario_i,sep="") , legendShow = T,
                          legendOutside = legendOutsideAnimated,
                          facetFreeScale = T,
                          frameShow = frameShow, facetLabelBorderLwd=facetLabelBorderLwd, facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor ,
                          legendSingleColorOn=legendSingleColorOn,legendSingleValue=legendSingleValue,legendSingleColor=legendSingleColor,
                          labels=labels,fillcolorNA=fillcolorNA,fillshowNA=fillshowNA,fillcolorNULL=fillcolorNULL,
                          labelsSize = labelsSize,
                          legendTitleSize = legendTitleSizeAnim,legendTextSize = legendTextSizeAnim,
                          legendTitle = legendTitleAnimated,
                          legendStyle="kmeans",
                          legendDigits = NULL,
                          legendFixedBreaks=legendFixedBreaks,
                          legendOutsidePosition = legendOutsidePosition,
                          legendPosition = legendAnimatedPosition,
                          fillPalette = fillPalette,
                          bgColor = bgColorChosen,
                          figWidth=figWidth,
                          figHeight=figHeight,
                          pdfpng = pdfpng,
                          fileName = paste("map_","raster_",param_i,"_",x_i,"_",scenario_i,nameAppend,"_FREESCALE",sep=""),
                          dirOutputs = paste(dirOutputsX,"/",folderName, "/",mapFolder,"/raster/",param_i,"/", scenario_i,"/byYear",sep = ""))

                # numeric2Cat_list=numeric2Cat_list
                # catParam=param_i
                # panelLabel=panelLabelAnimated
                # underLayer=underLayer
                # dataPolygon=shape
                # dataGrid=mapx
                # fillColumn = names(mapx@data)
                # mapTitle=paste(param_i," ",scenario_i,sep="") , legendShow = T
                # legendOutside = legendOutsideAnimated
                # facetFreeScale = T
                # frameShow = frameShow
                # labels=labels
                # labelsSize = labelsSize
                # legendTitleSize = legendTitleSizeAnim
                # legendTextSize = legendTextSizeAnim
                # legendTitle = legendTitleAnimated
                # legendStyle="kmeans"
                # legendDigits = animLegendDigits
                # legendFixedBreaks=legendFixedBreaks
                # legendOutsidePosition = legendOutsidePosition
                # legendPosition = legendAnimatedPosition
                # fillPalette = fillPalette
                # bgColor = bgColorChosen
                # figWidth=figWidth
                # figHeight=figHeight
                # fileName = paste("map_","raster_",param_i,"_",x_i,"_",scenario_i,nameAppend,"_FREESCALE",sep="")
                # dirOutputs = paste(dirOutputsX,"/",folderName, "/",mapFolder,"/raster/",param_i,"/", scenario_i,"/byYear",sep = "")

              } # if nrow(datax) > 1
            }# Close years loop

            # Animate 1 : each param: If class > 1 { (Map x Class) x Anim Years}

            if(animateOn==T){

              animName<-paste("anim_","raster_",param_i,"_",scenario_i,nameAppend,"_PRETTY.gif",sep="")
              animFiles <- list.files(path = paste(dirOutputsX,"/",folderName, "/",mapFolder,"/raster/",param_i,"/", scenario_i,"/byYear",sep=""),
                                      pattern = paste(".*",param_i,".*",nameAppend,".*PRETTY", ".", pdfpng,sep=""), full.names=T,ignore.case = T, include.dirs = T);
              animation <- magick::image_animate(magick::image_join(lapply(animFiles, magick::image_read)),fps=fps)
              magick::image_write(animation,paste(dirOutputsX,"/",folderName, "/",mapFolder,"/raster/",param_i,"/", scenario_i,"/",
                                        animName,sep = ""))
              print(gsub("//","/",paste("animation saved in :",dirOutputsX,"/",folderName, "/",mapFolder,"/raster/",param_i,"/", scenario_i,"/",
                                                       animName,sep = "")))
              # fnameTempImage=paste(dirOutputsX,"/",folderName, "/",mapFolder,"/raster/",param_i,"/", scenario_i,"/",
              #                      animName,sep = "")
              # tempImage<-magick::image_read(fnameTempImage)
              # croppedImage<-magick::image_trim(tempImage,fuzz=0);
              # magick::image_write(croppedImage,fnameTempImage)


              animName<-paste("anim_","raster_",param_i,"_",scenario_i,nameAppend,"_KMEANS.gif",sep="")
              animFiles <- list.files(path = paste(dirOutputsX,"/",folderName, "/",mapFolder,"/raster/",param_i,"/", scenario_i,"/byYear",sep=""),
                                      pattern = paste(".*",param_i,".*",nameAppend,".*KMEANS", ".", pdfpng,sep=""), full.names=T,ignore.case = T, include.dirs = T);
              animation <- magick::image_animate(magick::image_join(lapply(animFiles, magick::image_read)),fps=fps)
              magick::image_write(animation,paste(dirOutputsX,"/",folderName, "/",mapFolder,"/raster/",param_i,"/", scenario_i,"/",
                                          animName,sep = ""))
              print(gsub("//","/",paste("animation saved in :",dirOutputsX,"/",folderName, "/",mapFolder,"/raster/",param_i,"/", scenario_i,"/",
                          animName,sep = "")))
              # fnameTempImage=paste(dirOutputsX,"/",folderName, "/",mapFolder,"/raster/",param_i,"/", scenario_i,"/",
              #                      animName,sep = "")
              # tempImage<-magick::image_read(fnameTempImage)
              # croppedImage<-magick::image_trim(tempImage,fuzz=0);
              # magick::image_write(croppedImage,fnameTempImage)


              animName<-paste("anim_","raster_",param_i,"_",scenario_i,nameAppend,"_FREESCALE.gif",sep="")
              animFiles <- list.files(path = paste(dirOutputsX,"/",folderName, "/",mapFolder,"/raster/",param_i,"/", scenario_i,"/byYear",sep=""),
                                      pattern = paste(".*",param_i,".*",nameAppend,".*FREESCALE", ".", pdfpng,sep=""), full.names=T,ignore.case = T, include.dirs = T);
              animation <- magick::image_animate(magick::image_join(lapply(animFiles, magick::image_read)),fps=fps)
              magick::image_write(animation,paste(dirOutputsX,"/",folderName, "/",mapFolder,"/raster/",param_i,"/", scenario_i,"/",
                                          animName,sep = ""))
              print(gsub("//","/",paste("animation saved in :",dirOutputsX,"/",folderName, "/",mapFolder,"/raster/",param_i,"/", scenario_i,"/",
                          animName,sep = "")))
              # fnameTempImage=paste(dirOutputsX,"/",folderName, "/",mapFolder,"/raster/",param_i,"/", scenario_i,"/",
              #                      animName,sep = "")
              # tempImage<-magick::image_read(fnameTempImage)
              # croppedImage<-magick::image_trim(tempImage,fuzz=0);
              # magick::image_write(croppedImage,fnameTempImage)


              #unlink(paste(dirOutputsX,"/",folderName, "/",mapFolder,"/raster/",param_i,"/", scenario_i,"/byYear/animate_",param_i,sep = ""), recursive = TRUE) #-------------- cleaning up plots and temporary variables
            } # If Animate ON==t


            #------------------------------
            # Figure 2 : each param: If class ==1 { Map x years}
            #-----------------------------

            checkTbl<-gridTbl%>%dplyr::filter(scenario==scenario_i,param==param_i)
            checkTbl<-droplevels(checkTbl)

            if(length(unique(checkTbl$class))==1 & length(unique(checkTbl$x))>1){
              rm(checkTbl)

              datax<-gridTbl%>%dplyr::filter(scenario==scenario_i,param==param_i)

              if(nrow(datax)>0){

                legendTitle<-unique(datax$units)
                if(!is.null(classPaletteOrig)){fillPalette<-classPaletteOrig}else{
                  fillPalette<-as.character(unique(datax$classPalette))}

                animScaleGrid<-datax$value

                # Choose correct scaleRange
                scaleRange_i=scaleRange
                if(grepl("DiffPrcnt",scenario_i)){
                  scaleRange_i=scaleRangeDiffPrcnt
                  datax <- datax %>% dplyr::mutate(units="Percent")
                  if(is.null(legendSingleColorOnOrig)){legendSingleColorOn=T}else{legendSingleColorOn=legendSingleColorOnOrig}
                }
                if(grepl("DiffAbs",scenario_i)){
                  scaleRange_i=scaleRangeDiffAbs
                  if(is.null(legendSingleColorOnOrig)){legendSingleColorOn=T}else{legendSingleColorOn=legendSingleColorOnOrig}
                }
                if(grepl("DiffxPrcnt",scenario_i)){
                  scaleRange_i=scaleRangeDiffxPrcnt
                  datax <- datax %>% dplyr::mutate(units="Percent")
                  if(is.null(legendSingleColorOnOrig)){legendSingleColorOn=T}else{legendSingleColorOn=legendSingleColorOnOrig}
                }
                if(grepl("DiffxAbs",scenario_i)){
                  scaleRange_i=scaleRangeDiffxAbs
                  if(is.null(legendSingleColorOnOrig)){legendSingleColorOn=T}else{legendSingleColorOn=legendSingleColorOnOrig}
                }

                if(is.null(legendSingleColorOnOrig)){
                  if(min(range(datax$value))<0 & max(range(datax$value))>0){
                    legendSingleColorOn=T}}else{legendSingleColorOn=legendSingleColorOnOrig}

                if(!is.null(scaleRange_i)){
                   if(any(param_i %in% unique(scaleRange_i$param))){
                    if(max(animScaleGrid) < (scaleRange_i %>% dplyr::filter(param==param_i))$maxScale){
                      animScaleGrid<-c(animScaleGrid,(scaleRange_i %>% dplyr::filter(param==param_i))$maxScale)} else {
                        animScaleGrid <- c((scaleRange_i %>% dplyr::filter(param==param_i))$maxScale,
                                           animScaleGrid[animScaleGrid<(scaleRange_i %>% dplyr::filter(param==param_i))$maxScale])
                      }
                    if(min(animScaleGrid) > (scaleRange_i %>% dplyr::filter(param==param_i))$minScale){
                      animScaleGrid<-c(animScaleGrid,(scaleRange_i %>% dplyr::filter(param==param_i))$minScale)} else {
                        animScaleGrid <-  c((scaleRange_i %>% dplyr::filter(param==param_i))$minScale,
                                            animScaleGrid[animScaleGrid>(scaleRange_i %>% dplyr::filter(param==param_i))$minScale])
                      }
                  }
                }
                animPrettyBreaksGrid<-scales::pretty_breaks(n=legendFixedBreaks)(animScaleGrid)
                animKmeanBreaksGrid<-sort(as.vector((stats::kmeans(animScaleGrid,
                                                                   centers=max(1,min(length(unique(animScaleGrid))-1,(legendFixedBreaks-1)))))$centers[,1]))
                if(!min(animScaleGrid) %in% animKmeanBreaksGrid){
                  animKmeanBreaksGrid <- sort(c(min(animScaleGrid),animKmeanBreaksGrid))}
                if(!max(animScaleGrid) %in% animKmeanBreaksGrid){
                  animKmeanBreaksGrid <- sort(c(animKmeanBreaksGrid,max(animScaleGrid)))}


                if((max(range(animScaleGrid))-min(range(animScaleGrid)))<1E-10 &
                   (max(range(animScaleGrid))-min(range(animScaleGrid)))>-1E-10){animScaleGridRange=min(animScaleGrid)}else{
                     animScaleGridRange=range(animScaleGrid)
                   }
                if(abs(min(animScaleGridRange,na.rm = T))==abs(max(animScaleGridRange,na.rm = T))){animScaleGridRange=abs(min(animScaleGridRange,na.rm = T))}
                if(mean(animScaleGridRange,na.rm = T)<0.01 & mean(animScaleGridRange,na.rm = T)>(-0.01)){animLegendDigits<-5}else{
                  if(mean(animScaleGridRange,na.rm = T)<0.1 & mean(animScaleGridRange,na.rm = T)>(-0.1)){animLegendDigits<-4}else{
                    if(mean(animScaleGridRange,na.rm = T)<1 & mean(animScaleGridRange,na.rm = T)>(-1)){animLegendDigits<-3}else{
                      if(mean(animScaleGridRange,na.rm = T)<10 & mean(animScaleGridRange,na.rm = T)>(-10)){animLegendDigits<-2}else{animLegendDigits<-1}}}}

                datax<-datax%>%dplyr::select(lat,lon,x,value)%>%
                  dplyr::distinct(lat,lon,x,.keep_all = TRUE) %>%
                  tidyr::spread(key=x,value=value)

                rasterx<-sp::SpatialPointsDataFrame(sp::SpatialPoints(coords=(cbind(datax$lon,datax$lat))),data=datax)
                sp::proj4string(rasterx)<-sp::proj4string(shape)
                sp::gridded(rasterx)<-T

                mapx<-rasterx
                mapx@data<-mapx@data%>%dplyr::select(-lat,-lon)
                names(mapx@data)<-paste("X",names(mapx@data),sep="")

                metis.map(facetsOn=T,innerMargins=innerMargins, legendDigitsOverride=legendDigitsOverride,facetLabelSize=facetLabelSize,mapTitleOn=mapTitleOn, facetCols=facetCols,numeric2Cat_list=numeric2Cat_list, catParam=param_i, underLayer=underLayer,  dataPolygon=shape,
                          dataGrid=mapx,
                          fillColumn = names(mapx@data),
                          mapTitle=paste(param_i," ",scenario_i,sep="") , legendShow = T,
                          legendOutside = legendOutsideSingle,
                          facetFreeScale = F,
                          frameShow = frameShow, facetLabelBorderLwd=facetLabelBorderLwd, facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor ,
                          legendSingleColorOn=legendSingleColorOn,legendSingleValue=legendSingleValue,legendSingleColor=legendSingleColor,
                          labels=labels,fillcolorNA=fillcolorNA,fillshowNA=fillshowNA,fillcolorNULL=fillcolorNULL,
                          labelsSize = labelsSize,
                          legendTitle =legendTitle,legendTitleSize = legendTitleSizeS,legendTextSize = legendTextSizeS,
                          legendStyle="fixed",
                          legendBreaks = animKmeanBreaksGrid,
                          legendFixedBreaks=legendFixedBreaks,
                          legendDigits = animLegendDigits,
                          legendOutsidePosition = legendOutsidePosition,
                          legendPosition = legendPositionS,
                          fillPalette = fillPalette,
                          bgColor = bgColorChosen,
                          figWidth=figWidth,
                          figHeight=figHeight,
                          pdfpng = pdfpng,
                          fileName = paste("map_","raster_",param_i,"_",scenario_i,nameAppend,"_KMEANS",sep=""),
                          dirOutputs = paste(dirOutputsX,"/",folderName, "/",mapFolder,"/raster/",param_i,"/", scenario_i,sep = ""))


                # numeric2Cat_list=numeric2Cat_list
                # catParam=param_i
                # underLayer=underLayer
                # dataPolygon=shape
                # dataGrid=mapx
                # fillColumn = names(mapx@data)
                # mapTitle=paste(param_i," ",scenario_i,sep="") , legendShow = T
                # legendOutside = legendOutsideSingle
                # facetFreeScale = F
                # frameShow = frameShow
                # labels=labels
                # labelsSize = labelsSize
                # legendTitle =legendTitle
                # legendTitleSize = legendTitleSizeS
                # legendTextSize = legendTextSizeS
                # legendStyle="fixed"
                # legendBreaks = animKmeanBreaksGrid
                # legendFixedBreaks=legendFixedBreaks
                # legendDigits = animLegendDigits
                # legendOutsidePosition = legendOutsidePosition
                # legendPosition = legendPositionS
                # fillPalette = fillPalette
                # bgColor = bgColorChosen
                # figWidth=figWidth
                # figHeight=figHeight
                # fileName = paste("map_","raster_",param_i,"_",scenario_i,nameAppend,"_KMEANS",sep="")
                # dirOutputs = paste(dirOutputsX,"/",folderName, "/",mapFolder,"/raster/",param_i,"/", scenario_i,sep = "")


                metis.map(facetsOn=T,innerMargins=innerMargins, legendDigitsOverride=legendDigitsOverride,facetLabelSize=facetLabelSize,mapTitleOn=mapTitleOn, facetCols=facetCols,numeric2Cat_list=numeric2Cat_list, catParam=param_i, underLayer=underLayer,  dataPolygon=shape,
                          dataGrid=mapx,
                          fillColumn = names(mapx@data),
                          mapTitle=paste(param_i," ",scenario_i,sep="") , legendShow = T,
                          legendOutside = legendOutsideSingle,
                          facetFreeScale = F,
                          frameShow = frameShow, facetLabelBorderLwd=facetLabelBorderLwd, facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor ,
                          legendSingleColorOn=legendSingleColorOn,legendSingleValue=legendSingleValue,legendSingleColor=legendSingleColor,
                          labels=labels,fillcolorNA=fillcolorNA,fillshowNA=fillshowNA,fillcolorNULL=fillcolorNULL,
                          labelsSize = labelsSize,
                          legendTitle =legendTitle,legendTitleSize = legendTitleSizeS,legendTextSize = legendTextSizeS,
                          legendStyle="fixed",
                          legendBreaks = animPrettyBreaksGrid,
                          legendFixedBreaks=legendFixedBreaks,
                          legendDigits = animLegendDigits,
                          legendOutsidePosition = legendOutsidePosition,
                          legendPosition = legendPositionS,
                          fillPalette = fillPalette,
                          bgColor = bgColorChosen,
                          figWidth=figWidth,figHeight=figHeight, pdfpng = pdfpng,
                          fileName = paste("map_","raster_",param_i,"_",scenario_i,nameAppend,"_PRETTY",sep=""),
                          dirOutputs = paste(dirOutputsX,"/",folderName, "/",mapFolder,"/raster/",param_i,"/", scenario_i,sep = ""))


                if("subRegion" %in% names(mapx@data)){countCheck=2}else{countCheck=1}
                if(length(names(mapx@data))==countCheck){
                  legendOutsideAnimated=legendOutsideSingle
                  legendTitleSizeAnim = legendTitleSizeS
                  legendTextSizeAnim = legendTextSizeS}else{
                     legendOutsideAnimated=F
                    legendTitleSizeAnim = legendTitleSizeI
                    legendTextSizeAnim = legendTextSizeI
                  }

                metis.map(facetsOn=T,innerMargins=innerMargins, legendDigitsOverride=legendDigitsOverride,facetLabelSize=facetLabelSize,mapTitleOn=mapTitleOn, facetCols=facetCols,numeric2Cat_list=numeric2Cat_list, catParam=param_i, underLayer=underLayer,  dataPolygon=shape,
                          dataGrid=mapx,
                          fillColumn = names(mapx@data),
                          mapTitle=paste(param_i," ",scenario_i,sep="") , legendShow = T,
                          legendOutside = legendOutsideAnimated,
                          facetFreeScale = T,
                          frameShow = frameShow, facetLabelBorderLwd=facetLabelBorderLwd, facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor ,
                          legendSingleColorOn=legendSingleColorOn,legendSingleValue=legendSingleValue,legendSingleColor=legendSingleColor,
                          labels=labels,fillcolorNA=fillcolorNA,fillshowNA=fillshowNA,fillcolorNULL=fillcolorNULL,
                          labelsSize = labelsSize,
                          legendTitle =legendTitle,legendTitleSize = legendTitleSizeAnim,legendTextSize = legendTextSizeAnim,
                          legendStyle="kmeans",
                          legendFixedBreaks=legendFixedBreaks,
                          legendDigits = NULL,
                          legendOutsidePosition = legendOutsidePosition,
                          legendPosition = legendPosition,
                          fillPalette = fillPalette,
                          bgColor = bgColorChosen,
                          figWidth=figWidth,figHeight=figHeight, pdfpng = pdfpng,
                          fileName = paste("map_","raster_",param_i,"_",scenario_i,nameAppend,"_FREESCALE",sep=""),
                          dirOutputs = paste(dirOutputsX,"/",folderName, "/",mapFolder,"/raster/",param_i,"/", scenario_i,sep = ""))

              } # if(nrow(datax)>0){

              # Mean for all years provided

              datax<-gridTbl%>%dplyr::filter(scenario==scenario_i,param==param_i)

              if(length(unique(datax$x))>1){

              if(nrow(datax)>0){
                legendTitle<-unique(datax$units)
                if(!is.null(classPaletteOrig)){fillPalette<-classPaletteOrig}else{
                  fillPalette<-as.character(unique(datax$classPalette))}

                meanCol = paste("Mean_",min(datax$x),"to",max(datax$x),sep="")

                datax<-datax%>%dplyr::select(lat,lon,x,value)%>%
                  dplyr::group_by(lat,lon)%>%
                  dplyr::summarize(!!meanCol:=mean(value))%>%
                  dplyr::ungroup()

                  animScaleGrid<-datax[[meanCol]];animScaleGrid

                  # Choose correct scaleRange
                  scaleRange_i=scaleRange
                  if(grepl("DiffPrcnt",scenario_i)){
                    scaleRange_i=scaleRangeDiffPrcnt
                    datax <- datax %>% dplyr::mutate(units="Percent")
                    if(is.null(legendSingleColorOnOrig)){legendSingleColorOn=T}else{legendSingleColorOn=legendSingleColorOnOrig}
                  }
                  if(grepl("DiffAbs",scenario_i)){
                    scaleRange_i=scaleRangeDiffAbs
                    if(is.null(legendSingleColorOnOrig)){legendSingleColorOn=T}else{legendSingleColorOn=legendSingleColorOnOrig}
                  }
                  if(grepl("DiffxPrcnt",scenario_i)){
                    scaleRange_i=scaleRangeDiffxPrcnt
                    datax <- datax %>% dplyr::mutate(units="Percent")
                    if(is.null(legendSingleColorOnOrig)){legendSingleColorOn=T}else{legendSingleColorOn=legendSingleColorOnOrig}
                  }
                  if(grepl("DiffxAbs",scenario_i)){
                    scaleRange_i=scaleRangeDiffxAbs
                    if(is.null(legendSingleColorOnOrig)){legendSingleColorOn=T}else{legendSingleColorOn=legendSingleColorOnOrig}
                  }

                  if(is.null(legendSingleColorOnOrig)){
                    if(min(range(datax$value))<0 & max(range(datax$value))>0){
                      legendSingleColorOn=T}}else{legendSingleColorOn=legendSingleColorOnOrig}

                  if(!is.null(scaleRange_i)){
                     if(any(param_i %in% unique(scaleRange_i$param))){
                      if(max(animScaleGrid) < (scaleRange_i %>% dplyr::filter(param==param_i))$maxScale){
                        animScaleGrid<-c(animScaleGrid,(scaleRange_i %>% dplyr::filter(param==param_i))$maxScale)} else {
                          animScaleGrid <- c((scaleRange_i %>% dplyr::filter(param==param_i))$maxScale,
                                             animScaleGrid[animScaleGrid<(scaleRange_i %>% dplyr::filter(param==param_i))$maxScale])
                        }
                      if(min(animScaleGrid) > (scaleRange_i %>% dplyr::filter(param==param_i))$minScale){
                        animScaleGrid<-c(animScaleGrid,(scaleRange_i %>% dplyr::filter(param==param_i))$minScale)} else {
                          animScaleGrid <-  c((scaleRange_i %>% dplyr::filter(param==param_i))$minScale,
                                              animScaleGrid[animScaleGrid>(scaleRange_i %>% dplyr::filter(param==param_i))$minScale])
                        }
                    }
                  }
                  animPrettyBreaksGrid<-scales::pretty_breaks(n=legendFixedBreaks)(animScaleGrid)
                  animKmeanBreaksGrid<-sort(as.vector((stats::kmeans(animScaleGrid,
                                                                     centers=max(1,min(length(unique(animScaleGrid))-1,(legendFixedBreaks-1)))))$centers[,1]))
                  if(!min(animScaleGrid) %in% animKmeanBreaksGrid){
                    animKmeanBreaksGrid <- sort(c(min(animScaleGrid),animKmeanBreaksGrid))}
                  if(!max(animScaleGrid) %in% animKmeanBreaksGrid){
                    animKmeanBreaksGrid <- sort(c(animKmeanBreaksGrid,max(animScaleGrid)))}


                  if((max(range(animScaleGrid))-min(range(animScaleGrid)))<1E-10 &
                     (max(range(animScaleGrid))-min(range(animScaleGrid)))>-1E-10){animScaleGridRange=min(animScaleGrid)}else{
                       animScaleGridRange=range(animScaleGrid)
                     }
                  if(abs(min(animScaleGridRange,na.rm = T))==abs(max(animScaleGridRange,na.rm = T))){animScaleGridRange=abs(min(animScaleGridRange,na.rm = T))}
                  if(mean(animScaleGridRange,na.rm = T)<0.01 & mean(animScaleGridRange,na.rm = T)>(-0.01)){animLegendDigits<-5}else{
                    if(mean(animScaleGridRange,na.rm = T)<0.1 & mean(animScaleGridRange,na.rm = T)>(-0.1)){animLegendDigits<-4}else{
                      if(mean(animScaleGridRange,na.rm = T)<1 & mean(animScaleGridRange,na.rm = T)>(-1)){animLegendDigits<-3}else{
                        if(mean(animScaleGridRange,na.rm = T)<10 & mean(animScaleGridRange,na.rm = T)>(-10)){animLegendDigits<-2}else{animLegendDigits<-1}}}}


                rasterx<-sp::SpatialPointsDataFrame(sp::SpatialPoints(coords=(cbind(datax$lon,datax$lat))),data=datax)
                sp::proj4string(rasterx)<-sp::proj4string(shape)
                sp::gridded(rasterx)<-T

                mapx<-rasterx
                mapx@data<-mapx@data%>%dplyr::select(-lat,-lon)
                names(mapx@data)<-paste("X",names(mapx@data),sep="")

                metis.map(facetsOn=T,innerMargins=innerMargins, legendDigitsOverride=legendDigitsOverride,facetLabelSize=facetLabelSize,mapTitleOn=mapTitleOn, facetCols=facetCols,numeric2Cat_list=numeric2Cat_list, catParam=param_i, underLayer=underLayer,  dataPolygon=shape,
                          dataGrid=mapx,
                          fillColumn = names(mapx@data),
                          mapTitle=paste(param_i," ",scenario_i,sep="") , legendShow = T,
                          legendOutside = legendOutsideSingle,
                          facetFreeScale = F,
                          frameShow = frameShow, facetLabelBorderLwd=facetLabelBorderLwd, facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor ,
                          legendSingleColorOn=legendSingleColorOn,legendSingleValue=legendSingleValue,legendSingleColor=legendSingleColor,
                          labels=labels,fillcolorNA=fillcolorNA,fillshowNA=fillshowNA,fillcolorNULL=fillcolorNULL,
                          labelsSize = labelsSize,
                          panelLabel = paste(names(datax)[!names(datax) %in% c("lat","lon")],sep=""),
                          legendTitle =paste(legendTitle,sep=""),
                          legendTitleSize = legendTitleSizeS,legendTextSize = legendTextSizeS,
                          legendStyle="fixed",
                          legendBreaks = animKmeanBreaksGrid,
                          legendFixedBreaks=legendFixedBreaks,
                          legendDigits = animLegendDigits,
                          legendOutsidePosition = legendOutsidePosition,
                          legendPosition = legendPositionS,
                          fillPalette = fillPalette,
                          bgColor = bgColorChosen,
                          figWidth=figWidth,figHeight=figHeight, pdfpng = pdfpng,
                          fileName = paste("map_","raster_",param_i,"_",scenario_i,nameAppend,"_MEAN_KMEANS",sep=""),
                          dirOutputs = paste(dirOutputsX,"/",folderName, "/",mapFolder,"/raster/",param_i,"/", scenario_i,sep = ""))

                metis.map(facetsOn=T,innerMargins=innerMargins, legendDigitsOverride=legendDigitsOverride,facetLabelSize=facetLabelSize,mapTitleOn=mapTitleOn, facetCols=facetCols,numeric2Cat_list=numeric2Cat_list, catParam=param_i, underLayer=underLayer,  dataPolygon=shape,
                          dataGrid=mapx,
                          fillColumn = names(mapx@data),
                          mapTitle=paste(param_i," ",scenario_i,sep="") , legendShow = T,
                          legendOutside = legendOutsideSingle,
                          facetFreeScale = F,
                          frameShow = frameShow, facetLabelBorderLwd=facetLabelBorderLwd, facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor ,
                          legendSingleColorOn=legendSingleColorOn,legendSingleValue=legendSingleValue,legendSingleColor=legendSingleColor,
                          labels=labels,fillcolorNA=fillcolorNA,fillshowNA=fillshowNA,fillcolorNULL=fillcolorNULL,
                          labelsSize = labelsSize,
                          panelLabel = paste(names(datax)[!names(datax) %in% c("lat","lon")],sep=""),
                          legendTitle =paste(legendTitle,sep=""),legendTitleSize = legendTitleSizeS,legendTextSize = legendTextSizeS,
                          legendStyle="fixed",
                          legendBreaks = animPrettyBreaksGrid,
                          legendFixedBreaks=legendFixedBreaks,
                          legendDigits = animLegendDigits,
                          legendOutsidePosition = legendOutsidePosition,
                          legendPosition = legendPositionS,
                          fillPalette = fillPalette,
                          bgColor = bgColorChosen,
                          figWidth=figWidth,figHeight=figHeight, pdfpng = pdfpng,
                          fileName = paste("map_","raster_",param_i,"_",scenario_i,nameAppend,"_MEAN_PRETTY",sep=""),
                          dirOutputs = paste(dirOutputsX,"/",folderName, "/",mapFolder,"/raster/",param_i,"/", scenario_i,sep = ""))


                if("subRegion" %in% names(mapx@data)){countCheck=2}else{countCheck=1}
                if(length(names(mapx@data))==countCheck){
                  legendOutsideAnimated=legendOutsideSingle
                  legendTitleSizeAnim = legendTitleSizeS
                  legendTextSizeAnim = legendTextSizeS}else{
                     legendOutsideAnimated=F
                    legendTitleSizeAnim = legendTitleSizeI
                    legendTextSizeAnim = legendTextSizeI
                  }

                metis.map(facetsOn=T,innerMargins=innerMargins, legendDigitsOverride=legendDigitsOverride,facetLabelSize=facetLabelSize,mapTitleOn=mapTitleOn, facetCols=facetCols,numeric2Cat_list=numeric2Cat_list, catParam=param_i, underLayer=underLayer,  dataPolygon=shape,
                          dataGrid=mapx,
                          fillColumn = names(mapx@data),
                          mapTitle=paste(param_i," ",scenario_i,sep="") , legendShow = T,
                          legendOutside = legendOutsideAnimated,
                          facetFreeScale = T,
                          frameShow = frameShow, facetLabelBorderLwd=facetLabelBorderLwd, facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor ,
                          legendSingleColorOn=legendSingleColorOn,legendSingleValue=legendSingleValue,legendSingleColor=legendSingleColor,
                          labels=labels,fillcolorNA=fillcolorNA,fillshowNA=fillshowNA,fillcolorNULL=fillcolorNULL,
                          labelsSize = labelsSize,
                          panelLabel = paste(names(datax)[!names(datax) %in% c("lat","lon")],sep=""),
                          legendTitle =paste(legendTitle,sep=""),legendTitleSize = legendTitleSizeAnim,legendTextSize =  legendTextSizeAnim,
                          legendStyle="kmeans",
                          legendFixedBreaks=legendFixedBreaks,
                          legendDigits = NULL,
                          legendOutsidePosition = legendOutsidePosition,
                          legendPosition = legendPositionS,
                          fillPalette = fillPalette,
                          bgColor = bgColorChosen,
                          figWidth=figWidth,figHeight=figHeight, pdfpng = pdfpng,
                          fileName = paste("map_","raster_",param_i,"_",scenario_i,nameAppend,"_MEAN_FREESCALE",sep=""),
                          dirOutputs = paste(dirOutputsX,"/",folderName, "/",mapFolder,"/raster/",param_i,"/", scenario_i,sep = ""))


              } # if(nrow(datax)>0){
            }# If no multiple years
            } # If number of classes == 1

          } # If nrow greater than 0


        }# close run Section
          }# Close gridTbl nrow > 0


          gridTblReturn <- gridTblReturn %>% dplyr::bind_rows(gridTbl)

          } # close Params
      } # Close scenario loop
  } # Close if nrow gridTbl < 0

    animateOn <- animateOnOrig

    }# Close if gridTbl is Null
  } # Close Raster Plots

  # -------------------
  # Create Polygon Plots for Each scenario
  # -------------------

  if(T){

  if(!is.null(shapeTbl) & nrow(shapeTbl)>0){

    if(!length(unique(shapeTbl$x))>1){animateOn=F}

    shapeTblOrig <- shapeTbl


    if(multifacetsOn==T){

      # Only working with data with multiple MultiA or MultiBs
      shapeTblMult <- shapeTblMultiOrig%>%
        dplyr::filter(!(multiFacetCol=="MultiANone" | multiFacetRow=="MultiBNone" | is.na(multiFacetCol) | is.na(multiFacetRow)))

      if(multiFacetCols %in% names(shapeTblMult)){shapeTblMult[[multiFacetCols]] <- multiFacetCols}
      if(multiFacetRows %in% names(shapeTblMult)){shapeTblMult[[multiFacetRows]] <- multiFacetRows}

      if(multiFacetCols %in% names(shapeTblScenMultiABRef)){shapeTblScenMultiABRef[[multiFacetCols]] <- multiFacetCols}
      if(multiFacetRows %in% names(shapeTblScenMultiABRef)){shapeTblScenMultiABRef[[multiFacetRows]] <- multiFacetRows}

     if(nrow(shapeTblMult)<1){ print(paste("No MultiA's or MultiB's found in shapeTbl, skipping MultiA/MultiB comparison."))}else{


              for (param_i in unique(shapeTblMult$param)){
                for (class_i in unique(shapeTblMult$class)){

                  if(nrow(shapeTblMult%>%dplyr::filter(param==param_i,class==class_i))>0){

                  shapeTbl <- shapeTblOrig

                  subRegShape <- NULL
                  subRegType_ix<- NULL

                  if(all(is.null(subRegShapeOrig) & is.null(subRegShpFileOrig))){

                    # Telescope
                    mapFound <- metis.mapFind(shapeTbl)
                    shapeTbl = mapFound$dataTblFound
                    subRegShape = mapFound$subRegShapeFound
                    subRegType_ix = mapFound$subRegShapeTypeFound
                    if(is.null(subRegType_ix)){subRegType_ix="subRegShapeType"}

                    if(!is.null(shapeTbl)){

                    shapeTbl <- shapeTbl %>% dplyr::mutate(subRegType=subRegType_ix)

                    runSection = T
                    if(is.null(subRegShape)){
                      print(paste("For the selected param: ", param_i," and scenario: ", scenario_i,sep=""))
                      print("None of the pre-loaded shapefiles contain all the subRgions specified in a single shapefile.")
                      print(paste("subRegions specified: ",paste(unique((shapeTbl%>%dplyr::filter(param==param_i,scenario==scenario_i))$subRegion),collapse=", "),sep=""))
                      print(paste("Skipping map for param: ", param_i," and scenario: ", scenario_i,sep=""))
                      print("Please load a subRegShape directly for your region of interest.")
                      runSection = F
                    }

                  }else{
                    runSection = F
                  } # If is.null shapeTbl
                  }
                  else {
                    if(nrow(shapeTbl%>%dplyr::filter(param==param_i,scenario==scenario_i))>0){
                      subRegType_ix <- unique((shapeTbl%>%dplyr::filter(param==param_i,scenario==scenario_i))$subRegType)
                    }else{subRegType_ix="subRegion"}
                    subRegShape <- subRegShapeOrig
                  }


                  if(runSection){
                    #--------------------------------
                    # Checking Shapefiles
                    #--------------------------------
                    if(T){ # Checking Shapefiles

                      if(is.null(boundaryRegShapeOrig)){
                        if(!is.null(boundaryRegShpFolderOrig) & !is.null(boundaryRegShpFileOrig)){
                          if(!dir.exists(boundaryRegShpFolderOrig)){
                            stop("Shapefile folder: ", boundaryRegShpFolderOrig ," is incorrect or doesn't exist.",sep="")}
                          if(!file.exists(paste(boundaryRegShpFolderOrig,"/",boundaryRegShpFileOrig,".shp",sep=""))){
                            stop("Shape file: ", paste(boundaryRegShpFolderOrig,"/",boundaryRegShpFileOrig,".shp",sep="")," is incorrect or doesn't exist.",sep="")}
                          boundaryRegShape=rgdal::readOGR(dsn=boundaryRegShpFolderOrig,layer=boundaryRegShpFileOrig,use_iconv=T,encoding='UTF-8')
                          print(paste("Sub Reg Shape : ",boundaryRegShpFolderOrig,"/",boundaryRegShpFileOrig,".shp",sep=""))
                          print(raster::head(boundaryRegShape))
                        } else {
                         # If only boundary regionsSelect have been chosen then try and find a shapefile with those regions
                          if(!is.null(boundaryRegionsSelect)){
                            mapFound <- metis.mapFind(data.frame(subRegion=boundaryRegionsSelect))
                            boundaryRegShape <- mapFound$subRegShapeFound
                            boundaryRegionsSelect <- unique(mapFound$dataTblFound$subRegion)
                          }else{
                            mapFound <- metis.mapFind(data.frame(subRegion=unique(shapeTbl$subRegion)))
                            boundaryRegShape <- mapFound$subRegShapeFound
                          }
                        }
                      }

                  if(is.null(subRegShape)){
                    if(!is.null(subRegShpFolderOrig) & !is.null(subRegShpFileOrig)){
                      if(!dir.exists(subRegShpFolderOrig)){
                        stop("Shapefile folder: ", subRegShpFolderOrig ," is incorrect or doesn't exist.",sep="")}
                      if(!file.exists(paste(subRegShpFolderOrig,"/",subRegShpFileOrig,".shp",sep=""))){
                        stop("Shape file: ", paste(subRegShpFolderOrig,"/",subRegShpFileOrig,".shp",sep="")," is incorrect or doesn't exist.",sep="")}
                      subRegShape=rgdal::readOGR(dsn=subRegShpFolderOrig,layer=subRegShpFileOrig,use_iconv=T,encoding='UTF-8')
                      print(paste("Sub Reg Shape : ",subRegShpFolderOrig,"/",subRegShpFileOrig,".shp",sep=""))
                      print(raster::head(subRegShape))
                    } # if(!is.null(subRegShpFolder) & !is.null(subRegShpFile)){
                  }


                  if(is.null(subRegShape)){
                    stop("No valid subregional shape file available")}


                  if(!subRegCol %in% names(subRegShape)){stop(paste("SubRegCol: ",subRegCol," not present in subRegShape",sep=""))}

                   if(!is.null(subRegShape)){
                    subRegShape@data[[subRegCol]] <- as.character(subRegShape@data[[subRegCol]])
                    if(!all(unique((shapeTbl%>%dplyr::filter(param==param_i,scenario==scenario_i))$subRegion) %in% unique(subRegShape@data[[subRegCol]]))){
                      print(paste("Removing subRegions not present in shapefile from datatable: ",
                                  paste(unique((shapeTbl%>%dplyr::filter(param==param_i,scenario==scenario_i))$subRegion)[!unique((shapeTbl%>%dplyr::filter(param==param_i,scenario==scenario_i))$subRegion) %in% unique(subRegShape@data[[subRegCol]])],collapse=", "),sep=""))
                      shapeTbl <- shapeTbl%>%dplyr::filter(param==param_i,scenario==scenario_i)%>% dplyr::filter(subRegion %in% unique(subRegShape@data[[subRegCol]]))
                      print(paste("Remaining subRegions in dataTable are: ",paste(unique(shapeTbl$subRegion),collapse=", "),sep=""))}
                  }

                  if(!any(unique((shapeTbl%>%dplyr::filter(param==param_i,scenario==scenario_i))$subRegion) %in% unique(subRegShape@data[[subRegCol]]))){
                    print(paste("None of the subRegions in data provided for param: ", param_i,
                                " match subRegions in the shapefile provided or available.",sep=""))
                  }

                  subRegShape@data<-subRegShape@data%>%dplyr::mutate(subRegion=get(subRegCol))



                  #----------------
                  # Create Boundary and subRegional shapefiles
                  #---------------

                  if(!is.null(boundaryRegShape) & !is.null(subRegShape)){
                    if(!is.null(boundaryRegionsSelect)){
                      if(any(boundaryRegionsSelect %in% unique(boundaryRegShape@data[[boundaryRegCol]]))){
                        boundaryRegShapeLimits <- boundaryRegShape[boundaryRegShape@data[[boundaryRegCol]] %in% boundaryRegionsSelect,]
                        boundaryRegShapeLimits@data <- droplevels(boundaryRegShapeLimits@data)
                        bbox1<-as.data.frame(sp::bbox(boundaryRegShapeLimits))
                        bbox1$min;bbox1$max
                        rangeX<-abs(range(bbox1$min[1],bbox1$max[1])[2]-range(bbox1$min[1],bbox1$max[1])[1])
                        rangeY<-abs(range(bbox1$min[2],bbox1$max[2])[2]-range(bbox1$min[2],bbox1$max[2])[1])
                        bbox1$min[1]<-min(180,max(-180,(-rangeX*expandPercent/100)+bbox1$min[1]));
                        bbox1$min[2]<-min(90,max(-90,(-rangeY*expandPercent/100)+bbox1$min[2]));
                        bbox1$max[1]<-max(-180,min(180,(rangeX*expandPercent/100)+bbox1$max[1]));
                        bbox1$max[2]<-max(-90,min(90,(rangeY*expandPercent/100)+bbox1$max[2]));
                        bbox1$min;bbox1$max;
                        bbox1<-methods::as(raster::extent(as.vector(t(bbox1))), "SpatialPolygons")
                        sp::proj4string(bbox1)<-sp::CRS(projX) # ASSIGN COORDINATE SYSTEM
                        boundaryRegShape <- sp::spTransform(boundaryRegShape,sp::CRS(projX))
                        boundaryRegShape<-raster::crop(boundaryRegShape, bbox1)
                        boundaryRegShape@bbox <- bbox1@bbox
                        boundaryRegShape@data <- droplevels(boundaryRegShape@data)
                        print("Map cropped to regions with data. To plot full map extent set crop2Boundary = F.")
                        subRegShape <- sp::spTransform(subRegShape,raster::crs(boundaryRegShape))
                        subRegShape <- raster::crop(subRegShape,boundaryRegShape)
                        print("Scale will still include all data from original subRegShape extents")
                        expandPercent_i = 0 # Preventing extension for doubling expansion

                      } else {print(paste("boundaryRegionsSelect chosen are not available in the boundaryRegShapeFile.",paste(boundaryRegionsSelect,collapse=", "),sep=""))}
                    }else{
                      boundaryRegShape <- sp::spTransform(boundaryRegShape,raster::crs(subRegShape))
                      subRegShape <- raster::crop(subRegShape,boundaryRegShape)
                      subRegShape@data <- droplevels(subRegShape@data)
                    }
                  }


                  shape<-subRegShape

                  if(!subRegCol %in% names(shape)){stop(paste("SubRegCol: ",subRegCol," not present in shape",sep=""))}

                  shape@data<-shape@data%>%dplyr::mutate(subRegion=get(subRegCol), subRegion=as.character(subRegion))

                  #----------------
                  # Create Boundary Extension
                  #---------------

                  bgColorChosen="white"

                  if(extension==T){

                    frameShow=T;facetLabelBorderLwd=0.1;facetBGColor="grey30";facetLabelColor = "white"

                    if(is.null(extendedShape)){

                      extendedBoundary <- metis::mapCountries
                      bbox1<-as.data.frame(sp::bbox(shape))
                      extendedShapeCol<-subRegCol


                      if(!is.null(bbox1)){
                        bbox1$min;bbox1$max
                        rangeX<-abs(range(bbox1$min[1],bbox1$max[1])[2]-range(bbox1$min[1],bbox1$max[1])[1])
                        rangeY<-abs(range(bbox1$min[2],bbox1$max[2])[2]-range(bbox1$min[2],bbox1$max[2])[1])
                        bbox1$min[1]<-min(180,max(-180,(-rangeX*expandPercent_i/100)+bbox1$min[1]));
                        bbox1$min[2]<-min(90,max(-90,(-rangeY*expandPercent_i/100)+bbox1$min[2]));
                        bbox1$max[1]<-max(-180,min(180,(rangeX*expandPercent_i/100)+bbox1$max[1]));
                        bbox1$max[2]<-max(-90,min(90,(rangeY*expandPercent_i/100)+bbox1$max[2]));
                        bbox1$min;bbox1$max;
                        bbox1<-methods::as(raster::extent(as.vector(t(bbox1))), "SpatialPolygons")
                        sp::proj4string(bbox1)<-sp::CRS(projX) # ASSIGN COORDINATE SYSTEM
                        boundaryRegShape <- sp::spTransform(boundaryRegShape,sp::CRS(projX))
                        print("Creating extended boundary...")
                        extendedShape<-raster::crop(extendedBoundary, bbox1)
                        extendedShape@bbox <- bbox1@bbox
                        if(!is.null(boundaryRegShape)){extendedShape<-raster::crop(extendedShape, boundaryRegShape)}
                        extendedBGColor="lightblue1"
                      }else{print("No extended boundary.")}
                    }

                    if(!is.null(extendedShape)){
                      if(extendedShapeCol %in% names(extendedShape)){
                        underLayer<-metis.map(facetsOn=F,innerMargins=innerMargins, legendDigitsOverride=legendDigitsOverride,facetLabelSize=facetLabelSize,mapTitleOn=mapTitleOn, facetCols=facetCols,fillcolorNA=fillcolorNA,fillshowNA=fillshowNA,fillcolorNULL=fillcolorNULL, dataPolygon=extendedShape, printFig=F,labelsAutoPlace = F,
                                              fillColumn = extendedShapeCol,labels=extendedLabels, fillPalette = extendedFillColor,legendShow=F,
                                              bgColor = extendedBGColor, frameShow=T,facetLabelBorderLwd=facetLabelBorderLwd,labelsSize=extdendedLabelSize, labelsColor=extendedLabelsColor,
                                              figWidth=figWidth,figHeight=figHeight, pdfpng = pdfpng)
                        bgColorChosen= extendedBGColor
                      }
                    }
                  }
                  }

                    if(nrow(shapeTbl)>0){ # nrow shapeTbl after Shapefiles loaded

    #------------------
    # Create Shape Table Folders If Needed
    #------------------
    if(TRUE){
       if (!dir.exists(paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,sep = ""))){
          dir.create(paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,sep = ""))}
       if (!dir.exists(paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,sep = ""))){
              dir.create(paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,sep = ""))}
       if (!dir.exists(paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/", scenario_i,sep = ""))){
          dir.create(paste(dirOutputsX, "/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/",  scenario_i,sep = ""))}
        if (!dir.exists(paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/", scenario_i,"/byYear",sep = ""))){
          dir.create(paste(dirOutputsX, "/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/", scenario_i,"/byYear",sep = ""))}
      if (!dir.exists(paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/compareMultiFacets",sep = ""))){
        dir.create(paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/compareMultiFacets",sep = ""))}
      if (!dir.exists(paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/compareMultiFacets/byYear",sep = ""))){
        dir.create(paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/compareMultiFacets/byYear",sep = ""))}
      if (!dir.exists(paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/compareMultiFacets/compareYear",sep = ""))){
        dir.create(paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/compareMultiFacets/compareYear",sep = ""))}
            }


                  #------------------
                  # Plots
                  #------------------


                  if(nrow(shapeTblMult%>%dplyr::filter(class==class_i,param==param_i))>0){

                    shapeTblMultx<-shapeTblMult%>%dplyr::filter(param==param_i,class==class_i)

                    shapeTblMultxScenMultiABRefRef <- shapeTblScenMultiABRef%>%dplyr::filter(!(multiFacetCol=="MultiANone" | multiFacetRow=="MultiBNone" | is.na(multiFacetCol) | is.na(multiFacetRow))) %>%
                                                  dplyr::filter(param==param_i,class==class_i)


                    if(length(unique(shapeTblMultx$multiFacetCol))+length(unique(shapeTblMultx$multiFacetRow))>1){


                    animScalePoly<-(shapeTblMultxScenMultiABRefRef %>%
                                      dplyr::filter(!is.na(value),!is.infinite(value),!is.nan(value)))$value

                    # Choose correct scaleRange
                    scaleRange_i=scaleRange
                    if(grepl("DiffPrcnt",scenario_i)){
                      scaleRange_i=scaleRangeDiffPrcnt
                      shapeTblMultxScenMultiABRefRef <- shapeTblMultxScenMultiABRefRef %>% dplyr::mutate(units="Percent")
                      if(is.null(legendSingleColorOnOrig)){legendSingleColorOn=T}else{legendSingleColorOn=legendSingleColorOnOrig}
                    }
                    if(grepl("DiffAbs",scenario_i)){
                      scaleRange_i=scaleRangeDiffAbs
                      if(is.null(legendSingleColorOnOrig)){legendSingleColorOn=T}else{legendSingleColorOn=legendSingleColorOnOrig}
                    }
                    if(grepl("DiffxPrcnt",scenario_i)){
                      scaleRange_i=scaleRangeDiffxPrcnt
                      shapeTblMultxScenMultiABRefRef <- shapeTblMultxScenMultiABRefRef %>% dplyr::mutate(units="Percent")
                      if(is.null(legendSingleColorOnOrig)){legendSingleColorOn=T}else{legendSingleColorOn=legendSingleColorOnOrig}
                    }
                    if(grepl("DiffxAbs",scenario_i)){
                      scaleRange_i=scaleRangeDiffxAbs
                      if(is.null(legendSingleColorOnOrig)){legendSingleColorOn=T}else{legendSingleColorOn=legendSingleColorOnOrig}
                    }

                    if(is.null(legendSingleColorOnOrig)){
                      if(min(range(shapeTblMultxScenMultiABRefRef$value))<0 & max(range(shapeTblMultxScenMultiABRefRef$value))>0){
                        legendSingleColorOn=T}}else{legendSingleColorOn=legendSingleColorOnOrig}


                    if(!is.null(scaleRange_i)){
                       if(any(param_i %in% unique(scaleRange_i$param))){
                      if(max(animScalePoly) < (scaleRange_i %>% dplyr::filter(param==param_i))$maxScale){
                        animScalePoly<-c(animScalePoly,(scaleRange_i %>% dplyr::filter(param==param_i))$maxScale)} else {
                        animScalePoly <-  c((scaleRange_i %>% dplyr::filter(param==param_i))$maxScale,
                                            animScalePoly[animScalePoly<(scaleRange_i %>% dplyr::filter(param==param_i))$maxScale])
                        }
                      if(min(animScalePoly) > (scaleRange_i %>% dplyr::filter(param==param_i))$minScale){
                        animScalePoly<-c(animScalePoly,(scaleRange_i %>% dplyr::filter(param==param_i))$minScale)} else {
                          animScalePoly <-  c((scaleRange_i %>% dplyr::filter(param==param_i))$minScale,
                                              animScalePoly[animScalePoly>(scaleRange_i %>% dplyr::filter(param==param_i))$minScale])
                        }
                      }
                      }
                    animPrettyBreaksPoly<-scales::pretty_breaks(n=legendFixedBreaks)(animScalePoly)
                    animKmeanBreaksPoly<-sort(as.vector((stats::kmeans(animScalePoly,
                                                                       centers=max(1,min(length(unique(animScalePoly))-1,(legendFixedBreaks-1)))))$centers[,1]))


                    if(!min(animScalePoly) %in% animKmeanBreaksPoly){
                      animKmeanBreaksPoly <- sort(c(min(animScalePoly),animKmeanBreaksPoly))}
                    if(!max(animScalePoly) %in% animKmeanBreaksPoly){
                      animKmeanBreaksPoly <- sort(c(animKmeanBreaksPoly,max(animScalePoly)))}


                    if((max(range(animScalePoly))-min(range(animScalePoly)))<1E-10 &
                       (max(range(animScalePoly))-min(range(animScalePoly)))>-1E-10){animScalePolyRange=min(animScalePoly)}else{
                         animScalePolyRange=range(animScalePoly)
                       }
                    if(abs(min(animScalePolyRange,na.rm = T))==abs(max(animScalePolyRange,na.rm = T))){animScalePolyRange=abs(min(animScalePolyRange,na.rm = T))}
                    if(mean(animScalePolyRange,na.rm = T)<0.01 & mean(animScalePolyRange,na.rm = T)>(-0.01)){animLegendDigits<-4}else{
                      if(mean(animScalePolyRange,na.rm = T)<0.1 & mean(animScalePolyRange,na.rm = T)>(-0.1)){animLegendDigits<-3}else{
                        if(mean(animScalePolyRange,na.rm = T)<1 & mean(animScalePolyRange,na.rm = T)>(-1)){animLegendDigits<-2}else{
                          if(mean(animScalePolyRange,na.rm = T)<10 & mean(animScalePolyRange,na.rm = T)>(-10)){animLegendDigits<-1}else{animLegendDigits<-0}}}}

                    animKmeanBreaksPolyOrig<-animKmeanBreaksPoly
                    animPrettyBreaksPolyOrig<-animPrettyBreaksPoly
                    animScalePolyOrig<-animScalePoly
                    animScalePolyRangeOrig<-animScalePolyRange
                    animLegendDigitsOrig<-animLegendDigits

                    #------------------------------------
                    # Plot Mean values for the Reference Case and chosen years
                    #-----------------------------------

                    if(is.null(chosenRefMeanYears)){chosenRefMeanYearsX<-unique(shapeTblMultxScenMultiABRefRef$x)}else{chosenRefMeanYearsX<-chosenRefMeanYears}

                    datax <- shapeTblMultxScenMultiABRefRef %>% dplyr::filter(x %in% chosenRefMeanYearsX)
                    minX<-min(datax$x);maxX<-max(datax$x)

                    if(length(unique(datax$x))>1){

                    if(nrow(datax)>0){
                      legendTitle<-unique(datax$units)
                      if(!is.null(classPaletteOrig)){fillPalette<-classPaletteOrig}else{
                        fillPalette<-as.character(unique(datax$classPalette))}

                      meanCol = paste("Mean_",min(chosenRefMeanYearsX),"to",max(chosenRefMeanYearsX),sep="")

                      datax<-datax%>%dplyr::select(subRegion,class,value,multiFacetCol,multiFacetRow)%>%
                        dplyr::group_by(subRegion,multiFacetCol,multiFacetRow) %>%
                        dplyr::summarize(!!meanCol:=mean(value)) %>%
                        dplyr::ungroup()

                      # Need to makeunique ID's when assigning multiple variable for faceted plotting
                      mapx<-NULL
                      ScenMultiABRefcomb<-datax%>%dplyr::select(multiFacetCol,multiFacetRow)%>%dplyr::ungroup()%>%dplyr::distinct();ScenMultiABRefcomb

                      animScalePoly<-datax[[meanCol]]; animScalePoly

                      # Choose correct scaleRange
                      scaleRange_i=scaleRange
                      if(grepl("DiffPrcnt",scenario_i)){
                        scaleRange_i=scaleRangeDiffPrcnt
                        ScenMultiABRefcomb <- ScenMultiABRefcomb %>% dplyr::mutate(units="Percent")
                        if(is.null(legendSingleColorOnOrig)){legendSingleColorOn=T}else{legendSingleColorOn=legendSingleColorOnOrig}
                      }
                      if(grepl("DiffAbs",scenario_i)){
                        scaleRange_i=scaleRangeDiffAbs
                        if(is.null(legendSingleColorOnOrig)){legendSingleColorOn=T}else{legendSingleColorOn=legendSingleColorOnOrig}
                      }
                     if(grepl("DiffxPrcnt",scenario_i)){
                        scaleRange_i=scaleRangeDiffPrcnt
                        ScenMultiABRefcomb <- ScenMultiABRefcomb %>% dplyr::mutate(units="Percent")
                        if(is.null(legendSingleColorOnOrig)){legendSingleColorOn=T}else{legendSingleColorOn=legendSingleColorOnOrig}
                      }
                      if(grepl("DiffxAbs",scenario_i)){
                        scaleRange_i=scaleRangeDiffAbs
                        if(is.null(legendSingleColorOnOrig)){legendSingleColorOn=T}else{legendSingleColorOn=legendSingleColorOnOrig}
                      }

                      if(is.null(legendSingleColorOnOrig)){
                      if(min(range(ScenMultiABRefcomb$value))<0 & max(range(ScenMultiABRefcomb$value))>0){
                        legendSingleColorOn=T}}else{legendSingleColorOn=legendSingleColorOnOrig}

                      if(!is.null(scaleRange_i)){
                         if(any(param_i %in% unique(scaleRange_i$param))){
                          if(max(animScalePoly) < (scaleRange_i %>% dplyr::filter(param==param_i))$maxScale){
                            animScalePoly<-c(animScalePoly,(scaleRange_i %>% dplyr::filter(param==param_i))$maxScale)} else {
                              animScalePoly <-  c((scaleRange_i %>% dplyr::filter(param==param_i))$maxScale,
                                                  animScalePoly[animScalePoly<(scaleRange_i %>% dplyr::filter(param==param_i))$maxScale])
                            }
                          if(min(animScalePoly) > (scaleRange_i %>% dplyr::filter(param==param_i))$minScale){
                            animScalePoly<-c(animScalePoly,(scaleRange_i %>% dplyr::filter(param==param_i))$minScale)} else {
                              animScalePoly <-  c((scaleRange_i %>% dplyr::filter(param==param_i))$minScale,
                                                  animScalePoly[animScalePoly>(scaleRange_i %>% dplyr::filter(param==param_i))$minScale])
                            }
                        }
                      }
                      animPrettyBreaksPoly<-scales::pretty_breaks(n=legendFixedBreaks)(animScalePoly)
                      animKmeanBreaksPoly<-sort(as.vector((stats::kmeans(animScalePoly,
                                                                         centers=max(1,min(length(unique(animScalePoly))-1,(legendFixedBreaks-1)))))$centers[,1]))


                      if(!min(animScalePoly) %in% animKmeanBreaksPoly){
                        animKmeanBreaksPoly <- sort(c(min(animScalePoly),animKmeanBreaksPoly))}
                      if(!max(animScalePoly) %in% animKmeanBreaksPoly){
                        animKmeanBreaksPoly <- sort(c(animKmeanBreaksPoly,max(animScalePoly)))}


                      if((max(range(animScalePoly))-min(range(animScalePoly)))<1E-10 &
                         (max(range(animScalePoly))-min(range(animScalePoly)))>-1E-10){animScalePolyRange=min(animScalePoly)}else{
                           animScalePolyRange=range(animScalePoly)
                         }
                      if(abs(min(animScalePolyRange,na.rm = T))==abs(max(animScalePolyRange,na.rm = T))){animScalePolyRange=abs(min(animScalePolyRange,na.rm = T))}
                      if(mean(animScalePolyRange,na.rm = T)<0.01 & mean(animScalePolyRange,na.rm = T)>(-0.01)){animLegendDigits<-4}else{
                        if(mean(animScalePolyRange,na.rm = T)<0.1 & mean(animScalePolyRange,na.rm = T)>(-0.1)){animLegendDigits<-3}else{
                          if(mean(animScalePolyRange,na.rm = T)<1 & mean(animScalePolyRange,na.rm = T)>(-1)){animLegendDigits<-2}else{
                            if(mean(animScalePolyRange,na.rm = T)<10 & mean(animScalePolyRange,na.rm = T)>(-10)){animLegendDigits<-1}else{animLegendDigits<-0}}}}


                      # # Add in any missing subRegions to datax
                      # datax1<-expand.grid(unique(shape@data$subRegion)[!unique(shape@data$subRegion) %in% unique(datax$subRegion)],
                      #                     unique(datax$multiFacetCol),unique(datax$multiFacetRow)) %>%
                      #   dplyr::select(subRegion=Var1, multiFacetCol=Var2, multiFacetRow=Var3) %>%
                      #   dplyr::mutate(!!names(datax)[4]:=0)
                      #
                      # datax <- datax %>% dplyr::bind_rows(datax1%>%dplyr::mutate(subRegion=as.character(subRegion))) %>% dplyr::mutate(subRegion=as.factor(subRegion))

                      for (row_i in 1:nrow(ScenMultiABRefcomb)){
                        MultiA_i <- ScenMultiABRefcomb[row_i,]$multiFacetCol
                        MultiB_i <- ScenMultiABRefcomb[row_i,]$multiFacetRow
                        a1<-shape
                        a1@data<-a1@data%>%dplyr::left_join(datax%>%dplyr::filter(multiFacetCol==MultiA_i,multiFacetRow==MultiB_i),by="subRegion")%>%
                          dplyr::select(names(datax))
                        if(is.null(mapx)){mapx<-a1}else{mapx<-rbind(mapx,a1,makeUniqueIDs = TRUE)}
                      }

                      if(length(unique(animScalePoly))==1){legendStyleMulti="kmeans"}else{
                      legendStyleMulti="fixed"}
                      legendTitleMulti=paste(paste("Mean_",minX,"to",maxX,sep=""),"\n",legendTitle,sep="")
                      panelLabelMulti=NULL

                      metis.map(facetsOn=T,innerMargins=innerMargins, legendDigitsOverride=legendDigitsOverride,mapTitleOn=mapTitleOn, facetCols=facetCols,numeric2Cat_list=numeric2Cat_list, catParam=param_i, panelLabel=panelLabelMulti,
                                underLayer=NULL, dataPolygon=mapx,
                                fillColumn = names(mapx@data%>%dplyr::select(-subRegion,-multiFacetCol,-multiFacetRow)),
                                mapTitle=paste(param_i," Ref Years ",sep="") , legendShow = T,
                                legendOutside = legendOutsideMulti,
                                facetFreeScale = F,
                                facetLabelSize = facetLabelSizeMultiAB*max(min(1,length(unique(mapx[[multiFacetRows]]))*length(unique(mapx[[multiFacetCols]]))/10),2),
                                frameShow = frameShow, facetLabelBorderLwd=facetLabelBorderLwd, facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor ,
                          legendSingleColorOn=legendSingleColorOn,legendSingleValue=legendSingleValue,legendSingleColor=legendSingleColor,
                                labels=labels,fillcolorNA=fillcolorNA,fillshowNA=fillshowNA,fillcolorNULL=fillcolorNULL,
                                labelsSize = labelsSize,
                                legendTitleSize = legendTitleSizeMulti,legendTextSize = legendTextSizeMulti,
                                legendTitle =legendTitleMulti,
                                legendStyle=legendStyleMulti,
                                legendBreaks = animKmeanBreaksPoly,
                                legendFixedBreaks=legendFixedBreaks,
                                legendDigits = animLegendDigits,
                                legendOutsidePosition = legendOutsidePosition,
                                legendPosition = legendPositionMulti,
                                fillPalette = fillPalette,
                                bgColor = "white",
                                figWidth=figWidth*max(1,min(2,length(unique(mapx@data[[multiFacetCols]]))/2)),
                                figHeight=figHeight*max(1,min(2,length(unique(mapx@data[[multiFacetRows]]))/2)),
                                multiFacetRows="multiFacetRow",
                                multiFacetCols="multiFacetCol",
                                fileName = paste("map_",subRegType_ix,"_",param_i,"_RefYears_",class_i,nameAppend,"_MEAN_KMEANS",sep=""),
                                dirOutputs = paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/compareMultiFacets",sep = ""))


                      # panelLabel=panelLabelMulti
                      # underLayer=NULL
                      # dataPolygon=mapx
                      # fillColumn = names(mapx@data%>%dplyr::select(-subRegion,-multiFacetCol,-multiFacetRow))
                      # mapTitle=paste(param_i,sep="") , legendShow = T
                      # legendOutside = legendOutsideMulti
                      # facetFreeScale = F
                      # facetLabelSize = facetLabelSizeMultiAB*max(min(1,length(unique(mapx[[multiFacetRows]]))*length(unique(mapx[[multiFacetCols]]))/10),2),
                      # frameShow = frameShow
                      # labels=labels
                      # labelsSize = labelsSize
                      # legendTitleSize = legendTitleSizeMulti
                      # legendTextSize = legendTextSizeMulti
                      # legendTitle =legendTitleMulti
                      # legendStyle=legendStyleMulti
                      # legendBreaks = animKmeanBreaksPoly
                      # legendFixedBreaks=legendFixedBreaks
                      # legendDigits = animLegendDigits
                      # legendOutsidePosition = legendOutsidePosition
                      # legendPosition = legendPositionMulti
                      # fillPalette = fillPalette
                      # bgColor = "white"
                      # figWidth=figWidth*max(1,min(2,length(unique(mapx@data[[multiFacetCols]]))/2))
                      # figHeight=figHeight*max(1,min(2,length(unique(mapx@data[[multiFacetRows]]))/2))
                      # multiFacetRows=multiFacetRows
                      # multiFacetCols=multiFacetCols
                      # fileName = paste("map_",subRegType_ix,"_",param_i,"_",class_i,nameAppend,"_MEAN_KMEANS",sep="")
                      # dirOutputs = paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/compareMultiFacets",sep = "")


                      metis.map(facetsOn=T,innerMargins=innerMargins, legendDigitsOverride=legendDigitsOverride,mapTitleOn=mapTitleOn, facetCols=facetCols,numeric2Cat_list=numeric2Cat_list, catParam=param_i, panelLabel=panelLabelMulti,
                                underLayer=NULL, dataPolygon=mapx,
                                fillColumn = names(mapx@data%>%dplyr::select(-subRegion,-multiFacetCol,-multiFacetRow)),
                                mapTitle=paste(param_i," Ref Years ",sep="") , legendShow = T,
                                legendOutside = legendOutsideMulti,
                                facetLabelSize = facetLabelSizeMultiAB*max(min(1,length(unique(mapx[[multiFacetRows]]))*length(unique(mapx[[multiFacetCols]]))/10),2),
                                facetFreeScale = F,
                                frameShow = frameShow, facetLabelBorderLwd=facetLabelBorderLwd, facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor ,
                          legendSingleColorOn=legendSingleColorOn,legendSingleValue=legendSingleValue,legendSingleColor=legendSingleColor,
                                labels=labels,fillcolorNA=fillcolorNA,fillshowNA=fillshowNA,fillcolorNULL=fillcolorNULL,
                                labelsSize = labelsSize,
                                legendTitleSize = legendTitleSizeMulti,legendTextSize = legendTextSizeMulti,
                                legendTitle =legendTitleMulti,
                                legendStyle=legendStyleMulti,
                                legendBreaks = animPrettyBreaksPoly,
                                legendFixedBreaks=legendFixedBreaks,
                                legendDigits = animLegendDigits,
                                legendOutsidePosition = legendOutsidePosition,
                                legendPosition = legendPositionMulti,
                                fillPalette = fillPalette,
                                bgColor = "white",
                                figWidth=figWidth*max(1,min(2,length(unique(mapx@data[[multiFacetCols]]))/2)),
                                figHeight=figHeight*max(1,min(2,length(unique(mapx@data[[multiFacetRows]]))/2)),
                                multiFacetRows="multiFacetRow",
                                multiFacetCols="multiFacetCol",
                                fileName = paste("map_",subRegType_ix,"_",param_i,"_RefYears_",class_i,nameAppend,"_MEAN_PRETTY",sep=""),
                                dirOutputs = paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/compareMultiFacets",sep = ""))


                      metis.map(facetsOn=T,innerMargins=innerMargins, legendDigitsOverride=legendDigitsOverride,mapTitleOn=mapTitleOn, facetCols=facetCols,numeric2Cat_list=numeric2Cat_list, catParam=param_i, panelLabel=panelLabelMulti,
                                underLayer=NULL, dataPolygon=mapx,
                                fillColumn = names(mapx@data%>%dplyr::select(-subRegion,-multiFacetCol,-multiFacetRow)),
                                mapTitle=paste(param_i," Ref Years ",sep="") , legendShow = T,
                                legendOutside = F,
                                facetFreeScale = T,
                                facetLabelSize = facetLabelSizeMultiAB*max(min(1,length(unique(mapx[[multiFacetRows]]))*length(unique(mapx[[multiFacetCols]]))/10),2),
                                frameShow = frameShow, facetLabelBorderLwd=facetLabelBorderLwd, facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor ,
                          legendSingleColorOn=legendSingleColorOn,legendSingleValue=legendSingleValue,legendSingleColor=legendSingleColor,
                                labels=labels,fillcolorNA=fillcolorNA,fillshowNA=fillshowNA,fillcolorNULL=fillcolorNULL,
                                labelsSize = labelsSize,
                                legendTitleSize = legendTitleSizeI,legendTextSize = legendTextSizeI,
                                legendTitle =legendTitleMulti,
                                legendStyle="kmeans",
                                legendFixedBreaks=legendFixedBreaks,
                                legendDigits = NULL,
                                legendOutsidePosition = legendOutsidePosition,
                                legendPosition = legendPosition,
                                fillPalette = fillPalette,
                                bgColor = "white",
                                figWidth=figWidth*max(1,min(2,length(unique(mapx@data[[multiFacetCols]]))/2)),
                                figHeight=figHeight*max(1,min(2,length(unique(mapx@data[[multiFacetRows]]))/2)),
                                multiFacetRows="multiFacetRow",
                                multiFacetCols="multiFacetCol",
                                fileName = paste("map_",subRegType_ix,"_",param_i,"_RefYears_",class_i,nameAppend,"_MEAN_FREESCALE",sep=""),
                                dirOutputs = paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/compareMultiFacets",sep = ""))



                      if(!is.null(refMultiA)){
                        if(!any(refMultiA %in% unique(shapeTblMultxScenMultiABRefRef$multiFacetCol))){
                          print(paste(refMultiA," not available in available MultiAs: ",paste(unique(shapeTblMultxScenMultiABRefRef$multiFacetCol),collapse=", "),sep=""))
                          print(paste("Setting refMultiA as : ",paste(unique(shapeTblMultxScenMultiABRefRef$multiFacetCol)[1],collapse=", "),sep=""))
                          refMultiA=unique(shapeTblMultxScenMultiABRefRef$multiFacetCol)[1]
                        }
                      }

                      if(!is.null(refMultiB)){
                        if(!any(refMultiB %in% unique(shapeTblMultxScenMultiABRefRef$multiFacetRow))){
                          print(paste(refMultiB," not available in available MultiBs: ",paste(unique(shapeTblMultxScenMultiABRefRef$multiFacetRow),collapse=", "),sep=""))
                          print(paste("Setting refMultiB as : ",paste(unique(shapeTblMultxScenMultiABRefRef$multiFacetRow)[1],collapse=", "),sep=""))
                          refMultiB=unique(shapeTblMultxScenMultiABRefRef$multiFacetRow)[1]
                        }
                      }

                      # TODO Multi Ref Diffs
                      # shapeTblMultxDiff<-shapeTblMultx%>%
                      #   dplyr::left_join(datax%>%
                      #                      dplyr::filter(multiFacetCol==refMultiA,
                      #                                    multiFacetRow==refMultiB)%>%
                      #                      dplyr::select(-multiFacetCol,-multiFacetRow),
                      #                    by=c("subRegion"))%>%
                      #   dplyr::mutate(valueDiff:=value-get(names(datax%>%dplyr::select(-multiFacetCol,-multiFacetRow,-subRegion))))
                      # shapeTblMultxDiff


                      animKmeanBreaksPolyOrig<-animKmeanBreaksPolyOrig
                      animPrettyBreaksPolyOrig<-animPrettyBreaksPolyOrig
                      animScalePolyOrig<-animScalePolyOrig
                      animScalePolyRangeOrig<-animScalePolyRangeOrig
                      animLegendDigitsOrig<-animLegendDigitsOrig

                      #----------------------------------
                      # Plot For Each Year Regular
                      #---------------------------------

                      if(any(xRangeOrig=="All") | any(xRangeOrig =="all")){xRangeMult <-unique(shapeTblMult$x)}
                      for (x_i in unique(shapeTblMult$x)[unique(shapeTblMult$x) %in% xRangeMult]){

                        datax<-shapeTblMultx%>%dplyr::filter(x==x_i)

                        if(nrow(datax)>0){
                          legendTitle<-unique(datax$units)
                          if(!is.null(classPaletteOrig)){fillPalette<-classPaletteOrig}else{
                            fillPalette<-as.character(unique(datax$classPalette))}

                          datax<-datax%>%dplyr::select(subRegion,class,value,multiFacetCol,multiFacetRow)%>%
                            dplyr::distinct(subRegion,class,multiFacetCol,multiFacetRow,.keep_all = TRUE) %>%
                            tidyr::spread(key=class,value=value)

                          # Need to makeunique ID's when assigning multiple variable for faceted plotting
                          mapx<-NULL
                          ScenMultiABRefcomb<-datax%>%dplyr::select(multiFacetCol,multiFacetRow)%>%dplyr::ungroup()%>%dplyr::distinct();ScenMultiABRefcomb

                          # # Add in any missing subRegions to datax
                          # datax1<-expand.grid(unique(shape@data$subRegion)[!unique(shape@data$subRegion) %in% unique(datax$subRegion)],
                          #                     unique(datax$multiFacetCol),unique(datax$multiFacetRow)) %>%
                          #   dplyr::select(subRegion=Var1, multiFacetCol=Var2, multiFacetRow=Var3) %>%
                          #   dplyr::mutate(!!names(datax)[4]:=0)
                          #
                          # datax <- datax %>% dplyr::bind_rows(datax1%>%dplyr::mutate(subRegion=as.character(subRegion))) %>% dplyr::mutate(subRegion=as.factor(subRegion))

                          for (row_i in 1:nrow(ScenMultiABRefcomb)){
                            MultiA_i <- ScenMultiABRefcomb[row_i,]$multiFacetCol
                            MultiB_i <- ScenMultiABRefcomb[row_i,]$multiFacetRow
                            a1<-shape
                            a1@data<-a1@data%>%dplyr::left_join(datax%>%dplyr::filter(multiFacetCol==MultiA_i,multiFacetRow==MultiB_i),by="subRegion")%>%
                              dplyr::select(names(datax))
                            if(is.null(mapx)){mapx<-a1}else{mapx<-rbind(mapx,a1,makeUniqueIDs = TRUE)}
                          }

                          if(length(unique(animScalePoly))==1){legendStyleMulti="kmeans"}else{
                            legendStyleMulti="fixed"}
                          legendTitleMulti=paste(x_i,"\n",legendTitle,sep="")
                          panelLabelMulti=NULL

                          metis.map(facetsOn=T,innerMargins=innerMargins, legendDigitsOverride=legendDigitsOverride,mapTitleOn=mapTitleOn, facetCols=facetCols,numeric2Cat_list=numeric2Cat_list, catParam=param_i, panelLabel=panelLabelMulti,
                                    underLayer=NULL, dataPolygon=mapx,
                                    fillColumn = names(mapx@data%>%dplyr::select(-subRegion,-multiFacetCol,-multiFacetRow)),
                                    mapTitle=paste(param_i,sep="") , legendShow = T,
                                    legendOutside = legendOutsideMulti,
                                    facetFreeScale = F,
                                    facetLabelSize = facetLabelSizeMultiAB*max(min(1,length(unique(mapx[[multiFacetRows]]))*length(unique(mapx[[multiFacetCols]]))/10),2),
                                    frameShow = frameShow, facetLabelBorderLwd=facetLabelBorderLwd, facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor ,
                          legendSingleColorOn=legendSingleColorOn,legendSingleValue=legendSingleValue,legendSingleColor=legendSingleColor,
                                    labels=labels,fillcolorNA=fillcolorNA,fillshowNA=fillshowNA,fillcolorNULL=fillcolorNULL,
                                    labelsSize = labelsSize,
                                    legendTitleSize = legendTitleSizeMulti,legendTextSize = legendTextSizeMulti,
                                    legendTitle =legendTitleMulti,
                                    legendStyle=legendStyleMulti,
                                    legendBreaks = animKmeanBreaksPoly,
                                    legendFixedBreaks=legendFixedBreaks,
                                    legendDigits = animLegendDigits,
                                    legendOutsidePosition = legendOutsidePosition,
                                    legendPosition = legendPositionMulti,
                                    fillPalette = fillPalette,
                                    bgColor = "white",
                                    figWidth=figWidth*max(1,min(2,length(unique(mapx@data[[multiFacetCols]]))/2)),
                                    figHeight=figHeight*max(1,min(2,length(unique(mapx@data[[multiFacetRows]]))/2)),
                                    multiFacetRows="multiFacetRow",
                                    multiFacetCols="multiFacetCol",
                                    fileName = paste("map_",subRegType_ix,"_",param_i,"_",x_i,"_",class_i,nameAppend,"_KMEANS",sep=""),
                                    dirOutputs = paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/compareMultiFacets/byYear",sep = ""))

                          ##ZARRAR
                          # numeric2Cat_list=numeric2Cat_list
                          # catParam=param_i
                          # panelLabel=panelLabelMulti
                          # underLayer=NULL
                          # dataPolygon=mapx
                          # fillColumn = names(mapx@data%>%dplyr::select(-subRegion,-multiFacetCol,-multiFacetRow))
                          # mapTitle=paste(param_i," ",scenario_i,sep="")
                          # legendShow = T
                          # legendOutside = legendOutsideMulti
                          # facetFreeScale = F
                          # facetLabelSize = facetLabelSizeMultiAB*max(min(1,length(unique(mapx[[multiFacetRows]]))*length(unique(mapx[[multiFacetCols]]))/10),2),
                          # frameShow = frameShow
                          # labels=labels
                          # labelsSize = labelsSize
                          # legendTitleSize = legendTitleSizeMulti
                          # legendTextSize = legendTextSizeMulti
                          # legendTitle =legendTitleMulti
                          # legendStyle=legendStyleMulti
                          # legendBreaks = animKmeanBreaksPoly
                          # legendFixedBreaks=legendFixedBreaks
                          # legendDigits = animLegendDigits
                          # legendOutsidePosition = legendOutsidePosition
                          # legendPosition = legendPositionMulti
                          # fillPalette = fillPalette
                          # bgColor = "white"
                          # figWidth=figWidth*max(1,min(2,length(unique(mapx@data[[multiFacetCols]]))/2))
                          # figHeight=figHeight*max(1,min(2,length(unique(mapx@data[[multiFacetRows]]))/2))
                          # multiFacetRows=multiFacetRows
                          # multiFacetCols=multiFacetCols
                          # fileName = paste("map_",subRegType_ix,"_",param_i,"_",x_i,"_",class_i,nameAppend,"_KMEANS",sep="")
                          # dirOutputs = paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/compareMultiFacets/byYear",sep = "")


                          metis.map(facetsOn=T,innerMargins=innerMargins, legendDigitsOverride=legendDigitsOverride,mapTitleOn=mapTitleOn, facetCols=facetCols,numeric2Cat_list=numeric2Cat_list, catParam=param_i, panelLabel=panelLabelMulti,
                                    underLayer=NULL, dataPolygon=mapx,
                                    fillColumn = names(mapx@data%>%dplyr::select(-subRegion,-multiFacetCol,-multiFacetRow)),
                                    mapTitle=paste(param_i,sep="") , legendShow = T,
                                    legendOutside = legendOutsideMulti,
                                    facetFreeScale = F,
                                    facetLabelSize = facetLabelSizeMultiAB*max(min(1,length(unique(mapx[[multiFacetRows]]))*length(unique(mapx[[multiFacetCols]]))/10),2),
                                    frameShow = frameShow, facetLabelBorderLwd=facetLabelBorderLwd, facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor ,
                          legendSingleColorOn=legendSingleColorOn,legendSingleValue=legendSingleValue,legendSingleColor=legendSingleColor,
                                    labels=labels,fillcolorNA=fillcolorNA,fillshowNA=fillshowNA,fillcolorNULL=fillcolorNULL,
                                    labelsSize = labelsSize,
                                    legendTitleSize = legendTitleSizeMulti,legendTextSize = legendTextSizeMulti,
                                    legendTitle =legendTitleMulti,
                                    legendStyle=legendStyleMulti,
                                    legendBreaks = animPrettyBreaksPoly,
                                    legendFixedBreaks=legendFixedBreaks,
                                    legendDigits = animLegendDigits,
                                    legendOutsidePosition = legendOutsidePosition,
                                    legendPosition = legendPositionMulti,
                                    fillPalette = fillPalette,
                                    bgColor = "white",
                                    figWidth=figWidth*max(1,min(2,length(unique(mapx@data[[multiFacetCols]]))/2)),
                                    figHeight=figHeight*max(1,min(2,length(unique(mapx@data[[multiFacetRows]]))/2)),
                                    multiFacetRows="multiFacetRow",
                                    multiFacetCols="multiFacetCol",
                                    fileName = paste("map_",subRegType_ix,"_",param_i,"_",x_i,"_",class_i,nameAppend,"_PRETTY",sep=""),
                                    dirOutputs = paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/compareMultiFacets/byYear",sep = ""))


                          metis.map(facetsOn=T,innerMargins=innerMargins, legendDigitsOverride=legendDigitsOverride,mapTitleOn=mapTitleOn, facetCols=facetCols,numeric2Cat_list=numeric2Cat_list, catParam=param_i, panelLabel=panelLabelMulti,
                                    underLayer=NULL, dataPolygon=mapx,
                                    fillColumn = names(mapx@data%>%dplyr::select(-subRegion,-multiFacetCol,-multiFacetRow)),
                                    mapTitle=paste(param_i,sep="") , legendShow = T,
                                    legendOutside = F,
                                    facetFreeScale = T,
                                    facetLabelSize = facetLabelSizeMultiAB*max(min(1,length(unique(mapx[[multiFacetRows]]))*length(unique(mapx[[multiFacetCols]]))/10),2),
                                    frameShow = frameShow, facetLabelBorderLwd=facetLabelBorderLwd, facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor ,
                          legendSingleColorOn=legendSingleColorOn,legendSingleValue=legendSingleValue,legendSingleColor=legendSingleColor,
                                    labels=labels,fillcolorNA=fillcolorNA,fillshowNA=fillshowNA,fillcolorNULL=fillcolorNULL,
                                    labelsSize = labelsSize,
                                    legendTitleSize = legendTitleSizeI,legendTextSize = legendTextSizeI,
                                    legendTitle =legendTitleMulti,
                                    legendStyle="kmeans",
                                    legendFixedBreaks=legendFixedBreaks,
                                    legendDigits = NULL,
                                    legendOutsidePosition = legendOutsidePosition,
                                    legendPosition = legendPosition,
                                    fillPalette = fillPalette,
                                    bgColor = "white",
                                    figWidth=figWidth*max(1,min(2,length(unique(mapx@data[[multiFacetCols]]))/2)),
                                    figHeight=figHeight*max(1,min(2,length(unique(mapx@data[[multiFacetRows]]))/2)),
                                    multiFacetRows="multiFacetRow",
                                    multiFacetCols="multiFacetCol",
                                    fileName = paste("map_",subRegType_ix,"_",param_i,"_",x_i,"_",class_i,nameAppend,"_FREESCALE",sep=""),
                                    dirOutputs = paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/compareMultiFacets/byYear",sep = ""))



                        }# if(nrow(datax)>1){
                      }# Close years loop

                    # Animate 1 : each param: If class > 1 { (Map x Class) x Anim Years}

                    if(animateOn==T){

                      animName<-paste("anim_",subRegType_ix,"_",param_i,"_",class_i,nameAppend,"_PRETTY.gif",sep="")
                      animFiles <- list.files(path = paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/compareMultiFacets/byYear",sep=""),
                                              pattern = paste(".*",param_i,".*",nameAppend,".*PRETTY", ".", pdfpng,sep=""), full.names=T,ignore.case = T, include.dirs = T);
                      animation <- magick::image_animate(magick::image_join(lapply(animFiles, magick::image_read)),fps=fps)
                      magick::image_write(animation,paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/compareMultiFacets/",
                                                  animName,sep = ""))
                      print(gsub("//","/",paste("animation saved in :",dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/compareMultiFacets/",
                                  animName,sep = "")))
                      # fnameTempImage=paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/compareMultiFacets/",
                      #                      animName,sep = "")
                      # tempImage<-magick::image_read(fnameTempImage)
                      # croppedImage<-magick::image_trim(tempImage,fuzz=0);
                      # magick::image_write(croppedImage,fnameTempImage)


                      animName<-paste("anim_",subRegType_ix,"_",param_i,"_",class_i,nameAppend,"_KMEANS.gif",sep="")
                      animFiles <- list.files(path = paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/compareMultiFacets/byYear",sep=""),
                                              pattern = paste(".*",param_i,".*",nameAppend,".*KMEANS", ".", pdfpng,sep=""), full.names=T,ignore.case = T, include.dirs = T);
                      animation <- magick::image_animate(magick::image_join(lapply(animFiles, magick::image_read)),fps=fps)
                      magick::image_write(animation,paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/compareMultiFacets/",
                                                  animName,sep = ""))
                      print(gsub("//","/",paste("animation saved in :",dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/compareMultiFacets/",
                                  animName,sep = "")))
                      # fnameTempImage=paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/compareMultiFacets/",
                      #                      animName,sep = "")
                      # tempImage<-magick::image_read(fnameTempImage)
                      # croppedImage<-magick::image_trim(tempImage,fuzz=0);
                      # magick::image_write(croppedImage,fnameTempImage)

                      animName<-paste("anim_",subRegType_ix,"_",param_i,"_",class_i,nameAppend,"_FREESCALE.gif",sep="")
                      animFiles <- list.files(path = paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/compareMultiFacets/byYear",sep=""),
                                              pattern = paste(".*",param_i,".*",nameAppend,".*FREESCALE", ".", pdfpng,sep=""), full.names=T,ignore.case = T, include.dirs = T);
                      animation <- magick::image_animate(magick::image_join(lapply(animFiles, magick::image_read)),fps=fps)
                      magick::image_write(animation,paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/compareMultiFacets/",
                                                  animName,sep = ""))
                      print(gsub("//","/",paste("animation saved in :",dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/compareMultiFacets/",
                                  animName,sep = "")))
                      # fnameTempImage=paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/compareMultiFacets/",
                      #                      animName,sep = "")
                      # tempImage<-magick::image_read(fnameTempImage)
                      # croppedImage<-magick::image_trim(tempImage,fuzz=0);
                      # magick::image_write(croppedImage,fnameTempImage)


                      #unlink(paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/byYear/animate_",param_i,sep = ""), recursive = TRUE) #-------------- cleaning up plots and temporary variables
                    } # If Animate ON==t


                    # Plot For Each Year Diff
                      # TODO Diff multi plots
#
#                     animScalePoly<-shapeTblMultxDiff$valueDiff

                      # # Choose correct scaleRange
                      # scaleRange_i=scaleRange
                      # if(grepl("DiffPrcnt",scenario_i)){
                      #   scaleRange_i=scaleRangeDiffPrcnt
                      #  shapeTblMultxDiff <- shapeTblMultxDiff %>% dplyr::mutate(units="Percent")

                      #   if(is.null(legendSingleColorOnOrig)){legendSingleColorOn=T}else{legendSingleColorOn=legendSingleColorOnOrig}
                      # }
                      #   if(grepl("DiffAbs",scenario_i)){
                      #     scaleRange_i=scaleRangeDiffAbs
                      #     if(is.null(legendSingleColorOnOrig)){legendSingleColorOn=T}else{legendSingleColorOn=legendSingleColorOnOrig}
                      #   }

                      # if(grepl("DiffxPrcnt",scenario_i)){
                      #   scaleRange_i=scaleRangeDiffxPrcnt
                      #  shapeTblMultxDiff <- shapeTblMultxDiff %>% dplyr::mutate(units="Percent")

                      #   if(is.null(legendSingleColorOnOrig)){legendSingleColorOn=T}else{legendSingleColorOn=legendSingleColorOnOrig}
                      # }
                      #   if(grepl("DiffxAbs",scenario_i)){
                      #     scaleRange_i=scaleRangeDiffxAbs
                      #     if(is.null(legendSingleColorOnOrig)){legendSingleColorOn=T}else{legendSingleColorOn=legendSingleColorOnOrig}
                      #   }

                      # if(is.null(legendSingleColorOnOrig)){
                      #   if(min(range(shapeTblMultxDiff$value))<0 & max(range(shapeTblMultxDiff$value))>0){
                      #     legendSingleColorOn=T}}else{legendSingleColorOn=legendSingleColorOnOrig}


#                     if(!is.null(scaleRange_i)){
#                        if(any(param_i %in% unique(scaleRange_i$param))){
#                       if(max(animScalePoly) < (scaleRange_i %>% dplyr::filter(param==param_i))$maxScale){
#                         animScalePoly<-c(animScalePoly,(scaleRange_i %>% dplyr::filter(param==param_i))$maxScale)} else {
#                           animScalePoly <-  c((scaleRange_i %>% dplyr::filter(param==param_i))$maxScale,
#                                               animScalePoly[animScalePoly<(scaleRange_i %>% dplyr::filter(param==param_i))$maxScale])
#                         }
#                       if(min(animScalePoly) > (scaleRange_i %>% dplyr::filter(param==param_i))$minScale){
#                         animScalePoly<-c(animScalePoly,(scaleRange_i %>% dplyr::filter(param==param_i))$minScale)} else {
#                           animScalePoly <-  c((scaleRange_i %>% dplyr::filter(param==param_i))$minScale,
#                                               animScalePoly[animScalePoly>(scaleRange_i %>% dplyr::filter(param==param_i))$minScale])
#                         }
#                     }}
#                     animPrettyBreaksPoly<-scales::pretty_breaks(n=legendFixedBreaks)(animScalePoly)
#                     animKmeanBreaksPoly<-sort(as.vector((stats::kmeans(animScalePoly,
#                                                                        centers=max(1,min(length(unique(animScalePoly))-1,(legendFixedBreaks-1)))))$centers[,1]))
#
#                     if(!min(animScalePoly) %in% animKmeanBreaksPoly){
#                       animKmeanBreaksPoly <- sort(c(min(animScalePoly),animKmeanBreaksPoly))}
#                     if(!max(animScalePoly) %in% animKmeanBreaksPoly){
#                       animKmeanBreaksPoly <- sort(c(animKmeanBreaksPoly,max(animScalePoly)))}
#
#
#                     if((max(range(animScalePoly))-min(range(animScalePoly)))<1E-10 &
#                        (max(range(animScalePoly))-min(range(animScalePoly)))>-1E-10){animScalePolyRange=min(animScalePoly)}else{
#                          animScalePolyRange=range(animScalePoly)
#                        }
#                     if(abs(min(animScalePolyRange,na.rm = T))==abs(max(animScalePolyRange,na.rm = T))){animScalePolyRange=abs(min(animScalePolyRange,na.rm = T))}
#                     if(mean(animScalePolyRange,na.rm = T)<0.01 & mean(animScalePolyRange,na.rm = T)>(-0.01)){animLegendDigits<-4}else{
#                       if(mean(animScalePolyRange,na.rm = T)<0.1 & mean(animScalePolyRange,na.rm = T)>(-0.1)){animLegendDigits<-3}else{
#                         if(mean(animScalePolyRange,na.rm = T)<1 & mean(animScalePolyRange,na.rm = T)>(-1)){animLegendDigits<-2}else{
#                           if(mean(animScalePolyRange,na.rm = T)<10 & mean(animScalePolyRange,na.rm = T)>(-10)){animLegendDigits<-1}else{animLegendDigits<-0}}}}
#
#
#                     for (x_i in unique(shapeTblMultxDiff$x)[unique(shapeTblMultxDiff$x) %in% xRangeMult]){
#
#                       datax<-shapeTblMultxDiff%>%dplyr::filter(x==x_i)%>%dplyr::mutate(value=valueDiff)%>%dplyr::select(-valueDiff)
#
#                       if(any(unique(datax$classPalette) %in% c("pal_wet","pal_hot","pal_green"))){
#                         datax <- datax %>% dplyr::mutate(classPalette = dplyr::case_when(classPalette=="pal_wet"~"pal_div_wet",
#                                                                                           classPalette=="pal_hot"~classPaletteDiff,
#                                                                                           classPalette=="pal_green"~"pal_div_BrGn",
#                                                                                           TRUE~classPalette))
#                       }
#
#                       if(nrow(datax)>1){
#                         legendTitle<-unique(datax$units)
#                          if(!is.null(classPaletteOrig)){fillPalette<-classPaletteOrig}else{
                      #fillPalette<-as.character(unique(datax$classPalette))}
#
#                         datax<-datax%>%dplyr::select(subRegion,class,value,multiFacetCol,multiFacetRow)%>%
#                           dplyr::distinct(subRegion,class,multiFacetCol,multiFacetRow,.keep_all = TRUE) %>%
#                           tidyr::spread(key=class,value=value)
#
#                         # Need to makeunique ID's when assigning multiple variable for faceted plotting
#                         mapx<-NULL
#                         ScenMultiABRefcomb<-datax%>%dplyr::select(multiFacetCol,multiFacetRow)%>%dplyr::ungroup()%>%dplyr::distinct();ScenMultiABRefcomb
#
#                         # # Add in any missing subRegions to datax
#                         # datax1<-expand.grid(unique(shape@data$subRegion)[!unique(shape@data$subRegion) %in% unique(datax$subRegion)],
#                         #                     unique(datax$multiFacetCol),unique(datax$multiFacetRow)) %>%
#                         #   dplyr::select(subRegion=Var1, multiFacetCol=Var2, multiFacetRow=Var3) %>%
#                         #   dplyr::mutate(!!names(datax)[4]:=0)
#                         #
#                         # datax <- datax %>% dplyr::bind_rows(datax1%>%dplyr::mutate(subRegion=as.character(subRegion))) %>% dplyr::mutate(subRegion=as.factor(subRegion))
#
#
#                         for (row_i in 1:nrow(ScenMultiABRefcomb)){
#                           MultiA_i <- ScenMultiABRefcomb[row_i,]$multiFacetCol
#                           MultiB_i <- ScenMultiABRefcomb[row_i,]$multiFacetRow
#                           a1<-shape
#                           a1@data<-a1@data%>%dplyr::left_join(datax%>%dplyr::filter(multiFacetCol==MultiA_i,multiFacetRow==MultiB_i),by="subRegion")%>%
#                             dplyr::select(names(datax))
#                           if(is.null(mapx)){mapx<-a1}else{mapx<-rbind(mapx,a1,makeUniqueIDs = TRUE)}
#                         }
#
#                         if(length(unique(animScalePoly))==1){legendStyleMulti="kmeans"}else{
#                           legendStyleMulti="fixed"}
#                         legendTitleMulti=paste(x_i,"\n",legendTitle,sep="")
#                         panelLabelMulti=NULL
#                         mapTitle=paste("Absolute Difference (Scenario less Reference Mean)\nRef MultiA: ",
#                                        refMultiA," Ref MultiB:",refMultiB,
#                                        "\nReference mean years: ",min(chosenRefMeanYearsX),"to",max(chosenRefMeanYearsX))
#
#                         metis.map(facetsOn=T,innerMargins=innerMargins, legendDigitsOverride=legendDigitsOverride,mapTitleOn=mapTitleOn, facetCols=facetCols,numeric2Cat_list=numeric2Cat_list, catParam=param_i, mapTitle = mapTitle,panelLabel=panelLabelMulti,
#                                   underLayer=NULL, dataPolygon=mapx,
#                                   fillColumn = names(mapx@data%>%dplyr::select(-subRegion,-multiFacetCol,-multiFacetRow)),
#                                   legendShow = T,
#                                   legendOutside = legendOutsideMulti,
#                                   facetFreeScale = F,
#                                   facetLabelSize = facetLabelSizeMultiAB*max(min(1,length(unique(mapx[[multiFacetRows]]))*length(unique(mapx[[multiFacetCols]]))/10),2),
#                                   frameShow = frameShow, facetLabelBorderLwd=facetLabelBorderLwd, facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor ,
#                           legendSingleColorOn=legendSingleColorOn,legendSingleValue=legendSingleValue,legendSingleColor=legendSingleColor,
#                                   labels=labels,fillcolorNA=fillcolorNA,fillshowNA=fillshowNA,fillcolorNULL=fillcolorNULL,
#                                   labelsSize = labelsSize,
#                                   legendTitleSize = legendTitleSizeMulti,legendTextSize = legendTextSizeMulti,
#                                   legendTitle =legendTitleMulti,
#                                   legendStyle=legendStyleMulti,
#                                   legendBreaks = animKmeanBreaksPoly,
#                                   legendFixedBreaks=legendFixedBreaks,
#                                   legendDigits = animLegendDigits,
#                                   legendOutsidePosition = legendOutsidePosition,
#                                   legendPosition = legendPositionMulti,
#                                   fillPalette = fillPalette,
#                                   bgColor = "white",
#                                   figWidth=figWidth*max(1,min(2,length(unique(mapx@data[[multiFacetCols]]))/2)),
#                                   figHeight=figHeight*max(1,min(2,length(unique(mapx@data[[multiFacetRows]]))/2)),
#                                   multiFacetRows="multiFacetRow",
#                                   multiFacetCols="multiFacetCol",
#                                   mapTitleSize=mapTitleSize,
#                                   fileName = paste("map_",subRegType_ix,"_",param_i,"_",x_i,"_",class_i,nameAppend,"_DIFF_KMEANS",sep=""),
#                                   dirOutputs = paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/compareMultiFacets/compareYear",sep = ""))
#
#
#
#                         metis.map(facetsOn=T,innerMargins=innerMargins, legendDigitsOverride=legendDigitsOverride,mapTitleOn=mapTitleOn, facetCols=facetCols,numeric2Cat_list=numeric2Cat_list, catParam=param_i, mapTitle = mapTitle,panelLabel=panelLabelMulti,
#                                   underLayer=NULL, dataPolygon=mapx,
#                                   fillColumn = names(mapx@data%>%dplyr::select(-subRegion,-multiFacetCol,-multiFacetRow)),
#                                   legendShow = T,
#                                   legendOutside = legendOutsideMulti,
#                                   facetFreeScale = F,
#                                   facetLabelSize = facetLabelSizeMultiAB*max(min(1,length(unique(mapx[[multiFacetRows]]))*length(unique(mapx[[multiFacetCols]]))/10),2),
#                                   frameShow = frameShow, facetLabelBorderLwd=facetLabelBorderLwd, facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor ,
#                           legendSingleColorOn=legendSingleColorOn,legendSingleValue=legendSingleValue,legendSingleColor=legendSingleColor,
#                                   labels=labels,fillcolorNA=fillcolorNA,fillshowNA=fillshowNA,fillcolorNULL=fillcolorNULL,
#                                   labelsSize = labelsSize,
#                                   legendTitleSize = legendTitleSizeMulti,legendTextSize = legendTextSizeMulti,
#                                   legendTitle =legendTitleMulti,
#                                   legendStyle=legendStyleMulti,
#                                   legendBreaks = animPrettyBreaksPoly,
#                                   legendFixedBreaks=legendFixedBreaks,
#                                   legendDigits = animLegendDigits,
#                                   legendOutsidePosition = legendOutsidePosition,
#                                   legendPosition = legendPositionMulti,
#                                   fillPalette = fillPalette,
#                                   bgColor = "white",
#                                   figWidth=figWidth*max(1,min(2,length(unique(mapx@data[[multiFacetCols]]))/2)),
#                                   figHeight=figHeight*max(1,min(2,length(unique(mapx@data[[multiFacetRows]]))/2)),
#                                   multiFacetRows="multiFacetRow",
#                                   multiFacetCols="multiFacetCol",
#                                   mapTitleSize=mapTitleSize,
#                                   fileName = paste("map_",subRegType_ix,"_",param_i,"_",x_i,"_",class_i,nameAppend,"_DIFF_PRETTY",sep=""),
#                                   dirOutputs = paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/compareMultiFacets/compareYear",sep = ""))
#
#
#                         metis.map(facetsOn=T,innerMargins=innerMargins, legendDigitsOverride=legendDigitsOverride,mapTitleOn=mapTitleOn, facetCols=facetCols,numeric2Cat_list=numeric2Cat_list, catParam=param_i, mapTitle = mapTitle,panelLabel=panelLabelMulti,
#                                   underLayer=NULL, dataPolygon=mapx,
#                                   fillColumn = names(mapx@data%>%dplyr::select(-subRegion,-multiFacetCol,-multiFacetRow)),
#                                   legendShow = T,
#                                   legendOutside = F,
#                                   facetFreeScale = T,
#                                   facetLabelSize = facetLabelSizeMultiAB*max(min(1,length(unique(mapx[[multiFacetRows]]))*length(unique(mapx[[multiFacetCols]]))/10),2),
#                                   frameShow = frameShow, facetLabelBorderLwd=facetLabelBorderLwd, facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor ,
#                           legendSingleColorOn=legendSingleColorOn,legendSingleValue=legendSingleValue,legendSingleColor=legendSingleColor,
#                                   labels=labels,fillcolorNA=fillcolorNA,fillshowNA=fillshowNA,fillcolorNULL=fillcolorNULL,
#                                   labelsSize = labelsSize,
#                                   legendTitleSize = legendTitleSizeI,legendTextSize = legendTextSizeI,
#                                   legendTitle =legendTitleMulti,
#                                   legendStyle="kmeans",
#                                   legendFixedBreaks=legendFixedBreaks,
#                                   legendDigits = NULL,
#                                   legendOutsidePosition = legendOutsidePosition,
#                                   legendPosition = legendPosition,
#                                   fillPalette = fillPalette,
#                                   bgColor = "white",
#                                   figWidth=figWidth*max(1,min(2,length(unique(mapx@data[[multiFacetCols]]))/2)),
#                                   figHeight=figHeight*max(1,min(2,length(unique(mapx@data[[multiFacetRows]]))/2)),
#                                   multiFacetRows="multiFacetRow",
#                                   multiFacetCols="multiFacetCol",
#                                   mapTitleSize=mapTitleSize,
#                                   fileName = paste("map_",subRegType_ix,"_",param_i,"_",x_i,"_",class_i,nameAppend,"_DIFF_FREESCALE",sep=""),
#                                   dirOutputs = paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/compareMultiFacets/compareYear",sep = ""))
#
#
#
#                       }# if(nrow(datax)>1){
#                     }# Close years loop
#
#
#                   # Animate 1 : each param: If class > 1 { (Map x Class) x Anim Years}
#
#                   if(animateOn==T){
#
#                     animName<-paste("anim_",subRegType_ix,"_",param_i,"_",class_i,nameAppend,"_DIFF_PRETTY.gif",sep="")
#                     animFiles <- list.files(path = paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/compareMultiFacets/compareYear",sep=""),
#                                             pattern = paste(".*",param_i,".*",nameAppend,".*",class_i,".*PRETTY", ".", pdfpng,sep=""), full.names=T,ignore.case = T, include.dirs = T);animFiles
#                     animation <- magick::image_animate(magick::image_join(lapply(animFiles, magick::image_read)),fps=fps)
#                     magick::image_write(animation,paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/compareMultiFacets/",
#                                                 animName,sep = ""))
#                     print(gsub("//","/",paste("animation saved in :",dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/compareMultiFacets/",
#                                 animName,sep = "")))
#                     fnameTempImage=paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/compareMultiFacets/",
#                                          animName,sep = "")
#                     tempImage<-magick::image_read(fnameTempImage)
#                     croppedImage<-magick::image_trim(tempImage,fuzz=0);
#                     magick::image_write(croppedImage,fnameTempImage)
#
#
#                     animName<-paste("anim_",subRegType_ix,"_",param_i,"_",class_i,nameAppend,"_DIFF_KMEANS.gif",sep="")
#                     animFiles <- list.files(path = paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/compareMultiFacets/compareYear",sep=""),
#                                             pattern = paste(".*",param_i,".*",nameAppend,".*",class_i,".*KMEANS", ".", pdfpng,sep=""), full.names=T,ignore.case = T, include.dirs = T);animFiles
#                     animation <- magick::image_animate(magick::image_join(lapply(animFiles, magick::image_read)),fps=fps)
#                     magick::image_write(animation,paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/compareMultiFacets/",
#                                                 animName,sep = ""))
#                     print(gsub("//","/",paste("animation saved in :",dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/compareMultiFacets/",
#                                 animName,sep = "")))
#                     fnameTempImage=paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/compareMultiFacets/",
#                                          animName,sep = "")
#                     tempImage<-magick::image_read(fnameTempImage)
#                     croppedImage<-magick::image_trim(tempImage,fuzz=0);
#                     magick::image_write(croppedImage,fnameTempImage)
#
#                     animName<-paste("anim_",subRegType_ix,"_",param_i,"_",class_i,nameAppend,"_DIFF_FREESCALE.gif",sep="")
#                     animFiles <- list.files(path = paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/compareMultiFacets/compareYear",sep=""),
#                                             pattern = paste(".*",param_i,".*",nameAppend,".*",class_i,".*FREESCALE", ".", pdfpng,sep=""), full.names=T,ignore.case = T, include.dirs = T);animFiles
#                     animation <- magick::image_animate(magick::image_join(lapply(animFiles, magick::image_read)),fps=fps)
#                     magick::image_write(animation,paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/compareMultiFacets/",
#                                                 animName,sep = ""))
#                     print(gsub("//","/",paste("animation saved in :",dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/compareMultiFacets/",
#                                 animName,sep = "")))
#                     fnameTempImage=paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/compareMultiFacets/",
#                                          animName,sep = "")
#                     tempImage<-magick::image_read(fnameTempImage)
#                     croppedImage<-magick::image_trim(tempImage,fuzz=0);
#                     magick::image_write(croppedImage,fnameTempImage)
#
#                     #unlink(paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/byYear/animate_",param_i,sep = ""), recursive = TRUE) #-------------- cleaning up plots and temporary variables
#                   } # If Animate ON==t
#



                    } # Close nrow>1
                    } # If no multiple years
                } # Check if MultiA and MultiBs > 1
                    } # Close nrow shapeTblMult >1
                  }# Close run section
                  }# nrow shapeTbl after Shapefiles loaded
                  } # Close if nrow shapeTblMult nrow >0


                  shapeTblReturn <- shapeTblReturn %>% dplyr::bind_rows(shapeTblMult)

                  }# Close class
              } # close Params
             # Close subRegType loop

       } # Check for multiple MultiA and MultiBS
      } # Close if(multifacetsOn==T){


      for (scenario_i in unique(shapeTblOrig$scenario)){
          for (param_i in unique(shapeTblOrig$param)){

            subRegShape <- NULL
            subRegType_ix<- NULL
            runSection = T

            if(nrow(shapeTblOrig%>%dplyr::filter(param==param_i,scenario==scenario_i))>0){

            shapeTbl <- shapeTblOrig%>%dplyr::filter(param==param_i,scenario==scenario_i)


            if(all(is.null(subRegShapeOrig) & is.null(subRegShpFileOrig))){


              mapFound <- metis.mapFind(shapeTbl)
              shapeTbl <- mapFound$dataTblFound
              subRegShape = mapFound$subRegShapeFound
              subRegType_ix = mapFound$subRegShapeTypeFound
              if(is.null(subRegType_ix)){subRegType_ix="subRegShapeType"}

              if(!is.null(shapeTbl)){

                shapeTbl <- shapeTbl %>% dplyr::mutate(subRegType=subRegType_ix)

              if(is.null(subRegShape)){
                print(paste("For the selected param: ", param_i," and scenario: ", scenario_i,sep=""))
                print("None of the pre-loaded shapefiles contain all the subRgions specified in a single shapefile.")
                print(paste("subRegions specified: ",paste(unique((shapeTbl%>%dplyr::filter(param==param_i,scenario==scenario_i))$subRegion),collapse=", "),sep=""))
                print(paste("Skipping map for param: ", param_i," and scenario: ", scenario_i,sep=""))
                print("Please load a subRegShape directly for your region of interest.")
                runSection = F
              }

            } else {
              runSection = F
              }# If is null shapeTbl
            } else {
              if(nrow(shapeTbl%>%dplyr::filter(param==param_i,scenario==scenario_i))>0){
                subRegType_ix <- unique((shapeTbl%>%dplyr::filter(param==param_i,scenario==scenario_i))$subRegType)
              }else{subRegType_ix="subRegion"}
              subRegShape <- subRegShapeOrig
            }

            if(runSection){

              #--------------------------------
              # Checking Shapefiles
              #--------------------------------
              if(T){ # Checking Shapefiles

                if(is.null(boundaryRegShapeOrig)){
                  if(!is.null(boundaryRegShpFolderOrig) & !is.null(boundaryRegShpFileOrig)){
                    if(!dir.exists(boundaryRegShpFolderOrig)){
                      stop("Shapefile folder: ", boundaryRegShpFolderOrig ," is incorrect or doesn't exist.",sep="")}
                    if(!file.exists(paste(boundaryRegShpFolderOrig,"/",boundaryRegShpFileOrig,".shp",sep=""))){
                      stop("Shape file: ", paste(boundaryRegShpFolderOrig,"/",boundaryRegShpFileOrig,".shp",sep="")," is incorrect or doesn't exist.",sep="")}
                    boundaryRegShape=rgdal::readOGR(dsn=boundaryRegShpFolderOrig,layer=boundaryRegShpFileOrig,use_iconv=T,encoding='UTF-8')
                    print(paste("Sub Reg Shape : ",boundaryRegShpFolderOrig,"/",boundaryRegShpFileOrig,".shp",sep=""))
                    print(raster::head(boundaryRegShape))
                  }  else {
                  # If only boundary regionsSelect have been chosen then try and find a shapefile with those regions
                  if(!is.null(boundaryRegionsSelect)){
                    mapFound <- metis.mapFind(data.frame(subRegion=boundaryRegionsSelect))
                    boundaryRegShape <- mapFound$subRegShapeFound
                    boundaryRegionsSelect <- unique(mapFound$dataTblFound$subRegion)
                    print(paste("Boundary region shape automatically set to: ",
                                mapFound$subRegShapeTypeFound, sep=""))
                  }else{
                    mapFound <- metis.mapFind(data.frame(subRegion=unique(shapeTbl$subRegion)))
                    boundaryRegShape <- mapFound$subRegShapeFound
                  }
                }
                }


                if(is.null(subRegShape)){
                  if(!is.null(subRegShpFolderOrig) & !is.null(subRegShpFileOrig)){
                    if(!dir.exists(subRegShpFolderOrig)){
                      stop("Shapefile folder: ", subRegShpFolderOrig ," is incorrect or doesn't exist.",sep="")}
                    if(!file.exists(paste(subRegShpFolderOrig,"/",subRegShpFileOrig,".shp",sep=""))){
                      stop("Shape file: ", paste(subRegShpFolderOrig,"/",subRegShpFileOrig,".shp",sep="")," is incorrect or doesn't exist.",sep="")}
                    subRegShape=rgdal::readOGR(dsn=subRegShpFolderOrig,layer=subRegShpFileOrig,use_iconv=T,encoding='UTF-8')
                    print(paste("Sub Reg Shape : ",subRegShpFolderOrig,"/",subRegShpFileOrig,".shp",sep=""))
                    print(raster::head(subRegShape))
                  } # if(!is.null(subRegShpFolder) & !is.null(subRegShpFile)){
                }


                if(is.null(subRegShape)){
                  stop("No valid subregional shape file available")}

                if(!subRegCol %in% names(subRegShape)){stop(paste("SubRegCol: ",subRegCol," not present in subRegShape",sep=""))}

                if(!is.null(subRegShape)){
                  subRegShape@data[[subRegCol]] <- as.character(subRegShape@data[[subRegCol]])
                  if(!all(unique((shapeTbl%>%dplyr::filter(param==param_i,scenario==scenario_i))$subRegion) %in% unique(subRegShape@data[[subRegCol]]))){
                    print(paste("Removing subRegions not present in shapefile from datatable: ",
                                paste(unique((shapeTbl%>%dplyr::filter(param==param_i,scenario==scenario_i))$subRegion)[!unique((shapeTbl%>%dplyr::filter(param==param_i,scenario==scenario_i))$subRegion) %in% unique(subRegShape@data[[subRegCol]])],collapse=", "),sep=""))
                    shapeTbl <- shapeTbl%>%dplyr::filter(param==param_i,scenario==scenario_i)%>% dplyr::filter(subRegion %in% unique(subRegShape@data[[subRegCol]]))
                    print(paste("Remaining subRegions in dataTable are: ",paste(unique(shapeTbl$subRegion),collapse=", "),sep=""))}
                }

                if(!any(unique((shapeTbl%>%dplyr::filter(param==param_i,scenario==scenario_i))$subRegion) %in% unique(subRegShape@data[[subRegCol]]))){
                  print(paste("None of the subRegions in data provided for param: ", param_i,
                              " match subRegions in the shapefile provided or available.",sep=""))
                }

                subRegShape@data<-subRegShape@data%>%dplyr::mutate(subRegion=get(subRegCol))



                #----------------
                # Create Boundary and subRegional shapefiles
                #---------------

                if(!is.null(boundaryRegShape) & !is.null(subRegShape)){
                  if(!is.null(boundaryRegionsSelect)){
                    if(any(boundaryRegionsSelect %in% unique(boundaryRegShape@data[[boundaryRegCol]]))){
                      boundaryRegShapeLimits <- boundaryRegShape[boundaryRegShape@data[[boundaryRegCol]] %in% boundaryRegionsSelect,]
                      boundaryRegShapeLimits@data <- droplevels(boundaryRegShapeLimits@data)
                      bbox1<-as.data.frame(sp::bbox(boundaryRegShapeLimits))
                      bbox1$min;bbox1$max
                      rangeX<-abs(range(bbox1$min[1],bbox1$max[1])[2]-range(bbox1$min[1],bbox1$max[1])[1])
                      rangeY<-abs(range(bbox1$min[2],bbox1$max[2])[2]-range(bbox1$min[2],bbox1$max[2])[1])
                      bbox1$min[1]<-min(180,max(-180,(-rangeX*expandPercent/100)+bbox1$min[1]));
                      bbox1$min[2]<-min(90,max(-90,(-rangeY*expandPercent/100)+bbox1$min[2]));
                      bbox1$max[1]<-max(-180,min(180,(rangeX*expandPercent/100)+bbox1$max[1]));
                      bbox1$max[2]<-max(-90,min(90,(rangeY*expandPercent/100)+bbox1$max[2]));
                      bbox1$min;bbox1$max;
                      bbox1<-methods::as(raster::extent(as.vector(t(bbox1))), "SpatialPolygons")
                      sp::proj4string(bbox1)<-sp::CRS(projX) # ASSIGN COORDINATE SYSTEM
                      boundaryRegShape <- sp::spTransform(boundaryRegShape,sp::CRS(projX))
                      boundaryRegShape<-raster::crop(boundaryRegShape, bbox1)
                      boundaryRegShape@bbox <- bbox1@bbox
                      boundaryRegShape@data <- droplevels(boundaryRegShape@data)
                      print("Map cropped to regions with data. To plot full map extent set crop2Boundary = F.")
                      subRegShape <- sp::spTransform(subRegShape,raster::crs(boundaryRegShape))
                      subRegShape <- raster::crop(subRegShape,boundaryRegShape)
                      print("Scale will still include all data from original subRegShape extents")
                      expandPercent_i = 0 # Preventing extension for doubling expansion
                    } else {
                      print(paste("boundaryRegionsSelect chosen are not available in the boundaryRegShapeFile.",paste(boundaryRegionsSelect,collapse=", "),sep=""))}
                  }else{
                    boundaryRegShape <- sp::spTransform(boundaryRegShape,raster::crs(subRegShape))
                    subRegShape <- raster::crop(subRegShape,boundaryRegShape)
                    subRegShape@data <- droplevels(subRegShape@data)
                  }
                }

                shape<-subRegShape

                if(!subRegCol %in% names(shape)){stop(paste("SubRegCol: ",subRegCol," not present in shape",sep=""))}

                shape@data<-shape@data%>%dplyr::mutate(subRegion=get(subRegCol), subRegion=as.character(subRegion))

                #----------------
                # Create Boundary Extension
                #---------------

                bgColorChosen="white"

                if(extension==T){

                  frameShow=T;facetLabelBorderLwd=0.1;facetBGColor="grey30";facetLabelColor = "white"

                  if(is.null(extendedShape)){

                    extendedBoundary <- metis::mapCountries
                    bbox1<-as.data.frame(sp::bbox(shape))
                    extendedShapeCol<-subRegCol

                    if(!is.null(bbox1)){
                      bbox1$min;bbox1$max
                      rangeX<-abs(range(bbox1$min[1],bbox1$max[1])[2]-range(bbox1$min[1],bbox1$max[1])[1])
                      rangeY<-abs(range(bbox1$min[2],bbox1$max[2])[2]-range(bbox1$min[2],bbox1$max[2])[1])
                      bbox1$min[1]<-min(180,max(-180,(-rangeX*expandPercent_i/100)+bbox1$min[1]));
                      bbox1$min[2]<-min(90,max(-90,(-rangeY*expandPercent_i/100)+bbox1$min[2]));
                      bbox1$max[1]<-max(-180,min(180,(rangeX*expandPercent_i/100)+bbox1$max[1]));
                      bbox1$max[2]<-max(-90,min(90,(rangeY*expandPercent_i/100)+bbox1$max[2]));
                      bbox1$min;bbox1$max;
                      bbox1<-methods::as(raster::extent(as.vector(t(bbox1))), "SpatialPolygons")
                      sp::proj4string(bbox1)<-sp::CRS(projX) # ASSIGN COORDINATE SYSTEM
                      boundaryRegShape <- sp::spTransform(boundaryRegShape,sp::CRS(projX))
                      print("Creating extended boundary...")
                      extendedShape<-raster::crop(extendedBoundary, bbox1)
                      extendedShape@bbox <- bbox1@bbox
                      if(!is.null(boundaryRegShape)){extendedShape<-raster::crop(extendedShape, boundaryRegShape)}
                      extendedBGColor="lightblue1"
                    }else{print("No extended boundary.")}
                  }

                  if(!is.null(extendedShape)){
                    if(extendedShapeCol %in% names(extendedShape)){
                      underLayer<-metis.map(facetsOn=F,innerMargins=innerMargins, legendDigitsOverride=legendDigitsOverride,facetLabelSize=facetLabelSize,mapTitleOn=mapTitleOn, facetCols=facetCols,fillcolorNA=fillcolorNA,fillshowNA=fillshowNA,fillcolorNULL=fillcolorNULL, dataPolygon=extendedShape, printFig=F,labelsAutoPlace = F,
                                            fillColumn = extendedShapeCol,labels=extendedLabels, fillPalette = extendedFillColor,legendShow=F,
                                            bgColor = extendedBGColor, frameShow=T,facetLabelBorderLwd=facetLabelBorderLwd,labelsSize=extdendedLabelSize, labelsColor=extendedLabelsColor,
                                            figWidth=figWidth,figHeight=figHeight, pdfpng = pdfpng)
                      bgColorChosen= extendedBGColor
                    }
                  }
                }
              }


            if(nrow(shapeTbl)>0){ # nrow shapeTbl after Shapefiles loaded
              #------------------
              # Create Polygon Folders If Needed
              #------------------
              if(TRUE){
                if (!dir.exists(paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,sep = ""))){
                  dir.create(paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,sep = ""))}
                if (!dir.exists(paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,sep = ""))){
                  dir.create(paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,sep = ""))}
                if (!dir.exists(paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/", scenario_i,sep = ""))){
                  dir.create(paste(dirOutputsX, "/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/",  scenario_i,sep = ""))}
                if (!dir.exists(paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/", scenario_i,"/byYear",sep = ""))){
                  dir.create(paste(dirOutputsX, "/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/", scenario_i,"/byYear",sep = ""))}
              }


              #-------------------
              # Save Map related Data Table
              #-------------------

              if(nrow(shapeTbl%>%dplyr::filter(scenario==scenario_i,param==param_i))>0){
                data.table::fwrite(shapeTbl%>%dplyr::filter(scenario==scenario_i,param==param_i)%>%
                                     dplyr::select(scenario,subRegion,param,class,x, units,value),
                                   paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/", scenario_i,
                                         "/","map_",subRegType_ix,"_",param_i,"_",scenario_i,nameAppend,".csv",sep = ""))
                print(paste("Map data table written to ",dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/", scenario_i,
                            "/","map_",subRegType_ix,"_",param_i,"_",scenario_i,nameAppend,".csv",sep = ""))
              }



              animScalePoly<-(shapeTbl%>%dplyr::filter(scenario==scenario_i,param==param_i,
                                                       !is.na(value),!is.infinite(value), !is.nan(value)))$value

              # Choose correct scaleRange
              scaleRange_i=scaleRange
              if(grepl("DiffPrcnt",scenario_i)){
                scaleRange_i=scaleRangeDiffPrcnt
                shapeTbl <- shapeTbl %>% dplyr::mutate(units="Percent")
                if(is.null(legendSingleColorOnOrig)){legendSingleColorOn=T}else{legendSingleColorOn=legendSingleColorOnOrig}
              }
              if(grepl("DiffAbs",scenario_i)){
                scaleRange_i=scaleRangeDiffAbs
                if(is.null(legendSingleColorOnOrig)){legendSingleColorOn=T}else{legendSingleColorOn=legendSingleColorOnOrig}
              }
             if(grepl("DiffxPrcnt",scenario_i)){
                scaleRange_i=scaleRangeDiffxPrcnt
                shapeTbl <- shapeTbl %>% dplyr::mutate(units="Percent")
                if(is.null(legendSingleColorOnOrig)){legendSingleColorOn=T}else{legendSingleColorOn=legendSingleColorOnOrig}
              }
              if(grepl("DiffxAbs",scenario_i)){
                scaleRange_i=scaleRangeDiffxAbs
                if(is.null(legendSingleColorOnOrig)){legendSingleColorOn=T}else{legendSingleColorOn=legendSingleColorOnOrig}
              }


              if(is.null(legendSingleColorOnOrig)){
                if(min(range(shapeTbl$value))<0 & max(range(shapeTbl$value))>0){
                  legendSingleColorOn=T}}else{legendSingleColorOn=legendSingleColorOnOrig}


              if(!is.null(scaleRange_i)){
                if(any(param_i %in% unique(scaleRange_i$param))){
                if(max(animScalePoly) < (scaleRange_i %>% dplyr::filter(param==param_i))$maxScale){
                  animScalePoly<-c(animScalePoly,(scaleRange_i %>% dplyr::filter(param==param_i))$maxScale)} else {
                    animScalePoly <-  c((scaleRange_i %>% dplyr::filter(param==param_i))$maxScale,
                                        animScalePoly[animScalePoly<(scaleRange_i %>% dplyr::filter(param==param_i))$maxScale])
                  }
                if(min(animScalePoly) > (scaleRange_i %>% dplyr::filter(param==param_i))$minScale){
                  animScalePoly<-c(animScalePoly,(scaleRange_i %>% dplyr::filter(param==param_i))$minScale)} else {
                    animScalePoly <-  c((scaleRange_i %>% dplyr::filter(param==param_i))$minScale,
                                        animScalePoly[animScalePoly>(scaleRange_i %>% dplyr::filter(param==param_i))$minScale])
                  }
              }}
              animPrettyBreaksPoly<-scales::pretty_breaks(n=legendFixedBreaks)(animScalePoly)
              animKmeanBreaksPoly<-sort(as.vector((stats::kmeans(animScalePoly,
                                                                 centers=max(1,min(length(unique(animScalePoly))-1,(legendFixedBreaks-1)))))$centers[,1]))
              if(!min(animScalePoly) %in% animKmeanBreaksPoly){
                animKmeanBreaksPoly <- sort(c(min(animScalePoly),animKmeanBreaksPoly))}
              if(!max(animScalePoly) %in% animKmeanBreaksPoly){
                animKmeanBreaksPoly <- sort(c(animKmeanBreaksPoly,max(animScalePoly)))}

              if((max(range(animScalePoly))-min(range(animScalePoly)))<1E-10 &
                 (max(range(animScalePoly))-min(range(animScalePoly)))>-1E-10){animScalePolyRange=min(animScalePoly)}else{
                   animScalePolyRange=range(animScalePoly)
                 }

              if(abs(min(animScalePolyRange,na.rm = T))==abs(max(animScalePolyRange,na.rm = T))){animScalePolyRange=abs(min(animScalePolyRange,na.rm = T))}
              if(mean(animScalePolyRange,na.rm = T)<0.01 & mean(animScalePolyRange,na.rm = T)>(-0.01)){animLegendDigits<-4}else{
                if(mean(animScalePolyRange,na.rm = T)<0.1 & mean(animScalePolyRange,na.rm = T)>(-0.1)){animLegendDigits<-3}else{
                  if(mean(animScalePolyRange,na.rm = T)<1 & mean(animScalePolyRange,na.rm = T)>(-1)){animLegendDigits<-2}else{
                    if(mean(animScalePolyRange,na.rm = T)<10 & mean(animScalePolyRange,na.rm = T)>(-10)){animLegendDigits<-1}else{animLegendDigits<-0}}}}


              # Figure 1 : each param: If class > 1 { (Map x Class) x Selected Years}

              shapeTblx<-shapeTbl%>%dplyr::filter(scenario==scenario_i,
                                                  param==param_i)


              for (x_i in unique(shapeTbl$x)){

                datax<-shapeTblx%>%dplyr::filter(x==x_i)

                if(nrow(datax)>0){
                  legendTitle<-unique(datax$units)
                  if(!is.null(classPaletteOrig)){fillPalette<-classPaletteOrig}else{
                    fillPalette<-as.character(unique(datax$classPalette))}

                  datax<-datax%>%dplyr::select(subRegion,class,value)%>%
                   tidyr::spread(key=class,value=value)

#                   # Add in any missing subRegions to datax
#                   datax1<-expand.grid(unique(shape@data$subRegion)[!unique(shape@data$subRegion) %in% unique(datax$subRegion)]) %>%
#                     dplyr::select(subRegion=Var1)%>%
#                     dplyr::mutate(!!names(datax)[2]:=0)
#
#                   datax <- datax %>% dplyr::bind_rows(datax1%>%dplyr::mutate(subRegion=as.character(subRegion))) %>% dplyr::mutate(subRegion=as.factor(subRegion))
#                   datax[is.na(datax)]<-0


                  mapx<-shape
                  mapx@data<-mapx@data%>%dplyr::left_join(datax)%>%
                    dplyr::select(names(datax))


                  if("subRegion" %in% names(mapx@data)){countCheck=2}else{countCheck=1}
                  if(length(names(mapx@data))==countCheck){
                    legendOutsideAnimated=legendOutsideSingle
                    legendTitleAnimated=legendTitle
                    panelLabelAnimated=paste(x_i)
                    legendAnimatedPosition=legendPositionS
                    legendTitleSizeAnim = legendTitleSizeS
                    legendTextSizeAnim = legendTextSizeS
                    legendBreaksAnim = animKmeanBreaksPoly
                    legendStyleAnim="fixed"}else{
                      legendStyleAnim="fixed"
                      legendBreaksAnim = animKmeanBreaksPoly
                      legendOutsideAnimated=T
                      legendTitleAnimated=paste(x_i,"\n",legendTitle,sep="")
                      panelLabelAnimated=NULL
                      legendAnimatedPosition=legendPositionS
                      legendTitleSizeAnim = legendTitleSizeS
                      legendTextSizeAnim = legendTextSizeS
                    }

                  metis.map(facetsOn=T,innerMargins=innerMargins, legendDigitsOverride=legendDigitsOverride,facetLabelSize=facetLabelSize,mapTitleOn=mapTitleOn, facetCols=facetCols,numeric2Cat_list=numeric2Cat_list, catParam=param_i, panelLabel=panelLabelAnimated,
                            underLayer=underLayer,  dataPolygon=mapx,
                            fillColumn = names(mapx@data%>%dplyr::select(-subRegion)),
                            mapTitle=paste(param_i," ",scenario_i,sep="") , legendShow = T,
                            legendOutside = legendOutsideAnimated,
                            facetFreeScale = F,
                            frameShow = frameShow, facetLabelBorderLwd=facetLabelBorderLwd, facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor ,
                          legendSingleColorOn=legendSingleColorOn,legendSingleValue=legendSingleValue,legendSingleColor=legendSingleColor,
                            labels=labels,fillcolorNA=fillcolorNA,fillshowNA=fillshowNA,fillcolorNULL=fillcolorNULL,
                            labelsSize = labelsSize,
                            legendTitleSize = legendTitleSizeAnim,legendTextSize = legendTextSizeAnim,
                            legendTitle =legendTitleAnimated,
                            legendStyle=legendStyleAnim,
                            legendBreaks = animKmeanBreaksPoly,
                            legendFixedBreaks=legendFixedBreaks,
                            legendDigits = animLegendDigits,
                            legendOutsidePosition = legendOutsidePosition,
                            legendPosition = legendAnimatedPosition,
                            fillPalette = fillPalette,
                            bgColor = bgColorChosen,
                            figWidth=figWidth,
                            figHeight=figHeight, pdfpng = pdfpng,
                            fileName = paste("map_",subRegType_ix,"_",param_i,"_",x_i,"_",scenario_i,nameAppend,"_KMEANS",sep=""),
                            dirOutputs = paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/", scenario_i,"/byYear",sep = ""))

                  # legendSingleColorOn=legendSingleColorOn
                  # legendSingleValue=legendSingleValue
                  # legendSingleColor=legendSingleColor
                  # numeric2Cat_list=numeric2Cat_list
                  # catParam=param_i
                  # panelLabel=panelLabelAnimated
                  # underLayer=underLayer
                  # dataPolygon=mapx
                  # fillColumn = names(mapx@data%>%dplyr::select(-subRegion))
                  # mapTitle=paste(param_i," ",scenario_i,sep="")
                  # legendShow = T
                  # legendOutside = legendOutsideAnimated
                  # facetFreeScale = F
                  # frameShow = frameShow
                  # labels=labels
                  # labelsSize = labelsSize
                  # legendTitleSize = legendTitleSizeAnim
                  # legendTextSize = legendTextSizeAnim
                  # legendTitle =legendTitleAnimated
                  # legendStyle=legendStyleAnim
                  # legendBreaks = animKmeanBreaksPoly
                  # legendFixedBreaks=legendFixedBreaks
                  # legendDigits = animLegendDigits
                  # legendOutsidePosition = legendOutsidePosition
                  # legendPosition = legendAnimatedPosition
                  # fillPalette = fillPalette
                  # bgColor = bgColorChosen
                  # figWidth=figWidth
                  # figHeight=figHeight
                  # fileName = paste("map_",subRegType_ix,"_",param_i,"_",x_i,"_",scenario_i,nameAppend,"_KMEANS",sep="")
                  # dirOutputs = paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/", scenario_i,"/byYear",sep = "")

                  metis.map(facetsOn=T,innerMargins=innerMargins, legendDigitsOverride=legendDigitsOverride,facetLabelSize=facetLabelSize,mapTitleOn=mapTitleOn, facetCols=facetCols,numeric2Cat_list=numeric2Cat_list, catParam=param_i, panelLabel=panelLabelAnimated,
                            underLayer=underLayer,  dataPolygon=mapx,
                            fillColumn = names(mapx@data%>%dplyr::select(-subRegion)),
                            mapTitle=paste(param_i," ",scenario_i,sep="") , legendShow = T,
                            legendOutside = legendOutsideAnimated,
                            facetFreeScale = F,
                            frameShow = frameShow, facetLabelBorderLwd=facetLabelBorderLwd, facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor ,
                          legendSingleColorOn=legendSingleColorOn,legendSingleValue=legendSingleValue,legendSingleColor=legendSingleColor,
                            labels=labels,fillcolorNA=fillcolorNA,fillshowNA=fillshowNA,fillcolorNULL=fillcolorNULL,
                            labelsSize = labelsSize,
                            legendTitle =legendTitleAnimated,
                            legendTitleSize = legendTitleSizeAnim,legendTextSize = legendTextSizeAnim,
                            legendStyle="fixed",
                            legendBreaks = animPrettyBreaksPoly,
                            legendFixedBreaks=legendFixedBreaks,
                            legendDigits = animLegendDigits,
                            legendOutsidePosition = legendOutsidePosition,
                            legendPosition = legendAnimatedPosition,
                            fillPalette = fillPalette,
                            bgColor = bgColorChosen,
                            figWidth=figWidth,
                            figHeight=figHeight, pdfpng = pdfpng,
                            fileName = paste("map_",subRegType_ix,"_",param_i,"_",x_i,"_",scenario_i,nameAppend,"_PRETTY",sep=""),
                            dirOutputs = paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/", scenario_i,"/byYear",sep = ""))
#
                  # numeric2Cat_list=numeric2Cat_list
                  # catParam=param_i
                  # panelLabel=panelLabelAnimated
                  # underLayer=underLayer
                  # dataPolygon=mapx
                  # fillColumn = names(mapx@data%>%dplyr::select(-subRegion))
                  # mapTitle=paste(param_i," ",scenario_i,sep="") , legendShow = T
                  # legendOutside = legendOutsideAnimated
                  # facetFreeScale = F
                  # frameShow = frameShow
                  # labels=labels
                  # labelsSize = labelsSize
                  # legendTitle =legendTitleAnimated
                  # legendTitleSize = legendTitleSizeAnim
                  # legendTextSize = legendTextSizeAnim
                  # legendStyle="fixed"
                  # legendBreaks = animPrettyBreaksPoly
                  # legendFixedBreaks=legendFixedBreaks
                  # legendDigits = animLegendDigits
                  # legendOutsidePosition = legendOutsidePosition
                  # legendPosition = legendAnimatedPosition
                  # fillPalette = fillPalette
                  # bgColor = bgColorChosen
                  # figWidth=figWidth
                  # figHeight=figHeight
                  # pdfpng = pdfpng
                  # fileName = paste("map_",subRegType_ix,"_",param_i,"_",x_i,"_",scenario_i,nameAppend,"_PRETTY",sep="")
                  # dirOutputs = paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/", scenario_i,"/byYear",sep = "")

                  if("subRegion" %in% names(mapx@data)){countCheck=2}else{countCheck=1}
                  if(length(names(mapx@data))==countCheck){
                    legendOutsideAnimated=legendOutsideSingle
                    legendAnimatedPosition
                    legendTitleSizeAnim = legendTitleSizeS
                    legendTextSizeAnim = legendTextSizeS}else{
                       legendOutsideAnimated=F
                      legendAnimatedPosition=legendPosition
                      legendTitleSizeAnim = legendTitleSizeI
                      legendTextSizeAnim = legendTextSizeI
                    }

                  metis.map(facetsOn=T,innerMargins=innerMargins, legendDigitsOverride=legendDigitsOverride,facetLabelSize=facetLabelSize,mapTitleOn=mapTitleOn, facetCols=facetCols,numeric2Cat_list=numeric2Cat_list, catParam=param_i, panelLabel= panelLabelAnimated,underLayer=underLayer,dataPolygon=mapx,
                            fillColumn = names(mapx@data%>%dplyr::select(-subRegion)),
                            mapTitle=paste(param_i," ",scenario_i,sep="") , legendShow = T,
                            legendOutside = legendOutsideAnimated,
                            facetFreeScale = T,
                            frameShow = frameShow, facetLabelBorderLwd=facetLabelBorderLwd, facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor ,
                          legendSingleColorOn=legendSingleColorOn,legendSingleValue=legendSingleValue,legendSingleColor=legendSingleColor,
                            labels=labels,fillcolorNA=fillcolorNA,fillshowNA=fillshowNA,fillcolorNULL=fillcolorNULL,
                            labelsSize = labelsSize,
                            legendTitle =legendTitleAnimated,
                            legendTitleSize = legendTitleSizeAnim,legendTextSize = legendTextSizeAnim,
                            legendStyle="kmeans",
                            legendFixedBreaks=legendFixedBreaks,
                            legendDigits = NULL,
                            legendOutsidePosition = legendOutsidePosition,
                            legendPosition = legendAnimatedPosition,
                            fillPalette = fillPalette,
                            bgColor = bgColorChosen,
                            figWidth=figWidth,
                            figHeight=figHeight, pdfpng = pdfpng,
                            fileName = paste("map_",subRegType_ix,"_",param_i,"_",x_i,"_",scenario_i,nameAppend,"_FREESCALE",sep=""),
                            dirOutputs = paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/", scenario_i,"/byYear",sep = ""))

                  # innerMargins=innerMargins
                  # legendDigitsOverride=legendDigitsOverride
                  # facetLabelSize=facetLabelSize
                  # mapTitleOn=mapTitleOn
                  # facetCols=facetCols
                  # numeric2Cat_list=numeric2Cat_list
                  # catParam=param_i
                  # panelLabel= panelLabelAnimated
                  # underLayer=underLayer
                  # dataPolygon=mapx
                  # fillColumn = names(mapx@data%>%dplyr::select(-subRegion))
                  # mapTitle=paste(param_i," ",scenario_i,sep="")
                  # legendShow = T
                  # legendOutside = legendOutsideAnimated
                  # facetFreeScale = T
                  # frameShow = frameShow
                  # legendSingleColorOn=legendSingleColorOn
                  # legendSingleValue=legendSingleValue
                  # legendSingleColor=legendSingleColor
                  # labels=labels
                  # fillcolorNA=fillcolorNA
                  # fillshowNA=fillshowNA
                  # fillcolorNULL=fillcolorNULL
                  # labelsSize = labelsSize
                  # legendTitle =legendTitleAnimated
                  # legendTitleSize = legendTitleSizeAnim
                  # legendTextSize = legendTextSizeAnim
                  # legendStyle="kmeans"
                  # legendFixedBreaks=legendFixedBreaks
                  # legendDigits = NULL
                  # legendOutsidePosition = legendOutsidePosition
                  # legendPosition = legendAnimatedPosition
                  # fillPalette = fillPalette
                  # bgColor = bgColorChosen
                  # figWidth=figWidth
                  # figHeight=figHeight
                  # pdfpng = pdfpng
                  # fileName = paste("map_",subRegType_ix,"_",param_i,"_",x_i,"_",scenario_i,nameAppend,"_FREESCALE",sep="")
                  # dirOutputs = paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/", scenario_i,"/byYear",sep = "")


                }# if(nrow(datax)>1){
              }# Close years loop

              # Animate 1 : each param: If class > 1 { (Map x Class) x Anim Years}

              if(animateOn==T){

                animName<-paste("anim_",subRegType_ix,"_",param_i,"_",scenario_i,nameAppend,"_PRETTY.gif",sep="")
                animFiles <- list.files(path = paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/", scenario_i,"/byYear",sep=""),
                                        pattern = paste(".*",param_i,".*",nameAppend,".*PRETTY", ".", pdfpng,sep=""), full.names=T,ignore.case = T, include.dirs = T);animFiles
                print(animFiles)
                animation <- magick::image_animate(magick::image_join(lapply(animFiles, magick::image_read)),fps=fps)
                magick::image_write(animation,paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/", scenario_i,"/",
                                            animName,sep = ""))
                print(gsub("//","/",paste("animation saved in :",dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/", scenario_i,"/",
                            animName,sep = "")))
                # fnameTempImage=paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/", scenario_i,"/",
                #                      animName,sep = "")
                # tempImage<-magick::image_read(fnameTempImage)
                # croppedImage<-magick::image_trim(tempImage,fuzz=0);
                # magick::image_write(croppedImage,fnameTempImage)



                animName<-paste("anim_",subRegType_ix,"_",param_i,"_",scenario_i,nameAppend,"_KMEANS.gif",sep="")
                animFiles <- list.files(path = paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/", scenario_i,"/byYear",sep=""),
                                        pattern = paste(".*",param_i,".*",nameAppend,".*KMEANS", ".", pdfpng,sep=""), full.names=T,ignore.case = T, include.dirs = T);animFiles
                animation <- magick::image_animate(magick::image_join(lapply(animFiles, magick::image_read)),fps=fps)
                magick::image_write(animation,paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/", scenario_i,"/",
                                            animName,sep = ""))
                print(gsub("//","/",paste("animation saved in :",dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/", scenario_i,"/",
                            animName,sep = "")))
                # fnameTempImage=paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/", scenario_i,"/",
                #                      animName,sep = "")
                # tempImage<-magick::image_read(fnameTempImage)
                # croppedImage<-magick::image_trim(tempImage,fuzz=0);
                # magick::image_write(croppedImage,fnameTempImage)


                animName<-paste("anim_",subRegType_ix,"_",param_i,"_",scenario_i,nameAppend,"_FREESCALE.gif",sep="")
                animFiles <- list.files(path = paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/", scenario_i,"/byYear",sep=""),
                                        pattern = paste(".*",param_i,".*",nameAppend,".*FREESCALE", ".", pdfpng,sep=""), full.names=T,ignore.case = T, include.dirs = T);animFiles
                animation <- magick::image_animate(magick::image_join(lapply(animFiles, magick::image_read)),fps=fps)
                magick::image_write(animation,paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/", scenario_i,"/",
                                            animName,sep = ""))
                print(gsub("//","/",paste("animation saved in :",dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/", scenario_i,"/",
                            animName,sep = "")))
                # fnameTempImage=paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/", scenario_i,"/",
                #                      animName,sep = "")
                # tempImage<-magick::image_read(fnameTempImage)
                # croppedImage<-magick::image_trim(tempImage,fuzz=0);
                # magick::image_write(croppedImage,fnameTempImage)


                #unlink(paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/", scenario_i,"/byYear/animate_",param_i,sep = ""), recursive = TRUE) #-------------- cleaning up plots and temporary variables
              } # If Animate ON==t


              #------------------------------
              # Figure 2 : each param: If class ==1 { Map x years}
              #-----------------------------

              checkTbl<-shapeTbl%>%dplyr::filter(scenario==scenario_i,param==param_i)


              checkTbl<-droplevels(checkTbl)
              if(length(unique(checkTbl$class))==1 & length(unique(checkTbl$x))>1){

                rm(checkTbl)

                datax<-shapeTbl%>%dplyr::filter(scenario==scenario_i,param==param_i)


                if(nrow(datax)>0){
                  legendTitle<-paste(unique(datax$units),sep="")
                  if(!is.null(classPaletteOrig)){fillPalette<-classPaletteOrig}else{
                    fillPalette<-as.character(unique(datax$classPalette))}

                  animScalePoly<-datax$value

                  # Choose correct scaleRange
                  scaleRange_i=scaleRange
                  if(grepl("DiffPrcnt",scenario_i)){
                    scaleRange_i=scaleRangeDiffPrcnt
                    datax <- datax %>% dplyr::mutate(units="Percent")
                    if(is.null(legendSingleColorOnOrig)){legendSingleColorOn=T}else{legendSingleColorOn=legendSingleColorOnOrig}
                  }
                  if(grepl("DiffAbs",scenario_i)){
                      scaleRange_i=scaleRangeDiffAbs
                      if(is.null(legendSingleColorOnOrig)){legendSingleColorOn=T}else{legendSingleColorOn=legendSingleColorOnOrig}
                    }
                  if(grepl("DiffxPrcnt",scenario_i)){
                    scaleRange_i=scaleRangeDiffxPrcnt
                    datax <- datax %>% dplyr::mutate(units="Percent")
                    if(is.null(legendSingleColorOnOrig)){legendSingleColorOn=T}else{legendSingleColorOn=legendSingleColorOnOrig}
                  }
                  if(grepl("DiffxAbs",scenario_i)){
                    scaleRange_i=scaleRangeDiffxAbs
                    if(is.null(legendSingleColorOnOrig)){legendSingleColorOn=T}else{legendSingleColorOn=legendSingleColorOnOrig}
                  }

                  if(is.null(legendSingleColorOnOrig)){
                    if(min(range(datax$value))<0 & max(range(datax$value))>0){
                      legendSingleColorOn=T}}else{legendSingleColorOn=legendSingleColorOnOrig}

                  if(!is.null(scaleRange_i)){
                     if(any(param_i %in% unique(scaleRange_i$param))){
                      if(max(animScalePoly) < (scaleRange_i %>% dplyr::filter(param==param_i))$maxScale){
                        animScalePoly<-c(animScalePoly,(scaleRange_i %>% dplyr::filter(param==param_i))$maxScale)} else {
                          animScalePoly <-  c((scaleRange_i %>% dplyr::filter(param==param_i))$maxScale,
                                              animScalePoly[animScalePoly<(scaleRange_i %>% dplyr::filter(param==param_i))$maxScale])
                        }
                      if(min(animScalePoly) > (scaleRange_i %>% dplyr::filter(param==param_i))$minScale){
                        animScalePoly<-c(animScalePoly,(scaleRange_i %>% dplyr::filter(param==param_i))$minScale)} else {
                          animScalePoly <-  c((scaleRange_i %>% dplyr::filter(param==param_i))$minScale,
                                              animScalePoly[animScalePoly>(scaleRange_i %>% dplyr::filter(param==param_i))$minScale])
                        }
                    }}
                  animPrettyBreaksPoly<-scales::pretty_breaks(n=legendFixedBreaks)(animScalePoly)
                  animKmeanBreaksPoly<-sort(as.vector((stats::kmeans(animScalePoly,
                                                                     centers=max(1,min(length(unique(animScalePoly))-1,(legendFixedBreaks-1)))))$centers[,1]))
                  if(!min(animScalePoly) %in% animKmeanBreaksPoly){
                  animKmeanBreaksPoly <- sort(c(min(animScalePoly),animKmeanBreaksPoly))}
                  if(!max(animScalePoly) %in% animKmeanBreaksPoly){
                    animKmeanBreaksPoly <- sort(c(animKmeanBreaksPoly,max(animScalePoly)))}

                  if((max(range(animScalePoly))-min(range(animScalePoly)))<1E-10 &
                     (max(range(animScalePoly))-min(range(animScalePoly)))>-1E-10){animScalePolyRange=min(animScalePoly)}else{
                       animScalePolyRange=range(animScalePoly)
                     }

                  if(abs(min(animScalePolyRange,na.rm = T))==abs(max(animScalePolyRange,na.rm = T))){animScalePolyRange=abs(min(animScalePolyRange,na.rm = T))}
                  if(mean(animScalePolyRange,na.rm = T)<0.01 & mean(animScalePolyRange,na.rm = T)>(-0.01)){animLegendDigits<-4}else{
                    if(mean(animScalePolyRange,na.rm = T)<0.1 & mean(animScalePolyRange,na.rm = T)>(-0.1)){animLegendDigits<-3}else{
                      if(mean(animScalePolyRange,na.rm = T)<1 & mean(animScalePolyRange,na.rm = T)>(-1)){animLegendDigits<-2}else{
                        if(mean(animScalePolyRange,na.rm = T)<10 & mean(animScalePolyRange,na.rm = T)>(-10)){animLegendDigits<-1}else{animLegendDigits<-0}}}}


                  datax<-datax%>%dplyr::select(subRegion,x,value)%>%
                    tidyr::spread(key=x,value=value)

                  # # Add in any missing subRegions to datax
                  # datax1<-expand.grid(unique(shape@data$subRegion)[!unique(shape@data$subRegion) %in% unique(datax$subRegion)]) %>%
                  #   dplyr::select(subRegion=Var1) %>%
                  #   dplyr::mutate(!!names(datax)[2]:=0)
                  #
                  # datax <- datax %>% dplyr::bind_rows(datax1%>%dplyr::mutate(subRegion=as.character(subRegion))) %>% dplyr::mutate(subRegion=as.factor(subRegion))


                  mapx<-shape
                  mapx@data<-mapx@data%>%dplyr::left_join(datax)%>%
                    dplyr::select(names(datax))

                  metis.map(facetsOn=T,innerMargins=innerMargins, legendDigitsOverride=legendDigitsOverride,facetLabelSize=facetLabelSize,mapTitleOn=mapTitleOn, facetCols=facetCols,numeric2Cat_list=numeric2Cat_list, catParam=param_i, underLayer=underLayer,  dataPolygon=mapx,
                            fillColumn = names(mapx@data%>%dplyr::select(-subRegion)),
                            mapTitle=paste(param_i," ",scenario_i,sep="") , legendShow = T,
                            legendOutside = legendOutsideSingle,
                            facetFreeScale = F,
                            frameShow = frameShow, facetLabelBorderLwd=facetLabelBorderLwd, facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor ,
                          legendSingleColorOn=legendSingleColorOn,legendSingleValue=legendSingleValue,legendSingleColor=legendSingleColor,
                            labels=labels,fillcolorNA=fillcolorNA,fillshowNA=fillshowNA,fillcolorNULL=fillcolorNULL,
                            labelsSize = labelsSize,
                            legendTitle =legendTitle,legendTitleSize = legendTitleSizeS,legendTextSize = legendTextSizeS,
                            legendStyle="fixed",
                            legendBreaks = animKmeanBreaksPoly,
                            legendFixedBreaks=legendFixedBreaks,
                            legendDigits = animLegendDigits,
                            legendOutsidePosition = legendOutsidePosition,
                            legendPosition = NULL,
                            fillPalette = fillPalette,
                            bgColor = bgColorChosen,figWidth=figWidth,figHeight=figHeight, pdfpng = pdfpng,
                            fileName = paste("map_",subRegType_ix,"_",param_i,"_",scenario_i,nameAppend,"_KMEANS",sep=""),
                            dirOutputs = paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/", scenario_i,sep = ""))

                  # numeric2Cat_list=numeric2Cat_list
                  # catParam=param_i
                  # underLayer=underLayer
                  # dataPolygon=mapx
                  # fillColumn = names(mapx@data%>%dplyr::select(-subRegion))
                  # mapTitle=paste(param_i," ",scenario_i,sep="") , legendShow = T
                  # legendOutside = legendOutsideSingle
                  # facetFreeScale = F
                  # frameShow = frameShow
                  # labels=labels
                  # labelsSize = labelsSize
                  # legendTitle =legendTitle
                  # legendTitleSize = legendTitleSizeS
                  # legendTextSize = legendTextSizeS
                  # legendStyle="fixed"
                  # legendBreaks = animKmeanBreaksPoly
                  # legendFixedBreaks=legendFixedBreaks
                  # legendDigits = animLegendDigits
                  # legendOutsidePosition = legendOutsidePosition
                  # legendPosition = NULL
                  # fillPalette = fillPalette
                  # bgColor = bgColorChosen
                  # figWidth=figWidth
                  # figHeight=figHeight
                  # fileName = paste("map_",subRegType_ix,"_",param_i,"_",scenario_i,nameAppend,"_KMEANS",sep="")
                  # dirOutputs = paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/", scenario_i,sep = "")

                  metis.map(facetsOn=T,innerMargins=innerMargins, legendDigitsOverride=legendDigitsOverride,facetLabelSize=facetLabelSize,mapTitleOn=mapTitleOn, facetCols=facetCols,numeric2Cat_list=numeric2Cat_list, catParam=param_i, underLayer=underLayer,  dataPolygon=mapx,
                            fillColumn = names(mapx@data%>%dplyr::select(-subRegion)),
                            mapTitle=paste(param_i," ",scenario_i,sep="") , legendShow = T,
                            legendOutside = legendOutsideSingle,
                            facetFreeScale = F,
                            frameShow = frameShow, facetLabelBorderLwd=facetLabelBorderLwd, facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor ,
                          legendSingleColorOn=legendSingleColorOn,legendSingleValue=legendSingleValue,legendSingleColor=legendSingleColor,
                            labels=labels,fillcolorNA=fillcolorNA,fillshowNA=fillshowNA,fillcolorNULL=fillcolorNULL,
                            labelsSize = labelsSize,
                            legendTitle =legendTitle,legendTitleSize = legendTitleSizeS,legendTextSize = legendTextSizeS,
                            legendStyle="fixed",
                            legendBreaks = animPrettyBreaksPoly,
                            legendFixedBreaks=legendFixedBreaks,
                            legendDigits = animLegendDigits,
                            legendOutsidePosition = legendOutsidePosition,
                            legendPosition = NULL,
                            fillPalette = fillPalette,
                            bgColor = bgColorChosen,figWidth=figWidth,figHeight=figHeight, pdfpng = pdfpng,
                            fileName = paste("map_",subRegType_ix,"_",param_i,"_",scenario_i,nameAppend,"_PRETTY",sep=""),
                            dirOutputs = paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/", scenario_i,sep = ""))


                  if("subRegion" %in% names(mapx@data)){countCheck=2}else{countCheck=1}
                  if(length(names(mapx@data))==countCheck){
                    legendOutsideAnimated=legendOutsideSingle
                    legendTitleSizeAnim = legendTitleSizeS
                    legendTextSizeAnim = legendTextSizeS}else{
                      legendOutsideAnimated=F
                      legendTitleSizeAnim = legendTitleSizeI
                      legendTextSizeAnim = legendTextSizeI
                    }

                  metis.map(facetsOn=T,innerMargins=innerMargins, legendDigitsOverride=legendDigitsOverride,facetLabelSize=facetLabelSize,mapTitleOn=mapTitleOn, facetCols=facetCols,numeric2Cat_list=numeric2Cat_list, catParam=param_i, underLayer=underLayer,  dataPolygon=mapx,
                            fillColumn = names(mapx@data%>%dplyr::select(-subRegion)),
                            mapTitle=paste(param_i," ",scenario_i,sep="") , legendShow = T,
                            legendOutside = legendOutsideAnimated,
                            facetFreeScale = T,
                            frameShow = frameShow, facetLabelBorderLwd=facetLabelBorderLwd, facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor ,
                          legendSingleColorOn=legendSingleColorOn,legendSingleValue=legendSingleValue,legendSingleColor=legendSingleColor,
                            labels=labels,fillcolorNA=fillcolorNA,fillshowNA=fillshowNA,fillcolorNULL=fillcolorNULL,
                            labelsSize = labelsSize,
                            legendTitle =legendTitle,legendTitleSize = legendTitleSizeAnim,legendTextSize = legendTextSizeAnim,
                            legendStyle="kmeans",
                            legendFixedBreaks=legendFixedBreaks,
                            legendDigits = NULL,
                            legendOutsidePosition = legendOutsidePosition,
                            legendPosition = legendPosition,
                            fillPalette = fillPalette,
                            bgColor = bgColorChosen,figWidth=figWidth,figHeight=figHeight, pdfpng = pdfpng,
                            fileName = paste("map_",subRegType_ix,"_",param_i,"_",scenario_i,nameAppend,"_FREESCALE",sep=""),
                            dirOutputs = paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/", scenario_i,sep = ""))

                  # Animate 2 : each param: If class == 1 { (Map x Anim Years}

                  # Calculate Mean

                  datax<-shapeTbl%>%dplyr::filter(scenario==scenario_i,param==param_i)

                  if(length(unique(datax$x))>1){

                  if(nrow(datax)>0){
                    legendTitle<-paste(unique(datax$units),sep="")
                    if(!is.null(classPaletteOrig)){fillPalette<-classPaletteOrig}else{
                      fillPalette<-as.character(unique(datax$classPalette))}

                    meanCol = paste("Mean_",min(datax$x),"to",max(datax$x),sep="")

                    datax<-datax%>%dplyr::select(subRegion,x,value)%>%
                      dplyr::group_by(subRegion)%>%
                      dplyr::summarize(!!meanCol:=mean(value))%>%
                      dplyr::ungroup()

                    animScalePoly<-datax[[meanCol]]

                    if(!is.null(scaleRange_i)){
                       if(any(param_i %in% unique(scaleRange_i$param))){
                        if(max(animScalePoly) < (scaleRange_i %>% dplyr::filter(param==param_i))$maxScale){
                          animScalePoly<-c(animScalePoly,(scaleRange_i %>% dplyr::filter(param==param_i))$maxScale)} else {
                            animScalePoly <-  c((scaleRange_i %>% dplyr::filter(param==param_i))$maxScale,
                                                animScalePoly[animScalePoly<(scaleRange_i %>% dplyr::filter(param==param_i))$maxScale])
                          }
                        if(min(animScalePoly) > (scaleRange_i %>% dplyr::filter(param==param_i))$minScale){
                          animScalePoly<-c(animScalePoly,(scaleRange_i %>% dplyr::filter(param==param_i))$minScale)} else {
                            animScalePoly <-  c((scaleRange_i %>% dplyr::filter(param==param_i))$minScale,
                                                animScalePoly[animScalePoly>(scaleRange_i %>% dplyr::filter(param==param_i))$minScale])
                          }
                      }}
                    animPrettyBreaksPoly<-scales::pretty_breaks(n=legendFixedBreaks)(animScalePoly)
                    animKmeanBreaksPoly<-sort(as.vector((stats::kmeans(animScalePoly,
                                                                       centers=max(1,min(length(unique(animScalePoly))-1,(legendFixedBreaks-1)))))$centers[,1]))
                    if(!min(animScalePoly) %in% animKmeanBreaksPoly){
                      animKmeanBreaksPoly <- sort(c(min(animScalePoly),animKmeanBreaksPoly))}
                    if(!max(animScalePoly) %in% animKmeanBreaksPoly){
                      animKmeanBreaksPoly <- sort(c(animKmeanBreaksPoly,max(animScalePoly)))}

                    if((max(range(animScalePoly))-min(range(animScalePoly)))<1E-10 &
                       (max(range(animScalePoly))-min(range(animScalePoly)))>-1E-10){animScalePolyRange=min(animScalePoly)}else{
                         animScalePolyRange=range(animScalePoly)
                       }

                    if(abs(min(animScalePolyRange,na.rm = T))==abs(max(animScalePolyRange,na.rm = T))){animScalePolyRange=abs(min(animScalePolyRange,na.rm = T))}
                    if(mean(animScalePolyRange,na.rm = T)<0.01 & mean(animScalePolyRange,na.rm = T)>(-0.01)){animLegendDigits<-4}else{
                      if(mean(animScalePolyRange,na.rm = T)<0.1 & mean(animScalePolyRange,na.rm = T)>(-0.1)){animLegendDigits<-3}else{
                        if(mean(animScalePolyRange,na.rm = T)<1 & mean(animScalePolyRange,na.rm = T)>(-1)){animLegendDigits<-2}else{
                          if(mean(animScalePolyRange,na.rm = T)<10 & mean(animScalePolyRange,na.rm = T)>(-10)){animLegendDigits<-1}else{animLegendDigits<-0}}}}

                    # # Add in any missing subRegions to datax
                    # datax1<-expand.grid(unique(shape@data$subRegion)[!unique(shape@data$subRegion) %in% unique(datax$subRegion)]) %>%
                    #   dplyr::select(subRegion=Var1) %>%
                    #   dplyr::mutate(!!names(datax)[2]:=0)
                    #
                    # datax <- datax %>% dplyr::bind_rows(datax1%>%dplyr::mutate(subRegion=as.character(subRegion))) %>% dplyr::mutate(subRegion=as.factor(subRegion))


                    mapx<-shape
                    mapx@data<-mapx@data%>%dplyr::left_join(datax)%>%
                      dplyr::select(names(datax))

                    metis.map(facetsOn=T,innerMargins=innerMargins, legendDigitsOverride=legendDigitsOverride,facetLabelSize=facetLabelSize,mapTitleOn=mapTitleOn, facetCols=facetCols,numeric2Cat_list=numeric2Cat_list, catParam=param_i, underLayer=underLayer, dataPolygon=mapx,
                              fillColumn = names(mapx@data%>%dplyr::select(-subRegion)),
                              mapTitle=paste(param_i," ",scenario_i,sep="") , legendShow = T,
                              legendOutside = legendOutsideSingle,
                              facetFreeScale = F,
                              frameShow = frameShow, facetLabelBorderLwd=facetLabelBorderLwd, facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor ,
                          legendSingleColorOn=legendSingleColorOn,legendSingleValue=legendSingleValue,legendSingleColor=legendSingleColor,
                              labels=labels,fillcolorNA=fillcolorNA,fillshowNA=fillshowNA,fillcolorNULL=fillcolorNULL,
                              labelsSize = labelsSize,
                              panelLabel = paste((names(datax%>%dplyr::select(-subRegion))[!names(datax%>%dplyr::select(-subRegion)) %in% c("lat","lon")]),sep=""),
                              legendTitle =paste(legendTitle,sep=""),
                              legendTitleSize = legendTitleSizeS,legendTextSize = legendTextSizeS,
                              legendStyle="fixed",
                              legendBreaks = animKmeanBreaksPoly,
                              legendFixedBreaks=legendFixedBreaks,
                              legendDigits = animLegendDigits,
                              legendOutsidePosition = legendOutsidePosition,
                              legendPosition = legendPositionS,
                              fillPalette = fillPalette,
                              bgColor = bgColorChosen,figWidth=figWidth,figHeight=figHeight, pdfpng = pdfpng,
                              fileName = paste("map_",subRegType_ix,"_",param_i,"_",scenario_i,nameAppend,"_MEAN_KMEANS",sep=""),
                              dirOutputs = paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/", scenario_i,sep = ""))

                    # numeric2Cat_list=numeric2Cat_list; catParam=param_i; underLayer=underLayer; dataPolygon=mapx;
                    # fillColumn = names(mapx@data%>%dplyr::select(-subRegion))
                    # mapTitle=paste(param_i," ",scenario_i,sep="");legendShow = T
                    # legendOutside = legendOutsideSingle;
                    # facetFreeScale = F;
                    # frameShow = frameShow;
                    # legendSingleColorOn=legendSingleColorOn;legendSingleValue=legendSingleValue;legendSingleColor=legendSingleColor;
                    # labels=labels;fillcolorNA=fillcolorNA;fillshowNA=fillshowNA;fillcolorNULL=fillcolorNULL;
                    # labelsSize = labelsSize;
                    # panelLabel = paste((names(datax%>%dplyr::select(-subRegion))[!names(datax%>%dplyr::select(-subRegion)) %in% c("lat","lon")]),sep="")
                    # legendTitle =paste(legendTitle,sep="")
                    # legendTitleSize = legendTitleSizeS;legendTextSize = legendTextSizeS;
                    # legendStyle="fixed";
                    # legendBreaks = animKmeanBreaksPoly;
                    # legendFixedBreaks=legendFixedBreaks;
                    # legendDigits = animLegendDigits;
                    # legendOutsidePosition = legendOutsidePosition;
                    # legendPosition = legendPositionS;
                    # fillPalette = fillPalette;
                    # bgColor = bgColorChosen;figWidth=figWidth;figHeight=figHeight; pdfpng = pdfpng
                    # fileName = paste("map_",subRegType_ix,"_",param_i,"_",scenario_i,nameAppend,"_MEAN_KMEANS",sep="")
                    # dirOutputs = paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/", scenario_i,sep = "")

                    metis.map(facetsOn=T,innerMargins=innerMargins, legendDigitsOverride=legendDigitsOverride,facetLabelSize=facetLabelSize,mapTitleOn=mapTitleOn, facetCols=facetCols,numeric2Cat_list=numeric2Cat_list, catParam=param_i, underLayer=underLayer,  dataPolygon=mapx,
                              fillColumn = names(mapx@data%>%dplyr::select(-subRegion)),
                              mapTitle=paste(param_i," ",scenario_i,sep="") , legendShow = T,
                              legendOutside = legendOutsideSingle,
                              facetFreeScale = F,
                              frameShow = frameShow, facetLabelBorderLwd=facetLabelBorderLwd, facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor ,
                          legendSingleColorOn=legendSingleColorOn,legendSingleValue=legendSingleValue,legendSingleColor=legendSingleColor,
                              labels=labels,fillcolorNA=fillcolorNA,fillshowNA=fillshowNA,fillcolorNULL=fillcolorNULL,
                              labelsSize = labelsSize,
                              panelLabel = paste((names(datax%>%dplyr::select(-subRegion))[!names(datax%>%dplyr::select(-subRegion)) %in% c("lat","lon")]),sep=""),
                              legendTitle =paste(legendTitle,sep=""),
                              legendTitleSize = legendTitleSizeS,legendTextSize = legendTextSizeS,
                              legendStyle="fixed",
                              legendBreaks = animPrettyBreaksPoly,
                              legendFixedBreaks=legendFixedBreaks,
                              legendDigits = animLegendDigits,
                              legendOutsidePosition = legendOutsidePosition,
                              legendPosition = legendPositionS,
                              fillPalette = fillPalette,
                              bgColor = bgColorChosen,figWidth=figWidth,figHeight=figHeight, pdfpng = pdfpng,
                              fileName = paste("map_",subRegType_ix,"_",param_i,"_",scenario_i,nameAppend,"_MEAN_PRETTY",sep=""),
                              dirOutputs = paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/", scenario_i,sep = ""))

                    if("subRegion" %in% names(mapx@data)){countCheck=2}else{countCheck=1}
                    if(length(names(mapx@data))==countCheck){
                      legendOutsideAnimated=legendOutsideSingle
                      legendTitleSizeAnim = legendTitleSizeS
                      legendTextSizeAnim = legendTextSizeS}else{
                         legendOutsideAnimated=F
                        legendTitleSizeAnim = legendTitleSizeI
                        legendTextSizeAnim = legendTextSizeI
                      }

                    metis.map(facetsOn=T,innerMargins=innerMargins, legendDigitsOverride=legendDigitsOverride,facetLabelSize=facetLabelSize,mapTitleOn=mapTitleOn, facetCols=facetCols,numeric2Cat_list=numeric2Cat_list, catParam=param_i, underLayer=underLayer,  dataPolygon=mapx,
                              fillColumn = names(mapx@data%>%dplyr::select(-subRegion)),
                              mapTitle=paste(param_i," ",scenario_i,sep="") , legendShow = T,
                              legendOutside = legendOutsideAnimated,
                              facetFreeScale = T,
                              frameShow = frameShow, facetLabelBorderLwd=facetLabelBorderLwd, facetBGColor=facetBGColor,  facetLabelColor=facetLabelColor ,
                          legendSingleColorOn=legendSingleColorOn,legendSingleValue=legendSingleValue,legendSingleColor=legendSingleColor,
                              labels=labels,fillcolorNA=fillcolorNA,fillshowNA=fillshowNA,fillcolorNULL=fillcolorNULL,
                              labelsSize = labelsSize,
                              panelLabel = paste((names(datax%>%dplyr::select(-subRegion))[!names(datax%>%dplyr::select(-subRegion)) %in% c("lat","lon")]),sep=""),
                              legendTitle =paste(legendTitle,sep=""),
                              legendTitleSize = legendTitleSizeAnim,legendTextSize = legendTextSizeAnim,
                              legendStyle="kmeans",
                              legendFixedBreaks=legendFixedBreaks,
                              legendDigits = NULL,
                              legendOutsidePosition = legendOutsidePosition,
                              legendPosition = legendPositionS,
                              fillPalette = fillPalette,
                              bgColor = bgColorChosen,figWidth=figWidth,figHeight=figHeight, pdfpng = pdfpng,
                              fileName = paste("map_",subRegType_ix,"_",param_i,"_",scenario_i,nameAppend,"_MEAN_FREESCALE",sep=""),
                              dirOutputs = paste(dirOutputsX,"/",folderName, "/",mapFolder,"/",subRegType_ix,"/",param_i,"/", scenario_i,sep = ""))


                    # Animate 2 : each param: If class == 1 { (Map x Anim Years}

                  }  #if(nrow(datax)>0){
                  } # If no multiple years
                }  #if(nrow(datax)>1){
              } # If number of classes == 1


               # If nrow greater than 0

          }# nrow shapeTbl after Shapefiles loaded
            }# Close Run Section
            }# If shapeTbl rows >0


            shapeTblReturn <- shapeTblReturn %>% dplyr::bind_rows(shapeTbl)

            } # close Params

      } # Close scenario loop

  } # Close if shapeTbl is Null
  } # Close create polygon plots


  if(!is.null(gridTblReturn)){
    for(c_i in c("group","query","mapPalette")){
      if(c_i %in% names(gridTblReturn)){
        gridTblReturn <- gridTblReturn %>% dplyr::select(-c_i)
      }
    }
  }

  if(!is.null(shapeTblReturn)){
    for(c_i in c("group","query","mapPalette")){
      if(c_i %in% names(shapeTblReturn)){
        shapeTblReturn <- shapeTblReturn %>% dplyr::select(-c_i)
      }
    }
  }

  listx <-list(gridTbl=gridTblReturn,
               shapeTbl=shapeTblReturn)

  print("metis.mapsProcess run completed.")

  invisible(listx)

} # close function
