#' metis.downscale
#'
#' This function downscales data by subregion by given gridded data
#'
#'@param polygonTable Default = NULL,
#' @param gridTable Default = NULL. Full path to grid file or R tibble or for pre-loaded data one of "pop".
#' @param downscaleRes Default = grid050. Can Choose grid025 or provide a dataframe with equally spaced lat and lon.
#' @param shape Default = NULL,
#' @param shapeFolder Default = NULL,
#' @param shapeFile Default = NULL,
#' @param colName Default = "subRegion",
#' @param dirOutputs Default = paste(getwd(),"/outputs",sep=""),
#' @param fname Default = "gridByPoly"
#' @param nameAppend Default = NULL
#' @param folderName Default = NULL
#' @param saveFile Default = F. If want csv output then change to T
#' @param printFig Default = F. If grid overlap with shape is wanted
#' @keywords grid, shape, polygon
#' @return Prints out graphic
#' @export


metis.downscale <- function(  polygonTable=NULL,
                              gridTable = NULL,
                              downscaleRes = "grid050",
                              shape = NULL,
                              shapeFolder = NULL,
                              shapeFile = NULL,
                              colName = "subRegion",
                              dirOutputs = paste(getwd(),"/outputs",sep=""),
                              fname = "downscale",
                              nameAppend = NULL,
                              folderName=NULL,
                              saveFile = T,
                              printFig = F){

  # polygonTable=NULL
  # gridTable = NULL
  # downscaleRes = "grid050"
  # shape = NULL
  # shapeFolder = NULL
  # shapeFile = NULL
  # colName = "subRegion"
  # dirOutputs = paste(getwd(),"/outputs",sep="")
  # fname = "downscale"
  # nameAppend = NULL
  # folderName=NULL
  # saveFile = T
  # printFig = F


  print(paste("Starting metis.downscale.R...",sep=""))

  #----------------
  # Initialize
  #---------------

  NULL -> scenario -> year -> subRegType -> region -> classPalette -> param -> lat -> lon -> subRegion ->
    id -> gridlat -> gridlon -> gridID -> gridChoice -> x -> valueGrid -> sumVal -> value -> ratio -> scenario

  #----------------
  # Functions
  #---------------

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


  #----------------
  # Create Folders
  #---------------

  if(T){

  if(saveFile){

  downscaleFolder <- gsub(" ","",paste("downscale",nameAppend,sep=""))

  if (!dir.exists(dirOutputs)){dir.create(dirOutputs)}
  if (!dir.exists(paste(dirOutputs,"/",folderName, sep = ""))){dir.create(paste(dirOutputs, "/",folderName,sep = ""))}
  if (!dir.exists(paste(dirOutputs,"/",folderName, "/",downscaleFolder,"/", sep = ""))){dir.create(paste(dirOutputs, "/",folderName, "/",downscaleFolder,"/", sep = ""))}
    dir = paste(dirOutputs,"/",folderName, "/",downscaleFolder,"/",sep = "")
  }
    } # Close Section

  #------------------
  # Assign Default Grid Data if Available
  #------------------

  if(T){
    if(grepl("^pop$",gridTable,ignore.case = T) | grepl("^population$",gridTable,ignore.case =T)){
       gridTable <- metis::grid_pop_GPWv4To2015
       print("Downscaling by population. Using pre-loaded population data (metis::grid_pop_GPWv4To2015:")
       print("Gridded Population of the World, Version 4 (GPWv4)")
       print("Available at: https://sedac.ciesin.columbia.edu/data/set/gpw-v4-population-count-rev11/data-download#")
    }
  }

  #------------------
  # Check Grid Data
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

        if(!"lat" %in% names(gridTbl)){stop("'lat' column not present in grid data provided. Need to have lat. Check data.",sep="")}
        if(!"lon" %in% names(gridTbl)){stop("'lon' column not present in grid data provided. Need to have lat. Check data.",sep="")}


        gridTbl <- gridTbl %>%
          tidyr::gather(key="x",value="valueGrid",-lat,-lon) %>%
          tibble::as_tibble()

        if(!"x"%in%names(gridTbl)){gridTbl<-gridTbl%>%dplyr::mutate(x="x")}

      }}

  } # Close read in Grid Tables

  #------------------
  # Check Polygon Data
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

        if(!"value" %in% names(shapeTbl)){stop("'value' column not present in polygon data provided. Need to have values. Check data.",sep="")}
        if(!"subRegion" %in% names(shapeTbl)){stop("'subRegion' column not present in polygon data provided. Need to have values. Check data.",sep="")}

      }}

    shapeTbl <- addMissing(shapeTbl)

  } # Read in Shape Tables


  #------------------
  # Check Shape Data
  #------------------

  if(T){

    # Check if folder and file provided to read in Shapefile
    if(is.null(shape)){
      if(!is.null(shapeFolder) & !is.null(shapeFile)){
        if(!dir.exists(shapeFolder)){
          stop("shapefile folder: ", shapeFolder ," is incorrect or doesn't exist.",sep="")}
        if(!file.exists(paste(shapeFolder,"/",shapeFile,".shp",sep=""))){
          stop("shape file: ", paste(shapeFolder,"/",shapeFile,".shp",sep="")," is incorrect or doesn't exist.",sep="")}
        shape=rgdal::readOGR(dsn=shapeFolder,layer=shapeFile,use_iconv=T,encoding='UTF-8')
        print(paste("Sshape : ",shapeFolder,"/",shapeFile,".shp",sep=""))
        print(raster::head(shape))
      } # if(!is.null(shapeFolder) & !is.null(shapeFile)){
    }

    # Check if Pre-loaded shapes available
    if(is.null(shape)){
      shape <- metis.mapFind(shapeTbl)$subRegShapeFound
    }

    # Check if any valid shape found
    if(is.null(shape)){
      stop("No valid subregional shape file available")}

    # Check if colName is available in Data
    if(!colName %in% names(shape)){stop(paste("colName: ",colName," not present in shape",sep=""))}


    # Check if any subregions in data Table are not present in the shape file
    if(!is.null(shape)){
      shape@data[[colName]] <- as.character(shape@data[[colName]])
      if(!all(unique(shapeTbl$subRegion) %in% unique(shape@data[[colName]]))){
        print(paste("Removing subRegions not present in shapefile from datatable: ",
                    paste(unique(shapeTbl$subRegion)[!unique(shapeTbl$subRegion) %in% unique(shape@data[[colName]])],collapse=", "),sep=""))
        shapeTbl <- shapeTbl %>% dplyr::filter(subRegion %in% unique(shape@data[[colName]]))
        print(paste("Remaining subRegions in dataTable are: ",paste(unique(shapeTbl$subRegion),collapse=", "),sep=""))}
    }

    if(!any(unique(shapeTbl$subRegion) %in% unique(shape@data[[colName]]))){
      print(paste("None of the subRegions in data provided match subRegions in the shapefile provided or available.",sep=""))
    }

    # Subset the shape to the colName regions in the data provided
    shape <- shape[shape@data[[colName]] %in% unique(shapeTbl$subRegion),]
    shape@data <- droplevels(shape@data)

    # Set COlumn Name
    shape@data<-shape@data%>%dplyr::mutate(subRegion=get(colName))

  }

  #------------------------------------
  # Check Downscale Resolution
  #------------------------------------

  if(T){

  if(!any(class(downscaleRes) %in% c("tbl_df","tbl","data.frame"))){

    if(!any(downscaleRes %in% c("grid050","grid025"))){
      print(paste("downscaleRes chosen ", downscaleRes, "is not valid. Please choose either grid025 or grid050.",sep=""))
      print("Setting downscaleRes to grid050")
      downscaleRes <- metis::grid050
    }

    if(downscaleRes == "grid025"){
      print(paste("downscaleRes chosen: ", downscaleRes,sep=""))
      downscaleRes <- metis::grid025
    }

    if(downscaleRes == "grid050"){
      print(paste("downscaleRes chosen: ", downscaleRes,sep=""))
      downscaleRes <- metis::grid050
    }

  } else {

    if(!any(grepl("lat",names(downscaleRes),ignore.case=T))){stop("downscaleRes must contain column lat and lon.")}
    if(!any(grepl("lon",names(downscaleRes),ignore.case=T))){stop("downscaleRes must contain column lat and lon.")}

      if(!any(grepl("\\<lat\\>",names(downscaleRes),ignore.case = F))){}else{
        downscaleRes <- downscaleRes %>% dplyr::rename(!!"lat" := (names(downscaleRes)[grepl("\\<lat\\>",names(downscaleRes),ignore.case = T)])[1])
        downscaleRes<-downscaleRes%>%dplyr::mutate(lat=as.character(lat),lat=dplyr::case_when(is.na(lat)~"lat",TRUE~lat))}
      if(!any(grepl("\\<lon\\>",names(downscaleRes),ignore.case = F))){}else{
        downscaleRes <- downscaleRes %>% dplyr::rename(!!"lon" := (names(downscaleRes)[grepl("\\<lon\\>",names(downscaleRes),ignore.case = T)])[1])
        downscaleRes<-downscaleRes%>%dplyr::mutate(lon=as.character(lon),lat=dplyr::case_when(is.na(lon)~"lon",TRUE~lon))}

      downscaleRes <- downscaleRes %>%
        dplyr::select(lat,lon) %>%
        dplyr::mutate(lat=as.numeric(as.character(lat)),
                      lon=as.numeric(as.character(lon)))
    }
  }


  #------------------------------------
  # Re-assign gridded data to chosen downscaleRes
  #------------------------------------

  if(T){

    listOfGridCells <- downscaleRes

    if(!("id" %in% names(listOfGridCells))){
      print("grid id column not found within grid file, creating a new id column...")
      listOfGridCells <- tibble::rowid_to_column(listOfGridCells, var = "id")
    }

    listOfGridCells <- listOfGridCells %>%
      dplyr::mutate(
        gridlat = lat,
        gridlon = lon,
        gridID = id) %>%
      dplyr::select(gridlat,gridlon,gridID)

    latmin<-min(listOfGridCells$gridlat); latmin
    latmax<-max(listOfGridCells$gridlat); latmax
    lonmin<-min(listOfGridCells$gridlon); lonmin
    lonmax<-max(listOfGridCells$gridlon); lonmax

    latranked<-listOfGridCells$gridlat[sort.list(listOfGridCells$gridlat)]%>%
      unique(); latranked
    lonranked<-listOfGridCells$gridlon[sort.list(listOfGridCells$gridlon)]%>%
      unique(); lonranked

    # This assumes equally spaced grids by degree.
    gridDimlat<-min(abs(latranked[2:length(latranked)]-latranked[1:length(latranked)-1])); gridDimlat
    gridDimlon<-min(abs(lonranked[2:length(lonranked)]-lonranked[1:length(lonranked)-1])); gridDimlon
    gridShiftlat<-latranked[sort.list(abs(latranked))][1]; gridShiftlat  # The latitude of the center of the grid cells closest to the equator
    gridShiftlon<-lonranked[sort.list(abs(lonranked))][1]; gridShiftlon  # The longitude of the center of the grid cells closest to prime meridian, Greenwich Meridian
    listOfGridCells$gridlat<-round(listOfGridCells$gridlat, digits = 10); listOfGridCells$gridlat
    listOfGridCells$gridlon<-round(listOfGridCells$gridlon, digits = 10); listOfGridCells$gridlon

    if(!(sum(round(latranked, digits = 4) %in% round(seq(latmin,latmax,length.out = (round((latmax-latmin)/gridDimlat)+1)),digits = 4))==length(latranked))){
      stop(paste("grid file ", gridChoice, " does not contain the centers of regurlarly-spaced lat lon grid cells.",sep=""))}


    # Re-organizing gridTable
    gridTblx <- gridTbl %>%
      dplyr::mutate(
        latOrig = lat,
        lonOrig = lon,
        lat = round(gridDimlat*round(lat*(1/gridDimlat)-(gridShiftlat/gridDimlat))+gridShiftlat, digits = 10),
        lon = round(gridDimlon*round(lon*(1/gridDimlon)-(gridShiftlon/gridDimlon))+gridShiftlon, digits = 10))%>%
      tibble::as_tibble(); gridTblx

      }

  #------------------------------------
  # Get grid cells in each shape
  # Get sum of values per shape and x
  # Get ratio of grid cell to sum per shape and x
  #------------------------------------

  if(T){
    shapeGrids <- metis.gridByPoly(gridTable=gridTblx %>% dplyr::ungroup(),
                                   shape=shape,colName=colName); shapeGrids

    shapeGridsx <- shapeGrids %>%
      dplyr::group_by(subRegion,x) %>%
      dplyr::mutate(sumVal=sum(valueGrid,na.rm=T),
                    ratio = valueGrid/sumVal)%>%
      dplyr::ungroup(); shapeGridsx
  }

  #------------------------------------
  # Attach polygon data value by subRegion and any common x values.
  # If no common x values use the latest x in the downscaling grid
  #------------------------------------

  if(T){
  shapeGridsAll <- tibble::tibble()
  yearsMissing <- unique(shapeTbl$x)[!unique(shapeTbl$x) %in% unique(shapeGridsx$x)]; yearsMissing

  if(length(yearsMissing)>0){
    print(paste("gridTable being used for downscaling does not contain years present in polygon data: ", paste(yearsMissing,collapse=", "),sep=""))
    print(paste("Setting missing years to data for latest available year in gridTable: ", max(unique(shapeGridsx$x)), sep=""))
  }

  for(x_i in yearsMissing){
    shapeGridsAll <- shapeGridsAll %>%
      dplyr::bind_rows(shapeGridsx %>%
                         dplyr::filter(x==max(unique(shapeGridsx$x))) %>%
                         dplyr::mutate(x=x_i))
  }
  shapeGridsAll <- shapeGridsAll %>%
    dplyr::mutate(x=as.character(x)) %>%
    dplyr::bind_rows(shapeGridsx); shapeGridsAll

  downscaled <- shapeGridsAll %>%
    dplyr::left_join(shapeTbl %>% dplyr::mutate(x=as.character(x)),by=c("subRegion","x")) %>%
    dplyr::mutate(valuePoly=value,
                  value=value*ratio) %>%
    dplyr::filter(!is.na(value))
  downscaled

  # Check
  shapeTbl %>% dplyr::group_by(scenario,param) %>% dplyr::summarize(SUM=sum(value))
  downscaled %>% dplyr::group_by(scenario,param) %>% dplyr::summarize(SUM=sum(value))

  }

  #----------------
  # Save Data
  #---------------

    # Save table with lat, lon, polygonID, param, downscaledVal, polygonOrigVal, units, aggType, any other columns from original polygon

  if(T){
    if(saveFile){
      for(param_i in unique(downscaled$param)){
        fname<-gsub(".csv","",fname)
        fname<- paste(dir,"/",fname,"_",param_i,nameAppend,".csv",sep="")
        data.table::fwrite(downscaled, file = fname,row.names = F)
        print(paste("File saved as ",fname,sep=""))}
    }
  }

  #----------------
  # Return Data
  #---------------
  print(paste("metis.downscale.R run complete.",sep=""))
  invisible(downscaled)

}
