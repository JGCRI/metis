#' metis.index
#'
#' This function finds the grids located within a given shapefiles regions
#'
#' @param data Default = NULL. Full path to grid file.
#' @param colIndex Default = NULL,
#' @param colValue Default = NULL,
#' @param numerators Default = NULL,
#' @param denominators Default = NULL,
#' @param scenariosSelect Default = NULL,
#' @param indexName Default = "index",
#' @param nameAppend Default = ""
#' @param meanYearsDenominator Default = NULL,
#' @param dirOutputs Default = paste(getwd(),"/outputs",sep=""),
#' @param fname Default = "gridByPoly"
#' @param folderName Default ="folderNameDefault",
#' @param saveFile Default = F. If want csv output then change to T
#' @keywords grid, shape, polygon
#' @return dataframe with index
#' @export

#-------------
# Print to PDF or PNG
#-------------

metis.index <- function(data = NULL,
                         colIndex = NULL,
                         colValue = NULL,
                         numerators = NULL,
                         denominators = NULL,
                         meanYearsDenominator = NULL,
                         scenariosSelect = NULL,
                         indexName = "index",
                         dirOutputs = paste(getwd(),"/outputs",sep=""),
                         fname = "index",
                         nameAppend = "",
                         folderName=NULL,
                         saveFile = F){

  # data = NULL
  # colIndex = NULL
  # colValue = NULL
  # numerators = NULL
  # denominators = NULL
  # meanYearsDenominator = NULL
  # scenariosSelect = NULL
  # indexName = "index"
  # dirOutputs = paste(getwd(),"/outputs",sep="")
  # fname = "index"
  # nameAppend = ""
  # folderName=NULL
  # saveFile = F


print(paste("Starting metis.index.R...",sep=""))


#----------------
# Initializa
#---------------

  NULL -> datax1 -> scenario -> year -> subRegType -> param -> value -> x -> subRegion -> scenarioD ->
    scenarioN

#----------------
# Create Folders
#---------------

  if(saveFile){
  if (!dir.exists(dirOutputs)){dir.create(dirOutputs)}
  if (!dir.exists(paste(dirOutputs,"/",folderName, sep = ""))){dir.create(paste(dirOutputs, "/",folderName,sep = ""))}
  if (!dir.exists(paste(dirOutputs,"/",folderName, "/Index/", sep = ""))){dir.create(paste(dirOutputs, "/",folderName, "/Index/", sep = ""))}
    dir = gsub("//","/",paste(dirOutputs,"/",folderName, "/Index/",sep = ""))
  }

#------------------
# Function for adding any missing columns if needed
# -----------------

  if(T){
    addMissing<-function(data){
      if(!any(grepl("\\<scenario\\>",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(scenario="scenario")}else{
        data <- data %>% dplyr::rename(!!"scenario" := (names(data)[grepl("\\<scenario\\>",names(data),ignore.case = T)])[1])
        data<-data%>%dplyr::mutate(scenario=as.character(scenario),scenario=dplyr::case_when(is.na(scenario)~"scenario",TRUE~scenario))}
      if(!any(grepl("\\<scenarios\\>",names(data),ignore.case = T))){}else{
        data <- data %>% dplyr::rename(!!"scenario" := (names(data)[grepl("\\<scenarios\\>",names(data),ignore.case = T)])[1])
        data<-data%>%dplyr::mutate(scenario=as.character(scenario),scenario=dplyr::case_when(is.na(scenario)~"scenario",TRUE~scenario))}
      if(!"x"%in%names(data)){if("year"%in%names(data)){
        data<-data%>%dplyr::mutate(x=year)}else{data<-data%>%dplyr::mutate(x="x")}}
      if(!any(grepl("\\<subregtype\\>",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(subRegType="subRegType")}else{
        data <- data %>% dplyr::rename(!!"subRegType" := (names(data)[grepl("\\<subregtype\\>",names(data),ignore.case = T)])[1])
        data<-data%>%dplyr::mutate(subRegType=as.character(subRegType),subRegType=dplyr::case_when(is.na(subRegType)~"subRegType",TRUE~subRegType))}
      return(data)
    }
  }



#----------------
# Check input data
#---------------

  if(!is.null(data)){

    if(all(!class(data) %in% c("tbl_df","tbl","data.frame"))){

      datax <- tibble::tibble()

      for(data_i in data){
        if(file.exists(data_i)){
          dataxNew<-data.table::fread(paste(data_i),encoding="Latin-1")%>%tibble::as_tibble()
          datax<-dplyr::bind_rows(datax,dataxNew)
          rm(dataxNew)
        } else {stop(paste(data_i," does not exist"))}
      }
    }else{datax<-data}

  }else{stop("Data is NULL.")}

  datax <- addMissing(datax)


  # Check if colIndex, colValue and scenarios exist
  #----------------------------------------------------------
  if(is.null(colIndex)){
    if(!any(grepl("\\<params\\>",names(datax),ignore.case = T))){}else{
      datax <- datax %>% dplyr::rename(!!"param" := (names(datax)[grepl("\\<params\\>",names(datax),ignore.case = T)])[1])
      datax<-datax%>%dplyr::mutate(param=as.character(param),param=dplyr::case_when(is.na(param)~"params",TRUE~param))}
    if(!any(grepl("\\<param\\>",names(datax),ignore.case = T))){datax<-datax%>%dplyr::mutate(param="param")}else{
      datax <- datax %>% dplyr::rename(!!"param" := (names(datax)[grepl("\\<param\\>",names(datax),ignore.case = T)])[1])
      datax<-datax%>%dplyr::mutate(param=as.character(param),param=dplyr::case_when(is.na(param)~"param",TRUE~param))}
    colIndex="param"
    print("colIndex not provided and set to default: param")
    }else{
    if(!colIndex %in% names(datax)){stop(paste("colIndex provided: ",colIndex," does not exist in datax.",sep=""))}
      }

  if(is.null(colValue)){
    if(!any(grepl("\\<values\\>",names(datax),ignore.case = T))){}else{
      datax <- datax %>% dplyr::rename(!!"value" := (names(datax)[grepl("\\<values\\>",names(datax),ignore.case = T)])[1])
      datax<-datax%>%dplyr::mutate(value=as.character(value),value=dplyr::case_when(is.na(value)~NA_real_,TRUE~value))
    if(!any(grepl("\\<value\\>",names(datax),ignore.case = T))){datax<-datax%>%dplyr::mutate(value=NA_real_)}else{
      datax <- datax %>% dplyr::rename(!!"value" := (names(datax)[grepl("\\<value\\>",names(datax),ignore.case = T)])[1])
      datax<-datax%>%dplyr::mutate(value=as.character(value),value=dplyr::case_when(is.na(value)~NA_real_,TRUE~value))}
    }
    colValue="value"
    print("colValue not provided and set to default: value")
  }else{
    if(!colValue %in% names(datax)){stop(paste("colValue provided: ",colValue," does not exist in datax.",sep=""))}
  }



  # Check if numerators provided exist in datax colIndex
  #--------------------------------------------------------------------
  if(!any(numerators %in% unique(datax[[colIndex]]))){
    stop(print(paste("None of the chosen numerators: ", paste(numerators,collapse=", "),
                     " exist in the colIndex: ", colIndex,sep="")))
  }else{
    if(!all(numerators %in% unique(datax[[colIndex]]))){
      print(paste("Not all chosen numerators: ",
                  paste(numerators,collapse=", "),
                  " exist in the colIndex: ",
                  paste(unique(datax[[colIndex]]),collapse=", "),sep=""))
      print(paste("Using available numerators: ",
            paste(numerators[numerators %in% unique(datax[[colIndex]])],collapse=", "),
            sep=""))
      numerators = numerators[numerators %in% unique(datax[[colIndex]])]
    }
  }

  # Check if denominators provided exist in datax colIndex
  #--------------------------------------------------------------------
  if(!any(denominators %in% unique(datax[[colIndex]]))){
    stop(print(paste("None of the chosen denominators: ", paste(denominators,collapse=", "),
                     " exist in the colIndex: ", colIndex,sep="")))
  }else{
    if(!all(denominators %in% unique(datax[[colIndex]]))){
      print(paste("Not all chosen denominators: ",
                  paste(denominators,collapse=", "),
                  " exist in the colIndex: ",
                  paste(unique(datax[[colIndex]]),collapse=", "),sep=""))
      print(paste("Using available denominators: ",
                  paste(denominators[denominators %in% unique(datax[[colIndex]])],collapse=", "),
                  sep=""))
      denominators = denominators[denominators %in% unique(datax[[colIndex]])]
    }
  }

  # Check if scenarios provided exist in datax scenario
  #--------------------------------------------------------------------
  if(is.null(scenariosSelect)){
    scenariosSelectx <- unique(datax[["scenario"]])
    print("Using available scenarios: ")
    print(paste(scenariosSelectx,collapse=", "))
  }else{
  if(!any(scenariosSelect %in% unique(datax$"scenario"))){
    stop(print(paste("None of the chosen scenario: ", paste(scenariosSelect,collapse=", "),
                     " exist in the column scenario",sep="")))
  }else{
    if(!all(scenariosSelect) %in% unique(datax[["scenario"]])){
      print(paste("Not all chosen scenarios: ",
                  paste(scenariosSelect,collapse=", "),
                  " exist in the col scenario: ",
                  paste(unique(datax[["scenario"]]),collapse=", "),sep=""))
      print("Using available scenarios: ")
      print(paste(scenariosSelect[scenariosSelect %in% unique(datax[["scenario"]])],collapse=", "))
      scenariosSelectx = scenariosSelect[scenariosSelect %in% unique(datax[["scenario"]])]
    }
  }
    }

#----------------
# Calculate Index
#---------------

  cols <- c("subRegion","scenario","subRegType",colIndex,colValue,"x");cols
  colsNoVal <- c("subRegion","scenario",colIndex,"subRegType","x"); colsNoVal
  datax <- datax %>% dplyr::select(tidyselect::all_of(cols))

   for(numerator_i in numerators){
      for(denominator_i in denominators){

  dataxNumerator <- datax %>%
    dplyr::filter(datax[["scenario"]] %in% scenariosSelectx,
                  datax[[colIndex]] == numerator_i) %>%
    dplyr::rename(!!numerator_i:=!!as.name(colValue),
                  scenarioN=scenario)%>%
    dplyr::select(-!!colIndex); dataxNumerator

  dataxDenominator <- datax %>%
    dplyr::filter(datax[["scenario"]] %in% scenariosSelectx,
                  datax[[colIndex]] == denominator_i) %>%
    dplyr::rename(!!denominator_i:=!!as.name(colValue),
                  scenarioD = scenario)%>%
    dplyr::select(-!!colIndex); dataxDenominator

  if(!any(unique(dataxDenominator$x)) %in% unique(dataxNumerator$x)){
      print("None of the years for the numerator and denominator selected match so can't compute an index.")
      print("Taking the mean of selected denominator years and setting to numerator years.")
      if(is.null(meanYearsDenominator)){meanYearsDenominator="All"}
  }

  if(any(meanYearsDenominator=="All")){
    meanYearsDenominator = unique(dataxDenominator$x)
    print("Using the mean of all denominator years: ")
    print(paste(unique(dataxDenominator$x),collapse=", "))
  }

  if(!is.null(meanYearsDenominator)){

    dataxDenominatorX <- dataxDenominator %>%
      dplyr::filter(x %in% meanYearsDenominator) %>%
      dplyr::group_by(subRegion,scenarioD)%>%
      dplyr::summarize(!!denominator_i:=mean(!!as.name(denominator_i),na.rm=T));dataxDenominatorX

    dataxDenominatorY <- tibble::tibble()
    for(x_i in unique(dataxNumerator$x)){
      dataxDenominatorY <- dataxDenominatorY %>%
        dplyr::bind_rows(dataxDenominatorX %>%
                           dplyr::mutate(x=x_i))
    }; dataxDenominatorY

    dataxDenominator <- dataxDenominatorY

  }

  datax1 <- dataxNumerator %>%
    dplyr::left_join(dataxDenominator)%>%
    dplyr::mutate(scenario=paste(scenarioN,scenarioD,sep="_"))%>%
    dplyr::select(-scenarioD,-scenarioN)%>%
    dplyr::mutate(!!indexName := dplyr::case_when(denominator_i==0~NA_real_,
                                           TRUE~!!as.name(numerator_i)/!!as.name(denominator_i)),
                  param=indexName)%>%
    dplyr::select(-!!denominator_i,-!!numerator_i)%>%
    dplyr::rename(!!colValue:=!!as.name(indexName)); datax1


      }
    }

  #-------------------
  # Save Data
  #-------------------

  if(saveFile==T){
  if(nrow(datax1)>0){
    data.table::fwrite(datax1,
                       paste(dir,"/",fname,nameAppend,".csv",sep = ""))
    print(gsub("//","/",paste("File saved to ",dir,"/",fname,nameAppend,".csv",sep = "")))
  }
  }

  print(paste("metis.index.R finished running succesfully.",sep=""))

return(datax1)

}
