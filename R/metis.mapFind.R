#' metis.mapFind
#'
#' Given a data.frame with a subRegion column, this function searches for an appropriate map from the pre-loaded metis maps.
#'
#' @param dataTbl Palette name to view the palette colors. Eg. metis.colors("pal_Basic")
#' @keywords map, find
#' @return dataframe with modified subRegions, subRegion shapefile and subRegion type
#' @export
#' @examples
#' library(metis)
#' data = data.frame(subRegion=c("FL","ID","MO"),value=c(-2,3,14))
#' metis::metis.mapFind(data)


metis.mapFind <- function(dataTbl) {

  #......................................................
  # Initialize
  #.....................................................

    if(T){
    NULL -> subRegShapeFoundx -> subRegShapeTypeFoundx -> subRegNotInShapeFoundx ->
      dataTblFound -> subRegionShapex -> mapStatesx -> subRegionAlt -> subRegion -> mapFindx -> subRegion1 ->
        subRegNum-> subRegionMetis}

  #......................................................
  # Check columns and map subRegions to metis shape regions
  #.....................................................

  if(T){

    # Check dataTbl to make sure basic columns are available
    addMissing<-function(data){
      if(any(grepl("\\<subregion\\>",names(data),ignore.case = T))){
        data <- data %>% dplyr::rename(!!"subRegion" := (names(data)[grepl("\\<subregion\\>",names(data),ignore.case = T)])[1])}
      if(any(grepl("\\<subregions\\>",names(data),ignore.case = T))){
        data <- data %>% dplyr::rename(!!"subRegion" := (names(data)[grepl("\\<subregions\\>",names(data),ignore.case = T)])[1])}
    return(data)
      }
    dataTbl <- addMissing(dataTbl)

    if(!all(c("subRegion") %in% names(dataTbl))){stop("dataTbl must have subRegion columns.")}

    subRegShapeTblOrig <- unique(dataTbl$subRegion)

    # Map subRegions to metis regions
    dataTbl <- dataTbl %>%
      dplyr::left_join(metis.mappings("subRegionMap"),by="subRegion")%>%
      dplyr::mutate(subRegion=dplyr::case_when(!is.na(subRegionMetis)~subRegionMetis,
                                        TRUE~subRegion))%>%
      dplyr::select(-subRegionMetis)

    subRegShapeTbl <- gsub("-", "_", tolower(unique(dataTbl$subRegion)))

  }

    #.....................................................
    # Load Pre-built map regions
    #......................................................

    if (T) {
      # Renaming subregions in mapStates so that states with USPS can be plotted with states with full names in other countries
      mapStatesx <- metis::mapStates
      mapStatesx@data <- mapStatesx@data %>%
        dplyr::mutate(
          subRegionAlt = as.character(subRegionAlt),
          subRegion = as.character(subRegion),
          subRegion1 = subRegionAlt,
          subRegionAlt = subRegion,
          subRegion = subRegion1,
          subRegion = dplyr::case_when(region != "USA" ~ subRegionAlt,
                                       TRUE ~ subRegion)
        ) %>%
        dplyr::select(-subRegion1)


      subRegStates <-
        tolower(mapStatesx@data$subRegion %>% unique() %>% as.character %>% sort())
      subRegUS49 <-
        tolower(metis::mapUS49@data$subRegion %>% unique() %>% as.character %>% sort())
      subRegUS52 <-
        tolower(metis::mapUS52@data$subRegion %>% unique() %>% as.character %>% sort())
      subRegGCAMReg32 <-
        tolower(metis::mapGCAMReg32@data$subRegion %>% unique() %>% as.character %>%
                  sort())
      subRegCountries <-
        tolower(metis::mapCountries@data$subRegion %>% unique() %>% as.character %>%
                  sort())
      subRegGCAMReg32US52 <-
        tolower(metis::mapGCAMReg32US52@data$subRegion %>% unique() %>% as.character %>%
                  sort())
      subRegCountriesUS52 <-
        tolower(metis::mapCountriesUS52@data$subRegion %>% unique() %>% as.character %>%
                  sort())
      subRegUS49County <-
        tolower(metis::mapUS49County@data$subRegion %>% unique() %>% as.character %>%
                  sort())
      subRegUS52County <-
        tolower(metis::mapUS52County@data$subRegion %>% unique() %>% as.character %>%
                  sort())
      subRegGCAMBasins <-
        tolower(metis::mapGCAMBasins@data$subRegion %>% unique() %>% as.character %>%
                  sort())
      subRegGCAMBasinsUS49 <-
        tolower(metis::mapGCAMBasinsUS49@data$subRegion %>% unique() %>% as.character %>%
                  sort())
      subRegGCAMBasinsUS52 <-
        tolower(metis::mapGCAMBasinsUS52@data$subRegion %>% unique() %>% as.character %>%
                  sort())
      subRegGCAMLand <-
        tolower(metis::mapGCAMLand@data$subRegion %>% unique() %>% as.character %>%
                  sort())
      subRegGCAMLandUS49 <-
        tolower(metis::mapGCAMLandUS49@data$subRegion %>% unique() %>% as.character %>%
                  sort())
      subRegGCAMLandUS52 <-
        tolower(metis::mapGCAMLandUS52@data$subRegion %>% unique() %>% as.character %>%
                  sort())
      subRegUS49HUC2 <-
        tolower(metis::mapUS49HUC2@data$subRegion %>% unique() %>% as.character %>%
                  sort())
      subRegUS52HUC2 <-
        tolower(metis::mapUS52HUC2@data$subRegion %>% unique() %>% as.character %>%
                  sort())
      subRegUS49HUC4 <-
        tolower(metis::mapUS49HUC4@data$subRegion %>% unique() %>% as.character %>%
                  sort())
      subRegUS52HUC4 <-
        tolower(metis::mapUS52HUC4@data$subRegion %>% unique() %>% as.character %>%
                  sort())
      # Alt Names
      subRegStatesAlt <-
        tolower(mapStatesx@data$subRegionAlt %>% unique() %>% as.character %>% sort())
      subRegUS49Alt <-
        tolower(metis::mapUS49@data$subRegionAlt %>% unique() %>% as.character %>%
                  sort())
      subRegUS49Alt
      subRegUS52Alt <-
        tolower(metis::mapUS52@data$subRegionAlt %>% unique() %>% as.character %>%
                  sort())
      subRegUS52Alt
     subRegGCAMReg32Alt <-
        tolower(metis::mapGCAMReg32@data$subRegionAlt %>% unique() %>% as.character %>%
                  sort())
      subRegCountriesAlt <-
        tolower(metis::mapCountries@data$subRegionAlt %>% unique() %>% as.character %>%
                  sort())
      subRegGCAMReg32US52Alt <-
        tolower(
          metis::mapGCAMReg32US52@data$subRegionAlt %>% unique() %>% as.character %>%
            sort()
        )
      subRegCountriesUS52Alt <-
        tolower(
          metis::mapCountriesUS52@data$subRegionAlt %>% unique() %>% as.character %>%
            sort()
        )
      subRegUS49CountyAlt <-
        tolower(metis::mapUS49County@data$subRegionAlt %>% unique() %>% as.character %>%
                  sort())
      subRegUS52CountyAlt <-
        tolower(metis::mapUS52County@data$subRegionAlt %>% unique() %>% as.character %>%
                  sort())
     subRegGCAMBasinsAlt <-
        tolower(metis::mapGCAMBasins@data$subRegionAlt %>% unique() %>% as.character %>%
                  sort())
      subRegGCAMBasinsUS49Alt <-
        tolower(
          metis::mapGCAMBasinsUS49@data$subRegionAlt %>% unique() %>% as.character %>%
            sort()
        )
      subRegGCAMBasinsUS52Alt <-
        tolower(
          metis::mapGCAMBasinsUS52@data$subRegionAlt %>% unique() %>% as.character %>%
            sort()
        )
      subRegGCAMLandAlt <-
        tolower(metis::mapGCAMLand@data$subRegionAlt %>% unique() %>% as.character %>%
                  sort())
      subRegGCAMLandUS49Alt <-
        tolower(
          metis::mapGCAMLandUS49@data$subRegionAlt %>% unique() %>% as.character %>%
            sort()
        )
      subRegGCAMLandUS52Alt <-
        tolower(
          metis::mapGCAMLandUS52@data$subRegionAlt %>% unique() %>% as.character %>%
            sort()
        )
      subRegUS49HUC2Alt <-
        tolower(metis::mapUS49HUC2@data$subRegionAlt %>% unique() %>% as.character %>%
                  sort())
      subRegUS52HUC2Alt <-
        tolower(metis::mapUS52HUC2@data$subRegionAlt %>% unique() %>% as.character %>%
                  sort())
      subRegUS49HUC4Alt <-
        tolower(metis::mapUS49HUC4@data$subRegionAlt %>% unique() %>% as.character %>%
                  sort())
      subRegUS52HUC4Alt <-
        tolower(metis::mapUS52HUC4@data$subRegionAlt %>% unique() %>% as.character %>%
                  sort())
      }

    #.....................................................
    # Assign map which has the most subRegions associated with it.
    #.....................................................

    if (T) {
      mapRegList <- list(
        "subRegUS49" = subRegUS49,
        "subRegUS52" = subRegUS52,
        "subRegGCAMReg32" = subRegGCAMReg32,
        "subRegCountries" = subRegCountries,
        "subRegGCAMReg32US52" = subRegGCAMReg32US52,
        "subRegCountriesUS52" = subRegCountriesUS52,
        "subRegStates" = subRegStates,
        "subRegUS49County" = subRegUS49County,
        "subRegUS52County" = subRegUS52County,
        "subRegGCAMBasins" = subRegGCAMBasins,
        "subRegGCAMBasinsUS49" = subRegGCAMBasinsUS49,
        "subRegGCAMBasinsUS52" = subRegGCAMBasinsUS52,
        "subRegGCAMLand" = subRegGCAMLand,
        "subRegGCAMLandUS49" = subRegGCAMLandUS49,
        "subRegGCAMLandUS52" = subRegGCAMLandUS52,
        "subRegUS49HUC2" = subRegUS49HUC2,
        "subRegUS52HUC2" = subRegUS52HUC2,
        "subRegUS49HUC4" = subRegUS49HUC4,
        "subRegUS52HUC4" = subRegUS52HUC4,
        "subRegUS49Alt" = subRegUS49Alt,
        "subRegUS52Alt" = subRegUS52Alt,
        "subRegGCAMReg32Alt" = subRegGCAMReg32Alt,
        "subRegCountriesAlt" = subRegCountriesAlt,
        "subRegGCAMReg32US52Alt" = subRegGCAMReg32US52Alt,
        "subRegCountriesUS52Alt" = subRegCountriesUS52Alt,
        "subRegStatesAlt" = subRegStatesAlt,
        "subRegUS49CountyAlt" = subRegUS49CountyAlt,
        "subRegUS52CountyAlt" = subRegUS52CountyAlt,
        "subRegGCAMBasinsAlt" = subRegGCAMBasinsAlt,
        "subRegGCAMBasinsUS49Alt" = subRegGCAMBasinsUS49Alt,
        "subRegGCAMBasinsUS52Alt" = subRegGCAMBasinsUS52Alt,
        "subRegGCAMLandAlt" = subRegGCAMLandAlt,
        "subRegGCAMLandUS49Alt" = subRegGCAMLandUS49Alt,
        "subRegGCAMLandUS52Alt" = subRegGCAMLandUS52Alt,
        "subRegUS49HUC2Alt" = subRegUS49HUC2Alt,
        "subRegUS52HUC2Alt" = subRegUS52HUC2Alt,
        "subRegUS49HUC4Alt" = subRegUS49HUC4Alt,
        "subRegUS52HUC4Alt" = subRegUS52HUC4Alt
      )
    }

    #.....................................................
    # Find how many regions in the datatbl belong to different maps
    #.....................................................

    if (T) {

      mapReg <- data.frame()

      for (i in 1:length(mapRegList)) {
        subRegNum_i <-
          length(subRegShapeTbl[subRegShapeTbl %in% mapRegList[[i]]])
        dfx <-
          data.frame(map = names(mapRegList)[i], subRegNum = subRegNum_i)
        mapReg <- mapReg %>% dplyr::bind_rows(dfx)
      }

      mapReg <-
        mapReg %>% dplyr::arrange(-subRegNum) %>% dplyr::filter(subRegNum > 0)

      mapReg
    }

    #.....................................................
    # Use pre ranked maps
    #.....................................................

    if (T) {
      mapRanked <- tibble::tribble(
        ~	map	, ~ rank,
        "subRegUS49",1,
        "subRegUS52",2,
        "subRegGCAMReg32",12,
        "subRegCountries",11,
        "subRegGCAMReg32US52",15.5,
        "subRegCountriesUS52",15,
        "subRegStates",9,
        "subRegUS49County",5,
        "subRegUS52County",6,
        "subRegGCAMBasins",19,
        "subRegGCAMBasinsUS49",17,
        "subRegGCAMBasinsUS52",18,
        "subRegGCAMLand",25,
        "subRegGCAMLandUS49",23,
        "subRegGCAMLandUS52",24,
        "subRegUS49HUC2",29,
        "subRegUS52HUC2",30,
        "subRegUS49HUC4",33,
        "subRegUS52HUC4",34,
        "subRegUS49Alt",3,
        "subRegUS52Alt",4,
        "subRegGCAMReg32Alt",14,
        "subRegCountriesAlt",13,
        "subRegGCAMReg32US52Alt",16.5,
        "subRegCountriesUS52Alt",16,
        "subRegStatesAlt",10,
        "subRegUS49CountyAlt",7,
        "subRegUS52CountyAlt",8,
        "subRegGCAMBasinsAlt",22,
        "subRegGCAMBasinsUS49Alt",20,
        "subRegGCAMBasinsUS52Alt",21,
        "subRegGCAMLandAlt",28,
        "subRegGCAMLandUS49Alt",26,
        "subRegGCAMLandUS52Alt",27,
        "subRegUS49HUC2Alt",31,
        "subRegUS52HUC2Alt",32,
        "subRegUS49HUC4Alt",35,
        "subRegUS52HUC4Alt",36,
      )

      mapRanked %>% dplyr::arrange(rank)
    }

    #.....................................................
    # Choose maps with highest number of regions and if more than one then attach rank and choose highest rnank(lowest rank number)
    #.....................................................

    if (T) {
      mapMax <- mapReg %>%
        dplyr::filter(subRegNum == max(subRegNum)) %>%
        dplyr::left_join(mapRanked, by = "map")
      mapMax

      if (nrow(mapMax) > 1) {
        print("More than one pre-loaded map contain the subRegions in the data provided.")
        print("Choosing map based on pre-set map ranking:")
        print(mapMax %>% dplyr::arrange(rank))
        print("To choose a different map, please assign it in subRegShape directly.")
      }

      subRegChosen <- (mapMax %>%
                         dplyr::filter(rank == min(rank)))$map
      subRegChosen
    }

    #.....................................................
    # Assign map
    #.....................................................

    if (T) {
      if (subRegChosen == "subRegUS49") {
        subRegShapeFoundx <-
          metis::mapUS49
        subRegShapeTypeFoundx <-
          unique(subRegShapeFoundx@data$subRegionType)

        subRegNotInShapeFoundx = subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion %>%
                                                                                   unique())]
      }
      if (subRegChosen == "subRegUS52") {
        subRegShapeFoundx <-
          metis::mapUS52
        subRegShapeTypeFoundx <-
          unique(subRegShapeFoundx@data$subRegionType)

        subRegNotInShapeFoundx = subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion %>%
                                                                                   unique())]
      }
      if (subRegChosen == "subRegGCAMReg32") {
        subRegShapeFoundx <-
          metis::mapGCAMReg32
        subRegShapeTypeFoundx <-
          unique(subRegShapeFoundx@data$subRegionType)

        subRegNotInShapeFoundx = subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion %>%
                                                                                   unique())]
      }
      if (subRegChosen == "subRegCountries") {
        subRegShapeFoundx <-
          metis::mapCountries
        subRegShapeTypeFoundx <-
          unique(subRegShapeFoundx@data$subRegionType)

        subRegNotInShapeFoundx = subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion %>%
                                                                                   unique())]
      }
      if (subRegChosen == "subRegGCAMReg32US52") {
        subRegShapeFoundx <-
          metis::mapGCAMReg32US52
        subRegShapeTypeFoundx <-
          unique(subRegShapeFoundx@data$subRegionType)

        subRegNotInShapeFoundx = subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion %>%
                                                                                   unique())]
      }

      if (subRegChosen == "subRegCountriesUS52") {
        subRegShapeFoundx <-
          metis::mapCountriesUS52
        subRegShapeTypeFoundx <-
          unique(subRegShapeFoundx@data$subRegionType)

        subRegNotInShapeFoundx = subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion %>%
                                                                                   unique())]
      }
      if (subRegChosen == "subRegStates") {
        subRegShapeFoundx <-
          mapStatesx
        subRegShapeTypeFoundx <-
          unique(subRegShapeFoundx@data$subRegionType)

        subRegNotInShapeFoundx = subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion %>%
                                                                                   unique())]
      }
      if (subRegChosen == "subRegUS49County") {
        subRegShapeFoundx <-
          metis::mapUS49County
        subRegShapeTypeFoundx <-
          unique(subRegShapeFoundx@data$subRegionType)

        subRegNotInShapeFoundx = subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion %>%
                                                                                   unique())]
      }
      if (subRegChosen == "subRegUS52County") {
        subRegShapeFoundx <-
          metis::mapUS52County
        subRegShapeTypeFoundx <-
          unique(subRegShapeFoundx@data$subRegionType)

        subRegNotInShapeFoundx = subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion %>%
                                                                                   unique())]
      }
      if (subRegChosen == "subRegGCAMBasins") {
        subRegShapeFoundx <-
          metis::mapGCAMBasins
        subRegShapeTypeFoundx <-
          unique(subRegShapeFoundx@data$subRegionType)

        subRegNotInShapeFoundx = subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion %>%
                                                                                   unique())]
      }
      if (subRegChosen == "subRegGCAMBasinsUS49") {
        subRegShapeFoundx <-
          metis::mapGCAMBasinsUS49
        subRegShapeTypeFoundx <-
          unique(subRegShapeFoundx@data$subRegionType)

        subRegNotInShapeFoundx = subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion %>%
                                                                                   unique())]
      }
      if (subRegChosen == "subRegGCAMBasinsUS52") {
        subRegShapeFoundx <-
          metis::mapGCAMBasinsUS52
        subRegShapeTypeFoundx <-
          unique(subRegShapeFoundx@data$subRegionType)

        subRegNotInShapeFoundx = subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion %>%
                                                                                   unique())]
      }
      if (subRegChosen == "subRegGCAMLand") {
        subRegShapeFoundx <-
          metis::mapGCAMLand
        subRegShapeTypeFoundx <-
          unique(subRegShapeFoundx@data$subRegionType)

        subRegNotInShapeFoundx = subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion %>%
                                                                                   unique())]
      }
      if (subRegChosen == "subRegGCAMLandUS49") {
        subRegShapeFoundx <-
          metis::mapGCAMLandUS49
        subRegShapeTypeFoundx <-
          unique(subRegShapeFoundx@data$subRegionType)

        subRegNotInShapeFoundx = subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion %>%
                                                                                   unique())]
      }
      if (subRegChosen == "subRegGCAMLandUS52") {
        subRegShapeFoundx <-
          metis::mapGCAMLandUS52
        subRegShapeTypeFoundx <-
          unique(subRegShapeFoundx@data$subRegionType)

        subRegNotInShapeFoundx = subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion %>%
                                                                                   unique())]
      }
      if (subRegChosen == "subRegUS49HUC2") {
        subRegShapeFoundx <-
          metis::mapUS49HUC2
        subRegShapeTypeFoundx <-
          unique(subRegShapeFoundx@data$subRegionType)

        subRegNotInShapeFoundx = subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion %>%
                                                                                   unique())]
      }
      if (subRegChosen == "subRegUS52HUC2") {
        subRegShapeFoundx <-
          metis::mapUS52HUC2
        subRegShapeTypeFoundx <-
          unique(subRegShapeFoundx@data$subRegionType)

        subRegNotInShapeFoundx = subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion %>%
                                                                                   unique())]
      }
      if (subRegChosen == "subRegUS49HUC4") {
        subRegShapeFoundx <-
          metis::mapUS49HUC4
        subRegShapeTypeFoundx <-
          unique(subRegShapeFoundx@data$subRegionType)

        subRegNotInShapeFoundx = subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion %>%
                                                                                   unique())]
      }
      if (subRegChosen == "subRegUS52HUC4") {
        subRegShapeFoundx <-
          metis::mapUS52HUC4
        subRegShapeTypeFoundx <-
          unique(subRegShapeFoundx@data$subRegionType)

        subRegNotInShapeFoundx = subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion %>%
                                                                                   unique())]
      }
      if (subRegChosen == "subRegUS49Alt") {
        subRegShapeFoundx <-
          metis::mapUS49
        subRegShapeFoundx@data<-subRegShapeFoundx@data%>%dplyr::mutate(subRegion=subRegionAlt);
        subRegShapeTypeFoundx <-
          unique(subRegShapeFoundx@data$subRegionType)
        subRegNotInShapeFoundx = subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion %>%
                                                                                   unique())]
      }
      if (subRegChosen == "subRegUS52Alt") {
        subRegShapeFoundx <-
          metis::mapUS52
        subRegShapeFoundx@data<-subRegShapeFoundx@data%>%dplyr::mutate(subRegion=subRegionAlt);
        subRegShapeTypeFoundx <-
          unique(subRegShapeFoundx@data$subRegionType)

        subRegNotInShapeFoundx = subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion %>%
                                                                                   unique())]
      }
      if (subRegChosen == "subRegGCAMReg32Alt") {
        subRegShapeFoundx <-
          metis::mapGCAMReg32
        subRegShapeFoundx@data<-subRegShapeFoundx@data%>%dplyr::mutate(subRegion=subRegionAlt);
        subRegShapeTypeFoundx <-
          unique(subRegShapeFoundx@data$subRegionType)

        subRegNotInShapeFoundx = subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion %>%
                                                                                   unique())]
      }
      if (subRegChosen == "subRegCountriesAlt") {
        subRegShapeFoundx <-
          metis::mapCountries
        subRegShapeFoundx@data<-subRegShapeFoundx@data%>%dplyr::mutate(subRegion=subRegionAlt);
        subRegShapeTypeFoundx <-
          unique(subRegShapeFoundx@data$subRegionType)

        subRegNotInShapeFoundx = subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion %>%
                                                                                   unique())]
      }
      if (subRegChosen == "subRegGCAMReg32US52Alt") {
        subRegShapeFoundx <-
          metis::mapGCAMReg32US52
        subRegShapeFoundx@data<-subRegShapeFoundx@data%>%dplyr::mutate(subRegionAlt=subRegion);
        subRegShapeTypeFoundx <-
          unique(subRegShapeFoundx@data$subRegionType)

        subRegNotInShapeFoundx = subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion %>%
                                                                                   unique())]
      }

      if (subRegChosen == "subRegCountriesUS52Alt") {
        subRegShapeFoundx <-
          metis::mapCountriesUS52
        subRegShapeFoundx@data<-subRegShapeFoundx@data%>%dplyr::mutate(subRegionAlt=subRegion);
        subRegShapeTypeFoundx <-
          unique(subRegShapeFoundx@data$subRegionType)

        subRegNotInShapeFoundx = subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion %>%
                                                                                   unique())]
      }
      if (subRegChosen == "subRegStatesAlt") {
        subRegShapeFoundx <-
          mapStatesx
        subRegShapeFoundx@data<-subRegShapeFoundx@data%>%dplyr::mutate(subRegion=subRegionAlt);
        subRegShapeTypeFoundx <-
          unique(subRegShapeFoundx@data$subRegionType)

        subRegNotInShapeFoundx = subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion %>%
                                                                                   unique())]
      }
      if (subRegChosen == "subRegUS49CountyAlt") {
        subRegShapeFoundx <-
          metis::mapUS49County
        subRegShapeFoundx@data<-subRegShapeFoundx@data%>%dplyr::mutate(subRegion=subRegionAlt);
        subRegShapeTypeFoundx <-
          unique(subRegShapeFoundx@data$subRegionType)

        subRegNotInShapeFoundx = subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion %>%
                                                                                   unique())]
      }
      if (subRegChosen == "subRegUS52CountyAlt") {
        subRegShapeFoundx <-
          metis::mapUS52County
        subRegShapeFoundx@data<-subRegShapeFoundx@data%>%dplyr::mutate(subRegion=subRegionAlt);
        subRegShapeTypeFoundx <-
          unique(subRegShapeFoundx@data$subRegionType)

        subRegNotInShapeFoundx = subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion %>%
                                                                                   unique())]
      }
      if (subRegChosen == "subRegGCAMBasinsAlt") {
        subRegShapeFoundx <-
          metis::mapGCAMBasins
        subRegShapeFoundx@data<-subRegShapeFoundx@data%>%dplyr::mutate(subRegion=subRegionAlt);
        subRegShapeTypeFoundx <-
          unique(subRegShapeFoundx@data$subRegionType)

        subRegNotInShapeFoundx = subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion %>%
                                                                                   unique())]
      }
      if (subRegChosen == "subRegGCAMBasinsUS49Alt") {
        subRegShapeFoundx <-
          metis::mapGCAMBasinsUS49
        subRegShapeFoundx@data<-subRegShapeFoundx@data%>%dplyr::mutate(subRegion=subRegionAlt);
        subRegShapeTypeFoundx <-
          unique(subRegShapeFoundx@data$subRegionType)

        subRegNotInShapeFoundx = subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion %>%
                                                                                   unique())]
      }
      if (subRegChosen == "subRegGCAMBasinsUS52Alt") {
        subRegShapeFoundx <-
          metis::mapGCAMBasinsUS52
        subRegShapeFoundx@data<-subRegShapeFoundx@data%>%dplyr::mutate(subRegion=subRegionAlt);
        subRegShapeTypeFoundx <-
          unique(subRegShapeFoundx@data$subRegionType)

        subRegNotInShapeFoundx = subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion %>%
                                                                                   unique())]
      }
      if (subRegChosen == "subRegGCAMLandAlt") {
        subRegShapeFoundx <-
          metis::mapGCAMLand
        subRegShapeFoundx@data<-subRegShapeFoundx@data%>%dplyr::mutate(subRegion=subRegionAlt);
        subRegShapeTypeFoundx <-
          unique(subRegShapeFoundx@data$subRegionType)

        subRegNotInShapeFoundx = subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion %>%
                                                                                   unique())]
      }
      if (subRegChosen == "subRegGCAMLandUS49Alt") {
        subRegShapeFoundx <-
          metis::mapGCAMLandUS49
        subRegShapeFoundx@data<-subRegShapeFoundx@data%>%dplyr::mutate(subRegion=subRegionAlt);
        subRegShapeTypeFoundx <-
          unique(subRegShapeFoundx@data$subRegionType)

        subRegNotInShapeFoundx = subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion %>%
                                                                                   unique())]
      }
      if (subRegChosen == "subRegGCAMLandUS52Alt") {
        subRegShapeFoundx <-
          metis::mapGCAMLandUS52
        subRegShapeFoundx@data<-subRegShapeFoundx@data%>%dplyr::mutate(subRegion=subRegionAlt);
        subRegShapeTypeFoundx <-
          unique(subRegShapeFoundx@data$subRegionType)

        subRegNotInShapeFoundx = subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion %>%
                                                                                   unique())]
      }
      if (subRegChosen == "subRegUS49HUC2Alt") {
        subRegShapeFoundx <-
          metis::mapUS49HUC2
        subRegShapeFoundx@data<-subRegShapeFoundx@data%>%dplyr::mutate(subRegion=subRegionAlt);
        subRegShapeTypeFoundx <-
          unique(subRegShapeFoundx@data$subRegionType)

        subRegNotInShapeFoundx = subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion %>%
                                                                                   unique())]
      }
      if (subRegChosen == "subRegUS52HUC2Alt") {
        subRegShapeFoundx <-
          metis::mapUS52HUC2
        subRegShapeFoundx@data<-subRegShapeFoundx@data%>%dplyr::mutate(subRegion=subRegionAlt);
        subRegShapeTypeFoundx <-
          unique(subRegShapeFoundx@data$subRegionType)

        subRegNotInShapeFoundx = subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion %>%
                                                                                   unique())]
      }
      if (subRegChosen == "subRegUS49HUC4Alt") {
        subRegShapeFoundx <-
          metis::mapUS49HUC4
        subRegShapeFoundx@data<-subRegShapeFoundx@data%>%dplyr::mutate(subRegion=subRegionAlt);
        subRegShapeTypeFoundx <-
          unique(subRegShapeFoundx@data$subRegionType)

        subRegNotInShapeFoundx = subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion %>%
                                                                                   unique())]
      }
      if (subRegChosen == "subRegUS52HUC4Alt") {
        subRegShapeFoundx <-
          metis::mapUS52HUC4
        subRegShapeFoundx@data<-subRegShapeFoundx@data%>%dplyr::mutate(subRegion=subRegionAlt);
        subRegShapeTypeFoundx <-
          unique(subRegShapeFoundx@data$subRegionType)

        subRegNotInShapeFoundx = subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion %>%
                                                                                   unique())]
      }


     print(paste("Using map: ", subRegShapeTypeFoundx, sep = ""))

    }

    #.....................................................
    # Check if no subregions in pre-loaded maps
    #.....................................................

    if(T){
    if (!is.null(subRegNotInShapeFoundx)) {
      if (length(subRegNotInShapeFoundx) > 0) {
        print(paste(
          "subRegions in data not present in shapefile are: ",
          paste(subRegNotInShapeFoundx, collapse = ", "),
          sep = ""
        ))
      }
    }
    }

    #.....................................................
    # Re-assign subRegion names based on data in maps
    #.....................................................

    if(T){
    if (!is.null(subRegShapeFoundx) & nrow(dataTbl) > 0) {

      subRegShapeFoundx <- subRegShapeFoundx[!is.na(subRegShapeFoundx@data$subRegion),]
      subRegShapeFoundx@data <- subRegShapeFoundx@data %>%
        dplyr::filter(!is.na(subRegion))
      subRegShapeFoundx@data <- droplevels(subRegShapeFoundx@data)

      subRegx <-
        data.frame(
          subRegion = tolower(
            subRegShapeFoundx@data$subRegion %>% unique() %>% as.character()
          ),
          subRegionShapex = subRegShapeFoundx@data$subRegion %>%
            unique() %>% as.character()
        )

      subRegx
      dataTblFound <- dataTbl %>%
        dplyr::mutate(subRegion = gsub("-","_",tolower(as.character(subRegion))),
                      subRegType = subRegShapeTypeFoundx) %>%
        dplyr::left_join(subRegx, by = "subRegion") %>%
        dplyr::mutate(subRegion = subRegionShapex) %>%
        dplyr::select(-subRegionShapex)
      dataTblFound
    }
    }


    #.....................................................
    # If no data found
    #.....................................................

    if(T){
    if (is.null(dataTblFound)) {
      print(
        paste(
          "None of the subregions in the data provided: ",
          paste(dataTbl$subRegion %>% unique(), collapse = ", "),
          " are available in any of the metis shapefiles available. Please provide a shapefile with at least one of the subRegions from the data.",
          sep = ""
        )
      )
    }
    }

    #.....................................................
    # Return Map
    #.....................................................

    mapFindx <- list(
      dataTblFound = dataTblFound,
      subRegShapeFound = subRegShapeFoundx,
      subRegShapeTypeFound = subRegShapeTypeFoundx,
      subRegNotInShapeFound = subRegNotInShapeFoundx
    )

    invisible(mapFindx)

  } # CLose map finding function
