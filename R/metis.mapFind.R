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


metis.mapFind <- function(dataTbl){ # Telescope out of finest regions to see which shapefile to use.

  #------------------------------------------------------
  # Initialize
  #-----------------------------------------------------

  NULL -> subRegShapeFoundx -> subRegShapeTypeFoundx -> subRegNotInShapeFoundx ->
    dataTblFound -> subRegionShapex -> mapStatesx -> subRegionAlt -> subRegion -> mapFindx -> subRegion1

  #-----------------------------------------------------
  # Load Pre-built map regions
  #------------------------------------------------------

  if(T){

    # Renaming subregions in mapStates so that states with USPS can be plotted with states with full names in other countries
    mapStatesx <- metis::mapStates
    mapStatesx@data <- mapStatesx@data %>%
      dplyr::mutate(
        subRegionAlt = as.character(subRegionAlt),
        subRegion = as.character(subRegion),
        subRegion1=subRegionAlt,subRegionAlt=subRegion, subRegion=subRegion1,
        subRegion=dplyr::case_when(region !="USA"~ subRegionAlt,
                                   TRUE~subRegion))%>%
      dplyr::select(-subRegion1);

    subRegUS49 <- tolower(metis::mapUS49@data$subRegion%>%unique()%>%as.character%>%sort())
    subRegUS52 <- tolower(metis::mapUS52@data$subRegion%>%unique()%>%as.character%>%sort())
    subRegUS52notUS49 <- tolower(metis::mapUS52@data$subRegion[!metis::mapUS52@data$subRegion %in% metis::mapUS49@data$subRegion]%>%unique()%>%as.character%>%sort())
    subRegGCAMReg32 <- tolower(metis::mapGCAMReg32@data$subRegion%>%unique()%>%as.character%>%sort())
    subRegCountries <- tolower(metis::mapCountries@data$subRegion%>%unique()%>%as.character%>%sort())
    subRegGCAMReg32US52 <- tolower(metis::mapGCAMReg32US52@data$subRegion%>%unique()%>%as.character%>%sort())
    subRegGCAM32notcountries <- tolower(metis::mapGCAMReg32@data$subRegion[!metis::mapGCAMReg32@data$subRegion %in% metis::mapCountries@data$subRegion]%>%unique()%>%as.character%>%sort())
    subRegCountriesnotGAMReg32 <- tolower(metis::mapCountries@data$subRegion[!metis::mapCountries@data$subRegion %in% metis::mapGCAMReg32@data$subRegion]%>%unique()%>%as.character%>%sort())
    subRegStatesnotUS52 <- tolower(mapStatesx@data$subRegion[!mapStatesx@data$subRegion %in% metis::mapUS52@data$subRegion]%>%unique()%>%as.character%>%sort())
    subRegUS49County <- tolower(metis::mapUS49County@data$subRegion%>%unique()%>%as.character%>%sort())
    subRegUS52CountynotUS49 <- tolower(metis::mapUS52County@data$subRegion[!metis::mapUS52County@data$subRegion %in% metis::mapUS49County@data$subRegion]%>%unique()%>%as.character%>%sort())
    subRegGCAMBasins <- tolower(metis::mapGCAMBasins@data$subRegion%>%unique()%>%as.character%>%sort())
    subRegGCAMBasinsUS49 <- tolower(metis::mapGCAMBasinsUS49@data$subRegion%>%unique()%>%as.character%>%sort())
    subRegGCAMBasinsUS52 <- tolower(metis::mapGCAMBasinsUS52@data$subRegion%>%unique()%>%as.character%>%sort())
    subRegGCAMBasinsUS52not49 <- tolower(metis::mapGCAMBasinsUS52@data$subRegion[!metis::mapGCAMBasinsUS52@data$subRegion %in% metis::mapGCAMBasinsUS49@data$subRegion]%>%unique()%>%as.character%>%sort())
    subRegGCAMBasinsnotUS52 <- tolower(metis::mapGCAMBasins@data$subRegion[!metis::mapGCAMBasins@data$subRegion%in% metis::mapGCAMBasinsUS52@data$subRegion]%>%unique()%>%as.character%>%sort())
    subRegGCAMLand <- tolower(metis::mapGCAMLand@data$subRegion%>%unique()%>%as.character%>%sort())
    subRegGCAMLandUS49 <- tolower(metis::mapGCAMLandUS49@data$subRegion%>%unique()%>%as.character%>%sort())
    subRegGCAMLandUS52 <- tolower(metis::mapGCAMLandUS52@data$subRegion%>%unique()%>%as.character%>%sort())
    subRegGCAMLandUS52not49 <- tolower(metis::mapGCAMLandUS52@data$subRegion[!metis::mapGCAMLandUS52@data$subRegion %in% metis::mapGCAMLandUS49@data$subRegion]%>%unique()%>%as.character%>%sort())
    subRegGCAMLandnotUS52 <- tolower(metis::mapGCAMLand@data$subRegion[!metis::mapGCAMLand@data$subRegion %in% metis::mapGCAMLandUS52@data$subRegion]%>%unique()%>%as.character%>%sort())
    subRegUS49HUC2 <- tolower(metis::mapUS49HUC2@data$subRegion%>%unique()%>%as.character%>%sort())
    subRegUS52HUC2notUS49 <- tolower(metis::mapUS52HUC2@data$subRegion[!metis::mapUS52HUC2@data$subRegion %in% metis::mapUS49HUC2@data$subRegion]%>%unique()%>%as.character%>%sort())
    subRegUS49HUC4 <- tolower(metis::mapUS49HUC4@data$subRegion%>%unique()%>%as.character%>%sort())
    subRegUS52HUC4notUS49 <- tolower(metis::mapUS52HUC4@data$subRegion[!metis::mapUS52HUC4@data$subRegion %in% metis::mapUS49HUC4@data$subRegion]%>%unique()%>%as.character%>%sort())
    # Alt Names
    subRegUS49Alt <- tolower(metis::mapUS49@data$subRegionAlt%>%unique()%>%as.character%>%sort());subRegUS49Alt
    subRegUS52Alt <- tolower(metis::mapUS52@data$subRegionAlt%>%unique()%>%as.character%>%sort());subRegUS52Alt
    subRegUS52notUS49Alt <- tolower(metis::mapUS52@data$subRegionAlt[!metis::mapUS52@data$subRegionAlt %in% metis::mapUS49@data$subRegionAlt]%>%unique()%>%as.character%>%sort())
    subRegGCAMReg32Alt <- tolower(metis::mapGCAMReg32@data$subRegionAlt%>%unique()%>%as.character%>%sort())
    subRegCountriesAlt <- tolower(metis::mapCountries@data$subRegionAlt%>%unique()%>%as.character%>%sort())
    subRegGCAMReg32US52Alt <- tolower(metis::mapGCAMReg32US52@data$subRegionAlt%>%unique()%>%as.character%>%sort())
    subRegGCAM32notcountriesAlt <- tolower(metis::mapGCAMReg32@data$subRegionAlt[!metis::mapGCAMReg32@data$subRegionAlt %in% metis::mapCountries@data$subRegionAlt]%>%unique()%>%as.character%>%sort())
    subRegCountriesnotGAMReg32Alt <- tolower(metis::mapCountries@data$subRegionAlt[!metis::mapCountries@data$subRegionAlt %in% metis::mapGCAMReg32@data$subRegionAlt]%>%unique()%>%as.character%>%sort())
    subRegStatesnotUS52Alt <- tolower(mapStatesx@data$subRegionAlt[!mapStatesx@data$subRegionAlt %in% metis::mapUS52@data$subRegionAlt]%>%unique()%>%as.character%>%sort())
    subRegUS49CountyAlt <- tolower(metis::mapUS49County@data$subRegionAlt%>%unique()%>%as.character%>%sort())
    subRegUS52CountynotUS49Alt <- tolower(metis::mapUS52County@data$subRegionAlt[!metis::mapUS52County@data$subRegionAlt %in% metis::mapUS49County@data$subRegionAlt]%>%unique()%>%as.character%>%sort())
    subRegGCAMBasinsAlt <- tolower(metis::mapGCAMBasins@data$subRegionAlt%>%unique()%>%as.character%>%sort())
    subRegGCAMBasinsUS49Alt <- tolower(metis::mapGCAMBasinsUS49@data$subRegionAlt%>%unique()%>%as.character%>%sort())
    subRegGCAMBasinsUS52Alt <- tolower(metis::mapGCAMBasinsUS52@data$subRegionAlt%>%unique()%>%as.character%>%sort())
    subRegGCAMBasinsUS52not49Alt <- tolower(metis::mapGCAMBasinsUS52@data$subRegionAlt[!metis::mapGCAMBasinsUS52@data$subRegionAlt %in% metis::mapGCAMBasinsUS49@data$subRegionAlt]%>%unique()%>%as.character%>%sort())
    subRegGCAMBasinsnotUS52Alt <- tolower(metis::mapGCAMBasins@data$subRegionAlt[!metis::mapGCAMBasins@data$subRegionAlt%in% metis::mapGCAMBasinsUS52@data$subRegionAlt]%>%unique()%>%as.character%>%sort())
    subRegGCAMLandAlt <- tolower(metis::mapGCAMLand@data$subRegionAlt%>%unique()%>%as.character%>%sort())
    subRegGCAMLandUS49Alt <- tolower(metis::mapGCAMLandUS49@data$subRegionAlt%>%unique()%>%as.character%>%sort())
    subRegGCAMLandUS52Alt <- tolower(metis::mapGCAMLandUS52@data$subRegionAlt%>%unique()%>%as.character%>%sort())
    subRegGCAMLandUS52not49Alt <- tolower(metis::mapGCAMLandUS52@data$subRegionAlt[!metis::mapGCAMLandUS52@data$subRegionAlt %in% metis::mapGCAMLandUS49@data$subRegionAlt]%>%unique()%>%as.character%>%sort())
    subRegGCAMLandnotUS52Alt <- tolower(metis::mapGCAMLand@data$subRegionAlt[!metis::mapGCAMLand@data$subRegionAlt %in% metis::mapGCAMLandUS52@data$subRegionAlt]%>%unique()%>%as.character%>%sort())
    subRegUS49HUC2Alt <- tolower(metis::mapUS49HUC2@data$subRegionAlt%>%unique()%>%as.character%>%sort())
    subRegUS52HUC2notUS49Alt <- tolower(metis::mapUS52HUC2@data$subRegionAlt[!metis::mapUS52HUC2@data$subRegionAlt %in% metis::mapUS49HUC2@data$subRegionAlt]%>%unique()%>%as.character%>%sort())
    subRegUS49HUC4Alt <- tolower(metis::mapUS49HUC4@data$subRegionAlt%>%unique()%>%as.character%>%sort())
    subRegUS52HUC4notUS49Alt <- tolower(metis::mapUS52HUC4@data$subRegionAlt[!metis::mapUS52HUC4@data$subRegionAlt %in% metis::mapUS49HUC4@data$subRegionAlt]%>%unique()%>%as.character%>%sort())
  }

  subRegShapeTblOrig <- unique(dataTbl$subRegion)
  subRegShapeTbl <- gsub("-","_",tolower(unique(dataTbl$subRegion)))

  #-----------------------------------------------------
  # Find Map
  #------------------------------------------------------

  # States, COuntries, GCAM Regions
  if(any(subRegShapeTbl %in% subRegUS49) & !any(subRegShapeTbl %in% subRegUS52notUS49) &
     !any(subRegShapeTbl %in% subRegGCAMReg32) & !any(subRegShapeTbl %in% subRegCountriesnotGAMReg32) &
     !any(subRegShapeTbl %in% subRegStatesnotUS52)){subRegShapeFoundx <- metis::mapUS49; subRegShapeTypeFoundx<-unique(subRegShapeFoundx@data$subRegionType);
     subRegNotInShapeFoundx=subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion%>%unique())]} else {

       if(any(subRegShapeTbl %in% subRegUS52) &
          !any(subRegShapeTbl %in% subRegGCAMReg32) & !any(subRegShapeTbl %in% subRegCountriesnotGAMReg32) &
          !any(subRegShapeTbl %in% subRegStatesnotUS52)){subRegShapeFoundx <- metis::mapUS52; subRegShapeTypeFoundx<-unique(subRegShapeFoundx@data$subRegionType);
          subRegNotInShapeFoundx=subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion%>%unique())]} else {

            if(any(subRegShapeTbl %in% subRegUS52) &
               any(subRegShapeTbl %in% subRegCountries) & !any(subRegShapeTbl %in% subRegGCAM32notcountries) &
               !any(subRegShapeTbl %in% subRegStatesnotUS52)){subRegShapeFoundx <- metis::mapCountriesUS52; subRegShapeTypeFoundx<-unique(subRegShapeFoundx@data$subRegionType);
               subRegNotInShapeFoundx=subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion%>%unique())]} else {

                 if(any(subRegShapeTbl %in% subRegUS52) & any(subRegShapeTbl %in% subRegGCAM32notcountries) &
                    !any(subRegShapeTbl %in% subRegStatesnotUS52)){subRegShapeFoundx <- metis::mapGCAMReg32US52; subRegShapeTypeFoundx<-unique(subRegShapeFoundx@data$subRegionType);
                    subRegNotInShapeFoundx=subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion%>%unique())]} else {

                      if(any(subRegShapeTbl %in% subRegUS52) &
                         any(subRegShapeTbl %in% subRegStatesnotUS52)){subRegShapeFoundx <- mapStatesx; subRegShapeTypeFoundx<-unique(subRegShapeFoundx@data$subRegionType);
                         subRegNotInShapeFoundx=subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion%>%unique())]} else {

                           if(!any(subRegShapeTbl %in% subRegUS52) &
                              any(subRegShapeTbl %in% subRegCountries) & !any(subRegShapeTbl %in% subRegGCAM32notcountries)
                           ){subRegShapeFoundx <- metis::mapCountries; subRegShapeTypeFoundx<-unique(subRegShapeFoundx@data$subRegionType);
                           subRegNotInShapeFoundx=subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion%>%unique())]} else {

                             if(!any(subRegShapeTbl %in% subRegUS52) &
                                any(subRegShapeTbl %in% subRegCountries) & any(subRegShapeTbl %in% subRegGCAM32notcountries)
                             ){subRegShapeFoundx <- metis::mapGCAMReg32; subRegShapeTypeFoundx<-unique(subRegShapeFoundx@data$subRegionType);
                             subRegNotInShapeFoundx=subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion%>%unique())]} else {


                               # Alternate States, COuntries, GCAM Regions
                               if(any(subRegShapeTbl %in% subRegUS49Alt) & !any(subRegShapeTbl %in% subRegUS52notUS49Alt) &
                                  !any(subRegShapeTbl %in% subRegGCAMReg32Alt) & !any(subRegShapeTbl %in% subRegCountriesnotGAMReg32Alt) &
                                  !any(subRegShapeTbl %in% subRegStatesnotUS52Alt)){subRegShapeFoundx <- metis::mapUS49; subRegShapeFoundx@data<-subRegShapeFoundx@data%>%dplyr::mutate(subRegion=subRegionAlt); subRegShapeTypeFoundx<-unique(subRegShapeFoundx@data$subRegionType);
                                  subRegNotInShapeFoundx=subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion%>%unique())]} else {

                                    if(any(subRegShapeTbl %in% subRegUS52Alt) &
                                       !any(subRegShapeTbl %in% subRegGCAMReg32Alt) & !any(subRegShapeTbl %in% subRegCountriesnotGAMReg32Alt) &
                                       !any(subRegShapeTbl %in% subRegStatesnotUS52Alt)){subRegShapeFoundx <- metis::mapUS52; subRegShapeFoundx@data<-subRegShapeFoundx@data%>%dplyr::mutate(subRegion=subRegionAlt); subRegShapeTypeFoundx<-unique(subRegShapeFoundx@data$subRegionType);
                                       subRegNotInShapeFoundx=subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion%>%unique())]} else {

                                         if(any(subRegShapeTbl %in% subRegUS52Alt) &
                                            any(subRegShapeTbl %in% subRegCountriesAlt) & !any(subRegShapeTbl %in% subRegGCAM32notcountries) &
                                            !any(subRegShapeTbl %in% subRegStatesnotUS52Alt)){subRegShapeFoundx <- metis::mapCountriesUS52; subRegShapeFoundx@data<-subRegShapeFoundx@data%>%dplyr::mutate(subRegion=subRegionAlt); subRegShapeTypeFoundx<-unique(subRegShapeFoundx@data$subRegionType);
                                            subRegNotInShapeFoundx=subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion%>%unique())]} else {

                                              if(any(subRegShapeTbl %in% subRegUS52Alt) & any(subRegShapeTbl %in% subRegGCAM32notcountriesAlt) &
                                                 !any(subRegShapeTbl %in% subRegStatesnotUS52Alt)){subRegShapeFoundx <- metis::mapGCAMReg32US52; subRegShapeFoundx@data<-subRegShapeFoundx@data%>%dplyr::mutate(subRegion=subRegionAlt); subRegShapeTypeFoundx<-unique(subRegShapeFoundx@data$subRegionType);
                                                 subRegNotInShapeFoundx=subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion%>%unique())]} else {


                                                   if(any(subRegShapeTbl %in% subRegUS52Alt) & any(subRegShapeTbl %in% subRegStatesnotUS52)){subRegShapeFoundx <- mapStatesx; subRegShapeFoundx@data<-subRegShapeFoundx@data%>%dplyr::mutate(subRegion=subRegionAlt); subRegShapeTypeFoundx<-unique(subRegShapeFoundx@data$subRegionType);
                                                   subRegNotInShapeFoundx=subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion%>%unique())]} else {

                                                     if(!any(subRegShapeTbl %in% subRegUS52Alt) &
                                                        any(subRegShapeTbl %in% subRegCountriesAlt) & !any(subRegShapeTbl %in% subRegGCAM32notcountriesAlt)
                                                     ){subRegShapeFoundx <- metis::mapCountries; subRegShapeFoundx@data<-subRegShapeFoundx@data%>%dplyr::mutate(subRegion=subRegionAlt); subRegShapeTypeFoundx<-unique(subRegShapeFoundx@data$subRegionType);
                                                     subRegNotInShapeFoundx=subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion%>%unique())]} else {

                                                       if(!any(subRegShapeTbl %in% subRegUS52Alt) &
                                                          any(subRegShapeTbl %in% subRegGCAMReg32Alt) & any(subRegShapeTbl %in% subRegGCAM32notcountriesAlt)
                                                       ){subRegShapeFoundx <- metis::mapGCAMReg32; subRegShapeFoundx@data<-subRegShapeFoundx@data%>%dplyr::mutate(subRegion=subRegionAlt); subRegShapeTypeFoundx<-unique(subRegShapeFoundx@data$subRegionType);
                                                       subRegNotInShapeFoundx=subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion%>%unique())]} else {


                                                         # Counties
                                                         if(any(subRegShapeTbl %in% subRegUS49County) & !any(subRegShapeTbl %in% subRegUS52CountynotUS49)){
                                                           subRegShapeFoundx <- metis::mapUS49County; subRegShapeTypeFoundx<-unique(subRegShapeFoundx@data$subRegionType);
                                                           subRegNotInShapeFoundx=subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion%>%unique())]} else {

                                                             if(any(subRegShapeTbl %in% subRegUS49County) & any(subRegShapeTbl %in% subRegUS52CountynotUS49)){
                                                               subRegShapeFoundx <- metis::mapUS52County; subRegShapeTypeFoundx<-unique(subRegShapeFoundx@data$subRegionType);
                                                               subRegNotInShapeFoundx=subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion%>%unique())]} else {

                                                                 # Alternate Counties
                                                                 if(any(subRegShapeTbl %in% subRegUS49CountyAlt) & !any(subRegShapeTbl %in% subRegUS52CountynotUS49Alt)){
                                                                   subRegShapeFoundx <- metis::mapUS49County; subRegShapeFoundx@data<-subRegShapeFoundx@data%>%dplyr::mutate(subRegion=subRegionAlt); subRegShapeTypeFoundx<-unique(subRegShapeFoundx@data$subRegionType);
                                                                   subRegNotInShapeFoundx=subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion%>%unique())]} else {

                                                                     if(any(subRegShapeTbl %in% subRegUS49CountyAlt) & any(subRegShapeTbl %in% subRegUS52CountynotUS49Alt)){
                                                                       subRegShapeFoundx <- metis::mapUS52County; subRegShapeFoundx@data<-subRegShapeFoundx@data%>%dplyr::mutate(subRegion=subRegionAlt); subRegShapeTypeFoundx<-unique(subRegShapeFoundx@data$subRegionType);
                                                                       subRegNotInShapeFoundx=subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion%>%unique())]} else {


                                                                       } # Close US52Alt counties
                                                                   } # Close US49Alt counties

                                                               } # Close US52 counties
                                                           } # Close US49 counties


                                                       } # Close Countries
                                                     } # Close GCAM32Alt
                                                   } # Close states
                                                 } # Close countriesUS52Alt
                                            } # Close GCAM32AltUS52Alt
                                       } # Close US52Alt
                                  } # Close US49Alt


                             } # Close Countries
                           } # Close GCAM32
                         } # Close states
                    } # Close countriesUS52
               } # Close GCAM32US52
          } # Close US52
     } # Close US49


  if(is.null(subRegShapeTypeFoundx)){
  # Basins
  if(any(subRegShapeTbl %in% subRegGCAMBasinsUS49) & !any(subRegShapeTbl %in% subRegGCAMBasinsUS52not49) &
     !any(subRegShapeTbl %in% subRegGCAMBasinsnotUS52)){
    subRegShapeFoundx <- metis::mapGCAMBasinsUS49; subRegShapeTypeFoundx<-unique(subRegShapeFoundx@data$subRegionType);
    subRegNotInShapeFoundx=subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion%>%unique())]} else {

      if(any(subRegShapeTbl %in% subRegGCAMBasinsUS52) &
         !any(subRegShapeTbl %in% subRegGCAMBasinsnotUS52)){
        subRegShapeFoundx <- metis::mapGCAMBasinsUS52; subRegShapeTypeFoundx<-unique(subRegShapeFoundx@data$subRegionType);
        subRegNotInShapeFoundx=subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion%>%unique())]} else {

          if(any(subRegShapeTbl %in% subRegGCAMBasins)){
            subRegShapeFoundx <- metis::mapGCAMBasins; subRegShapeTypeFoundx<-unique(subRegShapeFoundx@data$subRegionType);
            subRegNotInShapeFoundx=subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion%>%unique())]} else {

              # Alternate Basins
              if(any(subRegShapeTbl %in% subRegGCAMBasinsUS49Alt) & !any(subRegShapeTbl %in% subRegGCAMBasinsUS52not49Alt) &
                 !any(subRegShapeTbl %in% subRegGCAMBasinsnotUS52Alt)){
                subRegShapeFoundx <- metis::mapGCAMBasinsUS49; subRegShapeFoundx@data<-subRegShapeFoundx@data%>%dplyr::mutate(subRegion=subRegionAlt); subRegShapeTypeFoundx<-unique(subRegShapeFoundx@data$subRegionType);
                subRegNotInShapeFoundx=subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion%>%unique())]} else {

                  if(any(subRegShapeTbl %in% subRegGCAMBasinsUS52Alt) &
                     !any(subRegShapeTbl %in% subRegGCAMBasinsnotUS52Alt)){
                    subRegShapeFoundx <- metis::mapGCAMBasinsUS52; subRegShapeFoundx@data<-subRegShapeFoundx@data%>%dplyr::mutate(subRegion=subRegionAlt); subRegShapeTypeFoundx<-unique(subRegShapeFoundx@data$subRegionType);
                    subRegNotInShapeFoundx=subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion%>%unique())]} else {

                      if(any(subRegShapeTbl %in% subRegGCAMBasins)){
                        subRegShapeFoundx <- metis::mapGCAMBasins; subRegShapeFoundx@data<-subRegShapeFoundx@data%>%dplyr::mutate(subRegion=subRegionAlt); subRegShapeTypeFoundx<-unique(subRegShapeFoundx@data$subRegionType);
                        subRegNotInShapeFoundx=subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion%>%unique())]} else {


                        } # Close Basins
                    } # Close US52Alt Basins
                } # Close US49Alt Basins


            } # Close Basins
        } # Close US52 Basins
    } # Close US49 Basins
} # Is null


  if(is.null(subRegShapeTypeFoundx)){
    # Land
    if(any(subRegShapeTbl %in% subRegGCAMLandUS49) & !any(subRegShapeTbl %in% subRegGCAMLandUS52not49) &
       !any(subRegShapeTbl %in% subRegGCAMLandnotUS52)){
      subRegShapeFoundx <- metis::mapGCAMLandUS49; subRegShapeTypeFoundx<-unique(subRegShapeFoundx@data$subRegionType);
      subRegNotInShapeFoundx=subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion%>%unique())]} else {

        if(any(subRegShapeTbl %in% subRegGCAMLandUS52) &
           !any(subRegShapeTbl %in% subRegGCAMLandnotUS52)){
          subRegShapeFoundx <- metis::mapGCAMLandUS52; subRegShapeTypeFoundx<-unique(subRegShapeFoundx@data$subRegionType);
          subRegNotInShapeFoundx=subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion%>%unique())]} else {

            if(any(subRegShapeTbl %in% subRegGCAMLand)){
              subRegShapeFoundx <- metis::mapGCAMLand; subRegShapeTypeFoundx<-unique(subRegShapeFoundx@data$subRegionType);
              subRegNotInShapeFoundx=subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion%>%unique())]} else {


                # Alternate Land
                if(any(subRegShapeTbl %in% subRegGCAMLandUS49Alt) & !any(subRegShapeTbl %in% subRegGCAMLandUS52not49Alt) &
                   !any(subRegShapeTbl %in% subRegGCAMLandnotUS52Alt)){
                  subRegShapeFoundx <- metis::mapGCAMLandUS49; subRegShapeFoundx@data<-subRegShapeFoundx@data%>%dplyr::mutate(subRegion=subRegionAlt); subRegShapeTypeFoundx<-unique(subRegShapeFoundx@data$subRegionType);
                  subRegNotInShapeFoundx=subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion%>%unique())]} else {

                    if(any(subRegShapeTbl %in% subRegGCAMLandUS52Alt) &
                       !any(subRegShapeTbl %in% subRegGCAMLandnotUS52Alt)){
                      subRegShapeFoundx <- metis::mapGCAMLandUS52; subRegShapeFoundx@data<-subRegShapeFoundx@data%>%dplyr::mutate(subRegion=subRegionAlt); subRegShapeTypeFoundx<-unique(subRegShapeFoundx@data$subRegionType);
                      subRegNotInShapeFoundx=subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion%>%unique())]} else {

                        if(any(subRegShapeTbl %in% subRegGCAMLand)){
                          subRegShapeFoundx <- metis::mapGCAMLand; subRegShapeFoundx@data<-subRegShapeFoundx@data%>%dplyr::mutate(subRegion=subRegionAlt); subRegShapeTypeFoundx<-unique(subRegShapeFoundx@data$subRegionType);
                          subRegNotInShapeFoundx=subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion%>%unique())]} else {


                          } # Close Land
                      } # Close US52Alt Land
                  } # Close US49Alt Land
              } # Close Land
          } # Close US52 Land
      } # Close US49 Land
  } # If null




  if(is.null(subRegShapeTypeFoundx)){
  # HUC
  if(any(subRegShapeTbl %in% subRegUS49HUC4) & !any(subRegShapeTbl %in% subRegUS52HUC4notUS49)){
    subRegShapeFoundx <- metis::mapUS49HUC4; subRegShapeTypeFoundx<-unique(subRegShapeFoundx@data$subRegionType);
    subRegNotInShapeFoundx=subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion%>%unique())]} else {

      if(any(subRegShapeTbl %in% subRegUS49HUC4) & any(subRegShapeTbl %in% subRegUS52HUC4notUS49)){
        subRegShapeFoundx <- metis::mapUS52HUC4; subRegShapeTypeFoundx<-unique(subRegShapeFoundx@data$subRegionType);
        subRegNotInShapeFoundx=subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion%>%unique())]} else {

          if(any(subRegShapeTbl %in% subRegUS49HUC2) & !any(subRegShapeTbl %in% subRegUS52HUC2notUS49)){
            subRegShapeFoundx <- metis::mapUS49HUC2; subRegShapeTypeFoundx<-unique(subRegShapeFoundx@data$subRegionType);
            subRegNotInShapeFoundx=subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion%>%unique())]} else {

              if(any(subRegShapeTbl %in% subRegUS49HUC2) & any(subRegShapeTbl %in% subRegUS52HUC2notUS49)){
                subRegShapeFoundx <- metis::mapUS52HUC2; subRegShapeTypeFoundx<-unique(subRegShapeFoundx@data$subRegionType);
                subRegNotInShapeFoundx=subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion%>%unique())]} else {

                  # Alternate HUC
                  if(any(subRegShapeTbl %in% subRegUS49HUC4Alt) & !any(subRegShapeTbl %in% subRegUS52HUC4notUS49Alt)){
                    subRegShapeFoundx <- metis::mapUS49HUC4; subRegShapeFoundx@data<-subRegShapeFoundx@data%>%dplyr::mutate(subRegion=subRegionAlt); subRegShapeTypeFoundx<-unique(subRegShapeFoundx@data$subRegionType);
                    subRegNotInShapeFoundx=subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion%>%unique())]} else {

                      if(any(subRegShapeTbl %in% subRegUS49HUC4Alt) & any(subRegShapeTbl %in% subRegUS52HUC4notUS49Alt)){
                        subRegShapeFoundx <- metis::mapUS52HUC4; subRegShapeFoundx@data<-subRegShapeFoundx@data%>%dplyr::mutate(subRegion=subRegionAlt); subRegShapeTypeFoundx<-unique(subRegShapeFoundx@data$subRegionType);
                        subRegNotInShapeFoundx=subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion%>%unique())]} else {

                          if(any(subRegShapeTbl %in% subRegUS49HUC2Alt) & !any(subRegShapeTbl %in% subRegUS52HUC2notUS49Alt)){
                            subRegShapeFoundx <- metis::mapUS49HUC2; subRegShapeFoundx@data<-subRegShapeFoundx@data%>%dplyr::mutate(subRegion=subRegionAlt); subRegShapeTypeFoundx<-unique(subRegShapeFoundx@data$subRegionType);
                            subRegNotInShapeFoundx=subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion%>%unique())]} else {

                              if(any(subRegShapeTbl %in% subRegUS49HUC2Alt) & any(subRegShapeTbl %in% subRegUS52HUC2notUS49Alt)){
                                subRegShapeFoundx <- metis::mapUS52HUC2; subRegShapeFoundx@data<-subRegShapeFoundx@data%>%dplyr::mutate(subRegion=subRegionAlt); subRegShapeTypeFoundx<-unique(subRegShapeFoundx@data$subRegionType);
                                subRegNotInShapeFoundx=subRegShapeTblOrig[!subRegShapeTbl %in% tolower(subRegShapeFoundx@data$subRegion%>%unique())]} else {

                                } # Close US52Alt HUC2
                            } # Close US49Alt HUC2
                        } # Close US52Alt HUC4
                    } # Close US49Alt HUC4

                } # Close US52 HUC2
            } # Close US49 HUC2
        } # Close US52 HUC4
    } # Close US49 HUC4
  } # Is null


  if(!is.null(subRegNotInShapeFoundx)){
    if(length(subRegNotInShapeFoundx)>0){
      print(paste("subRegions in data not present in shapefile are: ", paste(subRegNotInShapeFoundx,collapse = ", "),sep=""))
    }
  }


  if(!is.null(subRegShapeFoundx) & nrow(dataTbl)>0){
    subRegx <- data.frame(subRegion=tolower(subRegShapeFoundx@data$subRegion%>%unique()%>%as.character()),
                          subRegionShapex=subRegShapeFoundx@data$subRegion%>%unique()%>%as.character()); subRegx
    dataTblFound <- dataTbl %>%
      dplyr::mutate(
        subRegion = tolower(as.character(subRegion)),
        subRegType=subRegShapeTypeFoundx)%>%
      dplyr::left_join(subRegx,by="subRegion")%>%
      dplyr::mutate(subRegion=subRegionShapex)%>%
      dplyr::select(-subRegionShapex); dataTblFound
  }

  if(is.null(dataTblFound)){
    print(paste("None of the subregions in the data provided: ", paste(dataTbl$subRegion%>%unique(),collapse=", "),
                " are available in any of the metis shapefiles available. Please provide a shapefile with at least one of the subRegions from the data.",sep=""))
  }

  mapFindx <-list(dataTblFound=dataTblFound,
              subRegShapeFound=subRegShapeFoundx,
              subRegShapeTypeFound=subRegShapeTypeFoundx,
              subRegNotInShapeFound=subRegNotInShapeFoundx)

  invisible(mapFindx)

} # CLose map finding function
