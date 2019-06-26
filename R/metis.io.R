#' metis.io
#'
#' This function prepares gridded data for use with domestic metis modules.
#' @param dirOutputs Default =paste(getwd(),"/outputs",sep=""),
#' @param ioTable0 Initial ioTable. Must have columns: supplySubSector,total,export and cap. Each supply sector should also have imports. Default = NULL,
#' @param useIntensity Boolean to use given intensity or not. Default is set to 0.
#' @param A0 Intensity matrix. Default Null.
#' @param nameAppend Modified intensity matrix. Default =NULL,
#' @param figWidth Default = 9,
#' @param figHeight Default = 7,
#' @param sankeyLabelAbsPlots Default = 0
#' @return A table with data by polygon ID for each shapefile provided
#' @keywords gcam, gcam database, query
#' @export


metis.io<-function(ioTable0 = NULL,
                   useIntensity = 0,
                   A0 = NULL,
                   dirOutputs=paste(getwd(),"/outputs",sep=""),
                   nameAppend="",
                   figWidth = 9,
                   figHeight = 7,
                   sankeyLabelAbsPlots=1
                        ){

  # ioTable0 = NULL
  # useIntensity = 0
  # A0 = NULL
  # dirOutputs=paste(getwd(),"/outputs",sep="")
  # nameAppend=""
  # figWidth = 9
  # figHeight = 7

#----------------
# Defaults:
#----------------

  # The default values should be updated based on local assumptions or GCAM outputs for each region and year
  # The deaults below are applied to all regions and time periods until they are replaced.


  # Default intensity
  A_default0 =tibble::tribble( # Initial total demand
    ~supplySubSector, ~supplySector, ~water_sw, ~water_import, ~elec_all, ~elec_import, ~irri_all, ~irri_import,
    "water_sw",        "water",     0,         0 ,0.404     ,0     ,0         ,0,
    "water_import",     "water",     0,         0 ,0         ,0     ,0         ,0,
    "elec_all",         "electricity", 0.27778,   0 ,0         ,0     ,0         ,0,
    "elec_import",      "electricity", 0,         0 ,0         ,0     ,0         ,0,
    "irri_all",          "Agriculture",      0,         0 ,0.0202    ,0     ,0         ,0,
    "irri_import",       "Agriculture",      0,         0 ,0         ,0     ,0         ,0
  )


  # Copy data for each year, subregion and scenario
  # When this default is based on GCAm or other data it may need to be adjusted for changes by region/scenario/year

  A_default0 %>% as.data.frame()

  supplySubSector_default0 = tibble::tribble( # Initial total demand
    ~supplySubSector, ~supplySector,      ~percentDistribution,
    # Water
    "water_all",          "Water",            100,
    "W_SW_Upstream",           "Water",            100/4,
    "water_gw",           "Water",            100/4,
    "water_desal",        "Water",            100/4,
    "water_upstream",     "Water",            100/4,
    "water_import",       "Water",            0,
    # Electricity
    "elec_all",            "Electricity",      100,
    "elec_wind",           "Electricity",      100/9,
    "elec_solar",          "Electricity",      100/9,
    "elec_oil",            "Electricity",      100/9,
    "elec_hydro",          "Electricity",      100/9,
    "elec_geothermal",     "Electricity",      100/9,
    "elec_gas",            "Electricity",      100/9,
    "elec_coal",           "Electricity",      100/9,
    "elec_biomass",        "Electricity",      100/9,
    "elec_cogen",          "Electricity",      100/9,
    "elec_import",         "Electricity",      0,
    # Irrigation
    "irri_all",            "Agriculture",       100,
    "Ag_specialty",          "Agriculture",       100/8,
    "irri_sugarCrop",      "Agriculture",       100/8,
    "irri_root_Tuber",     "Agriculture",       100/8,
    "irri_rice",           "Agriculture",       100/8,
    "irri_otherGrain",     "Agriculture",       100/8,
    "irri_oilCrop",        "Agriculture",       100/8,
    "irri_miscCrop",       "Agriculture",       100/8,
    "irri_corn",           "Agriculture",       100/8,
    "irri_import",         "Agriculture",       0
  );

  supplySubSector_default0 %>% as.data.frame()


#------------------
# Create Folders if needed
#------------------
if (!dir.exists(dirOutputs)){
  dir.create(dirOutputs)}
if (!dir.exists(paste(dirOutputs, "/IO", sep = ""))){
  dir.create(paste(dirOutputs, "/IO", sep = ""))}


#----------------
# Initialize variables by setting to NULL
#----------------

NULL -> ioTable -> A -> year -> Year -> YEAR -> supplySubSector -> supplySector -> scenario ->
  cap -> capNew -> nexusTotal -> otherTotal -> sectorFrom -> sectorTo -> subRegion ->
  total -> totalTemp -> value -> x -> . -> region -> missingSupplySubSectorNumber ->
  percentDistribution->percentDistribution_Adjusted->percentDistribution_Remainder->region->
  remainder->select->sumType->supplySectorSum->supplySubSectorNumber->
  totalsAll->totalsSubSectorOther->valueCalculated->valueOrig -> fname ->
    supplySubSector_supplySector->key -> otherAdjustedSupply -> param -> sectorToAgg


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
  if(!"units"%in%names(data)){data<-data%>%dplyr::mutate(units="units")}
  if(!"param"%in%names(data)){data<-data%>%dplyr::mutate(param="param")}
  return(data)
}

addedColumns <- names(addMissing(data.frame())); addedColumns

nonFlowCols <- c("total","cap","capNew","capOrig","surplus"); nonFlowCols
nonFlowColsAll <- c("supplySubSector","supplySector",nonFlowCols,addedColumns); nonFlowColsAll


# ------------------
# Check Columns
# ------------------

if(!is.null(ioTable0)){ioTable <- addMissing(ioTable0)}
if(!is.null(A0)){ A <- addMissing(A0)}


if(!is.null(ioTable)){

  for (column_i in c("supplySubSector","supplySector")){
    if(!any(grepl(column_i,colnames(ioTable)))){
      stop(print(paste("Column names in ioTable0: ",paste(colnames(ioTable),collapse=", ")," must include ",column_i,".",sep="")))}}

  # Check that each supplySector has a total (supplySector_all category. If not add it and sum the supplySubSector values
  for (sectorTotal_i in paste(unique(ioTable$supplySector),"_all",sep="")){
    if(!sectorTotal_i %in% unique(ioTable$supplySubSector)){
      colsToEdit <- names(ioTable)[!names(ioTable) %in% c(addedColumns,"supplySector","supplySubSector")]
      # Create empty supplySector_all column and sum across supplySubSectors to get total value
      sectorTotalTemp <- ioTable %>%
        dplyr::filter(supplySector == gsub("_all","",sectorTotal_i)) %>%
        dplyr::group_by(supplySector,scenario,region,subRegion,x) %>%
        dplyr::mutate_at(dplyr::vars(colsToEdit),sum, na.rm=T) %>%
        dplyr::select(-supplySubSector) %>%
        dplyr::distinct() %>%
        dplyr::mutate(supplySubSector=sectorTotal_i) %>%
        dplyr::ungroup();sectorTotalTemp

      # Bind to main iotable
      ioTable <- ioTable %>%
        dplyr::bind_rows(sectorTotalTemp) %>%
        dplyr::distinct(); ioTable
    }

    if(!is.null(A)){
    if(!sectorTotal_i %in% unique(A$supplySubSector)){
      colsToEdit <- names(A)[!names(A) %in% c(addedColumns,"supplySector","supplySubSector")]

      # Create empty supplySector_all column and sum across supplySubSectors to get total value
      sectorTotalTemp <- A %>%
        dplyr::filter(supplySector == gsub("_all","",sectorTotal_i)) %>%
        dplyr::group_by(supplySector,scenario,region,subRegion,x) %>%
        dplyr::mutate_at(dplyr::vars(colsToEdit),function(x) 0) %>%
        dplyr::mutate(supplySubSector=sectorTotal_i,
                      !!sectorTotal_i := 0) %>%
        dplyr::distinct() %>%
        dplyr::ungroup();sectorTotalTemp

      # Bind to main iotable
      A <- A %>%
        dplyr::bind_rows(sectorTotalTemp) %>%
        dplyr::mutate_at(dplyr::vars(!!sectorTotal_i),dplyr::funs(replace(., is.na(.), 0)))%>%
        dplyr::distinct(); A
    }}
  }

for (column_i in c("export","adjustedDemand","cap","capNew","capOrig","surplus")){
  if(!any(grepl(column_i,colnames(ioTable)))){
    print(paste(column_i,"added as new column in ioTable0.",sep=""))
    ioTable <- ioTable %>%
      dplyr::mutate(!!column_i := 0)}}
  if(!any(grepl("total",colnames(ioTable),ignore.case = T))){
    ioTable <- ioTable %>%
    dplyr::mutate(total=rowSums(dplyr::select(.,-c(names(ioTable)[names(ioTable)!="total"])),na.rm=T))
    } else {names(ioTable)[names(ioTable) %in% c("total","Total","TOTAL")]<-"total"}
  sectors <- ioTable %>% dplyr::select(supplySubSector) %>% unique()
  subRegions <- unique(ioTable$subRegion)
  years <- unique(ioTable$x)
  scenarios <- unique(ioTable$scenario)
  regions <- unique(ioTable$region)
}

if(!is.null(A)){

  for (column_i in c("supplySubSector","supplySector")){
    if(!any(grepl(column_i,colnames(A)))){
      stop(print(paste("Column names in A0: ",paste(colnames(A),collapse=", ")," must include ",column_i,".",sep="")))}}

  # Make sure supplSubSector column names are the same as the row names as this needs to be a square matrix
  if(any(!unique(A$supplySubSector) %in% names(A))){
    stop(print(paste("All suppluySubSectors in A0 must be represented as columns for a square intensity matrix.",sep="")))}

}


if(!is.null(A) & !is.null(ioTable)) {
  if(!any(unique(ioTable$supplySubSector) %in% unique(A$supplySubSector))){
    stop(print(paste("supplySubSector names in ioTable0 are different from supplySubSector names in A0.",sep="")))}
}



if(!is.null(supplySubSector_default0)){

  supplySubSector_default0 <- supplySubSector_default0 %>% addMissing()

  for (column_i in c("supplySubSector","supplySector")){
    if(!any(grepl(column_i,colnames(supplySubSector_default0)))){
      stop(print(paste("Column names in supplySubSector_default0: ",paste(colnames(ioTable),collapse=", ")," must include ",column_i,".",sep="")))}}

  # Check that each supplySector has a total (supplySector_all category. If not add it and sum the supplySubSector values

  for (sectorTotal_i in paste(unique(ioTable0$supplySector),"_all",sep="")){

    if(!sectorTotal_i %in% unique(supplySubSector_default0$supplySubSector)){

      colsToEdit <- names(supplySubSector_default0)[!names(supplySubSector_default0) %in% c(addedColumns,"supplySector","supplySubSector")]

      # Create empty supplySector_all column and sum across supplySubSectors to get total value
      sectorTotalTemp <- supplySubSector_default0 %>%
        dplyr::filter(supplySector == gsub("_all","",sectorTotal_i)) %>%
        dplyr::group_by(supplySector,scenario,region,subRegion,x) %>%
        dplyr::mutate_at(dplyr::vars(colsToEdit),function(x) x=100) %>%
        dplyr::mutate(supplySubSector=sectorTotal_i) %>%
        dplyr::distinct() %>%
        dplyr::ungroup();sectorTotalTemp

      # Bind to main iotable
      supplySubSector_default0 <- supplySubSector_default0 %>%
        dplyr::bind_rows(sectorTotalTemp) %>%
        dplyr::distinct();
    }

  }

  supplySubSector_default0 <- supplySubSector_default0 %>% dplyr::select(-addedColumns)

  supplySubSector_default0 %>% as.data.frame()
}

#------------------------------------------
# Update Defaults
#---------------------------------------

# subsetDefaults by provided supplySubSectors
supplySubSector_default_subSet <- supplySubSector_default0 %>%
  dplyr::mutate(supplySubSector_supplySector = paste(supplySubSector,supplySector,sep="_")) %>%
  dplyr::filter(supplySubSector %in% unique((ioTable %>%
                                              dplyr::mutate(supplySubSector_supplySector =
                                                              paste(supplySubSector,supplySector,sep="_")))$supplySubSector_supplySector
                                            )); supplySubSector_default_subSet

supplySubSector_default_ioTable0 <- ioTable %>%
  dplyr::select(supplySubSector,supplySector) %>%
  dplyr::distinct() %>%
  dplyr::filter(!supplySubSector %in% unique(supplySubSector_default_subSet$supplySubSector)) %>%
  dplyr::mutate(percentDistribution=NA); supplySubSector_default_ioTable0

supplySubSector_default_completeNA <- supplySubSector_default_ioTable0 %>%
  dplyr::bind_rows(supplySubSector_default_subSet) %>%
  dplyr::arrange(supplySector) %>%
  dplyr::select(-supplySubSector_supplySector); supplySubSector_default_completeNA

# Distribute Remaining categories evenly

supplySubSector_default <- supplySubSector_default_completeNA %>%
  dplyr::filter(!grepl("all",supplySubSector)) %>%
  dplyr::group_by(supplySector) %>%
  dplyr::mutate(missingSupplySubSectorNumber = dplyr::case_when(is.na(percentDistribution)~1,
                                                         TRUE~0),
                supplySubSectorNumber = sum(missingSupplySubSectorNumber),
                percentDistribution_Remainder = dplyr::case_when (is.na(percentDistribution) ~ 0,
                                                           TRUE ~ 100-percentDistribution),
                percentDistribution_Remainder = dplyr::case_when (sum(percentDistribution_Remainder) == 0 ~ 100/supplySubSectorNumber,
                                                           TRUE ~ percentDistribution_Remainder),
                percentDistribution = dplyr::case_when (is.na(percentDistribution) ~ sum(percentDistribution_Remainder)/supplySubSectorNumber,
                                                          TRUE ~ percentDistribution)) %>%
  dplyr::ungroup() %>%
  dplyr::select(-missingSupplySubSectorNumber,-supplySubSectorNumber,-percentDistribution_Remainder) %>%
  dplyr::bind_rows(supplySubSector_default_completeNA %>%
                    dplyr::filter(grepl("all",supplySubSector))) %>%
  dplyr::mutate(percentDistribution = dplyr::case_when(is.na(percentDistribution) & grepl("all",supplySubSector) ~ 100,
                                                TRUE~percentDistribution)); supplySubSector_default %>% as.data.frame()



if(!is.null(A_default0)){
  if(!any(grepl("supplySubSector",colnames(A_default0)))){
    stop(print(paste("Column names in A_default0: ",paste(colnames(A_default0),collapse=", ")," must include 'supplySubSector'.",sep="")))}

  if(!any(grepl("supplySector",colnames(A_default0)))){
    stop(print(paste("Column names in A_default0: ",paste(colnames(A_default0),collapse=", ")," must include 'supplySector'.",sep="")))}

  # Make sure supplySubSector column names are the same as the row names as this needs to be a square matrix
  if(any(!unique(A_default0$supplySubSector) %in% names(A_default0))){
    stop(print(paste("All suppluySubSectors in A_default0 must be represented as columns for a square intensity matrix.",sep="")))}

}


if(!is.null(supplySubSector_default)){

  for (column_i in c("supplySubSector","supplySector","percentDistribution")){
    if(!any(grepl(column_i,colnames(supplySubSector_default)))){
      stop(print(paste("Column names in supplySubSector_default0: ",paste(colnames(supplySubSector_default),collapse=", ")," must include ",column_i,".",sep="")))}}
}


# Check that supplySubSector_default and A_default0 have all supplySubSectors listed in A0 and ioTable
if(!is.null(A) & !is.null(A_default0)) {
  if(!any(unique(A$supplySubSector) %in% unique(A_default0$supplySubSector))){
    stop(print(paste("supplySubSector names in A0 are different from supplySubSector names in A_default0.",sep="")))}
}

if(!is.null(ioTable) & !is.null(supplySubSector_default)) {
  if(!any(unique(ioTable$supplySubSector) %in% unique(supplySubSector_default$supplySubSector))){
    stop(print(paste("supplySubSector names in ioTable0 are different from supplySubSector names in supplySubSector_default.",sep="")))}
}


#---------------------------------------------
# Repeat default values for all scenarios and regions not allocated
#------------------------------------

supplySubSector_default_temp <- data.frame()

if(!is.null(supplySubSector_default)){

for (scenario_i in scenarios[!scenarios %in% unique(supplySubSector_default$scenario)]){
  for (region_i in regions[!regions %in% unique(supplySubSector_default$region)]){
    for (subRegion_i in subRegions[!subRegions %in% unique(supplySubSector_default$subRegions)]){
      for (year_i in years[!years %in% unique(supplySubSector_default$years)]){

        supplySubSector_default_temp <- supplySubSector_default_temp  %>%
          dplyr::bind_rows(supplySubSector_default %>%
                      dplyr::filter(supplySubSector %in% unique(ioTable$supplySubSector)) %>%
                      dplyr::mutate(scenario = scenario_i,
                                    region = region_i,
                                    subRegion = subRegion_i,
                                    x = year_i))

      }}}}}

A_temp <- data.frame()

if(!is.null(A_default0)){

for (scenario_i in scenarios[!scenarios %in% unique(A_default0$scenario)]){
  for (region_i in regions[!regions %in% unique(A_default0$region)]){
    for (subRegion_i in subRegions[!subRegions %in% unique(A_default0$subRegions)]){
      for (year_i in years[!years %in% unique(A_default0$years)]){

        A_temp <- A_temp %>%
          dplyr::bind_rows(A_default0 %>%
                      dplyr::mutate(scenario = scenario_i,
                                    region = region_i,
                                    subRegion = subRegion_i,
                                    x = year_i))
      }}}}}

supplySubSector_default <- supplySubSector_default_temp %>% addMissing()
A_default <- A_temp %>% addMissing()

# View Inputs
supplySubSector_default %>% as.data.frame() %>% dplyr::arrange(supplySector);
A_default %>% as.data.frame()
ioTable %>% as.data.frame()
A %>% as.data.frame()


#------------------------------------
# Subset Default Data
#------------------------------------

# Make sure Sector percentages add up to 100 in Default
supplySubSector_default_sum <- supplySubSector_default %>%
  dplyr::filter(!grepl("_all",supplySubSector)) %>%
  dplyr::group_by(supplySector,scenario,region,subRegion,x) %>%
  dplyr::summarize(supplySectorSum = sum(percentDistribution)) %>%
  dplyr::ungroup(); supplySubSector_default_sum %>% as.data.frame()

if(any(supplySubSector_default_sum$supplySectorSum != 100)){
  print(supplySubSector_default_sum %>% dplyr::filter(supplySectorSum!=100))
  stop(print(paste("Not all supplySubSectors_default percentDistribution by supplySector add to 100.",sep="")))}


#------------------------------------
# For each year, suregion and scenario
#------------------------------------

tibble::tibble()->A_Output -> L_Output-> ioTbl_Output

for(region_i in regions){
for(scenario_i in scenarios){
  for(subRegion_i in subRegions){
    for(year_i in years){



      print(paste("Solving for scenario: ", scenario_i, ", year:", year_i, ", region:", region_i,  " and sub-region:", subRegion_i," ...",sep=""))


      # Subset Data (D0i,X0i,A0i,Z0i,Cap0i,Import0i,Export0i)
      NULL -> A0i -> ioTable0i -> A_default0i -> supplySubSector_default0i

      if(!is.null(A)){A0i<-A %>% dplyr::filter(region==region_i,scenario==scenario_i,subRegion==subRegion_i,x==year_i)}
      if(!is.null(ioTable)){ioTable0i<-ioTable %>% dplyr::filter(region==region_i,scenario==scenario_i,subRegion==subRegion_i,x==year_i)}
      if(!is.null(A_default)){A_default0i<-A_default %>% dplyr::filter(region==region_i,scenario==scenario_i,subRegion==subRegion_i,x==year_i)}
      if(!is.null(supplySubSector_default)){supplySubSector_default0i<-supplySubSector_default %>% dplyr::filter(region==region_i,scenario==scenario_i,subRegion==subRegion_i,x==year_i)}

      A0i
      ioTable0i %>% as.data.frame()
      A_default0i
      supplySubSector_default0i

      if(nrow(ioTable0i)>0 & nrow(supplySubSector_default0i)>0){
      supplySubSector_default0i <- supplySubSector_default0i %>%
        dplyr::select(-units) %>%
        dplyr::left_join(ioTable0i %>% dplyr::select(supplySector,units) %>% dplyr::distinct()) %>%
        dplyr::mutate(units=dplyr::case_when(is.na(units)~"units",
                                     TRUE~units))}

#-----------------------------------------------------------
# Check to see if subsector totals add up to totals provided.
# If total provided is > subsector totals then add to other supply
# Else increase total to total provided and print message.
#----------------------------------------------------------

      if(!is.null(ioTable0i)){

            # Get totals as supplied
            ioTable0i_totals <- ioTable0i %>%
              dplyr::filter(grepl("_all",supplySubSector)) %>%
              dplyr::mutate(sumType = "totalsAll"); ioTable0i_totals %>% as.data.frame()

            # Calculate total of subSectors Supplied
            colsToEdit <- names(ioTable0i)[!names(ioTable0i) %in% c(addedColumns,"supplySector","supplySubSector")]; colsToEdit

            ioTable0i_subSectorOther <-  ioTable0i %>%
              dplyr::filter(!grepl("_all",supplySubSector)) %>%
              dplyr::group_by(supplySector) %>%
              dplyr::mutate_at(dplyr::vars(colsToEdit),sum,na.rm=TRUE) %>%
              dplyr::select(-supplySubSector) %>%
              dplyr::distinct() %>%
              dplyr::mutate(sumType = "totalsSubSectorOther",
                            supplySubSector = paste(supplySector,"_other",sep="")) %>%
              dplyr::ungroup(); ioTable0i_subSectorOther %>% as.data.frame()


            # Calculate Remainder
            ioTable0i_Remainder <- ioTable0i_totals %>%
              dplyr::bind_rows(ioTable0i_subSectorOther) %>%
              dplyr::select(-supplySubSector)%>%
              tidyr::gather(colsToEdit,key="key",value="value") %>%
              dplyr::mutate(value = dplyr::case_when(is.na(value)~0,TRUE~value)) %>%
              tidyr::spread(sumType,value) %>%
              dplyr::mutate(remainder=totalsAll-totalsSubSectorOther); ioTable0i_Remainder %>% as.data.frame()

            ioTable0i_Remainder1 <- ioTable0i_Remainder %>%
              dplyr::mutate(otherAdjustedSupply = dplyr::case_when(remainder>0 ~ remainder,
                                                     TRUE~0),
                     totalsAll = dplyr::case_when(remainder<0 ~ -remainder,
                                           TRUE~totalsAll)); ioTable0i_Remainder1 %>% as.data.frame() %>% dplyr::arrange(supplySector,key)

            ioTable0i_Remainder2 <- ioTable0i_Remainder1 %>%
              dplyr::select(-totalsSubSectorOther,-remainder,-otherAdjustedSupply) %>%
              tidyr::spread(key,totalsAll) %>%
              dplyr::left_join(ioTable0i_totals %>% dplyr::select(supplySubSector,supplySector)); ioTable0i_Remainder2 %>% as.data.frame()

            ioTable0i_Remainder3 <- ioTable0i_Remainder1 %>%
              dplyr::select(-totalsAll,-remainder,-totalsSubSectorOther) %>%
              tidyr::spread(key,otherAdjustedSupply) %>%
              dplyr::mutate(supplySubSector=paste(supplySector,"_adjusted",sep="")); ioTable0i_Remainder3 %>% as.data.frame()

            # Combine ioTable after removing old totals with remainder 2 and 3

            ioTable0i <- ioTable0i %>%
              dplyr::filter(!grepl("_all",supplySubSector)) %>%
              dplyr::bind_rows(ioTable0i_Remainder2) %>%
              dplyr::bind_rows(ioTable0i_Remainder3); ioTable0i %>% as.data.frame() %>% dplyr::arrange(supplySector)


          }

#-----------------------------------------------------------
# Fill out missing data using default supplySubSector_default0i
#----------------------------------------------------------

# Add in all supplysubSectors and fill out missing values with NAs

ioTable0i_completeNA <- ioTable0i %>%
        tidyr::complete(supplySubSector=unique(c(supplySubSector_default0i$supplySubSector,ioTable0i$supplySubSector))) %>%
        dplyr::mutate(region = region_i,
                      scenario = scenario_i,
                      subRegion = subRegion_i,
                      x = year_i) %>%
        dplyr::select(-supplySector, -units, -param)%>%
        dplyr::distinct() %>%
        dplyr::left_join((ioTable %>%
                            dplyr::select(supplySector, supplySubSector, units, param) %>%
                            # In order to include added supplySubSectors such as Water_adjusted
                            dplyr::bind_rows(ioTable0i %>% dplyr::select(supplySector, supplySubSector, units, param)) %>%
                            dplyr::distinct()),
                         by="supplySubSector") %>%
        dplyr::select(names(ioTable0i));ioTable0i_completeNA %>% as.data.frame()

# Find totals if given by supplySector_all.
# Then find sum of given supplySubSectors and the remainder to distribute
# If cap of a supplySubSector is given as 0 then supply from that sector is 0.
# Calculate percent distribution of remaining NA supplySubSectors from supplySubSector_defaul0i.
# Distribute remainder based on the new distribution.

ioTable0i_completeNA_temp <- tibble::tibble()
# if supplySector_all category not specified in data then create by asumming across supply sectors for each region, scenario, subRegion and year
for(supplySectorTotal_i in paste(unique(ioTable0i_completeNA$supplySector),"_all",sep="")){
if(!supplySectorTotal_i %in% unique(ioTable0i_completeNA$supplySubSector)){

  ioTable0i_completeNA_temp <-  ioTable0i_completeNA_temp %>%
    dplyr::bind_rows(ioTable0i_completeNA %>%
                       dplyr::filter(supplySector == gsub("_all","",supplySectorTotal_i)) %>%
                       dplyr::group_by(supplySector) %>%
                       dplyr::mutate_at(dplyr::vars(colsToEdit),sum,na.rm=TRUE) %>%
                       dplyr::select(-supplySubSector)%>%
                       dplyr::distinct() %>%
                       dplyr::mutate(supplySubSector = paste(supplySector,"_all",sep="")) %>%
                       dplyr::ungroup())
}
}

ioTable0i_completeNA_temp %>% as.data.frame()

ioTable0i_completeNA <- ioTable0i_completeNA %>%
  dplyr::bind_rows(ioTable0i_completeNA_temp)

# Get totals as supplied
ioTable0i_totals <-  ioTable0i_completeNA %>%
  dplyr::filter(grepl("_all",supplySubSector)) %>%
  dplyr::mutate(sumType = "totalsAll"); ioTable0i_totals %>% as.data.frame()

# Calculate total of subSectors Supplied
colsToEdit <- names(ioTable0i_completeNA)[!names(ioTable0i_completeNA) %in% c(addedColumns,"supplySector","supplySubSector")]; colsToEdit

ioTable0i_subSectorOther <-  ioTable0i_completeNA %>%
  dplyr::filter(!grepl("_all",supplySubSector)) %>%
  dplyr::group_by(supplySector) %>%
  dplyr::mutate_at(dplyr::vars(colsToEdit),sum,na.rm=TRUE) %>%
  dplyr::select(-supplySubSector) %>%
  dplyr::distinct() %>%
  dplyr::mutate(sumType = "totalsSubSectorOther",
                   supplySubSector = paste(supplySector,"_other",sep="")) %>%
  dplyr::ungroup(); ioTable0i_subSectorOther %>% as.data.frame()


# Calculate Remainder
ioTable0i_Remainder <- ioTable0i_totals %>%
  dplyr::bind_rows(ioTable0i_subSectorOther) %>%
  dplyr::select(-supplySubSector)%>%
  tidyr::gather(colsToEdit,key="key",value="value") %>%
  dplyr::mutate(value = dplyr::case_when(is.na(value)~0,TRUE~value)) %>%
  tidyr::spread(sumType,value) %>%
  dplyr::mutate(remainder=totalsAll-totalsSubSectorOther); ioTable0i_Remainder %>% as.data.frame()


if(any(unique(ioTable0i_Remainder$remainder)[!is.na(unique(ioTable0i_Remainder$remainder))]<0)){
  stop(print(list("Totals for a supplySector is less than the sum of the indvidual supplySubSectors provided.",
                  ioTable0i_Remainder%>%dplyr::filter(remainder < 0) %>% as.data.frame(),
                  ioTable0i_completeNA %>% tidyr::drop_na() %>% as.data.frame())))
}


# Spread Remainder by supplySubSector
ioTable0i_Remainder_Spread <- ioTable0i_Remainder %>%
  dplyr::select(-totalsAll,-totalsSubSectorOther) %>%
  tidyr::spread(key = "key", value ="remainder") %>%
  dplyr::mutate(supplySubSector = paste(supplySector,"_remainder",sep="")); ioTable0i_Remainder_Spread %>% as.data.frame()

# Bind with Unallocated supplySubSectors
# Left_join the default distribution.
# Recalculate the distribution based on remaining supllySubSectors
# Distribute remainder based on the recalculated distribution
# Need to loop this for each category because may have different NA's

ioTable0i_completeNA_supplySubSectors <-  ioTable0i_completeNA %>%
  #dplyr::filter(!grepl("_all",supplySubSector)) %>%
  dplyr::left_join(supplySubSector_default %>% dplyr::select(-units)); ioTable0i_completeNA_supplySubSectors %>% as.data.frame()

ioTable0i_complete_RedistributeNA <- ioTable0i_completeNA_supplySubSectors %>%
  dplyr::select(-colsToEdit, -percentDistribution)

for(column_i in colsToEdit){

  colsToRemove = colsToEdit[colsToEdit != column_i]; colsToRemove

  ioTable0i_complete_RedistributeNA_temp <- ioTable0i_completeNA_supplySubSectors %>%
    # Assign 0 for NA sectors when cap is specified as 0.
    dplyr::mutate(!!column_i := dplyr::case_when(cap == 0 ~ 0, TRUE ~ !!as.name(column_i))) %>%
    dplyr::filter(is.na(!!as.name(column_i))) %>%
    dplyr::group_by(supplySector) %>%
    dplyr::mutate(supplySubSectorNumber = length(unique(supplySubSector)),
                  percentDistribution = dplyr::case_when(is.na(percentDistribution) ~ 0,
                                                  TRUE ~ percentDistribution),
                  percentDistribution_Adjusted = dplyr::case_when (sum(percentDistribution) == 0 ~ 100/supplySubSectorNumber,
                                                            TRUE ~ percentDistribution*100/sum(percentDistribution))) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(ioTable0i_Remainder_Spread %>%
                       dplyr::select(c(addedColumns,"supplySector",remainder=column_i))) %>%
    dplyr::mutate(!!column_i := percentDistribution_Adjusted*remainder/100) %>%
    dplyr::select(-colsToRemove,-percentDistribution, -supplySubSectorNumber,-percentDistribution_Adjusted,-remainder);  ioTable0i_complete_RedistributeNA_temp %>% as.data.frame()

  ioTable0i_complete_RedistributeNA <- ioTable0i_complete_RedistributeNA %>%
    dplyr::left_join(ioTable0i_complete_RedistributeNA_temp) %>%
    dplyr::filter(!is.na(!!as.name(column_i)))
  }

ioTable0i_complete_RedistributeNA %>% as.data.frame()


# Replace redistributed flows in ioTable0i_completeNA for values where NA for each edited column
ioTable0i_complete_Redistribute <- ioTable0i_completeNA %>%
  dplyr::select(-colsToEdit); ioTable0i_complete_Redistribute %>% as.data.frame()

for(column_i in colsToEdit){

  colsToRemove = colsToEdit[colsToEdit != column_i]; colsToRemove

ioTable0i_complete_Redistribute_temp <- ioTable0i_completeNA %>%
  dplyr::select(-colsToRemove) %>%
  dplyr::filter(!is.na(!!as.name(column_i))) %>%
  # Bind the redistributed supplySubSectors
  dplyr::bind_rows(ioTable0i_complete_RedistributeNA %>%
                     dplyr::select(-colsToRemove) %>%
                     dplyr::filter(!is.na(!!as.name(column_i))))

ioTable0i_complete_Redistribute <- ioTable0i_complete_Redistribute %>%
  dplyr::left_join(ioTable0i_complete_Redistribute_temp)
}

ioTable0i_complete_Redistribute %>% as.data.frame() %>% dplyr::arrange(supplySector)

# Check if re-distributed sums = initial total given.
check_calculated<-ioTable0i_complete_Redistribute %>%
  dplyr::filter(!grepl("_all",supplySubSector)) %>%
  dplyr::select(-supplySubSector) %>%
  dplyr::group_by(supplySector) %>%
  dplyr::summarize_at(dplyr::vars(colsToEdit),sum) %>%
  tidyr::gather(key="key",value="valueCalculated",colsToEdit) %>%
  dplyr::ungroup(); check_calculated %>% as.data.frame()

check_orig<-ioTable0i %>%
  dplyr::filter(grepl("_all",supplySubSector)) %>%
  dplyr::select(-supplySubSector) %>%
  dplyr::group_by(supplySector) %>%
  dplyr::summarize_at(dplyr::vars(colsToEdit),sum) %>%
  tidyr::gather(key="key",value="valueOrig",colsToEdit) %>%
  dplyr::ungroup(); check_orig %>% as.data.frame()

check_Orig_Calc <- check_orig %>%
  dplyr::left_join(check_calculated) %>%
  dplyr::mutate(diff = valueOrig - valueCalculated); check_Orig_Calc %>% as.data.frame()


if(any(abs(check_Orig_Calc$diff[!is.na(check_Orig_Calc$diff)]) >= 1e-10)){
  print(check_Orig_Calc %>% dplyr::filter(!is.na(diff) & diff >= 1e-10 ))
  stop(print("Redistribution of supplySubSectors did not sum to original total values."))
}


#----------------------
# Check and fix Totals
# If provided total is less than calculated total then adjust total to new value
# If provided total is more than calcualted total then create a new category called adjustedDemand equal to the difference.
#-------------------

ioTable_adjustTotal <- ioTable0i_complete_Redistribute %>%
  dplyr::mutate(otherTotal=rowSums(ioTable0i_complete_Redistribute %>% dplyr::select(-nonFlowColsAll),na.rm=T),
                total = dplyr::case_when(total < otherTotal ~ otherTotal,TRUE ~ total),
                adjustedDemand = dplyr::case_when(total > otherTotal ~ total - otherTotal,TRUE ~ 0)) %>%
  dplyr::select(-otherTotal)

ioTable_adjustTotal %>% as.data.frame()

#----------------------
# Adjust for Capacity and New Capacity
#-------------------

ioTable_adjustCap <- ioTable_adjustTotal %>%
  dplyr::mutate(capNew = dplyr::case_when(total > cap ~ (total-cap), TRUE~0),
                cap = dplyr::case_when(total > cap ~ cap + capNew, TRUE~cap),
                capOrig = cap - capNew)

ioTable_adjustCap %>% as.data.frame()


#-----------------------------------------------------------
# Update Default A matrix with given values
#----------------------------------------------------------


#--------------------------
# Use Intensity if specified
#--------------------------

if(useIntensity==1){
  if(!is.null(A0i)){

  print("Using A0 intensity matrix provided to recaclulate Z and new totals.")

  Ai <- as.matrix(A0i%>%dplyr::select(c(A0i$supplySubSector))); Ai
  Li <- solve(diag(nrow(Ai))-Ai); Li

  # Calculate nonNexusTotals
  colsRemove <- names(ioTable_adjustCap)[names(ioTable_adjustCap) %in% c(nonFlowCols,addedColumns,unique(ioTable_adjustCap$supplySubSector))];
  colsRemove
  Di <- ioTable_adjustCap %>%
    dplyr::select(-colsRemove) %>%
    dplyr::mutate(otherTotal=rowSums(dplyr::select(.,-supplySubSector,-supplySector),na.rm=T)); Di

  # Calculate New Total based on the intensities and existing other demands
  Xi <- tibble::as_tibble(Li %*% as.matrix(Di$otherTotal)); Xi
  Xi <- dplyr::bind_cols(Di%>%dplyr::select(supplySubSector),Xi)
  names(Xi)<-c("supplySubSector","total"); Xi

  # Calculate nexus flows based on the intensity provided
  Zi <- Ai %*% diag(as.vector(t(as.matrix(Xi$total))))
  Zi <- dplyr::bind_cols(Xi%>%dplyr::select(supplySubSector),tibble::as_tibble(Zi))
  names(Zi)<-c("supplySubSector",Xi$supplySubSector);
  Zi <- Zi %>%
  dplyr::mutate(nexusTotal=rowSums(Zi %>% dplyr::select(-supplySubSector),na.rm=T)); Zi

  # Join nexus and other demands to get totals and recalculate new Cap needed
  ZiCols <- names(Zi%>%dplyr::select(-supplySubSector))[names(Zi%>%dplyr::select(-supplySubSector)) %in% names(ioTable_adjustCap)]
  DiCols <- names(Di%>%dplyr::select(-supplySubSector))[names(Di%>%dplyr::select(-supplySubSector)) %in% names(ioTable_adjustCap)]

  ioTable_adjustIntensity <- ioTable_adjustCap %>%
    dplyr::select(-ZiCols,-DiCols) %>%
    dplyr::left_join(Zi) %>%
    dplyr::left_join(Di) %>%
    dplyr::mutate(total = nexusTotal + otherTotal,
                  capNew = dplyr::case_when(total > cap ~ (total-cap), TRUE~0),
                  cap = dplyr::case_when(total > cap ~ cap + capNew, TRUE~cap),
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

 for(nexus_sector_i in ioTable_fillNexus$supplySubSector[!ioTable_fillNexus$supplySubSector %in% names(ioTable_fillNexus)]){
   ioTable_fillNexus <- ioTable_fillNexus %>%
    dplyr::mutate(!!nexus_sector_i := 0)
 }

ioTable_fillNexus

#--------------------------
# Final IO Table Outputs
#--------------------------

# If total is 0 then we cannot infer a relationship
# Those values will be set to NA in the calculated A matrix
ioTable_complete<-ioTable_fillNexus %>%
  dplyr::mutate(surplus = cap-total) %>%
  dplyr::filter(total != 0)

Zorg = ioTable_complete %>%
  dplyr::select(supplySubSector,unique(ioTable_complete$supplySubSector)) %>%
  replace(is.na(.), 0); Zorg %>% as.data.frame()
Xorg = ioTable_complete %>%
  dplyr::select(supplySubSector,total); Xorg %>% as.data.frame()
Aorg <-as.matrix(Zorg%>%dplyr::select(c(unique(Zorg$supplySubSector)))) %*% as.matrix(Xorg$total^-1*diag(nrow(Zorg)));Aorg %>% as.data.frame()
Lorg <-solve(diag(nrow(Aorg))-Aorg);
Aorg <- tibble::as_tibble(Aorg); Lorg <- tibble::as_tibble(Lorg)
names(Aorg) <- names(Lorg) <- unique(Zorg$supplySubSector)
Aorg <- Aorg %>% dplyr::bind_cols(Zorg %>% dplyr::select(supplySubSector)) %>%
  dplyr::select(c("supplySubSector",sort(unique(Zorg$supplySubSector)))) %>% dplyr::arrange(supplySubSector) ;Aorg
Lorg <- Lorg %>% dplyr::bind_cols(Zorg %>% dplyr::select(supplySubSector));Lorg

colOrder <- c("supplySubSector",sort(ioTable_complete$supplySubSector),
              names(ioTable_complete)[!names(ioTable_complete) %in% c(ioTable_complete$supplySubSector,
                                                                      "export",nonFlowColsAll)],
              "export",nonFlowColsAll[nonFlowColsAll!="supplySubSector"]); colOrder

ioTable_complete <- ioTable_complete %>% dplyr::select(colOrder) %>% dplyr::arrange(supplySubSector);
ioTable_complete %>%as.data.frame()

A_Output = A_Output %>% dplyr::bind_rows(Aorg %>% tibble::as_tibble() %>% dplyr::mutate(region=region_i,scenario=scenario_i,x=year_i,subRegion=subRegion_i)); A_Output
L_Output = L_Output %>% dplyr::bind_rows(Lorg %>% tibble::as_tibble() %>% dplyr::mutate(region=region_i,scenario=scenario_i,x=year_i,subRegion=subRegion_i)); L_Output
ioTbl_Output = ioTbl_Output %>% dplyr::bind_rows(ioTable_complete %>% tibble::as_tibble() %>% dplyr::mutate(region=region_i,scenario=scenario_i,x=year_i,subRegion=subRegion_i)); ioTbl_Output %>% as.data.frame()


    } # close loop region
    } # Close loop scenario
  } # close loop subRegion
} # close loop year

    sol_list<-list(A_Output=A_Output,
                   L_Output=L_Output,
                   ioTbl_Output=ioTbl_Output)

  sol_list<-sol_list[sapply(sol_list, function(x) dim(x)[1]) > 0]



#-------------------------------------------------------------------------------------------------------
# IO Visualization (Eventually Move this to separate function?)
#-------------------------------------------------------------------------------------------------------


  figWidth_i = figWidth
  figHeight_i = figHeight


#------------------------
# Create Directories for Figure Outputs
#-----------------------


   for(region_i in regions){
    for(scenario_i in scenarios){
        # for(year_i in years){


      if(length(unique((sol_list$ioTbl_Output)$subRegion))>1){

          if (!dir.exists(paste(dirOutputs, "/IO/",region_i, sep = ""))){
            dir.create(paste(dirOutputs, "/IO/",region_i,  sep = ""))}
          if (!dir.exists(paste(dirOutputs, "/IO/",region_i,"/",scenario_i, sep = ""))){
            dir.create(paste(dirOutputs, "/IO/",region_i,"/",scenario_i,  sep = ""))}
          if (!dir.exists(paste(dirOutputs, "/IO/",region_i,"/",scenario_i,"/combSubReg", sep = ""))){
            dir.create(paste(dirOutputs, "/IO/",region_i,"/",scenario_i,"/combSubReg",  sep = ""))}


#--------------
# Combined Subregions
#----------

dir<-paste(dirOutputs, "/IO/",region_i,"/",scenario_i,"/combSubReg",sep = "")


#---------------------
# sol_Output
#---------------------


  # A Intensity Matrix

  A_mat <- sol_list$A_Output %>%
    dplyr::filter(region==region_i,scenario==scenario_i); A_mat

  A_matx <- A_mat %>%
    tidyr::gather(-c("supplySubSector",addedColumns[addedColumns %in% names(A_mat)]),key="sectorTo",value="value") %>%
    dplyr::rename (sectorFrom=supplySubSector) %>%
    dplyr::arrange(sectorFrom) %>%
    dplyr::filter(!is.nan(value),!is.infinite(value),value!=0, !is.na(value)); A_matx

  sectorFromOrder <- sort(unique(A_matx$sectorFrom)); sectorFromOrder
  sectorToOrder <-  sort(unique(A_matx$sectorTo)); sectorToOrder

  if(nrow(A_matx)>0){

  fname = paste("A_sub_",scenario_i,nameAppend,sep="")
  plotx <- A_matx
  figWidth_ix <- 1*figWidth_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));figWidth_ix
  figHeight_ix <- 1*figHeight_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));  figHeight_ix
  metis.chart(data=plotx, chartType="bubble", xData="sectorTo", yData="sectorFrom", classLabel="classLabel1",
              xLabel="sector To", yLabel="sector From", sectorToOrder=sectorToOrder, sectorFromOrder=sectorFromOrder,
              removeCols=nonFlowCols, bubbleSize = 10, facet_rows="x", facet_columns="subRegion",ncolrow=4, printFig = T,
              fileName =  fname, dirOutputs=dir, figWidth= figWidth_ix,
              figHeight=figHeight_ix,pdfpng="png")

  # data=A_matx; chartType="bubble"; xData="sectorTo"; yData="sectorFrom"; classLabel="classLabel1";
  # xLabel="sector To"; yLabel="sector From"; labelTextSize=5; sectorToOrder=sectorToOrder; sectorFromOrder=sectorFromOrder;
  # removeCols=nonFlowCols; bubbleSize = 10; facet_rows="x"; facet_columns="subRegion";ncolrow=4; printFig = T;
  # fileName =  fname; dirOutputs=dir; figWidth= figWidth_ix;
  # figHeight=figHeight_ix;pdfpng="png"

  }


  A_matxAgg <- A_matx %>% dplyr::filter(grepl("_all",sectorFrom) & grepl("_all",sectorTo)); A_matxAgg %>% as.data.frame()
  sectorFromOrder <- sort(unique(A_matxAgg $sectorFrom)); sectorFromOrder
  sectorToOrder <-  sort(unique(A_matxAgg $sectorTo)); sectorToOrder


  if(nrow(A_matxAgg )>0){

    fname = paste("A_agg_",scenario_i,nameAppend,sep="")
    plotx <- A_matxAgg
    figWidth_ix <- 1*figWidth_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));figWidth_ix
    figHeight_ix <- 1*figHeight_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));  figHeight_ix
    metis.chart(data=plotx , chartType="bubble", xData="sectorTo", yData="sectorFrom", classLabel="classLabel1",
                xLabel="sector To", yLabel="sector From", sectorToOrder=sectorToOrder, sectorFromOrder=sectorFromOrder,
                removeCols=nonFlowCols, bubbleSize = 10, facet_rows="x", facet_columns="subRegion",ncolrow=4, printFig = T,
                fileName =  fname, dirOutputs=dir, figWidth= figWidth_ix,
                figHeight=figHeight_ix,pdfpng="png")

  }



  # ioTable normalized

  sol<- sol_list$ioTbl_Output %>%
    dplyr::filter(scenario==scenario_i,region==region_i); sol


  df_Mn<-sol %>%
    dplyr::mutate(totalTemp=total) %>% # to move total to end to include columns on right side of total in the normalization
    dplyr::mutate_at(dplyr::vars(-c("supplySubSector","supplySector",addedColumns[addedColumns %in% names(sol)])),dplyr::funs(./totalTemp)) %>%
    dplyr::select(-totalTemp); df_Mn

  solx <- sol %>%
    tidyr::gather(-c("supplySubSector","supplySector",addedColumns[addedColumns %in% names(df_Mn)]),key="sectorTo",value="value") %>%
    dplyr::rename (sectorFrom=supplySubSector) %>%
    dplyr::mutate(sectorToAgg = sub("_[^_]*$", "", sectorTo)) %>%
    dplyr::arrange(sectorFrom)  %>%
    dplyr::filter(!is.nan(value),value!=0, !is.na(value));solx


  df_Mnx <- df_Mn %>%
    tidyr::gather(-c("supplySubSector","supplySector",addedColumns[addedColumns %in% names(df_Mn)]),key="sectorTo",value="value") %>%
    dplyr::rename (sectorFrom=supplySubSector) %>%
    dplyr::mutate(sectorToAgg = sub("_[^_]*$", "", sectorTo)) %>%
    dplyr::arrange(sectorFrom) %>%
    dplyr::filter(!is.nan(value),!is.infinite(value),value!=0, !is.na(value)); df_Mnx

  sectorFromOrder <- sort(unique(df_Mnx$sectorFrom)); sectorFromOrder
  sectorToOrder <-
    c(sort(unique(df_Mnx$sectorTo)[unique(df_Mnx$sectorTo) %in% unique(sol$supplySubSector)]),
      sort(unique(df_Mnx$sectorTo)[!unique(c(df_Mnx$sectorTo)) %in% c(unique(sol$supplySubSector),"export",nonFlowCols)]),
      "export",nonFlowCols); sectorToOrder

  # ioTable normalized bubbles
  if(nrow(df_Mnx)>0){
  fname = paste("ioNORM_",scenario_i,nameAppend,sep="")
  plotx <- df_Mnx
  figWidth_ix <- 1*figWidth_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));figWidth_ix
  figHeight_ix <- 1*figHeight_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));  figHeight_ix
  metis.chart(data=plotx , chartType="bubble", xData="sectorTo", yData="sectorFrom", classLabel="classLabel1",
              xLabel="sector To", yLabel="sector From", sectorToOrder=sectorToOrder, sectorFromOrder=sectorFromOrder,
              removeCols=nonFlowCols, bubbleSize = 10, facet_rows="x", facet_columns="subRegion",ncolrow=4, printFig = T,
              fileName =  fname, dirOutputs=dir, figWidth= figWidth_ix,
              figHeight=figHeight_ix,pdfpng="png")

  # ioTable absolute bubbles
  if(nrow(solx)>0){
  fname = paste("ioABS_",scenario_i,nameAppend,sep="")
  plotx <- solx
  figWidth_ix <- 1*figWidth_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));figWidth_ix
  figHeight_ix <- 1*figHeight_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));  figHeight_ix
  metis.chart(data=plotx , dataNorm=df_Mnx, chartType="bubble", xData="sectorTo", yData="sectorFrom", classLabel="classLabel1",
              xLabel="sector To", yLabel="sector From", sectorToOrder=sectorToOrder, sectorFromOrder=sectorFromOrder,
              removeCols=nonFlowCols, bubbleSize = 10, facet_rows="x", facet_columns="subRegion",ncolrow=4, printFig = T,
              fileName =  fname, dirOutputs=dir, figWidth= figWidth_ix,
              figHeight=figHeight_ix,pdfpng="png")
  }
  }


  df_Mnx_AggDem <- df_Mnx %>% dplyr::mutate(sectorTo=sectorToAgg) %>% dplyr::select(-sectorToAgg) %>%
    dplyr::group_by(sectorTo,sectorFrom,supplySector,scenario,x,region,subRegion,units,param) %>% dplyr::summarize(value=sum(value)) %>% dplyr::ungroup();
  df_Mnx_AggDem %>% as.data.frame()
  solx_AggDem <- solx %>% dplyr::mutate(sectorTo=sectorToAgg) %>% dplyr::select(-sectorToAgg) %>%
    dplyr::group_by(sectorTo,sectorFrom,supplySector,scenario,x,region,subRegion,units,param) %>% dplyr::summarize(value=sum(value)) %>% dplyr::ungroup();
  solx_AggDem %>% as.data.frame()

  sectorFromOrder <- sort(unique(df_Mnx_AggDem$sectorFrom)); sectorFromOrder
  sectorToOrder <-
    c(sort(unique(df_Mnx_AggDem$sectorTo)[unique(df_Mnx_AggDem$sectorTo) %in% unique(df_Mnx_AggDem$supplySubSector)]),
      sort(unique(df_Mnx_AggDem$sectorTo)[!unique(c(df_Mnx_AggDem$sectorTo)) %in% c(unique(df_Mnx_AggDem$supplySubSector),"export",nonFlowCols)]),
      "export",nonFlowCols); sectorToOrder

  # ioTable normalized bubbles
  if(nrow(df_Mnx_AggDem)>0){
    fname = paste("ioNORM_aggDem_",scenario_i,nameAppend,sep="")
    plotx <- df_Mnx_AggDem
    figWidth_ix <- 1*figWidth_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));figWidth_ix
    figHeight_ix <- 1*figHeight_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));  figHeight_ix
    metis.chart(data=plotx, chartType="bubble", xData="sectorTo", yData="sectorFrom", classLabel="classLabel1",
                xLabel="sector To", yLabel="sector From", sectorToOrder=sectorToOrder, sectorFromOrder=sectorFromOrder,
                removeCols=nonFlowCols, bubbleSize = 10, facet_rows="x", facet_columns="subRegion",ncolrow=4, printFig = T,
                fileName =  fname, dirOutputs=dir, figWidth= figWidth_ix,
                figHeight=figHeight_ix,pdfpng="png")

    # ioTable absolute bubbles
    if(nrow(solx_AggDem)>0){
      fname = paste("ioABS_aggDem_",scenario_i,nameAppend,sep="")
      fplotx <- solx_AggDem
      figWidth_ix <- 1*figWidth_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));figWidth_ix
      figHeight_ix <- 1*figHeight_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));  figHeight_ix
      metis.chart(data=plotx, dataNorm=df_Mnx_AggDem, chartType="bubble", xData="sectorTo", yData="sectorFrom", classLabel="classLabel1",
                  xLabel="sector To", yLabel="sector From", sectorToOrder=sectorToOrder, sectorFromOrder=sectorFromOrder,
                  removeCols=nonFlowCols, bubbleSize = 10, facet_rows="x", facet_columns="subRegion",ncolrow=4, printFig = T,
                  fileName =  fname, dirOutputs=dir, figWidth= figWidth_ix,
                  figHeight=figHeight_ix,pdfpng="png")
    }}



  df_Mnx_AggDemAggSup <- df_Mnx %>% dplyr::mutate(sectorTo=sectorToAgg) %>% dplyr::select(-sectorToAgg) %>%
    dplyr::group_by(sectorTo,sectorFrom,supplySector,scenario,x,region,subRegion,units,param) %>% dplyr::summarize(value=sum(value)) %>%
    dplyr::ungroup() %>% dplyr::filter(grepl("_all",sectorFrom));
  df_Mnx_AggDemAggSup %>% as.data.frame()
  solx_AggDemAggSup <- solx %>% dplyr::mutate(sectorTo=sectorToAgg) %>% dplyr::select(-sectorToAgg) %>%
    dplyr::group_by(sectorTo,sectorFrom,supplySector,scenario,x,region,subRegion,units,param) %>% dplyr::summarize(value=sum(value)) %>%
    dplyr::ungroup() %>% dplyr::filter(grepl("_all",sectorFrom));
  solx_AggDemAggSup %>% as.data.frame()

  sectorFromOrder <- sort(unique(df_Mnx_AggDemAggSup$sectorFrom)); sectorFromOrder
  sectorToOrder <-
    c(sort(unique(df_Mnx_AggDemAggSup$sectorTo)[unique(df_Mnx_AggDemAggSup$sectorTo) %in% unique(df_Mnx_AggDemAggSup$supplySubSector)]),
      sort(unique(df_Mnx_AggDemAggSup$sectorTo)[!unique(c(df_Mnx_AggDemAggSup$sectorTo)) %in% c(unique(df_Mnx_AggDemAggSup$supplySubSector),"export",nonFlowCols)]),
      "export",nonFlowCols); sectorToOrder

  # ioTable normalized bubbles
  if(nrow(df_Mnx_AggDemAggSup)>0){
    fname = paste("ioNORM_aggDemaggSup_",scenario_i,nameAppend,sep="")
    plotx <- df_Mnx_AggDemAggSup
    figWidth_ix <- 1*figWidth_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));figWidth_ix
    figHeight_ix <- 1*figHeight_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));  figHeight_ix
    metis.chart(data=plotx, chartType="bubble", xData="sectorTo", yData="sectorFrom", classLabel="classLabel1",
                xLabel="sector To", yLabel="sector From", sectorToOrder=sectorToOrder, sectorFromOrder=sectorFromOrder,
                removeCols=nonFlowCols, bubbleSize = 10, facet_rows="x", facet_columns="subRegion",ncolrow=4, printFig = T,
                fileName =  fname, dirOutputs=dir, figWidth= figWidth_ix,
                figHeight=figHeight_ix,pdfpng="png")

    # ioTable absolute bubbles
    if(nrow(solx_AggDemAggSup)>0){
      fname = paste("ioABS_aggDemaggSup_",scenario_i,nameAppend,sep="")
      plotx <- solx_AggDemAggSup
      figWidth_ix <- 1*figWidth_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));figWidth_ix
      figHeight_ix <- 1*figHeight_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));  figHeight_ix
      metis.chart(data=plotx, dataNorm=df_Mnx_AggDemAggSup, chartType="bubble", xData="sectorTo", yData="sectorFrom", classLabel="classLabel1",
                  xLabel="sector To", yLabel="sector From", sectorToOrder=sectorToOrder, sectorFromOrder=sectorFromOrder,
                  removeCols=nonFlowCols, bubbleSize = 10, facet_rows="x", facet_columns="subRegion",ncolrow=4, printFig = T,
                  fileName =  fname, dirOutputs=dir, figWidth= figWidth_ix,
                  figHeight=figHeight_ix,pdfpng="png")
    }}




  #-----------------
  # Sankey
  #--------------

  df <- sol %>%
    dplyr::select(-nonFlowCols); df

  dfx <- df %>%
    tidyr::gather(-c("supplySubSector","supplySector",addedColumns[addedColumns %in% names(df_Mn)]),key="sectorTo",value="value") %>%
    dplyr::rename (sectorFrom=supplySubSector) %>%
    dplyr::filter(value>0) %>%
    unique() %>%
    dplyr::arrange(sectorFrom); dfx

  if(nrow(dfx)>0){

  # Sankey Aggregated to supply subSectors and aggregated demand

  dfx_sankey <- dfx %>%dplyr::filter(value!=0) %>% dplyr::filter(!grepl("_all",sectorFrom)) %>%
      dplyr::mutate(sectorToAgg = sub("_[^_]*$", "", sectorTo)) %>%
      dplyr::mutate(sectorTo=sectorToAgg) %>% dplyr::select(-sectorToAgg) %>%
      dplyr::group_by(sectorTo,sectorFrom,supplySector,scenario,x,region,subRegion,units,param) %>% dplyr::summarize(value=sum(value)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(supplySector,region) %>%
      dplyr::mutate(normValue=value/sum(value)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(sectorFrom,region,subRegion,x) %>%
      dplyr::mutate(fromLabel = dplyr::case_when((sum(value) <= 1 & sum(value) >= -1) ~ paste(sectorFrom," ",signif(sum(value),2)," ",units,sep=""),
                                        (sum(value) >= 10 & sum(value) <= -10) ~ paste(sectorFrom," ",round(sum(value),0)," ",units,sep=""),
                                        TRUE ~ paste(sectorFrom," ",round(sum(value),1)," ",units,sep="")),
                  legendLabel = paste(sectorFrom," ",units,sep="")) %>%
      as.data.frame() %>%
    dplyr::ungroup();dfx_sankey

  sectorFromOrder <- sort(unique(dfx_sankey$sectorFrom)); sectorFromOrder
  sectorToOrder <-
    c(sort(unique(dfx_sankey$sectorTo)[unique(dfx_sankey$sectorTo) %in% unique(dfx_sankey$supplySubSector)]),
      sort(unique(dfx_sankey$sectorTo)[!unique(c(dfx_sankey$sectorTo)) %in% c(unique(dfx_sankey$supplySubSector),"export",nonFlowCols)]),
      "export",nonFlowCols); sectorToOrder


  sectorFromOrderSankey <- sectorFromOrder[sectorFromOrder %in% unique(dfx_sankey$sectorFrom)]; sectorFromOrderSankey
  sectorToOrderSankey <- sectorToOrder[sectorToOrder %in% unique(dfx_sankey$sectorTo)]; sectorToOrderSankey

  dfx_sankey$sectorFrom <- factor( as.character(dfx_sankey$sectorFrom), levels=sectorFromOrderSankey )
  dfx_sankey$sectorTo <- factor( as.character(dfx_sankey$sectorTo), levels=sectorToOrderSankey )


  fname = paste("sankeySub_aggDem_",scenario_i,nameAppend,sep="")
  plotx<-dfx_sankey
  figWidth_ix <- 1*figWidth_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));figWidth_ix
  figHeight_ix <- 1*figHeight_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));  figHeight_ix
  metis.chart(data=plotx, chartType="sankey",  xData="sectorTo", yData="normValue", sankeyGroupColor="supplySector",
              classLabel="From", class = "supplySector", classPalette = "pal_sankey",
              sankeyAxis1="fromLabel",sankeyAxis2="sectorTo",sankeyAxis1Label ="From",sankeyAxis2Label="To",
              sectorToOrder=sectorToOrder, sectorFromOrder=sectorFromOrder,
              removeCols=nonFlowCols, bubbleSize = 10, facet_rows="x", facet_columns="subRegion",ncolrow=4, printFig = T,
              fileName =  fname, dirOutputs=dir, figWidth= figWidth_ix,
              figHeight=figHeight_ix,pdfpng="png", sankeyLabelsOn=sankeyLabelAbsPlots)

  # data=dfx_sankey; chartType="sankey"; xData="sectorTo"; yData="normValue"; sankeyGroupColor="supplySector";
  # classLabel="From"; class = "supplySector"; classPalette = "pal_sankey";
  # sankeyAxis1="fromLabel";sankeyAxis2="sectorTo";sankeyAxis1Label ="From";sankeyAxis2Label="To";
  # labelTextSize=5; sectorToOrder=sectorToOrder; sectorFromOrder=sectorFromOrder;
  # removeCols=nonFlowCols; bubbleSize = 10; facet_rows="x"; facet_columns="subRegion";ncolrow=4; printFig = T;
  # fileName =  fname; dirOutputs=dir; figWidth= figWidth_ix;
  # figHeight=figHeight_ix;pdfpng="png"; sankeyLabelsOn=sankeyLabelAbsPlots

  dfx_sankey <- dfx%>%dplyr::filter(value!=0, !grepl("_all",sectorFrom)) %>%
    dplyr::mutate(sectorToAgg = sub("_[^_]*$", "", sectorTo)) %>%
    dplyr::mutate(sectorTo=sectorToAgg) %>% dplyr::select(-sectorToAgg) %>%
    dplyr::group_by(sectorTo,sectorFrom,supplySector,scenario,x,region,subRegion,units,param) %>% dplyr::summarize(value=sum(value)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(supplySector,region,subRegion,x) %>%
    dplyr::mutate(normValue=value/sum(value)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(sectorFrom,region,subRegion,x) %>%
    dplyr::mutate(fromLabel = dplyr::case_when((sum(value) <= 1 & sum(value) >= -1) ~ paste(sectorFrom," ",signif(sum(value),2)," ",units,sep=""),
                                        (sum(value) >= 10 & sum(value) <= -10) ~ paste(sectorFrom," ",round(sum(value),0)," ",units,sep=""),
                                        TRUE ~ paste(sectorFrom," ",round(sum(value),1)," ",units,sep="")),
                  legendLabel = paste(sectorFrom," ",units,sep="")) %>%
    as.data.frame() %>%
    dplyr::ungroup();dfx_sankey

  sectorFromOrder <- sort(unique(dfx_sankey$sectorFrom)); sectorFromOrder
  sectorToOrder <-
    c(sort(unique(dfx_sankey$sectorTo)[unique(dfx_sankey$sectorTo) %in% unique(dfx_sankey$supplySubSector)]),
      sort(unique(dfx_sankey$sectorTo)[!unique(c(dfx_sankey$sectorTo)) %in% c(unique(dfx_sankey$supplySubSector),"export",nonFlowCols)]),
      "export",nonFlowCols); sectorToOrder


  sectorFromOrderSankey <- sectorFromOrder[sectorFromOrder %in% unique(dfx_sankey$sectorFrom)]; sectorFromOrderSankey
  sectorToOrderSankey <- sectorToOrder[sectorToOrder %in% unique(dfx_sankey$sectorTo)]; sectorToOrderSankey

  dfx_sankey$sectorFrom <- factor( as.character(dfx_sankey$sectorFrom), levels=sectorFromOrderSankey )
  dfx_sankey$sectorTo <- factor( as.character(dfx_sankey$sectorTo), levels=sectorToOrderSankey )

  fname = paste("sankeySub_aggDem_",scenario_i,nameAppend,"_Free",sep="")
  plotx<-dfx_sankey
  figWidth_ix <- 1*figWidth_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));figWidth_ix
  figHeight_ix <- 1*figHeight_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));  figHeight_ix
  metis.chart(data=plotx, chartType="sankey",  xData="sectorTo", yData="normValue", sankeyGroupColor="supplySector",
              classLabel="From", class = "supplySector", classPalette = "pal_sankey",
              sankeyAxis1="fromLabel",sankeyAxis2="sectorTo",sankeyAxis1Label ="From",sankeyAxis2Label="To",
              sectorToOrder=sectorToOrder, sectorFromOrder=sectorFromOrder,
              removeCols=nonFlowCols, bubbleSize = 10, facet_rows="x", facet_columns="subRegion",ncolrow=4, printFig = T,
              fileName =  fname, dirOutputs=dir, figWidth= figWidth_ix,
              figHeight=figHeight_ix,pdfpng="png")


  # Sankey Aggregated to supply sectors and agg Demand

  dfx_sankey <- dfx %>%dplyr::filter(value!=0) %>% dplyr::filter(grepl("_all",sectorFrom)) %>%
    dplyr::mutate(sectorToAgg = sub("_[^_]*$", "", sectorTo)) %>%
    dplyr::mutate(sectorTo=sectorToAgg) %>% dplyr::select(-sectorToAgg) %>%
    dplyr::group_by(sectorTo,sectorFrom,supplySector,scenario,x,region,subRegion,units,param) %>% dplyr::summarize(value=sum(value)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(supplySector,region) %>%
    dplyr::mutate(normValue=value/sum(value)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(sectorFrom,region,subRegion,x) %>%
    dplyr::mutate(fromLabel = dplyr::case_when((sum(value) <= 1 & sum(value) >= -1) ~ paste(sectorFrom," ",signif(sum(value),2)," ",units,sep=""),
                                        (sum(value) >= 10 & sum(value) <= -10) ~ paste(sectorFrom," ",round(sum(value),0)," ",units,sep=""),
                                        TRUE ~ paste(sectorFrom," ",round(sum(value),1)," ",units,sep="")),
                  legendLabel = paste(sectorFrom," ",units,sep="")) %>%as.data.frame() %>%
    dplyr::ungroup();dfx_sankey

  sectorFromOrder <- sort(unique(dfx_sankey$sectorFrom)); sectorFromOrder
  sectorToOrder <-
    c(sort(unique(dfx_sankey$sectorTo)[unique(dfx_sankey$sectorTo) %in% unique(dfx_sankey$supplySubSector)]),
      sort(unique(dfx_sankey$sectorTo)[!unique(c(dfx_sankey$sectorTo)) %in% c(unique(dfx_sankey$supplySubSector),"export",nonFlowCols)]),
      "export",nonFlowCols); sectorToOrder


  sectorFromOrderSankey <- sectorFromOrder[sectorFromOrder %in% unique(dfx_sankey$sectorFrom)]; sectorFromOrderSankey
  sectorToOrderSankey <- sectorToOrder[sectorToOrder %in% unique(dfx_sankey$sectorTo)]; sectorToOrderSankey

  dfx_sankey$sectorFrom <- factor( as.character(dfx_sankey$sectorFrom), levels=sectorFromOrderSankey )
  dfx_sankey$sectorTo <- factor( as.character(dfx_sankey$sectorTo), levels=sectorToOrderSankey )

  fname = paste("sankeyAll_aggDem_",scenario_i,nameAppend,sep="")
  plotx<-dfx_sankey
  figWidth_ix <- 1*figWidth_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));figWidth_ix
  figHeight_ix <- 1*figHeight_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));  figHeight_ix
  metis.chart(data=plotx, chartType="sankey",  xData="sectorTo", yData="normValue", sankeyGroupColor="supplySector",
              classLabel="From", class = "supplySector", classPalette = "pal_sankey",
              sankeyAxis1="fromLabel",sankeyAxis2="sectorTo",sankeyAxis1Label ="From",sankeyAxis2Label="To",
              sectorToOrder=sectorToOrder, sectorFromOrder=sectorFromOrder,
              removeCols=nonFlowCols, bubbleSize = 10, facet_rows="x", facet_columns="subRegion",ncolrow=4, printFig = T,
              fileName =  fname, dirOutputs=dir, figWidth= figWidth_ix,
              figHeight=figHeight_ix,pdfpng="png",sankeyLabelsOn=sankeyLabelAbsPlots)



  dfx_sankey <- dfx%>%dplyr::filter(value!=0, grepl("_all",sectorFrom)) %>%
    dplyr::mutate(sectorToAgg = sub("_[^_]*$", "", sectorTo)) %>%
    dplyr::mutate(sectorTo=sectorToAgg) %>% dplyr::select(-sectorToAgg) %>%
    dplyr::group_by(sectorTo,sectorFrom,supplySector,scenario,x,region,subRegion,units,param) %>% dplyr::summarize(value=sum(value)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(supplySector,region,subRegion,x) %>%
    dplyr::mutate(normValue=value/sum(value)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(sectorFrom,region,subRegion,x) %>%
    dplyr::mutate(fromLabel = dplyr::case_when((sum(value) <= 1 & sum(value) >= -1) ~ paste(sectorFrom," ",signif(sum(value),2)," ",units,sep=""),
                                        (sum(value) >= 10 & sum(value) <= -10) ~ paste(sectorFrom," ",round(sum(value),0)," ",units,sep=""),
                                        TRUE ~ paste(sectorFrom," ",round(sum(value),1)," ",units,sep="")),
                  legendLabel = paste(sectorFrom," ",units,sep="")) %>%as.data.frame() %>%
    dplyr::ungroup();dfx_sankey

  sectorFromOrder <- sort(unique(dfx_sankey$sectorFrom)); sectorFromOrder
  sectorToOrder <-
    c(sort(unique(dfx_sankey$sectorTo)[unique(dfx_sankey$sectorTo) %in% unique(dfx_sankey$supplySubSector)]),
      sort(unique(dfx_sankey$sectorTo)[!unique(c(dfx_sankey$sectorTo)) %in% c(unique(dfx_sankey$supplySubSector),"export",nonFlowCols)]),
      "export",nonFlowCols); sectorToOrder


  sectorFromOrderSankey <- sectorFromOrder[sectorFromOrder %in% unique(dfx_sankey$sectorFrom)]; sectorFromOrderSankey
  sectorToOrderSankey <- sectorToOrder[sectorToOrder %in% unique(dfx_sankey$sectorTo)]; sectorToOrderSankey

  dfx_sankey$sectorFrom <- factor( as.character(dfx_sankey$sectorFrom), levels=sectorFromOrderSankey )
  dfx_sankey$sectorTo <- factor( as.character(dfx_sankey$sectorTo), levels=sectorToOrderSankey )

  fname = paste("sankeyAll_aggDem_",scenario_i,nameAppend,"_FREE",sep="")
  plotx<-dfx_sankey
  figWidth_ix <- 1*figWidth_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));figWidth_ix
  figHeight_ix <- 1*figHeight_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));  figHeight_ix
  metis.chart(data=plotx, chartType="sankey",  xData="sectorTo", yData="normValue", sankeyGroupColor="supplySector",
              classLabel="From", class = "supplySector", classPalette = "pal_sankey",
              sankeyAxis1="fromLabel",sankeyAxis2="sectorTo",sankeyAxis1Label ="From",sankeyAxis2Label="To",
              sectorToOrder=sectorToOrder, sectorFromOrder=sectorFromOrder,
              removeCols=nonFlowCols, bubbleSize = 10, facet_rows="x", facet_columns="subRegion",ncolrow=4, printFig = T,
              fileName =  fname, dirOutputs=dir, figWidth= figWidth_ix,
              figHeight=figHeight_ix,pdfpng="png")

  }


  #-------- Total Demands

  solFlows <- sol %>%
    dplyr::select(!!c("supplySubSector",names(sol)[names(sol) %in% c("total","supplySector",addedColumns)]))
  df <- solFlows;df

  dfx <- df %>%
    tidyr::gather(-c("supplySubSector","supplySector",addedColumns[addedColumns %in% names(df_Mn)]),key="sectorTo",value="value") %>%
    dplyr::rename (sectorFrom=supplySubSector) %>%
    dplyr::filter(value>0) %>%
    unique() %>%
    dplyr::arrange(sectorFrom); dfx

  if(nrow(dfx)>0){

    # Subsupply to Agg Demands

    dfx_sankey <- dfx%>%dplyr::filter(value!=0, !grepl("_all",sectorFrom)) %>%
      dplyr::group_by(supplySector,region) %>%
      dplyr::mutate(normValue=value/sum(value)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(sectorFrom,region,subRegion,x) %>%
      dplyr::mutate(fromLabel = dplyr::case_when((sum(value) <= 1 & sum(value) >= -1) ~ paste(sectorFrom," ",signif(sum(value),2)," ",units,sep=""),
                                          (sum(value) >= 10 & sum(value) <= -10) ~ paste(sectorFrom," ",round(sum(value),0)," ",units,sep=""),
                                          TRUE ~ paste(sectorFrom," ",round(sum(value),1)," ",units,sep="")),
                    legendLabel = paste(sectorFrom," ",units,sep="")) %>%as.data.frame() %>%
      dplyr::ungroup();dfx_sankey

    sectorFromOrder <- sort(unique(dfx_sankey$sectorFrom)); sectorFromOrder
    sectorToOrder <-
      c(sort(unique(dfx_sankey$sectorTo)[unique(dfx_sankey$sectorTo) %in% unique(dfx_sankey$supplySubSector)]),
        sort(unique(dfx_sankey$sectorTo)[!unique(c(dfx_sankey$sectorTo)) %in% c(unique(dfx_sankey$supplySubSector),"export",nonFlowCols)]),
        "export",nonFlowCols); sectorToOrder


    sectorFromOrderSankey <- sectorFromOrder[sectorFromOrder %in% unique(dfx_sankey$sectorFrom)]; sectorFromOrderSankey
    sectorToOrderSankey <- sectorToOrder[sectorToOrder %in% unique(dfx_sankey$sectorTo)]; sectorToOrderSankey

    dfx_sankey$sectorFrom <- factor( as.character(dfx_sankey$sectorFrom), levels=sectorFromOrderSankey )
    dfx_sankey$sectorTo <- factor( as.character(dfx_sankey$sectorTo), levels=sectorToOrderSankey )


    fname = paste("sankeySub2Total_",scenario_i,nameAppend,sep="")
    plotx<-dfx_sankey
    figWidth_ix <- 1*figWidth_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));figWidth_ix
    figHeight_ix <- 1*figHeight_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));  figHeight_ix
    metis.chart(data=plotx, chartType="sankey",  xData="sectorTo", yData="normValue", sankeyGroupColor="supplySector",
                classLabel="From", class = "supplySector", classPalette = "pal_sankey",
                sankeyAxis1="fromLabel",sankeyAxis2="sectorTo",sankeyAxis1Label ="From",sankeyAxis2Label="To",
                sectorToOrder=sectorToOrder, sectorFromOrder=sectorFromOrder,
                removeCols=nonFlowCols, bubbleSize = 10, facet_rows="x", facet_columns="subRegion",ncolrow=4, printFig = T,
                fileName =  fname, dirOutputs=dir, figWidth= figWidth_ix,
                figHeight=figHeight_ix,pdfpng="png", sankeyLabelsOn=sankeyLabelAbsPlots)


    dfx_sankey <- dfx%>%dplyr::filter(value!=0, !grepl("_all",sectorFrom)) %>%
      dplyr::group_by(supplySector,region,subRegion,x) %>%
      dplyr::mutate(normValue=value/sum(value)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(sectorFrom,region,subRegion,x) %>%
      dplyr::mutate(fromLabel = dplyr::case_when((sum(value) <= 1 & sum(value) >= -1) ~ paste(sectorFrom," ",signif(sum(value),2)," ",units,sep=""),
                                          (sum(value) >= 10 & sum(value) <= -10) ~ paste(sectorFrom," ",round(sum(value),0)," ",units,sep=""),
                                          TRUE ~ paste(sectorFrom," ",round(sum(value),1)," ",units,sep="")),
                    legendLabel = paste(sectorFrom," ",units,sep="")) %>%as.data.frame() %>%
      dplyr::ungroup();dfx_sankey

    sectorFromOrder <- sort(unique(dfx_sankey$sectorFrom)); sectorFromOrder
    sectorToOrder <-
      c(sort(unique(dfx_sankey$sectorTo)[unique(dfx_sankey$sectorTo) %in% unique(dfx_sankey$supplySubSector)]),
        sort(unique(dfx_sankey$sectorTo)[!unique(c(dfx_sankey$sectorTo)) %in% c(unique(dfx_sankey$supplySubSector),"export",nonFlowCols)]),
        "export",nonFlowCols); sectorToOrder


    sectorFromOrderSankey <- sectorFromOrder[sectorFromOrder %in% unique(dfx_sankey$sectorFrom)]; sectorFromOrderSankey
    sectorToOrderSankey <- sectorToOrder[sectorToOrder %in% unique(dfx_sankey$sectorTo)]; sectorToOrderSankey

    dfx_sankey$sectorFrom <- factor( as.character(dfx_sankey$sectorFrom), levels=sectorFromOrderSankey )
    dfx_sankey$sectorTo <- factor( as.character(dfx_sankey$sectorTo), levels=sectorToOrderSankey )


    fname = paste("sankeySub2Total_",scenario_i,nameAppend,"_FREE",sep="")
    plotx<-dfx_sankey
    figWidth_ix <- 1*figWidth_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));figWidth_ix
    figHeight_ix <- 1*figHeight_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));  figHeight_ix
    metis.chart(data=plotx, chartType="sankey",  xData="sectorTo", yData="normValue", sankeyGroupColor="supplySector",
                classLabel="From", class = "supplySector", classPalette = "pal_sankey",
                sankeyAxis1="fromLabel",sankeyAxis2="sectorTo",sankeyAxis1Label ="From",sankeyAxis2Label="To",
                sectorToOrder=sectorToOrder, sectorFromOrder=sectorFromOrder,
                removeCols=nonFlowCols, bubbleSize = 10, facet_rows="x", facet_columns="subRegion",ncolrow=4, printFig = T,
                fileName =  fname, dirOutputs=dir, figWidth= figWidth_ix,
                figHeight=figHeight_ix,pdfpng="png")


    # SubSupply to Total Demands

    dfx_sankey <- dfx%>%dplyr::filter(value!=0, !grepl("_all",sectorFrom)) %>%
      dplyr::group_by(supplySector,region) %>%
      dplyr::mutate(normValue=value/sum(value)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(sectorFrom,region,subRegion,x) %>%
      dplyr::mutate(fromLabel = dplyr::case_when((sum(value) <= 1 & sum(value) >= -1) ~ paste(sectorFrom," ",signif(sum(value),2)," ",units,sep=""),
                                          (sum(value) >= 10 & sum(value) <= -10) ~ paste(sectorFrom," ",round(sum(value),0)," ",units,sep=""),
                                          TRUE ~ paste(sectorFrom," ",round(sum(value),1)," ",units,sep="")),
                    legendLabel = paste(sectorFrom," ",units,sep="")) %>% as.data.frame() %>%
      dplyr::ungroup();dfx_sankey

    sectorFromOrder <- sort(unique(dfx_sankey$sectorFrom)); sectorFromOrder
    sectorToOrder <-
      c(sort(unique(dfx_sankey$sectorTo)[unique(dfx_sankey$sectorTo) %in% unique(dfx_sankey$supplySubSector)]),
        sort(unique(dfx_sankey$sectorTo)[!unique(c(dfx_sankey$sectorTo)) %in% c(unique(dfx_sankey$supplySubSector),"export",nonFlowCols)]),
        "export",nonFlowCols); sectorToOrder


    sectorFromOrderSankey <- sectorFromOrder[sectorFromOrder %in% unique(dfx_sankey$sectorFrom)]; sectorFromOrderSankey
    sectorToOrderSankey <- sectorToOrder[sectorToOrder %in% unique(dfx_sankey$sectorTo)]; sectorToOrderSankey

    dfx_sankey$sectorFrom <- factor( as.character(dfx_sankey$sectorFrom), levels=sectorFromOrderSankey )
    dfx_sankey$sectorTo <- factor( as.character(dfx_sankey$sectorTo), levels=sectorToOrderSankey )


  fname = paste("sankeySub2Total_",scenario_i,nameAppend,sep="")
  plotx<-dfx_sankey
  figWidth_ix <- 1*figWidth_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));figWidth_ix
  figHeight_ix <- 1*figHeight_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));  figHeight_ix
  metis.chart(data=plotx, chartType="sankey",  xData="sectorTo", yData="normValue", sankeyGroupColor="supplySector",
              classLabel="From", class = "supplySector", classPalette = "pal_sankey",
              sankeyAxis1="fromLabel",sankeyAxis2="sectorTo",sankeyAxis1Label ="From",sankeyAxis2Label="To",
              sectorToOrder=sectorToOrder, sectorFromOrder=sectorFromOrder,
              removeCols=nonFlowCols, bubbleSize = 10, facet_rows="x", facet_columns="subRegion",ncolrow=4, printFig = T,
              fileName =  fname, dirOutputs=dir, figWidth= figWidth_ix,
              figHeight=figHeight_ix,pdfpng="png", sankeyLabelsOn=sankeyLabelAbsPlots)

  dfx_sankey <- dfx%>%dplyr::filter(value!=0, !grepl("_all",sectorFrom)) %>%
    dplyr::group_by(supplySector,region,subRegion,x) %>%
    dplyr::mutate(normValue=value/sum(value)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(sectorFrom,region,subRegion,x) %>%
    dplyr::mutate(fromLabel = dplyr::case_when((sum(value) <= 1 & sum(value) >= -1) ~ paste(sectorFrom," ",signif(sum(value),2)," ",units,sep=""),
                                        (sum(value) >= 10 & sum(value) <= -10) ~ paste(sectorFrom," ",round(sum(value),0)," ",units,sep=""),
                                        TRUE ~ paste(sectorFrom," ",round(sum(value),1)," ",units,sep="")),
                  legendLabel = paste(sectorFrom," ",units,sep="")) %>%as.data.frame() %>%
    dplyr::ungroup();dfx_sankey

  sectorFromOrder <- sort(unique(dfx_sankey$sectorFrom)); sectorFromOrder
  sectorToOrder <-
    c(sort(unique(dfx_sankey$sectorTo)[unique(dfx_sankey$sectorTo) %in% unique(dfx_sankey$supplySubSector)]),
      sort(unique(dfx_sankey$sectorTo)[!unique(c(dfx_sankey$sectorTo)) %in% c(unique(dfx_sankey$supplySubSector),"export",nonFlowCols)]),
      "export",nonFlowCols); sectorToOrder


  sectorFromOrderSankey <- sectorFromOrder[sectorFromOrder %in% unique(dfx_sankey$sectorFrom)]; sectorFromOrderSankey
  sectorToOrderSankey <- sectorToOrder[sectorToOrder %in% unique(dfx_sankey$sectorTo)]; sectorToOrderSankey

  dfx_sankey$sectorFrom <- factor( as.character(dfx_sankey$sectorFrom), levels=sectorFromOrderSankey )
  dfx_sankey$sectorTo <- factor( as.character(dfx_sankey$sectorTo), levels=sectorToOrderSankey )

  fname = paste("sankeySub2Total_",scenario_i,nameAppend,"_FREE",sep="")
  plotx<-dfx_sankey
  figWidth_ix <- 1*figWidth_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));figWidth_ix
  figHeight_ix <- 1*figHeight_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));  figHeight_ix
  metis.chart(data=plotx, chartType="sankey",  xData="sectorTo", yData="normValue", sankeyGroupColor="supplySector",
              classLabel="From", class = "supplySector", classPalette = "pal_sankey",
              sankeyAxis1="fromLabel",sankeyAxis2="sectorTo",sankeyAxis1Label ="From",sankeyAxis2Label="To",
              sectorToOrder=sectorToOrder, sectorFromOrder=sectorFromOrder,
              removeCols=nonFlowCols, bubbleSize = 10, facet_rows="x", facet_columns="subRegion",ncolrow=4, printFig = T,
              fileName =  fname, dirOutputs=dir, figWidth= figWidth_ix,
              figHeight=figHeight_ix,pdfpng="png")



  dfx_sankey <- dfx%>%dplyr::filter(value!=0, grepl("_all",sectorFrom)) %>%
    dplyr::group_by(supplySector,region) %>%
    dplyr::mutate(normValue=value/sum(value)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(sectorFrom,region,subRegion,x) %>%
    dplyr::mutate(fromLabel = dplyr::case_when((sum(value) <= 1 & sum(value) >= -1) ~ paste(sectorFrom," ",signif(sum(value),2)," ",units,sep=""),
                                        (sum(value) >= 10 & sum(value) <= -10) ~ paste(sectorFrom," ",round(sum(value),0)," ",units,sep=""),
                                        TRUE ~ paste(sectorFrom," ",round(sum(value),1)," ",units,sep="")),
                  legendLabel = paste(sectorFrom," ",units,sep="")) %>%as.data.frame() %>%
    dplyr::ungroup();dfx_sankey

  sectorFromOrder <- sort(unique(dfx_sankey$sectorFrom)); sectorFromOrder
  sectorToOrder <-
    c(sort(unique(dfx_sankey$sectorTo)[unique(dfx_sankey$sectorTo) %in% unique(dfx_sankey$supplySubSector)]),
      sort(unique(dfx_sankey$sectorTo)[!unique(c(dfx_sankey$sectorTo)) %in% c(unique(dfx_sankey$supplySubSector),"export",nonFlowCols)]),
      "export",nonFlowCols); sectorToOrder


  sectorFromOrderSankey <- sectorFromOrder[sectorFromOrder %in% unique(dfx_sankey$sectorFrom)]; sectorFromOrderSankey
  sectorToOrderSankey <- sectorToOrder[sectorToOrder %in% unique(dfx_sankey$sectorTo)]; sectorToOrderSankey

  dfx_sankey$sectorFrom <- factor( as.character(dfx_sankey$sectorFrom), levels=sectorFromOrderSankey )
  dfx_sankey$sectorTo <- factor( as.character(dfx_sankey$sectorTo), levels=sectorToOrderSankey )


  fname = paste("sankeyAll2Total_",scenario_i,nameAppend,sep="")
  plotx<-dfx_sankey
  figWidth_ix <- 1*figWidth_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));figWidth_ix
  figHeight_ix <- 1*figHeight_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));  figHeight_ix
  metis.chart(data=plotx, chartType="sankey",  xData="sectorTo", yData="normValue", sankeyGroupColor="supplySector",
              classLabel="From", class = "supplySector", classPalette = "pal_sankey",
              sankeyAxis1="fromLabel",sankeyAxis2="sectorTo",sankeyAxis1Label ="From",sankeyAxis2Label="To",
              sectorToOrder=sectorToOrder, sectorFromOrder=sectorFromOrder,
              removeCols=nonFlowCols, bubbleSize = 10, facet_rows="x", facet_columns="subRegion",ncolrow=4, printFig = T,
              fileName =  fname, dirOutputs=dir, figWidth= figWidth_ix,
              figHeight=figHeight_ix,pdfpng="png")

  dfx_sankey <- dfx%>%dplyr::filter(value!=0, grepl("_all",sectorFrom)) %>%
    dplyr::group_by(supplySector,region,subRegion,x) %>%
    dplyr::mutate(normValue=value/sum(value)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(sectorFrom,region,subRegion,x) %>%
    dplyr::mutate(fromLabel = dplyr::case_when((sum(value) <= 1 & sum(value) >= -1) ~ paste(sectorFrom," ",signif(sum(value),2)," ",units,sep=""),
                                        (sum(value) >= 10 & sum(value) <= -10) ~ paste(sectorFrom," ",round(sum(value),0)," ",units,sep=""),
                                        TRUE ~ paste(sectorFrom," ",round(sum(value),1)," ",units,sep="")),
                  legendLabel = paste(sectorFrom," ",units,sep="")) %>%as.data.frame() %>%
    dplyr::ungroup();dfx_sankey

  sectorFromOrder <- sort(unique(dfx_sankey$sectorFrom)); sectorFromOrder
  sectorToOrder <-
    c(sort(unique(dfx_sankey$sectorTo)[unique(dfx_sankey$sectorTo) %in% unique(dfx_sankey$supplySubSector)]),
      sort(unique(dfx_sankey$sectorTo)[!unique(c(dfx_sankey$sectorTo)) %in% c(unique(dfx_sankey$supplySubSector),"export",nonFlowCols)]),
      "export",nonFlowCols); sectorToOrder


  sectorFromOrderSankey <- sectorFromOrder[sectorFromOrder %in% unique(dfx_sankey$sectorFrom)]; sectorFromOrderSankey
  sectorToOrderSankey <- sectorToOrder[sectorToOrder %in% unique(dfx_sankey$sectorTo)]; sectorToOrderSankey

  dfx_sankey$sectorFrom <- factor( as.character(dfx_sankey$sectorFrom), levels=sectorFromOrderSankey )
  dfx_sankey$sectorTo <- factor( as.character(dfx_sankey$sectorTo), levels=sectorToOrderSankey )


  fname = paste("sankeyAll2Total_",scenario_i,nameAppend,"_Free",sep="")
  plotx<-dfx_sankey
  figWidth_ix <- 1*figWidth_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));figWidth_ix
  figHeight_ix <- 1*figHeight_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));  figHeight_ix
  metis.chart(data=plotx, chartType="sankey",  xData="sectorTo", yData="normValue", sankeyGroupColor="supplySector",
              classLabel="From", class = "supplySector", classPalette = "pal_sankey",
              sankeyAxis1="fromLabel",sankeyAxis2="sectorTo",sankeyAxis1Label ="From",sankeyAxis2Label="To",
              sectorToOrder=sectorToOrder, sectorFromOrder=sectorFromOrder,
              removeCols=nonFlowCols, bubbleSize = 10, facet_rows="x", facet_columns="subRegion",ncolrow=4, printFig = T,
              fileName =  fname, dirOutputs=dir, figWidth= figWidth_ix,
              figHeight=figHeight_ix,pdfpng="png")

  }


    } # Close if more than one subRegion loop

#----------------
# By SubRegion
#---------------

  for(subRegion_i in subRegions){

    if (!dir.exists(paste(dirOutputs, "/IO/",region_i,"/",scenario_i,"/",subRegion_i, sep = ""))){
      dir.create(paste(dirOutputs, "/IO/",region_i,"/",scenario_i,"/",subRegion_i,  sep = ""))}

    dir<- paste(dirOutputs, "/IO/",region_i,"/",scenario_i,"/",subRegion_i,  sep = "")

    #---------------------
    # sol_Output
    #---------------------


    # A Intensity Matrix

    A_mat <- sol_list$A_Output %>%
      dplyr::filter(region==region_i, subRegion==subRegion_i,scenario==scenario_i); A_mat

    A_matx <- A_mat %>%
      tidyr::gather(-c("supplySubSector",addedColumns[addedColumns %in% names(A_mat)]),key="sectorTo",value="value") %>%
      dplyr::rename (sectorFrom=supplySubSector) %>%
      dplyr::arrange(sectorFrom) %>%
      dplyr::filter(!is.nan(value),!is.infinite(value),value!=0, !is.na(value)); A_matx

    sectorFromOrder <- sort(unique(A_matx$sectorFrom)); sectorFromOrder
    sectorToOrder <-  sort(unique(A_matx$sectorTo)); sectorToOrder

    if(nrow(A_matx)>0){

      fname = paste("A_sub_",scenario_i,nameAppend,sep="")
      plotx<-A_matx
      figWidth_ix <- 1*figWidth_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));figWidth_ix
      figHeight_ix <- 1*figHeight_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));  figHeight_ix
      metis.chart(data=plotx, chartType="bubble", xData="sectorTo", yData="sectorFrom", classLabel="classLabel1",
                  xLabel="sector To", yLabel="sector From", sectorToOrder=sectorToOrder, sectorFromOrder=sectorFromOrder,
                  removeCols=nonFlowCols, bubbleSize = 10, facet_rows="x", facet_columns="subRegion",ncolrow=4, printFig = T,
                  fileName =  fname, dirOutputs=dir, figWidth= figWidth_ix,
                  figHeight=figHeight_ix,pdfpng="png")

      # data=A_matx; chartType="bubble"; xData="sectorTo"; yData="sectorFrom"; classLabel="classLabel1";
      # xLabel="sector To"; yLabel="sector From"; labelTextSize=5; sectorToOrder=sectorToOrder; sectorFromOrder=sectorFromOrder;
      # removeCols=nonFlowCols; bubbleSize = 10; facet_rows="x"; facet_columns="subRegion";ncolrow=4; printFig = T;
      # fileName =  fname; dirOutputs=dir; figWidth= figWidth_ix;
      # figHeight=figHeight_ix;pdfpng="png"

    }


    A_matxAgg <- A_matx %>% dplyr::filter(grepl("_all",sectorFrom) & grepl("_all",sectorTo)); A_matxAgg %>% as.data.frame()
    sectorFromOrder <- sort(unique(A_matxAgg $sectorFrom)); sectorFromOrder
    sectorToOrder <-  sort(unique(A_matxAgg $sectorTo)); sectorToOrder


    if(nrow(A_matxAgg )>0){

      fname = paste("A_agg_",scenario_i,"_",subRegion_i,nameAppend,sep="")
      plotx<-A_matxAgg
      figWidth_ix <- 1*figWidth_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));figWidth_ix
      figHeight_ix <- 1*figHeight_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));  figHeight_ix
      metis.chart(data=plotx, chartType="bubble", xData="sectorTo", yData="sectorFrom", classLabel="classLabel1",
                  xLabel="sector To", yLabel="sector From", sectorToOrder=sectorToOrder, sectorFromOrder=sectorFromOrder,
                  removeCols=nonFlowCols, bubbleSize = 10, facet_rows="x", facet_columns="subRegion",ncolrow=4, printFig = T,
                  fileName =  fname, dirOutputs=dir, figWidth= figWidth_ix,
                  figHeight=figHeight_ix,pdfpng="png")

    }



    # ioTable normalized

    sol<- sol_list$ioTbl_Output %>%
      dplyr::filter(scenario==scenario_i,region==region_i, subRegion==subRegion_i); sol


    df_Mn<-sol %>%
      dplyr::mutate(totalTemp=total) %>% # to move total to end to include columns on right side of total in the normalization
      dplyr::mutate_at(dplyr::vars(-c("supplySubSector","supplySector",addedColumns[addedColumns %in% names(sol)])),dplyr::funs(./totalTemp)) %>%
      dplyr::select(-totalTemp); df_Mn

    solx <- sol %>%
      tidyr::gather(-c("supplySubSector","supplySector",addedColumns[addedColumns %in% names(df_Mn)]),key="sectorTo",value="value") %>%
      dplyr::rename (sectorFrom=supplySubSector) %>%
      dplyr::mutate(sectorToAgg = sub("_[^_]*$", "", sectorTo)) %>%
      dplyr::arrange(sectorFrom)  %>%
      dplyr::filter(!is.nan(value),value!=0, !is.na(value));solx


    df_Mnx <- df_Mn %>%
      tidyr::gather(-c("supplySubSector","supplySector",addedColumns[addedColumns %in% names(df_Mn)]),key="sectorTo",value="value") %>%
      dplyr::rename (sectorFrom=supplySubSector) %>%
      dplyr::mutate(sectorToAgg = sub("_[^_]*$", "", sectorTo)) %>%
      dplyr::arrange(sectorFrom) %>%
      dplyr::filter(!is.nan(value),!is.infinite(value),value!=0, !is.na(value)); df_Mnx

    sectorFromOrder <- sort(unique(df_Mnx$sectorFrom)); sectorFromOrder
    sectorToOrder <-
      c(sort(unique(df_Mnx$sectorTo)[unique(df_Mnx$sectorTo) %in% unique(sol$supplySubSector)]),
        sort(unique(df_Mnx$sectorTo)[!unique(c(df_Mnx$sectorTo)) %in% c(unique(sol$supplySubSector),"export",nonFlowCols)]),
        "export",nonFlowCols); sectorToOrder

    # ioTable normalized bubbles
    if(nrow(df_Mnx)>0){
      fname = paste("ioNORM_",scenario_i,"_",subRegion_i,nameAppend,sep="")
      plotx<-df_Mnx
      figWidth_ix <- 1*figWidth_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));figWidth_ix
      figHeight_ix <- 1*figHeight_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));  figHeight_ix
      metis.chart(data=plotx, chartType="bubble", xData="sectorTo", yData="sectorFrom", classLabel="classLabel1",
                  xLabel="sector To", yLabel="sector From", sectorToOrder=sectorToOrder, sectorFromOrder=sectorFromOrder,
                  removeCols=nonFlowCols, bubbleSize = 10, facet_rows="x", facet_columns="subRegion",ncolrow=4, printFig = T,
                  fileName =  fname, dirOutputs=dir, figWidth= figWidth_ix,
                  figHeight=figHeight_ix,pdfpng="png")

      # ioTable absolute bubbles
      if(nrow(solx)>0){
        fname = paste("ioABS_",scenario_i,"_",subRegion_i,nameAppend,sep="")
        plotx<-solx
        figWidth_ix <- 1*figWidth_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));figWidth_ix
        figHeight_ix <- 1*figHeight_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));  figHeight_ix
        metis.chart(data=plotx,dataNorm=df_Mnx, chartType="bubble", xData="sectorTo", yData="sectorFrom", classLabel="classLabel1",
                    xLabel="sector To", yLabel="sector From", sectorToOrder=sectorToOrder, sectorFromOrder=sectorFromOrder,
                    removeCols=nonFlowCols, bubbleSize = 10, facet_rows="x", facet_columns="subRegion",ncolrow=4, printFig = T,
                    fileName =  fname, dirOutputs=dir, figWidth= figWidth_ix,
                    figHeight=figHeight_ix,pdfpng="png")
      }
    }


    df_Mnx_AggDem <- df_Mnx %>% dplyr::mutate(sectorTo=sectorToAgg) %>% dplyr::select(-sectorToAgg) %>%
      dplyr::group_by(sectorTo,sectorFrom,supplySector,scenario,x,region,subRegion,units,param) %>% dplyr::summarize(value=sum(value)) %>% dplyr::ungroup();
    df_Mnx_AggDem %>% as.data.frame()
    solx_AggDem <- solx %>% dplyr::mutate(sectorTo=sectorToAgg) %>% dplyr::select(-sectorToAgg) %>%
      dplyr::group_by(sectorTo,sectorFrom,supplySector,scenario,x,region,subRegion,units,param) %>% dplyr::summarize(value=sum(value)) %>% dplyr::ungroup();
    solx_AggDem %>% as.data.frame()

    sectorFromOrder <- sort(unique(df_Mnx_AggDem$sectorFrom)); sectorFromOrder
    sectorToOrder <-
      c(sort(unique(df_Mnx_AggDem$sectorTo)[unique(df_Mnx_AggDem$sectorTo) %in% unique(df_Mnx_AggDem$supplySubSector)]),
        sort(unique(df_Mnx_AggDem$sectorTo)[!unique(c(df_Mnx_AggDem$sectorTo)) %in% c(unique(df_Mnx_AggDem$supplySubSector),"export",nonFlowCols)]),
        "export",nonFlowCols); sectorToOrder

    # ioTable normalized bubbles
    if(nrow(df_Mnx_AggDem)>0){
      fname = paste("ioNORM_aggDem_",scenario_i,"_",subRegion_i,nameAppend,sep="")
      plotx<-df_Mnx_AggDem
      figWidth_ix <- 1*figWidth_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));figWidth_ix
      figHeight_ix <- 1*figHeight_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));  figHeight_ix
      metis.chart(data=plotx, chartType="bubble", xData="sectorTo", yData="sectorFrom", classLabel="classLabel1",
                  xLabel="sector To", yLabel="sector From", sectorToOrder=sectorToOrder, sectorFromOrder=sectorFromOrder,
                  removeCols=nonFlowCols, bubbleSize = 10, facet_rows="x", facet_columns="subRegion",ncolrow=4, printFig = T,
                  fileName =  fname, dirOutputs=dir, figWidth= figWidth_ix,
                  figHeight=figHeight_ix,pdfpng="png")

      # ioTable absolute bubbles
      if(nrow(solx_AggDem)>0){
        fname = paste("ioABS_aggDem_",scenario_i,"_",subRegion_i,nameAppend,sep="")
        plotx<-solx_AggDem
        figWidth_ix <- 1*figWidth_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));figWidth_ix
        figHeight_ix <- 1*figHeight_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));  figHeight_ix
        metis.chart(data=plotx, dataNorm=df_Mnx_AggDem, chartType="bubble", xData="sectorTo", yData="sectorFrom", classLabel="classLabel1",
                    xLabel="sector To", yLabel="sector From", sectorToOrder=sectorToOrder, sectorFromOrder=sectorFromOrder,
                    removeCols=nonFlowCols, bubbleSize = 10, facet_rows="x", facet_columns="subRegion",ncolrow=4, printFig = T,
                    fileName =  fname, dirOutputs=dir, figWidth= figWidth_ix,
                    figHeight=figHeight_ix,pdfpng="png")
      }}



    df_Mnx_AggDemAggSup <- df_Mnx %>% dplyr::mutate(sectorTo=sectorToAgg) %>% dplyr::select(-sectorToAgg) %>%
      dplyr::group_by(sectorTo,sectorFrom,supplySector,scenario,x,region,subRegion,units,param) %>% dplyr::summarize(value=sum(value)) %>%
      dplyr::ungroup() %>% dplyr::filter(grepl("_all",sectorFrom));
    df_Mnx_AggDemAggSup %>% as.data.frame()
    solx_AggDemAggSup <- solx %>% dplyr::mutate(sectorTo=sectorToAgg) %>% dplyr::select(-sectorToAgg) %>%
      dplyr::group_by(sectorTo,sectorFrom,supplySector,scenario,x,region,subRegion,units,param) %>% dplyr::summarize(value=sum(value)) %>%
      dplyr::ungroup() %>% dplyr::filter(grepl("_all",sectorFrom));
    solx_AggDemAggSup %>% as.data.frame()

    sectorFromOrder <- sort(unique(df_Mnx_AggDemAggSup$sectorFrom)); sectorFromOrder
    sectorToOrder <-
      c(sort(unique(df_Mnx_AggDemAggSup$sectorTo)[unique(df_Mnx_AggDemAggSup$sectorTo) %in% unique(df_Mnx_AggDemAggSup$supplySubSector)]),
        sort(unique(df_Mnx_AggDemAggSup$sectorTo)[!unique(c(df_Mnx_AggDemAggSup$sectorTo)) %in% c(unique(df_Mnx_AggDemAggSup$supplySubSector),"export",nonFlowCols)]),
        "export",nonFlowCols); sectorToOrder

    # ioTable normalized bubbles
    if(nrow(df_Mnx_AggDemAggSup)>0){
      fname = paste("ioNORM_aggDemaggSup_",scenario_i,"_",subRegion_i,nameAppend,sep="")
      plotx<-df_Mnx_AggDemAggSup
      figWidth_ix <- 1*figWidth_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));figWidth_ix
      figHeight_ix <- 1*figHeight_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));  figHeight_ix
      metis.chart(data=plotx, chartType="bubble", xData="sectorTo", yData="sectorFrom", classLabel="classLabel1",
                  xLabel="sector To", yLabel="sector From", sectorToOrder=sectorToOrder, sectorFromOrder=sectorFromOrder,
                  removeCols=nonFlowCols, bubbleSize = 10, facet_rows="x", facet_columns="subRegion",ncolrow=4, printFig = T,
                  fileName =  fname, dirOutputs=dir, figWidth= figWidth_ix,
                  figHeight=figHeight_ix,pdfpng="png")

      # ioTable absolute bubbles
      if(nrow(solx_AggDemAggSup)>0){
        fname = paste("ioABS_aggDemaggSup_",scenario_i,"_",subRegion_i,nameAppend,sep="")
        plotx<-solx_AggDemAggSup
        figWidth_ix <- 1*figWidth_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));figWidth_ix
        figHeight_ix <- 1*figHeight_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));  figHeight_ix
        metis.chart(data=plotx, dataNorm=df_Mnx_AggDemAggSup, chartType="bubble", xData="sectorTo", yData="sectorFrom", classLabel="classLabel1",
                    xLabel="sector To", yLabel="sector From", sectorToOrder=sectorToOrder, sectorFromOrder=sectorFromOrder,
                    removeCols=nonFlowCols, bubbleSize = 10, facet_rows="x", facet_columns="subRegion",ncolrow=4, printFig = T,
                    fileName =  fname, dirOutputs=dir, figWidth= figWidth_ix,
                    figHeight=figHeight_ix,pdfpng="png")
      }}




    #-----------------
    # Sankey
    #--------------

    df <- sol %>%
      dplyr::select(-nonFlowCols); df

    dfx <- df %>%
      tidyr::gather(-c("supplySubSector","supplySector",addedColumns[addedColumns %in% names(df_Mn)]),key="sectorTo",value="value") %>%
      dplyr::rename (sectorFrom=supplySubSector) %>%
      dplyr::filter(value>0) %>%
      unique() %>%
      dplyr::arrange(sectorFrom); dfx

    if(nrow(dfx)>0){

      # Sankey Aggregated to supply subSectors and aggregated demand

      dfx_sankey <- dfx %>%dplyr::filter(value!=0) %>% dplyr::filter(!grepl("_all",sectorFrom)) %>%
        dplyr::mutate(sectorToAgg = sub("_[^_]*$", "", sectorTo)) %>%
        dplyr::mutate(sectorTo=sectorToAgg) %>% dplyr::select(-sectorToAgg) %>%
        dplyr::group_by(sectorTo,sectorFrom,supplySector,scenario,x,region,subRegion,units,param) %>% dplyr::summarize(value=sum(value)) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(supplySector,region) %>%
        dplyr::mutate(normValue=value/sum(value)) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(sectorFrom,region,subRegion,x) %>%
        dplyr::mutate(fromLabel = dplyr::case_when((sum(value) <= 1 & sum(value) >= -1) ~ paste(sectorFrom," ",signif(sum(value),2)," ",units,sep=""),
                                            (sum(value) >= 10 & sum(value) <= -10) ~ paste(sectorFrom," ",round(sum(value),0)," ",units,sep=""),
                                            TRUE ~ paste(sectorFrom," ",round(sum(value),1)," ",units,sep="")),
                      legendLabel = paste(sectorFrom," ",units,sep="")) %>%
        as.data.frame() %>%
        dplyr::ungroup();dfx_sankey

      sectorFromOrder <- sort(unique(dfx_sankey$sectorFrom)); sectorFromOrder
      sectorToOrder <-
        c(sort(unique(dfx_sankey$sectorTo)[unique(dfx_sankey$sectorTo) %in% unique(dfx_sankey$supplySubSector)]),
          sort(unique(dfx_sankey$sectorTo)[!unique(c(dfx_sankey$sectorTo)) %in% c(unique(dfx_sankey$supplySubSector),"export",nonFlowCols)]),
          "export",nonFlowCols); sectorToOrder


      sectorFromOrderSankey <- sectorFromOrder[sectorFromOrder %in% unique(dfx_sankey$sectorFrom)]; sectorFromOrderSankey
      sectorToOrderSankey <- sectorToOrder[sectorToOrder %in% unique(dfx_sankey$sectorTo)]; sectorToOrderSankey

      dfx_sankey$sectorFrom <- factor( as.character(dfx_sankey$sectorFrom), levels=sectorFromOrderSankey )
      dfx_sankey$sectorTo <- factor( as.character(dfx_sankey$sectorTo), levels=sectorToOrderSankey )


      fname = paste("sankeySub_aggDem_",scenario_i,"_",subRegion_i,nameAppend,sep="")
      plotx<-dfx_sankey
      figWidth_ix <- 1*figWidth_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));figWidth_ix
      figHeight_ix <- 1*figHeight_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));  figHeight_ix
      metis.chart(data=plotx, chartType="sankey",  xData="sectorTo", yData="normValue", sankeyGroupColor="supplySector",
                  classLabel="From", class = "supplySector", classPalette = "pal_sankey",
                  sankeyAxis1="fromLabel",sankeyAxis2="sectorTo",sankeyAxis1Label ="From",sankeyAxis2Label="To",
                  sectorToOrder=sectorToOrder, sectorFromOrder=sectorFromOrder,
                  removeCols=nonFlowCols, bubbleSize = 10, facet_rows="x", facet_columns="subRegion",ncolrow=4, printFig = T,
                  fileName =  fname, dirOutputs=dir, figWidth= figWidth_ix,
                  figHeight=figHeight_ix,pdfpng="png", sankeyLabelsOn=sankeyLabelAbsPlots)

      # data=dfx_sankey; chartType="sankey"; xData="sectorTo"; yData="normValue"; sankeyGroupColor="supplySector";
      # classLabel="From"; class = "supplySector"; classPalette = "pal_sankey";
      # sankeyAxis1="fromLabel";sankeyAxis2="sectorTo";sankeyAxis1Label ="From";sankeyAxis2Label="To";
      # labelTextSize=5; sectorToOrder=sectorToOrder; sectorFromOrder=sectorFromOrder;
      # removeCols=nonFlowCols; bubbleSize = 10; facet_rows="x"; facet_columns="subRegion";ncolrow=4; printFig = T;
      # fileName =  fname; dirOutputs=dir; figWidth= figWidth_ix;
      # figHeight=figHeight_ix;pdfpng="png"; sankeyLabelsOn=sankeyLabelAbsPlots

      dfx_sankey <- dfx%>%dplyr::filter(value!=0, !grepl("_all",sectorFrom)) %>%
        dplyr::mutate(sectorToAgg = sub("_[^_]*$", "", sectorTo)) %>%
        dplyr::mutate(sectorTo=sectorToAgg) %>% dplyr::select(-sectorToAgg) %>%
        dplyr::group_by(sectorTo,sectorFrom,supplySector,scenario,x,region,subRegion,units,param) %>% dplyr::summarize(value=sum(value)) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(supplySector,region,subRegion,x) %>%
        dplyr::mutate(normValue=value/sum(value)) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(sectorFrom,region,subRegion,x) %>%
        dplyr::mutate(fromLabel = dplyr::case_when((sum(value) <= 1 & sum(value) >= -1) ~ paste(sectorFrom," ",signif(sum(value),2)," ",units,sep=""),
                                            (sum(value) >= 10 & sum(value) <= -10) ~ paste(sectorFrom," ",round(sum(value),0)," ",units,sep=""),
                                            TRUE ~ paste(sectorFrom," ",round(sum(value),1)," ",units,sep="")),
                      legendLabel = paste(sectorFrom," ",units,sep="")) %>%
        as.data.frame() %>%
        dplyr::ungroup();dfx_sankey

      sectorFromOrder <- sort(unique(dfx_sankey$sectorFrom)); sectorFromOrder
      sectorToOrder <-
        c(sort(unique(dfx_sankey$sectorTo)[unique(dfx_sankey$sectorTo) %in% unique(dfx_sankey$supplySubSector)]),
          sort(unique(dfx_sankey$sectorTo)[!unique(c(dfx_sankey$sectorTo)) %in% c(unique(dfx_sankey$supplySubSector),"export",nonFlowCols)]),
          "export",nonFlowCols); sectorToOrder


      sectorFromOrderSankey <- sectorFromOrder[sectorFromOrder %in% unique(dfx_sankey$sectorFrom)]; sectorFromOrderSankey
      sectorToOrderSankey <- sectorToOrder[sectorToOrder %in% unique(dfx_sankey$sectorTo)]; sectorToOrderSankey

      dfx_sankey$sectorFrom <- factor( as.character(dfx_sankey$sectorFrom), levels=sectorFromOrderSankey )
      dfx_sankey$sectorTo <- factor( as.character(dfx_sankey$sectorTo), levels=sectorToOrderSankey )

      fname = paste("sankeySub_aggDem_",scenario_i,"_",subRegion_i,nameAppend,"_Free",sep="")
      plotx<-dfx_sankey
      figWidth_ix <- 1*figWidth_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));figWidth_ix
      figHeight_ix <- 1*figHeight_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));  figHeight_ix
      metis.chart(data=plotx, chartType="sankey",  xData="sectorTo", yData="normValue", sankeyGroupColor="supplySector",
                  classLabel="From", class = "supplySector", classPalette = "pal_sankey",
                  sankeyAxis1="fromLabel",sankeyAxis2="sectorTo",sankeyAxis1Label ="From",sankeyAxis2Label="To",
                  sectorToOrder=sectorToOrder, sectorFromOrder=sectorFromOrder,
                  removeCols=nonFlowCols, bubbleSize = 10, facet_rows="x", facet_columns="subRegion",ncolrow=4, printFig = T,
                  fileName =  fname, dirOutputs=dir, figWidth= figWidth_ix,
                  figHeight=figHeight_ix,pdfpng="png")


      # Sankey Aggregated to supply sectors and agg Demand

      dfx_sankey <- dfx %>%dplyr::filter(value!=0) %>% dplyr::filter(grepl("_all",sectorFrom)) %>%
        dplyr::mutate(sectorToAgg = sub("_[^_]*$", "", sectorTo)) %>%
        dplyr::mutate(sectorTo=sectorToAgg) %>% dplyr::select(-sectorToAgg) %>%
        dplyr::group_by(sectorTo,sectorFrom,supplySector,scenario,x,region,subRegion,units,param) %>% dplyr::summarize(value=sum(value)) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(supplySector,region) %>%
        dplyr::mutate(normValue=value/sum(value)) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(sectorFrom,region,subRegion,x) %>%
        dplyr::mutate(fromLabel = dplyr::case_when((sum(value) <= 1 & sum(value) >= -1) ~ paste(sectorFrom," ",signif(sum(value),2)," ",units,sep=""),
                                            (sum(value) >= 10 & sum(value) <= -10) ~ paste(sectorFrom," ",round(sum(value),0)," ",units,sep=""),
                                            TRUE ~ paste(sectorFrom," ",round(sum(value),1)," ",units,sep="")),
                      legendLabel = paste(sectorFrom," ",units,sep="")) %>%as.data.frame() %>%
        dplyr::ungroup();dfx_sankey

      sectorFromOrder <- sort(unique(dfx_sankey$sectorFrom)); sectorFromOrder
      sectorToOrder <-
        c(sort(unique(dfx_sankey$sectorTo)[unique(dfx_sankey$sectorTo) %in% unique(dfx_sankey$supplySubSector)]),
          sort(unique(dfx_sankey$sectorTo)[!unique(c(dfx_sankey$sectorTo)) %in% c(unique(dfx_sankey$supplySubSector),"export",nonFlowCols)]),
          "export",nonFlowCols); sectorToOrder


      sectorFromOrderSankey <- sectorFromOrder[sectorFromOrder %in% unique(dfx_sankey$sectorFrom)]; sectorFromOrderSankey
      sectorToOrderSankey <- sectorToOrder[sectorToOrder %in% unique(dfx_sankey$sectorTo)]; sectorToOrderSankey

      dfx_sankey$sectorFrom <- factor( as.character(dfx_sankey$sectorFrom), levels=sectorFromOrderSankey )
      dfx_sankey$sectorTo <- factor( as.character(dfx_sankey$sectorTo), levels=sectorToOrderSankey )

      fname = paste("sankeyAll_aggDem_",scenario_i,"_",subRegion_i,nameAppend,sep="")
      plotx<-dfx_sankey
      figWidth_ix <- 1*figWidth_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));figWidth_ix
      figHeight_ix <- 1*figHeight_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));  figHeight_ix
      metis.chart(data=plotx, chartType="sankey",  xData="sectorTo", yData="normValue", sankeyGroupColor="supplySector",
                  classLabel="From", class = "supplySector", classPalette = "pal_sankey",
                  sankeyAxis1="fromLabel",sankeyAxis2="sectorTo",sankeyAxis1Label ="From",sankeyAxis2Label="To",
                  sectorToOrder=sectorToOrder, sectorFromOrder=sectorFromOrder,
                  removeCols=nonFlowCols, bubbleSize = 10, facet_rows="x", facet_columns="subRegion",ncolrow=4, printFig = T,
                  fileName =  fname, dirOutputs=dir, figWidth= figWidth_ix,
                  figHeight=figHeight_ix,pdfpng="png",sankeyLabelsOn=sankeyLabelAbsPlots)



      dfx_sankey <- dfx%>%dplyr::filter(value!=0, grepl("_all",sectorFrom)) %>%
        dplyr::mutate(sectorToAgg = sub("_[^_]*$", "", sectorTo)) %>%
        dplyr::mutate(sectorTo=sectorToAgg) %>% dplyr::select(-sectorToAgg) %>%
        dplyr::group_by(sectorTo,sectorFrom,supplySector,scenario,x,region,subRegion,units,param) %>% dplyr::summarize(value=sum(value)) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(supplySector,region,subRegion,x) %>%
        dplyr::mutate(normValue=value/sum(value)) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(sectorFrom,region,subRegion,x) %>%
        dplyr::mutate(fromLabel = dplyr::case_when((sum(value) <= 1 & sum(value) >= -1) ~ paste(sectorFrom," ",signif(sum(value),2)," ",units,sep=""),
                                            (sum(value) >= 10 & sum(value) <= -10) ~ paste(sectorFrom," ",round(sum(value),0)," ",units,sep=""),
                                            TRUE ~ paste(sectorFrom," ",round(sum(value),1)," ",units,sep="")),
                      legendLabel = paste(sectorFrom," ",units,sep="")) %>%as.data.frame() %>%
        dplyr::ungroup();dfx_sankey

      sectorFromOrder <- sort(unique(dfx_sankey$sectorFrom)); sectorFromOrder
      sectorToOrder <-
        c(sort(unique(dfx_sankey$sectorTo)[unique(dfx_sankey$sectorTo) %in% unique(dfx_sankey$supplySubSector)]),
          sort(unique(dfx_sankey$sectorTo)[!unique(c(dfx_sankey$sectorTo)) %in% c(unique(dfx_sankey$supplySubSector),"export",nonFlowCols)]),
          "export",nonFlowCols); sectorToOrder


      sectorFromOrderSankey <- sectorFromOrder[sectorFromOrder %in% unique(dfx_sankey$sectorFrom)]; sectorFromOrderSankey
      sectorToOrderSankey <- sectorToOrder[sectorToOrder %in% unique(dfx_sankey$sectorTo)]; sectorToOrderSankey

      dfx_sankey$sectorFrom <- factor( as.character(dfx_sankey$sectorFrom), levels=sectorFromOrderSankey )
      dfx_sankey$sectorTo <- factor( as.character(dfx_sankey$sectorTo), levels=sectorToOrderSankey )

      fname = paste("sankeyAll_aggDem_",scenario_i,"_",subRegion_i,nameAppend,"_FREE",sep="")
      plotx<-dfx_sankey
      figWidth_ix <- 1*figWidth_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));figWidth_ix
      figHeight_ix <- 1*figHeight_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));  figHeight_ix
      metis.chart(data=plotx, chartType="sankey",  xData="sectorTo", yData="normValue", sankeyGroupColor="supplySector",
                  classLabel="From", class = "supplySector", classPalette = "pal_sankey",
                  sankeyAxis1="fromLabel",sankeyAxis2="sectorTo",sankeyAxis1Label ="From",sankeyAxis2Label="To",
                  sectorToOrder=sectorToOrder, sectorFromOrder=sectorFromOrder,
                  removeCols=nonFlowCols, bubbleSize = 10, facet_rows="x", facet_columns="subRegion",ncolrow=4, printFig = T,
                  fileName =  fname, dirOutputs=dir, figWidth= figWidth_ix,
                  figHeight=figHeight_ix,pdfpng="png")

    }


    #-------- Total Demands

    solFlows <- sol %>%
      dplyr::select(!!c("supplySubSector",names(sol)[names(sol) %in% c("total","supplySector",addedColumns)]))
    df <- solFlows;df

    dfx <- df %>%
      tidyr::gather(-c("supplySubSector","supplySector",addedColumns[addedColumns %in% names(df_Mn)]),key="sectorTo",value="value") %>%
      dplyr::rename (sectorFrom=supplySubSector) %>%
      dplyr::filter(value>0) %>%
      unique() %>%
      dplyr::arrange(sectorFrom); dfx

    if(nrow(dfx)>0){

      # Subsupply to Agg Demands

      dfx_sankey <- dfx%>%dplyr::filter(value!=0, !grepl("_all",sectorFrom)) %>%
        dplyr::group_by(supplySector,region) %>%
        dplyr::mutate(normValue=value/sum(value)) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(sectorFrom,region,subRegion,x) %>%
        dplyr::mutate(fromLabel = dplyr::case_when((sum(value) <= 1 & sum(value) >= -1) ~ paste(sectorFrom," ",signif(sum(value),2)," ",units,sep=""),
                                            (sum(value) >= 10 & sum(value) <= -10) ~ paste(sectorFrom," ",round(sum(value),0)," ",units,sep=""),
                                            TRUE ~ paste(sectorFrom," ",round(sum(value),1)," ",units,sep="")),
                      legendLabel = paste(sectorFrom," ",units,sep="")) %>%as.data.frame() %>%
        dplyr::ungroup();dfx_sankey

      sectorFromOrder <- sort(unique(dfx_sankey$sectorFrom)); sectorFromOrder
      sectorToOrder <-
        c(sort(unique(dfx_sankey$sectorTo)[unique(dfx_sankey$sectorTo) %in% unique(dfx_sankey$supplySubSector)]),
          sort(unique(dfx_sankey$sectorTo)[!unique(c(dfx_sankey$sectorTo)) %in% c(unique(dfx_sankey$supplySubSector),"export",nonFlowCols)]),
          "export",nonFlowCols); sectorToOrder


      sectorFromOrderSankey <- sectorFromOrder[sectorFromOrder %in% unique(dfx_sankey$sectorFrom)]; sectorFromOrderSankey
      sectorToOrderSankey <- sectorToOrder[sectorToOrder %in% unique(dfx_sankey$sectorTo)]; sectorToOrderSankey

      dfx_sankey$sectorFrom <- factor( as.character(dfx_sankey$sectorFrom), levels=sectorFromOrderSankey )
      dfx_sankey$sectorTo <- factor( as.character(dfx_sankey$sectorTo), levels=sectorToOrderSankey )


      fname = paste("sankeySub2Total_",scenario_i,"_",subRegion_i,nameAppend,sep="")
      plotx<-dfx_sankey
      figWidth_ix <- 1*figWidth_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));figWidth_ix
      figHeight_ix <- 1*figHeight_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));  figHeight_ix
      metis.chart(data=plotx, chartType="sankey",  xData="sectorTo", yData="normValue", sankeyGroupColor="supplySector",
                  classLabel="From", class = "supplySector", classPalette = "pal_sankey",
                  sankeyAxis1="fromLabel",sankeyAxis2="sectorTo",sankeyAxis1Label ="From",sankeyAxis2Label="To",
                  sectorToOrder=sectorToOrder, sectorFromOrder=sectorFromOrder,
                  removeCols=nonFlowCols, bubbleSize = 10, facet_rows="x", facet_columns="subRegion",ncolrow=4, printFig = T,
                  fileName =  fname, dirOutputs=dir, figWidth= figWidth_ix,
                  figHeight=figHeight_ix,pdfpng="png", sankeyLabelsOn=sankeyLabelAbsPlots)


      dfx_sankey <- dfx%>%dplyr::filter(value!=0, !grepl("_all",sectorFrom)) %>%
        dplyr::group_by(supplySector,region,subRegion,x) %>%
        dplyr::mutate(normValue=value/sum(value)) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(sectorFrom,region,subRegion,x) %>%
        dplyr::mutate(fromLabel = dplyr::case_when((sum(value) <= 1 & sum(value) >= -1) ~ paste(sectorFrom," ",signif(sum(value),2)," ",units,sep=""),
                                            (sum(value) >= 10 & sum(value) <= -10) ~ paste(sectorFrom," ",round(sum(value),0)," ",units,sep=""),
                                            TRUE ~ paste(sectorFrom," ",round(sum(value),1)," ",units,sep="")),
                      legendLabel = paste(sectorFrom," ",units,sep="")) %>%as.data.frame() %>%
        dplyr::ungroup();dfx_sankey

      sectorFromOrder <- sort(unique(dfx_sankey$sectorFrom)); sectorFromOrder
      sectorToOrder <-
        c(sort(unique(dfx_sankey$sectorTo)[unique(dfx_sankey$sectorTo) %in% unique(dfx_sankey$supplySubSector)]),
          sort(unique(dfx_sankey$sectorTo)[!unique(c(dfx_sankey$sectorTo)) %in% c(unique(dfx_sankey$supplySubSector),"export",nonFlowCols)]),
          "export",nonFlowCols); sectorToOrder


      sectorFromOrderSankey <- sectorFromOrder[sectorFromOrder %in% unique(dfx_sankey$sectorFrom)]; sectorFromOrderSankey
      sectorToOrderSankey <- sectorToOrder[sectorToOrder %in% unique(dfx_sankey$sectorTo)]; sectorToOrderSankey

      dfx_sankey$sectorFrom <- factor( as.character(dfx_sankey$sectorFrom), levels=sectorFromOrderSankey )
      dfx_sankey$sectorTo <- factor( as.character(dfx_sankey$sectorTo), levels=sectorToOrderSankey )


      fname = paste("sankeySub2Total_",scenario_i,"_",subRegion_i,nameAppend,"_FREE",sep="")
      plotx<-dfx_sankey
      figWidth_ix <- 1*figWidth_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));figWidth_ix
      figHeight_ix <- 1*figHeight_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));  figHeight_ix
      metis.chart(data=plotx, chartType="sankey",  xData="sectorTo", yData="normValue", sankeyGroupColor="supplySector",
                  classLabel="From", class = "supplySector", classPalette = "pal_sankey",
                  sankeyAxis1="fromLabel",sankeyAxis2="sectorTo",sankeyAxis1Label ="From",sankeyAxis2Label="To",
                  sectorToOrder=sectorToOrder, sectorFromOrder=sectorFromOrder,
                  removeCols=nonFlowCols, bubbleSize = 10, facet_rows="x", facet_columns="subRegion",ncolrow=4, printFig = T,
                  fileName =  fname, dirOutputs=dir, figWidth= figWidth_ix,
                  figHeight=figHeight_ix,pdfpng="png")


      # SubSupply to Total Demands

      dfx_sankey <- dfx%>%dplyr::filter(value!=0, !grepl("_all",sectorFrom)) %>%
        dplyr::group_by(supplySector,region) %>%
        dplyr::mutate(normValue=value/sum(value)) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(sectorFrom,region,subRegion,x) %>%
        dplyr::mutate(fromLabel = dplyr::case_when((sum(value) <= 1 & sum(value) >= -1) ~ paste(sectorFrom," ",signif(sum(value),2)," ",units,sep=""),
                                            (sum(value) >= 10 & sum(value) <= -10) ~ paste(sectorFrom," ",round(sum(value),0)," ",units,sep=""),
                                            TRUE ~ paste(sectorFrom," ",round(sum(value),1)," ",units,sep="")),
                      legendLabel = paste(sectorFrom," ",units,sep="")) %>% as.data.frame() %>%
        dplyr::ungroup();dfx_sankey

      sectorFromOrder <- sort(unique(dfx_sankey$sectorFrom)); sectorFromOrder
      sectorToOrder <-
        c(sort(unique(dfx_sankey$sectorTo)[unique(dfx_sankey$sectorTo) %in% unique(dfx_sankey$supplySubSector)]),
          sort(unique(dfx_sankey$sectorTo)[!unique(c(dfx_sankey$sectorTo)) %in% c(unique(dfx_sankey$supplySubSector),"export",nonFlowCols)]),
          "export",nonFlowCols); sectorToOrder


      sectorFromOrderSankey <- sectorFromOrder[sectorFromOrder %in% unique(dfx_sankey$sectorFrom)]; sectorFromOrderSankey
      sectorToOrderSankey <- sectorToOrder[sectorToOrder %in% unique(dfx_sankey$sectorTo)]; sectorToOrderSankey

      dfx_sankey$sectorFrom <- factor( as.character(dfx_sankey$sectorFrom), levels=sectorFromOrderSankey )
      dfx_sankey$sectorTo <- factor( as.character(dfx_sankey$sectorTo), levels=sectorToOrderSankey )


      fname = paste("sankeySub2Total_",scenario_i,"_",subRegion_i,nameAppend,sep="")
      plotx<-dfx_sankey
      figWidth_ix <- 1*figWidth_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));figWidth_ix
      figHeight_ix <- 1*figHeight_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));  figHeight_ix
      metis.chart(data=plotx, chartType="sankey",  xData="sectorTo", yData="normValue", sankeyGroupColor="supplySector",
                  classLabel="From", class = "supplySector", classPalette = "pal_sankey",
                  sankeyAxis1="fromLabel",sankeyAxis2="sectorTo",sankeyAxis1Label ="From",sankeyAxis2Label="To",
                  sectorToOrder=sectorToOrder, sectorFromOrder=sectorFromOrder,
                  removeCols=nonFlowCols, bubbleSize = 10, facet_rows="x", facet_columns="subRegion",ncolrow=4, printFig = T,
                  fileName =  fname, dirOutputs=dir, figWidth= figWidth_ix,
                  figHeight=figHeight_ix,pdfpng="png", sankeyLabelsOn=sankeyLabelAbsPlots)

      dfx_sankey <- dfx%>%dplyr::filter(value!=0, !grepl("_all",sectorFrom)) %>%
        dplyr::group_by(supplySector,region,subRegion,x) %>%
        dplyr::mutate(normValue=value/sum(value)) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(sectorFrom,region,subRegion,x) %>%
        dplyr::mutate(fromLabel = dplyr::case_when((sum(value) <= 1 & sum(value) >= -1) ~ paste(sectorFrom," ",signif(sum(value),2)," ",units,sep=""),
                                            (sum(value) >= 10 & sum(value) <= -10) ~ paste(sectorFrom," ",round(sum(value),0)," ",units,sep=""),
                                            TRUE ~ paste(sectorFrom," ",round(sum(value),1)," ",units,sep="")),
                      legendLabel = paste(sectorFrom," ",units,sep="")) %>%as.data.frame() %>%
        dplyr::ungroup();dfx_sankey

      sectorFromOrder <- sort(unique(dfx_sankey$sectorFrom)); sectorFromOrder
      sectorToOrder <-
        c(sort(unique(dfx_sankey$sectorTo)[unique(dfx_sankey$sectorTo) %in% unique(dfx_sankey$supplySubSector)]),
          sort(unique(dfx_sankey$sectorTo)[!unique(c(dfx_sankey$sectorTo)) %in% c(unique(dfx_sankey$supplySubSector),"export",nonFlowCols)]),
          "export",nonFlowCols); sectorToOrder


      sectorFromOrderSankey <- sectorFromOrder[sectorFromOrder %in% unique(dfx_sankey$sectorFrom)]; sectorFromOrderSankey
      sectorToOrderSankey <- sectorToOrder[sectorToOrder %in% unique(dfx_sankey$sectorTo)]; sectorToOrderSankey

      dfx_sankey$sectorFrom <- factor( as.character(dfx_sankey$sectorFrom), levels=sectorFromOrderSankey )
      dfx_sankey$sectorTo <- factor( as.character(dfx_sankey$sectorTo), levels=sectorToOrderSankey )

      fname = paste("sankeySub2Total_",scenario_i,"_",subRegion_i,nameAppend,"_FREE",sep="")
      plotx<-dfx_sankey
      figWidth_ix <- 1*figWidth_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));figWidth_ix
      figHeight_ix <- 1*figHeight_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));  figHeight_ix
      metis.chart(data=plotx, chartType="sankey",  xData="sectorTo", yData="normValue", sankeyGroupColor="supplySector",
                  classLabel="From", class = "supplySector", classPalette = "pal_sankey",
                  sankeyAxis1="fromLabel",sankeyAxis2="sectorTo",sankeyAxis1Label ="From",sankeyAxis2Label="To",
                  sectorToOrder=sectorToOrder, sectorFromOrder=sectorFromOrder,
                  removeCols=nonFlowCols, bubbleSize = 10, facet_rows="x", facet_columns="subRegion",ncolrow=4, printFig = T,
                  fileName =  fname, dirOutputs=dir, figWidth= figWidth_ix,
                  figHeight=figHeight_ix,pdfpng="png")



      dfx_sankey <- dfx%>%dplyr::filter(value!=0, grepl("_all",sectorFrom)) %>%
        dplyr::group_by(supplySector,region) %>%
        dplyr::mutate(normValue=value/sum(value)) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(sectorFrom,region,subRegion,x) %>%
        dplyr::mutate(fromLabel = dplyr::case_when((sum(value) <= 1 & sum(value) >= -1) ~ paste(sectorFrom," ",signif(sum(value),2)," ",units,sep=""),
                                            (sum(value) >= 10 & sum(value) <= -10) ~ paste(sectorFrom," ",round(sum(value),0)," ",units,sep=""),
                                            TRUE ~ paste(sectorFrom," ",round(sum(value),1)," ",units,sep="")),
                      legendLabel = paste(sectorFrom," ",units,sep="")) %>%as.data.frame() %>%
        dplyr::ungroup();dfx_sankey

      sectorFromOrder <- sort(unique(dfx_sankey$sectorFrom)); sectorFromOrder
      sectorToOrder <-
        c(sort(unique(dfx_sankey$sectorTo)[unique(dfx_sankey$sectorTo) %in% unique(dfx_sankey$supplySubSector)]),
          sort(unique(dfx_sankey$sectorTo)[!unique(c(dfx_sankey$sectorTo)) %in% c(unique(dfx_sankey$supplySubSector),"export",nonFlowCols)]),
          "export",nonFlowCols); sectorToOrder


      sectorFromOrderSankey <- sectorFromOrder[sectorFromOrder %in% unique(dfx_sankey$sectorFrom)]; sectorFromOrderSankey
      sectorToOrderSankey <- sectorToOrder[sectorToOrder %in% unique(dfx_sankey$sectorTo)]; sectorToOrderSankey

      dfx_sankey$sectorFrom <- factor( as.character(dfx_sankey$sectorFrom), levels=sectorFromOrderSankey )
      dfx_sankey$sectorTo <- factor( as.character(dfx_sankey$sectorTo), levels=sectorToOrderSankey )


      fname = paste("sankeyAll2Total_",scenario_i,"_",subRegion_i,nameAppend,sep="")
      plotx<-dfx_sankey
      figWidth_ix <- 1*figWidth_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));figWidth_ix
      figHeight_ix <- 1*figHeight_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));  figHeight_ix
      metis.chart(data=plotx, chartType="sankey",  xData="sectorTo", yData="normValue", sankeyGroupColor="supplySector",
                  classLabel="From", class = "supplySector", classPalette = "pal_sankey",
                  sankeyAxis1="fromLabel",sankeyAxis2="sectorTo",sankeyAxis1Label ="From",sankeyAxis2Label="To",
                  sectorToOrder=sectorToOrder, sectorFromOrder=sectorFromOrder,
                  removeCols=nonFlowCols, bubbleSize = 10, facet_rows="x", facet_columns="subRegion",ncolrow=4, printFig = T,
                  fileName =  fname, dirOutputs=dir, figWidth= figWidth_ix,
                  figHeight=figHeight_ix,pdfpng="png")

      dfx_sankey <- dfx%>%dplyr::filter(value!=0, grepl("_all",sectorFrom)) %>%
        dplyr::group_by(supplySector,region,subRegion,x) %>%
        dplyr::mutate(normValue=value/sum(value)) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(sectorFrom,region,subRegion,x) %>%
        dplyr::mutate(fromLabel = dplyr::case_when((sum(value) <= 1 & sum(value) >= -1) ~ paste(sectorFrom," ",signif(sum(value),2)," ",units,sep=""),
                                            (sum(value) >= 10 & sum(value) <= -10) ~ paste(sectorFrom," ",round(sum(value),0)," ",units,sep=""),
                                            TRUE ~ paste(sectorFrom," ",round(sum(value),1)," ",units,sep="")),
                      legendLabel = paste(sectorFrom," ",units,sep="")) %>%as.data.frame() %>%
        dplyr::ungroup();dfx_sankey

      sectorFromOrder <- sort(unique(dfx_sankey$sectorFrom)); sectorFromOrder
      sectorToOrder <-
        c(sort(unique(dfx_sankey$sectorTo)[unique(dfx_sankey$sectorTo) %in% unique(dfx_sankey$supplySubSector)]),
          sort(unique(dfx_sankey$sectorTo)[!unique(c(dfx_sankey$sectorTo)) %in% c(unique(dfx_sankey$supplySubSector),"export",nonFlowCols)]),
          "export",nonFlowCols); sectorToOrder


      sectorFromOrderSankey <- sectorFromOrder[sectorFromOrder %in% unique(dfx_sankey$sectorFrom)]; sectorFromOrderSankey
      sectorToOrderSankey <- sectorToOrder[sectorToOrder %in% unique(dfx_sankey$sectorTo)]; sectorToOrderSankey

      dfx_sankey$sectorFrom <- factor( as.character(dfx_sankey$sectorFrom), levels=sectorFromOrderSankey )
      dfx_sankey$sectorTo <- factor( as.character(dfx_sankey$sectorTo), levels=sectorToOrderSankey )


      fname = paste("sankeyAll2Total_",scenario_i,"_",subRegion_i,nameAppend,"_Free",sep="")
      plotx<-dfx_sankey
      figWidth_ix <- 1*figWidth_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));figWidth_ix
      figHeight_ix <- 1*figHeight_i*min(3,max(length(unique(plotx$subRegion)),length(unique(plotx$x))));  figHeight_ix
      metis.chart(data=plotx, chartType="sankey",  xData="sectorTo", yData="normValue", sankeyGroupColor="supplySector",
                  classLabel="From", class = "supplySector", classPalette = "pal_sankey",
                  sankeyAxis1="fromLabel",sankeyAxis2="sectorTo",sankeyAxis1Label ="From",sankeyAxis2Label="To",
                  sectorToOrder=sectorToOrder, sectorFromOrder=sectorFromOrder,
                  removeCols=nonFlowCols, bubbleSize = 10, facet_rows="x", facet_columns="subRegion",ncolrow=4, printFig = T,
                  fileName =  fname, dirOutputs=dir, figWidth= figWidth_ix,
                  figHeight=figHeight_ix,pdfpng="png")

    }





  } # Close subRegion_i


       # } # Close Year_i
    } # Close scenario_i
   } # close region_i



  return(sol_list)

} # Close Function

