#' metis.io
#'
#' This function prepares gridded data for use with domestic metis modules.
#' @param dirOutputs Default =paste(getwd(),"/outputs",sep=""),
#' @param ioTable0 Initial ioTable. Must have columns: supplySubSector,total,export and cap. Each supply sector should also have imports. Default = NULL,
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
# Defaults:
#----------------

  # The default values should be updated based on local assumptions or GCAM outputs for each region and year
  # The deaults below are applied to all regions and time periods until they are replaced.


  # Default intensity
  A_default0 =tibble::tribble( # Initial total demand
    ~supplySubSector, ~supplySector, ~water_sw, ~water_import, ~elec_all, ~elec_import, ~irri_all, ~irri_import,
    "water_sw",        "water",     0,         0 ,0.404     ,0     ,0         ,0,
    "water_import",     "water",     0,         0 ,0         ,0     ,0         ,0,
    "elec_all",         "elec",      0.27778,   0 ,0         ,0     ,0         ,0,
    "elec_import",      "elec",      0,         0 ,0         ,0     ,0         ,0,
    "irri_all",         "irri",      0,         0 ,0.0202    ,0     ,0         ,0,
    "irri_import",      "irri",      0,         0 ,0         ,0     ,0         ,0
  )


  # Copy data for each year, subregion and scenario
  # When this default is based on GCAm or other data it may need to be adjusted for changes by region/scenario/year

  A_temp <- data.frame()

  for (scenario_i in c("scenario")){
    for (region_i in c("region")){
      for (subRegion_i in c("subRegion")){
        for (year_i in c(0)){

        A_temp <- A_temp %>%
          bind_rows(A_default0 %>%
          dplyr::mutate(scenario = scenario_i,
                        region = region_i,
                        subRegion = subRegion_i,
                        x = year_i))
        }
      }
    }
  }

  A_default0 <- A_temp
  A_default0 %>% as.data.frame()


  supplySubSector_default0 = tibble::tribble( # Initial total demand
    ~supplySubSector, ~supplySector,      ~percentDistribution,
    # Water
    "water_all",          "water",            100,
    "water_sw",           "water",            100/4,
    "water_gw",           "water",            100/4,
    "water_desal",        "water",            100/4,
    "water_upstream",     "water",            100/4,
    "water_import",       "water",            0,
    # Electricity
    "elec_all",           "elec",      100,
    "elec_wind",          "elec",      100/9,
    "elec_solar",         "elec",      100/9,
    "elec_oil",           "elec",      100/9,
    "elec_hydro",         "elec",      100/9,
    "elec_geothermal",    "elec",      100/9,
    "elec_gas",           "elec",      100/9,
    "elec_coal",          "elec",      100/9,
    "elec_biomass",       "elec",      100/9,
    "elec_cogen",         "elec",      100/9,
    "elec_import",        "elec",      0,
    # Irrigation
    "irri_all",           "irri",       100,
    "irri_wheat",         "irri",       100/8,
    "irri_sugarCrop",     "irri",       100/8,
    "irri_root_Tuber",    "irri",       100/8,
    "irri_rice",          "irri",       100/8,
    "irri_otherGrain",    "irri",       100/8,
    "irri_oilCrop",       "irri",       100/8,
    "irri_miscCrop",      "irri",       100/8,
    "irri_corn",          "irri",       100/8,
    "irri_import",        "irri",       0
  );

  supplySubSector_default_temp <- data.frame()

  for (scenario_i in c("scenario")){
    for (region_i in c("region")){
      for (subRegion_i in c("subRegion")){
        for (year_i in c(00)){

        supplySubSector_default_temp <- supplySubSector_default_temp %>%
          bind_rows(supplySubSector_default0 %>%
                      dplyr::mutate(scenario = scenario_i,
                                    region = region_i,
                                    subRegion = subRegion_i,
                                    x = year_i))
        }
      }
    }
  }

  supplySubSector_default0 <- supplySubSector_default_temp
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
  total -> totalTemp -> value -> x -> .


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

nonFlowCols <- c("total","cap","capNew","capOrig","surplus"); nonFlowCols
nonFlowColsAll <- c("supplySubSector","supplySector",nonFlowCols,addedColumns); nonFlowColsAll


# ------------------
# Check Columns
# ------------------

if(!is.null(ioTable0)){ioTable <- addMissing(ioTable0)}
if(!is.null(A0)){ A <- addMissing(A0)}
if(!is.null(A_default0)){ A_default <- addMissing(A_default0)}
if(!is.null(supplySubSector_default0)){ supplySubSector_default <- addMissing(supplySubSector_default0)}

if(!is.null(A_default)){
  if(!any(grepl("supplySubSector",colnames(A_default)))){
    stop(print(paste("Column names in A_default: ",paste(colnames(A_default),collapse=", ")," must include 'supplySubSector'.",sep="")))}

  if(!any(grepl("supplySector",colnames(A_default)))){
    stop(print(paste("Column names in A_default: ",paste(colnames(A_default),collapse=", ")," must include 'supplySector'.",sep="")))}

  # Make sure supplySubSector column names are the same as the row names as this needs to be a square matrix
  if(any(!unique(A_default$supplySubSector) %in% names(A_default))){
    stop(print(paste("All suppluySubSectors in A_default must be represented as columns for a square intensity matrix.",sep="")))}

}

if(!is.null(supplySubSector_default)){

  for (column_i in c("supplySubSector","supplySector","percentDistribution")){
    if(!any(grepl(column_i,colnames(supplySubSector_default)))){
      stop(print(paste("Column names in supplySubSector_default0: ",paste(colnames(supplySubSector_default),collapse=", ")," must include ",column_i,".",sep="")))}}
  }

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
        dplyr::group_by(supplySector) %>%
        dplyr::mutate_at(vars(colsToEdit),sum) %>%
        dplyr::mutate(supplySubSector=sectorTotal_i) %>%
        dplyr::distinct();sectorTotalTemp

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
        dplyr::group_by(supplySector) %>%
        dplyr::mutate_at(vars(colsToEdit),function(x) 0) %>%
        dplyr::mutate(supplySubSector=sectorTotal_i,
                      !!sectorTotal_i := 0) %>%
        dplyr::distinct();sectorTotalTemp

      # Bind to main iotable
      A <- A %>%
        dplyr::bind_rows(sectorTotalTemp) %>%
        dplyr::mutate_at(vars(!!sectorTotal_i),funs(replace(., is.na(.), 0)))%>%
        dplyr::distinct(); A
    }

    }
  }

  for (column_i in c("export","adjustedDemand","cap","capNew","capOrig","surplus")){
  if(!any(grepl(column_i,colnames(ioTable)))){
    print(paste("Column names in ioTable0: ",paste(colnames(ioTable),collapse=", ")," must include ",column_i,".",sep=""))
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
    stop(print(paste("supplySubSector names in ioTable0: ",
                     paste(unique(ioTable$supplySubSector),collapse=", ")," are different from supplySubSector names in A0: ",
                     paste(unique(A$supplySubSector),collapse=", "),sep="")))}
}


# Check that supplySubSector_default and A_default have all supplySubSectors listed in A0 and ioTable0
if(!is.null(A) & !is.null(A_default)) {
  if(!any(unique(A$supplySubSector) %in% unique(A_default$supplySubSector))){
    stop(print(paste("supplySubSector names in A0: ",
                     paste(colnames(A),collapse=", ")," are different from supplySubSector names in A_default: ",
                     paste(colnames(A_default),collapse=", "),sep="")))}
}

if(!is.null(ioTable) & !is.null(supplySubSector_default)) {
  if(!any(unique(ioTable$supplySubSector) %in% unique(supplySubSector_default$supplySubSector))){
    stop(print(paste("supplySubSector names in ioTable0: ",
                     paste(colnames(ioTable),collapse=", ")," are different from supplySubSector names in supplySubSector_default: ",
                     paste(colnames(supplySubSector_default),collapse=", "),sep="")))}
}


# View Inputs
supplySubSector_default %>% as.data.frame()
A_default %>% as.data.frame()
ioTable %>% as.data.frame()
A %>% as.data.frame()


#------------------------------------
# Subset Default Data
#------------------------------------

# Make sure Sector percentages add up to 100 in Default
supplySubSector_default_sum <- supplySubSector_default %>%
  dplyr::filter(!grepl("_all",supplySubSector)) %>%
  dplyr::group_by(supplySector) %>%
  dplyr::summarize(supplySectorSum = sum(percentDistribution)); supplySubSector_default_sum %>% as.data.frame()

if(any(supplySubSector_default_sum$supplySectorSum != 100)){
  print(supplySubSector_default_sum)
  stop(print(paste("Not all supplySubSectors_default percentDistribution by supplySector add to 100.",sep="")))}


#------------------------------------
# For each year, suregion and scenario
#------------------------------------

tibble::tibble()->A_Output -> L_Output-> ioTbl_Output

for(scenario_i in scenarios){
  for(subRegion_i in subRegions){
    for(year_i in years){

#------------------------
# Create Directories
#-----------------------
 if (!dir.exists(paste(dirOutputs, "/IO/",scenario_i, sep = ""))){
     dir.create(paste(dirOutputs, "/IO/",scenario_i,  sep = ""))}

    dir<-paste(dirOutputs, "/IO/",scenario_i,  sep = "")




      print(paste("Solving for scenario: ", scenario_i, ", year:", year_i, " and sub-region:", subRegion_i," ...",sep=""))


      # Subset Data (D0i,X0i,A0i,Z0i,Cap0i,Import0i,Export0i)
      NULL -> A0i -> ioTable0i -> A_default0i -> supplySubSector_default0i

      if(!is.null(A)){A0i<-A %>% dplyr::filter(scenario==scenario_i,subRegion==subRegion_i,x==year_i)}
      if(!is.null(ioTable)){ioTable0i<-ioTable %>% dplyr::filter(scenario==scenario_i,subRegion==subRegion_i,x==year_i)}
      if(!is.null(A_default)){A_default0i<-A_default %>% dplyr::filter(scenario==scenario_i,subRegion==subRegion_i,x==year_i)}
      if(!is.null(supplySubSector_default)){supplySubSector_default0i<-supplySubSector_default %>% dplyr::filter(scenario==scenario_i,subRegion==subRegion_i,x==year_i)}

      A0i
      ioTable0i
      A_default0i
      supplySubSector_default0i

#-----------------------------------------------------------
# Fill out missing data using default supplySubSector_default0i
#----------------------------------------------------------

# Add in all supplysubSectors and fill out missing values with NAs

ioTable0i_completeNA <- ioTable0i %>%
        tidyr::complete(supplySubSector=unique(supplySubSector_default0i$supplySubSector)) %>%
        dplyr::select(-supplySector,-addedColumns) %>%
        dplyr::left_join(supplySubSector_default0i %>% select(supplySubSector,supplySector, addedColumns)) %>%
        dplyr::select(names(ioTable0i));

      ioTable0i_completeNA %>% as.data.frame()

# Find totals if given by supplySector_all.
# Then find sum of given supplySubSectors and the remainder to distribute
# If cap of a supplySubSector is given as 0 then supply from that sector is 0.
# Calculate percent distribution of remaining NA supplySubSectors from supplySubSector_defaul0i.
# Distribute remainder based on the new distribution.


# Get totals as supplied
ioTable0i_totals <-  ioTable0i_completeNA %>%
  dplyr::filter(grepl("_all",supplySubSector)) %>%
  dplyr::mutate(sumType = "totalsAll"); ioTable0i_totals %>% as.data.frame()


# Calculate total of subSectors Supplied
colsToEdit <- names(ioTable0i_completeNA)[!names(ioTable0i_completeNA) %in% c(addedColumns,"supplySector","supplySubSector")]; colsToEdit

ioTable0i_subSectorOther <-  ioTable0i_completeNA %>%
  dplyr::filter(!grepl("_all",supplySubSector)) %>%
  dplyr::group_by(supplySector) %>%
  dplyr::mutate_at(vars(colsToEdit),sum,na.rm=TRUE) %>%
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
  tidyr::spread(sumType,value) %>%
  dplyr::mutate(remainder=totalsAll-totalsSubSectorOther); ioTable0i_Remainder

if(any(unique(ioTable0i_Remainder$remainder)<0)){
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
  dplyr::filter(!grepl("_all",supplySubSector)) %>%
  dplyr::left_join(supplySubSector_default); ioTable0i_completeNA_supplySubSectors %>% as.data.frame()

ioTable0i_complete_RedistributeNA <- ioTable0i_completeNA_supplySubSectors %>%
  dplyr::select(-colsToEdit, -percentDistribution)

for(column_i in colsToEdit){

  colsToRemove = colsToEdit[colsToEdit != column_i]; colsToRemove

  ioTable0i_complete_RedistributeNA_temp <- ioTable0i_completeNA_supplySubSectors %>%
    # Assign 0 for NA sectors when cap is specified as 0.
    dplyr::mutate(!!column_i := case_when(cap == 0 ~ 0, TRUE ~ !!as.name(column_i))) %>%
    dplyr::filter(is.na(!!as.name(column_i))) %>%
    dplyr::group_by(supplySector) %>%
    dplyr::mutate(supplySubSectorNumber = length(unique(supplySubSector)),
                  percentDistribution = case_when(is.na(percentDistribution) ~ 0,
                                                  TRUE ~ percentDistribution),
                  percentDistribution_Adjusted = case_when (sum(percentDistribution) == 0 ~ 100/supplySubSectorNumber,
                                                            TRUE ~ percentDistribution*100/sum(percentDistribution)),
                  ) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(ioTable0i_Remainder_Spread %>%
                       dplyr::select(c(addedColumns,"supplySector",remainder=column_i))) %>%
    dplyr::mutate(!!column_i := percentDistribution_Adjusted*remainder/100) %>%
    dplyr::select(-colsToRemove,-percentDistribution, -supplySubSectorNumber,-percentDistribution_Adjusted,-remainder);  ioTable0i_complete_Redistribute_temp %>% as.data.frame()

  ioTable0i_complete_RedistributeNA <- ioTable0i_complete_RedistributeNA %>%
    dplyr::left_join(ioTable0i_complete_RedistributeNA_temp)
  }

ioTable0i_complete_RedistributeNA %>% as.data.frame()


# Replace redistributed flows in ioTable0i_completeNA for values where NA for each edited column

ioTable0i_complete_Redistribute <- ioTable0i_completeNA %>%
  dplyr::select(-colsToEdit)

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

ioTable0i_complete_Redistribute %>% as.data.frame()

# Check if re-distributed sums = initial total given.
check_calculated<-ioTable0i_complete_Redistribute %>%
  dplyr::filter(!grepl("_all",supplySubSector)) %>%
  dplyr::select(-supplySubSector) %>%
  dplyr::group_by(supplySector) %>%
  dplyr::summarize_at(vars(colsToEdit),sum) %>%
  tidyr::gather(key="key",value="valueCalculated",colsToEdit)

check_orig<-ioTable0i %>%
  dplyr::filter(grepl("_all",supplySubSector)) %>%
  dplyr::select(-supplySubSector) %>%
  dplyr::group_by(supplySector) %>%
  dplyr::summarize_at(vars(colsToEdit),sum) %>%
  tidyr::gather(key="key",value="valueOrig",colsToEdit)

check_Orig_Calc <- check_orig %>%
  dplyr::left_join(check_calculated) %>%
  dplyr::mutate(diff = valueOrig - valueCalculated); check_Orig_Calc

if(any(abs(check_Orig_Calc$diff) >= 1e-10)){
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

ioTable_adjustTotal

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
  dplyr::select(supplySubSector,unique(ioTable_complete$supplySubSector)); Zorg %>% as.data.frame()
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

A_Output = A_Output %>% dplyr::bind_rows(Aorg %>% tibble::as_tibble() %>% dplyr::mutate(scenario=scenario_i,x=year_i,subRegion=subRegion_i)); A_Output
L_Output = L_Output %>% dplyr::bind_rows(Lorg %>% tibble::as_tibble() %>% dplyr::mutate(scenario=scenario_i,x=year_i,subRegion=subRegion_i)); L_Output
ioTbl_Output = ioTbl_Output %>% dplyr::bind_rows(ioTable_complete %>% tibble::as_tibble() %>% dplyr::mutate(scenario=scenario_i,x=year_i,subRegion=subRegion_i)); ioTbl_Output %>% as.data.frame()


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
    tidyr::gather(-c("supplySubSector",addedColumns[addedColumns %in% names(A_mat)]),key="sectorTo",value="value") %>%
    dplyr::rename (sectorFrom=supplySubSector) %>%
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
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 15),
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
    dplyr::mutate_at(vars(-c("supplySubSector","supplySector",addedColumns[addedColumns %in% names(sol)])),dplyr::funs(./totalTemp)) %>%
    dplyr::select(-totalTemp); df_Mn

  solx <- sol %>%
    tidyr::gather(-c("supplySubSector","supplySector",addedColumns[addedColumns %in% names(df_Mn)]),key="sectorTo",value="value") %>%
    dplyr::rename (sectorFrom=supplySubSector) %>%
    dplyr::arrange(sectorFrom)  %>%
    dplyr::filter(!is.nan(value),value!=0, !is.na(value));solx


  df_Mnx <- df_Mn %>%
    tidyr::gather(-c("supplySubSector","supplySector",addedColumns[addedColumns %in% names(df_Mn)]),key="sectorTo",value="value") %>%
    dplyr::rename (sectorFrom=supplySubSector) %>%
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
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size = 15),
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
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size = 15),
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
    tidyr::gather(-c("supplySubSector","supplySector",addedColumns[addedColumns %in% names(df_Mn)]),key="sectorTo",value="value") %>%
    dplyr::rename (sectorFrom=supplySubSector) %>%
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

  fname = paste("sankeySub_",scenario_i,nameAppend,sep="")
  g<-ggplot(as.data.frame(dfx%>%dplyr::filter(value!=0, !grepl("_all",sectorFrom))),
            aes(y = value, axis1 = sectorFrom, axis2 = sectorTo, group=subRegion)) +
    theme_bw() +
    ggalluvial::geom_alluvium(aes(fill = sectorFrom), width = 1/12, color="black", alpha=0.6) +
    ggalluvial::geom_stratum(width = 1/12, fill = "grey30", color = "grey", alpha=1) +
    geom_label(stat = "stratum", label.strata = TRUE) +
    scale_x_discrete(limits = c("From", "To"), expand = c(.05, .05)) +
    #scale_fill_brewer(type = "qual", palette = "Set1", name="From") +
    scale_fill_manual(values=fillcolors, name="From") +
    facet_grid(x~subRegion) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size = 15),
          axis.text.y = element_text(size = 15),
          strip.text  = element_text(size = 15),
          axis.title = element_text(size = 15))+
    ggtitle(fname);g


  printf(figure=g,fileName = fname, dir=dir)

  fname = paste("sankeyAll_",scenario_i,nameAppend,sep="")
  g<-ggplot(as.data.frame(dfx%>%dplyr::filter(value!=0, grepl("_all",sectorFrom))),
            aes(y = value, axis1 = sectorFrom, axis2 = sectorTo, group=subRegion)) +
    theme_bw() +
    ggalluvial::geom_alluvium(aes(fill = sectorFrom), width = 1/12, color="black", alpha=0.6) +
    ggalluvial::geom_stratum(width = 1/12, fill = "grey30", color = "grey", alpha=1) +
    geom_label(stat = "stratum", label.strata = TRUE) +
    scale_x_discrete(limits = c("From", "To"), expand = c(.05, .05)) +
    #scale_fill_brewer(type = "qual", palette = "Set1", name="From") +
    scale_fill_manual(values=fillcolors, name="From") +
    facet_grid(x~subRegion) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size = 15),
          axis.text.y = element_text(size = 15),
          strip.text  = element_text(size = 15),
          axis.title = element_text(size = 15))+
    ggtitle(fname);g


  printf(figure=g,fileName = fname, dir=dir)

  }


  #-------- Aggregated Demands

  solFlows <- sol %>%
    dplyr::select(!!c("supplySubSector",names(sol)[names(sol) %in% c("total",addedColumns)]))
  df <- solFlows;df

  dfx <- df %>%
    tidyr::gather(-c("supplySubSector",addedColumns[addedColumns %in% names(df_Mn)]),key="sectorTo",value="value") %>%
    dplyr::rename (sectorFrom=supplySubSector) %>%
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

  fname = paste("sankeySub2Total_",scenario_i,nameAppend,sep="")
  g<-ggplot(as.data.frame(dfx%>%dplyr::filter(value!=0,  !grepl("_all",sectorFrom))),
            aes(y = value, axis1 = sectorFrom, axis2 = sectorTo, group=subRegion)) +
    theme_bw() +
    ggalluvial::geom_alluvium(aes(fill = sectorFrom), width = 1/12, color="black", alpha=0.6) +
    ggalluvial::geom_stratum(width = 1/12, fill = "grey30", color = "grey", alpha=1) +
    geom_label(stat = "stratum", label.strata = TRUE) +
    scale_x_discrete(limits = c("From", "To"), expand = c(.05, .05)) +
    #scale_fill_brewer(type = "qual", palette = "Set1", name="From") +
    scale_fill_manual(values=fillcolors, name="From") +
    facet_grid(x~subRegion) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size = 15),
          axis.text.y = element_text(size = 15),
          strip.text  = element_text(size = 15),
          axis.title = element_text(size = 15))+
    ggtitle(fname);g


  printf(figure=g,fileName = fname, dir=dir)

  fname = paste("sankeyAll2Total_",scenario_i,nameAppend,sep="")
  g<-ggplot(as.data.frame(dfx%>%dplyr::filter(value!=0,  grepl("_all",sectorFrom))),
            aes(y = value, axis1 = sectorFrom, axis2 = sectorTo, group=subRegion)) +
    theme_bw() +
    ggalluvial::geom_alluvium(aes(fill = sectorFrom), width = 1/12, color="black", alpha=0.6) +
    ggalluvial::geom_stratum(width = 1/12, fill = "grey30", color = "grey", alpha=1) +
    geom_label(stat = "stratum", label.strata = TRUE) +
    scale_x_discrete(limits = c("From", "To"), expand = c(.05, .05)) +
    #scale_fill_brewer(type = "qual", palette = "Set1", name="From") +
    scale_fill_manual(values=fillcolors, name="From") +
    facet_grid(x~subRegion) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size = 15),
          axis.text.y = element_text(size = 15),
          strip.text  = element_text(size = 15),
          axis.title = element_text(size = 15))+
    ggtitle(fname);g


  printf(figure=g,fileName = fname, dir=dir)
  }


  } # For Scenario i

  return(sol_list)

} # Close Function

