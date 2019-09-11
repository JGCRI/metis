
#----------------------------
# Install necessary packages
#----------------------------
if("devtools" %in% rownames(installed.packages()) == F){install.packages("devtools")}
library(devtools)
if("metis" %in% rownames(installed.packages()) == F){install_github(repo="JGCRI/metis")}
library(metis)
if("rgcam" %in% rownames(installed.packages()) == F){install_github(repo="JGCRI/rgcam")}
library(rgcam)
if("tibble" %in% rownames(installed.packages()) == F){install.packages("tibble")}
library(tibble)
if("dplyr" %in% rownames(installed.packages()) == F){install.packages("dlpyr")}
library(dplyr)
if("rgdal" %in% rownames(installed.packages()) == F){install.packages("rgdal")}
library(rgdal)
if("tmap" %in% rownames(installed.packages()) == F){install.packages("tmap")}
library(tmap)
if("rgeos" %in% rownames(installed.packages()) == F){install.packages("rgeos")}
library(rgeos)
if("ggplot2" %in% rownames(installed.packages()) == F){install.packages("ggplot2")}
library(ggplot2)
if("ggalluvial" %in% rownames(installed.packages()) == F){install.packages("ggaaluvial")}
library(ggalluvial)
if("tidyverse" %in% rownames(installed.packages()) == F){install.packages("tidyverse")}
library(tidyverse)
if("xlsx" %in% rownames(installed.packages()) == F){install.packages("xlsx")}
library(xlsx)

save_dir <- 'C:/Users/twild/all_git_repositories/metis/metis/dataFiles/io'
scenarios <- c('Reference', 'Policy')
# Import main data file for NO policy case, which we will then modify to reflect stakeholder preferences.
SupDem_data <- read.csv(paste(getwd(),'/datafiles/io/colorado_demand_data.csv', sep=""))
SupDem_data_NOGW <- SupDem_data  # Creating a copy we can modify later
subRegions <- as.character(unique(SupDem_data$subRegion))
# Import policy case files
SupDem_data_policy <-read.csv(paste(getwd(),'/datafiles/io/colorado_demand_data_AgPolicy.csv', sep=""))
SupDem_data_NOGW_policy <- SupDem_data_policy  # Creating a copy we can modify later
profit_landuse_mapping_file <- c('C:/Users/twild/all_git_repositories/metis/metis/dataFiles/io/Tables_subregional_Colorado.xlsx')

# Read in biophysical water demand data, for use in estimating crop water demands for ET (blue and green consumption)
gcamdatabasePath <- c('C:/Users/twild/all_git_repositories/idb_results/Colombia/GCAM/GCAM_runs')
gcamdatabaseName <- c('Reference')
dataProj <- c('C:/Users/twild/all_git_repositories/metis/metis/dataFiles/io/gcamBiophysWatCons.proj')
qF <- c('C:/Users/twild/all_git_repositories/metis/metis/dataFiles/gcam/BiophysWat_LandAlloc_Query.xml')
start_yr <- 2010
end_yr <- 2010
queryOut <- addScenario(conn = localDBConn(gcamdatabasePath, gcamdatabaseName), proj = dataProj,
                        scenario = 'Reference', queryFile = qF)  # Check your queries file
globalBiophysWatCons <- queryOut$Reference$`biophysical water demand by crop type and land region` %>%
  filter(year>=start_yr, year<=end_yr) %>%
  rename(crop=sector, basin=subsector)
  # remove crop from basin category
  globalBiophysWatCons$basin <- sapply(seq_along(globalBiophysWatCons$crop),
                                       function(x) gsub(globalBiophysWatCons$crop[x],
                                                        rep("", nrow(globalBiophysWatCons)),
                                                        globalBiophysWatCons$basin[x]))
  globalBiophysWatCons <- globalBiophysWatCons %>%
    mutate(basin=gsub('_',"",basin)) %>%
    rename(BasinBioPhysWatCons = value)

# Land Allocation
bas2ctry_mapping <- read_csv('C:/Users/twild/all_git_repositories/metis/metis/extras/basin_to_country_mapping.csv',
                             skip = 7)
basin_list <- bas2ctry_mapping$GLU_name
land_type_array <- c('Corn',	'FiberCrop', 'OilCrop',	'Forest',	'Grassland',	'MiscCrop',	'OtherGrain',
                     'PalmFruit',	'Rice',	'Root_Tuber',	'SugarCrop',	'Wheat', 'FodderGrass',	'FodderHerb',
                     'Pasture', 'Shrubland', 'biomass_grass', 'biomass_tree')
detLandAlloc <- queryOut$Reference$`detailed land allocation` %>%
  filter(year>=start_yr, year<=end_yr)
detLandAlloc$crop <- ''
for(crp in land_type_array){
  detLandAlloc <- detLandAlloc %>%
    mutate(crop_true_false=grepl(c(crp), landleaf)) %>%
    mutate(crop = if_else(crop_true_false==TRUE, crp, crop))
}
detLandAlloc$basin <- ''
for(bas in basin_list){
  detLandAlloc <- detLandAlloc %>%
    mutate(basin_true_false=grepl(c(bas), landleaf)) %>%
    mutate(basin = if_else(basin_true_false==TRUE, bas, basin))
}

detLandAlloc <- detLandAlloc %>%
  select(-crop_true_false, -basin_true_false) %>%  # drop extra columns
  rename(LandAllocValue = value) %>%   #rename so dont have multiple value columns
  filter(!crop=='')  # get rid of blank crop rows, which are non-crop land use types

GlobalSummaryCropBasin <- detLandAlloc %>%
#  select(-hi, -lo, -grass, -tree, -technology) %>%
  group_by(Units, scenario, region, crop, basin, year) %>%
  summarize(LandAllocValue=sum(LandAllocValue)) %>%
  ungroup()

Merged_Wat_Land <- globalBiophysWatCons %>%
  rename(Units_water=Units) %>%
  left_join(GlobalSummaryCropBasin %>% rename(Units_Land=Units),
            by=c('scenario', 'region', 'crop', 'basin', 'year')) %>%  # merge land DF into water DF
  mutate(water_per_land=BasinBioPhysWatCons/LandAllocValue) %>% # Compute water use per unit area for crop/basin/region/year
  mutate(Units = 'km^3/thous km^2')  # Units for new calculation

select_basin <- c('ArgColoR')  # Select a basin of interest
select_region <- c('Argentina')
Merged_Wat_Land_Colorado <- Merged_Wat_Land %>%
  filter(basin==select_basin, region==select_region) %>%  # filter basin and region of interest
  mutate(AggCrop = crop) %>%   # create new crop column to store aggregate categories
  mutate(AggCrop=if_else(crop %in% c('Corn', 'Wheat', 'OilCrop'), 'Ag_cereals', AggCrop)) %>%   # aggregate crop categories
  mutate(AggCrop=if_else(crop %in% c('FodderHerb'), 'Ag_pasture', AggCrop))  # aggregate crop categories

# Calculate average WIC values by Aggregate_crop-basin-region category
ColoradoWaterLand_Summary <- Merged_Wat_Land_Colorado %>%
  group_by(AggCrop) %>%
  summarize(mean=mean(water_per_land)) %>%
  mutate(mean = mean/1000) %>%   # convert from km3/thous km2 to km3/km2
  ungroup()

# Eliminate Groundwater usage from both the policy and non-policy files. Redistribute to surface water supply.
#for (scen in scenarios){
#  if(scen=='Reference'){
      data <- SupDem_data
      data_NOGW <- SupDem_data_NOGW
#  }else if(scen=='Policy'){
#      data <- SupDem_data_policy
#      data_NOGW <- SupDem_data_NOGW_policy
#  }
  for (sR in subRegions){
    SupDem_GW <- data %>%
      filter(subRegion==sR, supplySubSector=='W_GW_Reservoir')
    DemCls <- as.character(unique(SupDem_GW$demandClass))
    for(dC in DemCls){
      SupDem_GW_DemCls <- SupDem_GW %>%
        filter(demandClass==dC) # filter GW data for subregion just by demand classes
      SupDem_GW_DemCls_Value <- SupDem_GW_DemCls$localDataSubdivided[1]  # Get GW value for this demand class and subregion
      data_NOGW <- data_NOGW %>%
        mutate(localDataSubdivided=if_else(subRegion==sR & demandClass==dC & supplySubSector=='W_SW_Upstream', localDataSubdivided+SupDem_GW_DemCls_Value, localDataSubdivided)) %>%
        filter(!supplySubSector=='W_GW_Reservoir')
    }
  }
  # Replace dollar values with land values
  profitLanduseMapping <- read.xlsx(profit_landuse_mapping_file, sheetName='agg crop by 10 subreg') %>%
    rename(supplySubSector=crop) %>%
    mutate(supplySubSector = paste0('Ag_', supplySubSector)) %>%
    select(-gross.value..mil.pesos.) %>%
    filter(!subRegion %in% 'SUM')

  data_NOGW$units <- as.character(data_NOGW$units)

  data_NOGW <- data_NOGW %>%
    left_join(profitLanduseMapping, by=c('subRegion', 'supplySubSector')) %>%
    mutate(New_crop.area..km2.=if_else(is.na(crop.area..km2.), 0, crop.area..km2.)) %>%
    mutate(localDataSubdivided=if_else(New_crop.area..km2. > 0, New_crop.area..km2., localDataSubdivided)) %>%
    mutate(units=if_else(New_crop.area..km2. > 0, 'km2', units)) %>%
    select(-crop.area..km2., -New_crop.area..km2., -localData) %>%
    rename(localData = localDataSubdivided)

  misc_crop_coeff <- (ColoradoWaterLand_Summary %>% filter(AggCrop=='MiscCrop'))$mean[1]
  data_NOGW <- data_NOGW %>%
    left_join(ColoradoWaterLand_Summary %>% rename(WatIntensCoeff=mean), by=c('demandClass'='AggCrop')) %>%
    mutate(WatIntensCoeff=if_else(demandClass %in% c('Ag_specialty', 'Ag_vegetables', 'Ag_fruittrees'), misc_crop_coeff, WatIntensCoeff)) # insert coefficient for miscellaneous crops

  # Calculate the sum of the intensitity values (water per unit area) that occur within each region/subRegion
  Subreg_Crop_Intensity <- data_NOGW %>%
    filter(supplySector=='Water') %>%
    mutate(delete = grepl('Ag_', demandClass)) %>%
    filter(delete==TRUE) %>%
    select(region, subRegion, demandClass, WatIntensCoeff)

  # Join WIC values with land categories by supply subsector.
  SubRegLand <- data_NOGW %>%
    filter(supplySector=='Agriculture') %>%
    select(region, subRegion, supplySubSector, localData) %>%
    left_join(Subreg_Crop_Intensity %>% rename(supplySubSector = demandClass), by=c('region', 'subRegion', 'supplySubSector'))

  # Eliminates NA values for WIC (e.g., for Forest)
  SubRegLand <- SubRegLand %>%
    mutate(WatIntensCoeff = replace(WatIntensCoeff, which(is.na(WatIntensCoeff)), 0)) %>%
    mutate(Estim_TotBiophysWatDem = localData*WatIntensCoeff)

  # Calculate total biophysical estiomated water requirement for each subregion-Agg_crop category.
  TotalBiophys_SubRegLand <- SubRegLand %>%
    group_by(region, subRegion) %>%
    summarize(Total_subreg_Biophys=sum(Estim_TotBiophysWatDem)) %>%
    ungroup()

  # Merge in the above back into the SubRegLand and calculate fractions of water that should go to each Agg_crop-subregion category
  SubRegLand <- SubRegLand %>%
    left_join(TotalBiophys_SubRegLand, by=c('region', 'subRegion')) %>%
    mutate(fraction_water = Estim_TotBiophysWatDem / Total_subreg_Biophys)

  # Calculate the sum of water demanded within each region/subRegion. This water will be allocated using fractions calculated above.
  sumSubregWatDem <- data_NOGW %>%
    filter(supplySector=='Water') %>%
    mutate(delete = grepl('Ag_', demandClass)) %>%
    filter(delete==TRUE) %>%
    group_by(region, subRegion) %>%
    summarize(TotalWatDem=sum(localData))

  # Merge in the total water demand sum for each subregion into the main dataframe, and
  # merge fractions into dataframe
  data_NOGW <- data_NOGW %>%
    left_join(sumSubregWatDem, by=c('region', 'subRegion')) %>%
    left_join(SubRegLand %>%
                rename(demandClass=supplySubSector) %>%
                select(region, subRegion, demandClass, fraction_water), by=c('region', 'subRegion', 'demandClass')) %>%
    mutate(new_localData=localData) %>%  # preserve original values for non-crop water demand sectors
    mutate(new_localData=if_else(is.na(fraction_water)==FALSE, fraction_water*TotalWatDem, new_localData)) %>%
    select(-localData) %>%
    rename(localData = new_localData)

  # Get rid of extra unneeded columns
  data_NOGW <- data_NOGW %>%
    select(-WatIntensCoeff, -TotalWatDem, -fraction_water)

  # Save modified file
  write.csv(data_NOGW, file=paste0(save_dir, '/', 'colorado', '_', scen, '_NEW', '.csv'), row.names=FALSE)
#}

# Prepare dataframe for policy
policy_land_multiplier = list('RioNegro_baja' = 0.835, 'LaPampa_baja' = 127.083)
data_NOGW$supplySector <- as.character(data_NOGW$supplySector)
# Adjust main dataframe to account for increases in land usage
data_NOGW_policy <- data_NOGW
for(sR in names(policy_land_multiplier)){
data_NOGW_policy <- data_NOGW_policy %>%
  mutate(localData = ifelse(supplySector=='Agriculture' & subRegion==sR, localData*(1 + policy_land_multiplier[[sR]]), localData))
# Similarly adjust total water demands associated with the new land usage
data_NOGW_policy <- data_NOGW_policy %>%
  mutate(delete1 = grepl('Water', supplySector)) %>%
  mutate(delete2 = grepl('Ag_', demandClass)) %>%
  mutate(localData = ifelse(delete1==TRUE & delete2==TRUE & subRegion==sR, localData*(1 + policy_land_multiplier[[sR]]), localData)) %>%
  select(-delete1, -delete2)
}
# Save policy file
write.csv(data_NOGW_policy, file=paste0(save_dir, '/', 'colorado', '_', 'Policy', '_NEW', '.csv'), row.names=FALSE)


#}

data_NOGW_policy$scenario <- 'Policy'
data_NOGW$scenario <- 'Reference'
plot_DF <- rbind(data_NOGW_policy, data_NOGW) %>%
  select(region, subRegion, supplySector, supplySubSector, localData, units, demandClass, scenario)
plot_DF <- plot_DF %>%
    filter(supplySector=='Agriculture') %>%
    mutate(delete = grepl('Ag_', supplySubSector)) %>%
    filter(delete==TRUE) %>%
    select(-delete) %>%
    mutate(supplySubSector=if_else(supplySubSector=='Ag_cereals', 'Cereals (Wheat, Corn, etc.)', supplySubSector)) %>%
    mutate(supplySubSector=if_else(supplySubSector=='Ag_fruittrees', 'Fruit Trees (Apples, Pears, etc.)', supplySubSector)) %>%
    mutate(supplySubSector=if_else(supplySubSector=='Ag_vegetables', 'Vegetables (Onions, Pumpkins, etc.)', supplySubSector)) %>%
    mutate(supplySubSector=if_else(supplySubSector=='Ag_pasture', 'Pasture', supplySubSector)) %>%
    mutate(supplySubSector=if_else(supplySubSector=='Ag_specialty', 'Specialty (Wine, Olives, etc.)', supplySubSector)) %>%
    rename(crop=supplySubSector) %>%
    rename(FillLabel=units)


CropPalette <- c(Pasture = "#a6cdd9", `Specialty (Wine, Olives, etc.)` = "#d2e4ee",
                 `Vegetables (Onions, Pumpkins, etc.)` = "#8bb086",
                 `Cereals (Wheat, Corn, etc.)` = "#b7b079",
                 `Fruit Trees (Apples, Pears, etc.)` = "#efc750")

plot_DF$FillPalette <- 'CropPalette'
base_fig_path <- 'C:/Users/twild/Dropbox/Argentina and metis workplan/Metis_Colorado/paper/figures'
x_labels <-list('RioNegro_baja' = c("Cereal", "Fruit Trees", "Pasture", "Vegetables"),
                'LaPampa_baja' = c("Pasture", "Specialty", "Vegetables"))
label_names <- c("Reference" = "Reference Scenario",
                "Policy" = "Irrigation Expansion")
sR_array <- c('RioNegro_baja', 'LaPampa_baja')
for(sR in sR_array){
  plot_DF_sR <- plot_DF %>%
    filter(subRegion == sR) %>%
    mutate(FacetVar = scenario)
  plot_DF_sR$crop <- as.factor(plot_DF_sR$crop)
  paletteX<-get(plot_DF_sR$FillPalette)
  p <- ggplot(plot_DF_sR, aes( x = crop, y = localData, fill = crop) ) +  # group=scenario,
    geom_bar( stat = "identity", width = 0.75, color = "#2b2b2b", size = 0.05 ) +
  #  scale_y_continuous( labels = percent, limits = c( 0, 0.5 ) ) +
    scale_x_discrete( expand = c( 0, 1 ), labels = x_labels[[sR]] ) +
    scale_fill_manual( values = paletteX ) +  # ,    c("#efc750", "#a6cdd9", "#d2e4ee", "#b7b079")
    facet_wrap( ~ FacetVar, labeller = as_labeller(label_names) ) +
    #facet_grid(.~FacetVar, drop=TRUE, space="free", scales="free") +
  #  labs( x = NULL, y = NULL, title = "Reference vs. Policy", element_text(hjust=0.5) ) +
    ylab(expression(Land~Allocation~by~Crop~(km^2))) +
    theme( strip.text = element_text( size = 12, color = "white", hjust = 0.5 ),
           strip.background = element_rect( fill = "#858585", color = NA ),
           panel.background = element_rect( fill = "#efefef", color = NA ),
           panel.grid.major.x = element_blank(),
           panel.grid.minor.x = element_blank(),
           panel.grid.minor.y = element_blank(),
           panel.grid.major.y = element_line( color = "#b2b2b2" ),
           panel.spacing.x = unit( 1, "cm" ),
           panel.spacing.y = unit( 0.5, "cm" ),
           legend.position = "none" )
  p
  ggsave(paste0(base_fig_path, '/', 'ColoradoAg', '_', sR, ".png"), plot=p, dpi=300, width=10, height=5, units="in")  # Save image
}
