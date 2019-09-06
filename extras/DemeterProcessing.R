# This file reads in global gridded Demeter land allocation data for one time step (e.g., 2010) or multiple time steps
# (e.g., 2010, 2015), makes it long rather than its standard wide format, stores it in a format required by metis, and
# saves the formatted data in a csv file for use by Metis.

library("dplyr")
library("tidyr")
library("ggplot2")
library('readr')
library("rgcam")

results_basepath <- 'C:/Users/twild/all_git_repositories/metis/metis/dataFiles/io/'
start_yr <- 2010
end_yr <- 2010
yr_array <- seq(start_yr, end_yr, 5)
units <- c('sqkm')  # Must be either 'sqkm' or 'fraction'
land_type_array <- c('water', 'forest', 'shrub', 'grass',	'urban',	'snow',	'sparse',	'corn_irr',	'fibercrop_irr',
                     'foddergrass_irr',	'fodderherb_irr',	'misccrop_irr',	'oilcrop_irr',	'othergrain_irr',
                     'palmfruit_irr',	'rice_irr',	'root_tuber_irr',	'sugarcrop_irr',	'wheat_irr',	'corn_rfd',
                     'fibercrop_rfd',	'foddergrass_rfd',	'fodderherb_rfd',	'misccrop_rfd',	'oilcrop_rfd',
                     'othergrain_rfd',	'palmfruit_rfd',	'rice_rfd',	'root_tuber_rfd',	'sugarcrop_rfd',	'wheat_rfd',
                     'otherarableland', 'biomass_grass_irr', 'biomass_grass_rfd', 'biomass_tree_irr', 'biomass_tree_rfd')
# First, import and reorganize land allocation.
for(yr in yr_array){
  input <- read_csv(paste0(results_basepath, 'landcover_', yr, '_', units, '.csv')) %>%
    gather(supplySubSector, LandAllocData, land_type_array) %>% as.data.frame()
  if(yr == start_yr){
    DemeterLandAlloc <- input
  }else{
    DemeterLandAlloc <- rbind(DemeterLandAlloc, input)
  }
  DemeterLandAlloc$units <- units
  DemeterLandAlloc$param <- 'LandAllocDemeter'
  DemeterLandAlloc$supplySector <- 'Land'  # This probably needs to be Ag for it to work with current metis
  DemeterLandAlloc$year <- yr
}
DemeterLandAlloc$dataSource <- "Demeter Model"
write_csv(DemeterLandAlloc
          %>% rename(value=LandAllocData)
          %>% select(-basin_id, -region_id), paste0(results_basepath, 'landcover_', yr, '_', units, '_', 'reshape', '.csv'))

# Next, downscale GCAM agricultural production by basin and crop to the level of individual grid cells.
gcamdatabasePath <- c('C:/Users/twild/all_git_repositories/idb_results/Colombia/GCAM/GCAM_runs')
gcamdatabaseName <- c('Reference')
dataProj <- c('C:/Users/twild/all_git_repositories/metis/metis/dataFiles/io/gcamAgProd.proj')
qF <- c('C:/Users/twild/all_git_repositories/metis/metis/dataFiles/gcam/AgProdQuery.xml')

globalAg <- addScenario(conn = localDBConn(gcamdatabasePath, gcamdatabaseName), proj = dataProj,
                                    scenario = 'Reference', queryFile = qF)  # Check your queries file
globalAg <- globalAg$Reference$`ag production by tech` %>%
  filter(year>=start_yr, year<=end_yr) %>%
  rename(crop=sector, basin=subsector) %>%
  mutate(basin=gsub('_',"",basin))

globalAg$basin <- sapply(seq_along(globalAg$crop), function(x) gsub(globalAg$crop[x], rep("", nrow(globalAg)), globalAg$basin[x]))  # remove crop from basin category
globalAg <- globalAg %>% mutate(grass = "grass")
globalAg <- globalAg %>% mutate(tree = "tree")
globalAg$basin <- sapply(seq_along(globalAg$grass), function(x) gsub(globalAg$grass[x], rep("", nrow(globalAg)), globalAg$basin[x]))  # remove grass
globalAg$basin <- sapply(seq_along(globalAg$tree), function(x) gsub(globalAg$tree[x], rep("", nrow(globalAg)), globalAg$basin[x]))  # remove tree
globalAg$rfd <- grepl("RFD", globalAg$technology)
globalAg$irr <- grepl("IRR", globalAg$technology)
globalAg$hi <- grepl("hi", globalAg$technology)
globalAg$lo <- grepl("lo", globalAg$technology)

GlobalAg_RFD_IRR <- globalAg %>%
  select(-hi, -lo, -grass, -tree, -technology) %>%
  group_by(Units, scenario, region, crop, basin, output, year, rfd, irr) %>%
  summarize(value=sum(value)) %>%
  ungroup()

bas2ctry_mapping <- read_csv('C:/Users/twild/all_git_repositories/metis/metis/extras/basin_to_country_mapping.csv',
                             skip = 7)
GlobalAg_RFD_IRR <- GlobalAg_RFD_IRR %>%
  rename(GLU_name = basin) %>%
  left_join(bas2ctry_mapping, by='GLU_name')

DemeterLandAlloc <- DemeterLandAlloc %>%
  left_join(bas2ctry_mapping, by='basin_id')

# Determine how much land is actually allocated for each basin/crop type, to avoid having to query this out of gcam.
Sum_DemeterLandAlloc <- DemeterLandAlloc %>%
  group_by(basin.name.spaces, supplySubSector) %>%
  summarize(sumArea_by_LUCType=sum(LandAllocData)) %>%
  ungroup()

# Merge this sum by crop and basin back into the DemeterLandAlloc variable, so we can compute what fraction of the basin's
# land of this type exists in this particular cell. We will use this as the basis of downscaling.
DemeterLandAlloc <- DemeterLandAlloc %>%
  left_join(Sum_DemeterLandAlloc, by=c('basin.name.spaces', 'supplySubSector')) %>%
  mutate(fraction = LandAllocData/sumArea_by_LUCType)

# Sum up total ag production by crop, with new crop categories such as "corn_irr" and "corn_rfd" that match up with the
# demeter categories, in lower case.
AgProdSummary <- GlobalAg_RFD_IRR %>%
  group_by(crop, GLU_name, rfd, irr, Units) %>%
  summarize(BasinCropProd=sum(value)) %>%
  ungroup() %>%
  mutate(crop_irr_rfd=if_else(rfd==TRUE, paste0(crop, '_', 'rfd'), crop)) %>%
  mutate(crop_irr_rfd=if_else(irr==TRUE, paste0(crop, '_', 'irr'), crop)) %>%
  mutate(crop_irr_rfd=tolower(crop_irr_rfd)) %>%
  select(-irr, -rfd)

# Merge gcam ag production into demeter data by crop
DemeterLandAlloc <- DemeterLandAlloc %>%
  select(-units) %>% # Get rid of land allocation units, and instead merge in ag production units
  left_join(AgProdSummary %>%
              select(crop_irr_rfd, BasinCropProd, GLU_name, Units) %>%
              rename(supplySubSector = crop_irr_rfd), by=c('GLU_name', 'supplySubSector')) %>%
  mutate(AgProdGrid=BasinCropProd*fraction)

non_crops <- c('water', 'forest', 'shrub', 'grass', 'urban', 'snow', 'sparse', 'otherarableland')
DemeterLandAlloc %>%
  select(-basin_id, -region_id, -basin.name.underscores, -GLU_name, -basin.name.spaces, -BasinCropProd,
         -sumArea_by_LUCType, -fraction, -LandAllocData) %>%
  rename(value = AgProdGrid) %>%
  mutate(dataSource = 'Demeter_GCAM') %>%
  filter(!supplySubSector %in% non_crops) %>%
  write_csv('C:/Users/twild/all_git_repositories/metis/metis/dataFiles/io/AgProdCropBasinGrid.csv')
