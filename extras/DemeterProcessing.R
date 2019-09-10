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
    gather(supplySubSector, localData, land_type_array) %>% as.data.frame()
  if(yr == start_yr){
    data <- input
  }else{
    data <- rbind(data, input)
  }
  data$units <- units
  data$param <- 'LandAllocDemeter'
  data$supplySector <- 'Land'  # This probably needs to be Ag for it to work with current metis
  data$year <- yr
}
data$dataSource <- "Demeter Model"
write_csv(data, paste0(results_basepath, 'landcover_', yr, '_', units, '_', 'reshape', '.csv'))

# Next, downscale GCAM agricultural production by basin and crop to the level of individual grid cells.
gcamdatabasePath <- c('C:/Users/twild/all_git_repositories/idb_results/Colombia/GCAM/GCAM_runs')
gcamdatabaseName <- c('Reference')
dataProj <- c('C:/Users/twild/all_git_repositories/metis/metis/dataFiles/io/gcamAgProd.proj')
qF <- c('C:/Users/twild/all_git_repositories/metis/metis/dataFiles/gcam/AgProdQuery.xml')

globalAg <- addScenario(conn = localDBConn(gcamdatabasePath, gcamdatabaseName), proj = dataProj,
                                    scenario = 'Reference', queryFile = qF)  # Check your queries file
globalAg <- globalAg$Reference$`ag production by tech` %>%
  filter(year>=start_yr, year<=end_yr) %>%
  rename(crop=sector, basin=subsector)
  mutate(basin=gsub('_',"",basin))

globalAg$basin <- sapply(seq_along(globalAg$crop), function(x) gsub(globalAg$crop[x], rep("", nrow(globalAg)), globalAg$basin[x]))  # remove crop from basin category

globalAg <- globalAg %>% mutate(grass = "grass")
globalAg <- globalAg %>% mutate(tree = "tree")
globalAg$basin <- sapply(seq_along(globalAg$grass), function(x) gsub(globalAg$grass[x], rep("", nrow(globalAg)), globalAg$basin[x]))  # remove grass
globalAg$basin <- sapply(seq_along(globalAg$tree), function(x) gsub(globalAg$tree[x], rep("", nrow(globalAg)), globalAg$basin[x]))  # remove tree
globalAg$basin <- gsub('_', "", globalAg$basin)  # remove underscore
globalAg$rfd <- grepl("RFD", globalAg$technology)
globalAg$irr <- grepl("IRR", globalAg$technology)
globalAg$hi <- grepl("hi", globalAg$technology)
globalAg$lo <- grepl("lo", globalAg$technology)


