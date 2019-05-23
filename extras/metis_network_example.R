# This script uses MetisWatMod to build the simulation network then balance water flows in a historical year

source(paste(getwd(),'/extras/MetisWatMod.R',sep=""))
library(tidyverse)
data <- paste(getwd(),'/datafiles/io/network_data.csv',sep="")

# Simulation network/order
output <- network_main(data)
network_data <- output$network_data
network_order <- output$network_order
from_to <- output$from_to

#------------------------------------------------
# Import user-defined supply/demand/capacity files
# Import demands across sectors, with corresponding supplies specified
# Import base data
demand_data_file = 'C:/Users/twild/Dropbox/Argentina and metis workplan/Metis_Colorado/colorado_demand_data.csv'
capacity_data_file = 'C:/Users/twild/Dropbox/Argentina and metis workplan/Metis_Colorado/colorado_capacity_data.csv'
demand_data <- read.csv(demand_data_file)
demand_data <- demand_data %>% as_tibble()
capacity_data <- read.csv(capacity_data_file)
# Manipulate/rearrange demand/supply data frame to wide format
ioTable0 <- demand_data %>% select(-units, -localData, -dataSource, -year, -param) %>%
  mutate(demandClassCombined=ifelse(class2=="", paste0(demandClass, class2), paste0(demandClass,"_", class2))) %>%
  select(-class2, -demandClass) %>% spread(demandClassCombined, localDataSubdivided)
ioTable0[,c(which(colnames(ioTable0)=="supplySector"),which(colnames(ioTable0)!="supplySector"))]  # Shift supply to first column
# Manipulate/rearrange supply capacity data frame to wide format, and integrate it with existing ioTable0 dataframe
capTable <- capacity_data %>% select(-year, -units) %>% rename(cap=data)
# Merge ioTable0 and capTable
ioTable0 <- ioTable0 %>% left_join(capTable, by=c('subRegion', 'supplySector', 'supplySubSector'))
#------------------------------------------------

# Balancing water flows
output_water <- subReg_water_balance(ioTable0, network_order, network_data, from_to)
ioTable0 <- output_water$supply_demand_table  # updated demand-supply table that includes proper natural water exports, and capacities
