
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


# Import main data file for NO policy case, which we will then modify to reflect stakeholder preferences.
SupDem_data_file = paste(getwd(),'/datafiles/io/colorado_demand_data.csv', sep="")  # _AgPolicy
SupDem_data_file_policy = paste(getwd(),'/datafiles/io/colorado_demand_data_AgPolicy.csv', sep="")  # _AgPolicy
SupDem_data <- read.csv(SupDem_data_file)
SupDem_data_policy <-read.csv(SupDem_data_file_policy)
subRegions <- as.character(unique(SupDem_data$subRegion))
SupDem_data_NOGW <- SupDem_data  # Creating a copy we can modify later

# Eliminate Groundwater usage from both the policy and non-policy files. Redistribute to surface water supply.
for (sR in subRegions){
  SupDem_GW <- SupDem_data %>%
    filter(subRegion==sR, supplySubSector=='W_GW_Reservoir')
  DemCls <- as.character(unique(SupDem_GW$demandClass))
  for(dC in DemCls){
    SupDem_GW_DemCls <- SupDem_GW %>%
      filter(demandClass==dC) # filter GW data for subregion just by demand classes
    SupDem_GW_DemCls_Value <- SupDem_GW_DemCls$localDataSubdivided[1]  # Get GW value for this demand class and subregion
    SupDem_data_NOGW <- SupDem_data_NOGW %>%
      mutate(localDataSubdivided=if_else(subRegion==sR & demandClass==dC & supplySubSector=='W_SW_Upstream', localDataSubdivided+SupDem_GW_DemCls_Value, localDataSubdivided)) %>%
      filter(!supplySubSector=='W_GW_Reservoir')
  }
}

# Save modified file
save_dir <- 'C:/Users/twild/all_git_repositories/metis/metis/dataFiles/io'
write.csv(SupDem_data_NOGW, file=paste0(save_dir, '/', 'colorado_demand_data_NEW', '.csv'), row.names=FALSE)
