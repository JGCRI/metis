# This script contains functions to determine the simulation order for a network of interconnected sub-regions,
# which may or may not be hydrologic basins.
if("dplyr" %in% rownames(installed.packages()) == F){install.packages("dlpyr")}
library(dplyr)

network_main <- function(data_filepath){
  # Import user-defined sub-regional network
  network_data <- import_network_data(data_filepath = data_filepath)$network_data

  # Create key variables
  all_subRegs <- unique(as.character(network_data$subRegion))  # all subRegions being simulated
  remaining_subRegs <- all_subRegs  # all subRegions currently remain to be completed
  completed_subRegs <- NULL  # No subRegions have been completed yet; this stores simulation order

  # Complete basic checks on user-inputted data to be sure it passes basic logic checks
  logic_check(network_data, all_subRegs)

  # run network order code
  network_order <- network_order(network_data, all_subRegs, remaining_subRegs, completed_subRegs)$network_order
  from_to <- network_order(network_data, all_subRegs, remaining_subRegs, completed_subRegs)$from_to

  # Return outputs to script that called this function
  return(list("network_data" = network_data, "network_order" = network_order, "from_to" = from_to))
}

import_network_data <- function(data_filepath){
  # This function imports network .csv specified by user.
  network_data <- read.csv(data_filepath)
  return(list("network_data" = network_data))
}

logic_check <- function(network_data, all_subRegs){
  # This function checks that user-specified input data passes basic logic checks

  # Confirm that columns and rows are both specified
  for (subReg in all_subRegs){
    if(!subReg %in% names(network_data)){
      print("ERROR: user did not specify columns and rows for all subregions")
    }
  }

  # Confirm that 100% of water exiting a subRegion is allocated to some downstream subRegion
  # network_data <- network_data %>% mutate(rowsum)
  network_data_rowsum <- network_data %>% select(-subRegion) %>% mutate(rowsum=rowSums(.)) %>%
    mutate(rowsum = if_else(rowsum==0,1,rowsum))
  if(sum(network_data_rowsum$rowsum) != length(names(network_data_rowsum))-1){
    print("ERROR: user did not allocate flow from all elements to downstream elements")
  }
}

network_order <- function(network_data, all_subRegs, remaining_subRegs, completed_subRegs){

  # First, create a from_to list, describing, for every subRegion, which elements feed into it.
  from_to <- list()
  for(to_reg in all_subRegs){
    for(from_reg in all_subRegs){
      if((network_data %>% filter(subRegion==from_reg))[[to_reg]][1]>0){
        from_to[[to_reg]] <- append(from_to[[to_reg]], c(from_reg))
      }
    }
  }

  # Add headwater reaches (those for which no list key exists) to the completed_subRegs list
  for(to_reg in all_subRegs){
    if(!to_reg %in% names(from_to)){
      completed_subRegs <- append(completed_subRegs, c(to_reg))
    }
  }

  # Remove headwater reaches from remaining list
  remaining_subRegs <- remaining_subRegs[!remaining_subRegs %in% completed_subRegs]
  num_remaining_subRegs <- length(remaining_subRegs)

  while(num_remaining_subRegs > 0){
    for(e in remaining_subRegs){
      counter <- 0
      for(f in from_to[[e]]){
        if(!f %in% completed_subRegs){
          break
        }else{
          counter <- counter + 1
        }
      }
      if(counter == length(from_to[[e]])){
        completed_subRegs <- append(completed_subRegs, c(e))
        num_remaining_subRegs <- num_remaining_subRegs - 1
      }
    }
  }
  return(list("network_order" = completed_subRegs, "from_to" = from_to))
}

subReg_water_balance <- function(supply_demand_table, completed_subRegs, network_data, from_to){
  # This function seeks to determine the natural flows from upstream subRegions to downstream subRegions, given that the
  # model user is unlikely to specify this a priori because the values are affected by consumption.

  # Add capacity column if it's currently non-existent (not specified by user in initial table)
  if(!'cap' %in% names(supply_demand_table)){
    supply_demand_table$cap <- 0
  }

  # Add downstream column if it's currently non-existent (not specified by user in initial table)
  if(!'downstream' %in% names(supply_demand_table)){
    supply_demand_table$downstream <- 0
  }
  if(!'losses' %in% names(supply_demand_table)){
    supply_demand_table$losses <- 0
  }

  # Loop (in specified simulation order) through sub-regions to determine natural water imports, resulting capacity,
  # and resulting necessary exports
  for (e in completed_subRegs){
    #temp_supp_dem_tbl <- supply_demand_table %>% filter(subRegion==e, supplySector=='Water')
    upstream_inflow_c <- 0
    # Loop through inflow subRegions and add up flows they provide to subregion e
    for(from in from_to[[e]]){
      sum_inflow_df <- supply_demand_table %>% filter(subRegion==from, supplySector=='Water')
      upstream_component <- sum(sum_inflow_df$downstream, na.rm=TRUE)
      upstream_fraction <- (network_data %>% filter(subRegion==from) %>% select(e))[[e]]
      upstream_inflow_c <- upstream_inflow_c + upstream_component*upstream_fraction
    }
    # Set locally available capacity of inflow being supplied by upstream subRegions
    supply_demand_table <- supply_demand_table %>% mutate(upstream_inflow = upstream_inflow_c) %>%
      mutate(cap = if_else(subRegion == e & supplySubSector=='W_SW_Upstream', upstream_inflow, cap))
    df_user_wat_dem_tot <- supply_demand_table %>%
      filter(subRegion==e, supplySubSector %in% c('W_SW_Upstream', 'W_SW_Runoff', 'W_SW_Import')) %>%
      select(-one_of("downstream", "cap", "upstream_inflow", "units", "region"))
    df_user_wat_dem_tot <- df_user_wat_dem_tot %>% mutate(rowsum=rowSums(.[5:ncol(df_user_wat_dem_tot)], na.rm=TRUE))
    user_wat_dem_tot <- sum(df_user_wat_dem_tot$rowsum, na.rm=TRUE)
    current_water <- supply_demand_table %>%
      filter(subRegion == e, supplySubSector %in% c('W_SW_Upstream', 'W_SW_Runoff', 'W_SW_Import')) %>%
      select("cap") %>% mutate(rowsum=rowSums(., na.rm=TRUE))
    current_water <- sum(current_water$rowsum)
    consumption_frac <- 1 #  0.8  # Placeholder, not data-based
    if (current_water > user_wat_dem_tot){
      # Must increase water exports
      if(!e %in% names(from_to)){
        # This is a headwater sub-region that has no inflowing subregions. All water sent downstream must come from runoff.
        supply_demand_table <- supply_demand_table %>%
          mutate(downstream = if_else(subRegion == e & supplySubSector=='W_SW_Runoff', current_water - user_wat_dem_tot*consumption_frac, downstream))
          # %>% mutate(losses = if_else(subRegion == e & supplySubSector=='W_SW_Runoff', user_wat_dem_tot*consumption_frac, losses))
      }else{
        # This is not a headwater sub-region. Water sent downstream could come from both runoff and upstream flows.
        # But for simplicity here we assume all water comes from upstream flows in non headwater subregions.
        supply_demand_table <- supply_demand_table %>%
          mutate(downstream = if_else(subRegion == e & supplySubSector=='W_SW_Upstream', current_water - user_wat_dem_tot*consumption_frac, downstream))
          # %>% mutate(losses = if_else(subRegion == e & supplySubSector=='W_SW_Upstream', user_wat_dem_tot*consumption_frac, losses))
      }
    }
  }
  supply_demand_table <- supply_demand_table %>% select(-upstream_inflow)
  return(list("supply_demand_table"=supply_demand_table))
}
