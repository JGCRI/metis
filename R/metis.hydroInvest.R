#' metis.hydroInvest
#'
#' Function that calculates electricity subsector investment requirements from a given GCAM run.
#'
#' @param addition_costs list formatted as produced by metis.elecInvest.R function
#' @keywords investments, infrastructure
#' @return Returns data in a form required by metislreadgcam.R
#' @export

hydroInvest <- function(addition_costs, start_year, end_year){

  # Calculations hydropower investment, as elecInvest() function does not handle hydropower.

  # Add hydro installed capacity to addition_costs using addition_costs[['elec_prod']]
  hydro_energy <- addition_costs[['elec_prod']] %>% filter(agg_tech=='Hydro')
  hydro_energy <- hydro_energy %>% mutate(agg_tech = replace(agg_tech, agg_tech == "k Hydro", "Hydro"))  # Replace "k Hydro" with "Hydro"
  hydro_energy_orig <- hydro_energy
  if('2015' %in% names(addition_costs[['elec_prod']])){
    addition_costs[['elec_prod']] <- addition_costs[['elec_prod']] %>% select(-`2015`)
  }
  # Convert from energy to GW using capacity factor, then insert into GW dataframe
  years <- c(6:length(hydro_energy))
  for (yr in years){
    hydro_energy[yr] <- hydro_energy_orig[yr] - hydro_energy_orig[yr-1]
  }
  hydro_energy_inc <- hydro_energy %>% select(-`2015`)
  hydro_GW_inc <- hydro_energy_inc
  col_len <- length(hydro_energy_inc)
  col_len_2 <- col_len - 1
  hydro_GW_inc[5:col_len] <- metis.assumptions()$convEJ2GWh*(1/(8760 *  metis.assumptions()$hydro_cap_fact))*hydro_energy_inc[5:col_len]
  hydro_GW_inc$Units <- 'GW'
  addition_costs[['add by GW']] <- addition_costs[['add by GW']] %>% filter(agg_tech!='Hydro')
  addition_costs[['add by GW']] <- rbind(addition_costs[['add by GW']], hydro_GW_inc)

  # Apply costs to new hydropower numbers
  hydro_cost_inc <- hydro_GW_inc
  hydro_cost_inc[5:col_len] <- metis.assumptions()$hydro_cost_GW*hydro_cost_inc[5:col_len]
  hydro_cost_inc$Units <- 'billion 2010 USD'
  addition_costs[['add by cost']] <- addition_costs[['add by cost']] %>% filter(agg_tech!='Hydro')
  addition_costs[['add by cost']] <- rbind(addition_costs[['add by cost']], hydro_cost_inc)
  year_seq <- seq(start_year, end_year, 5)
  return(list("addition_costs"=addition_costs))
}

