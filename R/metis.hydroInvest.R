#' metis.hydroInvest
#'
#' Function that calculates electricity subsector investment requirements from a given GCAM run.
#'
#' @param addition_costs list formatted as produced by metis.elecInvest.R function
#' @param start_year start year for analysis
#' @keywords investments, infrastructure
#' @return Returns data in a form required by metislreadgcam.R
#' @export

metis.hydroInvest <- function(addition_costs, start_year=2010){

  #----------------
  # Initialize variables by setting to NULL
  #----------------

  NULL -> Units -> scenario -> region -> value -> cumValue -> valuePrev ->
    newCap -> agg_tech


  # Calculations hydropower investment, as elecInvest() function does not handle hydropower.

  # Add hydro installed capacity to addition_costs using addition_costs[['elec_prod']]
  hydro_energy <- addition_costs[['elec_prod']] %>% dplyr::filter(agg_tech=='Hydro')
  hydro_energy <- hydro_energy %>% dplyr::mutate(agg_tech = replace(agg_tech, agg_tech == "k Hydro", "Hydro"))  # Replace "k Hydro" with "Hydro"
  hydro_energy_orig <- hydro_energy

  # Convert from energy to GW using capacity factor, then insert into GW dataframe
  firstYrLoc <- match(start_year,names(hydro_energy))
  years <- c(firstYrLoc:length(hydro_energy)); years
  for (yr in years){
    if(yr==years[1]){hydro_energy[yr]<- hydro_energy_orig[yr]}else{
    hydro_energy[yr] <- hydro_energy_orig[yr] - hydro_energy_orig[yr-1]} # For first year assign original hydro energy otherwise lag
  }

  # Modifying so that new hydro capacity is only considered when capacity increase beyond historical max
  # This is assuming hydropower dams have a very long life-time and do not retire.
  hydro_energy_inc <- hydro_energy %>%
    tidyr::gather(key="year",value="value",-Units,-scenario,-region,-agg_tech) %>%
    dplyr::group_by(scenario, region,Units, agg_tech) %>%
    dplyr::mutate(cumValue = cumsum(value),
                  valuePrev = dplyr::lag(cumValue, n = 1, default = NA),
                  valuePrev = dplyr::if_else(is.na(valuePrev),cumValue,valuePrev),
                  newCap = dplyr::if_else(cumValue>valuePrev,cumValue-valuePrev,0))%>%
    dplyr::select(-cumValue,-valuePrev,-value)%>%
    dplyr::rename(value=newCap)%>%
    tidyr::spread(key="year",value="value")%>%
    dplyr::ungroup();hydro_energy_inc


  col_len <- length(hydro_energy_inc)

  hydro_GW_inc <- hydro_energy_inc %>%
    tidyr::gather(key="year",value="value",-Units,-scenario,-region,-agg_tech) %>%
    dplyr::mutate(value=value*metis.assumptions()$convEJ2GWh*(1/(8760 *  metis.assumptions()$hydro_cap_fact)),
                  Units="GW")%>%
    tidyr::spread(key="year",value="value")%>%
    dplyr::ungroup(); hydro_GW_inc
  addition_costs[['newCap_GW']] <- addition_costs[['newCap_GW']] %>% dplyr::filter(agg_tech!='Hydro')
  addition_costs[['newCap_GW']] <- rbind(addition_costs[['newCap_GW']], hydro_GW_inc);   addition_costs[['newCap_GW']]

  hydro_GW_inc_cum <-  hydro_GW_inc %>%
    tidyr::gather(key="year",value="value",-Units,-scenario,-region,-agg_tech) %>%
    dplyr::group_by(scenario, region,Units, agg_tech) %>%
    dplyr::mutate(value = cumsum(value)) %>%
    dplyr::ungroup()%>%
    tidyr::spread(key="year",value="value"); hydro_GW_inc_cum
  addition_costs[['cumCap_GW']] <- addition_costs[['cumCap_GW']] %>% dplyr::filter(agg_tech!='Hydro')
  addition_costs[['cumCap_GW']] <- rbind(addition_costs[['cumCap_GW']], hydro_GW_inc_cum); addition_costs[['cumCap_GW']]

  # Apply costs to new hydropower numbers
  hydro_cost_inc <- hydro_GW_inc %>%
    tidyr::gather(key="year",value="value",-Units,-scenario,-region,-agg_tech) %>%
    dplyr::ungroup()%>%
    dplyr::mutate(value=value*metis.assumptions()$hydro_cost_GW,
                  Units = 'billion 2010 USD',
                  value = dplyr::if_else(value<=0,0,value)) %>%
    tidyr::spread(key="year",value="value") %>%
    dplyr::ungroup(); hydro_cost_inc
  addition_costs[['newCap_cost']] <- addition_costs[['newCap_cost']] %>% dplyr::filter(agg_tech!='Hydro')
  addition_costs[['newCap_cost']] <- rbind(addition_costs[['newCap_cost']], hydro_cost_inc);  addition_costs[['newCap_cost']]

  hydro_cost_inc_cum <-  hydro_cost_inc %>%
    tidyr::gather(key="year",value="value",-Units,-scenario,-region,-agg_tech) %>%
    dplyr::group_by(scenario, region,Units, agg_tech) %>%
    dplyr::mutate(value = cumsum(value)) %>%
    dplyr::ungroup()%>%
    tidyr::spread(key="year",value="value"); hydro_cost_inc_cum
  addition_costs[['cumCap_cost']] <- addition_costs[['cumCap_cost']] %>% dplyr::filter(agg_tech!='Hydro')
  addition_costs[['cumCap_cost']] <- rbind(addition_costs[['cumCap_cost']], hydro_cost_inc_cum); addition_costs[['cumCap_cost']]


  return(list("addition_costs"=addition_costs))
}
