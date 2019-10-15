#' metis.elecInvest
#'
#' Function that calculates electricity subsector investment requirements from a given GCAM run.
#'
#' @param data Data table for charting
#' @param elec_gen_vintage Electricity vintage query result
#' @param start_year Start year of time frame of interest for analysis
#' @param end_year end_year of time frame of interest for analysis
#' @param world_regions GCAM regions for which to collect data
#' @keywords investments, infrastructure
#' @return Returns data in a form required by metislreadgcam.R
#' @export

elecInvest <- function(elec_gen_vintage, world_regions, start_year=2015, end_year=2050, skiprows=0) {
    # Load all support functions into memory
    source( "R/metis.diagHeaderSA.R" )	# configuration and helper functions

    #logstart( "diagnostics.R", savelog=F )

    # ============================================================================
    # Mapping files
    s_curve_shutdown <- read.csv("dataFiles/gcam/A23.globaltech_retirement.csv", skip=1)
    # s_curve_shutdown <- read.csv("mappings/A23.globaltech_retirement_low_shutdown.csv", skip=1)
    years_mapping <- read.csv("dataFiles/gcam/years_mapping.csv", skip=1)

    # cap_cost <- read.csv("inputs/mappings/capital_cost_mapping_4.4.csv", skip=4)
    cap_cost_tech <- read.csv("dataFiles/gcam/L2233.GlobalTechCapital_elecPassthru.csv", skip=4, stringsAsFactors = FALSE)
    cap_cost_cool <- read.csv("dataFiles/gcam/L2233.GlobalTechCapital_elec_cool.csv", skip=4, stringsAsFactors = FALSE)
    cap_cost_int_tech <- read.csv("dataFiles/gcam/L2233.GlobalIntTechCapital_elec.csv", skip=4, stringsAsFactors = FALSE)
    cap_cost_int_cool <- read.csv("dataFiles/gcam/L2233.GlobalIntTechCapital_elec_cool.csv", skip=4, stringsAsFactors = FALSE)

    # Combine the cooling technology cost sheets, and the electricity generating technology cost dataframes
    elec_gen_tech_cost <- rbind(cap_cost_tech, cap_cost_int_tech)
    # Get dispatchable capacity factor column added to elec_gen_tech_cost
    capac_fac <- read.csv("dataFiles/gcam/L223.GlobalTechCapFac_elec.csv", skip=4, stringsAsFactors = FALSE)
    capac_fac %>% select(-sector.name, -subsector.name) -> capac_fac_new
    elec_gen_tech_cost <- merge(elec_gen_tech_cost, capac_fac_new, by=c("technology", "year"), all=TRUE)
    elec_gen_tech_cost <- elec_gen_tech_cost[, c(3,4,1,2,5,6,7,8)]  # Rearrange columns
    # Get intermittent capacity factor column added to elec_gen_tech_cost
    capac_fac_int <- read.csv("dataFiles/gcam/L223.GlobalIntTechCapFac_elec.csv", skip=4, stringsAsFactors = FALSE)
    capac_fac_int %>% select(-sector.name, -subsector.name) %>%
      rename(technology=intermittent.technology) %>% rename(capacity.factor.temp=capacity.factor) -> capac_fac_int_new
    elec_gen_tech_cost <- merge(elec_gen_tech_cost, capac_fac_int_new, by=c("technology", "year"), all=TRUE)
    elec_gen_tech_cost <- elec_gen_tech_cost[, c(3,4,1,2,5,6,7,8,9)]  # Rearrange columns
    elec_gen_tech_cost[is.na(elec_gen_tech_cost)] <- 0
    elec_gen_tech_cost %>% mutate(capacity.factor=capacity.factor + capacity.factor.temp) %>%
      select(-capacity.factor.temp) ->elec_gen_tech_cost

    cool_tech_cost <- rbind(cap_cost_cool, cap_cost_int_cool)
    cool_tech_cost['capacity.factor'] <- NA  # New column for cap fac
    cool_tech_cost['old.technology'] <- NA  # New column for cap fac
    # Make list of years and technologies (by cooling)
    elec_tech_names_by_cooling_tech <- unique(cool_tech_cost$technology)  # Elec gen by cool tech to loop through
    years <- unique(cool_tech_cost$year)  # Years to loop through
    # Loop to replace costs with addition of capital costs and cooling technology costs.
    for (tech_name in elec_tech_names_by_cooling_tech){
      for (yr in years) {
        old_tech_name <- paste0(cool_tech_cost$subsector.name[(cool_tech_cost$year==yr) &
                                                                (cool_tech_cost$technology==tech_name)][1])
        cool_tech_cost$capital.overnight[(cool_tech_cost$year==yr) & (cool_tech_cost$technology==tech_name)] <-
        filter(elec_gen_tech_cost, year==yr, technology==paste0(cool_tech_cost$subsector.name[(cool_tech_cost$year==yr) &
          (cool_tech_cost$technology==tech_name)][1]))$capital.overnight +
          cool_tech_cost$capital.overnight[(cool_tech_cost$year==yr) & (cool_tech_cost$technology==tech_name)]

        cool_tech_cost$subsector.name[(cool_tech_cost$year==yr) & (cool_tech_cost$technology==tech_name)] <-
          paste0(filter(elec_gen_tech_cost, year==yr, technology==old_tech_name)$subsector.name[1])

        cool_tech_cost$old.technology[(cool_tech_cost$year==yr) & (cool_tech_cost$technology==tech_name)] <-
          paste0(filter(elec_gen_tech_cost, year==yr, technology==old_tech_name)$technology[1])

        cool_tech_cost$capacity.factor[(cool_tech_cost$year==yr) & (cool_tech_cost$technology==tech_name)] <-
         filter(elec_gen_tech_cost, year==yr, technology==old_tech_name)$capacity.factor[1]

      }
    }
    cap_cost <- cool_tech_cost
    A <- unique(cool_tech_cost$old.technology)
    B <- unique(elec_gen_tech_cost$technology)
    C <- setdiff(B,A)
    D <- filter(elec_gen_tech_cost, technology %in% C)
    D['old.technology'] <- NA
    cap_cost <- rbind(cap_cost, D)

    tech_mapping <- read.csv("dataFiles/gcam/agg_tech_mapping.csv", skip=1)

    # ============================================================================
    # Some constants and conversion factors

    # Constants
    tech_order <- c("Coal", "Coal CCS", "Gas", "Gas CCS", "Oil", "Oil CCS", "Biomass", "Biomass CCS", "Nuclear",
                    "Geothermal", "Hydro", "Wind", "Solar", "CHP", "Battery", "energy reduction")

    # Conversions
    conv_C_CO2 <- 44/12
    conv_MT_GT <- 1e-3
    print(world_regions)

    # ============================================================================

    #printlog( "Read In Electricity Generation by Vintage Data, Format, Filter" )

    # Filter scenarios that meet the cumulative emissions budgets
    print(world_regions)

    elec_gen_vintage %>%
      separate(scenario, "scenario", sep = ",") %>%
      gather(Year, value, - scenario, -region, -subsector, -technology, -Units) %>%
      mutate(Year = gsub('X', '', Year)) %>%
      mutate(Year = as.numeric(Year)) %>%
      mutate(value = as.numeric(value)) %>%
      separate(technology, c("technology", "temp"), sep = ",") %>%
      separate(temp, c("temp", "vintage"), sep = "=") %>%
      select(-temp) %>%
      mutate(vintage = as.numeric(vintage)) %>%
      filter(region %in% world_regions, Year <= end_year, vintage >= 2010, vintage <= end_year, Year >= vintage) -> elec_vintage

    #printlog( "Gross Additions and Retirements by Vintage" )

    # Calculate additions by vintage
    elec_vintage %>%
      mutate(additions = if_else(vintage == Year, value, 0)) -> elec_vintage_add

    # Calculate retirements by vintage
    elec_vintage %>%
      group_by(scenario, region, subsector, technology, Units, vintage) %>%
      mutate(prev_year = lag(value, n = 1L)) %>%
      ungroup() %>%
      mutate(prev_year = if_else(is.na(prev_year), 0, prev_year),
             retirements = prev_year - value,
             retirements = if_else(retirements < 0, 0, retirements)) %>%
      arrange(vintage, technology, region) -> elec_vintage_ret


    #printlog( "Expected Natural Retirements" )

    # Calculate s-curve output fraction
    # Hydro assumed to never retire, lifetime set to 110 years (hitting error, for now hydro is NA)

    elec_vintage %>%
      left_join(years_mapping, by = c("vintage")) %>%
      left_join(s_curve_shutdown %>% select(-supplysector),
                by = c("subsector", "technology", "year")) %>%
      # mutate(lifetime = if_else(technology == "hydro", 110, lifetime)) %>%
      mutate(half.life = as.numeric(half.life),
             steepness = as.numeric(steepness),
             half.life = if_else(is.na(half.life), 0, half.life),
             steepness = if_else(is.na(steepness), 0, steepness),
             s_curve_frac = if_else(Year > vintage & half.life != 0,
                                    (1 / (1 + exp( steepness * ((Year - vintage) - half.life )))),
                                    1)) -> s_curve_frac

    # Adjust s-curve output fraction to ensure that all of the capacity is retired at the end of lifetime
    s_curve_frac %>%
      mutate(s_curve_adj = if_else(Year - vintage >= lifetime, 0, s_curve_frac),
             s_curve_adj = if_else(is.na(s_curve_adj), 1, s_curve_adj)) %>%
      select(scenario, region, subsector, technology, vintage, Units, Year, value, s_curve_adj) -> s_curve_frac_adj

    # Expected generation assuming natural shutdowns only
    # Create variable reflecting each tech/ vintage generation in year of installment (OG_gen)
    s_curve_frac_adj %>%
      left_join(elec_vintage %>%
                  filter(vintage == Year) %>%
                  select(-Year) %>%
                  rename(OG_gen = value),
                by = c("scenario", "region", "subsector", "technology", "vintage", "Units")) %>%
      mutate(gen_expect = OG_gen * s_curve_adj) -> elec_gen_expect

    # libs <- c( "reshape2", "stringr", "scales", "readr", "plyr", "tidyr", "dplyr", "ggplot2" )
    #
    # elec_vintage %>%
    #   filter(vintage == Year) %>%
    #   select(-Year) %>%
    #   rename(value = OG_gen) -> TEST

    # Expected natural retirements
    elec_gen_expect %>%
      group_by(scenario, region, subsector, technology, Units, vintage) %>%
      mutate(prev_yr_expect = lag(gen_expect, n = 1L),
             natural_retire = if_else(Year > vintage & prev_yr_expect > gen_expect, prev_yr_expect - gen_expect, 0)) %>%
      ungroup() -> elec_retire_expect


    #printlog( "Gross Additions and Retirements by Technology" )

    # Total additions per region/ technology/ year (in EJ)
    elec_vintage_add %>%
      group_by(scenario, region, subsector, technology, Units, Year) %>%
      summarise(additions = sum(additions)) %>%
      ungroup() -> elec_total_add

    # Total retirements per region/ technology/ year (in EJ)
    elec_vintage_ret %>%
      group_by(scenario, region, subsector, technology, Units, Year) %>%
      summarise(retirements = sum(retirements)) %>%
      ungroup() -> elec_total_ret


    #printlog( "Adjusted Additions and Retirements by Vintage" )

    # Adjusted additions and retirements
    # Merge total additions and retirements data tables
    elec_total_add %>%
      left_join(elec_total_ret, by = c("scenario", "region", "subsector", "technology", "Units", "Year")) %>%
      mutate(add_adj = if_else(additions > retirements, additions - retirements, 0),
             ret_adj = if_else(retirements > additions, retirements - additions, 0)) -> elec_add_ret

    # Assign adjusted retirements to vintages, assuming older vintages retire first
    # Merge retirement by vintage and retirement by year data tables
    elec_vintage_ret %>%
      select(-value, -prev_year) %>%
      left_join(elec_add_ret %>%
                  select(-additions, -retirements, -add_adj),
                by = c("scenario", "region", "subsector", "technology", "Units", "Year")) -> elec_ret_adj_vintage

    # Filter years with zero adjusted retirements, set retirements for each vintage to zero
    elec_ret_adj_vintage %>%
      filter(ret_adj == 0) %>%
      mutate(retirements = ret_adj) -> elec_ret_adj_0

    # Filter years with non-zero adjusted retirements
    elec_ret_adj_vintage %>%
      filter(ret_adj != 0) -> elec_ret_adj

    # Create list of adjusted retirements by technology / year
    elec_ret_adj_vintage %>%
      distinct(scenario, region, subsector, technology, Units, Year, ret_adj) -> elec_ret_adj_year

    vintage <- unique(elec_ret_adj_vintage$Year)
    elec_ret_adjust <- tibble()

    for (v in vintage) {

      # Assign adjusted retirements to vintages, assuming older vintages retire first
      elec_ret_adj %>%
        filter(vintage == v) %>%
        select(-ret_adj) %>%
        left_join(elec_ret_adj_year,
                  by = c("scenario", "region", "subsector", "technology", "Units", "Year")) %>%
        mutate(ret_adj = ret_adj - retirements,
               retirements = if_else(ret_adj < 0, retirements + ret_adj, retirements),
               ret_adj = if_else(ret_adj < 0, 0, ret_adj)) -> elec_ret_adj_temp

      elec_ret_adjust %>%
        bind_rows(elec_ret_adj_temp) -> elec_ret_adjust

      # Revise list of adjusted retirements by technology / year, removing retirements allocated to vintage v
      elec_ret_adj_year %>%
        rename(ret_adj_OG = ret_adj) %>%
        left_join(elec_ret_adj_temp %>%
                    select(-vintage, -retirements),
                  by = c("scenario", "region", "subsector", "technology", "Units", "Year")) %>%
        mutate(ret_adj = if_else(is.na(ret_adj), ret_adj_OG, ret_adj)) %>%
        select(-ret_adj_OG) -> elec_ret_adj_year

    }

    # Re-bind adjusted retirements data frames
    elec_ret_adjust %>%
      select(-ret_adj) %>%
      bind_rows(elec_ret_adj_0 %>% select(-ret_adj)) %>%
      filter(Year >= vintage) -> elec_ret_vintage


    #printlog( "Adjusted Additions and Retirements by Vintage" )

    # Subtract expected retirements to calculate premature retirements
    elec_ret_vintage %>%
      left_join(elec_retire_expect %>%
                  select(scenario, region, subsector, technology, Units, Year, vintage, natural_retire),
                by = c("scenario", "region", "subsector", "technology", "Units", "Year", "vintage")) %>%
      mutate(early_ret = if_else(retirements > natural_retire, retirements - natural_retire, 0)) -> elec_ret_premature


    #printlog( "Adjusted Additions in GW and billion 2010 USD" )

    # Calculate final (adjusted) additions in GW
    elec_add_ret %>%
      select(-ret_adj) %>%
      left_join(cap_cost %>%
                  select(-sector.name, -input.capital, -fixed.charge.rate),
                by = c("subsector" = "subsector.name", "technology", "Year" = "year")) %>%
      mutate(add_GW = (add_adj * metis.assumptions()$convEJ2GWh) / (8760 * capacity.factor),
             Units = "GW") -> elec_add_GW

    # Calculate final capital investments in billion 2010 USD
    elec_add_GW %>%
      mutate(cap_invest = (add_GW * metis.assumptions()$convGW_kW * capital.overnight * metis.assumptions()$ConvUSD_1975_2010) / 1e9,
             Units = "billion 2010 USD") -> elec_add_cap_invest


    #printlog( "Premature Retirements in GW and billion 2010 USD" )

    # Calculate final premature retirements in GW
    # NOTE:  dividing capital costs for 2010 vintages in half
    elec_ret_premature %>%
      select(-retirements, -natural_retire) %>%
      left_join(cap_cost %>%
                  select(-sector.name, -input.capital, -fixed.charge.rate),
                by = c("subsector" = "subsector.name", "technology", "vintage" = "year")) %>%
      mutate(capital.overnight = if_else(vintage == 2010, capital.overnight * .5, capital.overnight * 1),
             early_ret_GW = (early_ret * metis.assumptions()$convEJ2GWh) / (8760 * capacity.factor),
             Units = "GW") -> elec_ret_GW

    # Calculate unrecovered capital costs of prematurely retired assets
    # Calculate depreciation factor for prematurely retired assets
    elec_ret_GW %>%
      left_join(years_mapping, by = c("vintage")) %>%
      left_join(s_curve_shutdown %>%
                  select(technology, year, lifetime),
                by = c("technology", "year")) %>%
      mutate(dep_factor = 1 - ((Year - vintage) / lifetime),
             unrec_cap = (early_ret_GW * metis.assumptions()$convGW_kW * capital.overnight * dep_factor * metis.assumptions()$ConvUSD_1975_2010) / 1e9,
             Units = "billion 2010 USD") -> elec_ret_cap_cost

    # ============================================================================

    #printlog( "Prepare Data for Output, Print" )

    #printlog( "Additions ($) by region & technology" )

    # Additions by region & technology
    elec_add_cap_invest %>%
      left_join(tech_mapping, by = c("technology")) %>%
      group_by(scenario, region, Year, Units, agg_tech) %>%
      summarise(cap_invest = sum(cap_invest)) %>%
      ungroup() %>%
      filter(Year >= start_year) %>%
      spread(Year, cap_invest) %>%
      mutate(agg_tech = factor(agg_tech, levels = tech_order)) %>%
      arrange(region, agg_tech) -> add_data_final

    # Additional code by Tom to track by region & technology
    elec_add_cap_invest %>%
      left_join(tech_mapping, by = c("technology")) %>%
      group_by(scenario, region, Year, Units, agg_tech) %>%
      summarise(add_adj = sum(add_adj)) %>%
      ungroup() %>%
      filter(Year >= start_year) %>%
      spread(Year, add_adj) %>%
      mutate(agg_tech = factor(agg_tech, levels = tech_order)) %>%
      arrange(region, agg_tech) -> delta_final


    # Additions by technology (LAC total)
    elec_add_cap_invest %>%
      left_join(tech_mapping, by = c("technology")) %>%
      group_by(scenario, Year, Units, agg_tech) %>%
      summarise(cap_invest = sum(cap_invest)) %>%
      ungroup() %>%
      mutate(region = "LAC") %>%
      filter(Year >= start_year) %>%
      spread(Year, cap_invest) %>%
      mutate(agg_tech = factor(agg_tech, levels = tech_order)) %>%
      arrange(region, agg_tech) -> add_data_final_LAC

    # Additions by region & technology (including LAC total)
    add_data_final %>%
      bind_rows(add_data_final_LAC) -> add_data_final

    # ============================================================================

    # #printlog( "Additions ($) by region" )
    #
    # # Additions by region
    # elec_add_cap_invest %>%
    #   mutate(cap_invest = if_else(is.na(cap_invest), 0, cap_invest)) %>%
    #   group_by(scenario, region, Year, Units) %>%
    #   summarise(cap_invest = sum(cap_invest)) %>%
    #   ungroup() %>%
    #   filter(Year >= start_year) %>%
    #   spread(Year, cap_invest) -> add_data_agg
    #
    # # Additions (LAC total)
    # elec_add_cap_invest %>%
    #   mutate(cap_invest = if_else(is.na(cap_invest), 0, cap_invest)) %>%
    #   group_by(scenario, Year, Units) %>%
    #   summarise(cap_invest = sum(cap_invest)) %>%
    #   ungroup() %>%
    #   mutate(region = "LAC") %>%
    #   filter(Year >= start_year) %>%
    #   spread(Year, cap_invest) -> add_data_agg_LAC
    #
    # # Additions by region (including LAC total)
    # add_data_agg %>%
    #   bind_rows(add_data_agg_LAC) -> add_data_agg

    # ============================================================================

    #printlog( "Premature retirements ($) by region & technology" )

    # Premature retirements by region & technology
    elec_ret_cap_cost %>%
      left_join(tech_mapping, by = c("technology")) %>%
      group_by(scenario, region, Year, Units, agg_tech) %>%
      summarise(unrec_cap = sum(unrec_cap)) %>%
      ungroup() %>%
      mutate(unrec_cap = unrec_cap * -1) %>%
      filter(Year >= start_year) %>%
      spread(Year, unrec_cap) %>%
      mutate(agg_tech = factor(agg_tech, levels = tech_order)) %>%
      arrange(region, agg_tech) -> ret_data_final

    # Premature retirements by technology (LAC total)
    elec_ret_cap_cost %>%
      left_join(tech_mapping, by = c("technology")) %>%
      group_by(scenario, Year, Units, agg_tech) %>%
      summarise(unrec_cap = sum(unrec_cap)) %>%
      ungroup() %>%
      mutate(region = "LAC") %>%
      mutate(unrec_cap = unrec_cap * -1) %>%
      filter(Year >= start_year) %>%
      spread(Year, unrec_cap) %>%
      mutate(agg_tech = factor(agg_tech, levels = tech_order)) %>%
      arrange(region, agg_tech) -> ret_data_final_LAC

    # Premature retirements by region & technology (including LAC total)
    ret_data_final %>%
      bind_rows(ret_data_final_LAC) -> ret_data_final

    # ============================================================================

    # #printlog( "Premature retirements ($) by region" )
    #
    # # Premature retirements by region
    # elec_ret_cap_cost %>%
    #   mutate(unrec_cap = if_else(is.na(unrec_cap), 0, unrec_cap)) %>%
    #   group_by(scenario, region, Year, Units) %>%
    #   summarise(unrec_cap = sum(unrec_cap)) %>%
    #   ungroup() %>%
    #   filter(Year >= start_year) %>%
    #   spread(Year, unrec_cap) -> ret_data_agg
    #
    # # Premature retirements (LAC total)
    # elec_ret_cap_cost %>%
    #   mutate(unrec_cap = if_else(is.na(unrec_cap), 0, unrec_cap)) %>%
    #   group_by(scenario, Year, Units) %>%
    #   summarise(unrec_cap = sum(unrec_cap)) %>%
    #   ungroup() %>%
    #   mutate(region = "LAC") %>%
    #   filter(Year >= start_year) %>%
    #   spread(Year, unrec_cap) -> ret_data_agg_LAC
    #
    # # Premature retirements by region (including LAC total)
    # ret_data_agg %>%
    #   bind_rows(ret_data_agg_LAC) -> ret_data_agg

    # ============================================================================

    #printlog( "Additions (GW) by region & technology" )

    # Additions by region & technology
    elec_add_GW %>%
      left_join(tech_mapping, by = c("technology")) %>%
      group_by(scenario, region, Year, Units, agg_tech) %>%
      summarise(add_GW = sum(add_GW)) %>%
      ungroup() %>%
      filter(Year >= start_year) %>%
      spread(Year, add_GW) %>%
      mutate(agg_tech = factor(agg_tech, levels = tech_order)) %>%
      arrange(region, agg_tech) -> add_GW_data

    # Additions by technology (LAC total)
    elec_add_GW %>%
      left_join(tech_mapping, by = c("technology")) %>%
      group_by(scenario, Year, Units, agg_tech) %>%
      summarise(add_GW = sum(add_GW)) %>%
      ungroup() %>%
      mutate(region = "LAC") %>%
      filter(Year >= start_year) %>%
      spread(Year, add_GW) %>%
      mutate(agg_tech = factor(agg_tech, levels = tech_order)) %>%
      arrange(region, agg_tech) -> add_GW_data_LAC

    # Additions by region & technology (including LAC total)
    add_GW_data %>%
      bind_rows(add_GW_data_LAC) -> add_GW_data_final

    # ============================================================================

    #printlog( "Premature retirements (GW) by region & technology" )

    # Premature retirements by region & technology
    elec_ret_GW %>%
      left_join(tech_mapping, by = c("technology")) %>%
      group_by(scenario, region, Year, Units, agg_tech) %>%
      summarise(early_ret_GW = sum(early_ret_GW)) %>%
      ungroup() %>%
      mutate(early_ret_GW = early_ret_GW * -1) %>%
      filter(Year >= start_year) %>%
      spread(Year, early_ret_GW) %>%
      mutate(agg_tech = factor(agg_tech, levels = tech_order)) %>%
      arrange(region, agg_tech) -> ret_GW_data

    # Premature retirements by technology (LAC total)
    elec_ret_GW %>%
      left_join(tech_mapping, by = c("technology")) %>%
      group_by(scenario, Year, Units, agg_tech) %>%
      summarise(early_ret_GW = sum(early_ret_GW)) %>%
      ungroup() %>%
      mutate(region = "LAC") %>%
      mutate(early_ret_GW = early_ret_GW * -1) %>%
      filter(Year >= start_year) %>%
      spread(Year, early_ret_GW) %>%
      mutate(agg_tech = factor(agg_tech, levels = tech_order)) %>%
      arrange(region, agg_tech) -> ret_GW_data_LAC

    # Premature retirements by region & technology (including LAC total)
    ret_GW_data %>%
      bind_rows(ret_GW_data_LAC) -> ret_GW_data_final

    # ============================================================================

    # Print data
    write.csv(add_data_final, "outputs/stranding/add_data_final.csv", row.names = F)
    write.csv(ret_data_final, "outputs/stranding/ret_data_final.csv", row.names = F)
    # write.csv(add_data_agg, "stranding/add_data_agg.csv", row.names = F)
    # write.csv(ret_data_agg, "stranding/ret_data_agg.csv", row.names = F)
    write.csv(add_GW_data_final, "outputs/stranding/add_GW_data_final.csv", row.names = F)
    write.csv(ret_GW_data_final, "outputs/stranding/ret_GW_data_final.csv", row.names = F)

    # ============================================================================
    return(list("add by GW"=add_GW_data, "add by cost"=add_data_final, "Delta elec"=delta_final))
    logstop()
}


Hydropower_Investment <- function(addition_costs){

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

