#' metis.elecInvest
#'
#' Function that calculates electricity subsector investment requirements from a given GCAM run.
#'
#' @param elec_gen_vintage Electricity vintage query result
#' @param start_year Start year of time frame of interest for analysis
#' @param end_year end_year of time frame of interest for analysis
#' @param world_regions GCAM regions for which to collect data
#' @keywords investments, infrastructure
#' @return Returns data in a form required by metislreadgcam.R
#' @export

metis.elecInvest <- function(elec_gen_vintage, world_regions, start_year=2010, end_year=2050) {


  #----------------
  # Initialize variables by setting to NULL
  #----------------

  NULL -> year ->technology->wtechnology->sector.name->subsector.name->
    intermittent.technology->capacity.factor->capacity.factor.temp->sector->
    Year->value->scenario->region->subsector->Units->temp->prev_year->retirements->
    supplysector->half.life->steepness->lifetime->s_curve_adj->OG_gen->gen_expect->
    prev_yr_expect->additions->add_adj->ret_adj->ret_adj_OG->natural_retire->input.capital->
    fixed.charge.rate->add_GW->capital.overnight->early_ret->early_ret_GW->agg_tech->
    cap_invest->mutate_all->unrec_Cap-> dep_factor -> unrec_cap

  # ============================================================================
  # Mapping files

  years_mapping <- (data.frame(year=c(rep("final-calibration-year",1),rep("initial-future-year",18)),
                              vintage=c(metis.assumptions()$GCAMbaseYear,seq(metis.assumptions()$GCAMbaseYear+5,2100,by=5))))%>%
    dplyr::mutate(year=as.character(year));years_mapping

  cap_cost_tech <- data.table::fread(paste(getwd(),"/dataFiles/gcam/investCalcs/L2233.GlobalTechCapital_elecPassthru.csv", sep=""), skip=1, stringsAsFactors = FALSE)
  cap_cost_cool <- data.table::fread(paste(getwd(),"/dataFiles/gcam/investCalcs/L2233.GlobalTechCapital_elec_cool.csv", sep=""), skip=1, stringsAsFactors = FALSE)
  cap_cost_int_tech <- data.table::fread(paste(getwd(),"/dataFiles/gcam/investCalcs/L2233.GlobalIntTechCapital_elec.csv", sep=""), skip=1, stringsAsFactors = FALSE)
  cap_cost_int_cool <- data.table::fread(paste(getwd(),"/dataFiles/gcam/investCalcs/L2233.GlobalIntTechCapital_elec_cool.csv", sep=""), skip=1, stringsAsFactors = FALSE)

  s_curve_shutdown <- tibble::as_tibble(data.table::fread(paste(getwd(),"/dataFiles/gcam/investCalcs/A23.globaltech_retirement.csv",sep=""), skip=1))%>%
    dplyr::mutate(year=dplyr::if_else(year=="final-historical-year","final-calibration-year",year),
                  year=dplyr::if_else(year=="initial-nonhistorical-year","initial-future-year",year)); s_curve_shutdown

  # Add water cooling technologies if they dont exist
  waterTechs <- s_curve_shutdown %>% dplyr::select(technology); waterTechs
  if(any(!grepl("once through",unique(waterTechs$technology),ignore.case = T))){
    waterTechsCooling <- waterTechs %>% dplyr::mutate(wtechnology=paste(technology," (dry cooling)",sep="")) %>%
      dplyr::bind_rows(waterTechs %>% dplyr::mutate(wtechnology=paste(technology," (once through)",sep=""))) %>%
      dplyr::bind_rows(waterTechs %>% dplyr::mutate(wtechnology=paste(technology," (recirculating)",sep=""))) %>%
      dplyr::bind_rows(waterTechs %>% dplyr::mutate(wtechnology=paste(technology," (seawater)",sep="")))
  }else{waterTechsCooling <- waterTechs %>% dplyr::mutate(wtechnology=technology)}

  s_curve_shutdown <- s_curve_shutdown %>%
    dplyr::left_join(waterTechsCooling,by="technology")%>%
    dplyr::mutate(technology=wtechnology)%>%
    dplyr::select(-wtechnology)

  # Combine the cooling technology cost sheets, and the electricity generating technology cost dataframes
  elec_gen_tech_cost <- rbind(cap_cost_tech, cap_cost_int_tech)
  # Get dispatchable capacity factor column added to elec_gen_tech_cost
  capac_fac <- data.table::fread(paste(getwd(),"/dataFiles/gcam/investCalcs/L223.GlobalTechCapFac_elec.csv", sep=""), skip=1, stringsAsFactors = FALSE)
  capac_fac %>% dplyr::select(-sector.name, -subsector.name) -> capac_fac_new
  elec_gen_tech_cost <- merge(elec_gen_tech_cost, capac_fac_new, by=c("technology", "year"), all=TRUE)
  elec_gen_tech_cost <- elec_gen_tech_cost[, c(3,4,1,2,5,6,7,8)]  # Redplyr::arrange columns
  # Get intermittent capacity factor column added to elec_gen_tech_cost
  capac_fac_int <- data.table::fread(paste(getwd(),"/dataFiles/gcam/investCalcs/L223.GlobalIntTechCapFac_elec.csv", sep=""), skip=1, stringsAsFactors = FALSE)
  capac_fac_int %>% dplyr::select(-sector.name, -subsector.name) %>%
    dplyr::rename(technology=intermittent.technology) %>% dplyr::rename(capacity.factor.temp=capacity.factor) -> capac_fac_int_new
  elec_gen_tech_cost <- merge(elec_gen_tech_cost, capac_fac_int_new, by=c("technology", "year"), all=TRUE)
  elec_gen_tech_cost <- elec_gen_tech_cost[, c(3,4,1,2,5,6,7,8,9)]  # Redplyr::arrange columns
  elec_gen_tech_cost[is.na(elec_gen_tech_cost)] <- 0
  elec_gen_tech_cost %>% dplyr::mutate(capacity.factor=capacity.factor + capacity.factor.temp) %>%
    dplyr::select(-capacity.factor.temp) ->elec_gen_tech_cost

  cool_tech_cost <- rbind(cap_cost_cool, cap_cost_int_cool)
  cool_tech_cost[,'capacity.factor'] <- NA  # New column for cap fac
  cool_tech_cost[,'old.technology'] <- NA  # New column for cap fac
  # Make list of years and technologies (by cooling)
  elec_tech_names_by_cooling_tech <- unique(cool_tech_cost$technology)  # Elec gen by cool tech to loop through
  years <- unique(cool_tech_cost$year)  # Years to loop through
  # Loop to replace costs with addition of capital costs and cooling technology costs.
  for (tech_name in elec_tech_names_by_cooling_tech){
    for (yr in years) {
      old_tech_name <- paste0(cool_tech_cost$subsector.name[(cool_tech_cost$year==yr) &
                                                              (cool_tech_cost$technology==tech_name)][1])
      cool_tech_cost$capital.overnight[(cool_tech_cost$year==yr) & (cool_tech_cost$technology==tech_name)] <-
        dplyr::filter(elec_gen_tech_cost, year==yr, technology==paste0(cool_tech_cost$subsector.name[(cool_tech_cost$year==yr) &
                                                                                                (cool_tech_cost$technology==tech_name)][1]))$capital.overnight +
        cool_tech_cost$capital.overnight[(cool_tech_cost$year==yr) & (cool_tech_cost$technology==tech_name)]

      cool_tech_cost$subsector.name[(cool_tech_cost$year==yr) & (cool_tech_cost$technology==tech_name)] <-
        paste0(dplyr::filter(elec_gen_tech_cost, year==yr, technology==old_tech_name)$subsector.name[1])

      cool_tech_cost$old.technology[(cool_tech_cost$year==yr) & (cool_tech_cost$technology==tech_name)] <-
        paste0(dplyr::filter(elec_gen_tech_cost, year==yr, technology==old_tech_name)$technology[1])

      cool_tech_cost$capacity.factor[(cool_tech_cost$year==yr) & (cool_tech_cost$technology==tech_name)] <-
        dplyr::filter(elec_gen_tech_cost, year==yr, technology==old_tech_name)$capacity.factor[1]

    }
  }
  cap_cost <- cool_tech_cost
  A <- unique(cool_tech_cost$old.technology)
  B <- unique(elec_gen_tech_cost$technology)
  C <- setdiff(B,A)
  D <- dplyr::filter(elec_gen_tech_cost, technology %in% C)
  D[,'old.technology'] <- NA
  cap_cost <- rbind(cap_cost, D)

  tech_mapping <- data.table::fread(paste(getwd(),"/dataFiles/gcam/investCalcs/agg_tech_mapping.csv", sep=""), skip=1)

  # ============================================================================
  # Some constants and conversion factors

  # Constants
  tech_order <- c("Coal", "Coal CCS", "Gas", "Gas CCS", "Oil", "Oil CCS", "Biomass", "Biomass CCS", "Nuclear",
                  "Geothermal", "Hydro", "Wind", "Solar", "CHP", "Battery", "energy reduction")


  # ============================================================================

  # dplyr::filter scenarios that meet the cumulative emissions budgets

  elec_gen_vintage %>%
    dplyr::select(-sector)%>%
    tidyr::gather(Year, value, - scenario, -region, -subsector, -technology, -Units) %>%
    dplyr::mutate(Year = gsub('X', '', Year)) %>%
    dplyr::mutate(Year = as.numeric(Year)) %>%
    dplyr::mutate(value = as.numeric(value)) %>%
    tidyr::separate(technology, c("technology", "temp"), sep = ",") %>%
    tidyr::separate(temp, c("temp", "vintage"), sep = "=") %>%
    dplyr::select(-temp) %>%
    dplyr::mutate(vintage = as.numeric(vintage)) %>%
    dplyr::filter(region %in% world_regions, Year <= end_year, vintage >= metis.assumptions()$GCAMbaseYear, vintage <= end_year, Year >= vintage) -> elec_vintage

  # Calculate additions by vintage
  elec_vintage %>%
    dplyr::mutate(additions = dplyr::if_else(vintage == Year, value, 0)) -> elec_vintage_add

  # Calculate retirements by vintage
  elec_vintage %>%
    dplyr::group_by(scenario, region, subsector, technology, Units, vintage) %>%
    dplyr::mutate(prev_year = dplyr::lag(value, n = 1L)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(prev_year = dplyr::if_else(is.na(prev_year), 0, prev_year),
           retirements = prev_year - value,
           retirements = dplyr::if_else(retirements < 0, 0, retirements)) %>%
    dplyr::arrange(vintage, technology, region) -> elec_vintage_ret

  # Calculate s-curve output fraction
  # Hydro assumed to never retire, lifetime set to 110 years (hitting error, for now hydro is NA)
  elec_vintage %>%
    dplyr::left_join(years_mapping, by = c("vintage")) %>%
    dplyr::left_join(s_curve_shutdown %>% dplyr::select(-supplysector),
              by = c("subsector", "technology", "year")) %>%
    # dplyr::mutate(lifetime = dplyr::if_else(technology == "hydro", 110, lifetime)) %>%
    dplyr::mutate(half.life = as.numeric(half.life),
           steepness = as.numeric(steepness),
           half.life = dplyr::if_else(is.na(half.life), 0, half.life),
           steepness = dplyr::if_else(is.na(steepness), 0, steepness),
           s_curve_frac = dplyr::if_else(Year > vintage & half.life != 0,
                                  (1 / (1 + exp( steepness * ((Year - vintage) - half.life )))),
                                  1)) %>%
    unique()-> s_curve_frac

  # Adjust s-curve output fraction to ensure that all of the capacity is retired at the end of lifetime
  s_curve_frac %>%
    dplyr::mutate(s_curve_adj = dplyr::if_else(Year - vintage >= lifetime, 0, s_curve_frac),
           s_curve_adj = dplyr::if_else(is.na(s_curve_adj), 1, s_curve_adj)) %>%
    dplyr::select(scenario, region, subsector, technology, vintage, Units, Year, value, s_curve_adj) -> s_curve_frac_adj

  # Expected generation assuming natural shutdowns only
  # Create variable reflecting each tech/ vintage generation in year of installment (OG_gen)
  s_curve_frac_adj %>%
    dplyr::left_join(elec_vintage %>%
                dplyr::filter(vintage == Year) %>%
                dplyr::select(-Year) %>%
                dplyr::rename(OG_gen = value),
              by = c("scenario", "region", "subsector", "technology", "vintage", "Units")) %>%
    dplyr::mutate(gen_expect = OG_gen * s_curve_adj) -> elec_gen_expect


  # Expected natural retirements
  elec_gen_expect %>%
    dplyr::group_by(scenario, region, subsector, technology, Units, vintage) %>%
    dplyr::mutate(prev_yr_expect = dplyr::lag(gen_expect, n = 1L),
           natural_retire = dplyr::if_else(Year > vintage & prev_yr_expect > gen_expect, prev_yr_expect - gen_expect, 0)) %>%
    dplyr::ungroup() -> elec_retire_expect


  # Total additions per region/ technology/ year (in EJ)
  elec_vintage_add %>%
    dplyr::group_by(scenario, region, subsector, technology, Units, Year) %>%
    dplyr::summarise(additions = sum(additions)) %>%
    dplyr::ungroup() -> elec_total_add

  # Total retirements per region/ technology/ year (in EJ)
  elec_vintage_ret %>%
    dplyr::group_by(scenario, region, subsector, technology, Units, Year) %>%
    dplyr::summarise(retirements = sum(retirements)) %>%
    dplyr::ungroup() -> elec_total_ret

  # Adjusted additions and retirements
  # Merge total additions and retirements data tables
  elec_total_add %>%
    dplyr::left_join(elec_total_ret, by = c("scenario", "region", "subsector", "technology", "Units", "Year")) %>%
    dplyr::mutate(add_adj = dplyr::if_else(additions > retirements, additions - retirements, 0),
           ret_adj = dplyr::if_else(retirements > additions, retirements - additions, 0)) -> elec_add_ret

  # Assign adjusted retirements to vintages, assuming older vintages retire first
  # Merge retirement by vintage and retirement by year data tables
  elec_vintage_ret %>%
    dplyr::select(-value, -prev_year) %>%
    dplyr::left_join(elec_add_ret %>%
                dplyr::select(-additions, -retirements, -add_adj),
              by = c("scenario", "region", "subsector", "technology", "Units", "Year")) -> elec_ret_adj_vintage

  # dplyr::filter years with zero adjusted retirements, set retirements for each vintage to zero
  elec_ret_adj_vintage %>%
    dplyr::filter(ret_adj == 0) %>%
    dplyr::mutate(retirements = ret_adj) -> elec_ret_adj_0

  # dplyr::filter years with non-zero adjusted retirements
  elec_ret_adj_vintage %>%
    dplyr::filter(ret_adj != 0) -> elec_ret_adj

  # Create list of adjusted retirements by technology / year
  elec_ret_adj_vintage %>%
    dplyr::distinct(scenario, region, subsector, technology, Units, Year, ret_adj) -> elec_ret_adj_year

  vintage <- unique(elec_ret_adj_vintage$Year)
  elec_ret_adjust <- dplyr::tibble()

  for (v in vintage) {

    # Assign adjusted retirements to vintages, assuming older vintages retire first
    elec_ret_adj %>%
      dplyr::filter(vintage == v) %>%
      dplyr::select(-ret_adj) %>%
      dplyr::left_join(elec_ret_adj_year,
                by = c("scenario", "region", "subsector", "technology", "Units", "Year")) %>%
      dplyr::mutate(ret_adj = ret_adj - retirements,
             retirements = dplyr::if_else(ret_adj < 0, retirements + ret_adj, retirements),
             ret_adj = dplyr::if_else(ret_adj < 0, 0, ret_adj)) -> elec_ret_adj_temp

    elec_ret_adjust %>%
      dplyr::bind_rows(elec_ret_adj_temp) -> elec_ret_adjust

    # Revise list of adjusted retirements by technology / year, removing retirements allocated to vintage v
    elec_ret_adj_year %>%
      dplyr::rename(ret_adj_OG = ret_adj) %>%
      dplyr::left_join(elec_ret_adj_temp %>%
                  dplyr::select(-vintage, -retirements),
                by = c("scenario", "region", "subsector", "technology", "Units", "Year")) %>%
      dplyr::mutate(ret_adj = dplyr::if_else(is.na(ret_adj), ret_adj_OG, ret_adj)) %>%
      dplyr::select(-ret_adj_OG) -> elec_ret_adj_year

  }

  # Re-bind adjusted retirements data frames
  elec_ret_adjust %>%
    dplyr::select(-ret_adj) %>%
    dplyr::bind_rows(elec_ret_adj_0 %>% dplyr::select(-ret_adj)) %>%
    dplyr::filter(Year >= vintage) -> elec_ret_vintage


  # Subtract expected retirements to calculate premature retirements
  elec_ret_vintage %>%
    dplyr::left_join(elec_retire_expect %>%
                dplyr::select(scenario, region, subsector, technology, Units, Year, vintage, natural_retire),
              by = c("scenario", "region", "subsector", "technology", "Units", "Year", "vintage")) %>%
    dplyr::mutate(early_ret = dplyr::if_else(retirements > natural_retire, retirements - natural_retire, 0)) -> elec_ret_premature


  # Calculate final (adjusted) additions in GW
  elec_add_ret %>%
    dplyr::select(-ret_adj) %>%
    dplyr::left_join(cap_cost %>%
                dplyr::select(-sector.name, -input.capital, -fixed.charge.rate),
              by = c("subsector" = "subsector.name", "technology", "Year" = "year")) %>%
    dplyr::mutate(add_GW = (add_adj * metis.assumptions()$convEJ2GWh) / (8760 * capacity.factor),
           Units = "GW") -> elec_add_GW

  # Calculate final capital investments in billion 2010 USD
  elec_add_GW %>%
    dplyr::mutate(cap_invest = (add_GW * metis.assumptions()$convGW_kW * capital.overnight * metis.assumptions()$convUSD_1975_2010) / 1e9,
           Units = "billion 2010 USD") -> elec_add_cap_invest

  # Calculate final premature retirements in GW
  # NOTE:  dividing capital costs for 2010 vintages in half
  elec_ret_premature %>%
    dplyr::select(-retirements, -natural_retire) %>%
    dplyr::left_join(cap_cost %>%
                dplyr::select(-sector.name, -input.capital, -fixed.charge.rate),
              by = c("subsector" = "subsector.name", "technology", "vintage" = "year")) %>%
    dplyr::mutate(capital.overnight = dplyr::if_else(vintage == 2010, capital.overnight * .5, capital.overnight * 1),
           early_ret_GW = (early_ret * metis.assumptions()$convEJ2GWh) / (8760 * capacity.factor),
           Units = "GW") -> elec_ret_GW

  # Calculate unrecovered capital costs of prematurely retired assets
  # Calculate depreciation factor for prematurely retired assets
  elec_ret_GW %>%
    dplyr::left_join(years_mapping, by = c("vintage")) %>%
    dplyr::left_join(s_curve_shutdown %>%
                dplyr::select(technology, year, lifetime),
              by = c("technology", "year")) %>%
    dplyr::mutate(dep_factor = 1 - ((Year - vintage) / lifetime),
           unrec_cap = (early_ret_GW * metis.assumptions()$convGW_kW * capital.overnight * dep_factor * metis.assumptions()$convUSD_1975_2010) / 1e9,
           Units = "billion 2010 USD") -> elec_ret_cap_cost

  # ============================================================================

  # New Cap Costs
  elec_add_cap_invest %>%
    dplyr::left_join(tech_mapping, by = c("technology")) %>%
    dplyr::group_by(scenario, region, Year, Units, agg_tech) %>%
    dplyr::summarise(cap_invest = sum(cap_invest,na.rm=T)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(Year >= start_year) %>%
    dplyr::mutate(cap_invest=dplyr::if_else(Year==metis.assumptions()$GCAMbaseYear,0,cap_invest))%>%
    tidyr::spread(Year, cap_invest) %>%
    mutate_all(~replace(., is.na(.), 0))%>%
    dplyr::mutate(agg_tech = factor(agg_tech, levels = tech_order)) %>%
    dplyr::arrange(region, agg_tech)-> newCap_cost

  # Cum Cap Costs
  elec_add_cap_invest %>%
    dplyr::left_join(tech_mapping, by = c("technology")) %>%
    dplyr::group_by(scenario, region, Year, Units, agg_tech) %>%
    dplyr::summarise(cap_invest = sum(cap_invest,na.rm=T)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(scenario, region,Units, agg_tech) %>%
    dplyr::mutate(cap_invest = cumsum(cap_invest)) %>%
    dplyr::ungroup()%>%
    dplyr::filter(Year >= start_year) %>%
    dplyr::mutate(cap_invest=dplyr::if_else(Year==metis.assumptions()$GCAMbaseYear,0,cap_invest))%>%
    tidyr::spread(Year, cap_invest) %>%
    mutate_all(~replace(., is.na(.), 0))%>%
    dplyr::mutate(agg_tech = factor(agg_tech, levels = tech_order)) %>%
    dplyr::arrange(region, agg_tech)-> cumCap_cost

  # New Capacity
  elec_add_GW %>%
    dplyr::left_join(tech_mapping, by = c("technology")) %>%
    dplyr::group_by(scenario, region, Year, Units, agg_tech) %>%
    dplyr::summarise(add_GW = sum(add_GW,na.rm=T)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(Year >= start_year) %>%
    tidyr::spread(Year, add_GW) %>%
    mutate_all(~replace(., is.na(.), 0)) %>%
    dplyr::mutate(agg_tech = factor(agg_tech, levels = tech_order)) %>%
    dplyr::arrange(region, agg_tech) -> newCap_GW

  # Cummulative Capacity
  elec_add_GW %>%
    dplyr::left_join(tech_mapping, by = c("technology")) %>%
    dplyr::group_by(scenario, region, Year, Units, agg_tech) %>%
    dplyr::summarise(add_GW = sum(add_GW,na.rm=T)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(scenario, region,Units, agg_tech) %>%
    dplyr::mutate(add_GW = cumsum(add_GW)) %>%
    dplyr::ungroup()%>%
    dplyr::filter(Year >= start_year) %>%
    tidyr::spread(Year, add_GW) %>%
    mutate_all(~replace(., is.na(.), 0)) %>%
    dplyr::mutate(agg_tech = factor(agg_tech, levels = tech_order)) %>%
    dplyr::arrange(region, agg_tech) -> cumCap_GW

  # Premature retirements by region & technology
  elec_ret_cap_cost %>%
    dplyr::left_join(tech_mapping, by = c("technology")) %>%
    dplyr::group_by(scenario, region, Year, Units, agg_tech) %>%
    dplyr::summarise(unrec_cap = sum(unrec_cap,na.rm=T)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(unrec_cap = unrec_cap * -1) %>%
    dplyr::filter(Year >= start_year) %>%
    dplyr::mutate(unrec_cap=dplyr::if_else(Year==metis.assumptions()$GCAMbaseYear,0,unrec_cap))%>%
    tidyr::spread(Year, unrec_cap) %>%
    mutate_all(~replace(., is.na(.), 0)) %>%
    dplyr::mutate(agg_tech = factor(agg_tech, levels = tech_order)) %>%
    dplyr::arrange(region, agg_tech) -> annualPrematureRet_cost

  # Cum Premature retirements by region & technology
  elec_ret_cap_cost %>%
    dplyr::left_join(tech_mapping, by = c("technology")) %>%
    dplyr::group_by(scenario, region, Year, Units, agg_tech) %>%
    dplyr::summarise(unrec_cap = sum(unrec_cap,na.rm=T)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(scenario, region,Units, agg_tech) %>%
    dplyr::mutate(unrec_cap = cumsum(unrec_cap)) %>%
    dplyr::ungroup()%>%
    dplyr::mutate(unrec_cap = unrec_cap * -1) %>%
    dplyr::filter(Year >= start_year) %>%
    dplyr::mutate(unrec_cap=dplyr::if_else(Year==metis.assumptions()$GCAMbaseYear,0,unrec_cap))%>%
    tidyr::spread(Year, unrec_cap) %>%
    mutate_all(~replace(., is.na(.), 0)) %>%
    dplyr::mutate(agg_tech = factor(agg_tech, levels = tech_order)) %>%
    dplyr::arrange(region, agg_tech) -> cumPrematureRet_cost


  # Premature retirements by region & technology
  elec_ret_GW %>%
    dplyr::left_join(tech_mapping, by = c("technology")) %>%
    dplyr::group_by(scenario, region, Year, Units, agg_tech) %>%
    dplyr::summarise(early_ret_GW = sum(early_ret_GW,na.rm=T)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(early_ret_GW = early_ret_GW * -1) %>%
    dplyr::filter(Year >= start_year) %>%
    tidyr::spread(Year, early_ret_GW) %>%
    mutate_all(~replace(., is.na(.), 0)) %>%
    dplyr::mutate(agg_tech = factor(agg_tech, levels = tech_order)) %>%
    dplyr::arrange(region, agg_tech) -> annualPrematureRet_GW

  # Cum Premature retirements by region & technology
  elec_ret_GW %>%
    dplyr::left_join(tech_mapping, by = c("technology")) %>%
    dplyr::group_by(scenario, region, Year, Units, agg_tech) %>%
    dplyr::summarise(early_ret_GW = sum(early_ret_GW,na.rm=T)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(scenario, region,Units, agg_tech) %>%
    dplyr::mutate(early_ret_GW = cumsum(early_ret_GW)) %>%
    dplyr::ungroup()%>%
    dplyr::mutate(early_ret_GW = early_ret_GW * -1) %>%
    dplyr::filter(Year >= start_year) %>%
    tidyr::spread(Year, early_ret_GW) %>%
    mutate_all(~replace(., is.na(.), 0)) %>%
    dplyr::mutate(agg_tech = factor(agg_tech, levels = tech_order)) %>%
    dplyr::arrange(region, agg_tech) -> cumPrematureRet_GW


  # ============================================================================
  return(list("newCap_cost"=newCap_cost,
              "newCap_GW"=newCap_GW,
              "annualPrematureRet_cost"=annualPrematureRet_cost,
              "annualPrematureRet_GW"=annualPrematureRet_GW,
              "cumCap_cost"=cumCap_cost,
              "cumCap_GW"=cumCap_GW,
              "cumPrematureRet_cost"=cumPrematureRet_cost,
              "cumPrematureRet_GW"=cumPrematureRet_GW))

}
