#' metis.mappings
#'
#' This function has all the mappings used to convert between different categories
#'
#' List of Mappings
#' \itemize{
#' \item mapParamQuery}
#' @keywords assumptions
#' @return A list of assumptions
#' @export
#' @examples
#' library(metis)
#' a<-metis.mappings()
#' a # will give full list of mappings
#' @importFrom magrittr %>%

metis.mappings <- function() {

  #------------------------------------------------------------
  # Metis param, query, palette, maps Mapping
  #-----------------------------------------------------------

  # Used by metis.readgcam()
  # USed by metis.mapsProcess()

  mapParamQuery <- tibble::tribble(
    ~group, ~param, ~query,~mapPalette,
    "energy","energyPrimaryByFuelEJ","primary energy consumption by region (direct equivalent) ORDERED SUBSECTORS","pal_hot",
    "energy","energyPrimaryRefLiqProdEJ", "refined liquids production by subsector","pal_hot",
    "energy","energyFinalConsumBySecEJ", "total final energy by aggregate sector","pal_hot",
    "energy","energyFinalByFuelBySectorEJ", "Final energy by detailed end-use sector and fuel","pal_hot",
    "energy","energyFinalSubsecByFuelTranspEJ", "transport final energy by fuel","pal_hot",
    "energy","energyFinalSubsecByFuelBuildEJ", "building final energy by fuel","pal_hot",
    "energy","energyFinalSubsecByFuelIndusEJ", "industry final energy by fuel","pal_hot",
    "energy","energyFinalSubsecBySectorBuildEJ", "building final energy by subsector","pal_hot",
    "energy","energyFinalConsumByIntlShpAvEJ", "transport final energy by mode and fuel","pal_hot",
    "energy","energyPrimaryByFuelMTOE", "primary energy consumption by region (direct equivalent) ORDERED SUBSECTORS","pal_hot",
    "energy","energyPrimaryRefLiqProdMTOE", "refined liquids production by subsector","pal_hot",
    "energy","energyFinalConsumBySecMTOE", "total final energy by aggregate sector","pal_hot",
    "energy","energyFinalbyFuelMTOE", "Final energy by detailed end-use sector and fuel","pal_hot",
    "energy","energyFinalSubsecByFuelTranspMTOE", "transport final energy by fuel","pal_hot",
    "energy","energyFinalSubsecByFuelBuildMTOE", "building final energy by fuel","pal_hot",
    "energy","energyFinalSubsecByFuelIndusMTOE", "industry final energy by fuel","pal_hot",
    "energy","energyFinalSubsecBySectorBuildMTOE", "building final energy by subsector","pal_hot",
    "energy","energyFinalConsumByIntlShpAvMTOE", "transport final energy by mode and fuel","pal_hot",
    "energy","energyPrimaryByFuelTWh", "primary energy consumption by region (direct equivalent) ORDERED SUBSECTORS","pal_hot",
    "energy","energyPrimaryRefLiqProdTWh", "refined liquids production by subsector","pal_hot",
    "energy","energyFinalConsumBySecTWh", "total final energy by aggregate sector","pal_hot",
    "energy","energyFinalbyFuelTWh", "Final energy by detailed end-use sector and fuel","pal_hot",
    "energy","energyFinalSubsecByFuelTranspTWh", "transport final energy by fuel","pal_hot",
    "energy","energyFinalSubsecByFuelBuildTWh", "building final energy by fuel","pal_hot",
    "energy","energyFinalSubsecByFuelIndusTWh", "industry final energy by fuel","pal_hot",
    "energy","energyFinalSubsecBySectorBuildTWh", "building final energy by subsector","pal_hot",
    "energy","energyFinalConsumByIntlShpAvTWh", "transport final energy by mode and fuel","pal_hot",
    # Electricity
    "electricity","elecByTechTWh", c("elec gen by gen tech cogen USA","elec gen by gen tech USA","elec gen by gen tech and cooling tech"),"pal_hot",
    "electricity","elecCapByFuel", c("elec gen by gen tech cogen USA","elec gen by gen tech USA","elec gen by gen tech and cooling tech"),"pal_hot",
    "electricity","elecFinalBySecTWh", "Final energy by detailed end-use sector and fuel","pal_hot",
    "electricity","elecFinalByFuelTWh", "Final energy by detailed end-use sector and fuel","pal_hot",
    "electricity","elecNewCapCost", c("elec gen by gen tech and cooling tech and vintage","Electricity generation by aggregate technology"),"pal_hot",
    "electricity","elecNewCapGW", c("elec gen by gen tech and cooling tech and vintage","Electricity generation by aggregate technology"),"pal_hot",
    "electricity","elecAnnualRetPrematureCost", c("elec gen by gen tech and cooling tech and vintage","Electricity generation by aggregate technology"),"pal_hot",
    "electricity","elecAnnualRetPrematureGW", c("elec gen by gen tech and cooling tech and vintage","Electricity generation by aggregate technology"),"pal_hot",
    "electricity","elecCumCapCost", c("elec gen by gen tech and cooling tech and vintage","Electricity generation by aggregate technology"),"pal_hot",
    "electricity","elecCumCapGW", c("elec gen by gen tech and cooling tech and vintage","Electricity generation by aggregate technology"),"pal_hot",
    "electricity","elecCumRetPrematureCost", c("elec gen by gen tech and cooling tech and vintage","Electricity generation by aggregate technology"),"pal_hot",
    "electricity","elecCumRetPrematureGW", c("elec gen by gen tech and cooling tech and vintage","Electricity generation by aggregate technology"),"pal_hot",
    # Transport
    "transport","transportPassengerVMTByMode", "transport service output by mode","pal_hot",
    "transport","transportFreightVMTByMode", "transport service output by mode","pal_hot",
    "transport","transportPassengerVMTByFuel","transport service output by tech (new)","pal_hot",
    "transport","transportFreightVMTByFuel", "transport service output by tech (new)","pal_hot",
    # Water
    "water","watConsumBySec", "water consumption by state, sector, basin (includes desal)","pal_wet",
    "water","watWithdrawBySec", "water withdrawals by state, sector, basin (includes desal)","pal_wet",
    "water","watWithdrawByCrop", "water withdrawals by crop","pal_wet",
    "water","watBioPhysCons", "biophysical water demand by crop type and land region","pal_wet",
    "water","watIrrWithdrawBasin", "water withdrawals by water mapping source","pal_wet",
    "water","watIrrConsBasin", "water consumption by water mapping source","pal_wet",
    "water","watSupRunoffBasin", "Basin level available runoff","pal_wet",
    # Socio-economics
    "socioecon","gdpPerCapita", "GDP per capita MER by region","pal_hot",
    "socioecon","gdp", "GDP MER by region","pal_hot",
    "socioecon","gdpGrowthRate", "GDP Growth Rate (Percent)","pal_hot",
    "socioecon","pop", "Population by region","pal_hot",
    # Agriculture
    "ag","agProdbyIrrRfd", "ag production by tech","pal_green",
    "ag","agProdBiomass", "Ag Production by Crop Type","pal_green",
    "ag","agProdForest", "Ag Production by Crop Type","pal_green",
    "ag","agProdByCrop", "Ag Production by Crop Type","pal_green",
    #Livestock
    "livestock","livestock_MeatDairybyTechMixed", "meat and dairy production by tech","pal_green",
    "livestock","livestock_MeatDairybyTechPastoral", "meat and dairy production by tech","pal_green",
    "livestock","livestock_MeatDairybyTechImports", "meat and dairy production by tech","pal_green",
    "livestock","livestock_MeatDairybySubsector", "meat and dairy production by tech","pal_green",
    # Land use
    "land","landIrrRfd", "land allocation by crop and water source","pal_green",
    "land","landIrrCrop", "land allocation by crop and water source","pal_green",
    "land","landRfdCrop", "land allocation by crop and water source","pal_green",
    "land","landAlloc", "aggregated land allocation","pal_green",
    "land","landAllocByCrop", "land allocation by crop","pal_green",
    # Emissions
    "emissions","emissNonCO2BySectorGWPAR5", "nonCO2 emissions by sector","pal_hot",
    "emissions","emissNonCO2BySectorGTPAR5", "nonCO2 emissions by sector","pal_hot",
    "emissions","emissNonCO2BySectorOrigUnits", "nonCO2 emissions by sector","pal_hot",
    "emissions","emissLUC", "Land Use Change Emission (future)","pal_hot",
    "emissions","emissCO2BySectorNoBio", "CO2 emissions by sector (no bio)","pal_hot",
    "emissions","emissNonCO2ByResProdGWPAR5", "nonCO2 emissions by resource production","pal_hot",
    "emissions","emissMethaneBySourceGWPAR5", "nonCO2 emissions by sector","pal_hot",
    "emissions","emissByGasGWPAR5FFI", c("nonCO2 emissions by resource production","nonCO2 emissions by sector"),"pal_hot",
    "emissions","emissByGasGWPAR5LUC", c("nonCO2 emissions by resource production","nonCO2 emissions by sector"),"pal_hot",
    "emissions","emissBySectorGWPAR5FFI", c("nonCO2 emissions by resource production","nonCO2 emissions by sector"),"pal_hot",
    "emissions","emissBySectorGWPAR5LUC", c("nonCO2 emissions by resource production","nonCO2 emissions by sector"),"pal_hot",
    "emissions","emissNonCO2ByResProdGTPAR5", "nonCO2 emissions by resource production","pal_hot",
    "emissions","emissMethaneBySourceGTPAR5", "nonCO2 emissions by sector","pal_hot",
    "emissions","emissByGasGTPAR5FFI", c("nonCO2 emissions by resource production","nonCO2 emissions by sector"),"pal_hot",
    "emissions","emissByGasGTPAR5LUC", c("nonCO2 emissions by resource production","nonCO2 emissions by sector"),"pal_hot",
    "emissions","emissBySectorGTPAR5FFI",  c("nonCO2 emissions by resource production","nonCO2 emissions by sector"),"pal_hot",
    "emissions","emissBySectorGTPAR5LUC", c("nonCO2 emissions by resource production","nonCO2 emissions by sector"),"pal_hot",
  ); mapParamQuery


  return(list(
         mapParamQuery=mapParamQuery))
}
