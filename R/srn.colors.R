#' srn.colors
#'
#' This function loads various color palettes used previously in GCAM
#' as well as new palettes for SRN modeling to the global environment
#'
#' List of Color Palettes
#' \itemize{
#' \item pal_HDDCDD
#' \item pal_16
#' \item elec_tech_colors
#' \item elec_renew_colors
#' \item building_colors
#' \item trn_fuel_colors
#' \item enduse_fuel_numbered
#' \item enduse_colors
#' \item pal_pri_ene
#' \item pal_pri_fuelcost
#' \item pal_emiss_sector
#' \item pal_landuse
#' \item pal_hydrogen
#' \item pal_refliq
#' \item emiss_by_enduse_colors
#' \item biouse_colors
#' \item pal_Basic
#' \item pal_Gas
#' \item pal_Diff
#' \item pal_Diff5
#' \item pal_Absolute
#' \item pal_Absolute5
#' \item pal_Unassigned
#' \item pal_elec_subsec
#' \item pal_elec_finalNrgFuel
#' \item pal_elec_techs
#' \item pal_elec_sec
#' \item pal_finalNrg_sec
#' \item pal_pri_ene
#' \item pal_elec_tech_colors}
#' @param palx Palette name to view the palette colors. Eg. srn.colors("pal_Basic")
#' @keywords colors, palette
#' @return A list of color palettes.
#' @export
#' @examples
#' library(srn)
#' a<-srn.colors()
#' pie(rep(1,length(a$pal_Basic)),label=names(a$pal_Basic),col=a$pal_Basic)
#' @import RColorBrewer

srn.colors <- function(palx=NULL) {

  #------------------
  # Load required Libraries
  # -----------------
  requireNamespace("RColorBrewer",quietly = T)

  NULL->pie

    # GCAM Color Palettes

    # HDDCDD Palette for Cooling and Heating
    pal_HDDCDD<-c("CDD"="cornflowerblue","HDD"="coral2")

    # General purpose color scheme where sequentail colors do not clash too much
    pal_16 <- rep(c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
        "#333333", "#FFCC00", "#CC6600", "#006600", "#3333CC", "#CC0033", "#0099CC", "#999966"),100)

    # Color scheme for electricity generation by aggregate fuel
    elec_tech_colors <- c(`a Coal` = "#a0237c", `b Coal w/CCS` = "#dab4c7", `c Gas` = "#25a9e0", `d Gas w/CCS` = "#84e7f9",
        `e Oil` = "#d01c2a", `f Oil w/CCS` = "#f7988f", `g Biomass` = "#00931d", `h Biomass w/CCS` = "#88c892",
        `i Nuclear` = "#ef8e27", `j Geothermal` = "#ad440c", `k Hydro` = "#fdfa28", `l Wind` = "#3d86f9",
        `m Solar` = "#fdd67b", `n CHP` = "#507fab", `o Battery` = "#92a75d", `energy reduction` = "grey")

    elec_renew_colors <- c(liquids = "#d01c2a", gas = "#25a9e0", coal = "#a0237c", biomass = "#00931d",
        nuclear = "#ef8e27", hydro = "#fdfa28", `wind w/ backup` = "#3e85fd", `wind w/ storage` = "#9ec1fd",
        `csp w/ backup` = "#ec931a", `csp w/ storage` = "#f5c88c", `pv w/ backup` = "#ffd125", `pv w/ storage` = "#ffe791",
        `rooftop solar` = "#ff7a17", geothermal = "#aa440a", cogen = "#507fab", battery = "#92a75d",
        `energy reduction` = "grey")

    # Buildings colors by fuel
    building_colors <- c(liquids = "#d01c2a", gas = "#25a9e0", coal = "#a0237c", biomass = "#00931d",
        electricity = "#ef8e27", `trad biomass` = "#11d081")

    # Transportation colors by fuel
    trn_fuel_colors <- c(liquids = "#d01c2a", gas = "#25a9e0", coal = "#a0237c", biomass = "#00931d",
        electricity = "#ef8e27", hydrogen = "#fffbac")

    # Generic end-use color scheme by numbered fuel
    enduse_fuel_numbered <- c(`1 liquids` = "#d01c2a", `2 gas` = "#25a9e0", `3 coal` = "#a0237c",
        `4 biomass` = "#00931d", `5 electricity` = "#ef8e27", `6 hydrogen` = "#fffbac", `7 trad biomass` = "#11d081",
        `8 district heat` = "#b14d38", feedstocks = "#ff7467", `energy reduction` = "grey")

    # Color scheme for the aggregate final demands
    enduse_colors <- c(building = "#facda4", industry = "#cef4d1", transportation = "#d0f6f7", `energy reduction` = "grey")

    # Primary energy colors including CCS
    pal_pri_ene <- c(`a oil` = "#d01c2a", `a oil CCS` = "#f7988f", `b natural gas` = "#25a9e0", `b natural gas CCS` = "#84e7f9",
        `c coal` = "#a0237c", `c coal CCS` = "#dab4c7", `d biomass` = "#00931d", `d biomass CCS` = "#88c892",
        `e nuclear` = "#ef8e27", `f hydro` = "#fdfa28", `g wind` = "#3d86f9", `h solar` = "#fdd67b",
        `i geothermal` = "#ad440c", `j traditional biomass` = "#11d081", `energy reduction` = "grey")

    # Color scheme for primary fuel costs
    pal_pri_fuelcost <- c(`regional oil` = "#d01c2a", `regional natural gas` = "#25a9e0", `regional coal` = "#a0237c",
        `regional biomass` = "#00931d", nuclearFuelGenIII = "#ef8e27")

    # Colors for emissions by aggregate sector
    pal_emiss_sector <- c(`liquid systems` = "#d01c2a", `gas systems` = "#25a9e0", coal = "#a0237c",
        `biomass systems` = "#00931d", electricity = "#ef8e27", hydrogen = "#fffbac", `N fertilizer` = "#11d081",
        `district heat` = "#b14d38", building = "#facda4", industry = "#cef4d1", transportation = "#d0f6f7")

    # Aggregated (more that usual) land use
    pal_landuse <- c(urban = "#000000", crops = "#ffd125", `pasture (grazed)` = "#aacf22", `forest (managed)` = "#41a67a",
        biomass = "#00f629", `forest (unmanaged)` = "#938e15", shrubs = "#b14d00", `grass/other pasture` = "#ffd081",
        desert = "#a7a7a7")

    # And hydrogen even
    pal_hydrogen <- c(gas = "#25a9e0", `gas w/CCS` = "#84e7f9", coal = "#a0237c", `coal w/CCS` = "#dab4c7",
        biomass = "#00931d", `biomass w/CCS` = "#88c892", nuclear = "#ef8e27", `bulk electricity` = "#fdfa28",
        wind = "#3d86f9", solar = "#fdd67b", `energy reduction` = "grey")

    # Refined liquids
    pal_refliq <- c(`conventional oil` = "#ab4500", `unconventional oil` = "#ff9593", coal = "#ff2600",
        `coal w/CCS` = "#ff8d78", biomass = "#00931d", `biomass w/CCS` = "#728f72", gas = "#1633ff",
        `energy reduction` = "grey")

    # Color scheme for emissions by aggregate sector
    emiss_by_enduse_colors <- c(buildings = "#facda4", industry = "#cef4d1", transportation = "#d0f6f7",
        electricity = "#ea9219", cement = "#ff230e")

    # Color scheme for biomass consumption by use
    biouse_colors <- c(`direct buildings` = "#a8d8fe", industry = "#0080d4", electricity = "#ec931a",
        refining = "#aa4a79", hydrogen = "#fffbac", `synthetic gas` = "#ff230e", `energy reduction` = "grey")

    #------------------------------------------
    # New SRN Color Schemes
    #-------------------------------------------

    # Creating Colors getcol = colorRampPalette(brewer.pal(9, 'RdGy')) values=getcol(9); values
    # library(colorspace) pal<-choose_palette() pal(9)
    # pie(rep(1,length(colx1)),label=names(colx1),col=colx1)


    # Basic Colors
    pal_Basic <- rep(c("red", "green3", "blue", "black", "magenta", "yellow", "cyan", "gray"),100)


    # Gas Colors
    pal_Gas <- c(`gas (CC CCS)` = "black", `gas (CT)` = "red", `gas (steam)` = "green3", `gas (CC)` = "blue",
        "cyan", "magenta", "yellow", "gray")

    pal_Diff <- c("#693C01", "#815A3D", "#9C7D6A", "#BDA89D", "white", "#ACAABF", "#8480A2", "#635D8B",
        "#473E7A")

    pal_Diff5 <- c("#693C01", "#9C7D6A", "white", "#8480A2", "#473E7A")

    pal_Absolute <- rev(c("#67001F", "#8C0C25", "#B2182B", "#C43C3C", "#D6604D", "#E58267", "#F4A582",
        "#F8C0A4", "#FDDBC7", "#FEEDE3", "#FFFFFF"))

    pal_Absolute5 <- rev(c("#67001F", "#D6604D", "#F4A582", "#FDDBC7", "#FFFFFF"))

    colorsDiff <- c("#693C01", "#815A3D", "#9C7D6A", "#BDA89D", "white", "#ACAABF", "#8480A2", "#635D8B",
        "#473E7A")

    # Custom Colors for large unassigned palettes Custom Colors
    # https://stackoverflow.com/questions/15282580/how-to-generate-a-number-of-most-distinctive-colors-in-r
    qual_col_pals = brewer.pal.info[brewer.pal.info$category == "qual", ]
    pal_Unassigned <- rep(unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals))),
        100)


    # Color scheme for electricity generation by aggregate fuel
    pal_elec_subsec <- c(coal = "#a0237c", gas = "#25a9e0", oil = "#d01c2a", biomass = "#00931d",
        nuclear = "#ef8e27", geothermal = "#ad440c", hydro = "#fdfa28", wind = "#0000ff", solar = "#fdd67b",
        `refined liquids` = "#000000", electricity = "lavender", other = "grey70", `gas (CC CCS)` = "#25c9e0",
        `gas (CC)` = "#25a9e0", `gas (CT)` = "#25e9e0", `gas (steam)` = "greenyellow")

    # Color scheme for final energy by fuel
    pal_elec_finalNrgFuel <- c(`3 coal` = "#a0237c", `2 gas` = "#25a9e0", `4 biomass` = "#00931d",
        `1 liquids` = "#000000", `5 electricity` = "lavender", `6 hydrogen` = "orange", "grey60")

    pal_elec_techs <- c(nuclear = "#af8e27", Gen_III = "#af8e27", Gen_II_LWR = "#ef8e27", CSP = "#cdd67b",
        PV = "#fdd67b", `biomass (IGCC CCS)` = "#00a31d", `biomass (IGCC)` = "#00c31d", `biomass (conv CCS)` = "#00e31d",
        `biomass (conv)` = "#00f31d", `coal (IGCC CCS)` = "#c0237c", `coal (IGCC)` = "#f0237c", `coal (conv pul CCS)` = "#e0237c",
        `coal (conv pul)` = "#a0237c", `gas (CC CCS)` = "#25c9e0", `gas (CC)` = "#25a9e0", `gas (CT)` = "#25e9e0",
        `gas (steam)` = "greenyellow", hydro = "#fdfa28", `refined liquids` = "#000000", `refined liquids (CC CCS)` = "#000000",
        `refined liquids (CC)` = "#000000", `refined liquids (CT)` = "#000000", `refined liquids (steam)` = "#000000",
        wind = "#0000ff", "red", "green", geothermal = "gray40", unlist(mapply(brewer.pal, qual_col_pals$maxcolors,
            rownames(qual_col_pals))))


    pal_elec_sec <- c(elect_td_bld = "#ff230e", elect_td_ind = "darkolivegreen4", elect_td_trn = "dodgerblue",
        electricity = "#ea9219")


    pal_finalNrg_sec <- c(building = "#facda4", `comm non-building` = "#ff230e", industry = "#cef4d1",
        transportation = "#d0f6f7", trn_pass_road_bus = "purple",buildings = "#facda4", electricity="lightcoral",
        `LUC Emission`="grey30",`LUC Absorption`="darkolivegreen4")


    # Modified Original GCAM color scheme for Primary energy consumption Modified the 'Primary energy
    # colors including CCS - pal_pri_ene' color scheme from color_scheme.R Changed hydro to current
    # wind color(blue), switched solar to current hydro color(yellow) switched wind to current solar
    # color (biege), switched gas & gas CCS too a lighter blue because hydro and gas were too similar
    pal_pri_ene <- c(`a oil` = "#d01c2a", `a oil CCS` = "#f7988f", `b natural gas` = "darkslategray1",
        `b natural gas CCS` = "darkslategray4", `c coal` = "gray60", `c coal CCS` = "gray20", `d biomass` = "#00931d",
        `d biomass CCS` = "#88c892", `e nuclear` = "#ef8e27", `f hydro` = "#3d86f9", `g wind` = "#fdd67b",
        `h solar` = "#fdfa28", `i geothermal` = "#ad440c", `j traditional biomass` = "#11d081", `energy reduction` = "black")

    # Modified color scheme for elec Modified the 'Primary energy colors including CCS - pal_pri_ene'
    # color scheme from color_scheme.R Changed hydro to current wind color(blue), switched solar to
    # current hydro color(yellow) switched wind to current solar color (biege), switched gas & gas CCS
    # too a lighter blue because hydro and gas were too similar switched coal to grays and changed
    # energy reduction to black
    pal_elec_tech_colors <- c(`a Coal` = "gray60", `b Coal w/CCS` = "gray20", `c Gas` = "darkslategray1",
        `d Gas w/CCS` = "darkslategray4", `e Oil` = "#d01c2a", `f Oil w/CCS` = "#f7988f", `g Biomass` = "#00931d",
        `h Biomass w/CCS` = "#88c892", `i Nuclear` = "#ef8e27", `j Geothermal` = "#ad440c", `k Hydro` = "#3d86f9",
        `l Wind` = "#fdd67b", `m Solar` = "#fdfa28", `n CHP` = "#507fab", `o Battery` = "#92a75d",
        `energy reduction` = "grey", Total = "black", Other = "black")


    # Agriculture Production Type
    pal_ag_type <- c(Forest = "darkgreen" , NonFoodDemand_Forest = "darkolivegreen1",
                     biomass = "grey50", Corn = "gold3" ,
                     FiberCrop = "gold4",  MiscCrop = "darkorange4", OilCrop = "gray20",
                     OtherGrain  = "indianred2",
                     PalmFruit = "firebrick3" ,  Rice = "steelblue2", Root_Tuber  = "lightslateblue", SugarCrop = "yellow2",
                     Wheat  = "burlywood", FodderGrass = "darkseagreen1",
                     FodderHerb = "darkseagreen4", Pasture = "goldenrod1",
                     UnmanagedLand = "black")

    # Water Demand Sectors
    pal_wat_dem <- c(municipal = "dodgerblue", mining = "grey75", livestock = "darkseagreen3",
                     industry = "gold2", electricity = "darkorange", agriculture = "darkolivegreen4")

    pal_lu_type <- c(urban="indianred2",tundra="antiquewhite1",shrubs="lightslateblue",`rock and desert`="black",
                pasture="goldenrod1",otherarable="darkorange4",grass="darkolivegreen1",forest="darkgreen",
                crops="yellow2",biomass="grey50")

    if(!is.null(palx)){
    if(length(get(palx))>1){
      a<-get(palx)
      if(palx=="pal_Basic" | palx=="pal_16"){a<-a[1:(length(a)/100)]}
      pie(rep(1,length(a)),label=names(a),col=a)
    }}

    return(list(pal_lu_type=pal_lu_type,pal_ag_type=pal_ag_type,pal_wat_dem=pal_wat_dem,pal_HDDCDD=pal_HDDCDD, pal_16 = pal_16, elec_tech_colors = elec_tech_colors, elec_renew_colors = elec_renew_colors,
        building_colors = building_colors, trn_fuel_colors = trn_fuel_colors, enduse_fuel_numbered = enduse_fuel_numbered,
        enduse_colors = enduse_colors, pal_pri_ene = pal_pri_ene, pal_pri_fuelcost = pal_pri_fuelcost,
        pal_emiss_sector = pal_emiss_sector, pal_landuse = pal_landuse, pal_hydrogen = pal_hydrogen,
        pal_refliq = pal_refliq, emiss_by_enduse_colors = emiss_by_enduse_colors, biouse_colors = biouse_colors,
        pal_Basic = pal_Basic, pal_Gas = pal_Gas, pal_Diff = pal_Diff, pal_Diff5 = pal_Diff5, pal_Absolute = pal_Absolute,
        pal_Absolute5 = pal_Absolute5, pal_Unassigned = pal_Unassigned, pal_elec_subsec = pal_elec_subsec,
        pal_elec_finalNrgFuel = pal_elec_finalNrgFuel, pal_elec_techs = pal_elec_techs, pal_elec_sec = pal_elec_sec,
        pal_finalNrg_sec = pal_finalNrg_sec, pal_pri_ene = pal_pri_ene, pal_elec_tech_colors = pal_elec_tech_colors))
}
