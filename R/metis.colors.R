#' metis.colors
#'
#' This function loads various color palettes used previously in GCAM
#' as well as new palettes for Metis modeling to the global environment
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
#' \item pal_pri_ene
#' \item pal_nrg
#' \item pal_hot
#' \item pal_wet
#' \item pal_div_wet
#' \item pal_div_RdBl
#' \item pal_green
#' \item pal_div_BrGn
#' \item pal_div_BlRd
#' \item pal_sankey
#' \item pal_spectral
#' \item pal_ScarcityCat}
#' @param palx Palette name to view the palette colors. Eg. metis.colors("pal_Basic")
#' @keywords colors, palette
#' @return A list of color palettes.
#' @export
#' @examples
#' library(metis)
#' a<-metis.colors()
#' pie(rep(1,length(a$pal_Basic)),label=names(a$pal_Basic),col=a$pal_Basic)


metis.colors <- function(palx=NULL) {


  NULL->pie

    # GCAM Color Palettes

    # HDDCDD Palette for Cooling and Heating
    pal_HDDCDD<-c("CDD"="cornflowerblue","HDD"="coral2")

    # General purpose color scheme where sequentail colors do not clash too much
    pal_16 <- rep(c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
        "#333333", "#FFCC00", "#CC6600", "#006600", "#3333CC", "#CC0033", "#0099CC", "#999966"),100)

    # Spectral colors
    pal_spectral <- rep(c("#9E0142", "#D53E4F", "#F46D43", "#FDAE61", "#FEE08B", "#FFFFBF", "#E6F598",
                          "#ABDDA4", "#66C2A5", "#3288BD", "#5E4FA2"),100)

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
    # New Metis Color Schemes
    #-------------------------------------------

    # Creating Colors getcol = colorRampPalette(brewer.pal(9, 'RdGy')) values=getcol(9); values
    # library(colorspace) pal<-choose_palette() pal(9)
    # colx1<-pal_div_BlRd;pie(rep(1,length(colx1)),label=names(colx1),col=colx1)


    # Basic Colors
    pal_Basic <- rep(c("red", "green3", "blue", "black", "magenta", "yellow", "cyan", "gray"),100)


    # Gas Colors
    pal_Gas <- c(`gas (CC CCS)` = "black", `gas (CT)` = "red", `gas (steam)` = "green3", `gas (CC)` = "blue",
        "cyan", "magenta", "yellow", "gray")

    pal_hot <- c('#ffffcc','#ffeda0','#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#bd0026','#800026',"#4d0000")

    pal_wet <- c('#f7fbff','#deebf7','#c6dbef','#9ecae1','#6baed6','#4292c6','#2171b5','#08519c','#08306b','#00004d')
    pal_div_wet <- c("#624100", "#7D5D23" ,"#9C7F57", "#C0A88B",'white','#2171b5','#08519c','#08306b','#00004d')

    pal_green <- c('#ffffe5','#f7fcb9','#d9f0a3','#addd8e','#78c679','#41ab5d','#238443','#006837','#004529','#003300')

    pal_div_BlRd <- rev(c('#67001f','#b2182b','#d6604d','#f4a582','#fddbc7','white','#e0e0e0','#bababa','#878787','#4d4d4d','#1a1a1a'))
    pal_div_RdBl <- c('#67001f','#b2182b','#d6604d','#f4a582','#fddbc7','white','#e0e0e0','#bababa','#878787','#4d4d4d','#1a1a1a')
    pal_div_BrGn <- c('#543005','#8c510a','#bf812d','#dfc27d','#f6e8c3','white','#c7eae5','#80cdc1','#35978f','#01665e','#003c30')

    pal_seq <- c('lemonchiffon1','burlywood1','orange1','red1','purple4','black')


    pal_nrg <- c(`a Coal` = "gray40",  `c coal` = "gray40", `3 coal` = "gray40", coal = "gray40", `coal (conv pul)` = "gray40",
                  Coal = "gray40",
                 `b Coal w/CCS` = "gray20", `c coal CCS` = "gray20",
                 `coal (IGCC CCS)` = "#c0237c",
                 `coal (IGCC)` = "#f0237c",
                 `coal (conv pul CCS)` = "#e0237c",
                 `c Gas` = "darkslategray1", `b natural gas` = "darkslategray1", `2 gas` = "darkslategray1", Gas = "darkslategray1",
                 gas = "darkslategray1", `gas (CC)` = "darkslategray1",
                 `d Gas w/CCS` = "darkslategray4", `b natural gas CCS` = "darkslategray4",
                 `gas (CC CCS)` = "#25c9e0",
                 `gas (CT)` = "#25e9e0",
                 `gas (steam)` = "greenyellow",
                 `e Oil` = "firebrick4", `a oil` = "firebrick4", oil = "firebrick4", Oil = "firebrick4",
                 `refined liquids` = "firebrick4", `refined liquids (steam)` = "firebrick4", `1 liquids` = "firebrick4",
                 `refined liquids (CC)` = "firebrick4", `refined liquids (CT)` = "firebrick4",
                 `refined liquids (CC CCS)` = "#f7988f", `f Oil w/CCS` = "#f7988f", `a oil CCS` = "#f7988f",
                 `g Biomass` = "#00931d", `d biomass` = "#00931d",  biomass = "#00931d",  Biomass = "#00931d",
                 `biomass (IGCC CCS)` = "#00a31d", `4 biomass` = "#00a31d",
                 `biomass (IGCC)` = "#00c31d",
                 `biomass (conv CCS)` = "#00e31d",
                 `biomass (conv)` = "#00f31d",
                  biofuel = "darkolivegreen1", Biofuel = "darkolivegreen1",
                 `j traditional biomass` = "#11d081",
                 `h Biomass w/CCS` = "#88c892", `d biomass CCS` = "#88c892",
                 `i Nuclear` = "#ef8e27", `e nuclear` = "#ef8e27", nuclear = "#ef8e27", Gen_II_LWR = "#ef8e27",
                  Gen_III = "#af8e27",
                 `j Geothermal` = "darkmagenta", `i geothermal` = "darkmagenta", geothermal = "darkmagenta", Geothermal = "darkmagenta",
                 `k Hydro` = "#3d86f9", `f hydro` = "#3d86f9", hydro = "#3d86f9", Hydro = "#3d86f9",
                 `l Wind` = "#fdd67b", `g wind` = "#fdd67b",  wind = "#fdd67b", Wind = "#fdd67b",
                 `m Solar` = "#fdfa28", `h solar` = "#fdfa28", solar = "#fdfa28", Solar = "#fdfa28",
                  CSP = "#cdd67b",
                  PV = "#fdd67b",
                 `n CHP` = "#507fab",
                 #---------------
                 # Other
                 #--------------
                 `heat`="darkslategray",
                 `6 hydrogen` = "orange", `hydrogen` = "orange",
                 `o Battery` = "#92a75d",
                 `energy reduction` = "grey",
                 `Total` = "black", `total` = "black",
                 `Other` = "grey70", other = "grey70",
                 `Electricity` = "lavender", electricity="lavender", `5 electricity` = "lavender",
                 #---------------
                 # Sectors
                 #--------------
                 `building` = "#facda4", buildings = "#facda4", elect_td_bld = "#facda4",
                 `comm non-building` = "#ff230e",
                 `industry` = "#cef4d1", elect_td_ind = "#cef4d1",
                 `transportation` = "#d0f6f7", `transport` = "#d0f6f7", elect_td_trn = "#d0f6f7",
                 `trn_pass_road_bus` = "purple",
                 #---------------
                 # Emissions
                 #--------------
                 `LUC Emission`="grey30",
                 `LUC Absorption`="darkolivegreen4")


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
                crops="yellow2",biomass="grey50",naturalOther="grey75")

    pal_sankey <- c(Ag = "forestgreen", Ag_import = "chartreuse",
                    E = "red", E_import = "orange",
                    W = "dodgerblue4", W_import = "cadetblue1"); pal_sankey

    pal_ScarcityCat <- c("None (0<WSI<0.1)" = "#3288BD",
                         "Low (0.1<WSI<0.2)" = "#ABDDA4",
                         "Moderate (0.2<WSI<0.4)" = "#FDAE61",
                         "Severe (WSI>0.4)" = "#9E0142"); pal_ScarcityCat


    if(!is.null(palx)){
    if(length(get(palx))>1){
      a<-get(palx)
      if(palx=="pal_Basic" | palx=="pal_16"){a<-a[1:(length(a)/100)]}
      pie(rep(1,length(a)),label=names(a),col=a)
      print(a)
    }}



    invisible(list(pal_lu_type=pal_lu_type,pal_ag_type=pal_ag_type,pal_wat_dem=pal_wat_dem,pal_HDDCDD=pal_HDDCDD, pal_16 = pal_16, elec_tech_colors = elec_tech_colors, elec_renew_colors = elec_renew_colors,
        building_colors = building_colors, trn_fuel_colors = trn_fuel_colors, enduse_fuel_numbered = enduse_fuel_numbered,
        enduse_colors = enduse_colors, pal_pri_ene = pal_pri_ene, pal_pri_fuelcost = pal_pri_fuelcost,
        pal_emiss_sector = pal_emiss_sector, pal_landuse = pal_landuse, pal_hydrogen = pal_hydrogen,
        pal_refliq = pal_refliq, emiss_by_enduse_colors = emiss_by_enduse_colors, biouse_colors = biouse_colors,
        pal_Basic = pal_Basic, pal_Gas = pal_Gas, pal_nrg = pal_nrg,
        pal_pri_ene = pal_pri_ene,
        pal_hot = pal_hot,pal_wet=pal_wet, pal_div_wet=pal_div_wet,pal_div_RdBl=pal_div_RdBl,pal_green=pal_green,
        pal_div_BrGn=pal_div_BrGn,pal_div_BlRd=pal_div_BlRd,pal_sankey=pal_sankey, pal_spectral=pal_spectral, pal_ScarcityCat=pal_ScarcityCat))
}
