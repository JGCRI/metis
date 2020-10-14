#' metis.colors
#'
#' This function loads various color palettes used previously in GCAM
#' as well as new palettes for Metis modeling to the global environment
#'
#' List of Color Palettes
#' \itemize{
#' \item "pal_metis"
#' \item "pal_16"
#' \item "pal_Basic"
#' \item "pal_hot"
#' \item "pal_wet"
#' \item "pal_div_wet"
#' \item "pal_div_BlRd"
#' \item "pal_div_RdBl"
#' \item "pal_div_BrGn"
#' \item "pal_div_GnBr"
#' \item "pal_div_BluRd"
#' \item "pal_div_RdBlu"
#' \item "pal_green"
#' \item "pal_div_BrGn"
#' \item "pal_div_BlRd"
#' \item "pal_sankey"
#' \item "pal_sankey_alpha"
#' \item "pal_spectral"
#' \item "pal_ScarcityCat"}
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

  #------------------------------------------
  # New Metis Color Schemes
  #-------------------------------------------
  # Creating Colors
  # library(RColorBrewer)
  # getcol = colorRampPalette(brewer.pal(9, 'Pastel1')); values=getcol(9); values
  # pie(rep(1,length(values)),label=names(values),col=values)
  # library(colorspace) pal<-choose_palette() pal(9)
  # colx1<-pal_div_BlRd;pie(rep(1,length(colx1)),label=names(colx1),col=colx1)

  # Add Alpha
  add.alpha <- function(col, alpha=1){
    if(missing(col))
      stop("Please provide a vector of colours.")
    apply(sapply(col, grDevices::col2rgb)/255, 2,
          function(x)
            grDevices::rgb(x[1], x[2], x[3], alpha=alpha))
  }

    # General purpose color scheme where sequentail colors do not clash too much
    pal_16 <- rep(c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
        "#333333", "#FFCC00", "#CC6600", "#006600", "#3333CC", "#CC0033", "#0099CC", "#999966"),100)

    # Spectral colors
    pal_spectral <- rep(c("#9E0142", "#D53E4F", "#F46D43", "#FDAE61", "#FEE08B", "#FFFFBF", "#E6F598",
                          "#ABDDA4", "#66C2A5", "#3288BD", "#5E4FA2"),100)

    # Basic Colors
    pal_Basic <- add.alpha(rep(c("firebrick3", "forestgreen", "dodgerblue3","black","darkgoldenrod3","darkorchid3","gray50", "darkturquoise"),100),alpha=0.8)

    pal_hot <- c('#ffffcc','#ffeda0','#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#bd0026','#800026',"#4d0000")
    pal_wet <- c('#f7fbff','#deebf7','#c6dbef','#9ecae1','#6baed6','#4292c6','#2171b5','#08519c','#08306b','#00004d')
    pal_green <- c('#ffffe5','#f7fcb9','#d9f0a3','#addd8e','#78c679','#41ab5d','#238443','#006837','#004529','#003300')

    pal_div_wet <- c("#624100", "#7D5D23" ,"#9C7F57", "#C0A88B",'white','#2171b5','#08519c','#08306b','#00004d')
    pal_div_BlRd <- rev(c('#67001f','#b2182b','#d6604d','#f4a582','#fddbc7','white','#e0e0e0','#bababa','#878787','#4d4d4d','#1a1a1a'))
    pal_div_RdBl <- c('#67001f','#b2182b','#d6604d','#f4a582','#fddbc7','white','#e0e0e0','#bababa','#878787','#4d4d4d','#1a1a1a')
    pal_div_BrGn <- c('#543005','#8c510a','#bf812d','#dfc27d','#f6e8c3','white','#c7eae5','#80cdc1','#35978f','#01665e','#003c30')
    pal_div_GnBr <- rev(pal_div_BrGn)
    pal_div_RdBlu <- c("#67001f","#b2182b","#d6604d","#f4a582","#fddbc7","#f7f7f7","#d1e5f0","#92c5de","#4393c3","#2166ac","#053061")
    pal_div_BluRd <- rev(pal_div_RdBlu)

    pal_seq <- c('lemonchiffon1','burlywood1','orange1','red1','purple4','black')

    pal_metis <- c(`a Coal` = "gray40",  `c coal` = "gray40", `3 coal` = "gray40", coal = "gray40", `coal (conv pul)` = "gray40",
                 Coal = "gray40",
                 `b Coal w/CCS` = "gray20", `c coal CCS` = "gray20", `Coal CCS` = "gray20",
                 `coal (IGCC CCS)` = "#c0237c",
                 `coal (IGCC)` = "gray60",
                 `coal (conv pul CCS)` = "#e0237c",
                 `c Gas` = "darkslategray1", `b natural gas` = "darkslategray1", `2 gas` = "darkslategray1", Gas = "darkslategray1",
                 gas = "darkslategray1", `gas (CC)` = "darkslategray1",
                 `d Gas w/CCS` = "deepskyblue1", `b natural gas CCS` = "deepskyblue1",
                 `gas (CC CCS)` = "deepskyblue1", `Gas CCS` = "deepskyblue1",
                 `gas (CT)` = "darkslategray4",
                 `gas (steam)` = "darkslategray",
                 `e Oil` = "firebrick4", `a oil` = "firebrick4", oil = "firebrick4", Oil = "firebrick4", liquids="firebrick4",
                 `refined liquids` = "firebrick4", `1 liquids` = "firebrick4",
                 `refined liquids (CC)` = "firebrick4",
                 `refined liquids (CT)` = "firebrick1",
                 `refined liquids (steam)` = "firebrick3",
                 `refined liquids (CC CCS)` = "#f7988f", `f Oil w/CCS` = "#f7988f", `a oil CCS` = "#f7988f", `Oil w/CCS` = "#f7988f",
                 `Oil CCS` = "#f7988f",
                 `fossil fuel liquid`="firebrick4",
                 `International Aviation liquids` = "indianred",`International Aviation Oil` = "indianred", `liquids av`="indianred",
                 `liquids intl av`="indianred", `fossil fuel liquids intl av`="indianred", `oil intl av`="indianred",
                 `liquids intl shp`="lightcoral", `fossil fuel liquids intl shp`="lightcoral",`oil intl shp`="lightcoral",
                 `International Ship liquids` = "lightcoral", `International Ship Oil` = "lightcoral",`liquids shp`="lightcoral",
                 `g Biomass` = "darkolivegreen2", `g Bioenergy` = "darkolivegreen2", `d biomass` = "darkolivegreen2",  biomass = "darkolivegreen2",
                 Biomass = "darkolivegreen2", `Biomass CCS` = "darkolivegreen2",
                 bioenergy = "darkolivegreen2", `d bioenergy` = "darkolivegreen2", Bioenergy = "darkolivegreen2", `h Bioenergy w/CCS` = '#7efd97',
                 `j traditional bioenergy` = "chartreuse3",`traditional bioenergy` = "chartreuse3",`Traditional Bioenergy` = "chartreuse3",
                 `biomass (IGCC CCS)` = "#00a31d", `4 biomass` = "#00a31d",
                 `biomass (IGCC)` = "#00c31d",
                 `biomass (conv CCS)` = "#00e31d",
                 `biomass (conv)` = "#00f31d",
                 biofuel = "darkolivegreen1", Biofuel = "darkolivegreen1", `biomass liquids`="darkolivegreen1",
                 `coal to liquids`='gray40', `gas to liquids`='darkslategray1', `oil refining`='firebrick4',
                 `j traditional biomass` = "#11d081", `traditional biomass` = "#11d081",
                 `h Biomass w/CCS` = "#88c892", `d biomass CCS` = "#88c892",
                 `i Nuclear` = "#ef8e27", `e nuclear` = "#ef8e27", nuclear = "#ef8e27", Gen_II_LWR = "#ef8e27",
                  Nuclear = "#ef8e27",
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
                 `6 hydrogen` = "orange", `hydrogen` = "orange",`Hydrogen Production and Refining` = "black", `Refining` = "black",
                 `Refining and Hydrogen Production` = "black", `CO2 Refining and Hydrogen Production` = "black",
                 `o Battery` = "#92a75d",
                 `energy reduction` = "grey",
                 `Total` = "black", `total` = "black",
                 `Other` = "grey70", other = "grey70",
                 `energy` = "grey50", `Energy` = "grey50",
                 `Fossil` = "gray20", `fossil` = "gray20", `FOSSIL` = "gray20",
                 `fossil fuel`="gray20",
                 #---------------
                 # Sectors
                 #--------------
                 `transport intl. aviation` = "cadetblue3",`Transport Intl Av`="cadetblue3",`trans intl av`="cadetblue3", `transport intl av`="cadetblue3",
                 `International Aviation` = "cadetblue3",
                 `International Ship` = "cadetblue4",
                 `transport intl. shipping` = "cadetblue4",`Transport Intl Shp`="cadetblue4",`trans intl shp`="cadetblue4", `transport intl shp`="cadetblue4",
                 `building` = "#facda4", buildings = "#facda4", elect_td_bld = "#facda4", Building = "#facda4",
                  Buildings = "#facda4", `CO2 Buildings` = "#facda4", `comm non-building` = "#ff230e",
                 `CDD`="cornflowerblue",
                 `Cooling`= "cornflowerblue", `cooling`= "cornflowerblue", `COOLING`= "cornflowerblue",
                 `HDD`="coral2",
                 `Heating`= "coral2", `heating`= "coral2", `HEATING`= "coral2",
                 `Commercial Cooling` = '#342DFC', `Commercial Heating` = '#E11F26', `Commercial Others`='#BD8A25',
                 `Residential Cooling` = '#6865C1', `Residential Heating` = "#D8686C", `Residential Others`='#D4C592',
                 `Residential CoolingHeating` = '#6865C1', `Commercial CoolingHeating` = '#342DFC',
                 `industry` = "#cef4d1", elect_td_ind = "#cef4d1",`Industry` = "#cef4d1", `CO2 Industry`="#cef4d1",
                 `transportation` = "#d0f6f7", `Transportation` = "#d0f6f7", `transport` = "#d0f6f7", `Transport` = "#d0f6f7",
                 `CO2 Transport` = "#d0f6f7",
                 elect_td_trn = "#d0f6f7", `trn_pass_road_bus` = "purple",
                 Agriculture = "forestgreen", agriculture = "forestgreen", Ag = "forestgreen", AG = "forestgreen", Agri = "forestgreen",
                 ag = "forestgreen", crops='forestgreen', Crops='forestgreen',
                 `Electricity` = "lavender", electricity="lavender", `5 electricity` = "lavender",
                  municipal = "dodgerblue", mining = "grey75",
                 livestock='goldenrod2', Livestock= 'goldenrod2', `CO2 Livestock` = 'goldenrod2',
                 #---------------
                 # Transportation
                 #--------------
                 LDV = "#B34545", Bus = "#CADC28", Rail = "#58A989", Plane = "#697080", MotorBike = "#5E85D2",
                 Liquids = 'firebrick4', Electric = 'lavender', Truck='#792f2f', Ship='#2f4e79',

                 #---------------
                 # Emissions
                 #--------------
                 `LUC Emission`="grey30", LUC="grey30", LUCemiss="grey30",
                 `LUC Absorption`="darkolivegreen4",
                 delivered_gas="darkslategray1", refined_liquids_enduse="#d01c2a", H2_enduse = "#507fab",
                 delivered_coal = "black", wholesale_gas="darkslategray1", delivered_biomass="darkolivegreen2",
                 refined_liquids_industrial="#d01c2a",
                 sewage_landfills = 'brown', Waste='#BC7508', `CO2 Waste` = '#BC7508', `CO2 Electricity`='lavender',
                 urban="indianred2",Urban="indianred2",
                 tundra="antiquewhite1",
                 shrubs="lightslateblue",Shrubs="lightslateblue",SHRUBS="lightslateblue",
                 `rock and desert`="black",
                 pasture="goldenrod1",otherarable="darkorange4",grass="darkolivegreen1",forest="darkgreen",
                 naturalOther="grey75",`CO2 Transport Intl Av`="cadetblue3",`CO2 Transport Intl Shp`="cadetblue4",
                 CH4 = "firebrick4", CO2 = "black", HFCs = "cadetblue3", N2O = "forestgreen",
                 #---------------
                 # Agriculture
                 #--------------
                 Forest = "darkgreen" , NonFoodDemand_Forest = "darkolivegreen1",
                 biomass_grass='darkolivegreen2', biomass_tree='darkolivegreen2',
                 Corn = "gold3", corn = "gold3",
                 FiberCrop = "gold4",  MiscCrop = "darkorange4", OilCrop = "gray20", SoySunflower = "gray20",Soy = "gray20",soy = "gray20", SOY = "gray20",
                 OtherGrain  = "indianred2",
                 PalmFruit = "firebrick3" ,  Rice = "steelblue2",
                 RootTuber  = "mediumpurple", Root_Tuber  = "mediumpurple",
                 SugarCrop = "yellow2",
                 Wheat  = "burlywood",
                 shrubland="lightslateblue",Shrubland="lightslateblue",SHRUBLAND="lightslateblue",
                 FodderHerb = "darkseagreen4", FodderGrass = "mediumseagreen",
                 Fodder = "darkseagreen4",
                 Pasture = "goldenrod1",
                 UnmanagedLand = "black",
                 IRR = "dodgerblue3", irr = "dodgerblue3", Irr = "dodgerblue3",
                 irrigation = "dodgerblue3", Irrigation = "dodgerblue3", IRRIGATION = "dodgerblue3",
                 RFD = "gold1", rfd = "gold1", Rfd = "gold1",
                 RAINFED = "gold1", rainfed = "gold1", Rainfed = "gold1"
    )

    if(length(names(pal_metis)[duplicated(names(pal_metis))])>0){
      stop(paste("Duplicated color names in pal_metis: ",paste(names(pal_metis)[duplicated(names(pal_metis))],collapse=", "),sep=""))
    }

    pal_sankey <- c(Agriculture = "forestgreen", agriculture = "forestgreen", Ag = "forestgreen", AG = "forestgreen", Agri = "forestgreen",
                    ag = "forestgreen",
                    AGRI = "forestgreen", agri = "forestgreen", irri = "forestgreen", IRRI = "forestgreen", IRRIGATION = "forestgreen",
                    irrigation = "forestgreen",
                    LIVESTOCK="goldenrod2", Livestock="goldenrod2",livestock="goldenrod2",
                    electricity = "red", Electricity = "red", ELECTRICITY = "red", elec = "red", ELEC = "red", Elec = "red", E = "red",
                    Energy = "darkorange3", ENERGY = "darkorange3", nrg = "darkorange3", NRG = "darkorange3", Nrg = "darkorange3", En = "darkorange3",
                    W = "dodgerblue4", Water = "dodgerblue4", WATER = "dodgerblue4", water = "dodgerblue4", WAT = "dodgerblue4", Wat = "dodgerblue4",
                    wat = "dodgerblue4",
                    primary="goldenrod3", Primary="goldenrod3",PRIMARY="goldenrod3",
                    other="gray",Other="gray",OTHER="gray"); pal_sankey

    pal_sankey_alpha <- add.alpha(pal_sankey,0.7); pal_sankey_alpha

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

    metisPalettes <-list(pal_metis=pal_metis,pal_16 = pal_16,pal_seq=pal_seq,pal_Basic = pal_Basic,
                         pal_hot = pal_hot,pal_green=pal_green,pal_wet=pal_wet,
                         pal_div_wet=pal_div_wet,pal_div_RdBl=pal_div_RdBl,pal_div_BlRd=pal_div_BlRd,
                         pal_div_GnBr=pal_div_GnBr,pal_div_BrGn=pal_div_BrGn,
                         pal_div_BluRd=pal_div_BluRd,pal_div_RdBlu=pal_div_RdBlu,
                         pal_sankey=pal_sankey,pal_sankey_alpha=pal_sankey_alpha,
                         pal_spectral=pal_spectral, pal_ScarcityCat=pal_ScarcityCat)

    invisible(metisPalettes)
}
