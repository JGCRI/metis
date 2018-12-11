#' metis.assumptions
#'
#' This function loads holds the different assumptions used throughout the metis package.
#'
#' List of Assumptions
#' \itemize{
#' \item convEJ2TWh
#' \item convEJ2GW
#' \item conv1975USDperGJ22017USDperMWh
#' \item conv1975USDperGJ22017USDperMBTU
#' \item convertGgTgMTC
#' \item GWPType}
#' @keywords assumptions
#' @return A list of assumptions
#' @export
#' @examples
#' library(metis)
#' a<-metis.assumptions()
#' a # will give full list of assumptions
#' @importFrom magrittr %>%

metis.assumptions <- function() {

  #------------------
  # Load required Libraries
  # -----------------

  requireNamespace("tibble",quietly = T)
  requireNamespace("utils",quietly = T)
  requireNamespace("magrittr",quietly = T)

  #------------------
  # Conversions
  #------------------

  convEJ2TWh<-277.77777777778
  convEJ2GW<-convEJ2TWh*1000/8760
  conv1975USDperGJ22017USDperMWh<-3.62/0.2777778    # Deflators 1975 to 2017 from World Bank https://data.worldbank.org/indicator/NY.GDP.DEFL.ZS?locations=US&view=chart
  conv1975USDperGJ22017USDperMBTU<-3.62/0.947       # Deflators 1975 to 2017 from World Bank https://data.worldbank.org/indicator/NY.GDP.DEFL.ZS?locations=US&view=chart

  # Emissions Conversion to CO2eq
  # GWP conversions - uses 100-yr GWPs from IPPC AR4 and AR5
  # https://www.ghgprotocol.org/sites/default/files/ghgp/Global-Warming-Potential-Values%20%28Feb%2016%202016%29_1.pdf
  # Does not include all covnersions. Add them if they are tracked in GCAM
  GWP<- tibble::tribble(
    ~ghg, ~GWPSAR, ~GWPAR4,~GWPAR5,
    "CO2",44/12,44/12,44/12,
    "CH4",21,25,28,
    "N20",310,298,265,
    "C2F6",9.2,12.2,11.1,
    "CF4",6.5,7.39,6.63,
    "HFC125",2.8,3.5,3.17,
    "HFC134a",1.3,1.43,1.3,
    "HFC245fa",1.03,1.03,0.858,
    "HFC143a",3.8,4.47,4.8,
    "HFC152a",0.140,0.124,0.138,
    "HFC227ea",2.9,3.22,3.35,
    "HFC23",11.7,14.8,12.4,
    "HFC236fa",6.3,9.81,8.06,
    "HFC32",0.650,0.675,0.677,
    "HFC365mfc",0.794,0.794,0.804,
    "SF6",23.9,22.8,23.5
  )%>%as.data.frame;

  convertGgTgMTC<- tibble::tribble(
    ~Units,~Convert,
    "Gg",0.001,
    "Tg",1,
    "MTC",1
  )%>%as.data.frame;

  GWPType<-"GWPAR4" #GWPSAR,GWPAR4,GWPAR5

  return(list(
         convEJ2TWh=convEJ2TWh,
         convEJ2GW=convEJ2GW,
         conv1975USDperGJ22017USDperMWh=conv1975USDperGJ22017USDperMWh,
         conv1975USDperGJ22017USDperMBTU=conv1975USDperGJ22017USDperMBTU,
         convertGgTgMTC=convertGgTgMTC,
         GWPType=GWPType,
         GWP=GWP))
}
