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
  # Mean HFC numbers GWP %>% dplyr::filter(grepl("HFC",ghg)) %>% summarize_at(vars(names(GWP)[!grepl("ghg",names(GWP))]),funs(mean))
  # GTP AR5 Box 3.2 Table 1 https://www.ipcc.ch/site/assets/uploads/2018/02/SYR_AR5_FINAL_full.pdf

  GWP<- tibble::tribble(
    ~ghg, ~GWPSAR, ~GWPAR4,~GWPAR5,~GTPAR5,
    "CO2",44/12,44/12,44/12,44/12,
    "CH4",21,25,28,4,
    "N20",310,298,265,234,
    "C2F6",9200,12200,11100,NA,
    "CF4",6500,7390,6630,8040,
    "HFC125",2800,3500,3170,NA,
    "HFC134a",1300,1430,1300,NA,
    "HFC245fa",1030,1030,858,NA,
    "HFC143a",3800,4470,4800,NA,
    "HFC152a",140,124,138,19,
    "HFC227ea",2900,3220,3350,NA,
    "HFC23",11700,14800,12400,NA,
    "HFC236fa",6300,9810,8060,NA,
    "HFC32",650,675,677,NA,
    "HFC365mfc",794,794,804,NA,
    "HFCs", 3141, 3985, 3555,NA,
    "SF6",23900,22800,23500,NA
  )%>%as.data.frame;

  # https://nepis.epa.gov/Exe/ZyNET.exe/P1001YTS.txt?ZyActionD=ZyDocument&Client=EPA&Index=2000%20Thru%202005&Docs=&Query=&Time=&EndTime=&SearchMethod=1&TocRestrict=n&Toc=&TocEntry=&QField=&QFieldYear=&QFieldMonth=&QFieldDay=&UseQField=&IntQFieldOp=0&ExtQFieldOp=0&XmlQuery=&File=D%3A%5CZYFILES%5CINDEX%20DATA%5C00THRU05%5CTXT%5C00000017%5CP1001YTS.txt&User=ANONYMOUS&Password=anonymous&SortMethod=h%7C-&MaximumDocuments=1&FuzzyDegree=0&ImageQuality=r75g8/r75g8/x150y150g16/i425&Display=hpfr&DefSeekPage=x&SearchBack=ZyActionL&Back=ZyActionS&BackDesc=Results%20page&MaximumPages=1&ZyEntry=3
  # https://www.firescience.gov/projects/09-2-01-9/supdocs/09-2-01-9_Appendix_C_-_Unit_Conversion_and_Other_Tables.pdf
  # MTC is megatonnes (10^6 tonnes) of Carbon.
  # Tg one terragram = 1 Megatonne = 10^6 tonnes
  # Convert to Mega tonnes of CO2 eq.

  convertGgTgMTC<- tibble::tribble(
    ~Units,~Convert,
    "Gg",0.001*1,
    "Tg",1,
    "MTC",1
  )%>%as.data.frame;

  return(list(
         convEJ2TWh=convEJ2TWh,
         convEJ2GW=convEJ2GW,
         conv1975USDperGJ22017USDperMWh=conv1975USDperGJ22017USDperMWh,
         conv1975USDperGJ22017USDperMBTU=conv1975USDperGJ22017USDperMBTU,
         convertGgTgMTC=convertGgTgMTC,
         GWP=GWP))
}
