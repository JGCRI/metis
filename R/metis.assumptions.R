#' metis.assumptions
#'
#' This function loads holds the different assumptions used throughout the metis package.
#'
#'@param name Default=NULL. Name of assumption object.
#' List of Assumptions
#' \itemize{
#' \item GCAMbaseYear,
#' \item convEJ2MTOE,
#' \item convEJ2TWh,
#' \item convEJ2GW,
#' \item convEJ2GWh,
#' \item convGW_kW,
#' \item conv_C_CO2,
#' \item conv_MT_GT,
#' \item hydro_cap_fact,
#' \item hydro_cost_GW,
#' \item convUSD_1975_2010,
#' \item convUSD_1975_2015,
#' \item conv1975USDperGJ22017USDperMWh,
#' \item conv1975USDperGJ22017USDperMBTU,
#' \item convertGgTgMTC,
#' \item GWP,
#' \item US52,
#' \item US49,
#' \item countryToGCAMReg32}
#' @keywords assumptions
#' @return A list of assumptions
#' @export
#' @examples
#' library(metis)
#' a<-metis.assumptions("US49")
#' @importFrom magrittr %>%

metis.assumptions <- function(name=NULL) {

  #------------------
  # Conversions
  #------------------
  GCAMbaseYear <- 2010
  convEJ2MTOE<-23.8845897  #https://www.iea.org/statistics/resources/unitconverter/
  convEJ2TWh<-277.77777777778
  convEJ2GW<-convEJ2TWh*1000/8760
  convEJ2GWh <- 277777.778
  convGW_kW <- 1e6
  conv1975USDperGJ22017USDperMWh<- (103.015/28.485)/0.2777778 # (13.02) Deflators 1975 (28.485) to 2017 (103.015) from World Bank https://data.worldbank.org/indicator/NY.GDP.DEFL.ZS?locations=US&view=chart
  conv1975USDperGJ22017USDperMBTU<- (103.015/28.485)/0.947 # (3.82) Deflators 1975 (28.485) to 2017 (103.015) from World Bank https://data.worldbank.org/indicator/NY.GDP.DEFL.ZS?locations=US&view=chart
  convUSD_1975_2010	<- 91.718/28.485 # (3.22) Deflators 1975 (28.485) to 2010 (91.718) from World Bank https://data.worldbank.org/indicator/NY.GDP.DEFL.ZS?locations=US&view=chart
  convUSD_1975_2015	<- 100/28.485 # (3.51) Deflators 1975 (28.485) to 2015 (100) from World Bank https://data.worldbank.org/indicator/NY.GDP.DEFL.ZS?locations=US&view=chart
  conv_C_CO2 <- 44/12
  conv_MT_GT <- 1e-3

  #--------------------------------------------------------------------------------------------------
  # Emissions Conversion to CO2eq
  # GWP conversions - uses 100-yr GWPs from IPPC AR4 and AR5
  # https://www.ghgprotocol.org/sites/default/files/ghgp/Global-Warming-Potential-Values%20%28Feb%2016%202016%29_1.pdf
  # Does not include all covnersions. Add them if they are tracked in GCAM
  # Mean HFC numbers GWP %>% dplyr::filter(grepl("HFC",ghg)) %>% summarize_at(vars(names(GWP)[!grepl("ghg",names(GWP))]),funs(mean))
  # GTP AR5 Box 3.2 Table 1 https://www.ipcc.ch/site/assets/uploads/2018/02/SYR_AR5_FINAL_full.pdf
  #--------------------------------------------------------------------------------------------------

  GWP<- tibble::tribble(
    ~ghg, ~GWPSAR, ~GWPAR4,~GWPAR5,~GTPAR5,
    "CO2",44/12,44/12,44/12,44/12,
    "CH4",21,25,28,4,
    "CH4_AGR", 21,	25, 28, 28,
    "CH4_AWB", 21,	25, 28, 28,
    "N2O",310,298,265,234,
    "N2O_AGR",310,298,265,234,
    "N2O_AWB",310,298,265,234,
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
    "MTC",1,
    "MtC/yr",1
  )%>%as.data.frame;


  #--------------------------------------------------------------------------------------------------
  # Data source for capacity factor:
  # https://hub.globalccsinstitute.com/publications/renewable-power-generation-costs-2012-overview/52-capacity-factors-hydropower
  # Data source for hydropower capital cost
  # https://hub.globalccsinstitute.com/publications/renewable-power-generation-costs-2012-overview/51-hydropower-capital-costs
  #--------------------------------------------------------------------------------------------------

  hydro_cap_fact <- 0.38
  hydro_cost_GW <- 1.5

  #--------------------------------------------------------------------------------------------------
  # GCAM USA Regions
  #--------------------------------------------------------------------------------------------------
  US52 <- c("AK","AL","AR","AZ","CA","CO","CT","DC","DE","FL","GA",
            "HI","IA","ID","IL","IN","KS","KY","LA","MA","MD","ME",
            "MI","MN","MO","MS","MT","NC","ND","NE","NH","NJ","NM",
            "NV","NY","OH","OK","OR","PA","PR","RI","SC","SD","TN",
            "TX","UT","VA","VT","WA","WI","WV","WY")

  # GCAM USA 49. Excludes Alaska, Hawaii and Puerto Rico. Includes DC.
  US49 <- c("AL","AR","AZ","CA","CO","CT","DC","DE","FL","GA",
            "IA","ID","IL","IN","KS","KY","LA","MA","MD","ME",
            "MI","MN","MO","MS","MT","NC","ND","NE","NH","NJ","NM",
            "NV","NY","OH","OK","OR","PA","RI","SC","SD","TN",
            "TX","UT","VA","VT","WA","WI","WV","WY")

  #--------------------------------------------------------------------------------------------------
  # Country to GCAM 32 Region Mapping
  #--------------------------------------------------------------------------------------------------

  countryToGCAMReg32 <- tibble::tribble(
    ~region_code	,	~ctry_code	,	~region	,	~ctry_name	,
    29	,	238	, "Southeast Asia", "Fiji",
    23	,	103	, "Russia", "Russia",
    13	,	17	, "EU-15", "Wallis & Futuna",
    6	,	223	, "Australia_NZ", "New Zealand",
    1	,	150	, "USA", "United States",
    29	,	40	, "Southeast Asia", "Midway Is.",
    29	,	16	, "Southeast Asia", "Tonga",
    29	,	38	, "Southeast Asia", "Kiribati",
    29	,	14	, "Southeast Asia", "Samoa",
    29	,	15	, "Southeast Asia", "Tokelau",
    29	,	9	, "Southeast Asia", "American Samoa",
    29	,	13	, "Southeast Asia", "Niue",
    29	,	37	, "Southeast Asia", "Johnston Atoll",
    29	,	10	, "Southeast Asia", "Cook Is.",
    29	,	11	, "Southeast Asia", "French Polynesia",
    8	,	34	, "Canada", "Canada",
    29	,	3	, "Southeast Asia", "Pitcairn Is.",
    20	,	39	, "Mexico", "Mexico",
    26	,	21	, "South America_Southern", "Chile",
    9	,	35	, "Central America and Caribbean", "Guatemala",
    26	,	22	, "South America_Southern", "Ecuador",
    9	,	62	, "Central America and Caribbean", "El Salvador",
    9	,	63	, "Central America and Caribbean", "Honduras",
    9	,	57	, "Central America and Caribbean", "Belize",
    9	,	64	, "Central America and Caribbean", "Nicaragua",
    9	,	60	, "Central America and Caribbean", "Costa Rica",
    9	,	61	, "Central America and Caribbean", "Cuba",
    9	,	65	, "Central America and Caribbean", "Panama",
    32	,	59	, "Colombia", "Colombia",
    26	,	24	, "South America_Southern", "Peru",
    9	,	58	, "Central America and Caribbean", "Cayman Is.",
    9	,	55	, "Central America and Caribbean", "The Bahamas",
    9	,	53	, "Central America and Caribbean", "Jamaica",
    9	,	52	, "Central America and Caribbean", "Haiti",
    7	,	20	, "Brazil", "Brazil",
    31	,	18	, "Argentina", "Argentina",
    25	,	8	, "South America_Northern", "Venezuela",
    13	,	67	, "EU-15", "Greenland",
    13	,	56	, "EU-15", "Turks & Caicos Is.",
    9	,	51	, "Central America and Caribbean", "Dominican Republic",
    26	,	19	, "South America_Southern", "Bolivia",
    9	,	54	, "Central America and Caribbean", "Netherlands Antilles",
    1	,	30	, "USA", "Puerto Rico",
    1	,	32	, "USA", "Virgin Is.",
    9	,	50	, "Central America and Caribbean", "Bermuda",
    9	,	26	, "Central America and Caribbean", "Anguilla",
    9	,	31	, "Central America and Caribbean", "St. Kitts & Nevis",
    26	,	23	, "South America_Southern", "Paraguay",
    9	,	29	, "Central America and Caribbean", "Montserrat",
    9	,	7	, "Central America and Caribbean", "Trinidad & Tobago",
    9	,	43	, "Central America and Caribbean", "Grenada",
    9	,	44	, "Central America and Caribbean", "Guadeloupe",
    9	,	27	, "Central America and Caribbean", "Antigua & Barbuda",
    9	,	48	, "Central America and Caribbean", "St. Vincent & the Grenadines",
    9	,	45	, "Central America and Caribbean", "Martinique",
    9	,	42	, "Central America and Caribbean", "Dominica",
    13	,	0	, "EU-15", "Falkland Is.",
    25	,	2	, "South America_Northern", "Guyana",
    9	,	41	, "Central America and Caribbean", "Barbados",
    26	,	25	, "South America_Southern", "Uruguay",
    25	,	6	, "South America_Northern", "Suriname",
    13	,	47	, "EU-15", "St. Pierre & Miquelon",
    25	,	1	, "South America_Northern", "French Guiana",
    26	,	4	, "South America_Southern", "South Georgia & the South Sandwich Is.",
    13	,	81	, "EU-15", "Portugal",
    5	,	75	, "Africa_Western", "Cape Verde",
    16	,	69	, "European Free Trade Association", "Iceland",
    13	,	82	, "EU-15", "Spain",
    5	,	89	, "Africa_Western", "Senegal",
    5	,	88	, "Africa_Western", "Mauritania",
    3	,	83	, "Africa_Northern", "Western Sahara",
    5	,	91	, "Africa_Western", "The Gambia",
    5	,	86	, "Africa_Western", "Guinea-Bissau",
    5	,	85	, "Africa_Western", "Guinea",
    13	,	5	, "EU-15", "St. Helena",
    5	,	90	, "Africa_Western", "Sierra Leone",
    3	,	80	, "Africa_Northern", "Morocco",
    5	,	87	, "Africa_Western", "Mali",
    5	,	79	, "Africa_Western", "Liberia",
    13	,	70	, "EU-15", "Ireland",
    16	,	72	, "European Free Trade Association", "Jan Mayen",
    5	,	76	, "Africa_Western", "Cote d'Ivoire",
    3	,	106	, "Africa_Northern", "Algeria",
    13	,	74	, "EU-15", "United Kingdom",
    13	,	66	, "EU-15", "Faroe Is.",
    5	,	84	, "Africa_Western", "Burkina Faso",
    13	,	145	, "EU-15", "France",
    13	,	71	, "EU-15", "Isle of Man",
    5	,	77	, "Africa_Western", "Ghana",
    13	,	68	, "EU-15", "Guernsey",
    13	,	73	, "EU-15", "Jersey",
    5	,	119	, "Africa_Western", "Togo",
    5	,	116	, "Africa_Western", "Niger",
    5	,	111	, "Africa_Western", "Benin",
    5	,	117	, "Africa_Western", "Nigeria",
    13	,	144	, "EU-15", "Belgium",
    13	,	248	, "EU-15", "Netherlands",
    16	,	102	, "European Free Trade Association", "Norway",
    5	,	118	, "Africa_Western", "Sao Tome & Principe",
    13	,	148	, "EU-15", "Luxembourg",
    13	,	146	, "EU-15", "Germany",
    16	,	149	, "European Free Trade Association", "Switzerland",
    13	,	123	, "EU-15", "Italy",
    3	,	110	, "Africa_Northern", "Tunisia",
    5	,	115	, "Africa_Western", "Equatorial Guinea",
    13	,	138	, "EU-15", "Denmark",
    5	,	220	, "Africa_Western", "Gabon",
    5	,	112	, "Africa_Western", "Cameroon",
    3	,	108	, "Africa_Northern", "Libya",
    13	,	136	, "EU-15", "Austria",
    13	,	104	, "EU-15", "Sweden",
    16	,	143	, "European Free Trade Association", "Svalbard",
    5	,	218	, "Africa_Western", "Congo Rep.",
    4	,	222	, "Africa_Southern", "Namibia",
    4	,	217	, "Africa_Southern", "Angola",
    5	,	219	, "Africa_Western", "Congo DRC",
    12	,	137	, "EU-12", "Czech Republic",
    5	,	114	, "Africa_Western", "Chad",
    15	,	122	, "Europe_Non_EU", "Croatia",
    12	,	142	, "EU-12", "Slovenia",
    12	,	125	, "EU-12", "Malta",
    5	,	113	, "Africa_Western", "Central African Republic",
    12	,	140	, "EU-12", "Poland",
    15	,	121	, "Europe_Non_EU", "Bosnia & Herzegovina",
    24	,	224	, "South Africa", "South Africa",
    12	,	139	, "EU-12", "Hungary",
    12	,	141	, "EU-12", "Slovakia",
    15	,	247	, "Europe_Non_EU", "Montenegro",
    15	,	120	, "Europe_Non_EU", "Albania",
    15	,	246	, "Europe_Non_EU", "Serbia",
    13	,	132	, "EU-15", "Greece",
    13	,	153	, "EU-15", "Finland",
    4	,	202	, "Africa_Southern", "Botswana",
    15	,	124	, "Europe_Non_EU", "Macedonia",
    12	,	157	, "EU-12", "Romania",
    12	,	155	, "EU-12", "Lithuania",
    12	,	154	, "EU-12", "Latvia",
    12	,	152	, "EU-12", "Estonia",
    4	,	207	, "Africa_Southern", "Zambia",
    2	,	95	, "Africa_Eastern", "Sudan",
    12	,	128	, "EU-12", "Bulgaria",
    14	,	158	, "Europe_Eastern", "Ukraine",
    14	,	151	, "Europe_Eastern", "Belarus",
    3	,	130	, "Africa_Northern", "Egypt",
    4	,	208	, "Africa_Southern", "Zimbabwe",
    15	,	135	, "Europe_Non_EU", "Turkey",
    4	,	221	, "Africa_Southern", "Lesotho",
    14	,	156	, "Europe_Eastern", "Moldova",
    2	,	203	, "Africa_Eastern", "Burundi",
    2	,	205	, "Africa_Eastern", "Rwanda",
    4	,	206	, "Africa_Southern", "Tanzania",
    2	,	96	, "Africa_Eastern", "Uganda",
    4	,	216	, "Africa_Southern", "Mozambique",
    4	,	225	, "Africa_Southern", "Swaziland",
    12	,	129	, "EU-12", "Cyprus",
    4	,	215	, "Africa_Southern", "Malawi",
    2	,	94	, "Africa_Eastern", "Ethiopia",
    2	,	204	, "Africa_Eastern", "Kenya",
    21	,	97	, "Middle East", "Gaza Strip",
    21	,	99	, "Middle East", "Israel",
    21	,	173	, "Middle East", "Saudi Arabia",
    21	,	100	, "Middle East", "Jordan",
    21	,	105	, "Middle East", "West Bank",
    21	,	133	, "Middle East", "Lebanon",
    21	,	134	, "Middle East", "Syria",
    2	,	93	, "Africa_Eastern", "Eritrea",
    21	,	98	, "Middle East", "Iraq",
    2	,	212	, "Africa_Eastern", "French Southern & Antarctic Lands",
    10	,	131	, "Central Asia", "Georgia",
    2	,	162	, "Africa_Eastern", "Somalia",
    2	,	92	, "Africa_Eastern", "Djibouti",
    21	,	166	, "Middle East", "Yemen",
    2	,	214	, "Africa_Eastern", "Juan De Nova I.",
    2	,	228	, "Africa_Eastern", "Madagascar",
    2	,	211	, "Africa_Eastern", "Comoros",
    10	,	167	, "Central Asia", "Armenia",
    21	,	170	, "Middle East", "Iran",
    29	,	230	, "Southeast Asia", "Mayotte",
    10	,	168	, "Central Asia", "Azerbaijan",
    29	,	232	, "Southeast Asia", "Seychelles",
    21	,	171	, "Middle East", "Kuwait",
    2	,	227	, "Africa_Eastern", "Glorioso Is.",
    10	,	101	, "Central Asia", "Kazakhstan",
    21	,	172	, "Middle East", "Qatar",
    21	,	169	, "Middle East", "Bahrain",
    21	,	174	, "Middle East", "United Arab Emirates",
    21	,	161	, "Middle East", "Oman",
    10	,	164	, "Central Asia", "Turkmenistan",
    2	,	231	, "Africa_Eastern", "Reunion",
    10	,	165	, "Central Asia", "Uzbekistan",
    2	,	229	, "Africa_Eastern", "Mauritius",
    27	,	175	, "South Asia", "Afghanistan",
    22	,	178	, "Pakistan", "Pakistan",
    10	,	179	, "Central Asia", "Tajikistan",
    17	,	159	, "India", "India",
    10	,	176	, "Central Asia", "Kyrgyzstan",
    27	,	226	, "South Asia", "British Indian Ocean Territory",
    6	,	213	, "Australia_NZ", "Heard I. & McDonald Is.",
    27	,	160	, "South Asia", "Maldives",
    11	,	183	, "China", "China",
    27	,	163	, "South Asia", "Sri Lanka",
    27	,	177	, "South Asia", "Nepal",
    27	,	180	, "South Asia", "Bangladesh",
    10	,	184	, "Central Asia", "Mongolia",
    27	,	181	, "South Asia", "Bhutan",
    29	,	190	, "Southeast Asia", "Myanmar",
    18	,	236	, "Indonesia", "Indonesia",
    29	,	235	, "Southeast Asia", "Cocos Is.",
    29	,	194	, "Southeast Asia", "Thailand",
    29	,	189	, "Southeast Asia", "Malaysia",
    29	,	188	, "Southeast Asia", "Laos",
    29	,	187	, "Southeast Asia", "Cambodia",
    29	,	195	, "Southeast Asia", "Vietnam",
    29	,	192	, "Southeast Asia", "Singapore",
    29	,	234	, "Southeast Asia", "Christmas I.",
    6	,	233	, "Australia_NZ", "Australia",
    29	,	182	, "Southeast Asia", "Brunei",
    29	,	186	, "Southeast Asia", "Philippines",
    30	,	183	, "Taiwan", "China",
    19	,	197	, "Japan", "Japan",
    29	,	237	, "Southeast Asia", "Timor-Leste",
    29	,	191	, "Southeast Asia", "North Korea",
    28	,	193	, "South Korea", "South Korea",
    29	,	185	, "Southeast Asia", "Palau",
    29	,	199	, "Southeast Asia", "Micronesia",
    29	,	242	, "Southeast Asia", "Papua New Guinea",
    29	,	196	, "Southeast Asia", "Guam",
    29	,	200	, "Southeast Asia", "Northern Mariana Is.",
    29	,	243	, "Southeast Asia", "Solomon Is.",
    29	,	198	, "Southeast Asia", "Marshall Is.",
    29	,	240	, "Southeast Asia", "New Caledonia",
    29	,	245	, "Southeast Asia", "Vanuatu",
    29	,	239	, "Southeast Asia", "Nauru",
    29	,	201	, "Southeast Asia", "Wake I.",
    29	,	241	, "Southeast Asia", "Norfolk I.",
    29	,	244	, "Southeast Asia", "Tuvalu",
    33	,	248	, "Uruguay", "Uruguay",
    33	,	150	, "AK", "United States",
    34	,	150	, "AL", "United States",
    35	,	150	, "AR", "United States",
    36	,	150	, "AZ", "United States",
    37	,	150	, "CA", "United States",
    38	,	150	, "CO", "United States",
    39	,	150	, "CT", "United States",
    40	,	150	, "DC", "United States",
    41	,	150	, "DE", "United States",
    42	,	150	, "FL", "United States",
    43	,	150	, "GA", "United States",
    44	,	150	, "HI", "United States",
    45	,	150	, "IA", "United States",
    46	,	150	, "ID", "United States",
    47	,	150	, "IL", "United States",
    48	,	150	, "IN", "United States",
    49	,	150	, "KS", "United States",
    50	,	150	, "KY", "United States",
    51	,	150	, "LA", "United States",
    52	,	150	, "MA", "United States",
    53	,	150	, "MD", "United States",
    54	,	150	, "ME", "United States",
    55	,	150	, "MI", "United States",
    56	,	150	, "MN", "United States",
    57	,	150	, "MO", "United States",
    58	,	150	, "MS", "United States",
    59	,	150	, "MT", "United States",
    60	,	150	, "NC", "United States",
    61	,	150	, "ND", "United States",
    62	,	150	, "NE", "United States",
    63	,	150	, "NH", "United States",
    64	,	150	, "NJ", "United States",
    65	,	150	, "NM", "United States",
    66	,	150	, "NV", "United States",
    67	,	150	, "NY", "United States",
    68	,	150	, "OH", "United States",
    69	,	150	, "OK", "United States",
    70	,	150	, "OR", "United States",
    71	,	150	, "PA", "United States",
    72	,	150	, "RI", "United States",
    73	,	150	, "SC", "United States",
    74	,	150	, "SD", "United States",
    75	,	150	, "TN", "United States",
    76	,	150	, "TX", "United States",
    77	,	150	, "UT", "United States",
    78	,	150	, "VA", "United States",
    79	,	150	, "VT", "United States",
    80	,	150	, "WA", "United States",
    81	,	150	, "WI", "United States",
    82	,	150	, "WV", "United States",
    83	,	150	, "WY", "United States"
  )

  #--------------------------------------------------------------------------------------------------
  # Return Data
  #--------------------------------------------------------------------------------------------------

  listx <- list(
    GCAMbaseYear=GCAMbaseYear,
    convEJ2MTOE=convEJ2MTOE,
    convEJ2TWh=convEJ2TWh,
    convEJ2GW=convEJ2GW,
    convEJ2GWh=convEJ2GWh,
    convGW_kW=convGW_kW,
    conv_C_CO2=conv_C_CO2,
    conv_MT_GT=conv_MT_GT,
    hydro_cap_fact=hydro_cap_fact,
    hydro_cost_GW=hydro_cost_GW,
    convUSD_1975_2010=convUSD_1975_2010,
    convUSD_1975_2015=convUSD_1975_2015,
    conv1975USDperGJ22017USDperMWh=conv1975USDperGJ22017USDperMWh,
    conv1975USDperGJ22017USDperMBTU=conv1975USDperGJ22017USDperMBTU,
    convertGgTgMTC=convertGgTgMTC,
    GWP=GWP,
    US52=US52,
    US49=US49,
    countryToGCAMReg32=countryToGCAMReg32)

  if(!is.null(name)){returnx <- listx[[name]]} else {returnx <- NULL }


  return(returnx)
}
