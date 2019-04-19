
#----------------------------
# Install necessary packages
#----------------------------
if("devtools" %in% rownames(installed.packages()) == F){install.packages("devtools")}
library(devtools)
if("metis" %in% rownames(installed.packages()) == F){install_github(repo="JGCRI/metis")}
library(metis)
if("rgcam" %in% rownames(installed.packages()) == F){install_github(repo="JGCRI/rgcam")}
library(rgcam)
if("tibble" %in% rownames(installed.packages()) == F){install.packages("tibble")}
library(tibble)
if("dplyr" %in% rownames(installed.packages()) == F){install.packages("dlpyr")}
library(dplyr)
if("rgdal" %in% rownames(installed.packages()) == F){install.packages("rgdal")}
library(rgdal)
if("tmap" %in% rownames(installed.packages()) == F){install.packages("tmap")}
library(tmap)
if("rgeos" %in% rownames(installed.packages()) == F){install.packages("rgeos")}
library(rgeos)
if("ggplot2" %in% rownames(installed.packages()) == F){install.packages("ggplot2")}
library(ggplot2)
if("ggalluvial" %in% rownames(installed.packages()) == F){install.packages("ggaaluvial")}
library(ggalluvial)


#-------------
# Workflow for Metis I/O Analysis

# Small Example
Z0=tibble::tribble( # Initial Flows
  ~sector ,    ~W,         ~E,   ~Ag,
  "W"     ,    0,           50,   100,
  "E"     ,    20,          0,     10,
  "Ag"    ,     0,          5,     0);Z0


A0=tibble::tribble( # Initial Flows
  ~sector ,    ~W,         ~E,     ~Ag,
  "W"     ,    0,           0.4,    0.5,
  "E"     ,    0.2,         0,     0.0513,
  "Ag"    ,    0,           0.1,     0);A0


D0=tibble::tribble( # Initial total demand
  ~sector, ~other, ~industry, ~domestic,
      "W",    50,     25,      25,
      "E",    100,    50,      50,
      "Ag",   150,    30,      10
);D0

Cap0=tibble::tribble( # Initial total demand
  ~sector, ~cap,
  "W",    100,
  "E",    50,
  "Ag",   200
);Cap0


Import0=tibble::tribble( # Initial total demand
  ~sector, ~import,
  "W",    1000,
  "E",    30,
  "Ag",   0
);Import0

Export0=tibble::tribble( # Initial total demand
  ~sector, ~export,
  "W",    5000,
  "E",    0,
  "Ag",   0
);Export0

X0=tibble::tribble( # Initial total demand
  ~sector, ~localProduction,
  "W",    320,
  "E",    225,
  "Ag",   220
);X0


io1<-metis.io(Z0=Z0,D0=D0, Cap0=Cap0, nameAppend = "_smallEg1"); io1$sol_Output%>%as.data.frame(); io1$A_Output%>%as.data.frame(); Z0;D0; io1$ioTblImports_Output%>%as.data.frame()
io2<-metis.io(Z0=Z0,X0=X0, Cap0=Cap0, nameAppend = "_smallEg2"); io2$sol_Output%>%as.data.frame(); io2$A_Output%>%as.data.frame(); Z0;X0; io2$ioTblImports_Output%>%as.data.frame()
io3<-metis.io(A0=A0,D0=D0, Cap0=Cap0, nameAppend = "_smallEg3"); io3$sol_Output%>%as.data.frame(); io3$A_Output%>%as.data.frame(); A0; D0; io3$ioTblImports_Output%>%as.data.frame()
io4<-metis.io(A0=A0,X0=X0, Cap0=Cap0, nameAppend = "_smallEg4"); io4$sol_Output%>%as.data.frame(); io4$A_Output%>%as.data.frame(); A0; X0; io4$ioTblImports_Output%>%as.data.frame()
# Combinations of only A0 & Z0 or D0 & X0 not enogh to solve for nexus flows.
#io5<-metis.io(D0=D0,X0=X0, Cap0=Cap0, Export0=Export0, Import0=Import0,nameAppend = "_smallEg5"); io5$sol_Output%>%as.data.frame(); io5$A_Output%>%as.data.frame(); Z0;X0; io5$ioTblImports_Output%>%as.data.frame()
#io6<-metis.io(A0=A0,Z0=Z0, Cap0=Cap0, Import0=Import0,nameAppend = "_smallEg6"); io6$sol_Output%>%as.data.frame(); io6$A_Output%>%as.data.frame(); Z0;X0; io6$ioTblImports_Output%>%as.data.frame()

# With Exports
io1<-metis.io(Z0=Z0,D0=D0, Cap0=Cap0, Export0=Export0, nameAppend = "_smallEgExp1"); io1$sol_Output%>%as.data.frame(); io1$A_Output%>%as.data.frame(); Z0;D0; io1$ioTblImports_Output%>%as.data.frame()
io2<-metis.io(Z0=Z0,X0=X0, Cap0=Cap0, Export0=Export0, nameAppend = "_smallEgExp2"); io2$sol_Output%>%as.data.frame(); io2$A_Output%>%as.data.frame(); Z0;X0; io2$ioTblImports_Output%>%as.data.frame()
io3<-metis.io(A0=A0,D0=D0, Cap0=Cap0, Export0=Export0, nameAppend = "_smallEgExp3"); io3$sol_Output%>%as.data.frame(); io3$A_Output%>%as.data.frame(); A0; D0; io3$ioTblImports_Output%>%as.data.frame()
io4<-metis.io(A0=A0,X0=X0, Cap0=Cap0, Export0=Export0, nameAppend = "_smallEgExp4"); io4$sol_Output%>%as.data.frame(); io4$A_Output%>%as.data.frame(); A0; X0; io4$ioTblImports_Output%>%as.data.frame()

# With Imports
io1<-metis.io(Z0=Z0,D0=D0, Cap0=Cap0, Import0=Import0,nameAppend = "_smallEgImp1"); io1$sol_Output%>%as.data.frame(); io1$A_Output%>%as.data.frame(); Z0;D0; io1$ioTblImports_Output%>%as.data.frame()
io2<-metis.io(Z0=Z0,X0=X0, Cap0=Cap0, Import0=Import0,nameAppend = "_smallEgImp2"); io2$sol_Output%>%as.data.frame(); io2$A_Output%>%as.data.frame(); Z0;X0; io2$ioTblImports_Output%>%as.data.frame()
io3<-metis.io(A0=A0,D0=D0, Cap0=Cap0, Import0=Import0,nameAppend = "_smallEgImp3"); io3$sol_Output%>%as.data.frame(); io3$A_Output%>%as.data.frame(); A0; D0; io3$ioTblImports_Output%>%as.data.frame()
io4<-metis.io(A0=A0,X0=X0, Cap0=Cap0, Import0=Import0,nameAppend = "_smallEgImp4"); io4$sol_Output%>%as.data.frame(); io4$A_Output%>%as.data.frame(); A0; X0; io4$ioTblImports_Output%>%as.data.frame()

# With Exports & Imports
io1<-metis.io(Z0=Z0,D0=D0, Cap0=Cap0, Export0=Export0, Import0=Import0,nameAppend = "_smallEgImpExp1"); io1$sol_Output%>%as.data.frame(); io1$A_Output%>%as.data.frame(); Z0;D0; io1$ioTblImports_Output%>%as.data.frame()
io2<-metis.io(Z0=Z0,X0=X0, Cap0=Cap0, Export0=Export0, Import0=Import0,nameAppend = "_smallEgImpExp2"); io2$sol_Output%>%as.data.frame(); io2$A_Output%>%as.data.frame(); Z0;X0; io2$ioTblImports_Output%>%as.data.frame()
io3<-metis.io(A0=A0,D0=D0, Cap0=Cap0, Export0=Export0, Import0=Import0,nameAppend = "_smallEgImpExp3"); io3$sol_Output%>%as.data.frame(); io3$A_Output%>%as.data.frame(); A0; D0; io3$ioTblImports_Output%>%as.data.frame()
io4<-metis.io(A0=A0,X0=X0, Cap0=Cap0, Export0=Export0, Import0=Import0,nameAppend = "_smallEgImpExp4"); io4$sol_Output%>%as.data.frame(); io4$A_Output%>%as.data.frame(); A0; X0; io4$ioTblImports_Output%>%as.data.frame()



#---------------------
# Multi Scenario Examples
#----------------------

A0=tibble::tribble( # Initial Flows
  ~sector ,    ~W,         ~E,    ~scenario,  ~subRegion,  ~year,
  "W"     ,    0,           0.23,  "ScenA",   "SubRegA",  2010,
  "E"     ,    0.13,          0,   "ScenA",   "SubRegA",  2010,
  "W"     ,    0,           0.3,  "ScenB",   "SubRegA",  2010,
  "E"     ,    0.2,          0,   "ScenB",   "SubRegA",  2010,
  "W"     ,    0,           0.1,  "ScenA",   "SubRegB",  2010,
  "E"     ,    0.05,          0,   "ScenA",   "SubRegB",  2010,
  "W"     ,    0,           0.15,  "ScenB",   "SubRegB",  2010,
  "E"     ,    0.25,          0,   "ScenB",   "SubRegB",  2010,
  "W"     ,    0,           0.4,  "ScenA",   "SubRegA",  2015,
  "E"     ,    0.1,          0,   "ScenA",   "SubRegA",  2015,
  "W"     ,    0,           0.35,  "ScenB",   "SubRegA",  2015,
  "E"     ,    0.25,          0,   "ScenB",   "SubRegA",  2015,
  "W"     ,    0,           0.17,  "ScenA",   "SubRegB",  2015,
  "E"     ,    0.15,          0,   "ScenA",   "SubRegB",  2015,
  "W"     ,    0,           0.45,  "ScenB",   "SubRegB",  2015,
  "E"     ,    0.03,          0,   "ScenB",   "SubRegB",  2015
);A0


Z0=tibble::tribble( # Initial Flows
  ~sector ,    ~W,         ~E,    ~scenario,  ~subRegion,  ~year,
  "W"     ,    0,           50,  "ScenA",   "SubRegA",  2010,
  "E"     ,    13,          0,   "ScenA",   "SubRegA",  2010,
  "W"     ,    0,           40,  "ScenB",   "SubRegA",  2010,
  "E"     ,    20,          0,   "ScenB",   "SubRegA",  2010,
  "W"     ,    0,           100,  "ScenA",   "SubRegB",  2010,
  "E"     ,    5,           0,   "ScenA",   "SubRegB",  2010,
  "W"     ,    0,           70,  "ScenB",   "SubRegB",  2010,
  "E"     ,    25,          0,   "ScenB",   "SubRegB",  2010,
  "W"     ,    0,           60,  "ScenA",   "SubRegA",  2015,
  "E"     ,    10,          0,   "ScenA",   "SubRegA",  2015,
  "W"     ,    0,           65,  "ScenB",   "SubRegA",  2015,
  "E"     ,    25,          0,   "ScenB",   "SubRegA",  2015,
  "W"     ,    0,           35,  "ScenA",   "SubRegB",  2015,
  "E"     ,    20,          0,   "ScenA",   "SubRegB",  2015,
  "W"     ,    0,           25,  "ScenB",   "SubRegB",  2015,
  "E"     ,    10,          0,   "ScenB",   "SubRegB",  2015
);Z0


D0=tibble::tribble( # Initial total demand
  ~sector, ~other, ~industry, ~domestic,  ~scenario,  ~subRegion,  ~year,
  "W",    50,     25, 25,  "ScenA",   "SubRegA",  2010,
  "E",    100,    50, 50, "ScenA",   "SubRegA",  2010,
  "W",    20,     30, 35,  "ScenB",   "SubRegA",  2010,
  "E",    10,     70, 70, "ScenB",   "SubRegA",  2010,
  "W",    30,     30, 85,  "ScenA",   "SubRegB",  2010,
  "E",    70,     25, 30, "ScenA",   "SubRegB",  2010,
  "W",    55,     30, 45,  "ScenB",   "SubRegB",  2010,
  "E",    50,     60, 60, "ScenB",   "SubRegB",  2010,
  "W",    20,     45, 25,  "ScenA",   "SubRegA",  2015,
  "E",    15,     50, 40, "ScenA",   "SubRegA",  2015,
  "W",    55,     25, 65,  "ScenB",   "SubRegA",  2015,
  "E",    75,     50, 80, "ScenB",   "SubRegA",  2015,
  "W",    500,    25, 95,  "ScenA",   "SubRegB",  2015,
  "E",    10,     50, 20, "ScenA",   "SubRegB",  2015,
  "W",    55,     25, 45,  "ScenB",   "SubRegB",  2015,
  "E",    110,    50, 60, "ScenB",   "SubRegB",  2015
);D0

Cap0=tibble::tribble( # Initial total demand
  ~sector ,    ~cap,        ~scenario,  ~subRegion,  ~year,
  "W"     ,    100,           "ScenA",   "SubRegA",  2010,
  "E"     ,    130,          "ScenA",   "SubRegA",  2010,
  "W"     ,    10,           "ScenB",   "SubRegA",  2010,
  "E"     ,    200,          "ScenB",   "SubRegA",  2010,
  "W"     ,    10,           "ScenA",   "SubRegB",  2010,
  "E"     ,    150,           "ScenA",   "SubRegB",  2010,
  "W"     ,    0,           "ScenB",   "SubRegB",  2010,
  "E"     ,    250,          "ScenB",   "SubRegB",  2010,
  "W"     ,    110,           "ScenA",   "SubRegA",  2015,
  "E"     ,    110,          "ScenA",   "SubRegA",  2015,
  "W"     ,    250,           "ScenB",   "SubRegA",  2015,
  "E"     ,    250,          "ScenB",   "SubRegA",  2015,
  "W"     ,    200,           "ScenA",   "SubRegB",  2015,
  "E"     ,    2000,          "ScenA",   "SubRegB",  2015,
  "W"     ,    1000,           "ScenB",   "SubRegB",  2015,
  "E"     ,    100,          "ScenB",   "SubRegB",  2015
);Cap0


Import0=tibble::tribble( # Initial total demand
  ~sector ,    ~import,        ~scenario,  ~subRegion,  ~year,
  "W"     ,    10,           "ScenA",   "SubRegA",  2010,
  "E"     ,    10,          "ScenA",   "SubRegA",  2010,
  "W"     ,    0,           "ScenB",   "SubRegA",  2010,
  "E"     ,    20,          "ScenB",   "SubRegA",  2010,
  "W"     ,    0,           "ScenA",   "SubRegB",  2010,
  "E"     ,    150,           "ScenA",   "SubRegB",  2010,
  "W"     ,    0,           "ScenB",   "SubRegB",  2010,
  "E"     ,    20,          "ScenB",   "SubRegB",  2010,
  "W"     ,    0,           "ScenA",   "SubRegA",  2015,
  "E"     ,    0,          "ScenA",   "SubRegA",  2015,
  "W"     ,    20,           "ScenB",   "SubRegA",  2015,
  "E"     ,    20,          "ScenB",   "SubRegA",  2015,
  "W"     ,    20,           "ScenA",   "SubRegB",  2015,
  "E"     ,    0,          "ScenA",   "SubRegB",  2015,
  "W"     ,    10,           "ScenB",   "SubRegB",  2015,
  "E"     ,    10,          "ScenB",   "SubRegB",  2015
);Import0

X0=tibble::tribble( # Initial total demand
  ~sector ,    ~localProduction,        ~scenario,  ~subRegion,  ~year,
  "W"     ,    1000,           "ScenA",   "SubRegA",  2010,
  "E"     ,    130,          "ScenA",   "SubRegA",  2010,
  "W"     ,    10,           "ScenB",   "SubRegA",  2010,
  "E"     ,    200,          "ScenB",   "SubRegA",  2010,
  "W"     ,    100,           "ScenA",   "SubRegB",  2010,
  "E"     ,    150,           "ScenA",   "SubRegB",  2010,
  "W"     ,    100,           "ScenB",   "SubRegB",  2010,
  "E"     ,    20,          "ScenB",   "SubRegB",  2010,
  "W"     ,    10,           "ScenA",   "SubRegA",  2015,
  "E"     ,    110,          "ScenA",   "SubRegA",  2015,
  "W"     ,    250,           "ScenB",   "SubRegA",  2015,
  "E"     ,    250,          "ScenB",   "SubRegA",  2015,
  "W"     ,    200,           "ScenA",   "SubRegB",  2015,
  "E"     ,    200,          "ScenA",   "SubRegB",  2015,
  "W"     ,    100,           "ScenB",   "SubRegB",  2015,
  "E"     ,    10,          "ScenB",   "SubRegB",  2015
);X0


io0<-metis.io(A0=A0,D0=D0, Cap0=Cap0, nameAppend = "_multiEg0"); io0$sol_Output;
io1<-metis.io(A0=A0,D0=D0, Cap0=Cap0, Import0=Import0,nameAppend = "_multiEg1"); io1$sol_Output; io1$A_Output; A0; D0
io2<-metis.io(A0=A0,X0=X0, Cap0=Cap0, Import0=Import0,nameAppend = "_multiEg2"); io2$sol_Output%>%as.data.frame; io2$A_Output; A0; X0
io3<-metis.io(Z0=Z0,D0=D0, Cap0=Cap0, Import0=Import0,nameAppend = "_multiEg3"); io3$sol_Output; io3$A_Output; Z0;D0
io4<-metis.io(Z0=Z0,X0=X0, Cap0=Cap0, Import0=Import0,nameAppend = "_multiEg4"); io4$sol_Output; io4$A_Output; Z0;X0


#--------------------------------
# Real Example With metis Outputs
#--------------------------------

# Tethys (Water Demands)
#Water_E
#Water_Ag
#Water_domestic
#Water_localProduction

# Demeter + GCAM (Ag Demands)
#Ag_localProduction # Ag All

# GCAM (Elec Demands)
#E_localProduction # Buildings, Industry, Transport


dataTables<-c(paste(getwd(),"/outputs/Maps/Tables/subReg_origData_byClass_Colombia_subBasin_origDownscaled_local.csv",sep=""))
a<-read.csv(dataTables);
head(a); s<-unique(a$scenario); p<-unique(a$param); y<-unique(a$x); r<-unique(a$subRegion)
# Choose 2 scenarios, 3 regions, tethys, xanthos, 2010, 2030
a1 <- a %>%
  dplyr::select(scenario,param,units,class,value,x,subRegion,region) %>%
  dplyr::filter(scenario %in% c("Eg1_NA_NA","gfdl-esm2m_rcp4p5_NA_NA"),
                param %in% c("tethysWatWithdraw_indv","tethysWatWithdraw_total","xanthosRunoff"),
                subRegion %in% r[3:5],
                x %in% c(2010,2030)) %>%
  dplyr::left_join(data.frame(subRegion=r[3:5],subRegionN=c("bermejo1","bermejo2","sanFransisco"))) %>%
  dplyr::mutate(subRegion=subRegionN, region="Argentina") %>% dplyr::select(-subRegionN) %>%
  dplyr::left_join(data.frame(class=c("Domestic", "Electric", "Irrigation", "Livestock", "Manufacturing","Mining", "Total", "Runoff"),
                              ioClass=c("domestic","E","Ag","domestic","domestic","domestic","localProduction","cap"),
                              sector=c("W","W","W","W","W","W","W","W"))) %>%
  dplyr::left_join(data.frame(scenario=c("Eg1_NA_NA","gfdl-esm2m_rcp4p5_NA_NA"),
                              scenarioN=c("Scenario_Ref","Scenario_Impact"))) %>%
  dplyr::mutate(scenario=scenarioN) %>% dplyr::select(-scenarioN) %>%
  dplyr::select(-class) %>%
  dplyr::group_by_at(dplyr::vars(-value))%>%
  dplyr::summarize_at(c("value"),dplyr::funs(sum)) %>%
  ungroup() %>%
  unique(); a1

a2 <- a1 %>% bind_rows(a1 %>%
                         filter(ioClass %in% c("Ag","E")) %>%
                         dplyr::select(-ioClass,-sector,-value) %>%
                         unique %>%
                         mutate (ioClass="W",sector="W",value=0))
a3 <- a2 %>% bind_rows(a2 %>%
                         filter(ioClass %in% c("Ag","E","W")) %>%
                         mutate(scenario="Scenario_Impact", value=value*2*runif(1)));a3

z <- tidyr::spread(a3 %>% filter(ioClass %in% c("Ag","E","W")), key=ioClass, value=value);
z1 <- z %>%
  mutate(param="Flows", units="m3") %>%
  bind_rows(z %>%
             mutate(sector="E",
                    param="Flows",
                    units ="TWh",
                    Ag=0,
                    E=0,
                    W=0)) %>%
  bind_rows(z %>%
             mutate(sector="Ag",
                    param="Flows",
                    units="kg",
                    Ag=0,
                    E=E*2*runif(1),
                    W=0));z1

x <- tidyr::spread(a1 %>% filter(ioClass %in% c("localProduction")), key=ioClass, value=value);x
x <- x %>% bind_rows(x %>%
                       mutate(scenario="Scenario_Impact",
                                       localProduction=localProduction*2*runif(1)));x

x1 <- x %>%
  mutate(param="localProduction", units="m3") %>%
  bind_rows(x %>%
              mutate(sector="E",
                     units="TWh",
                     param="localProduction",
                     localProduction=localProduction*3*runif(1))) %>%
  bind_rows(x %>%
              mutate(sector="Ag",
                     units="kg",
                     param="localProduction",
                     localProduction=localProduction*2*runif(1))); x1

cap <- tidyr::spread(a1 %>% filter(ioClass %in% c("cap")), key=ioClass, value=value);cap
cap <- cap %>% bind_rows(cap %>%
                       mutate(scenario="Scenario_Ref",
                              cap=cap*2*runif(1)));cap


cap1 <- cap %>%
  mutate(param="cap", units="m3") %>%
  bind_rows(cap %>%
              mutate(sector="E",
                     units="TWh",
                     param="cap",
                     cap=cap*2*runif(1))) %>%
  bind_rows(cap %>%
              mutate(sector="Ag",
                     units="kg",
                     param="cap",
                     cap=cap*1*runif(1))); cap1


A0=tibble::tribble( # Initial Flows
  ~sector ,    ~W,       ~E,     ~ Ag,
  "W"     ,    0,         0.25,    0.0025,
  "E"     ,    0.002,         0,   0.0005,
  "Ag"    ,    0,         0.005,    0);A0

a1<- z1 %>% dplyr::select(-c(unique(z1$sector))) %>% unique() %>%
  dplyr::left_join(A0,by=c("sector")); a1

A0=a1;
X0=x1;
ZPartial=z1
Cap0=cap1

io<-metis.io(A0=A0,X0=X0,nameAppend = "_test")



