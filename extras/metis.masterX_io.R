
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




#-------------
# Workflow for Metis I/O Analysis

# Small Example
Z0=tibble::tribble( # Initial Flows
  ~sector ,    ~W,         ~E,
  "W"     ,    0,           50,
  "E"     ,    20,          0);Z0


A0=tibble::tribble( # Initial Flows
  ~sector ,    ~W,         ~E,
  "W"     ,    0,           0.23,
  "E"     ,    0.13,          0);A0


D0=tibble::tribble( # Initial total demand
  ~sector, ~domestic,
      "W",    100,
      "E",    200
);D0

X0=tibble::tribble( # Initial total demand
  ~sector, ~processed,
  "W",    1000,
  "E",    2000
);X0

Cap0=tibble::tribble( # Initial total demand
  ~sector, ~cap,
  "W",    500,
  "E",    4000
);Cap0


Trade0=tibble::tribble( # Initial total demand
  ~sector, ~trade,
  "W",    0,
  "E",    0
);Trade0

DNew=tibble::tribble( # Initial processed demand
  ~sector, ~domestic,
  "W",    150,
  "E",    250
);DNew

ANew=tibble::tribble( # Initial Flows
  ~sector ,    ~W,         ~E,
  "W"     ,    0,           0.4,
  "E"     ,    0.2,          0);ANew

ZNew=tibble::tribble( # Initial Flows
  ~sector ,    ~W,         ~E,
  "W"     ,    0,           500,
  "E"     ,    20,          0);ZNew

XNew=tibble::tribble( # Initial processed demand
  ~sector, ~processed,
  "W",    300,
  "E",    500
);XNew

A0i=A0;
D0i=D0;
Cap0i=Cap0;
Trade0i=Trade0


io1<-metis.io(A0i=A0,D0i=D0, Cap0i=Cap0, Trade0i=Trade0)
io2<-metis.io(A0=A0,X0=X0, Cap0=Cap0)
io3<-metis.io(Z0=Z0,D0=D0, Cap0=Cap0)
io4<-metis.io(Z0=Z0,X0=X0, Cap0=Cap0)
io5<-metis.io(X0=X0,Z0=Z0,ANew=ANew, DNew=DNew, XNew=XNew, ZNew=ZNew, Cap0=Cap0)
#io2<-metis.io(D0=D0,X0=X0, Cap0=Cap0)
io$A_Orig
io$L_Orig


install.packages("corrplot")
library(corrplot)
install.packages("gganimate")
library(gganimate)
install.packages("gifski")
library(gifski)
# https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html

sol<-io$sol_Orig %>%
  dplyr::select(sector, io$sol_Orig$sector, internal, local, processed, cap, surplus, trade, domestic)


Mn <- as.matrix(sol %>% dplyr::select(-sector) %>%
  mutate_all(funs(./processed))); Mn
rownames(Mn)<-sol$sector;Mn
M <-as.matrix(sol%>%dplyr::select(-sector));M
rownames(M)<-sol$sector;M
A <-as.matrix(io$A_Orig);A
sol$sector->rownames(A)->colnames(A);A
#col<- colorRampPalette(metis.colors()$pal_div_wet)(20)
corrplot(Mn, method="circle", is.corr=F, type="full",addCoef.col="red")
mtext("Normalized Flows", side=1, line=0, cex=1)
corrplot(M, method="number", is.corr=F, type="full",col="black",cl.pos = "n")
mtext("Flow Values", side=1, line=0, cex=1)
corrplot(A, method="circle", is.corr=F, type="full",addCoef.col="red")
mtext("Normalized Coefficients", side=1, line=0, cex=1)
corrplot(A, method="number", is.corr=F, type="full",col="black",cl.pos = "n")
mtext("Absolute Coefficients", side=1, line=0, cex=1)


df_Mn<-sol %>%
  select (-processed,processed) %>% # to place processed last for following function
  mutate_at(vars(-sector),funs(./processed)); df_Mn

df_Mn1a <- df_Mn %>%
  gather(-sector,key="sectorTo",value="value") %>%
  rename (sectorFrom=sector) %>%
  arrange(sectorFrom)  %>%
  mutate(subRegion="Sub-Region A",
         value=value*1.3,
         year=2009); df_Mn1a

df_Mn1b <- df_Mn %>%
  gather(-sector,key="sectorTo",value="value") %>%
  rename (sectorFrom=sector) %>%
  arrange(sectorFrom)  %>%
  mutate(subRegion="Sub-Region A",
         value=value*1.7,
         year=2010); df_Mn1b

df_Mn2a <- df_Mn %>%
  gather(-sector,key="sectorTo",value="value") %>%
  rename (sectorFrom=sector) %>%
  arrange(sectorFrom)  %>%
  mutate(subRegion="Sub-Region B",
         value=value*2,
         year=2009); df_Mn2a

df_Mn2b <- df_Mn %>%
  gather(-sector,key="sectorTo",value="value") %>%
  rename (sectorFrom=sector) %>%
  arrange(sectorFrom)  %>%
  mutate(subRegion="Sub-Region B",
         value=value*0.3,
         year=2010); df_Mn2b

df_Mnx <- bind_rows(df_Mn1a,df_Mn1b,df_Mn2a,df_Mn2b); df_Mnx

ga <- ggplot(df_Mnx, aes(y = sectorFrom, x = sectorTo)) +
  labs(subtitle="test",
       title="Bubble chart") +
  scale_x_discrete(expand = c(0.1,0.1)) +
  geom_point(aes(col=value, size=value)) +
  geom_text(aes(label=round(value,1)),col="red") +
  coord_fixed(ratio = 1) +
  scale_size_continuous(range = c(1,30)) +
  facet_grid(year~subRegion); ga

gb <- ggplot(df_Mnx, aes(y = sectorFrom, x = sectorTo)) +
  labs(subtitle="test",
       title="Bubble chart") +
  scale_x_discrete(limits = c(unique(df_Mn1$sectorFrom),"intermediateOutput", "processed"), expand = c(0.1,0.1)) +
  geom_text(aes(label=round(value,1)),col="black") +
  coord_fixed(ratio = 1) +
  scale_size_continuous(range = c(1,30)) +
  facet_grid(year~subRegion)+theme_bw(); gb


g1 <- ggplot(df_Mnx, aes(y = sectorFrom, x = sectorTo)) +
  labs(subtitle="test",
       title="Bubble chart") +
  scale_x_discrete(limits = c(unique(df_Mn1$sectorFrom),"intermediateOutput", "processed"), expand = c(0.1,0.1)) +
  geom_point(aes(col=value, size=value)) +
  coord_fixed(ratio = 1) +
  scale_size_continuous(range = c(1,30)) +
  facet_wrap(~subRegion) +
  labs(title = 'Year: {frame_time}') +
  transition_time(year,range=c(2009,2010))

animate(g1,nframes = length(unique(df_Mnx$year)),fps=0.5)

# https://cran.r-project.org/web/packages/ggalluvial/vignettes/ggalluvial.html
install.packages("ggalluvial")
library(ggalluvial)
library(tidyr)

solFlows <- io$sol_Orig %>%
  dplyr::select(sector, io$sol_Orig$sector,local,trade)
df <- solFlows;df


df1a <- df %>%
  gather(-sector,key="sectorTo",value="value") %>%
  rename (sectorFrom=sector) %>%
  arrange(sectorFrom)  %>%
  mutate(subRegion="Sub-Region A",
         year=2009,
         value=value*1); df1a

df1b <- df %>%
  gather(-sector,key="sectorTo",value="value") %>%
  rename (sectorFrom=sector) %>%
  arrange(sectorFrom)  %>%
  mutate(subRegion="Sub-Region A",
         year=2010,
         value=value*2); df1b

df2a <- df %>%
  gather(-sector,key="sectorTo",value="value") %>%
  rename (sectorFrom=sector) %>%
  arrange(sectorFrom)  %>%
  mutate(subRegion="Sub-Region B",
         year=2009,
         value=value*1.5); df2a

df2b <- df %>%
  gather(-sector,key="sectorTo",value="value") %>%
  rename (sectorFrom=sector) %>%
  arrange(sectorFrom)  %>%
  mutate(subRegion="Sub-Region B",
         year=2010,
         value=value*0.3); df2b

dfx <- bind_rows(df1a,df2a,df1b,df2b);dfx
dfx<-dfx%>%mutate(Trade=sectorTo);dfx


g<-ggplot(as.data.frame(dfx%>%filter(value!=0)),
       aes(y = value, axis1 = sectorFrom, axis2 = sectorTo, axis3=Trade,group=subRegion)) +
  geom_alluvium(aes(fill = sectorFrom), width = 1/12, color="black") +
  geom_stratum(width = 1/12, fill = "black", color = "grey", alpha=0.7) +
  geom_label(stat = "stratum", label.strata = TRUE) +
  scale_x_discrete(limits = c("From", "To"), expand = c(.05, .05)) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  facet_grid(year~subRegion) +
  ggtitle("Title")+theme_bw();g



# https://gjabel.wordpress.com/2014/03/28/circular-migration-flow-plots-in-r/



#--------------------------------
# Real Example With metis Outputs
#--------------------------------

library(dplyr);library(magrittr)library(tidyr)

# Tethys (Water Demands)
#Water_E
#Water_Ag
#Water_domestic
#Water_processed

# Demeter + GCAM (Ag Demands)
#Ag_processed # Ag All

# GCAM (Elec Demands)
#E_processed # Buildings, Industry, Transport


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
                              ioClass=c("domestic","E","Ag","domestic","domestic","domestic","processed","cap"),
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

x <- tidyr::spread(a1 %>% filter(ioClass %in% c("processed")), key=ioClass, value=value);x
x <- x %>% bind_rows(x %>%
                       mutate(scenario="Scenario_Impact",
                                       processed=processed*2*runif(1)));x

x1 <- x %>%
  mutate(param="processed", units="m3") %>%
  bind_rows(x %>%
              mutate(sector="E",
                     units="TWh",
                     param="processed",
                     processed=processed*3*runif(1))) %>%
  bind_rows(x %>%
              mutate(sector="Ag",
                     units="kg",
                     param="processed",
                     processed=processed*2*runif(1))); x1

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

a1<- z1 %>% select(-c(unique(z1$sector))) %>% unique() %>%
  dplyr::left_join(A0,by=c("sector")); a1

A0i=a1;
X0i=x1;
ZPartiali=z1
Cap0i=cap1

io<-metis.io(A0i=A0i,X0i=X0i,ZPartiali=ZPartiali)
io$A_Orig

z1

ZPartial=tibble::tribble( # Known Flows
  ~sector ,    ~W,       ~E,     ~ Ag,
  "W"     ,    NA,         NA,       20,
  "E"     ,    NA,         NA,      NA,
  "Ag"    ,    NA,         NA,      NA);ZPartial


ioCal<-metis.io(A0=A0,ZPartial=ZPartial,X0=X0)
ioCal$sol_Orig
ioCal$sol_ZPartial

# Problem 2.1
Z0=tibble::tribble( # Initial Flows
  ~sector ,    ~Wgw,      ~Ws, ~ W,   ~Ebio, ~ Esol, ~ E,
  "Wgw"   ,    0,         0,    0.5,  0,         0,    0,
  "Ws"    ,    0,         0,    0.5,  0,         0,    0,
  "W"     ,    0,         0,    0,    0.5,       0.5,   0,
  "Ebio"  ,    0,         0,    0,    0,         0,    0.3,
  "Esol"  ,    0,         0,    0,    0,         0,    0.7,
  "E"     ,    0,         0,    0.1,    0,         0,    0);Z0

A0=tibble::tribble( # Initial Flows
  ~sector ,    ~Wgw,      ~Ws, ~ W,   ~Ebio, ~ Esol, ~ E,
  "Wgw"   ,    0,         0,    0.5,  0,         0,    0,
  "Ws"    ,    0,         0,    0.5,  0,         0,    0,
  "W"     ,    0,         0,    0,    1,         1,    0,
  "Ebio"  ,    0,         0,    0,    0,         0,    1,
  "Esol"  ,    0,         0,    0,    0,         0,    0,
  "E"     ,    0,         0,    10,   0,         0,    0);A0

D0=tibble::tribble( # Initial processed demand
  ~processed,
  0,
  0,
  1,
  0,
  0,
  1
);D0


io<-metis.io(Z0=Z0,D0=D0,D=c(0,0,1,0,0,0))
io$A
io$L

# Commodities
# 1. Water demands
# 2. Agricultural production by Crop
# 3. Electricity Production by Type

# Downscaled Outputs:
# 1. Ag production by crop
# 2. Elec production by fuel
# 3. Water demands by ag, elec, domestic

# Initial Coefficient Assumptions
# A0

# Steps
# 1. Use A0 and D0 (Demand domestic) to find Intermediate flows and processed production
# 2. Compare Aggregated demands to initial assumption
# 3. Adjust A to reflect actual data
# 4.


A0=tibble::tribble( # Initial Flows
  ~sector     ,    ~W,   ~Ag_corn,  ~Ag_rice, ~E_coal, ~E_solar, ~E,
  "W"         ,    0,       0.1,     2,     0.5,    0.01,   0,
  "Ag_corn"     ,    0,        0,      0,     0,      0,      0,
  "Ag_rice"     ,    0,        0,      0,     0,      0,      0,
  "E_coal"     ,    0,        0,      0,     0,      0,      0,
  "E_solar"    ,    0,        0,      0,     0,      0,      0,
  "E"         ,    1,      0.3,    0.4,   0,      0,      0);A0

Dreal = tibble::tribble( # From tethys
  ~sector, ~W,
  "Ag"   ,  1000,
  "E"    ,    20);Dreal



D0=tibble::tribble( # domestic demands (Not internal flows)
  ~domestic,
  1000, # W
  10,  # Acorn
  10,  # Arice
  50, # Ecoal
  50,   # Esolar
  100 # E
);D0


io<-metis.io(D0=D0,A0=A0, D=c(10,0,0,0,1,1))
io$A
io$L

install.packages("corrplot")
library(corrplot)
M<-as.matrix(A0%>%dplyr::select(-sector))
rownames(M)<-colnames(M);M
col<- colorRampPalette(metis.colors()$pal_div_wet)(20)
corrplot(M, method="circle", is.corr=F, type="upper",addCoef.col="red", col=col, add=F, cl.length=20, cl.lim=c(0,2))

