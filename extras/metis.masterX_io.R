
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


#----------------------------
# Read GCAM Data
#---------------------------

# Blair and Miller Problems Chapter 2 - Simple I/O

# Problem 2.1
Z0=tibble::tribble( # Initial Flows
  ~sector ,    ~ag,         ~man,
  "ag"     ,    500,         350,
  "man"     ,    320,        360);Z0


X0=tibble::tribble( # Initial total demand
  ~total,
  1000,
  800
);X0


io<-metis.io(Z0=Z0,X0=X0,D=c(200,100))
io$A
io$L

#Problem 2.1b
#(diag(nrow(io$ioTbl)) + io$A + io$A%*%io$A + io$A%*%io$A%*%io$A + io$A%*%io$A%*%io$A%*%io$A)%*%as.matrix(io$D)
#i. [794.9863,613.7669]
#ii. [1139,844]

#Problem 2.2
Z0=tibble::tribble( # Initial Flows
  ~sector ,    ~S1,         ~S2, ~S3,
  "S1"     ,    350,         0, 0,
  "S2"     ,    50,        250, 150,
  "S3"     ,    200,       150, 550);Z0


X0=tibble::tribble( # Initial total demand
  ~total,
  1000,
  500,
  1000
);X0


io<-metis.io(Z0=Z0,X0=X0,D=c(1300,100,200))
io$A
io$L


#Problem 2.3
Z0=tibble::tribble( # Initial Flows
  ~sector ,    ~ag,         ~man, ~hhc,
  "ag"     ,    500,         350, 90,
  "man"     ,    320,        360, 50,
  "hhc"    ,     100,         60, 40);Z0


X0=tibble::tribble( # Initial total demand
  ~total,
  1000,
  800,
  300
);X0


io<-metis.io(Z0=Z0,X0=X0,D=c(110,50,100))
io$A
io$L

# Problem 2.4
Z0=tibble::tribble( # Initial Flows
  ~sector ,    ~lum,         ~pap, ~mach,
  "lum"     ,    50*0.05,     50*0.2, 50*0.05,
  "pap"     ,    50*0.05,        50*0.1, 50*0.05,
  "mach"    ,     100*0.3,         100*0.3, 100*0.15);Z0


X0=tibble::tribble( # Initial total demand
  ~total,
  50,
  50,
  100
);X0


io<-metis.io(Z0=Z0,X0=X0,D=c(35*0.75,40*0.9,25*0.95))
io$A
io$L

# Problem 2.5
Z0=tibble::tribble( # Initial Flows
  ~sector ,    ~A,         ~B,
  "A"     ,    2,         8,
  "B"     ,    6,        4);Z0


D0=tibble::tribble( # Initial total demand
  ~external,
  20,
  20
);D0


io<-metis.io(Z0=Z0,D0=D0,D=c(15,18))
io$A
io$L

# Problem 2.6
Z0=tibble::tribble( # Initial Flows
  ~sector ,    ~A,         ~B,
  "A"     ,    6,         2,
  "B"     ,    4,        2);Z0


X0=tibble::tribble( # Initial total demand
  ~total,
  20,
  15
);X0


io<-metis.io(Z0=Z0,X0=X0)
io$A
io$L

# Problem 2.11
Z0=tibble::tribble( # Initial Flows
  ~sector ,    ~ag,         ~sv, ~comp,
  "ag"     ,    2,         2, 1,
  "sv"     ,    1,         0, 0,
  "comp"   ,    2,         0, 1);Z0


X0=tibble::tribble( # Initial total demand
  ~total,
  5,
  2,
  2
);X0


io<-metis.io(Z0=Z0,X0=X0)
io$A
io$L


#----------------------------
# Inter-regional irio
# Blair and Miller Problems Chapter 3 - Simple I/O
#---------------------------


# Example
Z0=tibble::tribble( # Initial Flows
  ~sector ,    ~r1,         ~r2, ~r3, ~s1, ~s2,
  "r1",    150,        500, 50, 25, 75,
  "r2",    200,        100, 400, 200, 100,
  "r3",   300, 500, 50, 60, 40,
  "s1",  75, 100, 60, 200, 250,
  "s2", 50, 25, 25, 150, 100
  );Z0

X0=tibble::tribble( # Initial total demand
  ~total,
  1000,
  2000,
  1000,
  1200,
  800
);X0

irio<-metis.irio(Z0=Z0,X0=X0, D=c(100,0,0,0,0))

# Problem 3.2
Z0=tibble::tribble( # Initial Flows
  ~sector ,    ~r1,  ~r2,  ~s1, ~s2,
  "r1",    40,50,  30, 45,
  "r2",    60,10,  70, 45,
  "s1",  50, 60, 50, 80,
  "s2", 70, 70, 50, 50
);Z0


D0=tibble::tribble( # Initial total demand
  ~total,
  200,
  200,
  300,
  400
);D0

irio<-metis.irio(Z0=Z0,D0=D0, D=c(280,360,0,0))

# Problem 3.3
Z0=tibble::tribble( # Initial Flows
  ~sector ,    ~r1,  ~r2,  ~s1, ~s2,
  "r1",    40,50,  0, 0,
  "r2",    60,10,  0, 0,
  "s1",  0, 0, 30, 45,
  "s2", 0, 0, 70, 45
);Z0

Q0=tibble::tribble( # Initial Flows
  ~sector ,    ~r1,  ~r2,  ~s1, ~s2,
  "r1",    50,0,  60, 0,
  "r2",    0,50,  0, 80,
  "s1",  70, 0, 70, 0,
  "s2", 0, 50, 0, 50
);Q0

D0=tibble::tribble( # Initial total demand
  ~total,
  200,
  200,
  300,
  400
);D0

irio<-metis.irio(Z0=Z0,D0=D0, D=c(280,360,0,0))



#-------------
# Workflow for Metis I/O Analysis

# Small Example

# Problem 2.1
Z0=tibble::tribble( # Initial Flows
  ~sector ,    ~ag,         ~w,
  "ag"     ,    0,         0,
  "w"     ,    10,        0);Z0

A0=tibble::tribble( # Initial Flows
  ~sector ,    ~ag,         ~w,
  "ag"     ,    0,         0,
  "w"     ,    0.3,        0);A0


X0=tibble::tribble( # Initial total demand
  ~total,
  1000,
  1000
);X0


io<-metis.io(Z0=Z0,X0=X0,A0=A0,D=c(1,20))
io$A
io$L

# Commodities
# 1. Water demands
# 2. Agricultural production by Crop
# 3. Electricity Production by Type

# Downscaled Outputs:
# 1. Ag production by crop
# 2. Elec production by fuel
# 3. Water demands by ag, elec, other

# Initial Coefficient Assumptions
# A0

# Steps
# 1. Use A0 and D0 (Demand other) to find Intermediate flows and total production
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



D0=tibble::tribble( # Other demands (Not internal flows)
  ~other,
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

