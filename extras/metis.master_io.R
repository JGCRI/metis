
#----------------------------
# Install necessary packages
#----------------------------
if("devtools" %in% rownames(installed.packages()) == F){install.packages("devtools")}
library(devtools)
if("metis" %in% rownames(installed.packages()) == F){install_github(repo="zarrarkhan/metis")}
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


io<-met.io(Z0=Z0,X0=X0,D=c(200,100))
io$A
io$L

#Problem 2.1b
(diag(nrow(io$ioTbl)) + io$A + io$A%*%io$A + io$A%*%io$A%*%io$A + io$A%*%io$A%*%io$A%*%io$A)%*%as.matrix(io$D)
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


io<-met.io(Z0=Z0,X0=X0,D=c(1300,100,200))
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


io<-met.io(Z0=Z0,X0=X0,D=c(110,50,100))
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


io<-met.io(Z0=Z0,X0=X0,D=c(35*0.75,40*0.9,25*0.95))
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


io<-met.io(Z0=Z0,D0=D0,D=c(15,18))
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


io<-met.io(Z0=Z0,X0=X0)
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


io<-met.io(Z0=Z0,X0=X0)
io$A
io$L


# --------------------------
# metis.irio (Interregional)
# --------------------------


#----------------------------
# Install necessary packages
#----------------------------
if("devtools" %in% rownames(installed.packages()) == F){install.packages("devtools")}
library(devtools)
if("metis" %in% rownames(installed.packages()) == F){install_github(repo="zarrarkhan/metis")}
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
  ~sector ,    ~r1,         ~r2, ~r3, ~s1, ~s2, ~s3,
  "ag"     ,    500,         350,
  "man"     ,    320,        360);Z0


X0=tibble::tribble( # Initial total demand
  ~total,
  1000,
  800
);X0


io<-met.io(Z0=Z0,X0=X0,D=c(200,100))
io$A
io$L


