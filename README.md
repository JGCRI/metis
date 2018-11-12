

# Table of Contents

* [Introduction](#Introduction)
* [Installation Guide](#InstallGuide)
* [Step-by-step Phase 1 walkthrough](#Phase1WalkThrough)


# <a name="Introduction"></a>Introduction

The overall goal of the SRN model is develop a framework to analyze dynamics across multiple sectors including water, energy, land and socio-economics at various spatial and temporal scales. 

The model development can be divided into two main phases as shown in Figure 1 below. The plan for Phase 1 is to develop a multi-sector accounting tool which can be used to visualize and analyze demands, supplies and distribution of various resources across multiple sectors at flexible sub-regional scales. The tools in this phase will be designed to be flexible and with an aim to allow interactions with multiple stakeholders and other models. In Phase 2 of the model development links between the different sectors and sub-regions will be developed to allow analyis of cross-sector dynamics. Further details on each Phase are described in the corresponding sectionss.  

- The Sub-Regional Nexus (SRN) modeling development platform is hosted on github at: https://github.com/zarrarkhan/srn
- The address to clone this repo is: https://github.com/zarrarkhan/srn.git
- The model is comprised of an R package and additional files with sample data

<p align="center"> <b> Figure 1: SRN Phases </b> </p>
<p align="center"> <img src="READMEfigs/srn_workflowLongTerm.png" alt="SRN Phases"></p>


# <a name="InstallGuide"></a>Installation Guide

1. Clone the repo from github:

```r
# Download a git software (eg. https://git-scm.com/downloads)
# Clone repo
git clone https://github.com/zarrarkhan/srn.git

# Or download from github https://github.com/zarrarkhan/srn 
```  

2. Open the "srn.master.R"" file which contains code to help install and run remaining model.


2. Install the R package - SRN (This code is also part of the "srn.master.R"" file):

```r
# Check and load required packages (devtools, srn, rgcam)
if("devtools" %in% rownames(installed.packages()) == F){install.packages("devtools")}
library(devtools)
if("srn" %in% rownames(installed.packages()) == F){install_github(repo="zarrarkhan/srn")}
library(srn)
if("rgcam" %in% rownames(installed.packages()) == F){install_github(repo="JGCRI/rgcam")}
library(rgcam)
```  

# <a name="Phase1WalkThrough"></a> Step-by-step Phase 1 Walkthrough

## Phase 1 Modeling Framework

The modeling framework and key functions used in Phase 1 of the model are shown in Figure 2.

<p align="center"> <b> Figure 2: SRN Phase 1 Structure </b> </p>
<p align="center"> <img src="READMEfigs/srn_workflowPhase1.png" alt="SRN Phases"></p>


## Reading GCAM data (srn.readgcam)

<b> Key Inputs </b>  

- GCAM database: A gcam database
- Query File: An ".xml" query file

GCAM produces an output in the form of a database. The database contains outputs from various scenario runs. "srn.readgcam" uses another package "rgcam" to connects with the gcam database and retrieves data based on "queries" provided in an ".xml" file. Often scenario names in the model can be long and not appropriate for final figures. This function allows you to rename the scenarios as they are read in. Once the data has been extracted from a gcam database it is saved in a ".proj" file. Reading data from the gcam database can take a considerable amount of time depending on the number of scenarios it contains. The "srn.readgcam" function gives the option of directly providing a ".proj" which can be loaded directly or using the ".proj" file from a previous run by setting the parameter "reReadData" to FALSE. If "reReadData" is set to FALSE the function will first search for a user provided ".proj" file (which is entered in the param "dataProj"), then in the "inputs/gcam" folder and if it doesn't exist will give an error message. If "reReadData" is set to TRUE then the function will create a file called "dataProj.proj" in the same folder as the GCAM database.

```r
# Check and load required packages
if("devtools" %in% rownames(installed.packages()) == F){install.packages("devtools")}
library(devtools)
if("devtools" %in% rownames(installed.packages()) == F){install_github(repo="zarrarkhan/srn")} 
library(srn)

gcamdatabasePath <-paste(getwd(),"/inputs/gcam",sep="")
gcamdatabaseName <-"example_database_basexdb"

# Use function localDBConn from package rgcam to get a list of scenarios if needed.
localDBConn(gcamdatabasePath,gcamdatabaseName)

gcamData<-srn.readgcam(gcamdatabasePath=gcamdatabasePath,
                       gcamdatabaseName=gcamdatabaseName,
                       queryxml=paste(getwd(),"/inputs/gcam/srnQueries.xml",sep=""),
                       scenOrigNames=c("ExampleScen1","ExampleScen2"),
                       scenNewNames=c("Eg1","Eg2"),
                       reReadData=T, # Default Value
                       dataProj=NULL, # Default Value
                       dirOutputs= paste(getwd(),"/outputs",sep="") # Default Value
                       )
```  
