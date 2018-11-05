

# Table of Contents

* [Introduction](#Introduction)
* [Work Plan](#WorkPlan)
* [Modeling Framework](#ModelingFramework)


# <a name="Introduction"></a>Introduction

The overall goal of the SRN model is develop a framework to analyze dynamics across multiple sectors including water, energy, land and socio-economics at various spatial and temporal scales.Key features and additional guides for the model are listed below:

- The Sub-Regional Nexus (SRN) modeling development platform is hosted on github at: https://github.com/zarrarkhan/srn
- The address to clone this repo is: https://github.com/zarrarkhan/srn.git
- The model is comprised of an R package and additional files with sample data
- The R package can be installed from within R from this github site using the following code:

```r
# Check and load required packages
if("devtools" %in% rownames(installed.packages()) == F){install.packages("devtools")}
library(devtools)
if("devtools" %in% rownames(installed.packages()) == F){install_github(repo="zarrarkhan/srn")} 
library(srn)
```  

- Once installed the "srn.master.R"" file can be opened from the downloaded diretory to start analysis
- The following guides are available for further guidance:
    i. This README.md file at https://github.com/zarrarkhan/srn 
    ii. The R package user manual "srn.pdf" in the home directory "srn" after cloning or downloading. This is automatically created during the package build and contains an index of all the functions with brief descriptions and dependencies.
    iii. The vignette inside srn/doc/srn.vignette.html. The vignette examples for each function.

# <a name="WorkPlan"></a>Work Plan

The work plan is divided into two phases as described in Table 1 and Figure 1 below. The plan for Phase 1 is to develop a multi-sector accounting tool which can be used to visualize and analyze the demands, supplies and distribution of various resources across multiple sectors at flexible sub-regional scales. The tools in this phase will be designed to be flexible and with an aim to allow interactions with multiple stakeholders and other models. In Phase 2 of the model development links between the different sectors and sub-regions will be developed to allow analyis of cross-sector dynamics. Further details on each Phase are described in the corresponding sectionss.  

<p align="center">
<b> Table 1: SRN Phases </b> 
</p>

| Phase |Components       |Description            |Proposed Deadline | Status  |
| :------:|:---------------:|:--------------------:|:-----------------:|:--------:|
| 1     | srn.readgcam.R  | Read and format GCAM data | 7 Dec 2018       | Under development |
| 1     | srn.chart.R     |  Produce charts and diff plots | 7 Dec 2018  |Under development |
| 1     | srn.grid2poly.R |   Create polygon tables from grid | 7 Dec 2018 | Under development |
| 1     | srn.maps.R      |   Create maps | 7 Dec 2018 | Under development |

<p align="center"> <b> Figure 1: SRN Phases </b> </p>
<p align="center"> <img src="READMEfigs/srn_workflowLongTerm.png" alt="SRN Phases"></p>

<p align="center"> **Figure 2: SRN Phase 1 Structure** </p>
<p align="center"> <img src="READMEfigs/srn_workflowPhase1.png" alt="SRN Phases"></p>

# <a name="ModelingFramework"></a> Phase 1 Modeling Framework

This section describes the modeling framework and key functions used in Phase 1 of the model. For a full list of functions please read the user manual "srn.pdf".

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

# Use function localDBConn from package rgcam to get a list of scenarios if needed.
localDBConn(gcamdatabase)

gcamData<-srn.readgcam(gcamdatabase=paste(getwd(),"/inputs/gcam/example_database_basexdb",sep=""),
             queryxml=paste(getwd(),"/inputs/gcam/srnQueries.xml",sep=""),
             scenOrigNames=c("example_Reference1","example_Reference2"),
             scenNewNames=c("Ref1","Ref2"),
             reReadData=T, # Default Value
             dataProj=NULL, # Default Value
             dirOutputs= paste(getwd(),"/outputs",sep="") # Default Value
             )
```  
