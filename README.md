---
title: 'Metis 1.0'
tags:
  - sub-region
  - nexus
  - water
  - energy
  - land
authors:
  - name: Zarrar Khan
    affiliation: "1" # (Multiple affiliations must be quoted)
  - name: Thomas Bernard Wild
    affiliation: "1,2"
  - name: Chris Vernon
    affiliation: "1"
  - name: Mohamad Hejazi
    affiliation: "1,2"
  - name: Leon Clarke
    affiliation: "1,2"
  - name: Fernando Miralles
    affiliation: "1,2"
affiliations:
 - name: Joint Global Change Research Institute (JGCRI) - PNNL
   index: 1
 - name: University of Maryland
   index: 2
date: January 2019
---
<p align="center"> <img src="READMEfigs/metisHeaderThin.PNG"></p>
<p align="center"> <img src="READMEfigs/pnnlUMDLogos.PNG"></p>
<p align="center"> <img src="READMEfigs/metisHeaderThick.PNG"></p>

<!-- ------------------------>
<!-- ------------------------>
# <a name="Contents"></a>Contents
<!-- ------------------------>
<!-- ------------------------>

- [Introduction](#Introduction)
- [Need](#Need)
- [Framework](#Framework)
- [Installation Guide](#InstallGuide)
- [Metis Step-by-step walkthrough](#WalkThrough) 
    + [metis.readgcam.R](#metis.readgcam.R)
    + [metis.chart.R](#metis.chart.R)
    + [metis.chartsProcess.R](#metis.chartsProcess.R)
    + [metis.map.R](#metis.map.R)
    + [metis.boundaries.R](#metis.boundaries.R)
    + [metis.grid2poly.R](#metis.grid2poly.R)
    + [metis.mapsProcess.R](#metis.mapsProcess.R)
    + [metis.io.R](#metis.io.R)
- [Under development](#underDevelopment) 
    + [metis.infra.R](#metis.infra.R)
  
<!-- ------------------------>
<!-- ------------------------>
# <a name="Introduction"></a>Introduction
<p align="center"> <img src="READMEfigs/metisHeaderThick.PNG"></p>
<!-- ------------------------>
<!-- ------------------------>

[Back to Contents](#Contents)

The Metis model is a joint effort by the Pacific Northwest National Laboratory (PNNL) and the University of Maryland (UMD). The name "Metis" is inspired by the the Greek Goddes of "good counsel, planning, cunning and wisdom". The overall goal of the Metis model is to develop a framework allowing analysis of dynamics across multiple sectors including water, energy, land and socio-economics at various spatial and temporal scales. The Metis model is designed to be a flexible tool to be used in conjunction with both global integrated assessment models (IAMs) which capture global dynamics as well as more detailed models which can capture specific sub-regional and sectoral details at finer resolutions. Metis integrates with these other tools by sharing data in standarized formats across the platforms and stakeholders. 

In most projects looking at multi-sector dynamics one of the key constraints is data availability and a key goal of Metis is to overcome this barrier by providing users with default data across sectors for their specific region. This default data comes from GCAM which operates at a relatively aggregated resolution when compared with sub-basins or states. The GCAM data is downscaled to 0.5 degree grids and offers users a first look at the water-energy-land situation in their regions of interest. The idea is to use this as a starting point and then improve the data as and when it becomes available while working with local stakeholders. The advantage of this system is that even without more precise local data the baseline data already permits users to review trends across the WEL nexus for various socio-economic and climate change pathways which will hold even if the underlying absolute data is adjusted.

<p align="center"> <b> Metis - A model to bridge the gap between coarse resolution IAMs and finer-resolution sector specific models </b> </p>
<p align="center"> <img src="READMEfigs/metis_MidSpatial.PNG"></p>

- The Metis modeling development platform is hosted on github at: https://github.com/JGCRI/metis
- To clone this repo is: https://github.com/JGCRI/metis.git
- The model is comprised of an R package and additional files with sample data


<!-- ------------------------>
<!-- ------------------------>
# <a name="Need"></a>Need
<p align="center"> <img src="READMEfigs/metisHeaderThick.PNG"></p>
<!-- ------------------------>
<!-- ------------------------>

[Back to Contents](#Contents)

The motivation behind developing the Metis model is to bridge the gap between models working at different spatial and temporal resolutions. Sector specific details such as diurnal electricity demand profiles and the availability of intermittent technologies are often captured by fine resolution sector specific models. These finer scale models are however often limited in scope to the specific sector in question. With increasing pressures on limited resources from growing populations, technological advances and a changing climate the links across human-earth systems are becoming more critical. These links include feedbacks across system such as water demands for powerplant cooling and hydropower; energy demands for water purification, transfers and distribution; both energy and water demands for agriculture production and harvesting; and the impacts of land-use change emissions and water demands as a result of biofuel expansion and deforestation. All of these are driven by socio-economic developments and policy decisions across these systems. Global integrateed assessment models are often used to capture these broader longterm dynamics on larger spatial and temporal scales. The development of the Metis model will address this need to capture both broader long-term dynamcis across systems and global markets as well as sub-regional details. The model will do this by taking global data and dynamcis from an IAM such as GCAM and then downscale the results to relevant sub-regional boundaries based on the needs of local stakeholders. The downscaled data will then be refined and calibrated with local sector specific data. This will be followed by calibration of the local inter-sectoral links and the model will then be used for future sector specific policy analysis at the relevant scales while still including global dynamics.


<!-- ------------------------>
<!-- ------------------------>
# <a name="Framework"></a> Framework
<p align="center"> <img src="READMEfigs/metisHeaderThick.PNG"></p>
<!-- ------------------------>
<!-- ------------------------>

[Back to Contents](#Contents)

Metis is designed to be accesible to a range of stakeholders with varying expertise and goals related to nexus analyses. The various functions of metis can be used independently for different purposes ranging from visualization, charting, spatial aggregation, mapping and inter-sectoral dynamics. The following lists and figure provides a summary of the existing and planned capabilities of metis. The lists and figure are updated as progress is made and new developments are planned. 

<p align="left"> <b> Metis Module & Functions </b> </p>

- Charts: Various functions to produce charts to compare outputs across regions and scenarios (functions: metis.chart.R, metis.chartsProcess.R)
- Maps: Various functions to vizualize spatial boundaries and data as rasters and polygon
(functions: metis.boundaries.R, mets.map.R, metis.mapsProcess.R)
- Spatial Aggregation: Functions to aggregate gridded data to different spatial boundaries.
(functions: metis.grid2poly.R)
- Data preparation: Functions to prepare data from other modules into the appropriate format.
(functions: metis.readgcam.R, metis.prepGrid.R)
- Input/Output (IO) Analysis: Functions to build IO inter-sectoral tables and sankey diagrams.
(functions: metis.io.R)

<p align="left"> <b> Under Development </b> </p>

- Flows: Module to route water through upstream and downstream sub-regions.
- Infrastructure: Optimization module to find infrastructure development options to address supply-demand gaps. (functions: metis.infra.R)

<p align="center"> <img src="READMEfigs/metis_workflow1.png"></p>


<!-- ------------------------>
<!-- ------------------------>
# <a name="InstallGuide"></a>Installation Guide
<p align="center"> <img src="READMEfigs/metisHeaderThick.PNG"></p>
<!-- ------------------------>
<!-- ------------------------>

[Back to Contents](#Contents)

1. Clone the repo from github:

```r
# Download a git software (eg. https://git-scm.com/downloads)
# Clone repo
git clone https://github.com/JGCRI/metis.git

# Or download from github https://github.com/JGCRI/metis 
```  

2. Download R studio (https://www.rstudio.com/) and R (https://www.r-project.org/)

3. Open the metis.Rproj file.

4. Open the "metis.master.R"" file which contains code to help install and run remaining model.

5. Install the necessary R packages including Metis.

```r
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
```  

6. Download the relevant shapefiles (gis.zip) from Open Science Framework (OSF) at https://osf.io/pbta5/.
Save and unzip the file in ./metis/datafiles/gis.

7. After downloading and unzipping you should have the following folder structure with the following sub-folders in ./metis/datafiles:
- examples: Contains example shapefiles, as well as gridded and polygon data to work with
- gcam: Contains an example gcam run output
- gis: This is the unzipped osf folder (https://osf.io/pbta5/) and contains other shape files including sub-basins, gcam regions, gcam basins, naturalEarth boundaries and other country specific boundaries.
- mapping: This contains two files which are used to map color palettes and other parameters not provided by stakeholder tables.

<details><summary>Click here to expand for further details, code and example figures.</summary>
<p>

<p align="center"> <b> Metis Initial Folders </b> </p>
<p align="center"> <img src="READMEfigs/metis_initialFolders.PNG"></p>

</p>
</details>


<!-- ------------------------>
<!-- ------------------------>
# <a name="WalkThrough"></a> Metis Step-by-step Walkthrough
<p align="center"> <img src="READMEfigs/metisHeaderThick.PNG"></p>
<!-- ------------------------>
<!-- ------------------------>

[Back to Contents](#Contents)

This section walks through the different features of the metis package using the example data provided in order to familiarize the user with the different functions. All metis R functions are stored in ./metis/R. The key functions available are:

| Function  | Description |
| ------------- | ------------- |
| metis.readgcam.R  | This functions is designed to interact specifically with GCAM outputs. The function processes GCAM outputs into .csv files by GCAM region which can then be used as inputs to metis.chartsProcess.R |
| metis.chart.R  | metis charting function which allows quick and easy access to features like facets, labels and colors. The function is based on ggplot and returns a ggplot chart.  |
| metis.chartsProcess.R  | metis charting function used to compare scenarios and regions. The function also creates diff plots with percentage and absolute differences from a given reference scenario.  |
| metis.map.R  | metis mapping function to plot raster and polygon data. The function uses the tmap package and returns a tmap object. Several maps can be combined by overlaying and underlaying using this function. Options allow for different colors palettes, labels, text-size as well as legend breaks which are freescale, kmeans or equally divided to highlight different kinds of data.  |
| metis.boundaries.R  | metis mapping function to plot shape file boundaries and surrounding regions for quick visualization of region of interest.  |
| metis.grid2poly.R  | Function used to crop and aggregate gridded data by a given polygon shape file. If no grid is provided the function can still be used to produce regional and subregional maps  |
| metis.gridByPoly.R  | Function used to crop and aggregate gridded data by a given polygon shape file. If no grid is provided the function can still be used to produce regional and subregional maps  |
| metis.mapsProcess.R  | metis mapping function used to compare across scenarios. The function produces diff maps with percentage and absolute differences from a given reference scenario.  |
| metis.prepGrid.R  | This function is designed to be used with specific open-source downscaling models Xanthos, Demeter and Tethys which downscale GCAM data to the grid level. The function takes outputs from these various models and processes them into the metis format which is then used as an input to the metis.mapsProcess.R function.  |
| metis.io.R  | This functions is designed to interact specifically with GCAM outputs. The function processes GCAM outputs into .csv files by GCAM region which can then be used as inputs to metis.chartsProcess.R |
| metis.assumptions.R  | Contains all conversions and assumptions used in the model  |
| metis.colors.R  | Collection of metis color palettes. A list of palettes can be viewed in the function help file (?metis.colors). To view a particular palette metis.colors("pal_hot")  |


<details><summary>Click here to expand for further details, code and example figures.</summary>
<p>

<p align="center"> <b> Metis Functions </b> </p>
<p align="center"> <img src="READMEfigs/metis_functions.PNG"></p>


The workflow for data processing, charting and mapping is shown below. The following subsections will guide the user through each of these processes further expaning on the functionality within each of the functions.

<p align="center"> <b> Metis chart and mapping processes </b> </p>
<p align="center"> <img src="READMEfigs/metis_workflowChart.png"></p>
<p align="center"> <img src="READMEfigs/metis_workflowMap.png"></p>

</p>
</details>


<!-- ------------------------>
<!-- ------------------------>
## <a name="metis.readgcam.R"></a> metis.readgcam.R
<p align="center"> <img src="READMEfigs/metisHeaderThick.PNG"></p>
<!-- ------------------------>
<!-- ------------------------>

[Back to Contents](#Contents)


<b> Key Inputs </b>  

- GCAM data: A gcamdatabase OR a gcamdata ".proj"" file (with complete paths)
- Query File: An ".xml" query file (with complete paths)


The model comes with an example gcamdatabase ".proj" file called "Example_dataProj.proj" which lies in the folder metis/dataFiles/gcam/ as shown in the figure below. It is recommended that other gcamdatabases are also kept in this folder. GCAM produces an output in the form of a database. The database contains outputs from various scenario runs. "metis.readgcam" uses another package "rgcam" to connects with the gcam database and retrieves data based on "queries" provided in an ".xml" file. Often scenario names in the model can be long and not appropriate for final figures. This function allows you to rename the scenarios as they are read in. Once the data has been extracted from a gcam database it is saved in a ".proj" file. Reading data from the gcam database can take a considerable amount of time depending on the number of scenarios it contains. The "metis.readgcam" function gives the option of directly providing a ".proj" which can be loaded directly or using the ".proj" file from a previous run by setting the parameter "reReadData" to FALSE. If "reReadData" is set to FALSE the function will first search for a user provided ".proj" file (which is entered in the param "dataProj"), then in the "inputs/gcam" folder and if it doesn't exist will give an error message. If "reReadData" is set to TRUE then the function will create a file called "dataProj.proj" in the same folder as the GCAM database. The code below shows how to read in data using the database or .proj file. The user can choose the regions of interest.

<details><summary>Click here to expand for further details, code and example figures.</summary>
<p>

<p align="center"> <b> GCAM data </b> </p>
<p align="center"> <img src="READMEfigs/metis_moduleGCAM1.png"></p>


```r
#----------------------------
# Read GCAM Data (metis.readgcam.R)
#---------------------------

# Connect to gcam database or project
  # gcamdatabasePath_i <-paste(getwd(),"/dataFiles/gcam",sep="") # Use if gcamdatabase is needed
  # gcamdatabaseName_i <-"example_database_basexdb" # Use if gcamdatabse is needed
  dataProjPath_i <- paste(getwd(),"/dataFiles/gcam",sep="") # Path to dataProj file.
  dataProj_i <-"Example_dataProj.proj"  # Use if gcamdata has been saved as .proj file

# Get list of scenarios and rename if desired.
  # rgcam::localDBConn(gcamdatabasePath,gcamdatabaseName) # if connecting directly to gcam database
  dataProjLoaded <- loadProject(paste(dataProjPath_i, "/",dataProj_i , sep = ""))
  listScenarios(dataProjLoaded)  # List of Scenarios in GCAM database
  scenOrigNames_i = c("exampleScen1","ExampleScen2")
  scenNewNames_i = c("Eg1","Eg2")  # These are the names that will be used in figures

# Choose Parameters or set to "All" for all params. For complete list see ?metis.readgcam
  paramsSelect_i = "All"

# Select regions from the 32 GCAM regions.
  regionsSelect_i <- c("Colombia","Argentina")

  dataGCAM<-metis.readgcam(reReadData = T,
                         #gcamdatabasePath = NULL,
                         #gcamdatabaseName = NULL,
                         scenOrigNames = scenOrigNames_i,
                         scenNewNames = scenNewNames_i,
                         dataProj = dataProj_i,
                         dataProjPath = dataProjPath_i,
                         regionsSelect = regionsSelect_i ,
                         paramsSelect=paramsSelect_i
                       )

  dataGCAM$data # To view the data read that was read.
                       
```  

The function metis.readgcam() returns a list which contains a dataframe with the values it read from the database. In this case it is saved in the variable dataGCAM. The function also creates a new folder "outputs" and stores GCAM data tables in this folder. The function will save the data as a .csv file for each region and one .csv for all regions. Tables with "aggClass" aggregate data across classes to provided sector summaries. In addition the function will produce an empty template data table for each region to be shared with local stakeholders so they can fill in their data. If not specified then all these tables will by default go to a folder called ./metis/outputs/readGCAMTables as shown in the figure below. Once the empty template tables are filled with appropriate local data these can be saved in the folder outputs/Tables/Tables_Local. 

<p align="center"> <b> GCAM data tables</b> </p>
<p align="center"> <img src="READMEfigs/metis_moduleGCAM2.png"></p>


</p>
</details>


<!-- ------------------------>
<!-- ------------------------>
## <a name="metis.chart.R"></a> metis.chart.R
<p align="center"> <img src="READMEfigs/metisHeaderThick.PNG"></p>
<!-- ------------------------>
<!-- ------------------------>

[Back to Contents](#Contents)

metis.chart.R is the metis charting software used in metis.chartsProcess.R. It allows users to create line, bar, bubble and sankey charts. The default settings maintain a conistent look across the metis products.


<details><summary>Click here to expand for further details, code and example figures.</summary>
<p>

The following section shows some basic example to use metis.chart.R.

```r

# Simple example with progressively more features
   tbl <- tribble (
   ~x,     ~value,
   2010,   15,
   2020,   20,
   2030,   30
   )
   metis.chart(data = tbl, xData = "x", yData = "value", chartType = "line", fileName = "chart_eg_line")
   metis.chart(data = tbl, xData = "x", yData = "value", chartType = "bar", fileName = "chart_eg_bar")
   metis.chart(data = tbl, xData = "x", yData = "value", chartType = "bar", color = "blue",
               yLabel = "New y Label", xLabel = "New X label", fileName = "chart_eg_bar_blue", title = "Title")
   # See ?metis.chart for more details on further customization eg. tick marks, title size ect.

# More detailed data with facets
   # Simple example with progressively more features
   tbl_multi <- tribble (
     ~x,     ~value, ~region,     ~scen,   ~fuel,
     2010,   25,     "region1",   "scenA",  "Oil",
     2020,   30,     "region1",   "scenA",  "Oil",
     2030,   40,     "region1",   "scenA",  "Oil",
     2010,   25,     "region2",   "scenA",  "Oil",
     2020,   10,     "region2",   "scenA",  "Oil",
     2030,   60,     "region2",   "scenA",  "Oil",
     2010,   75,     "region1",   "scenB",  "Oil",
     2020,   30,     "region1",   "scenB",  "Oil",
     2030,   20,     "region1",   "scenB",  "Oil",
     2010,   25,     "region2",   "scenB",  "Oil",
     2020,   10,     "region2",   "scenB",  "Oil",
     2030,   90,     "region2",   "scenB",  "Oil",
     2010,   55,     "region1",   "scenA",  "Gas",
     2020,   40,     "region1",   "scenA",  "Gas",
     2030,   30,     "region1",   "scenA",  "Gas",
     2010,   35,     "region2",   "scenA",  "Gas",
     2020,   30,     "region2",   "scenA",  "Gas",
     2030,   32,     "region2",   "scenA",  "Gas",
     2010,   16,     "region1",   "scenB",  "Gas",
     2020,   28,     "region1",   "scenB",  "Gas",
     2030,   39,     "region1",   "scenB",  "Gas",
     2010,   12,     "region2",   "scenB",  "Gas",
     2020,   26,     "region2",   "scenB",  "Gas",
     2030,   37,     "region2",   "scenB",  "Gas"
   )

   my_pal <- RColorBrewer::brewer.pal(9, "Set1")

   metis.chart(data = tbl_multi, xData = "x", yData = "value", class="fuel",
               chartType = "line",  classPalette=my_pal,
               facet_rows="region",facet_columns="scen")

   my_pal <- metis.colors()$pal_Basic

   metis.chart(data = tbl_multi, xData = "x", yData = "value", class="fuel", position="stack",
               group="fuel",chartType = "bar", classPalette=my_pal,
               facet_rows="region",facet_columns="scen")

   metis.chart(data = tbl_multi, xData = "x", yData = "value", class="fuel", position="dodge",
               group="fuel",chartType = "bar", classPalette=my_pal,
               facet_rows="region",facet_columns="scen")


# Sankey Diagram Example

   # Data Frame with 2 regions, 3 supply sectors and 3 demand sectors
   df <- data.frame(region = c("A","A","A","B","B","B"),
                    supplySector = c("coal","gas","wind","coal","gas","wind"),
                    demandSector = c("resid","indus","ag","resid","indus","ag"),
                    value = 10*runif(6)); df
                    
   if("ggalluvial" %in% rownames(installed.packages()) == F){install.packages("ggalluvial")}; library(ggalluvial)

   metis.chart(data=df, chartType="sankey", yData="value", sankeyGroupColor="supplySector",
               classLabel="From", class = "supplySector", classPalette = metis.colors()$pal_Basic,
               sankeyAxis1="supplySector",sankeyAxis2="demandSector",sankeyAxis1Label ="From",sankeyAxis2Label="To",
               facet_columns="region")
```

</p>
</details>


<!-- ------------------------>
<!-- ------------------------>
## <a name="metis.chartsProcess.R"></a> metis.chartsProcess.R
<p align="center"> <img src="READMEfigs/metisHeaderThick.PNG"></p>
<!-- ------------------------>
<!-- ------------------------>

[Back to Contents](#Contents)

<b> Key Inputs </b>  

- rTable: An rTable which can be the output of metis.readgcam() OR/AND
- dataTable: A .csv table which can be the original or modified tables produced by metis.readgcam
- (Optional) paramsSelect: A subset of the parameters read using the query file. 
- (Optional) regionsSelect: A subset of the regions available in the data.
- (Optional) scenRef: Select a scenario which will be used as a reference to compare differences with other scenarios.


After running metis.chartsProcess several charts will be produced. These will be saved in separate folders for each region and then within each there will be a folder for each scenario as well as a folder for cross-scenario comparisons. The folder paths are shown in the figure below. In addition to the individual regional folders a folder called compareRegions will contain inter-regional comparisons for each scenario as well as a cross-region/cross-scenario comparison.
Tables with the data used for each figure will also be provided in ./metis/outputs/Charts.

<p align="center"> <b> metis.chartsProcess Example Outputs </b> </p>
<p align="center"> <img src="READMEfigs/metis_moduleChart2.PNG"></p>


<details><summary>Click here to expand for further details, code and example figures.</summary>
<p>

<p align="center"> <b> metis.chartsProcess Output folders </b> </p>
<p align="center"> <img src="READMEfigs/metis_moduleChart1.png"></p>


```r
#----------------------------
# Produce Data Charts
#---------------------------

# Read in csv tables if available.
  # For example metis.readgcam.R produces datatable and templates which are saved by default in
  # ./metis/outputs/readGCAMTables/Tables_gcam/ for each gcam region selected
  # ./metis/outputs/readGCAMTables/Tables_Templates which gives an example template to fill out with local or other data.
  # Custom or local csv need to have "scenario", "region", "sources",	"param", "units",	"class",	"x",	"value"
  # As an example we are creating a local scenario for Argentina and will save it into Tables_Local.

   localData <- tibble::tribble (
     ~scenario,    ~region,     ~sources,	 ~param,          ~units,               ~class,             ~x,    ~value,
     "Local Data", "Argentina", "Sources", "finalNrgbySec", "Final Energy (TWh)", "building",        "2010", "10",
     "Local Data", "Argentina", "Sources", "finalNrgbySec", "Final Energy (TWh)", "industry",        "2010", "20",
     "Local Data", "Argentina", "Sources", "finalNrgbySec", "Final Energy (TWh)", "transportation",  "2010", "30",
     "Local Data", "Argentina", "Sources", "finalNrgbySec", "Final Energy (TWh)", "building",        "2015", "15",
     "Local Data", "Argentina", "Sources", "finalNrgbySec", "Final Energy (TWh)", "industry",        "2015", "22",
     "Local Data", "Argentina", "Sources", "finalNrgbySec", "Final Energy (TWh)", "transportation",  "2015", "34")


   if (!dir.exists(paste(getwd(), "/outputs", sep = ""))){dir.create(paste(getwd(), "/outputs", sep = ""))}
   data.table::fwrite(localData, file = paste(getwd(), "/outputs/example_localFile.csv", sep = ""),row.names = F)
   dataTables_i =  c(paste(getwd(), "/outputs/example_localFile.csv", sep = ""))

   # Can also add data .csv outputs from metis.readgcam.R which are autmatically saved in
   # ./metis/outputs/readGCAMTables/Tables_gcam
   # for each of the regions selected.
   # gcamDataTable_Argentina.csv, gcamDataTable_China.csv, gcamDataTable_Pakistan.csv
   # This would be added to dataTables_i as:
   # dataTables_i = c(paste(getwd(), "/example_localFile.csv", sep = ""),
   #                  paste(getwd(), "outputs/readGCAMTables/Tables_gcam/gcamDataTable_Argentina.csv", sep = ""),
   #                  paste(getwd(), "outputs/readGCAMTables/Tables_gcam/gcamDataTable_China.csv", sep = ""),
   #                  paste(getwd(), "outputs/readGCAMTables/Tables_gcam/gcamDataTable_Pakistan.csv", sep = ""))

# Read in the data from the function metis.readgcam.
  rTable_i <- dataGCAM$data;

# Choose Parameters or set to "All" for all params. For complete list see ?metis.chartsProcess
  paramsSelect_i=c("All")

# Select regions from the 32 GCAM regions.
  regionsSelect_i=c("Argentina","China", "Pakistan")

# Charts Process
  charts<-metis.chartsProcess(rTable=rTable_i, # Default is NULL
                          dataTables=dataTables_i, # Default is NULL
                          paramsSelect=paramsSelect_i, # Default is "All"
                          regionsSelect=regionsSelect_i, # Default is "All"
                          xCompare=c("2015","2030","2050","2100"), # Default is c("2015","2030","2050","2100")
                          scenRef="Eg1", # Default is NULL
                          dirOutputs=paste(getwd(),"/outputs",sep=""), # Default is paste(getwd(),"/outputs",sep="")
                          regionCompareOnly=0, # Default 0. If set to 1, will only run comparison plots and not individual
                          scenarioCompareOnly=0) # Default 0. If set to 1, will only run comparison plots and not individual

```

</p>
</details>



<!-- ------------------------>
<!-- ------------------------>
## <a name="metis.map.R"></a> metis.map.R
<p align="center"> <img src="READMEfigs/metisHeaderThick.PNG"></p>
<!-- ------------------------>
<!-- ------------------------>

[Back to Contents](#Contents)

metis.map.R is the metis charting software used in metis.mapsProcess.R and boundaries. It allows users to map outputs to shapefile polygons and raster. The default settings maintain a conistent look across the metis products.


<details><summary>Click here to expand for further details, code and example figures.</summary>
<p>

The following section shows some basic example to use metis.map.R.



```r
#-------------------
# Maps (metis.map.R)
#-------------------

# Polygons. An example Shapefile is Provided with metis in ./metis/dataFiles/examples.
  examplePolyFolder<-paste(getwd(),"/dataFiles/examples",sep="")
  examplePolyFile<-paste("bermejo3Cropped",sep="")

  # Read in the shape file and not the column name to use for fills and labels.
  bermejo3Cropped=readOGR(dsn=examplePolyFolder,
                          layer=examplePolyFile,use_iconv=T,encoding='UTF-8')
  head(bermejo3Cropped@data) # Choose the column name

  # Categorical Shapefile
  metis.map(dataPolygon=bermejo3Cropped,fillColumn = "SUB_NAME",labels=T ,printFig=F,facetsON=F)

  # Shapefile with values
  metis.map(dataPolygon=bermejo3Cropped,fillColumn = "SUB_AREA",labels=T ,printFig=F,facetsON=T,
            legendShow = T, legendOutside = T, fillPalette = "Reds", labelsAutoPlace = F)
```

</p>
</details>


<!-- ------------------------>
<!-- ------------------------>
## <a name="metis.boundaries.R"></a> metis.boundaries.R
<p align="center"> <img src="READMEfigs/metisHeaderThick.PNG"></p>
<!-- ------------------------>
<!-- ------------------------>


[Back to Contents](#Contents)

<b> Key Inputs </b>  

- boundaryRegShape: A polygon shapefile which has already been read into R defining the boundary region. Default is NULL. If left NULL then a folder and filename should be specified if a boundary is desired.
- boundaryRegShpFolder: If an extended map is desired then this can be provided. naturalEarth admin boundaries are available if downloaded from the osf link https://osf.io/pbta5/). These will be lying in ./metis/dataFiles/gis/naturalEarth, The default value is NULL and in this case no extended boundary will be drawn.
- boundaryRegShpFile: If desired and chosen from the naturalEarth data then "ne_10m_admin_0_countries" can be chosen.
- boundaryRegCol: If boundaryRegShpFolder and File are specified then this column should be specified. If the naturalEarth data is used the column is "NAME".
- boundaryRegionsSelect: Desired boundary region. In the naturalEarth data these are country names eg. "Argentina",
- subRegShape: A polygon shapefile which has already been read into R defining the subregion boundaries. Default is NULL. If left NULL then a folder and filename should be specified.
- subRegShpFolder: Folder where subregion shapefile is saved. 
- subRegShpFile: Name of subregion shapefile.
- subRegCol: Column name in shape data which defines the different sub-regions. The sub-region shape file should be read and this column name should be checked.
- (Optional) subRegionsSelect: If desired the subregion polygons can be subset by the sub-regions specified here.
- subRegType: The kind of sub-region being analyzed e.g. "state", "subBasin" etc. 
- nameAppend: If any name should be appended to the files produced e.g. "_hydrobidBermeo3",
- extension: If the extended boundary region should be plotted with the subregion.
- overlapShpFolder: If an overlapping shapefile is desired then the folder should be specified. For example GCAM basin boundaries saved in ./metis/dataFiles/gis/basin_gcam
- overlapShpFile: Overlapping shapefile name for eg. "Global235_CLM_final_5arcmin_multipart"

metis.boundaries is used to create maps showing where the sub-region lies in the greater region. For this option a boundary region should be defined. NaturalEarth boundary region files have been made available throught the osf repository https://osf.io/pbta5/). Once metis.boundaries has been run a folder for "Maps" is created. The Maps folder will contain a "Boundary" folder which contains the boundary files defining each region and subregion. The function also shows how different grid cells sizes compare with the selected regions. This is useful to understand if the desired regional resolution is too fine for a particular grid size. The folder structure and example output boundary maps are shown in the figures that follow.

<p align="center"> <b> metis.boundaries Outputs and Example Charts </b> </p>
<p align="center"> <img src="READMEfigs/metis_moduleBoundary1.png"></p>
<p align="center"> <img src="READMEfigs/metis_moduleBoundary2.png"></p>

<details><summary>Click here to expand for further details, code and example figures.</summary>
<p>

```r
# Example Shape File. Provided with metis in ./metis/dataFiles/examples
  examplePolyFolder_i<-paste(getwd(),"/dataFiles/examples",sep="")
  examplePolyFile_i<-paste("bermejo3Cropped",sep="")
  bermejo3Cropped=readOGR(dsn=examplePolyFolder_i,
                        layer=examplePolyFile_i,use_iconv=T,encoding='UTF-8')
  head(bermejo3Cropped@data)
  subRegCol_i = "SUB_NAME"
  metis.map(dataPolygon=bermejo3Cropped,fillColumn = subRegCol_i,labels=T ,printFig=F,facetsON=F)


  bermejoBoundaries<- metis.boundaries(
                            boundaryRegShape=NULL,
                            boundaryRegShpFolder=paste(getwd(),"/dataFiles/gis/naturalEarth",sep=""),
                            boundaryRegShpFile=paste("ne_10m_admin_0_countries",sep=""),
                            boundaryRegCol="NAME",
                            boundaryRegionsSelect="Argentina",
                            subRegShape=bermejo3Cropped,
                            subRegCol=subRegCol_i,
                            subRegType="subRegType",
                            nameAppend="_test",
                            expandPercent=2,
                            overlapShpFile="Global235_CLM_final_5arcmin_multipart",
                            overlapShpFolder=paste(getwd(),"/dataFiles/gis/basin_gcam",sep=""),
                            extension = T,
                            grids = c(paste(getwd(),"/dataFiles/grids/emptyGrids/grid_025.csv",sep=""),
                                      paste(getwd(),"/dataFiles/grids/emptyGrids/grid_050.csv",sep="")))


```

</p>
</details>

<!-- ------------------------>
<!-- ------------------------>
## <a name="metis.grid2poly.R"></a> metis.grid2poly.R
<p align="center"> <img src="READMEfigs/metisHeaderThick.PNG"></p>
<!-- ------------------------>
<!-- ------------------------>

[Back to Contents](#Contents)

<b> Key Inputs </b>  

- grid: Grid file with lat and lon coordinates. 
- boundaryRegionsSelect: Name of general region. The model will create a folder for each region.
- (Optional) subRegShape: A polygon shapefile which has already been read into R defining the subregion boundaries. Default is NULL. If left NULL then a folder and filename should be specified.
- subRegShpFolder: Folder where subregion shapefile is saved.
- subRegShpFile: Name of subregion shapefile.  If desired and chosen from the naturalEarth data then "ne_10m_admin_1_states_provinces" can be chosen.
- subRegCol: Column name in shape data which defines the different sub-regions. The sub-region shape file should be read and this column name should be checked. For the "ne_10m_admin_1_states_provinces" data this name will be "name"
- (Optional) subRegionsSelect: If desired the subregion polygons can be subset by the sub-regions specified here.
- subRegType: The kind of sub-region being analyzed e.g. "state", "subBasin" etc. 


metis.grid2poly is used to aggregate gridded data to given sub-regional spatial scales. Depending on the type of gridded data (volume or depth) the aggregation is done based on the part of the polygon which intersect with the grid cells as shown in the figure below. 

metis.grid2poly can also be used to create maps showing where the sub-region lies in the greater region. For this option a boundary region should be defined. naturalEarth boundary region files have been made available throught the osf repository https://osf.io/pbta5/). Once grid2poly has been run a folder for "Maps" and a folder for "Grids" (if a grid file was provided) are created. Within the Maps folder (./metis/outputs/Maps) the user will find a Tables folder which contains all the data used to create the map as a well as a template for stakeholders to input their own data as a new scenario by sub-region. The Maps folder will also contain a "Boundary" folder which contains the boundary files defining each region and subregion and also includes shapefiles incase these need to be shared with others. The folder structure and example output boundary maps are shown in the figures that follow. If a grid has been provided then a "Grid" folder is also created which contains the cropped gridded data which can be used in for mapping.

<details><summary>Click here to expand for further details, code and example figures.</summary>
<p>

```r
#-----------
# Grid to Poly
#-------------

# Example Grid File (csv with lats and lons that overlap the shapefile)
    gridExample<-paste(getwd(),"/dataFiles/examples/example_grid_ArgentinaBermejo3_Eg1Eg2.csv",sep="")

# Polygons. An example Shapefile is Provided with metis in ./metis/dataFiles/examples.
    examplePolyFolder_i<-paste(getwd(),"/dataFiles/examples",sep="")
    examplePolyFile_i<-paste("bermejo3Cropped",sep="")
    bermejo3Cropped=readOGR(dsn=examplePolyFolder_i,
                            layer=examplePolyFile_i,use_iconv=T,encoding='UTF-8')
    head(bermejo3Cropped@data) # TO choose subRegCol name
    subRegCol_i = "SUB_NAME"

# Run metis.grid2poly
    polyBermeo3Cropped<-metis.grid2poly(grid=gridExample,
                                    subRegShpFolder=examplePolyFolder_i,
                                    subRegShpFile=examplePolyFile_i,
                                    subRegCol=subRegCol_i,
                                    aggType="depth", # Aggregation type. Depth or volume. See docuemntation for further details.
                                    nameAppend="_Bermeo3")

```

<p align="center"> <b> metis.grid2poly Outputs and Example Charts </b> </p>
<p align="center"> <img src="READMEfigs/metis_moduleGrid2Poly1.png"></p>

<p align="center"> <b> metis.grid2poly metis.grid2poly conceptual aggregation methods  </b> </p>
<p align="center"> <img src="READMEfigs/metis_moduleGrid2PolySpatAgg.png"></p>


</p>
</details>

<!-- ------------------------>
<!-- ------------------------>
## <a name="metis.mapsProcess"></a> metis.mapsProcess
<p align="center"> <img src="READMEfigs/metisHeaderThick.PNG"></p>
<!-- ------------------------>
<!-- ------------------------>

[Back to Contents](#Contents)

<b> Key Inputs </b>  

- polygonDataTables: Data table with data per sub-region. This can be an individual table or outputs from metis.grid2poly. The individual table cna be built from the template provided by metis.grid2poly. 
- gridDataTables: This is gridded data whith lat, lon and values. The outputs from metis.grid2poly can be used or the output can be used as a template to populates user data.
- xRange=c(2005,2010,2020): Range of years to analyze. If left blank will analyze all available years.
- boundaryRegionsSelect: Name of overall region. Will create a folder with this name. "Argentina",
- (Optional) subRegShape: If a subregional shape is already available in R. If not then the shape folder and file need to be specified in the following paramters. 
- subRegShpFolder: Sub-region shape file folder. This can be an output of metis.grid2poly and will be lying in ./metis/outputs/Maps/Boundary/"REGIONX". It can also be a subregion from the downloaded gis files E.g. paste(getwd(),"/dataFiles/gis/subbasin_hydrobid",sep=""),
- subRegShpFile: Sub-region shapefile name e.g. paste("bermejo3Cropped",sep=""),
- subRegCol: Columns whith unique identifiers for each sub-region. In this case e.g. "SUB_NAME". The shapefile data should be read and checked to determine this. 
- subRegType: Name the type of sub-region being analyzed. A separate folder for each type will be create. For exampel "subBasin" or "state"
- nameAppend: Any additional names to append to files.
- legendPosition: Legend position for freescale maps. Since different shapes may warrant different legend positions. Default is =c("RIGHT","top").
- scenRef: The reference scenario name. All other scenarios will use this as the base for diff maps.


After running metis.mapsProcess additional folders are created in the ./metis/outputs/Maps/ folder for each region and sub-region indicated. Within each of these there are plots for each scenario as well as diff plots showing the absolute and percentage difference between the selected reference scenario and all other scenarios. Each map is produced with three kinds of legends Freescale, Kmeans and pretty (or equal breaks) which allow the user to analyze different kinds of data. THe colors schemes for the plots are determined in metis.colors and can be adjusted by advanced users. Animations showing changes through the years are also created for each type of map and legend type. Example of the folder structure and ouputs from metis.mapsProcess are shown below.

<p align="center"> <b> metis.mapsProcess Example Outputs Spatial Scales </b> </p>

Grid |  State
:-------------------------:|:-------------------------:
![](READMEfigs/anim_India_raster_griddedScarcityLimit1_Eg1_Local_KMEANS.gif)  |  ![](READMEfigs/anim_India_State_griddedScarcityLimit1_Eg1_Local_KMEANS.gif)  

Grid |  Basin 
:-------------------------:|:-------------------------:
![](READMEfigs/anim_India_raster_xanthosRunoff_Eg1_Local_KMEANS.gif)  |  ![](READMEfigs/anim_India_State_xanthosRunoff_Eg1_Local_KMEANS.gif)  


<details><summary>Click here to expand for further details, code and example figures.</summary>
<p>


```r
#------------------------------
# Mapping (metis.mapsProcess.R)
#------------------------------

# Simple Example. See example csv tables provided for ideal column names needed.
    exampleGridTable_i<-paste(getwd(),"/dataFiles/examples/example_grid_ArgentinaBermejo3_Eg1Eg2.csv",sep="")
    examplePolygonTable_i<-paste(getwd(),"/dataFiles/examples/example_poly_ArgentinaBermejo3_Eg1Eg2.csv",sep="")
    gridTable=read.csv(exampleGridTable_i);head(gridTable)
    polyTable=read.csv(examplePolygonTable_i);head(polyTable)

    subRegShpFolder_i <- paste(getwd(),"/dataFiles/examples",sep="")
    subRegShpFile_i <- paste("bermejo3Cropped",sep="")
    subRegShp_i = readOGR(dsn=subRegShpFolder_i,layer=subRegShpFile_i,use_iconv=T,encoding='UTF-8')
    head(subRegShp_i@data)
    subRegCol_i = "SUB_NAME"
    metis.map(dataPolygon=subRegShp_i,fillColumn = subRegCol_i,labels=F ,printFig=F,facetsON=F)

    metis.mapProcess(polygonDataTables=examplePolygonTable_i,
                 gridDataTables=exampleGridTable_i,
                 xRange=c(2005,2010,2020),
                 mapsOutFolderName="BermejoExample",
                 subRegShape=NULL,
                 subRegShpFolder=examplePolyFolder_i,
                 subRegShpFile=examplePolyFile_i,
                 subRegCol=subRegCol_i,
                 subRegType="subBasin",
                 nameAppend="_exampleSubRegionMap",
                 legendPosition=c("RIGHT","top"),
                 animateOn=T,
                 delay=100,
                 scenRef="Eg1",
                 #expandPercent = 2,
                 extension=F)

# Extended Map showing the subregion within a wider boudnary region

    boundaryRegShpFolder_i <- paste(getwd(),"/dataFiles/gis/naturalEarth",sep="")
    boundaryRegShpFile_i <- paste("ne_10m_admin_0_countries",sep="")
    boundaryRegShp_i = readOGR(dsn=boundaryRegShpFolder_i,layer=boundaryRegShpFile_i,use_iconv=T,encoding='UTF-8')
    head(boundaryRegShp_i@data)
    boundaryRegCol_i = "NAME"
    metis.map(dataPolygon=boundaryRegShp_i,fillColumn = boundaryRegCol_i,labels=F ,printFig=F,facetsON=F)
    # Pick country names from the list of countries in the natural earth shapefile.
    unique(boundaryRegShp_i@data[[boundaryRegCol_i]])
    boundaryRegionsSelect_i = c("Argentina") # Must be a region in the boundaryRegShp

    metis.mapProcess(polygonDataTables=examplePolygonTable_i,
                     gridDataTables=exampleGridTable_i,
                     xRange=c(2005,2010,2020),
                     mapsOutFolderName="BermejoExampleExtended",
                     boundaryRegionsSelect=boundaryRegionsSelect_i,
                     boundaryRegShpFolder = boundaryRegShpFolder_i,
                     boundaryRegShpFile = boundaryRegShpFile_i,
                     boundaryRegCol = boundaryRegCol_i,
                     subRegShape=NULL,
                     subRegShpFolder=examplePolyFolder_i,
                     subRegShpFile=examplePolyFile_i,
                     subRegCol=subRegCol_i,
                     subRegType="subBasin",
                     nameAppend="_exampleSubRegionMapExtended",
                     legendPosition=c("RIGHT","top"),
                     animateOn=T,
                     delay=100,
                     scenRef="Eg1",
                     expandPercent = 10,
                     extension=T)


#--------------------------------------------------
# Mapping (metis.mapsProcess.R) - Extensive Example
#--------------------------------------------------
    # Steps
    # Read in the boundary Shapefile to crop underlying data to.
    # Choose the boundary region name or names to subset the boundary shapefile.
    # Read in sub-region shape file (Example the GCAM Basins shapefile)
    # Run metis.boundaries.R to crop the sub-region shapefile to the boudnary region selected.
    # Read in polygon data table with data per sub-regions of interest
    # Runs metis.mapsProcess.R


# Read in Boundary Region
    # Read in the GCAM 32 regions shapefile which comes with metis.
    boundaryRegShpFolder_i <- paste(getwd(),"/dataFiles/gis/admin_gcam32",sep="")
    boundaryRegShpFile_i <- paste("region32_0p5deg_regions",sep="")
    boundaryRegShp_i = readOGR(dsn=boundaryRegShpFolder_i,layer=boundaryRegShpFile_i,use_iconv=T,encoding='UTF-8')
    head(boundaryRegShp_i@data)
    boundaryRegCol_i = "region"
    metis.map(dataPolygon=boundaryRegShp_i,fillColumn = boundaryRegCol_i,labels=F ,printFig=F,facetsON=F)

    # Choose GCAM region
    # Full list: USA, Africa_Eastern, Africa_Northern, Africa_Southern, Africa_Western,
    # Australia_NZ, Brazil, Canada Central America and Caribbean, Central Asia, China, EU-12,
    # EU-15, Europe_Eastern, Europe_Non_EU, European Free Trade Association, India, Indonesia, Japan,
    # Mexico, Middle East, Pakistan, Russia, South Africa, South America_Northern, South America_Southern,
    # South Asia, South Korea, Southeast Asia,

# A similar analysis can be done using the country shapefile for any country.
    # Uncomment the following lines of code and choose and appropriate region to crop to.
    # boundaryRegShpFolder_i <- paste(getwd(),"/dataFiles/gis/naturalEarth",sep="")
    # boundaryRegShpFile_i <- paste("ne_10m_admin_0_countries",sep="")
    # boundaryRegShp_i = readOGR(dsn=boundaryRegShpFolder_i,layer=boundaryRegShpFile_i,use_iconv=T,encoding='UTF-8')
    # head(boundaryRegShp_i@data)
    # boundaryRegCol_i = "NAME"
    # metis.map(dataPolygon=boundaryRegShp_i,fillColumn = boundaryRegCol_i,labels=F ,printFig=F,facetsON=F)
    # Pick country names from the list of countries in the natural earth shapefile.
    # unique(boundaryRegShp_i@data[[boundaryRegCol_i]])

    boundaryRegionsSelect_i = c("China") # Must be a region in the boundaryRegShp


# Read in subregion shapefile
    # Read in the  SubBasin GCAM Basins shapefile which comes with metis.
    subRegShpFolder_i <- paste(getwd(),"/dataFiles/gis/basin_gcam",sep="")
    subRegShpFile_i <- paste("Global235_CLM_final_5arcmin_multipart",sep="")
    subRegShp_i = readOGR(dsn=subRegShpFolder_i,layer=subRegShpFile_i,use_iconv=T,encoding='UTF-8')
    head(subRegShp_i@data)
    subRegCol_i = "basin_name"
    metis.map(dataPolygon=subRegShp_i,fillColumn = subRegCol_i,labels=F ,printFig=F,facetsON=F)

# Run metis.boundaries on the two shapefiles and selected region to get the cropped shapefile.
    boundaries<- metis.boundaries(
      boundaryRegShape=boundaryRegShp_i,
      boundaryRegCol=boundaryRegCol_i,
      boundaryRegionsSelect=boundaryRegionsSelect_i,
      subRegShape=subRegShp_i,
      subRegCol=subRegCol_i,
      subRegType="GCAMBasin",
      nameAppend="",
      expandPercent=2,
      #overlapShpFile="Global235_CLM_final_5arcmin_multipart",
      #overlapShpFolder=paste(getwd(),"/dataFiles/gis/basin_gcam",sep=""),
      extension = T,
      cropSubShape2Bound = T)

# The subregion shapefile created by boundaries can now be selected to be used for mapping values.
    subRegShp_i_Crop = boundaries$subRegShape # or can point to the subRegShapeFolder and subRegShpFile as produced by metis.boundaries.R
    head(subRegShp_i_Crop@data); levels(subRegShp_i_Crop@data[[subRegCol_i]])
    metis.map(dataPolygon=subRegShp_i_Crop,fillColumn = subRegCol_i,labels=T ,printFig=F,facetsON=F)
    # Sometimes the cropping results in slivers of regions left around boundaries when the boundayr and subregion shape don't line up.
    # The extra regions can be removed as follows.
    # Choose regions from the list of regions printed above.
    # Can also subset to the regions available in the polygon data table below.(unique(polyTable$subRegion)
    regions_to_remove =c("Amu_Darya")
    subRegShp_i_Crop<-subRegShp_i_Crop[(!subRegShp_i_Crop[[subRegCol_i]] %in% regions_to_remove) & !is.na(subRegShp_i_Crop[[subRegCol_i]]),]
    subRegShp_i_Crop@data <- droplevels(subRegShp_i_Crop@data)
    head(subRegShp_i_Crop@data); levels(subRegShp_i_Crop@data[[subRegCol_i]])
    metis.map(dataPolygon=subRegShp_i_Crop,fillColumn = subRegCol_i,labels=T ,printFig=F,facetsON=F)


# Read in the datatable with values by subRegion
    examplePolygonTable_i<-paste(getwd(),"/dataFiles/examples/example_GCAMBasins_analysis.csv",sep="")
    polyTable=read.csv(examplePolygonTable_i);head(polyTable)
    unique(polyTable$x); # check available number of years.

    # Make sure shapefile subRegions and PolygonTable subregions match
    unique(polyTable$subRegion); unique(subRegShp_i_Crop@data[[subRegCol_i]])

    metis.mapProcess(polygonDataTables=examplePolygonTable_i,
                     #gridDataTables=exampleGridTable_i,
                     xRange=c(2010,2020,2100),
                     mapsOutFolderName=boundaryRegionsSelect_i,
                     boundaryRegionsSelect=boundaryRegionsSelect_i,
                     boundaryRegShape=boundaryRegShp_i,
                     subRegShape=subRegShp_i_Crop,
                     subRegCol=subRegCol_i,
                     subRegType="GCAMBasin",
                     nameAppend="",
                     animateOn=T,
                     delay=100,
                     scenRef="SSP2_Ref",
                     extension=F,
                     diffOn = F)


# Improved map using available parameters.
    # Shift legend outside and change the scale_range to get conistent scale across scenarios.

# Set scale ranges across scenarios to be the same.
    # Check range of data for each param
    for(param_i in unique(polyTable$param)){
      print(paste("param: ", param_i, sep=""));print("Range is:")
      print(range((polyTable%>%dplyr::filter(param==param_i))$value))}

    scaleRange_i = tibble::tribble(
      ~param,~minScale, ~maxScale,
      "waterConsumption", 0, 10)

    metis.mapProcess(polygonDataTables=examplePolygonTable_i,
                     #gridDataTables=exampleGridTable_i,
                     xRange=c(2010,2020,2100),
                     mapsOutFolderName=paste(boundaryRegionsSelect_i,"_Edited",sep=""),
                     boundaryRegionsSelect=boundaryRegionsSelect_i,
                     boundaryRegShape=boundaryRegShp_i,
                     subRegShape=subRegShp_i_Crop,
                     subRegCol=subRegCol_i,
                     subRegType="GCAMBasin",
                     nameAppend="_improvedFig",
                     legendPosition=c("LEFT","bottom"),
                     animateOn=T,
                     delay=100,
                     scenRef="SSP2_Ref",
                     extension=F,
                     diffOn = F,
                     legendOutsideSingle = T,
                     scaleRange = scaleRange_i)

```

<p align="center"> <b> metis.mapsProcess Outputs Folders </b> </p>
<p align="center"> <img src="READMEfigs/metis_moduleMapsProcessFolders.png"></p>

<p align="center"> <b> metis.mapsProcess Example Output Data Scales </b> </p>
<p align="center"> <img src="READMEfigs/metis_moduleMapsProcessRaster.png"></p>


</p>
</details>


<!-- ------------------------>
<!-- ------------------------>
# <a name="underDevelopment"></a> Under development
<p align="center"> <img src="READMEfigs/metisHeaderThick.PNG"></p>
<!-- ------------------------>
<!-- ------------------------>

[Back to Contents](#Contents)

This section describes functions currently under development. Comments and suggestions are welcome. Functions under development include:

- metis.io.R: Input Output analysis
- metis.irio.R: Regional Input Output analysis

<!-- ------------------------>
<!-- ------------------------>
## <a name="metis.io"></a> metis.io
<p align="center"> <img src="READMEfigs/metisHeaderThick.PNG"></p>
<!-- ------------------------>
<!-- ------------------------>

[Back to Contents](#Contents)


<b> Key Inputs </b>  


<!-- ------------------------>
<!-- ------------------------>
## <a name="metis.irio"></a> metis.irio
<p align="center"> <img src="READMEfigs/metisHeaderThick.PNG"></p>
<!-- ------------------------>
<!-- ------------------------>

[Back to Contents](#Contents)


<b> Key Inputs </b>  




<p align="center"> <img src="READMEfigs/metisHeaderThin.PNG"></p>

[Back to Contents](#Contents)
