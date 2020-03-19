
# metis.master.R
# Script to run different parts of the metis package.

#----------------------------
# Install necessary packages
#----------------------------
if("devtools" %in% rownames(installed.packages()) == F){install.packages("devtools")}
library(devtools)
if("metis" %in% rownames(installed.packages()) == F){install_github(repo="JGCRI/rgcam")}
library(metis)





if("rgcam" %in% rownames(installed.packages()) == F){install_github(repo="JGCRI/rgcam")}
library(rgcam)
if("tibble" %in% rownames(installed.packages()) == F){install.packages("tibble")}
library(tibble)
if("rgdal" %in% rownames(installed.packages()) == F){install.packages("rgdal")}
library(rgdal)
if("tmap" %in% rownames(installed.packages()) == F){install.packages("tmap")}
library(tmap)
if("zoo" %in% rownames(installed.packages()) == F){install.packages("zoo")}
library(zoo)
if("RSQLite" %in% rownames(installed.packages()) == F){install.packages("RSQLite")}
library(RSQLite)
if("ggplot2" %in% rownames(installed.packages()) == F){install.packages("ggplot2")}
library(ggplot2)
if("ggalluvial" %in% rownames(installed.packages()) == F){install.packages("ggalluvial")}
library(ggalluvial)

if("dplyr" %in% rownames(installed.packages()) == F){install.packages("dplyr")}
library(dplyr)
if("dbplyr" %in% rownames(installed.packages()) == F){install.packages("dbplyr")}
library(dbplyr)



if("tidyr" %in% rownames(installed.packages()) == F){install.packages("tidyr")}
library(tidyr)


#----------------------------
# Read GCAM Data (metis.readgcam.R)
#---------------------------

# Connect to gcam database or project
  gcamdatabasePath_i <-'G:/IDBNexus/Final' # 'C:/Users/twild/Downloads/pic'  #  # Use if gcamdatabase is needed
  gcamdatabaseName_i <-"PricePathAppliedCO2NonCO2_CO_ARG" # "Reference_originalSW" Use if gcamdatabse is needed
  dataProjPath_i <- paste(getwd(),"/outputs",sep="") # Path to dataProj file.
  dataProj_i <-"IDBNexusFinal.proj"  # Use if gcamdata has been saved as .proj file

# Get list of scenarios and rename if desired.
#  rgcam::localDBConn(gcamdatabasePath_i,gcamdatabaseName_i) # if connecting directly to gcam database
#  dataProjLoaded <- loadProject(paste(dataProjPath_i, "/",dataProj_i , sep = ""))
#  listScenarios(dataProjLoaded)  # List of Scenarios in GCAM database
  scenOrigNames_i = c("Reference", "Impacts", "Policy") #  c(, "DeepDecarb1Mkt_2DS")  #'GCAMOriginal',
  scenNewNames_i = c("Reference", "Climate Impacts", "Climate Policy") # "Original",   c(, "DeepDecarb1Mkt_2DS"")  # Names to replace the original names for final figures.

# Choose Parameters or set to "All" for all params. For complete list see ?metis.readgcam
 # paramsSelect_i <- c('aggLandAlloc')

  paramsSelect_i <- c('All')

#  queriesSelect_i <- c("land allocation by crop and water source",
#                       "aggregated land allocation",
#                       "land allocation by crop")
  queriesSelect_i <- c("All")

# Select regions from the 32 GCAM regions.
  regionsSelect_i <- c("Colombia", "Argentina", "Uruguay")

  # Reading in the no bio query so it works with Rgcam

  dataGCAM<-metis.readgcam(reReadData = F,  # F
                           gcamdatabasePath = gcamdatabasePath_i,
                           gcamdatabaseName = gcamdatabaseName_i,
                           scenOrigNames = scenOrigNames_i,
                           scenNewNames = scenNewNames_i,
                           dataProj = dataProj_i,
                           dataProjPath = dataProjPath_i,
                           regionsSelect = regionsSelect_i,
                           paramsSelect=paramsSelect_i,
                           queriesSelect=queriesSelect_i)

    # reReadData = T  # F
  # gcamdatabasePath = gcamdatabasePath_i
  # gcamdatabaseName = gcamdatabaseName_i
  # scenOrigNames = scenOrigNames_i
  # scenNewNames = scenNewNames_i
  # #dataProj = dataProj_i
  # #dataProjPath = dataProjPath_i
  # regionsSelect = regionsSelect_i
  # paramsSelect=paramsSelect_i

  dataGCAM$data # To view the data read that was read.

#------------------------------------------------------------------------------------------
# Charts Process (metis.chartsProcess.R)
#------------------------------------------------------------------------------------------

# Can also add data .csv outputs from metis.readgcam.R which are autmatically saved in
  # ./metis/outputs/readGCAMTables/Tables_gcam
  # for each of the regions selected.
  # gcamDataTable_Argentina.csv, gcamDataTable_China.csv, gcamDataTable_Pakistan.csv
  # This would be added to dataTables_i as:
  dataTables_i = c(paste(getwd(), "/outputs/readGCAMTables/Tables_local/local_Regional_Colombia.csv", sep = "")
                   #paste(getwd(), "/outputs/readGCAMTables/Tables_gcam/gcamDataTable_Colombia.csv", sep = "")
                   )

# Read in the data from the function metis.readgcam.
  rTable_i <- dataGCAM$data;

# Choose Parameters or set to "All" for all params. For complete list see ?metis.chartsProcess

  paramsSelect_i <- c("finalNrgbySec", "TranspFinalNrgByFuel", "BuildFinalNrgByFuel",
                   "IndFinalNrgByFuel", "primNrgConsumByFuel", "elecByTech", "watWithdrawBySec",
                   "aggLandAlloc", "LUCemiss", "nonco2emissionBySectorGWPAR5",
                   "finalNrgbyFuel","finalElecbySec","finalElecbyFuel",
                   "NonCo2EmissionsByResProdGWPAR5",
                   "TotalFFIEmissBySec", "CO2BySector_NonCO2Gases_GWPAR5", "CO2BySector_NonCO2Gases_GWPAR5_LUC",
                   "TotalEmissBySec", "LandAllocByCrop", "MethaneBySource", "PassengerVMTByMode", "FreightVMTByMode",
                   "BuildFinalNrgBySector",
                   "co2emissionBySectorNoBio", "PassengerVMTByFuel", "FreightVMTByFuel", "RefiningByLiq")
  paramsSelect_i <- "All"
  paramsSelect_i <-  "elecByTechTWh"  # "elecInvest"

  #paramsSelect_i <- c('watWithdrawByCrop', 'aggLandAlloc')

# Select regions from the 32 GCAM regions.
# paramsSelect_i <- c('BuildFinalNrgBySector')
# Charts Process
  #regionsSelect_i <- c('Colombia')
  #paramsSelect_i <- c("elecByTech", "watWithdrawBySec")
  charts<-metis.chartsProcess(
                          rTable=rTable_i, # Default is NULL
                          #dataTables=dataTables_i, # Default is NULL
                          paramsSelect=paramsSelect_i, # Default is "All"
                          regionsSelect=regionsSelect_i, # Default is "All"
                          xCompare=c("2010","2030","2050"), # Default is c("2015","2030","2050","2100")
                          scenRef="Reference", # Default is NULL
                          dirOutputs=paste(getwd(),"/outputs",sep=""), # Default is paste(getwd(),"/outputs",sep="")
                          regionCompareOnly=0, # Default 0. If set to 1, will only run comparison plots and not individual
                          scenarioCompareOnly=0,
                          regionCompare=0,
                          useNewLabels = 0,
                          folderName = "IDBNexusFinal",
                          xRange = c(2020, 2030, 2040, 2050),
                          colOrder1 = c("Reference", "Climate Impacts", "Climate Policy"), #"Original",
                          colOrderName1 = "scenario",
                          pdfpng='pdf') # Default 0. If set to 1, will only run comparison plots and not individual

  # Colombia: Plot reference case CO2 emissions and Policy Co2 emissions CO2 emissions.
  CO2_Emissions_RefPolicy <- read.csv("//essi12.umd.edu/documents/twild/Documents/Publications/2019/Wild et al. (2019) - Climatic Change - Colombia energy-water-land/Figures/Figure_EmissionsGoalsLine/Figure_EmissionsGoalsLine.csv", skip=1)
  CO2_Emissions_RefPolicy <- CO2_Emissions_RefPolicy %>%
    gather(Scenario, value, Reference:Policy)
  p <- ggplot(data = CO2_Emissions_RefPolicy %>% filter(!Scenario=='NDC'), mapping = aes(x = year, y = value, color = Scenario, fill=Scenario))
  p <- p + scale_color_manual(values=c("#1bab55", "black"))
  p <- p + ylim(0,200)
  p <- p + ylab(expression(CO[2]~Emissions~(10^6~tons)))
  p <- p + xlab('Year')
  p <- p + geom_line(size=1)
  p <- p + theme(text = element_text(size=12), axis.text.x = element_text(size=12))
  p <- p + theme_bw()
  dirOutputs <- '//essi12.umd.edu/documents/twild/Documents/Publications/2019/Wild et al. (2019) - Climatic Change - Colombia energy-water-land/Figures/Figure_EmissionsGoalsLine'
  fname <- 'Figure_emissCO2_RefPolicyCompare'
  pdfpng <- 'pdf'
  figWidth <- 4
  figHeight <- 3
  p
  metis.printPdfPng(figure=p,
                    dir=dirOutputs,
                    filename=fname,
                    figWidth=figWidth,
                    figHeight=figHeight,
                    pdfpng=pdfpng)

  # Argentina: Plot Reference case CO2 emissions and Policy Co2 emissions CO2 emissions.
  CO2_Emissions_RefPolicy <- read.csv("//essi12.umd.edu/documents/twild/Documents/Publications/2019/Wild et al. (2019) - Water - Argentina/Figures/Figure_EmissionsGoalsLine/Figure_EmissionsGoalsLine.csv", skip=1)
  CO2_Emissions_RefPolicy <- CO2_Emissions_RefPolicy %>%
    gather(Scenario, value, Reference:Policy)
  p <- ggplot(data = CO2_Emissions_RefPolicy %>% filter(!Scenario=='NDC'), mapping = aes(x = year, y = value, color = Scenario, fill=Scenario))
  p <- p + scale_color_manual(values=c("#1bab55", "black"))
  p <- p + ylim(0,300)
  p <- p + ylab(expression(CO[2]~Emissions~(10^6~tons)))
  p <- p + xlab('Year')
  p <- p + geom_line(size=1)
  p <- p + theme(text = element_text(size=12), axis.text.x = element_text(size=12))
  p <- p + theme_bw()
  dirOutputs <- '//essi12.umd.edu/documents/twild/Documents/Publications/2019/Wild et al. (2019) - Water - Argentina/Figures/Figure_EmissionsGoalsLine'
  fname <- 'Figure_emissCO2_RefPolicyCompare'
  pdfpng <- 'pdf'
  figWidth <- 4
  figHeight <- 3
  p
  metis.printPdfPng(figure=p,
                    dir=dirOutputs,
                    filename=fname,
                    figWidth=figWidth,
                    figHeight=figHeight,
                    pdfpng=pdfpng)
