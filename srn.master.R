

#----------------------------
# Install The SRN Package
#----------------------------
if("devtools" %in% rownames(installed.packages()) == F){install.packages("devtools")}
library(devtools)
if("srn" %in% rownames(installed.packages()) == F){install_github(repo="zarrarkhan/srn")}
library(srn)
if("rgcam" %in% rownames(installed.packages()) == F){install_github(repo="JGCRI/rgcam")}
library(rgcam)
library(tibble)
library(dplyr)

#----------------------------
# Tests
#----------------------------
a<-srn.colors()
testcolor<-a$pal_Basic
pie(rep(1,length(testcolor)),label=names(testcolor),col=a$testcolor)

#----------------------------
# Read GCAM Data
#---------------------------

gcamdatabasePath <-paste(getwd(),"/dataFiles/gcam",sep="")
gcamdatabaseName <-"example_database_basexdb"
gcamdataProjFile <-"Example_dataProj.proj"
regionsSelect <- c("Colombia","Argentina")

# Use function localDBConn from package rgcam to get a list of scenarios if needed.
#localDBConn(gcamdatabasePath,gcamdatabaseName)

dataGCAM<-srn.readgcam(reReadData=F, # Default Value is T
                       dataProj=gcamdataProjFile, # Default Value is "dataProj.proj"
                       scenOrigNames=c("ExampleScen1","ExampleScen2"),
                       scenNewNames=c("Eg1","Eg2"),
                       gcamdatabasePath=gcamdatabasePath,
                       gcamdatabaseName=gcamdatabaseName,
                       queryxml="srnQueries.xml",  # Default Value is "srnQueries.xml"
                       dirOutputs= paste(getwd(),"/outputs",sep=""), # Default Value is paste(getwd(),"/outputs",sep="")
                       regionsSelect=regionsSelect, # Default Value is NULL
                       queriesSelect="All" # Default value is "All"
                       )

dataGCAM$data # To view the data read that was read.

#----------------------------
# Produce Data Charts
#---------------------------

# ?srn.chartsProcess # For more help on charting process

# Read in Tables (If exist)
dataTables<-c(paste(getwd(),"/outputs/Tables/Tables_Local/local_Regional_Colombia.csv",sep=""),
              paste(getwd(),"/outputs/Tables/Tables_Local/local_Regional_Argentina.csv",sep=""))

# Read in the data from the function srn.readgcam
rTable <- dataGCAM$data;
unique(rTable$param)

# Choose Parameters or set to "All" for all params
paramsSelect=c("gdp","finalNrgbySec")
regionsSelect=c("Argentina","Colombia")

charts<-srn.chartsProcess(rTable=rTable, # Default is NULL
                          dataTables=dataTables, # Default is NULL
                          paramsSelect=paramsSelect, # Default is "All"
                          regionsSelect=regionsSelect, # Default is "All"
                          xCompare=c("2015","2035","2050","2100"), # Default is c("2015","2030","2050","2100")
                          scenRef="Eg1", # Default is NULL
                          dirOutputs=paste(getwd(),"/outputs",sep=""), # Default is paste(getwd(),"/outputs",sep="")
                          pdfpng="png" # Default is "png"
                          )


