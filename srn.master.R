

#----------------------------
# Install The SRN Package
#----------------------------
if("devtools" %in% rownames(installed.packages()) == F){install.packages("devtools")}
library(devtools)
if("srn" %in% rownames(installed.packages()) == F){install_github(repo="zarrarkhan/srn")}
library(srn)
if("rgcam" %in% rownames(installed.packages()) == F){install_github(repo="JGCRI/rgcam")}
library(rgcam)

#----------------------------
# Tests
#----------------------------
a<-srn.colors()
testcolor<-a$elec_tech_colors
pie(rep(1,length(testcolor)),label=names(testcolor),col=a$testcolor)

#----------------------------
# Read GCAM Data
#---------------------------

gcamdatabasePath <-paste(getwd(),"/dataFiles/gcam",sep="")
gcamdatabaseName <-"example_database_basexdb"
regions <- c("Colombia","Argentina")

# Use function localDBConn from package rgcam to get a list of scenarios if needed.
#localDBConn(gcamdatabasePath,gcamdatabaseName)

gcamData<-srn.readgcam(reReadData=F, # Default Value is T
                       dataProj="Example_dataProj.proj", # Default Value is "dataProj.proj"
                       scenOrigNames=c("ExampleScen1","ExampleScen2"),
                       scenNewNames=c("Eg1","Eg2"),
                       gcamdatabasePath=gcamdatabasePath,
                       gcamdatabaseName=gcamdatabaseName,
                       queryxml="srnQueries.xml",  # Default Value is "srnQueries.xml"
                       dirOutputs= paste(getwd(),"/outputs",sep=""), # Default Value is paste(getwd(),"/outputs",sep="")
                       regions=regions # Default Value is NULL
                       )

#----------------------------
# Produce Data Charts
#---------------------------

dataTables<-c(paste(getwd(),"/outputs/Colombia/regional/dataTable_Colombia_1975to2100.csv",sep=""),
              paste(getwd(),"/outputs/Colombia/regional/dataTableLocal_Colombia_1975to2100.csv",sep=""))

charts<-srn.chartsProcess(dataTables)


