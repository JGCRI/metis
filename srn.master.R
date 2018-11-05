

#----------------------------
# Install The SRN Package
#----------------------------
#install.packages("devtools");
#library(devtools)
#install_github(repo="zarrarkhan/srn",'dev_srn') # To install directly from branch
library(srn)
#install_github(repo="JGCRI/rgcam")
library(rgcam)

#----------------------------
# Load Libraries
#----------------------------
a<-srn.colors()
testcolor<-a$elec_tech_colors
pie(rep(1,length(testcolor)),label=names(testcolor),col=a$testcolor)

#----------------------------
# Read GCAM Data
#---------------------------
gcamdatabasePath <-paste(getwd(),"/inputs/gcam",sep="")
gcamdatabaseName <-"example_database_basexdb"
queryxml <- "srnQueries.xml"

# Use function localDBConn from package rgcam to get a list of scenarios if needed.
#localDBConn(gcamdatabasePath,gcamdatabaseName)

gcamData<-srn.readgcam(reReadData=F, # Default Value is T
                       dataProj="dataProj.proj", # Default Value is "dataProj.proj"
                       scenOrigNames=c("ExampleScen1","ExampleScen2"),
                       scenNewNames=c("Eg1","Eg2"),
                       gcamdatabasePath=gcamdatabasePath,
                       gcamdatabaseName=gcamdatabaseName,
                       queryxml=queryxml,
                       dirOutputs= paste(getwd(),"/outputs",sep=""), # Default Value is paste(getwd(),"/outputs",sep="")
                       regions=NULL # Default Value is NULL
                       )
