

#----------------------------
# Install The SRN Package
#----------------------------
#install.packages("devtools");
#library(devtools)
#install_github(repo="zarrarkhan/srn",'dev_srn') # To install directly from branch
library(srn)

#----------------------------
# Load Libraries
#----------------------------
a<-srn.colors()
testcolor<-a$elec_tech_colors
pie(rep(1,length(testcolor)),label=names(testcolor),col=a$testcolor)

#----------------------------
# Read GCAM Data
#---------------------------

gcamdatabasePath <-"C:/Users/khan404/Desktop/srn/inputs/gcam"
gcamdatabaseName <-"example_database_basexdb"

# Use function localDBConn from package rgcam to get a list of scenarios if needed.
localDBConn(gcamdatabasePath,gcamdatabaseName)

gcamData<-srn.readgcam(gcamdatabase=paste(getwd(),"/inputs/gcam/example_database_basexdb",sep=""),
                       queryxml=paste(getwd(),"/inputs/gcam/srnQueries.xml",sep=""),
                       scenOrigNames=c("example_Reference1","example_Reference2"),
                       scenNewNames=c("Ref1","Ref2"),
                       reReadData=T, # Default Value
                       dataProj=NULL, # Default Value
                       dirOutputs= paste(getwd(),"/outputs",sep="") # Default Value
)
