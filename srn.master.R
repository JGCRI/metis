

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
