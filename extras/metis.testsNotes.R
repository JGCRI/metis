
#----------------------------
# Install necessary packages
#----------------------------
if("devtools" %in% rownames(installed.packages()) == F){install.packages("devtools")}
library(devtools)
if("metis" %in% rownames(installed.packages()) == F){install_github(repo="zarrarkhan/metis")}
library(metis)
if("rgcam" %in% rownames(installed.packages()) == F){install_github(repo="JGCRI/rgcam")}
library(rgcam)

for(package_i in c("tibble","dplyr","rgdal","tmap")){
  if(package_i %in% rownames(installed.packages()) == F){install.packages(package_i)}
  library(package_i,character.only = TRUE)}


#------------
# Check Compare get versus !!

polygonDataTables_i=paste(getwd(),"/outputs/Maps/Tables/subReg_origData_byClass_India_state_origDownscaled_indiaLocal.csv",sep="")
a<-read.csv(polygonDataTables_i); head(a); unique(a$scenario)
a<-a%>%dplyr::mutate(valueA=value,valueB=value*3); head(a)

colA="valueA"
colB="valueB"
a1 <-a%>%
  dplyr::mutate(!!paste("Diff_X_", colA,"_", colB,sep="") := !!rlang::sym(colB) - !!rlang::sym(colA),
                classPalette="pal_div")
tibble::as_tibble(a1)


a2 <-a%>%
  dplyr::mutate(!!paste("Diff_X_", colA,"_", colB,sep="") := get(colB) - get(colA),
                classPalette="pal_div")
tibble::as_tibble(a2)


#----------
