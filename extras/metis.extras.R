
#---------------------
# R Package Writing Guides
#---------------------
# Hadley Wickham R Packages http://r-pkgs.had.co.nz/intro.html
# Testing http://r-pkgs.had.co.nz/tests.html
# Checking http://r-pkgs.had.co.nz/check.html

#---------------------
# FAQs and common issues
#---------------------
# Sys.getenv("R_LIBS_USER")  # For location of Libraries
# Close other R sessions to install certain packages
# install.packages(c("digest","devtools", "roxygen2", "testthat", "knitr","rstudioapi"))
# devtools::install_github("r-lib/devtools")
# rstudioapi::isAvailable("0.99.149")
# install Rtools (Not an R package)

#---------------------
# Create User Manual
#---------------------
# Add R to system variable path C:
# install.packages('tinytex')
# tinytex::install_tinytex()
# tinytex:::is_tinytex()
if(file.exists(paste(getwd(),"/metis.pdf",sep=""))){unlink(paste(getwd(),"/metis.pdf",sep=""))}
system(paste("R CMD Rd2pdf ",getwd(),sep=""))

#--------------------
# Create Vignettes
#---------------------
# Create a subdirectory in the package folder called "vignettes"
# Place NAME.vignette.Rmd file in there
# http://kbroman.org/pkg_primer/pages/vignettes.html
devtools::build_vignettes()


#---------------------
# Testing
#--------------------
library(roxygen2)
library(testthat)
devtools::session_info() # For loaded packages

#---------------------
# Checking
#--------------------
system("R CMD check --help")

#---------------------
# Code Formatting
#--------------------
#install.packages("lintr")
library(lintr)
lintr::lint_package() # Will give warnings but not change code

#install.packages("formatR")
library(formatR)
#formatR::tidy_app() # App to paste code for formatting
#formatR::tidy_dir("R") # Will change your code

#------------------------
# Push and Pull to Github
#------------------------
# In metis folder, open gitbash (right click GitBash Here)
# git status
# git add .
# git commit -m "Messages"
# git push
# On github site merge dev branch with master
# git pull origin/master (To get the extra commits)

install_github(repo="zarrarkhan/metis",'dev_metis')


#--------------
# Size of Objects in Evnvironment
#----------------

size = 0
dfSize = data.frame()
for (x in ls() ){
  thisSize = object.size(get(x))
  size = size + thisSize
  #message(x, " = ", appendLF = F); print(thisSize, units='auto')
  dfSizeTemp <- data.frame(file=c(paste(x)),size=paste(print(thisSize, units='Mb')),units="MB")
  dfSize <- dfSize %>% bind_rows(dfSizeTemp)
}
message("total workspace is ",appendLF = F); print(size, units='auto')
dfSize$size = as.numeric(dfSize$size);
dfSize %>% arrange(size)
gc()


#--------------
# Custom Legends for presentations
#-------------

df= data.frame(subsec=c("wind","solar","refined liquids","nuclear","hydro","geothermal","gas","coal","biomass"),
               value=c(1:9))

order=c("wind","solar","refined liquids","nuclear","hydro","geothermal","gas","coal","biomass")

df$subsec <- factor( as.character(df$subsec), levels= order )

ggplot(data=df)+
  geom_bar(aes(x=subsec,fill=subsec))+
  scale_fill_manual(name="",values=metis.colors()$pal_elec_subsec)+
  theme(legend.position="bottom")+
  guides(fill=guide_legend(nrow=1,byrow=TRUE))



#----------------------------
# Profiling Code
#------------------------------
if("profvis" %in% rownames(installed.packages()) == F){install.packages("profvis")}
library(profvis)


#profvis(metis.colors("pal_hot"))


# countryName="Argentina"
# grid=paste(getwd(),"/outputs/Grids/gridMetis.RData", sep = "")
# boundaryRegionsSelect=countryName
# subRegShpFolder=paste(getwd(),"/dataFiles/gis/shapefiles_",countryName,sep="")
# subRegShpFile=paste(countryName,"NE1",sep="")
# subRegCol="name"
# subRegType = "state"
# nameAppend="_NEState"
# sqliteUSE = T
# sqliteDBNamePath = paste(getwd(),"/outputs/Grids/gridMetis.sqlite", sep = "")
# paramsSelect= c("All")

profvis(metis.grid2poly(
  #grid=grid,
  boundaryRegionsSelect=countryName,
  subRegShpFolder=paste(getwd(),"/dataFiles/gis/shapefiles_",countryName,sep=""),
  subRegShpFile=paste(countryName,"NE1",sep=""),
  subRegCol="name",
  subRegType = "state",
  nameAppend="_NEState",
  sqliteUSE = sqliteUSE,
  sqliteDBNamePath = sqliteDBNamePath,
  paramsSelect=c("All")))


# -------------
# install.packges("diffobj")
# diffObj(a,b) to see diff
