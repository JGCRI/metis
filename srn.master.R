


install_github(repo="zarrarkhan/srn",'dev_srn')
library(srn)
srn.colors()
pie(rep(1,length(colorsX_Basic)),label=names(colorsX_Basic),col=colorsX_Basic)
