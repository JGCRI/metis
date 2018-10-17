
# Hadley Wickham R Packages http://r-pkgs.had.co.nz/intro.html
# Testing http://r-pkgs.had.co.nz/tests.html
# Checking http://r-pkgs.had.co.nz/check.html

# Sys.getenv("R_LIBS_USER")  # For location of Libraries
# Close other R sessions to install certain packages
#install.packages(c("digest","devtools", "roxygen2", "testthat", "knitr","rstudioapi"))
#devtools::install_github("r-lib/devtools")
# rstudioapi::isAvailable("0.99.149")
# install Rtools (Not an R package)


library(roxygen2)
library(testthat)
devtools::session_info()


# Clean up Code Formatting
install.packages("lintr")
lintr::lint_package()

# If you change options or working directories, change them back on exit
old <- options(stringsAsFactors = FALSE)
on.exit(options(old), add = TRUE)
old <- setwd(tempdir())
on.exit(setwd(old), add = TRUE)

devtools::install_git("https://khan404@stash.pnnl.gov/scm/~khan404/srn.git", ref="dev_userguide")

