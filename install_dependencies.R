pkgs <- c(
  "devtools",
  "roxygen2",
  "testthat",
  "Rmpi"
)

# Determine missing packages 
installed.pkgs <- rownames(installed.packages())
missing.pkgs <- setdiff(pkgs, installed.pkgs)

if(length(missing.pkgs) > 0) {
  cat("Installing the following packages:\n")
  print(missing.pkgs)
  install.packages(missing.pkgs, dep = TRUE, repos="http://cran.at.r-project.org")
} else {
  cat("All packages installed!\n")
}

#install everything from 
library("devtools")
install_deps(".", TRUE)

#Rweka packages
library("RWeka")
WPM("install-package", "Xmeans")

if (FALSE){
  # Update from github
  install_github(repo = "berndbischl/parallelMap")  
  install_github(repo = "berndbischl/ParamHelpers")
}


