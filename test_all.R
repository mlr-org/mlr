library(methods)
library(devtools)
library(testthat)
library(BBmisc)
library(parallelMap)
library(ParamHelpers)

library(e1071)
library(adabag)
library(MASS)
library(ROCR)
library(pROC)

if (interactive()) {
  load_all(".")
} else {
  library(mlr)
}

source("inst/tests/helper_helpers.R")
source("inst/tests/helper_objects.R")

args = commandArgs()
file = args[which(args == "--args")+1]
if(length(file)==0 || is.na(file)) {
  test_dir("inst/tests")
}else{
  catf("Run test for file %s", file.path("inst",file))
  test_file(file.path("inst","tests",file))
}
