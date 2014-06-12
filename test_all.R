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
library(pls) # otherwise pls shadows crossval

if (interactive()) {
  load_all(".")
} else {
  library(mlr)
}

source("tests/testthat/helper_helpers.R")
source("tests/testthat/helper_objects.R")

args = commandArgs()
file = args[which(args == "--args") + 1L]
if (length(file) == 0 || is.na(file)) {
  test_dir("tests/testthat", filter = "base")
} else {
  catf("Run test for file %s", file.path("tests", "testthat", file))
  test_file(file.path("tests", "testthat", file))
}
