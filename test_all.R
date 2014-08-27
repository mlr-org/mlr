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
library(party)
library(LiblineaR)
library(stepPlr)
library(FNN)
library(randomForestSRC)
library(glmnet) # otherwise auc is shadowed
library(caret) # otherwise train shadowed

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
  test.info = test_dir("tests/testthat", filter = "onv")
} else {
  catf("Run test for file %s", file.path("tests", "testthat", file))
  test.info = test_file(file.path("tests", "testthat", file))
}
save2(file = "testinfo.RData", test.info)

