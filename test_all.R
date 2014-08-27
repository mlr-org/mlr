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
library(plyr)
library(survival)

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
  tests = test_dir("tests/testthat")
} else {
  catf("Run test for file %s", file.path("tests", "testthat", file))
  tests = test_file(file.path("tests", "testthat", file))
}
tests$group = sapply(strsplit(tests$file, "_"), function(x) x[2])
groups = ddply(tests, "group", summarise,
  failed = sum(failed), error = sum(error), time = sum(real))
save2(file = "testinfo.RData", tests, groups)

