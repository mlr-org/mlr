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
options(mlr.debug.seed=123L)
configureMlr(show.learner.output=FALSE)
test_dir("inst/tests")
