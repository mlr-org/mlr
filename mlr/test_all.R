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

if (interactive()) {
  load_all("skel")
} else {
  library(mlr)  
}

source("skel/inst/tests/helpers.R")
source("skel/inst/tests/objects.R")
options(mlr.debug.seed=123L)
configureMlr(show.learner.output=FALSE)
test_dir("skel/inst/tests")
