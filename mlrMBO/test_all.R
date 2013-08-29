library(methods)
library(testthat)
library(devtools)
library(mlr)

if (interactive()) {
  load_all("skel", reset=TRUE)
} else {
  library(mlrMBO)  
}

configureMlr(show.learner.output=FALSE)
test_dir("skel/inst/tests/")
