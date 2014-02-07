library(testthat)
library(reshape)
library(mlr)

# source(system.file("tests/helpers.R", package="mlr"))
# source(system.file("tests/objects.R", package="mlr"))

options(mlr.debug.seed=123L)
configureMlr(show.learner.output=FALSE)

test_package("mlr")
