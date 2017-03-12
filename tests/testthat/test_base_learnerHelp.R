
context("learnerHelp")

test_that("learnerHelp of learner with single help page", {
  expect_true(length(learnerHelp("classif.logreg")) == 1)
})

test_that("learnerHelp of learner with multiple help pages", {
  testfn = learnerHelp
  environment(testfn) = new.env(parent = environment(testfn))

  environment(testfn)$readline = function(x) { cat(x, "\n") ; 0 }

  expect_output(testfn("classif.qda"), "Choose help page:(\\n[0-9]+ : [0-9a-zA-Z._]+)+\\n\\.\\.\\.: *$")

  expect_null(testfn("classif.qda"))

  environment(testfn)$readline = function(x) { cat(x, "\n") ; 1 }

  hlp1 = testfn("classif.qda")

  hlp2 = testfn("classif.qda")

  # for regr.randomForest, there is mlr-specific help which should be the first option.
  expect_equivalent(help("regr.randomForest"), testfn("regr.randomForest"))

  environment(testfn)$readline = function(x) { cat(x, "\n") ; 2 }

  hlp3 = testfn("classif.qda")

  expect_identical(hlp1, hlp2)

  expect_false(identical(hlp1, hlp3))

})

test_that("learnerParamHelp", {
  # mention parameters
  expect_output(learnerParamHelp("classif.qda"), "method")
  expect_output(learnerParamHelp("classif.qda"), "nu")

  expect_output(learnerParamHelp("classif.qda", "nu"), "nu")

  # mention package
  expect_output(learnerParamHelp("classif.qda"), "MASS::qda")

  expect_output(learnerParamHelp("classif.qda", "nu"), "MASS::qda")

  # mention requirement

  nureq = capture.output(print(getParamSet("classif.qda")$pars$nu$requires))
  expect_output(learnerParamHelp("classif.qda", "nu"), paste("Requires:", nureq), fixed = TRUE)

  # error when giving unknown parameter
  expect_error(learnerParamHelp("classif.qda", "this_parameter_does_not_exist"))

  # message when querying parameter without documentation
  expect_output(learnerParamHelp("classif.__mlrmocklearners__2", "alpha"), "No documentation found")

  # check this doesn't give an error
  learnerParamHelp("classif.__mlrmocklearners__2")

  # check that values are printed
  expect_output(learnerParamHelp(
    makeLearner("classif.qda", nu = 3), "nu"),
    "Value: +3")

  # values for vectorial params work
  expect_output(learnerParamHelp(
    makeLearner("classif.randomForest", cutoff = c(.1, .2, .3)), "cutoff"),
    "Value:.+0\\.1.+0\\.2.+0\\.3")

})

