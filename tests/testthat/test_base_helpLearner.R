
context("helpLearner")

test_that("helpLearner of learner with single help page", {
  testfn = helpLearner
  environment(testfn) = new.env(parent = environment(testfn))

  environment(testfn)$readline = function(x) stop("Was not expecting readline.")

  expect_true(length(quiet(testfn("classif.logreg"))) == 1)
})

test_that("helpLearner of learner with multiple help pages", {
  testfn = helpLearner
  environment(testfn) = new.env(parent = environment(testfn))

  environment(testfn)$readline = function(x) {
    cat(x, "\n")
    0
  }

  expect_output(testfn("classif.qda"), "Choose help page:(\\n[0-9]+ : [0-9a-zA-Z._]+)+\\n\\.\\.\\.: *$")

  expect_null(quiet(testfn("classif.qda")))

  environment(testfn)$readline = function(x) {
    cat(x, "\n")
    1
  }

  hlp1 = quiet(testfn("classif.qda"))

  hlp2 = quiet(testfn("classif.qda"))

  # for regr.randomForest, there is mlr-specific help which should be the first option.
  rfhelp = utils::help("regr.randomForest", package = "mlr")
  # unfortunately, rtest breaks help("regr.randomForest"), so we skip this test if help() is broken.
  if (length(rfhelp) > 0) {
    expect_equivalent(rfhelp, quiet(testfn("regr.randomForest")))
  }

  environment(testfn)$readline = function(x) {
    cat(x, "\n")
    2
  }

  hlp3 = quiet(testfn("classif.qda"))

  expect_identical(hlp1, hlp2)

  expect_false(identical(hlp1, hlp3))

  # regr.randomForest with option '2' should give the randomForest help page.
  expect_true(length(quiet(testfn("regr.randomForest"))) == 1)
})

test_that("helpLearner of wrapped learner", {
  testfn = helpLearner
  environment(testfn) = new.env(parent = environment(testfn))

  environment(testfn)$readline = function(x) stop("Was not expecting readline.")

  # check that it doesn't give an error
  expect_output(testfn(makeBaggingWrapper(makeLearner("classif.qda"), 2)), "No information about learner")
})

test_that("helpLearnerParam", {
  # mention parameters
  expect_output(helpLearnerParam("classif.qda"), "method")
  expect_output(helpLearnerParam("classif.qda"), "nu")

  expect_output(helpLearnerParam("classif.qda", "nu"), "nu")

  # mention package
  expect_output(helpLearnerParam("classif.qda"), "MASS::qda")

  expect_output(helpLearnerParam("classif.qda", "nu"), "MASS::qda")

  # mention requirement

  nureq = capture.output(print(getParamSet("classif.qda")$pars$nu$requires))
  expect_output(helpLearnerParam("classif.qda", "nu"), paste("Requires:", nureq), fixed = TRUE)

  # error when giving unknown parameter
  expect_error(helpLearnerParam("classif.qda", "this_parameter_does_not_exist"))

  # message when querying parameter without documentation
  expect_output(helpLearnerParam("classif.__mlrmocklearners__2", "alpha"), "No documentation found")

  # check this doesn't give an error
  quiet(helpLearnerParam("classif.__mlrmocklearners__2"))

  # check that values are printed
  expect_output(helpLearnerParam(
    makeLearner("classif.qda", nu = 3), "nu"),
  "Value: +3")

  # values for vectorial params work
  expect_output(helpLearnerParam(
    makeLearner("classif.randomForest", cutoff = c(.1, .2, .3)), "cutoff"),
  "Value:.+0\\.1.+0\\.2.+0\\.3")
})

test_that("helpLearnerParam of wrapped learner", {
  w1 = makeBaggingWrapper(makeLearner("classif.qda", nu = 4), 2)
  w2 = makeOversampleWrapper(w1)

  # correct info is given
  expect_output(helpLearnerParam(w1, "nu"), "Value: +4")
  expect_output(helpLearnerParam(w2, "nu"), "Value: +4")

  expect_message(quiet(helpLearnerParam(w1)),
    "is a wrapped learner. Showing documentation of 'classif.qda' instead", fixed = TRUE, all = TRUE)
  expect_message(quiet(helpLearnerParam(w2)),
    "is a wrapped learner. Showing documentation of 'classif.qda' instead", fixed = TRUE, all = TRUE)
})
