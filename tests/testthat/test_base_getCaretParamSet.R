context("getCaretParamSet")

test_that("getCaretParamSet", {
  requirePackagesOrSkip(c("caret", "rpart", "earth"))
  checkCaretParams = function(lrn, k, task) {

    set.seed(getOption("mlr.debug.seed"))
    capture.output({
      cps1 = getCaretParamSet(lrn, length = k, task = task, discretize = TRUE)
    })
    set.seed(getOption("mlr.debug.seed"))
    capture.output({
      cps2 = getCaretParamSet(lrn, length = k, task = task, discretize = FALSE)
    })
    expect_identical(cps1$par.vals, cps2$par.vals)
    expect_identical(names(cps1$par.set$pars), names(cps2$par.set$pars))
    expect_identical(class(cps1$par.set), "ParamSet")
    expect_identical(class(cps2$par.set), "ParamSet")

    if (!is.null(cps1$par.vals)) {
      expect_identical(class(cps1$par.vals), "list")
    }
  }

  caret.learners = c("gbm", "rf", "svmPoly", "svmLinear", "svmRadial",
    "rpart", "J48", "stepLDA", "earth")

  # binaryclass problems
  r1 = lapply(caret.learners, checkCaretParams, k = 9, task = binaryclass.task)
  r2 = lapply(caret.learners, checkCaretParams, k = 5, task = binaryclass.task)

  # multiclass problems
  r1 = lapply(caret.learners, checkCaretParams, k = 9, task = multiclass.task)
  r2 = lapply(caret.learners, checkCaretParams, k = 5, task = multiclass.task)

  # regression problems
  caret.learners = c("gbm", "rf", "svmPoly", "svmLinear",
    "rpart", "J48", "stepLDA", "earth")
  r1 = lapply(caret.learners, checkCaretParams, k = 9, task = regr.task)
  r2 = lapply(caret.learners, checkCaretParams, k = 5, task = regr.task)
})
