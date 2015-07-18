context("getCaretParamSet")

test_that("getCaretParamSet", {
  requirePackages(c("caret", "rpart", "earth"))
  caret.learners = c("gbm", "rf", "svmPoly", "svmLinear", "svmRadial",
    "rpart", "J48", "stepLDA", "earth")
  checkCaretParams = function(lrn, k, x, y) {
    set.seed(123)
    a = capture.output({cps1 = getCaretParamSet(lrn, length = k, x = x, y = y, discretize = TRUE)})
    set.seed(123)
    b = capture.output({cps2 = getCaretParamSet(lrn, length = k, x = x, y = y, discretize = FALSE)})
    expect_identical(cps1$par.vals, cps2$par.vals)
    expect_identical(names(cps1$par.set$pars), names(cps2$par.set$pars))
    expect_identical(class(cps1$par.set), "ParamSet")
    expect_identical(class(cps2$par.set), "ParamSet")
    if (!is.null(cps1$par.vals))
      expect_identical(class(cps1$par.vals), "list")
  }

  # binaryclass problems
  x = subset(binaryclass.df, select = setdiff(colnames(binaryclass.df), binaryclass.target))
  y = subset(binaryclass.df, select = binaryclass.target)[[1]]
  r1 = lapply(caret.learners, checkCaretParams, k = 9, x = x, y = y)
  r2 = lapply(caret.learners, checkCaretParams, k = 5, x = x, y = y)

  # multiclass problems
  x = subset(multiclass.df, select = setdiff(colnames(multiclass.df), multiclass.target))
  y = subset(multiclass.df, select = multiclass.target)[[1]]
  r1 = lapply(caret.learners, checkCaretParams, k = 9, x = x, y = y)
  r2 = lapply(caret.learners, checkCaretParams, k = 5, x = x, y = y)

  # regression problems
  x = subset(regr.df, select = setdiff(colnames(regr.df), regr.target))
  y = subset(regr.df, select = regr.target)[[1]]
  caret.learners = c("gbm", "rf", "svmPoly", "svmLinear",
    "rpart", "J48", "stepLDA", "earth")
  r1 = lapply(caret.learners, checkCaretParams, k = 9, x = x, y = y)
  r2 = lapply(caret.learners, checkCaretParams, k = 5, x = x, y = y)
})
