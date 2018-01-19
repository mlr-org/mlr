context("classif_ada")

test_that("classif_ada", {
  requirePackagesOrSkip("ada", default.method = "load")

  parset.list = list(
    list(),
    list(iter = 5L)
  )

  old.predicts.list = list()
  old.probs.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    pars = list(binaryclass.formula, data = binaryclass.train)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(ada::ada, pars)
    set.seed(getOption("mlr.debug.seed"))
    p = predict(m, newdata = binaryclass.test, type = "probs")
    old.probs.list[[i]] = p[, 1]
    old.predicts.list[[i]] = as.factor(binaryclass.class.levs[ifelse(p[, 2] > 0.5, 2, 1)])
  }

  testSimpleParsets("classif.ada", binaryclass.df, binaryclass.target,
    binaryclass.train.inds, old.predicts.list, parset.list)

  testProbParsets("classif.ada", binaryclass.df, binaryclass.target,
    binaryclass.train.inds, old.probs.list, parset.list)
})

test_that("classif_ada passes parameters correctly to rpart.control (#732)", {
  cp.vals = c(0.022, 0.023)
  loss.vals = c("exponential", "logistic")
  for (cp in cp.vals) {
    for (loss in loss.vals) {
      lrn = makeLearner("classif.ada", cp = cp, loss = loss)
      mod = getLearnerModel(train(lrn, pid.task))
      expect_equal(mod$model$trees[[1]]$control$cp, cp)
      expect_equal(mod$model$lossObj$loss, loss)
    }
  }
})
