context("regr_penalized_ridge")

test_that("regr_penalized_ridge", {
  requirePackagesOrSkip("!penalized", default.method = "load")

  parset.list = list(
    list(),
    list(lambda2 = 0.3),
    list(lambda2 = 1),
    list(lambda2 = 2)
  )

  old.predicts.list = list()
  old.probs.list = list()

  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    pars = list(regr.formula, data = regr.train, trace = FALSE)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    capture.output(
      m <- do.call(penalized::penalized, pars)
      )
    set.seed(getOption("mlr.debug.seed"))
    p = penalized::predict(m, data = regr.test)
    old.predicts.list[[i]] = p[,"mu"]
  }

  testSimpleParsets("regr.penalized.ridge", regr.df, regr.target,
    regr.train.inds, old.predicts.list, parset.list)

  tt = function(formula, data, subset = 1:nrow(data), ...) {
    penalized::penalized(formula, data = data[subset, ], fusedl = FALSE, ...)
  }

  tp = function(model, newdata, ...) {
    penalized::predict(model, data = newdata,...)[, "mu"]
  }

  testCVParsets("regr.penalized.ridge", regr.df, regr.target,
    tune.train = tt, tune.predict = tp, parset.list = parset.list)
})
