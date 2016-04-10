context("classif_penalized_lasso")

test_that("classif_penalized_lasso", {
  requirePackages("!penalized", default.method = "load")
  parset.list = list(
    list(),
    list(lambda1 = 0.3),
    list(lambda1 = 1),
    list(lambda1 = 2)
  )
  old.probs.list = list()

  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    pars = list(binaryclass.formula, data = binaryclass.train, trace = FALSE)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    capture.output(
      m <- do.call(penalized::penalized, pars)
    )
    old.probs.list[[i]] = 1 - penalized::predict(m, data = binaryclass.test)
  }
  testProbParsets("classif.penalized.lasso", binaryclass.df, binaryclass.target,
    binaryclass.train.inds, old.probs.list, parset.list)

  tt = function(formula, data, subset = 1:nrow(data), ...) {
    penalized::penalized(formula, data = data[subset, ],
      fusedl = FALSE, trace = FALSE, ...)
  }

  tp = function(model, newdata, ...) {
    pred = penalized::predict(model, data = newdata,...)
    ifelse(pred > 0.5, binaryclass.class.levs[2L], binaryclass.class.levs[1L])
  }
  
  testCVParsets("classif.penalized.lasso", binaryclass.df, binaryclass.target,
    tune.train = tt, tune.predict = tp, parset.list = parset.list)
})
