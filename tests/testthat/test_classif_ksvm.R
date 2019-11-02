context("classif_ksvm")

test_that("classif_ksvm", {
  requirePackagesOrSkip("kernlab", default.method = "load")

  parset.list1 = list(
    list(fit = FALSE),
    list(kpar = list(sigma = 20), fit = FALSE),
    list(kernel = "laplacedot", kpar = list(sigma = 10), fit = FALSE),
    list(kernel = "polydot", kpar = list(degree = 3, offset = 2, scale = 1.5))
  )

  parset.list2 = list(
    list(),
    list(sigma = 20),
    list(kernel = "laplacedot", sigma = 10),
    list(kernel = "polydot", degree = 3, offset = 2, scale = 1.5)
  )

  old.predicts.list = list()
  old.probs.list = list()

  for (i in seq_along(parset.list1)) {
    parset = parset.list1[[i]]
    pars = list(x = multiclass.formula, data = multiclass.train)
    pars = c(pars, parset)
    pars$prob.model = TRUE

    set.seed(getOption("mlr.debug.seed"))
    m = do.call(kernlab::ksvm, pars)
    old.predicts.list[[i]] = kernlab::predict(m, newdata = multiclass.test)
    old.probs.list[[i]] = kernlab::predict(m, newdata = multiclass.test, type = "prob")
  }

  testSimpleParsets("classif.ksvm", multiclass.df, multiclass.target,
    multiclass.train.inds, old.predicts.list, parset.list2)
  testProbParsets("classif.ksvm", multiclass.df, multiclass.target,
    multiclass.train.inds, old.probs.list, parset.list2)

  tt = function(formula, data, subset = 1:150, ...) {
    kernlab::ksvm(x = formula, data = data[subset, ], kernel = "polydot",
      kpar = list(degree = 3, offset = 2, scale = 1.5))
  }
  tp = function(model, newdata, ...) {
    kernlab::predict(model, newdata = newdata)
  }

  testCV("classif.ksvm", multiclass.df, multiclass.target, tune.train = tt, tune.predict = tp,
    parset = list(kernel = "polydot", degree = 3, offset = 2, scale = 1.5))
})
