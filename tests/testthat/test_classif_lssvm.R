context("classif_lssvm")

test_that("classif_lssvm", {
  requirePackagesOrSkip("kernlab", default.method = "load")

  parset.list1 = list(
    list(fit = FALSE),
    list(fit = FALSE, kpar = list(sigma = 20)),
    list(fit = FALSE, kernel = "laplacedot", kpar = list(sigma = 10))
  )

  parset.list2 = list(
    list(),
    list(sigma = 20),
    list(kernel = "laplacedot", sigma = 10)
  )

  old.predicts.list = list()

  for (i in seq_along(parset.list1)) {
    parset = parset.list1[[i]]
    pars = list(x = multiclass.formula, data = multiclass.train)
    pars = c(pars, parset)
    #set.seed(getOption("mlr.debug.seed"))
    m = do.call(kernlab::lssvm, pars)
    old.predicts.list[[i]] = kernlab::predict(m, newdata = multiclass.test)
  }

  testSimpleParsets("classif.lssvm", multiclass.df, multiclass.target,
    multiclass.train.inds, old.predicts.list, parset.list2)


  # Bug in kernel = "polydot"
  set.seed(getOption("mlr.debug.seed"))
  # m = kernlab::lssvm(x=multiclass.formula, data=multiclass.train, kernel="polydot", kpar=list(degree=3, offset=2, scale=1.5))
  # p = kernlab::predict(m, newdata=multiclass.test)
  # testSimple("classif.lssvm", multiclass.df, multiclass.target, multiclass.train.inds, p,  parset=list(kernel="polydot", degree=3, offset=2, scale=1.5))

  tt = function(formula, data, subset = 1:150, ...) {
    kernlab::lssvm(x = formula, data = data[subset, ], kernel = "rbfdot", kpar = list(sigma = 20))
  }

  tp = function(model, newdata, ...) {
    kernlab::predict(model, newdata = newdata)
  }

  testCV("classif.lssvm", multiclass.df, multiclass.target, tune.train = tt, tune.predict = tp,
    parset = list(kernel = "rbfdot", sigma = 20))
})
