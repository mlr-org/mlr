context("classif_lssvm")

test_that("classif_lssvm", {
  requirePackages("kernlab", default.method = "load")
  set.seed(getOption("mlr.debug.seed"))
  m = kernlab::lssvm(x=multiclass.formula, data=multiclass.train, kernel="rbfdot", kpar=list(sigma=20))
  p = kernlab::predict(m, newdata=multiclass.test)
  testSimple("classif.lssvm", multiclass.df, multiclass.target, multiclass.train.inds, p,  parset=list(sigma=20))

  set.seed(getOption("mlr.debug.seed"))
  m = kernlab::lssvm(x=multiclass.formula, data=multiclass.train, kernel="laplacedot", kpar=list(sigma=10))
  p = kernlab::predict(m, newdata=multiclass.test)
  testSimple("classif.lssvm",multiclass.df, multiclass.target, multiclass.train.inds, p,  parset=list(kernel="laplacedot", sigma=10))

  # Bug in kernel = "polydot"
  set.seed(getOption("mlr.debug.seed"))
  # m = kernlab::lssvm(x=multiclass.formula, data=multiclass.train, kernel="polydot", kpar=list(degree=3, offset=2, scale=1.5))
  # p = kernlab::predict(m, newdata=multiclass.test)
  # testSimple("classif.lssvm", multiclass.df, multiclass.target, multiclass.train.inds, p,  parset=list(kernel="polydot", degree=3, offset=2, scale=1.5))

  tt = function (formula, data, subset=1:150, ...) {
    kernlab::lssvm(x=formula, data=data[subset,], kernel="rbfdot", kpar=list(sigma=20))
  }

  tp = function (model, newdata, ...) {
    kernlab::predict(model, newdata=newdata)
  }

  testCV("classif.lssvm", multiclass.df, multiclass.target, tune.train=tt, tune.predict=tp,
    parset=list(kernel="rbfdot", sigma=20))
})
