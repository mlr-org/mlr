context("classif_ksvm")

test_that("classif_ksvm", {
  requirePackages("kernlab", default.method = "load")
  set.seed(getOption("mlr.debug.seed"))
  m = kernlab::ksvm(x=multiclass.formula, data=multiclass.train, kernel="rbfdot", kpar=list(sigma=20), prob.model = T)
  p =  kernlab::predict(m, newdata=multiclass.test)
  p2 = kernlab::predict(m, newdata=multiclass.test, type="prob")
  testSimple("classif.ksvm", multiclass.df, multiclass.target, multiclass.train.inds, p,  parset=list(sigma=20))
  testProb  ("classif.ksvm", multiclass.df, multiclass.target, multiclass.train.inds, p2, parset=list(sigma=20))

  set.seed(getOption("mlr.debug.seed"))
  m = kernlab::ksvm(x=multiclass.formula, data=multiclass.train, kernel="laplacedot", kpar=list(sigma=10), prob.model = T)
  p = kernlab::predict(m, newdata=multiclass.test)
  p2 = kernlab::predict(m, newdata=multiclass.test, type="prob")
  testSimple("classif.ksvm",multiclass.df, multiclass.target, multiclass.train.inds, p,  parset=list(kernel="laplacedot", sigma=10))
  testProb  ("classif.ksvm",multiclass.df, multiclass.target, multiclass.train.inds, p2, parset=list(kernel="laplacedot", sigma=10))

  set.seed(getOption("mlr.debug.seed"))
  m = kernlab::ksvm(x=multiclass.formula, data=multiclass.train, kernel="polydot", kpar=list(degree=3, offset=2, scale=1.5), prob.model = T)
  p = kernlab::predict(m, newdata=multiclass.test)
  p2 = kernlab::predict(m, newdata=multiclass.test, type="prob")
  testSimple("classif.ksvm", multiclass.df, multiclass.target, multiclass.train.inds, p,  parset=list(kernel="polydot", degree=3, offset=2, scale=1.5))
  testProb  ("classif.ksvm", multiclass.df, multiclass.target, multiclass.train.inds, p2, parset=list(kernel="polydot", degree=3, offset=2, scale=1.5))

  tt = function (formula, data, subset=1:150, ...) {
    kernlab::ksvm(x=formula, data=data[subset,], kernel="polydot", kpar=list(degree=3, offset=2, scale=1.5))
  }
  tp = function (model, newdata, ...) {
    kernlab::predict(model, newdata=newdata)
  }

  testCV("classif.ksvm", multiclass.df, multiclass.target, tune.train=tt, tune.predict=tp,
    parset=list(kernel="polydot", degree=3, offset=2, scale=1.5))

})
