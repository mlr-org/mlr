context("oneclass_ksvm")

test_that("oneclass_ksvm", {
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

  for (i in seq_along(parset.list1)) {
    parset = parset.list1[[i]]
    pars = list(x = as.matrix(oneclass.train[, -oneclass.col]))
    pars = c(pars, list(type = "one-svc"))
    pars = c(pars, parset)
    pars$prob.model = TRUE

    set.seed(getOption("mlr.debug.seed"))
    m = do.call(kernlab::ksvm, pars)
    pred =  kernlab::predict(m, oneclass.test[, -oneclass.col], type = "response")
    old.predicts.list[[i]] = factor(pred, levels = c("FALSE", "TRUE"), labels = c(oneclass.positive, oneclass.negative))
  }

  testSimpleParsets("oneclass.ksvm", oneclass.df, oneclass.target,
    oneclass.train.inds, old.predicts.list,  parset.list2)
})
