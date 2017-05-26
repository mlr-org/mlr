context("oneclass_ksvm")

test_that("oneclass_ksvm", {
  requirePackagesOrSkip("kernlab", default.method = "load")

  parset.list = list(
    list(),
    list(scaled = FALSE),
    list(kernel = "tanhdot", offset = 2),
    list(kernel = "polydot", degree = 3, offset = 2, scale = 1.5)
  )

  old.predicts.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    #pars = list(x = as.kernelMatrix(crossprod(t(oneclass.train[1:4]))))
    pars = list(x = as.matrix(oneclass.train[,1:4]))
    pars = c(pars, list(type = "one-svc"))
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    m1 = do.call(kernlab::ksvm, pars)
    #Ktest = as.kernelMatrix(crossprod(t(oneclass.test[1:4]), t(oneclass.train[1:4][SVindex(m1), ])))
    #Ktest =
    old.predicts.list[[i]] = kernlab::predict(m1, oneclass.test[,1:4], type = "response")
  }

   testSimpleParsets("oneclass.ksvm", oneclass.df, oneclass.target,
     oneclass.train.inds, old.predicts.list,  parset.list)
})
