context("oneclass_svm")

# we cannot do a prob test, as set.seed sems not to work on e1071 svm for the prob parameters!
#requirePackagesOrSkip("e1071", default.method = "load")

test_that("oneclass_svm", {
  requirePackagesOrSkip("e1071", default.method = "load")

  parset.list = list(
    list(),
    list(gamma = 20),
    list(kernel = "sigmoid", gamma = 10),
    list(kernel = "polynomial", degree = 3, coef0 = 2, gamma = 1.5)
  )

  old.predicts.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    pars = list(x = oneclass.train[, -oneclass.col])
    pars = c(pars, list(type = "one-classification"))
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    m1 = do.call(e1071::svm, pars)
    pred = predict(m1, newdata = oneclass.test[, -oneclass.col])
    old.predicts.list[[i]] = factor(pred, levels = c("FALSE", "TRUE"), labels = c(oneclass.positive, oneclass.negative))
  }

   testSimpleParsets("oneclass.svm", oneclass.df, oneclass.target,
     oneclass.train.inds, old.predicts.list,  parset.list)
})
