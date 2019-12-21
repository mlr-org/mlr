context("classif_plsdaCaret")

test_that("classif_plsdaCaret_binary", {
  requirePackagesOrSkip("caret", default.method = "load")
  parset.list = list(
    list(),
    list(ncomp = 4),
    list(probMethod = "Bayes"),
    list(method = "oscorespls")
  )

  old.predicts.list = list()
  old.probs.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    x = binaryclass.train
    y = x[, binaryclass.class.col]
    x[, binaryclass.class.col] = NULL
    pars = list(x = x, y = y)
    pars = c(pars, parset)
    m = do.call(caret::plsda, pars)
    newx = binaryclass.test
    newx[, binaryclass.class.col] = NULL
    p = predict(m, newdata = newx, type = "class")
    p2 = predict(m, newdata = newx, type = "prob")[, 1, 1]
    old.predicts.list[[i]] = p
    old.probs.list[[i]] = p2
  }

  testSimpleParsets("classif.plsdaCaret", binaryclass.df, binaryclass.target,
    binaryclass.train.inds,
    old.predicts.list, parset.list)
  testProbParsets("classif.plsdaCaret", binaryclass.df, binaryclass.target,
    binaryclass.train.inds,
    old.probs.list, parset.list)
})

test_that("classif_plsdaCaret_multiclass", {
  requirePackagesOrSkip("caret", default.method = "load")
  parset.list = list(
    list(),
    list(ncomp = 4),
    list(probMethod = "Bayes"),
    list(method = "oscorespls")
  )

  old.predicts.list = list()
  old.probs.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    pars = list(x = multiclass.train[, names(multiclass.train) %nin%
      multiclass.target],
    y = multiclass.train[, multiclass.target])
    pars = c(pars, parset)
    m = do.call(caret::plsda, pars)
    newx = multiclass.test
    newx[, multiclass.class.col] = NULL
    p = predict(m, newdata = newx, type = "class")
    p2 = predict(m, newdata = newx, type = "prob")[, , 1]
    old.predicts.list[[i]] = p
    old.probs.list[[i]] = p2
  }

  testSimpleParsets("classif.plsdaCaret", multiclass.df, multiclass.target,
    multiclass.train.inds, old.predicts.list, parset.list)
  testProbParsets("classif.plsdaCaret", multiclass.df, multiclass.target,
    multiclass.train.inds, old.probs.list, parset.list)
})
