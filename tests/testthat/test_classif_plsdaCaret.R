context("classif_plsdaCaret")

test_that("classif_plsdaCaret", {
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
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(caret::plsda, pars)
    newx = binaryclass.test
    newx[, binaryclass.class.col] = NULL
    set.seed(getOption("mlr.debug.seed"))
    p = predict(m, newdata = newx, type = "class")
    set.seed(getOption("mlr.debug.seed"))
    p2 = predict(m, newdata = newx, type = "prob")[, 1, 1]
    old.predicts.list[[i]] = p
    old.probs.list[[i]] = p2
  }

  testSimpleParsets("classif.plsdaCaret", binaryclass.df, binaryclass.target, binaryclass.train.inds,
    old.predicts.list, parset.list)
  testProbParsets("classif.plsdaCaret", binaryclass.df, binaryclass.target, binaryclass.train.inds,
    old.probs.list, parset.list)

})
