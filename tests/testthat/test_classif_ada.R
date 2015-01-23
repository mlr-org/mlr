context("classif_ada")

test_that("classif_ada", {
  requirePackages("ada", default.method = "load")
  set.seed(getOption("mlr.debug.seed"))
  m = ada::ada(formula = binaryclass.formula, data = binaryclass.train, iter = 5L)
  set.seed(getOption("mlr.debug.seed"))
  p = predict(m, newdata = binaryclass.test, type = "prob")
  p.class = as.factor(binaryclass.class.levs[ifelse(p[,2] > 0.5, 2, 1)])

  testSimple("classif.ada", binaryclass.df, binaryclass.target,
    binaryclass.train.inds, p.class, parset = list(iter = 5L))

  p = p[,1]
  testProb("classif.ada", binaryclass.df, binaryclass.target,
    binaryclass.train.inds, p, parset = list(iter = 5L))

})

