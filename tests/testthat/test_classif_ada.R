context("classif_ada")

test_that("classif_ada", {
  requirePackagesOrSkip("ada", default.method = "load")

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

test_that("classif_ada passes parameters correctly to rpart.control (#732)", {
    lrn = makeLearner("classif.ada", minsplit=20, minbucket=20, cp=0.01, maxcompete=4, maxsurrogate=5, usesurrogate=2, surrogatestyle=0, maxdepth=30, xval=10)
    train(lrn, binaryclass.task)
})
