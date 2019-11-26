context("classif_lda")

test_that("classif_lda", {
  requirePackagesOrSkip("MASS", default.method = "load")

  m = MASS::lda(formula = multiclass.formula, data = multiclass.train)
  p = predict(m, newdata = multiclass.test)

  testSimple("classif.lda", multiclass.df, multiclass.target, multiclass.train.inds, p$class)
  testProb("classif.lda", multiclass.df, multiclass.target, multiclass.train.inds, p$posterior)

  tt = MASS::lda
  tp = function(model, newdata) predict(model, newdata)$class

  testCV("classif.lda", multiclass.df, multiclass.target, tune.train = tt, tune.predict = tp)
})
