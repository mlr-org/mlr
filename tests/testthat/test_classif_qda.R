context("classif_qda")

test_that("classif_qda", {
  requirePackagesOrSkip("MASS", default.method = "load")
  m = try(MASS::qda(formula = multiclass.formula, data = multiclass.train))
  if (class(m) != "try-error") {
   p = predict(m, newdata = multiclass.test)
  } else {
   p = m
  }

  testSimple("classif.qda", multiclass.df, multiclass.target, multiclass.train.inds, p$class)
  testProb("classif.qda", multiclass.df, multiclass.target, multiclass.train.inds, p$posterior)

  tt = MASS::qda
  tp = function(model, newdata) predict(model, newdata)$class

  testCV("classif.qda", multiclass.df, multiclass.target, tune.train = tt, tune.predict = tp)
})
