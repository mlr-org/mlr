context("classif_naiveBayes")

test_that("classif_naiveBayes", {
  requirePackagesOrSkip("e1071", default.method = "load")

  m = e1071::naiveBayes(formula = multiclass.formula, data = multiclass.train)
  p  = predict(m, newdata = multiclass.test[, -multiclass.class.col])
  p2 = predict(m, newdata = multiclass.test[, -multiclass.class.col], type = "raw")

  testSimple("classif.naiveBayes", multiclass.df, multiclass.target, multiclass.train.inds, p)
  testProb("classif.naiveBayes", multiclass.df, multiclass.target, multiclass.train.inds, p2)

  tt = e1071::naiveBayes
  tp = function(model, newdata) predict(model, newdata[, -multiclass.class.col])

  testCV("classif.naiveBayes", multiclass.df, multiclass.target, tune.train = tt, tune.predict = tp)
})
