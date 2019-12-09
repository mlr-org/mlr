context("classif_rrlda")

test_that("classif_rrlda", {
  requirePackagesOrSkip("!rrlda", default.method = "load")

  m = rrlda::rrlda(x = multiclass.train[, -multiclass.class.col],
    grouping = multiclass.train[, multiclass.target])
  p = predict(m, x = multiclass.test[, -multiclass.class.col])$class

  testSimple("classif.rrlda", multiclass.df, multiclass.target,
    multiclass.train.inds, p)
})
