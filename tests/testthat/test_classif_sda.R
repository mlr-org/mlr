context("classif_sda")

test_that("classif_sda", {
  requirePackagesOrSkip("sda", default.method = "load")

  capture.output({
    set.seed(getOption("mlr.debug.seed"))
    m = sda::sda(as.matrix(dropNamed(multiclass.train, multiclass.target)), multiclass.train[, multiclass.target])
    set.seed(getOption("mlr.debug.seed"))
    p = sda::predict.sda(m, as.matrix(dropNamed(multiclass.test, multiclass.target)))
  })

  testSimple("classif.sda", multiclass.df, multiclass.target, multiclass.train.inds, p$class)
  testProb("classif.sda", multiclass.df, multiclass.target, multiclass.train.inds, p$posterior)
})
