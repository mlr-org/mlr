context("classif_LiblineaRMultiClass")

test_that("classif_LiblineaRMultiClass", {
  requirePackages("LiblineaR")
  
  set.seed(getOption("mlr.debug.seed"))
  m = LiblineaR::LiblineaR(data=multiclass.train[, -multiclass.class.col],
                labels=multiclass.train[, multiclass.target],
                type=4)
  p = predict(m, newx=multiclass.test[, -multiclass.class.col])
  old.predicts = as.factor(p$predictions)
  
  testSimple("classif.LiblineaRMultiClass", multiclass.df,
             multiclass.target, multiclass.train.inds, old.predicts)
  
})
