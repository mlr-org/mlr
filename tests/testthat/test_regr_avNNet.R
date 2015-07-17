context("regr_avNNet")

test_that("regr_avNNet", {
  requirePackages("nnet", default.method = "load")
  
  set.seed(getOption("mlr.debug.seed"))
  repeats = 5
  pred = 0
  for (i in 1:repeats) {
    model = nnet::nnet(regr.formula, data = regr.train, size = 7, linout = TRUE)
    pred = pred+predict(model, regr.test)
  }
  pred = as.vector(pred/repeats)
  
  set.seed(getOption("mlr.debug.seed"))
  testSimple("regr.avNNet", regr.df, regr.target, regr.train.inds, pred, 
    parset = list(size = 7))
})
