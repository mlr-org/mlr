context("classif_avNNet")

test_that("classif_avNNet", {
  requirePackages("nnet", default.method = "load")
  
  set.seed(getOption("mlr.debug.seed"))
  repeats = 5
  pred = 0
  for (i in 1:repeats) {
    model = nnet::nnet(binaryclass.formula, data = binaryclass.train, size = 3)
    pred = pred+predict(model, binaryclass.test)
  }
  pred = pred/repeats
  pred = factor(as.numeric(pred>0.5), labels = binaryclass.class.levs)
  
  set.seed(getOption("mlr.debug.seed"))
  testSimple("classif.avNNet", binaryclass.df, binaryclass.target, binaryclass.train.inds, pred, 
    parset = list(size = 3))
})
