context("FDA_base_benchmark")

test_that("FDA_base_benchmark", {
  configureMlr(show.info = TRUE)

  # # start with a simple CART tree (its simple and fast)
  lrn = makeLearner("classif.rpart")

  # # train, predict, measure perf, all manual
  # # (and use complete data, so insample eval....)
  model = train(lrn, gunpoint.task, subset = 1:50)
  pred = predict(model, gunpoint.task, subset = 51:200)
  p = performance(pred, measures = list(mmce, tpr))
  print(p)

  # create the sample GP splits as on UCR page, they used the first 50 for train, last 150 for test
  # then resample (this is holdout eval)
  rin = makeFixedHoldoutInstance(size = 200, train.inds = 1:50, test.inds = 51:200)
  r = resample(lrn, gunpoint.task, rin)
  print(r)

  # lets benchmark the tree vs an RBF SVM
  learners = list(
    makeLearner("classif.rpart"),
    makeLearner("classif.ksvm", C = 1, sigma = 0.01)
  )
  b = benchmark(learners, gunpoint.task, rin)
  print(b)


  # here are some R examples for feature extraction!
  # http://www.rdatamining.com/examples/time-series-clustering-classification

  # library(wavelets)
  # wtData <- NULL
  # for (i in 1:nrow(sc)) {
  #   a <- t(sc[i,])
  #   wt <- dwt(a, filter=”haar”, boundary=”periodic”)
  #   wtData <- rbind(wtData, unlist(c(wt@W,wt@V[[wt@level]])))
  # }
  #  wtData <- as.data.frame(wtData)
})
