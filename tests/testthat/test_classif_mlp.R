context("classif_mlp")

test_that("classif_mlp", {
  requirePackages("RSNNS", default.method = "load")
  
  set.seed(getOption("mlr.debug.seed"))
  capture.output({
    # neuralnet is not dealing with formula with `.` well
    x = data.matrix(binaryclass.train[,-ncol(binaryclass.train)])
    y = RSNNS::decodeClassLabels(binaryclass.train[,ncol(binaryclass.train)])
    m = RSNNS::mlp(x = x, y = y, size = 7, maxit = 100)
    p = predict(m, data.matrix(binaryclass.test[,-ncol(binaryclass.test)]))
    p = max.col(p)
    p = factor(p, labels = binaryclass.class.levs)
  })
  
  set.seed(getOption("mlr.debug.seed"))
  testSimple("classif.mlp", binaryclass.df, binaryclass.target, binaryclass.train.inds, p, 
             parset = list(size = 7, maxit = 100))
  # Neuralnet doesn't have the `predict` method
  #   set.seed(getOption("mlr.debug.seed"))
  #   lrn = makeLearner("classif.neuralnet",hidden=7)
  #   task = makeClassifTask(data = binaryclass.df, target = binaryclass.target)
  #   m2 = try(train(lrn, task, subset = binaryclass.train.inds))
  #   p2 = predictLearner(.learner=lrn,.model=m2,
  #                       .newdata = binaryclass.test[,-ncol(binaryclass.test)])
  #   expect_equal(p,p2,tol=1e-4)
})
