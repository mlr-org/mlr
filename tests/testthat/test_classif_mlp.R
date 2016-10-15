context("classif_mlp")

test_that("classif_mlp", {
  requirePackagesOrSkip("RSNNS", default.method = "load")
  
  parset.list = list(
    list()
    #list(size = 7L, maxit = 150L)
  )
  
  old.predicts.list = list()
  
  for (i in length(parset.list)){
    parset = parset.list[[i]]
    # neuralnet is not dealing with formula with `.` well
    # capture.output({
    x = data.matrix(binaryclass.train[,-ncol(binaryclass.train)])
    y = RSNNS::decodeClassLabels(binaryclass.train[,ncol(binaryclass.train)])
    pars = list(x = x, y = y)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(RSNNS::mlp, pars)
    #m = RSNNS::mlp(x = x, y = y)
    p = predict(m, data.matrix(binaryclass.test[,-ncol(binaryclass.test)]))
    p = max.col(p)
    old.predicts.list[[i]] = factor(p, labels = binaryclass.class.levs)
    #})
  }
  
  # set.seed(getOption("mlr.debug.seed"))
  testSimpleParsets("classif.mlp", binaryclass.df, binaryclass.target,
    binaryclass.train.inds, old.predicts.list, parset.list)
  
#   set.seed(getOption("mlr.debug.seed"))
#   capture.output({
#     # neuralnet is not dealing with formula with `.` well
#     x = data.matrix(binaryclass.train[,-ncol(binaryclass.train)])
#     y = RSNNS::decodeClassLabels(binaryclass.train[,ncol(binaryclass.train)])
#     m = RSNNS::mlp(x = x, y = y, size = 7, maxit = 100)
#     p = predict(m, data.matrix(binaryclass.test[,-ncol(binaryclass.test)]))
#     p = max.col(p)
#     p = factor(p, labels = binaryclass.class.levs)
#   #})
#   
#   set.seed(getOption("mlr.debug.seed"))
#   testSimple("classif.mlp", binaryclass.df, binaryclass.target, binaryclass.train.inds, p,
#     parset = list(size = 7, maxit = 100))

})
