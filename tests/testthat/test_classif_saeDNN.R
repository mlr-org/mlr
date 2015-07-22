context("classif_saeDNN")

test_that("classif_saeDNN", {
  requirePackages("deepnet", default.method = "load")
  
  set.seed(getOption("mlr.debug.seed"))
  capture.output({
    # neuralnet is not dealing with formula with `.` well
    x = data.matrix(binaryclass.train[,-ncol(binaryclass.train)])
    y = binaryclass.train[,ncol(binaryclass.train)]
    
    dict = sort(unique(y))
    onehot = matrix(0, length(y), length(dict))
    for (i in 1:length(dict)) {
      ind = which(y == dict[i])
      onehot[ind,i] = 1
    }
    
    m = deepnet::sae.dnn.train(x = x, y = onehot, hidden = 7, output = "softmax")
    p = deepnet::nn.predict(m, data.matrix(binaryclass.test[,-ncol(binaryclass.test)]))
    colnames(p) = binaryclass.class.levs
    p = as.factor(colnames(p)[max.col(p)])
  })
  
  set.seed(getOption("mlr.debug.seed"))
  testSimple("classif.saeDNN", binaryclass.df, binaryclass.target, binaryclass.train.inds, p, 
             parset = list(hidden = 7))
})
