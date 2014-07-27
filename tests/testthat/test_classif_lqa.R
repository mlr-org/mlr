context("classif_lqa")

test_that("classif_lqa", {
  library(lqa)
  parset.list = list(
    list(),
    list(penalty = 'lasso', lambda = 0.01),
    list(penalty = 'fused.lasso', lambda1 = 0.001, lambda2 = 0.01)
  )

  old.predicts.list = list()
  old.probs.list = list()

  x = binaryclass.train
  y = as.numeric(x[, binaryclass.class.col]) - 1
  x[, binaryclass.class.col] = NULL
  newx = cbind(1, binaryclass.test)
  newx[, binaryclass.target] = NULL
  
  old.probs.list[[1]] = predict(lqa(x, y, family = binomial(), penalty = lasso(0.1)), newx)$mu.new
  old.probs.list[[2]] = predict(lqa(x, y, family = binomial(), penalty = lasso(0.01)), newx)$mu.new
  old.probs.list[[3]] = predict(lqa(x, y, family = binomial(), penalty = fused.lasso(c(0.001, 0.01))), newx)$mu.new
  
  old.predicts.list[[1]] =  factor(old.probs.list[[1]] > 0.5, c(TRUE, FALSE), binaryclass.class.levs)
  old.predicts.list[[2]] =  factor(old.probs.list[[2]] > 0.5, c(TRUE, FALSE), binaryclass.class.levs)
  old.predicts.list[[3]] =  factor(old.probs.list[[3]] > 0.5, c(TRUE, FALSE), binaryclass.class.levs)


  testSimpleParsets("classif.lqa", binaryclass.df, binaryclass.target, binaryclass.train.inds,
    old.predicts.list, parset.list)
  testProbParsets ("classif.lqa", binaryclass.df, binaryclass.target, binaryclass.train.inds,
    old.probs.list, parset.list)

})
