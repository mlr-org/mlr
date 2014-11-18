context("classif_xgboost")

test_that("classif_xgboost", {
  library(xgboost)
  
  # binary class
  set.seed(getOption("mlr.debug.seed"))
  train.inds = sample(seq_row(Sonar), size = nrow(Sonar)*0.6, replace = FALSE)
  set.seed(getOption("mlr.debug.seed"))
  d = xgb.DMatrix(data = as.matrix(Sonar[train.inds,-61]), label = as.integer(Sonar[train.inds,61]) - 1L)
  m = xgb.train(data = d, nrounds = 1, objective = "binary:logistic")
  set.seed(getOption("mlr.debug.seed"))
  p = predict(m, newdata = as.matrix(Sonar[-train.inds,-61]))
  p.class = factor(ifelse(p < 0.5, "M", "R"))
  p.mat = propVectorToMatrix(p = p, c("M", "M"))
  
  testSimple("classif.xgboost", Sonar, "Class", train.inds, p.class)
  testProb  ("classif.xgboost", Sonar, "Class", train.inds, p.mat[,1])
  
  # mulit class
  data = xgb.DMatrix(
    data = as.matrix(multiclass.train[multiclass.train.inds,-multiclass.class.col]), 
    label = as.integer(multiclass.train[multiclass.train.inds,multiclass.class.col]) - 1L)
  
  set.seed(getOption("mlr.debug.seed"))
  levs = levels(multiclass.df[, multiclass.class.col])
  m1 = xgb.train(
    data = data,
    nrounds = 1, objective = "multi:softmax", num_class = length(levs))
  set.seed(getOption("mlr.debug.seed"))
  p = predict(m1, newdata = as.matrix(multiclass.test[,-multiclass.class.col]))
  p.class = factor(levs[p + 1], levels = levs)
  # testSimple("classif.xgboost", multiclass.df, multiclass.target, multiclass.train.inds, p.class)
  
  set.seed(getOption("mlr.debug.seed"))
  m2 = xgb.train(
    data = data,
    nrounds = 1, objective = "multi:softprob", num_class = length(levs))
  set.seed(getOption("mlr.debug.seed"))
  p = predict(m2, newdata = as.matrix(multiclass.test[,-multiclass.class.col]))
  p.probs = matrix(p, ncol = length(levs), byrow = TRUE)
  # testProb  ("classif.xgboost", multiclass.df, multiclass.target, multiclass.train.inds, p.probs)
})