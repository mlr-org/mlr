context("classif_xgboost")

test_that("classif_xgboost", {
  library(xgboost)
  set.seed(getOption("mlr.debug.seed"))
  d = xgb.DMatrix(data = as.matrix(Sonar[,-61]), label = as.integer(Sonar[,61]) - 1L)
  m = xgb.train(data = d, nrounds = 1, objective = "binary:logistic", num_class = 2)
  set.seed(getOption("mlr.debug.seed"))
  p = predict(m, newdata = as.matrix(Sonar[,-61]))
  
  testSimple("classif.lda", multiclass.df, multiclass.target, multiclass.train.inds, p$class)
  testProb  ("classif.lda", multiclass.df, multiclass.target, multiclass.train.inds, p$posterior)
  
  tt = "lda"
  tp = function(model, newdata) predict(model, newdata)$class
  
  testCV("classif.lda", multiclass.df, multiclass.target, tune.train = tt, tune.predict = tp)
})

library("devtools")
load_all()
 
source("tests/testthat/helper_objects.R")
# source("todo-files/RLearner_classif_xgboost.R")
# task = makeClassifTask("sonar", data = Sonar, target = "Class")
# library("Matrix")
# library("xgboost")
# lrn = makeLearner("classif.xgboost", nrounds = 1000, objective = "binary:logistic")
# r = makeResampleDesc("CV")
# rr = resample(learner = lrn, task = task, resampling = r)
# ri = makeResampleInstance(desc = makeResampleDesc("Holdout"), task = task)
# m = train(learner = lrn, task = task, subset = ri$train.inds[[1]])
# p = predict(m, task = subsetTask(task = .task, subset = ri$test.inds[[1]]))
# performance(p)
# 
# library(xgboost)
# d = matrix(runif(300), nrow = 100)
# y = as.integer(d[,1]*2)
# #y = sample(0:1, size = 100, replace = T)
# fy = factor(letters)
# d = xgb.DMatrix(data = d, label = y)
# m = xgb.train(data = d, nrounds = 10, objective = "binary:logistic", num_class = 3)
# p = predict(m, newdata = d)
# p
# cbind(p, y)
