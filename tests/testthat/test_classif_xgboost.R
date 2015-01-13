context("classif_xgboost")

test_that("classif_xgboost", {
  requirePackages("xgboost")

  # binary class
  set.seed(getOption("mlr.debug.seed"))
  train.inds = sample(seq_row(Sonar), size = nrow(Sonar)*0.6, replace = FALSE)
  set.seed(getOption("mlr.debug.seed"))
  d = xgboost::xgb.DMatrix(data = as.matrix(Sonar[train.inds,-61]), label = as.integer(Sonar[train.inds,61]) - 1L)
  m = xgboost::xgb.train(data = d, nrounds = 1, objective = "binary:logistic")
  set.seed(getOption("mlr.debug.seed"))
  p = xgboost::predict(m, newdata = as.matrix(Sonar[-train.inds,-61]))
  p.class = factor(ifelse(p < 0.5, "M", "R"))
  p.mat = propVectorToMatrix(p, c("M", "R"))

  testSimple("classif.xgboost", Sonar, "Class", train.inds, p.class)
  testProb  ("classif.xgboost", Sonar, "Class", train.inds, p.mat[,1])

  # mulit class
  lrn = makeLearner("classif.xgboost")
  set.seed(getOption("mlr.debug.seed"))
  m = train(learner = lrn, task = multiclass.task, subset = multiclass.train.inds)

  lrn2 = setPredictType(lrn, "prob")
  set.seed(getOption("mlr.debug.seed"))
  m = train(learner = lrn2, task = multiclass.task, subset = multiclass.train.inds)
  set.seed(getOption("mlr.debug.seed"))
  p = xgboost::predict(m, newdata = multiclass.test)
})

#   set.seed(getOption("mlr.debug.seed"))
#   m2 = xgb.train(
#     data = data,
#     nrounds = 1, objective = "multi:softprob", num_class = length(levs),
#     eta = 0.3, max.depth = 6, min.child.weight = 1, subsample = 1, colsample.bytree = 1, lambda = 0, alpha = 0)
#   set.seed(getOption("mlr.debug.seed"))
#   p = predict(m2, newdata = as.matrix(multiclass.test[,-multiclass.class.col]))
#   p.probs = matrix(p, ncol = length(levs), byrow = TRUE)
#   testProb  ("classif.xgboost", multiclass.df, multiclass.target, multiclass.train.inds, p.probs)
#   data = xgb.DMatrix(
#     data = as.matrix(multiclass.train[multiclass.train.inds,-multiclass.class.col]),
#     label = as.integer(multiclass.train[multiclass.train.inds,multiclass.class.col]) - 1L)
#
#   set.seed(getOption("mlr.debug.seed"))
#   levs = levels(multiclass.df[, multiclass.class.col])
#   m1 = xgb.train(
#     data = data,
#     nrounds = 1, objective = "multi:softmax", num_class = length(levs))
#   set.seed(getOption("mlr.debug.seed"))
#   p = predict(m1, newdata = as.matrix(multiclass.test[,-multiclass.class.col]))
#   p.class = factor(levs[p + 1], levels = levs)
#   testSimple("classif.xgboost", multiclass.df, multiclass.target, multiclass.train.inds, p.class)
