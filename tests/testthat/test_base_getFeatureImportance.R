context("getFeatureImportance")

test_that("getFeatureImportance", {
    
  # test on binaryclass.task and rpart
  lrn = makeLearner("classif.rpart")
  mod = train(lrn, binaryclass.task)
  getFeatureImportance(mod)
  
  
  # test on multiclass.task and randomForest
  lrn = makeLearner("classif.randomForest")
  mod = train(lrn, multiclass.task)
  getFeatureImportance(mod)
})