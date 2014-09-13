test_that("Stacking binary task without features (stack.nocv) works", {
  for (sm.pt in c("response", "prob")) {
    for (bms.pt in c("response", "prob")) {

      base = list(makeLearner("classif.rpart", predict.type=bms.pt),
        makeLearner("classif.kknn", predict.type=bms.pt),
        makeLearner("classif.ksvm", predict.type=bms.pt))

      super = makeLearner("classif.randomForest", predict.type=sm.pt)
      
      slrn = makeStackedLearner(base.learners = base, super.learner = super,
        method = "stack.nocv", use.feat = FALSE)

      tr = train(slrn, binaryclass.task)

      pr = predict(tr, newdata=getTaskData(binaryclass.task))

      if (sm.pt == "prob") {
        expect_equal(ncol(pr$data[,grepl("prob", colnames(pr$data))]),
          length(binaryclass.task$task.desc$class.levels))
      }
      
      expect_equal(getBaseLearnerPredictions(tr), 
                   getBaseLearnerPredictions(tr, newdata=getTaskData(binaryclass.task)))
    }
  }
})

test_that("Stacking binary task with features (stack.nocv) works", {
  for (sm.pt in c("response", "prob")) {
    for (bms.pt in c("response", "prob")) {
      
      base = list(makeLearner("classif.rpart", predict.type=bms.pt),
                  makeLearner("classif.kknn", predict.type=bms.pt),
                  makeLearner("classif.ksvm", predict.type=bms.pt))
      
      super = makeLearner("classif.randomForest", predict.type=sm.pt)
      
      slrn = makeStackedLearner(base.learners = base, super.learner = super,
                                method = "stack.nocv", use.feat = TRUE)
      
      tr = train(slrn, binaryclass.task)
      
      pr = predict(tr, newdata=getTaskData(binaryclass.task))
      
      if (sm.pt == "prob") {
        expect_equal(ncol(pr$data[,grepl("prob", colnames(pr$data))]),
                     length(binaryclass.task$task.desc$class.levels))
      }
       
      expect_equal(getBaseLearnerPredictions(tr), 
                   getBaseLearnerPredictions(tr, newdata=getTaskData(binaryclass.task)))
    }
  }
})

test_that("Stacking regr task without features (stack.nocv) works", {
  sm.pt = bms.pt = "response"
  base = list(makeLearner("regr.rpart", predict.type=bms.pt),
              makeLearner("regr.kknn", predict.type=bms.pt),
              makeLearner("regr.ksvm", predict.type=bms.pt))
  
  super = makeLearner("regr.randomForest", predict.type=sm.pt)
  
  slrn = makeStackedLearner(base.learners = base, super.learner = super,
                            method = "stack.nocv", use.feat = TRUE)
  
  tr = train(slrn, regr.task)
  
  pr = predict(tr, newdata=getTaskData(regr.task))
  
  expect_equal(getBaseLearnerPredictions(tr), 
               getBaseLearnerPredictions(tr, newdata=getTaskData(regr.task)))
})

test_that("Stacking regr task without features (stack.nocv) works", {
  sm.pt = bms.pt = "response"
  base = list(makeLearner("regr.rpart", predict.type=bms.pt),
              makeLearner("regr.kknn", predict.type=bms.pt),
              makeLearner("regr.ksvm", predict.type=bms.pt))
  
  super = makeLearner("regr.randomForest", predict.type=sm.pt)
  
  slrn = makeStackedLearner(base.learners = base, super.learner = super,
                            method = "stack.nocv", use.feat = FALSE)
  
  tr = train(slrn, regr.task)
  
  pr = predict(tr, newdata=getTaskData(regr.task))
  
  expect_equal(getBaseLearnerPredictions(tr), 
               getBaseLearnerPredictions(tr, newdata=getTaskData(regr.task)))
})

test_that("Stacking binary task without features (stack.nocv) works", {
  for (sm.pt in c("response", "prob")) {
    for (bms.pt in c("response", "prob")) {
      
      base = list(makeLearner("classif.rpart", predict.type=bms.pt),
                  makeLearner("classif.kknn", predict.type=bms.pt),
                  makeLearner("classif.ksvm", predict.type=bms.pt))
      
      super = makeLearner("classif.randomForest", predict.type=sm.pt)
      
      slrn = makeStackedLearner(base.learners = base, super.learner = super,
                                method = "stack.nocv", use.feat = FALSE)
      
      tr = train(slrn, multiclass.task)
      
      pr = predict(tr, newdata=getTaskData(multiclass.task))
      
      if (sm.pt == "prob") {
        expect_equal(ncol(pr$data[,grepl("prob", colnames(pr$data))]),
                     length(multiclass.task$task.desc$class.levels))
      }
      
      expect_equal(getBaseLearnerPredictions(tr), 
                   getBaseLearnerPredictions(tr, newdata=getTaskData(multiclass.task)))
    }
  }
})

test_that("Stacking binary task with features (stack.nocv) works", {
  for (sm.pt in c("response", "prob")) {
    for (bms.pt in c("response", "prob")) {
      
      base = list(makeLearner("classif.rpart", predict.type=bms.pt),
                  makeLearner("classif.kknn", predict.type=bms.pt),
                  makeLearner("classif.ksvm", predict.type=bms.pt))
      
      super = makeLearner("classif.randomForest", predict.type=sm.pt)
      
      slrn = makeStackedLearner(base.learners = base, super.learner = super,
                                method = "stack.nocv", use.feat = TRUE)
      
      tr = train(slrn, multiclass.task)
      
      pr = predict(tr, newdata=getTaskData(multiclass.task))
      
      if (sm.pt == "prob") {
        expect_equal(ncol(pr$data[,grepl("prob", colnames(pr$data))]),
                     length(multiclass.task$task.desc$class.levels))
      }
      
      expect_equal(getBaseLearnerPredictions(tr), 
                   getBaseLearnerPredictions(tr, newdata=getTaskData(multiclass.task)))
    }
  }
})