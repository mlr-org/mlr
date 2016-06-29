context("learners_all_surv")

test_that("learners work: surv ", {
  
  # settings to make learners faster and deal with small sample size
  hyperpars = list(
    surv.cforest = list(mtry = 1L)
  )
  
<<<<<<< HEAD
=======
  
>>>>>>> 534a7f73d7c8a14b4ac1a9b9572a30ed6fe20649
  # normal survival analysis
  sub.task = subsetTask(surv.task, subset = c(1:70),
    features = getTaskFeatureNames(surv.task)[c(3,4)])
  lrns = mylist("surv", create = TRUE)
<<<<<<< HEAD
  lapply(lrns, testThatLearnerCanTrainPredict, task = sub.task, hyperpars = hyperpars)
=======
  lapply(lrns, checkPerformance, task = sub.task, hyperpars = hyperpars)
  
>>>>>>> 534a7f73d7c8a14b4ac1a9b9572a30ed6fe20649
  
  # survival analysis with factors
  lrns = mylist("surv", properties = "factors", create = TRUE)
  lapply(lrns, testThatLearnerHandlesFactors, task = surv.task, hyperpars = hyperpars)
  
<<<<<<< HEAD
=======
  
>>>>>>> 534a7f73d7c8a14b4ac1a9b9572a30ed6fe20649
  # surv with weights
  # normal size of surv.task necessary otherwise cvglmnet does not converge
  lrns = mylist("surv", properties = "weights", create = TRUE)
  lapply(lrns, testThatLearnerRespectsWeights, hyperpars = hyperpars,
    task = surv.task, train.inds = surv.train.inds,
    test.inds = surv.test.inds,
    weights = rep(c(1L, 5L), length.out = length(surv.train.inds)),
    pred.type = "response", get.pred.fun = getPredictionResponse)
  
<<<<<<< HEAD
=======

>>>>>>> 534a7f73d7c8a14b4ac1a9b9572a30ed6fe20649
  # survival with missings
  lrns = mylist("surv", properties = "missings", create = TRUE)
  lapply(lrns, testThatLearnerHandlesMissings, task = surv.task, hyperpars = hyperpars) 

})
