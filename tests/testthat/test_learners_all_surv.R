context("learners_all_surv")

test_that("learners work: surv ", {
  
  # settings to make learners faster and deal with small sample size
  hyperpars = list(
    surv.cforest = list(mtry = 1L)
  )
  
  fixHyperPars = function(lrn) {
    if (lrn$id %in% names(hyperpars))
      lrn = setHyperPars(lrn, par.vals = hyperpars[[lrn$id]])
    return(lrn)
  }
  
  # normal survival analysis
  sub.task = subsetTask(surv.task, subset = c(1:70),
    features = getTaskFeatureNames(surv.task)[c(3,4)])
  checkLearnersAllOnTask = function(task){
    lrns = mylist(task)
    lrns = lapply(lrns$class, makeLearner)
    for (lrn in lrns){
      expect_output(print(lrn), lrn$id)
      lrn = fixHyperPars(lrn)
      m = train(lrn, task)
      p = predict(m, task)
      expect_true(!is.na(performance(p)))
    }
  }
  
  checkLearnersAllOnTask(surv.task)
  
  # survival analysis with factors
  checkLearnersHandleFactors = function(task) {
    type = getTaskType(task) # to create new task
    costs = getTaskCosts(task) # used to create new.task in case of costsens
    target = getTaskTargetNames(task) # used to create new.task
    d = getTaskData(task)
    f = getTaskFeatureNames(task)[1] # get 1 feature to create factor variable
    d[,f] = factor(sample(c("a", "b"), size = nrow(d), replace = TRUE)) # create factor variable
    
    new.task = switch(type,
      classif = makeClassifTask(data = d, target = target),
      cluster = makeClusterTask(data = d),
      costsens = makeCostSensTask(data = d, costs = costs),
      multilabel = makeMultilabelTask(data = d, target = target),
      regr = makeRegrTask(data = d, target = target),
      surv = makeSurvTask(data = d, target = target))
  
    lrns = mylist(task, properties = "factors", create = TRUE) # use task or new.task here?
    
    for (lrn in lrns) {
      expect_output(print(lrn), lrn$id)
      lrn = fixHyperPars(lrn)
      m = train(lrn, task)
      p = predict(m, task)
      expect_true(!is.na(performance(p)))
    }
  }
  
  checkLearnersHandleFactors(surv.task)
  
  # mlr doesn't support prediction of probabilities yet
  # binary classif with prob: only coxph and ranger
  # task = subsetTask(surv.task, subset = c(1:70),
  #   features = getTaskFeatureNames(surv.task)[c(3,4)])
  # lrns = mylist(task, properties = "prob")
  # lrns = lapply(lrns$class, makeLearner, predict.type = "prob")
  # lapply(lrns, function(lrn) {
  #   lrn = fixHyperPars(lrn)
  #   m = train(lrn, task)
  #   p = predict(m, task)
  #   getPredictionProbabilities(p)
  #   expect_true(!is.na(performance(p)))
  # })
  
  # surv with weights
  # bigger dataset necessary otherwise cvglmnet does not converge
  task = surv.task
  lrns = mylist("surv", properties = "weights", create = TRUE)
  lapply(lrns, testThatLearnerRespectsWeights, hyperpars = hyperpars,
    task = task, train.inds = surv.train.inds,
    test.inds = surv.test.inds,
    weights = rep(c(1L, 5L), length.out = length(surv.train.inds)),
    pred.type = "response", get.pred.fun = getPredictionResponse)
  # surv.randomForestSRC is not working
  

  # surv with missing.
  checkLearnersHandleMissings = function(task) {
    target = getTaskTargetNames(task)
    type = getTaskType(task)
    f = getTaskFeatureNames(task)[1] # get one feature where an NA will be placed
    d = getTaskData(task)
    costs = getTaskCosts(task)
    d[1,f] = NA
    new.task = switch(type,
      classif = makeClassifTask(data = d, target = target),
      cluster = makeClusterTask(data = d),
      costsens = makeCostSensTask(data = d, costs = costs),
      multilabel = makeMultilabelTask(data = d, target = target),
      regr = makeRegrTask(data = d, target = target),
      surv = makeSurvTask(data = d, target = target))
    
    lrns = mylist(new.task, properties = "missings", create = TRUE) # do this on task instead of new.task?
    
    for (lrn in lrns) {
      lrn = fixHyperPars(lrn)
      m = train(lrn, task)
      p = predict(m, task)
      expect_true(!is.na(performance(p)))
    }
  }
  checkLearnersHandleMissings(surv.task)

  })