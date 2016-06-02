context("learners_all_surv")

test_that("learners work: surv ", {
  
  # settings to make learners faster and deal with small sample size
  # These values come from multi-criteria tunining (timetrain, timepredict)
  hyperpars = list(
    surv.cforest = list(mtry = 1L)
    #surv.cforest = list(ntree = 198L, mtry = 1L, replace = TRUE, teststat = "max",
     # testtpe = "Teststatistic", mincriterion = 0.92),
    #surv.CoxBoost = list(penalty = 600, criterion = "pscore", stepno = 72L)
    # surv.coxph = list(outer.max = 19L),
    # surv.cv.CoxBoost = list(maxstepno = 4L, penalty = 393, stepsize.factor = 2.85),
    # surv.cvglmnet = list(alpha = 0.1038067, nfolds = 5L, standardize = FALSE,
    #   maxit = 29000L),
    # surv.glmboost = list(mstop = 10L, nu = 0.6349),
    #surv.penalized.fusedlasso = list(lambda1 = 0.4045103, lambda2 = 1,
    #   standardize = TRUE)
    # surv.penalized.ridge = list(maxiter = 65L),
    # surv.randomForestSRC = list(ntree = 720, bootstrap = "none",
    #   mtry = 2L, nodesize = 17L, splitrule = "random", na.action = "na.omit",
    #   split.depth = "by.tree"),
    # surv.ranger = list(num.trees = 400L, mtry = 1L, min.node.size = 20L),
    # surv.rpart = list(minsplit = 7L, minbucket = 8L, cp = 0.6900071,
    #   maxcompete = 1L, maxdepth = 27L)
  )
  
  fixHyperPars = function(lrn) {
    if (lrn$id %in% names(hyperpars))
      lrn = setHyperPars(lrn, par.vals = hyperpars[[lrn$id]])
    return(lrn)
  }
  
  # normal survival analysis
  task = subsetTask(surv.task, subset = c(1:70),
    features = getTaskFeatureNames(surv.task)[c(3,4)])
  lrns = mylist(task)
  lrns = lapply(lrns$class, makeLearner)
  for (lrn in lrns){
    expect_output(print(lrn), lrn$id)
    lrn = fixHyperPars(lrn)
    m = train(lrn, task)
    p = predict(m, task)
    expect_true(!is.na(performance(p)))
  }
  
  
  # survival analysis with factors
  data = surv.df[, c("time", "status", "x3", "x4")]
  data[, 4L] = factor(sample(c("a", "b"), size = nrow(data), replace = TRUE))
  task = makeSurvTask(data = data, target = c("time", "status"))
  lrns = mylist(task, create = TRUE)
  # delete the next for loop when penalized learners are fixd
  for (i in 1:(length(lrns)-1)) {
    if (lrns[[i]]$id == "surv.penalized.fusedlasso") {
    lrns[[i]] = NULL
    }
    if(lrns[[i]]$id == "surv.penalized.lasso") {
      lrns[[i]] = NULL
    }
    if (lrns[[i]]$id == "surv.penalized.ridge") {
      lrns[[i]] = NULL
    }
  }
  for (lrn in lrns) {
    expect_output(print(lrn), lrn$id)
    lrn = fixHyperPars(lrn)
    m = train(lrn, task)
    p = predict(m, task)
    expect_true(!is.na(performance(p)))
  }
  
  
  # binary classif with prob (only coxph and ranger)
  # mlr doesn't supprt prediction of probabilities yet
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
  
  # FIXME: geht noch nicht
  lrns = mylist("surv", properties = "weights", create = TRUE)
  lapply(lrns, testThatLearnerRespectsWeights, hyperpars = hyperpars,
    task = surv.task, train.inds = surv.train.inds, surv.test.inds,
    weights = rep(c(10000L, 1L), c(10L, length(surv.train.inds) - 10L)),
    pred.type = "response", get.pred.fun = getPredictionResponse)
  
  
})