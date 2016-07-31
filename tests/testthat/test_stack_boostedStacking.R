context("stack_boostedStacking")

test_that("Parameters for makeBoostedStackingLearner (classif)", {
  tasks_classif = list(binaryclass.task, multiclass.task) 
  ctrl = makeTuneControlRandom(maxit = 3L)
  pts = c("prob", "response")

  #spt = "prob"; bpt = "prob"
  for (tsk in tasks_classif) {
    for (spt in pts) {
      for (bpt in pts) {
        context(paste(tsk$task.desc$id, spt, bpt))
        configureMlr(on.learner.warning = "quiet")
        if (length(tsk$task.desc$class.levels) > 2) {
          dist = "multinomial"
        } else {
          dist = "bernoulli" 
        }
        lrns = list(
          makeLearner("classif.gbm", distribution = dist),
          makeLearner("classif.rpart"))
        lrns = lapply(lrns, setPredictType, bpt)
        mm = makeModelMultiplexer(lrns)
        ps = makeModelMultiplexerParamSet(mm,
          #makeNumericParam("sigma", lower = -5, upper = 5, trafo = function(x) 2^x),
          #makeNumericParam("C", lower = -5, upper = 5, trafo = function(x) 2^x),
          makeIntegerParam("n.trees", lower = 1L, upper = 3L),
          makeIntegerParam("minsplit", lower = 1L, upper = 3L))
        stb = makeBoostedStackingLearner(model.multiplexer = mm, 
          predict.type = spt, 
          resampling = cv5,
          mm.ps = ps, 
          measures = mmce, 
          control = ctrl, 
          niter = 2L)
        n.obs = getTaskSize(tsk)
        train.set = 1:as.integer(n.obs * 0.8)
        test.set =  as.integer(n.obs * 0.8 + 1):n.obs
        #model = train(stb, subsetTask(tsk, subset = train.set))
        #res = predict(model, subsetTask(tsk, subset = test.set))
        #res
        #performance(res)
        # debugo
        
        r = resample(stb, task = tsk, resampling = cv2, models = TRUE, show.info = TRUE)
        expect_is(r$aggr, "numeric")
        if (spt == "prob") {
          p = getPredictionProbabilities(r$pred, cl = tsk$task.desc$class.levels)
          expect_that(dim(p), is_identical_to(c(getTaskSize(tsk), length(levels(getTaskTargets(tsk))))))
        } else {
          p = getPredictionResponse(r$pred)
          expect_class(p, "factor")
          expect_equal(length(p), (getTaskSize(tsk)))
        }
      }
    }
  }
})
  
#test_that("Parameters for boost.stack model (regr)", {
#
#  tasks_regr = list(regr.num.task, regr.task)
#  lrns = list(
#    #makeLearner("classif.ksvm", kernel = "rbfdot"),
#    makeLearner("regr.gbm"),
#    makeLearner("regr.randomForest"))
#  mm = makeModelMultiplexer(lrns)
#  ctrl = makeTuneControlRandom(maxit = 3L)
#  for (tsk in tasks_regr) {
#    ps = makeModelMultiplexerParamSet(mm,
#      #makeNumericParam("sigma", lower = -5, upper = 5, trafo = function(x) 2^x),
#      #makeNumericParam("C", lower = -5, upper = 5, trafo = function(x) 2^x),
#      makeIntegerParam("n.trees", lower = 1L, upper = 500L),
#      makeIntegerParam("interaction.depth", lower = 1L, upper = 10L),
#      makeIntegerParam("ntree", lower = 1L, upper = 500L),
#      makeIntegerParam("mtry", lower = 1L, upper = getTaskNFeats(tsk)))
#    stb = makeBoostedStackingLearner(model.multiplexer = mm, 
#      predict.type = "response", #because regr  
#      resampling = cv3,
#      mm.ps = ps, 
#      measures = mse, 
#      control = ctrl, 
#      niter = 2L)
#      n.obs = getTaskSize(tsk)
#    n.obs = getTaskSize(tsk)
#    train.set = sort(sample(n.obs, 0.7 * n.obs))
#    test.set =  setdiff(1:n.obs, train.set)
#    #model = train(stb, subsetTask(tsk, subset = train.set))
#    #pre = predict(model, subsetTask(tsk, subset = test.set))
#    r = resample(stb, task = tsk, resampling = cv3)
#    expect_is(r, "ResampleResult")
#  }  
#})

context("Check makeXBestLearnersFromMMTuneResult")
test_that("Check makeXBestLearnersFromMMTuneResult", {
  
  mm.lrns = list(
    makeLearner("classif.xgboost", predict.type = "prob", fix.factors.prediction = TRUE,
      max_depth = 3, nrounds = 10, verbose = 0),
    makeLearner("classif.randomForest", predict.type = "prob", fix.factors.prediction = TRUE))
  mm = makeModelMultiplexer(mm.lrns)
  
  mm.ps = makeModelMultiplexerParamSet(mm,
    classif.xgboost = makeParamSet(
      makeNumericParam("eta", lower = -7L, upper = -5L, trafo = function(x) 2^x)
    ),
    classif.randomForest = makeParamSet(
     makeIntegerParam("ntree", lower = 1L, upper = 50L), 
     makeIntegerParam("mtry", lower = 1L, upper = 3)
    )
  )
  
  ctrl = makeTuneControlRandom(maxit = 4L)
  set.seed(1)
  res = tuneParams(mm, binaryclass.task, cv2, par.set = mm.ps, 
    measures = mmce, control = ctrl)
  
  lrns = makeXBestLearnersFromMMTuneResult(res, mm, mm.ps, x.best = 3, measure = mmce)
  expect_class(lrns, "list")
  expect_equal(lrns[[2]]$par.vals$nrounds, 10) # checks if fix values are passed
  expect_equal(lrns[[2]]$par.vals$max_depth, 3)
  expect_equal(lrns[[2]]$par.vals$eta, 0.007848372) # checks if trafo works
  # FIXME: add tests
})
