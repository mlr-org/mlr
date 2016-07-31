context("stack_resampleStackedLearnerAgain")

test_that("resampleStackedLearnerAgain", {
  tasks_classif = list(binaryclass.task, multiclass.task) 
  bls = list(
    makeLearner("classif.rpart", id = "r1", minsplit = 5),
    makeLearner("classif.rpart", id = "r2", minsplit = 10),
    makeLearner("classif.rpart", id = "r3", minsplit = 15),
    makeLearner("classif.rpart", id = "r4", minsplit = 20),
    makeLearner("classif.rpart", id = "r5", minsplit = 25),
    makeLearner("classif.rpart", id = "r6", minsplit = 30),
    makeLearner("classif.rpart", id = "r7", minsplit = 35),
    makeLearner("classif.rpart", id = "r8", minsplit = 40),
    makeLearner("classif.rpart", id = "r9", minsplit = 45),
    makeLearner("classif.rpart", id = "r10", minsplit = 50)
  )
  bls = lapply(bls, function(x) setPredictType(x, predict.type = "prob"))
  ste = makeStackedLearner(id = "stackES", bls, resampling = cv3, 
    predict.type = "prob", method = "ensembleselection", parset = list(init = 1, 
    bagprob = 0.5, bagtime = 3, metric = mmce), save.on.disc = FALSE)
  
  stc = makeStackedLearner(id = "stackSL", bls, resampling = cv3,
      predict.type = "prob", method = "superlearner", 
      super.learner = makeLearner("classif.rpart", predict.type = "prob"))
  
  for (tsk in tasks_classif) {
    #context(paste("ES on", tsk$task.desc$id))
    rdesc = cv2
    resES = resample(ste, tsk, rdesc, models = TRUE) 
    ee1 = resampleStackedLearnerAgain(obj = resES, task = tsk, measures = mmce, parset = list(init = 2, bagprob = .7, bagtime = 10, replace = TRUE, metric = mmce))
    ee2 = resampleStackedLearnerAgain(obj = resES, task = tsk, measures = acc, parset = list(init = 2, bagprob = .7, bagtime = 10, replace = FALSE, metric = acc, maxiter = 2))
    ee3 = resampleStackedLearnerAgain(obj = resES, task = tsk, measures = list(mmce), parset = list(init = 1, bagprob = .7, bagtime = 10, replace = TRUE, tolerance = 0.5))
    ee4 = resampleStackedLearnerAgain(obj = resES, task = tsk, measures = mmce, parset = list(init = 2, bagtime = 10, replace = TRUE, maxiter = 20))
    ee5 = resampleStackedLearnerAgain(obj = resES, task = tsk, measures = mmce, parset = list(init = 5, bagprob = .7, bagtime = 10, replace = TRUE))
    # check if new settings for es1 are adopted 
    expect_is(ee1$models, "list")
    expect_equal(length(ee1$models), rdesc$iters)
    expect_equal(length(ee1$models[[1]]), 3) # i.e. freq, freq.list, weights
    expect_equal(ee1$parset$init, 2)
    expect_equal(ee1$parset$bagprob, 0.7)
    expect_equal(ee1$parset$bagtime, 10)
    expect_equal(ee1$parset$replace, TRUE)
    expect_is(ee1$parset$metric, "Measure")

    es1 = resampleStackedLearnerAgain(obj = resES, task = tsk, measures = mmce, super.learner = makeLearner("classif.rpart"))
    es2 = resampleStackedLearnerAgain(obj = resES, task = tsk, measures = list(mmce), super.learner = makeLearner("classif.rpart", predict.type = "prob"))
    es3 = resampleStackedLearnerAgain(obj = resES, task = tsk, measures = list(mmce, acc), super.learner = makeLearner("classif.rpart"))
    es4 = resampleStackedLearnerAgain(obj = resES, task = tsk, measures = list(mmce, acc), super.learner = makeLearner("classif.kknn"), use.feat = TRUE)
    #es5 = resampleStackedLearnerAgain(obj = resES, task = tsk, measures = list(mmce, acc), super.learner = makeLearner("classif.kknn"))
    # check if new settings for sl1 are adopted    
    expect_is(es1$models, "list")
    expect_equal(length(es1$models), rdesc$iters)
    expect_is(es1$models[[1]], "WrappedModel")
    res = list(ee1, ee2, ee3, ee4, ee5, es1, es2, es3, es4)
    # check if all models contain results (non-NAs)
    expect_equal(anyNA(lapply(seq_along(res), function(x) res[[x]]$aggr)), FALSE)
    # FIXME: more tests
    #context(paste("AV on", tsk$task.desc$id))
    av1 = resampleStackedLearnerAgain(obj = resES, task = tsk, measures = mmce)

    
    
    #context(paste("SL on", tsk$task.desc$id))
    resSL = resample(stc, tsk, cv2, models = TRUE) 
    ss1 = resampleStackedLearnerAgain(obj = resSL, task = tsk, measures = mmce, super.learner = makeLearner("classif.rpart", predict.type = "prob"))
    ss2 = resampleStackedLearnerAgain(obj = resSL, task = tsk, measures = list(mmce), super.learner = makeLearner("classif.rpart"))
    ss3 = resampleStackedLearnerAgain(obj = resSL, task = tsk, measures = list(mmce, acc), super.learner = makeLearner("classif.rpart"))
    ss4 = resampleStackedLearnerAgain(obj = resSL, task = tsk, measures = list(mmce, acc), super.learner = makeLearner("classif.kknn"), use.feat = TRUE)
    #ss5 = resampleStackedLearnerAgain(obj = resSL, task = tsk, measures = list(acc), super.learner = makeLearner("classif.kknn"))
    # check if new settings for sl1 are adopted    
    expect_is(ss1$models, "list")
    expect_equal(length(ss1$models), rdesc$iters)
    expect_is(ss1$models[[1]], "WrappedModel")
    
    resSL = list(ss1, ss2, ss3, ss4)
    # check if all models contain results (non-NAs)
    expect_equal(anyNA(lapply(seq_along(resSL), function(x) resSL[[x]]$aggr)), FALSE)
    # FIXME: more tests
  }

}
)

# FIXME: regr