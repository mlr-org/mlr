context("stack_base")

checkStack = function(task, method, base, super, bls.predict.type, sup.predict.type, use.feat) {

  base = lapply(base, makeLearner, predict.type = bls.predict.type)

  par.vals = list()
  if (method == "superlearner")
    par.vals = list(super.learner = super, use.feat = use.feat)

  if (method == "ensembleselection")
    par.vals = list(bagiter = 4L, bagprop = 0.9, replace = TRUE, init = 1L)


  stk = makeStackedLearner(id = "stack", base.learner = base, method = method, par.vals = par.vals,
    predict.type = sup.predict.type, save.preds = TRUE)

  mod = train(stk, task)
  prd = predict(mod, task)

  if (sup.predict.type == "prob") {
    expect_equal(ncol(prd$data[,grepl("prob", colnames(prd$data))]), length(getTaskClassLevels(task)))
  }

  if (method %nin% c("superlearner", "ensembleselection")) {
    expect_equal(
      lapply(getStackedBaseLearnerPredictions(mod), function(x) getPredictionResponse(x)),
      lapply(getStackedBaseLearnerPredictions(mod, newdata = getTaskData(task)), function(x) getPredictionResponse(x))
    )
  }
}

test_that("Stacking base functions", {
  tasks = list(binaryclass.task, multiclass.task, regr.task)
  for (task in tasks) {
    td = getTaskDescription(task)
    if (inherits(task, "ClassifTask")) {
      pts = c("response", "prob")
      base = c("classif.rpart", "classif.lda", "classif.kknn")
      super = "classif.rpart"
    } else {
      pts = "response"
      base = c("regr.rpart", "regr.lm", "regr.kknn")
      super = "regr.rpart"
    }
    for (method in c("aggregate", "superlearner", "ensembleselection")) {
      ufs = if (method %in% c("aggregate", "ensembleselection")) FALSE else c(FALSE, TRUE)
      for (use.feat in ufs) {
        for (sm.pt in pts) {
          for (bms.pt in pts) {
            checkStack(task, method, base, super, bms.pt, sm.pt, use.feat)
          }
        }
      }
    }
  }
})

test_that("Works without specifying all parameters", {
  base = c("regr.rpart", "regr.lm", "regr.ksvm")
  lrns = lapply(base, makeLearner)
  stk = makeStackedLearner(method = "superlearner", base.learners = lrns, par.vals = list(super.learner = "regr.lm"))
  mod = train(stk, regr.task)
  prd = predict(mod, regr.task)
  expect_class(prd, c("PredictionRegr", "Prediction"))
})


test_that("doTrainPredict works", {
  l = doTrainPredict(makeLearner("classif.rpart", id = "rpart1"), binaryclass.task, save.on.disc = NULL, id = "nostack", show.info = FALSE)
  expect_list(l, len = 2, names = "named")
  expect_message(doTrainPredict(makeLearner("classif.rpart"), binaryclass.task, id = "nostack", save.on.disc = NULL, show.info = TRUE))
  l2 = doTrainPredict(makeLearner("classif.rpart"), id = "nostack", binaryclass.task, save.on.disc = ".", show.info = FALSE)
  expect_list(l2, len = 2, names = "named")
  expect_string(l2$base.models, pattern = "model.nostack.classif")
  unlink(l2$base.models)
})

test_that("getStackedBaseLearnerPredictions works", {
  # Works for character vectors
  base = c("classif.rpart", "classif.lda")
  stk = makeStackedLearner(method = "aggregate", base.learners = base, save.on.disc = ".", predict.type = "prob")
  mod = train(stk, tsk)
  prds = getStackedBaseLearnerPredictions(model = mod, newdata = getTaskData(iris.task))
  expect_list(prds, len = 2, names = "named")
  expect_set_equal(names(prds), base)
  unlink(mod$learner.model$base.models)

  # Works with a single learner
  lrns = makeLearner("classif.rpart")
  stk = makeStackedLearner(method = "aggregate", base.learners = lrns, save.on.disc = ".", predict.type = "prob")
  mod = train(stk, tsk)
  prds = getStackedBaseLearnerPredictions(model = mod, newdata = getTaskData(iris.task))
  expect_list(prds, len = 1, names = "named")
  expect_set_equal(names(prds), lrns$id)
  unlink(mod$learner.model$base.models)

  # Works if save = FALSE
  base = c("classif.rpart", "classif.lda")
  stk = makeStackedLearner(method = "aggregate", base.learners = base, save.on.disc = NULL, predict.type = "prob")
  mod = train(stk, tsk)
  prds = getStackedBaseLearnerPredictions(model = mod, newdata = getTaskData(iris.task))
  expect_list(prds, len = 2, names = "named")
  expect_set_equal(names(prds), base)
})

test_that("aggregateModelPredictions works for classification", {
  pts = c("prob", "response")
  tasks = list(binaryclass.task, multiclass.task)
  for (tsk in tasks) {
    for (pt in pts) {
      for (super.predict.type in pts) {
        lrn0 = makeLearner("classif.rpart", predict.type = pt)
        lrn1 = makeLearner("classif.kknn", "kknn1", predict.type = pt)
        lrn2 = makeLearner("classif.kknn", "kknn2", k = 10, predict.type =pt)
        stk = makeStackedLearner(method = "aggregate",
          base.learners = list(lrn0, lrn1, lrn2),
          predict.type = super.predict.type)
        mod = train(stk, tsk)
        prd = predict(mod, tsk)
        expect_class(prd, c("PredictionRegr", "Prediction"))
      }
    }
  }
})

test_that("aggregateModelPredictions works for classification", {
  tasks = list(regr.task, regr.small.task, regr.num.task)
  for (tsk in tasks) {
    lrn0 = makeLearner("regr.rpart")
    lrn1 = makeLearner("regr.kknn", id = "kknn1")
    lrn2 = makeLearner("regr.kknn", k = 10)
    stk = makeStackedLearner(method = "aggregate",
      base.learners = list(lrn0, lrn1, lrn2))
    mod = train(stk, tsk)
    prd = predict(mod, tsk)
    expect_class(prd, c("PredictionRegr", "Prediction"))
  }
})


test_that("Failuremodels are removed aggregate", {
  configureMlr(on.learner.error = "quiet", show.learner.output = FALSE)
  # 1 Failed Learner
  lrn = makeLearner("classif.qda", predict.type = "prob")
  stk = makeStackedLearner(method = "aggregate", base.learners = lrn)
  expect_warning(m <- train(stk, multiclass.task, subset = c(1, 51, 101)))
  expect_true(unlist(m$learner.model$failed.models) == "classif.qda")
  expect_true(length(m$learner.model$base.models) == 0)
  p = predict(m, newdata = iris)
  expect_true(all(is.na(p$data$response)))

  # 1 Failed Learner 1 Working
  lrn1 = makeLearner("classif.qda", predict.type = "prob")
  lrn2 = makeLearner("classif.rpart", predict.type = "prob")
  stk = makeStackedLearner(method = "aggregate", base.learners = list(lrn1, lrn2))
  expect_warning(m <- train(stk, multiclass.task, subset = c(1, 51, 101)))
  expect_true(length(m$learner.model$failed.models) == 1)
  expect_true(length(m$learner.model$base.models) == 1)
  p = predict(m, newdata = iris)
  expect_true(!any(is.na(p$data$response)))

  configureMlr(on.learner.error = "stop", show.learner.output = FALSE)
})

test_that("Failuremodels are removed ensemblesel", {
  configureMlr(on.learner.error = "quiet", show.learner.output = FALSE)

  # ResampleDesc Fails
  lrn = makeLearner("classif.qda", predict.type = "prob")
  stk = makeStackedLearner(method = "ensembleselection", base.learners = lrn)
  m = train(stk, multiclass.task, subset = c(1, 51, 101, 102, 52))
  expect_true(inherits(m, "FailureModel"))

  # 1 Learner fails 1 Works
  stk = makeStackedLearner(method = "ensembleselection", base.learners = list(lrn1, lrn2),
    resampling = makeResampleDesc("CV", iters = 2, stratify = TRUE))
  expect_warning(m <- train(stk, multiclass.task, subset = c(1:2, 51:52, 101:102)))
  expect_true(unlist(m$learner.model$failed.models) == "classif.qda")
  expect_true(length(m$learner.model$base.models) == 1)
  expect_equal(m$learner.model$selected, list(1))
  p = predict(m, newdata = iris)
  expect_true(!any(is.na(p$data$response)))

  configureMlr(on.learner.error = "stop", show.learner.output = FALSE)
})


test_that("Ensembleselection works") {
  lrns = list(
    makeLearner("classif.rpart", "rpart1", minsplit = 1),
    makeLearner("classif.rpart", "rpart2", minsplit = 3),
    makeLearner("classif.rpart", "rpart3", minsplit = 5),
    makeLearner("classif.rpart", "rpart4", minsplit = 10),
    makeLearner("classif.rpart", "rpart5", minsplit = 20),
    makeLearner("classif.ksvm", "ksvm1"),
    makeLearner("classif.ksvm", "ksvm2", kernel = "rbfdot", sigma = 0.1),
    makeLearner("classif.ksvm", "ksvm3", kernel = "rbfdot", sigma = 0.01),
    makeLearner("classif.ksvm", "ksvm4", kernel = "polydot"),
    makeLearner("classif.ksvm", "ksvm5", kernel = "laplacedot"),
    makeLearner("classif.lda", "lda")
  )
  lrns = lapply(lrns, setPredictType, "prob")
  stk = makeStackedLearner(method = "ensembleselection",
    base.learners = lrns,
    predict.type = "prob",
    par.vals = list(init = 3, bagprop = 0.7, maxiter = 5L, bagiter = 3L),
    measure = mmce)
  mod = train(stk, pid.task)
  prd = predict(mod, pid.task)
  expect_class(mod, "BaseEnsembleModel")
  expect_class(mod, "PredictionClassif")
  expect_true(length(mod$learner.model$selected) == 3)
  expect_true(max(sapply(mod$learner.model$selected, sum)) <= 8)
  expect_true(min(sapply(mod$learner.model$selected, sum)) <= 3)
}
