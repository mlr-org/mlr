context("stack")

checkStack = function(task, method, base, super, bms.pt, sm.pt, use.feat) {

  base = lapply(base, makeLearner, predict.type = bms.pt)
  if (method %in% c("average", "hill.climb")) {
    super = NULL
  } else {
    super = makeLearner(super, predict.type = sm.pt)
    # sm.pt = NULL
  }
  if (method == "hill.climb" && bms.pt == "response" && inherits(task,
    "ClassifTask")) {
    return()
  }

  slrn = makeStackedLearner(base, super, method = method, use.feat = use.feat,
    predict.type = sm.pt)
  tr = train(slrn, task)
  pr = predict(tr, task)

  if (sm.pt == "prob") {
    expect_equal(ncol(pr$data[, grepl("prob", colnames(pr$data))]),
      length(getTaskClassLevels(task)))
  }

  if (method %nin% c("stack.cv", "hill.climb")) {
    expect_equal(
      getStackedBaseLearnerPredictions(tr),
      getStackedBaseLearnerPredictions(tr, newdata = getTaskData(task))
    )
  }
}

test_that("Stacking works", {
  tasks = list(binaryclass.task, multiclass.task, regr.task)
  for (task in tasks) {
    td = getTaskDesc(task)
    if (inherits(task, "ClassifTask")) {
      pts = c("response", "prob")
      base = c("classif.rpart", "classif.lda", "classif.svm")
      super = "classif.randomForest"
    } else {
      pts = "response"
      base = c("regr.rpart", "regr.lm", "regr.svm")
      super = "regr.randomForest"
    }
    for (method in c("average", "stack.cv", "stack.nocv", "hill.climb")) {
      ufs = if (method %in% c("average", "hill.climb")) FALSE else c(FALSE, TRUE)
      for (use.feat in ufs) {
        for (sm.pt in pts) {
          for (bms.pt in pts) {
            # cat(td$type, td$id, method, use.feat, sm.pt, bms.pt, fill = TRUE)
            checkStack(task, method, base, super, bms.pt, sm.pt, use.feat)
          }
        }
      }
    }
  }
})

test_that("Stacking works with wrapped learners (#687)", {
  base = "classif.rpart"
  lrns = lapply(base, makeLearner)
  lrns = lapply(lrns, setPredictType, "prob")
  lrns[[1]] = makeFilterWrapper(lrns[[1]], fw.abs = 2)
  expect_silent(makeStackedLearner(base.learners = lrns, predict.type = "prob",
    method = "hill.climb"))
})

test_that("Parameters for hill climb works", {
  tsk = binaryclass.task
  base = c("classif.rpart", "classif.lda", "classif.svm")
  lrns = lapply(base, makeLearner)
  lrns = lapply(lrns, setPredictType, "prob")
  m = makeStackedLearner(base.learners = lrns, predict.type = "prob",
    method = "hill.climb",
    parset = list(bagprob = 0.8, bagtime = 5, replace = FALSE))
  tmp = train(m, tsk)
  res = predict(tmp, tsk)

  expect_equal(sum(tmp$learner.model$weights), 1)

  metric = function(pred, true) {
    pred = colnames(pred)[max.col(pred)]
    tb = table(pred, true)
    return(1 - sum(diag(tb)) / sum(tb))
  }

  m = makeStackedLearner(base.learners = lrns, predict.type = "prob",
    method = "hill.climb",
    parset = list(replace = TRUE, bagprob = 0.7, bagtime = 3, init = 2,
      metric = metric))
  tmp = train(m, tsk)
  res = predict(tmp, tsk)

  expect_equal(sum(tmp$learner.model$weights), 1)
})

test_that("Parameters for compress model", {
  tsk = binaryclass.task
  base = c("classif.rpart", "classif.lda", "classif.svm")
  lrns = lapply(base, makeLearner)
  lrns = lapply(lrns, setPredictType, "prob")
  m = makeStackedLearner(base.learners = lrns, predict.type = "prob",
    method = "compress",
    parset = list(k = 5, prob = 0.3))
  tmp = train(m, tsk)
  res = predict(tmp, tsk)


  tsk = regr.task
  base = c("regr.rpart", "regr.svm")
  lrns = lapply(base, makeLearner)
  lrns = lapply(lrns, setPredictType, "response")
  m = makeStackedLearner(base.learners = lrns, predict.type = "response",
    method = "compress", parset = list(k = 5, prob = 0.3))
  tmp = train(m, tsk)
  expect_silent(predict(tmp, tsk))
})
