# FIXME: old tests here, should be redone

context("stack_base")

checkStack = function(task, method, base, super, bms.pt, sm.pt, use.feat) {
  base = lapply(base, makeLearner, predict.type = bms.pt)
  if (method %in% c("aggregate", "ensembleselection")) {
    super = NULL
  } else {
    super = makeLearner(super, predict.type = sm.pt)
  }
  if (method == "ensembleselection" && bms.pt == "response" && inherits(task, "ClassifTask")) return()

  stk = makeStackedLearner(id = "stack", base.learner = base, super.learner = super,
    method = method, use.feat = use.feat, predict.type = sm.pt, save.preds = T)
  tr = train(stk, task)
  pr = predict(tr, task)

  if (sm.pt == "prob") {
    expect_equal(ncol(pr$data[,grepl("prob", colnames(pr$data))]), length(getTaskClassLevels(task)))
  }

  if (method %nin% c("superlearner", "ensembleselection")) {
    expect_equal(
      lapply(getStackedBaseLearnerPredictions(tr), function(x) getPredictionResponse(x)),
      lapply(getStackedBaseLearnerPredictions(tr, newdata = getTaskData(task)), function(x) getPredictionResponse(x))
    )
  }
}

test_that("Stacking base functions", {
  tasks = list(binaryclass.task, multiclass.task, regr.task)
  for (task in tasks) {
    td = getTaskDescription(task)
    if (inherits(task, "ClassifTask")) {
      pts = c("response", "prob")
      base = c("classif.rpart", "classif.randomForest", "classif.kknn")
      super = "classif.randomForest"
    } else {
      pts = "response"
      base = c("regr.rpart", "regr.lm", "regr.kknn")
      super = "regr.randomForest"
    }
    for (method in c("aggregate", "superlearner", "ensembleselection")) {
      ufs = if (method %in% c("aggregate", "ensembleselection")) FALSE else c(FALSE, TRUE)
      for (use.feat in ufs) {
        for (sm.pt in pts) {
          for (bms.pt in pts) {
            #cat(td$type, td$id, method, use.feat, sm.pt, bms.pt, fill = TRUE)
            checkStack(task, method, base, super, bms.pt, sm.pt, use.feat)
          }
        }
      }
    }
  }
})


test_that("doTrainPredict works", {
  l = doTrainPredict(makeLearner("classif.rpart"), binaryclass.task, save.on.disc = FALSE, show.info = FALSE)
  expect_list(l, len = 2, names = "named")
  expect_message(doTrainPredict(makeLearner("classif.rpart"), binaryclass.task, save.on.disc = FALSE, show.info = TRUE))
  l2 = doTrainPredict(makeLearner("classif.rpart"), id = "stack", binaryclass.task, save.on.disc = TRUE, show.info = TRUE)
  expect_list(l2, len = 2, names = "named")
  expect_string(l2$base.models, pattern = "saved.model")
  unlink(l2$base.models)
})

test_that("getStackedBaseLearnerPredictions works", {
  # Works for character vectors
  base = c("classif.rpart", "classif.lda")
  stk = makeStackedLearner(method = "aggregate", base.learners = base, save.on.disc = TRUE, predict.type = "prob")
  mod = train(stk, tsk)
  prds = getStackedBaseLearnerPredictions(model = mod, newdata = getTaskData(iris.task))
  expect_list(prds, len = 2, names = "named")
  expect_set_equal(names(prds), base)
  unlink(mod$learner.model$base.models)

  # Works with a single learner
  lrns = makeLearner("classif.rpart")
  stk = makeStackedLearner(method = "aggregate", base.learners = lrns, save.on.disc = TRUE, predict.type = "prob")
  mod = train(stk, tsk)
  prds = getStackedBaseLearnerPredictions(model = mod, newdata = getTaskData(iris.task))
  expect_list(prds, len = 1, names = "named")
  expect_set_equal(names(prds), lrns$id)
  unlink(mod$learner.model$base.models)
})
