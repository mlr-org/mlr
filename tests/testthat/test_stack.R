checkStack = function(task, method, base, super, bms.pt, sm.pt, use.feat) {
  base = lapply(base, makeLearner, predict.type = bms.pt)
  if (method == "average") {
    super = NULL
  } else {
    super = makeLearner(super, predict.type = sm.pt)
    # sm.pt = NULL
  }

  slrn = makeStackedLearner(base, super, method = method, use.feat = use.feat, predict.type = sm.pt)
  tr = train(slrn, task)
  pr = predict(tr, task)

  if (sm.pt == "prob") {
    expect_equal(ncol(pr$data[,grepl("prob", colnames(pr$data))]),
      length(task$task.desc$class.levels))
  }

  if (method != "stack.cv") {
    expect_equal(
      getStackedBaseLearnerPredictions(tr),
      getStackedBaseLearnerPredictions(tr, newdata = getTaskData(task))
    )
  }
}

test_that("Stacking works", {
  tasks = list(binaryclass.task, multiclass.task, regr.task)
  for (task in tasks) {
    if (inherits(task, "ClassifTask")) {
      pts = c("response", "prob")
      base = c("classif.rpart", "classif.lda", "classif.svm")
      super = "classif.randomForest"
    } else {
      pts = "response"
      base = c("regr.rpart", "regr.lm", "regr.svm")
      super = "regr.randomForest"
    }
    for (method in c("average", "stack.cv", "stack.nocv")) {
      ufs = if (method == "average") FALSE else c(FALSE, TRUE)
      for (use.feat in ufs) {
        for (sm.pt in pts) {
          for (bms.pt in pts) {
            #cat(task$task.desc$type, task$task.desc$id, method, use.feat, sm.pt, bms.pt, fill = TRUE)
            checkStack(task, method, base, super, bms.pt, sm.pt, use.feat)
          }
        }
      }
    }
  }
})
