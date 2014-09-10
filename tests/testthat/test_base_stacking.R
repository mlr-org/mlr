test_that("Stacking with average works", {
  for (sm.pt in c("response", "prob")) {
    for (bms.pt in c("response", "prob")) {

      base = list(makeLearner("classif.rpart", predict.type=bms.pt),
        makeLearner("classif.kknn", predict.type=bms.pt),
        makeLearner("classif.ksvm", predict.type=bms.pt))

      slrn = makeStackedLearner(base.learners = base, predict.type = sm.pt,
        method = "average", use.feat = FALSE)

      tr = train(slrn, binaryclass.task)

      pr = predict(tr, newdata=getTaskData(binaryclass.task))

      if (sm.pt == "prob") {
        expect_equal(ncol(pr$data[,grepl("prob", colnames(pr$data))]),
          length(binaryclass.task$task.desc$class.levels))
      }
    }
  }
})
