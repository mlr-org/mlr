context("analyzeFeatSelResult")

test_that("analyzeFeatSelResult", {
  lrn = makeLearner("classif.rpart")
  rdesc = makeResampleDesc("Holdout")
  ctrl.seq = makeFeatSelControlSequential(method = "sfs")
  sf.seq = selectFeatures(learner = lrn, task = multiclass.task,
    resampling = rdesc, control = ctrl.seq, show.info = FALSE)
  expect_output(analyzeFeatSelResult(sf.seq, reduce = TRUE), "Petal.Width")
})

test_that("analyzeFeatSelResult with tune threshold (cf. issue #245)", {
  ctrl = makeFeatSelControlSequential(method = "sfs", alpha = 0.1,
    tune.threshold = TRUE)
  rdesc = makeResampleDesc("Holdout")
  lrn = makeLearner("classif.lda", predict.type = "prob")
  task = subsetTask(sonar.task, features = paste("V", 11:16, sep = ""))
  sfeats = selectFeatures(learner = lrn, task = task, resampling = rdesc,
    control = ctrl, show.info = FALSE)
  expect_output(analyzeFeatSelResult(sfeats, reduce = TRUE), "V11")
})
