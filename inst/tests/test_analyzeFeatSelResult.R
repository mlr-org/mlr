context("analyzeFeatSelResult")

test_that("analyzeFeatSelResult", {
  lrn = makeLearner("classif.rpart")
  rdesc = makeResampleDesc("Holdout")
  ctrlSeq = makeFeatSelControlSequential(method = "sfs")
  set.seed(getOption("mlr.debug.seed"))
  sfSeq = selectFeatures(learner = lrn, task = multiclass.task, resampling = rdesc,
    control = ctrlSeq, show.info = FALSE)
  expect_output(analyzeFeatSelResult(sfSeq, reduce = TRUE), "Petal.Width")
})
