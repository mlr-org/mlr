context("remove.missing.levels.lm.R")

test_that("remove.missing.levels.lm.R", {
  data("basque")

  data("bc.task.spatial")

  rdesc = makeResampleDesc("RepCV", folds = 10, reps = 2)

  lrn = makeLearner("classif.binomial",
                    link = "logit",
                    predict.type = "prob")

  resa = resample(lrn, task, rdesc, measures = list(auc))

  expect_length(resa, 12)
})
