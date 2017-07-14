context("remove_missing_levels.R")

test_that("remove_missing_levels.R", {
  data("basque")

  task = makeClassifTask(id = "pathogen_data", data = basque,
                         target = "diplo01", positive = "1")

  rdesc = makeResampleDesc("RepCV", folds = 10, reps = 2)

  lrn = makeLearner("classif.binomial",
                    link = "logit",
                    predict.type = "prob")

  resa = resample(lrn, task, rdesc, measures = list(auc))

  expect_length(resa, 12)
})
