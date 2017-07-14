context("remove.missing.levels.lm.R")

test_that("remove.missing.levels.lm.R", {

  data("bc.task.spatial")
  task = bc.task.spatial

  rdesc = makeResampleDesc("RepCV", folds = 10, reps = 2)

  lrn = makeLearner("classif.binomial",
                    link = "logit",
                    predict.type = "prob",
                    fix.factors.prediction = TRUE)

  resa = resample(lrn, task, rdesc, measures = list(auc))

  expect_length(resa, 12)
})

test_that("remove.missing.levels.lm.R", {

  data("bc.task.spatial")
  task = bc.task.spatial

  n = getTaskSize(task)
  train.set = seq(1, n, by = 2)
  test.set = seq(2, n, by = 2)
  mod <- train(lrn, task, subset = train.set)

  task.pred = predict(mod, task = task, subset = test.set)

  expect_length(task.pred, 7)
})

