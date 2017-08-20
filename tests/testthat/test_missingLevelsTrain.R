context("missingLevelsTrain.R")

test_that("missingLevelsTrain.R", {

  task = sonar.task
  # introduce factor level with only one observation
  task$env$data$test_fac = factor(c(rep("1", 1), c(rep("2", 3), rep("3", length(task1$env$data$V1) - 4))))

  rdesc = makeResampleDesc("RepCV", folds = 5, reps = 2)

  lrn = makeLearner("classif.binomial",
                    link = "logit",
                    predict.type = "prob",
                    fix.factors.prediction = TRUE)

  resa = resample(lrn, task, rdesc, measures = list(auc))

  expect_length(resa, 12)
})

test_that("missingLevelsTrain.R", {

  task = sonar.task
  # introduce factor level with only one observation
  task$env$data$test_fac = factor(c(rep("1", 1), c(rep("2", 3), rep("3", length(task1$env$data$V1) - 4))))

  n = getTaskSize(task)
  train.set = seq(1, n, by = 2)
  test.set = seq(2, n, by = 2)
  mod = train(lrn, task, subset = train.set)

  task.pred = predict(mod, task = task, subset = test.set)

  expect_length(task.pred, 7)
})

