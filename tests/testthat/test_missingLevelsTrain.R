context("missingLevelsTrain.R")

test_that("additional factor levels in test data are removed and resample() does not error", {

  task = sonar.task
  # introduce factor level with only one observation
  task$env$data$test_fac = factor(c(rep("1", 1), c(rep("2", 3),
    rep("3", length(task$env$data$V1) - 4))))

  rdesc = makeResampleDesc("RepCV", folds = 5, reps = 2)

  lrn = makeLearner("classif.binomial",
                    link = "logit",
                    predict.type = "prob",
                    fix.factors.prediction = TRUE)

  resa = resample(lrn, task, rdesc, measures = list(auc))

  expect_length(resa, 12)
})

test_that("resample() DOES error if additional factor levels in test data are not removed (default behaviour)", {

  task = sonar.task
  # introduce factor level with only one observation
  task$env$data$test_fac = factor(c(rep("1", 1), c(rep("2", 3),
    rep("3", length(task$env$data$V1) - 4))))

  rdesc = makeResampleDesc("RepCV", folds = 5, reps = 2)

  lrn = makeLearner("classif.binomial",
                    link = "logit",
                    predict.type = "prob",
                    fix.factors.prediction = FALSE)

  expect_error(resample(lrn, task, rdesc, measures = list(auc)),
     "factor test_fac has new levels 1")
})
