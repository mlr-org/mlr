context("resample_convenience")

test_that("resample convenience functions", {
  mycheck = function(r) {
    expect_true(all(!is.na(r$aggr)))
  }

  r = holdout("classif.rpart", multiclass.task)
  mycheck(r)

  r = subsample("classif.rpart", multiclass.task, iters = 1L, split = 0.2,
    minsplit = 50L, models = TRUE)
  mycheck(r)
  expect_equal(r$models[[1L]]$learner.model$control$minsplit, 50L)

  lrn = makeLearner("classif.rpart")
  r = crossval(lrn, multiclass.task, iters = 2L)
  mycheck(r)

  r = repcv("classif.rpart", multiclass.task, folds = 2L, reps = 2L, stratify = TRUE)
  mycheck(r)

  r = bootstrapOOB("classif.rpart", multiclass.task, iters = 1L)
  mycheck(r)
  r = bootstrapB632("classif.rpart", multiclass.task, iters = 1L)
  mycheck(r)
  r = bootstrapB632plus("classif.rpart", multiclass.task, iters = 1L)
  mycheck(r)
})
