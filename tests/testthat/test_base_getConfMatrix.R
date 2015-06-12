context("getConfMatrix")

test_that("getConfMatrix", {
  rdesc = makeResampleDesc("CV", iters=3)
  r = resample(makeLearner("classif.rpart"), multiclass.task, rdesc)
  cm = getConfMatrix(r$pred, relative = FALSE)
  expect_true(is.matrix(cm) && nrow(cm) == 4 && ncol(cm) == 4)
  cm = getConfMatrix(r$pred, relative = TRUE)
  expect_true(is.matrix(cm) && nrow(cm) == 4 && ncol(cm) == 4)
})

test_that("getConfMatrix works with dropped class levels in newdata", {
  # lets take first line and me mean and drop class levels
  # (we had a reported bug here)
  newdata = droplevels(multiclass.df[1L,])
  m = train("classif.rpart", multiclass.task)
  p = predict(m, newdata = newdata)
  cm = getConfMatrix(p)
  expect_true(is.matrix(cm) && nrow(cm) == 4 && ncol(cm) == 4)
})

test_that("getConfMatrix produces error for FailureModel predicttions", {
  data = iris; data[,1] = 1
  lrn = makeLearner("classif.lda", config = list(on.learner.error = "quiet"))
  task = makeClassifTask(data = data, target = "Species")
  r = holdout(lrn, task, measures = ber)
  expect_error(getConfMatrix(r$pred), "FailureModel")
})



