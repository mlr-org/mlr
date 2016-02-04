context("overundersample")

test_that("over and undersample works",  {
  y = binaryclass.df[, binaryclass.target]
  tab1 = table(y)
  task = oversample(binaryclass.task, rate = 2)
  df = getTaskData(task)
  tab2 = table(df[, binaryclass.target])
  expect_equal(tab2["M"], tab1["M"])
  expect_equal(tab2["R"], tab1["R"] * 2)
  task = undersample(binaryclass.task, rate = 0.5)
  df = getTaskData(task)
  tab2 = table(df[, binaryclass.target])
  expect_equal(tab2["M"], round(tab1["M"] / 2))
  expect_equal(tab2["R"], tab1["R"])
})

test_that("over and undersample wrapper",  {
  rdesc = makeResampleDesc("CV", iters = 2)
  lrn1 = makeLearner("classif.rpart")
  lrn2 = makeUndersampleWrapper(lrn1, usw.rate = 0.5)
  r = resample(lrn2, binaryclass.task, rdesc)
  expect_true(!is.na(r$aggr))
  lrn2 = makeOversampleWrapper(lrn1, osw.rate = 1.5)
  r = resample(lrn2, binaryclass.task, rdesc)
  expect_true(!is.na(r$aggr))
})

test_that("over and undersample arg check works", {
  task = makeClassifTask(data = multiclass.df, target = multiclass.target)
  expect_error(undersample(task, rate = 0.5), "binary")
  expect_error(oversample(task, rate = 0.5), "binary")
})

test_that("over and undersample works with weights", {
  task = makeClassifTask(data = binaryclass.df, target = binaryclass.target, weights = 1:nrow(binaryclass.df))
  task2 = undersample(task, rate = 0.5)
  expect_true(length(task2$weights) < length(task$weights))
  expect_true(all(task2$weights %in% task$weights))
})

test_that("oversampling keeps all min obs", {
  y = binaryclass.df[, binaryclass.target]
  z = getMinMaxClass(y)
  new.inds = sampleBinaryClass(y, 1.05, cl = "min", clreplace = TRUE, othreplace = FALSE)
  expect_true(setequal(intersect(z$min.inds, new.inds), z$min.inds))
})
