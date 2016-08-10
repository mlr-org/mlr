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

test_that("oversampling keeps all min / max obs", {
  y = binaryclass.df[, binaryclass.target]
  z = getMinMaxClass(y)
  new.inds = sampleBinaryClass(y, 1.05, cl = z$min.name, resample.other.class = FALSE)
  expect_true(setequal(intersect(z$min.inds, new.inds), z$min.inds))
})

test_that("control which class gets over or under sampled", {
  set.seed(getOption("mlr.debug.seed"))
  #check function oversample(), undersample()
  y = binaryclass.df[, binaryclass.target]
  tab1 = table(y)
  z = getMinMaxClass(y)
  task = oversample(binaryclass.task, rate = 2, cl = z$max.name)
  df = getTaskData(task)
  tab2 = table(df[, binaryclass.target])
  expect_equal(tab2["R"], tab1["R"])
  expect_equal(tab2["M"], tab1["M"] * 2)
  task = undersample(binaryclass.task, rate = 0.5, cl = z$min.name)
  df = getTaskData(task)
  tab2 = table(df[, binaryclass.target])
  expect_equal(tab2["R"], round(tab1["R"] / 2))
  expect_equal(tab2["M"], tab1["M"])

  #check over- and undersample-wrapper
  z = getMinMaxClass(binaryclass.df[, binaryclass.target])
  rdesc = makeResampleDesc("CV", iters = 2)
  lrn1 = makeLearner("classif.rpart")
  lrn2 = makeUndersampleWrapper(lrn1, usw.rate = 0.1, usw.cl = z$min.name)
  r = resample(lrn2, binaryclass.task, rdesc)
  expect_true(!is.na(r$aggr))
  lrn2 = makeOversampleWrapper(lrn1, osw.rate = 1.5, osw.cl = z$max.name)
  r = resample(lrn2, binaryclass.task, rdesc)
  expect_true(!is.na(r$aggr))
})
