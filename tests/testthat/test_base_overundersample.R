context("over and undersample")

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


