context("smote")

test_that("smote works",  {
  y = binaryclass.df[, binaryclass.target]
  tab1 = table(y)
  task = smote(binaryclass.task, rate = 2)
  df = getTaskData(task)
  tab2 = table(df[, binaryclass.target])
  expect_equal(tab2["M"], tab1["M"])
  expect_equal(tab2["R"], tab1["R"] * 2)
})

test_that("smote works with rate 1 (no new examples)",  {
  y = binaryclass.df[, binaryclass.target]
  tab1 = table(y)
  task = smote(binaryclass.task, rate = 1)
  df = getTaskData(task)
  tab2 = table(df[, binaryclass.target])
  expect_equal(tab2["M"], tab1["M"])
  expect_equal(tab2["R"], tab1["R"])
})

test_that("smote wrapper",  {
  rdesc = makeResampleDesc("CV", iters = 2)
  lrn1 = makeLearner("classif.rpart")
  lrn2 = makeSMOTEWrapper(lrn1, sw.rate = 2)
  r = resample(lrn2, binaryclass.task, rdesc)
  expect_true(!is.na(r$aggr))
})
