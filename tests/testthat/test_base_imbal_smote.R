context("smote")

test_that("smote works",  {
  y = binaryclass.df[, binaryclass.target]
  tab1 = table(y)
  task = smote(binaryclass.task, rate = 2)
  df = getTaskData(task)
  tab2 = table(df[, binaryclass.target])
  expect_equal(tab2["M"], tab1["M"])
  expect_equal(tab2["R"], tab1["R"] * 2)

  # check trivial error check
  d = data.frame(
    x1 = rep(c("a", "b"), 3, replace = TRUE),
    y = rep(c("a", "b"), 3, replace = TRUE)
  )
  task = makeClassifTask(data = d, target = "y")
  expect_error(smote(task, rate = 2), "minimal class has size 3")
  expect_error(smote(task, rate = 2, standardize = TRUE), "minimal class has size 3")
  expect_error(smote(task, rate = 2, alt.logic = TRUE), "minimal class has size 3")
})

test_that("smote works with rate 1 (no new examples)",  {
  y = binaryclass.df[, binaryclass.target]
  tab1 = table(y)
  task = smote(binaryclass.task, rate = 1)
  df = getTaskData(task)
  tab2 = table(df[, binaryclass.target])
  expect_equal(tab2["M"], tab1["M"])
  expect_equal(tab2["R"], tab1["R"])

  taskAlt = smote(binaryclass.task, rate = 1, alt.logic = TRUE)
  dfAlt = getTaskData(taskAlt)
  tab2Alt = table(dfAlt[, binaryclass.target])
  expect_equal(tab2Alt["M"], tab1["M"])
  expect_equal(tab2Alt["R"], tab1["R"])
})

test_that("smote works with only factor features",  {
  n = 10
  d = data.frame(
    x1 = sample(c("a", "b"), n, replace = TRUE),
    x2 = sample(c("a", "b"), n, replace = TRUE),
    y = c(rep("a",2),rep("b",8))
  )
  task = makeClassifTask(data = d, target = "y")
  task2 = smote(task, rate = 1.4, nn = 2L)
  expect_equal(getTaskSize(task2), 11)
  task3 = smote(task, rate = 2, nn = 2L, alt.logic = TRUE)
  expect_equal(getTaskSize(task3), 12)
})

test_that("smote wrapper",  {
  rdesc = makeResampleDesc("CV", iters = 2)
  lrn1 = makeLearner("classif.rpart")
  lrn2 = makeSMOTEWrapper(lrn1, sw.rate = 2)
  r = resample(lrn2, binaryclass.task, rdesc)
  expect_true(!is.na(r$aggr))
  lrn3 = makeSMOTEWrapper(lrn1, sw.rate = 2, sw.standardize = TRUE)
  r = resample(lrn3, binaryclass.task, rdesc)
  expect_true(!is.na(r$aggr))
  lrn4 = makeSMOTEWrapper(lrn1, sw.rate = 2, sw.alt.logic = TRUE)
  r = resample(lrn4, binaryclass.task, rdesc)
  expect_true(!is.na(r$aggr))
})
