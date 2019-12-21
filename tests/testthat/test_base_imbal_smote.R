context("smote")

test_that("smote works", {
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

test_that("smote works with rate 1 (no new examples)", {
  y = binaryclass.df[, binaryclass.target]
  tab1 = table(y)
  task = smote(binaryclass.task, rate = 1)
  df = getTaskData(task)
  tab2 = table(df[, binaryclass.target])
  expect_equal(tab2["M"], tab1["M"])
  expect_equal(tab2["R"], tab1["R"])

  task.alt = smote(binaryclass.task, rate = 1, alt.logic = TRUE)
  df.alt = getTaskData(task.alt)
  tab2alt = table(df.alt[, binaryclass.target])
  expect_equal(tab2alt["M"], tab1["M"])
  expect_equal(tab2alt["R"], tab1["R"])
})

test_that("smote works with only factor features", {
  n = 10
  d = data.frame(
    x1 = sample(c("a", "b"), n, replace = TRUE),
    x2 = sample(c("a", "b"), n, replace = TRUE),
    y = c(rep("a", 2), rep("b", 8))
  )
  task = makeClassifTask(data = d, target = "y")
  task2 = smote(task, rate = 1.4, nn = 2L)
  expect_equal(getTaskSize(task2), 11)
  task3 = smote(task, rate = 2, nn = 2L, alt.logic = TRUE)
  expect_equal(getTaskSize(task3), 12)
})

test_that("smote wrapper", {
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

  # test that param "nn" is passed down, we had a bug here, see PR #742
  # it is hard to test the effect of nn in a the stochastic algo so we test that we get
  # an error in smote() when the value is too large.
  lrn4 = makeSMOTEWrapper(lrn1, sw.nn = 100)
  expect_error(resample(lrn4, binaryclass.task, rdesc), "when the minimal class has size")
})

test_that("smote works with only integer features", {
  dat = getTaskData(pid.task)
  i = sapply(dat, is.numeric)
  dat[, i] = lapply(dat[, i], as.integer)
  tsk = makeClassifTask(data = dat, target = "diabetes")
  task2 = smote(tsk, 2)
  expect_equal(getTaskSize(task2), 1036)
})

test_that("smote works with constant factor features", {
 # This reproduces the bug from issue #1951
 d = data.frame(
   x1 = rpois(100, 2),
   x2 = gl(5, 20, labels = LETTERS[1:5]),
   y = as.factor(c(rep("+", 90), rep("-", 10)))
 )

 task = makeClassifTask(data = d, target = "y")
 task2 = smote(task, rate = 9, nn = 4L)

 expect_equal(table(getTaskData(task2)$x2, getTaskData(task2)$y)[5, 1], 90)
})
