context("removeConstantFeatures")

test_that("removeConstantFeatures", {

  testData = data.frame(
    a = c(1L,2L),
    b = as.factor(1:2),
    c = c("a","b"),
    d = c(TRUE,FALSE),
    e = c(NA,1),
    f = c(1,NA),
    g = c(1,1),
    n = c(0, 1 - 0.7 - 0.3),
    target = as.factor(1:2)
  )
  testData = testData[c(rep(1,9),2),]
  testData$safe = seq_row(testData)

  testData_task = testData
  testData_task$d = as.factor(testData_task$d)
  task = makeClassifTask("test", data=testData_task, target="target")

  task_res = getTaskData(removeConstantFeatures(task, perc=0.1, dont.rm="g"))
  testData_res = removeConstantFeatures(testData, perc=0.1, dont.rm=c("g","target"))
  expect_equal(testData_res, task_res)
  expect_equal(colnames(testData_res), c("g", "target", "safe"))

  testData_res = removeConstantFeatures(testData)
  expect_equal(colnames(testData_res), c("a","b","c","d","target", "safe"))

  testData_res = removeConstantFeatures(testData, tol=0)
  expect_true(setequal(colnames(testData_res), c("a","b","c","d","target", "safe", "n")))

  testData_res = removeConstantFeatures(testData, na.mode="distinct", perc=0.2)
  expect_equal(colnames(testData_res), c("e", "safe"))
  testData_res = removeConstantFeatures(testData, na.mode="single", perc=0.2)
  expect_equal(colnames(testData_res), "safe")
})
