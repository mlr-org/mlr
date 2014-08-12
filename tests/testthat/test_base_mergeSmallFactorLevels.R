context("mergeSmallFactorLevels")


test_that("mergeSmallFactorLevels", {
  f1 = as.factor(rep(c("a", "b", "c"), times = c(85, 10, 5)))
  f2 = 1:65
  d = data.frame(f1 = f1)

  e = mergeSmallFactorLevels(d)
  expect_equal(levels(e$f1), c("a", "b", "c"))
  e = mergeSmallFactorLevels(d, min.perc = 0.04)
  expect_equal(levels(e$f1), c("a", "b", "c"))
  e = mergeSmallFactorLevels(d, min.perc = 0.06)
  expect_equal(levels(e$f1), c("a", "b", ".merged"))
  expect_equal(sum(e$f1 == ".merged"), 5)
  e = mergeSmallFactorLevels(d, min.perc = 0.11)
  expect_equal(levels(e$f1), c("a", ".merged"))
  expect_equal(sum(e$f1 == ".merged"), 15)

  expect_error(mergeSmallFactorLevels(d, new.level = "a"), "already a level")

  task = makeClassifTask(data = data.frame(f1 = f1, y = f1), target = "y")
  task2 = mergeSmallFactorLevels(task, min.perc = 0.11)
  expect_equal(levels(getTaskData(task2)$f1), c("a", ".merged"))
  expect_equal(getTaskData(task2)$y, f1)
})
