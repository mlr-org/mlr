context("fda multiclass")

test_that("FDAClassifMultiClassTask", {
  task2 = makeFDAClassifTask(data = iris, target = "Species")
  expect_class(task2, "FDAClassifTask")
  expect_equal(task2$type, "fdaclassif")
  expect_length(task2$task.desc$fd.features$fd, 4L)

  task3 = makeFDAClassifTask(data = iris, target = "Species", fd.features = list(fd1 = 1:2, fd2 = 3:4))
  expect_class(task3, "FDAClassifTask")
  expect_equal(task3$type, "fdaclassif")
  expect_length(unlist(task3$task.desc$fd.features), 4L)
  expect_equal(task3$task.desc$fd.features$fd1, c("Sepal.Length", "Sepal.Width"))
  expect_equal(getTaskData(task2, target.extra = TRUE)$target, iris$Species)
  expect_equal(getTaskData(task3, target.extra = TRUE)$target, iris$Species)
})


test_that("measures for multilclass", {
  requirePackagesOrSkip("fda.usc", default.method = "load")
  lrn = makeLearner("fdaclassif.knn", par.vals = list(knn = 1L, trim = 0.5))
  task = makeFDAClassifTask(data = iris, target = "Species")
  holdout("fdaclassif.knn", task, measures = tpr)
  holdout("fdaclassif.knn", task, measures = multiclass.au1p)
})

