context("FDA_Task")
test_that("FDA_Regr_Task", {
  requirePackagesOrSkip('FDboost')
  data(fuelSubset)
  fuelsub = data.frame(heatan = fuelSubset$heatan, h2o = fuelSubset$h2o,
    NIR = fuelSubset$NIR, UVVIS = fuelSubset$UVVIS)
  fdf = list(NIR = stri_paste("NIR.", 1:231), UVVIS = stri_paste("UVVIS.", 1:134))
  fdg = list(NIR = fuelSubset$nir.lambda, UVVIS = fuelSubset$uvvis.lambda)
  task1 = makeFDARegrTask(data = fuelsub, target = "heatan", fd.features = fdf,
                          fd.grids = fdg)
  expect_class(task1, "FDARegrTask")
  expect_equal(task1$type, "fdaregr")

})

test_that("FDA_Classif_Task", {
  gunpoint = getTaskData(gunpoint.task, target.extra = FALSE)
  task2 = makeFDAClassifTask(data = gunpoint, target = "X1", fd.features = list(fd = 2:3))
  expect_class(task2, "FDAClassifTask")
  expect_equal(task2$type, "fdaclassif")
  expect_length(task2$task.desc$fd.features$fd, 2L)

  task3 = makeFDAClassifTask(data = gunpoint, target = "X1", fd.features = list(fd1 = 2:3, fd2 = 7:10))
  expect_class(task3, "FDAClassifTask")
  expect_equal(task3$type, "fdaclassif")
  expect_length(unlist(task3$task.desc$fd.features), 6L)
  expect_equal(task3$task.desc$fd.features$fd1, c("X2", "X3"))
})
