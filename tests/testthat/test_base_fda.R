context("fda tasks and operations")

test_that("FDARegrTask", {
  requirePackagesOrSkip("FDboost")
  data(fuelSubset)
  fuelsub = data.frame(heatan = fuelSubset$heatan, h2o = fuelSubset$h2o,
    NIR = fuelSubset$NIR, UVVIS = fuelSubset$UVVIS)
  fdf = list(NIR = stri_paste("NIR.", 1:231), UVVIS = stri_paste("UVVIS.", 1:134))
  fdg = list(NIR = fuelSubset$nir.lambda, UVVIS = fuelSubset$uvvis.lambda)
  task1 = makeFDARegrTask(data = fuelsub, target = "heatan", fd.features = fdf,
                          fd.grids = fdg)
  expect_class(task1, "FDARegrTask")
  expect_equal(task1$type, "fdaregr")
  expect_error(subsetTask(fuelsubset.task, features = 1:1000), regexp = "All elements must be")
  expect_equal(ncol(fuelsub), ncol(getTaskData(task1)))
  expect_equal(getTaskData(task1, target.extra = TRUE)$target, fuelsub$heatan)
})

test_that("FDAClassifTask", {
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
  expect_equal(getTaskData(task2, target.extra = TRUE)$target, gunpoint$X1)
  expect_equal(getTaskData(task3, target.extra = TRUE)$target, gunpoint$X1)
})

test_that("FDA_Task_error_regr", {
  requirePackagesOrSkip("FDboost")
  data(fuelSubset)
  fuelsub = data.frame(heatan = fuelSubset$heatan, h2o = fuelSubset$h2o,
    NIR = fuelSubset$NIR, UVVIS = fuelSubset$UVVIS)
  fdf0 = list(NIR = 3:234, UVVIS = 235:240)
  fdf1 = list(NIR = 1:231, UVVIS = 1:134)  # two functional covariate can't occupy the same variable
  fdf2 = list(UVVIS = c("hello", "world")) # input column names that does not exist
  fdf3 = list(NIR = 3:8, UVVIS = stri_paste("UVVIS", 1:6)) # mixed case input (both character and integer)
  fdf4 = list(NIR = 1:367)  # functional covariate can't be greater or equal to ncol of the dataframe
  fdf5 = list(NIR = "heatan")  # functional covariate can't be the target !
  fdf6 = list(NIR = 1L, NIR = 3:4) # functional covariate contain same name
  fdf7 = list(NIR = 1:10)  # can't have target name as features
  fdg = list(NIR = fuelSubset$nir.lambda, UVVIS = fuelSubset$uvvis.lambda)

  expect_error(makeFDARegrTask(data = fuelsub, target = "heatan", fd.features = fdf0, fd.grids = fdg))
  expect_error(makeFDARegrTask(data = fuelsub, target = "heatan", fd.features = fdf1,
    fd.grids = fdg))
  expect_error(makeFDARegrTask(data = fuelsub, target = "heatan", fd.features = fdf2))
  expect_error(makeFDARegrTask(data = fuelsub, target = "heatan", fd.features = fdf3,
    fd.grids = fdg))
  expect_error(makeFDARegrTask(data = fuelsub, target = "heatan", fd.features = fdf4))
  expect_error(makeFDARegrTask(data = fuelsub, target = "heatan", fd.features = fdf5))
  expect_error(makeFDARegrTask(data = fuelsub, target = "heatan", fd.features = fdf6, fd.grids = fdg))
  expect_error(makeFDARegrTask(data = fuelsub, target = "heatan", fd.features = fdf7, fd.grids = fdg))
})

test_that("FDA_Task_error_classif", {
  gunpoint = getTaskData(gunpoint.task, target.extra = FALSE)
  expect_error(makeFDAClassifTask(data = gunpoint, target = "X1", fd.features = list(fd = 1:3)))
  expect_error(makeFDAClassifTask(data = gunpoint, target = "X1", fd.features = list(fd = 2:3), fd.grids = list(fd = 1:3)))
})


test_that("FDA_subsetTask", {
  st = subsetTask(fuelsubset.task, features = 1:200)
  expect_equal(length(st$task.desc$fd.features$NIR), 66L)
  expect_equal(length(st$task.desc$fd.features$UVVIS), 134L)
  expect_equal(length(st$task.desc$fd.grids$NIR), 66L)
  expect_equal(length(st$task.desc$fd.grids$UVVIS), 134L)
  st.gp = subsetTask(gunpoint.task, features = 1:10)
  expect_equal(length(st.gp$task.desc$fd.features$fd1), 10L)
  expect_equal(length(st.gp$task.desc$fd.grids$fd1), 10L)
})

