context("FDA_TASK")
test_that("FDA_Task", {
  requirePackagesOrSkip('FDboost')
  data(fuelSubset)
  fuelsub = data.frame(heatan = fuelSubset$heatan, h2o = fuelSubset$h2o,
    NIR = fuelSubset$NIR, UVVIS = fuelSubset$UVVIS)
  fdf = list(NIR = paste0("NIR.", 1:231), UVVIS = paste0("UVVIS.", 1:134))
  fdg = list(NIR = fuelSubset$nir.lambda, UVVIS = fuelSubset$uvvis.lambda)
  task1 = makeFDARegrTask(data = fuelsub, target = "heatan",
    fd.features = fdf, fd.grids = fdg)
  gunpoint = getTaskData(gunpoint.task, target.extra = FALSE)
  task2 = makeFDAClassifTask(data = gunpoint, target = "X1", fd.features = list(fd = 2:3))
})
