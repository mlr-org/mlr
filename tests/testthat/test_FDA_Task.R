context("FDA_TASK")
test_that("FDA_Task", {
requirePackagesOrSkip('FDboost')
#library(FDboost)
data(fuelSubset)
fuelsub = data.frame(heatan = fuelSubset$heatan, h2o = fuelSubset$h2o,
  NIR = fuelSubset$NIR, UVVIS = fuelSubset$UVVIS)
fdf = list(NIR = paste0("NIR.", 1:231), UVVIS = paste0("UVVIS.", 1:134))
fdg = list(NIR = fuelSubset$nir.lambda, UVVIS = fuelSubset$uvvis.lambda)
task1 = makeFDARegrTask(data = fuelsub, target = "heatan",
  fd.features = fdf, fd.grids = fdg)
#print(task1)
# d = load2("demo4FDA/gunpoint.RData")
#task2 = makeFDAClassifTask(data = d, target = "X1", fd.features = list(fd = 2:3))
gunpoint = getTaskData(gunpoint.task, target.extra = FALSE)
task2 = makeFDAClassifTask(data = gunpoint, target = "X1", fd.features = list(fd = 2:3))
})