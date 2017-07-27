context("fda_classif_shapelets")

test_that("fda_classif_shapelets", {
  requirePackagesOrSkip("shapeletLib", default.method = "load")

  set.seed(getOption("mlr.debug.seed"))
  gp = getTaskData(gunpoint.task)
  #gp = load2("../../demo4FDA/gunpoint.RData")
  #df = as.data.frame(matrix(data = runif(1000), ncol = 100))
  #df[,"X1"] = as.factor(sample(x = c(1,-1), replace = TRUE, size = 10))

  m = shapeletLib::learnShapeletModel(data = gp[1:50,-1], label = as.factor(gp[1:50,1]))
  p1 = predict(object = m, newdata = as.matrix(gp[51:200,-1]))
  levs = c(2,1)
  p1 = as.factor(ifelse(p1 > 0, levs[2L], levs[1L]))


  lrn = makeLearner("fdaclassif.shapelet")
  task = makeFDAClassifTask(data = gp, target = "X1", positive = "1")
  m = try(train(lrn, task, subset = 1:50))
  cp = predict(m, task, subset = 51:200)

  expect_equal(as.character(cp$data$response), as.character(p1))

})
