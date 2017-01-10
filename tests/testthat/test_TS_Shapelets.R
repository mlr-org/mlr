context("Ts_Shapelets")

test_that("Ts_Shapelets", {

  K = 0.02
  L = 0.2
  max.iter = 100

  #gp = data.frame(v1  =  t(1:4), X1= as.factor(1))
  #df = data.frame( X1= as.factor(c(-1,1,1,-1, 1)) ,v1 = 1:5, v2 = 12:16, v3 = 3:7, v4 = 4:8, v5 = 1:5, v6 = c(1,4,7,3,6))
  #df = load2("demo4TS/gunpoint.RData")
  df = as.data.frame(matrix(data = runif(1000), ncol = 100))
  df[,"X1"] = as.factor(sample(x = c(1,-1), replace = TRUE, size = 10))

  taskTs = makeTimeSeriesClassifTask(data = df, target = "X1", positive = "1")
  originalData = getTaskData(task = taskTs, target.extra = TRUE)
  #taskS = makeTSFeaturesClassifTask(task = taskTs, method = "shapelets"), pars = list(K = K, L = L, show.info = TRUE))
  taskS = makeTSFeaturesClassifTask(task = taskTs, method = "shapelets")

  shapes = taskS$model[[1]]$shapelets
  m = taskS$model[[1]]$M


  expect_true(dim(shapes)[1] == round(K * dim(originalData$data)[2]))
  expect_true(dim(shapes)[2] == round(L * dim(originalData$data)[2]))

  expect_true(dim(m)[1] == dim(originalData$data)[1])
  expect_true(dim(m)[2] == round(K * dim(originalData$data)[2]))

  expect_true(taskS$model[[1]]$max.iter <= max.iter)

})
