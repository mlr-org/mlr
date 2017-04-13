context("FDA_shapelets")

test_that("FDA_shapelets", {

  # k = 0.02
  # l = 0.2
  # max.iter = 5
  #
  # #gp = data.frame(v1  =  t(1:4), X1= as.factor(1))
  # #df = data.frame( X1= as.factor(c(-1,1,1,-1, 1)) ,v1 = 1:5, v2 = 12:16, v3 = 3:7, v4 = 4:8, v5 = 1:5, v6 = c(1,4,7,3,6))
  # #df = load2("demo4TS/gunpoint.RData")
  # df = as.data.frame(matrix(data = runif(1000), ncol = 100))
  # df[,"X1"] = as.factor(sample(x = c(1,-1), replace = TRUE, size = 10))
  #
  # taskTs = makeFDAClassifTask(data = df, target = "X1", positive = "1")
  # originalData = getTaskData(task = taskTs, target.extra = TRUE)
  # #taskS = convertTSTaskToNormalTask(task = taskTs, method = "shapelets"), pars = list(K = K, L = L, show.info = TRUE))
  # taskS = convertFDATaskToNormalTask(task = taskTs, method = "shapelets", pars = list(k = k, l = l, max.iter = max.iter))
  #
  # shapes = taskS$model$shapelets
  # m = taskS$model$m
  #
  #
  # expect_true(dim(shapes)[1] == round(k * dim(originalData$data)[2]))
  # expect_true(dim(shapes)[2] == round(l * dim(originalData$data)[2]))
  #
  # expect_true(dim(m)[1] == dim(originalData$data)[1])
  # expect_true(dim(m)[2] == round(k * dim(originalData$data)[2]))
  #
  # expect_true(taskS$model$max.iter == max.iter)
  # expect_true(length(taskS$model$f.val) == max.iter)
  #
  # expect_match(taskS$model$class, "Binary")
  # expect_match(taskS$model$method, "hinge")
  #
  #
  #
  # df = matrix(1:500, nrow = 5)
  # label = as.factor(c(1,2,3,2,3))
  # lm = getFDAShapeletFeatures(curves = df, label = label, method = "hinge", step = "user", step.size = 0.001, max.iter = 5)
  # expect_true(length(lm$model) == 2)

})
