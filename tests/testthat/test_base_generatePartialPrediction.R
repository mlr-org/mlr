context("generatePartialPrediction")

test_that("generatePartialPredictionData", {
  fr = train("regr.rpart", regr.task)
  dr = generatePartialPredictionData(fr, getTaskData(regr.task), c("lstat", "chas"), TRUE,
                                     fmin = c(1, NA), fmax = c(40, NA), gridsize = 5L)
  expect_that(max(dr$data$lstat), equals(40.))
  expect_that(min(dr$data$lstat), equals(1.))
  expect_that(nrow(dr$data), equals(5L^2))
  plotPartialPrediction(dr, facet = "chas")
  ## plotPartialPredictionGGVIS(dr, interaction = "chas")

  fc = train("classif.rpart", multiclass.task)
  dc = generatePartialPredictionData(fc, getTaskData(multiclass.task), c("Petal.Width", "Petal.Length"),
                                     fun = function(x) table(x) / length(x), gridsize = 5L)
  plotPartialPrediction(dc)
  ## plotPartialPredictionGGVIS(dc)
  fcp = train(makeLearner("classif.rpart", predict.type = "prob"), multiclass.task)
  expect_error(generatePartialPrediction(fcp, getTaskData(multiclass.task), "Petal.Width",
                                         function(x) quantile(x, c(.025, .5, .975))), gridsize = 5L)
  dcp = generatePartialPredictionData(fcp, getTaskData(iris.task), c("Petal.Width", "Petal.Length"),
                                      interaction = TRUE, gridsize = 5L)
  plotPartialPrediction(dcp, facet = "Petal.Length")
  ## plotPartialPredictionGGVIS(dcp, interaction = "Petal.Length")
  fs = train("surv.coxph", surv.task)
  ds = generatePartialPredictionData(fs, getTaskData(surv.task), c("Petal.Width", "Petal.Length"), gridsize = 5L)
  plotPartialPrediction(ds)
  ## plotPartialPredictionGGVIS(ds)

  db = generatePartialPredictionData(fr, getTaskData(regr.task), c("lstat", "chas"), TRUE,
                                     fun = function(x) quantile(x, c(.25, .5, .75)), gridsize = 5L)
  expect_that(colnames(db$data), equals(c("lower", "medv", "upper", "lstat", "chas")))
  plotPartialPrediction(db, facet = "chas")
  ## plotPartialPredictionGGVIS(db, interaction = "chas")

  db2 = generatePartialPredictionData(fr, getTaskData(regr.task), c("lstat", "crim"), FALSE,
                                      fun = function(x) quantile(x, c(.25, .5, .75)), gridsize = 5L)
  plotPartialPrediction(db2)
  ## plotPartialPredictionGGVIS(db2)

  fcpb = train(makeLearner("classif.rpart", predict.type = "prob"), binaryclass.task)
  bc = generatePartialPredictionData(fcpb, getTaskData(binaryclass.task), c("V11", "V12"))
  plotPartialPrediction(bc)
  ## plotPartialPredictionGGVIS(bc)
})

test_that("generateFeatureGrid", {
  dat = data.frame(
    w = seq(0, 1, length.out = 3),
    x = factor(letters[1:3]),
    y = ordered(1:3),
    z = 1:3
  )
  expect_that(generateFeatureGrid("w", dat), is_a("numeric"))
  expect_that(generateFeatureGrid("x", dat), is_a("factor"))
  expect_that(levels(generateFeatureGrid("x", dat)), equals(letters[1:3]))
  expect_that(generateFeatureGrid("y", dat), is_a("ordered"))
  expect_that(generateFeatureGrid("z", dat), is_a("integer"))

  expect_error(generateFeatureGrid("z", dat, fmin = .5, fmax = 3.5))
  expect_error(generateFeatureGrid("x", dat, fmin = 1))
  expect_error(generateFeatureGrid("a", dat))
})
