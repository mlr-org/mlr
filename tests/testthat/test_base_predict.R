context("predict")

test_that("predict", {
  inds = multiclass.train.inds
  data = multiclass.df
  formula = multiclass.formula

  wl.lda = makeLearner("classif.lda", predict.type = "prob")

  cm2 = train(makeLearner("classif.lda"), multiclass.task, subset = inds)
  cp2 = predict(cm2, newdata = data[inds,])
  cp2b = predict(cm2, newdata = data[inds,-5])
  ext2 = lda(formula, data = data[inds,])
  pred2 = predict(ext2,newdata = data[inds,])$class

  expect_equal(cp2$data$response, pred2)
  expect_equal(cp2b$data$response, pred2)

  cm3 = train(wl.lda, multiclass.task, subset = inds)
  cp3 = predict(cm3, newdata = data[multiclass.test.inds,])
  ext3 = lda(formula, data = data[inds,])
  pred3 = predict(ext3,newdata = data[multiclass.test.inds,])$class
  prob3 = predict(ext3,newdata = data[multiclass.test.inds,])$post
  expect_equal(cp3$data$response, pred3)
  expect_equal(prob3, as.matrix(getProbabilities(cp3, colnames(prob3))))
  expect_true(is.data.frame(getProbabilities(cp3, "setosa")))
  expect_true(is.numeric(getProbabilities(cp3, "setosa")[, 1]))
  expect_equal(colnames(getProbabilities(cp3, c("setosa", "versicolor"))), c("setosa", "versicolor"))
  expect_equal(colnames(getProbabilities(cp3, c("versicolor", "setosa"))), c("versicolor", "setosa"))

  cp4 = predict(cm3, task = multiclass.task, subset = multiclass.test.inds)
  expect_equal(cp4$data$response, pred3)
  expect_equal(cp4$data$truth, data[multiclass.test.inds, multiclass.target])
  expect_equal(cp4$data$id, multiclass.test.inds)

  df3 = as.data.frame(cp3)
  df4 = as.data.frame(cp4)
  expect_equal(df3, df4[,-1])

  cm5 = train(wl.lda, binaryclass.task, subset = binaryclass.train.inds)
  cp5a = predict(cm5, task = binaryclass.task, subset = binaryclass.test.inds)
  cp5b = predict(cm5, task = binaryclass.task, subset = binaryclass.test.inds)
  cp5c = setThreshold(cp5b, 0)
  cp5d = setThreshold(cp5b, 1)
  cp5e = predict(cm5, task = binaryclass.task, subset = 1)
  expect_equal(cp5a$data$response, cp5b$data$response)
  f1 = factor(rep(binaryclass.task$task.desc$positive, length(binaryclass.test.inds)),
    levels = binaryclass.task$task.desc$class.levels)
  expect_equal(cp5c$data$response, f1)
  f2 = factor(rep(binaryclass.task$task.desc$negative, length(binaryclass.test.inds)),
    levels = binaryclass.task$task.desc$class.levels)
  expect_equal(cp5d$data$response, f2)
  expect_true(setequal(levels(cp5e$data$response), c("M", "R")))

})


test_that("predict works with type = se", {
  lrn = makeLearner("regr.lm", predict.type = "se")
  mod = train(lrn, regr.task)
  p = predict(mod, regr.task)
  expect_equal(colnames(p$data), c("id", "truth", "response", "se"))
})


test_that("predict works with strange class labels", {
  df = binaryclass.df
  levels(df[,binaryclass.target]) = c(-1,1)
  task = makeClassifTask(data = df, target = binaryclass.target)
  mod = train(makeLearner("classif.lda", predict.type = "prob"), task = task)
  p = predict(mod, task = task)
  expect_equal(colnames(p$data), c("id", "truth", "prob.-1", "prob.1", "response"))
})


test_that("predict correctly propagates exception in predictLearner", {
  capture.output(expect_error(holdout("classif.mock1", multiclass.task), "foo"))
})

test_that("predict works with newdata / subset", {
  mod = train(makeLearner("classif.lda"), multiclass.task)
  p = predict(mod, newdata = multiclass.df, subset = 1:10)
  expect_equal(nrow(p$data), 10)
})
