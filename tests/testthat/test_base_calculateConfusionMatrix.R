context("calculateConfusionMatrix")

test_that("calculateConfusionMatrix", {

  rdesc = makeResampleDesc("CV", iters = 3)
  r = resample(makeLearner("classif.rpart"), iris.task, rdesc)
  test.confMatrix(r$pred)

  r = resample(makeLearner("classif.rpart"), binaryclass.task, rdesc)
  test.confMatrix(r$pred)

  # dropped class lvls
  newdata = droplevels(multiclass.df[1L, ])
  m = train("classif.rpart", multiclass.task)
  p = predict(m, newdata = newdata)
  test.confMatrix(p)

  # failure model
  data = iris
  data[, 1] = 1
  lrn = makeLearner("classif.lda", config = list(on.learner.error = "quiet"))
  task = makeClassifTask(data = data, target = "Species")
  r = holdout(lrn, task, measures = ber)
  expect_error(calculateConfusionMatrix(r$pred), "FailureModel")
})

test_that("calculateConfusionMatrix elements are consistent with implemented measures", {

  # check values itself
  task = subsetTask(sonar.task, 1:32, features = getTaskNFeats(sonar.task))
  pred = holdout(makeLearner("classif.rpart"), task, split = 1 / 2)$pred
  truth = factor(rep(c("M", "R"), c(4, 12)))
  predicted = factor(rep(c("M", "R", "M"), c(1, 10, 5)))
  pred$data$truth = truth
  pred$data$response = predicted
  cm = calculateConfusionMatrix(pred, relative = TRUE)
  err.abs = 8
  err.rel = 8 / 16
  expect_equal(cm$relative.error, err.rel)
  expect_equal(cm$result[3, 3], err.abs)

  # check absolute counts in confusion matrix
  tp = cm$result[1, 1]
  fn = cm$result[1, 2]
  fp = cm$result[2, 1]
  tn = cm$result[2, 2]
  cp = tp + fn # condition positive
  cn = tn + fp # condition negative
  pp = tp + fp # predicted positive
  pn = tn + fn # predicted negative

  # expect_equivalent instead of expect_equal because the performance() result
  # contains an attribute (the name)
  expect_equivalent(performance(pred, ppv), tp / pp)
  expect_equivalent(performance(pred, acc), (tp + tn) / (cp + cn))
  expect_equivalent(performance(pred, bac), mean(c(tp / cp, tn / cn)))
  expect_equivalent(performance(pred, ber), mean(c(fp / cn, fn / cp)))

  expect_equivalent(performance(pred, tpr), tp / cp)
  expect_equivalent(performance(pred, fpr), fp / cn)
  expect_equivalent(performance(pred, tnr), tn / cn)
  expect_equivalent(performance(pred, fnr), fn / cp)

  # check relative confusion matrices
  expect_equivalent(colSums(cm$relative.col[1:2, 1:2]), c(1, 1))
  expect_equivalent(rowSums(cm$relative.row[1:2, 1:2]), c(1, 1))
  expect_equal(cm$relative.row[1, 1], cm$result[1, 1] / sum(cm$result[1, 1:2]))
  expect_equal(cm$relative.row[1, 2], cm$result[1, 2] / sum(cm$result[1, 1:2]))
  expect_equal(cm$relative.row[2, 1], cm$result[2, 1] / sum(cm$result[2, 1:2]))
  expect_equal(cm$relative.row[2, 2], cm$result[2, 2] / sum(cm$result[2, 1:2]))
  expect_equal(cm$relative.col[1, 1], cm$result[1, 1] / sum(cm$result[1:2, 1]))
  expect_equal(cm$relative.col[1, 2], cm$result[1, 2] / sum(cm$result[1:2, 2]))
  expect_equal(cm$relative.col[2, 1], cm$result[2, 1] / sum(cm$result[1:2, 1]))
  expect_equal(cm$relative.col[2, 2], cm$result[2, 2] / sum(cm$result[1:2, 2]))
})

test_that("calculateConfusionMatrix with different factor levels (#2030)", {

  lrn = makeLearner("classif.rpart")
  m = train(lrn, iris.task)
  nd = iris[101:150, ]
  nd$Species = factor(nd$Species)

  p = predict(m, newdata = nd)
  cm = calculateConfusionMatrix(p)
  expect_equal(cm$result[1, 4], 0)
  expect_equal(cm$result[4, 4], 5)
})


test_that("calculateConfusionMatrix set argument works", {

  mod = train("classif.lda", iris.task)
  pred1 = predict(mod, iris.task)
  rdesc = makeResampleDesc("CV", iters = 10, predict = "both")
  # here, you have set=train and set=test in pred3$data:
  pred2 = resample("classif.rpart", iris.task, rdesc)$pred

  # pred1$data has no column "set" => argument set="train" would *not* make sense
  expect_error(calculateConfusionMatrix(pred1, set = "train"))

  # pred3 was predicted on both train and test set. Both subsetted matrices should give
  # a positive total count:
  test.obs = table(pred2$data$set)["test"]
  train.obs = table(pred2$data$set)["train"]
  expect_equivalent(sum(calculateConfusionMatrix(pred2, set = "train")$result[1:3, 1:3]), train.obs)
  expect_equivalent(sum(calculateConfusionMatrix(pred2, set = "test")$result[1:3, 1:3]), test.obs)
})

test_that("calculateConfusionMatrix raises error when set argument is 'wrong'", {

  # if a resampled prediction was computed with predict = "train" and is passed
  # with set = "test" (and vice-versa), calculateConfusionMatrix should raise
  # an error

  rdesc.test = makeResampleDesc("CV", iters = 3, predict = "test")
  pred.test = resample("classif.rpart", iris.task, rdesc.test)$pred

  expect_error(calculateConfusionMatrix(pred.test, set = "train"))
})

test_that("calculateConfusionMatrix returns all-zero matrix when prediction object is empty", {
  mod = train("classif.lda", iris.task)
  pred = predict(mod, iris.task)
  pred$data = pred$data[FALSE, ]
  expect_true(all(calculateConfusionMatrix(pred)$result == 0))
})
