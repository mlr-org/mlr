context("calculateConfusionMatrix")

test_that("calculateConfusionMatrix", {

  test.confMatrix = function(p) {
    lvls = getTaskClassLevels(p$task.desc)
    n = getTaskSize(p$task.desc)
    l = length(lvls)

    #test absolute
    cm = calculateConfusionMatrix(p, relative = FALSE)
    expect_true(is.matrix(cm$result) && nrow(cm$result) ==  l + 1 && ncol(cm$result) == l + 1)
    expect_set_equal(cm$result[1:l, l + 1], cm$result[l + 1, 1:l])
    #test absolute number of errors
    d = cm$result[1:l, 1:l]
    diag(d) = 0
    expect_true(sum(unlist(d)) == cm$result[l + 1, l + 1])

    #test absolute with sums
    cm = calculateConfusionMatrix(p, sums = TRUE)
    expect_true(is.matrix(cm$result) && nrow(cm$result) ==  l + 2 && ncol(cm$result) == l + 2)
    expect_set_equal(cm$result[1:l, l + 1], cm$result[l + 1, 1:l])
    #test absolute number of errors
    d = cm$result[1:l, 1:l]
    diag(d) = 0
    expect_true(sum(unlist(d)) == cm$result[l + 1, l + 1])

    #test relative
    cm = calculateConfusionMatrix(p, relative = TRUE)

    #sums have to be 1 or 0 (if no observation in that group)
    expect_true(all(rowSums(cm$relative.row[, 1:l]) == 1 |
        rowSums(cm$relative.row[, 1:l]) == 0))
    expect_true(all(colSums(cm$relative.col[1:l, ]) == 1 |
        colSums(cm$relative.col[1:l, ]) == 0))
  }


  rdesc = makeResampleDesc("CV", iters = 3)
  r = resample(makeLearner("classif.rpart"), iris.task, rdesc)
  test.confMatrix(r$pred)

  r = resample(makeLearner("classif.rpart"), binaryclass.task, rdesc)
  test.confMatrix(r$pred)


  #dropped class lvls
  newdata = droplevels(multiclass.df[1L, ])
  m = train("classif.rpart", multiclass.task)
  p = predict(m, newdata = newdata)
  test.confMatrix(p)

  #failure model
  data = iris; data[, 1] = 1
  lrn = makeLearner("classif.lda", config = list(on.learner.error = "quiet"))
  task = makeClassifTask(data = data, target = "Species")
  r = holdout(lrn, task, measures = ber)
  expect_error(calculateConfusionMatrix(r$pred), "FailureModel")
})

test_that("calculateConfusionMatrix elements are consistent with implemented measures", {
  #check values itself
  task = subsetTask(sonar.task, 1:32)
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
  cp = tp + fn  # condition positive
  cn = tn + fp  # condition negative
  pp = tp + fp  # predicted positive
  pn = tn + fn  # predicted negative

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
    pred1 = crossval("classif.rpart", iris.task)$pred
    pred2 = predict(mod, iris.task)
    rdesc = makeResampleDesc("CV", iters = 10, predict = "both")
    # here, you have set=train and set=test in pred3$data:
    pred3 = resample("classif.rpart", iris.task, rdesc)$pred

    # pred1 was only predicted on test data, so a subset to train data has no entries:
    expect_equal(sum(calculateConfusionMatrix(pred1, set = "train")$result), 0)

    # pred2$data has no column "set" => argument set="train" would *not* make sense
    expect_error(calculateConfusionMatrix(pred2, set = "train"))

    # pred3 was predicted on both train and test set. Both subsetted matrices should give
    # a positive total count:
    expect_gt(sum(calculateConfusionMatrix(pred3, set = "train")$result), 0)
    expect_gt(sum(calculateConfusionMatrix(pred3, set = "test")$result), 0)
})
