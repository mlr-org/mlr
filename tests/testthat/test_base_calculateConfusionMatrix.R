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
    expect_true(sum(unlist(d)) == cm$result[l + 1,l + 1])
    
    #test absolute with sums 
    cm = calculateConfusionMatrix(p, sums = TRUE)
    expect_true(is.matrix(cm$result) && nrow(cm$result) ==  l + 2 && ncol(cm$result) == l + 2)
    expect_set_equal(cm$result[1:l, l + 1], cm$result[l + 1, 1:l])
    #test absolute number of errors
    d = cm$result[1:l, 1:l]
    diag(d) = 0
    expect_true(sum(unlist(d)) == cm$result[l + 1,l + 1])
    
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
  newdata = droplevels(multiclass.df[1L,])
  m = train("classif.rpart", multiclass.task)
  p = predict(m, newdata = newdata)
  test.confMatrix(p)
  
  #failure model
  data = iris; data[,1] = 1
  lrn = makeLearner("classif.lda", config = list(on.learner.error = "quiet"))
  task = makeClassifTask(data = data, target = "Species")
  r = holdout(lrn, task, measures = ber)
  expect_error(calculateConfusionMatrix(r$pred), "FailureModel")
  
})