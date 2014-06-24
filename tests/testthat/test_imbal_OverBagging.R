context("OverBagging")

test_that("OverBagging wrapper",  {
  rdesc = makeResampleDesc("CV", iters = 2)
  lrn1 = makeLearner("classif.rpart")
  lrn2 = makeOverBaggingWrapper(lrn1, obw.rate = 2)
  r = resample(lrn2, binaryclass.task, rdesc)
  expect_true(!is.na(r$aggr))
})

test_that("OverBagging wrapper arg check works", {
  task = makeClassifTask(data = binaryclass.df, target = binaryclass.target)
  lrn1 = makeLearner("classif.rpart")
  expect_error(makeOverBaggingWrapper(lrn1, obw.rate = 0.5))
})

test_that("oversampling in each bag works", {
   y = binaryclass.df[, binaryclass.target]
   tab1 = table(y)
   task = makeClassifTask(data = binaryclass.df, target = binaryclass.target)
   lrn1 = makeLearner("classif.rpart")
   lrn2 = makeOverBaggingWrapper(lrn1, obw.rate = 5, obw.iters = 3)
   mod = train(lrn2, task)
   models = getBaggingModels(mod)
   tab = lapply(1:length(models), function(i) { 
     data[[i]] = getTaskData(task, models[[i]]$subset)
     table(data[[i]][, binaryclass.target]) }) 
   expect_equal(tab1["M"], tab[[1]]["M"])
   expect_equal(tab1["M"], tab[[length(models)]]["M"])
   expect_equal(tab1["R"], round(tab[[1]]["R"] / 5))
   expect_equal(tab1["R"], round(tab[[length(models)]]["R"] / 5))
})
