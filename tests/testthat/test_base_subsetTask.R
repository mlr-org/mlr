context("subsetTask")
test_that("subsetTask subsampleFeature", {
  sub.task = subsetTask(surv.task, subset = c(1:70), features = getTaskFeatureNames(surv.task)[c(1,2)])
  ff = getTaskFeatureNames(surv.task)[c(1,2)]
  sub.df = getTaskData(sub.task)
  colns = names(surv.df)
  expect_equal(sub.df, surv.df[1:70,colns[is.element(colns,union(ff,surv.target))]])  
  })

test_that("subsetTask subsampleFeatureLabel", {
  sub.feature = getTaskFeatureNames(yeast.task)[c(1,2)]
  sub.task = subsetTask(yeast.task, subset = c(1:70), 
    features = sub.feature,
    labels = getTaskTargetNames(yeast.task)[c(1,2)])
  sub.df = getTaskData(sub.task)
  df = getTaskData(yeast.task)
  ff = getTaskTargetNames(yeast.task)[c(1,2)]
  colns = names(getTaskData(yeast.task))
  expect_equal(sub.df, df[1:70,colns[is.element(colns,union(sub.feature,ff))]])  
})
