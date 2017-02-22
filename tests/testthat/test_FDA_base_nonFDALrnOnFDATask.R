context("FDA_base_task")
test_that("FDA_base_task", {
data = data.frame(x = rnorm(10), y = as.factor(sample(10, 2, replace = TRUE)))
data[1,1] = NA
task = makeFDAClassifTask(data = data, target = "y")
learner = makeLearner("classif.xgboost")
model = train(learner, task)
})