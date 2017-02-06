library(data.table)
load_all()


data = data.frame(x = rnorm(10), y = as.factor(sample(10, 2, replace = TRUE)))
data[1,1] = NA
task = makeClassifTask(data = data, target = "y")
learner = makeLearner("classif.xgboost")
model = train(learner, task)
