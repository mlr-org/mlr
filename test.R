library(devtools)
load_all()

lrn = makeLearner("regr.mxff", layers = 2, num.layer1 = 10, num.layer2 = 6,
  act1 = "sigmoid", act2 = "relu", learning.rate = 0.2, dropout.input = 0.5,
  predict.type = "prob")


mod = train(lrn, iris.task)
predict(mod, newdata = getTaskData(iris.task))
