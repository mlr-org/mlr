library(devtools)
load_all()

configureMlr(on.learner.error = "warn")

task = subsetTask(sonar.task, 1:100)
lrn = makeLearner("classif.rpart")
lrn = makeSMOTEWrapper(lrn, sw.nn = 10)
m = train(lrn, task)
p = predict(m, task = task)
print(p)
