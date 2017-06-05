
library("roxygen2")

roxygenise("..")


rm(cpotest.parvals)

devtools::load_all("..")
options(error = dump.frames)
configureMlr(show.info = TRUE, on.learner.error = "stop", show.learner.output = TRUE)

debugger()

library("testthat")
devtools::test(pkg = "..", filter = "cpo")
devtools::test(pkg = "..", filter = "ParamSetSugar")

mkl = function() {
  res = makeRLearnerClassif("testlearner", package = character(0), par.set = paramSetSugar(a1: integer(0, 10), a2: discrete(a, b, c)),
                            properties = c("twoclass", "multiclass", "numerics", "factors"))
  res$fix.factors.prediction = TRUE
  res
}

trainLearner.testlearner = function(.learner, .task, .subset, .weights = NULL, ...) {
  print(head(getTaskData(.task)))
  getTaskData(.task, .subset)[[getTaskTargetNames(.task)[1]]][1]
}

predictLearner.testlearner = function(.learner, .model, .newdata, ...) {
  print(head(.newdata))
  rep(.model$learner.model, nrow(.newdata))
}

t = train(mkl(), pid.task)
p = predict(t, pid.task)



cpoo = cpoPca()

pid.task %>>% cpoPca
debugonce(cpoo$trafo)

pid.task %>>% cpoPca()

pid.task %>>% cpoScale()

debugonce(cpoo$trafo)
debugonce(cpoo$retrafo)
debugonce(trainLearner.classif.logreg)
debugonce(predictLearner.classif.logreg)
resample(makeLearner("classif.naiveBayes"), pid.task, cv5)
resample(cpoPca() %>>% makeLearner("classif.naiveBayes"), pid.task, cv5)



cpoo = cpoPca()
t = train(cpoScale() %>>% makeLearner("classif.logreg"), pid.task)
predict(t, pid.task)





df = as.data.frame(t(replicate(1000, rnorm(2) + 10 * rnorm(1))))

plot(df)

plot(df %>>% cpoPca())

cpo = cpoPca(scale = FALSE)

setHyperPars(cpo, list(center = FALSE))


