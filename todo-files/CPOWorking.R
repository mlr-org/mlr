
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


cpoPca = makeCPOObject("pca", center = TRUE: logical, scale = TRUE: logical, cpo.trafo = {
  targetdata = data[target]
  data[target] = NULL
  pcr = prcomp(as.matrix(data), center = center, scale. = scale)
  data = as.data.frame(pcr$x)
  data[target] = targetdata
  control = list(rotation = pcr$rotation, center = pcr$center, scale = pcr$scale)
  data
}, cpo.retrafo = {
  as.data.frame(scale(as.matrix(data), center = control$center, scale = control$scale) %*% control$rotation)
})

cpoScale = makeCPOObject("scale", center = TRUE: logical, scale = TRUE: logical, cpo.trafo = {
  targetdata = data[target]
  data[target] = NULL
  result = scale(as.matrix(data), center = center, scale = scale)
  data[] = result
  data[target] = targetdata
  control = list(center = attr(result, "scaled:center"), scale = attr(result, "scaled:scale"))
  data
}, cpo.retrafo = {
  as.data.frame(scale(as.matrix(data) , center = control$center, scale = control$scale))
})


cpoo = cpoPca()
debugonce(cpoo$trafo)

pid.task %>>% cpoPca()

pid.task %>>% cpoScale()

debugonce(cpoo$trafo)
debugonce(cpoo$retrafo)
debugonce(trainLearner.classif.logreg)
debugonce(predictLearner.classif.logreg)
resample(makeLearner("classif.logreg"), pid.task, cv5)
resample(cpoo %>>% makeLearner("classif.logreg"), pid.task, cv5)

cpoo = cpoPca()
t = train(cpoo %>>% makeLearner("classif.logreg"), pid.task)
predict(t, pid.task)





df = as.data.frame(t(replicate(1000, rnorm(2) + 10 * rnorm(1))))

plot(df)

plot(df %>>% cpoPca())
