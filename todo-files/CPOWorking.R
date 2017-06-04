
library("roxygen2")

roxygenise("..")


devtools::load_all("..")
options(error=dump.frames)
configureMlr(show.info=TRUE, on.learner.error="warn", show.learner.output=TRUE)


mkl = function() {
  res = makeRLearnerClassif("testlearner", package=character(0), par.set = paramSetSugar(a1: integer(0, 10), a2: discrete(a, b, c)),
                            properties = c("twoclass", "multiclass", "numerics", "factors"))
  res$fix.factors.prediction = TRUE
  res
}

trainLearner.testlearner = function(.learner, .task, .subset, .weights = NULL, ...) {
  print(head(getTaskData(.task)))
  getTaskData(.task, .subset)[getTaskTargetNames(.task)]
}

predictLearner.testlearner = function(.learner, .model, .newdata, ...) {
  print(head(.newdata))
  sample(.model$learner.model[[1]], nrow(.newdata), replace = TRUE)
}

debugonce(trainLearner.testlearner)
debugonce(predictLearner.testlearner)


t <- train(mkl(), pid.task)

p <- predict(t, pid.task)

testcpo = makeCPOObject("testCPO1", a1 = 0: integer(0, 10), cpo.trafo = {
  data[[1]] = data[[1]] - a1
  list(data = data, control = a1)
}, cpo.retrafo = {
  print(control)
  data[[1]] = data[[1]] - control ^ 2
  data
})


testcpo(10) %>>% testcpo(id="second", 20)

train(mkl(), pid.task)
train(testcpo(0, id='x') %>>% mkl(), pid.task)
t = train(testcpo(10, id='x') %>>% mkl(), pid.task)

predict(t, pid.task)

debugonce(predictLearner.CPOObjectLearner)
debugonce(trainLearner.CPOObjectLearner)

t$learner.model$next.model$learner.model
