
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
  getTaskData(.task, .subset)[[getTaskTargetNames(.task)[1]]][1]
}

predictLearner.testlearner = function(.learner, .model, .newdata, ...) {
  print(head(.newdata))
  rep(.model$learner.model, nrow(.newdata))
}

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
t = train(testcpo(10, id='y') %>>% testcpo(10, id='x') %>>% mkl(), pid.task)

lrnx = testcpo(10, id='y') %>>% testcpo(10, id='x') %>>% mkl()

x <- predict(train(setHyperPars(lrnx, y.a1=2), pid.task), pid.task)

p <- predict(t, pid.task)


t$learner.model$next.model$learner.model

testcpof = makeCPOFunctional("testCPO2", a1 = 0: integer(0, 10), b1: discrete(1, 2, 3), cpo.trafo = {
  data[[1]] = data[[1]] - a1
  attr(data, "retrafo") = function(data) {
    print(a1)
    print(b1)
    data[[1]] = data[[1]] - a1 ^ 2
    data
  }
})



testcpof(10, b1=2)

str(x)

attributes(testcpof(0))

