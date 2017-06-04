
library("roxygen2")

roxygenise("..")


devtools::load_all("..")
options(error = dump.frames)
configureMlr(show.info = TRUE, on.learner.error = "stop", show.learner.output = TRUE)


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

testcpo = makeCPOObject("testCPO1", a1 = 0: integer(0, 10), b: discrete(1, 2, 3), cpo.trafo = {
  data[[1]] = data[[1]] - a1
  control = a1
  data
}, cpo.retrafo = {
  print(control)
  data[[1]] = data[[1]] - control ^ 2
  data
})

testcpo()

testcpo(10) %>>% testcpo(id = "second", 20)

train(mkl(), pid.task)

x = predict(train(testcpo(2, 1, id = "x") %>>% mkl(), pid.task), pid.task)

t = train(testcpo(10, 1, id = "y") %>>% testcpo(10, 2, id = "x") %>>% mkl(), pid.task)

debugger()


lrnx = testcpo(10, id = "y") %>>% testcpo(10, id = "x") %>>% mkl()

x = predict(train(setHyperPars(lrnx, y.a1 = 2, x.b = 1, y.b = 2), pid.task), pid.task)

p = predict(t, pid.task)


t$learner.model$next.model$learner.model

testcpof = makeCPOFunctional("testCPO2", a1 = 0: integer(0, 10), b1: discrete(1, 2, 3), cpo.trafo = {
  data[[1]] = data[[1]] - a1
  attr(data, "retrafo") = function(data) {
    print(a1)
    print(b1)
    data[[1]] = data[[1]] - a1 ^ 2
    data
  }
  data
})


debugonce(testcpof)

testcpof(0, id = "x")

testcpof(10, b1 = 2) %>>% testcpof(id = "test")

getParamSet(testcpof(id = "test"))
getParamSet(testcpof(10, b1 = 2))


ot = testcpof(4, id = "x") %>>% testcpof(2, 2, id = "y")
ot2 = setHyperPars(ot, x.b1 = 1, x.a1 = -1)
t = predict(train(ot2 %>>% mkl(), pid.task), pid.task)
debugger()

res = train(testcpof(0, 1, id = "x") %>>% mkl(), pid.task)
res2 = train(testcpof(6, 1, id = "x") %>>% mkl(), pid.task)
res3 = train(testcpof(4, 1, id = "x") %>>% testcpof(2, 2, id = "y") %>>% mkl(), pid.task)

t = predict(res, pid.task)
t = predict(res2, pid.task)
t = predict(res3, pid.task)



debugonce(trainLearner.CPOFunctionalLearner)

debugger()



