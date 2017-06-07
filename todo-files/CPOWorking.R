
library("roxygen2")

roxygenise("..")

codetools::checkUsagePackage("mlr")

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

rm(cpotest.parvals)


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

retrafo(pid.task %>>% cpo)

retrafo(pid.task %>>% cpo)


retrafo(pid.task %>>% (cpo %>>% cpoPca(id = "snd")))

retrafo(pid.task %>>% (cpo %>>% cpoPca(id = "snd")))

retrafo(pid.task %>>% cpo %>>% cpoPca(id = "snd"))

retrafo(train(cpo %>>% cpoPca(id = "snd") %>>% makeLearner("classif.logreg"), pid.task))

retrafo(train(cpo %>>% (cpoPca(id = "snd") %>>% makeLearner("classif.logreg")), pid.task))


cpo = setHyperPars(cpo, par.vals = list(center = FALSE))

ls(environment(retrafo(pid.task %>>% (cpo %>>% cpoPca(id = "snd"))))$cpo)

environment(retrafo(pid.task %>>% (cpo %>>% cpoPca(id = "snd"))))$cpo$retrafo
ls(environment(environment(retrafo(pid.task %>>% (cpo %>>% cpoPca(id = "snd"))))$cpo$retrafo))

environment(retrafo(pid.task %>>% cpo %>>% cpoPca(id = "snd")))$prevfun

environment(retrafo(train(cpo %>>% cpoPca(id = "snd") %>>% makeLearner("classif.logreg"), pid.task)))$prevfun

environment(retrafo(train(cpo %>>% (cpoPca(id = "snd") %>>% makeLearner("classif.logreg")), pid.task)))$prevfun


debugger()



asat = function(obj, val) {
  eval.parent(substitute({attr(obj, "retrafo") = val}))
}

x = 10
x
asat(x, 10)
x

attributes(x)


  cpomultiplier.f = makeCPOFunctional("multiplierF", factor = 1: numeric[~., ~.], dummy = TRUE: logical, cpo.trafo = {
    data[[1]] = data[[1]] * factor
    attr(data, "retrafo") = function(data) {
      data[[1]] = data[[1]] / factor
      data
    }
    data
  })

retrafo(testdf %>>% cpomultiplier.f(2))

orig = retrafo(testdf %>>% cpomultiplier.f(2) %>>% cpomultiplier.f(10, id="xy"))
xl = as.list(orig)
compound2 = (xl[[1]] %>>% xl[[2]])
orig(testdf2)

compound2

getCPOName(orig)
getHyperPars(orig)
getParamSet(orig)
getHyperPars(xl[[1]])
getHyperPars(xl[[2]])
getParamSet(xl[[1]])
setHyperPars(orig, par.vals = list())
setHyperPars(xl[[1]], par.vals = list())

debugger()

getCPOName(compound2)
lapply(as.list(compound2), getHyperPars)

parent.env(parent.env(environment(retrafo(testdf %>>% cpomultiplier.f(2)))))$cpo.name


f = (function(x) {
  function() x
})(10)

class(f) = "xyz"

y = f
y
environment(y)$z
environment(y) = new.env()
environment(y)$x = 100
y()
f()


# TODO: different retrafo inner and outer
