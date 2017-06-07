
# helper objects for cpo tests in test_base_cpo

# this list is written using '<<-' to communicate the state inside
# a function during execution to the "outside"
cpotest.parvals = list()

# simple learner that writes the first data element it receives to cpotest.parvals
testlearnercpo = makeRLearnerClassif("testlearnercpo", package = character(0), par.set = makeParamSet(makeUntypedLearnerParam("env", when = "both")),
  properties = c("twoclass", "multiclass", "numerics", "factors"))
testlearnercpo$fix.factors.prediction = TRUE

trainLearner.testlearnercpo = function(.learner, .task, .subset, .weights = NULL, env, ...) {
  env$cpotest.parvals = c(env$cpotest.parvals, getTaskData(.task)[1, 1])
  getTaskData(.task, .subset)[[getTaskTargetNames(.task)[1]]][1]
}

predictLearner.testlearnercpo = function(.learner, .model, .newdata, env, ...) {
  env$cpotest.parvals = c(env$cpotest.parvals, .newdata[1, 1])
  rep(.model$learner.model, nrow(.newdata))
}

registerS3method("trainLearner", "testlearnercpo", trainLearner.testlearnercpo)
registerS3method("predictLearner", "testlearnercpo", predictLearner.testlearnercpo)

testlearnercpo = setHyperPars(testlearnercpo, env = environment(trainLearner.testlearnercpo))

# simple test data frame
testtaskcpo = makeClassifTask(data = data.frame(A = c(1, 2), B = factor(c("a", "b"))), target = "B")
testtaskcpo2 = makeClassifTask(data = data.frame(A = c(3, 4), B = factor(c("a", "b"))), target = "B")

# same data, but as data frame, without target
testdfcpo = data.frame(A = c(1, 2))
testdfcpo2 = data.frame(A = c(3, 4))

# cpoOPERATION.TASK.BACKEND
# OPERATION is multiplier or adder (adds or multiplies first column, subtracts / divides on retrafo)
# if TASK is present (being 'task'), the target column is verified to be the one from testtaskcpo
# BACKEND is 'f' for functional, 'o' for object


cpomultiplier.task.f = makeCPOFunctional("multiplierF", factor = 1: numeric[~., ~.], cpo.trafo = {
  expect_identical(data[[target]], factor(c("a", "b")))
  data[[1]] = data[[1]] * factor
  cpo.retrafo = function(data) {
    data[[1]] = data[[1]] / factor
    data
  }
  data
})

cpoadder.task.f = makeCPOFunctional("adderF", summand = 1: integer[, ], cpo.trafo = {
  expect_identical(data[[target]], factor(c("a", "b")))
  meandata = mean(data[[1]])
  data[[1]] = data[[1]] + summand
  cpo.retrafo = function(data) {
    data[[1]] = data[[1]] - summand - meandata
    data
  }
  data
})

cpomultiplier.task.o = makeCPOObject("multiplierO", factor = 1: numeric[~., ~.], cpo.trafo = {
  expect_identical(data[[target]], factor(c("a", "b")))
  data[[1]] = data[[1]] * factor
  control = 0
  data
}, cpo.retrafo = {
  data[[1]] = data[[1]] / factor
  data
})


cpoadder.task.o = makeCPOObject("adderO", summand = 1: integer[, ], cpo.trafo = {
  expect_identical(data[[target]], factor(c("a", "b")))
  control = mean(data[[1]])
  data[[1]] = data[[1]] + summand
  data
}, cpo.retrafo = {
  data[[1]] = data[[1]] - summand - control
  data
})



cpomultiplier.f = makeCPOFunctional("multiplierF", factor = 1: numeric[~., ~.], cpo.trafo = {
  if (length(target)) {
    expect_identical(data[[target]], factor(c("a", "b")))
  }
  data[[1]] = data[[1]] * factor
  cpo.retrafo = function(data) {
    data[[1]] = data[[1]] / factor
    data
  }
  data
})

cpoadder.f = makeCPOFunctional("adderF", summand = 1: integer[, ], cpo.trafo = {
  if (length(target)) {
    expect_identical(data[[target]], factor(c("a", "b")))
  }
  meandata = mean(data[[1]])
  data[[1]] = data[[1]] + summand
  cpo.retrafo = function(data) {
    data[[1]] = data[[1]] - summand - meandata
    data
  }
  data
})

cpomultiplier.o = makeCPOObject("multiplierO", factor = 1: numeric[~., ~.], cpo.trafo = {
  if (length(target)) {
    expect_identical(data[[target]], factor(c("a", "b")))
  }
  data[[1]] = data[[1]] * factor
  control = 0
  data
}, cpo.retrafo = {
  data[[1]] = data[[1]] / factor
  data
})


cpoadder.o = makeCPOObject("adderO", summand = 1: integer[, ], cpo.trafo = {
  if (length(target)) {
    expect_identical(data[[target]], factor(c("a", "b")))
  }
  control = mean(data[[1]])
  data[[1]] = data[[1]] + summand
  data
}, cpo.retrafo = {
  data[[1]] = data[[1]] - summand - control
  data
})


