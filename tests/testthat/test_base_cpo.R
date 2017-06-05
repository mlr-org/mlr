
context("cpo")

test_that("CPOs can be created", {

  expect_class(makeCPOFunctional("testCPO", cpo.trafo = { }), "CPOConstructor")

  expect_class(makeCPOFunctional("testCPO", a: integer(, 1), cpo.trafo = { }), "CPOConstructor")

  expect_class(makeCPOFunctional("testCPO", a = 1: integer(, 1), .par.vals = list(a = 0), cpo.trafo = { }), "CPOConstructor")

  expect_class(makeCPOFunctional("testCPO", .par.set = paramSetSugar(a: integer(0, 1)), .par.vals = list(a = 0), cpo.trafo = { }), "CPOConstructor")


  expect_class(makeCPOObject("testCPO", cpo.trafo = { }, cpo.retrafo = { }), "CPOConstructor")

  expect_class(makeCPOObject("testCPO", a: integer(, 1), cpo.trafo = { }, cpo.retrafo = { }), "CPOConstructor")

  expect_class(makeCPOObject("testCPO", a = 1: integer(, 1), .par.vals = list(a = 0), cpo.trafo = { }, cpo.retrafo = { }), "CPOConstructor")

  expect_class(makeCPOObject("testCPO", .par.set = paramSetSugar(a: integer(0, 1)), .par.vals = list(a = 0), cpo.trafo = { }, cpo.retrafo = { }), "CPOConstructor")

})

test_that("CPO with no parameters don't crash", {

  emptycpoF = makeCPOFunctional("testCPOEmptyF", cpo.trafo = {
    attr(data, "retrafo") = function(data) data
    data
  })


  emptycpoO = makeCPOObject("testCPOEmptyF", cpo.trafo = {
    control = 0
    data
  }, cpo.retrafo = {
    data
  })


  testCPO = function(ecpo) {
    assert_class(ecpo(), "CPO")
    assert_class(ecpo(id = "test"), "CPO")
    train(ecpo() %>>% makeLearner("classif.logreg"), pid.task)
    train(setCPOId(ecpo(), "test") %>>% makeLearner("classif.logreg"), pid.task)
    train(setCPOId(ecpo("test"), "test2") %>>% makeLearner("classif.logreg"), pid.task)

    expect_equal(length(getHyperPars(ecpo())), 0)
    expect_equal(length(getHyperPars(ecpo("test"))), 0)

    expect_equal(length(getParamSet(ecpo())$pars), 0)
    expect_equal(length(getParamSet(ecpo("test"))$pars), 0)
  }

  testCPO(emptycpoF)
  testCPO(emptycpoO)
})

test_that("CPO parameters behave as expected", {

  cpotest.parvals = list()
  cpotest.parvals2 = list()
  cpotest.parvals3 = list()


  cpoF = makeCPOFunctional("testCPOF",
    a: integer(, ), b = 1: integer(, ), c = 1: integer(, ), d: integer(, ), e: integer(, ),
    .par.vals = list(a = 1, b = 2, d = 1),
    cpo.trafo = {
      cpotest.parvals <<- list(a = a, b = b, c = c, d = d, e = e)  # nolint
      attr(data, "retrafo") = function(data) data
      data
    })

  cpo2F = makeCPOFunctional("testCPO2F",
    a: numeric(, ), z: integer(, ), model = TRUE: logical,
    cpo.trafo = {
      cpotest.parvals2 <<- list(a = a, z = z)  # nolint
      attr(data, "retrafo") = function(data) data
      data
    })

  cpo3F = makeCPOFunctional("testCPO3F",
    f: integer(, ),
    cpo.trafo = {
      cpotest.parvals3 <<- c(cpotest.parvals3, f)  # nolint
      attr(data, "retrafo") = function(data) data
      data
    })

  cpoO = makeCPOObject("testCPOO",
    a: integer(, ), b = 1: integer(, ), c = 1: integer(, ), d: integer(, ), e: integer(, ),
    .par.vals = list(a = 1, b = 2, d = 1),
    cpo.trafo = {
      cpotest.parvals <<- list(a = a, b = b, c = c, d = d, e = e)  # nolint
      control = 0
      data
    },
    cpo.retrafo = {
      data
    })

  cpo2O = makeCPOObject("testCPO2O",
    a: numeric(, ), z: integer(, ), model = TRUE: logical,
    cpo.trafo = {
      cpotest.parvals2 <<- list(a = a, z = z)  # nolint
      control = 0
        data
    },
    cpo.retrafo = {
      data
    })

  cpo3O = makeCPOObject("testCPO3O",
    f: integer(, ),
    cpo.trafo = {
      cpotest.parvals3 <<- c(cpotest.parvals3, f)  # nolint
      control = 0
      data
    },
    cpo.retrafo = {
      data
    })

  testCPO = function(cpo, cpo2, cpo3) {

    # normal parameters
    expect_class(cpo, "CPOConstructor")

    expect_identical(getHyperPars(cpo()), list(a = 1, b = 2, c = 1, d = 1))

    expect_identical(getHyperPars(cpo(b = 3)), list(a = 1, b = 3, c = 1, d = 1))

    expect_identical(getHyperPars(cpo(3)), list(a = 3, b = 2, c = 1, d = 1))

    cpoObj = setHyperPars(cpo(3, 4), b = 0, c = -1)

    expect_identical(getHyperPars(cpoObj), list(a = 3, b = 0, c = -1, d = 1))

    cpoLearner = cpoObj %>>% makeLearner("classif.logreg", model = FALSE)

    expect_identical(getHyperPars(cpoLearner), list(model = FALSE, a = 3, b = 0, c = -1, d = 1))

    expect_error(train(cpoLearner, pid.task), "Parameter e.*missing")

    cpotest.parvals <<- list()  # nolint
    train(setHyperPars(cpoLearner, e = 900), pid.task)
    expect_identical(cpotest.parvals, list(a = 3, b = 0, c = -1, d = 1, e = 900))

    # parameters of cpo with id
    expect_identical(getHyperPars(cpo(id = "x")), list(x.a = 1, x.b = 2, x.c = 1, x.d = 1))

    expect_identical(getHyperPars(cpo(b = 3, id = "x")), list(x.a = 1, x.b = 3, x.c = 1, x.d = 1))

    cpoObj = setHyperPars(cpo(3, 4, id = "x"), x.b = 0, x.c = -1)

    expect_identical(getHyperPars(cpoObj), list(x.a = 3, x.b = 0, x.c = -1, x.d = 1))

    cpoLearner = cpoObj %>>% makeLearner("classif.logreg", model = FALSE)

    expect_identical(getHyperPars(cpoLearner), list(model = FALSE, x.a = 3, x.b = 0, x.c = -1, x.d = 1))

    expect_error(train(cpoLearner, pid.task), "Parameter (x\\.)?e .*missing")

    cpoLearner = setCPOId(cpoObj, "y") %>>% makeLearner("classif.logreg", model = FALSE)

    expect_error(train(cpoLearner, pid.task), "Parameter (y\\.)?e .*missing")

    cpotest.parvals <<- list()  # nolint
    train(setHyperPars(cpoLearner, y.e = 901), pid.task)
    expect_identical(cpotest.parvals, list(a = 3, b = 0, c = -1, d = 1, e = 901))

    expect_error(setCPOId(cpoObj %>>% cpo3(), "testx"), "Cannot set ID of compound CPO")

    # parameters of coupled CPOs
    expect_error(cpo(3) %>>% cpo2(4) %>>% cpo3(), 'Parameter "a" occurs in both')

    expect_class(cpo(3) %>>% cpo2(4, id = "2nd") %>>% cpo3(), "CPO")

    expect_error(cpo2(4) %>>% cpo3() %>>% makeLearner("classif.logreg"), 'Parameter "model" occurs in both')

    lrn = cpo(3) %>>% cpo2(4, id = "2nd") %>>% cpo3() %>>% makeLearner("classif.logreg", model = TRUE)

    expect_identical(getHyperPars(lrn), list(model = TRUE, a = 3, b = 2, c = 1, d = 1, `2nd.a` = 4, `2nd.model` = TRUE))

    expect_error(train(lrn, pid.task), "Parameters? e.*missing")

    expect_error(train(setHyperPars(lrn, e = 90), pid.task), "Parameters? (2nd\\.)?z.*missing")

    cpotest.parvals <<- list()  # nolint
    cpotest.parvals2 <<- list()  # nolint
    cpotest.parvals3 <<- list()  # nolint
    train(setHyperPars(lrn, e = 90, `2nd.a` = 9000, `2nd.z` = -10, f = 222), pid.task)
    expect_identical(cpotest.parvals, list(a = 3, b = 2, c = 1, d = 1, e = 90))
    expect_identical(cpotest.parvals2, list(a = 9000, z = -10))
    expect_identical(cpotest.parvals3, list(222))

    # multiple instances of the same CPO
    cpotest.parvals3 <<- list()  # nolint
    train(cpo3(id = 'a', 100) %>>% cpo3(id = 'b', 10) %>>% cpo3(20) %>>% makeLearner("classif.logreg"), pid.task)
    expect_identical(cpotest.parvals3, list(100, 10, 20))

    cpotest.parvals3 <<- list()  # nolint
    lrn = cpo3(id = 'a') %>>% cpo3(id = 'b', 10) %>>% cpo3() %>>% makeLearner("classif.logreg")
    train(setHyperPars(lrn, a.f = 1000, f = 99), pid.task)
    expect_identical(cpotest.parvals3, list(1000, 10, 99))
  }

  testCPO(cpoF, cpo2F, cpo3F)

  testCPO(cpoO, cpo2O, cpo3O)

})


test_that("Functional CPO Parameter feasibility is checked", {

  expect_error(makeCPOFunctional("testCPOF",
    a: integer(, ),
    .par.vals = list(a = 1, b = 2),
    cpo.trafo = { }))

  cpo = makeCPOFunctional("testCPOF",
    a: integer(, ), b: integer(, ),
    .par.vals = list(a = 1, b = 2),
    cpo.trafo = { })

  expect_error(cpo(1.5, 2), "is not feasible for parameter 'a'")

  cpoo = cpo(1, 2)
  expect_class(cpoo, "CPO")
  expect_class(setHyperPars(cpoo, a = 10), "CPO")
  expect_error(setHyperPars(cpoo, a = 0.4), "is not feasible for parameter 'a'")

  expect_error(makeCPOFunctional("testCPOF",
    a: integer(, ), b: integer(0, 1),
    .par.vals = list(a = 1, b = 2),
    cpo.trafo = { }), "2 is not feasible for parameter 'b'")

  cpo = makeCPOFunctional("testCPOF",
    a: integer(, ), b: integer(0, 1),
    cpo.trafo = { })

  expect_error(cpo(1, 2), "2 is not feasible for parameter 'b'")
  cpoo = cpo(0, 0)
  expect_class(setHyperPars(cpoo, b = 1), "CPO")
  expect_error(setHyperPars(cpoo, b = 3), "is not feasible for parameter 'b'")

  expect_error(makeCPOFunctional("testCPOF",
    a: integer(, ), b = 2: integer(0, 1),
    cpo.trafo = { }), "'default' must be a feasible parameter setting")

  makeCPOFunctional("testCPOF",
    a = (function() 1): discrete(a = function() 1, b = function() 2),
    cpo.trafo = { })

  expect_error(makeCPOFunctional("testCPOF",
    a = (function() 3): discrete(a = function() 1, b = function() 2),
    cpo.trafo = { }),  "'default' must be a feasible parameter setting")

  cpo = makeCPOFunctional("testCPOF",
    a: discrete(a = function() 1, b = function() 2),
    .par.vals = list(a = function() 1),
    cpo.trafo = { })

  expect_error(cpo(function() 3), "<function> is not feasible for parameter 'a'")
  cpoo = cpo(function() 1)
  expect_class(cpoo, "CPO")
  expect_class(setHyperPars(cpoo, a = function() 1), "CPO")
  expect_error(setHyperPars(cpoo, a = function() 3), "not feasible for parameter 'a'")

  expect_error(makeCPOFunctional("testCPOF",
    a: discrete(a = function() 1, b = function() 2),
    .par.vals = list(a = function() 3),
    cpo.trafo = { }),  "<function> is not feasible for parameter 'a'")

})

test_that("Object based CPO Parameter feasibility is checked", {

  expect_error(makeCPOObject("testCPOO",
    a: integer(, ),
    .par.vals = list(a = 1, b = 2),
    cpo.trafo = { }, cpo.retrafo = { }))

  cpo = makeCPOObject("testCPOO",
    a: integer(, ), b: integer(, ),
    .par.vals = list(a = 1, b = 2),
    cpo.trafo = { }, cpo.retrafo = { })

  expect_error(cpo(1.5, 2), "is not feasible for parameter 'a'")

  cpoo = cpo(1, 2)
  expect_class(cpoo, "CPO")
  expect_class(setHyperPars(cpoo, a = 10), "CPO")
  expect_error(setHyperPars(cpoo, a = 0.4), "is not feasible for parameter 'a'")

  expect_error(makeCPOObject("testCPOO",
    a: integer(, ), b: integer(0, 1),
    .par.vals = list(a = 1, b = 2),
    cpo.trafo = { }, cpo.retrafo = { }), "2 is not feasible for parameter 'b'")

  cpo = makeCPOObject("testCPOO",
    a: integer(, ), b: integer(0, 1),
    cpo.trafo = { }, cpo.retrafo = { })

  expect_error(cpo(1, 2), "2 is not feasible for parameter 'b'")
  cpoo = cpo(0, 0)
  expect_class(setHyperPars(cpoo, b = 1), "CPO")
  expect_error(setHyperPars(cpoo, b = 3), "is not feasible for parameter 'b'")

  expect_error(makeCPOObject("testCPOO",
    a: integer(, ), b = 2: integer(0, 1),
    cpo.trafo = { }, cpo.retrafo = { }), "'default' must be a feasible parameter setting")

  makeCPOObject("testCPOO",
    a = (function() 1): discrete(a = function() 1, b = function() 2),
    cpo.trafo = { }, cpo.retrafo = { })

  expect_error(makeCPOObject("testCPOO",
    a = (function() 3): discrete(a = function() 1, b = function() 2),
    cpo.trafo = { }, cpo.retrafo = { }),  "'default' must be a feasible parameter setting")

  cpo = makeCPOObject("testCPOO",
    a: discrete(a = function() 1, b = function() 2),
    .par.vals = list(a = function() 1),
    cpo.trafo = { }, cpo.retrafo = { })

  expect_error(cpo(function() 3), "<function> is not feasible for parameter 'a'")
  cpoo = cpo(function() 1)
  expect_class(cpoo, "CPO")
  expect_class(setHyperPars(cpoo, a = function() 1), "CPO")
  expect_error(setHyperPars(cpoo, a = function() 3), "not feasible for parameter 'a'")

  expect_error(makeCPOObject("testCPOO",
    a: discrete(a = function() 1, b = function() 2),
    .par.vals = list(a = function() 3),
    cpo.trafo = { }, cpo.retrafo = { }),  "<function> is not feasible for parameter 'a'")

})

test_that("discrete parameters work well", {
  cpotest.parvals = list()

  X = 1
  Y = 2

  cpoF = makeCPOFunctional("testCPOF",
    a: logical, b: discrete(a, b, 1), c = 1: discrete(a, b, 1), d = c(TRUE, TRUE): logical^2, e: discrete(a = function() 1, b = function() Y)^2,
    cpo.trafo = {
      cpotest.parvals <<- list(a = a, b = b, c = c, d = d, e = c(e[[1]](), e[[2]]()))  # nolint
      attr(data, "retrafo") = function(data) data
      data
    })

  cpoO = makeCPOObject("testCPOO",
    a: logical, b: discrete(a, b, 1), c = 1: discrete(a, b, 1), d = c(TRUE, TRUE): logical^2, e: discrete(a = function() 1, b = function() Y)^2,
    cpo.trafo = {
      cpotest.parvals <<- list(a = a, b = b, c = c, d = d, e = c(e[[1]](), e[[2]]()))  # nolint
      control = 0
      data
    },
    cpo.retrafo = {
      data
    })

  testCPO = function(cpo) {
    cpotest.parvals <<- list()  # nolint
    train(cpoF(TRUE, "a", e = list(function() 1, function() 1)) %>>% makeLearner("classif.logreg"), pid.task)
    expect_identical(cpotest.parvals, list(a = TRUE, b = "a", c = 1, d = c(TRUE, TRUE), e = c(1, 1)))

    cpotest.parvals <<- list()  # nolint
    train(cpoF(TRUE, 1, e = list(function() Y, function() 1)) %>>% makeLearner("classif.logreg"), pid.task)
    expect_identical(cpotest.parvals, list(a = TRUE, b = 1, c = 1, d = c(TRUE, TRUE), e = c(2, 1)))
  }

  testCPO(cpoF)
  testCPO(cpoO)
})

test_that("preprocessing actually changes data", {

  cpotest.parvals = list()

  testlearner = makeRLearnerClassif("testlearner", package = character(0), par.set = paramSetSugar(),
    properties = c("twoclass", "multiclass", "numerics", "factors"))
  testlearner$fix.factors.prediction = TRUE

  trainLearner.testlearner = function(.learner, .task, .subset, .weights = NULL, ...) {
    cpotest.parvals <<- c(cpotest.parvals, getTaskData(.task)[1, 1])  # nolint
    getTaskData(.task, .subset)[[getTaskTargetNames(.task)[1]]][1]
  }

  predictLearner.testlearner = function(.learner, .model, .newdata, ...) {
    cpotest.parvals <<- c(cpotest.parvals, .newdata[1, 1])  # nolint
    rep(.model$learner.model, nrow(.newdata))
  }

  testtask = makeClassifTask(data = data.frame(A = c(1, 2), B = factor(c("a", "b"))), target = "B")
  testtask2 = makeClassifTask(data = data.frame(A = c(3, 4), B = factor(c("a", "b"))), target = "B")

  cpoMultiplierF = makeCPOFunctional("multiplierF", factor = 1: numeric(., .), cpo.trafo = {
    expect_identical(data[[target]], factor(c("a", "b")))
    data[[1]] = data[[1]] * factor
    attr(data, "retrafo") = function(data) {
      data[[1]] = data[[1]] / factor
      data
    }
    data
  })

  cpoAdderF = makeCPOFunctional("adderF", summand = 1: integer(, ), cpo.trafo = {
    expect_identical(data[[target]], factor(c("a", "b")))
    meandata = mean(data[[1]])
    data[[1]] = data[[1]] + summand
    attr(data, "retrafo") = function(data) {
      data[[1]] = data[[1]] - summand - meandata
      data
    }
    data
  })

  cpoMultiplierO = makeCPOObject("multiplierO", factor = 1: numeric(., .), cpo.trafo = {
    expect_identical(data[[target]], factor(c("a", "b")))
    data[[1]] = data[[1]] * factor
    control = 0
    data
  }, cpo.retrafo = {
    data[[1]] = data[[1]] / factor
    data
  })


  cpoAdderO = makeCPOObject("adderO", summand = 1: integer(, ), cpo.trafo = {
    expect_identical(data[[target]], factor(c("a", "b")))
    control = mean(data[[1]])
    data[[1]] = data[[1]] + summand
    data
  }, cpo.retrafo = {
    data[[1]] = data[[1]] - summand - control
    data
  })

  testCPO = function(cpoMultiplier, cpoAdder) {
    cpotest.parvals <<- list()  # nolint
    predict(train(testlearner, testtask), testtask2)
    expect_identical(cpotest.parvals, list(1, 3))


    cpotest.parvals <<- list()  # nolint
    predict(train(cpoMultiplier(10) %>>% testlearner, testtask), testtask2)
    expect_identical(cpotest.parvals, list(10, 0.3))

    cpotest.parvals <<- list()  # nolint
    predict(train(cpoAdder(3) %>>% testlearner, testtask), testtask2)
    expect_identical(cpotest.parvals, list(4, -1.5))


    cpotest.parvals <<- list()  # nolint
    predict(train(cpoAdder(3) %>>% cpoMultiplier(3) %>>% cpoAdder(2, id="second") %>>% cpoMultiplier(10, id="second") %>>% testlearner, testtask), testtask2)
    # Calculation happening:
    # Training:
    #   c(1, 2), +3, *3, +2, *10 -> c(140, 170)
    #   first adder gets a meandata of 1.5, second adder gets meandata of 13.5
    # Prediction:
    #   c(3, 4) - 3 - 1.5, / 3, - 2 - 13.5, / 10 -> c(-1.6, -1.57)
    expect_identical(cpotest.parvals, list(140, -1.6))
  }

  testCPO(cpoMultiplierF, cpoAdderF)
  testCPO(cpoMultiplierO, cpoAdderO)

})

test_that("CPO trafo functions work", {

  expect_error(makeCPOObject("testCPO", a: integer(, ), b: integer(, ),
    cpo.trafo = function(a) { }, cpo.retrafo = function(b, ...) { }), "Must have formal arguments")

  expect_error(makeCPOObject("testCPO", a: integer(, ), b: integer(, ),
    cpo.trafo = function(a, ...) { }, cpo.retrafo = function(b) { }), "Must have formal arguments")

  expect_class(makeCPOObject("testCPO", a: integer(, ), b: integer(, ),
    cpo.trafo = function(a, ...) { }, cpo.retrafo = function(b, ...) { }), "CPOConstructor")

  expect_error(makeCPOFunctional("testCPO", a: integer(, ), b: integer(, ),
    cpo.trafo = function(a) { }), "Must have formal arguments")

  expect_class(makeCPOFunctional("testCPO", a: integer(, ), b: integer(, ),
    cpo.trafo = function(a, ...) { }), "CPOConstructor")

  cpotest.parvals = list()
  t = train(makeCPOObject("testCPO", a: integer(, ), b: integer(, ),
    cpo.trafo = function(data, target, b, ...) {
      cpotest.parvals <<- list(b, list(...))
      control = 0
      data
    }, cpo.retrafo = function(data, b, ...) {
      cpotest.parvals <<- list(b, list(...))
      data
    })(1, 2) %>>% makeLearner("classif.logreg"), pid.task)

  expect_identical(cpotest.parvals, list(2, list(a = 1)))

  cpotest.parvals = list()
  predict(t, pid.task)
  expect_identical(cpotest.parvals, list(2, list(a = 1, control = 0)))

  cpotest.parvals = list()
  t = train(makeCPOFunctional("testCPO", a: integer(, ), b: integer(, ),
    cpo.trafo = function(data, target, b, ...) {
      cpotest.parvals <<- list(b, list(...))
      attr(data, "retrafo") = function(data, ...) {
        cpotest.parvals <<- list(b, list(...))
        data
      }
      data
    })(1, 2) %>>% makeLearner("classif.logreg"), pid.task)

  expect_identical(cpotest.parvals, list(2, list(a = 1)))

  cpotest.parvals = list()
  predict(t, pid.task)
  expect_identical(cpotest.parvals, list(2, list()))

})

test_that("CPO arguments may be missing if requirements allow", {

  cpoc = makeCPOObject("testCPO", a = FALSE: logical, b: integer(, ) [requires = quote(!!a)],
    cpo.trafo = {
      if (!a) {
        expect_true(missing(b))
      } else {
        expect_class(b, "integer")
      }
      control = 0
      data
    }, cpo.retrafo = {
      if (!a) {
        expect_true(missing(b))
      } else {
        expect_class(b, "integer")
      }
      data
    })

  t = train(cpoc() %>>% makeLearner("classif.logreg"), pid.task)
  predict(t, pid.task)
  expect_error(train(cpoc(a = TRUE) %>>% makeLearner("classif.logreg"), pid.task), "Parameter b .*missing")
  t = train(cpoc(a = TRUE, b = 1L) %>>% makeLearner("classif.logreg"), pid.task)
  predict(t, pid.task)

  t = train(cpoc(id = "test") %>>% makeLearner("classif.logreg"), pid.task)
  predict(t, pid.task)
  expect_error(train(cpoc(a = TRUE, id = "test") %>>% makeLearner("classif.logreg"), pid.task), "Parameter test\\.b .*missing")
  t = train(cpoc(a = TRUE, b = 1L, id = "test") %>>% makeLearner("classif.logreg"), pid.task)
  predict(t, pid.task)

  expect_identical(getParamSet(cpoc(a = TRUE, id = "test"))$pars$test.b$requires, quote(!!test.a))


  cpoc = makeCPOFunctional("testCPO", a = FALSE: logical, b: integer(, ) [requires = quote(!!a)],
    cpo.trafo = {
      if (!a) {
        expect_true(missing(b))
      } else {
        expect_class(b, "integer")
      }
      control = 0
      attr(data, "retrafo") = function(data) data
      data
    })

  train(cpoc() %>>% makeLearner("classif.logreg"), pid.task)
  expect_error(train(cpoc(a = TRUE) %>>% makeLearner("classif.logreg"), pid.task), "Parameter b .*missing")
  train(cpoc(a = TRUE, b = 1L) %>>% makeLearner("classif.logreg"), pid.task)

  train(cpoc(id = "test") %>>% makeLearner("classif.logreg"), pid.task)
  expect_error(train(cpoc(a = TRUE, id = "test") %>>% makeLearner("classif.logreg"), pid.task), "Parameter b .*missing")
  train(cpoc(a = TRUE, b = 1L, id = "test") %>>% makeLearner("classif.logreg"), pid.task)

  expect_identical(getParamSet(cpoc(a = TRUE, id = "test"))$pars$test.b$requires, quote(!!test.a))
})
