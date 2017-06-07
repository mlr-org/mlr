
context("cpo")

test_that("CPOs can be created", {

  expect_class(makeCPOFunctional("testCPO", cpo.trafo = { }), "CPOConstructor")

  expect_class(makeCPOFunctional("testCPO", a: integer[, 1], cpo.trafo = { }), "CPOConstructor")

  expect_class(makeCPOFunctional("testCPO", a = 1: integer[, 1], .par.vals = list(a = 0), cpo.trafo = { }), "CPOConstructor")

  expect_class(makeCPOFunctional("testCPO", .par.set = paramSetSugar(a: integer[0, 1]), .par.vals = list(a = 0), cpo.trafo = { }), "CPOConstructor")


  expect_class(makeCPOObject("testCPO", cpo.trafo = { }, cpo.retrafo = { }), "CPOConstructor")

  expect_class(makeCPOObject("testCPO", a: integer[, 1], cpo.trafo = { }, cpo.retrafo = { }), "CPOConstructor")

  expect_class(makeCPOObject("testCPO", a = 1: integer[, 1], .par.vals = list(a = 0), cpo.trafo = { }, cpo.retrafo = { }), "CPOConstructor")

  expect_class(makeCPOObject("testCPO", .par.set = paramSetSugar(a: integer[0, 1]), .par.vals = list(a = 0), cpo.trafo = { }, cpo.retrafo = { }), "CPOConstructor")

})

test_that("CPO with no parameters don't crash", {

  emptycpo.f = makeCPOFunctional("testCPOEmptyF", cpo.trafo = {
    cpo.retrafo = function(data) data
    data
  })

  emptycpo.o = makeCPOObject("testCPOEmptyF", cpo.trafo = {
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

  testCPO(emptycpo.f)
  testCPO(emptycpo.o)
})

test_that("CPO parameters behave as expected", {

  cpotest.parvals = list()
  cpotest.parvals2 = list()
  cpotest.parvals3 = list()

  cpof = makeCPOFunctional("testCPOF",
    a: integer[, ], b = 1: integer[, ], c = 1: integer[, ], d: integer[, ], e: integer[, ],
    .par.vals = list(a = 1, b = 2, d = 1),
    cpo.trafo = {
      cpotest.parvals <<- list(a = a, b = b, c = c, d = d, e = e)  # nolint
      cpo.retrafo = function(data) data
      data
    })

  cpo2f = makeCPOFunctional("testCPO2F",
    a: numeric[, ], z: integer[, ], model = TRUE: logical,
    cpo.trafo = {
      cpotest.parvals2 <<- list(a = a, z = z)  # nolint
      cpo.retrafo = function(data) data
      data
    })

  cpo3f = makeCPOFunctional("testCPO3F",
    f: integer[, ],
    cpo.trafo = {
      cpotest.parvals3 <<- c(cpotest.parvals3, f)  # nolint
      cpo.retrafo = function(data) data
      data
    })

  cpoo = makeCPOObject("testCPOO",
    a: integer[, ], b = 1: integer[, ], c = 1: integer[, ], d: integer[, ], e: integer[, ],
    .par.vals = list(a = 1, b = 2, d = 1),
    cpo.trafo = {
      cpotest.parvals <<- list(a = a, b = b, c = c, d = d, e = e)  # nolint
      control = 0
      data
    },
    cpo.retrafo = {
      data
    })

  cpo2o = makeCPOObject("testCPO2O",
    a: numeric[, ], z: integer[, ], model = TRUE: logical,
    cpo.trafo = {
      cpotest.parvals2 <<- list(a = a, z = z)  # nolint
      control = 0
        data
    },
    cpo.retrafo = {
      data
    })

  cpo3o = makeCPOObject("testCPO3O",
    f: integer[, ],
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

    cpo.obj = setHyperPars(cpo(3, 4), b = 0, c = -1)

    expect_identical(getHyperPars(cpo.obj), list(a = 3, b = 0, c = -1, d = 1))

    cpo.learner = cpo.obj %>>% makeLearner("classif.logreg", model = FALSE)

    expect_identical(getHyperPars(cpo.learner), list(model = FALSE, a = 3, b = 0, c = -1, d = 1))

    expect_error(train(cpo.learner, pid.task), "Parameter e.*missing")

    cpotest.parvals <<- list()  # nolint
    train(setHyperPars(cpo.learner, e = 900), pid.task)
    expect_identical(cpotest.parvals, list(a = 3, b = 0, c = -1, d = 1, e = 900))

    # parameters of cpo with id
    expect_identical(getHyperPars(cpo(id = "x")), list(x.a = 1, x.b = 2, x.c = 1, x.d = 1))

    expect_identical(getHyperPars(cpo(b = 3, id = "x")), list(x.a = 1, x.b = 3, x.c = 1, x.d = 1))

    cpo.obj = setHyperPars(cpo(3, 4, id = "x"), x.b = 0, x.c = -1)

    expect_identical(getHyperPars(cpo.obj), list(x.a = 3, x.b = 0, x.c = -1, x.d = 1))

    cpo.learner = cpo.obj %>>% makeLearner("classif.logreg", model = FALSE)

    expect_identical(getHyperPars(cpo.learner), list(model = FALSE, x.a = 3, x.b = 0, x.c = -1, x.d = 1))

    expect_error(train(cpo.learner, pid.task), "Parameter (x\\.)?e .*missing")

    cpo.learner = setCPOId(cpo.obj, "y") %>>% makeLearner("classif.logreg", model = FALSE)

    expect_error(train(cpo.learner, pid.task), "Parameter (y\\.)?e .*missing")

    cpotest.parvals <<- list()  # nolint
    train(setHyperPars(cpo.learner, y.e = 901), pid.task)
    expect_identical(cpotest.parvals, list(a = 3, b = 0, c = -1, d = 1, e = 901))

    expect_error(setCPOId(cpo.obj %>>% cpo3(), "testx"), "Cannot set ID of compound CPO")

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
    train(cpo3(id = "a", 100) %>>% cpo3(id = "b", 10) %>>% cpo3(20) %>>% makeLearner("classif.logreg"), pid.task)
    expect_identical(cpotest.parvals3, list(100, 10, 20))

    cpotest.parvals3 <<- list()  # nolint
    lrn = cpo3(id = "a") %>>% cpo3(id = "b", 10) %>>% cpo3() %>>% makeLearner("classif.logreg")
    train(setHyperPars(lrn, a.f = 1000, f = 99), pid.task)
    expect_identical(cpotest.parvals3, list(1000, 10, 99))
  }

  testCPO(cpof, cpo2f, cpo3f)

  testCPO(cpoo, cpo2o, cpo3o)

})


test_that("Functional CPO Parameter feasibility is checked", {

  expect_error(makeCPOFunctional("testCPOF",
    a: integer[, ],
    .par.vals = list(a = 1, b = 2),
    cpo.trafo = { }))

  cpo = makeCPOFunctional("testCPOF",
    a: integer[, ], b: integer[, ],
    .par.vals = list(a = 1, b = 2),
    cpo.trafo = { })

  expect_error(cpo(1.5, 2), "is not feasible for parameter 'a'")

  cpoo = cpo(1, 2)
  expect_class(cpoo, "CPO")
  expect_class(setHyperPars(cpoo, a = 10), "CPO")
  expect_error(setHyperPars(cpoo, a = 0.4), "is not feasible for parameter 'a'")

  expect_error(makeCPOFunctional("testCPOF",
    a: integer[, ], b: integer[0, 1],
    .par.vals = list(a = 1, b = 2),
    cpo.trafo = { }), "2 is not feasible for parameter 'b'")

  cpo = makeCPOFunctional("testCPOF",
    a: integer[, ], b: integer[0, 1],
    cpo.trafo = { })

  expect_error(cpo(1, 2), "2 is not feasible for parameter 'b'")
  cpoo = cpo(0, 0)
  expect_class(setHyperPars(cpoo, b = 1), "CPO")
  expect_error(setHyperPars(cpoo, b = 3), "is not feasible for parameter 'b'")

  expect_error(makeCPOFunctional("testCPOF",
    a: integer[, ], b = 2: integer[0, 1],
    cpo.trafo = { }), "'default' must be a feasible parameter setting")

  makeCPOFunctional("testCPOF",
    a = (function() 1): discrete[a = function() 1, b = function() 2],
    cpo.trafo = { })

  expect_error(makeCPOFunctional("testCPOF",
    a = (function() 3): discrete[a = function() 1, b = function() 2],
    cpo.trafo = { }),  "'default' must be a feasible parameter setting")

  cpo = makeCPOFunctional("testCPOF",
    a: discrete[a = function() 1, b = function() 2],
    .par.vals = list(a = function() 1),
    cpo.trafo = { })

  expect_error(cpo(function() 3), "<function> is not feasible for parameter 'a'")
  cpoo = cpo(function() 1)
  expect_class(cpoo, "CPO")
  expect_class(setHyperPars(cpoo, a = function() 1), "CPO")
  expect_error(setHyperPars(cpoo, a = function() 3), "not feasible for parameter 'a'")

  expect_error(makeCPOFunctional("testCPOF",
    a: discrete[a = function() 1, b = function() 2],
    .par.vals = list(a = function() 3),
    cpo.trafo = { }),  "<function> is not feasible for parameter 'a'")

})

test_that("Object based CPO Parameter feasibility is checked", {

  expect_error(makeCPOObject("testCPOO",
    a: integer[, ],
    .par.vals = list(a = 1, b = 2),
    cpo.trafo = { }, cpo.retrafo = { }))

  cpo = makeCPOObject("testCPOO",
    a: integer[, ], b: integer[, ],
    .par.vals = list(a = 1, b = 2),
    cpo.trafo = { }, cpo.retrafo = { })

  expect_error(cpo(1.5, 2), "is not feasible for parameter 'a'")

  cpoo = cpo(1, 2)
  expect_class(cpoo, "CPO")
  expect_class(setHyperPars(cpoo, a = 10), "CPO")
  expect_error(setHyperPars(cpoo, a = 0.4), "is not feasible for parameter 'a'")

  expect_error(makeCPOObject("testCPOO",
    a: integer[, ], b: integer[0, 1],
    .par.vals = list(a = 1, b = 2),
    cpo.trafo = { }, cpo.retrafo = { }), "2 is not feasible for parameter 'b'")

  cpo = makeCPOObject("testCPOO",
    a: integer[, ], b: integer[0, 1],
    cpo.trafo = { }, cpo.retrafo = { })

  expect_error(cpo(1, 2), "2 is not feasible for parameter 'b'")
  cpoo = cpo(0, 0)
  expect_class(setHyperPars(cpoo, b = 1), "CPO")
  expect_error(setHyperPars(cpoo, b = 3), "is not feasible for parameter 'b'")

  expect_error(makeCPOObject("testCPOO",
    a: integer[, ], b = 2: integer[0, 1],
    cpo.trafo = { }, cpo.retrafo = { }), "'default' must be a feasible parameter setting")

  makeCPOObject("testCPOO",
    a = (function() 1): discrete[a = function() 1, b = function() 2],
    cpo.trafo = { }, cpo.retrafo = { })

  expect_error(makeCPOObject("testCPOO",
    a = (function() 3): discrete[a = function() 1, b = function() 2],
    cpo.trafo = { }, cpo.retrafo = { }),  "'default' must be a feasible parameter setting")

  cpo = makeCPOObject("testCPOO",
    a: discrete[a = function() 1, b = function() 2],
    .par.vals = list(a = function() 1),
    cpo.trafo = { }, cpo.retrafo = { })

  expect_error(cpo(function() 3), "<function> is not feasible for parameter 'a'")
  cpoo = cpo(function() 1)
  expect_class(cpoo, "CPO")
  expect_class(setHyperPars(cpoo, a = function() 1), "CPO")
  expect_error(setHyperPars(cpoo, a = function() 3), "not feasible for parameter 'a'")

  expect_error(makeCPOObject("testCPOO",
    a: discrete[a = function() 1, b = function() 2],
    .par.vals = list(a = function() 3),
    cpo.trafo = { }, cpo.retrafo = { }),  "<function> is not feasible for parameter 'a'")

})

test_that("discrete parameters work well", {


  X = 1
  Y = 2

  cpof = makeCPOFunctional("testCPOF",
    a: logical, b: discrete[a, b, 1], c = 1: discrete[a, b, 1], d = c(TRUE, TRUE): logical^2, e: discrete[a = function() 1, b = function() Y]^2,
    cpo.trafo = {
      cpotest.parvals <<- list(a = a, b = b, c = c, d = d, e = c(e[[1]](), e[[2]]()))  # nolint
      cpo.retrafo = function(data) data
      data
    })

  cpoo = makeCPOObject("testCPOO",
    a: logical, b: discrete[a, b, 1], c = 1: discrete[a, b, 1], d = c(TRUE, TRUE): logical^2, e: discrete[a = function() 1, b = function() Y]^2,
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
    train(cpo(TRUE, "a", e = list(function() 1, function() 1)) %>>% makeLearner("classif.logreg"), pid.task)
    expect_identical(cpotest.parvals, list(a = TRUE, b = "a", c = 1, d = c(TRUE, TRUE), e = c(1, 1)))

    cpotest.parvals <<- list()  # nolint
    train(cpo(TRUE, 1, e = list(function() Y, function() 1)) %>>% makeLearner("classif.logreg"), pid.task)
    expect_identical(cpotest.parvals, list(a = TRUE, b = 1, c = 1, d = c(TRUE, TRUE), e = c(2, 1)))
  }

  testCPO(cpof)
  testCPO(cpoo)
})

test_that("preprocessing actually changes data", {

  cpotest.parvals <<- list()  # nolint
  t = train(testlearnercpo, testtaskcpo)
  predict(t, testtaskcpo2)
  expect_equal(cpotest.parvals, list(1, 3))

  testCPO = function(cpoMultiplier, cpoAdder) {
    cpotest.parvals <<- list()  # nolint
    t = train(testlearnercpo, testtaskcpo)
    predict(t, testtaskcpo2)
    expect_identical(cpotest.parvals, list(1, 3))


    cpotest.parvals <<- list()  # nolint
    predict(train(cpoMultiplier(10) %>>% testlearnercpo, testtaskcpo), testtaskcpo2)
    expect_identical(cpotest.parvals, list(10, 0.3))

    cpotest.parvals <<- list()  # nolint
    predict(train(cpoAdder(3) %>>% testlearnercpo, testtaskcpo), testtaskcpo2)
    expect_identical(cpotest.parvals, list(4, -1.5))


    cpotest.parvals <<- list()  # nolint
    predict(train(cpoAdder(3) %>>%
                  cpoMultiplier(3) %>>%
                  cpoAdder(2, id = "second") %>>%
                  cpoMultiplier(10, id = "second") %>>%
                  testlearnercpo, testtaskcpo), testtaskcpo2)
    # Calculation happening:
    # Training:
    #   c(1, 2), +3, *3, +2, *10 -> c(140, 170)
    #   first adder gets a meandata of 1.5, second adder gets meandata of 13.5
    # Prediction:
    #   c(3, 4) - 3 - 1.5, / 3, - 2 - 13.5, / 10 -> c(-1.6, -1.57)
    expect_identical(cpotest.parvals, list(140, -1.6))
  }

  testCPO(cpomultiplier.task.f, cpoadder.f)
  testCPO(cpomultiplier.task.o, cpoadder.o)

})

test_that("CPO trafo functions work", {

  expect_error(makeCPOObject("testCPO", a: integer[, ], b: integer[, ],
    cpo.trafo = function(a) { }, cpo.retrafo = function(b, ...) { }), "Must have formal arguments")

  expect_error(makeCPOObject("testCPO", a: integer[, ], b: integer[, ],
    cpo.trafo = function(a, ...) { }, cpo.retrafo = function(b) { }), "Must have formal arguments")

  expect_class(makeCPOObject("testCPO", a: integer[, ], b: integer[, ],
    cpo.trafo = function(a, ...) { }, cpo.retrafo = function(b, ...) { }), "CPOConstructor")

  expect_error(makeCPOFunctional("testCPO", a: integer[, ], b: integer[, ],
    cpo.trafo = function(a) { }), "Must have formal arguments")

  expect_class(makeCPOFunctional("testCPO", a: integer[, ], b: integer[, ],
    cpo.trafo = function(a, ...) { }), "CPOConstructor")

  cpotest.parvals = list()
  t = train(makeCPOObject("testCPO", a: integer[, ], b: integer[, ],
    cpo.trafo = function(data, target, b, ...) {
      cpotest.parvals <<- list(b, list(...))  # nolint
      control = 0
      data
    }, cpo.retrafo = function(data, b, ...) {
      cpotest.parvals <<- list(b, list(...))  # nolint
      data
    })(1, 2) %>>% makeLearner("classif.logreg"), pid.task)

  expect_identical(cpotest.parvals, list(2, list(a = 1)))

  cpotest.parvals = list()
  predict(t, pid.task)
  expect_identical(cpotest.parvals, list(2, list(a = 1, control = 0)))

  cpotest.parvals = list()
  t = train(makeCPOFunctional("testCPO", a: integer[, ], b: integer[, ],
    cpo.trafo = function(data, target, b, ...) {
      cpotest.parvals <<- list(b, list(...))  # nolint
      cpo.retrafo = function(data, ...) {
        cpotest.parvals <<- list(b, list(...))  # nolint
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

  cpoc = makeCPOObject("testCPO", a = FALSE: logical, b: integer[, ] [[requires = quote(!!a)]],
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


  cpoc = makeCPOFunctional("testCPO", a = FALSE: logical, b: integer[, ] [[requires = quote(!!a)]],
    cpo.trafo = {
      if (!a) {
        expect_true(missing(b))
      } else {
        expect_class(b, "integer")
      }
      control = 0
      cpo.retrafo = function(data) data
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

test_that("CPOs can be applied to data", {

  expect_identical(getTaskData(testtaskcpo %>>% cpomultiplier.f(10))$A, c(10, 20))
  expect_identical(getTaskData(testtaskcpo %>>% cpoadder.f(10))$A, c(11, 12))
  expect_identical(getTaskData(testtaskcpo %>>% cpoadder.f(10) %>>% cpomultiplier.f(10))$A, c(110, 120))
  expect_identical(getTaskData(testtaskcpo %>>% (cpoadder.f(10) %>>% cpomultiplier.f(10)))$A, c(110, 120))


  expect_identical(getTaskData(testtaskcpo %>>% cpomultiplier.o(10))$A, c(10, 20))
  expect_identical(getTaskData(testtaskcpo %>>% cpoadder.o(10))$A, c(11, 12))
  expect_identical(getTaskData(testtaskcpo %>>% cpoadder.o(10) %>>% cpomultiplier.o(10))$A, c(110, 120))
  expect_identical(getTaskData(testtaskcpo %>>% (cpoadder.o(10) %>>% cpomultiplier.o(10)))$A, c(110, 120))

  testdata = data.frame(A = c(1, 2))

  testdata %>>% cpomultiplier.f(10)

  expect_identical((testdata %>>% cpomultiplier.f(10))$A, c(10, 20))
  expect_identical((testdata %>>% cpoadder.f(10))$A, c(11, 12))
  expect_identical((testdata %>>% cpoadder.f(10) %>>% cpomultiplier.f(10))$A, c(110, 120))
  expect_identical((testdata %>>% (cpoadder.f(10) %>>% cpomultiplier.f(10)))$A, c(110, 120))


  expect_identical((testdata %>>% cpomultiplier.o(10))$A, c(10, 20))
  expect_identical((testdata %>>% cpoadder.o(10))$A, c(11, 12))
  expect_identical((testdata %>>% cpoadder.o(10) %>>% cpomultiplier.o(10))$A, c(110, 120))
  expect_identical((testdata %>>% (cpoadder.o(10) %>>% cpomultiplier.o(10)))$A, c(110, 120))

})

test_that("retrafo accessor does what it is supposed to do", {

  expect_null(retrafo(pid.task))

  expect_warning(expect_null(retrafo(10)), "not a Task or data.frame")

  x = 10
  expect_warning(expect_null(retrafo(x)), "not a Task or data.frame")

  expect_warning({retrafo(x) = identity}, "Task nor data.frame")

  expect_warning(expect_identical(retrafo(x), identity), "not a Task or data.frame")

  transformed = pid.task %>>% cpoScale()

  expect_function(retrafo(transformed))

  expect_equal(getTaskData(retrafo(transformed)(pid.task)), getTaskData(transformed))

  cpotest.parvals <<- list()  # nolint
  t = train(testlearnercpo, testtaskcpo)
  predict(t, testtaskcpo2)
  expect_identical(cpotest.parvals, list(1, 3))

  f1 = function(data, target, args) {
    data[[1]] = data[[1]] * 10
    return(list(data = data, control = list()))
  }

  f2 = function(data, target, args, control) {
    data[[1]] = data[[1]] / 10
    return(data)
  }
  wrappedlearner = makePreprocWrapper(testlearnercpo, train = f1, predict = f2, par.set = makeParamSet(), par.vals = list())


  testCPO = function(cpoadder, cpomultiplier) {
    # short chain, task
    result = testtaskcpo %>>% cpoadder(10)
    expect_identical(getTaskData(result)$A, c(11, 12))
    expect_equal(getTaskData(retrafo(result)(testtaskcpo2))$A, c(-8.5, -7.5))

    # short chain, data.frame
    result = testdfcpo %>>% cpoadder(10)
    expect_identical(result$A, c(11, 12))
    expect_equal(getTaskData(retrafo(result)(testtaskcpo2))$A, c(-8.5, -7.5))
    expect_equal(retrafo(result)(testdfcpo2)$A, c(-8.5, -7.5))

    # long chain, task
    result = (testtaskcpo %>>% cpoadder(10) %>>% cpomultiplier(2)) %>>% (cpoadder(-10) %>>% cpomultiplier(2))
    expect_equal(getTaskData(result)$A, c(24, 28))
    expect_equal(getTaskData(retrafo(result)(testtaskcpo2))$A, ((c(3, 4) - 10 - 1.5) / 2 + 10 - 23) / 2)

    # long chain, data.frame
    result = (testdfcpo %>>% cpoadder(10) %>>% cpomultiplier(2)) %>>% (cpoadder(-10) %>>% cpomultiplier(2))
    expect_equal(result$A, c(24, 28))
    expect_equal(getTaskData(retrafo(result)(testtaskcpo2))$A, ((c(3, 4) - 10 - 1.5) / 2 + 10 - 23) / 2)
    expect_equal(retrafo(result)(testdfcpo2)$A, ((c(3, 4) - 10 - 1.5) / 2 + 10 - 23) / 2)

    # short chain, learner model
    cpotest.parvals <<- list()  # nolint
    m = train(cpoadder(10) %>>% testlearnercpo, testtaskcpo)

    expect_equal(cpotest.parvals, list(11))
    expect_equal(getTaskData(retrafo(m)(testtaskcpo2))$A, c(-8.5, -7.5))
    expect_equal(retrafo(m)(testdfcpo2)$A, c(-8.5, -7.5))
    predict(m, testtaskcpo2)
    expect_equal(cpotest.parvals, list(11, -8.5))


    # long chain, learner model
    cpotest.parvals <<- list()  # nolint
    m = train((cpoadder(10, id = "fst") %>>% cpomultiplier(2, id = "snd")) %>>%
              ((cpoadder(-10, id = "thd") %>>% cpomultiplier(2, id = "frth")) %>>% testlearnercpo), testtaskcpo)

    expect_equal(cpotest.parvals,  list(24))
    expect_equal(getTaskData(retrafo(m)(testtaskcpo2))$A, ((c(3, 4) - 10 - 1.5) / 2 + 10 - 23) / 2)
    expect_equal(retrafo(result)(testdfcpo2)$A, ((c(3, 4) - 10 - 1.5) / 2 + 10 - 23) / 2)
    predict(m, testtaskcpo2)
    expect_equal(cpotest.parvals, list(24, ((3 - 10 - 1.5) / 2 + 10 - 23) / 2))

    # message when learner contains something else
    # THIS WILL NOT WORK WHEN PREPROC WRAPPERS ARE GONE!
    cpotest.parvals <<- list()  # nolint
    m = train((cpoadder(10, id = "fst") %>>% cpomultiplier(2, id = "snd")) %>>%
              ((cpoadder(-10, id = "thd") %>>% cpomultiplier(2, id = "frth")) %>>% wrappedlearner), testtaskcpo)
    expect_equal(cpotest.parvals, list(240))

    expect_message({ retr = retrafo(m) }, "has some wrappers besides CPOs", all = TRUE)
    expect_equal(getTaskData(retr(testtaskcpo2))$A, ((c(3, 4) - 10 - 1.5) / 2 + 10 - 23) / 2)
    expect_equal(retr(testdfcpo2)$A, ((c(3, 4) - 10 - 1.5) / 2 + 10 - 23) / 2)
    predict(m, testtaskcpo2)
    expect_equal(cpotest.parvals, list(240, (((3 - 10 - 1.5) / 2 + 10 - 23) / 2) / 10))

    # warning when learner contains buried CPOs
    # THIS WILL NOT HAPPEN WHEN PREPROC WRAPPERS ARE GONE!
    buriedlearner = makePreprocWrapper(cpoadder(-10, id = "thd") %>>% (cpomultiplier(2, id = "frth") %>>% testlearnercpo),
      train = f1, predict = f2, par.set = makeParamSet(), par.vals = list())

    cpotest.parvals <<- list()  # nolint
    m = train((cpoadder(10, id = "fst") %>>% (cpomultiplier(2, id = "snd")) %>>% buriedlearner), testtaskcpo)
    expect_equal(cpotest.parvals, list((11 * 2 * 10 - 10) * 2))

    expect_warning({ retr = retrafo(m) }, "has some CPOs wrapped by other wrappers", all = TRUE)
    expect_equal(getTaskData(retr(testtaskcpo2))$A, ((c(3, 4) - 10 - 1.5) / 2))
    expect_equal(retr(testdfcpo2)$A, ((c(3, 4) - 10 - 1.5) / 2))
    predict(m, testtaskcpo2)
    expect_equal(cpotest.parvals, list((11 * 2 * 10 - 10) * 2, (((3 - 10 - 1.5) / 2 / 10 + 10 - 230) / 2)))
  }

  testCPO(cpoadder.f, cpomultiplier.f)

  testCPO(cpoadder.o, cpomultiplier.o)

})

test_that("functional trafo and retrafo return values are checked", {

  cpoone.f = makeCPOFunctional("one", a: logical, cpo.trafo = {
    cpo.retrafo = identity
    data
  })

  cpotwo.f = makeCPOFunctional("two", b: logical, cpo.trafo = {
    cpo.retrafo = identity
    data
  })

  cpobad.trafo.f = makeCPOFunctional("badtrafo", c: logical, cpo.trafo = {
    cpo.retrafo = identity
    data[[1]]
  })

  cpobad.retrafo.f = makeCPOFunctional("badretrafo", d: logical, cpo.trafo = {
    cpo.retrafo = function(data) data[[1]]
    data
  })

  expect_class(pid.task %>>% cpoone.f(TRUE) %>>% cpotwo.f(TRUE), "Task")
  expect_error(pid.task %>>% cpoone.f(TRUE) %>>% cpotwo.f(TRUE) %>>% cpobad.trafo.f(TRUE), "badtrafo cpo\\.trafo .*cpo.trafo must return a data.frame")
  expect_error(pid.task %>>% cpoone.f(TRUE) %>>% cpobad.trafo.f(TRUE) %>>% cpotwo.f(TRUE), "badtrafo cpo\\.trafo .*cpo.trafo must return a data.frame")
  expect_error(pid.task %>>% cpobad.trafo.f(TRUE) %>>% cpoone.f(TRUE) %>>% cpotwo.f(TRUE), "badtrafo cpo\\.trafo .*cpo.trafo must return a data.frame")

  expect_class({res = pid.task %>>% cpoone.f(TRUE) %>>% cpotwo.f(TRUE) %>>% cpobad.retrafo.f(TRUE)}, "Task")
  expect_error(retrafo(res)(pid.task), "badretrafo retrafo .*must return a data.frame")
  expect_class({res = pid.task %>>% cpoone.f(TRUE) %>>% cpobad.retrafo.f(TRUE) %>>% cpotwo.f(TRUE)}, "Task")
  expect_error(retrafo(res)(pid.task), "badretrafo retrafo .*must return a data.frame")
  expect_class({res = pid.task %>>% cpobad.retrafo.f(TRUE) %>>% cpoone.f(TRUE) %>>% cpotwo.f(TRUE)}, "Task")
  expect_error(retrafo(res)(pid.task), "badretrafo retrafo .*must return a data.frame")
  expect_class({res = pid.task %>>% (cpobad.retrafo.f(TRUE) %>>% cpoone.f(TRUE) %>>% cpotwo.f(TRUE))}, "Task")
  expect_error(retrafo(res)(pid.task), "badretrafo retrafo .*must return a data.frame")

  expect_class(train(cpoone.f(TRUE) %>>% cpotwo.f(TRUE) %>>% makeLearner("classif.logreg"), pid.task), "WrappedModel")
  expect_error(train(cpoone.f(TRUE) %>>% cpotwo.f(TRUE) %>>%
                     cpobad.trafo.f(TRUE) %>>% makeLearner("classif.logreg"), pid.task),
    "badtrafo cpo\\.trafo .*cpo.trafo must return a data.frame")
  expect_error(train(cpoone.f(TRUE) %>>% cpobad.trafo.f(TRUE) %>>%
                     cpotwo.f(TRUE) %>>% makeLearner("classif.logreg"), pid.task),
    "badtrafo cpo\\.trafo .*cpo.trafo must return a data.frame")
  expect_error(train(cpobad.trafo.f(TRUE) %>>% cpoone.f(TRUE) %>>%
                     cpotwo.f(TRUE) %>>% makeLearner("classif.logreg"), pid.task),
    "badtrafo cpo\\.trafo .*cpo.trafo must return a data.frame")


  expect_class({res = train(cpoone.f(TRUE) %>>% cpotwo.f(TRUE) %>>% cpobad.retrafo.f(TRUE) %>>%
                            makeLearner("classif.logreg"), pid.task)}, "WrappedModel")
  expect_error(predict(res, pid.task), "badretrafo retrafo .*must return a data.frame")
  expect_class({res = train(cpoone.f(TRUE) %>>% cpobad.retrafo.f(TRUE) %>>% cpotwo.f(TRUE) %>>%
                            makeLearner("classif.logreg"), pid.task)}, "WrappedModel")
  expect_error(predict(res, pid.task), "badretrafo retrafo .*must return a data.frame")
  expect_class({res = train(cpobad.retrafo.f(TRUE) %>>% cpoone.f(TRUE) %>>% cpotwo.f(TRUE) %>>%
                            makeLearner("classif.logreg"), pid.task)}, "WrappedModel")
  expect_error(predict(res, pid.task), "badretrafo retrafo .*must return a data.frame")
})

test_that("object based trafo and retrafo return values are checked", {

  cpoone.o = makeCPOObject("one", a: logical, cpo.trafo = {
    control = 0
    data
  }, cpo.retrafo = {
    data
  })

  cpotwo.o = makeCPOObject("two", b: logical, cpo.trafo = {
    control = 0
    data
  }, cpo.retrafo = {
    data
  })

  cpobad.trafo.o = makeCPOObject("badtrafo", c: logical, cpo.trafo = {
    data
  }, cpo.retrafo = {
    data
  })

  cpobad.retrafo.o = makeCPOObject("badretrafo", d: logical, cpo.trafo = {
    control = 0
    data
  }, cpo.retrafo = {
    data[[1]]
  })

  expect_class(pid.task %>>% cpoone.o(TRUE) %>>% cpotwo.o(TRUE), "Task")
  expect_error(pid.task %>>% cpoone.o(TRUE) %>>% cpotwo.o(TRUE) %>>% cpobad.trafo.o(TRUE), "badtrafo cpo\\.trafo did not create a 'control'")
  expect_error(pid.task %>>% cpoone.o(TRUE) %>>% cpobad.trafo.o(TRUE) %>>% cpotwo.o(TRUE), "badtrafo cpo\\.trafo did not create a 'control'")
  expect_error(pid.task %>>% cpobad.trafo.o(TRUE) %>>% cpoone.o(TRUE) %>>% cpotwo.o(TRUE), "badtrafo cpo\\.trafo did not create a 'control'")

  expect_class({res = pid.task %>>% cpoone.o(TRUE) %>>% cpotwo.o(TRUE) %>>% cpobad.retrafo.o(TRUE)}, "Task")
  expect_error(retrafo(res)(pid.task), "badretrafo cpo\\.retrafo .*must return a data.frame")
  expect_class({res = pid.task %>>% cpoone.o(TRUE) %>>% cpobad.retrafo.o(TRUE) %>>% cpotwo.o(TRUE)}, "Task")
  expect_error(retrafo(res)(pid.task), "badretrafo cpo\\.retrafo .*must return a data.frame")
  expect_class({res = pid.task %>>% cpobad.retrafo.o(TRUE) %>>% cpoone.o(TRUE) %>>% cpotwo.o(TRUE)}, "Task")
  expect_error(retrafo(res)(pid.task), "badretrafo cpo\\.retrafo .*must return a data.frame")
  expect_class({res = pid.task %>>% (cpobad.retrafo.o(TRUE) %>>% cpoone.o(TRUE) %>>% cpotwo.o(TRUE))}, "Task")
  expect_error(retrafo(res)(pid.task), "badretrafo cpo\\.retrafo .*must return a data.frame")

  expect_class(train(cpoone.o(TRUE) %>>% cpotwo.o(TRUE) %>>% makeLearner("classif.logreg"), pid.task), "WrappedModel")
  expect_error(train(cpoone.o(TRUE) %>>% cpotwo.o(TRUE) %>>%
                     cpobad.trafo.o(TRUE) %>>% makeLearner("classif.logreg"), pid.task),
    "badtrafo cpo\\.trafo did not create a 'control'")
  expect_error(train(cpoone.o(TRUE) %>>% cpobad.trafo.o(TRUE) %>>%
                     cpotwo.o(TRUE) %>>% makeLearner("classif.logreg"), pid.task),
    "badtrafo cpo\\.trafo did not create a 'control'")
  expect_error(train(cpobad.trafo.o(TRUE) %>>% cpoone.o(TRUE) %>>%
                     cpotwo.o(TRUE) %>>% makeLearner("classif.logreg"), pid.task),
    "badtrafo cpo\\.trafo did not create a 'control'")

  expect_class({res = train(cpoone.o(TRUE) %>>% cpotwo.o(TRUE) %>>% cpobad.retrafo.o(TRUE) %>>%
                            makeLearner("classif.logreg"), pid.task)}, "WrappedModel")
  expect_error(predict(res, pid.task), "badretrafo cpo\\.retrafo .*must return a data.frame")
  expect_class({res = train(cpoone.o(TRUE) %>>% cpobad.retrafo.o(TRUE) %>>% cpotwo.o(TRUE) %>>%
                            makeLearner("classif.logreg"), pid.task)}, "WrappedModel")
  expect_error(predict(res, pid.task), "badretrafo cpo\\.retrafo .*must return a data.frame")
  expect_class({res = train(cpobad.retrafo.o(TRUE) %>>% cpoone.o(TRUE) %>>% cpotwo.o(TRUE) %>>%
                            makeLearner("classif.logreg"), pid.task)}, "WrappedModel")
  expect_error(predict(res, pid.task), "badretrafo cpo\\.retrafo .*must return a data.frame")
})

test_that("to.list and chainCPO work", {

  testCPO = function(cpoadder, cpomultiplier) {

    cpochain = ((cpoadder(10, id = "fst") %>>% cpomultiplier(2, id = "snd")) %>>%
                (cpoadder(-10, id = "thd") %>>% cpomultiplier(2, id = "frth")))


    result = testdfcpo %>>% cpochain
    expect_equal(result$A, c(24, 28))
    expect_equal(retrafo(result)(testdfcpo2)$A, ((c(3, 4) - 10 - 1.5) / 2 + 10 - 23) / 2)

    cpolist = as.list(cpochain)
    expect_list(cpolist, len = 4)
    expect_equal(cpolist[[1]], cpoadder(10, id = "fst"))
    expect_equal(cpolist[[2]], cpomultiplier(2, id = "snd"))
    expect_equal(cpolist[[3]], cpoadder(-10, id = "thd"))
    expect_equal(cpolist[[4]], cpomultiplier(2, id = "frth"))

    result = testdfcpo %>>% chainCPO(cpolist)
    expect_equal(result$A, c(24, 28))
    expect_equal(retrafo(result)(testdfcpo2)$A, ((c(3, 4) - 10 - 1.5) / 2 + 10 - 23) / 2)

    expect_equal(cpolist, as.list(chainCPO(cpolist)))

    cpolist.chg = as.list(setHyperPars(cpochain, fst.summand = 20))
    expect_equal(cpolist.chg[[1]], cpoadder(20, id = "fst"))

    cpolist.chg[[2]] = setHyperPars(cpolist.chg[[2]], snd.factor = 10)
    result = testdfcpo %>>% chainCPO(cpolist.chg)
    expect_equal(result$A, ((c(1, 2) + 20) * 10 - 10) * 2)
    expect_equal(retrafo(result)(testdfcpo2)$A, ((c(3, 4) - 20 - 1.5) / 10 + 10 - 215) / 2)

  }

  testCPO(cpoadder.f, cpomultiplier.f)
  testCPO(cpoadder.o, cpomultiplier.o)
})


test_that("retrafo catabolization and anabolization work", {

  cpoadder = cpoadder.f
  cpomultiplier = cpomultiplier.f

  testCPO = function(cpoadder, cpomultiplier) {

    cpochain = ((cpoadder(20, id = "fst") %>>% cpomultiplier(2, id = "snd")) %>>%
                (cpoadder(-10, id = "thd") %>>% cpomultiplier(2, id = "frth")) %>>%
                cpoadder(10) %>>% cpomultiplier(2))

    cpochain = setHyperPars(cpochain, frth.factor = -2)

    res = testdfcpo %>>% cpochain

    expect_equal(res[[1]], (((c(1, 2) + 20) * 2 - 10) * -2 + 10) * 2)


    expect_equal(retrafo(res)(testdfcpo2)[[1]], (((c(3, 4) - 20 - 1.5) / 2 + 10 - 43) / -2 - 10 + 66) / 2)

    lrn = cpomultiplier(2, id = "a") %>>% cpomultiplier(0.5, id = "b") %>>% cpochain %>>% testlearnercpo

    retrafochain = retrafo(train(setHyperPars(lrn, fst.summand = 10), testtaskcpo))

    rfclist = as.list(retrafochain)

    expect_equal(getRetrafoState(rfclist[[1]])$factor, 2)
    expect_equal(getRetrafoState(rfclist[[2]])$factor, 0.5)
    expect_equal(getRetrafoState(rfclist[[3]])$summand, 10)
    expect_equal(getRetrafoState(rfclist[[6]])$factor, -2)

    rfclist.states = lapply(rfclist, getRetrafoState)

    constructors = list(cpomultiplier, cpomultiplier, cpoadder, cpomultiplier, cpoadder, cpomultiplier, cpoadder, cpomultiplier)
    rfclist2 = lapply(seq_along(constructors), function(idx) makeRetrafoFromState(constructors[[idx]], rfclist.states[[idx]]))

    expect_equal(chainCPO(rfclist)(testdfcpo2)[[1]], (((c(3, 4) - 10 - 1.5) / 2 + 10 - 23) / -2 - 10 + 26) / 2)
    expect_equal(chainCPO(rfclist2)(testdfcpo2)[[1]], (((c(3, 4) - 10 - 1.5) / 2 + 10 - 23) / -2 - 10 + 26) / 2)

    expect_equal((chainCPO(rfclist[c(1:5)]) %>>% chainCPO(rfclist[c(6:8)]))(testdfcpo2)[[1]], (((c(3, 4) - 10 - 1.5) / 2 + 10 - 23) / -2 - 10 + 26) / 2)
    expect_equal((chainCPO(rfclist2[c(1:5)]) %>>% chainCPO(rfclist2[c(6:8)]))(testdfcpo2)[[1]], (((c(3, 4) - 10 - 1.5) / 2 + 10 - 23) / -2 - 10 + 26) / 2)

    rfclist.states2 = rfclist.states
    rfclist.states2[[1]]$factor = 4
    rfclist2 = lapply(seq_along(constructors), function(idx) makeRetrafoFromState(constructors[[idx]], rfclist.states2[[idx]]))
    expect_equal(chainCPO(rfclist2)(testdfcpo2)[[1]], (((c(3, 4) / 2 - 10 - 1.5) / 2 + 10 - 23) / -2 - 10 + 26) / 2)

    chain2 = chainCPO(rfclist[c(1, 2, 3)]) %>>% chainCPO(rfclist[c(4, 5, 6, 7, 8)])

    expect_equal(chain2(testdfcpo2)[[1]], (((c(3, 4) - 10 - 1.5) / 2 + 10 - 23) / -2 - 10 + 26) / 2)

    chain3 = chainCPO(rfclist[3:4]) %>>% chainCPO(rfclist[c(5:6, 1:2)]) %>>% chainCPO(rfclist[7:8])

    expect_equal(chain3(testdfcpo2)[[1]], (((c(3, 4) - 10 - 1.5) / 2 + 10 - 23) / -2 - 10 + 26) / 2)

    expect_equal((testdfcpo2 %>>% chainCPO(rfclist[7:8]) %>>% chainCPO(rfclist[5:6]) %>>% chainCPO(rfclist[1:2]) %>>% chainCPO(rfclist[3:4]))[[1]],
      (((c(3, 4) - 10 + 26) / 2 + 10 - 23) / -2 - 10 - 1.5) / 2)
    expect_equal((testdfcpo2 %>>% chainCPO(rfclist[c(7, 8, 5, 6, 3, 4, 1, 2)]))[[1]], (((c(3, 4) - 10 + 26) / 2 + 10 - 23) / -2 - 10 - 1.5) / 2)
    chained.again = chainCPO(rfclist[7:8]) %>>% (chainCPO(rfclist[5:6]) %>>% chainCPO(rfclist[1:4]))
    expect_equal((testdfcpo2 %>>% chained.again)[[1]], (((c(3, 4) - 10 + 26) / 2 + 10 - 23) / -2 - 10 - 1.5) / 2)

    expect_error(setHyperPars(as.list(chained.again)[[1]], summand = 10), "Cannot change parameter values")
    expect_error(removeHyperPars(as.list(chained.again)[[1]], "summand"))

    expect_error(getHyperPars(chained.again), "Cannot get parameters of compound retrafo")
    expect_error(getParamSet(chained.again), "Cannot get param set of compound retrafo")

    expect_identical(getHyperPars(as.list(chained.again)[[1]]), list(summand = 10))
    expect_identical(getHyperPars(as.list(chained.again)[[7]]), list(summand = 10))
    expect_identical(getHyperPars(as.list(chained.again)[[4]]), list(factor = -2))

    expect_identical(getParamSet(as.list(chained.again)[[1]]), paramSetSugar(summand = 1: integer[, ]))
    expect_identical(getParamSet(as.list(chained.again)[[7]]), paramSetSugar(summand = 1: integer[, ]))
    expect_identical(getParamSet(as.list(chained.again)[[4]]), paramSetSugar(factor = 1: numeric[~., ~.]))

    testdfcpo %>>% chainCPO(as.list(cpochain)[1:3])
    firsthalf = retrafo(testdfcpo %>>% chainCPO(as.list(cpochain)[1:3]))
    secondhalf = retrafo(testdfcpo %>>% chainCPO(as.list(cpochain)[4:6]))

    expect_equal((testdfcpo2 %>>% firsthalf)[[1]], (c(3, 4) - 20 - 1.5) / 2 + 10 - 43)
    expect_equal((testdfcpo2 %>>% secondhalf)[[1]], (c(3, 4) / -2 - 10 + 3) / 2)

    expect_equal((testdfcpo2 %>>% (firsthalf %>>% secondhalf))[[1]], (((c(3, 4) - 20 - 1.5) / 2 + 10 - 43) / -2 - 10 + 3) / 2)

    cpotest.parvals <<- list()  # nolint
    retrafos = list(
        (testdfcpo %>>% cpoadder(10, id = "fst")) %>>% cpomultiplier(2, id = "snd"),  # apply CPO twice
        testdfcpo %>>% (cpoadder(10, id = "fst") %>>% cpomultiplier(2, id = "snd")),  # apply compound CPO
        train(setHyperPars(cpoadder(1, id = "fst") %>>% (cpomultiplier(1, id = "snd") %>>%
                                                         testlearnercpo), fst.summand = 10, snd.factor = 2), testtaskcpo), # wrap with CPO twice
        train(setHyperPars((cpoadder(1, id = "fst") %>>% cpomultiplier(1, id = "snd")), fst.summand = 10, snd.factor = 2) %>>%
              testlearnercpo, testtaskcpo))  # wrap with compound CPO

    expect_equal(cpotest.parvals, list(22, 22))


    for (retgen in retrafos) {
      ret = retrafo(retgen)
      expect_equal((testdfcpo2 %>>% ret)[[1]], (c(3, 4) - 10 - 1.5) / 2)
      expect_equal((testdfcpo2 %>>% as.list(ret)[[1]])[[1]], c(3, 4) - 10 - 1.5)
      expect_equal((testdfcpo2 %>>% as.list(ret)[[2]])[[1]], c(3, 4) / 2)
      expect_equal((testdfcpo2 %>>% chainCPO(as.list(ret)[c(2, 1)]))[[1]], c(3, 4) / 2 - 10 - 1.5)
    }
  }

  testCPO(cpoadder.o, cpomultiplier.o)
  testCPO(cpoadder.f, cpomultiplier.f)


})


