
context("cpo")

test_that("CPOs can be created", {

  expect_class(makeCPOFunctional("testCPO", cpo.trafo = { }), "CPOConstructor")

  expect_class(makeCPOFunctional("testCPO", a: integer(, 1), cpo.trafo = { }), "CPOConstructor")

  expect_class(makeCPOFunctional("testCPO", a = 1: integer(, 1), par.vals = list(a = 0), cpo.trafo = { }), "CPOConstructor")

  expect_class(makeCPOFunctional("testCPO", par.set = paramSetSugar(a: integer(0, 1)), par.vals = list(a = 0), cpo.trafo = { }), "CPOConstructor")


  expect_class(makeCPOObject("testCPO", cpo.trafo = { }, cpo.retrafo = { }), "CPOConstructor")

  expect_class(makeCPOObject("testCPO", a: integer(, 1), cpo.trafo = { }, cpo.retrafo = { }), "CPOConstructor")

  expect_class(makeCPOObject("testCPO", a = 1: integer(, 1), par.vals = list(a = 0), cpo.trafo = { }, cpo.retrafo = { }), "CPOConstructor")

  expect_class(makeCPOObject("testCPO", par.set = paramSetSugar(a: integer(0, 1)), par.vals = list(a = 0), cpo.trafo = { }, cpo.retrafo = { }), "CPOConstructor")

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
