
context("ParamSetSugar")

test_that("ParamSetSugar generates the expected ParamSets", {

  param.list = list(a = "b", b = "a")

  x = 10

  # list of pairs that whould have identical elements
  param.sets = list(
      list(makeParamSet(makeIntegerLearnerParam("a")),
           paramSetSugar(a: integer(, ))),
      list(makeParamSet(makeDiscreteLearnerParam("b", c("a", "b", "c")),
                        makeDiscreteLearnerParam("c", list(a = 1, b = 2, c = 3), default = 1),
                        makeDiscreteVectorLearnerParam("d", 2, list(a = "a", b = "b", `1` = 1), default = list("a", 1)),
                        makeDiscreteLearnerParam("e", param.list, when = "both", requires = expression(b == "a"), default = "b")),
           paramSetSugar(b: discrete(a, b, c),
                         c = 1: discrete(a = 1, b = 2, c = 3),
                         d = list("a", 1): discrete(a, b, 1)^2,
                         e = "b": discrete(param.list) [when = "both", requires = expression(b == "a")])),
      list(makeParamSet(makeNumericParam("a", allow.inf = TRUE, default = 1, tunable = FALSE),
                        makeNumericParam("b", lower = 0, allow.inf = TRUE, default = 2),
                        makeNumericVectorParam("c", 3, upper = 0, allow.inf = FALSE, special.vals = list(-1)),
                        makeNumericVectorParam("d", 2, lower = 0, upper = 1, default = c(0.5, 0.5), requires = expression(a == 0))),
           paramSetSugar(a = 1: numeric(, ) [tunable = FALSE],
                         b = 2: numeric(0, ),
                         c: numeric(., x - 10)^(2 + 1) [special.vals = list(1 - 2)],
                         d = c(1 / 2, 0.5): numeric(0, 1)^2 [requires = expression(a == 0), allow.inf = FALSE], pss.learner.params = FALSE))
  )

  for (ps.pair in param.sets) {
    expect_identical(ps.pair[[1]], ps.pair[[2]])
  }

})

test_that("ParamSetSugar handles exotic bounds well", {

  # list of triplets: (param set, feasible example points, infeasible example points).

  rangetests = list(list(paramSetSugar(a: integer(, )),
            list(list(a = -1), list(a = 0), list(a = 2^30)),
            list(list(a = -Inf), list(a = Inf), list(a = 0.5))),
       list(paramSetSugar(a: numeric(, )),
            list(list(a = 0), list(a = .Machine$double.xmax), list(a = -Inf)),
            list()),
       list(paramSetSugar(a: integer(0, )),
            list(list(a = 0), list(a = 1), list(a = 100)),
            list(list(a = -1), list(a = 0.5))),
       list(paramSetSugar(a: numeric(0, .)),
            list(list(a = 0), list(a = .Machine$double.xmax)),
            list(list(a = -1), list(a = Inf))),
       list(paramSetSugar(a: numeric(~0, )),
            list(list(a = Inf), list(a = 1), list(a = .Machine$double.eps)),
            list(list(a = -1), list(a = 0), list(a = -Inf))),
       list(paramSetSugar(a: numeric(., )),
            list(list(a = Inf), list(a = -.Machine$double.xmax)),
            list(list(a = -Inf))),
       list(paramSetSugar(a: numeric(., .)),
            list(list(a = 0), list(a = .Machine$double.xmax)),
            list(list(a = Inf), list(a = -Inf))),
       list(paramSetSugar(a: numeric(~0, 1)),
            list(list(a = .Machine$double.eps), list(a = 1), list(a = 0.5)),
            list(list(a = 0), list(a = -Inf), list(a = Inf), list(a = 1 + .Machine$double.eps))),
       list(paramSetSugar(a: discrete(1, 2, 3)),
            list(list(a = 1), list(a = 2), list(a = 3)),
            list(list(a = "1"), list(a = 4))),
       list(paramSetSugar(a: discrete(a = "b", b = "c")),
            list(list(a = "b"), list(a = "c")),
            list(list(a = "a"))),
       list(paramSetSugar(a: numeric(~1, ~2^30)),
            list(list(a = 2), list(a = 2^30 - 1)),
            list(list(a = 1), list(a = 2^30)))
       )

  for (rt in rangetests) {
    for (feas in rt[[2]]) {
      expect_true(isFeasible(rt[[1]], feas))
    }
    for (infeas in rt[[3]]) {
      expect_false(isFeasible(rt[[1]], infeas))
    }
  }

})

test_that("ParamSetSugar works when called indirectly", {

  pss2 = function(..., env) {
    x = 3
    paramSetSugar(..., pss.env = env)
  }

  pss1 = function(...) {
    x = 2
    pss2(..., env = parent.frame())
  }

  x = 1
  expect_identical(pss1(pss.learner.params = FALSE, a = x: integer(x, x * 2)^x [tunable = (x == 2)]),
                   makeParamSet(makeIntegerVectorParam("a", 1, 1, 2, default = 1, tunable = FALSE)))
})

