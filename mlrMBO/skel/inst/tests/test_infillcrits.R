context("infill crits")

test_that("infill crits", {
  f1 = makeMBOFunction(function(x) sum(x^2))
  f2 = makeMBOFunction(function(x) sum(x^2) + rnorm(1, 0, 0.1))
  ps = makeParamSet(
    makeNumericVectorParam("x", len=2, lower=-10, upper=10)
  )
  mycontrol = function(minimize, crit) {
    makeMBOControl(minimize=minimize, init.design.points=20, iters=10, infill.opt.random.points=500,
      infill.crit=crit, infill.opt="random", final.evals = 100)
  }
  mycheck = function(or, minimize) {
    expect_equal(getOptPathLength(or$opt.path), 30)
    expect_true(!is.na(or$y))
    if (minimize)
      expect_true(or$y < 1)
    else
      expect_true(or$y > 100)
  }

  for (noisy in c(FALSE, TRUE)) {
    for (minimize in c(TRUE, FALSE)) {
      crits = if (!noisy) c("mean", "ei") else c("aei")
      nugget.estim = if (!noisy) FALSE else TRUE
      learner = makeLearner("regr.km", predict.type="se", nugget.estim = nugget.estim)
      for (crit in crits) {
        ctrl = mycontrol(minimize, crit)
        f = if (!noisy) f1 else f2
        or = mbo(f, ps, NULL, learner, ctrl, show.info=FALSE)
        mycheck(or, minimize)
      }
    }
  }
})


