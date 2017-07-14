# FIXME: the underlying learner API seems to suport weights, but the learner does not really change when we use them
# see mlr issue 857 for further info

#' @export
makeRLearner.classif.lqa = function() {
  makeRLearnerClassif(
    cl = "classif.lqa",
    package = "lqa",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "penalty",
        values = c("adaptive.lasso", "ao", "bridge", "enet", "fused.lasso", "genet", "icb", "lasso",
          "licb", "oscar", "penalreg", "ridge", "scad", "weighted.fusion")),
      makeNumericLearnerParam(id = "lambda", lower = 0,
        requires = quote(penalty %in% c("adaptive.lasso", "ao", "bridge", "genet", "lasso",
          "oscar", "penalreg", "ridge", "scad"))),
      makeNumericLearnerParam(id = "gamma", lower = 1 + .Machine$double.eps,
        requires = quote(penalty %in% c("ao", "bridge", "genet", "weighted.fusion"))),
      makeNumericLearnerParam(id = "alpha", lower = 0, upper = 1, requires = quote(penalty == "genet")),
      makeNumericLearnerParam(id = "oscar.c", lower = 0, requires = quote(penalty == "oscar")),
      makeNumericLearnerParam(id = "a", lower = 2 + .Machine$double.eps,
        requires = quote(penalty == "scad")),
      makeNumericLearnerParam(id = "lambda1", lower = 0,
        requires = quote(penalty %in% c("enet", "fused.lasso", "icb", "licb", "weighted.fusion"))),
      makeNumericLearnerParam(id = "lambda2", lower = 0,
        requires = quote(penalty %in% c("enet", "fused.lasso", "icb", "licb", "weighted.fusion"))),
      makeDiscreteLearnerParam(id = "method", default = "lqa.update2",
        values = c("lqa.update2", "ForwardBoost", "GBlockBoost")),
      makeNumericLearnerParam(id = "var.eps", default = .Machine$double.eps, lower = 0),
      makeIntegerLearnerParam(id = "max.steps", lower = 1L, default = 5000L),
      makeNumericLearnerParam(id = "conv.eps", default = 0.001, lower = 0),
      makeLogicalLearnerParam(id = "conv.stop", default = TRUE),
      makeNumericLearnerParam(id = "c1", default = 1e-08, lower = 0),
      makeIntegerLearnerParam(id = "digits", default = 5L, lower = 1L)
    ),
    properties = c("numerics", "prob", "twoclass"),
    par.vals = list(penalty = "lasso", lambda = 0.1),
    name = "Fitting penalized Generalized Linear Models with the LQA algorithm",
    short.name = "lqa",
    note = "`penalty` has been set to `\"lasso\"` and `lambda` to `0.1` by default. The parameters `lambda`, `gamma`, `alpha`, `oscar.c`, `a`, `lambda1` and `lambda2` are the tuning parameters of the `penalty` function being used, and correspond to the parameters as named in the respective help files. Parameter `c` for penalty method `oscar` has been named `oscar.c`. Parameters `lambda1` and `lambda2` correspond to the parameters named 'lambda_1' and 'lambda_2' of the penalty functions `enet`, `fused.lasso`, `icb`, `licb`, as well as `weighted.fusion`.",
    callees = c("lqa", "lqa.control", "adaptive.lasso", "ao", "bridge", "enet", "fused.lasso", "genet", "icb", "lasso", "licb", "oscar", "penalreg", "ridge", "scad", "weighted.fusion")
  )
}

#' @export
trainLearner.classif.lqa = function(.learner, .task, .subset, .weights = NULL,
  var.eps, max.steps, conv.eps, conv.stop, c1, digits, ...) {

  ctrl = learnerArgsToControl(lqa::lqa.control, var.eps, max.steps, conv.eps, conv.stop, c1, digits)
  d = getTaskData(.task, .subset, target.extra = TRUE, recode.target = "01")
  args = c(list(x = d$data, y = d$target, family = binomial(), control = ctrl), list(...))
  rm(d)
  if (!args$penalty %in% c("adaptive.lasso", "ao", "bridge", "genet", "lasso",
                           "oscar", "penalreg", "ridge", "scad")) {
    args$lambda = NULL
  }
  tune.param.order = c("lambda", "lambda1", "lambda2", "alpha", "gamma", "oscar.c", "a")
  penfun = getFromNamespace(args$penalty, "lqa")
  args$penalty = do.call(penfun, list(lambda = unlist(args[tune.param.order])))
  args = args[!names(args) %in% tune.param.order]
  # if (!is.null(.weights))
    # args$weights = .weights

  do.call(lqa::lqa.default, args)
}

#' @export
predictLearner.classif.lqa = function(.learner, .model, .newdata, ...) {
  p = lqa::predict.lqa(.model$learner.model, new.x = cbind(1, .newdata), ...)$mu.new
  levs = c(.model$task.desc$negative, .model$task.desc$positive)
  if (.learner$predict.type == "prob") {
    p = propVectorToMatrix(p, levs)
  } else {
    p = as.factor(ifelse(p > 0.5, levs[2L], levs[1L]))
    names(p) = NULL
  }
  return(p)
}
