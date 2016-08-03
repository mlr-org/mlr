#' @export
makeRLearner.regr.GPfit = function(){
  makeRLearnerRegr(
    cl = "regr.GPfit",
    package = "GPfit",
    par.set = makeParamSet(
      makeNumericVectorLearnerParam(id = "control", len = 3, lower = c(1, 1, 1)),
      makeNumericLearnerParam(id = "nug_thres", default = 20, lower = 0),
      makeLogicalLearnerParam(id = "trace", default = FALSE, tunable = FALSE),
      makeIntegerLearnerParam(id = "maxit", default = 100, lower = 1),
      makeUntypedLearnerParam(id = "optim_start", default = NULL),  
      makeLogicalLearnerParam(id = "scale", default = TRUE),
      makeDiscreteLearnerParam(id = "corr_type", values = c("exponential", "matern"), default = "exponential"),
      makeIntegerLearnerParam(id = "matern_nu_k", default = 0L, lower = 0L, requires = quote(corr_type == "matern")), 
      makeNumericLearnerParam(id = "exp_power", default = 1.95, lower = 1.0, upper = 2.0, requires = quote(corr_type == "exponential"))
    ),
    par.vals = list(scale = TRUE, corr_type = "exponential",  matern_nu_k = 0L, exp_power = 1.95),
    properties = c("numerics","se"),
    name = "Gaussian Process",
    short.name = "GPfit",
    note = "As the optimization routine assumes that the inputs are scaled to the unit hypercube [0,1]^d, 
            the input gets scaled for each variable by default. If this is not wanted, scale = FALSE has
            to be set."
  )
}
#' @export
trainLearner.regr.GPfit = function(.learner, .task, .subset, .weights = NULL, scale, corr_type, matern_nu_k, exp_power, ...) {
  # tri_dots = list(...)
  # trans.vec = c("corr_type", "matern_nu_k", "exp_power")
  # dots = dropNamed(tri_dots, trans.vec)
  # print(dots)
  # if (tri_dots$corr_type == "exponential") {
  #   corr = list(type="exponential", power = tri_dots$"exp_power")
  # } else {
  #   k = tri_dots$matern_nu_k
  #   corr = list(type="matern",nu = k+0.5 )
  # }
  # args = c(dots, list(corr))
  
  d = getTaskData(.task, .subset, target.extra = TRUE)
  low = apply(d$data, 2, min)
  high = apply(d$data, 2, max)
  not.const = colnames(d$data)[high != low]
  if (scale) {
    d$data[,not.const] = apply(d$data[,not.const], 2, function(x) x = (x - min(x)) / (max(x) - min(x)))
    mlist = list(scaled = TRUE, not.const = not.const, high = high, low = low)
  } else {
    mlist = list(scaled = FALSE, not.const = not.const)
  }
  res = GPfit::GP_fit(d$data[, not.const], d$target, corr = list(type = corr_type, power = exp_power, nu = matern_nu_k+0.5 ), ...)
  # h.GPfit = function(...) {
  #   GPfit::GP_fit(X = d$data[, not.const], Y = d$target, ...)
  # }
  # res = do.call(h.GPfit, args)
  res = attachTrainingInfo(res, mlist)
  return(res)
}
#' @export
predictLearner.regr.GPfit = function(.learner, .model, .newdata, ...) {
  tr.info = getTrainingInfo(.model)
  if (tr.info$scaled) {
      for (col.name in tr.info$not.const) {
        .newdata[,col.name] =  (.newdata[,col.name] - tr.info$low[col.name]) / (tr.info$high[col.name] - tr.info$low[col.name])
    }
  } 
  rst=predict(.model$learner.model, xnew = .newdata[, tr.info$not.const])
  
  se = (.learner$predict.type != "response")
  if (!se)
    return(rst$Y_hat)
  else
    cbind(rst$Y_hat, rst$MSE)
}

