#'@export
makeRLearner.fcregr.garch = function() {
  makeRLearnerForecastRegr(
    cl = "fcregr.garch",
    package = "rugarch",
    par.set = makeParamSet(
      # BEGIN: spec
      ## BEGIN:variance.model
      makeDiscreteLearnerParam(id = "model",
                               values = c("sGARCH", "fGARCH",
                                          "eGARCH", "gjrGARCH",
                                          "apARCH", "iGARCH",
                                          "csGARCH"), default = "sGARCH"),
      makeIntegerVectorLearnerParam("garchOrder",
                                    len = 2L,
                                    lower = 1L,
                                    default = c(1L,1L)),
      makeDiscreteLearnerParam("submodel",
                               values = c("GARCH", "TGARCH",
                                          "AVGARCH", "NGARCH",
                                          "NAGARCH", "APARCH",
                                          "GJRGARCH", "ALL-GARCH"),
                               requires = quote(model == "fGARCH")),
      # FIXME: external.regressors exists in both variance.model and mean.model...
      makeUntypedLearnerParam(id = "external.regressors", default = NULL),
      makeLogicalLearnerParam("variance.targeting", default = FALSE),
      ## END: variance.model
      ## BEGIN: mean.model
      makeIntegerVectorLearnerParam("armaOrder", len = 2L, lower = 1L, default = c(1L,1L)),
      makeLogicalLearnerParam("include.mean", default = TRUE),
      makeLogicalLearnerParam("archm", default = FALSE),
      makeDiscreteLearnerParam("archpow", values = c(1L,2L), default = 1L),
      makeLogicalLearnerParam("arfima", default = FALSE),
      makeLogicalLearnerParam("archex", default = FALSE),
      ## END: variance.model
      makeDiscreteLearnerParam("distribution.model", values = c("norm","snorm",
                                                                "std","sstd",
                                                                "ged","sged",
                                                                "nig","ghyp",
                                                                "jsu"),
                               default = "norm"),
      ## END: variance.model
      # END: spec
      makeDiscreteLearnerParam("solver", values = c("nlminb", "solnp",
                                                    "lbfgs", "gosolnp",
                                                    "nloptr", "hybrid"), default = "solnp"),
      makeUntypedLearnerParam("solver.control", default = list()),
      makeUntypedLearnerParam("numderiv.control", default = list(grad.eps=1e-4, grad.d=0.0001,
                                                                 grad.zero.tol=sqrt(.Machine$double.eps/7e-7), hess.eps=1e-4, hess.d=0.1,
                                                                 hess.zero.tol=sqrt(.Machine$double.eps/7e-7), r=4, v=2)),
      # BEGIN: fit.control
      makeDiscreteLearnerParam("stationarity", values = c(0L,1L), default = 1L),
      makeDiscreteLearnerParam("fixed.se", values = c(0L,1L), default = 0L),
      makeDiscreteLearnerParam("scale", values = c(0L,1L), default = 0L),
      makeNumericLearnerParam("rec.init", lower = 1E-10, upper = Inf, special.vals = list('all')),
      # END: fit.control
      makeIntegerLearnerParam("n.ahead", lower = 1L, default = 10L, when = "predict"),
      makeIntegerLearnerParam("n.roll", lower = 0L, default = 0L, when = "predict"),
      makeUntypedLearnerParam("probs", default = c(.05, .95), when = "predict")
    ),
    properties = c("numerics","quantile"),
    name = "Generalized AutoRegressive Conditional Heteroskedasticity",
    short.name = "garch",
    note = ""
  )
}
#'@export
trainLearner.fcregr.garch = function(.learner, .task, .subset, .weights = NULL, ...) {
  dots = list(...)

  # get names of ugarchspec args
  spec.names.args = formals(rugarch::ugarchspec)
  spec.names = names(spec.names.args)
  spec.names.variance.model = names(spec.names.args[['variance.model']])
  spec.names.mean.model = names(spec.names.args[['mean.model']])
  #subset into variance.model, mean.model, spec, and all other
  variance.model.args = dots[intersect(names(dots), spec.names.variance.model)]
  mean.model.args = dots[intersect(names(dots), spec.names.mean.model)]
  spec.args = dots[intersect(names(dots), spec.names)]

  garch.spec.args = c(variance.model = list(variance.model.args),
                      mean.model = list(mean.model.args),
                      spec.args)
  garch.spec = function(...) {
    rugarch::ugarchspec(...)
  }
  spec = do.call(garch.spec,garch.spec.args)
  # get names of ugarchfit args
  garch.fit.names.args = formals(rugarch::ugarchfit)
  garch.fit.names = names(garch.fit.names.args)
  fit.ctrl.names  = names(garch.fit.names.args[["fit.control"]])
  fit.ctrl.args = dots[intersect(names(dots), fit.ctrl.names)]

  # drop everything but params for ugarchfit
  dots = dropNamed(dots, c(spec.names, spec.names.variance.model, spec.names.mean.model,
                           fit.ctrl.names))


  data = getTaskData(.task,.subset,target.extra = TRUE)
  data$target = ts(data$target, start = 1, frequency = .task$task.desc$frequency)

  garch.fit.args = c(dots, spec = list(spec), fit.control = list(fit.ctrl.args))
  garch.fun = function(...){
    rugarch::ugarchfit(data = data$target, ...)
  }
  do.call(garch.fun,garch.fit.args)
}

#'@export
predictLearner.fcregr.garch = function(.learner, .model, .newdata, ...){

  se.fit = .learner$predict.type == "quantile"
  if (!se.fit){
    garchForecast = rugarch::ugarchforecast(.model$learner.model, ...)
    p = as.numeric(garchForecast@forecast$seriesFor)
  } else {
    garchForecast = rugarch::ugarchforecast(.model$learner.model, ...)
    # FIXME: This is probably not good. Need to not care about if probs are created by user
    if (is.null(.model$learner$par.vals$probs)) .model$learner$par.vals$probs = c(.05,.95)
    garchQuantile = lapply(.model$learner$par.vals$probs, function(x) rugarch::quantile(garchForecast, x))
    pMean  = as.matrix(garchForecast@forecast$seriesFor)
    pQuantile = do.call(cbind, garchQuantile)
    #pLower = as.numeric(garchQuantile[[1]])
    #pUpper = as.numeric(garchQuantile[[2]])
    colnames(pMean)  = "point_forecast"
    #FIXME: Need to get names of quantiles
    colnames(pQuantile) = paste0("quantile",.model$learner$par.vals$probs )#,colnames(pLower))
    #colnames(pUpper) = paste0("quantile")#,colnames(pUpper))
    p = cbind(pMean,pQuantile)
  }
  return(p)
}
