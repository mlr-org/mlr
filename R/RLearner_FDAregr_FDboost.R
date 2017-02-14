#' @title Functional linear array model bossting
#'
#' @description Learner for Functional linear array modeling boosting
#'
#' @export
makeRLearner.fdaregr.FDboost = function() {
  makeRLearnerRegr(
    cl = "fdaregr.FDboost",
    package = "FDboost",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "mstop", default = 200L),
      makeIntegerLearnerParam(id = "df", default = 3L, lower = 1L),
      makeIntegerLearnerParam(id = "knots", default = 40L, lower = 1L),
      makeUntypedLearnerParam(id = "timeformular", default = NULL),
      makeUntypedLearnerParam(id = "index.list", default = NULL),
      makeLogicalLearnerParam(id = "normalize", default = FALSE),  # whether to normalize column to fit the need of mboost
      makeLogicalLearnerParam(id = "check.indent", default = TRUE)  
      ),
    properties = c("numerics"),
    name = "FLAM regression",
    short.name = "FDboost"
  )
}

#' @export
trainLearner.fdaregr.FDboost = function(.learner, .task, .subset, .weights = NULL, ...) {
  d = getTaskData(.task, subset = .subset, target.extra = TRUE)
  tn = getTaskTargetNames(.task)
  df = d$data
  z = d$target
  mfuelSubset = list()
  mfuelSubset[[eval(tn)]] = d$target
  mfuelSubset$UVVIS = subset(mdata, select = fdboost.task$channel.list[[1]])
  mfuelSubset$NIR = subset(mdata, select = fdboost.task$channel.list[[2]])
  mfuelSubset$uvvis.lambda = index.list[[1]]
  mfuelSubset$nir.lambda = index.list[[2]]
  #ff1 = bsignal(UVVIS, uvvis.lambda, knots = 40, df = 4, check.ident = FALSE)
  #ff2 = bsignal(NIR, nir.lambda, knots = 40, df = 4, check.indent = FALSE)
  #formula = as.formula(paste0(eval(tn),"~", eval(f1), eval(f2)))
  #mod2f <- FDboost(formula = formula, timeformula = ~bols(1), data = fuelSubset, control = boost_control(mstop = 200))
  mod2f <- FDboost(heatan ~ bsignal(UVVIS, uvvis.lambda, knots = 40, df = 4, check.ident = FALSE)
                   + bsignal(NIR, nir.lambda, knots = 40, df = 4, check.ident = FALSE),
                   timeformula = ~bols(1), data = fuelSubset, control = boost_control(mstop = 200))  
  #FIXME: This one does not work: 
  #mod2f <- FDboost(paste0(eval(tn), "~ bsignal(UVVIS, uvvis.lambda, knots = 40, df = 4, check.ident = FALSE)+bsignal(NIR, nir.lambda, knots = 40, df = 4, check.ident = FALSE)"), timeformula = ~bols(1), data = fuelSubset, control = boost_control(mstop = 200))
  return(mod2f)
}

#' @export
predictLearner.fdaregr.FDboost = function(.learner, .model, .newdata, ...) {
  pred = predict(object = .model$learner.model, newdata = .newdata)
  return(pred)
}


