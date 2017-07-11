#' @export
makeRLearner.classif.gam = function() {
  makeRLearnerClassif(
    cl = "classif.gam",
    package = "mgcv",
    par.set = makeParamSet(
      # method, optimizer, select
      makeDiscreteLearnerParam(id = "method", default = "GCV.Cp",
                               values = c("GACV.Cp", "GCV.Cp", "REML", "P-REML", "ML", "P-ML")),
      # wrong! check how to supply vectors as parameters
      makeDiscreteLearnerParam(id = "optimizer", default = c("outer", "newton"),
                               values = list(newton = c("outer", "newton"), perf = "perf", efs = "efs",
                                             bfgs = c("outer", "bfgs"), optim = c("outer", "optim"),
                                             nlm = c("outer", "nlm"), nlm.fd = c("outer", "nlm.fd"))),
      makeDiscreteLearnerParam(id = "family", default = "gaussian",
                               values = c("binomial", "gaussian", "binomial", "poisson", "Gamma",
                                          "inverse.gaussian", "quasi", "quasibinomial", "quasipoisson",
                                          "Tweedie", "negbin", "ocat", "tw", "nb", "betar", "scat", "ziP",
                                          "cox.ph", "gaulss", "gevlss", "ziplss", "mvn",
                                          "multinom")),
      makeDiscreteLearnerParam(id = "link", default = "logit",
                               values = c("logit", "probit", "identity", "inverse", "1/mu^2", "log",
                                          "power(0)", "list = 'identity', 'logb')", "list('identity','identity','logit'')")),
      makeNumericLearnerParam(id = "gamma", default = 1, lower = 0),
      makeNumericLearnerParam(id = "scale", default = 0),
      makeLogicalLearnerParam(id = "select", default = FALSE),
      makeNumericVectorLearnerParam(id = "knots"),
      makeNumericVectorLearnerParam(id = "sp"),
      makeNumericLearnerParam(id = "min.sp"),
      makeNumericLearnerParam(id = "H"),
      makeNumericLearnerParam(id = "in.out"),
      makeLogicalLearnerParam(id = "fit", default = TRUE),
      # "optional list specifying any penalties to be applied to parametric model terms. gam.models explains more."
      # how to specify such a Param?
      # makeDiscreteLearnerParam(id = "paraPen", default = NULL, values = list()),
      makeLogicalLearnerParam(id = "drop.unused.levels", default = TRUE),
      makeLogicalLearnerParam(id = "drop.intercept", default = FALSE)
    ),
    par.vals = list(
      family = "gaussian",
      model = FALSE
    ),
    properties = c("twoclass", "numerics", "factors", "prob", "weights"),
    name = "Generalized Additive Models for Classification",
    short.name = "gam",
    note = "Uses mgcv::gam for classification using GAMs"
  )
}

#' @export
trainLearner.classif.gam = function(.learner, .task, .subset, .weights = NULL, family = "gaussian", link = "logit", K = 1,
                                    theta = stop("'theta' must be specified"), p = 1, optimizer = optimizer,
                                    method = method, ...) {

  ctrl = learnerArgsToControl(mgcv::gam.control)
  f = getTaskFormula(.task, explicit.features = TRUE)
  family = switch(family,
                  binomial = stats::bionomial(link = link),
                  multinom = mgcv::multinom(K = K),
                  negbin = mgcv::negbin(theta = theta, link = link),
                  Tweedie = mgcv::Tweedie(p = p, link = link)
  )
  mgcv::gam(f, data = getTaskData(.task, .subset), control = ctrl, family = family,
            optimizer = optimizer, method = method, ...)
}

#' @export
predictLearner.classif.gam = function(.learner, .model, .newdata, ...) {
  x = predict(.model$learner.model, newdata = .newdata, type = "response", ...)
  levs = .model$task.desc$class.levels
  if (.learner$predict.type == "prob") {
    propVectorToMatrix(x, levs)
  } else {
    levs = .model$task.desc$class.levels
    p = as.factor(ifelse(x > 0.5, levs[2L], levs[1L]))
    unname(p)
  }
}
