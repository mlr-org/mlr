#' @export
makeRLearner.classif.gam = function() {
  makeRLearnerClassif(
    cl = "classif.gam",
    package = "mgcv",
    par.set = makeParamSet(
      makeUntypedLearnerParam(id = "formula", default = NULL),
      makeUntypedLearnerParam(id = "sp", default = NULL),
      makeUntypedLearnerParam(id = "in.out", default = NULL),
      makeUntypedLearnerParam(id = "paraPen", default = NULL),
      makeDiscreteLearnerParam(id = "method", default = "GCV.Cp",
        values = c("GACV.Cp", "GCV.Cp", "REML", "P-REML", "ML", "P-ML")),
      makeDiscreteLearnerParam(id = "optimizer", default = c("outer", "newton"),
        values = list(newton = c("outer", "newton"), perf = "perf", efs = "efs",
        bfgs = c("outer", "bfgs"), optim = c("outer", "optim"),
        nlm = c("outer", "nlm"), nlm.fd = c("outer", "nlm.fd"))),
      makeDiscreteLearnerParam(id = "family", default = "binomial",
        values = c("binomial", "quasibinomial", "negbin")),
      makeDiscreteLearnerParam(id = "binomial.link", default = "logit",
        values = c("logit", "probit", "cauchit", "log", "cloglog"), requires = quote(family == "binomial")),
      makeDiscreteLearnerParam(id = "quasibinomial.link", default = "logit",
        values = c("logit", "probit", "identity", "inverse", "log", "1/mu^2", "sqrt"), requires = quote(family == "quasibinomial")),
      makeDiscreteLearnerParam(id = "nb.link", default = "log",
        values = c("log", "identity", "sqrt"), requires = quote(family == "negbin")),
      makeIntegerLearnerParam(id = "theta", requires = quote(family == "negbin")),
      makeNumericLearnerParam(id = "scale", default = 0),
      makeLogicalLearnerParam(id = "select", default = FALSE),
      makeUntypedLearnerParam(id = "knots", default = NULL),
      makeNumericLearnerParam(id = "H"),
      makeNumericLearnerParam(id = "gamma", default = 1, lower = 0),
      makeLogicalLearnerParam(id = "fit", default = TRUE),
      makeLogicalLearnerParam(id = "drop.unused.levels", default = TRUE),
      makeLogicalLearnerParam(id = "drop.intercept", default = FALSE)
    ),
    properties = c("twoclass", "numerics", "factors", "prob", "weights"),
    name = "Generalized Additive Models for Classification",
    short.name = "gam",
    note = "Uses mgcv::gam for classification using GAMs. Please specify smooth terms using the formula argument. If missing, all predictors are assumed to be linear."
  )
}

#' @export
trainLearner.classif.gam = function(.learner, .task, .subset, .weights = NULL, formula = NULL, family = "binomial",
  binomial.link = "logit", quasibinomial.link = "logit", nb.link = "log", K = 1, in.out = NULL,
  theta = NULL, p = 1, optimizer = c("outer", "newton"), sp = NULL, paraPen = NULL,
  method = "GCV.Cp", scale = 0, select = FALSE, knots = NULL, H = NULL, gamma = 1, drop.unused.levels = TRUE,
  drop.intercept = FALSE, ...) {

  ctrl = learnerArgsToControl(mgcv::gam.control)
  if (is.null(formula)) {
    f = getTaskFormula(.task, explicit.features = TRUE)
  } else {
    f = BBmisc::asQuoted(formula)
  }
  family = switch(family,
    binomial = stats::binomial(link = make.link(binomial.link)),
    quasibinomial = stats::quasibinomial(link = make.link(quasibinomial.link)),
    negbin = mgcv::nb(theta = theta, link = make.link(nb.link))
  )
  if (is.null(.weights)) {
    mgcv::gam(formula = f, data = getTaskData(.task, .subset), control = ctrl, family = family,
      optimizer = optimizer, method = method, knots = knots, H = H, gamma = gamma,
      drop.unused.levels = drop.unused.levels, drop.intercept = drop.intercept,
      in.out = in.out, sp = sp, paraPen = paraPen, ...)
  } else {
    mgcv::gam(formula = f, data = getTaskData(.task, .subset), weights = .weights, control = ctrl, family = family,
      optimizer = optimizer, method = method, knots = knots, H = H, gamma = gamma,
      drop.unused.levels = drop.unused.levels, drop.intercept = drop.intercept,
      in.out = in.out, sp = sp, paraPen = paraPen, ...)
  }
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
