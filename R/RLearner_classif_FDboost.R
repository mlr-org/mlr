#' @export
makeRLearner.classif.FDboost = function() {
  makeRLearnerClassif(
    cl = "classif.FDboost",
    package = c("FDboost", "mboost"),
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "family", default = "Binomial", values = c("AdaExp", "Binomial", "AUC", "custom.family")),
      makeIntegerLearnerParam(id = "mstop", default = 100L, lower = 1L),
      makeNumericLearnerParam(id = "nu", default = 0.1, lower = 0, upper = 1),  # the learning rate
      makeUntypedLearnerParam(id = "custom.family.definition", requires = quote(family == "custom.family")),  # list of parameters for the custom family
      # makeDiscreteLearnerParam(id = "risk", values = c("inbag", "oobag", "none")), we don't need this in FDboost
      makeDiscreteLearnerParam(id = "Binomial.link", default = "logit",
        values = c("logit", "probit"), requires = quote(family == "Binomial")),
      makeNumericLearnerParam(id = "df", default = 4, lower = 0.5),  # effective degrees of freedom, depend on the regularization parameter of the penality matrix and number of splines, must be the same for all base learners(covariates), the maximum value is the rank of the design matrix
      # makeDiscreteLearnerParam(id = "baselearner", values = c("bbs", "bols")),  # we don't use "btree" in FDboost
      makeIntegerLearnerParam(id = "knots", default = 10L, lower = 1L),  # determine the number of knots of splines, does not matter once there is sufficient number of knots, 30,40, 50 for example
      makeIntegerLearnerParam(id = "degree", default = 3L, lower = 1L),  # degree of the b-spline
      makeIntegerLearnerParam(id = "differences", default = 1L, lower = 1L),  # degree of the penalty
      makeLogicalLearnerParam(id = "bsignal.check.ident", default = FALSE, tunable = FALSE)  # identifiability check by testing matrix degeneracy
    ),
    par.vals = list(family = "Binomial"),
    properties = c("functionals", "numerics", "twoclass"),
    name = "Functional linear array classification boosting",
    short.name = "FDboost",
    note = "Only allow one base learner for functional covariate and one base learner for scalar covariate, the parameters for these base learners are the same. Also we currently do not support interaction between scalar covariates"
  )
}

#' @export
trainLearner.classif.FDboost = function(.learner, .task, .subset, .weights = NULL, mstop = 100L,
  knots = 10L, df = 4L, bsignal.check.ident = FALSE, degree = 3L, differences = 1L, Binomial.link = "logit",
  nu = 0.1, family = "Gaussian", custom.family.definition = NULL, nuirange = c(0, 100), d = NULL, ...) {

  family = switch(family,
    Binomial = mboost::Binomial(link = Binomial.link),
    AdaExp = mboost::AdaExp(),
    AUC = mboost::AUC(),
    #PropOdds = mboost::PropOdds(nuirange = nuirange, offrange = offrange),
    custom.family = custom.family.definition)

  ctrl = learnerArgsToControl(mboost::boost_control, mstop, nu)

  suppressMessages({d = getTaskData(.task, functionals.as = "dfcols")})
  m = getTaskData(.task, functionals.as = "matrix")
  tn = getTaskTargetNames(.task)

  formula.terms = namedList()
  mat.list = namedList(getTaskFeatureNames(.task))

  # Treat functional covariates
  if (hasFunctionalFeatures(m)) {
    fdns = colnames(getFunctionalFeatures(m))
    # later on, the grid elements in mat.list should have suffix ".grid"
    fdg = namedList(fdns)
    fd.grids = lapply(fdns, function(name) seq_len(ncol(m[, name])))
    names(fd.grids) = fdns
    fdg = setNames(fd.grids, stri_paste(fdns, ".grid"))
    # setup mat.list: for each func covar we add its data matrix and its grid. and once the target col
    # also setup charvec of formula terms for func covars
    mat.list = namedList(fdns)
    #formula.terms = setNames(character(length = fdns))
    formula.terms = namedList(fdns)
    # for each functional covariate
    for (fdn in fdns) {
      # ... create a corresponding grid name
      gn = stri_paste(fdn, ".grid")
      # ... extract the corresponding original data into a list of matrices
      mat.list[[fdn]] = m[, fdn]
      # ... create a formula item
      formula.terms[fdn] = sprintf("bsignal(%s, %s, knots = %i, df = %f, degree = %i, differences = %i, check.ident = %s)",
        fdn, gn, knots, df, degree, differences, bsignal.check.ident)
    }
    # add grid names
    mat.list = c(mat.list, fdg)
  } else {
    fdns = NULL
  }

  # Add formula to each scalar covariate, if there is no scalar covariate, this fd.scalars will be empty
  for (fsn in setdiff(colnames(m), c(fdns, tn))) {
    mat.list[[fsn]] = as.vector(as.matrix(d[, fsn, drop = FALSE]))
    formula.terms[fsn] = sprintf("bbs(%s, knots = %i, df = %f, degree = %i, differences = %i)",
      fsn, knots, df, degree, differences)
  }


  # add target names
  mat.list[[tn]] = d[, tn]

  # Create the formula and train the model
  form = as.formula(sprintf("%s ~ %s", tn, collapse(unlist(formula.terms), "+")))
  FDboost::FDboost(formula = form, timeformula = ~bols(1), data = mat.list, control = ctrl, family = family)
}

#' @export
predictLearner.classif.FDboost = function(.learner, .model, .newdata, ...) {
  assert(hasFunctionalFeatures(.newdata))
  type = ifelse(.learner$predict.type == "response", "class", "response")  # additional parameters passed to mboost::predict(), in mboost, "response" returns probabilities and "class" returns the predicted class
  p = predict(.model$learner.model, newdata = as.list(.newdata), type = type, ...)
  if (.learner$predict.type  == "prob") {
    if (!is.matrix(p) && is.na(p)){
      stopf("The selected family %s does not support probabilities", getHyperPars(.learner)$family)
    } else {
      td = .model$task.desc
      # one observation prediction + family PropOdds returns a numeric vector instead of matrix
      # FIXME: add/change the outcommented line below to enable predicting one obs
      # (caution: check whether the right class is assigned)
      # if (nrow(.newdata) == 1 && is.vector(p)) dim(p) = c(1,2)
      p = p[, 1L]
      levs = c(td$negative, td$positive)
      return(propVectorToMatrix(p, levs))
    }
  } else {
    return(p)
  }
}
