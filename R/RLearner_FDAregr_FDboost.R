#' @title Functional linear array model boosting.
#'
#' @description
#' Learner for Functional linear array modele boosting.
#'
#' @export
makeRLearner.fdaregr.FDboost = function() {
  makeRLearnerRegr(
    cl = "fdaregr.FDboost",
    package = c("FDboost", "mboost"),
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "family", default = "Gaussian", values = c("Gaussian", "Laplace",
        "Huber", "Poisson", "GammaReg", "NBinomial", "Hurdle", "custom.family")),
      makeIntegerLearnerParam(id = "mstop", default = 100L, lower = 1L),
      makeNumericLearnerParam(id = "nu", default = 0.1, lower = 0, upper = 1),
      makeUntypedLearnerParam(id = "custom.family.definition", requires = quote(family == "custom.family")),
      makeNumericVectorLearnerParam(id = "nuirange", default = c(0,100), requires = quote(family %in% c("GammaReg", "NBinomial", "Hurdle"))),
      makeNumericLearnerParam(id = "d", default = NULL, requires = quote(family == "Huber"), special.vals = list(NULL)), # delta parameter for Huber distribution
      # makeDiscreteLearnerParam(id = "risk", values = c("inbag", "oobag", "none")), we don't need this in FDboost
      makeNumericLearnerParam(id = "df", default = 4, lower = 0.5),  # effective degrees of freedom, depend on the regularization parameter of the penality matrix and number of splines, must be the same for all base learners(covariates)
      # makeDiscreteLearnerParam(id = "baselearner", values = c("bbs", "bols")),  # we don't use "btree" in FDboost
      makeIntegerLearnerParam(id = "bsignal.knots", default = 10L, lower = 1L),  # determine the number of splines
      makeIntegerLearnerParam(id = "bsignal.degree", default = 3L, lower = 1L),  # degree of the b spline
      makeIntegerLearnerParam(id = "bsignal.differences", default = 1L, lower = 1L),  # degree of the penalty
      makeLogicalLearnerParam(id = "bsignal.check.ident", default = FALSE, tunable = FALSE)  # identifiability check by testing matrix degeneracy
      ),
    properties = c("numerics"),
    name = "FLAM regression",
    short.name = "FDboost"
  )
}

#' @export
trainLearner.fdaregr.FDboost = function(.learner, .task, .subset, .weights = NULL, mstop = 100L, 
  bsignal.knots = 10L, df = 4L, bsignal.check.ident = FALSE, bsignal.degree = 3L, bsignal.differences = 1L, 
  nu = 0.1, family = "Gaussian", custom.family.definition = NULL, nuirange = c(0,100), d = NULL, ...) {
  family = switch(family,
    Gaussian = mboost::Gaussian(),
    Laplace = mboost::Laplace(),
    Huber = mboost::Huber(d),
    Poisson = mboost::Poisson(),
    GammaReg = mboost::GammaReg(nuirange = nuirange),
    NBinomial = mboost::NBinomial(nuirange = nuirange),
    Hurdle = mboost::Hurdle(nuirange = nuirange),
    custom.family = custom.family.definition
  )
  ctrl = learnerArgsToControl(mboost::boost_control, mstop, nu)
  d = getTaskData(.task, subset = .subset)
  tn = getTaskTargetNames(.task)
  tdesc = getTaskDescription(.task)
  fdf = tdesc$fd.features

  # later on, the grid elements in mat.list should have suffix ".grid"
  fdg = setNames(tdesc$fd.grids, paste0(names(tdesc$fd.grids), ".grid"))
  fdns = names(fdf)
  # setup mat.list: for each func covar we add its data matrix and its grid. and once the target col
  # also setup charvec of formula terms for func covars
  mat.list = namedList(fdns)
  formula.terms = c()
  for (fdn in fdns) {
    gn = paste0(fdn, ".grid")
    mat.list[[fdn]] = as.matrix(d[, tdesc$fd.features[[fdn]], drop = FALSE])
    formula.terms[fdn] = sprintf("bsignal(%s, %s, knots = %i, df = %f, degree = %i, differences = %i, check.ident = %s)",
      fdn, gn, bsignal.knots, df, bsignal.degree, bsignal.differences, bsignal.check.ident)
  }
  mat.list = c(mat.list, fdg)
  mat.list[[tn]] = d[, tn]
  form = as.formula(sprintf("%s ~ %s", tn, collapse(formula.terms, "+")))
  FDboost::FDboost(formula = form, timeformula = ~bols(1), data = mat.list, 
    control = ctrl, family = family)
}

reformat2mat.list = function(data, tdesc) {
  tn = tdesc$target
  fdns = names(tdesc$fd.features)
  mat.list = namedList(fdns)
  for (fdn in fdns) {
    mat.list[[fdn]] = as.matrix(subset(data, select = tdesc$fd.features[[fdn]]))
    mat.list[[stri_paste(fdn, ".index")]] = tdesc$fd.grids[[fdn]]
  }
  return(mat.list)
}

#' @export
predictLearner.fdaregr.FDboost = function(.learner, .model, .newdata, ...) {
  mextra_para = list(...)
  tdesc = getTaskDescription(.model)
  mat.list = reformat2mat.list(.newdata, tdesc)
  pred = predict(object = .model$learner.model, newdata = mat.list)
  return(pred)
}
