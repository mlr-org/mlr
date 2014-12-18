#' @title Fuse learner with preprocessing
#'
#' @description
#' Fuses a learner with  preprocessing methods proviced by \code{\link[caret]{preProcess}}.
#  Before training the preprocessing will be performed and the preprocessing model will be saved.
#  Before prediction the preprocessing model will transform the test data according to the trained model.
#'
#' @template arg_learner
#' @param ppc.BoxCox [\code{logical(1)}]\cr
#' @param ppc.YeoJohnson [\code{logical(1)}]\cr
#' @param ppc.expoTrans [\code{logical(1)}]\cr
#' @param ppc.center [\code{logical(1)}]\cr
#' @param ppc.scale [\code{logical(1)}]\cr
#' @param ppc.range [\code{logical(1)}]\cr
#' @param ppc.knnImpute [\code{logical(1)}]\cr
#' @param ppc.bagImpute [\code{logical(1)}]\cr
#' @param ppc.medianImpute [\code{logical(1)}]\cr
#' @param ppc.pca [\code{logical(1)}]\cr
#' @param ppc.ica [\code{logical(1)}]\cr
#' @param ppc.spatialSign [\code{logical(1)}]\cr
#' @param ppc.thresh [\code{numeric(1)}]\cr
#' @param ppc.na.remove [\code{logical(1)}]\cr
#' @param ppc.k [\code{numeric(1)}]\cr
#' @param ppc.fudge [\code{numeric(1)}]\cr
#' @param ppc.numUnique [\code{numeric(1)}]\cr
#' @param ppc.pcaComp [\code{number(1)}]\cr
#' @param ppc.knnSummary [\code{function}]\cr
#' @param ppc.method [\code{character}]\cr
#'   This is the method character vector used as in \code{caret::preProcess}.
#'   Using this will override the logical set methods from above.
#'   Default is \code{NULL}.
#' @param add.par.set [\code{\link[ParamHelpers]{ParamSet}}] \cr
#'   Parameter set of (hyper)parameters and their constraints.
#' @param ... \cr
#'   see \code{\link[caret]{preProcess}} for parameters not listed above.
#'   If you use them you might want to define them in the \code{add.par.set} so that they can be tuned.
#' @template ret_learner
#' @family wrapper
#' @export
makePreprocWrapperCaret = function (learner, 
  ppc.BoxCox = FALSE, ppc.YeoJohnson = FALSE, ppc.expoTrans = FALSE, ppc.center = TRUE, ppc.scale = TRUE,
  ppc.range = FALSE, ppc.knnImpute = FALSE, ppc.bagImpute = FALSE, ppc.medianImpute = FALSE, ppc.pca = FALSE,
  ppc.ica = FALSE, ppc.spatialSign = FALSE, ppc.thresh = 0.94, ppc.pcaComp = NULL, ppc.na.remove = TRUE, 
  ppc.k = 5L, ppc.fudge = 0.2, ppc.numUnique = 3L, ppc.method = NULL,
  add.par.set = NULL, ...) {

  all.methods = c("BoxCox", "YeoJohnson", "expoTrans", "center", "scale", "range", "knnImpute", "bagImpute", "medianImpute", "pca", "ica", "spatialSign")

  if (!is.null(ppc.method)) {
    assertSubset(ppc.method, all.methods)
  } else {
    methods = list(
      BoxCox = ppc.BoxCox,
      YeoJohnson = ppc.YeoJohnson, 
      expoTrans = ppc.expoTrans, 
      center = ppc.center, 
      scale = ppc.scale,
      range = ppc.range, 
      knnImpute = ppc.knnImpute, 
      bagImpute = ppc.bagImpute, 
      medianImpute = ppc.medianImpute, 
      pca = ppc.pca,
      ica = ppc.ica, 
      spatialSign = ppc.spatialSign
    )
    assertLogical(unlist(methods), any.missing = FALSE)
    ppc.method = logList2charVec(methods)
  }

  if (length(ppc.method)) {
    return(learner) #FIXME can we do it like that?
  }
  
  trainfun = function(data, target, args) {
    requirePackages("caret", "train PreprocWrapperCaret")
    cns = colnames(data)
    work.cols = setdiff(cns, target) 
    x = data[, work.cols, drop = FALSE]
    mod = do.call(caret::preProcess, c(list(x = x), args))
    x = cbind.data.frame(predict(mod, data[, work.cols, drop = FALSE]), data[, setdiff(cns, work.cols), drop = FALSE])
    list(data = x, control = mod)
  }
  
  predictfun = function(data, target, args, control) {
    requirePackages("caret", "predict PreprocWrapperCaret")
    data.frame(predict(control, data))
  }

  par.set = makeParamSet(
    makeLogicalLearnerParam("ppc.BoxCox", default = FALSE),
    makeLogicalLearnerParam("ppc.YeoJohnson", default = FALSE),
    makeLogicalLearnerParam("ppc.expoTrans", default = FALSE),
    makeLogicalLearnerParam("ppc.center", default = FALSE),
    makeLogicalLearnerParam("ppc.scale", default = FALSE),
    makeLogicalLearnerParam("ppc.range", default = FALSE),
    makeLogicalLearnerParam("ppc.knnImpute", default = FALSE),
    makeLogicalLearnerParam("ppc.bagImpute", default = FALSE),
    makeLogicalLearnerParam("ppc.medianImpute", default = FALSE),
    makeLogicalLearnerParam("ppc.pca", default = FALSE),
    makeLogicalLearnerParam("ppc.ica", default = FALSE),
    makeLogicalLearnerParam("ppc.spatialSign", default = FALSE),
    makeNumericLearnerParam("ppc.thresh", lower = 0, default = 0.94),
    makeIntegerLearnerParam("ppc.pcaComp", lower = 1L), #FIXME: Default = NULL
    makeLogicalLearnerParam("ppc.na.remove", default = TRUE),
    makeIntegerLearnerParam("ppc.k", lower = 1L, default = 5L),
    # knnSummary function #FIXME is function parameter
    # outcome #FIXME do we need that?
    makeNumericLearnerParam("ppc.fudge", default = 0.2, lower = 0),
    makeIntegerLearnerParam("ppc.numUnique", default = 3L, lower = 1L)
  )

  args = list(
    method = ppc.method,
    thresh = ppc.thresh, 
    pcaComp = ppc.pcaComp,
    na.remove = ppc.na.remove, 
    k = ppc.k,
    fudge = ppc.fudge, 
    numUnique = ppc.numUnique
  )
  
  add.args = list(...)

  if(names(add.args) %in% args)
    stopf("Overwrite wrapper parameters in ... is not allowed.")

  args = c(args, add.args)

  makePreprocWrapper(learner, trainfun, predictfun, par.set, par.vals = args)
}

# HELPERS
logList2charVec = function(list) {
  log.vec = unlist(list)
  res = names(list)[log.vec]
}

# charVec2logList = function(vec, ref = vec) {
#   list = as.list(rep(FALSE, length(ref)))
#   names(list) = ref
#   list[vec] = TRUE
#   list
# }