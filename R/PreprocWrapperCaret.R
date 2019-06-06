#' @title Fuse learner with preprocessing.
#'
#' @description
#' Fuses a learner with preprocessing methods provided by [caret::preProcess].
#'  Before training the preprocessing will be performed and the preprocessing model will be stored.
#'  Before prediction the preprocessing model will transform the test data according to the trained model.
#'
#'  After being wrapped the learner will support missing values although this will only be the case if `ppc.knnImpute`, `ppc.bagImpute` or `ppc.medianImpute` is set to `TRUE`.
#'
#' @template arg_learner
#' @param ... (any)\cr
#'   See [caret::preProcess] for parameters not listed above.
#'   If you use them you might want to define them in the `add.par.set` so that they can be tuned.
#' @template ret_learner
#' @family wrapper
#' @export
makePreprocWrapperCaret = function(learner, ...) {

  par.set = makeParamSet(
    makeLogicalLearnerParam("ppc.BoxCox", default = FALSE),
    makeLogicalLearnerParam("ppc.YeoJohnson", default = FALSE),
    makeLogicalLearnerParam("ppc.expoTrans", default = FALSE),
    makeLogicalLearnerParam("ppc.center", default = TRUE),
    makeLogicalLearnerParam("ppc.scale", default = TRUE),
    makeLogicalLearnerParam("ppc.range", default = FALSE),
    makeLogicalLearnerParam("ppc.knnImpute", default = FALSE),
    makeLogicalLearnerParam("ppc.bagImpute", default = FALSE),
    makeLogicalLearnerParam("ppc.medianImpute", default = FALSE),
    makeLogicalLearnerParam("ppc.pca", default = FALSE),
    makeLogicalLearnerParam("ppc.ica", default = FALSE),
    makeLogicalLearnerParam("ppc.spatialSign", default = FALSE),
    makeLogicalLearnerParam("ppc.corr", default = FALSE),
    makeLogicalLearnerParam("ppc.zv", default = FALSE),
    makeLogicalLearnerParam("ppc.nzv", default = FALSE),
    makeNumericLearnerParam("ppc.thresh", lower = 0, default = 0.95),
    # FIXME: pcaComp has data dep. default, number of features
    makeIntegerLearnerParam("ppc.pcaComp", lower = 1L),
    makeLogicalLearnerParam("ppc.na.remove", default = TRUE),
    makeIntegerLearnerParam("ppc.k", lower = 1L, default = 5L),
    # knnSummary function #FIXME is function parameter
    # outcome #FIXME do we need that?
    makeNumericLearnerParam("ppc.fudge", default = 0.2, lower = 0),
    makeIntegerLearnerParam("ppc.numUnique", default = 3L, lower = 1L),
    makeIntegerLearnerParam("ppc.n.comp", lower = 1L),
    makeNumericLearnerParam("ppc.cutoff", default = 0.9, lower = 0, upper = 1),
    makeNumericLearnerParam("ppc.freqCut", default = 95 / 5, lower = 1),
    makeNumericLearnerParam("ppc.uniqueCut", default = 10, lower = 0)
  )
  par.vals = getDefaults(par.set)
  par.vals = insert(par.vals, list(...))

  trainfun = function(data, target, args) {

    all.methods = c(
      "BoxCox", "YeoJohnson", "expoTrans", "center",
      "scale", "range", "knnImpute", "bagImpute",
      "medianImpute", "pca", "ica", "spatialSign",
      "zv", "nzv", "corr"
    )
    logindex = c(
      args$ppc.BoxCox, args$ppc.YeoJohnson, args$ppc.expoTrans, args$ppc.center,
      args$ppc.scale, args$ppc.range, args$ppc.knnImpute, args$ppc.bagImpute,
      args$ppc.medianImpute, args$ppc.pca, args$ppc.ica, args$ppc.spatialSign,
      args$ppc.zv, args$ppc.nzv, args$ppc.corr
    )

    cargs = list(
      method = all.methods[logindex],
      thresh = args$ppc.thresh,
      pcaComp = args$ppc.pcaComp,
      na.remove = args$ppc.na.remove,
      k = args$ppc.k,
      fudge = args$ppc.fudge,
      numUnique = args$ppc.numUnique,
      n.comp = args$ppc.n.comp,
      cutoff = args$ppc.cutoff,
      freqCut = args$ppc.freqCut,
      uniqueCut = args$ppc.uniqueCut
    )

    cns = colnames(data)
    work.cols = setdiff(cns, target)
    x = data[, work.cols, drop = FALSE]
    mod = do.call(caret::preProcess, c(list(x = x), cargs))
    x = cbind.data.frame(predict(mod, data[, work.cols, drop = FALSE]),
      data[, setdiff(cns, work.cols), drop = FALSE])
    list(data = x, control = mod)
  }

  predictfun = function(data, target, args, control) {
    data.frame(predict(control, data))
  }

  x = makePreprocWrapper(learner, trainfun, predictfun, par.set, par.vals)
  addClasses(x, "PreprocWrapperCaret")
}

#' @export
getLearnerProperties.PreprocWrapperCaret = function(learner) {
  props = getLearnerProperties(learner$next.learner)
  par.vals = getHyperPars(learner)
  if (par.vals$ppc.bagImpute | par.vals$ppc.knnImpute | par.vals$ppc.medianImpute) {
    props = union(props, "missings")
  }
  props
}
