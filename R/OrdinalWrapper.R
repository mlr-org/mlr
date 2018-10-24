#' @title Fuse learner with multiclass method.
#'
#' @description
#' Fuses a base classificatio or regression learner with a ordinal method.
#' Creates a learner object, which can be used like any other learner object.
#' This way learners which cannot handle ordinal tasks will be able to
#' handle those problems, too.
#'
#' For classification learners, we use multiclass-to-binary reduction principle, where multiple binary
#' problems are created from the ordinal task. How these binary problems
#' are generated is defined by an error-correcting-output-code (ECOC) code book.
#' This also allows the orderd partitions, one-vs-next, one-vs-followers and one-vs-previous  
#' approaches. Decoding is currently done via Hamming decoding, see # FIXME
#' e.g. here <http://jmlr.org/papers/volume11/escalera10a/escalera10a.pdf>.
#'
#' For regression learners we implemented a threshold based approach which seperates
#' the response space into optimal areas accrording to the ordinal target. # FIXME
#' 
#' Currently, the approach always operates on the discrete predicted labels
#' of the binary base models (instead of their probabilities) and the created
#' wrapper cannot predict posterior probabilities.
#'
#' @template arg_learner
#' @param method (`character(1)` | `function`) \cr
#'   \dQuote{orderedpartitions} or \dQuote{onevsnext} or \dQuote{onevsfollowers}
#'   or \dQuote{onevsprevious} or \dQuote{tune.threshold}.
#'   For classification learners, you can also pass a function, with signature `function(task)` and which
#'   returns a ECOC codematrix with entries +1,-1,0.
#'   Columns define new binary problems, rows correspond to ordinal levels (rows must be named).
#'   0 means level is not included in binary problem.
#'   Default for classification learners is \dQuote{orderedpartitions} and
#'   \dQuote{tune.threshold} for regression learners.
#' @template ret_learner
#' @family wrapper
#' @export
makeOrdinalWrapper = function(learner, method = "tune.threshold") {
  learner = checkLearner(learner)
  ps = makeParamSet(
    makeDiscreteLearnerParam(id = "method", values = c("tune.threshold",
     "orderedpartitions", "onevsnext", "onevsfollowers", "onevsprevious"))
  )

  type = learner$type
  if(is.null(method))
    ifelse(type == "regr", "tune.threshold", "orderedpartitions")

  assert(
    checkChoice(method, c("tune.threshold", "orderedpartitions",
      "onevsnext", "onevsfollowers", "onevsprevious")),
    checkFunction(method, args = "task")
  )
  
  pv = list(method = method)
  id = stri_paste(learner$id, "ordinal", sep = ".")

  x = makeHomogeneousEnsemble(id = id, type = "ordinal", next.learner = learner,
    package = learner$package,  par.set = ps, par.vals = pv,
    learner.subclass = "OrdinalWrapper", model.subclass = "OrdinalModel")
  x = setPredictType(x, predict.type = "response")
  return(x)
}

#' @export
trainLearner.OrdinalWrapper = function(.learner, .task, .subset = NULL, .weights = NULL, method, ...) {
  .task = subsetTask(.task, .subset)
  y = getTaskTargets(.task)
  if (method != "tune.threshold") {
    om = buildOrdMatrix(method, .task)
  x = multi.to.binary(y, om)
  args = list(x = x, learner = .learner, task = .task, weights = .weights)
  parallelLibrary("mlr", master = FALSE, level = "mlr.ensemble", show.info = FALSE)
  exportMlrOptions(level = "mlr.ensemble")
  models = parallelMap(i = seq_along(x$row.inds), doOrdinalTrainIteration,
    more.args = args, level = "mlr.ensemble")
  m = makeHomChainModel(.learner, models)
  m$om = om
  return(m)
  } else {
    #tune threhold
  }
}

doOrdinalTrainIteration = function(x, i, learner, task, weights) {
  setSlaveOptions()
  d = getTaskData(task)
  tn = getTaskTargetNames(task)
  data2 = d[x$row.inds[[i]], , drop = FALSE]
  data2[, tn] = x$targets[[i]]
  ct = changeData(task, data2)
  ct$task.desc$positive = "1"
  ct$task.desc$negative = "-1"
  train(learner$next.learner, ct, weights = weights)
}

#' @export
predictLearner.OrdinalWrapper = function(.learner, .model, .newdata, .subset = NULL, ...) {
  models = .model$learner.model$next.model
  om = .model$learner.model$om
  # predict newdata with every binary model, get n x n.models matrix of +1,-1
  # FIXME: this will break for length(models) == 1? do not use sapply!
  p = sapply(models, function(m) {
    pred = predict(m, newdata = .newdata, subset = .subset, ...)$data$response
    if (is.factor(pred))
      pred = as.numeric(pred == "1") * 2 - 1
    pred
  })
  rns = rownames(om)
  # we use hamming decoding here, see http://jmlr.org/papers/volume11/escalera10a/escalera10a.pdf
  y = apply(p, 1L, function(v) {
    d = apply(om, 1L, function(z) sum((1 - sign(v * z)) / 2))
    rns[getMinIndex(d)]
  })
  as.factor(y)
}

#' @export
getLearnerProperties.OrdinalWrapper = function(learner){
  props = getLearnerProperties(learner$next.learner)
  props = union(props, "ordinal")
  setdiff(props, "prob")
}

##############################               helpers                      ##############################

buildOrdMatrix = function(method, .task) {
  if (is.function(method)) {
    meth = method
  } else {
    meth = switch(method,
      orderedpartitions = ord.orderedpartitions,
      onevsnext = ord.onevsnext,
      onevsfollowers = ord.onevsfollowers,
      onevsprevious = ord.onevsprevious)
  }
  levs = getTaskClassLevels(.task)
  om = meth(.task)
  if (!setequal(rownames(om), levs))
    stop("Rownames of codematrix must be class levels!")
  if (!all(om == 1 | om == -1 | om == 0))
    stop("Codematrix must only contain: -1, 0, +1!")
  om
}


# function for multi-to-binary problem conversion
multi.to.binary = function(target, codematrix) {
  if (anyMissing(codematrix))
    stop("Code matrix contains missing values!")
  levs = levels(target)
  rns = rownames(codematrix)
  if (is.null(rns) || !setequal(rns, levs))
    stop("Rownames of code matrix have to be the class levels!")

  binary.targets = as.data.frame(codematrix[target, , drop = FALSE])
  row.inds = lapply(binary.targets, function(v) which(v != 0))
  names(row.inds) = NULL
  targets = Map(function(y, i) factor(y[i]), binary.targets, row.inds)
  return(list(row.inds = row.inds, targets = targets))
}

ord.orderedpartitions = function(task) {
  tcl = getTaskClassLevels(task)
  n = length(tcl)
  om = matrix(0, n, n - 1)
  om[lower.tri(om)] = 1
  setRowNames(om, tcl)
}
ord.onevsnext = function(task) {
  tcl = getTaskClassLevels(task)
  n = length(tcl)
  om = matrix(0, n, n - 1)
  delta = row(om) - col(om)
  om[delta == 1] = 1
  om[delta == 0] = -1
  setRowNames(om, tcl)
}
ord.onevsfollowers = function(task) {
  tcl = getTaskClassLevels(task)
  n = length(tcl)
  om = matrix(0, n, n - 1)
  delta = row(om) - col(om)
  om[delta >= 1] = 1
  om[delta == 0] = -1
  setRowNames(om, tcl)
}
ord.onevsprevious = function(task) {
  tcl = getTaskClassLevels(task)
  n = length(tcl)
  om = ord.onevsfollower[n:1,]
  setRowNames(om, tcl)
}
