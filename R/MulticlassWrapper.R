#' @title Fuse learner with multiclass method.
#'
#' @description
#' Fuses a base learner with a multi-class method.
#' Creates a learner object, which can be used like any other learner object.
#' This way learners which can only handle binary classification will be able to
#' handle multi-class problems, too.
#'
#' We use a multiclass-to-binary reduction principle, where multiple binary
#' problems are created from the multiclass task. How these binary problems
#' are generated is defined by an error-correcting-output-code (ECOC) code book.
#' This also allows the simple and well-known one-vs-one and one-vs-rest
#' approaches. Decoding is currently done via Hamming decoding, see
#' e.g. here <http://jmlr.org/papers/volume11/escalera10a/escalera10a.pdf>.
#'
#' Currently, the approach always operates on the discrete predicted labels
#' of the binary base models (instead of their probabilities) and the created
#' wrapper cannot predict posterior probabilities.
#'
#' @template arg_learner
#' @param mcw.method (`character(1)` | `function`) \cr
#'   \dQuote{onevsone} or \dQuote{onevsrest}.
#'   You can also pass a function, with signature `function(task)` and which
#'   returns a ECOC codematrix with entries +1,-1,0.
#'   Columns define new binary problems, rows correspond to classes (rows must be named).
#'   0 means class is not included in binary problem.
#'   Default is \dQuote{onevsrest}.
#' @template ret_learner
#' @family wrapper
#' @export
makeMulticlassWrapper = function(learner, mcw.method = "onevsrest") {

  learner = checkLearner(learner)
  ps = makeParamSet(
    makeUntypedLearnerParam(id = "mcw.method", default = "onevsrest")
  )
  assert(
    checkChoice(mcw.method, c("onevsrest", "onevsone")),
    checkFunction(mcw.method, args = "task")
  )
  pv = list(mcw.method = mcw.method)
  id = stri_paste(learner$id, "multiclass", sep = ".")

  x = makeHomogeneousEnsemble(id = id, type = "classif", next.learner = learner,
    package = learner$package, par.set = ps, par.vals = pv,
    learner.subclass = "MulticlassWrapper", model.subclass = "MulticlassModel")
  x = setPredictType(x, predict.type = "response")
  return(x)
}

#' @export
trainLearner.MulticlassWrapper = function(.learner, .task, .subset = NULL, .weights = NULL, mcw.method, ...) {

  .task = subsetTask(.task, .subset)
  y = getTaskTargets(.task)
  cm = buildCMatrix(mcw.method, .task)
  x = multi.to.binary(y, cm)
  args = list(x = x, learner = .learner, task = .task, weights = .weights)
  parallelLibrary("mlr", master = FALSE, level = "mlr.ensemble", show.info = FALSE)
  exportMlrOptions(level = "mlr.ensemble")
  models = parallelMap(i = seq_along(x$row.inds), doMulticlassTrainIteration,
    more.args = args, level = "mlr.ensemble")
  m = makeHomChainModel(.learner, models)
  m$cm = cm
  return(m)
}

doMulticlassTrainIteration = function(x, i, learner, task, weights) {

  setSlaveOptions()
  d = getTaskData(task, functionals.as = "matrix")
  tn = getTaskTargetNames(task)
  data2 = d[x$row.inds[[i]], , drop = FALSE]
  data2[, tn] = x$targets[[i]]
  ct = changeData(task, data2)
  ct$task.desc$positive = "1"
  ct$task.desc$negative = "-1"
  train(learner$next.learner, ct, weights = weights)
}

#' @export
predictLearner.MulticlassWrapper = function(.learner, .model, .newdata, .subset = NULL, ...) {

  models = .model$learner.model$next.model
  cm = .model$learner.model$cm
  # predict newdata with every binary model, get n x n.models matrix of +1,-1
  # FIXME: this will break for length(models) == 1? do not use sapply!
  p = sapply(models, function(m) {
    pred = predict(m, newdata = .newdata, subset = .subset, ...)$data$response
    if (is.factor(pred)) {
      pred = as.numeric(pred == "1") * 2 - 1
    }
    pred
  })
  rns = rownames(cm)
  # we use hamming decoding here, see http://jmlr.org/papers/volume11/escalera10a/escalera10a.pdf
  y = apply(p, 1L, function(v) {
    d = apply(cm, 1L, function(z) sum((1 - sign(v * z)) / 2))
    rns[getMinIndex(d)]
  })
  as.factor(y)
}

#' @export
getLearnerProperties.MulticlassWrapper = function(learner) {
  props = getLearnerProperties(learner$next.learner)
  props = union(props, "multiclass")
  setdiff(props, "prob")
}

##############################               helpers                      ##############################

buildCMatrix = function(mcw.method, .task) {
  if (is.function(mcw.method)) {
    meth = mcw.method
  } else {
    meth = switch(mcw.method,
      onevsrest = cm.onevsrest,
      onevsone = cm.onevsone)
  }
  levs = getTaskClassLevels(.task)
  cm = meth(.task)
  if (!setequal(rownames(cm), levs)) {
    stop("Rownames of codematrix must be class levels!")
  }
  if (!all(cm == 1 | cm == -1 | cm == 0)) {
    stop("Codematrix must only contain: -1, 0, +1!")
  }
  cm
}


# function for multi-to-binary problem conversion
multi.to.binary = function(target, codematrix) {

  if (anyMissing(codematrix)) {
    stop("Code matrix contains missing values!")
  }
  levs = levels(target)
  rns = rownames(codematrix)
  if (is.null(rns) || !setequal(rns, levs)) {
    stop("Rownames of code matrix have to be the class levels!")
  }

  binary.targets = as.data.frame(codematrix[target, , drop = FALSE])
  row.inds = lapply(binary.targets, function(v) which(v != 0))
  names(row.inds) = NULL
  targets = Map(function(y, i) factor(y[i]), binary.targets, row.inds)
  return(list(row.inds = row.inds, targets = targets))
}

cm.onevsrest = function(task) {
  tcl = getTaskClassLevels(task)
  n = length(tcl)
  cm = matrix(-1, n, n)
  diag(cm) = 1
  setRowNames(cm, tcl)
}

cm.onevsone = function(task) {
  tcl = getTaskClassLevels(task)
  n = length(tcl)
  cm = matrix(0, n, choose(n, 2))
  combs = combn(n, 2)
  for (i in seq_col(combs)) {
    j = combs[, i]
    cm[j, i] = c(1, -1)
  }
  setRowNames(cm, tcl)
}
