# Switch to train \code{StackedLearner}.
# @param .learner StackedLearner.
# @param .task Task.
# @param .subset Subset of Task.
# @param ... ...\cr Further arguments passed on to the learner.
#' @export
trainLearner.StackedLearner = function(.learner, .task, .subset, ...) {
  .task = subsetTask(.task, subset = .subset)
  switch(.learner$method,
    aggregate = aggregateBaseLearners(.learner, .task),
    superlearner = superlearnerBaseLearners(.learner, .task),
    # ensembleselection = ensembleselectionBaseLearners(.learner, .task, ...)
    ensembleselection = do.call(ensembleselectionBaseLearners, c(list(.learner, .task), .learner$es.par.vals))
  )
}

################################################################
# Aggregation method
################################################################

# Train function for simple aggregation of base learner predictions without weights.
#
# @param learner ([`StackedLearner`]).
# @template arg_task
aggregateBaseLearners = function(learner, task) {
  id = learner$id
  save.on.disc = learner$save.on.disc
  save.preds = learner$save.preds
  bls = learner$base.learners
  bls.names = names(bls)

  # parallelMap: train, predict
  parallelLibrary("mlr", master = FALSE, level = "mlr.stackedLearner", show.info = FALSE)
  exportMlrOptions(level = "mlr.stackedLearner")
  show.info = getMlrOption("show.info")
  results = parallelMap(doTrainPredict, bls, more.args = list(task, show.info, id, save.on.disc),
    impute.error = function(x) x, level = "mlr.stackedLearner")

  base.models = lapply(results, function(x) x[["base.models"]])
  pred.list = lapply(results, function(x) x[["pred"]])
  names(base.models) = bls.names
  names(pred.list) = bls.names

  ##FIXME: Ok way to remove bls1?
  #broke.idx.bm = which(unlist(lapply(base.models, function(x) any(class(x) == "FailureModel"))))
  ##broke.idx.pd1 = which(unlist(lapply(pred.data, function(x) anyNA(x)))) # if models is FailesModels and NAs are returend in a Prediction
  ##broke.idx.pd2 = which(unlist(lapply(pred.data, function(x) class(x) %nin% c("numeric", "factor", "data.frame")))) # if model fails and error message is returned it is not class numeric (regr, binary classif) nor data.frame (multiclassif)
  ##broke.idx = unique(c(broke.idx.pd1, broke.idx.pd2))
  #broke.idx = broke.idx.bm
  #
  #if (length(broke.idx) > 0) {
  #  messagef("Base Learner %s is broken and will be removed\n", bls.names[broke.idx])
  #  base.models = base.models[-broke.idx]
  #  pred.list = pred.list[-broke.idx]
  #  #pred.data = pred.data[-broke.idx]
  #}

  # return
  if (save.preds == FALSE) pred.list = NULL
  list(method = "aggregate", base.models = base.models, super.model = NULL,
    pred.train = pred.list)
}




################################################################
# Supervised method as combiner based on cross validated predictions as level 1 data (short: super learner)
################################################################

# Train function for stacking method "Super Learner", which uses meta learner to obtain level 1 data and uses inner cross-validation for prediction.
#
# @param learner ([`StackedLearner`]).
# @template arg_task

superlearnerBaseLearners = function(learner, task) {
  # setup
  td = getTaskDescription(task)
  type = getPreciseTaskType(td)
  bls = learner$base.learners
  bls.names = names(bls)
  bpt = unique(extractSubList(bls, "predict.type"))
  use.feat = learner$use.feat
  id = learner$id
  save.on.disc = learner$save.on.disc

  # resampling, training (parallelMap)
  rin = makeResampleInstance(learner$resampling, task = task)
  parallelLibrary("mlr", master = FALSE, level = "mlr.stackedLearner", show.info = FALSE)
  exportMlrOptions(level = "mlr.stackedLearner")
  show.info = getMlrOption("show.info")
  results = parallelMap(doTrainResample, bls, more.args = list(task, rin,
    measures = getDefaultMeasure(task), show.info, id, save.on.disc),
    impute.error = function(x) x, level = "mlr.stackedLearner")

  base.models = lapply(results, function(x) x[["base.models"]])
  pred.list = lapply(results, function(x) x[["resres"]])
  pred.data = lapply(pred.list, function(x) getPredictionDataNonMulticoll(x))

  names(base.models) = bls.names
  names(pred.list) = bls.names
  names(pred.data) = bls.names

  ## Remove broken models/predictions
  ##broke.idx.bm = which(unlist(lapply(base.models, function(x) any(class(x) == "FailureModel"))))
  #broke.idx.pd1 = which(unlist(lapply(pred.data, function(x) anyNA(x))))
  #broke.idx.pd2 = which(unlist(lapply(pred.data, function(x) class(x) %nin% c("numeric", "factor", "data.frame"))))
  ##broke.idx = unique(broke.idx.bm, broke.idx.pd)
  #broke.idx = unique(c(broke.idx.pd1, broke.idx.pd2))
  #
  #if (length(broke.idx) > 0) {
  #  messagef("Base Learner %s is broken and will be removed\n", names(bls)[broke.idx])
  #  base.models = base.models[-broke.idx]
  #  pred.list = pred.list[-broke.idx]
  #  pred.data = pred.data[-broke.idx]
  #}
  # add true value
  tn = getTaskTargetNames(task)
  pred.data[[tn]] = results[[1]]$resres$pred$data$truth
  # convert list to data.frame
  pred.data = as.data.frame(pred.data)

  if (use.feat) {
    # add data with normal features IN CORRECT ORDER
    org.feat = getTaskData(task)
    org.feat = org.feat[, !colnames(org.feat) %in% tn, drop = FALSE]
    pred.data = cbind(pred.data, org.feat)
  }
  super.task = makeSuperLearnerTask(learner$super.learner$type, data = pred.data, target = tn)
  if (show.info)
    messagef("[Super Learner] Train %s with %s features on %s observations", learner$super.learner$id, getTaskNFeats(super.task), getTaskSize(super.task))
  super.model = train(learner$super.learner, super.task)
  # return
  list(method = "superlearner", base.models = base.models,
       super.model = super.model, pred.train = pred.list)
}

################################################################
# Ensemble selection
################################################################


# Train function for "Ensemble Selection" method.
#
# @param learner [`StackedLearner`]
# @param task [`Task`]
# @param replace [`logical(1)`]
# @param init [`integer(1)`] init >= 1
# @param bagprop [`numeric(1)`] 0 < bagprop < 1
# @param bagtime [`integer(1)`] bagtime >= 1
# @param maxiter [`integer(1)`] maxiter >= 1
# @param tolerance [`numeric(1)`] small numeric value.
# @param metric [`Measure`]
# @param ... (any)\cr
# @export
ensembleselectionBaseLearners = function(learner, task, replace = TRUE, init = 1, bagprop = 1, bagtime = 1,
  maxiter = NULL, tolerance = 1e-8, metric = NULL, ...) {
  # check, defaults
  assertFlag(replace)
  assertInt(init, lower = 1, upper = length(learner$base.learners)) #807
  assertNumber(bagprop, lower = 0, upper = 1)
  assertInt(bagtime, lower = 1)
  if (is.null(metric)) metric = getDefaultMeasure(task)
  assertClass(metric, "Measure")
  if (is.null(maxiter)) maxiter = length(learner$base.learners)
  assertInt(maxiter, lower = 1)
  assertNumber(tolerance)

  # setup
  id = learner$id
  save.on.disc = learner$save.on.disc
  td = getTaskDescription(task)
  type = getTaskType(task)
  bls = learner$base.learners
  bls.names = names(bls)
  # check
  if (type != "regr") {
    if (any(extractSubList(bls, "predict.type") == "response"))
      stop("Hill climbing algorithm only takes probability predict type for classification.")
  }

  # resampling, train (parallelMap)
  rin = makeResampleInstance(learner$resampling, task = task)
  parallelLibrary("mlr", master = FALSE, level = "mlr.stackedLearner", show.info = FALSE)
  exportMlrOptions(level = "mlr.stackedLearner")
  show.info = getMlrOption("show.info")
  results = parallelMap(doTrainResample, bls, more.args = list(task, rin,
      measures = metric, show.info, id, save.on.disc),
      impute.error = function(x) x, level = "mlr.stackedLearner")
  base.models = lapply(results, function(x) x[["base.models"]])
  resres = lapply(results, function(x) x[["resres"]])
  pred.list = lapply(resres, function(x) x[["pred"]])
  bls.performance = vapply(resres, function(x) x$aggr, numeric(1)) #sapply(resres, function(x) x$aggr)

  names(base.models) = bls.names
  names(resres) = bls.names
  names(pred.list) = bls.names
  names(bls.performance) = bls.names # this will not be removed below!

  ## Remove FailureModels which would occur problems later #FIXME!?
  ##broke.idx.bm = which(unlist(lapply(base.models, function(x) any(class(x) == "FailureModel"))))
  #broke.idx.pl = which(unlist(lapply(pred.list, function(x) anyNA(x$data))))# FIXME?!
  #broke.idx.rr = which(unlist(lapply(resres, function(x) is.na(x$aggr[1]))))
  ##broke.idx = unique(c(broke.idx.bm, broke.idx.rr, broke.idx.pl))
  #broke.idx = unique(c(broke.idx.rr, broke.idx.pl))
  #if (length(broke.idx) > 0) {
  #  messagef("Base Learner %s is broken and will be removed\n", names(bls)[broke.idx])
  #  resres = resres[-broke.idx]
  #  #pred.data = pred.data[-broke.idx]
  #  base.models = base.models[-broke.idx]
  #  pred.list = pred.list[-broke.idx]
  #}

  ensel = applyEnsembleSelection(pred.list = pred.list,
    bls.performance = bls.performance, es.par.vals = list(replace = replace,
    init = init, bagprop = bagprop, bagtime = bagtime, maxiter = maxiter,
    metric = metric, tolerance = tolerance))

  # return
  list(method = "ensembleselection", base.models = base.models, super.model = NULL,
    pred.train = pred.list, bls.performance = bls.performance,
    weights = ensel$weights, freq = ensel$freq, freq.list = ensel$freq.list)
}




# Ensemble selection algorithm
#
# @param pred.list A named list of predictions.
# @param bls.performance Named vector of performance results from training
#   (note that this should be results from resampled predictions to overcome overfitting issues).
# @param es.par.vals list of parameters. See [`makeStackedLearner`].
# @references Caruana, Rich, et al. "Ensemble selection from libraries of models."
#   Proceedings of the twenty-first international conference on Machine learning.
#   ACM, 2004. \url{http://www.cs.cornell.edu/~caruana/ctp/ct.papers/caruana.icml04.icdm06long.pdf}
# @export

applyEnsembleSelection = function(pred.list = pred.list, bls.performance = bls.performance,
  es.par.vals = list(replace = TRUE, init = 1, bagprop = 1, bagtime = 1, maxiter = NULL, tolerance = 1e-8, metric = NULL)) {
  # check
  assertClass(es.par.vals, "list")
  # setup
  bls.names = names(pred.list)
  bls.length = length(pred.list)

  # FIXME: Need defaults. should be nicer
  if (is.null(es.par.vals$replace)) es.par.vals$replace = TRUE
  if (is.null(es.par.vals$init)) es.par.vals$init = 1
  if (is.null(es.par.vals$bagprop)) es.par.vals$bagprop = 0.5
  if (is.null(es.par.vals$bagtime)) es.par.vals$bagtime = 20
  if (is.null(es.par.vals$metric)) es.par.vals$metric = getDefaultMeasure(pred.list[[1]]$task.desc)
  if (is.null(es.par.vals$maxiter)) es.par.vals$maxiter = bls.length
  if (is.null(es.par.vals$tolerance)) es.par.vals$tolerance = 1e-8

  #FIXME: neeeded!? or is es.par.vals$bar ok!?
  replace = es.par.vals$replace
  init = es.par.vals$init
  bagprop = es.par.vals$bagprop
  bagtime = es.par.vals$bagtime
  maxiter = es.par.vals$maxiter
  metric = es.par.vals$metric
  tolerance = es.par.vals$tolerance

  # setup
  m = bls.length
  freq = rep(0, m)
  names(freq) = bls.names
  freq.list = vector("list", bagtime)

  # outer loop (bagtimes bagging iterations)
  for (bagind in seq_len(bagtime)) {
    # bagging of models
    bagsize = ceiling(m * bagprop)
    bagmodel = sample(1:m, bagsize)

    # Initial selection of strongest learners
    inds.init = NULL
    inds.selected = NULL
    sel.algo = NULL
    single.scores = rep(ifelse(metric$minimize, Inf, -Inf), m)

    #
    for (i in bagmodel) {
      single.scores[i] = bls.performance[i]
    }
    if (metric$minimize) { # FIXME use orderScore
      inds.init = order(single.scores)[1:init]
    } else {
      inds.init = rev(order(single.scores))[1:init]
    }
    # Increment 'freq' for init best starting values
    freq[inds.init] = freq[inds.init] + 1

    # Create a list of predictions, aggregate them (averaging) and apply metric to the new prediction
    current.pred.list = pred.list[inds.init]
    current.pred = aggregatePredictions(current.pred.list, pL = FALSE)
    bench.score = metric$fun(pred = current.pred)

    inds.selected = inds.init

    # inner loop (adds as many models in current bagging iteration until tolerance or maxiter is reached)
    for (inneriter in seq_len(maxiter)) {
      temp.score = rep(ifelse(metric$minimize, Inf, -Inf), m)
      # uniquely add every prediction form the bag to the current prediction and calc. performance
      for (i in bagmodel) {
        temp.pred.list = append(current.pred.list, pred.list[i])
        aggr.pred = aggregatePredictions(temp.pred.list, pL = FALSE)
        temp.score[i] = metric$fun(pred = aggr.pred)
      }
      # order scores
      if (metric$minimize) { #FIXME use orderScore
        inds.ordered = order(temp.score)
      } else {
        inds.ordered  = rev(order(temp.score))
      }
      # identify best one (only one model is added per inner iteration)
      if (!replace) {
        best.ind = setdiff(inds.ordered, inds.selected)[1]
      } else {
        best.ind = inds.ordered[1]
      }
      # take the best ones score
      new.score = temp.score[best.ind]
      # check if new ensemble improves overall performance
      if (bench.score - new.score < tolerance) {
        break() # stop inner loop
      } else {
        current.pred.list = append(current.pred.list, pred.list[best.ind])
        current.pred = aggregatePredictions(current.pred.list, pL = FALSE)
        freq[best.ind] = freq[best.ind] + 1
        inds.selected = c(inds.selected, best.ind)
        bench.score = new.score
      }
    }
    # freq.list lists names of all selected bls in each bagging iteration
    selected.innerloop = bls.names[inds.selected]
    freq.list[[bagind]] = selected.innerloop
  }
  weights = freq/sum(freq) #TODO: drop in future?
  # return
  list(freq = freq, freq.list = freq.list, weights = weights)
}
