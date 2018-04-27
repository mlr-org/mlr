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
    ensembleselection = do.call(ensembleselectionBaseLearners, c(list(.learner, .task), .learner$es.par.vals))
  )
  # FIXME: We should maybe handle failed learners here generally
}

# Train function for simple aggregation of base learner predictions without weights.
#
# @param learner ([`StackedLearner`]).
# @template arg_task
aggregateBaseLearners = function(learner, task) {
  id = learner$id
  save.on.disc = learner$save.on.disc
  save.preds = learner$save.preds
  bls = learner$base.learners

  # parallelMap: train, predict
  parallelLibrary("mlr", master = FALSE, level = "mlr.stackedLearner", show.info = FALSE)
  exportMlrOptions(level = "mlr.stackedLearner")
  show.info = getMlrOption("show.info")
  results = parallelMap(doTrainPredict, bls, more.args = list(task, show.info, id, save.on.disc),
    impute.error = function(x) x, level = "mlr.stackedLearner")

  names(results) = names(bls)
  base.models = extractSubList(results, "base.models", simplify = FALSE)
  pred.list = extractSubList(results, "pred", simplify = FALSE)

  # return
  list(
    method = "aggregate",
    base.models = base.models,
    super.model = NULL,
    pred.train = ifelse(save.preds, pred.list, NULL)
  )
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
  td = getTaskDesc(task)
  type = getPreciseTaskType(td)
  bls = learner$base.learners
  bls.names = names(bls)
  bpt = unique(extractSubList(bls, "predict.type"))
  use.feat = learner$use.feat
  id = learner$id
  save.on.disc = learner$save.on.disc

  bls = learner$base.learners

  # Do the resampling (parallelMap)
  rin = makeResampleInstance(learner$resampling, task = task)
  parallelLibrary("mlr", master = FALSE, level = "mlr.stackedLearner", show.info = FALSE)
  exportMlrOptions(level = "mlr.stackedLearner")
  show.info = getMlrOption("show.info")
  results = parallelMap(doTrainResample, bls, more.args = list(task, rin,
    measures = getDefaultMeasure(task), show.info, learner$id, learner$save.on.disc),
    impute.error = function(x) x, level = "mlr.stackedLearner")

  # Extract relevant results
  names(results) = names(bls)
  base.models = extractSubList(results, "base.models", simplify = FALSE)
  resres = extractSubList(results, "resres", simplify = FALSE)
  pred.list = extractSubList(resres, "pred", simplify = FALSE)
  pred.data = lapply(pred.list, function(x) getPredictionDataNonMulticoll(x))
  bls.perf = vnapply(resres, function(x) x$aggr)

  # add true target
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
    super.model = super.model, pred.train = pred.list, bls.perf = bls.perf)
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

  # Check Inputs
  assertFlag(replace)
  assertInt(init, lower = 1, upper = length(learner$base.learners)) #807
  assertNumber(bagprop, lower = 0, upper = 1)
  assertInt(bagtime, lower = 1)
  if (is.null(metric)) metric = getDefaultMeasure(task)
  assertClass(metric, "Measure")
  if (is.null(maxiter)) maxiter = length(learner$base.learners)
  assertInt(maxiter, lower = 1)
  assertNumber(tolerance)
  if (getTaskType(task) != "regr") {
    if (any(extractSubList(learner$base.learners, "predict.type") == "response"))
      stop("Hill climbing algorithm only takes probability predict type for classification.")
  }
  bls = learner$base.learners

  # Do the resampling (parallelMap)
  rin = makeResampleInstance(learner$resampling, task = task)
  parallelLibrary("mlr", master = FALSE, level = "mlr.stackedLearner", show.info = FALSE)
  exportMlrOptions(level = "mlr.stackedLearner")
  show.info = getMlrOption("show.info")
  results = parallelMap(doTrainResample, bls, more.args = list(task, rin,
    measures = metric, show.info, learner$id, learner$save.on.disc),
    impute.error = function(x) x, level = "mlr.stackedLearner")

  # Extract relevant results
  names(results) = names(bls)
  base.models = extractSubList(results, "base.models", simplify = FALSE)
  resres = extractSubList(results, "resres", simplify = FALSE)
  pred.list = extractSubList(resres, "pred", simplify = FALSE)
  bls.perf = vnapply(resres, function(x) x$aggr)


  ensel = applyEnsembleSelection(learner, pred.list = pred.list,
    bls.perf = bls.perf, es.par.vals = list(replace = replace,
    init = init, bagprop = bagprop, bagtime = bagtime, maxiter = maxiter,
    metric = metric, tolerance = tolerance))

  # FIXME Florian: Throw out unused learners?
  # FIXME metric -> measure
  list(method = "ensembleselection", base.models = base.models, super.model = NULL,
    pred.train = pred.list, bls.performance = bls.perf,
    weights = ensel$weights, freq = ensel$freq, freq.list = ensel$freq.list, measure = metric)
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
applyEnsembleSelection = function(learner, pred.list = pred.list, bls.performance = bls.performance,
  es.par.vals = list(replace = TRUE, init = 1, bagprop = 1, bagtime = 1, maxiter = NULL, tolerance = 1e-8, metric = NULL)) {

  bls.names = names(pred.list)
  m = length(pred.list)

  replace = es.par.vals$replace
  init = es.par.vals$init
  bagprop = es.par.vals$bagprop
  bagtime = es.par.vals$bagtime
  maxiter = es.par.vals$maxiter
  metric = es.par.vals$metric
  tolerance = es.par.vals$tolerance

  # setup
  freq = rep(0, m)
  names(freq) = bls.names
  freq.list = vector("list", bagtime)

  # outer loop (bagtimes bagging iterations)
  for (bag.id in seq_len(bagtime)) {
    # bagging models
    bagsize = ceiling(m * bagprop)
    bagmodel = sample(seq_len(m), bagsize)

    # Initial selection of strongest learners
    inds.init = NULL
    inds.selected = NULL
    sel.algo = NULL
    single.scores = rep(ifelse(metric$minimize, Inf, -Inf), m)

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
    current.pred = aggregatePredictions(current.pred.list)
    bench.score = metric$fun(pred = current.pred)

    inds.selected = inds.init

    # inner loop (adds as many models in current bagging iteration until tolerance or maxiter is reached)
    for (inneriter in seq_len(maxiter)) {
      temp.score = rep(ifelse(metric$minimize, Inf, -Inf), m)
      # uniquely add every prediction form the bag to the current prediction and calc. performance
      for (i in bagmodel) {
        temp.pred.list = append(current.pred.list, pred.list[i])
        aggr.pred = aggregatePredictions(temp.pred.list)
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
        current.pred = aggregatePredictions(current.pred.list)
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

  # Drop unused learners
  # only apply prediction to models which are relevant for ensembleselection
  used.bls = names(which(model$learner.model$freq > 0))
  bms = model$learner.model$base.models[used.bls]

  # return
  list(freq = freq, freq.list = freq.list, weights = weights)
}
