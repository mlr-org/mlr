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

  # parallelMap: train, predict
  parallelLibrary("mlr", master = FALSE, level = "mlr.stackedLearner", show.info = FALSE)
  exportMlrOptions(level = "mlr.stackedLearner")
  show.info = getMlrOption("show.info")
  results = parallelMap(doTrainPredict, learner$base.learners, more.args =
      list(task, show.info, learner$id, learner$save.on.disc),
    impute.error = function(x) x, level = "mlr.stackedLearner")

  names(results) = names(learner$base.learners)
  base.models = extractSubList(results, "base.models", simplify = FALSE)
  pred.list = extractSubList(results, "pred", simplify = FALSE)

  # return
  list(
    method = "aggregate",
    base.models = base.models,
    super.model = NULL,
    pred.train = ifelse(learner$save.preds, pred.list, NULL)
  )
}


################################################################
# SuperLearner as a combiner based on cross validated predictions of level 1 data.
################################################################
# Train function for stacking method "Super Learner", which uses meta learner to obtain level 1 data
# and uses inner cross-validation for prediction.
#
# @param learner ([`StackedLearner`]).
# @param task ([`Task`])
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
  # Get predictions, but drop one column to avoid multicolinearity
  pred.data = lapply(pred.list, function(x) getPredictionDataNonMulticoll(x))
  # Get aggregated performances
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
    messagef("[Super Learner] Train %s with %s features on %s observations", learner$super.learner$id,
      getTaskNFeats(super.task), getTaskSize(super.task))
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
# @param measure [`Measure`]
# @param ... (any)\cr
# @export
ensembleselectionBaseLearners = function(learner, task, replace = TRUE, init = 1, bagprop = 1, bagtime = 1,
  maxiter = NULL, tolerance = 1e-8, measure = NULL, ...) {

  # Check Inputs
  assertFlag(replace)
  assertInt(init, lower = 1, upper = length(learner$base.learners)) #807
  assertNumber(bagprop, lower = 0, upper = 1)
  assertInt(bagtime, lower = 1)
  if (is.null(measure)) measure = getDefaultMeasure(task)
  assertClass(measure, "Measure")
  if (is.null(maxiter)) maxiter = length(learner$base.learners)
  assertInt(maxiter, lower = 1)
  assertNumber(tolerance)
  if (getTaskType(task) != "regr") {
    if (any(extractSubList(learner$base.learners, "predict.type") == "response"))
      stop("Hill climbing algorithm only takes probability predict type for classification.")
  }

  # Do the resampling (parallelMap)
  rin = makeResampleInstance(learner$resampling, task = task)
  parallelLibrary("mlr", master = FALSE, level = "mlr.stackedLearner", show.info = FALSE)
  exportMlrOptions(level = "mlr.stackedLearner")
  show.info = getMlrOption("show.info")
  results = parallelMap(doTrainResample, learner$base.learners,
    more.args = list(task, rin, measures = measure, show.info, learner$id, learner$save.on.disc),
    impute.error = function(x) x, level = "mlr.stackedLearner")

  # Extract relevant results
  names(results) = names(learner$base.learners)
  base.models = extractSubList(results, "base.models", simplify = FALSE)
  resres = extractSubList(results, "resres", simplify = FALSE)
  pred.list = extractSubList(resres, "pred", simplify = FALSE)
  bls.perf = vnapply(resres, function(x) x$aggr)

  # Do the bagging, return a list of selected learners in each bag
  selected.list = lapply(seq_len(bagtime), doEnsembleBagIter(learner, pred.list, bls.perf, replace = TRUE,
    init = 1, bagprop = 1, bagtime = 1, maxiter = NULL, tolerance = 1e-8, measure = NULL))

  list(method = "ensembleselection", base.models = base.models, super.model = NULL,
    pred.train = pred.list, bls.performance = bls.perf,
    selected = selected.list, measure = measure)
}


# Do a single bagging iteration for enseble selection
doEnsembleBagIter = function(learner, pred.list, bls.perf, replace, init, bagprop, bagtime, maxiter,
  tolerance, measure) {

  # Compute an index of models in this bagging iteration
  m = length(pred.list)
  bag_index = sample(seq_len(length(pred.list)), m * bagprop, replace = replace)

  # Get #init best models and save into selected
  best.init = order(models$perf.list[bag_index], decreasing = !measure$minimize)[seq_len(init)]
  # Selected is a integer vector, where the entry refers to the number of times a learner was
  # selected, and the position refer to the learner.
  selected = numeric(length(pred.list))
  selected[bag_index][best.init] = selected[bag_index][best.init] + 1

  # Compute predictions and performance
  current.pred = aggregateModelPredictions(learner, pred.list[rep(seq_len(m)), selected])
  current.perf = measure$fun(current.pred)

  # Sequentially add maxiter models to the bag
  for (i in seq_len(maxiter)) {

    # If we do not replace, we drop the learner from the bag index
    if (!replace) bag_index = setdiff(bag_index, which(selected == 1))
    if (length(bag_index) == 0) break

    # Add each model from the bag once and compute performance
    preds = lapply(seq_len(m)[bag_index], function(x) addModel(models, x, selected, current.pred))
    perfs = lapply(preds, measure$fun)

    # Get performance of best model
    best.perf = ifelse(measure$minimize, min(perfs), max(perfs))
    # Break if delta is smaller then tolerance or performance decreases
    if (abs(best.pe1rf - current.perf) < tolerance |
       ((best.perf > current.perf) == measure$minimize)) break

    # Add the best model if adding it increases performance
    if ((best.perf < current.perf) == measure$minimize) {
      selected[bag_index][which(perfs == best.perf)] = selected[bag_index][which(perfs == best.perf)] + 1
      current.pred = preds
      current.perf = best.perf
    }
  }
  return(selected)
}


# Return performance after adding a new model in the bag
addModel = function(i, learner, models, selected, current.pred) {
  # Do a weighted mean between current model and new model
  pred = aggregateModelPredictions(learner, list(current.pred, pred.list[[i]]),
    lrn.weights = c(sum(selected), 1) / (sum(selected) + 1))
  measure$fun(pred)
}
