#' Measure performance of prediction.
#'
#' Measures the quality of a prediction w.r.t. some performance measure.
#'
#' @template arg_pred
#' @template arg_measures
#' @param task [\code{\link{Task}}]\cr
#'   Learning task, might be requested by performance measure, usually not needed except for clustering.
#' @param model [\code{\link{WrappedModel}}]\cr
#'   Model built on training data, might be requested by performance measure, usually not needed.
#' @param feats [\code{data.frame}]\cr
#'   Features of predicted data, usually not needed except for clustering.
#'   If the prediction was generated from a \code{task}, you can also pass this instead and the features
#'   are extracted from it.
#' @return [named \code{numeric}]. Performance value(s), named by measure(s).
#' @export
#' @family performance
#' @examples
#' training.set = seq(1, nrow(iris), by = 2)
#' test.set = seq(2, nrow(iris), by = 2)
#'
#' task = makeClassifTask(data = iris, target = "Species")
#' lrn = makeLearner("classif.lda")
#' mod = train(lrn, task, subset = training.set)
#' pred = predict(mod, newdata = iris[test.set, ])
#' performance(pred, measures = mmce)
#'
#' # Compute multiple performance measures at once
#' ms = list("mmce" = mmce, "acc" = acc, "timetrain" = timetrain)
#' performance(pred, measures = ms, task, mod)
performance = function(pred, measures, task = NULL, model = NULL, feats = NULL) {
  if (!is.null(pred))
    assertClass(pred, classes = "Prediction")
  measures = checkMeasures(measures, pred$task.desc)
  res = vnapply(measures, doPerformanceIteration, pred = pred, task = task, model = model, td = NULL, feats = feats)
  # FIXME: This is really what the names should be, but it breaks all kinds of other stuff
  #if (inherits(pred, "ResamplePrediction")) {
  #  setNames(res, vcapply(measures, measureAggrName))
  #} else {
  #  setNames(res, extractSubList(measures, "id"))
  #}
  setNames(res, extractSubList(measures, "id"))
}

doPerformanceIteration = function(measure, pred = NULL, task = NULL, model = NULL, td = NULL, feats = NULL) {
  m = measure
  props = m$properties
  if ("req.pred" %in% props) {
    if (is.null(pred))
      stopf("You need to pass pred for measure %s!", m$id)
  }
  if ("req.truth" %in% props) {
    type = getTaskDescription(pred)$type
    if (type == "surv") {
      if (is.null(pred$data$truth.time) || is.null(pred$data$truth.event))
        stopf("You need to have 'truth.time' and 'truth.event' columns in your pred object for measure %s!", m$id)
    } else if (type == "multilabel") {
      if (!(any(stri_detect_regex(colnames(pred$data), "^truth\\."))))
        stopf("You need to have 'truth.*' columns in your pred object for measure %s!", m$id)
    } else if (type == "mfcregr") {
      if (!(any(stri_detect_regex(colnames(pred$data), "^truth\\."))))
        # FIXME: We can either have it return one truth or multiple
        #  How do we make this correct?
        if (is.null(pred$data$truth))
          stopf("You need to have 'truth.*' columns in your pred object for measure %s!", m$id)
    } else {
      if (is.null(pred$data$truth))
        stopf("You need to have a 'truth' column in your pred object for measure %s!", m$id)
    }
  }
  if ("req.model" %in% props) {
    if (is.null(model))
      stopf("You need to pass model for measure %s!", m$id)
    assertClass(model, classes = "WrappedModel")
  }
  if ("req.task" %in% props) {
    if (is.null(task))
      stopf("You need to pass task for measure %s!", m$id)
    assertClass(task, classes = "Task")
  }
  if ("req.feats" %in% props) {
    if (is.null(task) && is.null(feats))
      stopf("You need to pass either task or features for measure %s!", m$id)
    else if (is.null(feats))
      feats = task$env$data[pred$data$id,, drop = FALSE]
    else
      assertClass(feats, "data.frame")
  }
  # we need to find desc somewhere
  td = if (!is.null(pred))
    pred$task.desc
  else if (!is.null(model))
    model$task.desc
  else if (!is.null(task))
    getTaskDescription(task)

  # null only happens in custom resampled measure when we do no individual measurements
  if (!is.null(td)) {
    if (td$type %nin% props)
      stopf("Measure %s does not support task type %s!", m$id, td$type)
    if (td$type == "classif" && length(td$class.levels) > 2L && "classif.multi" %nin% props)
      stopf("Multiclass problems cannot be used for measure %s!", m$id)

    # if we have multiple req.pred.types, check if we have one of them (currently we only need prob)
    req.pred.types = if ("req.prob" %in% props) "prob" else character(0L)
    if (!is.null(pred) && length(req.pred.types) > 0L && pred$predict.type %nin% req.pred.types)
      stopf("Measure %s requires predict type to be: '%s'!",
        m$id, collapse(req.pred.types))
  }

  # if it's a ResamplePrediction, aggregate
  if (inherits(pred, "ResamplePrediction")) {
    if (is.null(pred$data$iter)) pred$data$iter = 1L
    if (is.null(pred$data$set)) pred$data$set = "test"
    fun = function(ss) {
      is.train = ss$set == "train"
      if (any(is.train)) {
        pred$data = as.data.frame(ss[is.train, ])
        perf.train = measure$fun(task, model, pred, feats, m$extra.args)
      } else {
        perf.train = NA_real_
      }
      pred$data = as.data.frame(ss[!is.train, ])
      perf.test = measure$fun(task, model, pred, feats, m$extra.args)
      list(perf.train = perf.train, perf.test = perf.test)
    }
    perfs = as.data.table(pred$data)[, fun(.SD), by= "iter"]
    measure$aggr$fun(task, perfs$perf.test, perfs$perf.train, measure, perfs$iter, pred)
  } else {
    measure$fun(task, model, pred, feats, m$extra.args)
  }
}
