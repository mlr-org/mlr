# all argumemts below are checked by called functions

#' @rdname resample
#' @export
crossval = function(learner, task, iters = 10L, stratify = FALSE, measures, models = FALSE, keep.pred = TRUE, ..., show.info = getMlrOption("show.info")) {
  learner = checkLearner(learner)
  learner = setHyperPars(learner, ...)
  rdesc = makeResampleDesc("CV", iters = iters, stratify = stratify)
  resample(learner, task, rdesc, measures = measures, models = models, keep.pred = keep.pred, show.info = show.info)
}

#' @rdname resample
#' @export
repcv = function(learner, task, folds = 10L, reps = 10L, stratify = FALSE, measures, models = FALSE, keep.pred = TRUE, ..., show.info = getMlrOption("show.info")) {
  learner = checkLearner(learner)
  learner = setHyperPars(learner, ...)
  rdesc = makeResampleDesc("RepCV", folds = folds, reps = reps, stratify = stratify)
  resample(learner, task, rdesc, measures = measures, models = models, keep.pred = keep.pred, show.info = show.info)
}

#' @rdname resample
#' @export
holdout = function(learner, task, split = 2 / 3, stratify = FALSE, measures, models = FALSE, keep.pred = TRUE, ..., show.info = getMlrOption("show.info")) {
  learner = checkLearner(learner)
  learner = setHyperPars(learner, ...)
  rdesc = makeResampleDesc("Holdout", split = split, stratify = stratify)
  resample(learner, task, rdesc, measures = measures, models = models, keep.pred = keep.pred, show.info = show.info)
}

#' @rdname resample
#' @export
subsample = function(learner, task, iters = 30, split = 2 / 3, stratify = FALSE, measures, models = FALSE, keep.pred = TRUE, ..., show.info = getMlrOption("show.info")) {
  learner = checkLearner(learner)
  learner = setHyperPars(learner, ...)
  rdesc = makeResampleDesc("Subsample", iters = iters, split = split, stratify = stratify)
  resample(learner, task, rdesc, measures = measures, models = models, keep.pred = keep.pred, show.info = show.info)
}

#' @rdname resample
#' @export
bootstrapOOB = function(learner, task, iters = 30, stratify = FALSE, measures, models = FALSE, keep.pred = TRUE, ..., show.info = getMlrOption("show.info")) {
  learner = checkLearner(learner)
  learner = setHyperPars(learner, ...)
  rdesc = makeResampleDesc("Bootstrap", iters = iters, stratify = stratify)
  resample(learner, task, rdesc, measures = measures, models = models, keep.pred = keep.pred, show.info = show.info)
}

#' @rdname resample
#' @export
bootstrapB632 = function(learner, task, iters = 30, stratify = FALSE, measures, models = FALSE, keep.pred = TRUE, ..., show.info = getMlrOption("show.info")) {
  learner = checkLearner(learner)
  learner = setHyperPars(learner, ...)
  assertClass(task, classes = "Task")
  rdesc = makeResampleDesc("Bootstrap", predict = "both", iters = iters, stratify = stratify)
  measures = checkMeasures(measures, task, aggr = b632)
  resample(learner, task, rdesc, measures = measures, models = models, keep.pred = keep.pred, show.info = show.info)
}

#' @rdname resample
#' @export
bootstrapB632plus = function(learner, task, iters = 30, stratify = FALSE, measures, models = FALSE, keep.pred = TRUE, ..., show.info = getMlrOption("show.info")) {
  learner = checkLearner(learner)
  learner = setHyperPars(learner, ...)
  assertClass(task, classes = "Task")
  rdesc = makeResampleDesc("Bootstrap", predict = "both", iters = iters, stratify = stratify)
  measures = checkMeasures(measures, task, aggr = b632plus)
  resample(learner, task, rdesc, measures = measures, models = models, keep.pred = keep.pred, show.info = show.info)
}

#' @rdname resample
#' @export
growingcv = function(learner, task, horizon = 10L, initialWindow = 10L, size = 100L, skip = 0L, measures, models = FALSE, keep.pred = TRUE, ..., show.info = getMlrOption("show.info")) {
  learner = checkLearner(learner, ...)
  assertClass(task, classes = "ForecastTask")
  rdesc = makeResampleDesc("GrowingCV", predict = "both", horizon = horizon, initialWindow = initialWindow,
                           size = size, skip = skip)
  measures = checkMeasures(measures, task, aggr = b632plus)
  resample(learner, task, rdesc, measures = measures, models = models, keep.pred = keep.pred, show.info = show.info)
}

#' @rdname resample
#' @export
fixedcv = function(learner, task, horizon = 10L, initialWindow = 10L, size = 100L, skip = 0L, measures, models = FALSE, keep.pred = TRUE, ..., show.info = getMlrOption("show.info")) {
  learner = checkLearner(learner, ...)
  assertClass(task, classes = "ForecastTask")
  rdesc = makeResampleDesc("FixedCV", predict = "both", horizon = horizon, initialWindow = initialWindow,
                           size = size, skip = skip)
  measures = checkMeasures(measures, task, aggr = b632plus)
  resample(learner, task, rdesc, measures = measures, models = models, keep.pred = keep.pred, show.info = show.info)
}

