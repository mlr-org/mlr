# all argumemts below are checked by called functions

#' @rdname resample
#' @export
crossval = function(learner, task, iters = 10L, stratify = FALSE, measures, models = FALSE, keep.pred = TRUE, ..., show.info = getMlrOption("show.info")) {
  learner = checkLearner(learner, ...)
  rdesc = makeResampleDesc("CV", iters = iters, stratify = stratify)
  resample(learner, task, rdesc, measures = measures, models = models, keep.pred = keep.pred, show.info = show.info)
}

#' @rdname resample
#' @export
repcv = function(learner, task, folds = 10L, reps = 10L, stratify = FALSE, measures, models = FALSE, keep.pred = TRUE, ..., show.info = getMlrOption("show.info")) {
  learner = checkLearner(learner, ...)
  rdesc = makeResampleDesc("RepCV", folds = folds, reps = reps, stratify = stratify)
  resample(learner, task, rdesc, measures = measures, models = models, keep.pred = keep.pred, show.info = show.info)
}

#' @rdname resample
#' @export
holdout = function(learner, task, split = 2/3, stratify = FALSE, measures, models = FALSE, keep.pred = TRUE, ..., show.info = getMlrOption("show.info")) {
  learner = checkLearner(learner, ...)
  rdesc = makeResampleDesc("Holdout", split = split, stratify = stratify)
  resample(learner, task, rdesc, measures = measures, models = models, keep.pred = keep.pred, show.info = show.info)
}

#' @rdname resample
#' @export
subsample = function(learner, task, iters = 30, split = 2/3, stratify = FALSE, measures, models = FALSE, keep.pred = TRUE, ..., show.info = getMlrOption("show.info")) {
  learner = checkLearner(learner, ...)
  rdesc = makeResampleDesc("Subsample", iters = iters, split = split, stratify = stratify)
  resample(learner, task, rdesc, measures = measures, models = models, keep.pred = keep.pred, show.info = show.info)
}

#' @rdname resample
#' @export
bootstrapOOB = function(learner, task, iters = 30, stratify = FALSE, measures, models = FALSE, keep.pred = TRUE, ..., show.info = getMlrOption("show.info")) {
  learner = checkLearner(learner, ...)
  rdesc = makeResampleDesc("Bootstrap", iters = iters, stratify = stratify)
  resample(learner, task, rdesc, measures = measures, models = models, keep.pred = keep.pred, show.info = show.info)
}

#' @rdname resample
#' @export
bootstrapB632 = function(learner, task, iters = 30, stratify = FALSE, measures, models = FALSE, keep.pred = TRUE, ..., show.info = getMlrOption("show.info")) {
  learner = checkLearner(learner, ...)
  assertClass(task, classes = "Task")
  rdesc = makeResampleDesc("Bootstrap", predict = "both", iters = iters, stratify = stratify)
  measures = checkMeasures(measures, task, aggr = b632)
  resample(learner, task, rdesc, measures = measures, models = models, keep.pred = keep.pred, show.info = show.info)
}

#' @rdname resample
#' @export
bootstrapB632plus = function(learner, task, iters = 30, stratify = FALSE, measures, models = FALSE, keep.pred = TRUE, ..., show.info = getMlrOption("show.info")) {
  learner = checkLearner(learner, ...)
  assertClass(task, classes = "Task")
  rdesc = makeResampleDesc("Bootstrap", predict = "both", iters = iters, stratify = stratify)
  measures = checkMeasures(measures, task, aggr = b632plus)
  resample(learner, task, rdesc, measures = measures, models = models, keep.pred = keep.pred, show.info = show.info)
}

#' @rdname makeResampleDesc
#' @include ResampleDesc.R
#' @section Standard ResampleDesc objects:
#' For common resampling strategies you can save some typing
#' by using the following description objects:
#' \describe{
#' \item{hout}{holdout a.k.a. test sample estimation
#' (two-thirds training set, one-third testing set)}
#' \item{cv2}{2-fold cross-validation}
#' \item{cv3}{3-fold cross-validation}
#' \item{cv5}{5-fold cross-validation}
#' \item{cv10}{10-fold cross-validation}
#' }
#' @export
#' @usage NULL
#' @docType NULL
#' @format NULL
#' @keywords NULL
hout = makeResampleDesc("Holdout")

#' @rdname makeResampleDesc
#' @include ResampleDesc.R
#' @export
#' @usage NULL
#' @docType NULL
#' @format NULL
#' @keywords NULL
cv2 = makeResampleDesc("CV", iters = 2L)

#' @rdname makeResampleDesc
#' @include ResampleDesc.R
#' @export
#' @usage NULL
#' @docType NULL
#' @format NULL
#' @keywords NULL
cv3 = makeResampleDesc("CV", iters = 3L)

#' @rdname makeResampleDesc
#' @include ResampleDesc.R
#' @export
#' @usage NULL
#' @docType NULL
#' @format NULL
#' @keywords NULL
cv5 = makeResampleDesc("CV", iters = 5L)

#' @rdname makeResampleDesc
#' @include ResampleDesc.R
#' @export
#' @usage NULL
#' @docType NULL
#' @format NULL
#' @keywords NULL
cv10 = makeResampleDesc("CV", iters = 10L)
