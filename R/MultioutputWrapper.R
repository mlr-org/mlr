#' @title ADD TITLE #FIXME
#'
#' @description
#' ADD DESCRIPTION #FIXME
#'
#' Models can easily be accessed via [getLearnerModel].
#'
#' @template arg_learner
#' @template arg_multilabel_cvfolds
#' @template ret_learner
#' @references
#' ADD REFRENCES #FIXME
#' @family wrapper
#' @family multilabel
#' @family multiregr
#' @family mixedoutput
#' @export
#' @example # add example FIXME#inst/examples/MultilabelWrapper.R
makeMultioutputWrapper = function(learner, output.type = "mixedoutput", learner.lvl1, learner.meta, true.label = TRUE,
  cv.folds = 2, chaining = FALSE, order = NULL) {
  if (!output.type %in% c("mixedoutput", "multilabel", "multiregr")) stop("type must be 'mixedoutput', 'multilabel', or 'multiregr'")
  if (!xor(missing(learner), missing(learner.lvl1) & missing(learner.meta)))
    stop("Pass either a learner alone or learner.lvl1 together with learner.meta to predict, but not both!")

  if (missing(learner)) {
    if (xor(missing(learner.lvl1), missing(learner.meta)))
    stop("both learner.lvl1 and learner.meta must be specified")
  }

  assertLogical(true.label)
  if (true.label) extra.label = "trueLabel" else extra.label = "predLabel"

  assertLogical(chaining)
  if (chaining) conditioning = "chaining" else conditioning = "full"



  if (output.type == "multilabel") learner = checkLearner(learner, type = "classif", props = "twoclass")
  if (output.type == "mixedoutput") learner = checkLearner(learner)
  if (output.type == "multiregr") learner = checkLearner(learner, type = "regr")

  id = stri_paste(output.type, extra.label, conditioning, getLearnerId(learner), sep = ".")
  packs = getLearnerPackages(learner)
  type = getLearnerType(learner)
  x = makeHomogeneousEnsemble(id, type, learner, packs, learner.subclass = "MultioutputWrapper",
    model.subclass = "MultioutputModel")
  x$type = output.type
  x$extra.label = extra.label
  x$conditioning = conditioning
  if (extra.label == "pred.label") x$cv.folds = cv.folds
  return(x)
}

#' @export
trainLearner.MultioutputWrapper = function(.learner, .task, .subset = NULL, .weights = NULL, ...) {
  targets = getTaskTargetNames(.task)
  .task = subsetTask(.task, subset = .subset)
  data = getTaskData(.task)
  # train level 1 learners
  models.lvl1 = getLearnerModel(train(makeMultilabelBinaryRelevanceWrapper(.learner$next.learner), .task, weights = .weights))
  # predict labels
  f = function(tn) {
    data2 = dropNamed(data, setdiff(targets, tn))
    ctask = makeClassifTask(id = tn, data = data2, target = tn)
    rdesc = makeResampleDesc("CV", iters = .learner$cv.folds)
    r = resample(.learner$next.learner, ctask, rdesc, weights = .weights, show.info = FALSE)
    as.numeric(as.logical(r$pred$data[order(r$pred$data$id), ]$response)) #did not use getPredictionResponse, because of ordering
  }
  pred.labels = sapply(targets, f)
  # train meta level learners
  g = function(tn) {
    data.meta = dropNamed(data.frame(data, pred.labels), setdiff(targets, tn))
    ctask = makeClassifTask(id = tn, data = data.meta, target = tn)
    train(.learner$next.learner, ctask, weights = .weights)
  }
  models.meta = lapply(targets, g)
  makeHomChainModel(.learner, c(models.lvl1, models.meta))
}

#' @export
predictLearner.MultilabelStackingWrapper = function(.learner, .model, .newdata, .subset = NULL, ...) {
  models = getLearnerModel(.model, more.unwrap = FALSE)
  # Level 1 prediction (binary relevance)
  models.lvl1 = models[seq_along(.model$task.desc$target)]
  f = if (.learner$predict.type == "response") {
    function(m) as.logical(getPredictionResponse(predict(m, newdata = .newdata, subset = .subset, ...)))
  } else {
    function(m) getPredictionProbabilities(predict(m, newdata = .newdata, subset = .subset, ...), cl = "TRUE")
  }
  if (.learner$predict.type == "response") {
    pred.lvl1 = sapply(data.frame(asMatrixCols(lapply(models.lvl1, f))), as.numeric)
  } else {
    pred.lvl1 = data.frame(asMatrixCols(lapply(models.lvl1, f)))
  }
  colnames(pred.lvl1) = paste(.model$task.desc$target, ".1", sep = "")
  # Meta level prediction
  models.meta = models[(length(.model$task.desc$target) + 1):(2 * length(.model$task.desc$target))]
  nd = data.frame(.newdata, pred.lvl1)
  g = if (.learner$predict.type == "response") {
    function(m) as.logical(getPredictionResponse(predict(m, newdata = nd, subset = .subset, ...)))
  } else {
    function(m) getPredictionProbabilities(predict(m, newdata = nd, subset = .subset, ...), cl = "TRUE")
  }
  asMatrixCols(lapply(models.meta, g), col.names = .model$task.desc$target)
}
