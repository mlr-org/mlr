#' @title Fit models according to a resampling strategy.
#'
#' @description
#' Given a resampling strategy, which defines sets of training and test indices,
#' fits the selected learner using the training sets and performs predictions for
#' the training/test sets. This depends on what you selected in the resampling strategy,
#' see parameter \code{predict} in \code{\link{makeResampleDesc}}.
#'
#' Then performance measures are calculated on all respective data sets and aggregated.
#'
#' You are able to return all fitted models (parameter \code{models}) or extract specific parts
#' of the models (parameter \code{extract}) as returning all of them completely
#' might be memory intensive.
#'
#' For construction of the resampling strategies use the factory methods
#' \code{\link{makeResampleDesc}} and \code{\link{makeResampleInstance}}.
#'
#' @param learner [\code{\link{Learner}}]\cr
#'   The learner.
#' @param task [\code{\link{SupervisedTask}}]\cr
#'   The task.
#' @param resampling [\code{\link{ResampleDesc}} or \code{\link{ResampleInstance}}]\cr
#'   Resampling strategy.
#'   If a description is passed, it is instantiated automatically.
#' @param measures [\code{\link{Measure}} | list of \code{\link{Measure}}]\cr
#'   Performance measure(s) to evaluate.
#' @param weights [\code{numeric}]\cr
#'   Optional, non-negative case weight vector to be used during fitting.
#'   If given, must be of same length as observations in task and in corresponding order.
#'   Overwrites weights specified in the \code{task}.
#'   By default missing which means no weights are used unless specified in the task.
#' @param models [\code{logical(1)}]\cr
#'   Should all fitted models be returned?
#'   Default is \code{FALSE}.
#' @param extract [\code{function(model)}]\cr
#'   Function used to extract information from a fitted model during resampling.
#'   Is applied to every \code{\link{WrappedModel}} resulting from calls to \code{\link{train}}
#'   during resampling.
#'   Default is to extract nothing.
#' @param show.info [\code{logical(1)}]\cr
#'   Should a few informative lines about the current resampling iteration and the result be
#'   logged to the R console?
#'   Default is \code{TRUE}.
#' @return List of:
#'   \item{measures.test [\code{data.frame}]}{Rows correspond to test sets in resampling iterations,
#'     columns to performance measures.}
#'   \item{measures.train [\code{data.frame}]}{Rows correspond to training sets in resampling
#'     iterations, columns to performance measures.}
#'   \item{aggr [\code{numeric}]}{Named vector of aggregated performance values. Names are coded like
#'     this <measure>.<aggregation>.}
#'   \item{pred [\code{\link{ResamplePrediction}}]}{Container for all predictions during resampling.}
#'   \item{models [list of \code{\link{WrappedModel}}]}{List of fitted models or \code{NULL}.}
#'   \item{extract [\code{list}]}{List of extracted parts from fitted models or \code{NULL}.}
#' @export
#' @seealso \code{\link{makeResampleDesc}}, \code{\link{makeResampleInstance}}
#' @examples
#' task <- makeClassifTask(data = iris, target = "Species")
#' rdesc <- makeResampleDesc("CV", iters = 2)
#' r <- resample(makeLearner("classif.qda"), task, rdesc)
#' print(r$aggr)
#' print(r$measures.test)
#' print(r$pred)
resample = function(learner, task, resampling, measures, weights, models=FALSE,
  extract, show.info=TRUE) {

  checkArg(learner, "Learner")
  checkArg(task, "SupervisedTask")
  # instantiate resampling
  if (inherits(resampling, "ResampleDesc"))
    resampling = makeResampleInstance(resampling, task=task)
  checkArg(resampling, "ResampleInstance")
  if (missing(measures))
    measures = default.measures(task)
  if (inherits(measures, "Measure"))
    measures = list(measures)
  checkListElementClass(measures, "Measure")
  if (!missing(weights)) {
    checkArg(weights, "numeric", len=task$task.desc$size, na.ok=FALSE, lower=0)
  }
  checkArg(models, "logical", len=1L, na.ok=FALSE)
  if (missing(extract))
    extract = function(model) {}
  else
    checkArg(extract, formals="model")
  checkArg(show.info, "logical", len=1L, na.ok=FALSE)

  n = task$task.desc$size
  r = resampling$size
  if (n != r)
    stop(paste("Size of data set:", n, "and resampling instance:", r, "differ!"))

  checkTaskLearner(task, learner, weights)

  rin = resampling
  more.args = list(learner=learner, task=task, rin=rin,
    measures=measures, model=models, extract=extract, show.info=show.info)
  if (!missing(weights))
    more.args$weights = weights
  else
    more.args$weights = task$weights
  parallelLibrary("mlr", master=FALSE, level="mlr.resample")
  exportMlrOptions()
  iter.results = parallelMap(doResampleIteration, seq_len(rin$desc$iters), level="mlr.resample", more.args=more.args)
  mergeResampleResult(task, iter.results, measures, rin, models, extract, show.info)
}

doResampleIteration = function(learner, task, rin, i, measures, weights, model, extract, show.info) {
  if (show.info)
    messagef("[Resample] %s iter: %i", rin$desc$id, i)
  train.i = rin$train.inds[[i]]
  test.i = rin$test.inds[[i]]

  if (missing(weights))
    m = train(learner, task, subset=train.i)
  else
    m = train(learner, task, subset=train.i, weights=weights[train.i])

  # does a measure require to calculate pred.train?
  ms.train = rep(NA, length(measures))
  ms.test = rep(NA, length(measures))
  pred.train = NULL
  pred.test = NULL
  pp = rin$desc$predict
  if (pp == "train") {
    pred.train = predict(m, task, subset=train.i)
    ms.train = sapply(measures, function(pm) performance(task=task, model=m, pred=pred.train, measures=pm))
  } else if (pp == "test") {
    pred.test = predict(m, task, subset=test.i)
    ms.test = sapply(measures, function(pm) performance(task=task, model=m, pred=pred.test, measures=pm))
  } else { # "both"
    pred.train = predict(m, task, subset=train.i)
    ms.train = sapply(measures, function(pm) performance(task=task, model=m, pred=pred.train, measures=pm))
    pred.test = predict(m, task, subset=test.i)
    ms.test = sapply(measures, function(pm) performance(task=task, model=m, pred=pred.test, measures=pm))
  }
  ex = extract(m)
  list(
    measures.test = ms.test,
    measures.train = ms.train,
    model = if (model) m else NULL,
    pred.test = pred.test,
    pred.train = pred.train,
    extract = ex
  )
}

mergeResampleResult = function(task, iter.results, measures, rin, models, extract, show.info) {
  iters = length(iter.results)
  mids = sapply(measures, function(m) m$id)

  ms.test = extractSubList(iter.results, "measures.test", simplify=FALSE)
  ms.test = as.data.frame(do.call(rbind, ms.test))
  colnames(ms.test) = sapply(measures, function(pm) pm$id)
  rownames(ms.test) = NULL
  ms.test = cbind(iter=seq_len(iters), ms.test)

  ms.train = extractSubList(iter.results, "measures.train", simplify=FALSE)
  ms.train = as.data.frame(do.call(rbind, ms.train))
  colnames(ms.train) = mids
  rownames(ms.train) = NULL
  ms.train = cbind(iter=1:iters, ms.train)

  preds.test = extractSubList(iter.results, "pred.test", simplify=FALSE)
  preds.train = extractSubList(iter.results, "pred.train", simplify=FALSE)
  pred = makeResamplePrediction(instance=rin, preds.test=preds.test, preds.train=preds.train)

  aggr = sapply(measures, function(m)  m$aggr$fun(task, ms.test[, m$id], ms.train[, m$id], m, rin$group, pred))
  names(aggr) = sapply(measures, measureAggrName)
  if (show.info) {
    messagef("[Resample] Result: %s", perfsToString(aggr))
  }
  list(
    measures.train = ms.train,
    measures.test = ms.test,
    aggr = aggr,
    pred = pred,
    models = if(models) lapply(iter.results, function(x) x$model) else NULL,
    extract = if(is.function(extract)) extractSubList(iter.results, "extract", simplify=FALSE) else NULL
  )
}

