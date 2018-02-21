#' @title Supported parallelization methods
#'
#' @description
#' mlr supports different methods to activate parallel computing capabilities through the integration of the [parallelMap::parallelMap] package, which supports all major parallelization backends for R.
#' You can start parallelization with \code{\link[parallelMap]{parallelStart}*}, where `*` should be replaced with the chosen backend.
#' [parallelMap::parallelStop] is used to stop all parallelization backends.
#'
#' Parallelization is divided into different levels and will automatically be carried out for the first level that occurs, e.g. if you call `resample()` after [parallelMap::parallelStart], each resampling iteration is a parallel job and possible underlying calls like parameter tuning won't be parallelized further.
#'
#' The supported levels of parallelization are:
#' \describe{
#'   \item{`"mlr.resample"`}{Each resampling iteration (a train/test step) is a parallel job.}
#'  \item{`"mlr.benchmark"`}{Each experiment "run this learner on this data set" is a parallel job.}
#'  \item{`"mlr.tuneParams"`}{Each evaluation in hyperparameter space "resample with these parameter settings" is a parallel job.
#'   How many of these can be run independently in parallel depends on the tuning algorithm.
#'   For grid search or random search there is no limit, but for other tuners it depends on how many points to evaluate are produced in each iteration of the optimization.
#'   If a tuner works in a purely sequential fashion, we cannot work magic and the hyperparameter evaluation will also run sequentially. But note that you can still parallelize the underlying resampling.}
#'   \item{`"mlr.selectFeatures"`}{Each evaluation in feature space "resample with this feature subset" is a parallel job. The same comments as for `"mlr.tuneParams"` apply here.}
#'   \item{`"mlr.ensemble"`}{For all ensemble methods, the training and prediction of each individual learner is a parallel job.
#'   Supported ensemble methods are the [makeBaggingWrapper], [makeCostSensRegrWrapper], [makeMulticlassWrapper], [makeMultilabelBinaryRelevanceWrapper] and the [makeOverBaggingWrapper].}
#' }
#'
#'
#' @name parallelization
#' @rdname parallelization
NULL
