#' @title Get the resampling indices from a tuning or feature selection wrapper..
#'
#' @description
#' After you resampled a tuning or feature selection wrapper (see [makeTuneWrapper])
#' with `resample(..., extract = getTuneResult)` or `resample(..., extract = getFeatSelResult)` this helper returns a `list` with
#' the resampling indices used for the respective method.
#'
#' @importFrom purrr map flatten set_names
#' @param object ([ResampleResult]) \cr
#'   The result of resampling of a tuning or feature selection wrapper.
#' @param inner ([logical]) \cr
#'   If `TRUE`, returns the inner indices of a nested resampling setting.
#' @return ([list]). One list for each outer resampling fold.
#' @family tune
#' @examples
#' task = makeClassifTask(data = iris, target = "Species")
#' lrn = makeLearner("classif.rpart")
#' # stupid mini grid
#' ps = makeParamSet(
#'   makeDiscreteParam("cp", values = c(0.05, 0.1)),
#'   makeDiscreteParam("minsplit", values = c(10, 20))
#'  )
#' ctrl = makeTuneControlGrid()
#' inner = makeResampleDesc("Holdout")
#' outer = makeResampleDesc("CV", iters = 2)
#' lrn = makeTuneWrapper(lrn, resampling = inner, par.set = ps, control = ctrl)
#' # nested resampling for evaluation
#' # we also extract tuned hyper pars in each iteration and by that the resampling indices
#' r = resample(lrn, task, outer, extract = getTuneResult)
#' # get tuning indices
#' getResamplingIndices(r, inner = TRUE)
#'
#' @export
getResamplingIndices = function(object, inner = FALSE) {
  assertClass(object, "ResampleResult")
  assertList(object$extract)
  if (inner == TRUE) {
    if (!inherits(object$extract[[1]], c("TuneResult", "FeatSelResult"))) {
      stopf("No object of class 'TuneResult' or 'FeatuSelResult' found in slot 'extract'.
             Did you run 'resample()' with 'extract = getTuneResult' or 'extract = getFeatSelResult'?")
    }
    inner_inds = lapply(object$extract, function(x) x$resampling[c("train.inds", "test.inds")])

    outer_inds = object$pred$instance[c("train.inds", "test.inds")]

    # now translate the inner inds back to the outer inds so we have the correct indices https://github.com/mlr-org/mlr/issues/2409

    inner_inds_translated = map(1:length(inner_inds), function(z) # map over number of outer folds

      set_names(
        map(c("train.inds", "test.inds"), function(u) # map over train/test level

          # list() -> create list for "train.inds" and "test.inds"
          # flatten() -> reduce by one level
          # set_names(c("train.inds", "test.inds")) -> now set list names
          flatten(
            map(inner_inds[[z]][[u]], ~ # map over number of inner folds
                  list(outer_inds[["train.inds"]][[z]][.x]) # the inner test.inds are a subset of the outer train.inds! That's why "train.inds" is hardcoded here
            )
          )
        ),
        c("train.inds", "test.inds")
      )
    )

    return(inner_inds_translated)

  } else {
    object$pred$instance[c("train.inds", "test.inds")]
  }
}
