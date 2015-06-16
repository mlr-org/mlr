#' @title Return task ids used in benchmark.
#'
#' @template arg_bmr
#' @return [\code{character}].
#' @export
#' @family benchmark
getBMRTaskIds = function(bmr) {
  assertClass(bmr, "BenchmarkResult")
  return(names(bmr$results))
}

#' @title Return learners used in benchmark.
#'
#' @template arg_bmr
#' @return [\code{list}].
#' @export
#' @family benchmark
getBMRLearners = function(bmr) {
  assertClass(bmr, "BenchmarkResult")
  return(bmr$learners)
}

#' @title Return learner ids used in benchmark.
#'
#' @template arg_bmr
#' @return [\code{character}].
#' @export
#' @family benchmark
getBMRLearnerIds = function(bmr) {
  assertClass(bmr, "BenchmarkResult")
  extractSubList(bmr$learners, "id", use.names = FALSE)
}

#' @title Return measures used in benchmark.
#'
#' @template arg_bmr
#' @return [\code{list}]. See above.
#' @export
#' @family benchmark
getBMRMeasures = function(bmr) {
  assertClass(bmr, "BenchmarkResult")
  return(bmr$measures)
}

#' @title Return measures used in benchmark.
#'
#' @template arg_bmr
#' @return [\code{list}]. See above.
#' @export
#' @family benchmark
getBMRMeasureIds = function(bmr) {
  assertClass(bmr, "BenchmarkResult")
  extractSubList(bmr$measures, "id", use.names = FALSE)
}

# returns buried object in BMR, either as list of lists or data.frame with task.id, learner.id cols
# you can restrict to subsets for tasks and learners and pass function to extract object
getBMRObjects = function(bmr, task.ids = NULL, learner.ids = NULL, fun, as.df = FALSE) {
  brtids = getBMRTaskIds(bmr)
  brlids = getBMRLearnerIds(bmr)
  if (is.null(task.ids))
    task.ids = brtids
  else
    assertSubset(task.ids, brtids)
  if (is.null(learner.ids))
    learner.ids = brlids
  else
    assertSubset(learner.ids, brlids)
  res = lapply(task.ids, function(tid) {
    xs = lapply(learner.ids, function(lid) {
      p = fun(bmr$results[[tid]][[lid]])
      if (as.df) {
        if (!is.null(p))
          p = as.data.frame(cbind(task.id = tid, learner.id = lid, p))
      }
      return(p)
    })
    if (as.df)
      xs = do.call(rbind.fill, xs)
    else
      xs = setNames(xs, learner.ids)
    return(xs)
  })
  if (as.df)
    res = do.call(rbind.fill, res)
  else
    res = setNames(res, task.ids)
  return(res)
}


#' @title Extract the predictions from a benchmark result.
#'
#' @description
#' Either a list of lists of \code{\link{ResamplePrediction}} objects, as returned by
#' \code{\link{resample}}, or these objects are rbind-ed with extra columns
#' \dQuote{task.id} and \dQuote{learner.id}.
#' @template arg_bmr
#' @template arg_bmr_taskids
#' @template arg_bmr_learnerids
#' @template arg_bmr_asdf
#' @template ret_bmr_list_or_df
#' @export
#' @family benchmark

#FIXME: rbind.fill is stupid. this will not work for probs,
#FIXME: simply take response, offer to get as list for rest
# in as.data.frame allow to select cols / only response
# FIXME: at least have an option to only take rsponse?
getBMRPredictions = function(bmr, task.ids = NULL, learner.ids = NULL, as.df = FALSE) {
  f = if (as.df)
    function(x) as.data.frame(getRRPredictions(x))
  else
    function(x) getRRPredictions(x)
  getBMRObjects(bmr, task.ids, learner.ids, fun = f, as.df = as.df)
}


#' @title Extract the test performance values from a benchmark result.
#'
#' @description
#' Either a list of lists of \dQuote{measure.test} data.frames, as returned by
#' \code{\link{resample}}, or these objects are rbind-ed with extra columns
#' \dQuote{task.id} and \dQuote{learner.id}.
#'
#' @template arg_bmr
#' @template arg_bmr_taskids
#' @template arg_bmr_learnerids
#' @template arg_bmr_asdf
#' @template ret_bmr_list_or_df
#' @export
#' @family benchmark
getBMRPerformances = function(bmr, task.ids = NULL, learner.ids = NULL, as.df = FALSE) {
  f = function(x) x$measures.test
  getBMRObjects(bmr, task.ids, learner.ids, fun = f, as.df = as.df)
}

#' @title Extract the aggregated performance values from a benchmark result.
#'
#' @description
#' Either a list of lists of \dQuote{aggr} numeric vectors, as returned by
#' \code{\link{resample}}, or these objects are rbind-ed with extra columns
#' \dQuote{task.id} and \dQuote{learner.id}.
#'
#' @template arg_bmr
#' @template arg_bmr_taskids
#' @template arg_bmr_learnerids
#' @template arg_bmr_asdf
#' @template ret_bmr_list_or_df
#' @export
#' @family benchmark
getBMRAggrPerformances = function(bmr, task.ids = NULL, learner.ids = NULL, as.df = FALSE) {
  f = if (as.df)
    function(x) as.data.frame(as.list(x$aggr))
  else
    function(x) x$aggr
  getBMRObjects(bmr, task.ids, learner.ids, fun = f, as.df = as.df)
}


getBMROptResults = function(bmr, task.ids = NULL, learner.ids = NULL, as.df = FALSE,
  wrapper.class, fun) {

  f = if (as.df) {
    function(x) {
      niters = nrow(x$measures.test)
      if (inherits(x$learner, wrapper.class)) {
        # FIXME: this wont work for vector params?
        xs = lapply(x$extract, fun)
        cbind(iter = 1:niters, do.call(rbind.fill, xs))
      } else {
        NULL
      }
    }
  } else {
    function(x) {
      if (inherits(x$learner, wrapper.class))
        x$extract
      else
        NULL
    }
  }
  getBMRObjects(bmr, task.ids, learner.ids, fun = f, as.df = as.df)
}

#' @title Extract the tuning results from a benchmark result.
#'
#' @description
#' Returns a list of lists of ??? as returned by
#' \code{\link{resample}}, or these objects are rbind-ed with extra columns
#' \dQuote{task.id} and \dQuote{learner.id}.
#'
#' @template arg_bmr
#' @template arg_bmr_taskids
#' @template arg_bmr_learnerids
#' @template arg_bmr_asdf
#' @template ret_bmr_list_or_df
#' @export
#' @family benchmark
getBMRTuneResults = function(bmr, task.ids = NULL, learner.ids = NULL, as.df = FALSE) {
  getBMROptResults(bmr, task.ids, learner.ids, as.df, "TuneWrapper", function(x) {
    as.data.frame(x$x)
  })
}

#' @title Extract the feature selection results from a benchmark result.
#'
#' @description
#' Returns a list of lists of ???? data.frames, as returned by
#' \code{\link{resample}}, or these objects are rbind-ed with extra columns
#' \dQuote{task.id} and \dQuote{learner.id}.
#'
#' @template arg_bmr
#' @template arg_bmr_taskids
#' @template arg_bmr_learnerids
#' @template arg_bmr_asdf
#' @template ret_bmr_list_or_df
#' @export
#' @family benchmark
getBMRFeatSelResults = function(bmr, task.ids = NULL, learner.ids = NULL, as.df = FALSE) {
  getBMROptResults(bmr, task.ids, learner.ids, as.df, "FeatSelWrapper", function(x) {
    as.data.frame(x$x)
  })
}

#' @title Extract the feature selection results from a benchmark result.
#'
#' @description
#' Returns a list of lists of ??? data.frames, as returned by
#' \code{\link{resample}}, or these objects are rbind-ed with extra columns
#' \dQuote{task.id} and \dQuote{learner.id}.
#'
#' @template arg_bmr
#' @template arg_bmr_taskids
#' @template arg_bmr_learnerids
#' @template arg_bmr_asdf
#' @template ret_bmr_list_or_df
#' @export
#' @family benchmark
getBMRFilteredFeatures = function(bmr, task.ids = NULL, learner.ids = NULL, as.df = FALSE) {
  getBMROptResults(bmr, task.ids, learner.ids, as.df, "FilterWrapper", function(x) {
    as.data.frame(x$x)
  })
}
