#' @title Return task ids used in benchmark.
#'
#' @description
#' Gets the task IDs used in a benchmark experiment.
#'
#' @template arg_bmr
#' @return ([character]).
#' @export
#' @family benchmark
getBMRTaskIds = function(bmr) {
  assertClass(bmr, "BenchmarkResult")
  return(names(bmr$results))
}

#' @title Return learners used in benchmark.
#'
#' @description
#' Gets the learners used in a benchmark experiment.
#'
#' @template arg_bmr
#' @return ([list]).
#' @export
#' @family benchmark
getBMRLearners = function(bmr) {
  assertClass(bmr, "BenchmarkResult")
  return(bmr$learners)
}

#' @title Return learner ids used in benchmark.
#'
#' @description
#' Gets the IDs of the learners used in a benchmark experiment.
#'
#' @template arg_bmr
#' @return ([character]).
#' @export
#' @family benchmark
getBMRLearnerIds = function(bmr) {
  assertClass(bmr, "BenchmarkResult")
  extractSubList(bmr$learners, "id", use.names = FALSE)
}

#' @title Return learner short.names used in benchmark.
#'
#' @description
#' Gets the learner short.names of the learners used in a benchmark experiment.
#'
#' @template arg_bmr
#' @return ([character]).
#' @export
#' @family benchmark
getBMRLearnerShortNames = function(bmr) {
  assertClass(bmr, "BenchmarkResult")
  vcapply(bmr$learners, getLearnerShortName, use.names = FALSE)
}

#' @title Return measures used in benchmark.
#'
#' @description
#' Gets the measures used in a benchmark experiment.
#'
#' @template arg_bmr
#' @return ([list]). See above.
#' @export
#' @family benchmark
getBMRMeasures = function(bmr) {
  assertClass(bmr, "BenchmarkResult")
  return(bmr$measures)
}

#' @title Return measures IDs used in benchmark.
#'
#' @description
#' Gets the IDs of the measures used in a benchmark experiment.
#'
#' @template arg_bmr
#' @return ([list]). See above.
#' @export
#' @family benchmark
getBMRMeasureIds = function(bmr) {
  assertClass(bmr, "BenchmarkResult")
  extractSubList(bmr$measures, "id", use.names = FALSE)
}

# returns buried object in BMR, either as list of lists or data.frame with task.id, learner.id cols
# you can restrict to subsets for tasks and learners and pass function to extract object
getBMRObjects = function(bmr, task.ids = NULL, learner.ids = NULL, fun, as.df = FALSE, drop = FALSE) {
  assertClass(bmr, "BenchmarkResult")
  brtids = getBMRTaskIds(bmr)
  brlids = getBMRLearnerIds(bmr)
  if (is.null(task.ids)) {
    task.ids = brtids
  } else {
    assertSubset(task.ids, brtids)
  }
  if (is.null(learner.ids)) {
    learner.ids = brlids
  } else {
    assertSubset(learner.ids, brlids)
  }
  res = lapply(task.ids, function(tid) {
    xs = lapply(learner.ids, function(lid) {
      p = fun(bmr$results[[tid]][[lid]])
      if (as.df) {
        if (!is.null(p)) {
          p = as.data.frame(cbind(task.id = tid, learner.id = lid, p))
        }
      }
      return(p)
    })
    if (as.df) {
      xs = setDF(rbindlist(xs, fill = TRUE, use.names = TRUE))
    } else {
      xs = setNames(xs, learner.ids)
    }
    return(xs)
  })
  if (as.df) {
    res = setDF(rbindlist(res, fill = TRUE, use.names = TRUE))
  } else {
    res = setNames(res, task.ids)
    if (drop) {
      # when drop is on we check if learner.ids and/or task.ids are of length 1
      # if so the list is unnested
      drop.tasks = length(task.ids) == 1L
      drop.learners = length(learner.ids) == 1L
      if (drop.tasks | drop.learners) {
        res = unlist(res, recursive = FALSE)
        if (drop.tasks & drop.learners) {
          res = res[[1L]]
        }
        if (drop.tasks & !drop.learners) {
          res = setNames(res, learner.ids)
        }
        if (!drop.tasks & drop.learners) {
          res = setNames(res, task.ids)
        }
      }
    }
  }
  return(res)
}


#' @title Extract the predictions from a benchmark result.
#'
#' @description
#' Either a list of lists of [ResamplePrediction] objects, as returned by
#' [resample], or these objects are rbind-ed with extra columns
#' \dQuote{task.id} and \dQuote{learner.id}.
#'
#' If `predict.type` is \dQuote{prob}, the probabilities for each class are returned in addition to the response.
#'
#' If `keep.pred` is `FALSE` in the call to [benchmark], the function will return `NULL`.
#'
#' @template arg_bmr
#' @template arg_bmr_taskids
#' @template arg_bmr_learnerids
#' @template arg_bmr_asdf
#' @template arg_bmr_drop
#' @template ret_bmr_list_or_df
#' @export
#' @family benchmark

getBMRPredictions = function(bmr, task.ids = NULL, learner.ids = NULL, as.df = FALSE, drop = FALSE) {
  assertClass(bmr, "BenchmarkResult")
  f = if (as.df) {
    function(x) as.data.frame(getRRPredictions(x))
  } else {
    function(x) getRRPredictions(x)
  }
  getBMRObjects(bmr, task.ids, learner.ids, fun = f, as.df = as.df, drop = drop)
}


#' @title Extract the test performance values from a benchmark result.
#'
#' @description
#' Either a list of lists of \dQuote{measure.test} data.frames, as returned by
#' [resample], or these objects are rbind-ed with extra columns
#' \dQuote{task.id} and \dQuote{learner.id}.
#'
#'
#' @template arg_bmr
#' @template arg_bmr_taskids
#' @template arg_bmr_learnerids
#' @template arg_bmr_asdf
#' @template arg_bmr_drop
#' @template ret_bmr_list_or_df
#' @export
#' @family benchmark
getBMRPerformances = function(bmr, task.ids = NULL, learner.ids = NULL, as.df = FALSE, drop = FALSE) {
  assertClass(bmr, "BenchmarkResult")
  f = function(x) x$measures.test
  getBMRObjects(bmr, task.ids, learner.ids, fun = f, as.df = as.df, drop = drop)
}

#' @title Extract the aggregated performance values from a benchmark result.
#'
#' @description
#' Either a list of lists of \dQuote{aggr} numeric vectors, as returned by
#' [resample], or these objects are rbind-ed with extra columns
#' \dQuote{task.id} and \dQuote{learner.id}.
#'
#'
#' @template arg_bmr
#' @template arg_bmr_taskids
#' @template arg_bmr_learnerids
#' @template arg_bmr_asdf
#' @template arg_bmr_drop
#' @template ret_bmr_list_or_df
#' @export
#' @family benchmark
getBMRAggrPerformances = function(bmr, task.ids = NULL, learner.ids = NULL, as.df = FALSE, drop = FALSE) {
  assertClass(bmr, "BenchmarkResult")
  f = if (as.df) {
    function(x) as.data.frame(as.list(x$aggr))
  } else {
    function(x) x$aggr
  }
  getBMRObjects(bmr, task.ids, learner.ids, fun = f, as.df = as.df, drop = drop)
}


getBMROptResults = function(bmr, task.ids = NULL, learner.ids = NULL, as.df = FALSE,
  wrapper.class, fun, drop = FALSE) {
  f = if (as.df) {
    function(x) {
      if (inherits(x$learner, wrapper.class)) {
        xs = lapply(x$extract, fun)
        xs = setDF(rbindlist(lapply(seq_along(xs), function(i) cbind(iter = i, xs[[i]])), fill = TRUE, use.names = TRUE))
      } else {
        NULL
      }
    }
  } else {
    function(x) {
      if (inherits(x$learner, wrapper.class)) {
        x$extract
      } else {
        NULL
      }
    }
  }
  getBMRObjects(bmr, task.ids, learner.ids, fun = f, as.df = as.df, drop = drop)
}

#' @title Extract the tuning results from a benchmark result.
#'
#' @description
#' Returns a nested list of [TuneResult]s. The first level of nesting is by data set, the second by learner, the third for the benchmark resampling iterations. If `as.df` is `TRUE`, a data frame with the \dQuote{task.id}, \dQuote{learner.id}, the resample iteration, the parameter values and the performances is returned.
#'
#' @template arg_bmr
#' @template arg_bmr_taskids
#' @template arg_bmr_learnerids
#' @template arg_bmr_asdf
#' @template arg_bmr_drop
#' @template ret_bmr_list_or_df
#' @export
#' @family benchmark
getBMRTuneResults = function(bmr, task.ids = NULL, learner.ids = NULL, as.df = FALSE, drop = FALSE) {
  assertClass(bmr, "BenchmarkResult")
  getBMROptResults(bmr, task.ids, learner.ids, as.df, "TuneWrapper", function(x) {
    data.frame(x$x, as.list(x$y))
  }, drop = drop)
}

#' @title Extract the feature selection results from a benchmark result.
#'
#' @description
#' Returns a nested list of [FeatSelResult]s. The first level of nesting is by data set, the second by learner, the third for the benchmark resampling iterations. If `as.df` is `TRUE`, a data frame with \dQuote{task.id}, \dQuote{learner.id}, the resample iteration and the selected features is returned.
#'
#' Note that if more than one feature is selected and a data frame is requested, there will be multiple rows for the same dataset-learner-iteration; one for each selected feature.
#'
#' @template arg_bmr
#' @template arg_bmr_taskids
#' @template arg_bmr_learnerids
#' @template arg_bmr_asdf
#' @template arg_bmr_drop
#' @template ret_bmr_list_or_df
#' @export
#' @family benchmark
getBMRFeatSelResults = function(bmr, task.ids = NULL, learner.ids = NULL, as.df = FALSE, drop = FALSE) {
  assertClass(bmr, "BenchmarkResult")
  getBMROptResults(bmr, task.ids, learner.ids, as.df, "FeatSelWrapper", function(x) {
    as.data.frame(x$x)
  }, drop = drop)
}

#' @title Extract the feature selection results from a benchmark result.
#'
#' @description
#' Returns a nested list of characters The first level of nesting is by data set, the second by learner, the third for the benchmark resampling iterations. The list at the lowest level is the list of selected features. If `as.df` is `TRUE`, a data frame with \dQuote{task.id}, \dQuote{learner.id}, the resample iteration and the selected features is returned.
#'
#' Note that if more than one feature is selected and a data frame is requested, there will be multiple rows for the same dataset-learner-iteration; one for each selected feature.
#'
#' @template arg_bmr
#' @template arg_bmr_taskids
#' @template arg_bmr_learnerids
#' @template arg_bmr_asdf
#' @template arg_bmr_drop
#' @template ret_bmr_list_or_df
#' @export
#' @family benchmark
getBMRFilteredFeatures = function(bmr, task.ids = NULL, learner.ids = NULL, as.df = FALSE, drop = FALSE) {
  assertClass(bmr, "BenchmarkResult")
  getBMROptResults(bmr, task.ids, learner.ids, as.df, "FilterWrapper", function(x) {
    as.data.frame(x)
  }, drop = drop)
}

#' @title Extract all models from benchmark result.
#'
#' @description
#' A list of lists containing all [WrappedModel]s trained in the benchmark experiment.
#'
#' If `models` is `FALSE` in the call to [benchmark], the function will return `NULL`.
#'
#' @template arg_bmr
#' @template arg_bmr_taskids
#' @template arg_bmr_learnerids
#' @template arg_bmr_drop
#' @return ([list]).
#' @export
#' @family benchmark
getBMRModels = function(bmr, task.ids = NULL, learner.ids = NULL, drop = FALSE) {
  assertClass(bmr, "BenchmarkResult")
  f = function(x) {
    x$models
  }
  getBMRObjects(bmr, task.ids, learner.ids, fun = f, as.df = FALSE, drop = drop)
}

#' @title Extract all task descriptions from benchmark result (DEPRECATED).
#'
#' @description
#' A list containing all [TaskDesc]s for each task contained in the benchmark experiment.
#' @template arg_bmr
#' @return ([list]).
#' @export
getBMRTaskDescriptions = function(bmr) {
  .Deprecated("getBMRTaskDesc")
  getBMRTaskDescs(bmr)
}


#' @title Extract all task descriptions from benchmark result.
#'
#' @description
#' A list containing all [TaskDesc]s for each task contained in the benchmark experiment.
#' @template arg_bmr
#' @return ([list]).
#' @export
#' @family benchmark
getBMRTaskDescs = function(bmr) {
  lapply(bmr$results, function(x) lapply(x, getRRTaskDesc))
}
