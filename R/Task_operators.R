#' Get a summarizing task description.
#'
#' @template arg_task_or_desc
#' @return [\code{\link{TaskDesc}}].
#' @export
#' @family task
getTaskDescription = function(x) {
  UseMethod("getTaskDescription")
}

#' @export
getTaskDescription.default = function(x) {
  x$task.desc
}

#' @export
getTaskDescription.TaskDesc = function(x) {
  x
}

#' Get the type of the task.
#'
#' @template arg_task_or_desc
#' @return [\code{character(1)}].
#' @export
#' @family task
getTaskType = function(x) {
  getTaskDescription(x)$type
}

#' Get the id of the task.
#'
#' @template arg_task_or_desc
#' @return [\code{character(1)}].
#' @export
#' @family task
getTaskId = function(x) {
  getTaskDescription(x)$id
}

#' @title Get the name(s) of the target column(s).
#'
#' @description
#' NB: For multilabel, \code{\link{getTaskTargetNames}} and \code{\link{getTaskClassLevels}}
#' actually return the same thing.
#'
#' @template arg_task_or_desc
#' @return [\code{character}].
#' @export
#' @family task
getTaskTargetNames = function(x) {
  UseMethod("getTaskTargetNames")
}

#' @export
getTaskTargetNames.Task = function(x) {
  getTaskTargetNames(getTaskDescription(x))
}

#' @export
getTaskTargetNames.TaskDescSupervised = function(x) {
  x$target
}

#' @export
getTaskTargetNames.TaskDescUnsupervised = function(x) {
  character(0L)
}


#' @title Get the class levels for classification and multilabel tasks.
#'
#' @description
#' NB: For multilabel, \code{\link{getTaskTargetNames}} and \code{\link{getTaskClassLevels}}
#' actually return the same thing.
#'
#' @template arg_task_or_desc
#' @return [\code{character}].
#' @export
#' @family task
getTaskClassLevels = function(x) {
  UseMethod("getTaskClassLevels")
}

#' @export
getTaskClassLevels.Task = function(x) {
  getTaskClassLevels(getTaskDescription(x))
}

#' @export
getTaskClassLevels.TaskDescClassif = function(x) {
  getTaskDescription(x)$class.levels
}

#' @export
getTaskClassLevels.TaskDescMultilabel = function(x) {
  getTaskDescription(x)$class.levels
}

#' Get feature names of task.
#'
#' Target column name is not included.
#'
#' @template arg_task
#' @return [\code{character}].
#' @family task
#' @export
getTaskFeatureNames = function(task) {
  setdiff(names(task$env$data), getTaskDescription(task)$target)
}

#' Get number of features in task.
#'
#' @template arg_task_or_desc
#' @return [\code{integer(1)}].
#' @export
#' @family task
getTaskNFeats = function(x) {
  sum(getTaskDescription(x)$n.feat)
}

#' Get number of observations in task.
#'
#' @template arg_task_or_desc
#' @return [\code{integer(1)}].
#' @export
#' @family task
getTaskSize = function(x) {
  getTaskDescription(x)$size
}

#' @title Get formula of a task.
#'
#' @description
#' This is usually simply \dQuote{<target> ~ .}.
#' For multilabel it is \dQuote{<target_1> + ... + <target_k> ~ .}.
#'
#' @template arg_task_or_desc
#' @param target [\code{character(1)}]\cr
#'   Left hand side of the formula.
#'   Default is defined by task \code{x}.
#' @param explicit.features [\code{logical(1)}]\cr
#'   Should the features (right hand side of the formula) be explicitly listed?
#'   Default is \code{FALSE}, i.e., they will be represented as \code{"."}.
#' @param env [\code{environment}]\cr
#'   Environment of the formula.
#'   Default is \code{parent.frame()}.
#' @return [\code{formula}].
#' @family task
#' @export
getTaskFormula = function(x, target = getTaskTargetNames(x), explicit.features = FALSE, env = parent.frame()) {
  td = getTaskDescription(x)
  type = td$type
  if (type == "surv") {
    lookup = setNames(c("left", "right", "interval2"), c("lcens", "rcens", "icens"))
    target = sprintf("Surv(%s, %s, type = \"%s\")", target[1L], target[2L], lookup[td$censoring])
  } else if (type == "multilabel") {
    target = collapse(target, "+")
  } else if (type == "costsens") {
    stop("There is no formula available for cost-sensitive learning.")
  } else if (type == "cluster") {
    stop("There is no formula available for clustering.")
  }
  if (explicit.features) {
    if (!inherits(x, "Task"))
      stopf("'explicit.features' can only be used when 'x' is of type 'Task'!")
    features = getTaskFeatureNames(x)
  } else {
    features = "."
  }
  # FIXME in the future we might want to create formulas w/o an environment
  # currently this is impossible for survival because the namespace is not imported
  # properly in many packages -> survival::Surv not found
  as.formula(paste(target, "~", paste(features, collapse = " + ")), env = env)
}

#' @title Get target data of task.
#'
#' @description
#' Get target data of task.
#'
#' @template arg_task
#' @param recode.target [\code{character(1)}] \cr
#'   Should target classes be recoded? Only for binary classification.
#'   Possible are \dQuote{no} (do nothing), \dQuote{01}, and \dQuote{-1+1}.
#'   In the two latter cases the target vector is converted into a numeric vector.
#'   The positive class is coded as +1 and the negative class either as 0 or -1.
#'   Default is \dQuote{no}.
#' @return A \code{factor} for classification or a \code{numeric} for regression, a data.frame
#'   of logical columns for multilabel.
#' @family task
#' @export
#' @examples
#' task = makeClassifTask(data = iris, target = "Species")
#' getTaskTargets(task)
getTaskTargets = function(task, recode.target = "no") {
  UseMethod("getTaskTargets")
}

#' @export
getTaskTargets.SupervisedTask = function(task, recode.target = "no") {
  y = task$env$data[, task$task.desc$target, drop = TRUE]
  recodeY(y, recode.target, task$task.desc)
}

#' @export
getTaskTargets.UnsupervisedTask = function(task, recode.target = "no") {
  stop("There is no target available for unsupervised tasks.")
}

#' @export
getTaskTargets.CostSensTask = function(task, recode.target = "no") {
  stop("There is no target available for costsens tasks.")
}


#' Extract data in task.
#'
#' Useful in \code{\link{trainLearner}} when you add a learning machine to the package.
#'
#' @template arg_task
#' @param subset [\code{integer}]\cr
#'   Selected cases.
#'   Default is all cases.
#' @param features [\code{character}]\cr
#'   Selected features.
#'   Default is all.
#' @param target.extra [\code{logical(1)}]\cr
#'   Should target vector be returned separately?
#'   If not, a single data.frame including the target columns is returned, otherwise a list
#'   with the input data.frame and an extra vector or data.frame for the targets.
#'   Default is \code{FALSE}.
#' @param recode.target [\code{character(1)}]\cr
#'   Should target classes be recoded? Supported are binary classification and survival.
#'   Possible values for binary classification are \dQuote{01}, \dQuote{-1+1} and \dQuote{drop.levels}.
#'   In the two latter cases the target vector is converted into a numeric vector.
#'   The positive class is coded as \dQuote{+1} and the negative class either as \dQuote{0} or \dQuote{-1}.
#'   \dQuote{drop.levels} will remove empty factor levels in the target column.
#'   For survival, you may choose to recode the survival times to \dQuote{left}, \dQuote{right} or \dQuote{interval2} censored times
#'   using \dQuote{lcens}, \dQuote{rcens} or \dQuote{icens}, respectively.
#'   See \code{\link[survival]{Surv}} for the format specification.
#'   Default for both binary classification and survival is \dQuote{no} (do nothing).
#' @return Either a data.frame or a list with data.frame \code{data} and vector \code{target}.
#' @family task
#' @export
#' @examples
#' library("mlbench")
#' data(BreastCancer)
#'
#' df = BreastCancer
#' df$Id = NULL
#' task = makeClassifTask(id = "BreastCancer", data = df, target = "Class", positive = "malignant")
#' head(getTaskData)
#' head(getTaskData(task, features = c("Cell.size", "Cell.shape"), recode.target = "-1+1"))
#' head(getTaskData(task, subset = 1:100, recode.target = "01"))
getTaskData = function(task, subset, features, target.extra = FALSE, recode.target = "no") {
  #FIXME: argument checks currently not done for speed
  indexHelper = function(df, i, j, drop = TRUE) {
    switch(2L * is.null(i) + is.null(j) + 1L,
      df[i, j, drop = drop],
      df[i,  , drop = drop],
      df[ , j, drop = drop],
      df
    )
  }

  tn = task$task.desc$target
  task.features = getTaskFeatureNames(task)
  if (missing(subset) || identical(subset, seq_len(task$task.desc$size)))
    subset = NULL

  if (target.extra) {
    if (missing(features))
      features = task.features
    res = list(
      data = indexHelper(task$env$data, subset, setdiff(features, tn), drop = FALSE),
      target = recodeY(indexHelper(task$env$data, subset, tn), type = recode.target, task$task.desc)
    )
  } else {
    if (missing(features) || identical(features, task.features))
      features = NULL
    else
      features = union(features, tn)

    res = indexHelper(task$env$data, subset, features, drop = FALSE)
    if (recode.target %nin% c("no", "surv")) {
      res[, tn] = recodeY(res[, tn], type = recode.target, task$task.desc)
    }
  }
  res
}

recodeY = function(y, type, td) {
  if (type == "no")
    return(y)
  if (type == "drop.levels")
    return(factor(y))
  if (type == "01")
    return(as.numeric(y == td$positive))
  if (type == "-1+1")
    return(as.numeric(2L * (y == td$positive) - 1L))
  if (type %in% c("lcens", "rcens", "icens"))
    return(recodeSurvivalTimes(y, from = td$censoring, to = type))
  stopf("Unknown value for 'type': %s", type)
}

recodeSurvivalTimes = function(y, from, to) {
  is.neg.infinite = function(x) is.infinite(x) & x < 0
  is.pos.infinite = function(x) is.infinite(x) & x > 0
  lookup = setNames(c("left", "right", "interval2"), c("lcens", "rcens", "icens"))

  if (from == to)
    return(Surv(y[, 1L], y[, 2L], type = lookup[to]))
  if (setequal(c(from, to), c("lcens", "rcens")))
    stop("Converting left censored to right censored data (or vice versa) is not possible")

  switch(from,
    rcens = {
      time1 = y[, 1L]
      time2 = ifelse(y[, 2L], y[, 1L], Inf)
    },
    lcens = {
      time1 = ifelse(y[, 2L], y[, 1L], -Inf)
      time2 = y[, 1L]
    },
    icens = {
      if (to == "lcens") {
        if (!all(is.neg.infinite(y[, 1L] | y[, 1L] == y[, 2L])))
          stop("Could not convert interval2 survival data to left censored data")
        time1 = y[, 2L]
        time2 = is.infinite(y[, 1L])
      } else {
        if (!all(is.pos.infinite(y[, 2L] | y[, 2L] == y[, 1L])))
          stop("Could not convert interval2 survival data to right censored data")
        time1 = y[, 1L]
        time2 = is.infinite(y[, 2L])
      }
    }
  )
  Surv(time1, time2, type = lookup[to])
}

#' Extract costs in task.
#'
#' Retuns \dQuote{NULL} if the task is not of type \dQuote{costsens}.
#'
#' @param task [\code{\link{CostSensTask}}]\cr
#'   The task.
#' @param subset [\code{integer}]\cr
#'   Selected cases.
#'   Default is all cases.
#' @return [\code{matrix} | \code{NULL}].
#' @family task
#' @export
getTaskCosts = function(task, subset) {
  if (task$task.desc$type != "costsens")
    return(NULL)
  ms = missing(subset) || identical(subset, seq_len(task$task.desc$size))
  d = if (ms)
    task$env$costs
  else
    task$env$costs[subset, , drop = FALSE]
  return(d)
}


#' Subset data in task.
#'
#' @template arg_task
#' @param subset [\code{integer} | \code{logical(n)}]\cr
#'   Selected cases.
#'   Default is all cases.
#' @param features [\code{character}]\cr
#'   Selected inputs. Note that target feature is always included in the
#'   resulting task, you should not pass it here.
#'   Default is all features.
#' @return [\code{\link{Task}}]. Task with subsetted data.
#' @family task
#' @export
#' @examples
#' task = makeClassifTask(data = iris, target = "Species")
#' subsetTask(task, subset = 1:100)
subsetTask = function(task, subset, features) {
  # FIXME: we recompute the taskdesc for each subsetting. do we want that? speed?
  # FIXME: maybe we want this independent of changeData?
  td = task$desc
  if (!missing(subset))
    assert(checkIntegerish(subset), checkLogical(subset, len = td$size))
  task = changeData(task, getTaskData(task, subset, features), getTaskCosts(task, subset), task$weights)
  if (!missing(subset)) {
    if (task$task.desc$has.blocking)
      task$blocking = task$blocking[subset]
    if (task$task.desc$has.weights)
      task$weights = task$weights[subset]
  }
  return(task)
}


# we create a new env, so the reference is not changed
changeData = function(task, data, costs, weights) {
  if (missing(data))
    data = getTaskData(task)
  if (missing(costs))
    costs = getTaskCosts(task)
  if (missing(weights))
    weights = task$weights
  task$env = new.env(parent = emptyenv())
  task$env$data = data
  # FIXME: I hate R, this is all bad
  if (!is.null(costs))
    task$env$costs = costs
  if (is.null(weights))
    task["weights"] = list(NULL)
  else
    task$weights = weights
  td = task$task.desc
  # FIXME: this is bad style but I see no other way right now
  task$task.desc = switch(td$type,
    "classif" = makeTaskDesc(task, td$id, td$target, td$positive),
    "surv" = makeTaskDesc(task, td$id, td$target, td$censoring),
    "cluster" = makeTaskDesc(task, td$id),
    makeTaskDesc(task, td$id, td$target))
  return(task)
}


# returns factor levels of all factors in a task a named list of char vecs
# non chars do not occur in the output
getTaskFactorLevels = function(task) {
  cols = vlapply(task$env$data, is.factor)
  lapply(task$env$data[cols], levels)
}

getTaskWeights = function(task) {
  task$weights
}
