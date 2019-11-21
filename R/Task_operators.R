#' @title Get a summarizing task description.
#'
#' @description See title.
#' @template arg_task_or_desc
#' @return ret_taskdesc
#' @export
#' @family task
getTaskDesc = function(x) {
  UseMethod("getTaskDesc")
}


#' @export
getTaskDesc.default = function(x) {
  # FIXME: would be much cleaner to specialize here
  x$task.desc
}

#' @export
getTaskDesc.TaskDesc = function(x) {
  x
}

#' Deprecated, use [getTaskDesc] instead.
#' @inheritParams getTaskDesc
#' @export
getTaskDescription = function(x) {
  .Deprecated("getTaskDesc")
  getTaskDesc(x)
}

#' @title Get the type of the task.
#'
#' @description See title.
#' @template arg_task_or_desc
#' @return (`character(1)`).
#' @export
#' @family task
getTaskType = function(x) {
  getTaskDesc(x)$type
}

#' @title Get the id of the task.
#'
#' @description See title.
#' @template arg_task_or_desc
#' @return (`character(1)`).
#' @export
#' @family task
getTaskId = function(x) {
  getTaskDesc(x)$id
}

#' @title Get the name(s) of the target column(s).
#'
#' @description
#' NB: For multilabel, [getTaskTargetNames] and [getTaskClassLevels]
#' actually return the same thing.
#'
#' @template arg_task_or_desc
#' @return ([character]).
#' @export
#' @family task
getTaskTargetNames = function(x) {
  UseMethod("getTaskTargetNames")
}

#' @export
getTaskTargetNames.Task = function(x) {
  getTaskTargetNames(getTaskDesc(x))
}

#' @export
getTaskTargetNames.SupervisedTaskDesc = function(x) {
  x$target
}

#' @export
getTaskTargetNames.UnsupervisedTaskDesc = function(x) {
  character(0L)
}


#' @title Get the class levels for classification and multilabel tasks.
#'
#' @description
#' NB: For multilabel, [getTaskTargetNames] and [getTaskClassLevels]
#' actually return the same thing.
#'
#' @template arg_task_or_desc
#' @return ([character]).
#' @export
#' @family task
getTaskClassLevels = function(x) {
  UseMethod("getTaskClassLevels")
}

#' @export
getTaskClassLevels.ClassifTask = function(x) {
  getTaskClassLevels(getTaskDesc(x))
}

#' @export
getTaskClassLevels.MultilabelTask = function(x) {
  getTaskClassLevels(getTaskDesc(x))
}

#' @export
getTaskClassLevels.ClassifTaskDesc = function(x) {
  getTaskDesc(x)$class.levels
}

#' @export
getTaskClassLevels.MultilabelTaskDesc = function(x) {
  getTaskDesc(x)$class.levels
}

#' Get feature names of task.
#'
#' Target column name is not included.
#'
#' @template arg_task
#' @return ([character]).
#' @family task
#' @export
getTaskFeatureNames = function(task) {
  UseMethod("getTaskFeatureNames")
}

#' @export
getTaskFeatureNames.Task = function(task) {
  setdiff(names(task$env$data), getTaskDesc(task)$target)
}

#' @title Get number of features in task.
#'
#' @description See title.
#' @template arg_task_or_desc
#' @return (`integer(1)`).
#' @export
#' @family task
getTaskNFeats = function(x) {
  sum(getTaskDesc(x)$n.feat)
}

#' @title Get number of observations in task.
#'
#' @description See title.
#' @template arg_task_or_desc
#' @return (`integer(1)`).
#' @export
#' @family task
getTaskSize = function(x) {
  getTaskDesc(x)$size
}

#' @title Get formula of a task.
#'
#' @description
#' This is usually simply `<target> ~ `.
#' For multilabel it is `<target_1> + ... + <target_k> ~`.
#'
#' @template arg_task_or_desc
#' @param target (`character(1)`)\cr
#'   Left hand side of the formula.
#'   Default is defined by task `x`.
#' @param explicit.features (`logical(1)`)\cr
#'   Should the features (right hand side of the formula) be explicitly listed?
#'   Default is `FALSE`, i.e., they will be represented as `"."`.
#' @param env ([environment])\cr
#'   Environment of the formula.
#'   Default is `parent.frame()`.
#' @return ([formula]).
#' @family task
#' @export
getTaskFormula = function(x, target = getTaskTargetNames(x), explicit.features = FALSE, env = parent.frame()) {

  assertCharacter(target, any.missing = FALSE)
  assertFlag(explicit.features)
  assertEnvironment(env)
  td = getTaskDesc(x)
  type = td$type
  if (type == "surv") {
    target = sprintf("Surv(%s, %s, type = \"right\")", target[1L], target[2L])
  } else if (type == "multilabel") {
    target = collapse(target, "+")
  } else if (type == "costsens") {
    stop("There is no formula available for cost-sensitive learning.")
  } else if (type == "cluster") {
    stop("There is no formula available for clustering.")
  }
  if (explicit.features) {
    if (!inherits(x, "Task")) {
      stopf("'explicit.features' can only be used when 'x' is of type 'Task'!")
    }
    features = getTaskFeatureNames(x)
  } else {
    features = "."
  }
  # FIXME in the future we might want to create formulas w/o an environment
  # currently this is impossible for survival because the namespace is not imported
  # properly in many packages -> survival::Surv not found
  as.formula(stri_paste(target, "~", stri_paste(features, collapse = " + ", sep = " "), sep = " "), env = env)
}

#' @title Get target data of task.
#'
#' @description
#' Get target data of task.
#'
#' @template arg_task
#' @inheritParams getTaskData
#' @return A `factor` for classification or a `numeric` for regression, a data.frame
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


#' @title Extract data in task.
#'
#' @description
#' Useful in [trainLearner] when you add a learning machine to the package.
#'
#' @template arg_task
#' @template arg_subset
#' @template arg_features
#' @param target.extra (`logical(1)`)\cr
#'   Should target vector be returned separately?
#'   If not, a single data.frame including the target columns is returned, otherwise a list
#'   with the input data.frame and an extra vector or data.frame for the targets.
#'   Default is `FALSE`.
#' @param recode.target (`character(1)`)\cr
#'   Should target classes be recoded? Supported are binary and multilabel classification and survival.
#'   Possible values for binary classification are \dQuote{01}, \dQuote{-1+1} and \dQuote{drop.levels}.
#'   In the two latter cases the target vector is converted into a numeric vector.
#'   The positive class is coded as \dQuote{+1} and the negative class either as \dQuote{0} or \dQuote{-1}.
#'   \dQuote{drop.levels} will remove empty factor levels in the target column.
#'   In the multilabel case the logical targets can be converted to factors with \dQuote{multilabel.factor}.
#'   For survival, you may choose to recode the survival times to \dQuote{left}, \dQuote{right} or \dQuote{interval2} censored times
#'   using \dQuote{lcens}, \dQuote{rcens} or \dQuote{icens}, respectively.
#'   See [survival::Surv] for the format specification.
#'   Default for both binary classification and survival is \dQuote{no} (do nothing).
#' @param functionals.as (`character(1)`)\cr
#'   How to represents functional features?
#'   Option \dQuote{matrix}: Keep them as matrix columns in the data.frame.
#'   Option \dQuote{dfcols}: Convert them to individual numeric data.frame columns.
#'   Default is \dQuote{dfcols}.
#' @return Either a data.frame or a list with data.frame `data` and vector `target`.
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
getTaskData = function(task, subset = NULL, features, target.extra = FALSE, recode.target = "no",
  functionals.as = "dfcols") {

  checkTask(task, "Task")
  checkTaskSubset(subset, size = task$task.desc$size)
  assertLogical(target.extra)
  assertChoice(functionals.as, choices = c("matrix", "dfcols"))

  task.features = getTaskFeatureNames(task)

  # if supplied check if the input is right and always convert 'features'
  # to character vec
  if (!missing(features)) {
    assert(
      checkIntegerish(features, lower = 1L, upper = length(task.features)),
      checkLogical(features), checkCharacter(features)
    )

    if (!is.character(features)) {
      features = task.features[features]
    }
  }

  tn = task$task.desc$target

  indexHelper = function(df, i, j, drop = TRUE, functionals.as) {
    df = switch(2L * is.null(i) + is.null(j) + 1L,
      df[i, j, drop = drop],
      df[i, , drop = drop],
      df[, j, drop = drop],
      df
    )
    # If we don't keep functionals and functionals are present, convert to numerics
    if (functionals.as == "dfcols" && hasFunctionalFeatures(task)) {
      df = functionalToNormalData(df)
    }
    return(df)
  }

  if (target.extra) {
    if (missing(features)) {
      features = task.features
    }
    res = list(
      data = indexHelper(task$env$data, subset, setdiff(features, tn), drop = FALSE, functionals.as),
      # in the next line we should not rtouch functionals anyway (just Y), so let us keep them as matrix
      target = recodeY(indexHelper(task$env$data, subset, tn, functionals.as = "matrix"), type = recode.target, task$task.desc)
    )
  } else {
    if (missing(features) || identical(features, task.features)) {
      features = NULL
    } else {
      features = union(features, tn)
    }

    res = indexHelper(task$env$data, subset, features, drop = FALSE, functionals.as)
    if (recode.target %nin% c("no", "surv")) {
      res[, tn] = recodeY(res[, tn], type = recode.target, task$task.desc)
    }
  }
  res
}

recodeY = function(y, type, td) {
  if (type == "no") {
    return(y)
  }
  if (type == "drop.levels") {
    return(factor(y))
  }
  if (type == "01") {
    return(as.numeric(y == td$positive))
  }
  if (type == "-1+1") {
    return(as.numeric(2L * (y == td$positive) - 1L))
  }
  if (type == "surv") {
    return(Surv(y[, 1L], y[, 2L], type = "right"))
  }
  if (type == "multilabel.factor") {
    return(lapply(y, function(x) factor(x, levels = c("TRUE", "FALSE"))))
  }
  stopf("Unknown value for 'type': %s", type)
}

#' @title Extract costs in task.
#'
#' @description
#' Returns \dQuote{NULL} if the task is not of type \dQuote{costsens}.
#'
#' @param task ([CostSensTask])\cr
#'   The task.
#' @template arg_subset
#' @return (`matrix` | `NULL`).
#' @family task
#' @export
getTaskCosts = function(task, subset = NULL) {
  UseMethod("getTaskCosts")
}

#' @export
getTaskCosts.Task = function(task, subset = NULL) {
  NULL
}

#' @export
getTaskCosts.CostSensTask = function(task, subset = NULL) {
  subset = checkTaskSubset(subset, size = getTaskDesc(task)$size)
  getTaskDesc(task)$costs[subset, , drop = FALSE]
}


#' @title Subset data in task.
#'
#' @description See title.
#' @template arg_task
#' @template arg_subset
#' @template arg_features
#' @return ([Task]). Task with subsetted data.
#' @family task
#' @export
#' @examples
#' task = makeClassifTask(data = iris, target = "Species")
#' subsetTask(task, subset = 1:100)
subsetTask = function(task, subset = NULL, features) {
  # FIXME: we recompute the taskdesc for each subsetting. do we want that? speed?
  # FIXME: maybe we want this independent of changeData?
  # Keep functionals here as they are (matrix)
  task = changeData(task, getTaskData(task, subset, features, functionals.as = "matrix"), getTaskCosts(task, subset), task$weights)
  if (!is.null(subset)) {
    if (task$task.desc$has.blocking) {
      task$blocking = task$blocking[subset]
    }
    if (task$task.desc$has.weights) {
      task$weights = task$weights[subset]
    }
    if (task$task.desc$has.coordinates) {
      task$coordinates = task$coordinates[subset, ]
    }
  }
  return(task)
}


# we create a new env, so the reference is not changed
#' Change Task Data
#'
#' Mainly for internal use. Changes the data associated with a task, without modifying other task properties.
#'
#' @template arg_task
#' @param data ([data.frame])\cr
#'   The new data to associate with the task. The names and types of the feature columns must match with the old data.
#' @param costs ([data.frame`\cr
#'   Optional: cost matrix.
#' @param weights ([numeric])\cr
#'   Optional: weight vector.
#' @keywords internal
#' @export
changeData = function(task, data, costs, weights, coordinates) {

  if (missing(data)) {
    data = getTaskData(task)
  }
  if (missing(costs)) {
    costs = getTaskCosts(task)
  }
  if (missing(weights)) {
    weights = task$weights
  }
  if (missing(coordinates)) {
    coordinates = task$coordinates
  }
  task$env = new.env(parent = emptyenv())
  task$env$data = data
  task["weights"] = list(weights) # so also 'NULL' gets set
  td = task$task.desc
  # FIXME: this is bad style but I see no other way right now
  task$task.desc = switch(td$type,
    "classif" = makeClassifTaskDesc(td$id, data, td$target, task$weights, task$blocking, td$positive, task$coordinates),
    "regr" = makeRegrTaskDesc(td$id, data, td$target, task$weights, task$blocking, task$coordinates),
    "cluster" = makeClusterTaskDesc(td$id, data, task$weights, task$blocking, task$coordinates),
    "surv" = makeSurvTaskDesc(td$id, data, td$target, task$weights, task$blocking, task$coordinates),
    "costsens" = makeCostSensTaskDesc(td$id, data, td$target, task$blocking, costs, task$coordinates),
    "multilabel" = makeMultilabelTaskDesc(td$id, data, td$target, task$weights, task$blocking, task$coordinates)
  )

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
