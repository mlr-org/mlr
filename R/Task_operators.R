getTaskDescription = function(x) {
  if (inherits(x, "TaskDesc"))
    x
  else
    x$task.desc
}

getTaskType = function(x) {
  getTaskDescription(x)$type
}

getTargetNames = function(x) {
  getTaskDescription(x)$target
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
  #FIXME: argument checks currently not done for speed
  setdiff(colnames(task$env$data), task$task.desc$target)
}

#' Get number of feature in task.
#'
#' @template arg_task
#' @return [\code{integer(1)}].
#' @export
#' @family task
getTaskNFeats = function(task) {
  sum(task$task.desc$n.feat)
}

#' @export
#' @rdname getTaskFormula
getTaskFormulaAsString = function(x, target = getTargetNames(x)) {
  td = getTaskDescription(x)
  type = td$type
  if (type == "surv")
    target = sprintf("Surv(%s, %s, type = \"%s\")", target[1L], target[2L], td$censoring)
  else if (type == "costsens")
    stop("There is no formula available for cost-sensitive learning.")
  else if (type == "cluster")
    stop("There is no formula available for clustering.")
  paste(target, "~.")
}


#' Get formula of a task.
#'
#' This is simply \dQuote{<target> ~ .}.
#'
#' @template arg_task_or_desc
#' @param target [\code{character(1)}]\cr
#'   Left hand side of formula.
#'   Default is defined by task \code{x}.
#' @param env [\code{environment}]\cr
#'   Environment of the formula. Set this to \code{parent.frame()}
#'   for the default behaviour.
#'   Default is \code{NULL} which deletes the environment.
#' @return [\code{formula} | \code{character(1)}].
#' @family task
#' @export
getTaskFormula = function(x, target = getTargetNames(x), env = NULL) {
  as.formula(getTaskFormulaAsString(x, target = target), env = env)
}


#' Get target column of task.
#'
#' @template arg_task
#' @param subset [\code{integer}]\cr
#'   Selected cases.
#'   Default is all cases.
#' @param recode.target [\code{character(1)}] \cr
#'   Should target classes be recoded? Only for binary classification.
#'   Possible are \dQuote{no} (do nothing), \dQuote{01}, and \dQuote{-1+1}.
#'   In the two latter cases the target vector is converted into a numeric vector.
#'   The positive class is coded as +1 and the negative class either as 0 or -1.
#'   Default is \dQuote{no}.
#' @return A \code{factor} for classification or a \code{numeric} for regression.
#' @family task
#' @export
#' @examples
#' task = makeClassifTask(data = iris, target = "Species")
#' getTaskTargets(task)
#' getTaskTargets(task, subset = 1:50)
getTaskTargets = function(task, subset, recode.target = "no") {
  #FIXME: argument checks currently not done for speed
  if (task$task.desc$type == "costsens")
    stop("There is no target available for cost-sensitive learning.")
  if (task$task.desc$type == "cluster")
    stop("There is no target available for cluster.")
  y = task$env$data[subset, task$task.desc$target, drop = TRUE]
  recodeY(y, recode.target, task$task.desc$positive)
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
#'   If not, a single data.frame including the target is returned, otherwise a list
#'   with the input data.frame and an extra vector for the targets.
#'   Default is FALSE.
#' @param recode.target [\code{character(1)}]\cr
#'   Should target classes be recoded? Only for binary classification.
#'   Possible are \dQuote{no} (do nothing), \dQuote{01}, and \dQuote{-1+1}.
#'   In the two latter cases the target vector is converted into a numeric vector.
#'   The positive class is coded as +1 and the negative class either as 0 or -1.
#'   Default is \dQuote{no}.
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
      target = recodeY(indexHelper(task$env$data, subset, tn), type = recode.target, positive = task$task.desc$positive)
    )
  } else {
    if (missing(features) || identical(features, task.features))
      features = NULL
    else
      features = union(features, tn)

    res = indexHelper(task$env$data, subset, features, drop = FALSE)
    if (recode.target %nin% c("no", "surv")) {
      res[, tn] = recodeY(res[, tn], type = recode.target, positive = task$task.desc$positive)
    }
  }
  res
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
#' @param task [\code{\link{Task}}]\cr
#'   The task.
#' @param subset [\code{integer}]\cr
#'   Selected cases.
#'   Default is all cases.
#' @param features [character]\cr
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
  task = changeData(task, getTaskData(task, subset, features), getTaskCosts(task, subset))
  if (!missing(subset)) {
    if (task$task.desc$has.blocking)
      task$blocking = task$blocking[subset]
    if (task$task.desc$has.weights)
      task$weights = task$weights[subset]
  }
  return(task)
}


# we create a new env, so the reference is not changed
# FIXME: really check what goes on here! where is this called / used?
changeData = function(task, data, costs, weights) {
  if (missing(data))
    data = getTaskData(task)
  if (missing(costs))
    costs = getTaskCosts(task)
  if (missing(weights))
    weights = task$env$weights
  task$env = new.env(parent = emptyenv())
  task$env$data = data
  task$env$costs = costs
  task$env$weights = weights
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
