getTargetNames = function(x) {
  if (inherits(x, "TaskDesc"))
    x$target
  else
    x$task.desc$target
}

#' Get feature names of task.
#'
#' Target column name is not included.
#'
#' @param task [\code{\link{SupervisedTask}}]\cr
#'   The task.
#' @return [\code{character}].
#' @export
#' @examples
#' task <- makeClassifTask(data = iris, target = "Species")
#' getTaskFeatureNames(task)
getTaskFeatureNames = function(task) {
  #FIXME argument checks currently not done for speed
  setdiff(colnames(task$env$data), task$task.desc$target)
}


#' Get formula of a task as a string.
#'
#' This is simply \dQuote{<target> ~ .}.
#'
#' @param x [\code{\link{SupervisedTask}} | \code{\link{TaskDesc}}]\cr
#'   Task or its description object.
#' @param target [\code{character(1)}]\cr
#'   Left hand side of formula.
#'   Default is defined by task \code{x}.
#' @return [\code{character(1)}].
#' @export
#' @examples
#' task <- makeClassifTask(data = iris, target = "Species")
#' getTaskFormulaAsString(task)
#' @export
getTaskFormulaAsString = function(x, target=getTargetNames(x)) {
  if (length(target) != 1L)
    target = sprintf("Surv(%s, %s)", target[1L], target[2L])
    # FIXME: best way to check if x is survival task or desc?
    # Using just the length is pretty error prone
  paste(target, "~.")
}


#' Get formula of a task.
#'
#' This is simply the \code{target ~ .} formula.
#'
#' @param x [\code{\link{SupervisedTask}} | \code{\link{TaskDesc}}]\cr
#'   Task or its description object.
#' @param target [\code{character(1)}]\cr
#'   Left hand side of formula.
#'   Default is defined by task \code{x}.
#' @param env [\code{environment}]\cr
#'   Environment of the formula. Set this to \code{parent.frame()}
#'   for the default behaviour.
#'   Default is \code{NULL} which deletes the environment.
#' @return [\code{formula}].
#' @export
#' @examples
#' task <- makeClassifTask(data = iris, target = "Species")
#' getTaskFormula(task)
#' @export
getTaskFormula = function(x, target=getTargetNames(x), env=NULL) {
  as.formula(getTaskFormulaAsString(x, target=target), env=env)
}


#' Get target column of task.
#'
#' @param task [\code{\link{SupervisedTask}}]\cr
#'   The task.
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
#' @export
#' @examples
#' task <- makeClassifTask(data = iris, target = "Species")
#' getTaskTargets(task)
#' getTaskTargets(task, subset = 1:50)
getTaskTargets = function(task, subset, recode.target="no") {
  #FIXME argument checks currently not done for speed
  y = task$env$data[subset, task$task.desc$target]
  recodeY(y, recode.target, task$task.desc$positive)
}


#' Extract data in task. Useful in \code{\link{trainLearner}} when you add a learning
#' machine to the package.
#'
#' @param task [\code{\link{SupervisedTask}}]\cr
#'   The task.
#' @param subset [\code{integer}]\cr
#'   Selected cases.
#'   Default is all cases.
#' @param features [\code{character}]\cr
#'   Selected inputs.  Default is all input variables.
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
#' @export
#' @examples
#' library("mlbench")
#' data(BreastCancer)
#'
#' df <- BreastCancer
#' df$Id <- NULL
#' task <- makeClassifTask(id = "BreastCancer", data = df, target = "Class", positive = "malignant")
#' head(getTaskData)
#' head(getTaskData(task, features = c("Cell.size", "Cell.shape"), recode.target = "-1+1"))
#' head(getTaskData(task, subset = 1:100, recode.target = "01"))
getTaskData = function(task, subset, features, target.extra=FALSE, recode.target="no") {
  indexHelper = function(df, i, j, drop=TRUE) {
    switch(2L * is.null(i) + is.null(j) + 1L,
      df[i, j, drop=drop],
      df[i,  , drop=drop],
      df[ , j, drop=drop],
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
      data = indexHelper(task$env$data, subset, setdiff(features, tn), drop=FALSE),
      target = recodeY(indexHelper(task$env$data, subset, tn), type=recode.target, positive=task$task.desc$positive)
    )
  } else {
    if (missing(features) || identical(features, task.features))
      features = NULL
    else
      features = union(features, tn)

    res = indexHelper(task$env$data, subset, features, drop=FALSE)
    if (recode.target %nin% c("no", "surv")) {
      res[, tn] = recodeY(res[, tn], type=recode.target, positive=task$task.desc$positive)
    }
  }
  res
}


#' Subset data in task.
#'
#' @param task [\code{\link{SupervisedTask}}]\cr
#'   The task.
#' @param subset [\code{integer}]\cr
#'   Selected cases.
#'   Default is all cases.
#' @param features [character]\cr
#'   Selected inputs. Note that target feature is always included in the
#'   resulting task, you should not pass it here.
#'   Default is all features.
#' @return [\code{\link{SupervisedTask}}]. Task with subsetted data.
#' @export
#' @examples
#' task <- makeClassifTask(data = iris, target = "Species")
#' subsetTask(task, subset = 1:100)
subsetTask = function(task, subset, features) {
  task = changeData(task, getTaskData(task, subset, features))
  if (!missing(subset)) {
    if (task$task.desc$has.blocking)
      task$blocking = task$blocking[subset]
  }
  return(task)
}


# we create a new env, so the reference is not changed
# FIXME really check what goes on here!
changeData = function(task, data) {
  force(data)
  task$env = new.env(parent=emptyenv())
  task$env$data = data
  d = task$task.desc
  task$task.desc = makeTaskDesc(d$type, d$id, data, d$target, d$weight, task$blocking, d$positive)
  return(task)
}
