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
#' @return [\code{character(1)}].
#' @export
#' @examples
#' task <- makeClassifTask(data = iris, target = "Species")
#' getTaskFormulaAsString(task)
#' @export
getTaskFormulaAsString = function(x) {
  g = function(target) paste(target, "~.")
  if (inherits(x, "TaskDesc"))
    f = g(x$target)
  else
    f = g(x$task.desc$target)
}


#' Get formula of a task.
#'
#' This is simply the \code{target ~ .} formula.
#'
#' @param x [\code{\link{SupervisedTask}} | \code{\link{TaskDesc}}]\cr
#'   Task or its description object.
#' @param delete.env [\code{delete.env}]\cr
#'   Delete enviroment attached to returned formula?
#'   Don't ask why this option exists, R sucks.
#'   Default is \code{TRUE}.
#' @return [\code{formula}].
#' @export
#' @examples
#' task <- makeClassifTask(data = iris, target = "Species")
#' getTaskFormula(task)
#' @export
getTaskFormula = function(x, delete.env = TRUE) {
  target = if (inherits(x, "TaskDesc")) x$target  else x$task.desc$target
  form = reformulate(".", target)
  if (delete.env)
    environment(form) = NULL
  return(form)
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
  tn = task$task.desc$target
  ms = missing(subset) || identical(subset, seq_len(task$task.desc$size))
  mv = missing(features) || identical(features, getTaskFeatureNames(task))

  if (target.extra) {
    # FIXME wtf ...
    list(
      data =
        if (ms && mv)
          {d=task$env$data;d[,tn]=NULL;d}
        else if (ms)
          task$env$data[,features,drop=FALSE]
        else if (mv)
          {d=task$env$data[subset,,drop=FALSE];d[,tn]=NULL;d}
        else
          task$env$data[subset,c(features, tn),drop=FALSE],
      target =
        if (ms)
          recodeY(getTaskTargets(task), type=recode.target, positive=task$task.desc$positive)
        else
          recodeY(getTaskTargets(task)[subset], type=recode.target, positive=task$task.desc$positive)
    )
  } else {
    d =
      if (ms && mv)
        task$env$data
      else if (ms)
        task$env$data[,c(features, tn),drop=FALSE]
      else if (mv)
        task$env$data[subset,,drop=FALSE]
      else
        task$env$data[subset,c(features, tn),drop=FALSE]
    if (recode.target != "no")
      d[,tn] = recodeY(d[, tn], type=recode.target, positive=task$task.desc$positive)
    return(d)
  }
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
  task$env = new.env()
  task$env$data = data
  d = task$task.desc
  task$task.desc = makeTaskDesc(d$type, d$id, data, d$target, d$weight, task$blocking, d$positive)
  return(task)
}
