#' Description object for task.
#'
#' Description object for task, encapsulates basic statistics
#' without having to store the complete data set.
#'
#' Object members:
#' \describe{
#' \item{id [\code{character(1)}]}{Id string of task.}
#' \item{type [\code{character(1)}]}{type Type of task, either \dQuote{classif} for classification, \dQuote{regr} for regression or
#'   \dQuote{surv} for survival.}
#' \item{target [\code{character(1)}]}{Name of target variable.}
#' \item{size[\code{integer(1)}]}{Number of cases.}
#' \item{n.feat [\code{integer}]}{Number of features, named vector with entries: \dQuote{numerics}, \dQuote{factors}.}
#' \item{class.levels [\code{character}]}{All possible classes. \code{NA} if not classification.}
#' \item{has.missings [\code{logical(1)}]}{Are missing values present?}
#' \item{has.weights [\code{logical(1)}]}{Are weights specified for each observation?}
#' \item{has.blocking [\code{logical(1)}]}{Is blocking available in task for observations?}
#' \item{positive [\code{character(1)}]}{Positive class label for binary classification, \code{NA} else.}
#' \item{negative [\code{character(1)}]}{Negative class label for binary classification, \code{NA} else.}
#' }
#' @name TaskDesc
#' @rdname TaskDesc
NULL

makeTaskDesc = function(type, id, data, target, weights, blocking, positive) {
  cl = dropNamed(vapply(data, function(x) head(class(x), 1L), character(1L)), target)
  td = list(
    id = id,
    type = type,
    target = target,
    size = nrow(data),
    n.feat = c(numerics = sum(cl %in% c("integer", "numeric")), factors = sum(cl == "factor")),
    has.missings = any(is.na(data)),
    has.weights = length(weights) > 0L,
    has.blocking = length(blocking) > 0L,
    class.levels = NA_character_,
    positive = NA_character_,
    negative = NA_character_
  )

  if(type == "classif") {
    td$class.levels = levels(data[, target])
    td$positive = positive
    if (length(td$class.levels) == 1L)
      td$negative = paste0("not_", positive)
    else if(length(td$class.levels) == 2L)
      td$negative = setdiff(td$class.levels, positive)
  }

  cl = switch(type, classif = "ClassifTaskDesc", regr = "RegrTaskDesc", surv = "SurvTaskDesc")
  setClasses(td, c(cl, "TaskDesc"))
}
