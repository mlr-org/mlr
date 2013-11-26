#' Description object for task.
#'
#' Description object for task, encapsulates basic statistics
#' without having to store the complete data set.
#'
#' Object members:
#' \describe{
#' \item{id [\code{character(1)}]}{Id string of task.}
#' \item{type [\code{character(1)}]}{type Type of task, either \dQuote{classif} for classification or \dQuote{regr} for regression.}
#' \item{target [\code{character(1)}]}{Name of target variable.}
#' \item{weight [\code{character(1)}]}{Name of weight variable.}
#' \item{size[\code{integer(1)}]}{Number of cases.}
#' \item{n.feat [\code{integer}]}{Number of features, named vector with entries: \dQuote{numerics}, \dQuote{factors}.}
#' \item{class.levels [\code{character}]}{All possible classes. \code{NA} if not classification.}
#' \item{has.missings [\code{logical(1)}]}{Are missing values present?}
#' \item{has.blocking [\code{logical(1)}]}{Is blocking available in task for observations?}
#' \item{positive [\code{character(1)}]}{Positive class label for binary classification, \code{NA} else.}
#' \item{negative [\code{character(1)}]}{Negative class label for binary classification, \code{NA} else.}
#' }
#' @name TaskDesc
#' @rdname TaskDesc
NULL

makeTaskDesc = function(type, id, data, target, weight, blocking, positive) {
  td = list()
  td$id = id
  td$type = type
  i = which(colnames(data) %in% c(target))
  td$target = target
  td$weight = weight
  td$size = nrow(data)
  y = data[, target]
  td$n.feat = c(
    numerics = sum(sapply(data, is.numeric)) - is.numeric(y),
    factors = sum(sapply(data, is.factor)) - is.factor(y)
  )
  if(type == "classif")
    td$class.levels = levels(y)
  else
    td$class.levels = NA_character_
  td$has.missings = any(sapply(data, function(x) any(is.na(x))))
  td$has.blocking = length(blocking) > 0L
  if (type == "classif") {
    td$positive = positive
    if (length(td$class.levels) == 1L)
      td$negative = paste("not_", positive)
    else if(length(td$class.levels) == 2L)
      td$negative = setdiff(td$class.levels, positive)
    else
      td$negative = NA_character_
  } else {
    td$positive = td$negative = NA_character_
  }
  return(structure(td, class="TaskDesc"))
}

