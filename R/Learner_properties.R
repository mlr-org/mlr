#' @title Set, add, remove or query properties of learners
#'
#' @description
#' Properties can be accessed with \code{learner$properties}, which returns a
#' character vector.
#'
#' @template arg_learner
#' @param props [\code{character}]\cr
#'   Vector of properties to set, add, remove or query.
#' @return \code{setProperties}, \code{addProperties} and \code{removeProperties}
#'  return an updated \code{\link{Learner}}.
#'  \code{hasProperties} returns a logical vector of the same length of \code{props}.
#' @name LearnerProperties
#' @rdname LearnerProperties
#' @family learner
NULL

#' @rdname LearnerProperties
#' @export
setProperties = function(learner, props) {
  learner = checkLearner(learner)
  assertSubset(props, getSupportedLearnerProperties(learner$type))
  learner$properties = unique(props)
  learner
}

#' @rdname LearnerProperties
#' @export
addProperties = function(learner, props) {
  learner = checkLearner(learner)
  assertSubset(props, getSupportedLearnerProperties(learner$type))
  learner$properties = union(learner$properties, props)
  learner
}

#' @rdname LearnerProperties
#' @export
removeProperties = function(learner, props) {
  learner = checkLearner(learner)
  assertSubset(props, getSupportedLearnerProperties(learner$type))
  learner$properties = setdiff(learner$properties, props)
  learner
}

#' @rdname LearnerProperties
#' @export
hasProperties = function(learner, props) {
  learner = checkLearner(learner)
  assertSubset(props, getSupportedLearnerProperties())
  props %in% learner$properties
}
