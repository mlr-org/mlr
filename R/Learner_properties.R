#' @title Query properties of learners.
#'
#' @description
#' Properties can be accessed with \code{getLearnerProperties(learner)}, which returns a
#' character vector.
#'
#' @template arg_learner
#' @param props [\code{character}]\cr
#'   Vector of properties to query.
#' @return \code{getLearnerProperties} returns a character vector with learner properties.
#'  \code{hasLearnerProperties} returns a logical vector of the same length as \code{props}.
#' @name LearnerProperties
#' @rdname LearnerProperties
#' @aliases getLearnerProperties hasLearnerProperties
#' @family learner
NULL

#' @rdname LearnerProperties
#' @export
getLearnerProperties = function(learner) {
  UseMethod("getLearnerProperties")
}

#' @export
getLearnerProperties.Learner = function(learner) {
  learner$properties
}

#' @rdname LearnerProperties
#' @export
hasLearnerProperties = function(learner, props) {
  learner = checkLearner(learner)
  assertSubset(props, getSupportedLearnerProperties())
  props %in% getLearnerProperties(learner)
}

#' Deprecated, use \code{hasLearnerProperties} instead.
#' @param learner Deprecated.
#' @param props Deprecated.
#' @export
hasProperties = function(learner, props) {
  .Deprecated("hasLearnerProperties")
  hasLearnerProperties(learner, props)
}

