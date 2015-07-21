#' @title Set, add, remove or query properties of learners
#'
#' @description
#' Properties can be accessed with \code{getLearnerProperties(learner)}, which returns a
#' character vector.
#'
#' @template arg_learner
#' @param props [\code{character}]\cr
#'   Vector of properties to set, add, remove or query.
#' @return \code{setLearnerProperties}, \code{addLearnerProperties} and \code{removeLearnerProperties}
#'  return an updated \code{\link{Learner}}.
#'  \code{hasLearnerProperties} returns a logical vector of the same length of \code{props}.
#' @name LearnerProperties
#' @rdname LearnerProperties
#' @family learner
NULL

#' @rdname LearnerProperties
#' @export
getLearnerProperties = function(learner) {
  UseMethod("getLearnerProperties")
}

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
