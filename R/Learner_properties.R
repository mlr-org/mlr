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
setLearnerProperties = function(learner, props) {
  learner = checkLearner(learner)
  assertSubset(props, getSupportedLearnerProperties(learner$type))
  learner$properties = unique(props)
  learner
}

#' @export
setProperties = function(learner, props) {
  .Deprecated("setLearnerProperties")
  setLearnerProperties(learner, props)
}

#' @rdname LearnerProperties
#' @export
addLearnerProperties = function(learner, props) {
  learner = checkLearner(learner)
  assertSubset(props, getSupportedLearnerProperties(learner$type))
  learner$properties = union(getLearnerProperties(learner), props)
  learner
}

#' @export
addProperties = function(learner, props) {
  .Deprecated("addLearnerProperties")
  addLearnerProperties(learner, props)
}

#' @rdname LearnerProperties
#' @export
removeLearnerProperties = function(learner, props) {
  learner = checkLearner(learner)
  assertSubset(props, getSupportedLearnerProperties(learner$type))
  learner$properties = setdiff(getLearnerProperties(learner), props)
  learner
}

#' @export
removeProperties = function(learner, props) {
  .Deprecated("removeLearnerProperties")
  removeLearnerProperties(learner, props)
}

#' @rdname LearnerProperties
#' @export
hasLearnerProperties = function(learner, props) {
  learner = checkLearner(learner)
  assertSubset(props, getSupportedLearnerProperties())
  props %in% getLearnerProperties(learner)
}

#' @export
hasProperties = function(learner, props) {
  .Deprecated("hasLearnerProperties")
  hasLearnerProperties(learner, props)
}
