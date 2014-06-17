#' Set, add, remove or query properties of learners
#'
#' @template arg_learner
#' @param props [\code{character}]\cr
#'   Vector of properties to set, add, remove or query.
#' @param prop [\code{character(1)}]\cr
#'   Single property.
#' @return \code{setProperties}, \code{addProperties} and \code{removeProperties}
#'  return an updated \code{\link{Learner}}.
#'  \code{hasProperty} returns scalar logical if the property is present
#'  \code{hasPropertiesAll} returns a scalar logical if all properties are present.
#'  \code{hasProperties} returns a logical vector of the same length of \code{props}.
#' @export
#' @name LearnerProperties
setProperties = function(learner, props) {
  checkArg(props, "character", na.ok = FALSE)
  learner$properties = unique(props)
  learner
}

#' @rdname LearnerProperties
#' @export
addProperties = function(learner, props) {
  checkArg(props, "character", na.ok = FALSE)
  learner$properties = union(learner$properties, props)
  learner
}

#' @rdname LearnerProperties
#' @export
removeProperties = function(learner, props) {
  checkArg(props, "character", na.ok = FALSE)
  learner$properties = setdiff(learner$properties, props)
  learner
}

#' @rdname LearnerProperties
#' @export
hasProperties = function(learner, props) {
  checkArg(props, "character", na.ok = FALSE)
  props %in% learner$properties
}

#' @rdname LearnerProperties
#' @export
hasPropertiesAll = function(learner, props) {
  checkArg(props, "character", na.ok = FALSE)
  all(props %in% learner$properties)
}

#' @rdname LearnerProperties
#' @export
hasProperty = function(learner, prop) {
  checkArg(prop, "character", len = 1L, na.ok = FALSE)
  prop %in% learner$properties
}



