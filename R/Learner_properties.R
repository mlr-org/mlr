#' @title Query properties of learners.
#'
#' @description
#' Properties can be accessed with `getLearnerProperties(learner)`, which returns a
#' character vector.
#'
#' The learner properties are defined as follows:
#' \describe{
#'   \item{numerics, factors, ordered}{Can numeric, factor or ordered factor features be handled?}
#'   \item{functionals}{Can an arbitrary number of functional features be handled?}
#'   \item{single.functional}{Can exactly one functional feature be handled?}
#'   \item{missings}{Can missing values in features be handled?}
#'   \item{weights}{Can observations be weighted during fitting?}
#'   \item{oneclas, twoclass, multiclass}{Only for classif: Can one-class, two-class or multi-class classification problems be handled?}
#'   \item{class.weights}{Only for classif: Can class weights be handled?}
#'   \item{rcens, lcens, icens}{Only for surv: Can right, left, or interval censored data be handled?}
#'   \item{prob}{For classif, cluster, multilabel, surv: Can probabilites be predicted?}
#'   \item{se}{Only for regr: Can standard errors be predicted?}
#'   \item{oobpreds}{Only for classif, regr and surv: Can out of bag predictions be extracted from the trained model?}
#'   \item{featimp}{For classif, regr, surv: Does the model support extracting information on feature importance?}
#' }
#'
#' @template arg_learner
#' @param props ([character])\cr
#'   Vector of properties to query.
#' @return `getLearnerProperties` returns a character vector with learner properties.
#'  `hasLearnerProperties` returns a logical vector of the same length as `props`.
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

#' @export
getLearnerProperties.character = function(learner) {
  getLearnerProperties(checkLearner(learner))
}

#' @export
getLearnerProperties.ModelMultiplexer = function(learner) {
  selected = learner$par.vals$selected.learner
  # NB: this is not set during construction
  if (is.null(selected)) learner$properties else getLearnerProperties(learner$base.learners[[selected]])
}

#' @rdname LearnerProperties
#' @export
hasLearnerProperties = function(learner, props) {
  learner = checkLearner(learner)
  assertSubset(props, listLearnerProperties())
  props %in% getLearnerProperties(learner)
}

#' Deprecated, use `hasLearnerProperties` instead.
#' @param learner Deprecated.
#' @param props Deprecated.
#' @export
hasProperties = function(learner, props) {
  .Deprecated("hasLearnerProperties")
  hasLearnerProperties(learner, props)
}

#' @title List the supported learner properties
#'
#' @description
#'   This is useful for determining which learner properties are available.
#'
#' @param type (`character(1)`)\cr
#'   Only return properties for a specified task type. Default is \dQuote{any}.
#'
#' @return ([character]).
#'
#' @export
listLearnerProperties = function(type = "any") {
  all.props = c(listTaskTypes(), "any")
  assertSubset(type, all.props)
  mlr$learner.properties[[type]]
}
