#' Internal construction / wrapping of learner object.
#'
#' Wraps an already implemented learning method from R to make it accessible to mlr.
#' Call this method in your constructor. You have to pass an id (name), the required
#' package(s), a description object for all changeable parameters (you dont have to do this for the
#' learner to work, but it is strongly recommended), and use property tags to define features of the learner.
#'
#' @param cl [\code{character(1)}] \cr
#'   Class name for learner to create.
#'   By convention, all classification learners start with \dQuote{classif.},
#'   all regression learners with \dQuote{regr.}, all cluster learners with \dQuote{cluster.}
#'   and all survival learners start with \dQuote{surv.}.
#' @param package [\code{character}]\cr
#'   Package(s) to load for the implementation of the learner.
#' @param properties [\code{character(1)}]\cr
#'   Set of learner properties. Some standard property names include:
#'   \describe{
#'     \item{numerics}{Can numeric features be handled?}
#'     \item{factors}{Can factor features be handled?}
#'     \item{missings}{Can missing features be handled?}
#'     \item{oneclas,twoclass,multiclass}{Can one-class, two-class or multi-class classification problems be handled?}
#'     \item{prob}{Can probabilites be predicted?}
#'     \item{se}{Can standard errors be predicted?}
#'     \item{class.weights}{Can class weights be handled?}
#'     \item{submodel}{Can submodels be created?}
#'   }
#'   Default is \code{character(0)}.
#' @param class.weights.param [\code{character(1)}] \cr
#'   Name of the parameter, which can be used for providing class weights.
#' @param submodel.param [\code{character(1)}] \cr
#'   Name of the parameter, which allows the submodel-trick, i.e., which allows
#'   to derive submodels of the original model by training the model with the
#'   maximal value of that submodel parameter and afterwards accessing all
#'   models (which belong to smaller values of that parameter) "for free".
#'   For instance, one can train a \code{\link[randomForest]{randomForest}}
#'   consisting of \code{ntree = 500} trees and afterwards access all forests,
#'   which consist of less than 500 trees. Note that the default value of that
#'   param (as defined in \code{par.set}) will be used as maximal value.
#' @param par.set [\code{\link[ParamHelpers]{ParamSet}}] \cr
#'   Parameter set of (hyper)parameters and their constraints.
#' @param par.vals [\code{list}] \cr
#'   Always set hyperparameters to these values when the object is constructed.
#'   Useful when default values are missing in the underlying function.
#'   The values can later be overwritten when the user sets hyperparameters.
#'   Default is empty list.
#' @param name [\code{character(1)}]
#'   Meaningful name for learner.
#'   Default is \code{id}.
#' @param short.name [\code{character(1)}]
#'   Short name for learner.
#'   Should only be a few characters so it can be used in plots and tables.
#'   Default is \code{id}.
#' @param note [\code{character(1)}]
#'   Additional notes regarding the learner and its integration in mlr.
#'   Default is \dQuote{}.
#' @return [\code{\link{RLearnerClassif}}, \code{\link{RLearnerCluster}}, \code{\link{RLearnerMultilabel}} \code{\link{RLearnerRegr}} or \code{\link{RLearnerSurv}}].
#' @name RLearner
#' @rdname RLearner
#' @aliases RLearnerClassif RLearnerCluster RLearnerMultilabel RLearnerRegr RLearnerSurv
NULL

#' @export
#' @rdname RLearner
makeRLearner = function() {
  UseMethod("makeRLearner")
}

makeRLearnerInternal = function(id, type, package, par.set, par.vals, properties,
  name = id, short.name = id, note = "") {

  # must do that before accessing par.set
  # one case where lazy eval is actually helpful...
  assertCharacter(package, any.missing = FALSE)
  requirePackages(package, why = paste("learner", id), default.method = "load")

  assertString(id)
  assertChoice(type, choices = c("classif", "regr", "multilabel", "surv", "cluster"))
  assertSubset(properties, getSupportedLearnerProperties(type))
  assertClass(par.set, classes = "ParamSet")
  checkListElementClass(par.set$pars, "LearnerParam")
  assertList(par.vals)
  if(!isProperlyNamed(par.vals))
    stop("Argument par.vals must be a properly named list!")
  assertString(name)
  assertString(short.name)
  assertString(note)

  learner = setClasses(list(
    id = id,
    type = type,
    package = package,
    properties = unique(properties),
    par.set = par.set,
    par.vals = par.vals,
    predict.type = "response",
    name = name,
    short.name = short.name,
    note = note
  ), c("RLearner", "Learner"))
}

#' @export
#' @rdname RLearner
makeRLearnerClassif = function(cl, package, par.set, par.vals = list(), properties = character(0L),
  name = cl, short.name = cl, note = "", class.weights.param = NULL, submodel.param = NULL) {

  lrn = addClasses(
    makeRLearnerInternal(cl, "classif", package, par.set, par.vals, properties, name, short.name, note),
    c(cl, "RLearnerClassif")
  )

  # include the class.weights.param
  if ("class.weights" %in% getLearnerProperties(lrn)) {
    assertString(class.weights.param)
    if (!is.null(par.set$pars[[class.weights.param]]))
      lrn$class.weights.param = class.weights.param
    else
      stopf("'%s' needs to be defined in the parameter set as well.", class.weights.param)
  }

  # include the submodel.param
  if ("submodel" %in% getLearnerProperties(lrn)) {
    assertString(submodel.param)
    if (!is.null(par.set$pars[[submodel.param]])) {
      lrn$submodel.param = submodel.param
    } else {
      stopf("'%s' needs to be defined in the parameter set as well.", submodel.param)
    }
  }
  return(lrn)
}

#' @export
#' @rdname RLearner
makeRLearnerMultilabel = function(cl, package, par.set, par.vals = list(), properties = character(0L), name = cl, short.name = cl, note = "") {
  addClasses(
    makeRLearnerInternal(cl, "multilabel", package, par.set, par.vals, properties, name, short.name, note),
    c(cl, "RLearnerClassif")
  )
}

#' @export
#' @rdname RLearner
makeRLearnerRegr = function(cl, package, par.set, par.vals = list(), properties = character(0L),
  name = cl, short.name = cl, note = "", submodel.param = NULL) {

  lrn = addClasses(
    makeRLearnerInternal(cl, "regr", package, par.set, par.vals, properties, name, short.name, note),
    c(cl, "RLearnerRegr")
  )

  # include the submodel.param
  if ("submodel" %in% getLearnerProperties(lrn)) {
    assertString(submodel.param)
    if (!is.null(par.set$pars[[submodel.param]])) {
      lrn$submodel.param = submodel.param
    } else {
      stopf("'%s' needs to be defined in the parameter set as well.", submodel.param)
    }
  }
  return(lrn)
}

#' @export
#' @rdname RLearner
makeRLearnerSurv = function(cl, package, par.set, par.vals = list(), properties = character(0L), name = cl, short.name = cl, note = "") {
  addClasses(
    makeRLearnerInternal(cl, "surv", package, par.set, par.vals, properties, name, short.name, note),
    c(cl, "RLearnerSurv")
  )
}

#' @export
#' @rdname RLearner
makeRLearnerCluster = function(cl, package, par.set, par.vals = list(), properties = character(0L), name = cl, short.name = cl, note = "") {
  addClasses(
    makeRLearnerInternal(cl, "cluster", package, par.set, par.vals, properties, name, short.name, note),
    c(cl, "RLearnerCluster")
  )
}

