#' Internal construction / wrapping of learner object. 
#' 
#' Wraps an already implemented learning method from R to make it accessible to mlr.
#' Call this method in your constructor. You have to pass an id (name), the required
#' package(s), a description object for all changeable parameters (you dont have to do this for the
#' learner to work, but it is strongly recommended), and define what the learner can / cannot do.
#' 
#' @param cl [\code{character(1)}] \cr
#'   Class name for learner to create. 
#'   By convention, all classification learners start with \dQuote{classif.} 
#'   and all regression learners with \dQuote{regr.}
#' @param package [\code{character}]\cr
#'   Package(s) to load for the implementation of the learner.
#' @param par.set [\code{\link[ParamHelpers]{ParamSet}}] \cr
#'   Parameter set of (hyper)parameters and their constraints.
#' @param numerics [\code{logical(1)}]\cr
#'   Can numeric features be handled?
#'   Default is \code{FALSE}.
#' @param factors [\code{logical(1)}]\cr
#'   Can factor features be handled?
#'   Default is \code{FALSE}.
#' @param missings [\code{logical(1)}]\cr
#'   Can missing values be handled?
#'   Default is \code{FALSE}.
#' @param weights [\code{logical(1)}]\cr
#'   Can case weights be handled?
#'   Default is \code{FALSE}.
#' @param oneclass [\code{logical(1)}]\cr
#'   Can one-class problems be handled?
#'   Default is \code{FALSE}.
#' @param twoclass [\code{logical(1)}]\cr
#'   Can two-class problems be handled?
#'   Default is \code{FALSE}.
#' @param multiclass [\code{logical(1)}]\cr
#'   Can multi-class problems be handled?
#'   Default is \code{FALSE}.
#' @param prob [\code{logical(1)}]\cr
#'   Can probabilities be predicted?
#'   Default is \code{FALSE}.
#' @param se [\code{logical(1)}]\cr
#'   Can standard errors be predicted??
#'   Default is \code{FALSE}.
#' @param par.vals [\code{list}] \cr
#'   Always set hyperparameters to these values when the object is constructed.
#'   Useful when default values are missing in the underlying function.
#'   The values can later be overwritten when the user sets hyperparameters.
#'   Default is empty list.  
#' @return [\code{\link{RLearnerClassif}} or \code{\link{RLearnerRegr}}].
#' @name RLearner
#' @rdname RLearner
#' @aliases RLearnerClassif RLearnerRegr
NULL

#' @export
#' @rdname RLearner
makeRLearner = function() {
  UseMethod("makeRLearner")
}

makeRLearnerInternal = function(id, type, package, par.set, numerics, factors, missings, weights, 
  oneclass, twoclass, multiclass, prob, se, par.vals) {

  # must do that before accessing par.set
  # one case where lazy eval is actually helpful...
  checkArg(package, "character", na.ok=FALSE)  
  requirePackages(package, paste("learner", id))

  checkArg(id, "character", len=1L, na.ok=FALSE)  
  checkArg(type, choices=c("classif", "regr"))  
  checkArg(package, "character", na.ok=FALSE)  
  checkArg(par.set, "ParamSet")  
  checkListElementClass(par.set$pars, "LearnerParam")
  checkArg(factors, "logical", len=1L, na.ok=FALSE)  
  checkArg(missings, "logical", len=1L, na.ok=FALSE)  
  checkArg(weights, "logical", len=1L, na.ok=FALSE)  
  checkArg(oneclass, "logical", len=1L, na.ok=FALSE)  
  checkArg(twoclass, "logical", len=1L, na.ok=FALSE)  
  checkArg(multiclass, "logical", len=1L, na.ok=FALSE)  
  checkArg(prob, "logical", len=1L, na.ok=FALSE)  
  checkArg(se, "logical", len=1L, na.ok=FALSE)  
  checkArg(par.vals, "list")  
  if(!isProperlyNamed(par.vals))
    stop("Argument par.vals must be a properly named list!")

  learner = structure(list(
    id = id,
    type = type,
    package = package,
    par.set = par.set,
    par.vals = list(),
    numerics = numerics,
    factors = factors,
    predict.type = "response",
    missings = missings,
    weights = weights,
    oneclass = oneclass,
    twoclass = twoclass,
    multiclass = multiclass,
    prob = prob,
    se = se
  ), class = c("RLearner", "Learner"))
  setHyperPars(learner, par.vals=par.vals)
}

#' @export
#' @rdname RLearner
makeRLearnerClassif = function(cl, package, par.set, numerics=FALSE, factors=FALSE, 
  missings=FALSE, weights=FALSE, oneclass=FALSE, twoclass=FALSE, multiclass=FALSE, 
  prob=FALSE, par.vals=list()) {
  
  x = makeRLearnerInternal(cl, "classif", package, par.set, numerics, factors, missings, weights, 
    oneclass, twoclass, multiclass, prob, FALSE, par.vals)
  class(x) = c(cl, "RLearnerClassif", class(x))  
  return(x)
}


#' @export
#' @rdname RLearner
makeRLearnerRegr = function(cl, package, par.set, numerics, factors=FALSE,
  missings=FALSE, weights=FALSE, se=FALSE, par.vals=list()) {
  
  x = makeRLearnerInternal(cl, "regr", package, par.set, numerics, factors, missings, weights,
    FALSE, FALSE, FALSE, FALSE, se, par.vals)
  class(x) = c(cl, "RLearnerRegr", class(x))  
  return(x)
}

