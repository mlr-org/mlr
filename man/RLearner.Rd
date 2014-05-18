\name{RLearner}
\alias{RLearner}
\alias{RLearnerClassif}
\alias{RLearnerRegr}
\alias{RLearnerSurv}
\alias{makeRLearner}
\alias{makeRLearnerClassif}
\alias{makeRLearnerRegr}
\alias{makeRLearnerSurv}
\title{Internal construction / wrapping of learner object.}
\usage{
makeRLearner()

makeRLearnerClassif(cl, package, par.set, numerics = FALSE, factors = FALSE,
  missings = FALSE, weights = FALSE, oneclass = FALSE, twoclass = FALSE,
  multiclass = FALSE, prob = FALSE, par.vals = list())

makeRLearnerRegr(cl, package, par.set, numerics, factors = FALSE,
  missings = FALSE, weights = FALSE, se = FALSE, par.vals = list())

makeRLearnerSurv(cl, package, par.set, numerics, factors = FALSE,
  missings = FALSE, weights = FALSE, se = FALSE, par.vals = list())
}
\arguments{
  \item{cl}{[\code{character(1)}] \cr Class name for
  learner to create.  By convention, all classification
  learners start with \dQuote{classif.}, all regression
  learners with \dQuote{regr.} and all survival learners
  start with \dQuote{surv.}.}

  \item{package}{[\code{character}]\cr Package(s) to load
  for the implementation of the learner.}

  \item{par.set}{[\code{\link[ParamHelpers]{ParamSet}}] \cr
  Parameter set of (hyper)parameters and their
  constraints.}

  \item{numerics}{[\code{logical(1)}]\cr Can numeric
  features be handled?  Default is \code{FALSE}.}

  \item{factors}{[\code{logical(1)}]\cr Can factor features
  be handled?  Default is \code{FALSE}.}

  \item{missings}{[\code{logical(1)}]\cr Can missing values
  be handled?  Default is \code{FALSE}.}

  \item{weights}{[\code{logical(1)}]\cr Can case weights be
  handled?  Default is \code{FALSE}.}

  \item{oneclass}{[\code{logical(1)}]\cr Can one-class
  problems be handled?  Default is \code{FALSE}.}

  \item{twoclass}{[\code{logical(1)}]\cr Can two-class
  problems be handled?  Default is \code{FALSE}.}

  \item{multiclass}{[\code{logical(1)}]\cr Can multi-class
  problems be handled?  Default is \code{FALSE}.}

  \item{prob}{[\code{logical(1)}]\cr Can probabilities be
  predicted?  Default is \code{FALSE}.}

  \item{se}{[\code{logical(1)}]\cr Can standard errors be
  predicted??  Default is \code{FALSE}.}

  \item{par.vals}{[\code{list}] \cr Always set
  hyperparameters to these values when the object is
  constructed.  Useful when default values are missing in
  the underlying function.  The values can later be
  overwritten when the user sets hyperparameters.  Default
  is empty list.}
}
\value{
[\code{\link{RLearnerClassif}}, \code{\link{RLearnerRegr}} or \code{\link{RLearnerSurv}}].
}
\description{
Wraps an already implemented learning method from R to make it accessible to mlr.
Call this method in your constructor. You have to pass an id (name), the required
package(s), a description object for all changeable parameters (you dont have to do this for the
learner to work, but it is strongly recommended), and define what the learner can / cannot do.
}

