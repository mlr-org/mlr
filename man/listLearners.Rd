\name{listLearners}
\alias{listLearners}
\alias{listLearnersForTask}
\title{Find matching learning algorithms.}
\usage{
listLearners(type = NA, numerics = NA, factors = NA, missings = NA,
  weights = NA, oneclass = NA, twoclass = NA, multiclass = NA,
  prob = NA, se = NA, quiet = TRUE, warn.missing.packages = TRUE)

listLearnersForTask(task, weights = NA, prob = NA, se = NA,
  warn.missing.packages = TRUE)
}
\arguments{
  \item{type}{[\code{character(1)}]\cr Type of the learning
  algorithm, one of \dQuote{classif}, \dQuote{regr} or
  \dQuote{surv}.}

  \item{numerics}{[\code{logical(1)}]\cr Supports
  real-valued features?}

  \item{factors}{[\code{logical(1)}]\cr Supports factor
  features?}

  \item{missings}{[\code{logical(1)}]\cr Supports missing
  values in features?}

  \item{weights}{[\code{logical(1)}]\cr Supports case
  weights?}

  \item{oneclass}{[\code{logical(1)}]\cr Supports oneclass
  problems?}

  \item{twoclass}{[\code{logical(1)}]\cr Supports twoclass
  problems?}

  \item{multiclass}{[\code{logical(1)}]\cr Supports
  multiclass problems?}

  \item{prob}{[\code{logical(1)}]\cr Can predict
  probabilities (classification)?}

  \item{se}{[\code{logical(1)}]\cr Can predict standard
  errors (regression)?}

  \item{quiet}{[\code{logical(1)}]\cr Construct learners
  quietly to check their properties, shows no package
  startup messages.  Turn off if you suspect errors.
  Default is code{TRUE}.}

  \item{warn.missing.packages}{[\code{logical(1)}]\cr If
  some learner cannot be constructed because its package is
  missing, should a warning be shown?  Default is
  code{TRUE}.}

  \item{task}{[\code{\link{SupervisedTask}}]\cr The task.
  Learners are returned that are applicable.}
}
\value{
[\code{character}]. Class names of matching learners.
}
\description{
Returns the class names of learning algorithms which have
specific characteristics, e.g. whether they supports
missing values, case weights, etc.
}
\details{
The default for all search parameters is \code{NA},
meaning: property is not required, do not care.

Note that the packages of all learners are loaded during
the search.
}

