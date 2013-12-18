\name{TaskDesc}
\alias{TaskDesc}
\title{Description object for task.}
\description{
  Description object for task, encapsulates basic
  statistics without having to store the complete data set.
}
\details{
  Object members: \describe{ \item{id
  [\code{character(1)}]}{Id string of task.} \item{type
  [\code{character(1)}]}{type Type of task, either
  \dQuote{classif} for classification or \dQuote{regr} for
  regression.} \item{target [\code{character(1)}]}{Name of
  target variable.} \item{size[\code{integer(1)}]}{Number
  of cases.} \item{n.feat [\code{integer}]}{Number of
  features, named vector with entries: \dQuote{numerics},
  \dQuote{factors}.} \item{class.levels
  [\code{character}]}{All possible classes. \code{NA} if
  not classification.} \item{has.missings
  [\code{logical(1)}]}{Are missing values present?}
  \item{has.weights [\code{logical(1)}]}{Are weights
  specified for each observation?} \item{has.blocking
  [\code{logical(1)}]}{Is blocking available in task for
  observations?} \item{positive
  [\code{character(1)}]}{Positive class label for binary
  classification, \code{NA} else.} \item{negative
  [\code{character(1)}]}{Negative class label for binary
  classification, \code{NA} else.} }
}

