\name{removeConstantFeatures}
\alias{removeConstantFeatures}
\title{Remove constant features from a data set.}
\usage{
removeConstantFeatures(x, perc = 0, dont.rm = character(0L),
  na.ignore = FALSE, tol = .Machine$double.eps^0.5, show.info = TRUE)
}
\arguments{
  \item{x}{[\code{\link{data.frame}} |
  \code{\link{SupervisedTask}}]\cr The data set or task.}

  \item{perc}{[\code{numeric(1)}]\cr The percentage of a
  feature values in [0, 1) that must differ from the mode
  value.  Default is 0, which means only constant features
  with exactly one observed level are removed.}

  \item{dont.rm}{[\code{character}]\cr Names of the columns
  which must not be deleted.  Default is no columns.  Note
  that for a task the target column is always added to
  these so it is never removed.}

  \item{na.ignore}{[\code{logical(1)}]\cr Should NAs be
  ignored in the percentage calculation?  (Or should they
  be treated as a single, extra level in the percentage
  calculation?)  Default is \code{FALSE}.}

  \item{tol}{[\code{numeric(1)}]\cr Numerical tolerance to
  treat two numbers as equal.  Variables stored as
  \code{double} will get rounded accordingly before
  computing the mode.  Default is
  \code{sqrt(.Maschine$double.eps)}.}

  \item{show.info}{[\code{logical(1)}]\cr Verbose output on
  console?  Default is \code{TRUE}.}
}
\value{
[\code{\link{data.frame}} | \code{\link{SupervisedTask}}].
}
\description{
Constant features can lead to errors in some models and
obviously provide no information in the training set that
can be learned from. With the argument \dQuote{perc}, there
is a possibility to also remove features for which less
than \dQuote{perc} percent of the observations differ from
the mode value.
}

