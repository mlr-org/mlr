\name{removeConstantFeatures}
\alias{removeConstantFeatures}
\title{Remove constant features from a data set.}
\usage{
removeConstantFeatures(x, perc = 0, dont.rm = character(0),
  na.mode = "na.rm", show.info = TRUE)
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

  \item{na.mode}{[\code{character(1)}]\cr \itemize{ \item
  \code{na.rm}: \code{NA}s will be ignored in the
  percentage calculation.\cr \item \code{single}: NAs will
  be seen as a single, extra level in the percentage
  calculation.\cr \item \code{distinct}: \code{NA}s will be
  seen as multiple extra levels in the percentage
  calculation.\cr }}

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

