\name{removeConstantFeatures}
\alias{removeConstantFeatures}
\title{Remove constant features from a data set.}
\usage{
removeConstantFeatures(x, perc = 0, dont.rm = character(0),
  na.mode = "ignore", show.info = TRUE, printed.features = 10L)
}
\arguments{
  \item{x}{[\code{\link{data.frame}} |
  \code{\link{SupervisedTask}}]\cr The data set or task.}

  \item{perc}{[\code{numeric(1)}]\cr The percentage of a
  feature values in [0, 1) that must differ from the mode
  value.  Default is 0, which means only constant features
  with exactly one observed level are removed.}

  \item{dont.rm}{[\code{character()}]\cr Names of the
  columns which must not be deleted.  E.g. the target
  column - automatically added when provided a
  \code{task}.}

  \item{na.mode}{[\code{character}]\cr \itemize{ \item
  \code{ignore}: \code{NA}s will be ignored (lower value of
  differation)\cr \item \code{factor}: \code{NA}s will be
  seen as own value (higher value of differation) \cr \item
  \code{modus}: like factor but \code{NA}s can also be a
  modal value }}
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

