\name{removeConstantFeatures}
\alias{removeConstantFeatures}
\title{Remove constant features from a data set.}
\usage{
removeConstantFeatures(x, perc = 0, safeCols = NULL, na.mode = "ignore",
  show.info = TRUE, printed.features = 10L)
}
\arguments{
  \item{x}{[\code{\link{data.frame}}] or
  [\code{\link{SupervisedTask}}] \cr The data set or a task
  inlcuding it.}

  \item{perc}{[\code{numeric}]\cr The percentage of a
  features' values (in [0, 1)) that must differ from the
  mode value.  Default is 0, which means only constant
  features with exactly one observed level are removed.}

  \item{safeCols}{[\code{character()}]\cr Name(s) of the
  columns which must not be deleted. (E.g. the target
  column - automaticaly added when provided a \code{task})}

  \item{na.mode}{[\code{character}]\cr \itemize{ \item
  \code{ignore}: \code{NA}s will be ignored (lower value of
  differation)\cr \item \code{factor}: \code{NA}s will be
  seen as own value (higher value of differation) \cr \item
  \code{modus}: like factor but \code{NA}s can also be a
  modal value }}
}
\value{
[\code{\link{data.frame}}] or
[\code{\link{SupervisedTask}}]. The input \code{data.frame}
or the task containing it without the columns containing
the constant features.
}
\description{
Constant features can lead to errors in some models. Given
a \code{data.frame}, this function removes all constant
features. With the argument 'perc', there is a possibility
to also remove features for whom less than 'perc' percent
of the observations differ from the mode value.
}

