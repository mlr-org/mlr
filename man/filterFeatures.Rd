\name{filterFeatures}
\alias{filterFeatures}
\title{Filter features by thresholding filter values.}
\usage{
filterFeatures(task, method = "random.forest.importance", select = "perc",
  val, ...)
}
\arguments{
  \item{task}{[\code{\link{Task}}]\cr The task.}

  \item{method}{[\code{character(1)}]\cr See
  \code{\link{getFilterValues}}.  Default is
  \dQuote{random.forest.importance}.}

  \item{select}{[\code{character(1)}]\cr How to select
  top-scoring features.  \dQuote{perc} = select top-scoring
  percentage, \dQuote{abs} = select absolute number of
  top-scoring features, \dQuote{threshold} = select all
  features whose criterion value is >= \code{val}.  Default
  is \dQuote{perc}.}

  \item{val}{[\code{numeric(1)}]\cr Depends on
  \code{select}: Either a percentage from [0, 1], a number
  of features or a threshold value for the criterion.}

  \item{...}{[any]\cr Passed down to selected method.}
}
\value{
[\code{\link{Task}}].
}
\description{
First, calls \code{\link{getFilterValues}}.
Features are then selected via \code{select} and \code{val}.
}
\seealso{
Other filter: \code{\link{FilterResult}},
  \code{\link{getFilterResult}};
  \code{\link{FilterValues}};
  \code{\link{getFilterValues}};
  \code{\link{getFilteredFeatures}};
  \code{\link{listFilterMethods}};
  \code{\link{makeFilterWrapper}}
}

