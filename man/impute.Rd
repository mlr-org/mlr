\name{impute}
\alias{imp.const}
\alias{imp.hist}
\alias{imp.max}
\alias{imp.median}
\alias{imp.min}
\alias{imp.mode}
\alias{imp.normal}
\alias{impute}
\title{Impute and re-impute data}
\usage{
  impute(data, target, classes = list(), cols = list(),
    dummies = character(0L), impute.newlevels = TRUE)

  imp.const(const)

  imp.median()

  imp.mode()

  imp.min(multiplier = 1)

  imp.max(multiplier = 1)

  imp.normal()

  imp.hist(breaks, use.mids = TRUE)
}
\arguments{
  \item{data}{[\code{data.frame}]\cr Input data.}

  \item{target}{[\code{character}]\cr Name of the column
  specifying the response.}

  \item{classes}{[\code{named list}]\cr Named list
  containing imputation techniques for classes of columns.
  E.g. \code{list(integer = 0)}.}

  \item{cols}{[\code{named list}]\cr Named list containing
  names of the built-in imputation methods to impute
  missing values in the data column referenced by the list
  element's name. Overwrites imputation set via
  \code{classes}.}

  \item{dummies}{[\code{character()}]\cr Set of column
  names for which dummy variables (binary missing
  indicator) should be created.  Default is
  \code{character(0)}.}

  \item{impute.newlevels}{[\code{logical(1)}]\cr Impute new
  levels in factors the same way as missing values? Default
  is \code{TRUE}.}

  \item{const}{[any]\cr Any object to use for imputation.}

  \item{multiplier}{[\code{numeric(1)}]\cr Numeric value to
  multiply the minimum or maximum.}

  \item{breaks}{[\code{numeric(1)}]\cr Number of breaks to
  use in \code{\link[graphics]{hist}}. If missing, defaults
  to auto-detection via \dQuote{Sturges}.}

  \item{use.mids}{[\code{logical(1)}]\cr If \code{x} is
  numeric and a histogram is used, impute with bin mids
  (default) or instead draw uniformly distributed samples
  within bin range.}
}
\value{
  \code{list} with two named elements \dQuote{data} and
  \dQuote{desc}.
}
\description{
  These functions provide some convenience for imputation
  of missing values. A noteworthy feature is given by the
  possibility to re-impute a data set in the same way as
  learned previously which especially becomes handy in
  combination with resampling strategies.
}
\details{
  The function \code{impute} performs the imputations on a
  data set and returns, alongside with the imputed data
  set, an \dQuote{ImputationDesc} object which then can be
  passed together with a new data set to
  \code{\link{reimpute}}.

  The imputation techniques can be specified for certain
  features or using the feature classes, see parameters.
  You can either provide an arbitrary object use a built-in
  imputation method or create one yourself using ... FIXME
  we should provide a constructor and document it. The
  built-ins are: \itemize{ \item \code{imp.const(const)}
  for imputation using a constant value, \item
  \code{imp.median()} for imputation using the median,
  \item \code{imp.mode()} for imputation using the mode,
  \item \code{imp.min(multiplier)} for imputation using the
  minimum, \item \code{imp.max(multiplier)} for imputation
  using the maximum, \item \code{imp.normal()} for
  imputation using normally distributed random values with
  mean and variance estimated from the data, \item
  \code{imp.hist(breaks, use.mids)} for imputation using
  random values with probabilities calculated using
  \code{table} or \code{hist}. }
}
\examples{
df = data.frame(x = c(1, 1, NA), y = factor(c("a", "a", "b")), z=1:3)
 imputed = impute(df, target=character(0), cols=list(x = 99, y = imp.mode()))
 print(imputed$data)
 reimpute(data.frame(x=NA), imputed$desc)
}

