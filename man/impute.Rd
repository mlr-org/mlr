\name{imputeConstant}
\alias{impute}
\alias{imputeConstant}
\alias{imputeHist}
\alias{imputeMax}
\alias{imputeMedian}
\alias{imputeMin}
\alias{imputeMode}
\alias{imputeNormal}
\title{Impute and re-impute data}
\usage{
imputeConstant(const)

imputeMedian()

imputeMode()

imputeMin(multiplier = 1)

imputeMax(multiplier = 1)

imputeNormal(mu, sd)

imputeHist(breaks, use.mids = TRUE)

impute(data, target, classes = list(), cols = list(),
  dummies = character(0L), impute.newlevels = TRUE,
  resort.factor.levels = TRUE)
}
\arguments{
  \item{const}{[any]\cr Any object to use for imputation.}

  \item{multiplier}{[\code{numeric(1)}]\cr Numeric value to
  multiply the minimum or maximum.}

  \item{mu}{[\code{numeric(1)}]\cr Mean of normal
  distribution. If missing it will get estimated from the
  data.}

  \item{sd}{[\code{numeric(1)}]\cr Standard deviation of
  normal distribution. If missing it will get estimated
  from the data.}

  \item{breaks}{[\code{numeric(1)}]\cr Number of breaks to
  use in \code{\link[graphics]{hist}}. If missing, defaults
  to auto-detection via \dQuote{Sturges}.}

  \item{use.mids}{[\code{logical(1)}]\cr If \code{x} is
  numeric and a histogram is used, impute with bin mids
  (default) or instead draw uniformly distributed samples
  within bin range.}

  \item{data}{[\code{data.frame}]\cr Input data.}

  \item{target}{[\code{character(1)}]\cr Name of the column
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
}
\value{
\code{list} with two named elements \dQuote{data} and
\dQuote{desc}.
}
\description{
These functions provide some convenience for imputation of
missing values. A noteworthy feature is given by the
possibility to re-impute a data set in the same way as
learned previously. This especially becomes handy in
whenever resampling is deployed.
}
\details{
The function \code{impute} performs the imputations on a
data set and returns, alongside with the imputed data set,
an \dQuote{ImputationDesc} object which then can be passed
together with a new data set to \code{\link{reimpute}}.

The imputation techniques can be specified for certain
features or for feature classes, see parameters. You can
either provide an arbitrary object, use a built-in
imputation method or create one yourself using
\code{\link{makeImputeMethod}}. The built-ins are:
\itemize{ \item \code{imputeConstant(const)} for imputation
using a constant value, \item \code{imputeMedian()} for
imputation using the median, \item \code{imputeMode()} for
imputation using the mode, \item
\code{imputeMin(multiplier)} for imputation using the
minimum, \item \code{imputeMax(multiplier)} for imputation
using the maximum, \item \code{imputeNormal(mean, sd)} for
imputation using normally distributed random values. Mean
and standard deviation will be calculated from the data if
not provided.  \item \code{imputeHist(breaks, use.mids)}
for imputation using random values with probabilities
calculated using \code{table} or \code{hist}. }
}
\examples{
df = data.frame(x = c(1, 1, NA), y = factor(c("a", "a", "b")), z=1:3)
 imputed = impute(df, target=character(0), cols=list(x = 99, y = imputeMode()))
 print(imputed$data)
 reimpute(data.frame(x=NA), imputed$desc)
}

