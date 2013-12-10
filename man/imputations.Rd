\name{imputations}
\alias{imputations}
\alias{imputeConstant}
\alias{imputeHist}
\alias{imputeLearner}
\alias{imputeMax}
\alias{imputeMedian}
\alias{imputeMin}
\alias{imputeMode}
\alias{imputeNormal}
\alias{imputeUniform}
\title{Built in imputation methods
The built-ins are:
\itemize{
  \item \code{imputeConstant(const)} for imputation using a constant value,
  \item \code{imputeMedian()} for imputation using the median,
  \item \code{imputeMode()} for imputation using the mode,
  \item \code{imputeMin(multiplier)} for imputation using the minimum,
  \item \code{imputeMax(multiplier)} for imputation using the maximum,
  \item \code{imputeNormal(mean, sd)} for imputation using normally
    distributed random values. Mean and standard deviation will be calculated
    from the data if not provided.
  \item \code{imputeHist(breaks, use.mids)} for imputation using random values
    with probabilities calculated using \code{table} or \code{hist}.
}}
\usage{
  imputeConstant(const)

  imputeMedian()

  imputeMode()

  imputeMin(multiplier = 1)

  imputeMax(multiplier = 1)

  imputeUniform(min, max)

  imputeNormal(mu, sd)

  imputeHist(breaks, use.mids = TRUE)

  imputeLearner(learner, preimpute = list())
}
\arguments{
  \item{const}{[any]\cr Constant valued use for
  imputation.}

  \item{multiplier}{[\code{numeric(1)}]\cr Value that
  stored minimum or maximum is multiplied with when
  imputation is done.}

  \item{min}{[\code{numeric(1)}]\cr Lower bound for uniform
  distribution. If missing it will be estimated from the
  data.}

  \item{max}{[\code{numeric(1)}]\cr Upper bound for uniform
  distribution. If missing it will be estimated from the
  data.}

  \item{mu}{[\code{numeric(1)}]\cr Mean of normal
  distribution. If missing it will be estimated from the
  data.}

  \item{sd}{[\code{numeric(1)}]\cr Standard deviation of
  normal distribution. If missing it will be estimated from
  the data.}

  \item{breaks}{[\code{numeric(1)}]\cr Number of breaks to
  use in \code{\link[graphics]{hist}}. If missing, defaults
  to auto-detection via \dQuote{Sturges}.}

  \item{use.mids}{[\code{logical(1)}]\cr If \code{x} is
  numeric and a histogram is used, impute with bin mids
  (default) or instead draw uniformly distributed samples
  within bin range.}
}
\description{
  Built in imputation methods The built-ins are: \itemize{
  \item \code{imputeConstant(const)} for imputation using a
  constant value, \item \code{imputeMedian()} for
  imputation using the median, \item \code{imputeMode()}
  for imputation using the mode, \item
  \code{imputeMin(multiplier)} for imputation using the
  minimum, \item \code{imputeMax(multiplier)} for
  imputation using the maximum, \item
  \code{imputeNormal(mean, sd)} for imputation using
  normally distributed random values. Mean and standard
  deviation will be calculated from the data if not
  provided.  \item \code{imputeHist(breaks, use.mids)} for
  imputation using random values with probabilities
  calculated using \code{table} or \code{hist}. }
}

