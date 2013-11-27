\name{reimpute}
\alias{reimpute}
\title{Re-impute a data set}
\usage{
  reimpute(x, desc, .initial = FALSE)
}
\arguments{
  \item{x}{[\code{data.frame}]\cr Object to reimpute.
  Currently only data frames are supported.}

  \item{desc}{[\code{ImputationDesc}]\cr Imputation
  description as returned by \code{\link{impute}}.}

  \item{.initial}{[\code{logical(1)}]\cr Internally used to
  disable some imputations for the first run of impute.}
}
\value{
  Imputated \code{x}.
}
\description{
  This function accepts a data set and a imputation
  description returned by \code{\link{impute}} to perform
  the following actions: \enumerate{ \item Restore dropped
  columns, setting them to \code{NA} \item Add dummy
  variables for columns as specified in \code{impute} \item
  Optionally check factors for new levels to treat them as
  \code{NA}s \item Reorder factor levels to ensure the same
  integer representation \item Impute missing values using
  data collected on original data set }
}

