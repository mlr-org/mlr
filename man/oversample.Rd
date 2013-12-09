\name{oversample}
\alias{oversample}
\alias{undersample}
\title{Over- or undersample binary classification task to handle class imbalancy.}
\usage{
oversample(obj, target, rate)

undersample(obj, target, rate)
}
\arguments{
  \item{obj}{[\code{data.frame} |
  \code{\link{ClassifTask}}]\cr Input data.}

  \item{target}{[\code{character(1)}]\cr Name of the column
  specifying the response.  Only used when \code{obj} is a
  data.frame, otherwise ignored.}

  \item{rate}{[\code{numeric(1)}]\cr Factor to upsample the
  smaller or downsample the bigger class.  For
  undersampling: Must be between 0 and 1, where 1 means no
  downsampling, 0.5 implies reduction to 50 percent and 0
  would imply reduction to 0 observations.  For
  oversampling: Must be between 1 and \code{Inf}, where 1
  means no oversampling and 2 would mean doubling the class
  size.}
}
\value{
[\code{data.frame} | \code{\link{ClassifTask}}]. Same type
as \code{obj}.
}
\description{
Oversampling: From the smaller class, observations are
randomly drawn with repetitions.

Undersampling: From the larger class, observations are
randomly drawn without repetitions.
}

