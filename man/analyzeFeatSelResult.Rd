\name{analyzeFeatSelResult}
\alias{analyzeFeatSelResult}
\title{Show and visualized the steps of the feature selection.}
\usage{
analyzeFeatSelResult(res, reduce = TRUE, printed.features = 10L)
}
\arguments{
  \item{res}{[\code{FeatSelResult(1)}]\cr The result of of
  \code{\link{selectFeatures}}.}

  \item{reduce}{[\code{logical(1)}]\cr Per iteration: Print
  only the selected feature or all features that were
  evaluated?  Default is \code{TRUE}.}
}
\value{
Nothing.
}
\description{
This function prints the steps \code{\link{selectFeatures}}
took to find it's optimal set of features and the reason
why it stopped. It can also give information about all
calculations done on each intermediate step. Currently only
implemented for sequential feature selection.
}

