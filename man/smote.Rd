\name{smote}
\alias{smote}
\title{Synthetic Minority Oversampling Technique to handle class imbalancy in binary classification.}
\usage{
smote(task, rate, nn = 5L)
}
\arguments{
  \item{task}{[\code{\link{Task}}]\cr The task.}

  \item{rate}{[\code{numeric(1)}]\cr Factor to upsample the
  smaller class.  Must be between 1 and \code{Inf}, where 1
  means no oversampling and 2 would mean doubling the class
  size.}

  \item{nn}{[\code{integer(1)}]\cr Number of nearest
  neighbors to consider.  Default is 5.}
}
\value{
[\code{\link{Task}}].
}
\description{
In each iteration, samples one minority class element x1, then one of x1's nearest neighbors: x2.
Both points are now interpolated / convex-combined, resulting in a new virtual data point x3
for the minority class.

The method handles factor features, too. The gower distance is used for nearest neighbor
calculation, see \code{\link[cluster]{daisy}}.
For interpolation, the new factor level for x3
is sampled from the two given levels of x1 and x2 per feature.
}
\seealso{
Other imbalancy: \code{\link{makeOverBaggingWrapper}};
  \code{\link{makeOversampleWrapper}},
  \code{\link{makeUndersampleWrapper}};
  \code{\link{oversample}}, \code{\link{undersample}}
}

