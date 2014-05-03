\name{getFeatureFilterValues}
\alias{getFeatureFilterValues}
\title{Calculates feature filter values by using a numerical importance criterion.}
\usage{
getFeatureFilterValues(obj, target, method = "random.forest.importance")
}
\arguments{
  \item{obj}{[\code{\link{data.frame}} |
  \code{\link{SupervisedTask}}]\cr The data set or task.}

  \item{target}{[\code{character}]\cr Name of the column(s)
  specifying the response if you passed a
  \code{data.frame}.  User input is ignored if you pass a
  task and \code{target} is automatically set.  Never
  removed.}

  \item{method}{[\code{character(1)}]\cr Filter method.
  Available are:\cr \dQuote{linear.correlation}: Pearson's
  correlation for regression with numerical data\cr
  \dQuote{rank.correlation}: Spearman's correlation for
  regression with numerical data\cr
  \dQuote{information.gain}: Entropy-based information gain
  for classification and regression of mixed feature
  sets\cr \dQuote{gain.ratio}: Entropy-based gain ratio for
  classification and regression of mixed feature sets\cr
  \dQuote{symmetrical.uncertainty}: Entropy-based
  symmetrical uncertainty for classification and regression
  of mixed feature sets\cr \dQuote{chi.squared}: Finds
  weights of mixed feature sets using chi-squared test for
  classification and regression\cr
  \dQuote{random.forest.importance}: Finds weights of mixed
  feature sets using RandomForest algorithm for
  classification and regression\cr \dQuote{relief}: Finds
  weights of mixed feature sets using distances between
  instances for classification and regression\cr
  \dQuote{oneR}: Finds weights of mixed feature sets using
  association rules for classification and regression\cr
  Default is \dQuote{random.forest.importance}.}
}
\value{
[\code{numeric}]. A named numeric vector that contains an
importance value for each feature.
}
\description{
Calculates numerical importance values for all features.
Thresholding of these values can be used to select
\dQuote{useful} features. Look at package
\code{\link[FSelector]{FSelector}} for details on the
filter algorithms.
}
\seealso{
\code{\link{makeFilterWrapper}}
}

