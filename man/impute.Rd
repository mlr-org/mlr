\name{impute}
\alias{impute}
\title{Impute and re-impute data}
\usage{
impute(data, target, classes = list(), cols = list(),
  dummies = character(0L), impute.new.levels = TRUE,
  recode.factor.levels = TRUE)
}
\arguments{
  \item{data}{[\code{data.frame}]\cr Input data.}

  \item{target}{[\code{character}]\cr Name of the column
  specifying the response.}

  \item{classes}{[\code{named list}]\cr Named list
  containing imputation techniques for classes of columns.
  E.g. \code{list(numeric = imputeMedian)}.}

  \item{cols}{[\code{named list}]\cr Named list containing
  names of the built-in imputation methods to impute
  missing values in the data column referenced by the list
  element's name. Overwrites imputation set via
  \code{classes}.}

  \item{dummies}{[\code{character}]\cr Column names for
  which dummy variables (binary missing indicator) should
  be created.  Default is \code{character(0)}.}

  \item{impute.new.levels}{[\code{logical(1)}]\cr If new,
  unencountered factor level occur during reimputation,
  should these be handles as NAs and then be imputed the
  same way?  Default is \code{TRUE}.}

  \item{recode.factor.levels}{[\code{logical(1)}]\cr Recode
  factor levels after reimputation, so they match the
  respective element of \code{lvls} (in the description
  object) and therefore match the levels of the feature
  factor in the training data after imputation?.  Default
  is \code{TRUE}.}
}
\value{
[\code{list}] \item{data \code{data.frame}}{Imputed data.}
\item{desc \code{ImputationDesc}}{Description object.}
}
\description{
Allows imputation of missing feature values through various
techniques. Note that you have the possibility to re-impute
a data set in the same way as the imputation was performed
during training. This especially comes in handy during
resampling when one wants to perform the same imputation on
the test set as on the training set.

The function \code{impute} performs the imputation on a
data set and returns, alongside with the imputed data set,
an \dQuote{ImputationDesc} object which can contain
\dQuote{learned} coefficients and helpful data. It can then
be passed together with a new data set to
\code{\link{reimpute}}.

The imputation techniques can be specified for certain
features or for feature classes, see function arguments.

You can either provide an arbitrary object, use a built-in
imputation method listed under \code{\link{imputations}} or
create one yourself using \code{\link{makeImputeMethod}}.
}
\details{
The description object contains these slots \describe{
\item{target [\code{character}]}{See argument.}
\item{features [\code{character}]}{Feature names, these are
the column names of \code{data}, excluding \code{target}.}
\item{lvls [\code{named list}]}{Mapping of column names of
factor features to their levels, including newly created
ones during imputation.} \item{impute [\code{named
list}]}{Mapping of column names to imputation functions.}
\item{dummies [\code{named list}]}{Mapping of column names
to imputation functions.} \item{impute.new.levels
[\code{logical(1)}]}{See argument.}
\item{recode.factor.levels [\code{logical(1)}]}{See
argument.} }
}
\examples{
df = data.frame(x = c(1, 1, NA), y = factor(c("a", "a", "b")), z=1:3)
imputed = impute(df, target=character(0), cols=list(x = 99, y = imputeMode()))
print(imputed$data)
}

