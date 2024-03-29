% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculateConfusionMatrix.R
\name{calculateConfusionMatrix}
\alias{calculateConfusionMatrix}
\alias{print.ConfusionMatrix}
\title{Confusion matrix.}
\usage{
calculateConfusionMatrix(pred, relative = FALSE, sums = FALSE, set = "both")

\method{print}{ConfusionMatrix}(x, both = TRUE, digits = 2, ...)
}
\arguments{
\item{pred}{(\link{Prediction})\cr
Prediction object.}

\item{relative}{(\code{logical(1)})\cr
If \code{TRUE} two additional matrices are calculated. One is normalized by rows and one by
columns.}

\item{sums}{(\code{logical(1)})\cr
If \code{TRUE} add absolute number of observations in each group.}

\item{set}{(\code{character(1)})\cr
Specifies which part(s) of the data are used for the calculation.
If \code{set} equals \code{train} or \code{test}, the \code{pred} object must be the result of a
resampling, otherwise an error is thrown.
Defaults to \dQuote{both}. Possible values are \dQuote{train}, \dQuote{test}, or \dQuote{both}.}

\item{x}{(\link{ConfusionMatrix})\cr
Object to print.}

\item{both}{(\code{logical(1)})\cr
If \code{TRUE} both the absolute and relative confusion matrices are printed.}

\item{digits}{(\code{integer(1)})\cr
How many numbers after the decimal point should be printed, only relevant for relative confusion matrices.}

\item{...}{(any)\cr
Currently not used.}
}
\value{
(\link{ConfusionMatrix}).
}
\description{
Calculates the confusion matrix for a (possibly resampled) prediction.
Rows indicate true classes, columns predicted classes. The marginal elements count the number of
classification errors for the respective row or column, i.e., the number of errors
when you condition on the corresponding true (rows) or predicted (columns) class.
The last bottom right element displays the total amount of errors.

A list is returned that contains multiple matrices.
If \code{relative = TRUE} we compute three matrices, one with absolute values and two with relative.
The relative confusion matrices are normalized based on rows and columns respectively,
if \code{FALSE} we only compute the absolute value matrix.

The \code{print} function returns the relative matrices in
a compact way so that both row and column marginals can be seen in one matrix.
For details see \link{ConfusionMatrix}.

Note that for resampling no further aggregation is currently performed.
All predictions on all test sets are joined to a vector yhat, as are all labels
joined to a vector y. Then yhat is simply tabulated vs. y, as if both were computed on
a single test set. This probably mainly makes sense when cross-validation is used for resampling.
}
\section{Functions}{
\itemize{
\item \code{print(ConfusionMatrix)}: 

}}
\examples{
# get confusion matrix after simple manual prediction
allinds = 1:150
train = sample(allinds, 75)
test = setdiff(allinds, train)
mod = train("classif.lda", iris.task, subset = train)
pred = predict(mod, iris.task, subset = test)
print(calculateConfusionMatrix(pred))
print(calculateConfusionMatrix(pred, sums = TRUE))
print(calculateConfusionMatrix(pred, relative = TRUE))

# now after cross-validation
r = crossval("classif.lda", iris.task, iters = 2L)
print(calculateConfusionMatrix(r$pred))
}
\seealso{
Other performance: 
\code{\link{ConfusionMatrix}},
\code{\link{calculateROCMeasures}()},
\code{\link{estimateRelativeOverfitting}()},
\code{\link{makeCostMeasure}()},
\code{\link{makeCustomResampledMeasure}()},
\code{\link{makeMeasure}()},
\code{\link{measures}},
\code{\link{performance}()},
\code{\link{setAggregation}()},
\code{\link{setMeasurePars}()}
}
\concept{performance}
