\name{getConfMatrix}
\alias{getConfMatrix}
\title{Confusion matrix.}
\usage{
getConfMatrix(pred, relative = FALSE)
}
\arguments{
  \item{pred}{[\code{\link{Prediction}}]\cr Prediction
  object.}

  \item{relative}{[\code{logical(1)}]\cr If \code{TRUE}
  rows are normalized to show relative frequencies.
  Default is \code{FALSE}.}
}
\value{
[\code{matrix}]. A confusion matrix.
}
\description{
Calculates confusion matrix for (possibly resampled) prediction.
Rows indicate true classes, columns predicted classes.
}
\details{
Code inspired by \code{\link[klaR]{errormatrix}}.
}
\examples{
## create classification task and use linear discriminant analysis for classification
task = makeClassifTask(data = iris, target = "Species")
lrn = makeLearner("classif.lda")

## set up training and test data
n = nrow(iris)
mixed.set = sample(1:n)
training.set = mixed.set[1:(n/2)]
test.set = mixed.set[(n/2 + 1):n]

## train model
mod = train(lrn, task, subset = training.set)

## get predictions and show calculate confusion matrix
pred = predict(mod, newdata = iris[test.set, ])
print(getConfMatrix(pred))
print(getConfMatrix(pred, relative = TRUE))
}
\seealso{
\code{\link{predict.WrappedModel}}
}

