\name{performance}
\alias{performance}
\title{Measure performance of prediction.}
\usage{
performance(pred, measures, task, model)
}
\arguments{
  \item{pred}{[\code{\link{Prediction}}] \cr Prediction
  object to evaluate.}

  \item{measures}{[\code{\link{Measure}} | list of
  \code{\link{Measure}}]\cr Performance measure(s) to
  evaluate.}

  \item{task}{[\code{\link{SupervisedTask}}]\cr Learning
  task, might be requested by performance measure, usually
  not needed.}

  \item{model}{[\code{\link{WrappedModel}}]\cr Model built
  on training data, might be requested by performance
  measure, usually not needed.}
}
\value{
[named \code{numeric}]. Performance value(s), named by measure(s).
}
\description{
Measures the quality of a prediction w.r.t. some performance measure.
}
\examples{
training.set <- seq(1, nrow(iris), by = 2)
test.set <- seq(2, nrow(iris), by = 2)

task <- makeClassifTask(data = iris, target = "Species")
lrn <- makeLearner("classif.lda")
mod <- train(lrn, task, subset = training.set)
pred <- predict(mod, newdata = iris[test.set, ])
performance(pred, measures = mmce)

# Compute multiple performance measures at once
ms <- list("mmce" = mmce, "acc" = acc, "timetrain" = timetrain)
performance(pred, measures = ms, task, mod)
}
\seealso{
\code{\link{makeMeasure}}, \code{\link{measures}}
}

