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
A single numerical performance value.
}
\description{
Measures the quality of a prediction w.r.t. some
performance measure.
}
\examples{
training.set <- seq(1, nrow(iris), by = 2)
test.set <- seq(2, nrow(iris), by = 2)

task <- makeClassifTask(data = iris, target = "Species")
lrn <- makeLearner("classif.lda")
mod <- train(lrn, task, subset = training.set)
pred <- predict(mod, newdata = iris[test.set, ])

## Here we define the mean misclassification error (MMCE) as our performance measure
my.mmce <- function(task, model, pred, extra.args) {
  length(which(pred$data$response != pred$data$truth)) / nrow(pred$data)
}
ms <- makeMeasure(id = "misclassification.rate",
                  minimize = TRUE,
                  classif = TRUE,
                  allowed.pred.types = "response",
                  fun = my.mmce)
performance(pred, ms, task, mod)

## Indeed the MMCE is already implemented in mlr beside other common performance measures
performance(pred, measures = mmce)

## Compute multiple performance measures at once
ms <- list("mmce" = mmce, "acc" = acc, "timetrain" = timetrain)
sapply(ms, function(the.ms) {
  performance(pred, measures = the.ms, task, mod)
})
}
\seealso{
\code{\link{makeMeasure}}, \code{\link{measures}}
}

