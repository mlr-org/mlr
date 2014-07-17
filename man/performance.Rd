\name{performance}
\alias{performance}
\title{Measure performance of prediction.}
\usage{
performance(pred, measures, task, model)
}
\arguments{
  \item{pred}{[\code{\link{Prediction}}]\cr Prediction
  object.}

  \item{measures}{[\code{\link{Measure}} | list of
  \code{\link{Measure}}]\cr Performance measure(s) to
  evaluate.}

  \item{task}{[\code{\link{SupervisedTask}},
  \code{\link{UnsupervisedTask}}]\cr Learning task, might
  be requested by performance measure, usually not needed
  except for clustering.}

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
training.set = seq(1, nrow(iris), by = 2)
test.set = seq(2, nrow(iris), by = 2)

task = makeClassifTask(data = iris, target = "Species")
lrn = makeLearner("classif.lda")
mod = train(lrn, task, subset = training.set)
pred = predict(mod, newdata = iris[test.set, ])
performance(pred, measures = mmce)

# Compute multiple performance measures at once
ms = list("mmce" = mmce, "acc" = acc, "timetrain" = timetrain)
performance(pred, measures = ms, task, mod)
}
\seealso{
Other performance: \code{\link{Measure}},
  \code{\link{makeMeasure}}; \code{\link{acc}},
  \code{\link{auc}}, \code{\link{bac}}, \code{\link{ber}},
  \code{\link{cindex}}, \code{\link{db}}, \code{\link{f1}},
  \code{\link{fdr}}, \code{\link{featperc}},
  \code{\link{fn}}, \code{\link{fnr}}, \code{\link{fp}},
  \code{\link{fpr}}, \code{\link{gmean}},
  \code{\link{gpr}}, \code{\link{mae}}, \code{\link{mcc}},
  \code{\link{mcp}}, \code{\link{meancosts}},
  \code{\link{measures}}, \code{\link{medae}},
  \code{\link{medse}}, \code{\link{mmce}},
  \code{\link{mse}}, \code{\link{multiclass.auc}},
  \code{\link{npv}}, \code{\link{ppv}}, \code{\link{rmse}},
  \code{\link{sae}}, \code{\link{sse}},
  \code{\link{timeboth}}, \code{\link{timepredict}},
  \code{\link{timetrain}}, \code{\link{tn}},
  \code{\link{tnr}}, \code{\link{tp}}, \code{\link{tpr}};
  \code{\link{makeCostMeasure}};
  \code{\link{makeCustomResampledMeasure}}
}

