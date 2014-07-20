\name{makeCustomResampledMeasure}
\alias{makeCustomResampledMeasure}
\title{Construct your own resampled performance measure.}
\usage{
makeCustomResampledMeasure(id, minimize = TRUE, properties = character(0L),
  allowed.pred.types = character(0L), fun, extra.args = list(),
  best = NULL, worst = NULL)
}
\arguments{
  \item{id}{[\code{character(1)}]\cr Name of aggregated
  measure.}

  \item{fun}{[\code{function(task, pred, group, pred,
  extra.args)}]\cr Calculates performance value from
  \code{\link{ResamplePrediction}} object.  For rare case
  you can also use the task, the grouping or the extra
  arguments \code{extra.args}.}

  \item{extra.args}{[\code{list}]\cr List of extra
  arguments which will always be passed to \code{fun}.
  Default is empty list.}

  \item{minimize}{[\code{logical(1)}]\cr Should the measure
  be minimized?  Default is in \code{TRUE}.}

  \item{properties}{[\code{character}]\cr Set of measure
  properties. Some standard property names include:
  \describe{ \item{classif}{Is the measure applicable for
  classification?} \item{classif.multi}{Is the measure
  applicable for multi-class classification?}
  \item{regr}{Is the measure applicable for regression?}
  \item{surv}{Is the measure applicable for survival?}
  \item{costsens}{Is the measure applicable for
  cost-sensitve learning?} } Default is
  \code{character(0)}.}

  \item{allowed.pred.types}{[\code{character}]\cr Which
  prediction types are allowed for this measure?  Subset of
  \dQuote{response},\dQuote{prob}, \dQuote{se}.  Default is
  \code{character(0)}.}

  \item{best}{[\code{numeric(1)}]\cr Best obtainable value
  for measure.  Default is -\code{Inf} or \code{Inf},
  depending on \code{minimize}.}

  \item{worst}{[\code{numeric(1)}]\cr Worst obtainable
  value for measure.  Default is \code{Inf} or -\code{Inf},
  depending on \code{minimize}.}
}
\value{
[\code{\link{Measure}}].
}
\description{
Construct your own performance measure, used after resampling.
Note that individual training / test set performance values will be set to \code{NA}, you
only calculate an aggregated value. If you can define a function that makes sense
for every single training / test set, implement your own \code{\link{Measure}}.
}
\seealso{
Other performance: \code{\link{G1}}, \code{\link{G2}},
  \code{\link{acc}}, \code{\link{auc}}, \code{\link{bac}},
  \code{\link{ber}}, \code{\link{cindex}},
  \code{\link{db}}, \code{\link{dunn}}, \code{\link{f1}},
  \code{\link{fdr}}, \code{\link{featperc}},
  \code{\link{fn}}, \code{\link{fnr}}, \code{\link{fp}},
  \code{\link{fpr}}, \code{\link{gmean}},
  \code{\link{gpr}}, \code{\link{mae}}, \code{\link{mcc}},
  \code{\link{mcp}}, \code{\link{meancosts}},
  \code{\link{measures}}, \code{\link{medae}},
  \code{\link{medse}}, \code{\link{mmce}},
  \code{\link{mse}}, \code{\link{multiclass.auc}},
  \code{\link{npv}}, \code{\link{ppv}}, \code{\link{rmse}},
  \code{\link{sae}}, \code{\link{silhouette}},
  \code{\link{sse}}, \code{\link{timeboth}},
  \code{\link{timepredict}}, \code{\link{timetrain}},
  \code{\link{tn}}, \code{\link{tnr}}, \code{\link{tp}},
  \code{\link{tpr}}; \code{\link{Measure}},
  \code{\link{makeMeasure}}; \code{\link{makeCostMeasure}};
  \code{\link{performance}}
}

