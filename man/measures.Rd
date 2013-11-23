\docType{data}
\name{measures}
\alias{acc}
\alias{auc}
\alias{ber}
\alias{f1}
\alias{fdr}
\alias{featperc}
\alias{fn}
\alias{fnr}
\alias{fp}
\alias{fpr}
\alias{gmean}
\alias{gpr}
\alias{mae}
\alias{mcc}
\alias{measures}
\alias{medae}
\alias{medse}
\alias{mmce}
\alias{mse}
\alias{multiclass.auc}
\alias{npv}
\alias{ppv}
\alias{rmse}
\alias{sae}
\alias{sse}
\alias{timeboth}
\alias{timepredict}
\alias{timetrain}
\alias{tn}
\alias{tnr}
\alias{tp}
\alias{tpr}
\title{Performance measures.}
\format{none}
\usage{
  none
}
\description{
  A performance measure is evaluated after a single
  train/predict step and returns a single number to assess
  the quality of the prediction (or maybe only the model,
  think AIC). The measure itself knows whether it wants to
  be minimized or maximized and for what tasks it is
  applicable. See below for a list of already implemented
  measures. If you want a measure for a misclassification
  cost matrix, look at \code{\link{makeCostMeasure}}. If
  you want to implement your own measure, look at
  \code{\link{makeMeasure}}.

  Classification: \itemize{ \item{\bold{mmce}}{\cr Mean
  misclassification error.} \item{\bold{acc}}{\cr
  Accuracy.} \item{\bold{ber}}{\cr Balanced error rate.
  Mean of misclassification error rates on all individual
  classes.} \item{\bold{tp}}{\cr True positives.}
  \item{\bold{tpr}}{\cr True positive rate, also called hit
  rate or recall.} \item{\bold{fp}}{\cr False positives,
  also called false alarms.} \item{\bold{fpr}}{\cr False
  positive rate, also called false alarm rate or fall-out.}
  \item{\bold{tn}}{\cr True negatives, also called correct
  rejections.} \item{\bold{tnr}}{\cr True negative rate.
  Also called specificity.} \item{\bold{fn}}{\cr False
  negatives, also called misses.} \item{\bold{fnr}}{\cr
  False negative rate.} \item{\bold{ppv}}{\cr Positive
  predictive value, also called precision.}
  \item{\bold{npv}}{\cr Negative predictive value.}
  \item{\bold{fdr}}{\cr False discovery rate.}
  \item{\bold{f1}}{\cr F1 measure.} \item{\bold{mcc}}{\cr
  Matthews correlation coefficient.}
  \item{\bold{gmean}}{\cr G-mean, geometric mean of recall
  and specificity.} \item{\bold{gpr}}{\cr Geometric mean of
  precision and recall.} \item{\bold{auc}}{\cr Area under
  the curve.} \item{\bold{multiclass.auc}}{\cr Area under
  the curve for multiclass problems. Calls
  \code{pROC::multiclass.roc}.} } Only \code{mmce},
  \code{acc}, \code{multiclass.auc} and \code{ber} can be
  used for multiclass problems.

  Regression: \itemize{ \item{\bold{sse}}{\cr Sum of
  squared errors} \item{\bold{mse}}{\cr Mean of squared
  errors} \item{\bold{medse}}{\cr Median of squared errors}
  \item{\bold{sae}}{\cr Sum of absolute errors}
  \item{\bold{mae}}{\cr Mean of absolute errors}
  \item{\bold{medae}}{\cr Median of absolute errors}
  \item{\bold{rmse}}{\cr Root mean square error} }

  General: \itemize{ \item{\bold{timetrain}}{\cr Time of
  fitting the model} \item{\bold{timepredict}}{\cr Time of
  predicting test set} \item{\bold{timeboth}}{\cr timetrain
  + trainpredict} \item{\bold{featperc}}{\cr Percentage of
  original features used for model, useful for feature
  selection.} }
}
\keyword{datasets}

