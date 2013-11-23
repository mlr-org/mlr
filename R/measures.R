#' @title Performance measures.
#'
#' @description
#' A performance measure is evaluated after a single train/predict step and returns a single number to assess the quality
#' of the prediction (or maybe only the model, think AIC).
#' The measure itself knows whether it wants to be minimized or maximized and for what tasks it is applicable.
#' See below for a list of already implemented measures.
#' If you want a measure for a misclassification cost matrix, look at \code{\link{makeCostMeasure}}.
#' If you want to implement your own measure, look at \code{\link{makeMeasure}}.
#'
#' Classification:
#' \itemize{
#'   \item{\bold{mmce}}{\cr Mean misclassification error.}
#'   \item{\bold{acc}}{\cr Accuracy.}
#'   \item{\bold{ber}}{\cr Balanced error rate. Mean of misclassification error rates on all individual classes.}
#'   \item{\bold{tp}}{\cr True positives.}
#'   \item{\bold{tpr}}{\cr True positive rate, also called hit rate or recall.}
#'   \item{\bold{fp}}{\cr False positives, also called false alarms.}
#'   \item{\bold{fpr}}{\cr False positive rate, also called false alarm rate or fall-out.}
#'   \item{\bold{tn}}{\cr True negatives, also called correct rejections.}
#'   \item{\bold{tnr}}{\cr True negative rate. Also called specificity.}
#'   \item{\bold{fn}}{\cr False negatives, also called misses.}
#'   \item{\bold{fnr}}{\cr False negative rate.}
#'   \item{\bold{ppv}}{\cr Positive predictive value, also called precision.}
#'   \item{\bold{npv}}{\cr Negative predictive value.}
#'   \item{\bold{fdr}}{\cr False discovery rate.}
#'   \item{\bold{f1}}{\cr F1 measure.}
#'   \item{\bold{mcc}}{\cr Matthews correlation coefficient.}
#'   \item{\bold{gmean}}{\cr G-mean, geometric mean of recall and specificity.}
#'   \item{\bold{gpr}}{\cr Geometric mean of precision and recall.}
#'   \item{\bold{auc}}{\cr Area under the curve.}
#'   \item{\bold{multiclass.auc}}{\cr Area under the curve for multiclass problems. Calls \code{pROC::multiclass.roc}.}
#' }
#' Only \code{mmce}, \code{acc}, \code{multiclass.auc} and \code{ber} can be used for multiclass problems.
#'
#' Regression:
#' \itemize{
#'   \item{\bold{sse}}{\cr Sum of squared errors}
#'   \item{\bold{mse}}{\cr Mean of squared errors}
#'   \item{\bold{medse}}{\cr Median of squared errors}
#'   \item{\bold{sae}}{\cr Sum of absolute errors}
#'   \item{\bold{mae}}{\cr Mean of absolute errors}
#'   \item{\bold{medae}}{\cr Median of absolute errors}
#'   \item{\bold{rmse}}{\cr Root mean square error}
#' }
#'
#' General:
#' \itemize{
#'   \item{\bold{timetrain}}{\cr Time of fitting the model}
#'   \item{\bold{timepredict}}{\cr Time of predicting test set}
#'   \item{\bold{timeboth}}{\cr timetrain + trainpredict}
#'   \item{\bold{featperc}}{\cr Percentage of original features used for model, useful for feature selection.}
#' }
#' @export
#' @usage none
#' @format none
measures = function() {}


#general
#' @export featperc
#' @rdname measures
#' @usage none
#' @format none
featperc = makeMeasure(id="featperc", minimize=TRUE, classif=TRUE, regr=TRUE, allowed.pred.types=c("response", "prob"),
  fun=function(task, model, pred, extra.args) {
    length(model$features) / sum(pred$task.desc$n.feat)
  }
)

#' @export timetrain
#' @rdname measures
#' @usage none
#' @format none
timetrain = makeMeasure(id="timetrain", minimize=TRUE, classif=TRUE, regr=TRUE, allowed.pred.types=c("response", "prob"),
  fun=function(task, model, pred, extra.args) {
    model$time
  }
)

#' @export timepredict
#' @rdname measures
#' @usage none
#' @format none
timepredict = makeMeasure(id="timepredict", minimize=TRUE, classif=TRUE, regr=TRUE, allowed.pred.types=c("response", "prob"),
  fun=function(task, model, pred, extra.args) {
    pred$time
  }
)

#' @export timeboth
#' @rdname measures
#' @usage none
#' @format none
timeboth = makeMeasure(id="timeboth", minimize=TRUE, classif=TRUE, regr=TRUE, allowed.pred.types=c("response", "prob"),
  fun=function(task, model, pred, extra.args) {
    model$time + pred$time
  }
)

### regression ###

#' @export sse
#' @rdname measures
#' @usage none
#' @format none
sse = makeMeasure(id="sse", minimize=TRUE, regr=TRUE, allowed.pred.types=c("response", "se"),
  fun=function(task, model, pred, extra.args) {
    sum((pred$data$response - pred$data$truth)^2)
  }
)

#' @export mse
#' @rdname measures
#' @usage none
#' @format none
mse = makeMeasure(id="mse", minimize=TRUE, regr=TRUE, allowed.pred.types=c("response", "se"),
  fun=function(task, model, pred, extra.args) {
    mean((pred$data$response - pred$data$truth)^2)
  }
)

#' @export rmse
#' @format none 
#' @rdname measures
#' @usage none
#' @format none
rmse = makeMeasure(id="rmse", minimize=TRUE, regr=TRUE, allowed.pred.types=c("response", "se"),
  fun=function(task, model, pred, extra.args) {
    mean((pred$data$response - pred$data$truth)^2)
  },
  aggr = test.sqrt.of.mean
)

#' @export medse
#' @rdname measures
#' @usage none
#' @format none
medse = makeMeasure(id="medse", minimize=TRUE, regr=TRUE, allowed.pred.types=c("response", "se"),
  fun=function(task, model, pred, extra.args) {
    median((pred$data$response - pred$data$truth)^2)
  }
)

#' @export sae
#' @rdname measures
#' @usage none
#' @format none
sae = makeMeasure(id="sae", minimize=TRUE, regr=TRUE, allowed.pred.types=c("response", "se"),
  fun=function(task, model, pred, extra.args) {
    sum(abs(pred$data$response - pred$data$truth))
  }
)

#' @export mae
#' @rdname measures
#' @usage none
#' @format none
mae = makeMeasure(id="mae", minimize=TRUE, regr=TRUE, allowed.pred.types=c("response", "se"),
  fun=function(task, model, pred, extra.args) {
    mean(abs(pred$data$response - pred$data$truth))
  }
)

#' @export medae
#' @rdname measures
#' @usage none
#' @format none
medae = makeMeasure(id="medae", minimize=TRUE, regr=TRUE, allowed.pred.types=c("response", "se"),
  fun=function(task, model, pred, extra.args) {
    median(abs(pred$data$response - pred$data$truth))
  }
)


# classif_multi

#' @export mmce
#' @rdname measures
#' @usage none
#' @format none
mmce = makeMeasure(id="mmce", minimize=TRUE, classif=TRUE, allowed.pred.types=c("response", "prob"),
  fun=function(task, model, pred, extra.args) {
    mean(pred$data$response != pred$data$truth)
  }
)

#' @export acc
#' @rdname measures
#' @usage none
#' @format none
acc = makeMeasure(id="acc", minimize=FALSE, classif=TRUE, allowed.pred.types=c("response", "prob"),
  fun=function(task, model, pred, extra.args) {
    mean(pred$data$response == pred$data$truth)
  }
)

#' @export ber
#' @rdname measures
#' @usage none
#' @format none
ber = makeMeasure(id="ber", minimize=TRUE, classif=TRUE, allowed.pred.types=c("response", "prob"),
  fun=function(task, model, pred, extra.args) {
    n = length(pred$task.desc$class.levels) + 1L
    mean(getConfMatrix(pred, relative=TRUE)[-n, n])
  }
)

#' @export multiclass.auc
#' @rdname measures
#' @usage none
#' @format none
multiclass.auc = makeMeasure(id="multiclass.auc", minimize=FALSE, classif=TRUE, only.binary=FALSE, allowed.pred.types=c("response", "prob"),
  fun=function(task, model, pred, extra.args) {
    # pROC does allow NAs
    requirePackages("pROC", "multiclass.auc")
    auc = pROC::multiclass.roc(response=pred$data$response, predictor=as.matrix(getProbabilities(pred)))$auc
    as.numeric(auc)
  }
)


# classif_two

#' @export auc
#' @rdname measures
#' @usage none
#' @format none
auc = makeMeasure(id="auc", minimize=FALSE, classif=TRUE, only.binary=TRUE, allowed.pred.types="prob",
  fun=function(task, model, pred, extra.args) {
    # ROCR does not work with NAs
    if (any(is.na(pred$data$response)) || length(unique(pred$data$truth)) == 1L)
      return(NA_real_)
    rpreds = asROCRPrediction(pred)
    ROCR::performance(rpreds, "auc")@y.values[[1L]]
  }
)

#' @export tp
#' @rdname measures
#' @usage none
#' @format none
tp = makeMeasure(id="tp", minimize=FALSE, classif=TRUE, only.binary=TRUE, allowed.pred.types=c("response", "prob"),
  fun=function(task, model, pred, extra.args) {
    sum(pred$data$truth == pred$data$response & pred$data$response == pred$task.desc$positive)
  }
)

#' @export tn
#' @rdname measures
#' @usage none
#' @format none
tn = makeMeasure(id="tn", minimize=FALSE, classif=TRUE, only.binary=TRUE, allowed.pred.types=c("response", "prob"),
  fun=function(task, model, pred, extra.args) {
    sum(pred$data$truth == pred$data$response & pred$data$response == pred$task.desc$negative)
  }
)

#' @export fp
#' @rdname measures
#' @usage none
#' @format none
fp = makeMeasure(id="fp", minimize=TRUE, classif=TRUE, only.binary=TRUE, allowed.pred.types=c("response", "prob"),
  fun=function(task, model, pred, extra.args) {
    sum(pred$data$truth != pred$data$response & pred$data$response == pred$task.desc$positive)
  }
)

#' @export fn
#' @rdname measures
#' @usage none
#' @format none
fn = makeMeasure(id="fn", minimize=TRUE, classif=TRUE, only.binary=TRUE, allowed.pred.types=c("response", "prob"),
  fun=function(task, model, pred, extra.args) {
    sum(pred$data$truth != pred$data$response & pred$data$response == pred$task.desc$negative)
  }
)


#' @export tpr
#' @rdname measures
#' @usage none
#' @format none
tpr = makeMeasure(id="tpr", minimize=FALSE, classif=TRUE, only.binary=TRUE, allowed.pred.types=c("response", "prob"),
  fun=function(task, model, pred, extra.args) {
    tp$fun(pred=pred) / sum(pred$data$truth == pred$task.desc$positive)
  }
)

#' @export tnr
#' @rdname measures
#' @usage none
#' @format none
tnr = makeMeasure(id="tnr", minimize=FALSE, classif=TRUE, only.binary=TRUE, allowed.pred.types=c("response", "prob"),
  fun=function(task, model, pred, extra.args) {
    tn$fun(pred=pred) / sum(pred$data$truth == pred$task.desc$negative)
  }
)

#' @export fpr
#' @rdname measures
#' @usage none
#' @format none
fpr = makeMeasure(id="fpr", minimize=TRUE, classif=TRUE, only.binary=TRUE, allowed.pred.types=c("response", "prob"),
  fun=function(task, model, pred, extra.args) {
    fp$fun(pred=pred) / sum(pred$data$truth == pred$task.desc$negative)
  }
)

#' @export fnr
#' @rdname measures
#' @usage none
#' @format none
fnr = makeMeasure(id="fnr", minimize=TRUE, classif=TRUE, only.binary=TRUE, allowed.pred.types=c("response", "prob"),
  fun=function(task, model, pred, extra.args) {
    fn$fun(pred=pred) / sum(pred$data$truth == pred$task.desc$positive)
  }
)

#' @export ppv
#' @rdname measures
#' @usage none
#' @format none
ppv = makeMeasure(id="ppv", minimize=FALSE, classif=TRUE, only.binary=TRUE, allowed.pred.types=c("response", "prob"),
  fun=function(task, model, pred, extra.args) {
    tp$fun(pred=pred) / sum(pred$data$response == pred$task.desc$positive)
  }
)

#' @export npv
#' @rdname measures
#' @usage none
#' @format none
npv = makeMeasure(id="npv", minimize=FALSE, classif=TRUE, only.binary=TRUE, allowed.pred.types=c("response", "prob"),
  fun=function(task, model, pred, extra.args) {
    tn$fun(pred=pred) / sum(pred$data$response == pred$task.desc$negative)
  }
)

#' @export fdr
#' @rdname measures
#' @usage none
#' @format none
fdr = makeMeasure(id="fdr", minimize=TRUE, classif=TRUE, only.binary=TRUE, allowed.pred.types=c("response", "prob"),
  fun=function(task, model, pred, extra.args) {
    fp$fun(pred=pred) / sum(pred$data$response == pred$task.desc$positive)
  }
)

#' @export mcc
#' @rdname measures
#' @usage none
#' @format none
mcc = makeMeasure(id="mcc", minimize=FALSE, classif=TRUE, only.binary=TRUE, allowed.pred.types=c("response", "prob"),
  fun=function(task, model, pred, extra.args) {
    (tp$fun(pred=pred) *
    tn$fun(pred=pred) -
    fp$fun(pred=pred) *
    fn$fun(pred=pred)) /
    sqrt(prod(table(pred$data$truth, pred$data$response)))
  }
)

#' @export f1
#' @rdname measures
#' @usage none
#' @format none
f1 = makeMeasure(id="f1", minimize=FALSE, classif=TRUE, only.binary=TRUE, allowed.pred.types=c("response", "prob"),
  fun=function(task, model, pred, extra.args) {
    2 * tp$fun(pred=pred) /
      (sum(pred$data$truth == pred$task.desc$positive) + sum(pred$data$response == pred$task.desc$positive))
  }
)

#' @export gmean
#' @rdname measures
#' @usage none
#' @format none
gmean = makeMeasure(id="gmean", minimize=FALSE, classif=TRUE, only.binary=TRUE, allowed.pred.types=c("response", "prob"),
  fun=function(task, model, pred, extra.args) {
    sqrt(tpr$fun(pred=pred) * tnr$fun(pred=pred))
  }
)

#' @export gpr
#' @rdname measures
#' @usage none
#' @format none
gpr = makeMeasure(id="gpr", minimize=FALSE, classif=TRUE, only.binary=TRUE, allowed.pred.types=c("response", "prob"),
  fun=function(task, model, pred, extra.args) {
    sqrt(ppv$fun(pred=pred) * tpr$fun(pred=pred))
  }
)


# FIXME: this usage none, format none stuff is crap, but i currently do no know a better
# way to do this with roxygen2. We are neither documenting functions nor datsets here, but R objects.
