#' @title Performance measures.
#' @name measures
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
#'   \item{\bold{bac}}{\cr Balanced accuracy. Mean of true positive rate and true negative rate}
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
#' Survival:
#' \itemize{
#'   \item{\bold{cindex}}{\cr Concordance index}
#' }
#'
#' Cost-sensitive:
#' \itemize{
#'   \item{\bold{meancosts}}{\cr Mean costs of the predicted choices.}
#'   \item{\bold{mcp}}{\cr Misclassification penalty, i.e. average difference between
#'     costs of oracle and model prediction.}
#' }
#'
#' Clustering:
#' \itemize{
#'   \item{\bold{db}}{\cr Davies-Bouldin cluster separation measure, see \code{\link[clusterSim]{index.DB}}}
#'   \item{\bold{dunn}}{\cr Dunn index, see \code{\link[clValid]{dunn}}}
#'   \item{\bold{G1}}{\cr Calinski-Harabasz pseudo F statistic, see \code{\link[clusterSim]{index.G1}}}
#'   \item{\bold{G2}}{\cr Baker and Hubert adaptation of Goodman-Kruskal's gamma statistic, see \code{\link[clusterSim]{index.G2}}}
#'   \item{\bold{silhouette}}{\cr Rousseeuw's silhouette internal cluster quality index, see \code{\link[clusterSim]{index.S}}}
#' }
#'
#' General:
#' \itemize{
#'   \item{\bold{timetrain}}{\cr Time of fitting the model}
#'   \item{\bold{timepredict}}{\cr Time of predicting test set}
#'   \item{\bold{timeboth}}{\cr timetrain + trainpredict}
#'   \item{\bold{featperc}}{\cr Percentage of original features used for model, useful for feature selection.}
#' }
#' @usage none
#' @format none
#' @family performance
NULL

###############################################################################
### general ###
###############################################################################
#' @export featperc
#' @rdname measures
#' @usage none
#' @format none
featperc = makeMeasure(id = "featperc", minimize = TRUE, best = 0, worst = 1,
  properties = c("classif", "classif.multi", "regr", "surv", "costsens", "cluster"),
  allowed.pred.types = c("response", "prob"),
  fun = function(task, model, pred, feats, extra.args) {
    length(model$features) / sum(pred$task.desc$n.feat)
  }
)

#' @export timetrain
#' @rdname measures
#' @usage none
#' @format none
timetrain = makeMeasure(id = "timetrain", minimize = TRUE, best = 0, worst = Inf,
  properties = c("classif", "classif.multi", "regr", "surv", "costsens", "cluster"),
  allowed.pred.types = c("response", "prob"),
  fun = function(task, model, pred, feats, extra.args) {
    model$time
  }
)

#' @export timepredict
#' @rdname measures
#' @usage none
#' @format none
timepredict = makeMeasure(id = "timepredict", minimize = TRUE, best = 0, worst = Inf,
  properties = c("classif", "classif.multi", "regr", "surv", "costsens", "cluster"),
  allowed.pred.types = c("response", "prob"),
  fun = function(task, model, pred, feats, extra.args) {
    pred$time
  }
)

#' @export timeboth
#' @rdname measures
#' @usage none
#' @format none
timeboth = makeMeasure(id = "timeboth", minimize = TRUE, best = 0, worst = Inf,
  properties = c("classif", "classif.multi", "regr", "surv", "costsens", "cluster"),
  allowed.pred.types = c("response", "prob"),
  fun = function(task, model, pred, feats, extra.args) {
    model$time + pred$time
  }
)

###############################################################################
### regression ###
###############################################################################
#' @export sse
#' @rdname measures
#' @usage none
#' @format none
sse = makeMeasure(id = "sse", minimize = TRUE, best = 0, worst = Inf,
  properties = "regr",
  allowed.pred.types = c("response", "se"),
  fun = function(task, model, pred, feats, extra.args) {
    sum((pred$data$response - pred$data$truth)^2)
  }
)

#' @export mse
#' @rdname measures
#' @usage none
#' @format none
mse = makeMeasure(id = "mse", minimize = TRUE, best = 0, worst = Inf,
  properties = "regr",
  allowed.pred.types = c("response", "se"),
  fun = function(task, model, pred, feats, extra.args) {
    mean((pred$data$response - pred$data$truth)^2)
  }
)

#' @export rmse
#' @format none
#' @rdname measures
#' @usage none
#' @format none
rmse = makeMeasure(id = "rmse", minimize = TRUE, best = 0, worst = Inf,
  properties = "regr",
  allowed.pred.types = c("response", "se"),
  fun = function(task, model, pred, feats, extra.args) {
    mean((pred$data$response - pred$data$truth)^2)
  },
  aggr = test.sqrt.of.mean
)

#' @export medse
#' @rdname measures
#' @usage none
#' @format none
medse = makeMeasure(id = "medse", minimize = TRUE, best = 0, worst = Inf,
  properties = "regr",
  allowed.pred.types = c("response", "se"),
  fun = function(task, model, pred, feats, extra.args) {
    median((pred$data$response - pred$data$truth)^2)
  }
)

#' @export sae
#' @rdname measures
#' @usage none
#' @format none
sae = makeMeasure(id = "sae", minimize = TRUE, best = 0, worst = Inf,
  properties = "regr",
  allowed.pred.types = c("response", "se"),
  fun = function(task, model, pred, feats, extra.args) {
    sum(abs(pred$data$response - pred$data$truth))
  }
)

#' @export mae
#' @rdname measures
#' @usage none
#' @format none
mae = makeMeasure(id = "mae", minimize = TRUE, best = 0, worst = Inf,
  properties = "regr",
  allowed.pred.types = c("response", "se"),
  fun = function(task, model, pred, feats, extra.args) {
    mean(abs(pred$data$response - pred$data$truth))
  }
)

#' @export medae
#' @rdname measures
#' @usage none
#' @format none
medae = makeMeasure(id = "medae", minimize = TRUE, best = 0, worst = Inf,
  properties = "regr",
  allowed.pred.types = c("response", "se"),
  fun = function(task, model, pred, feats, extra.args) {
    median(abs(pred$data$response - pred$data$truth))
  }
)

###############################################################################
### classif multi ###
###############################################################################
#' @export mmce
#' @rdname measures
#' @usage none
#' @format none
mmce = makeMeasure(id = "mmce", minimize = TRUE, best = 0, worst = 1,
  properties = c("classif", "classif.multi"),
  allowed.pred.types = c("response", "prob"),
  fun = function(task, model, pred, feats, extra.args) {
    mean(pred$data$response != pred$data$truth)
  }
)

#' @export acc
#' @rdname measures
#' @usage none
#' @format none
acc = makeMeasure(id = "acc", minimize = FALSE, best = 1, worst = 0,
  properties = c("classif", "classif.multi"),
  allowed.pred.types = c("response", "prob"),
  fun = function(task, model, pred, feats, extra.args) {
    mean(pred$data$response == pred$data$truth)
  }
)

#' @export ber
#' @rdname measures
#' @usage none
#' @format none
ber = makeMeasure(id = "ber", minimize = TRUE, best = 0, worst = 1,
  properties = c("classif", "classif.multi"),
  allowed.pred.types = c("response", "prob"),
  fun = function(task, model, pred, feats, extra.args) {
    n = length(pred$task.desc$class.levels) + 1L
    mean(getConfMatrix(pred, relative = TRUE)[-n, n])
  }
)

#' @export multiclass.auc
#' @rdname measures
#' @usage none
#' @format none
multiclass.auc = makeMeasure(id = "multiclass.auc", minimize = FALSE, best = 1, worst = 0,
  properties = c("classif", "classif.multi"),
  allowed.pred.types = c("response", "prob"),
  fun = function(task, model, pred, feats, extra.args) {
    # pROC does allow NAs
    requirePackages("pROC", "multiclass.auc")
    resp = pred$data$response
    predP = getProbabilities(pred)
    # choose the probablity of the choosen response
    predV = vnapply(seq_row(predP), function(i) {
      predP[i, resp[i]]
    })
    auc = multiclass.roc(response = resp, predictor = predV)$auc
    as.numeric(auc)
  }
)

###############################################################################
### classif binary ###
###############################################################################
#' @export auc
#' @rdname measures
#' @usage none
#' @format none
auc = makeMeasure(id = "auc", minimize = FALSE, best = 1, worst = 0,
  properties = "classif",
  allowed.pred.types = "prob",
  fun = function(task, model, pred, feats, extra.args) {
    # ROCR does not work with NAs
    if (anyMissing(pred$data$response) || length(unique(pred$data$truth)) == 1L)
      return(NA_real_)
    rpreds = asROCRPrediction(pred)
    ROCR::performance(rpreds, "auc")@y.values[[1L]]
  }
)

#' @export bac
#' @rdname measures
#' @usage none
#' @format none
bac = makeMeasure(id = "bac", minimize = FALSE, best = 1, worst = 0,
  properties = c("classif"),
  allowed.pred.types = c("response", "prob"),
  fun = function(task, model, pred, feats, extra.args) {
    mean(c(tp$fun(pred = pred) / sum(pred$data$truth == pred$task.desc$positive),
           tn$fun(pred = pred) / sum(pred$data$truth == pred$task.desc$negative)))
  }
)

#' @export tp
#' @rdname measures
#' @usage none
#' @format none
tp = makeMeasure(id = "tp", minimize = FALSE, best = Inf, worst = 0,
  properties = "classif",
  allowed.pred.types = c("response", "prob"),
  fun = function(task, model, pred, feats, extra.args) {
    sum(pred$data$truth == pred$data$response & pred$data$response == pred$task.desc$positive)
  }
)

#' @export tn
#' @rdname measures
#' @usage none
#' @format none
tn = makeMeasure(id = "tn", minimize = FALSE, best = Inf, worst = 0,
  properties = "classif",
  allowed.pred.types = c("response", "prob"),
  fun = function(task, model, pred, feats, extra.args) {
    sum(pred$data$truth == pred$data$response & pred$data$response == pred$task.desc$negative)
  }
)

#' @export fp
#' @rdname measures
#' @usage none
#' @format none
fp = makeMeasure(id = "fp", minimize = TRUE, best = 0, worst = Inf,
  properties = "classif",
  allowed.pred.types = c("response", "prob"),
  fun = function(task, model, pred, feats, extra.args) {
    sum(pred$data$truth != pred$data$response & pred$data$response == pred$task.desc$positive)
  }
)

#' @export fn
#' @rdname measures
#' @usage none
#' @format none
fn = makeMeasure(id = "fn", minimize = TRUE, best = 0, worst = Inf,
  properties = "classif",
  allowed.pred.types = c("response", "prob"),
  fun = function(task, model, pred, feats, extra.args) {
    sum(pred$data$truth != pred$data$response & pred$data$response == pred$task.desc$negative)
  }
)

#' @export tpr
#' @rdname measures
#' @usage none
#' @format none
tpr = makeMeasure(id = "tpr", minimize = FALSE, best = 1, worst = 0,
  properties = "classif",
  allowed.pred.types = c("response", "prob"),
  fun = function(task, model, pred, feats, extra.args) {
    tp$fun(pred = pred) / sum(pred$data$truth == pred$task.desc$positive)
  }
)

#' @export tnr
#' @rdname measures
#' @usage none
#' @format none
tnr = makeMeasure(id = "tnr", minimize = FALSE, best = 1, worst = 0,
  properties = "classif",
  allowed.pred.types = c("response", "prob"),
  fun = function(task, model, pred, feats, extra.args) {
    tn$fun(pred = pred) / sum(pred$data$truth == pred$task.desc$negative)
  }
)

#' @export fpr
#' @rdname measures
#' @usage none
#' @format none
fpr = makeMeasure(id = "fpr", minimize = TRUE, best = 0, worst = 1,
  properties = "classif",
  allowed.pred.types = c("response", "prob"),
  fun = function(task, model, pred, feats, extra.args) {
    fp$fun(pred = pred) / sum(pred$data$truth == pred$task.desc$negative)
  }
)

#' @export fnr
#' @rdname measures
#' @usage none
#' @format none
fnr = makeMeasure(id = "fnr", minimize = TRUE, best = 0, worst = 1,
  properties = "classif",
  allowed.pred.types = c("response", "prob"),
  fun = function(task, model, pred, feats, extra.args) {
    fn$fun(pred = pred) / sum(pred$data$truth == pred$task.desc$positive)
  }
)

#' @export ppv
#' @rdname measures
#' @usage none
#' @format none
ppv = makeMeasure(id = "ppv", minimize = FALSE, best = 1, worst = 0,
  properties = "classif",
  allowed.pred.types = c("response", "prob"),
  fun = function(task, model, pred, feats, extra.args) {
    tp$fun(pred = pred) / sum(pred$data$response == pred$task.desc$positive)
  }
)

#' @export npv
#' @rdname measures
#' @usage none
#' @format none
npv = makeMeasure(id = "npv", minimize = FALSE, best = 1, worst = 0,
  properties = "classif",
  allowed.pred.types = c("response", "prob"),
  fun = function(task, model, pred, feats, extra.args) {
    tn$fun(pred = pred) / sum(pred$data$response == pred$task.desc$negative)
  }
)

#' @export fdr
#' @rdname measures
#' @usage none
#' @format none
fdr = makeMeasure(id = "fdr", minimize = TRUE, best = 0, worst = 1,
  properties = "classif",
  allowed.pred.types = c("response", "prob"),
  fun = function(task, model, pred, feats, extra.args) {
    fp$fun(pred = pred) / sum(pred$data$response == pred$task.desc$positive)
  }
)

#' @export mcc
#' @rdname measures
#' @usage none
#' @format none
mcc = makeMeasure(id = "mcc", minimize = FALSE,
  properties = "classif",
  allowed.pred.types = c("response", "prob"), best = 1, worst = -1,
  fun = function(task, model, pred, feats, extra.args) {
    (tp$fun(pred = pred) *
    tn$fun(pred = pred) -
    fp$fun(pred = pred) *
    fn$fun(pred = pred)) /
    sqrt(prod(table(pred$data$truth, pred$data$response)))
  }
)

#' @export f1
#' @rdname measures
#' @usage none
#' @format none
f1 = makeMeasure(id = "f1", minimize = FALSE, best = 1, worst = 0,
  properties = "classif",
  allowed.pred.types = c("response", "prob"),
  fun = function(task, model, pred, feats, extra.args) {
    2 * tp$fun(pred = pred) /
      (sum(pred$data$truth == pred$task.desc$positive) + sum(pred$data$response == pred$task.desc$positive))
  }
)

#' @export gmean
#' @rdname measures
#' @usage none
#' @format none
gmean = makeMeasure(id = "gmean", minimize = FALSE, best = 1, worst = 0,
  properties = "classif",
  allowed.pred.types = c("response", "prob"),
  fun = function(task, model, pred, feats, extra.args) {
    sqrt(tpr$fun(pred = pred) * tnr$fun(pred = pred))
  }
)

#' @export gpr
#' @rdname measures
#' @usage none
#' @format none
gpr = makeMeasure(id = "gpr", minimize = FALSE, best = 1, worst = 0,
  properties = "classif",
  allowed.pred.types = c("response", "prob"),
  fun = function(task, model, pred, feats, extra.args) {
    sqrt(ppv$fun(pred = pred) * tpr$fun(pred = pred))
  }
)

###############################################################################
### survival ###
###############################################################################
#' @export cindex
#' @rdname measures
#' @usage none
#' @format none
cindex = makeMeasure(id = "cindex", minimize = FALSE, best = 1, worst = 0,
  properties = "surv",
  allowed.pred.types = c("response", "prob"),
  fun = function(task, model, pred, feats, extra.args) {
    requirePackages("Hmisc")
    # FIXME: we need to ensure the censoring here
    s = Surv(pred$data$truth.time, pred$data$truth.event)
    rcorr.cens(-1 * pred$data$response, s)[["C Index"]]
  }
)

###############################################################################
### cost-sensitive ###
###############################################################################
#' @export meancosts
#' @rdname measures
meancosts = makeMeasure(id = "meancosts", minimize = TRUE, best = 0, worst = Inf,
  properties = "costsens",
  allowed.pred.types = "response",
  fun = function(task, model, pred, feats, extra.args) {
    classes = as.character(pred$data$response)
    ids = pred$data$id
    costs = task$env$costs
    y = mapply(function(id, cl) {
      costs[id, cl]
    }, ids, classes, SIMPLIFY = TRUE, USE.NAMES = FALSE)
    mean(y)
  }
)

#' @export mcp
#' @rdname measures
mcp = makeMeasure(id = "mcp", minimize = TRUE, best = 0, worst = Inf,
  properties = "costsens",
  allowed.pred.types = "response",
  fun = function(task, model, pred, feats, extra.args) {
    mc = meancosts$fun(task, NULL, pred, NULL, extra.args)
    oc = mean(apply(task$env$costs, 1L, min))
    mc - oc
  }
)

###############################################################################
### clustering ###
###############################################################################
#' @export db
#' @rdname measures
#' @usage none
#' @format none
db = makeMeasure(id = "db", minimize = TRUE, best = 0, worst = Inf,
  properties = "cluster",
  allowed.pred.types = c("response", "prob"),
  fun = function(task, model, pred, feats, extra.args) {
    requirePackages("clusterSim")
    index.DB(feats, pred$data$response)$DB
  }
)

#' @export dunn
#' @rdname measures
#' @usage none
#' @format none
dunn = makeMeasure(id = "dunn", minimize = FALSE, best = Inf, worst = 0,
  properties = "cluster",
  allowed.pred.types = c("response", "prob"),
  fun = function(task, model, pred, feats, extra.args) {
    requirePackages("clValid")
    dunn(Data = feats, clusters = pred$data$response)
  }
)

#' @export G1
#' @rdname measures
#' @usage none
#' @format none
G1 = makeMeasure(id = "G1", minimize = FALSE, best = Inf, worst = 0,
  properties = "cluster",
  allowed.pred.types = c("response", "prob"),
  fun = function(task, model, pred, feats, extra.args) {
    requirePackages("clusterSim")
    index.G1(feats, pred$data$response)
  }
)

#' @export G2
#' @rdname measures
#' @usage none
#' @format none
G2 = makeMeasure(id = "G2", minimize = FALSE, best = Inf, worst = 0,
  properties = "cluster",
  allowed.pred.types = c("response", "prob"),
  fun = function(task, model, pred, feats, extra.args) {
    requirePackages("clusterSim")
    index.G2(dist.GDM(feats), pred$data$response)
  }
)

#' @export silhouette
#' @rdname measures
#' @usage none
#' @format none
silhouette = makeMeasure(id = "silhouette", minimize = FALSE, best = Inf, worst = 0,
  properties = "cluster",
  allowed.pred.types = c("response", "prob"),
  fun = function(task, model, pred, feats, extra.args) {
    requirePackages("clusterSim")
    index.S(dist.GDM(feats), pred$data$response)
  }
)

