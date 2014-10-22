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
#' @seealso \code{\link{listMeasures}}
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
  allowed.pred.types = c("response", "prob", "se"),
  note = "Percentage of original features used for model, useful for feature selection.",
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
  allowed.pred.types = c("response", "prob", "se"),
  note = "Time of fitting the model",
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
  allowed.pred.types = c("response", "prob", "se"),
  note = "Time of predicting test set",
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
  allowed.pred.types = c("response", "prob", "se"),
  note = "timetrain + trainpredict",
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
  note = "Sum of squared errors",
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
  note = "Mean of squared errors",
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
  note = "Root mean square error",
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
  note = "Median of squared errors",
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
  note = "Sum of absolute errors",
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
  note = "Mean of absolute errors",
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
  note = "Median of absolute errors",
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
  note = "Mean misclassification error",
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
  note = "Accuracy",
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
  note = "Balanced error rate. Mean of misclassification error rates on all individual classes.",
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
  note = "Area under the curve for multiclass problems. Calls `pROC::multiclass.roc`.",
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
  note = "Area under the curve",
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
  note = "Balanced accuracy. Mean of true positive rate and true negative rate", 
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
  note = "True positives", 
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
  note = "rue negatives, also called correct rejections.", 
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
  note = "False positives, also called false alarms.", 
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
  note = "False negatives, also called misses.", 
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
  note = "True positive rate, also called hit rate or recall.", 
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
 	note = "True negative rate. Also called specificity.",
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
 	note = "False positive rate, also called false alarm rate or fall-out.",
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
  note = "False negative rate", 
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
  note = "Positive predictive value, also called precision.", 
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
  note = "Negative predictive value", 
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
  note = "False discovery rate",
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
  note = "Matthews correlation coefficient", 
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
  note = "F1 measure", 
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
  note = "G-mean, geometric mean of recall and specificity.", 
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
  note = "Geometric mean of precision and recall", 
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
  note = "Concordance index", 
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
  note = "Mean costs of the predicted choices.", 
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
  note = "Misclassification penalty, i.e. average difference between costs of oracle and model prediction.", 
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
  note = "Davies-Bouldin cluster separation measure, see `?clusterSim::index.DB`", 
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
  note = "Dunn index, see `?clValid::dunn`", 
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
  note = "Calinski-Harabasz pseudo F statistic, see `?clusterSim::index.G1`", 
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
  note = "Baker and Hubert adaptation of Goodman-Kruskal's gamma statistic, see `?clusterSim::index.G2`", 
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
  note = "Rousseeuw's silhouette internal cluster quality index, see `?clusterSim::index.S`", 
  fun = function(task, model, pred, feats, extra.args) {
    requirePackages("clusterSim")
    index.S(dist.GDM(feats), pred$data$response)
  }
)

