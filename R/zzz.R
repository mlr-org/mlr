#' @import methods
#' @importFrom survival Surv is.Surv
#' @importFrom graphics hist
#' @importFrom utils browseURL capture.output combn data getFromNamespace getS3method head tail methods
#' @import BBmisc
#' @import checkmate
#' @import parallelMap
#' @import ParamHelpers
#' @import ggplot2
#' @import stats
#' @import stringi
#' @import data.table

.onLoad = function(libname, pkgname) {
  backports::import(pkgname)
}

.onAttach = function(libname, pkgname) {
  configureMlr()
  parallelRegisterLevels(package = "mlr", levels = c("benchmark", "resample", "selectFeatures", "tuneParams", "ensemble"))
}

mlr = new.env(parent = emptyenv())

### Learner properties
mlr$learner.properties = list(
  classif    = c("numerics", "factors", "ordered", "missings", "weights", "prob", "oneclass", "twoclass", "multiclass", "class.weights", "featimp", "oobpreds", "functionals"),
  multilabel = c("numerics", "factors", "ordered", "missings", "weights", "prob", "oneclass", "twoclass", "multiclass", "functionals"),
  regr       = c("numerics", "factors", "ordered", "missings", "weights", "se", "featimp", "oobpreds", "functionals"),
  cluster    = c("numerics", "factors", "ordered", "missings", "weights", "prob", "functionals"),
  surv       = c("numerics", "factors", "ordered", "missings", "weights", "prob", "lcens", "rcens", "icens", "featimp", "oobpreds", "functionals"),
  costsens   = c("numerics", "factors", "ordered", "missings", "weights", "prob", "twoclass", "multiclass", "functionals")
)
mlr$learner.properties$any = unique(unlist(mlr$learner.properties))

### Measure properties
mlr$measure.properties = c("classif", "classif.multi", "multilabel", "regr", "surv", "cluster", "costsens", "req.pred", "req.truth", "req.task", "req.feats", "req.model", "req.prob")
