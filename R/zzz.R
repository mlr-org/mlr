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
"_PACKAGE"

.onLoad = function(libname, pkgname) {
  configureMlr()
  backports::import(pkgname)
}

.onAttach = function(libname, pkgname) {
  configureMlr()
  parallelRegisterLevels(package = "mlr", levels = c("benchmark", "resample", "selectFeatures", "tuneParams", "ensemble"))

  packageStartupMessage(paste0(strwrap("'mlr' is in maintenance mode since
      July 2019. Future development efforts will go into its successor 'mlr3'
      (<https://mlr3.mlr-org.com>)."), collapse = "\n"))
}

mlr = new.env(parent = emptyenv())

### Learner properties
mlr$learner.properties = list(
  classif = c("numerics", "factors", "ordered", "missings", "weights", "prob", "oneclass", "twoclass", "multiclass", "class.weights", "featimp", "oobpreds", "functionals", "single.functional"),
  multilabel = c("numerics", "factors", "ordered", "missings", "weights", "prob", "oneclass", "twoclass", "multiclass", "functionals", "single.functional"),
  regr = c("numerics", "factors", "ordered", "missings", "weights", "se", "featimp", "oobpreds",
    "functionals", "single.functional"),
  cluster = c("numerics", "factors", "ordered", "missings", "weights", "prob", "functionals",
    "single.functional"),
  surv = c("numerics", "factors", "ordered", "missings", "weights", "prob", "lcens", "rcens", "icens", "featimp", "oobpreds", "functionals", "single.functional"),
  costsens = c("numerics", "factors", "ordered", "missings", "weights", "prob", "twoclass", "multiclass", "functionals", "single.functional")
)
mlr$learner.properties$any = unique(unlist(mlr$learner.properties))

### Measure properties
mlr$measure.properties = c("classif", "classif.multi", "multilabel", "regr", "surv", "cluster", "costsens", "req.pred", "req.truth", "req.task", "req.feats", "req.model", "req.prob")
