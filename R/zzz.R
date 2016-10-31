#' @import methods
#' @importFrom survival Surv is.Surv
#' @importFrom graphics hist
#' @importFrom utils browseURL capture.output combn data getFromNamespace getS3method head tail methods
#' @import BBmisc
#' @import backports
#' @import checkmate
#' @import parallelMap
#' @import ParamHelpers
#' @import ggplot2
#' @import stats
#' @import stringi
#' @import data.table
#' @importFrom xts xts lag.xts diff.xts
#' @importFrom zoo index
#' @importFrom ggvis ggvis prop layer_ribbons layer_paths layer_points layer_lines bind_shiny ggvisOutput
#' @importFrom shiny selectInput shinyUI pageWithSidebar headerPanel sidebarPanel mainPanel uiOutput shinyServer reactive shinyApp



.onAttach = function(libname, pkgname) {
  configureMlr()
  parallelRegisterLevels(package = "mlr", levels = c("benchmark", "resample", "selectFeatures", "tuneParams"))
}

mlr = new.env(parent = emptyenv())

### Learner properties
mlr$learner.properties = list(
  classif    = c("numerics", "factors", "ordered", "missings", "weights", "prob", "oneclass", "twoclass", "multiclass", "class.weights", "featimp"),
  multilabel = c("numerics", "factors", "ordered", "missings", "weights", "prob", "oneclass", "twoclass", "multiclass"),
  regr       = c("numerics", "factors", "ordered", "missings", "weights", "se", "featimp"),
  cluster    = c("numerics", "factors", "ordered", "missings", "weights", "prob"),
  surv       = c("numerics", "factors", "ordered", "missings", "weights", "prob", "lcens", "rcens", "icens", "featimp"),
  costsens   = c("numerics", "factors", "ordered", "missings", "weights", "prob", "twoclass", "multiclass"),
  fcregr       = c("numerics", "quantile", "weights", "featimp"),
  mfcregr       = c("numerics", "quantile")
)
mlr$learner.properties$any = unique(unlist(mlr$learner.properties))
