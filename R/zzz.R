#' @import methods
#' @importFrom survival Surv is.Surv
#' @importFrom graphics hist
#' @importFrom utils browseURL capture.output combn data getFromNamespace getS3method head methods tail .S3methods
#' @import BBmisc
#' @import checkmate
#' @import parallelMap
#' @import ParamHelpers
#' @import ggplot2
#' @import stats
#' @import stringi
#' @import data.table
#' @importFrom ggvis ggvis prop layer_ribbons layer_paths layer_points layer_lines bind_shiny ggvisOutput
#' @importFrom shiny selectInput shinyUI pageWithSidebar headerPanel sidebarPanel mainPanel uiOutput shinyServer reactive shinyApp

.onAttach = function(libname, pkgname) {
  configureMlr()
  parallelRegisterLevels(package = "mlr", levels = c("benchmark", "resample", "selectFeatures", "tuneParams", "stackedLearner"))
}
