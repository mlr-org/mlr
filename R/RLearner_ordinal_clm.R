#' #' @export
#' makeRLearner.ordinal.clm = function() {
#'   makeRLearnerOrdinal(
#'     cl = "ordinal.clm",
#'     package = "ordinal",
#'     par.set = makeParamSet(
#'       makeDiscreteLearnerParam(id = "link", default = "logit",
#'          values = c("logit", "probit", "cloglog", "loglog", "cauchit")),
#'       makeDiscreteLearnerParam(id = "threshold", default = "flexible",
#'          values = c("flexible", "symmetric", "symmetric2", "equidistant"))
#'     ),
#'     properties = c("numerics", "factors", "weights", "ordered"),
#'     name = "Cumulative Link Models",
#'     short.name = "clm",
#'     callees = "clm"
#'   )
#' }
#'
#' #' @export
#' trainLearner.ordinal.glm = function(.learner, .task, .subset, .weights = NULL, ...) {
#'   d = getTaskData(.task, .subset, target.extra = TRUE)
#'
#' }
#'
#' #' @export
#' predictLearner.ordinal.glm = function(.learner, .model, .newdata, ...) {
#'
#' }
