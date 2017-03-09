# TODO: This is numerical instable.
#' @export td.auc.kw
#' @rdname measures
#' @format none
td.auc.kw = makeMeasure(
  id = "td.auc.kw",
  name = "Time-dependent AUC estimated using kernel weighting method",
  note = "To set an upper time limit, set argument max.time (defaults to max time in complete task).",
  properties = c("surv", "req.pred", "req.truth"),
  minimize = FALSE, best = 1, worst = 0,
  fun = function(task, model, pred, feats, extra.args) {
    measureTDAUCKW = function(truth, response, max.time) {
      max.time = assertNumber(max.time, null.ok = TRUE) %??% max(getTaskTargets(task)[, 1L]) - sqrt(.Machine$double.eps)
      # biggest time value has to be adapted as it does not provide results otherwise
      tdROC::tdROC(X = response, Y = truth[, 1L], delta = truth[, 2L], tau = max.time)$AUC$value
    }
    requirePackages("_tdROC")
    measureTDAUCKW(getPredictionTruth(pred), getPredictionResponse(pred), extra.args$max.time)
  },
  extra.args = list(max.time = NULL)
)

#' @export td.auc.km
#' @rdname measures
#' @format none
td.auc.km = makeMeasure(
  id = "td.auc.km",
  name = "Time-dependent AUC estimated using Kaplan Meier method",
  note = "To set an upper time limit, set argument max.time (defaults to max time in complete task).",
  properties = c("surv", "req.pred", "req.truth"),
  minimize = FALSE, best = 1, worst = 0,
  fun = function(task, model, pred, feats, extra.args) {
    measureTDAUCKM = function(truth, response, max.time) {
      max.time = assertNumber(max.time, null.ok = TRUE) %??% max(getTaskTargets(task)[, 1L]) - sqrt(.Machine$double.eps)
      # biggest time value has to be adapted as it does not provide results otherwise
      survivalROC::survivalROC(Stime = truth[, 1L], status = truth[, 2L], marker = response,
        predict.time = max.time, method = "KM")$AUC
    }
    requirePackages("_survivalROC")
    measureTDAUCKM(getPredictionTruth(pred), getPredictionResponse(pred), max.time = extra.args$max.time)
  },
  extra.args = list(max.time = NULL)
)

#' @export td.auc.nne
#' @rdname measures
#' @format none
td.auc.nne = makeMeasure(
  id = "td.auc.nne",
  name = "Time-dependent AUC estimated using nearest neighbor method",
  note = "To set an upper time limit, set argument max.time (defaults to max time in complete task).",
  properties = c("surv", "req.pred", "req.truth"),
  minimize = FALSE, best = 1, worst = 0,
  fun = function(task, model, pred, feats, extra.args) {
    measureTDAUCNNE = function(truth, response, max.time) {
      max.time = assertNumber(max.time, null.ok = TRUE) %??% max(getTaskTargets(task)[, 1L]) - sqrt(.Machine$double.eps)
      # biggest time value has to be adapted as it does not provide results otherwise
      survivalROC::survivalROC.C(Stime = truth[, 1L], status = truth[, 2L], marker = response,
        predict.time = max.time, span = 0.1)$AUC
    }
    requirePackages("_survivalROC")
    measureTDAUCNNE(getPredictionTruth(pred), getPredictionResponse(pred), max.time = extra.args$max.time)
  },
  extra.args = list(max.time = NULL)
)
