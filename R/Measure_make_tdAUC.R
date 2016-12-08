#' @title Creates a measure for time dependent AUC.
#'
#' @description
#' Creates one of several possible measures for time dependent AUC in the survival setting.
#'
#' @param id [\code{character(1)}]\cr
#'   Name of measure.
#'   Default is \dQuote{Time dependent AUC for survival}.
#' @param method [\code{character(1)}]\cr
#'   Method that should be used to calculate the time dependent AUC for survival predictions.
#'   Should be one of the following:
#'   \code{"kw"} for using kernel weighting method,
#'   \code{"km"} for using Kaplan Meier method,
#'   \code{"nne"} for using nearest neigbour method,
#'   \code{"ipcw"} for using inverse probability of censoring weighting.
#'   Default is \code{"kw"}.
#' @param time [\code{numeric(1)}]\cr
#'   For what time should be the time dependent AUC be calculated?
#'   If the value exceeds the maximum value of the true values, it is reset to this maximum value.
#'   Default is \code{NA}.
#' @inheritParams makeMeasure
#' @export
#' @family performance
makeTimeDependentAUCMeasure = function(id = "td.AUC", method = "km", time = NA, name = "Time dependent AUC for survival", note = "") {
  assertString(id)
  assertSubset(method, choices = c("kw", "km", "nne", "ipcw"))
  assertNumber(time, na.ok = TRUE)
  assertString(name)
  assertString(note)

  makeMeasure(id = paste0(id, ".", method), extra.args = list(method, time), properties = c("surv", "req.pred", "req.truth"),
    minimize = FALSE, best = 1, worst = 0, fun = function(task, model, pred, feats, extra.args = list(method, time)) {
      measureTDAUC = function(truth, response, method, time) {
        # Set the time value:
        # If maximum value is given by user trough time, use minimum of the time and the maximum of truth
        # Otherwise just use the maximum value of truth
        max.truth = max(truth[, 1L])
        if (method == "ipcw")
          max.truth = max.truth - 1e-10
        time = if (is.na(time)) max.truth else min(time, max.truth)

        # Depending on the chosen method, calculate the time dependent AUC
        if (method == "kw") {
          requirePackages("_tdROC")
          return(tdROC::tdROC(X = response, Y = truth[, 1L], delta = truth[, 2L], tau = time)$AUC$value)
        }
        if (method == "km") {
          requirePackages("_survivalROC")
          return(survivalROC::survivalROC(Stime = truth[, 1L], status = truth[, 2L], marker = response,
              predict.time = time, method = "KM")$AUC)
        }
        if (method == "nne") {
          requirePackages("_survivalROC")
          return(survivalROC::survivalROC.C(Stime = truth[, 1L], status = truth[, 2L], marker = response,
              predict.time = time, span = 0.1)$AUC)
        }
        if (method == "ipcw") {
          requirePackages(c("_timeROC", "!survival"))
          return(timeROC::timeROC(T = truth[,1L], delta = truth[,2L], marker = response, times = time,
              cause = 1L)$AUC[[2L]])
        }
      }
      measureTDAUC(getPredictionTruth(pred), getPredictionResponse(pred), method = extra.args[[1L]], time = extra.args[[2L]])
    },
    name = paste(name, "using", method, "method"),
    note = note
  )
}
