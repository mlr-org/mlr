#' @title Calculate receiver operator measures.
#'
#' @description
#' Calculate the relative number of correct/incorrect classifications and the following evaluation measures:
#'
#'
#' \itemize{
#'  \item \code{tpr} True positive rate (Sensitivity, Recall)
#'  \item \code{fpr} False positive rate (Fall-out)
#'  \item \code{fnr} False negative rate (Miss rate)
#'  \item \code{tnr} True negative rate (Specificity)
#'  \item \code{ppv} Positive predictive value (Precision)
#'  \item \code{for} False omission rate
#'  \item \code{lrp} Positive likelihood ratio (LR+)
#'  \item \code{fdr} False discovery rate
#'  \item \code{npv} Negative predictive value
#'  \item \code{acc} Accuracy
#'  \item \code{lrm} Negative likelihood ratio (LR-)
#'  \item \code{dor} Diagnostic odds ratio
#' }
#'
#' For details on the used measures see \code{\link{measures}} and also
#' \url{https://en.wikipedia.org/wiki/Receiver_operating_characteristic}.
#'
#' The element for the false omission rate in the resulting object is not called \code{for} but
#' \code{fomr} since \code{for} should never be used as a variable name in an object.
#'
#' @template arg_pred
#'
#' @return [\code{ROCMeasures}].
#'    A list containing two elements \code{confusion.matrix} which is
#'    the 2 times 2 confusion matrix of relative frequencies and \code{measures}, a list of the above mentioned measures.
#' @export
#' @family roc
#' @family performance
#' @examples
#' lrn = makeLearner("classif.rpart", predict.type = "prob")
#' fit = train(lrn, sonar.task)
#' pred = predict(fit, task = sonar.task)
#' calculateROCMeasures(pred)
#'
calculateROCMeasures = function(pred) {
  checkPrediction(pred, task.type = c("oneclass", "classif", check.truth = TRUE, no.na = TRUE, binary = TRUE)

  tab = calculateConfusionMatrix(pred, relative = TRUE)$relative.row[1:2, 1:2]
  response = getPredictionResponse(pred)
  truth = getPredictionTruth(pred)
  positive = pred$task.desc$positive
  negative = pred$task.desc$negative

  #calculate measures
  r.tpr = measureTPR(truth, response, positive)
  r.fnr = measureFNR(truth, response, negative, positive)
  r.fpr = measureFPR(truth, response, negative, positive)
  r.tnr = measureTNR(truth, response, negative)
  r.ppv = measurePPV(truth, response, positive)
  r.fdr = 1 - r.ppv
  r.npv = measureNPV(truth, response, negative)
  r.for = 1 - r.npv
  r.acc = measureACC(truth, response)
  r.lr.plus = r.tpr / r.fpr
  r.lr.minus = r.fnr / r.tnr
  r.dor = r.lr.plus / r.lr.minus

  makeS3Obj("ROCMeasures",
    confusion.matrix = tab,
    measures = list(tpr = r.tpr,
      fnr = r.fnr,
      fpr = r.fpr,
      tnr = r.tnr,
      ppv = r.ppv,
      fdr = r.fdr,
      npv = r.npv,
      fomr = r.for,
      acc = r.acc,
      lrp = r.lr.plus,
      lrm = r.lr.minus,
      dor = r.dor))
}

#' @describeIn calculateROCMeasures
#'
#' @param x [\code{ROCMeasures}]\cr
#'   Created by \code{\link{calculateROCMeasures}}.
#' @param abbreviations [\code{logical(1)}]\cr
#'   If \code{TRUE} a short paragraph with explanations of the used measures is printed additionally.
#' @param digits [\code{integer(1)}]\cr
#'   Number of digits the measures are rounded to.
#' @param ... \code{[any]}\cr
#'  Currently not used.
#' @export
print.ROCMeasures = function(x, abbreviations = TRUE, digits = 2, ...) {

  checkFlag(abbreviations)
  checkInt(digits, lower = 1)

  #format measures
  x$measures = mapply(function(m, v) paste0(m, ": ", round(v, digits)), names(x$measures), x$measures)

  res = cbind(round(x$confusion.matrix, digits = digits),
    c(x$measures[["tpr"]], x$measures[["fpr"]]),
    c(x$measures[["fnr"]], x$measures[["tnr"]]))
  res = rbind(res,
    c(x$measures[["ppv"]], x$measures[["fomr"]], x$measures[["lrp"]], x$measures[["acc"]]),
    c(x$measures[["fdr"]], x$measures[["npv"]], x$measures[["lrm"]], x$measures[["dor"]]))

  #since we should not use "for" as a variable name in a list we replace it in the printer
  res[3, 2] = stri_replace_all_fixed(res[3, 2], "fomr", "for")

  names(dimnames(res)) = c("true", "predicted")
  print(noquote(res))
  if (abbreviations) {
    cat("\n\nAbbreviations:\n")
    cat("tpr - True positive rate (Sensitivity, Recall)\n")
    cat("fpr - False positive rate (Fall-out)\n")
    cat("fnr - False negative rate (Miss rate)\n")
    cat("tnr - True negative rate (Specificity)\n")
    cat("ppv - Positive predictive value (Precision)\n")
    cat("for - False omission rate\n")
    cat("lrp - Positive likelihood ratio (LR+)\n")
    cat("fdr - False discovery rate\n")
    cat("npv - Negative predictive value\n")
    cat("acc - Accuracy\n")
    cat("lrm - Negative likelihood ratio (LR-)\n")
    cat("dor - Diagnostic odds ratio\n")
  }
}

