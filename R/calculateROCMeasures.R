#' @title Calculate the reciever operator measures
#' 
#' @description 
#' Calculate the relative number of correct/incorrect classifications and the following evaluation measures:
#' 
#' 
#' \itemize{
#'  \item \code{TPR} True positive rate (Sensisivity, Recall)
#'  \item \code{FPR} False positve rate (Fall-out)
#'  \item \code{FNR} False negative rate (Miss rate)
#'  \item \code{TNR} True negative rate (Specifity)
#'  \item \code{PPV} Positive predictive value (Precision)
#'  \item \code{FOR} False omission rate
#'  \item \code{LRP} Positive likelihood ratio (LR+)
#'  \item \code{FDR} False discovery rate
#'  \item \code{NPV} Negative predictive value
#'  \item \code{ACC} Accuracy
#'  \item \code{LRM} Nevative likelihood ratio (LR-)
#'  \item \code{DOR} Diagnostic odds ratio)
#' }
#' 
#' For details on the used measures see \code{\link{measures}}.
#' 
#' @template arg_pred
#' 
#' @return [\code{ROCMeasures}] A list containing the above mentioned measures.
#' @export
#' @examples 
#' lrn = makeLearner("classif.rpart", predict.type = "prob")
#' fit = train(lrn, sonar.task)
#' pred = predict(fit, task = sonar.task)
#' calculateROCMeasures(pred)
#' 

calculateROCMeasures = function(pred) {
  
  checkPrediction(pred, task.type = "classif", check.truth = TRUE, no.na = TRUE, binary = TRUE)
  tab = getConfMatrix(pred, relative = TRUE)[1:2, 1:2]
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
    confusionMatrix = tab,
    measures = list(TPR = r.tpr,
      FNR = r.fnr,
      FPR = r.fpr,
      TNR = r.tnr,
      PPV = r.ppv,
      FDR = r.fdr,
      NPV = r.npv,
      FOR = r.for,
      ACC = r.acc,
      LRP = r.lr.plus,
      LRM = r.lr.minus,
      DOR = r.dor))
}

#' @export
#' @describeIn calculateROCMeasures
#' 
#' @param x \code{ROCMeasures}\cr
#'  A list of ROC measures created by \code{\link{calculateROCMeasures}}.
#' @param abbreviations \code{logical(1)}\cr
#'  If \code{TRUE} a short paragraph with explainations of the used measures is printed additionally.
#' @param digits \code{numeric(1)}\cr
#'  Number of digits the measures are rounded to.
#' @param ... \code{[any]}\cr
#'  Currently not used.
#'  

print.ROCMeasures = function(x, abbreviations = TRUE, digits = 2, ...) {
  
  checkFlag(abbreviations)
  checkInt(digits, lower = 1)

  #format measures
  x$measures = mapply(function(m, v) paste0(m, ": ", round(v, digits)), names(x$measures), x$measures)
  
  res = cbind(round(x$confusionMatrix, digits = digits), 
    c(x$measures[["TPR"]], x$measures[["FPR"]]), 
    c(x$measures[["FNR"]], x$measures[["TNR"]]))
  res = rbind(res, 
    c(x$measures[["PPV"]], x$measures[["FOR"]], x$measures[["LRP"]], x$measures[["ACC"]]),
    c(x$measures[["FDR"]], x$measures[["NPV"]], x$measures[["LRM"]], x$measures[["DOR"]]))
  
  names(dimnames(res)) = c("true", "predicted")
  print(noquote(res))
  if (abbreviations) {
    cat("\n\nAbbreviations:\n")
    cat("TPR - True positive rate (Sensisivity, Recall)\n")
    cat("FPR - False positve rate (Fall-out)\n")
    cat("FNR - False negative rate (Miss rate)\n")
    cat("TNR - True negative rate (Specifity)\n")
    cat("PPV - Positive predictive value (Precision)\n")
    cat("FOR - False omission rate\n")
    cat("LRP - Positive likelihood ratio (LR+)\n")
    cat("FDR - False discovery rate")
    cat("NPV - Negative predictive value")
    cat("ACC - Accuracy\n")
    cat("LRM - Nevative likelihood ratio (LR-)\n")
    cat("DOR - Diagnostic odds ratio)\n")
  }
}

