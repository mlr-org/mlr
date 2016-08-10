getROCmatrix = function(pred, relative = TRUE) {
  
  checkPrediction(pred, task.type = "classif", check.truth = TRUE, no.na = TRUE, binary = TRUE)
  tab = getConfMatrix(pred, relative = relative)$result[1:2, 1:2]
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
  r.fdr = 1 - r.prec
  r.npv = measureNPV(truth, response, negative)
  r.for = 1 - r.npv
  r.acc = measureACC(truth, response)
  r.lr.plus = r.tpr / r.fpr
  r.lr.minus = r.fnr / r.tnr
  r.dor = r.lr.plus / r.lr.minus
  
  makeS3Obj("ROCmatrix",
    confusionMatrix = tab,
    measures = list(TPR = r.tpr,
      FNR = r.fnr,
      FPR = r.fpr,
      TNR = r.tnr,
      PPV = r.prec,
      FDR = r.fdr,
      NPV = r.npv,
      FOR = r.for,
      ACC = r.acc,
      LRP = r.lr.plus,
      LRM = r.lr.minus,
      DOR = r.dor))
}


print.ROCmatrix = function(r, digits = 2) {

  #format measures
  sig = paste0(": %.", digits, "f")
  r$measures = mapply(function(m, v) paste0(m, sprintf(sig, v)), names(r$measures), r$measures)
  
  res = cbind(r$confusionMatrix, 
    c(r$measures[["TPR"]], r$measures[["FPR"]]), 
    c(r$measures[["FNR"]], r$measures[["TNR"]]))
  res = rbind(res, 
    c(r$measures[["PPV"]], r$measures[["FOR"]], r$measures[["LRP"]], r$measures[["ACC"]]),
    c(r$measures[["FDR"]], r$measures[["NPV"]], r$measures[["LRM"]], r$measures[["DOR"]]))
  
  names(dimnames(res)) = c("true", "predicted")
  print(noquote(res))
  
  cat("\n\nAbbreviations:\n")
  cat("TPR - True positive rate (Sensisivity, Recall)\n")
  cat("FPR - False positve rate (Fall-out)\n")
  cat("FNR - False negative rate (Miss rate)\n")
  cat("TNR - True negative rate (Specifity)\n")
  cat("PPV - Positive predictive value (Precision)\n")
  cat("FOR - False omission rate\n")
  cat("LRP - Positive likelihood ratio (LR+)\n")
  cat("ACC - Accuracy\n")
  cat("LRM - Nevative likelihood ratio (LR-)\n")
  cat("DOR - Diagnostic odds ratio)\n")

}

