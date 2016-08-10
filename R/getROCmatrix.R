getROCmatrix = function(pred) {
  
  checkPrediction(pred, task.type = "classif", check.truth = TRUE, no.na = TRUE, binary = TRUE)
  tab = getConfMatrix(pred)
  response = getPredictionResponse(pred)
  truth = getPredictionTruth(truth)
  positive = pred$task.desc$positive
  negative = pred$task.desc$negative
  
  #calculate measures
  r.tpr = measureTPR(truth, response, positive)
  r.fnr = measureFNR(truth, response, negative, positive)
  r.fpr = measureFPR(truth, response, positive)
  r.tnr = measureTNR(truth, response, negative)
  r.prec = measurePPV(truth, response, positive)
  r.fdr = 1 - r.prec
  r.npv = measureNPV(truth, response, negative)
  r.for = 1 - r.npv
  r.acc = measureACC(truth, response)
  r.lr.plus = r.tpr / r.fpr
  r.lr.minus = r.fnr / r.tnr
  r.dor
  
  
  
  
}