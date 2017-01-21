library(mldr)
bincombo = matrix(c(1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE)
response = bincombo[rep(1:4, each = 4),]

mldr = mldr_from_dataframe(as.data.frame(response), labelIndices = c(1, 2), name = "testMLDR")
predictions = bincombo[rep(1:4, times = 4),]
storage.mode(predictions) = "numeric"

# this is copy-paste from mldr_evaluate
trueLabels = mldr$dataset[, mldr$labels$index]
bipartition = predictions
active = bipartition >= 0.5
bipartition[active] = 1
bipartition[!active] = 0
counters = data.frame(RealPositives = rowSums(trueLabels),
  RealNegatives = rowSums(!trueLabels), PredictedPositives = rowSums(bipartition),
  PredictedNegatives = rowSums(!bipartition), TruePositives = rowSums(trueLabels &
      bipartition), TrueNegatives = rowSums(!trueLabels &
          !bipartition))

#
mldr:::mldr_Recall(counters)
measureMultilabelTPR(response, predictions)

mldr:::mldr_HL(trueLabels, predictions)
measureMultilabelHamloss(response, predictions)

mldr:::mldr_Precision(counters)
measureMultilabelPPV(response, predictions)

mldr:::mldr_SubsetAccuracy(trueLabels, predictions)
1-measureMultilabelSubset01(response, predictions) # we have implemented the subset loss

# FIXME: WTF?
mldr:::mldr_Accuracy(counters)
measureMultilabelACC(response, predictions)

# FIXME: WTF?
mldr:::mldr_FMeasure(counters)
mldr:::mldr_MicroFMeasure(trueLabels, bipartition)
mldr:::mldr_MacroFMeasure(trueLabels, bipartition)
measureMultiLabelF1(response, predictions)

