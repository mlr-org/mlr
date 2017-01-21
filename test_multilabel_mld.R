df = matrix(as.numeric(c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE)),
  ncol = 2, byrow = TRUE)
mymldr = mldr_from_dataframe(as.data.frame(df), labelIndices = c(1, 2), name = "testMLDR")

p = !df
storage.mode(p) = "numeric"

meas = mldr_evaluate(mymldr, predictions = p)
meas$Recall
meas$HammingLoss
meas$Recall
meas$Precision

bincombo = matrix(c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE), ncol = 2, byrow = TRUE)
multi.y1 = bincombo[rep(1:4, each = 4),]
multi.p1 = bincombo[rep(1:4, times = 4),]

df = multi.y1
storage.mode(df) = "numeric"
mldr = mldr_from_dataframe(as.data.frame(df), labelIndices = c(1, 2), name = "testMLDR")
predictions = multi.p1
storage.mode(predictions) = "numeric"
#meas = mldr_evaluate(mymldr, predictions = p)

trueLabels <- mldr$dataset[, mldr$labels$index]
bipartition <- predictions
active <- bipartition >= 0.5
bipartition[active] <- 1
bipartition[!active] <- 0
counters <- data.frame(RealPositives = rowSums(trueLabels),
  RealNegatives = rowSums(!trueLabels), PredictedPositives = rowSums(bipartition),
  PredictedNegatives = rowSums(!bipartition), TruePositives = rowSums(trueLabels &
      bipartition), TrueNegatives = rowSums(!trueLabels &
          !bipartition))

mldr:::mldr_Recall(counters)
measureMultilabelTPR(multi.y1, multi.p1)

mldr:::mldr_HL(trueLabels, predictions)
measureMultilabelHamloss(multi.y1, multi.p1)

mldr:::mldr_Precision(counters)
measureMultilabelPPV(multi.y1, multi.p1)



# FIXME: WTF?
mldr:::mldr_SubsetAccuracy(trueLabels, predictions)
measureMultilabelSubset01(multi.y1, multi.p1)

# FIXME: WTF?
mldr:::mldr_Accuracy(counters)
measureMultilabelACC(multi.y1, multi.p1)

# FIXME: WTF?
mldr:::mldr_FMeasure(counters)
mldr:::mldr_MicroFMeasure(trueLabels, bipartition)
mldr:::mldr_MacroFMeasure(trueLabels, bipartition)
measureMultiLabelF1(multi.y1, multi.p1)

