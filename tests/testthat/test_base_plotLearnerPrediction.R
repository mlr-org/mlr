
test_that("plotLearnerPrediction", {
  requirePackagesOrSkip("clusterSim", default.method = "load")
  gs = 10
  plotLearnerPrediction("classif.rpart", multiclass.task, gridsize = gs)
  suppressMessages(ggsave(tempfile(fileext = ".png")))
  plotLearnerPrediction("classif.rpart", multiclass.task, gridsize = gs, err.mark = "none")
  suppressMessages(ggsave(tempfile(fileext = ".png")))
  plotLearnerPrediction("classif.rpart", binaryclass.task, gridsize = gs)
  suppressMessages(ggsave(tempfile(fileext = ".png")))
  plotLearnerPrediction("classif.rpart", binaryclass.task, gridsize = gs, err.mark = "none")
  suppressMessages(ggsave(tempfile(fileext = ".png")))

  plotLearnerPrediction("regr.rpart", regr.task, gridsize = gs)
  suppressMessages(ggsave(tempfile(fileext = ".png")))
  plotLearnerPrediction("regr.lm", regr.task,
    features = getTaskFeatureNames(regr.task)[1], gridsize = gs)
  suppressMessages(ggsave(tempfile(fileext = ".png")))

  plotLearnerPrediction("cluster.kmeans", noclass.task, gridsize = gs)
  suppressMessages(ggsave(tempfile(fileext = ".png")))

  # pretty.names works
  lrn = makeLearner("classif.rpart")
  plotLearnerPrediction(lrn, multiclass.task)
  dir = tempdir()
  path = file.path(dir, "test.svg")
  suppressMessages(ggsave(path))
  doc = XML::xmlParse(path)
  testDocForStrings(doc, getLearnerShortName(lrn))

  plotLearnerPrediction(lrn, multiclass.task, pretty.names = FALSE)
  dir = tempdir()
  path = file.path(dir, "test.svg")
  suppressMessages(ggsave(path))
  doc = XML::xmlParse(path)
  testDocForStrings(doc, getLearnerId(lrn))
})
