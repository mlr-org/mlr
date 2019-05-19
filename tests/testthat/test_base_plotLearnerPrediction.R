context("plotLearnerPrediction")

test_that("plotLearnerPrediction", {
  gs = 10
  plotLearnerPrediction("classif.rpart", multiclass.task, gridsize = gs)
  ggsave(tempfile(fileext = ".png"))
  plotLearnerPrediction("classif.rpart", multiclass.task, gridsize = gs, err.mark = "none")
  ggsave(tempfile(fileext = ".png"))
  plotLearnerPrediction("classif.rpart", binaryclass.task, gridsize = gs)
  ggsave(tempfile(fileext = ".png"))
  plotLearnerPrediction("classif.rpart", binaryclass.task, gridsize = gs, err.mark = "none")
  ggsave(tempfile(fileext = ".png"))

  plotLearnerPrediction("regr.rpart", regr.task, gridsize = gs)
  ggsave(tempfile(fileext = ".png"))
  plotLearnerPrediction("regr.lm", regr.task,
    features = getTaskFeatureNames(regr.task)[1], gridsize = gs)
  ggsave(tempfile(fileext = ".png"))

  plotLearnerPrediction("cluster.kmeans", noclass.task, gridsize = gs)
  ggsave(tempfile(fileext = ".png"))

  # pretty.names works
  lrn = makeLearner("classif.rpart")
  plotLearnerPrediction(lrn, multiclass.task)
  dir = tempdir()
  path = file.path(dir, "test.svg")
  ggsave(path)
  doc = XML::xmlParse(path)
  testDocForStrings(doc, getLearnerShortName(lrn))

  plotLearnerPrediction(lrn, multiclass.task, pretty.names = FALSE)
  dir = tempdir()
  path = file.path(dir, "test.svg")
  ggsave(path)
  doc = XML::xmlParse(path)
  testDocForStrings(doc, getLearnerId(lrn))
})
