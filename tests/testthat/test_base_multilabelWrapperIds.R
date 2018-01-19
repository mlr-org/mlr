context("multiLabelWrapperIds")

binary.learner = makeLearner("classif.rpart")

test_that("multiLabelWrapperIds DBR", {
  mlw = makeMultilabelDBRWrapper(binary.learner)
  expect_equal(getLearnerId(mlw), "multilabel.DBR.classif.rpart")
})

test_that("multiLabelWrapperIds NestedStacking", {
  mlw = makeMultilabelNestedStackingWrapper(binary.learner)
  expect_equal(getLearnerId(mlw), "multilabel.nestedStacking.classif.rpart")
})

test_that("multiLabelWrapperIds Stacking", {
  mlw = makeMultilabelStackingWrapper(binary.learner)
  expect_equal(getLearnerId(mlw), "multilabel.stacking.classif.rpart")
})

test_that("multiLabelWrapperIds BinaryRelevance", {
  mlw = makeMultilabelBinaryRelevanceWrapper(binary.learner)
  expect_equal(getLearnerId(mlw), "multilabel.binaryRelevance.classif.rpart")
})

test_that("multiLabelWrapperIds ClassifierChains", {
  mlw = makeMultilabelClassifierChainsWrapper(binary.learner)
  expect_equal(getLearnerId(mlw), "multilabel.classifierChains.classif.rpart")
})
