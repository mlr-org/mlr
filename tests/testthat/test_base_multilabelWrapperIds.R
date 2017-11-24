context("multiLabelWrapperIds")

test_that("multiLabelWrapperIds DBR", {
  binary.learner = makeLearner("classif.rpart")
  mlw = makeMultilabelDBRWrapper(binary.learner)
  expect_equal(getLearnerId(mlw), "multilabelDBR.classif.rpart")
})

test_that("multiLabelWrapperIds NestedStacking", {
  binary.learner = makeLearner("classif.rpart")
  mlw = makeMultilabelNestedStackingWrapper(binary.learner)
  expect_equal(getLearnerId(mlw), "multilabelNestedStacking.classif.rpart")
})

test_that("multiLabelWrapperIds Stacking", {
  binary.learner = makeLearner("classif.rpart")
  mlw = makeMultilabelStackingWrapper(binary.learner)
  expect_equal(getLearnerId(mlw), "multilabelStacking.classif.rpart")
})

test_that("multiLabelWrapperIds BinaryRelevance", {
  binary.learner = makeLearner("classif.rpart")
  mlw = makeMultilabelBinaryRelevanceWrapper(binary.learner)
  expect_equal(getLearnerId(mlw), "multilabelBinaryRelevance.classif.rpart")
})

test_that("multiLabelWrapperIds ClassifierChains", {
  binary.learner = makeLearner("classif.rpart")
  mlw = makeMultilabelClassifierChainsWrapper(binary.learner)
  expect_equal(getLearnerId(mlw), "multilabelClassifierChains.classif.rpart")
})
