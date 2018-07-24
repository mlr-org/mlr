\name{mlrFamilies}
\alias{mlrFamilies}
\title{mlr documentation families}
\description{List of all mlr documentation families with members.}
\arguments{
\item{benchmark}{batchmark, reduceBatchmarkResults, benchmark, benchmarkParallel, getBMRTaskIds, getBMRLearners, getBMRLearnerIds, getBMRLearnerShortNames, getBMRMeasures, getBMRMeasureIds, getBMRPredictions, getBMRPerformances, getBMRAggrPerformances, getBMRTuneResults, getBMRFeatSelResults, getBMRFilteredFeatures, getBMRModels, getBMRTaskDescs, convertBMRToRankMatrix, friedmanPostHocTestBMR, friedmanTestBMR, plotBMRBoxplots, plotBMRRanksAsBarChart, generateCritDifferencesData, plotCritDifferences}
\item{calibration}{generateCalibrationData, plotCalibration}
\item{configure}{configureMlr, getMlrOptions}
\item{costsens}{makeCostSensTask, makeCostSensWeightedPairsWrapper}
\item{debug}{predictFailureModel, getPredictionDump, getRRDump, print.ResampleResult}
\item{downsample}{downsample}
\item{eda_and_preprocess}{capLargeValues, createDummyFeatures, dropFeatures, mergeSmallFactorLevels, normalizeFeatures, removeConstantFeatures, summarizeColumns, summarizeLevels}
\item{extractFDAFeatures}{reextractFDAFeatures}
\item{fda_featextractor}{extractFDAFourier, extractFDAWavelets, extractFDAFPCA, extractFDAMultiResFeatures}
\item{fda}{makeExtractFDAFeatMethod, extractFDAFeatures}
\item{featsel}{analyzeFeatSelResult, makeFeatSelControl, getFeatSelResult, selectFeatures}
\item{filter}{filterFeatures, makeFilter, listFilterMethods, getFilteredFeatures, generateFilterValuesData, getFilterValues}
\item{generate_plot_data}{generateFeatureImportanceData, plotFilterValues, generatePartialDependenceData}
\item{help}{helpLearner, helpLearnerParam}
\item{imbalancy}{oversample, smote}
\item{impute}{makeImputeMethod, imputeConstant, impute, reimpute}
\item{learner}{getClassWeightParam, getHyperPars, getParamSet.Learner, getLearnerType, getLearnerId, getLearnerPredictType, getLearnerPackages, getLearnerParamSet, getLearnerParVals, setLearnerId, getLearnerShortName, getLearnerProperties, makeLearner, makeLearners, removeHyperPars, setHyperPars, setId, setPredictThreshold, setPredictType}
\item{learning_curve}{generateLearningCurveData}
\item{multilabel}{getMultilabelBinaryPerformances, makeMultilabelBinaryRelevanceWrapper, makeMultilabelClassifierChainsWrapper, makeMultilabelDBRWrapper, makeMultilabelNestedStackingWrapper, makeMultilabelStackingWrapper}
\item{performance}{calculateConfusionMatrix, calculateROCMeasures, makeCustomResampledMeasure, makeCostMeasure, setMeasurePars, setAggregation, makeMeasure, featperc, performance, estimateRelativeOverfitting}
\item{plot}{createSpatialResamplingPlots, plotLearningCurve, plotPartialDependence, plotBMRSummary, plotResiduals}
\item{predict}{asROCRPrediction, getPredictionProbabilities, getPredictionTaskDesc, getPredictionResponse, predict.WrappedModel}
\item{resample}{makeResampleDesc, makeResampleInstance, makeResamplePrediction, resample, getRRPredictions, getRRTaskDescription, getRRTaskDesc, getRRPredictionList, addRRMeasure}
\item{task}{getTaskDesc, getTaskType, getTaskId, getTaskTargetNames, getTaskClassLevels, getTaskFeatureNames, getTaskNFeats, getTaskSize, getTaskFormula, getTaskTargets, getTaskData, getTaskCosts, subsetTask}
\item{thresh_vs_perf}{generateThreshVsPerfData, plotThreshVsPerf, plotROCCurves}
\item{tune}{getNestedTuneResultsX, getNestedTuneResultsOptPathDf, getResamplingIndices, getTuneResult, makeModelMultiplexerParamSet, makeModelMultiplexer, makeTuneControlCMAES, makeTuneControlDesign, makeTuneControlGenSA, makeTuneControlGrid, makeTuneControlIrace, makeTuneControlMBO, makeTuneControl, makeTuneControlRandom, tuneParams, tuneThreshold}
\item{tune_multicrit}{plotTuneMultiCritResult, makeTuneMultiCritControl, tuneParamsMultiCrit}
\item{wrapper}{makeBaggingWrapper, makeClassificationViaRegressionWrapper, makeConstantClassWrapper, makeCostSensClassifWrapper, makeCostSensRegrWrapper, makeDownsampleWrapper, makeDummyFeaturesWrapper, makeExtractFDAFeatsWrapper, makeFeatSelWrapper, makeFilterWrapper, makeImputeWrapper, makeMulticlassWrapper, makeOverBaggingWrapper, makeUndersampleWrapper, makePreprocWrapperCaret, makePreprocWrapper, makeRemoveConstantFeaturesWrapper, makeSMOTEWrapper, makeTuneWrapper, makeWeightedClassesWrapper}
}
