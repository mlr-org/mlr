\name{mlrFamilies}
\title{mlr documentation families}
\description{List of all mlr documentation families with members.}
\arguments{
\item{benchmark}{benchmark, NULL, getBMRTaskIds, getBMRLearners, getBMRLearnerIds, getBMRLearnerShortNames, getBMRMeasures, getBMRMeasureIds, getBMRPredictions, getBMRPerformances, getBMRAggrPerformances, getBMRTuneResults, getBMRFeatSelResults, getBMRFilteredFeatures, getBMRModels, convertBMRToRankMatrix, friedmanPostHocTestBMR, friedmanTestBMR, plotBMRBoxplots, plotBMRRanksAsBarChart, generateCritDifferencesData, plotCritDifferences}
\item{calibration}{generateCalibrationData, plotCalibration}
\item{configure}{configureMlr, getMlrOptions}
\item{costsens}{makeCostSensTask, makeCostSensWeightedPairsWrapper}
\item{downsample}{downsample}
\item{eda_and_preprocess}{capLargeValues, createDummyFeatures, dropFeatures, mergeSmallFactorLevels, normalizeFeatures, removeConstantFeatures, summarizeColumns, summarizeLevels}
\item{featsel}{analyzeFeatSelResult, NULL, getFeatSelResult, selectFeatures}
\item{filter}{filterFeatures, getFilteredFeatures, generateFilterValuesData, getFilterValues, plotFilterValuesGGVIS}
\item{generate_plot_data}{generateFeatureImportanceData, plotFilterValues, generatePartialDependenceData, generateFunctionalANOVAData}
\item{imbalancy}{oversample, smote}
\item{impute}{makeImputeMethod, NULL, impute, reimpute}
\item{learner}{getClassWeightParam, getHyperPars, NULL, NULL, makeLearner, removeHyperPars, setHyperPars, setId, setPredictThreshold, setPredictType}
\item{learning_curve}{generateLearningCurveData, plotLearningCurveGGVIS}
\item{multilabel}{getMultilabelBinaryPerformances, makeMultilabelBinaryRelevanceWrapper, makeMultilabelClassifierChainsWrapper, makeMultilabelDBRWrapper, makeMultilabelNestedStackingWrapper, makeMultilabelStackingWrapper}
\item{performance}{calculateConfusionMatrix, NULL, calculateROCMeasures, makeCustomResampledMeasure, makeCostMeasure, makeMeasure, NULL, performance, estimateRelativeOverfitting}
\item{plot}{plotLearningCurve, plotPartialDependence, plotPartialDependenceGGVIS, plotBMRSummary}
\item{predict}{asROCRPrediction, plotViperCharts, getPredictionProbabilities, getPredictionResponse, predict.WrappedModel}
\item{resample}{makeResampleDesc, makeResampleInstance, NULL, resample, getRRPredictions, NULL}
\item{task}{getTaskDescription, getTaskType, getTaskId, getTaskTargetNames, getTaskClassLevels, getTaskFeatureNames, getTaskNFeats, getTaskSize, getTaskFormula, getTaskTargets, getTaskData, getTaskCosts, subsetTask}
\item{thresh_vs_perf}{generateThreshVsPerfData, plotThreshVsPerf, plotThreshVsPerfGGVIS, plotROCCurves}
\item{tune}{getNestedTuneResultsX, getNestedTuneResultsOptPathDf, getTuneResult, makeModelMultiplexerParamSet, makeModelMultiplexer, NULL, tuneParams, tuneThreshold}
\item{tune_multicrit}{plotTuneMultiCritResult, plotTuneMultiCritResultGGVIS, NULL, tuneParamsMultiCrit}
\item{wrapper}{makeBaggingWrapper, makeConstantClassWrapper, makeCostSensClassifWrapper, makeCostSensRegrWrapper, makeDownsampleWrapper, makeFeatSelWrapper, makeFilterWrapper, makeImputeWrapper, makeRemoveConstantFeaturesWrapper, makeMulticlassWrapper, makeOverBaggingWrapper, makeUndersampleWrapper, makePreprocWrapperCaret, makePreprocWrapper, makeSMOTEWrapper, makeTuneWrapper, makeWeightedClassesWrapper}
}
