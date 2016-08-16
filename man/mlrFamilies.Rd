\name{mlrFamilies}
\title{mlr documentation families}
\description{List of all mlr documentation families with members.}
\arguments{
\item{imbalancy}{oversample, smote}
\item{tune}{getNestedTuneResultsX, getNestedTuneResultsOptPathDf, getTuneResult, makeModelMultiplexerParamSet, makeModelMultiplexer, NULL, tuneParams, tuneThreshold}
\item{multilabel}{getMultilabelBinaryPerformances, makeMultilabelBinaryRelevanceWrapper, makeMultilabelClassifierChainsWrapper, makeMultilabelDBRWrapper, makeMultilabelNestedStackingWrapper, makeMultilabelStackingWrapper}
\item{performance}{calculateConfusionMatrix, NULL, calculateROCMeasures, makeCustomResampledMeasure, makeCostMeasure, makeMeasure, NULL, performance, estimateRelativeOverfitting}
\item{learning_curve}{generateLearningCurveData, plotLearningCurveGGVIS}
\item{generate_plot_data}{generateFeatureImportanceData, plotFilterValues, generatePartialDependenceData, generateFunctionalANOVAData}
\item{resample}{makeResampleDesc, makeResampleInstance, NULL, resample, getRRPredictions, NULL}
\item{learner}{getClassWeightParam, getHyperPars, NULL, NULL, makeLearner, removeHyperPars, setHyperPars, setId, setPredictThreshold, setPredictType}
\item{plot}{plotLearningCurve, plotPartialDependence, plotPartialDependenceGGVIS, plotBMRSummary}
\item{featsel}{analyzeFeatSelResult, NULL, getFeatSelResult, selectFeatures}
\item{task}{getTaskDescription, getTaskType, getTaskId, getTaskTargetNames, getTaskClassLevels, getTaskFeatureNames, getTaskNFeats, getTaskSize, getTaskFormula, getTaskTargets, getTaskData, getTaskCosts, subsetTask}
\item{impute}{makeImputeMethod, NULL, impute, reimpute}
\item{thresh_vs_perf}{generateThreshVsPerfData, plotThreshVsPerf, plotThreshVsPerfGGVIS, plotROCCurves}
\item{downsample}{downsample}
\item{benchmark}{benchmark, NULL, getBMRTaskIds, getBMRLearners, getBMRLearnerIds, getBMRLearnerShortNames, getBMRMeasures, getBMRMeasureIds, getBMRPredictions, getBMRPerformances, getBMRAggrPerformances, getBMRTuneResults, getBMRFeatSelResults, getBMRFilteredFeatures, getBMRModels, convertBMRToRankMatrix, friedmanPostHocTestBMR, friedmanTestBMR, plotBMRBoxplots, plotBMRRanksAsBarChart, generateCritDifferencesData, plotCritDifferences}
\item{eda_and_preprocess}{capLargeValues, createDummyFeatures, dropFeatures, mergeSmallFactorLevels, normalizeFeatures, removeConstantFeatures, summarizeColumns, summarizeLevels}
\item{costsens}{makeCostSensTask, makeCostSensWeightedPairsWrapper}
\item{configure}{configureMlr, getMlrOptions}
\item{predict}{asROCRPrediction, plotViperCharts, getPredictionProbabilities, getPredictionResponse, predict.WrappedModel}
\item{tune_multicrit}{plotTuneMultiCritResult, plotTuneMultiCritResultGGVIS, NULL, tuneParamsMultiCrit}
\item{wrapper}{makeBaggingWrapper, makeConstantClassWrapper, makeCostSensClassifWrapper, makeCostSensRegrWrapper, makeDownsampleWrapper, makeFeatSelWrapper, makeFilterWrapper, makeImputeWrapper, makeRemoveConstantFeaturesWrapper, makeMulticlassWrapper, makeOverBaggingWrapper, makeUndersampleWrapper, makePreprocWrapperCaret, makePreprocWrapper, makeSMOTEWrapper, makeTuneWrapper, makeWeightedClassesWrapper}
\item{calibration}{generateCalibrationData, plotCalibration}
\item{filter}{filterFeatures, getFilteredFeatures, generateFilterValuesData, getFilterValues, plotFilterValuesGGVIS}
}
