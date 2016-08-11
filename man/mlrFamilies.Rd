
ame{mlrFamilies}
	itle{mlr documentation families}
\description{List of all mlr documentation families with members.}
rguments{
\item{imbalancy}{oversample, smote}
\item{multilabel}{makeMultilabelBinaryRelevanceWrapper, makeMultilabelClassifierChainsWrapper, makeMultilabelDBRWrapper, makeMultilabelNestedStackingWrapper, makeMultilabelStackingWrapper, getMultilabelBinaryPerformances}
\item{tune}{makeModelMultiplexer, makeModelMultiplexerParamSet, NULL, getNestedTuneResultsX, getNestedTuneResultsOptPathDf, getTuneResult, tuneParams, tuneThreshold}
\item{performance}{makeMeasure, makeCustomResampledMeasure, makeCostMeasure, NULL, performance, estimateRelativeOverfitting}
\item{learning_curve}{generateLearningCurveData, plotLearningCurveGGVIS}
\item{generate_plot_data}{generateFeatureImportanceData, plotFilterValues, generatePartialDependenceData, generateFunctionalANOVAData}
\item{plot}{plotLearningCurve, plotPartialDependence, plotPartialDependenceGGVIS, plotBMRSummary}
\item{resample}{makeResampleDesc, makeResampleInstance, NULL, NULL, getRRPredictions, resample}
\item{learner}{NULL, getClassWeightParam, getHyperPars, NULL, makeLearner, removeHyperPars, setHyperPars, setId, setPredictThreshold, setPredictType}
\item{featsel}{NULL, analyzeFeatSelResult, getFeatSelResult, selectFeatures}
\item{thresh_vs_perf}{generateThreshVsPerfData, plotThreshVsPerf, plotThreshVsPerfGGVIS, plotROCCurves}
\item{downsample}{downsample}
\item{task}{getTaskDescription, getTaskType, getTaskId, getTaskTargetNames, getTaskClassLevels, getTaskFeatureNames, getTaskNFeats, getTaskSize, getTaskFormula, getTaskTargets, getTaskData, getTaskCosts, subsetTask}
\item{impute}{impute, reimpute, makeImputeMethod, NULL}
\item{benchmark}{getBMRTaskIds, getBMRLearners, getBMRLearnerIds, getBMRLearnerShortNames, getBMRMeasures, getBMRMeasureIds, getBMRPredictions, getBMRPerformances, getBMRAggrPerformances, getBMRTuneResults, getBMRFeatSelResults, getBMRFilteredFeatures, getBMRModels, benchmark, NULL, convertBMRToRankMatrix, friedmanPostHocTestBMR, friedmanTestBMR, plotBMRBoxplots, plotBMRRanksAsBarChart, generateCritDifferencesData, plotCritDifferences}
\item{eda_and_preprocess}{capLargeValues, createDummyFeatures, dropFeatures, mergeSmallFactorLevels, normalizeFeatures, removeConstantFeatures, summarizeColumns, summarizeLevels}
\item{costsens}{makeCostSensTask, makeCostSensWeightedPairsWrapper}
\item{configure}{configureMlr, getMlrOptions}
\item{predict}{getPredictionProbabilities, getPredictionResponse, asROCRPrediction, plotViperCharts, predict.WrappedModel}
\item{tune_multicrit}{NULL, plotTuneMultiCritResult, plotTuneMultiCritResultGGVIS, tuneParamsMultiCrit}
\item{wrapper}{makeBaggingWrapper, makeConstantClassWrapper, makeCostSensClassifWrapper, makeCostSensRegrWrapper, makeDownsampleWrapper, makeFeatSelWrapper, makeFilterWrapper, makeImputeWrapper, makeMulticlassWrapper, makeOverBaggingWrapper, makeUndersampleWrapper, makePreprocWrapper, makePreprocWrapperCaret, makeSMOTEWrapper, makeTuneWrapper, makeWeightedClassesWrapper, makeRemoveConstantFeaturesWrapper}
\item{calibration}{generateCalibrationData, plotCalibration}
\item{filter}{getFilteredFeatures, filterFeatures, generateFilterValuesData, getFilterValues, plotFilterValuesGGVIS}
}
