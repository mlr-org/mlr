\name{mlrFamilies}
\title{mlr documentation families}
\description{List of all mlr documentation families with members.}
\arguments{
\item{featsel}{analyzeFeatSelResult, makeFeatSelControl, getFeatSelResult, selectFeatures}
\item{thresh_vs_perf}{generateThreshVsPerfData, plotThreshVsPerf, plotThreshVsPerfGGVIS, plotROCCurves}
\item{multilabel}{getMultilabelBinaryPerformances, makeMultilabelBinaryRelevanceWrapper, makeMultilabelClassifierChainsWrapper, makeMultilabelDBRWrapper, makeMultilabelNestedStackingWrapper, makeMultilabelStackingWrapper}
\item{configure}{configureMlr, getMlrOptions}
\item{eda_and_preprocess}{capLargeValues, createDummyFeatures, dropFeatures, mergeSmallFactorLevels, normalizeFeatures, removeConstantFeatures, summarizeColumns, summarizeLevels}
\item{benchmark}{benchmark, benchmarkParallel, getBMRTaskIds, getBMRLearners, getBMRLearnerIds, getBMRLearnerShortNames, getBMRMeasures, getBMRMeasureIds, getBMRPredictions, getBMRPerformances, getBMRAggrPerformances, getBMRTuneResults, getBMRFeatSelResults, getBMRFilteredFeatures, getBMRModels, convertBMRToRankMatrix, friedmanPostHocTestBMR, friedmanTestBMR, plotBMRBoxplots, plotBMRRanksAsBarChart, generateCritDifferencesData, plotCritDifferences}
\item{generate_plot_data}{generatePartialDependenceData, generateFunctionalANOVAData}
\item{plot}{plotLearningCurve, plotPartialDependence, plotPartialDependenceGGVIS, plotBMRSummary}
\item{learning_curve}{generateLearningCurveData, plotLearningCurveGGVIS}
\item{downsample}{downsample}
\item{wrapper}{makeBaggingWrapper, makeConstantClassWrapper, makeCostSensClassifWrapper, makeCostSensRegrWrapper, makeDownsampleWrapper, makeFeatSelWrapper, makeFilterWrapper, makeImputeWrapper, makeRemoveConstantFeaturesWrapper, makeMulticlassWrapper, makeOverBaggingWrapper, makeUndersampleWrapper, makePreprocWrapperCaret, makePreprocWrapper, makeSMOTEWrapper, makeTuneWrapper, makeWeightedClassesWrapper}
\item{performance}{makeCustomResampledMeasure, makeCostMeasure, makeMeasure, featperc, performance, estimateRelativeOverfitting}
\item{tune}{getNestedTuneResultsX, getNestedTuneResultsOptPathDf, getTuneResult, makeModelMultiplexerParamSet, makeModelMultiplexer, makeTuneControl, tuneParams, tuneThreshold}
\item{resample}{makeResampleDesc, makeResampleInstance, makeResamplePrediction, resample, getRRPredictions, print.ResampleResult}
\item{learner}{getClassWeightParam, getHyperPars, getParamSet.Learner, getLearnerProperties, makeLearner, removeHyperPars, setHyperPars, setId, setPredictThreshold, setPredictType}
\item{task}{getTaskDescription, getTaskType, getTaskId, getTaskTargetNames, getTaskClassLevels, getTaskFeatureNames, getTaskNFeats, getTaskSize, getTaskFormula, getTaskTargets, getTaskData, getTaskCosts, subsetTask}
\item{tune_multicrit}{plotTuneMultiCritResult, plotTuneMultiCritResultGGVIS, makeTuneMultiCritControl, tuneParamsMultiCrit}
\item{predict}{asROCRPrediction, plotViperCharts, getPredictionProbabilities, getPredictionResponse, predict.WrappedModel}
\item{calibration}{generateCalibrationData, plotCalibration}
\item{filter}{filterFeatures, getFilteredFeatures, generateFilterValuesData, getFilterValues, plotFilterValues, plotFilterValuesGGVIS}
\item{imbalancy}{oversample, smote}
\item{impute}{makeImputeMethod, imputeConstant, impute, reimpute}
\item{costsens}{makeCostSensTask, makeCostSensWeightedPairsWrapper}
}
