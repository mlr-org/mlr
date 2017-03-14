# mlr 2.11:

## general
* The internal class naming of the task descriptions have been changed causing probable incompatibilities with tasks generated under old versions.
* New option on.error.dump to include dumps that can be inspected with the
  debugger with errors
* mlr now supports tuning with Bayesian optimization with mlrMBO

## functions - general
* tuneParams: fixed a small and obscure bug in logging for extremely large ParamSets
* getBMR-operators: now support "drop" argument that simplifies the resulting list
* configureMlr: added option "on.measure.not.applicable" to handle situations where performance
  cannot be calculated and one wants NA instead of an error - useful in, e.g., larger benchmarks
* tuneParams, selectFeatures: removed memory stats from default output for
  performance reasons (can be restored by using a control object with "log.fun"
  = "memory")
* listLearners: change check.packages default to FALSE
* tuneParams and tuneParamsMultiCrit: new parameter `resample.fun` to specify a custom resampling function to use.
* Deprecated: getTaskDescription, getBMRTaskDescriptions, getRRTaskDescription.
  New names: getTaskDesc, getBMRTaskDescs, getRRTaskDesc.

## functions - new
* getOOBPreds: get out-of-bag predictions from trained models for learners that store them -- these learners have the new "oobpreds" property
* listTaskTypes, listLearnerProperties
* getMeasureProperties, hasMeasureProperties, listMeasureProperties
* makeDummyFeaturesWrapper: fuse a learner with a dummy feature creator
* simplifyMeasureNames: shorten measure names to the actual measure, e.g.
  mmce.test.mean -> mmce
* getFailureModelDump, getPredictionDump, getRRDump: get error dumps
* batchmark: Function to run benchmarks with the batchtools package on high performance computing clusters
* makeTuneControlMBO: allows Bayesian optimization

## measures - new
* kendalltau, spearmanrho

## learners - general
* classif.plsdaCaret: added parameter "method".
* regr.randomForest: refactored se-estimation code, improved docs and default is now se.method = "jackknife".
* regr.xgboost, classif.xgboost: removed "factors" property as these learners do not handle categorical features
-- factors are silently converted to integers internally, which may misinterpret the structure of the data
* glmnet: control parameters are reset to factory settings before applying
  custom settings and training and set back to factory afterwards

## learners - removed
* {classif,regr}.avNNet: no longer necessary, mlr contains a bagging wrapper

# mlr 2.10:

## functions - general
* fixed bug in resample when using predict = "train" (issue #1284)
* update to irace 2.0 -- there are algorithmic changes in irace that may affect
  performance
* generateFilterValuesData: fixed a bug wrt feature ordering
* imputeLearner: fixed a bug when data actually contained no NAs
* print.Learner: if a learner hyperpar was set to value "NA" this was not
  displayed in printer
* makeLearner, setHyperPars: if you mistype a learner or hyperpar name, mlr
  uses fuzzy matching to suggest the 3 closest names in the message
* tuneParams: tuning with irace is now also parallelized, i.e., different
  learner configs are evaluated in parallel.
* benchmark: mini fix, arg 'learners' now also accepts class strings
* object printers: some mlr printers show head previews of data.frames.
  these now also print info on the total nr of rows and cols and are less confusing
* aggregations: have better properties now, they know whether they require training or
  test set evals
* the filter methods have better R docs
* filter randomForestSRC.var.select: new arg "method"
* filter mrmr: fixed some smaller bugs and updated properties
* generateLearningCurveData: also accepts single learner, does not require a list
* plotThreshVsPerf: added "measures" arg
* plotPartialDependence: can create tile plots with joint partial dependence
  on two features for multiclass classification by facetting across the classes
* generatePartialDependenceData and generateFunctionalANOVAData: expanded
  "fun" argument to allow for calculation of weights
* new "?mlrFamilies" manual page which lists all families and the functions
  belonging to it
* we are converging on data.table as a standard internally, this should not
  change any API behavior on the outside, though
* generateHyperParsEffectData and plotHyperParsEffect now support more than 2
  hyperparameters
* linear.correlation, rank.correlation, anova.test: use Rfast instead of
  FSelector/custom implementation now, performance should be much better
* use of our own colAUC function instead of the ROCR package for AUC calculation
  to improve performance
* we output resample performance messages for every iteration now
* performance improvements for the auc measure
* createDummyFeatures supports vectors now
* removed the pretty.names argument from plotHyperParsEffect -- labels can be set
  though normal ggplot2 functions on the returned object
* Fixed a bad bug in resample, the slot "runtime" or a ResampleResult,
  when the runtime was measured not in seconds but e.g. mins. R measures then potentially in mins,
  but mlr claimed it would be seconds.
* New "dummy" learners (that disregard features completely) can be fitted now for baseline comparisons,
  see "featureless" learners below.

## functions - new
* filter: randomForest.importance
* generateFeatureImportanceData: permutation-based feature importance and local
  importance
* getFeatureImportanceLearner: new Learner API function
* getFeatureImportance: top level function to extract feature importance
  information
* calculateROCMeasures
* calculateConfusionMatrix: new confusion-matrix like function that calculates
  and tables many receiver operator measures
* makeLearners: create multiple learners at once
* getLearnerId, getLearnerType, getLearnerPredictType, getLearnerPackages
* getLearnerParamSet, getLearnerParVals
* getRRPredictionList
* addRRMeasure
* plotResiduals
* getLearnerShortName
* mergeBenchmarkResults

## functions - renamed
* Renamed rf.importance filter (now deprecated) to randomForestSRC.var.rfsrc
* Renamed rf.min.depth filter (now deprecated) to randomForestSRC.var.select
* Renamed getConfMatrix (now deprecated) to calculateConfusionMatrix
* Renamed setId (now deprecated) to setLearnerId

## functions - removed
* mergeBenchmarkResultLearner, mergeBenchmarkResultTask

## learners - general
* classif.ada: fixed some param problem with rpart.control params
* classif.cforest, regr.cforest, surv.cforest:
  removed parameters "minprob", "pvalue", "randomsplits"
  as these are set internally and cannot be changed by the user
* regr.GPfit: some more params for correlation kernel
* classif.xgboost, regr.xgboost: can now properly handle NAs (property was missing and other problems), added "colsample_bylevel" parameter
* adapted {classif,regr,surv}.ranger parameters for new ranger version

## learners - new
* multilabel.cforest
* surv.gbm
* regr.cvglmnet
* {classif,regr,surv}.gamboost
* classif.earth
* {classif,regr}.evtree
* {classif,regr}.evtree

## learners - removed
* classif.randomForestSRCSyn, regr.randomForestSRCSyn: due to continued stability issues

## measures - new
* ssr, qsr, lsr
* rrse, rae, mape
* kappa, wkappa
* msle, rmsle

# mlr 2.9:

## functions - general
* various cleanups that removed unused code
* subsetTask, getTaskData: arg "features" now also accepts logical and integer
* removeConstantFeatures now also operates on data.frames and
  makeRemoveConstantFeaturesWrapper can be used to augment a learner with this
  preprocessing step.
* normalizeFeatures, createDummyFeatures: arg 'exclude' was replaced by 'cols'
* normalizeFeatures is now S3 and can be called also on data.frames
* SMOTEWrapper: fix a bug where "sw.nn" was not correctly passed down
* fixed a bug that caused hyperparameters to be not passed on correctly in the
  ModelMultiplexer in some cases
* fix bug with NoFeaturesModel and ModelMultiplexer
* fix small bug in DownsampleWrapper when trained with weights
* getNestedTuneResultsOptPathDf: added new arg "trafo"
* improve documentation for permutation.importance filter and perform slight
  argument renaming to fix potential name clashes
* plotPartialDependence can plot classification tasks with more than one
  interacted features now
* generateFilterValuesData: added argument 'more.args'
* add pretty.names arguments to plots that show learner short names instead of IDs
* addition of 'data' argument to plotPartialDependence which adds the training
  data to the graph
* added new arguments "facet.wrap.nrow" and "facet.wrap.ncol" which enable
  arrangement of facets in
  rows and columns to plotting functions

## functions - new
* generateHyperParsEffectData, plotHyperParsEffect
* makeMultilabelClassifierChainsWrapper, makeMultilabelDBRWrapper
  makeMultilabelNestedStackingWrapper, makeMultilabelStackingWrapper
* makeConstantClassWrapper
* generateFunctionalANOVAData

## functions - removed
* getParamSet generic (now in ParamHelpers package)

## functions - renamed
* generatePartialPrediction to generatePartialDependence
* plotPartialPrediction to plotPartialDependence
* plotPartialPredictionGGVIS to plotPartialDependenceGGVIS

## learners - general
* fixed weight handling and weight tag for some learners
* remove unnecessary linear.output parameter for classif.neuralnet
* remove unsupported KSVM parameter value stringdot
* fix some bartMachine compatibility issues
* classif.ranger, regr.ranger and surv.ranger: now respect unordered factors by
  default
* clean up randomForestSRC and randomForestSRCSyn learners
* the "penalized" learner were restructured and improved (params were added), also
  see below.
* add stability.nugget parameter for "regr.km"
* classif.blackboost, regr.blackboost: made sure that arg "stump" is passed on
  correctly
* fixed parameter values for WEKA learners IBk, J48, PART, EM, SimpleKMeans, XMeans
* classif.glmboost, regr.glmboost: add parameters stopintern and trace

## learners - new
* classif.C50
* classif.gausspr
* classif.penalized.fusedlasso
* classif.penalized.lasso
* classif.penalized.ridge
* classif.h2o.deeplearning
* classif.h2o.gbm
* classif.h2o.glm
* classif.h2o.randomForest
* classif.rrf
* regr.penalized.fusedlasso
* regr.gausspr
* regr.glm
* regr.GPfit
* regr.h2o.deeplearning
* regr.h2o.gbm
* regr.h2o.glm
* regr.h2o.randomForest
* regr.rrf
* surv.cv.CoxBoost
* surv.penalized.fusedlasso
* surv.penalized.lasso
* surv.penalized.ridge
* cluster.kkmeans
* multilabel.randomforestSRC

## learners - removed
* surv.optimCoxBoostPenalty
* surv.penalized (split up, see new learners above)

## measures - general
* updated gmean measure and unit test, added reference to formula of gmean
* makeCostMeasure: removed arg "task", names of cost matrix are checked on measure
  calculation

## measures - new
* multiclass.brier
* brier.scaled
* logloss
* multilabel.subset01, multilabel.f1, multilabel.acc, multilabel.ppv,
  multilabel.tpr
* multiclass.au1p, multiclass.au1u, multiclass.aunp, multiclass.aunu

## measures - renamed
* multiclass.auc to multiclass.au1u
* hamloss to multilabel.hamloss

# mlr 2.8:
* Feature filter "univariate" had a bad name, was deprecated and is now called
  "univariate.model.score". The new one also has better defaults.
* (generate/plot)PartialPrediction: added new arg "geom" for tile plots
* small fix for plotBMRSummary
* the ModelMultiplexer inherits its predict.type from the base learners now
* check that learners in an ensemble have the same predict.type
* new function getBMRModels to extract stored models from a benchmark result
* Fixed a bug where several learners from the LiblineaR package
  ("classif.LiblineaRL2LogReg", "classif.LiblineaRL2SVC", "regr.LiblineaRL2L2SVR")
  were calling the wrong value for "type" (0) and thus training the wrong model.
* Fixed a bug where the resampling objects hout, cv2, cv3, cv5, cv10 were not
  documented in the ResampleDesc help page
* regr.xgboost, classif.xgboost: add feval param
* fixed a bug in irace tuning interface with unamed discrete values
* Fixed bugs in "jackknife" and "bootstrap" se estimators for regr.randomForest.
* Added "sd" estimator for regr.randomForest.
* Fixed a mini bug in ModelMultiplexer where hyperpars that are only needed in
  predict were not passed down correctly
* Fixed a bug where the function capLargeValues wasn't working if you passed a
  task.
* capLargeValues now has a new argument "target", to prevent from capping response
  values.
* classif.gbm, regr.gbm: Updated possible 'distribution' settings a bit.
* oversample, undersample, makeOversampleWrapper, makeUndersampleWrapper,
  makeOverBaggingWrapper:
  Added arguments to specifically select the sampled class.

## API changes
* listLearners now returns a data frame with properties of the learners if
  create is false

## new functions
* getBMRModels

## removed functions
* generateROCRCurvesData, plotROCRCurves, plotROCRCurvesGGVIS

## new learners
* classif.randomForestSRCSyn
* classif.cvglmnet
* regr.randomForestSRCSyn
* cluster.dbscan

## new measures
* rsq, arsq, expvar

# mlr 2.7:
* New argument "models" for function benchmark
* fixed a bug where 'keep.pred' was ignored in the benchmark function
* some of the very new functions for benchmark plots had to be refactored and/or
  renamed.
  these names are gone from the API:
  plotBenchmarkResult, generateRankMatrixAsBarData, plotRankMatrixAsBar, generateBenchmarkSummaryData, plotBenchmarkSummary,
  this is the new API:
  plotBMRSummary, plotBMRBoxplots, plotBMRRanksAsBarChart

# mlr_2.6:
* cluster.kmeans: added support for fuzzy clustering (property "prob")
* regr.lm: removed some erroneous param settings
* regr.glmnet: added 'family' param and allowed 'gaussian', but also 'poisson'
* disabled plotViperCharts unit tests as VC seems to be offline currently
* multilabel: improve few task getter functions, especially getTaskFormula is
  now correct

## new learners
* regr.glmboost
* cluster.Cobweb

# mlr 2.5:
* fixed a bug that caused performance() to return incorrect values with
  ResamplePredictions
* we have (somewhat experimental) support for multilabel classification.
  so we now have a task, a new baselearner (rFerns),
  and a generic reduction-to-binary algorithm (MultilabelWrapper)
* tuning: added 'budget' parameter in makeTuneControl* (single-objective)
  and makeTuneMultiCritControl* (multi-objective scenarios), allowing to define
  a maximum "number of evaluations" budget for tuning algorithms
* tuning: added 'budget' parameter in makeTuneMultiCritControl*, allowing to
  define a maximum "number of evaluations" budget for tuning algorithms
  in the single-objective case
* makeTuneControlGenSA: optimized function will be considered non-smooth
  per default (change via ... args)
* classif.svm, regr.svm: added 'scale' param
* ksvm: added 'cache' param
* plotFilterValuesGGVIS: sort and n_show are interactive, interactive flag removed
* renamed getProbabilities to getPredictionProbabilities and deprecated
  getProbabilities
* plots now use long names for measures where possible
* there was a nasty bug in measure "mcc". fixed and unit tested. and apologies.
* removed getTaskFormulaAsString and improved getTaskFormula so the former is
  not needed anymore
* aggregations now have a 'name' property, which is a long name
* generateLearningCurveData and generateThreshVsPerfData now append the
  aggregation id to the output column name if the measure ids are the same
* plotLearningCurve, plotLearningCurveGGVIS, plotThreshVsPerf,
  plotThreshVsPerfGGVIS now have an argument
  'pretty.names' which plots the 'name' element of the measures instead of the 'id'.
* makeCustomResampledMeasure now has arguments 'measure.id' and 'aggregation.id'
  instead of only 'id' which corresponded to the measure. Also, 'name' and note (corresponding to the measure)
  as well as 'aggregation.name' have been added.
* makeCostMeasure now has arguments 'name' and 'id'.
* classification learner now can have a property 'class.weights', supported by
  'class.weights.param'. The latter indicates which of the parameters provides
  that class weights information to the learner.
* class weights integrated in the learner will be used as default for 'wcw.param'
  in 'makeWeightedClassesWrapper'
* listLearners with create = FALSE does not load packages anymore and is
  therefore faster and more reliable; it also supports the additional parameter
  check.packages now that will check whether required packages are installed
  without loading them
* many new functions for statistical benchmark comparisons are added, see below
* rename hasProperties, getProperties to hasLearnerProperties and
  getLearnerProperties
* Learner properties are now implemented object oriented as a state of a Learner.
  Only RLearners have the properties stored in a slot.
  For each class the getter can be overwritten.
* The hill climbing algorithm for stacking (Caruana 04) is implemented as method
  'hill.climb' in 'makeStackedLearner' to select models from base learners, which
  is equivalent to weighted average.
* The model compression algorithm for stacking (Caruana 06) is implemented as
  method 'compress' in 'makeStackedLearner' to first select models from base
  learners and then mimic the behaviour with a super learner. The default super
  learner is neural network.
* relativeOverfitting provides a way to estimate how much a model overfits to
  the training data according to a measure.
* restructured the LiblineaR learners to a more convenient format. These old ones
  were removed:
  classif.LiblineaRBinary, classif.LiblineaRLogReg,  classif.LiblineaRMultiClass.
  For the new ones, see below.
* Added some commonly used ResampleDesc description objects, to save typing in
  resample experiments:
  hout, cv2, cv3, cv5, cv10.
* regr.randomForest: changed default nodesize to 5 (according to randomForest
  defaults)

## new functions
* getDefaultMeasure
* getTaskClassLevels
* getPredictionTruth, getPredictionResponse, getPredictionSE
* convertMLBenchObjToTask
* getBMRLearners, getBMRMeasures, getBMRMeasureIds
* makeMultilabelTask, makeMultilabelWrapper, getMultilabelBinaryPerformances
* generatePartialPredictionData, plotPartialPrediction, and
  plotPartialPredictionGGVIS
* getClassWeightParam
* plotBenchmarkResult, convertBMRToRankMatrix, generateRankMatrixAsBarData,
  plotRankMatrixAsBar, generateBenchmarkSummaryData, plotBenchmarkSummary,
  friedmanTestBMR, friedmanPostHocTestBMR, generateCritDifferencesData,
  plotCritDifferences
* getCaretParamSet
* generateCalibrationData and plotCalibration
* relativeOverfitting
* plotROCCurves

## new measures
* hamloss

## new learners
* multilabel.rFerns
* classif.avNNet
* classif.neuralnet
* regr.avNNet
* classif.clusterSVM
* classif.dcSVM
* classif.gaterSVM
* classif.mlp
* classif.saeDNN
* classif.dbnDNN
* classif.nnTrain
* classif.rknn
* regr.rknn
* classif.xgboost
* regr.xgboost
* classif.rotationForest
* classif.LiblineaRL1L2SVC
* classif.LiblineaRL1LogReg
* classif.LiblineaRL2L1SVC
* classif.LiblineaRL2LogReg
* classif.LiblineaRL1LMultiClassSVC
* regr.LiblineaRL2L1SVR
* regr.LiblineaRL2L2SVR
* classif.ranger
* regr.ranger
* surv.ranger

## new filters
* permutation.importance

## removed functions
* setProperties, addProperties, removeProperties

# mlr 2.4:
* WrappedModel printer was slightly improved
* ReampleResult now stores the runtime it took to resample in a slot
* getTaskFormula / getTaskFormulaAsString have new argument 'explicit.features'
* getTaskData now has recodeY = "drop.levels" which drops empty factor levels
* option fix.factors in makeLearner was renamed to fix.factors.prediction for
  clarity
* showHyperPars was removed. getParamSet does exactly the same thing
* 'resample' and 'benchmark' got the argument keep.pred,
  setting it to FALSE allows to discard the prediction objects to save memory
* we had to slightly change how the mem usage is reported in tuning and feature
  selection
  See TuneControl and FeatSelControl where it is documented what is done now.
* tuneIrace: allows to set the precision / digits within irace (using the argument
  'digits' in makeTuneControlIrace); default is maximum precision
* for plotting in general we try to introduce a "data layer", so the data can be
  generated independently of the plotting first, into well-defined objects;
  these can then be plotted with mlr or custom code;
  the naming scheme is always generate<Foo>Data and plot<Foo>
* getFilterValues is deprecated in favor of generateFilterValuesData
* plotFilterValues can now plot multiple filter methods using facetting
* plotROCRCurves has been rewritten to use ggplot2
* classif.ada: added "loss" hyperpar
* add missings properties to all ctree and cforest methods:
  regr/classif for ctree, regr/classif/surv for cforest, and regr/classif for blackboost
* learner xgboost was removed, because the package is not on CRAN anymore,
  unfortunately
* reg.km: added param 'iso'
* classif.mda: added param 'start.method' and changed its default to 'lvq', added
  params 'sub.df', 'tot.df' and 'criterion'
* classif.randomForest: 'sampsize' can now be an int vector (instead of a scalar)
* plotThreshVsPerf and plotLearningCurve now have param 'facet'

## new functions
* getTaskSize
* getNestedTuneResultsX, getNestedTuneResultsOptPathDf
* tuneDesign
* generateROCRCurvesData, generateFilterValuesData, generateLearningCurveData,
  plotLearningCurve, generateThreshVsPerfData, plotThreshVsPerf,
* generateThreshVsPerfData accepts Prediction, ResampleResult, lists of
  ResampleResult, and BenchmarkResult objects.
* experimental ggvis functions: plotROCRCurvesGGVIS, plotLearningCurveGGVIS,
  plotTuneMultiCritResultGGVIS, plotThreshVsPerfGGVIS, and plotFilterValuesGGVIS

## new learners:
* classif.bst
* classif.hdrda
* classif.nodeHarvest
* classif.pamr
* classif.rFerns
* classif.sparseLDA
* regr.bst
* regr.frbs
* regr.nodeHarvest
* regr.slim

## new measures:
* brier

# mlr 2.3:
* resample now returns an object of class ResampleResult (downward compatible)
  to allow for a print method.
* resampling on features now supported for an arbitrary number of factor features
* mlr supports ViperCharts plots now
* ROC plot via ROCR can now be created automatically, before you had to call
  asROCRPrediction,
  then construct the plots via ROCR your self. See plotROCRCurves
* all mlr measures now have slots "name" and "note"
* exported a few very simple "getters" for tasks, see below
* in makeLearner a probability predict.threshold can be set for classifiers, also
  see setPredictThreshold
* in the control objects for tuning and feature selection, the user can now enable
  threshold tuning
* in the control objects for tuning and feature selection, the user can now define
  his own logging function
* default console logging for tuneParams and selectFeatures is more informative,
  it displays time and memory info
* updated some properties of some learners
* Default arguments of classif.bartMachine, classif.randomForestSRC,
  regr.randomForestSRC and sur.randomForestSRC
  have been changed to allow missing data support with default settings.
* externalized measure functions to be used on vectors.
* some minor bug fixes
* required basic learner packages are not loaded into the global namespace
  anymore, requireNamespace
  is used internally instead. this ensures less name clashes and name shadowing
* resample passes dot arguments to the learner hyperpars
* new option "on.par.out.of.bounds" to disable out-of-bound checks for model
  parameters
* measures were slightly internally changed. they expose more properties (check
  ?Measure) and some now unnecessary object slots were removed
* classif.lda and classif.qda now have hyperpar "predict.method"
* filterFeatures and makeFilterWrapper gain an argument for mandatory features
* plotLearnerPrediction has new option "err.size"
* classif.plsDA and cluster.DBscan for now removed because of problems with the
  underlying learning algorithm
* new aggregation test.join
* the following models now can handle factors and ordereds by extra dummy or int
  encoding:
  classif.glmnet, regr.glmnet, surv.glmnet, surv.cvglmnet, surv.penalized,
  surv.optimCoxBoostPenalty, surv.glmboost, surv.CoxBoost

## new functions
* getTaskType, getTaskId, getTaskTargetNames
* plotROCRCurves
* plotViperCharts
* measureSSE, measureMSE, measureRMSE, measureMEDSE, ...
* PreprocWrapperCaret
* setPredictThreshold

## new learners:
* classif.bdk
* classif.binomial
* classif.extraTrees
* classif.probit
* classif.xgboost
* classif.xyf
* regr.bartMachine
* regr.bcart
* regr.bdk
* regr.bgp
* regr.bgpllm
* regr.blm
* regr.brnn
* regr.btgp
* regr.btgpllm
* regr.btlm
* regr.cubist
* regr.elmNN
* regr.extraTrees
* regr.laGP
* regr.xgboost
* regr.xyf
* surv.rpart

# mlr 2.2:
* The web tutorial was MUCH improved!
* more example tasks and data sets
* Learners and tasks now support ordered factors as features.
  The task description knows whether ordered factors are present and it is checked
  whether the learner supports such a feature. We have set this property 'ordered'
  very conservatively, so very few learners have it, where we are sure ordered
  inputs are handled correctly during training.
  If you know of more models that support this, please inform us.
* basic R learners now have new slots: name (a descriptive name of the algorithm),
  short.name (abbreviation that can be used in plots and tables) and note
  (notes regarding slight changes for the mlr integration of the learner and such).
* makeLearner now supports some options regarding learner error handling and
  output which could before only be set globally via configureMlr
* Additional arguments for imputation functions to allow a more fine-grain
  control of dummy column creation
* imputeMin and imputeMax now subtract or add a multiple of the range of
  the data from the minimum or to the maximum, respectively.
* cluster methods now have property 'prob' when they support fuzzy cluster
  membership probabilities,
  and also then support predict.type = 'prob'. Everything basically works the same
  as for posterior probabilities in classif.* methods.
* predict preserves the rownames of the input in its output
* fixed a bug in createDummyFeatures that caused an error when the data contained
  missing values.
* plotLearnerPrediction works for clustering and allows greyscale plots (for
  printing or articles)
* the whole object-oriented structure behind feature filtering was much
  improved. Smaller changes in the signature of makeFilterWrapper and
  filterFeatures have become necessary.
* fixed a bug in filter methods of the FSelector package that caused an error when
  variable names contained accented letters
* filterFeatures can now be also applied to the result of getFilterValues
* We dropped the data.frame version of some preprocessing operations like
  mergeFactorLevelsBySize,
  joinClassLevels and removeConstantFeatures for consistency. These now always require tasks as input.
* We support a pretty generic framework for stacking / super-learning now, see
  makeStackedLearner
* imbalancy correction + smote:
  ** fix a bug in "smote" when only factor features are present
  ** change to oversampling: sample new observations only (with replacement)
  ** extension to smote algorithm (sampling): minority class observations in
  binary classification
  are either chosen via sampling or alternatively, each minority class observation
  is used an equal number of times
* made the getters for BenchmarkResult more consistent. These are now:
  getBMRTaskIds, getBMRLearnerIds, getBMRPredictions, getBMRPerformances,
  getBMRAggrPerformances
  getBMRTuneResults, getFeatSelResults, getBMRFilteredFeatures
  The following methods do not work for BenchmarkResult anymore: getTuneResult, getFeatSelResult
* Removed getFilterResult, because it does the same as getFilteredFeatures

## new learners:
* classif.bartMachine
* classif.lqa
* classif.randomForestSRC
* classif.sda
* regr.ctree
* regr.plsr
* regr.randomForestSRC
* cluster.cmeans
* cluster.DBScan
* cluster.kmeans
* cluster.FarthestFirst
* surv.cvglmnet
* surv.optimCoxBoostPenalty

## new filters:
* variance
* univariate
* carscore
* rf.importance, rf.min.depth
* anova.test, kruskal.test
* mrmr

## new functions
* makeMulticlassWrapper
* makeStackedLearner, getStackedBaseLearnerPredictions
* joinClassLevels
* summarizeColumns, summarizeLevels
* capLargeValues, mergeFactorLevelsBySize

# mlr 2.1:
* mlr now supports multi-criteria tuning
* mlr now supports cluster analysis (experimental)
* improve makeWeightedClassesWrapper: Hyperparams for class weighting are now
  supported, too.
* removed fix.factors option from randomForest, but added it in general to
  makeLearner, so it now works for all learners.
  Helps when feature factor levels where dropped in newdata prediction data.frames
* more consistent results for tuning algorithms and parameters with "trafos" :
  we always return the optimal settings on the transformed scale, but in the opt.path in the original scale.
* fix a bug when feature filtering resulted in a NoFeatureModel
* resample now returns a data.frame "err.mgs" or error messages that might have
  occurred during resampling
* stratified resampling for survival

## new learners:
* classif.cforest
* classif.glmnet
* classif.plsdaCaret
* regr.cforest
* regr.glmnet
* regr.svm
* surv.cforest
* cluster.SimpleKMeans
* cluster.EM
* cluster.XMeans

## new measures
* bac
* db, dunn, g1, g2, silhouette

## new functions
* makeClusterTask
* removeHyperPars
* tuneParamsMultiCrit
* makeTuneMultiCritControlGrid, makeTuneMultiCritControlRandom,
  makeTuneMultiCritControlNSGA2
* plotTuneMultiCritResult
* getFailureModelMsg

# mlr 2.0:
* mlr now supports survival analysis models (experimental)
* mlr now supports cost-sensitive learning with example-specific costs
  experimental)
* Some example tasks and data sets were added for simple access
* added FeatSelWrapper and getFeatSelResult
* performance functions now allows to compute multiple measures
* added multiclass.roc performance measure
* observation weights can now also be specified in the task
* added option on.learner.warning to configureMlr to suppress warnings in learners
* fixed a bug in stratified CV where elements where not distributed as evenly as
  possible when the split number did not divide the number of observation
* added class.weights param for classif.svm
* add fix.factors.prediction option to randomForest
* generic standard error estimation in randomForest and BaggingWrapper
* added fixup.data option to task constructors, so basic data cleanup can be
  performed
* show.info is now an option in configureMlr
* learners now support taggable properties that can be queried and changed. also
  see below.
* listLearners(forTask) was unified
* removed tuning via R' optim method (makeTuneControlOptim), as the optimizers in
  there really make no sense for tuning
* Grid search was improved so one does not have to discretize parameters manually
  anymore (although this is still possible). Instead one now passes a 'resolution' argument. Internally we
  now use ParamHelpers::generateGridDesign for this.
* toy tasks were added for convenient usage: iris.task, sonar.task, bh.task
  they also also have corresponding resampling instances, so you directly start
  working, e.g., iris.rin

## new learners:
* classif.knn
* classif.IBk
* classif.LiblineaRBinary
* classif.LiblineaRLogReg
* classif.LiblineaRMultiClass
* classif.linDA
* classif.plr
* classif.plsDA
* classif.rrlda
* regr.crs
* regr.IBk
* regr.mob
* surv.CoxBoost
* surv.coxph
* surv.glmboost
* surv.glmnet
* surv.penalized
* surv.randomForestSRC

## new measures
* multiauc
* cindex
* meancosts, mcp

## new functions
* removeConstantFeatures, normalizeFeatures, dropFeatures, createDummyFeatures
* getTaskNFeats
* hasProperties, getProperties, setProperties, addProperties, removeProperties
* showHyperPars
* setId
* listMeasures
* isFailureModel
* plotLearnerPrediction
* plotThreshVsPerf
* holdout, subsample, crossval, repcv, bootstrapOOB, bootstrapB632,
  bootstrapB632plus
* listFilterMethods, getFilterValues, filterFeatures, makeFilterWrapper,
  plotFilterValues
* benchmark
* getPerformances, getAggrPerformances, getPredictions, getFilterResult,
  getTuneResult, getFeatSelResult
* oversample, undersample, makeOversampleWrapper, makeUndersampleWrapper
* smote, makeSmoteWrapper
* downsample, makeDownsampleWrapper
* makeWeightedClassesWrapper
* makeTuneControlGenSA
* makeModelMultiplexer, makeModelMultiplexerParamSet
* makeCostSensTask, makeCostSensClassifWrapper, makeCostSensRegrWrapper,
  makeCostsSensWeightedPairsLearner
* makeSurvTask
* impute, reimpute, makeImputeWrapper, lots of impute<Method>, makeImputeMethod

# mlr 1.1:
* Initial release to CRAN




