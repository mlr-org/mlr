load_all("~/mlr")
load_all("~/mlrMBO")
configureMlr(show.learner.output = FALSE)

# The learner: SVM with WeightedClassesWrapper
learner = makeWeightedClassesWrapper(makeLearner("classif.svm")) # this is the learner that we want to tune the hyper parameter in a multiCriteria way

# The optimization space: Look at the trafo.
ps = makeParamSet(
  makeNumericParam("cost", lower = -15, upper = 15, trafo = function(x) 2^x),
  makeNumericParam("gamma", lower = -15, upper = 15, trafo = function(x) 2^x),
  makeNumericParam("wcw.weight", lower = -7L, upper = 7L, trafo = function(x) 2^x)
)

# The description of the validation procedure.
rdesc = makeResampleDesc("CV", iters = 2L)
# Tune Control for multi crit
# Here we can set all MBO parameters
mbo.ctrl2 = makeMBOControl(n.objectives = 2L)
mbo.ctrl2 = setMBOControlTermination(mbo.ctrl2, max.evals = 20L, iters = 300L)
mbo.ctrl2 = setMBOControlMultiCrit(mbo.ctrl2, method = "parego")  # class(mbo.ctrl2) = "MBOControl"
ctrl2 = mlr:::makeTuneMultiCritControlMBO(
  learner = makeLearner("regr.km"), # The MBO learner
  mbo.control = mbo.ctrl2, # The MBO control object
  mbo.design = generateDesign(n = 10L, par.set = ps) # The initial LHS
)
# class(ctrl2)  [1] "TuneMultiCritControlRandom" "TuneMultiCritControl"       "OptControl" 
# And do the tuning on the example sonar-dataset.
res2 = tuneParamsMultiCrit(learner, task = sonar.task, resampling = rdesc, par.set = ps,
  measures = list(fpr, fnr), control = ctrl2)
