
task = gunpoint.task
# lets tune the SVM with a random search
library(parallelMap)
parallelStartMulticore(cpus = 4, level = "mlr.tuneParams")
lrn = makeLearner("classif.ksvm", kernel = "rbfdot")
ctrl = makeTuneControlRandom(maxit = 200)
ps = makeParamSet(
  makeNumericParam("C", lower = -12, upper = 12, trafo = function(x) 2^x),
  makeNumericParam("sigma", lower = -12, upper = 12, trafo = function(x) 2^x)
)
tr = tuneParams(lrn, task, rin, par.set = ps, control = ctrl)
opd = as.data.frame(tr$opt.path)
op2 = trafoOptPath(tr$opt.path)
opd2 = as.data.frame(op2)
parallelStop()
print(tr)

# lets tune the SVM, but use nested resampling
# all iteration numbers are set to bullshit low values so stuff runs fast
outer = cv3
lrn = makeLearner("classif.ksvm", kernel = "rbfdot")
ctrl = makeTuneControlRandom(maxit = 3)
ps = makeParamSet(
  makeNumericParam("C", lower = -12, upper = 12, trafo = function(x) 2^x),
  makeNumericParam("sigma", lower = -12, upper = 12, trafo = function(x) 2^x)
)
inner = cv2 # shortcut to create resample description, long way is in next line
# inner = makeResampleDesc("CV", iters = 2)
lrn2 = makeTuneWrapper(lrn, inner, par.set = ps, control = ctrl)
r = resample(lrn2, task, resampling = outer, extract = getTuneResult)
print(r)
print(r$extract) # this allows access to ALL tuning data, it is simply a list of TuneResult objects

