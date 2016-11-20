load_all()

configureMlr(show.info = TRUE)

# load gunpoint data
gp = load2("gunpoint.RData")


# create our new tsclassif task (yuhu...)
task = makeTimeSeriesClassifTask(data = gp, target = "X1", positive = "1")

# # start with a simple CART tree (its simple and fast)
lrn = makeLearner("classif.rpart")

# # train, predict, measure perf, all manual
# # (and use complete data, so insample eval....)
model = train(lrn, task, subset = 1:50)
pred = predict(model, task, subset = 51:200)
p = performance(pred, measures = list(mmce, tpr))
print(p)

# create the sample GP splits as on UCR page, they used the first 50 for train, last 150 for test
# then resample (this is holdout eval)
rin = makeFixedHoldoutInstance(size = 200, train.inds = 1:50, test.inds = 51:200)
r = resample(lrn, task, rin)
print(r)

# lets benchmark the tree vs an RBF SVM
learners = list(
  makeLearner("classif.rpart"),
  makeLearner("classif.ksvm", C = 1, sigma = 0.01)
)
b = benchmark(learners, task, rin)
print(b)

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



# here are some R examples for feature extraction!
# http://www.rdatamining.com/examples/time-series-clustering-classification

# library(wavelets)
# wtData <- NULL
# for (i in 1:nrow(sc)) {
#   a <- t(sc[i,])
#   wt <- dwt(a, filter=”haar”, boundary=”periodic”)
#   wtData <- rbind(wtData, unlist(c(wt@W,wt@V[[wt@level]])))
# }
#  wtData <- as.data.frame(wtData)

