load_all()

set.seed(2)
configureMlr(show.info = TRUE, show.learner.output= TRUE)

gp = load2("gunpoint.RData")

task = makeTimeSeriesClassifTask(data = gp, target = "X1", positive = "1")
task.wd = makeTSFeaturesClassifTask(task = task, method = "wavelets", pars = list(filter = "haar"))
task.ft = makeTSFeaturesClassifTask(task = task, method = "fourier", pars = list(fft.coeff = "amplitude"))

lrn = makeLearner("classif.rpart")



# classification on wavelet features
model = train(lrn, task.wd, subset = 1:50)
pred = predict(model, task.wd, subset = 51:200)
p = performance(pred, measures = list(mmce, tpr))
print(p)

# classification on fourier features
model = train(lrn, task.ft, subset = 1:50)
pred = predict(model, task.ft, subset = 51:200)
p = performance(pred, measures = list(mmce, tpr))
print(p)

# time series as non-temporal data
model = train(lrn, task, subset = 1:50)
pred = predict(model, task, subset = 51:200)
p = performance(pred, measures = list(mmce, tpr))
print(p)

# ptrain = function(data, target, args) {
#   feats = setdiff(colnames(data), target)
#   feats = sample(feats, 2)
#   # print(feats)
#   data = data[, c(feats, target)]
#   control = list(feats = feats)
#   list(data = data, control = control)
# }
#
# ppredict = function(data, target, args, control) {
#   data = data[, control$feats]
#   return(data)
# }
#
# lrn2 = makePreprocWrapper(lrn, train = ptrain, predict = ppredict)
#
# # m = train(lrn2, task)
# # p = predict(m, task)
# r = crossval(lrn2, task)



