set.seed(2)
load_all()
configureMlr(show.info = TRUE, show.learner.output= TRUE)
library('BBmisc')
library('wavelets')
options(warn = 2L)



gp = load2("demo4TS/gunpoint.RData")


task = makeTimeSeriesClassifTask(data = gp, target = "X1", positive = "1", check.data = FALSE)
task.wv = convertTSTaskToNormalTask(task = task, method = "wavelets")

lrn = makeLearner("classif.rpart")


ptrain = function(data, target, args) {
  d = getTSWaveletFeatures(data = data, target = target, include.target = TRUE)
  list(data = d, control = list())
}

ppredict = function(data, target, args, control) {
  getTSWaveletFeatures(data = data, target = target, include.target = FALSE)
}

library('stringi')
lrn2 = makePreprocWrapper(lrn, train = ptrain, predict = ppredict, par.vals = list(positive = "1"))
lrn2$id = stri_replace(lrn$id, replacement = ".wavelet", regex = "\\.preproc$")
lrn2 = addClasses(lrn2, "WaveletFeaturesWrapper")


model = train(lrn2, task, subset = 1:150) # error: "Assertion on 'positive' failed: Must be element of set {''}" # assertChoice(positive, choices = levs) at ClassifTask.R#43
pred = predict(model, task, subset = 151:200)

crossval(lrn2, task)

p = performance(pred, measures = list(mmce, tpr))
print(p)
