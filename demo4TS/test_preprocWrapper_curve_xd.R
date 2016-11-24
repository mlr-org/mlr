set.seed(2)
load_all()
configureMlr(show.info = TRUE, show.learner.output= TRUE)
library('BBmisc')
library('wavelets')
options(warn = 2L)

createFIRFeatures = function(df_Ts, target, boundary = "periodic", psf = c(1/sqrt(2),1/sqrt(2))) {
  df_Ts = df_Ts[, setdiff(colnames(df_Ts), target)]
  kl = length(psf)
  wtdata = NULL
  for (i in seq_row(df_Ts)) {
    
  }
  wtdata = as.data.frame(wtdata)
  return(wtdata)
}


createDWTFeature = function(data, target, include.target) {
  cns = colnames(data)
  # potentially extract y-col and remove it from data, we dont need it for fourier-trafo
  if (target %in% cns) {
    y = data[, target]
    data[, target] = NULL
  }
  wtdata = NULL
  # FIXME: probably very inefficient
  for (i in seq_row(data)) {
    a = t(data[i,])
    wt = dwt(a, filter = "haar", boundary = "periodic")
    wtdata = rbind(wtdata, unlist(c(wt@W,wt@V[[wt@level]])))
  }
  wtdata = as.data.frame(wtdata)
  if (include.target)
    wtdata[, target] = y
  return(wtdata)
}

library('dtw')

creatDTWFeature = function (curves, ref = c(1,2) ){
  wtdata = NULL
  for (i in seq_row(curves)) {
    a = t(curves[i,])
    wt = lapply(ref, function(r) dtw(a,curves[r,], step.pattern = asymmetric, keep =TRUE)$distance)
    wtdata = rbind(wtdata, wt)
  }
  wtdata = as.data.frame(wtdata)
  return(wtdata)
}


gp = load2("demo4TS/gunpoint.RData")
gp.x = gp[,-1]
gp.y = gp[,1]
#gpw.x = createDWTFeature(gp.x)
#gpw = cbind(y = gp.y, gpw.x)
#gpw$y = as.factor(gpw$y)

#task = makeTimeSeriesClassifTask(data = gpw, target = "y", positive = "1", check.data = FALSE)

task = makeTimeSeriesClassifTask(data = gp, target = "X1", positive = "1", check.data = FALSE)

lrn = makeLearner("classif.rpart")


ptrain = function(data, target, args) {
  d = createDWTFeature(data = data, target = target, include.target = TRUE)
  list(data = d, control = list())
}

ppredict = function(data, target, args, control) {
  createDWTFeature(data = data, target = target, include.target = FALSE)
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
