set.seed(2)
load_all()
configureMlr(show.info = TRUE, show.learner.output= TRUE)
library('BBmisc')
library('wavelets')


createFIRFeatures = function(df_Ts, target, boundary = "periodic", psf = c(1/sqrt(2),1/sqrt(2))) {
  df_Ts = df_Ts[, setdiff(colnames(df_Ts), target)]
  kl = length(psf)
  wtdata = NULL
  
  for (i in seq_row(df_Ts)) {
    
  }
  wtdata = as.data.frame(wtdata)
  return(wtdata)
  
}


createDWTFeature = function(curves) {
  wtdata = NULL
  for (i in seq_row(curves)) {
    a = t(curves[i,])
    wt = dwt(a, filter = "haar", boundary = "periodic")
    wtdata = rbind(wtdata, unlist(c(wt@W,wt@V[[wt@level]])))
  }
  wtdata = as.data.frame(wtdata)
  return(wtdata)
}

library('dtw')

creatDTWFeature = function (curves, ref = c(1,2) ){
  wtdata = NULL
  for (i in seq_row(curves)) {
    a = t(curves[i,])
    wt = lapply(ref, function(r) dtw(a,curves[r,], step.pattern = asymmetric, keep =TRUE)$distance)
    #wt = lapply(ref, function(r) curves[r,])
    wtdata = rbind(wtdata, wt)
  }
  wtdata = as.data.frame(wtdata)
  return(wtdata)
}


gp = load2("gunpoint.RData")
gp.x = gp[,-1]
gp.y = gp[,1]
gpw.x = createDWTFeature(gp.x)
gpw = cbind(y = gp.y, gpw.x)
gpw$y = as.factor(gpw$y)

task = makeTimeSeriesClassifTask(data = gpw, target = "y", positive = "1", check.data = FALSE)

lrn = makeLearner("classif.rpart")


ptrain = function(data, target, args) {
    control = list(fun=createDWTFeature)
    d = createDWTFeature(data)
    xxx <<- d
    list(data = d, control = control)
    #Preprocessing train must result in list wil elements data[data.frame] and control[list]!
 }

ppredict = function(data, target, args, control) {
   y = intersect(target, colnames(data))  
   data = do.call(control$fun, c(list(curves = data), args))
   return(data)
 }

library('stringi')
lrn2 = makePreprocWrapper(lrn, train = ptrain, predict = ppredict, par.vals = list(positive ="1"))
lrn2$id = stri_replace(lrn$id, replacement = ".wavelet", regex = "\\.preproc$")
lrn2 = addClasses(lrn2, "WaveletFeaturesWrapper")


model = train(lrn2, task, subset = 1:150) # error: "Assertion on 'positive' failed: Must be element of set {''}" # assertChoice(positive, choices = levs) at ClassifTask.R#43
pred = predict(model, task, subset = 151:200)
p = performance(pred, measures = list(mmce, tpr))
print(p)
