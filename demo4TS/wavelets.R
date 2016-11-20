library(wavelets)
load_all()
configureMlr(show.info = TRUE)

# load gunpoint data
gp = load2("gunpoint.RData")

gp.x = gp[,-1]
gp.y = gp[,1]

generateWaveletData = function(curves) {
  wtdata = NULL
  for (i in seq_row(curves)) {
    a = t(curves[i,])
    wt = dwt(a, filter = "haar", boundary = "periodic")
    wtdata = rbind(wtdata, unlist(c(wt@W,wt@V[[wt@level]])))
  }
  wtdata = as.data.frame(wtdata)
  return(wtdata)
}

gpw.x = generateWaveletData(gp.x)
gpw = cbind(y = gp.y, gpw.x)
task = makeTimeSeriesClassifTask(data = gpw, target = "y")
rin = makeFixedHoldoutInstance(size = 200, train.inds = 1:50, test.inds = 51:200)

lrn = makeLearner("classif.ksvm", kernel = "rbfdot")
ctrl = makeTuneControlRandom(maxit = 200)
ps = makeParamSet(
  makeNumericParam("C", lower = -12, upper = 12, trafo = function(x) 2^x),
  makeNumericParam("sigma", lower = -12, upper = 12, trafo = function(x) 2^x)
)
tr = tuneParams(lrn, task, rin, par.set = ps, control = ctrl)

