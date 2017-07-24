library(devtools)
load_all()

configureMlr(show.info = TRUE)

mm =  makeMeasure("tpr_ppv", minimize = FALSE, properties = c("classif"),
  fun = function(task, model, pred, feats, extra.args) {
    ppv.val = ppv$fun(pred = pred)
    tpr.val = tpr$fun(pred = pred)
    ifelse(ppv.val > 0.8, tpr.val, ppv.val - 1)
  }
)


task = sonar.task
lrn = makeLearner("classif.randomForest", predict.type = "prob")
ps = makeParamSet(
  makeIntegerParam("mtry", lower = 2, upper = 10)
)
ctrl = makeTuneControlRandom(maxit = 3, tune.threshold = TRUE)
tr = tuneParams(lrn, task, hout, measures = list(mm, mmce, ppv, tpr), par.set = ps, control = ctrl)
print(tr)
op = as.data.frame(tr$opt.path)
