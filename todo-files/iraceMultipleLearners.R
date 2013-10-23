library("devtools")
library("testthat")
load_all("skel")


iraceMultipleLearners = function(task, learners, par.sets, resampling, measures, control, show.info = TRUE) {
  lrn.ids = extractSubList(learners, "id")
  names(par.sets) = lrn.ids
  lrn.par.set = makeParamSet(makeDiscreteParam("lrn", values = lrn.ids))
  global.par.set = c(lrn.par.set, Reduce(c, par.sets))
  irace.params = makeParamSet(makeDiscreteParam("lrn", values = lrn.ids))
  irace.params = convertParamSetToIrace(irace.params, as.chars = TRUE)
  k = length(learners)
  if (missing(measures))
    measures = mlr:::default.measures(task)
  if (is(measures, "Measure"))
    measures = list(measures)   
  
  for (i in 1:k) {
    s = convertParamSetToIrace(par.sets[[i]], as.chars=TRUE)
    condition = sprintf(" | lrn == '%s'", lrn.ids[i])
    s = paste(s, condition, sep="")
    irace.params = c(irace.params, s)
  }
  opt.paths = setNames(lapply(lrn.ids, function(x) makeOptPathDFFromMeasures(par.sets[[x]], measures)), lrn.ids)
  irace.params = collapse(irace.params, "\n")
  cat(irace.params)
  irace.params = readParameters(text=irace.params)
  hookRun = function(instance, candidate, extra.params=NULL, config=list()) {
    rin = instance
    vals = candidate$values
    vals = vals[!is.na(vals)]
    lrn.id = vals$lrn
    vals$lrn = NULL
    learner = makeLearner(lrn.id) 
    ps = par.sets[[lrn.id]]
    op = opt.paths[[lrn.id]]
    log.fun = function(...) 1
    tunerFitnFun(vals, learner=learner, task=task, resampling=rin, measures=measures, 
                 par.set=ps, ctrl=control, opt.path=op, show.info=show.info, 
                 log.fun=log.fun, trafo=TRUE, convertx=identity) 
  }
  n.instances = control$extra.args$n.instances
  control$extra.args$n.instances = NULL
  control$extra.args$show.irace.output = NULL
  instances = lapply(1:n.instances, function(i) makeResampleInstance(resampling, task=task))
  
  tuner.config = c(list(hookRun=hookRun, instances=instances), control$extra.args)
  
  #capture.output({
    or = irace(
      tunerConfig = tuner.config,
      parameters = irace.params
    )
  #})
}

lrn1 = makeLearner("classif.rpart")
ps1 = makeParamSet(
  makeNumericParam("cp", lower=0.001, upper=1), 
  makeIntegerParam("minsplit", lower=1, upper=10)
)

lrn2 = makeLearner("classif.ksvm")
ps2 = makeParamSet(
  makeNumericParam("C", lower=0.1, upper=10), 
  makeNumericParam("sigma", lower=0.1, upper=10)
)

lrns = list(lrn1, lrn2)
pss = list(ps1, ps2)


task = makeClassifTask(data=iris, target="Species")
rdesc = makeResampleDesc("Holdout")

  
ctrl = makeTuneControlIrace(maxExperiments = 1000)
iraceMultipleLearners(task, lrns, pss, rdesc, mmce, ctrl)

  