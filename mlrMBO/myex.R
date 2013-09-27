##### optimizing a simple sin(x) with mbo / EI #####

library(methods)
library(testthat)
library(devtools)
library(BBmisc)
library(mlr)
library(soobench)
library(ggplot2)
library(gridExtra)

load_all("skel", reset=TRUE)

configureMlr(show.learner.output=FALSE)

objfun = function(x) {
  sin(x$x)
}

ps = makeNumericParamSet(lower=3, upper=13, len=1)

ctrl = makeMBOControl(init.design.points=6, iters=10, propose.points=1, 
  infill.crit="ei", infill.opt="random", infill.opt.random.points=500)

lrn = makeLearner("regr.km", predict.type="se", covtype="matern3_2")

run = exampleRun(objfun, ps, global.opt=-1, learner=lrn, 
  control=ctrl, points.per.dim=100)

print(run)

autoplot.MBOexampleRun(run, iters=3, densregion=TRUE)
