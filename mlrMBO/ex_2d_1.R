##### optimizing branin in 2D with mbo / EI #####

library(methods)
library(testthat)
library(devtools)
library(BBmisc)
library(mlr)
library(soobench)

load_all("skel", reset=TRUE)

configureMlr(show.learner.output=FALSE)

objfun = branin_function()

ctrl = makeMBOControl(init.design.points=10, iters=10, propose.points=1, 
  infill.crit="ei", infill.opt="random", infill.opt.random.points=2000)

lrn = makeLearner("regr.km", predict.type="se", covtype="matern3_2")

run = exampleRun(objfun, learner=lrn, control=ctrl, points.per.dim=50)

print(run)

plot(run, pause=TRUE)
