##### optimizing branin in 2D with multipoint proposal #####

library(methods)
library(testthat)
library(devtools)
library(BBmisc)
library(mlr)
library(soobench)

load_all("skel", reset=TRUE)

configureMlr(show.learner.output=FALSE)

objfun = branin_function()

ctrl = makeMBOControl(init.design.points=10, iters=10, propose.points=5, 
  multipoint.method="multicrit")

# FIXME: Yeah this is crap, interface not done...
ctrl$multipoint.objective = "ei"
ctrl$multipoint.distfun = "nearest.neighbor"
ctrl$multipoint.multicrit.maxit = 200

lrn = makeLearner("regr.km", predict.type="se", covtype="matern3_2")

run = exampleRun(objfun, learner=lrn, control=ctrl, points.per.dim=50)

print(run)

plot(run, pause=TRUE)
