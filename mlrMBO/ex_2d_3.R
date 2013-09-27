##### optimizing a 2d function with both factor variable and numeric variable with mbo / EI #####

library(methods)
library(testthat)
library(devtools)
library(BBmisc)
library(mlr)
library(soobench)

load_all("skel", reset=TRUE)

configureMlr(show.learner.output=FALSE)

objfun = function(x) {
  if (x$foo == "a")
    sum(x$x^2)
  else if (x$foo == "b")
    sum(x$x^2) + 10
  else
    sum(x$x^2) - 10
}

ps = makeParamSet(
    makeDiscreteParam("foo", values = c("a", "b", "c")),
    makeNumericVectorParam("x", len=1, lower=-2, upper=2)
  )

lrn = makeLearner("regr.randomForest")
ctrl = makeMBOControl(init.design.points=20, iters=5, infill.opt.random.points=100, save.model.at=c(0,5))


ctrl = makeMBOControl(init.design.points=6, iters=10, propose.points=1, 
  infill.crit="ei", infill.opt="random", infill.opt.random.points=500)


run = exampleRun(objfun, ps, global.opt=-10, learner=lrn, control=ctrl, points.per.dim=100)

print(run)

#plot(run, pause=TRUE, densregion=FALSE)




  
