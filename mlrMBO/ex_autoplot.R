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

# NUMERIC FUNCTION
# objfun = function(x) {
#   sin(x$x)
# }

# ps = makeNumericParamSet(lower=3, upper=13, len=1)

# ctrl = makeMBOControl(init.design.points=6, iters=10, propose.points=1, 
#   infill.crit="ei", infill.opt="random", infill.opt.random.points=500)

# lrn = makeLearner("regr.km", predict.type="se", covtype="matern3_2")

# run = exampleRun(objfun, ps, global.opt=-1, learner=lrn, 
#   control=ctrl, points.per.dim=100)

# print(run)

# autoplot(run, iters=8, densregion=TRUE)
# stop()

# NOISY DISCRETE FUNCTION
objfun = function(x) {
	if (x$foo == "a") {
		return(5 + rnorm(1))
	} else if (x$foo == "b") {
		return(4 + rnorm(1, sd=0.5))
	} else {
		return(3 + rnorm(1, sd=1))
	}
}

ps = makeParamSet(
	makeDiscreteParam("foo", values = c("a", "b", "c"))
)

ctrl = makeMBOControl(init.design.points=20, iters=5, infill.opt.random.points=100, noisy=TRUE)

run = exampleRun(objfun, par.set = ps, control=ctrl, points.per.dim=50)

print(run)

autoplot(run, iters=3)
stop()

# 2d NUMERIC FUNCTION

# objfun = generate_branin_function()

# ctrl = makeMBOControl(init.design.points=10, iters=10, propose.points=1, 
#   infill.crit="ei", infill.opt="random", infill.opt.random.points=2000)

# lrn = makeLearner("regr.km", predict.type="se", covtype="matern3_2")

# run = exampleRun(objfun, learner=lrn, control=ctrl, points.per.dim=50)

# print(run)

pl = autoplot(run, iters=3, pause=TRUE)