##### optimizing branin in 2D with multipoint proposal #####

library(methods)
library(testthat)
library(devtools)
library(BBmisc)
library(mlr)
library(soobench)

load_all("skel", reset=TRUE)

configureMlr(show.learner.output=FALSE)

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

plot(run, pause=TRUE)