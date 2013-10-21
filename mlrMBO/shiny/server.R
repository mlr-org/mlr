library(shiny)
library(grid)
library(gridExtra)
library(soobench)
library(mlrMBO)

# FIXME: this is ugly
fun.map = list("branin"   = list("fun" = generate_branin_function),
			   "ackley_1" = list("fun" = generate_ackley_function, "dimension" = 1),
			   "ackley_2" = list("fun" = generate_ackley_function, "dimension" = 2),
			   "rastrigin_1" = list("fun" = generate_rastrigin_function, "dimension" = 1),
			   "rastrigin_2" = list("fun" = generate_rastrigin_function, "dimension" = 2),
			   "rosenbrock_2" = list("fun" = generate_rosenbrock_function, "dimension" = 2))

trafo = function(x) {
	trafo = NULL
	switch(x,
		"none"  = { trafo=NULL },
		"log"   = { trafo=mlrMBO:::logTrafo() },
		"log10" = { trafo=mlrMBO:::log10Trafo() })
	return(trafo)
}

# all the shiny magic
shinyServer(function(input, output) {
	# server logic comes here

	# return objective function we wish to optimize
	fun = reactive({
		name.fun = input$obj.fun
		fun.spec = fun.map[[name.fun]]
		if (!"dimension" %in% names(fun.spec)) {
			fun.spec$fun()
		} else {
			fun.spec$fun(fun.spec$dimension)
		}
	})

	# returns the objective function name as a string
	objFunName = reactive({
		obj.fun = fun()
    	paste(function_name(obj.fun))
  	})

	# render objective fun
	output$obj.fun.name = renderText({
		objFunName()
	})

	#render plot
	output$obj.fun.plot = renderPlot({
		# get data
		obj.fun = fun()
		iter = input$iter

		# determine desired z-transformation
		trafo.y = trafo(input$trafo.y)
		trafo.yhat = trafo(input$trafo.yhat)
		trafo.crit = trafo(input$trafo.crit)
		trafo.se = trafo(input$trafo.se)

		trafo = list(
			"y" = trafo.y,
			"yhat" = trafo.yhat,
			"crit" = trafo.crit,
			"se" = trafo.se)

		# build file name
		hash = function_name(obj.fun)
		file.name = paste(hash, ".Rdata", sep="")

		# load optimization
		# load mbo result object from file
		if (file.exists(file.name)) {
			load(file.name)
		} else {
			# otherwise apply learner and do example run
			ctrl = makeMBOControl(init.design.points=10, iters=10, propose.points=1, 
				infill.crit="ei", infill.opt="random", infill.opt.random.points=2000)

			lrn = makeLearner("regr.km", predict.type="se", covtype="matern3_2")

			run = mlrMBO:::exampleRun(obj.fun, learner=lrn, control=ctrl, points.per.dim=50)
			save(file=file.name, list=c("run"))
		}

		print(str(trafo))
		pl = autoplot(run, iters=iter, pause=FALSE, trafo=trafo)

		pl$pl.all
	})
})