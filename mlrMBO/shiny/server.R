library(shiny)
library(grid)
library(gridExtra)
library(soobench)
library(mlrMBO)

# FIXME: this is ugly
fun.map = list("branin"   = list("fun" = generate_branin_function),
			   "ackley_1" = list("fun" = generate_ackley_function, "dimension" = 1),
			   "ackley_2" = list("fun" = generate_ackley_function, "dimension" = 2),
			   "discus_1" = list("fun" = generate_discus_function, "dimension" = 1),
			   "discus_2" = list("fun" = generate_discus_function, "dimension" = 2))

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
		trafo = NULL
		switch(input$trafo,
			"none" = { trafo=NULL },
			"log10" = { trafo=mlrMBO:::logTrafo() })

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

		pl = autoplot(run, iters=iter, pause=FALSE, trafo=trafo)

		pl$pl.all
	})
})