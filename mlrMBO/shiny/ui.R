library(shiny)
library(soobench)

funs = list("Branin function" = "branin",
		    "Ackley function 1d" = "ackley_1",
		    "Ackley function 2d" = "ackley_2",
			"Rastrigin function 1d" = "rastrigin_1",
			"Rastrigin function 2d" = "rastrigin_2",
			"Rosenbrock function 2d" = "rosenbrock_2")

trafos = list("none" = "none", "log" = "log", "log10" = "log10")


shinyUI(pageWithSidebar(
	# Application title
	headerPanel("mlrMBO demo"),
	sidebarPanel(
		h3("Options"),
		p("Below you have the possibility to select one of the benchmarking function implemented in the current release of soobench R package by Olaf Mersmann."),
		selectInput("obj.fun", "Objective function:", funs, "Ackley function 1d"),

		sliderInput("iter", "Iterations:",
			min=1, max=10, value=2),
		
		p("Below you have the possibility to select transformations for different plots."),
		em("(Irrelevant trafos are not considered."),

		selectInput("trafo.y", 
			"y-Trafo:",
			trafos),
		selectInput("trafo.yhat", 
			"yhat-Trafo:",
			trafos),
		selectInput("trafo.crit", 
			"Criterion-Trafo:",
			trafos),
		selectInput("trafo.se", 
			"SE-Trafo:",
			trafos),
		p(em("Transformations currently applied only to 2D functions."))
	),
	mainPanel(
		h3(textOutput("obj.fun.name")),
		p("Be patient. This may take a while; especially in the case of", strong("2D functions"), "."),
		plotOutput("obj.fun.plot")
	)
))