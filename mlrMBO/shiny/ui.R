library(shiny)
library(soobench)

funs = list("Branin function" = "branin",
		    "Ackley function 1d" = "ackley_1",
		    "Ackley function 2d" = "ackley_2",
			"Discus function 1d" = "discus_1",
			"Discus function 2d" = "discus_2")

shinyUI(pageWithSidebar(
	# Application title
	headerPanel("mlrMBO demo"),
	sidebarPanel(
		h3("Options"),
		p("Below you have the possibility to select one of the benchmarking function implemented in the current release of soobench R package by Olaf Mersmann."),
		selectInput("obj.fun", "Objective function:", funs, "Ackley function 1d"),

		sliderInput("iter", "Iterations:",
			min=1, max=10, value=2),
		
		selectInput("trafo", 
			"Select a transformation method:", 
			list("none" = "none", "log10" = "log10")),
		p(em("Transformations currently applied only to 2D functions."))
	),
	mainPanel(
		h3(textOutput("obj.fun.name")),
		p("Be patient. This may take a while; especially in the case of", strong("2D functions"), "."),
		plotOutput("obj.fun.plot")
	)
))