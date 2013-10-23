









trainOperator: input x params --> (output, params) 
predictOperator: input x params --> output 



trainOperator 

setGeneric(
  name = "trainLearner",
  def = function(.learner, .task, .subset,  ...) {
    standardGeneric("trainLearner")
  }
)

setGeneric(
  name = "predictOperator",
  def = function(.learner, .task, .subset,  ...) {
    standardGeneric("trainLearner")
  }
)
