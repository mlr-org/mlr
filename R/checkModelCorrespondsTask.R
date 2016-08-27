# checks if the model was trained on the corresponding task by comparing 
# the descriptions
checkModelCorrespondsTask = function(model, task) {
  if(!identical(task$task.desc, model$task.desc))
    stopf("Description of the model does not correspond to the task")
}
