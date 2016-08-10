checkModelCorrespondsTask = function(model, task) {
  if(!identical(task$task.desc, model$task.desc))
    stopf("Trained model does not correspond to the task")
}
