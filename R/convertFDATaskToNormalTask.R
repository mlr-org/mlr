# Helper function to convert an arbitrary FDA Task to a normal Task by changing
# the class and dropping non-fda-task elements
changeFDATaskToNormalTask = function(obj) {
  # Set class for obj to turn it back into a normal class
  # This gets rid of all-FDA Task classes
  obj = setClasses(obj, class(obj)[- grep(pattern = "FDA", class(obj), fixed = TRUE)])

  # change task type and task.desc
  obj$type = stri_replace(obj$type, replacement = "", fixed = "fda")
  obj$task.desc$type = stri_replace(obj$task.desc$type, replacement = "", fixed = "fda")
  obj$task.desc$fd.features = NULL
  obj$task.desc$fd.grids = NULL

  return(obj)
}
