setMlrOption = function(name, val) {
  name = sprintf("mlr.%s", name)
  do.call(options, setNames(list(val), name))
}

getMlrOption = function(name, default) {
  name = sprintf("mlr.%s", name)
  getOption(name, default)
}

exportMlrOptions = function() {

}
