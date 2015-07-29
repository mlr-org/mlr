getResampleExtract2 = function(learner) {
  UseMethod("getResampleExtract2")
}

getResampleExtract2.NULL = function(learner) {
  stopf("Wrapper without underlying Learner.")
}

getResampleExtract2.Learner = function(learner) {
  NULL
}

getResampleExtract2.BaseWrapper = function(learner) {
  getResampleExtract2(learner$next.learner)
}

getResampleExtract2.TuneWrapper = function(learner) {
  c(list(TuneResult = getTuneResult), getResampleExtract2(learner$next.learner))
}

getResampleExtract2.FeatSelWrapper = function(learner) {
  c(list(FeatSelResult = getFeatSelResult), getResampleExtract2(learner$next.learner))
}

getResampleExtract2.FilterWrapper = function(learner) {
  c(list(FilteredFeatures = getFilteredFeatures), getResampleExtract2(learner$next.learner))
}

getResampleExtract = function(learner){
  functions = getResampleExtract2(learner)
  force(functions)
  function(x) {
    lapply(functions, function(fun) fun(x))
  }
}