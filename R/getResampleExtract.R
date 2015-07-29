#In the following we have the functions
# getReampleExtract2: Returns a feasable function we can use as extract in resample() for each specific learner class. All is handled recursively because we have wrapped learners. So getResampleExtract2 builds a list of all appliable functions
#
# getResampleExtract converts the list of functions obtained by getResampleExtract2 to a single function which returns a list of each function result.

getResampleExtract2 = function(learner) {
  UseMethod("getResampleExtract2")
}

getResampleExtract2.default = function(learner) {
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
  function(x) {
    lapply(functions, function(fun) fun(x))
  }
}