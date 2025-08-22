# In the following we have the functions
# - getReampleExtract2: Returns a feasible function we can use as extract in resample() for each specific learner class.
#   All is handled recursively because we have wrapped learners.
#   getResampleExtract2 returns a list of all applicable functions
#
# - getResampleExtract converts the list of functions obtained by getResampleExtract2 to a single function which returns a list of each function result.

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

getResampleExtract = function(learner) {
  functions = getResampleExtract2(learner)
  function(x) {
    if (length(functions) == 1L) {
      functions[[1L]](x)
    } else {
      lapply(functions, function(fun) fun(x))
    }
  }
}
