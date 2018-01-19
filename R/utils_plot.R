# checks for duplicated entries in learner.names and stops
# with error message containing the learner name that appeared more
# than once
checkDuplicatedLearnerNames = function(learner.names) {
  dupl = duplicated(learner.names)
  if (any(dupl)) {
    dupl.learners = unique(learner.names[dupl])
    stopf("Learner short names are not unique for: %s. \n  Set 'pretty.names = FALSE' to resolve this.",
      collapse(dupl.learners))
  }
}
