getTaskConstructorForLearner = function(learner) {
  while (inherits(learner, "BaseWrapper")) {
    learner = learner$next.learner
  }
  cl = class(learner)

  if ("RLearnerRegr" %in% cl) {
    constructor = makeRegrTask
  } else if ("RLearnerClassif" %in% cl) {
    constructor = makeClassifTask
  } else if ("RLearnerSurv" %in% cl) {
    constructor = makeSurvTask
  } else if ("RLearnerCluster" %in% cl) {
    constructor = makeClusterTask
  } else {
    stop("Unknown learner class for impute")
  }
  constructor
}
