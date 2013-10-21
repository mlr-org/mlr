#FIXME: param namwe clashes
#FIMXE was ist mit den vordefinierten parvals

makeModelSelectionEnsemble = function(base.learners, id="ModelSelectionEnsemble") {
  checkArg(id, "character", len=1L, na.o=FALSE)
  checkArg(base.learners, "list")
  checkListElementClass(base.learners, "Learner")
  ids = unique(extractSubList(base.learners, "id"))
  types = unique(extractSubList(base.learners, "type"))
  if (length(ids) != length(base.learners))
    stop("Base learners must all have unique ids!")
  if (length(types) > 1L)
    stopf("Base learners must all be of same type, but have: %s", collapse(types))
  
  # construct combined param set
  par.set = makeParamSet(makeDiscreteLearnerParam("selected.learner", values=ids))
  for (i in seq_along(base.learners)) {
    bl = base.learners[[i]]
    ps = bl$par.set
    pars = ps$pars
    pids = sprintf("%s.%s", ids[[i]], names(pars))
    for (j in seq_along(pars)) {
      ps$pars[[j]]$id = pids[[j]]
    }
    names(ps$pars) = pids
    par.set = c(par.set, ps)
  }

  lrn = structure(list(
    id = id,
    type = types,
    package = unique(extractSubList(base.learners, "package")),
    par.set = par.set,
    par.vals = list(),
    predict.type = "response",
    numerics = all(extractSubList(base.learners, "numerics")),
    factors = all(extractSubList(base.learners, "factors")),
    missings = all(extractSubList(base.learners, "missings")),
    weights = all(extractSubList(base.learners, "weights")),
    oneclass = all(extractSubList(base.learners, "oneclass")),
    twoclass = all(extractSubList(base.learners, "twoclass")),
    multiclass = all(extractSubList(base.learners, "multiclass")),
    multiclass = all(extractSubList(base.learners, "prob")),
    se = all(extractSubList(base.learners, "se"))
  ), class = c("ModelSelectionEnsemble", "Learner"))
  lrn$base.learners = setNames(base.learners, ids)
  # make 1st learner the default
  lrn = setHyperPars(lrn, selected.learner=ids[1])
  return(lrn)
}

#' @S3method trainLearner ModelSelectionEnsemble
trainLearner.ModelSelectionEnsemble = function(.learner, .task, .subset, selected.learner, ...) {
  bl = .learner$base.learners[[selected.learner]]
  args = list(...)
  id = bl$id 
  names(args) = substring(names(args), nchar(id) + 2)
  do.call(trainLearner, c(list(bl, .task, .subset), args))
}

#' @S3method predictLearner ModelSelectionEnsemble
predictLearner.ModelSelectionEnsemble = function(.learner, .model, .newdata, ...) {
  sl = .learner$par.vals$selected.learner
  bl = .learner$base.learners[[sl]]
  predictLearner(bl, .model, .newdata)
}


library(devtools)
library(parallelMap)
load_all("skel", reset=TRUE)

bls = list(makeLearner("classif.lda"), makeLearner("classif.rpart"))
lrn = makeModelSelectionEnsemble(bls)
task = makeClassifTask(data=iris, target="Species")
rdesc = makeResampleDesc("CV", iters=2)
# r = resample(lrn, task, rdesc)
# lrn = setHyperPars(lrn, selected.learner="classif.rpart")
# r = resample(lrn, task, rdesc)
# lrn = setHyperPars(lrn, selected.learner="classif.rpart", classif.rpart.minsplit=100)
# #train(lrn, task)
# r = resample(lrn, task, rdesc)

ps = makeParamSet(
  makeDiscreteParam("selected.learner", values=c("classif.lda", "classif.rpart")),
  makeIntegerParam("classif.rpart.minsplit", lower=1, upper=50, requires=quote(selected.learner == "classif.rpart"))
)  

ctrl = makeTuneControlIrace(n.instances=10, show.irace.output=TRUE, maxExperiments=100)
res = tuneParams(lrn, task, rdesc, par.set=ps, control=ctrl)



