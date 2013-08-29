

ensembleSelector = function(learners, ensemble.select, rt, par.set, control, opt.path, ...){
  n.learners = length(learner)
  if(ensemble.select=="random") {
    sel.learner = sample(1:n.learners,1)
  } else if (ensemble.select=="best.proposed") {
    loop.models = list()
    loop.prop = list()
    for(learner in learners){
      #mod = train(learner, rt)
      loop.models[[learner$id]] = train(learner, rt)  
      loop.prop[[learner$id]] = proposePoints(loop.models[[learner$id]], par.set, control, opt.path)      
    }
    best.learner <- names(which.min(sapply(loop.prop,FUN=function(d){d$y})))
    model = loop.models[[best.learner]]
    learner = learners[[best.learner]]
    sel.learner = best.learner
  } else if (ensemble.select=="our.ensemble") {
    sel.learner = sample(1:n.learners,1, prob=action.probs[j,])
  }
  list(sel.learner=sel.learner, learner=learner, model=model)
}



#list(learner, sel.learner=sel.learner)
