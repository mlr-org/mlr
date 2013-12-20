analyzeFeatSelResult = function(fs.obj, ...){
  cl = class(fs.obj$control)[1]
  stopifnot(cl %in% c("FeatSelControlSequential", "FeatSelControlGA"))
  analyzeFunc = switch(cl,
    FeatSelControlSequential = analyzeSequential,
    FeatSelControlGA = analyzeGA,
    stop(paste("Unknown class of control object:", cl)))
  analyzeFunc(fs.obj, ...)  
}


analyzeSequential = function(fs.obj, reduce=TRUE){
  df = as.data.frame(fs.obj$opt.path)
  feat.names = names(fs.obj$opt.path$par.set$pars)
  measure = fs.obj$opt.path$minimize[1]
  optimum = ifelse(measure, min(df[,names(measure)]), max(df[,names(measure)]))
  df$optimum = (df[,names(measure)] == optimum)
  df$selected = (df$dob < df$eol)
  df$n.feats = rowSums(df[,feat.names])
  if(reduce)
    df = df[(df$optimum | df$selected), ]
  ctrl = fs.obj$control
  names(fs.obj$y) = names(measure)
  result = list(reduced.data.frame = df, control = ctrl, x = fs.obj$x, y = fs.obj$y, features = feat.names)
  mode(result) = "list"
  class(result) = "analyzeFeatSelResult"
  return(result)
}


analyzeGA = function(fs.obj){
  df = as.data.frame(fs.obj$opt.path)
  feat.names = names(fs.obj$opt.path$par.set$pars)
  measure = fs.obj$opt.path$minimize[1]
  optimum = ifelse(measure, min(df[,names(measure)]), max(df[,names(measure)]))
  df$optimum = (df[,names(measure)] == optimum)
  df$selected = (is.na(df$eol) | (df$dob < df$eol))
  df$n.feats = rowSums(df[,feat.names])
  red.df = df[(df$optimum | df$selected), ]
  ctrl = fs.obj$control
  result = list(reduced.data.frame = red.df, control = ctrl, x = fs.obj$x, y = fs.obj$y, features = feat.names)
  mode(result) = "list"
  class(result) = "analyzeFeatSelResult"
  return(result)
}


printAnalyzeFeatSelResultSeq = function(x, printed.features=10) {
  catf("FeatSel result:")
  n.feats = length(x$x)
  final.feats.print = head(x$x, printed.features)
  if(n.feats > printed.features) 
    final.feats.print[printed.features+1] = "..."
  catf("- features (%i): %s", n.feats, paste(final.feats.print, collapse = ", "))
  catf("- Performance: %s", mlr:::perfsToString(x$y))
  catf("\nPath to optimum:")
  df = x$reduced.data.frame
  df$step = as.numeric(as.factor(df$dob))
  # Initialize first Values
  n.feats.old = df[1,"n.feats"]
  stepVars.old = x$features[df[1,x$features]==1]  
  measures.old = as.numeric(df[1,names(x$y)])
  names(measures.old) = names(x$y)
  # Iterate over all steps:
  for(i in unique(df$step)){
    df.step = df[df$step==i,]
    # Print what happens in each step
    for(j in seq_row(df.step)) {
      measures = as.numeric(df.step[j,names(x$y)])
      names(measures) = names(x$y)
      measures.gain = measures - measures.old
      n.feats = df.step[j, "n.feats"]
      if (n.feats < n.feats.old) changeTxt = "Removed: "
      else if (n.feats > n.feats.old) changeTxt = "Added: "
      else changeTxt = "Initial model"
      stepVars = x$features[df.step[j,x$features]==1]
      diffVar = setdiff(union(stepVars, stepVars.old), intersect(stepVars, stepVars.old))
      diffVar = paste(diffVar, collapse=",")
      if (df.step[j,"selected"]) {
        n.feats.opt = n.feats
        stepVars.opt = stepVars
        measures.opt = measures
        txtSelected = "SELECTED"
      } else 
        txtSelected = ""
      catf("- Features: %s  \t %s%s \t Gain: %s \t %s",
           n.feats, changeTxt, diffVar, mlr:::perfsToString(measures.gain), txtSelected)
    }
    # Print end result of each Step
    catf("Finished step: %i with \t %s \t Optimum: %s", 
         head(df.step[,"step"],1), mlr:::perfsToString(measures.opt), any(df.step$optimum))
    n.feats.old = n.feats.opt
    stepVars.old = stepVars.opt
    measures.old = measures.opt
  }
  
  if (!is.na(x$control$max.features) & (max(df$n.feats) == x$control$max.features)) {
    catf("\nStopped due to reached maximum of allowed features (%i).", x$control$max.features)
  } else {
    catf("\nStopped, because no improving set of features (w.r.t. %s) was found.", 
         paste(names(x$y), collapse = ", "))
  } 
}


printAnalyzeFeatSelResultGA = function(x, ...) {
  catf("FeatSel result:")
  n.feats = length(x$x)
  feat.names = x$features
  m.names = names(x$y)
  printed.features = 10
  if(length(x$x) > printed.features) {
    catf(" features (%i): %s", n.feats, paste(c(x$x[1:printed.features], "..."), collapse = ", "))
  } else {
    catf(" features (%i): %s", n.feats, paste(x$x[1:n.feats], collapse = ", "))
  }
  catf(" Performance:  %s", mlr:::perfsToString(x$y))
  catf("\nPath to optimum:")
  df = x$reduced.data.frame
  generations = 1L : x$control$maxit
  ind_counter = 0L
  ind_act = 1 : nrow(df)
  perf_pop = numeric()  
  ## initial generation
  X.init = df[df$dob == 0L, ]
  catf("Initial generation:")
  for(i in 1L : nrow(X.init)) {
    ind_counter = ind_counter + 1
    meas = X.init[i, m.names]
    names(meas) = m.names
    perf_pop = c(perf_pop, as.numeric(meas))
    if(X.init$n.feats[i] == 0L) {
      catf("  (%02i) select 0 features", ind_counter)
      catf("       performance: %s", mlr:::perfsToString(meas))
      next
    }
    feats = feat.names[as.logical(X.init[i, feat.names])]
    if(X.init$n.feats[i] == 1) {
      catf("  (%02i) select feature %s:", ind_counter, feats)
      catf("       performance: %s", mlr:::perfsToString(meas))
      next
    }
    nf = length(feats)
    if(nf > printed.features) {
      catf("  (%02i) select %02i features: %s", ind_counter, nf, 
           paste(c(feats[1:printed.features], "..."), collapse = ", "))
    } else {
      catf("  (%02i) select %02i features: %s", ind_counter, nf, 
           paste(feats[1:nf], collapse = ", "))
    }
    catf("       performance: %s", mlr:::perfsToString(meas))
  }
  ## Evaluate the generations (which of the new individuals replaces which of the old ones?)
  for(g in generations) {
    catf("Generation %i:", g)
    X.gen = df[df$dob == g, ]
    new_inds = nrow(X.gen)
    if(new_inds == 0L) {
      catf("- none of the new individuals is better than any from the current best population")
      next
    }
    replace_index = rev(order(perf_pop))[1:new_inds]
    to_be_replaced = ind_act[replace_index]
    if(length(replace_index) == 1L) {
      catf("- replace individual %s with:", paste(paste("(", to_be_replaced, ")", sep = ""), collapse = ", "))  
    } else {
      catf("- replace individuals %s with:", paste(paste("(", to_be_replaced, ")", sep = ""), collapse = ", "))
    }
    ## run through the new (better) individuals
    for(k in 1:new_inds) {
      X.sel = X.gen[k, ]
      ind_counter = ind_counter + 1
      ind_act[replace_index[k]] = ind_counter
      meas = X.sel[, m.names]
      names(meas) = m.names
      perf_pop[replace_index[k]] = as.numeric(meas)
      if(X.sel$n.feats == 0L) {
        catf("  (%02i) select 0 features", ind_counter)
        catf("       performance: %s", mlr:::perfsToString(meas))
        next
      }
      feats = feat.names[as.logical(X.sel[, feat.names])]
      if(X.init$n.feats[i] == 1) {
        catf("  (%02i) select feature %s:", ind_counter, feats)
        catf("       performance: %s", mlr:::perfsToString(meas))
        next
      }
      nf = length(feats)
      if(nf > printed.features) {
        catf("  (%02i) select %02i features: %s", ind_counter, nf, 
             paste(c(feats[1:printed.features], "..."), collapse = ", "))
      } else {
        catf("  (%02i) select %02i features: %s", ind_counter, nf, 
             paste(feats[1:nf], collapse = ", "))
      }
      catf("       performance: %s", mlr:::perfsToString(meas))
    } ## end of loop of individuals per generation
  } ## end of generations-loop 
}


#' @S3method print analyzeFeatSelResult
print.analyzeFeatSelResult = function(x, ...) {
  switch(class(x$control)[1],
         FeatSelControlSequential = printAnalyzeFeatSelResultSeq(x, ...),
         FeatSelControlGA = printAnalyzeFeatSelResultGA(x, ...))
}