analyzeFeatSelResult = function(fs.obj){
  cl = class(fs.obj$control)[1]
  stopifnot(cl %in% c("FeatSelControlSequential", "FeatSelControlGA"))
  analyzeFunc = switch(cl,
    FeatSelControlSequential = analyzeSequential,
    FeatSelControlGA = analyzeGA,
    stop(paste("Unknown class of control object:", cl)))
  analyzeFunc(fs.obj)  
}


analyzeSequential = function(fs.obj){
  df = as.data.frame(fs.obj$opt.path)
  feat.names = names(fs.obj$opt.path$par.set$pars)
  measure = fs.obj$opt.path$minimize[1]
  optimum = ifelse(measure, min(df[,names(measure)]), max(df[,names(measure)]))
  df$optimum = (df[,names(measure)] == optimum)
  df$selected = (df$dob < df$eol)
  df$n.feats = rowSums(df[,feat.names])
  red.df = df[(df$optimum | df$selected), ]
  ctrl = fs.obj$control
  result = list(reduced.data.frame = red.df, control = ctrl, x = fs.obj$x, y = fs.obj$y, features = feat.names)
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


printAnalyzeFeatSelResultSeq = function(x, ...) {
  catf("FeatSel result:")
  change = ifelse(x$control$extra.args$method %in% c("sfs", "sffs"), "add", "remove")
  n.feats = length(x$x)
  feat.names = x$features
  m.names = names(x$y)
  printed.features = 10
  if(length(x$x) > printed.features) {
    catf("- features (%i): %s", n.feats, paste(c(x$x[1:printed.features], "..."), collapse = ", "))
  } else {
    catf("- features (%i): %s", n.feats, paste(x$x[1:n.feats], collapse = ", "))
  }
  catf("- Performance:  %s", perfsToString(x$y))
  catf("\nPath to optimum:")
  df = x$reduced.data.frame
  final = FALSE
  steps = unique(df$dob)
  steps = min(steps):max(steps)
  ## summarize the evaluations for each step (dob)
  old_feats = character()
  for(i in steps) {
    X.step = df[df$dob == i, ]  # relevant data per dob (equal or better performance)
    ## 1st case: new optimum
    if(any(X.step$selected)) {
      catf("Step %i:", i)
      X.select = X.step[X.step$selected, ] # relevant data with a new optimum
      for(j in 1:nrow(X.select)) {
        if(X.select$n.feats[j] == 0) {
          catf("- select 0 features")
          meas = X.select[j, m.names]
          names(meas) = m.names
          catf("- performance: %s", perfsToString(meas))
        } else {
          ## extract features and performance for each optimum
          feats = feat.names[as.logical(X.select[j, feat.names])]
          of = feats
          if(length(feats) >= length(old_feats)) {
            feats = setdiff(feats, old_feats)
          } else {
            feats = setdiff(old_feats, feats)
          }
          old_feats = of
          meas = X.select[j, m.names]
          names(meas) = m.names
          if((i == 1) && (change == "remove")) {
            catf("- start with all features")
            catf("- performance: %s", perfsToString(meas))
            next
          }
          catf("- %s feature: %s", change, feats)
          catf("- performance: %s", perfsToString(meas))
        }
      }
    }
    ## 2nd case: new alternatives
    if(any((X.step$optimum) & (!X.step$selected))) {
      if(!final) {
        final = TRUE
        catf("--- found final result ---")
      }
      catf("\nFurther alternatives (as step %i):", i)
      X.alternative = X.step[(X.step$optimum) & (!X.step$selected), ] # relevant data with equal performance
      ## extract features and performance for each alternative
      for(j in 1:nrow(X.alternative)) {
        feats = feat.names[as.logical(X.alternative[j, feat.names])]
        of = feats
        if(length(feats) >= length(old_feats)) {
          feats = setdiff(feats, old_feats)
        } else {
          feats = setdiff(old_feats, feats)
        }
        nf = length(feats)
        catf("(%02i) %s feature: %s", j, change, feats)
      }
      meas = X.alternative[1, m.names]
      names(meas) = m.names
      catf("performance of alternatives: %s", perfsToString(meas))
    }
  } ## end of for-loop
  ## Comment at the end of path, explaining why the algorithm stopped there.
  if(!is.na(x$control$max.features) & 
       (max(df$n.feats) == x$control$max.features) & 
       (change == "add")) {
    catf("\nStopped due to reached maximum of allowed features (%i).", x$control$max.features)
  } else {
    catf("\nStopped, because no improving set of features (w.r.t. %s) was found.", 
         paste(m.names, collapse = ", "))
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
  catf(" Performance:  %s", perfsToString(x$y))
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
      catf("       performance: %s", perfsToString(meas))
      next
    }
    feats = feat.names[as.logical(X.init[i, feat.names])]
    if(X.init$n.feats[i] == 1) {
      catf("  (%02i) select feature %s:", ind_counter, feats)
      catf("       performance: %s", perfsToString(meas))
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
    catf("       performance: %s", perfsToString(meas))
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
        catf("       performance: %s", perfsToString(meas))
        next
      }
      feats = feat.names[as.logical(X.sel[, feat.names])]
      if(X.init$n.feats[i] == 1) {
        catf("  (%02i) select feature %s:", ind_counter, feats)
        catf("       performance: %s", perfsToString(meas))
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
      catf("       performance: %s", perfsToString(meas))
    } ## end of loop of individuals per generation
  } ## end of generations-loop 
}


#' @S3method print analyzeFeatSelResult
print.analyzeFeatSelResult = function(x, ...) {
  switch(class(x$control)[1],
         FeatSelControlSequential = printAnalyzeFeatSelResultSeq(x, ...),
         FeatSelControlGA = printAnalyzeFeatSelResultGA(x, ...))
}