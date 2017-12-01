# some code to display info about GA that I dont really get

analyzeGA = function(fs.obj, reduce=TRUE){
  analyzeSequential(fs.obj, reduce)
}

printAnalyzeFeatSelResultGA = function(x, printed.features=10L) {
  printAnalyzeFeatSelResultHead(x, printed.features)
  df = x$reduced.data.frame
  generations = 0:x$control$maxit
  for (i in generations) {
    df.this.gen = df[df$dob == i,]
    cat(if (i == 0L) "Initial generation:" else paste0("Generation ", i, ":"))
    if (nrow(df.this.gen) == 0L) {
      catf("- none of the new individuals is better than any from the current best population")
      next
    }
    for (j in seq_row(df.this.gen)) {
      feats = x$features[df.this.gen[j, x$features]==1]
      measures = as.numeric(df.this.gen[j,names(x$y)])
      names(measures) = names(x$y)
      dieTxt = if (!df.this.gen[j, "selected"]) "\t (died out)" else ""
      catf("- (%i) \t Features: %i  \t %s \t Features: %s %s",
           j, df.this.gen[j,"n.feats"], perfsToString(measures), stringMaxConcat(feats, printed.features), dieTxt)
    }
  }
}

printAnalyzeFeatSelResultGA.old = function(x, printed.features=10L) {
  generations = seq_len(x$control$maxit)
  ind_counter = 0L
  ind_act = seq_row(df)
  perf_pop = numeric()
  ## initial generation
  X.init = df[df$dob == 0L, ]
  catf("Initial generation:")
  for(i in seq_row(X.init)) {
    ind_counter = ind_counter + 1L
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


