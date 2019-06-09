# start is a named list, flatten it to an unnamed num vec, of correct order as in par.set
convertStartToNumeric = function(start, par.set) {
  ids = getParamIds(par.set, repeated = FALSE)
  start = start[ids]
  as.numeric(unlist(start))
}

# converter for single x
# leaves x unchanged
convertXIdentity = function(x, par.set) {
  return(x)
}

# converter for single x
# takes a flat vector of all joint values and produces a normal named list,
# looking at the par.set structure
convertXNumeric = function(x, par.set) {
  ids = getParamIds(par.set, repeated = TRUE, with.nr = FALSE)
  # factor usually does sort(unique(...)) for levels which changes order!
  x = split(x, factor(ids, levels = unique(ids)))
  names(x) = getParamIds(par.set, repeated = FALSE)
  roundIntegers(x, par.set = par.set)
}


# converter for vectorized eval
# takes a matrix where vec are in cols
# each col is a flat vector of all joint values,
# we produce a list of list parvals, calling convertXNumeric on the cols
convertXVectorizedMatrixCols = function(xs, par.set) {
  rownames(xs) = colnames(xs) = NULL
  xs = lapply(seq_col(xs), function(i) {
    convertXNumeric(xs[, i], par.set)
  })
}

roundIntegers = function(x, par.set) {
  Map(function(par, v) {
    if (par$type %in% c("integer", "integervector")) {
      as.integer(round(v))
    } else {
      v
    }
  }, par.set$pars, x)
}

# convert logical param values from chars to true logicals,
# eg irace produces strings in tuning
convertXVectorizedBooleanStringsToLogical = function(x, par.set) {
  cx = function(x) {
    types = getParamTypes(par.set, use.names = TRUE)
    j = types %in% c("logical", "logicalvector")
    if (any(j)) {
      x[j] = lapply(x[j], as.logical)
    }
    return(x)
  }
  lapply(x, cx)
}
