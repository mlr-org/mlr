#' Calculate distance to nearest neighbor for each point in a set
#' @param X [\code{matrix(n, d)}]\cr
#'   Matrix of n points of dimension d.
#' @return [\code{numeric(n)}]. Distances to nearest neighbor. 
distToNN = function(X, ...) {
  d = as.matrix(dist(X))
  diag(d) = Inf
  apply(d, 1, min)
}


#' Calculate distance to nearest better neighbor for each point in a set
#' @param X [\code{matrix(n, d)}]\cr
#'   Matrix of n points of dimension d.
#' @param y [\code{matrix(n, d)}]\cr
#'   Vector of numerical values for all points. 
#'   Smaller is better.
#' @param minimize [\code{matrix(n, d)}]\cr
#'   Is \code{y} minimized, so smaller is better?
#' @return [\code{numeric(n)}]. Distances to nearest better neighbor. 
distToNB = function(X, y, minimize = TRUE) {
  d = as.matrix(dist(X))
  y = ifelse(minimize, 1, -1) * y
  sapply(seq_col(d), function(i) {
    better = y < y[i]
    if (sum(better) == 0)
      Inf
    else
      min(d[better, i])
  })
}


nds_1d_selection <- function(values, n=1, index=1, ...) {  
  # N solutions given, remove n, k remain
  N = ncol(values)
  k = N - n
  ranks = nds_rank(values)
  # order according to 
  # 1) non-dominated front, 
  # 2) objective value at index (minimization)
  indicesOrderedByFitness = order(ranks, values[index, ])
  return(indicesOrderedByFitness[(k+1):N])
}



#FIXME: maybe add a local opt. hybrid step to get better
# into local opts

#' Implements our new infill criterion which optimizes EI and diversity in X space
#' 
#' Currently only numerical paramaters are handled, for them pm_operator and 
#' sbx_operator from emoa are used in the EA.
#' 
multipointInfillOptMulticrit = function(model, control, par.set, opt.path, design) {
  requirePackages("emoa", why="multipointInfillOptMulticrit")
  n = control$propose.points
  
  # add points to archive
  # FIXME dont always do this for speed
  # note: as distance is always recalculated we always add mu+1 point
  # and not 1 per generation
  addGenerationToPath = function(X, Y, gen) {
    lapply(1:nrow(X), function(i) {
      addOptPathEl(opt.path, list(x = as.numeric(X[i,])), as.numeric(Y[i,]), dob = gen)
    })
  }
  
  #messagef("Trying to find %i good points.", n)
  # vars: dim, mu, objectives, operators
  
  if (control$multipoint.objective == "ei") {
    y.dim = 2
    y.names = c("ei", "dist")
  } else if (control$multipoint.objective == "bicriteria") {
    y.dim = 3
    y.names = c("mean", "sd", "dist")  
  }
  repids = getParamIds(par.set, repeated=TRUE, with.nr = TRUE)
  d = sum(getParamLengths(par.set))
  mu = n
  # FIXME: reasobale standard defaults?
  eta = 15; p = 1
  mutate = pm_operator(eta, p, getLower(par.set), getUpper(par.set))
  crossover = sbx_operator(eta, p, getLower(par.set), getUpper(par.set))
  mydist = switch(control$multipoint.distfun,
    nearest.neighbor = distToNN,                 
    nearest.better = distToNB                 
  )
  minimize = minimize = rep(TRUE, y.dim)
  # FIXME minimize correct?
  opt.path = makeOptPathDF(par.set = par.set, y.names = y.names, minimize = minimize)

  # Random inital population:
  X = generateDesign(mu, par.set, fun=randomLHS)
  Y = matrix(NA, mu, y.dim)
  # mbo infill crits are always minimized
  if (control$multipoint.objective == "ei") {
    Y[, 1] = infillCritEI(X, model, ctrl, par.set, design)
  } else if (control$multipoint.objective == "bicriteria") {
    Y[, 1] = infillCritMeanResponse(X, model, ctrl, par.set, design)
    Y[, 2] = infillCritStandardError(X, model, ctrl, par.set, design)
  }
  # use first Y criterion to for nearest better
  Y[, y.dim] = -mydist(as.matrix(X), Y[,1], minimize[1])
  addGenerationToPath(X, Y, gen = 0L)
  
  for (i in 1:control$multipoint.multicrit.maxit) {
    # Create new individual (mu + 1)
    parents = sample(1:mu, 2)
    # get two kids from CX, sel. 1 randomly, mutate
    child = crossover(t(X[parents, , drop=FALSE]))
    child1 = child[,sample(c(1, 2), 1)]
    child1 = mutate(child1)
    # Add new individual:
    X[nrow(X) + 1,] = child1
    child2 = setColNames(as.data.frame(as.list(child1)), repids)
    # FIXME
    # distanace has potentielly calculated avccording to Q = P + A
    # best try arhoive with design and empthy both...
    Y = rbind(Y, rep(NA, y.dim))
    # mbo infill crits are always minimized
    if (control$multipoint.objective == "ei") {
      Y[nrow(Y), 1] = infillCritEI(child2, model, ctrl, par.set, design)
    } else if (control$multipoint.objective == "bicriteria") {
      Y[nrow(Y), 1] = infillCritMeanResponse(child2, model, ctrl, par.set, design)
      Y[nrow(Y), 2] = infillCritStandardError(child2, model, ctrl, par.set, design)
    }
    # use first Y criterion to for nearest better
    Y[, y.dim] = -mydist(as.matrix(X), Y[,1], minimize[1])
    addGenerationToPath(X, Y, gen = i)
    # Select and remove worst individual
    # FIXME maybe also select wrt first crit
    # this would be eithe EI or mu (?? 2nd case is less clear!) 
    col.mins = apply(Y, 2, min)
    col.maxs = apply(Y, 2, max)
    # define ref point adaptively by max + 10% of range
    # personal communication simon with m. emmerich
    ref = col.maxs + 0.1 * (col.maxs - col.mins)
    
    #to.kill = nds_hv_selection(t(Y), ref=ref)
    to.kill = nds_1d_selection(t(Y), index=2)
    X = X[-to.kill, ,drop=FALSE]
    Y = Y[-to.kill, ,drop=FALSE]
    #FIXME really display all this crap? only on show.info
    #messagef("Generation %i; best crit1 = %.2f, max dist = %s", 
    #  i, max(-Y[, 1]), max(-Y[, y.dim]))
  }
  rownames(X) = NULL
  #list(X = X, Y = Y, opt.path = opt.path)
  return(X)
}


