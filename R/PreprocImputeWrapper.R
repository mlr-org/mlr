imputeMode = function(data, target, args) {
  const = computeMode(data[[target]], na.rm=TRUE)
  rm(data, target, args)

  function(x) replace(x, is.na(x), const)
}

imputeMedian = function(data, target, args) {
  const = median(data[[target]], na.rm=TRUE)
  rm(data, target, args)

  function(x) replace(x, is.na(x), const)
}

imputeNormalDist = function(data, target, args) {
  mu = mean(data[[target]], na.rm=TRUE)
  sd = sd(data[[target]], na.rm=TRUE)
  rm(data, target, args)

  function(x) {
    ind = is.na(x)
    replace(x, ind, rnorm(length(ind), mean=mu, sd=sd))
  }
}

imputeHist = function(data, target, args) {
  x = data[[target]]
  if (is.numeric(x)) {
    tmp = hist(x, breaks=breaks, plot=FALSE)
    counts = tmp$counts
    vals=tmp$mids
  } else {
    tmp = table(x, useNAs="no")
    counts = as.integer(tmp)
    vals = names(x)
    if (is.logical(x))
      vals = as.logical(x)
  }
  rm(data, target, x, tmp, args)

  function(x) {
    ind = which(is.na(x))
    replace(x, ind, sample(info$vals, length(ind), replace=TRUE, prob=info$counts))
  }
}

getDummies = function(data, force=FALSE) {
  f = function(x) {
    y = is.na(x)
    if (force || any(y)) y else NULL
  }
  dummies = Filter(Negate(is.null), lapply(data, f))
  if (length(dummies))
    as.data.frame(dummies)
  else
    makeDataFrame(nrow(data), 0L)
}

renameDummies = function(data) {
  setNames(data, sprintf("%s.dummy", names(data)))
}

setMissingLevel = function(x, lvl) {
  stopifnot(is.factor(x))
  levels(x) = c(levels(x), lvl)
  replace(x, is.na(x), lvl)
}

makeImputeWrapper = function(learner, impute.class=list(), impute.var=list(),
  add.dummies="no", missing.lvl=NULL) {

  checkArg(learner, "Learner")
  checkArg(impute.class, "list")
  checkArg(impute.var, "list")
  checkArg(add.dummies, choices=c("no", "ifany", "always"))
  if (!is.null(missing.lvl))
    checkArg(missing.lvl, "character", len=1L, na.ok=FALSE)


  trainfun = function(data, target, args) {
    cn = setdiff(colnames(data), target)
    classes = vapply(data[cn], class, character(1L))
    factors = names(which(classes == "factor"))
    control = namedList(c("fun", "levels", "cols.with.dummies"))

    if (!all(names(args$impute.var) %in% cn))
      stop("Not all variables to impute found in training data")

    # replace NAs in factors with missing levels
    if (!is.null(args$missing.lvl))
      data[factors] = lapply(data[factors], setMissingLevel, lvl=args$missing.lvl)

    # collect factor levels
    # FIXME call droplevels() ?
    # FIXME mark empty levels as missing?
    control$levels = Filter(Negate(is.null), lapply(data[factors], levels))

    # transfer imputation method from impute.class to impute.var
    add = Map(function(cl, method) {
      cols = names(which(classes == cl))
      setNames(rep.int(method, length(cols)), cols)
    }, cl=names(args$impute.class), method=args$impute.class)
    add = unlist(unname(add), recursive=FALSE)
    impute.var = insert(add, args$impute.var)

    # construct impute functions
    control$fun = Map(function(target, method) {
      if (is.function(method)) {
        fun = match.fun(method)
        if (all(c("data", "target", "args") %in% names(formals(fun))))
          return(fun(data=data, target=target, args=args))
        return(fun)
      }
      constructor = switch(method,
        "mode" = imputeMode,
        "median" = imputeMedian,
        "hist" = imputeHist,
        "normal" = imputeNormalDist,
        stop("Unknown imputation method"))
      constructor(data, target, args)
    }, target=names(impute.var), method=impute.var)


    # make dummies
    if (args$add.dummies == "no") {
      dummies = makeDataFrame(nrow=nrow(data), ncol=0L)
      control$cols.with.dummies = character(0L)
    } else {
      dummies = getDummies(data, force=args$add.dummies == "always")
      control$cols.with.dummies = names(dummies)
    }

    # apply imputations
    cols = names(control$fun)
    if (length(cols))
      data[cols] = Map(function(impute, x) impute(x),
        impute=control$fun, x=data[cols])

    list(data=cbind(data, renameDummies(dummies)), control=control)
  }

  predictfun = function(data, target, args, control) {
    factors = names(which(vapply(data, is.factor, logical(1L))))

    if (length(factors)) {
      # add missing level to factors
      if (!is.null(args$missing.lvl))
        data[factors] = lapply(data[factors], setMissingLevel, lvl=args$missing.lvl)

      # convert additional levels to NA in order to trigger imputation
      # furthermore, reorder levels to match original order in train
      data[factors] = Map(function(x, expected) {
        factor(replace(x, x %nin% expected, NA_integer_), levels = expected)
      }, x=data[factors], control$levels[factors])
    }

    # apply imputations
    cols = names(control$fun)
    if (length(cols))
      data[cols] = Map(function(impute, x) impute(x),
        impute=control$fun, x=data[cols])

    # add dummies and return
    cbind(data, renameDummies(getDummies(data[control$cols.with.dummies], force=TRUE)))
  }

  pv = list(impute.class=impute.class, impute.var=impute.var,
    add.dummies=add.dummies, missing.lvl=missing.lvl)
  rm(impute.class, impute.var, add.dummies, missing.lvl)
  makePreprocWrapper(learner, trainfun, predictfun, par.vals=pv)
}
