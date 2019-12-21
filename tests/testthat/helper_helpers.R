requirePackagesOrSkip = function(packs, default.method = "attach") {
  ok = requirePackages(packs, why = "unit test", stop = FALSE, suppress.warnings = TRUE, default.method = default.method)
  if (any(!ok)) {
    skip(sprintf("Required packages not installed: %s", collapse(names(ok)[!ok])))
  }
  invisible(TRUE)
}

e1071CVToMlrCV = function(e1071.tune.result) {

  tr = e1071.tune.result
  inds = tr$train.ind
  size = max(unlist(inds))
  folds = length(inds)

  d = makeResampleDesc("CV", iters = folds)
  cv.instance = makeResampleInstance(d, size = size)

  for (i in 1:folds) {
    cv.instance$train.inds[[i]] = inds[[i]]
    cv.instance$test.inds[[i]] = setdiff(1:size, inds[[i]])
  }
  return(cv.instance)
}


e1071BootstrapToMlrBootstrap = function(e1071.tune.result) {

  tr = e1071.tune.result
  inds = tr$train.ind

  size = length(inds[[1]])
  iters = length(inds)

  d = makeResampleDesc("Bootstrap", iters = iters)
  bs.instance = makeResampleInstance(d, size = size)

  for (i in 1:iters) {
    bs.instance$train.inds[[i]] = inds[[i]]
    bs.instance$test.inds[[i]] = setdiff(1:size, inds[[i]])
  }
  return(bs.instance)
}


testSimple = function(t.name, df, target, train.inds, old.predicts, parset = list()) {

  inds = train.inds
  train = df[inds, ]
  test = df[-inds, ]

  lrn = do.call("makeLearner", c(list(t.name), parset))
  # FIXME this heuristic will backfire eventually
  if (length(target) == 0) {
    task = makeClusterTask(data = df)
  } else if (is.numeric(df[, target])) {
    task = makeRegrTask(data = df, target = target)
  } else if (is.factor(df[, target])) {
    task = makeClassifTask(data = df, target = target)
  } else if (is.data.frame(df[, target]) && is.numeric(df[, target[1L]]) && is.logical(df[, target[2L]])) {
    task = makeSurvTask(data = df, target = target)
  } else if (is.data.frame(df[, target]) && is.logical(df[, target[1L]])) {
    task = makeMultilabelTask(data = df, target = target)
  } else {
    stop("Should not happen!")
  }
  m = train(lrn, task, subset = inds)

  if (inherits(m, "FailureModel")) {
    expect_is(old.predicts, "try-error")
  } else {
    cp = predict(m, newdata = test)
    # Multilabel has a special data structure
    if (class(task)[1] == "MultilabelTask") {
      rownames(cp$data) = NULL
      expect_equal(unname(cp$data[, substr(colnames(cp$data), 1, 8) == "response"]), unname(old.predicts))
    } else {
      # to avoid issues with dropped levels in the class factor we only check the elements as chars
      if (is.numeric(cp$data$response) && is.numeric(old.predicts)) {
        if (lrn$predict.type == "se") {
          expect_equal(unname(cbind(cp$data$response, cp$data$se)), unname(old.predicts), tol = 1e-5)
        } else {
          expect_equal(unname(cp$data$response), unname(old.predicts), tol = 1e-5)
        }
      } else {
        expect_equal(as.character(cp$data$response), as.character(old.predicts))
      }
    }
  }
}


testSimpleParsets = function(t.name, df, target, train.inds, old.predicts.list, parset.list) {
  inds = train.inds
  train = df[inds, ]
  test = df[-inds, ]

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    old.predicts = old.predicts.list[[i]]
    testSimple(t.name, df, target, train.inds, old.predicts, parset)
  }
}


testProb = function(t.name, df, target, train.inds, old.probs, parset = list()) {

  inds = train.inds
  train = df[inds, ]
  test = df[-inds, ]

  if (length(target) == 1) {
    task = makeClassifTask(data = df, target = target)
  } else {
    task = makeMultilabelTask(data = df, target = target)
  }
  lrn = do.call("makeLearner", c(t.name, parset, predict.type = "prob"))
  m = try(train(lrn, task, subset = inds))

  if (inherits(m, "FailureModel")) {
    expect_is(old.predicts, "try-error")
  } else {
    cp = predict(m, newdata = test)
    # dont need names for num vector, 2 classes
    if (is.numeric(old.probs)) {
      names(old.probs) = NULL
    } else {
      old.probs = as.matrix(old.probs)
    }

    p = getPredictionProbabilities(cp)
    if (is.data.frame(p)) {
      p = as.matrix(p)
    }
    # we change names a bit so dont check them
    colnames(p) = colnames(old.probs) = NULL
    rownames(p) = rownames(old.probs) = NULL
    class(old.probs) = NULL
    expect_equal(p, old.probs, tolerance = 0.000001)
  }
}


testProbWithTol = function(t.name, df, target, train.inds, old.probs, parset = list(),
  tol = 1e-04) {

  inds = train.inds
  train = df[inds, ]
  test = df[-inds, ]

  if (length(target) == 1) {
    task = makeClassifTask(data = df, target = target)
  } else {
    task = makeMultilabelTask(data = df, target = target)
  }
  lrn = do.call("makeLearner", c(t.name, parset, predict.type = "prob"))
  m = try(train(lrn, task, subset = inds))

  if (inherits(m, "FailureModel")) {
    expect_is(old.predicts, "try-error")
  } else {
    cp = predict(m, newdata = test)
    # dont need names for num vector, 2 classes
    if (is.numeric(old.probs)) {
      names(old.probs) = NULL
    } else {
      old.probs = as.matrix(old.probs)
    }

    p = getPredictionProbabilities(cp)
    if (is.data.frame(p)) {
      p = as.matrix(p)
    }
    # we change names a bit so dont check them
    colnames(p) = colnames(old.probs) = NULL
    rownames(p) = rownames(old.probs) = NULL
    class(old.probs) = NULL
    expect_equal(p, old.probs, tolerance = tol)
  }
}


testProbParsets = function(t.name, df, target, train.inds, old.probs.list, parset.list) {
  inds = train.inds
  train = df[inds, ]
  test = df[-inds, ]

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    old.probs = old.probs.list[[i]]
    testProb(t.name, df, target, train.inds, old.probs, parset)
  }
}


testProbParsetsWithTol = function(t.name, df, target, train.inds, old.probs.list, parset.list,
  tol = 1e-04) {
  inds = train.inds
  train = df[inds, ]
  test = df[-inds, ]

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    old.probs = old.probs.list[[i]]
    testProbWithTol(t.name, df, target, train.inds, old.probs, parset, tol = tol)
  }
}

testCV = function(t.name, df, target, folds = 2, parset = list(),
  tune.train, tune.predict = predict) {

  requirePackages("e1071", default.method = "load")
  data = df
  formula = formula(paste(target, "~."))

  tt = function(formula, data, subset = seq_len(nrow(data)), ...) {
    pars = list(formula = formula, data = data[subset, ])
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    capture.output({
      m = do.call(tune.train, pars)
    })
    return(m)
  }

  tp = function(model, newdata) {
    set.seed(getOption("mlr.debug.seed"))
    p = tune.predict(model, newdata)
    return(p)
  }

  tr = e1071::tune(method = tt, predict.func = tp, train.x = formula,
    data = data,
    tunecontrol = e1071::tune.control(cross = folds, best.model = FALSE))

  cv.instance = e1071CVToMlrCV(tr)
  lrn = do.call("makeLearner", c(t.name, parset))
  if (is.numeric(df[, target])) {
    task = makeRegrTask(data = df, target = target)
  } else if (is.factor(df[, target])) {
    task = makeClassifTask(data = df, target = target)
  }
  ms = resample(lrn, task, cv.instance)$measures.test
  if (inherits(task, "ClassifTask")) {
    expect_equal(mean(ms[, "mmce"]), tr$performances[1, 2], check.names = FALSE)
    expect_equal(sd(ms[, "mmce"]), tr$performances[1, 3], check.names = FALSE)
  } else {
    expect_equal(mean(ms[, "mse"]), tr$performances[1, 2], check.names = FALSE)
    expect_equal(sd(ms[, "mse"]), tr$performances[1, 3], check.names = FALSE)
  }
  invisible(TRUE)
}

testCVParsets = function(t.name, df, target, folds = 2, tune.train,
  tune.predict = predict, parset.list) {
  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    testCV(t.name, df, target, folds, parset, tune.train, tune.predict)
  }
}

testBootstrap = function(t.name, df, target, iters = 3, parset = list(), tune.train, tune.predict = predict) {

  requirePackages("e1071", default.method = "load")
  data = df
  formula = formula(paste(target, "~."))
  tr = e1071::tune(method = tune.train, predict.func = tune.predict, train.x = formula, data = data,
    tunecontrol = e1071::tune.control(sampling = "bootstrap", nboot = iters, boot.size = 1))

  bs.instance = e1071BootstrapToMlrBootstrap(tr)
  lrn = do.call("makeLearner", c(t.name, parset))

  if (is.numeric(df[, target])) {
    task = makeRegrTask(data = df, target = target)
  } else if (is.factor(df[, target])) {
    task = makeClassifTask(data = df, target = target)
  }
  ms = resample(lrn, task, bs.instance)$measures.test
  if (inherits(task, "ClassifTask")) {
    expect_equal(mean(ms[, "mmce"]), tr$performances[1, 2], check.names = FALSE)
    expect_equal(sd(ms[, "mmce"]), tr$performances[1, 3], check.names = FALSE)
  } else {
    expect_equal(mean(ms[, "mse"]), tr$performances[1, 2], check.names = FALSE)
    expect_equal(sd(ms[, "mse"]), tr$performances[1, 3], check.names = FALSE)
  }
}


listLearnersCustom = function(..., create = FALSE) {
  lrns = listLearners(..., create = create)
  if (create) {
    ids = BBmisc::extractSubList(lrns, "id")
    return(lrns[!grepl("mock", ids) & !grepl("^(classif|regr).h2o", ids)])
  } else {
    ids = lrns$class
    return(lrns[!grepl("mock", ids) & !grepl("^(classif|regr).h2o", ids), ])
  }
}

testFacetting = function(obj, nrow = NULL, ncol = NULL) {
  expect_equal(obj$facet$params$nrow, nrow)
  expect_equal(obj$facet$params$ncol, ncol)
}

testDocForStrings = function(doc, x, grid.size = 1L, ordered = FALSE) {
  text.paths = paste("/svg:svg//svg:text[text()[contains(., '",
    x, "')]]", sep = "")
  nodes = XML::getNodeSet(doc, text.paths, ns.svg)
  expect_equal(length(nodes), length(x) * grid.size)
  if (ordered) {
    node.strings = vcapply(nodes, XML::getChildrenStrings)
    expect_equal(node.strings[seq_along(x)], x)
  }
}

constant05Resample = function(...) {
  res = resample(...)
  res$aggr = rep(0.5, length(res$aggr))
  res
}

# evaluate expr without giving its output.
quiet = function(expr) {
  capture.output({
    ret = expr
  })
  ret
}
