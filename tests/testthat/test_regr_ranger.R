context("regr_ranger")

test_that("regr_ranger", {
  requirePackagesOrSkip("ranger", default.method = "load")

  parset.list = list(
    list(),
    list(num.trees = 100),
    list(num.trees = 250, mtry = 4),
    list(num.trees = 500, min.node.size = 2)
  )
  old.predicts.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    parset = c(parset, list(data = regr.train, formula = regr.formula, respect.unordered.factors = "order"))
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(ranger::ranger, parset)
    p = predict(m, data = regr.test)
    old.predicts.list[[i]] = p$predictions
  }

  testSimpleParsets("regr.ranger", regr.df, regr.target, regr.train.inds, old.predicts.list, parset.list)
})

test_that("different se.methods work", {
  se.methods = c("jackknife", "sd")
  preds = setNames(vector("list", length(se.methods)), se.methods)
  for (se.method in se.methods) {
    par.vals = list(se.method = se.method, num.trees = 10L)
    learner = makeLearner("regr.ranger", predict.type = "se", par.vals = par.vals)
    set.seed(getOption("mlr.debug.seed"))
    model = train(learner, task = bh.task, subset = 1:500)
    set.seed(getOption("mlr.debug.seed"))
    preds[[se.method]] = predict(model, task = bh.task)
    expect_true(is.numeric(preds[[se.method]]$data$se))
    expect_true(all(preds[[se.method]]$data$se >= 0))
    # test if it works with one row
    pred.one = predict(model, task = bh.task, subset = 501)
    expect_true(is.numeric(pred.one$data$se))
    expect_true(all(pred.one$data$se >= 0))
  }
  # mean prediction should be unaffected from the se.method
  expect_equal(preds$sd$data$response, preds$jackknife$data$response)
})

test_that("regr_ranger se", {
  requirePackagesOrSkip("ranger", default.method = "load")

  parset.list = list(
    list(keep.inbag = TRUE),
    list(num.trees = 100, keep.inbag = TRUE),
    list(num.trees = 250, mtry = 4, keep.inbag = TRUE),
    list(num.trees = 500, min.node.size = 2, keep.inbag = TRUE)
  )
  old.predicts.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    parset.list[[i]] = c(parset, predict.type = "se")
    parset = c(parset, list(data = regr.train, formula = regr.formula, respect.unordered.factors = "order"))
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(ranger::ranger, parset)
    set.seed(getOption("mlr.debug.seed"))
    p = predict(m, data = regr.test, type = "se")
    old.predicts.list[[i]] = cbind(p$predictions, p$se)
  }

  testSimpleParsets("regr.ranger", regr.df, regr.target, regr.train.inds, old.predicts.list, parset.list)
})
