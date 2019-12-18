context("caching")

test_that("caching works with most filters", {
  filters = as.character(listFilterMethods()$id)
  filter.list = listFilterMethods(desc = FALSE, tasks = TRUE, features = FALSE)
  filter.list.classif = as.character(filter.list$id)[filter.list$task.classif]
  filter.list.classif = setdiff(filter.list.classif, c(
    "univariate.model.score", "permutation.importance", "auc",
    "univariate", "rf.importance", "randomForestSRC_var.select"))
  filter.list.regr = as.character(filter.list$id)[!filter.list$task.classif & filter.list$task.regr]

  # tune over various filters using all possible caching options
  # TRUE is not tested, as we are not allowed to write in the user's home dir
  out = lapply(list(FALSE, tempdir()), function(i) {
    tune.out = lapply(filter.list.regr, function(.x) {
      lrn = makeFilterWrapper(learner = "regr.ksvm", fw.method = .x, cache = i)
      ps = makeParamSet(makeNumericParam("fw.perc", lower = 0, upper = 1),
        makeNumericParam("C", lower = -1, upper = 1,
          trafo = function(x) 2^x),
        makeNumericParam("sigma", lower = -1, upper = 1,
          trafo = function(x) 2^x)
      )
      rdesc = makeResampleDesc("CV", iters = 2)

      tuneParams(lrn, task = regr.num.task, resampling = rdesc, par.set = ps,
        control = makeTuneControlRandom(maxit = 2),
        show.info = FALSE, measures = getDefaultMeasure(regr.num.task))
    })

  })
  expect_equal(out[[1]][["opt.path"]][["env"]][["path"]][["mse.test.mean"]],
    out[[2]][["opt.path"]][["env"]][["path"]][["mse.test.mean"]],
    out[[3]][["opt.path"]][["env"]][["path"]][["mse.test.mean"]])
})

test_that("cache dir is successfully deleted", {
  skip_on_cran() # we are not allowed to write to the user's home dir!
  dir = getCacheDir()
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
  expect_true(dir.exists(getCacheDir()))

  deleteCacheDir()
  expect_false(dir.exists(getCacheDir()))
})
