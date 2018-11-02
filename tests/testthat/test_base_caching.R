context("caching")

test_that("caching works with most filters", {

  filters = as.character(listFilterMethods()$id)
  filter.list = listFilterMethods(desc = FALSE, tasks = TRUE, features = FALSE)
  filter.list.classif = as.character(filter.list$id)[filter.list$task.classif]
  filter.list.classif = setdiff(filter.list.classif, c(
    "univariate.model.score", "permutation.importance", "auc",
    "univariate", "rf.importance", "rf.min.depth"))
  filter.list.regr = as.character(filter.list$id)[!filter.list$task.classif & filter.list$task.regr]

  # tune over various filters using all possible caching options
  out = lapply(list(NULL, TRUE, tempdir()), function (i) {

    tune_out = lapply(filter.list.regr, function(.x) {
      lrn = makeFilterWrapper(learner = "regr.ksvm", fw.method = .x, cache = i)
      ps = makeParamSet(makeNumericParam("fw.perc", lower = 0, upper = 1),
                        makeNumericParam("C", lower = -10, upper = 10,
                                         trafo = function(x) 2^x),
                        makeNumericParam("sigma", lower = -10, upper = 10,
                                         trafo = function(x) 2^x)
      )
      rdesc = makeResampleDesc("CV", iters = 3)

      # print(.x)

      tuneParams(lrn, task = regr.num.task, resampling = rdesc, par.set = ps,
                 control = makeTuneControlRandom(maxit = 5),
                 show.info = FALSE)
    })

  })
  expect_equal(out[[1]][["opt.path"]][["env"]][["path"]][["mse.test.mean"]],
               out[[2]][["opt.path"]][["env"]][["path"]][["mse.test.mean"]],
               out[[3]][["opt.path"]][["env"]][["path"]][["mse.test.mean"]])
})

test_that("cache dir is successfully deleted", {

  fs::dir_create(get_cache_dir())
  expect_true(fs::dir_exists(get_cache_dir()))

  delete_cache()
  expect_false(fs::dir_exists(get_cache_dir()))

})
