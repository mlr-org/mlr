context("cpo filter")


test_that("filterFeatures default test", {
  # Loop through all filters
  filter.list = listFilterMethods(desc = FALSE, tasks = TRUE, features = FALSE)
  filter.list.classif = as.character(filter.list$id)[filter.list$task.classif]
  # univariate.model.score and permutation.importance are handled extra test below
  # 'univariate', 'rf.importance' and 'rf.min.depth' are deprecated
  filter.list.classif = setdiff(filter.list.classif, c(
    "univariate.model.score", "permutation.importance",
    "univariate", "rf.importance", "rf.min.depth"))
  for (filter in filter.list.classif) {
    if (filter %in% c("randomForestSRC.rfsrc", "randomForestSRC.var.select")) {  # crash on my machine for some reason.
      next
    }
    set.seed(123)
    result1 = multiclass.task %>>% cpoFilterFeatures(method = filter, perc = 0.5)
    result2 = multiclass.task %>>% retrafo(result1)
    set.seed(123)
    filtered = filterFeatures(task = multiclass.task, method = filter, perc = 0.5)
    expect_equal(getTaskData(result1), getTaskData(result2))
    expect_equal(getTaskData(result1), getTaskData(filtered))
  }
  filter.list.regr = as.character(filter.list$id)[!filter.list$task.classif & filter.list$task.regr]
  for (filter in filter.list.regr) {
    if (filter %in% c("randomForestSRC.rfsrc", "randomForestSRC.var.select")) {
      next
    }
    set.seed(123)
    result1 = regr.num.task %>>% cpoFilterFeatures(method = filter, perc = 0.5)
    result2 = regr.num.task %>>% retrafo(result1)
    set.seed(123)
    filtered = filterFeatures(task = regr.num.task, method = filter, perc = 0.5)
    expect_equal(getTaskData(result1), getTaskData(result2))
    expect_equal(getTaskData(result1), getTaskData(filtered))
  }

})

test_that("specialised CPOs work", {
  specd = listCPO()[listCPO()$category == "featurefilter" & listCPO()$subcategory == "specialised", "name"]
  for (filter in specd) {
    cpoconst = get(filter)
    cpo = cpoconst(perc = 0.5)
    if (!"classif" %in% getCPOProperties(cpo)$properties ||
      getCPOName(cpo) %in% c("permutation.importance", "randomForestSRC.rfsrc", "randomForestSRC.var.select")) {
      # permutation.importance is missing an argument; the other two randomly crash on my machine.
      next
    }
    set.seed(123)
    result1 = multiclass.task %>>% cpo
    result2 = multiclass.task %>>% retrafo(result1)
    set.seed(123)
    filtered = filterFeatures(task = multiclass.task, method = getCPOName(cpo), perc = 0.5)
    expect_equal(getTaskData(result1), getTaskData(result2))
    expect_equal(getTaskData(result1), getTaskData(filtered))
  }

  for (filter in specd) {
    cpoconst = get(filter)
    cpo = cpoconst(perc = 0.5)
    if (!"regr" %in% getCPOProperties(cpo)$properties ||
      getCPOName(cpo) %in% c("permutation.importance", "randomForestSRC.rfsrc", "randomForestSRC.var.select")) {
      next
    }
    set.seed(123)
    result1 = regr.num.task %>>% cpo
    result2 = regr.num.task %>>% retrafo(result1)
    set.seed(123)
    filtered = filterFeatures(task = regr.num.task, method = getCPOName(cpo), perc = 0.5)
    expect_equal(getTaskData(result1), getTaskData(result2))
    expect_equal(getTaskData(result1), getTaskData(filtered))
  }
})
