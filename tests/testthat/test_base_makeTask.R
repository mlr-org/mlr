test_that("makeXXTask ignores bad columns if check.data = FALSE", {
  constructTask = function(data, target, type, id) {
    constructor = switch(type,
      classif = makeClassifTask,
      multilabel = makeMultilabelTask,
      regr = makeRegrTask,
      surv = makeSurvTask)
    constructor(id = id, data = data, target = target, fixup.data = "no", check.data = FALSE)
  }

  badcoldf = data.frame(x = c(TRUE, TRUE, FALSE), y = c("x", "y", "z"),
    stringsAsFactors = FALSE)
  expect_logical(badcoldf[[1]])
  expect_character(badcoldf[[2]])

  constructors = list(
    classif = function(...) {
      makeClassifTask("test", cbind(badcoldf, target = "x"),
        target = "target", ...)
    },
    multilabel = function(...) {
      makeMultilabelTask("test", cbind(badcoldf,
        target1 = TRUE, target2 = FALSE), target = c("target1", "target2"), ...)
    },
    regr = function(...) {
      makeRegrTask("test", cbind(badcoldf, target = 1),
        target = "target", ...)
    },
    surv = function(...) {
      makeSurvTask("test", cbind(badcoldf, target1 = 1,
        target2 = FALSE), target = c("target1", "target2"), ...)
    },
    cluster = function(...) makeClusterTask("test", badcoldf, ...))

  for (type in names(constructors)) {
    expect_error(constructors[[type]](),
      "Unsupported feature type (logical) in column 'x'", fixed = TRUE)
  }
})
