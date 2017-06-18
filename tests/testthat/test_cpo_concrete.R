
context("cpo concrete implementations")

test_that("cpoPca test", {

  ip = iris %>>% cpoPca()

  ret = retrafo(ip)
  retrafo(ip) = NULL


  expect_equal(iris %>>% ret, ip)
  hip = head(ip)
  row.names(hip) = row.names(hip)
  expect_equal(head(iris) %>>% ret, hip)

  prc = prcomp(iris[1:4])

  expect_equal(getRetrafoState(ret)$control$rotation, prc$rotation)

  expect_equal(getTaskData(iris.task %>>% cpoPca(center = FALSE, scale = TRUE), target.extra = TRUE)$data,
               as.data.frame(prcomp(iris[1:4], center = FALSE, scale. = TRUE)$x))

  expect_equal(getTaskData(iris.task %>>% cpoScale() %>>% cpoPca(center = FALSE, scale = FALSE)),
               getTaskData(iris.task %>>% cpoPca(center = TRUE, scale = TRUE)))

})

test_that("cpoScale test", {

  for (sets in list(list(FALSE, FALSE),
    list(TRUE, FALSE),
    list(FALSE, TRUE),
    list(TRUE, TRUE))) {
    ip = iris %>>% do.call(cpoScale, sets)
    ret = retrafo(ip)
    retrafo(ip) = NULL

    expect_equal(iris %>>% ret, ip)
    hip = head(ip)
    row.names(hip) = row.names(hip)
    expect_equal(head(iris) %>>% ret, hip)
  }



  scld = scale(iris[1:4])
  # last iteration of the loop above has both 'center' and 'scale' set to TRUE
  # otherwise, the following wouldn't work
  expect_equal(getRetrafoState(ret)$control$center, attr(scld, "scaled:center"))
  expect_equal(getRetrafoState(ret)$control$scale, attr(scld, "scaled:scale"))

  expect_equal(getTaskData(iris.task %>>% cpoScale(center = FALSE), target.extra = TRUE)$data,
               as.data.frame(scale(iris[1:4], center = FALSE)))
})

test_that("cpo multiplexer", {

  expect_error(cpoMultiplex(list(cpoScale, cpoScale)), "duplicates found")

  expect_equal(getTaskData(iris.task %>>% cpoMultiplex(list(cpoScale, cpoPca))),
               getTaskData(iris.task %>>% cpoScale()))
  expect_equal(getTaskData(iris.task %>>% cpoMultiplex(list(cpoScale(center = FALSE), cpoPca))),
               getTaskData(iris.task %>>% cpoScale(center = FALSE)))

  expect_equal(getTaskData(iris.task %>>% setHyperPars(cpoMultiplex(list(cpoScale(center = FALSE), cpoPca(center = FALSE, scale = FALSE, id = "pcaX"))),
                                                       selected.cpo = "pcaX", pcaX.scale = TRUE)),
               getTaskData(iris.task %>>% cpoPca(center = FALSE, scale = TRUE)))

  expect_equal(getParamSet(cpoMultiplex(list(a = cpoScale, b = cpoPca)))$pars$selected.cpo$values, list(a = "a", b = "b"))

  expect_equal(getHyperPars(cpoMultiplex(list(a = cpoScale, b = cpoPca)))$selected.cpo, "a")

  expect_error(setHyperPars(cpoMultiplex(list(a = cpoScale, b = cpoPca)), selected.cpo = "c"), "c is not feasible for parameter 'selected.cpo'")

  expect_error(cpoMultiplex(list(cpoScale(id = "pca"), cpoPca)), "duplicates found: pca")


  testa = makeCPO("testa", .properties = c("numerics", "missings"),
                  .properties.adding = "missings", .properties.needed = "ordered", cpo.trafo = { }, cpo.retrafo = NULL)
  testb = makeCPO("testb", .properties = c("numerics", "factors"),
                  .properties.adding = "factors", .properties.needed = c("missings", "ordered"), cpo.trafo = { }, cpo.retrafo = NULL)


  newprops = getCPOProperties(cpoMultiplex(list(testa, testb)))


  expect_set_equal(intersect(newprops$properties, cpo.dataproperties), c("numerics", "factors", "missings"))

  expect_set_equal(newprops$properties.adding, c("missings", "factors"))

  expect_set_equal(newprops$properties.needed, "ordered")

})

test_that("cpo applicator", {


  ip = iris %>>% cpoApply(cpoPca())

  ret = retrafo(ip)
  retrafo(ip) = NULL

  expect_equal(iris %>>% ret, ip)
  hip = head(ip)
  row.names(hip) = row.names(hip)
  expect_equal(head(iris) %>>% ret, hip)

  expect_equal(getTaskData(iris.task %>>% cpoPca()), iris %>>% ret)


  ip = iris %>>% cpoApply(cpoScale(center = FALSE))
  ret = retrafo(ip)
  retrafo(ip) = NULL

  expect_equal(iris %>>% ret, ip)
  hip = head(ip)
  row.names(hip) = row.names(hip)
  expect_equal(head(iris) %>>% ret, hip)

  expect_equal(getTaskData(iris.task %>>% cpoScale(center = FALSE)), iris %>>% ret)

})

test_that("cpo selector", {

names(iris)

  ip = iris %>>% cpoSelect(type = "factor", index = c(2, 1), names = c("Petal.Length", "Sepal.Width"))
  ret = retrafo(ip)
  retrafo(ip) = NULL

  expect_equal(iris %>>% ret, ip)

  expect_equal(iris %>>% ret, iris[c(2, 1, 3, 5)])

  expect_equal(names(iris %>>% cpoSelect(pattern = "Width")), c("Sepal.Width", "Petal.Width"))

  expect_error(iris %>>% cpoSelect(names = "nosuchcol"), "not found.*nosuchcol")

  expect_error(iris %>>% cpoSelect(index = 1000), "undefined columns selected")

})

test_that("cpo dummyencoder", {

  hi = head(iris)

  expected = hi
  expected$Species = NULL
  expected[paste0("Species", levels(iris$Species))] = 0
  expected$Speciessetosa = 1

  hip = hi %>>% cpoDummyEncode()
  ret = retrafo(hip)
  retrafo(hip) = NULL

  expect_equal(head(iris %>>% ret), hip)
  row.names(expected) = row.names(expected)
  expect_equal(hip, expected)

  hip2 = hi %>>% cpoDummyEncode(TRUE)
  expected$Speciessetosa = NULL
  retrafo(hip2) = NULL
  expect_equal(hip2, expected)

  hi2 = hi
  hi2$Species = factor(as.character(hi2$Species), levels = c("setosa", "versicolor"))
  hi3 = hi2
  hi3$Species = factor(as.character(hi3$Species), levels = c("versicolor", "setosa"))

  hi2p = hi2 %>>% cpoDummyEncode()
  ret2 = retrafo(hi2p)
  retrafo(hi2p) = NULL
  expect_equal(hi3 %>>% ret2, hi2p)

  expect_equal(hi %>>% ret2, hi2p)

  expect_equal(nrow(iris %>>% ret2), nrow(iris))

  it = makeRegrTask("iris2", iris, target = "Sepal.Length")

  nodumpred = predict(train("regr.lm", it), it)
  dumpred = predict(train(cpoDummyEncode(TRUE) %>>% makeLearner("regr.lm"), it), it)

  expect_equal(nodumpred$data, dumpred$data)

})
