
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

  for (sets in list(list(TRUE, TRUE),
    list(TRUE, FALSE),
    list(FALSE, TRUE),
    list(FALSE, FALSE))) {
    ip = iris %>>% do.call(cpoScale, sets)
    ret = retrafo(ip)
    retrafo(ip) = NULL

    expect_equal(iris %>>% ret, ip)
    hip = head(ip)
    row.names(hip) = row.names(hip)
    expect_equal(head(iris) %>>% ret, hip)
  }



  scld = scale(iris[1:4])

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

  ip = iris %>>% cpoSelect(type = "factor", index = c(2, 1))
  ret = retrafo(ip)
  retrafo(ip) = NULL

  expect_equal(iris %>>% ret, ip)

  expect_equal(iris %>>% ret, iris[c(2, 1, 5)])

  expect_equal(names(iris %>>% cpoSelect(pattern = "Width")), c("Sepal.Width", "Petal.Width"))

})

