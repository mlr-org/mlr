
context("cpo meta")

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


  ta = makeCPO("testa", .properties.target = c("classif", "twoclass"), .stateless = TRUE, cpo.trafo = NULL, cpo.retrafo = { data })
  tb = makeCPO("testb", .properties.target = c("classif", "oneclass"), .stateless = TRUE, cpo.trafo = NULL, cpo.retrafo = { data })

  expect_set_equal(getCPOProperties(cpoMultiplex(list(ta, tb)))$properties, c(cpo.dataproperties, cpo.predict.properties, "classif", "oneclass", "twoclass"))

})

test_that{"cpoMeta", {


})
