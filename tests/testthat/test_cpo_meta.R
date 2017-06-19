
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

test_that("cpoMeta", {

  expect_set_equal(names(getParamSet(cpoMeta(.export = list(cpoScale(id = "a"), cpoPca(id = "b")), cpo.build = { a })())$pars),
    c("a.center", "a.scale", "b.center", "b.scale"))
  expect_class(cpoMeta(.export = list(a = cpoScale, b = cpoScale), cpo.build = { a }), "CPOConstructor")
  expect_error(cpoMeta(.export = list(a = cpoScale(id = "a"), a = cpoScale(id = "b")), cpo.build = { a }), "uniquely named")


  expect_equal(getTaskData(iris.task %>>% cpoMeta(.export = list(cpoScale(id = "a"), cpoPca(id = "b")), cpo.build = { a })()),
    getTaskData(iris.task %>>% cpoScale()))
  expect_equal(getTaskData(iris.task %>>% cpoMeta(.export = list(cpoScale(id = "a", center = FALSE), cpoPca(id = "b")), cpo.build = { a })()),
    getTaskData(iris.task %>>% cpoScale(center = FALSE)))


  multiplex.emu = cpoMeta(selected.cpo = "a": discrete[a, b], .export = list(a = cpoScale(center = FALSE), b = cpoPca(center = FALSE, scale = FALSE, id = "pcaX")),
    cpo.build = { switch(selected.cpo, a = a, b = b) })

  expect_equal(getTaskData(iris.task %>>% setHyperPars(multiplex.emu(), selected.cpo = "b", pcaX.scale = TRUE)),
    getTaskData(iris.task %>>% cpoPca(center = FALSE, scale = TRUE)))


  # properties
  testa = makeCPO("testa", .properties = c("numerics", "missings"),
    .properties.adding = "missings", .properties.needed = "ordered", cpo.trafo = { }, cpo.retrafo = NULL)
  testb = makeCPO("testb", .properties = c("numerics", "factors"),
    .properties.adding = "factors", .properties.needed = c("missings", "ordered"), cpo.trafo = { }, cpo.retrafo = NULL)


  newprops = getCPOProperties(cpoMeta(.export = list(a = testa, b = testb), cpo.build = { a })())

  expect_set_equal(intersect(newprops$properties, cpo.dataproperties), c("numerics", "factors", "missings"))

  expect_set_equal(newprops$properties.adding, c("missings", "factors"))

  expect_set_equal(newprops$properties.needed, "ordered")


  ta = makeCPO("testa", .properties.target = c("classif", "twoclass"), .stateless = TRUE, cpo.trafo = NULL, cpo.retrafo = { data })
  tb = makeCPO("testb", .properties.target = c("classif", "oneclass"), .stateless = TRUE, cpo.trafo = NULL, cpo.retrafo = { data })

  expect_set_equal(getCPOProperties(cpoMeta(.export = list(a = ta, b = tb), cpo.build = { a })())$properties,
    c(cpo.dataproperties, cpo.predict.properties, "classif", "oneclass", "twoclass"))


  # data split
  for (split in c("task", "no", "target", "most", "all", "factor", "numeric", "ordered", "onlyfactor")) {

    checking.cpo = cpoMeta(hastarget: logical, .cpo.name = "checkingcpo", .datasplit = split, cpo.build = function(data, target, hastarget) {
      switch(split,
        task = {
          if (hastarget) {
            expect_equal(data, cpo.df5c)
          } else {
            expect_equal(data, makeClusterTask(getTaskId(data), cpo.df5))
          }
        },
        no = {
          expect_equal(data, cpo.df5)
        },
        target = {
          if (hastarget) {
            expect_equal(data, dropNamed(cpo.df5, names(target)))
          } else {
            expect_equal(data, cpo.df5)
          }
        },
        most = {
          expect_equal(data$numeric, cpo.df5[c(1, 4, 7)])
        },
        all = {
          expect_equal(data$numeric, cpo.df5[c(1, 4, 7)])
        },
        factor = {
          exp = cpo.df5[c(2, 3, 5, 6, 8, 9)]
          if (hastarget) {
            exp = dropNamed(exp, names(target))
          }
          expect_equal(data, exp)
        },
        numeric = {
          expect_equal(data, cpo.df5[c(1, 4, 7)])
        },
        ordered = {
          exp = cpo.df5[c(3, 6, 9)]
          expect_equal(data, exp)
        },
        onlyfactor = {
          exp = cpo.df5[c(2, 5, 8)]
          if (hastarget) {
            exp = dropNamed(exp, names(target))
          }
          expect_equal(data, exp)
        },
        stop("Unexpected split"))
      NULLCPO
    })

    cpo.df5c %>>% checking.cpo(TRUE)
    cpo.df5 %>>% checking.cpo(FALSE)

  }

  # data dependent cpo
  cpo = cpoMeta(logical.param: logical,
  .export = list(a = cpoScale(id = "scale"), b = cpoPca(id = "pca", scale = FALSE, center = FALSE)),
  cpo.build = function(data, target, logical.param, a, b) {
    assert(is.nullcpo(retrafo(data)))
    if (logical.param || mean(data[[1]]) > 10) {
      a %>>% b
    } else {
      b %>>% a
    }
  })

  iris.scale.pca = iris %>>% cpoScale() %>>% cpoPca(scale = FALSE, center = FALSE)
  iris.pca.scale = iris %>>% cpoPca(scale = FALSE, center = FALSE) %>>% cpoScale()
  retrafo(iris.scale.pca) = NULL
  retrafo(iris.pca.scale) = NULL

  bigiris = iris %>>% cpomultiplier.nt.o(factor = 1000)
  retrafo(bigiris) = NULL

  bigiris.scale.pca = bigiris %>>% cpoScale() %>>% cpoPca(scale = FALSE, center = FALSE)
  bigiris.pca.scale = bigiris %>>% cpoPca(scale = FALSE, center = FALSE) %>>% cpoScale()
  retrafo(bigiris.scale.pca) = NULL
  retrafo(bigiris.pca.scale) = NULL

  ip = iris %>>% cpo(TRUE)  # scale, then pca
  rip = iris %>>% retrafo(ip)
  retrafo(ip) = NULL
  expect_equal(ip, iris.scale.pca)
  expect_equal(rip, iris.scale.pca)

  ip = iris %>>% cpo(FALSE)  # pca, then scale
  rip = iris %>>% retrafo(ip)
  retrafo(ip) = NULL
  expect_equal(ip, iris.pca.scale)
  expect_equal(rip, iris.pca.scale)

  ip = bigiris %>>% cpo(FALSE)  # scale, then pca
  rip = bigiris %>>% retrafo(ip)
  retrafo(ip) = NULL
  expect_equal(ip, bigiris.scale.pca)
  expect_equal(rip, bigiris.scale.pca)

  ip = bigiris %>>% cpo(FALSE)  # scale, then pca, since mean(data[[1]]) is large
  rip = bigiris %>>% retrafo(ip)
  retrafo(ip) = NULL
  expect_equal(ip, bigiris.scale.pca)
  expect_equal(rip, bigiris.scale.pca)

})
