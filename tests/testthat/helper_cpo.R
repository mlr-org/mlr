
# helper objects for cpo tests in test_base_cpo

# this list is written using '<<-' to communicate the state inside
# a function during execution to the "outside"
cpotest.parvals = list()

# simple learner that writes the first data element it receives to cpotest.parvals
testlearnercpo = makeRLearnerClassif("testlearnercpo", package = character(0), par.set = makeParamSet(makeUntypedLearnerParam("env", when = "both")),
  properties = c("twoclass", "multiclass", "numerics", "factors", "ordered"))
testlearnercpo$fix.factors.prediction = TRUE

trainLearner.testlearnercpo = function(.learner, .task, .subset, .weights = NULL, env, ...) {
  env$cpotest.parvals = c(env$cpotest.parvals, getTaskData(.task)[1, 1])
  getTaskData(.task, .subset)[[getTaskTargetNames(.task)[1]]][1]
}

predictLearner.testlearnercpo = function(.learner, .model, .newdata, env, ...) {
  env$cpotest.parvals = c(env$cpotest.parvals, .newdata[1, 1])
  rep(.model$learner.model, nrow(.newdata))
}

registerS3method("trainLearner", "testlearnercpo", trainLearner.testlearnercpo)
registerS3method("predictLearner", "testlearnercpo", predictLearner.testlearnercpo)

testlearnercpo = setHyperPars(testlearnercpo, env = environment(trainLearner.testlearnercpo))

# dummy regression learner
testregrcpo = makeRLearnerRegr("testregrcpo", package = character(0), par.set = makeParamSet(makeUntypedLearnerParam("env", when = "both")),
  properties = c("numerics", "factors", "ordered"))
testregrcpo$fix.factors.prediction = TRUE

trainLearner.testregrcpo = function(.learner, .task, .subset, .weights = NULL, ...) {
  getTaskData(.task, .subset)[[getTaskTargetNames(.task)[1]]][1]
}

predictLearner.testregrcpo = function(.learner, .model, .newdata, ...) {
  rep(1, nrow(.newdata))
}

registerS3method("trainLearner", "testregrcpo", trainLearner.testregrcpo)
registerS3method("predictLearner", "testregrcpo", predictLearner.testregrcpo)

# dummy clustering learner
testclustercpo = makeRLearnerCluster("testclustercpo", package = character(0), par.set = makeParamSet(makeUntypedLearnerParam("env", when = "both")),
  properties = c("numerics", "factors", "ordered"))
testclustercpo$fix.factors.prediction = TRUE

trainLearner.testclustercpo = function(.learner, .task, .subset, .weights = NULL, ...) {
  getTaskData(.task, .subset)[[getTaskTargetNames(.task)[1]]][1]
}

predictLearner.testclustercpo = function(.learner, .model, .newdata, ...) {
  rep(1L, nrow(.newdata))
}

registerS3method("trainLearner", "testclustercpo", trainLearner.testclustercpo)
registerS3method("predictLearner", "testclustercpo", predictLearner.testclustercpo)

# dummy multilabel learner
testmlcpo = makeRLearnerMultilabel("testmlcpo", package = character(0), par.set = makeParamSet(makeUntypedLearnerParam("env", when = "both")),
  properties = c("numerics", "factors", "ordered"))
testmlcpo$fix.factors.prediction = TRUE

trainLearner.testmlcpo = function(.learner, .task, .subset, .weights = NULL, ...) {
  getTaskTargetNames(.task)
}

predictLearner.testmlcpo = function(.learner, .model, .newdata, ...) {
  matrix(FALSE, nrow = nrow(.newdata), ncol = length(.model$learner.model))
}

registerS3method("trainLearner", "testmlcpo", trainLearner.testmlcpo)
registerS3method("predictLearner", "testmlcpo", predictLearner.testmlcpo)


# simple test data frame
testtaskcpo = makeClassifTask(data = data.frame(A = c(1, 2), B = factor(c("a", "b"))), target = "B")
testtaskcpo2 = makeClassifTask(data = data.frame(A = c(3, 4), B = factor(c("a", "b"))), target = "B")
testtaskcpo3 = makeClassifTask(data = data.frame(A = c(2, 4), B = factor(c("a", "b"))), target = "B")

# same data, but as data frame, without target
testdfcpo = data.frame(A = c(1, 2))
testdfcpo2 = data.frame(A = c(3, 4))
testdfcpo3 = data.frame(A = c(2, 4))

# cpoOPERATION.TASK.BACKEND
# OPERATION is multiplier or adder (adds or multiplies first column, subtracts / divides on retrafo)
# if TASK is present (being 'task'), the target column is verified to be the one from testtaskcpo
# BACKEND is 'f' for functional, 'o' for object


cpomultiplier.task.f = makeCPOFunctional("multiplierF", factor = 1: numeric[~., ~.], cpo.trafo = {
  expect_identical(data[[target]], factor(c("a", "b")))
  data[[1]] = data[[1]] * factor
  cpo.retrafo = function(data) {
    data[[1]] = data[[1]] / factor
    data
  }
  data
})

cpoadder.task.f = makeCPOFunctional("adderF", summand = 1: integer[, ], cpo.trafo = {
  expect_identical(data[[target]], factor(c("a", "b")))
  meandata = mean(data[[1]])
  data[[1]] = data[[1]] + summand
  cpo.retrafo = function(data) {
    data[[1]] = data[[1]] - summand - meandata
    data
  }
  data
})

cpomultiplier.task.o = makeCPOObject("multiplierO", factor = 1: numeric[~., ~.], cpo.trafo = {
  expect_identical(data[[target]], factor(c("a", "b")))
  data[[1]] = data[[1]] * factor
  control = 0
  data
}, cpo.retrafo = {
  data[[1]] = data[[1]] / factor
  data
})


cpoadder.task.o = makeCPOObject("adderO", summand = 1: integer[, ], cpo.trafo = {
  expect_identical(data[[target]], factor(c("a", "b")))
  control = mean(data[[1]])
  data[[1]] = data[[1]] + summand
  data
}, cpo.retrafo = {
  data[[1]] = data[[1]] - summand - control
  data
})



cpomultiplier.f = makeCPOFunctional("multiplierF", factor = 1: numeric[~., ~.], cpo.trafo = {
  if (length(target)) {
    expect_identical(data[[target]], factor(c("a", "b")))
  }
  data[[1]] = data[[1]] * factor
  cpo.retrafo = function(data) {
    data[[1]] = data[[1]] / factor
    data
  }
  data
})

cpoadder.f = makeCPOFunctional("adderF", summand = 1: integer[, ], cpo.trafo = {
  if (length(target)) {
    expect_identical(data[[target]], factor(c("a", "b")))
  }
  meandata = mean(data[[1]])
  data[[1]] = data[[1]] + summand
  cpo.retrafo = function(data) {
    data[[1]] = data[[1]] - summand - meandata
    data
  }
  data
})

cpomultiplier.o = makeCPOObject("multiplierO", factor = 1: numeric[~., ~.], cpo.trafo = {
  if (length(target)) {
    expect_identical(data[[target]], factor(c("a", "b")))
  }
  data[[1]] = data[[1]] * factor
  control = 0
  data
}, cpo.retrafo = {
  data[[1]] = data[[1]] / factor
  data
})


cpoadder.o = makeCPOObject("adderO", summand = 1: integer[, ], cpo.trafo = {
  if (length(target)) {
    expect_identical(data[[target]], factor(c("a", "b")))
  }
  control = mean(data[[1]])
  data[[1]] = data[[1]] + summand
  data
}, cpo.retrafo = {
  data[[1]] = data[[1]] - summand - control
  data
})

pss = paramSetSugar

cpogen = function(name, type = c("o", "f"), ps, trafo, retrafo, datasplit,
                  properties = c("numerics", "factors", "ordered", "missings"),
                  properties.adding = character(0), properties.needed = character(0)) {
  type = match.arg(type)
  if (type == "o") {
    makeCPOObject(name, .par.set = ps, .datasplit = datasplit,
      .properties = properties, .properties.adding = properties.adding, .properties.needed = properties.needed,
      cpo.trafo = trafo, cpo.retrafo = retrafo)
  } else {
    makeCPOFunctional(name, .par.set = ps, .datasplit = datasplit,
      .properties = properties, .properties.adding = properties.adding, .properties.needed = properties.needed,
      cpo.trafo = function(data, target, ...) {
        cpo.retrafo = function(data) {
          retrafo(data = data, control = control, ...)
        }
        trafo = captureEnvWrapper(trafo)
        res = trafo(data = data, target = target, ...)
        control = environment(trafo)$.ENV$control
        res
      })
  }
}



# expect training / test data format according to parameters
# expect training data to be of length equal to the first entry in the first column
generateCPO = function(type, split = c("no", "target", "most", "all", "task")) {
  split = match.arg(split)
  cpo = cpogen("splittest", type,
    pss(numrows: integer[0, ],
      numtarget: integer[0, ],
      numnumeric: integer[0, ] [[requires = quote(!isdf && !istask)]],
      numfactor: integer[0, ] [[requires = quote(!isdf && !istask)]],
      numother: integer[0, ] [[requires = quote(!isdf && !istask)]],
      numordered: integer[-1, ] [[requires = quote(!isdf && !istask)]],  # -1 for no 'numordered'
      isdf = (split %in% c("no", "target")): logical, istask = (split == "task"): logical, targetisdf = (!split %in% c("no", "task")): logical),
    function(isdf, istask, targetisdf, numrows, numnumeric, numfactor, numother, numordered, numtarget, data, target) {
      expect_equal(length(target), numtarget)
      if (istask) {
        expect_class(data, "Task")
        expect_class(target, "character")
        expect_equal(getTaskTargetNames(data), target)
        expect_equal(getTaskDesc(data)$size, numrows)
        expect_equal(sum(getTaskDesc(data)$n.feat), numnumeric + numfactor + numother + max(numordered, 0))
      } else if (isdf) {
        expect_class(data, "data.frame")
        if (targetisdf) {
          expect_class(target, "data.frame")
          expect_equal(nrow(target), numrows)
          target = names(target)
        } else {
          expect_class(target, "character")
          expect_subset(target, names(data))
        }
        expect_equal(nrow(data), numrows)
        expect_equal(ncol(data), numtarget * (!targetisdf) + numnumeric + numfactor + numother + max(numordered, 0))
      } else {
        expect_class(target, "data.frame")
        expect_equal(nrow(target), numrows)
        target = names(target)

        expectnames = c("numeric", "factor", "other", if (numordered >= 0) "ordered")
        expect_set_equal(expectnames, names(data))
        numbers = c(numeric = numnumeric, factor = numfactor, other = numother, ordered = numordered)
        lapply(names(data), function(n) {
          x = data[[n]]
          expect_class(x, "data.frame")
          expect_true(length(intersect(target, names(x))) == 0)
          expect_equal(nrow(x), numrows)
          expect_equal(ncol(x), numbers[[n]])
        })
      }
      control = target
      data
    },
    function(isdf, istask, targetisdf, numrows, numnumeric, numfactor, numother, numordered, numtarget, data, control) {
      if (isdf || istask) {
        expect_class(data, "data.frame")
        expect_true(length(intersect(control, names(data))) == 0)
        expect_equal(data[[1]][1], nrow(data))
        expect_equal(ncol(data), numnumeric + numfactor + numother + max(numordered, 0))
      } else {
        expectnames = c("numeric", "factor", "other", if (numordered >= 0) "ordered")
        expect_set_equal(expectnames, names(data))
        numbers = c(numeric = numnumeric, factor = numfactor, other = numother, ordered = numordered)
        lapply(names(data), function(n) {
          x = data[[n]]
          expect_true(length(intersect(control, names(x))) == 0)

          expect_class(x, "data.frame")
          expect_equal(nrow(x), nrow(data[[1]]))
          expect_equal(ncol(x), numbers[[n]])
        })
      }
      data
    }, datasplit = split)
}

cpo.df.numeric = data.frame(N1 = c(1, 2, 3), N2 = c(2L, 4L, 6L),  N3 = c(10L, 20L, 30L))
cpo.df.numeric2 = data.frame(N1 = c(3, 2, 1), N2 = c(0L, -1L, 1L), N3 = c(1, 2, 3))
cpo.df.numeric3 = data.frame(N4 = c(3, 2, 1), N5 = c(0L, -1L, 1L), N6 = c(1, 2, 3), N7 = c(2L, 4L, 6L),  N8 = c(10L, 20L, 30L))

cpo.df.factorial = data.frame(F1 = factor(c("a", "b", "a")), F2 = factor(c("b", "b", "c")))
cpo.df.factorial2 = data.frame(F1 = factor(c("b", "b", "a")), F2 = factor(c("b", "b", "c")))
cpo.df.factorial3 = data.frame(F3 = factor(c("b", "b", "a")))

cpo.df.logical = data.frame(T1 = c(TRUE, TRUE, FALSE), T2 = c(FALSE, TRUE, FALSE))
cpo.df.logical2 = data.frame(T1 = c(TRUE, TRUE, TRUE), T2 = c(FALSE, FALSE, FALSE))
cpo.df.logical3 = data.frame(T3 = c(TRUE, TRUE, TRUE), T4 = c(FALSE, FALSE, FALSE))

cpo.df.ordered = data.frame(O1 = factor(c("x", "x", "y"), ordered = TRUE),
  O2 = factor(c("y", "y", "z"), ordered = TRUE),
  O3 = factor(c("y", "y", "z"), ordered = TRUE))
cpo.df.ordered2 = data.frame(O1 = factor(c("x", "y", "x"), ordered = TRUE),
  O2 = factor(c("y", "z", "z"), ordered = TRUE),
  O3 = factor(c("y", "z", "y"), ordered = TRUE))

cpo.df.ordered3 = data.frame(O4 = factor(c("x", "y", "x"), ordered = TRUE), O5 = factor(c("y", "y", "y"), levels = c("y", "z"), ordered = TRUE))

cpo.df.other = data.frame(U1 = c("m", "n", "o"), U2 = c("p", "q", "r"), U3 = c("s", "t", "u"), U4 = c("v", "w", "w"), stringsAsFactors = FALSE)
cpo.df.other2 = data.frame(U1 = c("mx", "nx", "ox"), U2 = c("px", "qx", "rx"), U3 = c("sx", "tx", "ux"), U4 = c("vx", "wx", "wx"), stringsAsFactors = FALSE)
cpo.df.other3 = data.frame(U5 = c("mx", "nx", "ox"), U6 = c("px", "qx", "rx"), stringsAsFactors = FALSE)


cpo.df1 = cbind(cpo.df.numeric, cpo.df.factorial)
cpo.df2 = cbind(cpo.df.numeric, cpo.df.other, cpo.df.factorial, cpo.df.ordered)
cpo.df2 = cpo.df2[c(1, 4, 8, 12, 2, 5, 9, 11, 3, 6, 7, 10)]

cpo.df3 = cbind(cpo.df.numeric, cpo.df.logical, cpo.df.factorial)
cpo.df3 = cpo.df3[, c(1, 4, 6, 2, 3, 5, 7)]

cpo.df4 = cbind(cpo.df.numeric, cpo.df.logical, cpo.df.factorial, cpo.df.ordered)
cpo.df4 = cpo.df4[c(1, 4, 8, 2, 5, 9, 3, 6, 7, 10)]

cpo.df5 = cbind(cpo.df.numeric, cpo.df.factorial3, cpo.df.factorial, cpo.df.ordered)
cpo.df5 = cpo.df5[c(1, 4, 8, 2, 5, 9, 3, 6, 7)]

cpo.df1c = makeClassifTask(data = cpo.df1, target = "F1")
cpo.df1cc = makeClusterTask(data = cpo.df1)
cpo.df3l = makeMultilabelTask(data = cpo.df3, target = c("T1", "T2"))

cpo.df4l = makeMultilabelTask(data = cpo.df4, target = c("T1", "T2"))

cpo.df5c = makeClassifTask(data = cpo.df5, target = "F1")
cpo.df5cc = makeClusterTask(data = cpo.df5)

cpo.df4l2 = makeMultilabelTask(data = cbind(cpo.df4, cpo.df.logical3), target = c("T1", "T2", "T3", "T4"))
