


splitColsByType = function(which = c("numeric", "factor", "ordered", "other"), data, types) {
  if (missing(types)) {
    types = vcapply(data, function(x) class(x)[1])
  }
  # types: may be a character of type names, then data can be something besides a data.frame, like just a vector of names or indices
  match.arg(which, several.ok = TRUE)
  factorsubset = c("factor", if (!"ordered" %in% which) "ordered")

  sapply(which, function(x) {
    subset = if (x == "other") {
               !types %in% c("integer", "numeric", "factor", "ordered")
             } else {
               types %in% switch(x,
                 numeric = c("integer", "numeric"),
                 factor = factorsubset,
                 ordered = "ordered")
             }
    data[subset]
  }, simplify = FALSE, USE.NAMES = TRUE)
}

# this performs no checks. possibly need to check that properties are adhered to
# in retrafo, must also check if the format is the same as during training
# 'possibly' here means: if not attached to a learner
splittask = function(task, datasplit = c("target", "most", "all", "no", "task")) {
  datasplit = match.arg(datasplit)


  if (datasplit %in% c("most", "all")) {
    splt = getTaskData(task, target.extra = TRUE)

  }

  switch(datasplit,
    task = list(data = task, target = getTaskTargetNames(task)),
    no = list(data = getTaskData(task), getTaskTargetNames(task)),
    target = list(data = getTaskData(task, target.extra = TRUE)$data,
     target = getTaskData(task, features = character(0))),  # want the target to always be a data.frame
    most = list(data = splitColsByType(c("numeric", "factor", "other"), splt$data),
      target = splt$target),
    all = list(data = splitColsByType(c("numeric", "factor", "ordered", "other"), splt$data),
      target = splt$target))
}

splitdf = function(df, datasplit = c("target", "most", "all", "no", "task")) {
  datasplit = match.arg(datasplit)
  switch(datasplit,
    task = list(data = makeClusterTask(data = df), target = character(0)),
    no = list(data = df, character(0)),
    target = list(data = df, target = df[, character(0), drop = FALSE]),
    most = list(data = splitColsByType(c("numeric", "factor", "other"), df),
      target = df[, character(0), drop = FALSE]),
    all = list(data = splitColsByType(c("numeric", "factor", "ordered", "other"), df),
      target = df[, character(0), drop = FALSE]))
}

recombineLL = function(olddata, allnames, targetdata, name) {
# need 'allnames' to preserve ordering of target columns within whole DF
  tnames = names(targetdata)

  dfs = sapply(newdata, is.data.frame)
  if (any(!dfs)) {
    is.plur = sum(!dfs) > 1
    stopf("Return of %s element%s %s %s not a data.frame.", name, ifelse(is.plur, "s", ""),
      collapse(names(dfs)[!dfs], sep = ", "), ifelse(is.plur, "are", "is"))
  }

  # check no new names clash with other names
  # this kind of sucks when a CPO just happens to change the names to something thats already there
  # but we also don't want to surprise the user about us unilaterally changing names, so he needs to
  # take care of that.
  allnames = c(tnames, unlist(lapply(newdata, names)))
  if (any(duplicated(allnames))) {
    stopf("CPO %s gave bad result\nduplicate column names %s", name, collapse(unique(allnames[duplicated(allnames)], sep = ", ")))
  }

  types = vcapply(olddata, function(x) class(x)[1])

  splitnames = splitColsByType(names(newdata), names(olddata), types) # list(numeric = [colnames], factor = [colnames]...

  numrows = nrow(olddata)
  namesorder = allnames
  for (splittype in names(splitnames)) {
    if (numrows != nrow(newdata[[splittype]])) {
      stopf("Number of rows of %s data returned by %s did not match input\nCPO must not change row number.",
        splittype, name)
    }
    if (!identical(splitnames[[splittype]], names(newdata[[splittype]]))) {
        namesorder = setdiff(originalorder, c(splitnames[[splittype]], tnames))
        namesorder = c(namesorder, c(names(newdata[[splittype]]), tnames))
    }
  }

  newdata = cbind(do.call(cbind, unname(newdata)), targetdata)
  assertSetEqual(names(newdata), namesorder)
  newdata[namesorder]
}

# this checks that the result has the proper type, that target and type didn't change
# (if datasplit == "task"), and that the number of rows is the same.
# checking that the properties are adhered to must also happen (somewhere else)
# in retrafo functions, it must also be checked that the number of columns matches the one
# seen after trafo.
recombinetask = function(task, newdata, datasplit = c("target", "most", "all", "no", "task"), name) {
  datasplit = match.arg(datasplit)

  assertTargetsEqual = function(old.targets, new.targets) {
    assertSetEqual(names(old.targets), names(new.targets))
    for (n in names(old.targets)) {
      if (any(old.targets[[n]] != new.targets[[n]])) {
        stopf("CPO %s must not change target, but changed %s.", name, n)
      }
    }
  }


  if (datasplit %in% c("no", "task")) {
    if (datasplit == "no") {
      if (!is.data.frame(newdata)) {
        stopf("CPO %s cpo.trafo gave bad result\ncpo.trafo must return a data.frame.", name)
      }
      assertClass(newdata, "data.frame")
      newdata = changeData(task, newdata)
    }
    assertClass(newdata, "Task")
    #check type didn't change
    assert(getTaskType(task) == getTaskType(newdata))

    # check target didn't change
    assertTargetsEqual(getTaskData(task, features = character(0)),
      getTaskData(newdata, features = character(0)))

    # check most of task description didn't change
    allowed.td.changes = c("id", "n.feat", "has.missings")
    old.td = getTaskDesc(task)
    new.td = getTaskDesc(newdata)
    assertSetEqual(names(old.td), names(new.td))
    for (n in names(old.td)) {  # implicitly checks row number
      if (!n %in% allowed.td.changes && !identical(old.td[[n]], new.td[[n]])) {
        stopf("CPO %s changed task description item %s.", name, n)
      }
    }

    return(task)
  }
  if (datasplit == "target") {
    if (!is.data.frame(newdata)) {
      stopf("CPO %s gave bad result\nmust return a data.frame.", name)
    }
    if (nrow(newdata) != getTaskDescription(task)$size) {
      stopf("CPO %s must not change number of rows.", name)
    }
    newdata = cbind(newdata, getTaskData(task, features = character(0)))
    if (identical(names(newdata), getTaskFeatureNames(task))) {
      # names didn't change, so we preserve column order
      newdata = newdata[names(task$env$data)]
    }

  } else {
    # datasplit is 'most' or 'all'
    assertSetEqual(names(newdata), c("numeric", "factor", "other", if (datasplit == "all") "ordered"))

    target = getTaskData(task, features = character(0))

    newdata = recombineLL(getTaskData(task, target.extra = TRUE)$data,
      names(task$env$data), target, name)
  }
  changeData(task, newdata)
}

# this assumes we that if 'datasplit' was task, it was created from a target-less task.
recombinedf = function(df, newdata, datasplit = c("target", "most", "all", "no", "task"), name) {
  datasplit = match.arg(datasplit)
  assertSetEqual(names(newdata), c("numeric", "factor", "other", if (datasplit == "all") "ordered"))
  if (datasplit == "task") {
    assertClass(newdata, "Task")
    getTaskData(newdata)
  } else if (datasplit %in% c("most", "all")) {
    return(recombineLL(df, names(df), df[, character(0), drop = FALSE], name))
  } else {
    if (nrow(df) != nrow(newdata)) {
      stopf("CPO %s must not change number of rows.", name)
    }
    newdata
  }
}


# prepare some information about the data shape, so retrafo can check that
# it gets the kind of data it expects
# this needs to be checked both for input and for output
makeShapeInfo = function(data) {
  # expects a data.frame
  prep.info = list()
  prep.info$colnames = colnames(data)
  prep.info$coltypes = vcapply(data, function(x) class(x)[1])
  prep.info
}

# like makeShapeInfo, but additionally get the target names
makeInputShapeInfo = function(indata) {
  if ("Task" %in% class(indata)) {
    target = getTaskTargetNames(indata)
    indata = getTaskData(indata, target.extra = TRUE)$data
    ret = makeShapeInfo(indata)
    ret$target = target
  } else {
    ret = makeShapeInfo(indata)
  }
  ret
}

# called before recombinetask for better error messages later
# it is recommended to call this after recombinetask was called
# (but with the not yet recombined data)
# since that checks that outdata has the correct types.
makeOutputShapeInfo = function(outdata) {
  if (is.data.frame(outdata)) {
    makeShapeInfo(outdata)
  } else if ("Task" %in% class(outdata)) {
    makeShapeInfo(getTaskData(outdata))
  } else {
    # data is split by type, so we get the shape of each of the constituents
    lapply(outdata, makeShapeInfo)
  }
}

# give error when shape is different than dictated by shapeinfo.
assertShapeConform(df, shapeinfo, checkordered, name) {
  assertSubsetEqual(names(indata), shapeinfo$colnames)
  indata = indata[shapeinfo$colnames]

  if (checkordered) {
    typesmatch = list(
        c("integer", "numeric"),
        "factor", "ordered")
  } else {
    typesmatch = list(
        c("integer", "numeric"),
        c("factor", "ordered"))
  }

  newcoltypes = vcapply(indata, function(x) class(x)[1])

  for (t in typesmatch) {
    typemismatch = (newcoltypes %in% t) != (newcoltypes %in% shapeinfo$coltypes)
    if (any(typemismatch)) {
      stopf("Error in CPO %s: Types of column%s %s mismatch.", name,
        ifelse(sum(typemismatch) > 1, "s", ""), collapse(names(indata)[typemismatch], sep = ", "))
    }
  }
}

checkRetrafoInput = function(indata, checkordered, shapeinfo, name) {
  # check that input column names and general types match (num / fac, or num/fac/ordered if datasplit == "all"
  #
  # how does mlr predict handle this stuff? they just drop target columns by name

  if ("Task" %in% class(indata)) {
    if (!is.null(shapeinfo$target)) {
      assertSetEqual(getTaskTargetNames(indata), shapeinfo$target)
    }
    indata = getTaskData(indata)
  }
  if (!is.data.frame(indata)) {
    stopf("Data fed into CPO %s retrafo is not a data.frame.", name)
  }

  assertShapeConform(indata, shapeinfo, checkordered, name)

}

# check the shape of outdata is as expected. Does not recombine the task.
# should be called after recombinetask was called (but with the not-combined data)
# because that one gives info about type errors etc.
assertRetrafoOutput = function(outdata, datasplit, shapeinfo, name) {
  if (datasplit %in% c("all", "most")) {
    assertSetEqual(names(outdata), names(shapeinfo))
    for (n in names(outdata)) {
      assertShapeConform(outdata[[n]], shapeinfo[[n]], datasplit == "all", name)
    }
  }
  invisible(NULL)
}

# data can be a task or a data.frame
# description should be something like 'data going into / coming out of %NAME%'
checkDataConformsProperties(data, allowed.properties, description) {
  if (is.data.frame(data)) {
    td = makeTaskDescInternal(NULL, NULL, data, character(0), NULL, NULL)
  } else {
    assertClass(data, "Task")
    td = getTaskDesc(data)
  }
  nf = td$n.feat
  present.properties = c(names(nf)[nf > 0], if (td$has.missings) "missings")
  assertSubset(present.properties, allowed.properties, .var.name = description)
}

# do the preparation before calling trafo:
#  - check the data is in an acceptable format (task or df)
#  - check the properties are fulfilled
#  - split the data
#  - get a shape info object
#  --> return list(indata = list(data, target), shapeinfo)
prepareTrafoInput = function(indata, datasplit, allowed.properties, name) {
  assert(checkClass(indata, "data.frame"), checkClass(indata, "Task"))

  checkDataConformsProperties(indata, allowed.properties, sprintf("Data going into %s trafo", name))

  shapeinfo = makeInputShapeinfo(indata)

  indata = if (is.data.frame(indata)) {
    splitdf(indata, datasplit)
  } else {
    splittask(indata, datasplit)
  }
  list(indata, shapeinfo)
}

# do the preparation before calling retrafo:
#  - check data is in an acceptable format (task or df)
#  - check the properties are fulfilled
#  - check the shape is the same as during trafo
#  - split the data
#  --> return the data in a shape fit to be fed into retrafo
prepareRetrafoInput = function(indata, datasplit, allowed.properties, shapeinfo.input, name) {

  checkRetrafoInput(indata, datasplit == "all", shapeinfo.input, name)

  checkDataConformsProperties(indata, allowed.properties, sprintf("Data going into %s retrafo", name))

  if (datasplit %in% c("most", "all")) {
    splitinto = c("numeric", "factor", "other", if (datasplit == "all") "ordered")
    splitColsByType(splitinto, indata)
  } else {
    indata
  }
}

# do the check of the trafo's return value
#  - check the data is in an acceptable format (task, df, split dfs)
#  - recombine into a task / df
#  - check properties are allowed
#  - get a shape info object
#  --> return list(outdata, shapeinfo)
handleTrafoOutput(outdata, olddata, datasplit, allowed.properties, name) {
  recombined = if (is.data.frame(olddata)) {
    recombinedf(olddata, outdata, datasplit, name)
  } else {
    recombinetask(olddata, outdata, datasplit, name)
  }
  checkDataConformsProperties(recombined, allowed.properties, sprintf("%s trafo return value", name))

  shapeinfo = makeOutputShapeInfo(olddata)

  list(outdata = recombined, shapeinfo = shapeinfo)
}

# do the check of the retrafo's return value
#  - check data is in an acceptable format (task, df, split dfs)
#  - check the properties are fulfilled
#  - check the shape is the same as during trafo
#  - recombine into a task / df
#  --> return the data in a shape fit to be fed into retrafo
handleRetrafoOutput = function(outdata, olddata, datasplit, allowed.properties, shapeinfo.output, name) {
  recombined = if (is.data.frame(olddata)) {
    recombinedf(olddata, outdata, datasplit, name)
  } else {
    recombinetask(olddata, outdata, datasplit, name)
  }

  checkDataConformsProperties(recombined, allowed.properties, sprintf("%s retrafo return value", name))

  assertRetrafoOutput(outdata, datasplit, shapeinfo.output, name)

  recombined
}




# test that:
#  changing some of the columns leaves the others in order
#  change of target gives error
#  duplicate introduced name gives error
#  new task is actually changed, has the expected data

# training with data.frame, predicting with task, etc.
#
