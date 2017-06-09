

##################################
### Externals                  ###
##################################

# do the preparation before calling trafo:
#  - check the data is in an acceptable format (task or df)
#  - check the properties are fulfilled
#  - split the data
#  - get a shape info object
#  --> return list(indata = list(data, target), shapeinfo, properties)
prepareTrafoInput = function(indata, datasplit, allowed.properties, name) {
  assert(checkClass(indata, "data.frame"), checkClass(indata, "Task"))

  present.properties = getDataProperties(indata)
  assertSubset(present.properties, allowed.properties, .var.name = sprintf("Data going into %s trafo", name))
  shapeinfo = makeInputShapeInfo(indata)

  indata = if (is.data.frame(indata)) {
    splitdf(indata, datasplit)
  } else {
    splittask(indata, datasplit)
  }
  list(indata = indata, shapeinfo = shapeinfo, properties = present.properties)
}

# do the preparation before calling retrafo:
#  - check data is in an acceptable format (task or df)
#  - check the properties are fulfilled
#  - check the shape is the same as during trafo
#  - split the data
#  --> return list(data in a shape fit to be fed into retrafo, properties)
# how does mlr predict handle this stuff? they just drop target columns by name
prepareRetrafoInput = function(indata, datasplit, allowed.properties, shapeinfo.input, name) {

  # check that input column names and general types match (num / fac, or num/fac/ordered if datasplit == "all"
  if ("Task" %in% class(indata)) {
    if (length(shapeinfo.input$target) && length(getTaskTargetNames(indata))) {
      # FIXME: this might be too strict: maybe the user wants to retrafo a Task with the target having a different name?
      # HOWEVER, then either the training data's task didnt matter (and he should have trained with a data.set?), or it
      #  DID matter, in which case it is probably important to have the same data type <==> target name
      assertSetEqual(getTaskTargetNames(indata), shapeinfo.input$target, .var.name = sprintf("Target names of Task %s", getTaskId(indata)))
    }
    indata = getTaskData(indata, target.extra = TRUE)$data
  } else {
    shapeinfo.input$target = NULL
  }
  if (!is.data.frame(indata)) {
    stopf("Data fed into CPO %s retrafo is not a Task or data.frame.", name)
  }

  assertShapeConform(indata, shapeinfo.input, datasplit == "all", name)

  present.properties = getDataProperties(indata)
  assertSubset(present.properties, allowed.properties, .var.name = sprintf("Data going into %s retrafo", name))

  if (datasplit %in% c("most", "all")) {
    splitinto = c("numeric", "factor", "other", if (datasplit == "all") "ordered")
    indata = splitColsByType(splitinto, indata)
  }
  list(indata = indata, properties = present.properties)
}

# do the check of the trafo's return value
#  - check the data is in an acceptable format (task, df, split dfs)
#  - recombine into a task / df
#  - check properties are allowed
#  - get a shape info object
#  --> return list(outdata, shapeinfo)
handleTrafoOutput = function(outdata, olddata, datasplit, allowed.properties, name) {
  recombined = if (is.data.frame(olddata)) {
    recombinedf(olddata, outdata, datasplit, name)
  } else {
    recombinetask(olddata, outdata, datasplit, name)
  }

  present.properties = getDataProperties(recombined)
  assertSubset(present.properties, allowed.properties, .var.name = sprintf("%s trafo return value", name))
  if (datasplit %in% c("no", "task")) {
    # in this case, take shape info with 'target' separated
    shapeinfo = makeOutputShapeInfo(recombined)
  } else {
    shapeinfo = makeOutputShapeInfo(outdata)
  }

  list(outdata = recombined, shapeinfo = shapeinfo)
}

# do the check of the retrafo's return value
#  - check data is in an acceptable format (task, df, split dfs)
#  - recombine into a task / df
#  - check the properties are fulfilled
#  - check the shape is the same as during trafo
#  --> return the data that can be returned by the outer retrafo layer
handleRetrafoOutput = function(outdata, olddata, datasplit, allowed.properties, shapeinfo.output, name) {
  if (datasplit %in% c("no", "task")) {
    # target is always split off during retrafo
    datasplit = "target"
  }
  recombined = if (is.data.frame(olddata)) {
    recombinedf(olddata, outdata, datasplit, name)
  } else {
    recombinetask(olddata, outdata, datasplit, name)
  }

  present.properties = getDataProperties(recombined)
  assertSubset(present.properties, allowed.properties, .var.name = sprintf("%s retrafo return value", name))

  # check the shape of outdata is as expected
  if (datasplit %in% c("all", "most")) {
    assertSetEqual(names(outdata), names(shapeinfo.output))
    for (n in names(outdata)) {
      assertShapeConform(outdata[[n]], shapeinfo.output[[n]], datasplit == "all", name)
    }
  }

  recombined
}

##################################
### Shape & Properties         ###
##################################

# calculate the properties of the data (only feature types & missings)
# data can be a task or data.frame
getDataProperties = function(data) {
  if (is.data.frame(data)) {
    td = makeTaskDescInternal(NULL, NULL, data, character(0), NULL, NULL)
  } else {
    assertClass(data, "Task")
    td = getTaskDesc(data)
  }
  nf = td$n.feat
  c(names(nf)[nf > 0], if (td$has.missings) "missings")
}

# calculate the properties, properties.adding and properties.needed for a composed CPO
# CPO1 %>>% CPO2
# returns a list(properties, properties.adding, properties.needed)
compositeProperties = function(properties.1, properties.adding.1, properties.needed.1,
                               properties.2, properties.adding.2, properties.needed.2,
                               name1, name2) {
  # some explanation about properties:
  # * 'properties' are the properties that a CPO can handle.
  # * 'properties.adding' are the properties it adds to the things coming after it, it is therefore
  #   the things it *removes* from a dataset. E.g. if it removes 'missings' from data, it adds the property
  #   'missings' to the pipeline.
  # * properties.needed are the properties it needs from the things coming after it, this are the
  #   the things it *adds* to a dataset. E.g. if it converts numerics to factors, it 'needs' the learner /
  #   CPOs coming after it to have the property 'factors'.

  # The conditions on the properties are:
  # A) properties.adding is a subset of properties
  # B) properties.adding and properties.needed have no common elements
  # (these should be checked upon creation of a CPO)

  # When composing two CPOs (CPO1 %>>% CPO2), there is an additional requirement:
  # * properties.needed.1 is a subset of properties.2
  missing.properties = setdiff(properties.needed.1, properties.2)
  if (length(missing.properties)) {
    stopf("CPO %s creates data with propert%s %s that %s can not handle.",
      name1, ifelse(length(missing.properties) > 1, "ies", "y"),
      collapse(missing.properties, sep = ", "),
      name2)
  }

  # The properties of the new CPO are obtained thus:
  properties.composite = intersect(properties.1, union(properties.2, properties.adding.1))
  properties.adding.composite = union(setdiff(properties.adding.1, properties.needed.2), intersect(properties.1, properties.adding.2))
  properties.needed.composite = union(setdiff(properties.needed.1, properties.adding.2), properties.needed.2)

  # Proofs that conditions (A) and (B) are still fulfilled:
  # A) using distribution of union and intersect, and the fact that (cond (A)) intersect(properties.adding.1, properties.1) == properties.adding.1,
  #    we rewrite
  #      properties.composite = union(properties.adding.1, intersect(properties.1, properties.2))
  #    Now the first term in the union of properties.adding.composite is a subset of the first term of the union of properties.composite;
  #    same with the second term of both.
  # B) We show that intersect(properties.adding.composite, properties.needed.composite) is empty by showing that both terms in the union
  #    of properties.adding.composite have empty intersect each with both terms in the union of properties.needed.composite.
  #    1) intersect(properties.adding.1 - properties.needed.2, properties.needed.2) is empty because properties.needed.2 is subtracted from the lhs
  #    2) intersect(properties.adding.1 - properties.needed.2, properties.needed.1 - properties.adding.2) is empty because
  #       intersect(properties.adding.1, properties.needed.1) is empty per condition (B)
  #    3) intersect(intersect(properties.1, properties.adding.2), properties.needed.1 - properties.adding.2) is empty because properties.adding.2
  #       is subtracted from the rhs
  #    4) intersect(intersect(properties.1, properties.adding.2), properties.needed.2) is empty because properties.needed.2 and properties.adding.2
  #       have empty intersect per condition (B)
  list(properties = properties.composite, propserties.adding = properties.adding.composite, properties.needed = properties.needed.composite)
}

# give error when shape is different than dictated by shapeinfo.
assertShapeConform = function(df, shapeinfo, checkordered, name) {
  assertSetEqual(names(df), shapeinfo$colnames)
  indata = df[shapeinfo$colnames]

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
    typemismatch = (newcoltypes %in% t) != (shapeinfo$coltypes %in% t)
    if (any(typemismatch)) {
      stopf("Error in CPO %s: Types of column%s %s mismatch between training and test data.", name,
        ifelse(sum(typemismatch) > 1, "s", ""), collapse(names(indata)[typemismatch], sep = ", "))
    }
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
    makeShapeInfo(getTaskData(outdata, target.extra = TRUE)$data)
  } else {
    # data is split by type, so we get the shape of each of the constituents
    lapply(outdata, makeShapeInfo)
  }
}

##################################
### Task Splitting             ###
##################################

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
    no = list(data = getTaskData(task), target = getTaskTargetNames(task)),
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
    no = list(data = df, target = character(0)),
    target = list(data = df, target = df[, character(0), drop = FALSE]),
    most = list(data = splitColsByType(c("numeric", "factor", "other"), df),
      target = df[, character(0), drop = FALSE]),
    all = list(data = splitColsByType(c("numeric", "factor", "ordered", "other"), df),
      target = df[, character(0), drop = FALSE]))
}

##################################
### Task Recombination         ###
##################################

# 'LL' == low level
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

    return(newdata)
  }
  if (datasplit == "target") {
    if (!is.data.frame(newdata)) {
      stopf("CPO %s gave bad result\nmust return a data.frame.", name)
    }
    if (nrow(newdata) != getTaskDesc(task)$size) {
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
  if (datasplit == "task") {
    assertClass(newdata, "Task")
    getTaskData(newdata)
  } else if (datasplit %in% c("most", "all")) {
    assertSetEqual(names(newdata), c("numeric", "factor", "other", if (datasplit == "all") "ordered"))
    return(recombineLL(df, names(df), df[, character(0), drop = FALSE], name))
  } else {
    if (!is.data.frame(newdata)) {
      stopf("CPO %s gave bad result\nmust return a data.frame.", name)
    }
    if (nrow(df) != nrow(newdata)) {
      stopf("CPO %s must not change number of rows.", name)
    }
    newdata
  }
}

# test that:
#  changing some of the columns leaves the others in order
#  change of target gives error
#  duplicate introduced name gives error
#  new task is actually changed, has the expected data
# training with data.frame, predicting with task, etc.
#

# with split type, generating bad data:
#  no df
#  missing / too many names
#  row number mismatch
#
# attaching cpo that needs wrong properties (multi level)
# combining cpos with incompatible property requirements
# properties change after detaching preprocs / retrafos
#
# adding properties to data that shouldn't be added (in preproc / retrafo)
# composing CPOs with incompatible properties
