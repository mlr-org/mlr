
cpo.dataproperties = c("numerics", "factors", "ordered", "missings")
cpo.tasktypes = c("cluster", "classif", "multilabel", "regr", "surv")  # these are the SUPPORTED tasks
cpo.targetproperties = c("oneclass", "twoclass", "multiclass", "lcens", "rcens", "icens")

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

  targets = if ("Task" %in% class(indata)) {
    getTaskTargetNames(indata)
  } else {
    character(0)
  }
  present.properties = getTaskProperties(indata)
  assertPropertiesOk(present.properties, allowed.properties, "trafo", "in", name)

  shapeinfo = makeInputShapeInfo(indata)

  indata = if (is.data.frame(indata)) {
    splitdf(indata, getLLDatasplit(datasplit))
  } else {
    splittask(indata, getLLDatasplit(datasplit))
  }
  list(indata = getIndata(indata, datasplit), shapeinfo = shapeinfo, properties = present.properties, tempdata = indata)
}


# do the preparation before calling retrafo:
#  - check data is in an acceptable format (task or df)
#  - check the properties are fulfilled
#  - check the shape is the same as during trafo
#  - split the data
#  --> return list(data in a shape fit to be fed into retrafo, properties)
# how does mlr predict handle this stuff? they just drop target columns by name
prepareRetrafoInput = function(indata, datasplit, allowed.properties, shapeinfo.input, name) {

  indata = prepareRetrafoData(indata, datasplit, allowed.properties, shapeinfo.input, name)$data

  lldatasplit = getLLDatasplit(datasplit)


  if (lldatasplit %in% c("most", "all")) {
    splitinto = c("numeric", "factor", "other", if (lldatasplit == "all") "ordered")
    indata = splitColsByType(splitinto, indata)
  }

  list(indata = getIndata(indata, datasplit), properties = present.properties, tempdata = indata)
}

# do the check of the trafo's return value
#  - check the data is in an acceptable format (task, df, split dfs)
#  - recombine into a task / df
#  - check properties are allowed
#  - get a shape info object
#  --> return list(outdata, shapeinfo)
handleTrafoOutput = function(outdata, olddata, tempdata, datasplit, allowed.properties, properties.adding, targetbound, convertto, name) {
  outdata = rebuildOutdata(outdata, tempdata, datasplit)
  datasplit = getLLDatasplit(datasplit)
  if (targetbound) {
    if (convertto == "surv") {
      censstyle = intersect(allowed.properties, c("lcens", "rcens", "icens"))
      assert(length(censstyle) == 1)
    }
    recombined = recombinetask(olddata, outdata, datasplit, TRUE, convertto, censstyle, name)
  } else {
    recombined = if (is.data.frame(olddata)) {
      recombinedf(olddata, outdata, datasplit, character(0), name)
    } else {
      recombinetask(olddata, outdata, datasplit, FALSE, name = name)
    }
  }

  present.properties = getTaskProperties(recombined)
  assertPropertiesOk(present.properties, allowed.properties, "trafo", "out", name)
  assertPropertiesOk(present.properties, setdiff(allowed.properties, properties.adding), "trafo", "adding", name)
  if (datasplit %in% c("no", "task")) {
    # in this case, take shape info with 'target' separated
    shapeinfo = makeOutputShapeInfo(recombined)
  } else {
    shapeinfo = makeOutputShapeInfo(outdata)
  }
  if ("Task" %in% class(olddata)) {
    shapeinfo$target = getTaskTargetNames(olddata)
  }

  list(outdata = recombined, shapeinfo = shapeinfo)
}

# do the check of the retrafo's return value
#  - check data is in an acceptable format (task, df, split dfs)
#  - recombine into a task / df
#  - check the properties are fulfilled
#  - check the shape is the same as during trafo
#  --> return the data that can be returned by the outer retrafo layer
handleRetrafoOutput = function(outdata, olddata, tempdata, datasplit, allowed.properties, properties.adding, shapeinfo.output, name) {
  outdata = rebuildOutdata(outdata, tempdata, datasplit)
  datasplit = getLLDatasplit(datasplit)
  if (datasplit %in% c("no", "task")) {
    # target is always split off during retrafo
    datasplit = "target"
  }
  targetcols = character(0)

  recombined = if (is.data.frame(olddata)) {
    if (any(shapeinfo.output$target %in% names(olddata))) {
      assert(all(shapeinfo.output$target %in% names(olddata)))  # we also check this in prepareRetrafoInput
      targetcols = shapeinfo.output$target
    }

    recombinedf(olddata, outdata, datasplit, targetcols, name)
  } else {
    recombinetask(olddata, outdata, datasplit, FALSE, name = name)
  }

  present.properties = getDataProperties(recombined, targetcols)
  assertPropertiesOk(present.properties, allowed.properties, "retrafo", "out", name)
  assertPropertiesOk(present.properties, setdiff(allowed.properties, properties.adding), "retrafo", "adding", name)

  # check the shape of outdata is as expected
  shapeinfo.output$target = NULL
  if (datasplit %in% c("all", "most")) {
    assertSetEqual(names(outdata), names(shapeinfo.output))
    for (n in names(outdata)) {
      assertShapeConform(outdata[[n]], shapeinfo.output[[n]], datasplit == "all", name)
    }
  } else {
    assertShapeConform(outdata, shapeinfo.output, FALSE, name)
  }

  recombined
}


# this is a subset of retrafo preparation which also needs to happen for target retrafo
# data prep.
# - split data into 'data' and 'target' DFs (latter possibly empty df)
# - test shapeinfo.input conformity
prepareRetrafoData = function(data, datasplit, allowed.properties, shapeinfo.input, name) {

  # check that input column names and general types match (num / fac, or num/fac/ordered if datasplit == "all"
  if ("Task" %in% class(data)) {
    if (length(shapeinfo.input$target) && length(getTaskTargetNames(data))) {
      # FIXME: this might be too strict: maybe the user wants to retrafo a Task with the target having a different name?
      # HOWEVER, then either the training data's task didnt matter (and he should have trained with a data.set?), or it
      #  DID matter, in which case it is probably important to have the same data type <==> target name
      assertSetEqual(getTaskTargetNames(data), shapeinfo.input$target, .var.name = sprintf("Target names of Task %s", getTaskId(data)))
    }
    target = getTaskData(data, features = character(0))
    data = getTaskData(data, target.extra = TRUE)$data
  } else {
    if (any(shapeinfo.input$target %in% names(data))) {
      if (!all(shapeinfo.input$target %in% names(data))) {
        badcols = intersect(shapeinfo.input$target, names(data))
        stopf("Some, but not all target columns of training data found in new data. This is probably an error.\n%s%s: %s",
          "Offending column", ifelse(length(badcols) > 1, "s", ""), collapse(badcols, sep = ", "))
      }
      data = dropNamed(data, shapeinfo.input$target)
      target = data[shapeinfo.input$target]
    } else {
      target = data[character(0)]
      shapeinfo.input$target = NULL
    }
  }
  if (!is.data.frame(data)) {
    stopf("Data fed into CPO %s retrafo is not a Task or data.frame.", name)
  }

  lldatasplit = getLLDatasplit(datasplit)

  assertShapeConform(data, shapeinfo.input, lldatasplit == "all", name)

  present.properties = getDataProperties(data, character(0))
  assertPropertiesOk(present.properties, allowed.properties, "retrafo", "in", name)

  list(data = data, target = target)
}


##################################
### Shape & Properties         ###
##################################

# calculate the properties of the data (only feature types & missings)
# data can be a task or data.frame
getDataProperties = function(data, targetnames) {
  if (is.data.frame(data)) {
    td = makeTaskDescInternal(NULL, NULL, data, targetnames, NULL, NULL)
  } else {
    assertClass(data, "Task")
    td = getTaskDesc(data)
  }
  nf = td$n.feat
  c(names(nf)[nf > 0], if (td$has.missings) "missings")
}

# calculate the properties of the data, as if it were a task.
# If data is a data.frame, we give it the property 'cluster'
# otherwise, we give it the propertye of the task type. If
# applicable, we also set oneclass, multiclass, etc (any from
# the variable 'cpo.targetproperties')
getTaskProperties = function(data) {
  if ("Task" %in% data) {
    targetnames = getTaskTargetNames(data)
  } else {
    targetnames = character(0)
  }
  props = getDataProperties(data, targetnames)
  if (is.data.frame(data)) {
    c(props, "cluster")
  } else {
    td = getTaskDesc(data)
    if (td$type == "classif") {
      others = switch(as.character(length(td$class.levels)),
        `1` = "oneclass", `2` = "twoclass", "multiclass")
    } else if (td$type == "surv") {
      others = td$censoring
    } else {
      others = NULL
    }
    c(props, td$type, others)
  }
}

# calculate the properties, properties.adding and properties.needed for a composed CPO
# CPO1 %>>% CPO2
# returns a list(properties, properties.adding, properties.needed)
compositeProperties = function(prop1, prop2, name1, name2) {
  properties.1 = prop1$properties
  properties.adding.1 = prop1$properties.adding
  properties.needed.1 = prop1$properties.needed
  properties.2 = prop2$properties
  properties.adding.2 = prop2$properties.adding
  properties.needed.2 = prop2$properties.needed
  assertCharacter(properties.1, unique = TRUE)
  assertCharacter(properties.2, unique = TRUE)
  assertCharacter(properties.adding.1, unique = TRUE)
  assertCharacter(properties.adding.2, unique = TRUE)
  assertCharacter(properties.needed.1, unique = TRUE)
  assertCharacter(properties.needed.2, unique = TRUE)
  assertString(name1)
  assertString(name2)
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
  list(properties = properties.composite, properties.adding = properties.adding.composite, properties.needed = properties.needed.composite)
}

# give error when shape is different than dictated by shapeinfo.
# wasresult: whether we are checking the result of retrafo
assertShapeConform = function(df, shapeinfo, checkordered, name) {
  if (!isTRUE(checkSetEqual(names(df), shapeinfo$colnames))) {
    stopf("Error in CPO %s: column name mismatch between training and test data.\nWas %s during training, is %s now.",
          name, collapse(shapeinfo$colnames, sep = ", "), collapse(names(df), sep = ", "))
  }
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
      plurs = ifelse(sum(typemismatch) > 1, "s", "")
      singes = ifelse(sum(typemismatch) > 1, "", "es")
      stopf("Error in CPO %s: Type%s of column%s %s mismatch%s between training and test data.", name,
        plurs, plurs, collapse(names(indata)[typemismatch], sep = ", "), singes)
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

# give userfriendly error message when needed properties are absent
assertPropertiesOk = function(present.properties, allowed.properties, whichfun, direction, name) {
  badprops = setdiff(present.properties, allowed.properties)
  if (length(badprops)) {
    if (direction == "in") {
      stopf("Data going into CPO %s has propert%s %s that %s can not handle.",
        whichfun, ifelse(length(badprops) > 1, "ies", "y"),
        collapse(badprops, sep = ", "), name)
    } else if (direction == "out") {
      stopf("Data returned by CPO %s has propert%s %s that %s did not declare in .properties.needed.",
        whichfun, ifelse(length(badprops) > 1, "ies", "y"),
        collapse(badprops, sep = ", "), name)
    } else {
      # 'adding' properties may not be present during output, but the error message
      # would be confusing if we used the 'out' message for this.
      assert(direction == "adding")
      stopf("Data returned by CPO %s has propert%s %s that %s declared in .properties.adding.\n%s",
        whichfun, ifelse(length(badprops) > 1, "ies", "y"),
        collapse(badprops, sep = ", "), name,
        paste("properties in .properties.adding may not be present in", whichfun, "output."))
    }
  }
}

##################################
### Task Splitting             ###
##################################

# most of CPOFormatCheck doesn't care about "factor", "onlyfactor", "ordered" or "numeric"
# so we translate those
getLLDatasplit = function(datasplit) {
  if (datasplit %in% c("factor", "numeric")) {
    datasplit = "most"
  } else if (datasplit %in% c("onlyfactor", "ordered")) {
    datasplit = "all"
  }
  datasplit
}

getIndata = function(indata, datasplit) {
  if (datasplit %in% c("factor", "onlyfactor", "ordered", "numeric")) {
    indata[[ifelse(datasplit == "onlyfactor", "factor", "datasplit")]]
  } else {
    indata
  }
}

rebuildOutdata = function(outdata, tempdata, datasplit) {
  if (datasplit %in% c("factor", "onlyfactor", "ordered", "numeric")) {
    tempdata[[ifelse(datasplit == "onlyfactor", "factor", "datasplit")]] = outdata
    outdata = tempdata
  }
  if (datasplit %in% c("numeric", "most", "all") && is.matrix(outdata$numeric)) {
    outdata$numeric = as.data.frame(outdata$numeric)
  }
  outdata
}


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
      target = getTaskData(task, features = character(0))),  # want the target to always be a data.frame
    all = list(data = splitColsByType(c("numeric", "factor", "ordered", "other"), splt$data),
      target = getTaskData(task, features = character(0))))  # want the target to always be a data.frame
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
recombineLL = function(olddata, newdata, targetnames, datasplit, name) {
  allnames = names(olddata)
  needednames = c("numeric", "factor", "other", if (datasplit == "all") "ordered")
  if (!isTRUE(checkSetEqual(names(newdata), needednames))) {
    stopf('CPO %s gave bad return. The returned value must be a list with names {"%s"}.',
      name, collapse(needednames, sep = '", "'))
  }

  targetdata = olddata[targetnames]
  olddata = dropNamed(olddata, targetnames)

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
  jointargetnames = c(targetnames, unlist(lapply(newdata, names)))
  if (any(duplicated(jointargetnames))) {
    stopf("CPO %s gave bad result\nduplicate column names %s", name, collapse(unique(jointargetnames[duplicated(jointargetnames)], sep = ", ")))
  }

  types = vcapply(olddata, function(x) class(x)[1])

  splitargetnames = splitColsByType(names(newdata), names(olddata), types) # list(numeric = [colnames], factor = [colnames]...

  numrows = nrow(olddata)
  namesorder = allnames
  for (splittype in names(splitargetnames)) {
    if (numrows != nrow(newdata[[splittype]])) {
      stopf("Number of rows of %s data returned by %s did not match input\nCPO must not change row number.",
        splittype, name)
    }
    if (!identical(splitargetnames[[splittype]], names(newdata[[splittype]]))) {
      namesorder = setdiff(namesorder, c(splitargetnames[[splittype]]))
      namesorder = c(namesorder, c(names(newdata[[splittype]])))
    }
  }

  newdata = cbind(do.call(cbind, unname(newdata)), targetdata)
  assertSetEqual(names(newdata), namesorder)
  newdata[namesorder]
}

# this checks that the result has the proper type, that target and type didn't change
# (if datasplit == "task"), and that the number of rows is the same.
# targetbound: TRUE or FALSE
# newtasktype: only if targetbound, type of new task. Give even if no task conversion happens.
# censtype only needed if newtasktype == surv
recombinetask = function(task, newdata, datasplit = c("no", "task", "target", "most", "all"),
                         targetbound, newtasktype, censtype, name) {
  datasplit = match.arg(datasplit)

  if (is.data.frame(task)) {
    # only if 'targetbound'
    task = makeClusterTask(id = "CPO-constructed", data = task)
  }

  if (datasplit %in% c("target", "most", "all")) {
    if (targetbound) {
      # return is just 'target' in a df.
      if (!is.data.frame(newdata)) {
        stopf("CPO %s gave bad result\nmust return a data.frame containing the target.",
          name)
      }
      olddata = getTaskData(task)
      oldtnames = getTaskTargetNames(task)
      newtnames = names(newdata)
      if (setequal(newtnames, oldtnames)) {
        olddata[newtnames] = newdata
        newdata = olddata
      } else if (length(oldtnames) == 1 && length(newdata) == 1) {
        # note that this can NOT be combined with
        # the olddata[newtnames] block above!
        # also note the double brackets [[ ]].
        olddata[[oldtnames]] = newdata
        newdata = olddata
      } else {
        newdata = cbind(dropNamed(olddata, oldtnames), newdata)
      }
      return(constructTask(task, newdata, newtnames, newtasktype, getTaskId(task), censtype))
    } else {
      return(changeData(task, recombinedf(getTaskData(task), newdata, datasplit, getTaskTargetNames(task), name)))
    }
  }

  if (datasplit == "no") {
    checkDFBasics(task, newdata, targetbound, name)
    if (!targetbound) {
      newdata = changeData(task, newdata)
    } else {
      newdata = constructTask(task, newdata, getTaskTargetNames(task), newtasktype, getTaskId(task), censtype)
    }
  }
  checkTaskBasics(task, newdata, name)
  old.td = getTaskDesc(task)
  new.td = getTaskDesc(newdata)

  if (targetbound) {
    checkColumnsEqual(getTaskData(task, target.extra = TRUE)$data,
      getTaskData(newdata, target.extra = TRUE)$data, "non-target column", name)
    # everything may change except size, n.feat and missings
    allowed.td.changes = setdiff(names(old.td), c("n.feat", "has.missings", "size"))
  } else {
    #check type didn't change
    assert(getTaskType(task) == getTaskType(newdata))

    # check target didn't change
    checkColumnsEqual(getTaskData(task, features = character(0)),
      getTaskData(newdata, features = character(0)), "target column", name)

    assertSetEqual(names(old.td), names(new.td))
    allowed.td.changes = c("id", "n.feat", "has.missings")
  }

  # check most of task description didn't change
  for (n in names(old.td)) {  # implicitly checks row number
    if (!n %in% allowed.td.changes && !identical(old.td[[n]], new.td[[n]])) {
      stopf("CPO %s changed task description item %s.", name, n)
    }
  }
  newdata
}

recombinedf = function(df, newdata, datasplit = c("target", "most", "all", "no", "task"), targetcols, name) {
# otherwise it contains the columns removed from the DF because they were target columns.
  datasplit = match.arg(datasplit)
  if (datasplit %in% c("most", "all")) {
    return(recombineLL(df, newdata, targetcols, datasplit, name))
  } else if (datasplit == "task") {
    assertClass(newdata, "Task")
    newdata = getTaskData(newdata)
  }
  if (!is.data.frame(newdata)) {
    stopf("CPO %s gave bad result\nmust return a data.frame.", name)
  }
  if (nrow(df) != nrow(newdata)) {
    stopf("CPO %s must not change number of rows.", name)
  }
  fullnames = c(names(newdata), targetcols)
  dubs = duplicated(fullnames)
  if (any(dubs)) {
    stopf("CPO %s gave bad result\ncolumn names %s duplicated (possibly with target)", name, collapse(unique(fullnames[dubs], sep = ", ")))
  }

  datanames = names(newdata)
  newdata = cbind(newdata, df[targetcols])
  if (identical(datanames, setdiff(names(df), targetcols))) {
    # names didn't change, so we preserve column order
    newdata = newdata[names(df)]
  }
  newdata
}

# relevant should be 'targets' or 'non-target features'
# old.relevants and new.relevants are the relevant columns.
checkColumnsEqual = function(old.relevants, new.relevants, relevant.name, name) {
  if (!isTRUE(checkSetEqual(names(old.relevants), names(new.relevants)))) {
    stopf("CPO %s must not change %s names.", name, relevant.name)
  }
  for (n in names(old.relevants)) {
    if (!identical(old.relevants[[n]], new.relevants[[n]])) {
      stopf("CPO %s must not change %ss, but changed %s.", name, relevant.name, n)
    }
  }
}

constructTask = function(oldtask, data, target, type, id, censtype) {
  if (type == "cluster") {
    return(makeClusterTask(id = id, data = data))
  }
  if (type == "surv") {
    return(makeSurvTask(id = id, data = data, censoring = censtype))
  }
  if (type == "classif" && getTaskType(oldtask) == "classif") {
    assert(length(target) == 1)
    oldtargets = levels(getTaskData(oldtask, target.extra = TRUE)$target)
    newtarget = levels(data[[target]])
    if (setequal(oldtargets, newtarget)) {
      positive = getTaskDesc(oldtask)$positive
      if (length(oldtargets) == 2 && oldtargets[1] != newtarget[1]) {
        positive = setdiff(oldtargets, positive)
      }
      return(makeClassifTask(id = id, data = data, target = target,
        positive = positive))
    }
  }

  constructor = switch(type,
    classif = makeClassifTask,
    multilabel = makeMultilabelTask,
    regr = makeRegrTask,
    surv = makeSurvTask)
  constructor(id = id, data = data, target = target)
}

checkTaskBasics = function(task, newdata, name) {
  if (!"Task" %in% class(newdata)) {
    stopf("CPO %s must return a Task", name)
  }

  if (getTaskDesc(task)$size != getTaskDesc(newdata)$size) {
    stopf("CPO %s must not change number of rows", name)
  }
}

checkDFBasics = function(task, newdata, targetbound, name) {
  if (!is.data.frame(newdata)) {
    stopf("CPO %s cpo.trafo gave bad result\ncpo.trafo must return a data.frame.", name)
  }
  assertClass(newdata, "data.frame")
  tnames = getTaskTargetNames(task)
  missingt = tnames[!tnames %in% names(newdata)]
  if (length(missingt)) {
    addendum = ""
    if (targetbound) {
      addendum = paste("\nIf you want to change names or number of target columns in targetbound CPOs",
        'you must use other .datasplit values, e.g. "target".', sep = "\n")
    }
   stopf("CPO %s cpo.trafo gave bad result\ndata.frame did not contain target column%s %s.%s",
     name, ifelse(length(missingt) > 1, "s", ""), missingt, addendum)
  }
}
