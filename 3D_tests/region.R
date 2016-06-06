learner = classif.ksvm
task = iris.task
features = NULL
measures
cv = 10L
gridsize = 100
show.point = TRUE
pointsize = 2
prob.alpha = TRUE
se.band = TRUE
err.mark = "train"
err.col = "white"
err.size = pointsize
greyscale = FALSE
pretty.names = TRUE
alpha = 1
err.alpha = alpha





learner = checkLearner(learner)
assert(
  checkClass(task, "ClassifTask"),
  checkClass(task, "RegrTask")
)
td = getTaskDescription(task)

# features and dimensionality
fns = getTaskFeatureNames(task)
if (is.null(features) && td$type != "classif") {
  features = if (length(fns) == 1L) fns else fns[1:2]
} else if (is.null(features) && td$type == "classif") {
  features = if (length(fns) == 1L) fns else fns[1:3]
} else {
  assertCharacter(features, max.len = 3L)
  assertSubset(features, choices = fns)
}
taskdim = length(features)
if (td$type == "classif" && taskdim != 3)
  stopf("Classification: currently only 3D plots supported in Plotly, not: %i", taskdim)
if (td$type == "regr" && taskdim != 2)
  stopf("Regression: currently only 3D plots supported in Plotly, not: %i", taskdim)

measures = checkMeasures(measures, task)
cv = asCount(cv)

if (missing(gridsize)) {
  gridsize = ifelse(taskdim == 1L, 500, 100)
} else {
  gridsize = asCount(gridsize)
}
assertNumber(pointsize, lower = 0)
assertFlag(prob.alpha)
assertFlag(se.band)
assertChoice(err.mark, choices = c("train", "cv", "none"))
assertString(err.col)
assertNumber(err.size, lower = 0)
assertLogical(greyscale)

if (td$type == "classif" && err.mark == "cv" && cv == 0L)
  stopf("Classification: CV must be switched on, with 'cv' > 0, for err.type = 'cv'!")

# subset to features, set hyperpars
task = subsetTask(task, features = features)
learner = setHyperPars(learner)

# some shortcut names
target = td$target
data = getTaskData(task)
y = getTaskTargets(task)
x1n = features[1L]
x1 = data[, x1n]

# predictions
# if learner supports prob or se, enable it
if (td$type == "regr" && taskdim == 1L && hasLearnerProperties(learner, "se"))
  learner = setPredictType(learner, "se")
if (td$type == "classif" && hasLearnerProperties(learner, "prob"))
  learner = setPredictType(learner, "prob")
mod = train(learner, task)
pred.train = predict(mod, task)
yhat = pred.train$data$response
perf.train = performance(pred.train, task = task, measures = measures)
if (cv > 0L) {
  cv = crossval(learner, task, iters = 10L, measures = measures, show.info = FALSE)
  perf.cv = cv$aggr
  pred.cv = cv$pred
} else {
  perf.cv = NA_real_
}

# 2d, 3d stuff
if (taskdim > 1L) {
  x2n = features[2L]
  x2 = data[, x2n]
  if (taskdim == 3L) {
    x3n = features[3L]
    x3 = data[, x3n]
  }
}

# grid for predictions
if (taskdim == 1L) {
  grid = data.frame(x = seq(min(x1), max(x1), length.out = gridsize))
} else if (taskdim == 2L) {
  # setup data frames for ggplot. grid = background, data = points
  grid = expand.grid(
    seq(min(x1), max(x1), length.out = gridsize),
    seq(min(x2), max(x2), length.out = gridsize)
  )
} else if (taskdim == 3L) {
  grid = expand.grid(
    seq(min(x1), max(x1), length.out = gridsize / 2),
    seq(min(x2), max(x2), length.out = gridsize / 2),
    seq(min(x3), max(x3), length.out = gridsize / 2)
  )
}
colnames(grid) = features
pred.grid = predict(mod, newdata = grid)
grid[, target] = pred.grid$data$response


# #pred.grid$data
# cdata = cbind(pred.grid$data, grid)
# head(cdata)
# unique(cdata[, x1n])



index = NULL

for (i in 1:c(nrow(pred.grid$data) - 1))
  if (pred.grid$data[i, "response"] != pred.grid$data[i + 1, "response"] && (i %% (gridsize / 2) != 0)) {
    index = append(index, i)
#    index = append(index, i + 1)
  }

index = index[!duplicated(index)]

head(grid[index,])


# grid.dcast = reshape2::dcast(grid[index, -4], as.formula(paste(x1n, x2n, sep = "~")), value.var = )
# # generate 3D plots data list
# grid.3d = list(x = grid.dcast[,1],
#                y = as.numeric(colnames(grid.dcast)[-1]),
#                z = t(as.matrix(grid.dcast[,-1])))


#temp.grid = grid[index, c(x1n, x2n, x3n)]


# 
# tmp = ddply(grid[index, ], x2n, function(x){ x$tmpid = 1:nrow(x); x})
# 
# grid.dcast = dcast(tmp, as.formula(paste(paste(x1n, "tmpid", sep = "+"), x2n, sep = "~")), value.var = x3n)
# grid.dcast = subset(grid.dcast, select = -c(tmpid))
# grid.3d = list(x = grid.dcast[,1],
#                y = as.numeric(colnames(grid.dcast)[-1]),
#                z = t(as.matrix(grid.dcast[,-1])))
# 
# p = plot_ly(x = grid.3d$x, y = grid.3d$y, z = grid.3d$z, 
#         type = "scatter3d", mode = "markers")
# p
# add_trace(p, data = data, x = get(x1n), y = get(x2n), z = get(x3n), 
#         type = "scatter3d", mode = "markers", symbol = data[, target], 
#         marker = list(size = pointsize, opacity = alpha), 
#         text = "Input Data")

p = plot_ly(grid[index,], x = get(x1n), y = get(x2n), z = get(x3n),
        type = "scatter3d", mode = "markers", marker = list(size = 2), opacity = 0.4, color = grid[index, target])

p




