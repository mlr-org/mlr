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


head(grid)
head(pred.grid$data)

cdata = cbind(pred.grid, grid)

cdata$nresponse = apply(subset(pred.grid$data, select = -response), 1, max)



grid.dcast = reshape2::dcast(cdata, as.formula(paste(x1n, x2n, sep = "~")), value.var = "nresponse")
# generate 3D plots data list
grid.3d = list(x = grid.dcast[,1] * 10,
               y = as.numeric(colnames(grid.dcast)[-1]) *10,
               z = t(as.matrix(grid.dcast[,-1])))

p = plot_ly(data = grid.3d, x = x, y = y, z = z,
        type = "surface", showscale = F, colorbar = list(title = target), name = "Density")

p = p %>% layout(title = title,
                 scene = list(xaxis = list(title = paste("x: ", x1n)),
                              yaxis = list(title = paste("y: ", x2n)),
                              zaxis = list(title = "z: f(x,y)", range = c(0, 1))))

p

#add_trace(p, data = grid.3d, z = z, type = "contour")


tmp = data
tmp$z = 0
head(tmp)
q = add_trace(p, data = tmp, x = get(x1n), y = get(x2n), z = z, 
        type = "scatter3d", mode = "markers", symbol = get(target),
        marker = list(size = 4), showlegend = T)
q %>% layout(legend = list(xanchor = "right"))

q = plot_ly(data = grid.3d, x = get(x1n), y = get(x2n), z = z, type = "contour")
q

add_trace(q, data = data, x = get(x1n), y = get(x2n), mode = "markers")



# plot_ly(x = iris$Sepal.Length, y = iris$Sepal.Width, z = iris$Petal.Length,
#         type = "mesh3d")

plot_ly(data = grid.3d, x = x, y = y, z = z,
        type = "surface", 
        contours = list(z = list(show = T),
                        y = list(show = T),
                        x = list(show = T)))


