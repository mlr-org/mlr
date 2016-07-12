#' @title Generate hyperparameter effect data.
#'
#' @description 
#' Generate cleaned hyperparameter effect data from a tuning result or from a 
#' nested cross-validation tuning result. The object returned can be used for 
#' custom visualization or passed downstream to an out of the box mlr method, 
#' \code{\link{plotHyperParsEffect}}.
#' 
#' @param tune.result [\code{\link{TuneResult}} | \code{\link{ResampleResult}}]\cr
#'  Result of \code{\link{tuneParams}} (or \code{\link{resample}} ONLY when used
#'  for nested cross-validation). The tuning result (or results if the 
#'  output is from nested cross-validation), also containing the 
#'  optimizer results. If nested CV output is passed, each element in the list 
#'  will be considered a separate run, and the data from each run will be 
#'  included in the dataframe within the returned \code{HyperParsEffectData}.
#' @param include.diagnostics [\code{logical(1)}]\cr
#'  Should diagnostic info (eol and error msg) be included?
#'  Default is \code{FALSE}.
#' @param trafo.scale [\code{logical(1)}]\cr
#'  Should the units of the hyperparameter path be converted back to the 
#'  ordinary scale? This is only necessary when trafo was used to create the
#'  path.
#'  Default is \code{FALSE}.
#'
#' @return [\code{HyperParsEffectData}]
#'  Object containing the hyperparameter effects dataframe, the tuning 
#'  performance measures used, the hyperparameters used, a flag for including 
#'  diagnostic info, a flag for whether nested cv was used, and the optimization 
#'  algorithm used.
#'
#' @examples \dontrun{
#' # 3-fold cross validation
#' ps = makeParamSet(makeDiscreteParam("C", values = 2^(-4:4)))
#' ctrl = makeTuneControlGrid()
#' rdesc = makeResampleDesc("CV", iters = 3L)
#' res = tuneParams("classif.ksvm", task = pid.task, resampling = rdesc,
#' par.set = ps, control = ctrl)
#' data = generateHyperParsEffectData(res)
#' plotHyperParsEffect(data, x = "C", y = "mmce.test.mean")
#' 
#' # nested cross validation
#' ps = makeParamSet(makeDiscreteParam("C", values = 2^(-4:4)))
#' ctrl = makeTuneControlGrid()
#' rdesc = makeResampleDesc("CV", iters = 3L)
#' lrn = makeTuneWrapper("classif.ksvm", control = ctrl,
#'                       resampling = rdesc, par.set = ps)
#' res = resample(lrn, task = pid.task, resampling = cv2, 
#'                extract = getTuneResult)
#' data = generateHyperParsEffectData(res)
#' plotHyperParsEffect(data, x = "C", y = "mmce.test.mean", plot.type = "line")
#' }
#' @export
#' @importFrom utils type.convert
generateHyperParsEffectData = function(tune.result, include.diagnostics = FALSE,
                                       trafo.scale = FALSE)
  {
  assert(checkClass(tune.result, "ResampleResult"), 
         checkClass(tune.result, classes = c("TuneResult", "OptResult")))
  assertFlag(include.diagnostics)
  
  # in case we have nested CV
  if (getClass1(tune.result) == "ResampleResult"){
    if (trafo.scale){
      d = data.frame()
      for (i in 1:length(tune.result$extract)){
        d = rbind(d, as.data.frame(trafoOptPath(tune.result$extract[[i]])))
      }
    } else {
      d = getNestedTuneResultsOptPathDf(tune.result)
    }
    num_hypers = length(tune.result$extract[[1]]$x)
    for (hyp in 1:num_hypers) {
      if (!is.numeric(d[, hyp]))
        d[, hyp] = type.convert(as.character(d[, hyp]))
    }
    # rename to be clear this denotes the nested cv
    names(d)[names(d) == "iter"] = "nested_cv_run"
  
    # items for object
    measures = tune.result$extract[[1]]$opt.path$y.names
    hyperparams = names(tune.result$extract[[1]]$x)
    optimization = getClass1(tune.result$extract[[1]]$control)
    nested = TRUE
  } else {
    if (trafo.scale){
      d = as.data.frame(trafoOptPath(tune.result$opt.path))
    } else {
      d = as.data.frame(tune.result$opt.path)
    }
    # what if we have numerics that were discretized upstream
    num_hypers = length(tune.result$x)
    for (hyp in 1:num_hypers) {
      if (!is.numeric(d[, hyp]))
        d[, hyp] = type.convert(as.character(d[, hyp]))
    }
    measures = tune.result$opt.path$y.names
    hyperparams = names(tune.result$x)
    optimization = getClass1(tune.result$control)
    nested = FALSE
  }
    
  # off by default unless needed by user
  if (include.diagnostics == FALSE)
    d = within(d, rm("eol", "error.message"))
  
  # users might not know what dob means, so let's call it iteration
  names(d)[names(d) == "dob"] = "iteration"
  
  makeS3Obj("HyperParsEffectData", data = d, measures = measures,
            hyperparams = hyperparams, 
            diagnostics = include.diagnostics, 
            optimization = optimization,
            nested = nested)
}

#' @export
print.HyperParsEffectData = function(x, ...) {
  catf("HyperParsEffectData:")
  catf("Hyperparameters: %s", collapse(x$hyperparams))
  catf("Measures: %s", collapse(x$measures))
  catf("Optimizer: %s", collapse(x$optimization))
  catf("Nested CV Used: %s", collapse(x$nested))
  catf("Snapshot of $data:")
  print(head(x$data))
}

#' @title Plot the hyperparameter effects data
#' 
#' @description 
#' Plot hyperparameter validation path. Automated plotting method for 
#' \code{HyperParsEffectData} object. Useful for determining the importance
#' or effect of a particular hyperparameter on some performance measure and/or
#' optimizer.
#'
#' @param hyperpars.effect.data [\code{HyperParsEffectData}]\cr
#'  Result of \code{\link{generateHyperParsEffectData}}
#' @param x [\code{character(1)}]\cr
#'  Specify what should be plotted on the x axis. Must be a column from
#'  \code{HyperParsEffectData$data}
#' @param y [\code{character(1)}]\cr
#'  Specify what should be plotted on the y axis. Must be a column from
#'  \code{HyperParsEffectData$data}
#' @param z [\code{character(1)}]\cr
#'  Specify what should be used as the extra axis for a particular geom. This
#'  could be for the fill on a heatmap or color aesthetic for a line. Must be a 
#'  column from \code{HyperParsEffectData$data}. Default is \code{NULL}.
#' @param plot.type [\code{character(1)}]\cr
#'  Specify the type of plot: "scatter" for a scatterplot, "heatmap" for a 
#'  heatmap, "line" for a scatterplot with a connecting line, or "contour" for a
#'  contour plot layered ontop of a heatmap.
#'  Default is scatter.
#' @param loess.smooth [\code{logical(1)}]\cr
#'  If TRUE, will add loess smoothing line to plots where possible. Note that 
#'  this is probably only useful when \code{plot.type} is set to either 
#'  "scatter" or "line". Must be a column from \code{HyperParsEffectData$data}
#'  Default is \code{FALSE}.
#' @param facet [\code{character(1)}]\cr
#'  Specify what should be used as the facet axis for a particular geom. When 
#'  using nested cross validation, set this to "nested_cv_run" to obtain a facet
#'  for each outer loop. Must be a column from \code{HyperParsEffectData$data}
#'  Default is \code{NULL}.
#' @template arg_prettynames
#' @param global.only [\code{logical(1)}]\cr
#'  If TRUE, will only plot the current global optima when setting 
#'  x = "iteration" and y as a performance measure from 
#'  \code{HyperParsEffectData$measures}. Set this to FALSE to always plot the 
#'  performance of every iteration, even if it is not an improvement.
#'  Default is \code{TRUE}.
#' @param interpolate [\code{logical(1)}]\cr
#'  If TRUE, will interpolate non-complete grids in order to visualize a more 
#'  complete path. Only meaningful when attempting to plot a heatmap or contour.
#'  This will fill in "empty" cells in the heatmap or contour plot. Note that 
#'  cases of irregular hyperparameter paths, you will most likely need to use
#'  this to have a meaningful visualization.
#'  Default is \code{TRUE}.
#' @param show.experiments [\code{logical(1)}]\cr
#'  If TRUE, will overlay the plot with points indicating where an experiment
#'  ran. This is only useful when creating a heatmap or contour plot with 
#'  interpolation so that you can see which points were actually on the 
#'  original path. Note: if any learner crashes occurred within the path, this
#'  will default to TRUE.
#'  Default is \code{FALSE}.
#'
#' @template ret_gg2
#'  
#' @note Any NAs incurred from learning algorithm crashes will be indicated in 
#' the plot and the NA values will be replaced with the column min/max depending
#' on the optimal values for the respective measure. Execution time will be
#' replaced with the max.
#' 
#' @export
#'
#' @examples
#' # see generateHyperParsEffectData
plotHyperParsEffect = function(hyperpars.effect.data, x = NULL, y = NULL, 
                               z = NULL, plot.type = "scatter", 
                               loess.smooth = FALSE, facet = NULL, 
                               pretty.names = TRUE, global.only = TRUE, 
                               interpolate = TRUE, show.experiments = TRUE) {
  assertClass(hyperpars.effect.data, classes = "HyperParsEffectData")
  assertChoice(x, choices = names(hyperpars.effect.data$data))
  assertChoice(y, choices = names(hyperpars.effect.data$data))
  assertSubset(z, choices = names(hyperpars.effect.data$data))
  assertChoice(plot.type, choices = c("scatter", "line", "heatmap", "contour"))
  assertFlag(loess.smooth)
  assertSubset(facet, choices = names(hyperpars.effect.data$data))
  assertFlag(pretty.names)
  assertFlag(global.only)
  assertFlag(interpolate)
  assertFlag(show.experiments)
 
  if (length(x) > 1 || length(y) > 1 || length(z) > 1 || length(facet) > 1)
    stopf("Greater than 1 length x, y, z or facet not yet supported")
  
  d = hyperpars.effect.data$data
  if (hyperpars.effect.data$nested)
    d$nested_cv_run = as.factor(d$nested_cv_run)
  
  # set flags for building plots
  na_flag = any(is.na(d[, hyperpars.effect.data$measures]))
  z_flag = !is.null(z)
  facet_flag = !is.null(facet)
  grid_flag = hyperpars.effect.data$optimization == "TuneControlGrid"
  heatcontour_flag = plot.type %in% c("heatmap", "contour")
  
  # deal with NAs where optimizer failed
  if (na_flag){
    d$learner_status = ifelse(is.na(d[, "exec.time"]), "Failure", "Success")
    for (col in hyperpars.effect.data$measures) {
      col_name = stri_split_fixed(col, ".test.mean", omit_empty = TRUE)[[1]]
      if (heatcontour_flag){
        d[,col][is.na(d[,col])] = get(col_name)$worst
      } else {
        if (get(col_name)$minimize){
          d[,col][is.na(d[,col])] = max(d[,col], na.rm = TRUE)
        } else {
          d[,col][is.na(d[,col])] = min(d[,col], na.rm = TRUE)
        }
      }
    }
    d$exec.time[is.na(d$exec.time)] = max(d$exec.time, na.rm = TRUE)
  }
  
  # assign for global only
  if (global.only && x == "iteration" && y %in% hyperpars.effect.data$measures){
    for (col in hyperpars.effect.data$measures) {
      col_name = stri_split_fixed(col, ".test.mean", omit_empty = TRUE)[[1]]
      if (get(col_name)$minimize){
        d[,col] = cummin(d[,col])
      } else {
        d[,col] = cummax(d[,col])
      }
    }
  }
  
  # FIXME: interpolation if heatmap / contour and not grid
  if (interpolate && z_flag && (heatcontour_flag)){
    # put ifelse to determine learner status
    if (!("learner_status" %in% names(d))){
      d$learner_status = "Success"
      }
    
    if (hyperpars.effect.data$nested){
      d_new = d
      new_d = data.frame()
      # for loop for each nested cv run
      for (run in unique(d$nested_cv_run)){
        d_run = d_new[d_new$nested_cv_run == run, ]
        if (anyDuplicated(d_run[,c(x,y,z)])){
          warningf("Detected duplicate (%s, %s) points on the path for nested cv
                   run %s. The duplicates will be removed before interpolation!"
                   , x, y, run)
          d_run = d_run[!duplicated(d_run[,c(x,y,z)]),]
        }
        # save succ and fail so we can reference after interpolation
        fld = akima::interp(d_run[, x], d_run[, y], d_run[, z])
        df = reshape2::melt(fld$z, na.rm = TRUE)
        names(df) <- c(x, y, z)
        df[, x] = fld$x[df[, x]]
        df[, y] = fld$y[df[, y]]
        df$learner_status = "Interpolated Point"
        df$iteration = NA
        combined = rbind(d_run[,c(x,y,z,"learner_status", "iteration")], df)
        new_d = rbind(new_d, combined)
      }
      d = new_d
      
    } else {
      # need to unique so interp doesn't crash with duplicates
      d_new = d
      if (anyDuplicated(d[,c(x,y)])){
        warningf("Detected duplicate (%s, %s) points on the path. The duplicates
                 will be removed before interpolation!", x, y)
        d_new = d[!duplicated(d[,c(x,y)]),]
      }
      fld = akima::interp(d_new[, x], d_new[, y], d_new[, z])
      df = reshape2::melt(fld$z, na.rm = TRUE)
      names(df) <- c(x, y, z)
      df[, x] = fld$x[df[, x]]
      df[, y] = fld$y[df[, y]]
      df$learner_status = "Interpolated Point"
      df$iteration = NA
      combined = rbind(d_new[,c(x,y,z,"learner_status", "iteration")], df)
      d = combined
    }
    
  }
  
  if (hyperpars.effect.data$nested && z_flag){
    averaging = d[, !(names(d) %in% c("iteration", "nested_cv_run", 
                                      hyperpars.effect.data$hyperparams, "eol",
                                      "error.message", "learner_status")), 
                  drop = FALSE]
    if (na_flag || interpolate){
      hyperpars = lapply(d[, c(hyperpars.effect.data$hyperparams, 
                               "learner_status")], "[")
    } else {
      hyperpars = lapply(d[, hyperpars.effect.data$hyperparams], "[")
    }
    
    d = aggregate(averaging, hyperpars, mean)
    # how to deal with iterations?
    d$iteration = 1:nrow(d)
  }
    
  # just x, y  
  if ((length(x) == 1) && (length(y) == 1) && !(z_flag)){
    if (hyperpars.effect.data$nested){
      plt = ggplot(d, aes_string(x = x, y = y, color = "nested_cv_run"))
    } else {
      plt = ggplot(d, aes_string(x = x, y = y))
    }
    if (na_flag){
      plt = plt + geom_point(aes_string(shape = "learner_status", 
                                        color = "learner_status")) +
        scale_shape_manual(values = c("Failure" = 4, "Success" = 0)) +
        scale_color_manual(values = c("red", "black"))
    } else {
      plt = plt + geom_point()
    }
    if (plot.type == "line")
      plt = plt + geom_line()
    if (loess.smooth)
      plt = plt + geom_smooth()
    if (facet_flag)
      plt = plt + facet_wrap(facet)
  } else if ((length(x) == 1) && (length(y) == 1) && (z_flag)){
    # FIXME: generalize logic here
    if (heatcontour_flag){
      plt = ggplot(data = d[d$learner_status == "Interpolated Point", ], 
                   aes_string(x = x, y = y, fill = z, z = z)) + geom_tile()
      if (na_flag || (interpolate && show.experiments)){
        plt = plt + geom_point(data = d[d$learner_status %in% c("Success", 
                                                                "Failure"), ],
                                        aes_string(shape = "learner_status"), 
                               fill = "red") +
          scale_shape_manual(values = c("Failure" = 24, "Success" = 0))
      } 
      if (plot.type == "contour")
        plt = plt + geom_contour()
    } else {
      plt = ggplot(d, aes_string(x = x, y = y, color = z))
      if (na_flag){
        plt = plt + geom_point(aes_string(shape = "learner_status", 
                                          color = "learner_status")) +
          scale_shape_manual(values = c("Failure" = 4, "Success" = 0)) +
          scale_color_manual(values = c("red", "black"))
      } else{
        plt = plt + geom_point()
      }
      if (plot.type == "line")
        plt = plt + geom_line()
    }
  }
  
  # pretty name changing
  if (pretty.names) {
    if (x %in% hyperpars.effect.data$measures)
      plt = plt + 
        xlab(eval(as.name(stri_split_fixed(x, ".test.mean")[[1]][1]))$name)
    if (y %in% hyperpars.effect.data$measures)
      plt = plt + 
        ylab(eval(as.name(stri_split_fixed(y, ".test.mean")[[1]][1]))$name)
    if (!is.null(z))
      if (z %in% hyperpars.effect.data$measures)
        plt = plt +
          labs(fill = eval(as.name(stri_split_fixed(z, 
                                                ".test.mean")[[1]][1]))$name) 
  }
  
  return(plt)
}