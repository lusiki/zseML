library(data.table)
library(paradox)
library(mlr3)
library(mlr3verse)
library(mlr3pipelines)
library(mlr3viz)
library(mlr3tuning)
library(mlr3misc)
library(mlr3extralearners)
library(future)
library(future.apply)
library(lubridate)
library(finautoml)


# SETUP -------------------------------------------------------------------
# snake to camel
snakeToCamel <- function(snake_str) {
  # Replace underscores with spaces
  spaced_str <- gsub("_", " ", snake_str)
  
  # Convert to title case using tools::toTitleCase
  title_case_str <- tools::toTitleCase(spaced_str)
  
  # Remove spaces and make the first character lowercase
  camel_case_str <- gsub(" ", "", title_case_str)
  camel_case_str <- sub("^.", tolower(substr(camel_case_str, 1, 1)), camel_case_str)
  
  # I haeve added this to remove dot
  camel_case_str <- gsub("\\.", "", camel_case_str)
  
  return(camel_case_str)
}

# path to save predictors
PATH_RESULTS = "F:/zse/results"


# PREPARE DATA ------------------------------------------------------------
print("Prepare data")

# read predictors
if (interactive()) {
  data_tbl = fread("data/zse-predictors-20240112.csv")
} else {
  data_tbl = fread("zse-predictors-20240112.csv")
}

# convert tibble to data.table
DT = as.data.table(data_tbl)

# define predictors
cols_non_features = c(
  "symbol", "date", "date_rolling", "month", "open", "high", "low", "close", 
  "volume", "returns"
)
cols_features = setdiff(colnames(DT), cols_non_features)

# change feature and targets columns names due to lighgbm
cols_features_new = vapply(cols_features, snakeToCamel, FUN.VALUE = character(1L), USE.NAMES = FALSE)
setnames(DT, cols_features, cols_features_new)
cols_features = cols_features_new

# convert columns to numeric. This is important only if we import existing features
chr_to_num_cols = setdiff(colnames(DT[, .SD, .SDcols = is.character]), c("symbol"))
print(chr_to_num_cols)
DT[, ..chr_to_num_cols]
DT = DT[, (chr_to_num_cols) := lapply(.SD, as.numeric), .SDcols = chr_to_num_cols]

# remove constant columns in set
features_ = DT[, ..cols_features]
remove_cols = colnames(features_)[apply(features_, 2, var, na.rm=TRUE) == 0]
print(paste0("Removing feature with 0 standard deviation: ", remove_cols))
cols_features = setdiff(cols_features, remove_cols)

# convert variables with low number of unique values to factors
int_numbers = na.omit(DT[, ..cols_features])[, lapply(.SD, function(x) all(floor(x) == x))]
int_cols = colnames(DT[, ..cols_features])[as.matrix(int_numbers)[1,]]
factor_cols = DT[, ..int_cols][, lapply(.SD, function(x) length(unique(x)))]
factor_cols = as.matrix(factor_cols)[1, ]
factor_cols = factor_cols[factor_cols <= 100]
DT = DT[, (names(factor_cols)) := lapply(.SD, as.factor), .SD = names(factor_cols)]

# change IDate to date, because of error
DT[, .SD, .SDcols = is.Date]
DT[, month := as.Date(month) + days(1)]
DT[, .SD, .SDcols = is.Date]
DT[, month := as.POSIXct(month, tz = "UTC")]


# TARGETS -----------------------------------------------------------------
# one month return as target variable
setorder(DT, symbol, month)
DT[, target := close / shift(close) - 1, by = symbol]
DT[symbol == "HRHT00RA0005", .(symbol, date, date_rolling, month, returns, close, target)]
DT[, .(symbol, date, date_rolling, month, returns, close, target)]

# remove observations with missing target
DT = na.omit(DT, cols = "target")

# sort
# this returns error on HPC. Some problem with memory
# setorder(DT, date)
print("This was the problem")
# DT = DT[order(date)] # DOESNT WORK TOO
DT = DT[order(month)]
head(DT[, .(symbol, date_rolling, month)], 30)
print("This was the problem. Solved.")


# TASKS -------------------------------------------------------------------
print("Tasks")

# id coluns we always keep
id_cols = c("symbol", "month")

# task with future month returns as target
cols_ = c(id_cols, cols_features, "target")
task = as_task_regr(DT[, ..cols_], id = "factorml", target = "target")

# set roles for symbol, date and yearmonth_id
task$col_roles$feature = setdiff(task$col_roles$feature, id_cols)


# CROSS VALIDATIONS -------------------------------------------------------
create_custom_rolling_windows <- function(tsk_,
                                          train_duration = 36,
                                          gap_duration = 1,
                                          tune_duration = 6,
                                          test_duration = 1) {
  # debug
  tsk_ = task$clone()
  
  # Define row ids
  data = tsk_$backend$data(cols = c("month", "..row_id"),
                           rows = 1:task$nrow)
  setnames(data, "..row_id", "row_id")
  stopifnot(all(tsk_$row_ids == data[, row_id]))
  
  # Ensure date is in Date format
  data[, date_col := as.Date(month)]
  
  # Initialize start and end dates based on duration unit
  start_date = data[, min(date_col)]
  end_date = data[, max(date_col)]
  
  # Initialize folds list
  folds = list()
  
  while (start_date < end_date) {
    train_end = start_date %m+% months(train_duration)
    gap1_end  = train_end %m+% months(gap_duration)
    tune_end  = gap1_end %m+% months(tune_duration)
    gap2_end  = tune_end %m+% months(gap_duration)
    test_end  = gap2_end %m+% months(test_duration)
    
    # Ensure the fold does not exceed the data range
    if (test_end > end_date) {
      break
    }
    
    # Extracting indices for each set
    train_indices = data[date_col %between% c(start_date, train_end), row_id]
    tune_indices  = data[date_col %between% c(gap1_end + days(1), tune_end), row_id]
    test_indices  = data[date_col %between% c(gap2_end + days(1), test_end), row_id]
    
    folds[[length(folds) + 1]] <- list(train = train_indices, tune = tune_indices, test = test_indices)
    
    # Update the start date for the next fold
    start_date = start_date %m+% months(1)
  }
  
  # Prepare sets for inner and outer resampling
  train_sets = lapply(folds, function(fold) fold$train)
  tune_sets  = lapply(folds, function(fold) fold$tune)
  test_sets  = lapply(folds, function(fold) fold$test)
  
  # Combine train and tune sets for outer resampling
  inner_sets = lapply(seq_along(train_sets), function(i) {
    c(train_sets[[i]], tune_sets[[i]])
  })
  
  # Instantiate custom inner resampling (train: train, test: tune)
  custom_inner = rsmp("custom", id = paste0(task$id, "-inner"))
  custom_inner$instantiate(task, train_sets, tune_sets)
  
  # Instantiate custom outer resampling (train: train+tune, test: test)
  custom_outer = rsmp("custom", id = paste0(task$id, "-outer"))
  custom_outer$instantiate(task, inner_sets, test_sets)
  
  return(list(outer = custom_outer, inner = custom_inner))
}

# create list of cvs
custom_cvs = list()
custom_cvs[[1]] = create_custom_rolling_windows(task$clone(), 48, 1, 3, 1)
custom_cvs[[2]] = create_custom_rolling_windows(task$clone(), 72, 1, 6, 1)

# visualize test
library(ggplot2)
library(patchwork)
prepare_cv_plot = function(x, set = "train") {
  x = lapply(x, function(x) data.table(ID = x))
  x = rbindlist(x, idcol = "fold")
  x[, fold := as.factor(fold)]
  x[, set := as.factor(set)]
  x[, ID := as.numeric(ID)]
}
plot_cv = function(cv, n = 5) {
  # cv = custom_cvs[[1]]
  print(cv)
  cv_test_inner = cv$inner
  cv_test_outer = cv$outer

  # prepare train, tune and test folds
  train_sets = cv_test_inner$instance$train[1:n]
  train_sets = prepare_cv_plot(train_sets)
  tune_sets = cv_test_inner$instance$test[1:n]
  tune_sets = prepare_cv_plot(tune_sets, set = "tune")
  test_sets = cv_test_outer$instance$test[1:n]
  test_sets = prepare_cv_plot(test_sets, set = "test")
  dt_vis = rbind(train_sets, tune_sets, test_sets)
  substr(colnames(dt_vis), 1, 1) <- toupper(substr(colnames(dt_vis), 1, 1))
  ggplot(dt_vis, aes(x = Fold, y = ID, color = Set)) +
    geom_point() +
    theme_minimal() +
    coord_flip() +
    labs(x = "", y = '',
         title = paste0(gsub("-.*|taskRet", "", cv_test_outer$id), " horizont"))
}
plots = lapply(custom_cvs, plot_cv, n = 35)
wp = wrap_plots(plots)
ggsave("plot_cv.png", plot = wp, width = 10, height = 8, dpi = 300)


# ADD PIPELINES -----------------------------------------------------------
# add my pipes to mlr dictionary
mlr_pipeops$add("uniformization", PipeOpUniform)
mlr_pipeops$add("winsorizesimple", PipeOpWinsorizeSimple)
mlr_pipeops$add("dropna", PipeOpDropNA)
mlr_pipeops$add("dropnacol", PipeOpDropNACol)
mlr_pipeops$add("dropcorr", PipeOpDropCorr)
# mlr_filters$add("gausscov_f1st", FilterGausscovF1st)
mlr_measures$add("linex", Linex)
# mlr_measures$add("adjloss2", AdjLoss2)
# mlr_measures$add("portfolio_ret", PortfolioRet)


# LEARNERS ----------------------------------------------------------------
# graph templates
graph_template =
  po("dropnacol", id = "dropnacol", cutoff = 0.05) %>>%
  po("dropna", id = "dropna") %>>%
  po("removeconstants", id = "removeconstants_1", ratio = 0)  %>>%
  po("fixfactors", id = "fixfactors") %>>%
  po("winsorizesimple", id = "winsorizesimple", probs_low = 0.01, probs_high = 0.99, na.rm = TRUE) %>>%
  po("removeconstants", id = "removeconstants_2", ratio = 0)  %>>%
  po("dropcorr", id = "dropcorr", cutoff = 0.99) %>>%
  po("uniformization") %>>%
  po("dropna", id = "dropna_v2") %>>%
  po("branch", options = c("jmi", "relief"), id = "filter_branch") %>>%
  gunion(list(po("filter", filter = flt("jmi"), filter.nfeat = 25),
              po("filter", filter = flt("relief"), filter.nfeat = 25)
  )) %>>%
  po("unbranch", id = "filter_unbranch") %>>%
  po("removeconstants", id = "removeconstants_3", ratio = 0)

# hyperparameters template
graph_template$param_set
search_space_template = ps(
  dropcorr.cutoff = p_fct(
    levels = c("0.80", "0.90", "0.95", "0.99"),
    trafo = function(x, param_set) {
      switch(x,
             "0.80" = 0.80,
             "0.90" = 0.90,
             "0.95" = 0.95,
             "0.99" = 0.99)
    }
  ),
  # dropcorr.cutoff = p_fct(levels = c(0.8, 0.9, 0.95, 0.99)),
  winsorizesimple.probs_high = p_fct(levels = c(0.999, 0.99, 0.98, 0.97, 0.90, 0.8)),
  winsorizesimple.probs_low = p_fct(levels = c(0.001, 0.01, 0.02, 0.03, 0.1, 0.2)),
  # scaling
  filter_branch.selection = p_fct(levels = c("jmi", "relief"))
)

# random forest graph
graph_rf = graph_template %>>%
  po("learner", learner = lrn("regr.ranger"))
graph_rf = as_learner(graph_rf)
as.data.table(graph_rf$param_set)[, .(id, class, lower, upper, levels)]
search_space_rf = search_space_template$clone()
search_space_rf$add(
  ps(regr.ranger.max.depth  = p_int(1, 15),
     regr.ranger.replace    = p_lgl(),
     regr.ranger.mtry.ratio = p_dbl(0.1, 1),
     regr.ranger.num.trees  = p_int(10, 2000),
     regr.ranger.splitrule  = p_fct(levels = c("variance", "extratrees")))
)

# xgboost graph
graph_xgboost = graph_template %>>%
  po("learner", learner = lrn("regr.xgboost"))
plot(graph_xgboost)
graph_xgboost = as_learner(graph_xgboost)
as.data.table(graph_xgboost$param_set)[grep("depth", id), .(id, class, lower, upper, levels)]
search_space_xgboost = ps(
  # preprocessing
  dropcorr.cutoff = p_fct(
    levels = c("0.80", "0.90", "0.95", "0.99"),
    trafo = function(x, param_set) {
      switch(x,
             "0.80" = 0.80,
             "0.90" = 0.90,
             "0.95" = 0.95,
             "0.99" = 0.99)
    }
  ),
  # dropcorr.cutoff = p_fct(levels = c(0.8, 0.9, 0.95, 0.99)),
  winsorizesimple.probs_high = p_fct(levels = c(0.999, 0.99, 0.98, 0.97, 0.90, 0.8)),
  winsorizesimple.probs_low = p_fct(levels = c(0.001, 0.01, 0.02, 0.03, 0.1, 0.2)),
  # filters
  filter_branch.selection = p_fct(levels = c("jmi", "relief")),
  # learner
  regr.xgboost.alpha     = p_dbl(0.001, 100, logscale = TRUE),
  regr.xgboost.max_depth = p_int(1, 20),
  regr.xgboost.eta       = p_dbl(0.0001, 1, logscale = TRUE),
  regr.xgboost.nrounds   = p_int(1, 5000),
  regr.xgboost.subsample = p_dbl(0.1, 1)
)

# nnet graph
graph_nnet = graph_template %>>%
  po("learner", learner = lrn("regr.nnet", MaxNWts = 50000))
graph_nnet = as_learner(graph_nnet)
as.data.table(graph_nnet$param_set)[, .(id, class, lower, upper, levels)]
search_space_nnet = search_space_template$clone()
search_space_nnet$add(
  ps(regr.nnet.size  = p_int(lower = 2, upper = 15),
     regr.nnet.decay = p_dbl(lower = 0.0001, upper = 0.1),
     regr.nnet.maxit = p_int(lower = 50, upper = 500))
)

# glmnet graph
graph_glmnet = graph_template %>>%
  po("learner", learner = lrn("regr.glmnet"))
graph_glmnet = as_learner(graph_glmnet)
as.data.table(graph_glmnet$param_set)[, .(id, class, lower, upper, levels)]
search_space_glmnet = ps(
  dropcorr.cutoff = p_fct(
    levels = c("0.80", "0.90", "0.95", "0.99"),
    trafo = function(x, param_set) {
      switch(x,
             "0.80" = 0.80,
             "0.90" = 0.90,
             "0.95" = 0.95,
             "0.99" = 0.99)
    }
  ),
  # dropcorr.cutoff = p_fct(levels = c(0.8, 0.9, 0.95, 0.99)),
  winsorizesimple.probs_high = p_fct(levels = c(0.999, 0.99, 0.98, 0.97, 0.90, 0.8)),
  winsorizesimple.probs_low = p_fct(levels = c(0.001, 0.01, 0.02, 0.03, 0.1, 0.2)),
  # filters
  filter_branch.selection = p_fct(levels = c("jmi", "relief")),
  # learner
  regr.glmnet.s     = p_int(lower = 5, upper = 30),
  regr.glmnet.alpha = p_dbl(lower = 1e-4, upper = 1, logscale = TRUE)
)

# threads
threads = 4
set_threads(graph_rf, n = threads)
set_threads(graph_xgboost, n = threads)
set_threads(graph_nnet, n = threads)
set_threads(graph_glmnet, n = threads)


# TODO: look below
# DESIGNS -----------------------------------------------------------------
designs_l = lapply(custom_cvs, function(cv_) {
  # debug
  # cv_ = custom_cvs[[1]]
  
  # get cv inner object
  cv_inner = cv_$inner
  cv_outer = cv_$outer
  cat("Number of iterations fo cv inner is ", cv_inner$iters, "\n")
  
  # # debug
  # if (interactive()) {
  #   to_ = 2
  # } else {
  #   to_ = cv_inner$iters
  # }
  to_ = cv_inner$iters
  
  designs_cv_l = lapply(1:to_, function(i) { # 1:cv_inner$iters
    # debug
    # i = 1
    
    # choose task_
    print(cv_inner$id)
    
    # with new mlr3 version I have to clone
    task_inner = task$clone()
    task_inner$filter(c(cv_inner$train_set(i), cv_inner$test_set(i)))
    
    # inner resampling
    custom_ = rsmp("custom")
    custom_$id = paste0("custom_", cv_inner$iters, "_", i)
    custom_$instantiate(task_inner,
                        list(cv_inner$train_set(i)),
                        list(cv_inner$test_set(i)))
    
    # objects for all autotuners
    measure_ = msr("regr.mse")
    tuner_   = tnr("random_search")
    term_evals = 20
    
    # auto tuner rf
    at_rf = auto_tuner(
      tuner = tuner_,
      learner = graph_rf,
      resampling = custom_,
      measure = measure_,
      search_space = search_space_rf,
      # terminator = trm("none")
      term_evals = term_evals
    )
    
    # auto tuner xgboost
    at_xgboost = auto_tuner(
      tuner = tuner_,
      learner = graph_xgboost,
      resampling = custom_,
      measure = measure_,
      search_space = search_space_xgboost,
      # terminator = trm("none")
      term_evals = term_evals
    )
    
    # auto tuner nnet
    at_nnet = auto_tuner(
      tuner = tuner_,
      learner = graph_nnet,
      resampling = custom_,
      measure = measure_,
      search_space = search_space_nnet,
      # terminator = trm("none")
      term_evals = term_evals
    )
    
    # auto tuner glmnet
    at_glmnet = auto_tuner(
      tuner = tuner_,
      learner = graph_glmnet,
      resampling = custom_,
      measure = measure_,
      search_space = search_space_glmnet,
      # terminator = trm("none")
      term_evals = term_evals
    )
    
    # outer resampling
    customo_ = rsmp("custom")
    customo_$id = paste0("custom_", cv_inner$iters, "_", i)
    customo_$instantiate(task, list(cv_outer$train_set(i)), list(cv_outer$test_set(i)))
    
    # nested CV for one round
    design = benchmark_grid(
      tasks = task,
      learners = list(at_rf, at_xgboost, at_nnet, at_glmnet),
      resamplings = customo_
    )
  })
  designs_cv = do.call(rbind, designs_cv_l)
})
designs = do.call(rbind, designs_l)

# exp dir
if (interactive()) {
  dirname_ = "experiments_test"
  if (dir.exists(dirname_)) system(paste0("rm -r ", dirname_))
} else {
  dirname_ = "experiments"
}

# create registry
print("Create registry")
packages = c("data.table", "gausscov", "paradox", "mlr3", "mlr3pipelines",
             "mlr3tuning", "mlr3misc", "future", "future.apply",
             "mlr3extralearners", "stats")
reg = makeExperimentRegistry(file.dir = dirname_, seed = 1, packages = packages)

# populate registry with problems and algorithms to form the jobs
print("Batchmark")
batchmark(designs, reg = reg)

# save registry
print("Save registry")
saveRegistry(reg = reg)

# create sh file
sh_file = sprintf("
#!/bin/bash

#PBS -N ZSEML
#PBS -l ncpus=4
#PBS -l mem=8GB
#PBS -J 1-%d
#PBS -o experiments/logs
#PBS -j oe

cd ${PBS_O_WORKDIR}
apptainer run image.sif run_job.R 0
", nrow(designs))
sh_file_name = "run_jobs.sh"
file.create(sh_file_name)
writeLines(sh_file, sh_file_name)
