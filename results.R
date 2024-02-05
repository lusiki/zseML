library(fs)
library(data.table)
library(mlr3batchmark)
library(batchtools)
library(matrixStats)




# SETUP -------------------------------------------------------------------
# help functions
sign01 = function(x) {
  ifelse(x > 0, 1, 0)
}

# load registry
PATH = "F:/zse/results"
reg = loadRegistry(PATH, work.dir=PATH)

# used memory
reg$status[!is.na(mem.used)]
reg$status[, max(mem.used, na.rm = TRUE)]

# done jobs
ids_done = findDone(reg=reg)
ids_notdone = findNotDone(reg=reg)

# get results
tabs = batchtools::getJobTable(ids_done, reg = reg)[
  , c("job.id", "job.name", "repl", "prob.pars", "algo.pars"), with = FALSE]
predictions_meta = cbind.data.frame(
  id = tabs[, job.id],
  task = vapply(tabs$prob.pars, `[[`, character(1L), "task_id"),
  learner = gsub(".*regr.|.tuned", "", vapply(tabs$algo.pars, `[[`, character(1L), "learner_id")),
  cv = gsub("custom_|_.*", "", vapply(tabs$prob.pars, `[[`, character(1L), "resampling_id")),
  fold = gsub("custom_\\d+_", "", vapply(tabs$prob.pars, `[[`, character(1L), "resampling_id"))
)
predictions_l = lapply(unlist(ids_done), function(id_) {
  # id_ = 1
  x = tryCatch({readRDS(fs::path(PATH, "results", id_, ext = "rds"))},
               error = function(e) NULL)
  if (is.null(x)) {
    print(id_)
    return(NULL)
  }
  x = x$prediction
  x["id"] = id_
  x
})
predictions = lapply(predictions_l, function(x) {
  # x = predictions_l[[1]]
  cbind.data.frame(
    id = x$id,
    row_ids = x$test$row_ids,
    truth = x$test$truth,
    response = x$test$response
  )
})
predictions = rbindlist(predictions)
predictions = merge(predictions_meta, predictions, by = "id")
predictions = as.data.table(predictions)

# import tasks
tasks_files = dir_ls(fs::path(PATH, "problems"))
tasks = lapply(tasks_files, readRDS)
names(tasks) = lapply(tasks, function(t) t$data$id)
tasks

# add backend to predictions
backend_l = lapply(tasks, function(tsk_) {
  # tsk_ = tasks[[1]]
  x = tsk_$data$backend$data(1:tsk_$data$nrow,
                             c("symbol", "month", "..row_id"))
  setnames(x, c("symbol", "month", "row_ids"))
  x
})
backends = rbindlist(backend_l, fill = TRUE)

# merge predictions and backends
predictions = backends[predictions, on = c("row_ids")]

# measures
# source("Linex.R")
# source("AdjLoss2.R")
# source("PortfolioRet.R")
# mlr_measures$add("linex", Linex)
# mlr_measures$add("adjloss2", AdjLoss2)
# mlr_measures$add("portfolio_ret", PortfolioRet)

# merge backs and predictions
predictions[, month := as.Date(month)]


# PREDICTIONS RESULTS -----------------------------------------------------
# remove dupliactes - keep firt
predictions = unique(predictions, by = c("row_ids", "month", "task", "learner", "cv"))

# predictions
predictions[, `:=`(
  truth_sign = as.factor(sign01(truth)),
  response_sign = as.factor(sign01(response))
)]
predictions[, .N, by = truth_sign]
predictions[, .N, by = response_sign]

# remove na value
predictions_dt = na.omit(predictions)

# number of predictions by task and cv
unique(predictions_dt, by = c("task", "learner", "cv", "row_ids"))[, .N, by = c("task")]
unique(predictions_dt, by = c("task", "learner", "cv", "row_ids"))[, .N, by = c("task", "cv")]

# classification measures across ids
# q: What is the defference between true positive rate and precision
# a: TPR is the ratio of correctly predicted positive observations to the all observations in actual class
#    Precision is the ratio of correctly predicted positive observations to the total predicted positive observations
#    TPR = TP / (TP + FN)
#    Precision = TP / (TP + FP)
measures = function(t, res) {
  list(acc   = mlr3measures::acc(t, res),
       fbeta = mlr3measures::fbeta(t, res, positive = "1"),
       tpr   = mlr3measures::tpr(t, res, positive = "1"),
       tnr   = mlr3measures::tnr(t, res, positive = "1"),
        precision = mlr3measures::precision(t, res, positive = "1"))
}
predictions_dt[, measures(truth_sign, response_sign), by = c("task")]
predictions_dt[, measures(truth_sign, response_sign), by = c("learner")]
predictions_dt[, measures(truth_sign, response_sign), by = c("cv", "task")]
predictions_dt[, measures(truth_sign, response_sign), by = c("cv", "learner")]
# predictions[, measures(truth_sign, response_sign), by = c("cv", "task", "learner")][order(V1)]



# create truth factor
predictions_dt[, truth_sign := as.factor(sign01(truth))]

# prediction to wide format
predictions_dt[, .N, by = c("task", "symbol", "month")][, table(N)]
predictions_wide = dcast(
  predictions_dt,
  task + symbol + month + truth + truth_sign + cv ~ learner,
  value.var = "response"
)

# ensambles
cols = colnames(predictions_wide)
cols = cols[which(cols == "glmnet"):ncol(predictions_wide)]
p = predictions_wide[, ..cols]
pm = as.matrix(p)
predictions_wide = cbind(predictions_wide, mean_resp = rowMeans(p, na.rm = TRUE))
predictions_wide = cbind(predictions_wide, median_resp = rowMedians(pm, na.rm = TRUE))
predictions_wide = cbind(predictions_wide, sum_resp = rowSums2(pm, na.rm = TRUE))
predictions_wide = cbind(predictions_wide, iqrs_resp = rowIQRs(pm, na.rm = TRUE))
predictions_wide = cbind(predictions_wide, sd_resp = rowMads(pm, na.rm = TRUE))
predictions_wide = cbind(predictions_wide, q9_resp = rowQuantiles(pm, probs = 0.9, na.rm = TRUE))
predictions_wide = cbind(predictions_wide, max_resp = rowMaxs(pm, na.rm = TRUE))
predictions_wide = cbind(predictions_wide, min_resp = rowMins(pm, na.rm = TRUE))
predictions_wide = cbind(predictions_wide, all_buy = rowAlls(pm >= 0, na.rm = TRUE))
predictions_wide = cbind(predictions_wide, all_sell = rowAlls(pm < 0, na.rm = TRUE))
predictions_wide = cbind(predictions_wide, sum_buy = rowSums2(pm >= 0, na.rm = TRUE))
predictions_wide = cbind(predictions_wide, sum_sell = rowSums2(pm < 0, na.rm = TRUE))
predictions_wide

# results by ensamble statistics for classification measures
calculate_measures = function(t, res) {
  list(acc       = mlr3measures::acc(t, res),
       fbeta     = mlr3measures::fbeta(t, res, positive = "1"),
       tpr       = mlr3measures::tpr(t, res, positive = "1"),
       precision = mlr3measures::precision(t, res, positive = "1"),
       tnr       = mlr3measures::tnr(t, res, positive = "1"),
       npv       = mlr3measures::npv(t, res, positive = "1"))
}
predictions_wide[, calculate_measures(truth_sign, as.factor(sign01(mean_resp))), by = task]
predictions_wide[, calculate_measures(truth_sign, as.factor(sign01(median_resp))), by = task]
predictions_wide[, calculate_measures(truth_sign, as.factor(sign01(sum_resp))), by = task]
predictions_wide[, calculate_measures(truth_sign, as.factor(sign01(max_resp + min_resp))), by = task]
predictions_wide[, calculate_measures(truth_sign, as.factor(sign01(q9_resp))), by = task]
predictions_wide[, calculate_measures(truth_sign, as.factor(sign01(max_resp))), by = task]

# calculate what would be sum of truth if xgboost, ranger or rsm are greater than 0
cols = colnames(dt)
cols = cols[which(cols == "bart"):which(cols == "sum_resp")]
cols = c("task", cols)
melt(na.omit(dt[, ..cols]), id.vars = "task")[value > 0, sum(value), by = .(task, variable)][order(V1)]
melt(na.omit(dt[, ..cols]), id.vars = "task")[value > 0 & value < 2, sum(value), by = .(task, variable)][order(V1)]

#  save to azure for QC backtest
cont = storage_container(BLOBENDPOINT, "qc-backtest")
file_name_ =  paste0("pead_qc.csv")
qc_data = unique(na.omit(dt), by = c("task", "symbol", "date"))
qc_data[, .(min_date = min(date), max_date = max(date))]
storage_write_csv(qc_data, cont, file_name_)


# CURRENT -----------------------------------------------------------------
# what we should buy by learners
dt_last = dt[month == max(month)]
dt_last[, .(symbol, month, cv, ranger)] |>
  _[cv == 234] |>
  _[order(ranger)]


