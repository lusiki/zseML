library(fs)
library(data.table)
library(mlr3batchmark)
library(batchtools)
library(matrixStats)


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
  # id_ = 10035
  x = tryCatch({readRDS(fs::path(PATH, "results", id_, ext = "rds"))},
               error = function(e) NULL)
  if (is.null(x)) {
    print(id_)
    return(NULL)
  }
  x["id"] = id_
  x
})
predictions = lapply(predictions_l, function(x) {
  cbind.data.frame(
    id = x$id,
    row_ids = x$prediction$test$row_ids,
    truth = x$prediction$test$truth,
    response = x$prediction$test$response
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
  truth_sign = as.factor(sign(truth+0.001)),
  response_sign = as.factor(sign(response))
)]
predictions[, .N, by = truth_sign]
predictions[, .N, by = response_sign]

# remove na value
predictions_dt = na.omit(predictions)

# number of predictions by task and cv
unique(predictions_dt, by = c("task", "learner", "cv", "row_ids"))[, .N, by = c("task")]
unique(predictions_dt, by = c("task", "learner", "cv", "row_ids"))[, .N, by = c("task", "cv")]

# classification measures across ids
measures = function(t, res) {
  list(acc   = mlr3measures::acc(t, res),
       fbeta = mlr3measures::fbeta(t, res, positive = "1"),
       tpr   = mlr3measures::tpr(t, res, positive = "1"),
       tnr   = mlr3measures::tnr(t, res, positive = "1"))
}
predictions_dt[, measures(truth_sign, response_sign), by = c("task")]
predictions_dt[, measures(truth_sign, response_sign), by = c("learner")]
predictions_dt[, measures(truth_sign, response_sign), by = c("cv", "task")]
predictions_dt[, measures(truth_sign, response_sign), by = c("cv", "learner")]
# predictions[, measures(truth_sign, response_sign), by = c("cv", "task", "learner")][order(V1)]

# create truth factor
predictions_dt[, truth_sign := as.factor(sign(truth))]

# prediction to wide format
predictions_dt[, .N, by = c("task", "symbol", "month")][, table(N)]
dt = dcast(
  predictions_dt,
  task + symbol + month + truth + truth_sign + cv ~ learner,
  value.var = "response"
)

# ensambles
cols = colnames(dt)
cols = cols[which(cols == "glmnet"):ncol(dt)]
p = dt[, ..cols]
pm = as.matrix(p)
dt = cbind(dt, mean_resp = rowMeans(p, na.rm = TRUE))
dt = cbind(dt, median_resp = rowMedians(pm, na.rm = TRUE))
dt = cbind(dt, sum_resp = rowSums2(pm, na.rm = TRUE))
dt = cbind(dt, iqrs_resp = rowIQRs(pm, na.rm = TRUE))
dt = cbind(dt, sd_resp = rowMads(pm, na.rm = TRUE))
dt = cbind(dt, q9_resp = rowQuantiles(pm, probs = 0.9, na.rm = TRUE))
dt = cbind(dt, max_resp = rowMaxs(pm, na.rm = TRUE))
dt = cbind(dt, min_resp = rowMins(pm, na.rm = TRUE))
dt = cbind(dt, all_buy = rowAlls(pm >= 0, na.rm = TRUE))
dt = cbind(dt, all_sell = rowAlls(pm < 0, na.rm = TRUE))
dt = cbind(dt, sum_buy = rowSums2(pm >= 0, na.rm = TRUE))
dt = cbind(dt, sum_sell = rowSums2(pm < 0, na.rm = TRUE))
dt

# results by ensamble statistics for classification measures
calculate_measures = function(t, res) {
  list(acc       = mlr3measures::acc(t, res),
       fbeta     = mlr3measures::fbeta(t, res, positive = "1"),
       tpr       = mlr3measures::tpr(t, res, positive = "1"),
       precision = mlr3measures::precision(t, res, positive = "1"),
       tnr       = mlr3measures::tnr(t, res, positive = "1"),
       npv       = mlr3measures::npv(t, res, positive = "1"))
}
dt[, calculate_measures(truth_sign, as.factor(sign(mean_resp+0.001))), by = task]
dt[, calculate_measures(truth_sign, as.factor(sign(median_resp))), by = task]
dt[, calculate_measures(truth_sign, as.factor(sign(sum_resp))), by = task]

dt[, calculate_measures(truth_sign, as.factor(sign(max_resp + min_resp))), by = task]
dt[, calculate_measures(truth_sign, as.factor(sign(q9_resp))), by = task]
dt[, calculate_measures(truth_sign, as.factor(sign(max_resp))), by = task]


dt[all_buy == TRUE]
dt[all_buy == TRUE, calculate_measures(truth_sign, factor(ifelse(all_buy, 1, -1), levels = c(-1, 1)))]
dt[all_sell == TRUE, calculate_measures(truth_sign, factor(ifelse(all_sell, -1, 1), levels = c(-1, 1)))]
dt[all_sell == TRUE, calculate_measures(truth_sign, factor(ifelse(all_sell, -1, 1), levels = c(-1, 1)))]
dt[, calculate_measures(truth_sign, factor(ifelse(sum_buy > 6, 1, -1), levels = c(-1, 1)))]
dt[, calculate_measures(truth_sign, factor(ifelse(sum_buy > 7, 1, -1), levels = c(-1, 1)))]
dt[, calculate_measures(truth_sign, factor(ifelse(sum_buy > 8, 1, -1), levels = c(-1, 1)))]
dt[, calculate_measures(truth_sign, factor(ifelse(sum_buy > 9, 1, -1), levels = c(-1, 1)))]
dt[, calculate_measures(truth_sign, factor(ifelse(sum_buy > 9, 1, -1), levels = c(-1, 1))), by = task]
dt[, calculate_measures(truth_sign, factor(ifelse(sum_buy > 10, 1, -1), levels = c(-1, 1)))]
dt[, calculate_measures(truth_sign, factor(ifelse(sum_buy > 10, 1, -1), levels = c(-1, 1))), by = task]
dt[, calculate_measures(truth_sign, factor(ifelse(sum_sell > 10, -1, 1), levels = c(-1, 1)))]

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


# SYSTEMIC RISK -----------------------------------------------------------
# import SPY data
con <- dbConnect(duckdb::duckdb())
query <- sprintf("
    SELECT *
    FROM 'F:/lean/data/stocks_daily.csv'
    WHERE Symbol = 'spy'
")
spy <- dbGetQuery(con, query)
dbDisconnect(con)
spy = as.data.table(spy)
spy = spy[, .(date = Date, close = `Adj Close`)]
spy[, returns := close / shift(close) - 1]
spy = na.omit(spy)
plot(spy[, close])

# systemic risk
task_ = "taskRetMonth"
indicator = dt[task == task_, .(indicator = mean(mean_resp, na.rm = TRUE),
                                indicator_sd = sd(mean_resp, na.rm = TRUE),
                                indicator_q1 = quantile(mean_resp, probs = 0.01, na.rm = TRUE)),
               by = date][order(date)]
cols = colnames(indicator)[2:ncol(indicator)]
indicator[, (cols) := lapply(.SD, nafill, type = "locf"), .SDcols = cols]
indicator[, `:=`(
  indicator_ema = TTR::EMA(indicator, 5, na.rm = TRUE),
  indicator_sd_ema = TTR::EMA(indicator_sd, 5, na.rm = TRUE),
  indicator_q1_ema = TTR::EMA(indicator_q1, 5, na.rm = TRUE)
)]
indicator = na.omit(indicator)
plot(as.xts.data.table(indicator)[, 4])
plot(as.xts.data.table(indicator)[, 5])
plot(as.xts.data.table(indicator)[, 6])

# create backtest data
backtest_data =  merge(spy, indicator, by = "date", all.x = TRUE, all.y = FALSE)
min_date = indicator[, min(date)]
backtest_data = backtest_data[date > min_date]
max_date = indicator[, max(date)]
backtest_data = backtest_data[date < max_date]
cols = colnames(backtest_data)[4:ncol(backtest_data)]
backtest_data[, (cols) := lapply(.SD, nafill, type = "locf"), .SDcols = cols]
backtest_data[, signal := 1]
backtest_data[shift(indicator_ema) < 0, signal := 0]
# backtest_data[shift(indicator_sd_ema) < 4, signal := 0]
backtest_data_xts = as.xts.data.table(backtest_data[, .(date, benchmark = returns, strategy = ifelse(signal == 0, 0, returns * signal * 1))])
charts.PerformanceSummary(backtest_data_xts)
# backtest performance
Performance <- function(x) {
  cumRetx = Return.cumulative(x)
  annRetx = Return.annualized(x, scale=252)
  sharpex = SharpeRatio.annualized(x, scale=252)
  winpctx = length(x[x > 0])/length(x[x != 0])
  annSDx = sd.annualized(x, scale=252)
  
  DDs <- findDrawdowns(x)
  maxDDx = min(DDs$return)
  # maxLx = max(DDs$length)
  
  Perf = c(cumRetx, annRetx, sharpex, winpctx, annSDx, maxDDx) # , maxLx)
  names(Perf) = c("Cumulative Return", "Annual Return","Annualized Sharpe Ratio",
                  "Win %", "Annualized Volatility", "Maximum Drawdown") # "Max Length Drawdown")
  return(Perf)
}
Performance(backtest_data_xts[, 1])
Performance(backtest_data_xts[, 2])

# analyse indicator
library(forecast)
ndiffs(as.xts.data.table(indicator)[, 1])
plot(diff(as.xts.data.table(indicator)[, 1]))


# IMPORTANT VARIABLES -----------------------------------------------------
# gausscov files
gausscov_files = dir_ls("F:/H4-v9-gausscov/gausscov_f3")

# arrange files
task_ = gsub(".*f3-|-\\d+.rds", "", gausscov_files)
gausscov_dt = cbind.data.frame(gausscov_files, task = task_)
setorder(gausscov_dt, task)
gausscov_dt[gausscov_dt$task == "taskRetWeek",]
gausscov_dt[gausscov_dt$task == "taskRetMonth",]
gausscov_dt[gausscov_dt$task == "taskRetMonth2",]
gausscov_dt[gausscov_dt$task == "taskRetQuarter",]

# import gausscov vars
gausscov_l = lapply(gausscov_dt[, "gausscov_files"], readRDS)
gausscov = lapply(gausscov_l, function(x) x[x > 0])
names(gausscov) = gausscov_dt[, "task"]
gausscov = lapply(gausscov, function(x) as.data.frame(as.list(x)))
gausscov = lapply(gausscov, melt)
gausscov = rbindlist(gausscov, idcol = "task")

# most important vars across all tasks
gausscov[, sum(value), by = variable][order(V1)][, tail(.SD, 10)]
gausscov[, sum(value), by = .(task, variable)][order(V1)][, tail(.SD, 5), by = task]


# ISSUES ------------------------------------------------------------------
# slow importing
res_test = loadResult(1, reg = reg)


dt[,  max(month)]
dt[month == max(month)]
