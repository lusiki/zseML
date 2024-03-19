library(batchtools)



# Parameters
# TYPE = "predictions" # can be predictions and models
# TARGETS = "target"

# Load registry
reg = loadRegistry("./experiments_live", work.dir = "./experiments_live")

# Done ids
ids = findDone(reg=reg)

# Get metadata for done jobs
tabs = getJobTable(ids, reg = reg)
tabs = tabs[, .SD, .SDcols = c("job.id", "job.name", "repl", "prob.pars", "algo.pars")]
predictions_meta = cbind.data.frame(
  id = tabs[, job.id],
  task = vapply(tabs$prob.pars, `[[`, character(1L), "task_id"),
  learner = gsub(".*regr.|.tuned", "", vapply(tabs$algo.pars, `[[`, character(1L), "learner_id")),
  cv = gsub("custom_|_.*", "", vapply(tabs$prob.pars, `[[`, character(1L), "resampling_id")),
  fold = gsub("custom_\\d+_", "", vapply(tabs$prob.pars, `[[`, character(1L), "resampling_id"))
)

# Extract predictions
predictions_l = lapply(ids[[1]], function(id_) {
  # id_ = 1
  x = tryCatch({readRDS(fs::path("./experiments_live", "results", id_, ext = "rds"))},
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
tasks_files = dir_ls(fs::path("./experiments_live", "problems"))
tasks = lapply(tasks_files, readRDS)
names(tasks) = lapply(tasks, function(t) t$data$id)

# add backend to predictions
backend_l = lapply(tasks, function(tsk_) {
  # tsk_ = tasks[[1]]
  x = tsk_$data$backend$data(1:tsk_$data$nrow,
                             c("symbol", "week", "..row_id", "target"))
  setnames(x, "..row_id", "row_ids")
  x
})
backends = rbindlist(backend_l, fill = TRUE)

# merge predictions and backends
predictions = backends[predictions, on = c("row_ids")]

# change month to date from Posixct
predictions[, week := as.Date(week)]

# clean predictions
preds = unique(predictions, by = c("row_ids", "week", "task", "learner", "cv"))
preds = na.omit(preds)

# prediction to wide format
predsw = dcast(
  preds,
  task + cv + week + symbol + cv + truth  ~ learner,
  value.var = "response"
)

# ensambles
cols = colnames(predsw)
cols = cols[(which(cols == "truth")+1):ncol(predsw)]
p = predsw[, ..cols]
pm = as.matrix(p)
predsw = cbind(predsw, mean_resp = rowMeans(p, na.rm = TRUE))
predsw = cbind(predsw, median_resp = rowMedians(pm, na.rm = TRUE))
predsw = cbind(predsw, sum_resp = rowSums2(pm, na.rm = TRUE))

cols = colnames(predsw)
cols = cols[(which(cols == "truth")+1):which(cols == "sum_resp")]
preds_perf = melt(predsw, 
                  id.vars = c("task", "cv", "truth", "week", "symbol"),
                  measure.vars = cols)

# Create portfolio returns
portfolios = preds_perf[value > 0]
portfolios = portfolios[
  , .(dt_ = .(dcast(.SD, week ~ symbol, value.var = "truth"))), 
  by = .(cv, variable)]
portfolios[, dt_ := lapply(dt_, setnafill, fill = 0)]
portfolios[, portfolio_returns := map(dt_, function(x) Return.portfolio(x))]

calculatePortfolioStats <- function(portfolioReturns) {
  # portfolioReturns = portfolios[, portfolio_returns][[1]]
  # Ensure the input is an xts or time series object
  if (!is.xts(portfolioReturns)) {
    stop("portfolioReturns must be an xts object.")
  }
  
  # Calculate statistics
  annualizedReturn = Return.annualized(portfolioReturns)[[1]]
  annualizedSD = sqrt(52) * sd(portfolioReturns)
  sharpeRatio = SharpeRatio.annualized(portfolioReturns)[[1]]
  maxDrawdown = maxDrawdown(portfolioReturns)
  sortinoRatio = SortinoRatio(portfolioReturns)[[1]]
  
  # Create data.table from statistics
  portfolio_perf = data.table(
    "Godišnji povrati" = annualizedReturn,
    `Godišnja SD` = annualizedSD,
    `Sharpov omjer` = sharpeRatio,
    `Maksimalni gubitak` = maxDrawdown,
    `Sortinov omjer` = sortinoRatio
  )
  
  return(portfolio_perf)
}


# Calculate portfolio statistics
portfolio_stats = portfolios[, .(lapply(portfolio_returns, function(x) calculatePortfolioStats(x)))]
portfolio_stats = rbindlist(portfolio_stats[[1]])
portfolio_stats = rcbind(portfolios[, .(Model = variable, CV = cv)], portfolio_stats)

# Flextable
ft = qflextable(portfolio_stats) |>
  colformat_double()
set_table_properties(
  ft,
  width = 1,
  layout = "autofit"
)

