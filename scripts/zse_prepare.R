options(progress_enabled = FALSE)

library(data.table)
library(checkmate)
library(finfeatures)
library(gausscov)
library(mlr3)
library(mlr3verse)
library(ggplot2)
library(DescTools)
library(TTR)
library(reticulate)
library(lubridate)
library(AzureStor)
library(PerformanceAnalytics)
library(fs)

# python packages
use_virtualenv("C:/Users/Mislav/projects_py/pyquant", required = TRUE)
builtins = import_builtins(convert = FALSE)
main = import_main(convert = FALSE)
tsfel = reticulate::import("tsfel", convert = FALSE)
# # tsfresh = reticulate::import("tsfresh", convert = FALSE)
warnigns = reticulate::import("warnings", convert = FALSE)
warnigns$filterwarnings('ignore')


# SET UP ------------------------------------------------------------------
# paths
PATH_PREDICTORS = "F:/zse/predictors/weekly"


# DATA --------------------------------------------------------------------
# import prices
prices = fread("F:/zse/prices.csv")

# remove column change
prices[, change := NULL]

# keep only data after 1999
prices = prices[date >= "2000-01-01"]

# remove duplicates
prices = unique(prices, by = c("isin", "date"))

# remove observations where open, high, low, close columns are below 1e-008
prices = prices[open > 1e-008 & high > 1e-008 & low > 1e-008 & close > 1e-008]

# keep only isin with at least 2 years of data
isin_keep = prices[, .N, isin][N >= 2 * 252, isin]
prices = prices[isin %chin% isin_keep]

# Missing values
prices[is.na(close), close := average]

# Sort
setorder(prices, isin, date)

# Add monthly column
prices[, date := as.IDate(date)]
prices[, week := ceiling_date(date, unit = "week") - 1]

# Checks: a)Test if week is fine b) max date
prices[, all(week >= date)]
prices[, diff(sort(unique(week))) == 7]
prices[, max(date)]

# Create a column last_month_day that will be equal to TRUE if date is last day of the month
prices[, last_week_day := last(date, 1) == date, by = c("isin", "week")]

# Check if last_week_day works as expected
isin_ = "HRHT00RA0005"
prices[isin == isin_, .(isin, date, close, week, last_week_day)][1:99]
tail(prices[isin == isin_, .(isin, date, close, week, last_week_day)], 100)

# Plot one stock
data_plot = as.xts.data.table(prices[isin == isin_, .(date, close)])
plot(data_plot, main = isin_)

#  remove symbols with low number of observations
# 0.6 returns 108 symbols; 0.7 returns 91 symbols; 0.75 returns 83 symbols
threshold = 0.7
monnb = function(d) { lt = as.POSIXlt(as.Date(d, origin="1900-01-01")); lt$year*52 + lt$mon*4 }
mondf = function(d1, d2) { as.integer(monnb(d2) - monnb(d1)) }
diff_in_weeks = prices[, .(monthdiff = mondf(min(date), max(date))), by = "isin"]
diff_in_weeks[, table(monthdiff)]
symbols_keep = prices[last_week_day == TRUE][
  , .(weeks_ = as.integer(.N)), by = isin][
    diff_in_weeks[, .(isin, monthdiff)], on = "isin"]
symbols_keep[, keep := weeks_ / monthdiff]
hist(symbols_keep[, keep])
symbols_keep[keep > threshold]
prices = prices[isin %chin% symbols_keep[keep > threshold, isin]]


# SUMMARY STATISTICS ------------------------------------------------------
# number of companies through time
n_firms = prices[, .N, by = date]
setorder(n_firms, date)
n_firms[, N_SMA := TTR::SMA(N, 22)]
n_firms = na.omit(n_firms)
ggplot(n_firms, aes(x = date, y = N_SMA)) + 
  geom_line() + geom_point() + 
  theme_bw() + 
  labs(title = "Simple moving average of number of companies through time", 
       x = "Date", y = "SMA Number of companies")

# summary statistics for stocks returns
prices_month = prices[last_week_day == TRUE, .(isin, date, close)]
prices_month[, returns := close / shift(close, 1) - 1]
summary_by_symbol = prices_month[, .(
  mean = mean(returns, na.rm = TRUE),
  median = median(returns, na.rm = TRUE),
  sd = sd(returns, na.rm = TRUE),
  skew = skewness(returns, na.rm = TRUE),
  kurt = kurtosis(returns, na.rm = TRUE),
  min = min(returns, na.rm = TRUE),
  max = max(returns, na.rm = TRUE)
), by = isin]
summary_returns = summary_by_symbol[, lapply(.SD, mean), 
                                    .SDcols = c("mean", "median", "sd", "skew", 
                                                "kurt", "min", "max")]
summary_returns


# PREDICTORS --------------------------------------------------------------
# Ohlcv feaures
OhlcvInstance = Ohlcv$new(prices, id_col = "isin", date_col = "date")

# Define lag parameter
lag_ = 0L

# Util function that returns most recently saved predictor object
get_latest = function(predictors) {
  # predictors = "RollingExuberFeatures"
  # dir_ls(gsub("/$", "", PATH_PREDICTORS), regexp = paste0("(^", predictors, ".*)"))
  f = file.info(dir_ls(PATH_PREDICTORS, regexp = paste0(".*/", predictors)))
  if (length(f) == 0) {
    print(paste0("There is no file with ", predictors))
    return(NULL)
  }
  latest = tail(f[order(f$ctime), ], 1)
  row.names(latest)
}

# Help function to import existing data
f_fread = function(x, remove_last_week = TRUE) {
  # x = "RollingGpdFeatures"
  # dt_ = tryCatch(fread(get_latest(x)), error = function(e) NULL)
  dt_ = tryCatch(fread(get_latest(x)), error = function(e) NULL)
  if (remove_last_week == TRUE) {
    weeks = dt_[, unique(ceiling_date(date, unit = "week") - 1)]
    last_week = tail(sort(weeks), 2)[1]
    return(dt_[date < last_week])
  } else {
    return(dt_)
  }
}

# Import existing data based on strategy
RollingBackCusumFeatures    = f_fread("RollingBackCusumFeatures")
RollingExuberFeatures       = f_fread("RollingExuberFeatures")
RollingForecatsFeatures     = f_fread("RollingForecatsFeatures")
RollingTheftCatch22Features = f_fread("RollingTheftCatch22Features")
RollingTheftTsfelFeatures   = f_fread("RollingTheftTsfelFeatures")
RollingTsfeaturesFeatures   = f_fread("RollingTsfeaturesFeatures")
RollingWaveletArimaFeatures = f_fread("RollingWaveletArimaFeatures")

# Util function for identifying missing dates and create at_ object
get_at_ = function(predictors) {
  # debug
  # predictors = copy(RollingBackCusumFeatures)
  # predictors = NULL

  if (is.null(predictors)) {
    at_ = OhlcvInstance$X[, which(last_week_day == TRUE)]
  } else {
    # get only new data
    new_dataset = fsetdiff(OhlcvInstance$X[last_week_day == TRUE, 
                                           .(symbol, date = as.IDate(date))],
                           predictors[, .(symbol, date)])
    if (nrow(new_dataset) == 0) {
      print("No new data.")
      return(NULL)
    }
    at_ = new_dataset[, index := 1][OhlcvInstance$X, on = c("symbol", "date")]
    at_ = at_[, which(last_week_day == TRUE & index == 1)]
  }
  at_
}

# Help function to create file name
create_file_name = function(name) {
  time_ = format.POSIXct(Sys.time(), format = "%Y%m%d%H%M%S")
  file_ = paste0(name, "-", time_, ".csv")
  file_ = path(PATH_PREDICTORS, file_)
  return(file_)
}

# Help function for number of workers
get_workers = function(at_) if (length(at_) < 50) 1L else 6L

# BackCUSUM features
print("Calculate BackCUSUM features.")
at_ = get_at_(RollingBackCusumFeatures)
if (length(at_) > 0) {
  RollingBackcusumInit = RollingBackcusum$new(
    windows = c(66, 125),
    workers = if (length(at_) < 50) 1L else 4L,
    at = at_,
    lag = lag_,
    alternative = c("greater", "two.sided"),
    return_power = c(1, 2)
  )
  RollingBackCusumFeatures_new = RollingBackcusumInit$get_rolling_features(OhlcvInstance)
  
  # merge and save
  RollingBackCusumFeatures_new[, date := as.IDate(date)]
  RollingBackCusumFeatures_new_merged = rbind(RollingBackCusumFeatures, RollingBackCusumFeatures_new)
  fwrite(RollingBackCusumFeatures_new_merged, create_file_name("RollingBackCusumFeatures"))
}

# Exuber features
print("Calculate Exuber features.")
at_ = get_at_(RollingExuberFeatures)
if (length(at_) > 0) {
  RollingExuberInit = RollingExuber$new(
    windows = c(125, 250, 500),
    workers = get_workers(at_),
    at = at_,
    lag = lag_,
    exuber_lag = 1L
  )
  RollingExuberFeaturesNew = RollingExuberInit$get_rolling_features(OhlcvInstance, TRUE)
  
  # merge and save
  RollingExuberFeaturesNew[, date := as.IDate(date)]
  RollingExuberFeatures_new_merged = rbind(RollingExuberFeatures, RollingExuberFeaturesNew)
  fwrite(RollingExuberFeatures_new_merged, create_file_name("RollingExuberFeatures"))
}

# Forecast Features
print("Calculate AutoArima features.")
at_ = get_at_(RollingForecatsFeatures)
if (length(at_) > 0) {
  RollingForecatsInstance = RollingForecats$new(
    windows = 500,
    workers = get_workers(at_),
    lag = lag_,
    at = at_,
    forecast_type = c("autoarima", "nnetar", "ets"),
    h = 22
  )
  RollingForecatsFeaturesNew = RollingForecatsInstance$get_rolling_features(OhlcvInstance)
  
  # merge and save
  RollingForecatsFeaturesNew[, date := as.IDate(date)]
  RollingForecatsFeaturesNewMerged = rbind(RollingForecatsFeatures, RollingForecatsFeaturesNew)
  fwrite(RollingForecatsFeaturesNewMerged, create_file_name("RollingForecatsFeatures"))
}

# Theft catch22 features
print("Calculate Catch22 and feasts features.")
at_ = get_at_(RollingTheftCatch22Features)
if (length(at_) > 0) {
  RollingTheftInit = RollingTheft$new(
    windows = c(22, 66, 250),
    workers = get_workers(at_),
    at = at_,
    lag = lag_,
    features_set = c("catch22", "feasts")
  )
  RollingTheftCatch22FeaturesNew = RollingTheftInit$get_rolling_features(OhlcvInstance)
  
  # save
  RollingTheftCatch22FeaturesNew[, date := as.IDate(date)]
  # RollingTheftCatch22FeaturesNew[, c("feasts____22_5", "feasts____25_22") := NULL]
  RollingTheftCatch22FeaturesNewMerged = rbind(RollingTheftCatch22Features, 
                                               RollingTheftCatch22FeaturesNew, fill = TRUE)
  fwrite(RollingTheftCatch22FeaturesNewMerged, create_file_name("RollingTheftCatch22Features"))
}

# Tsfeatures features
# Error in checkForRemoteErrors(val) : 
#   one node produced an error: ℹ In index: 1.
# Caused by error in `outlierinclude_mdrmd()`:
#   ! The time series is a constant!
# # tsfeatures features
# FOR 66 WINDOW
print("Calculate tsfeatures features.")
at_ = get_at_(RollingTsfeaturesFeatures)
if (length(at_) > 0) {
  RollingTsfeaturesInit = RollingTsfeatures$new(
    windows = c(250),
    workers = get_workers(at_),
    at = at_,
    lag = lag_,
    scale = TRUE
  )
  RollingTsfeaturesFeaturesNew = RollingTsfeaturesInit$get_rolling_features(OhlcvInstance)
  
  # save
  RollingTsfeaturesFeaturesNew[, date := as.IDate(date)]
  RollingTsfeaturesFeaturesNewMerged = rbind(RollingTsfeaturesFeatures, 
                                             RollingTsfeaturesFeaturesNew, fill = TRUE)
  fwrite(RollingTsfeaturesFeaturesNewMerged, create_file_name("RollingTsfeaturesFeatures"))
}

# theft tsfel features, Must be alone, because number of workers have to be 1L
print("Calculate tsfel features.")
at_ = get_at_(RollingTheftTsfelFeatures)
if (length(at_) > 0) {
  RollingTheftInit = RollingTheft$new(
    windows = c(88, 250),
    workers = get_workers(at_),
    at = at_,
    lag = lag_,
    features_set = "TSFEL"
  )
  RollingTheftTsfelFeaturesNew = suppressMessages(RollingTheftInit$get_rolling_features(OhlcvInstance))
  
  # save
  RollingTheftTsfelFeaturesNew[, date := as.IDate(date)]
  RollingTheftTsfelFeaturesNewMerged = rbind(RollingTheftTsfelFeatures, 
                                             RollingTheftTsfelFeaturesNew) # , fill = TRUE
  fwrite(RollingTheftTsfelFeaturesNewMerged, create_file_name("RollingTheftTsfelFeatures"))
}

# Wavelet arima
print("Wavelet predictors")
at_ = get_at_(RollingWaveletArimaFeatures)
if (length(at_) > 0) {
  RollingWaveletArimaInstance = RollingWaveletArima$new(
    windows = 250,
    workers = get_workers(at_),
    lag = lag_,
    at = at_,
    filter = "haar"
  )
  RollingWaveletArimaFeaturesNew = RollingWaveletArimaInstance$get_rolling_features(OhlcvInstance)
  
  # save
  RollingWaveletArimaFeaturesNew[, date := as.IDate(date)]
  RollingWaveletArimaFeaturesNewMerged = rbind(RollingWaveletArimaFeatures, 
                                               RollingWaveletArimaFeaturesNew) # , fill = TRUE
  fwrite(RollingWaveletArimaFeaturesNewMerged, create_file_name("RollingWaveletArimaFeatures"))
}

# Import existing data based on strategy
RollingBackCusumFeatures    = f_fread("RollingBackCusumFeatures", FALSE)
RollingExuberFeatures       = f_fread("RollingExuberFeatures", FALSE)
RollingForecatsFeatures     = f_fread("RollingForecatsFeatures", FALSE)
RollingTheftCatch22Features = f_fread("RollingTheftCatch22Features", FALSE)
RollingTheftTsfelFeatures   = f_fread("RollingTheftTsfelFeatures", FALSE)
RollingTsfeaturesFeatures   = f_fread("RollingTsfeaturesFeatures", FALSE)
RollingWaveletArimaFeatures = f_fread("RollingWaveletArimaFeatures", FALSE)

# Merge all features test
rolling_predictors = Reduce(
  function(x, y) merge( x, y, by = c("symbol", "date"), all.x = TRUE, all.y = FALSE),
  list(
    RollingBackCusumFeatures,
    RollingExuberFeatures,
    RollingForecatsFeatures,
    RollingTheftCatch22Features,
    RollingTheftTsfelFeatures,
    RollingTsfeaturesFeatures,
    RollingWaveletArimaFeatures
  )
)

# Features from OHLLCV
at_ = get_at_(NULL)
OhlcvFeaturesInit = OhlcvFeaturesDaily$new(at = NULL,
                                           windows = c(22, 66, 125, 250, 500),
                                           quantile_divergence_window =  c(22, 66, 125, 250, 500))
OhlcvFeaturesSet = OhlcvFeaturesInit$get_ohlcv_features(copy(OhlcvInstance$X))
OhlcvFeaturesSetSample = OhlcvFeaturesSet[at_ - lag_]
setorderv(OhlcvFeaturesSetSample, c("symbol", "date"))

# check if dates from Ohlcvfeatures and Rolling features are as expeted
isin_ = "HRHT00RA0005"
OhlcvFeaturesSetSample[symbol == isin_, .(symbol, date_ohlcv = date)]
rolling_predictors[symbol == isin_, .(symbol, date_rolling = date)]
# Seems good!

# Merge all predictors
rolling_predictors[, date_rolling := date]
OhlcvFeaturesSetSample[, date_ohlcv := date]
features = rolling_predictors[OhlcvFeaturesSetSample, on=c("symbol", "date"), roll = -Inf]

# check again merging dates
features[symbol == isin_, .(symbol, date_rolling, date_ohlcv, date)]
features[, max(date)]

# check for duplicates
anyDuplicated(features, by = c("symbol", "date"))
anyDuplicated(features, by = c("symbol", "date_rolling"))
anyDuplicated(features, by = c("symbol", "date_ohlcv"))
if (anyDuplicated(features, by = c("symbol", "date_rolling"))) {
  features = unique(features, by = c("symbol", "date_rolling")) 
}

# Merge predictors and weekly prices
anyDuplicated(features, by = c("symbol", "date"))
anyDuplicated(features, by = c("symbol", "date_rolling"))
features[, .(symbol, date_rolling, date_ohlcv, date)]
prices[last_week_day == TRUE, .(isin, date, week)]
dt = merge(features, prices[last_week_day == TRUE, .(isin, date)],
           by.x = c("symbol", "date_rolling"), by.y = c("isin", "date"),
           all.x = TRUE, all.y = FALSE)
dt[, .(symbol, date, date_rolling, date_ohlcv, week)]
anyDuplicated(dt, by = c("symbol", "date"))
anyDuplicated(dt, by = c("symbol", "date_ohlcv"))
anyDuplicated(dt, by = c("symbol", "date_rolling"))

# Check predictors for last 2 weeks
dt[date > (max(date)-10)][1:10, 1:10]
x = dt[date > (max(date)-10)][, colSums(is.na(.SD))]
x[x > 0]

# remove missing ohlcv
dt = dt[!is.na(date_ohlcv)]


# FEATURES SPACE ----------------------------------------------------------
# features space from features raw
cols_remove = c("date_ohlcv", "last_week_day") # duplicate with date_ohlcv
str(dt[, 1100:ncol(dt)])
cols_non_features = c("symbol", "date", "date_rolling", "week",
                      "open", "high", "low", "close","volume", "returns")
cols_predictors = setdiff(colnames(dt), c(cols_remove, cols_non_features))
head(cols_predictors, 10)
tail(cols_predictors, 500)
cols = c(cols_non_features, cols_predictors)
dt = dt[, .SD, .SDcols = cols]


# CLEAN DATA --------------------------------------------------------------
# remove duplicates
clf_data = copy(dt)
if (anyDuplicated(clf_data, by = c("symbol", "date")) > 0) {
  clf_data = unique(clf_data, by = c("symbol", "date"))
}

# Remove columns with many NA
keep_cols = names(which(colMeans(!is.na(clf_data)) > 0.5))
print(paste0("Removing columns with many NA values: ", setdiff(colnames(clf_data), c(keep_cols, "right_time"))))
if (length(setdiff(colnames(clf_data), c(keep_cols, "right_time"))) > 0) {
  clf_data = clf_data[, .SD, .SDcols = keep_cols] 
}

# remove Inf and Nan values if they exists
is.infinite.data.frame = function(x) do.call(cbind, lapply(x, is.infinite))
keep_cols = names(which(colMeans(!is.infinite(as.data.frame(clf_data))) > 0.98))
print(paste0("Removing columns with Inf values: ", setdiff(colnames(clf_data), keep_cols)))
if (length(setdiff(colnames(clf_data), keep_cols)) > 0) {
  clf_data = clf_data[, .SD, .SDcols = keep_cols]
}

# Remove inf values
n_0 = nrow(clf_data)
clf_data = clf_data[is.finite(rowSums(clf_data[, .SD, .SDcols = is.numeric], na.rm = TRUE))]
n_1 = nrow(clf_data)
print(paste0("Removing ", n_0 - n_1, " rows because of Inf values"))

# Final checks
clf_data[, .(symbol, date, date_rolling)]
dt[, .(symbol, date, date_rolling)]
dt[, max(date)]
clf_data[, max(date)]

########## I THINK I DONT NEED THIS ANYMORE ##########
# # Merge old and new data
# clf_data_old = fread(files_[last_date_ind])
# print(paste0("Columns miusmatch between old and new data: ", 
#              setdiff(colnames(clf_data_old), colnames(clf_data))))
# clf_data[, week := as.IDate(week)]
# clf_data_new = rbindlist(list(clf_data_old, clf_data), fill = TRUE)
########## I THINK I DONT NEED THIS ANYMORE ##########

# save predictors
last_date = strftime(clf_data[, max(date)], "%Y%m%d")
file_name = paste0("zse-predictors-", last_date, ".csv")
file_name_local = fs::path("data", file_name)
fwrite(clf_data, file_name_local)

# Save to Azure blob
endpoint = "https://snpmarketdata.blob.core.windows.net/"
blob_key = readLines('./blob_key.txt')
BLOBENDPOINT = storage_endpoint(endpoint, key=blob_key)
cont = storage_container(BLOBENDPOINT, "jphd")
storage_write_csv(clf_data, cont, file_name)



# ARCHIVE -----------------------------------------------------------------
# Get last date predictors were generated
# files_ = list.files("./data",
#                     pattern = "zse-predictors-\\d{8}\\.csv",
#                     full.names = TRUE)
# last_date = as.Date(gsub("\\.csv|.*-", "", files_), format = "%Y%m%d")
# last_date_ind = which.max(last_date)
# last_date = last_date[last_date_ind]

# rolling parameters
# week_id = OhlcvInstance$X[, which(last_week_day == TRUE)]
# date_id = OhlcvInstance$X[, which(date > last_date)]
# at_ = base::intersect(week_id, date_id)
# OhlcvInstance$X[at_]
# lag_ = 0L
