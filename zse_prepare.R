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
# Python environment and python modules
# Instructions: some functions use python modules. Steps to use python include:
# 1. create new conda environment:
#    https://docs.conda.io/projects/conda/en/latest/user-guide/tasks/manage-environments.html
#    Choose python 3.8. for example:
#    conda create -n mlfinlabenv python=3.8
# 2. Install following packages inside environments
#    mlfinlab
#    tsfresh
#    TSFEL
# python packages
reticulate::use_python("C:/Users/Mislav/.conda/envs/mlfinlabenv/python.exe", required = TRUE)
# # mlfinlab = reticulate::import("mlfinlab", convert = FALSE)
# pd = reticulate::import("pandas", convert = FALSE)
builtins = import_builtins(convert = FALSE)
main = import_main(convert = FALSE)
tsfel = reticulate::import("tsfel", convert = FALSE)
# # tsfresh = reticulate::import("tsfresh", convert = FALSE)
warnigns = reticulate::import("warnings", convert = FALSE)
warnigns$filterwarnings('ignore')




# SET UP ------------------------------------------------------------------
# paths
PATH_PREDICTORS = "F:/zse/predictors/monthly"


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

# missing values
prices[is.na(close), close := average]

# sort
setorder(prices, isin, date)

# add monthly column
prices[, date := as.IDate(date)]
prices[, month := round(date, digits = "month")]
prices[, month := ceiling_date(month, unit = "month") - 1]

# create a column last_month_day that will be equal to TRUE if date is last day of the month
prices[, last_month_day := last(date, 1) == date, by = c("isin", "month")]

# check if last_month_day works as expected
isin_ = "HRHT00RA0005"
prices[isin == isin_, .(isin, date, close, month, last_month_day)][1:99]
tail(prices[isin == isin_, .(isin, date, close, month, last_month_day)], 100)

# plot one stock
data_plot = as.xts.data.table(prices[isin == isin_, .(date, close)])
plot(data_plot, main = isin_)


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
prices_month = prices[last_month_day == TRUE, .(isin, date, close)]
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


# PREDICTORS --------------------------------------------------------------
# Ohlcv feaures
OhlcvInstance = Ohlcv$new(prices[, .(isin, date, open, high, low, close, volume, last_month_day)],
                          id_col = "isin",
                          date_col = "date")

# rolling parameters
at_ = which(OhlcvInstance$X[, .(last_month_day)][[1]])
which(is.na(OhlcvInstance$X[at_, close]))
which(is.na(OhlcvInstance$X[at_, returns]))
lag_ = 1L

# Exuber
RollingExuberInit = RollingExuber$new(windows = c(125, 250, 500),
                                      workers = 4L,
                                      at = at_,
                                      lag = lag_,
                                      exuber_lag = 1L)
RollingExuberFeatures = RollingExuberInit$get_rolling_features(OhlcvInstance$clone(), TRUE)


# Forecast Features
RollingForecatsInstance = RollingForecats$new(windows = c(500),
                                              workers = 4L,
                                              lag = lag_, 
                                              at = at_,
                                              forecast_type = c("autoarima", "nnetar", "ets"),
                                              h = 22)
RollingForecatsFeatures = RollingForecatsInstance$get_rolling_features(OhlcvInstance$clone())

# BackCUSUM features
RollingBackcusumInit = RollingBackcusum$new(windows = c(66, 125), 
                                            workers = 4L,
                                            at = at_, 
                                            lag = lag_,
                                            alternative = c("greater", "two.sided"),
                                            return_power = c(1, 2))
RollingBackCusumFeatures = RollingBackcusumInit$get_rolling_features(OhlcvInstance$clone())

# theft features
RollingTheftInit = RollingTheft$new(windows = c(22, 66, 250),
                                    workers = 6L, 
                                    at = at_, 
                                    lag = lag_,
                                    features_set = c("catch22", "feasts"))
RollingTheftCatch22Features = RollingTheftInit$get_rolling_features(OhlcvInstance$clone())

# Error in checkForRemoteErrors(val) : 
#   one node produced an error: ℹ In index: 1.
# Caused by error in `outlierinclude_mdrmd()`:
#   ! The time series is a constant!
# # tsfeatures features
# FOR 66 WINDOW
RollingTsfeaturesInit = RollingTsfeatures$new(windows = c(125, 250),
                                              workers = 6L,
                                              at = at_,
                                              lag = lag_,
                                              scale = TRUE)
RollingTsfeaturesFeatures = RollingTsfeaturesInit$get_rolling_features(OhlcvInstance$clone())

# theft
RollingTheftInit = RollingTheft$new(windows = c(66, 250), 
                                    workers = 1L,
                                    at = at_, 
                                    lag = lag_,  
                                    features_set = "TSFEL")
RollingTheftTsfelFeatures = suppressMessages(RollingTheftInit$get_rolling_features(OhlcvInstance$clone()))

# Wavelet arima
RollingWaveletArimaInstance = RollingWaveletArima$new(windows = 250, 
                                                      workers = 6L,
                                                      lag = lag_, 
                                                      at = at_, 
                                                      filter = "haar")
RollingWaveletArimaFeatures = RollingWaveletArimaInstance$get_rolling_features(OhlcvInstance$clone())

# merge all features test
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

# merge all predictors
rolling_predictors[, date_rolling := date]
OhlcvFeaturesSetSample[, date_ohlcv := date]
features = rolling_predictors[OhlcvFeaturesSetSample, on=c("symbol", "date"), roll = -Inf]

# check again merging dates
features[symbol == isin_, .(symbol, date_rolling, date_ohlcv, date)]
features[, max(date)]

# check for duplicates
features[duplicated(features[, .(symbol, date)]), .(symbol, date)]
features[duplicated(features[, .(symbol, date_ohlcv)]), .(symbol, date_ohlcv)]
features[duplicated(features[, .(symbol, date_rolling)]), .(symbol, date_rolling)]
features[duplicated(features[, .(symbol, date_rolling)]) | duplicated(features[, .(symbol, date_rolling)], fromLast = TRUE),
         .(symbol, date, date_ohlcv, date_rolling)]
features = unique(features, by = c("symbol", "date_rolling"))

# merge predictors and monthly prices
any(duplicated(prices[, .(isin, date)]))
any(duplicated(features[, .(symbol, date_rolling)]))
features[, .(symbol, date_rolling, date_ohlcv, date)]
prices[last_month_day == TRUE, .(isin, date, month)]
dt = merge(features, prices[last_month_day == TRUE, .(isin, date, month)],
           by.x = c("symbol", "date_rolling"), by.y = c("isin", "date"),
           all.x = TRUE, all.y = FALSE)
dt[, .(symbol, date, date_rolling, date_ohlcv)]
dt[duplicated(dt[, .(symbol, date)]), .(symbol, date)]
dt[duplicated(dt[, .(symbol, date_ohlcv)]), .(symbol, date_ohlcv)]
dt[duplicated(dt[, .(symbol, date_rolling)]), .(symbol, date_rolling)]

# remove missing ohlcv
dt = dt[!is.na(date_ohlcv)]


# FEATURES SPACE ----------------------------------------------------------
# features space from features raw
cols_remove = c("date_ohlcv", "last_month_day") # duplicate with date_ohlcv
str(dt[, 1100:ncol(dt)])
cols_non_features <- c("symbol", "date", "date_rolling", "month",
                       "open", "high", "low", "close","volume", "returns")
cols_predictors = setdiff(colnames(dt), c(cols_remove, cols_non_features))
head(cols_predictors, 10)
tail(cols_predictors, 500)
cols = c(cols_non_features, cols_predictors)
dt = dt[, .SD, .SDcols = cols]


# CLEAN DATA --------------------------------------------------------------
# remove duplicates
clf_data = copy(dt)
any(duplicated(clf_data[, .(symbol, date)]))
clf_data = unique(clf_data, by = c("symbol", "date"))

# remove columns with many NA
keep_cols = names(which(colMeans(!is.na(clf_data)) > 0.5))
print(paste0("Removing columns with many NA values: ", setdiff(colnames(clf_data), c(keep_cols, "right_time"))))
clf_data = clf_data[, .SD, .SDcols = keep_cols]

# remove Inf and Nan values if they exists
is.infinite.data.frame = function(x) do.call(cbind, lapply(x, is.infinite))
keep_cols = names(which(colMeans(!is.infinite(as.data.frame(clf_data))) > 0.98))
print(paste0("Removing columns with Inf values: ", setdiff(colnames(clf_data), keep_cols)))
clf_data = clf_data[, .SD, .SDcols = keep_cols]

# remove inf values
n_0 <- nrow(clf_data)
clf_data <- clf_data[is.finite(rowSums(clf_data[, .SD, .SDcols = is.numeric], na.rm = TRUE))]
n_1 <- nrow(clf_data)
print(paste0("Removing ", n_0 - n_1, " rows because of Inf values"))

# final checks
clf_data[, .(symbol, date, date_rolling)]
dt[, .(symbol, date, date_rolling)]
dt[, max(date)]
clf_data[, max(date)]

# save predictors
last_date = strftime(clf_data[, max(date)], "%Y%m%d")
file_name = paste0("zse-predictors-", last_date, ".csv")
file_name_local = fs::path("data", file_name)
fwrite(clf_data, file_name_local)

# Save to Azure blob
# file_name = "zse-predictors-20240117.csv"
# clf_data = fread(file.path("data", file_name))
endpoint = "https://snpmarketdata.blob.core.windows.net/"
blob_key = readLines('./blob_key.txt')
BLOBENDPOINT = storage_endpoint(endpoint, key=blob_key)
cont = storage_container(BLOBENDPOINT, "jphd")
storage_write_csv(clf_data, cont, file_name)
