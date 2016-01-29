library(forecast)
library(stringr)
library(xts)
library(plyr)
library(dplyr)
library(lubridate)

#### Reading data ####
# my data
my_df_read <-
  read.csv("C:/Users/Miha/Documents/GitHub/IMF_Macro_Forecasting_R/004_module_4_Forecast_uncertainty_and_Model_evaluation/Module_4_Workfile/Thailand_M.csv", 
           stringsAsFactors=FALSE)

# selecting data to be analysed within ARIMA framework 
my_x <- my_df_read$p

# arima specification
my_arima_order <- c(1, 1, 0)
my_seasonal_order <- c(0, 0, 0)

# forecasting parameters
my_w <- 55 # training time window if last date not used
my_h <- 6  # forecasting horizon  

# last date of training subsample
is_last_date_used <- FALSE
last_date <- "2008-12-01" 

# toggle between expanding and rolling forecasting strategy
is_expanding <- TRUE

# horizon column name in accuracy data.frame
my_id <- "my_fcast_h"

# stringr parameters for padding string (leave as is)
my_pad <- "0"
my_side <- "left"
my_prefix <- "fcast_"

#### Data wrangling ####
my_df <- 
  my_df_read %>% 
  mutate(dateid01 = as.Date(dateid01)) 

# getting first and last date
my_min_max_date <-
  my_df %>% 
  summarise(my_min_year = min(year(dateid01)),
            my_max_year = max(year(dateid01)))

# filter data according to last date of training sample
my_df_train <-
  my_df %>% 
  filter(dateid01 <= last_date)

# setting training subsample window length:
# if last date of training dataset is used, 
# than window size is calculated, if not,
# window size as set by the user is used.
if(last_date == TRUE){
  my_w <- my_df_train %>% count %>% unlist %>% unname
}

# setting time series object
my_ts <- 
  ts(my_x, start = my_min_max_date[, 1], frequency = 12)

# plot ts for visual inspection of a time series
tsdisplay(my_ts, las = 1)
abline(h = 0)

# my ts data.frame
my_ts_df <-
  data.frame(my_labels = my_ts %>% as.xts %>% index %>% as.character,
             my_obs_ts = my_ts %>% as.xts %>% coredata)

# length, start, end and frequency of time series
my_t <- length(my_ts)
my_start <- tsp(my_ts)[1]
my_end <- tsp(my_ts)[2]
my_freq <- tsp(my_ts)[3]

# parameter that tells how many times one can extend the training set 
# to produce forecasts for assumed horizon.
my_myst_param <- my_t - my_w - my_h

# start of validation set
my_valid <- time(my_ts)[my_w]

# stringr parameters (width)
my_width <- my_myst_param %>% nchar

# data
y <- ts(my_ts, start = my_start, frequency = my_freq)

fcasts <- list()

# expanding forecasts
for (i in 1:my_myst_param) {
  
  if(is_expanding){
    
    win.y <- window(y, end = my_valid + i / my_freq)
    
    fit <- Arima(win.y, order = my_arima_order,
                 seasonal = my_seasonal_order,
                 include.constant = TRUE)  
  } else {
    
    win.y <- window(y, 
                    start = my_start + i / my_freq, 
                    end = my_valid + i / my_freq)
    
    fit <- Arima(win.y, order = my_arima_order,
                 seasonal = my_seasonal_order,
                 include.constant = TRUE)
    
  }
  
  fcasts[[i]] <- forecast(fit, h = my_h)
}

names(fcasts) <- 
  str_c(my_prefix,                       
        str_pad(1:my_myst_param, width = my_width, 
                pad = my_pad, side = my_side))

# getting point forecasts and corresponding dates
my_means <- list()

for(k in seq_along(fcasts)){
  
  my_mean <- fcasts[[k]]$mean %>% as.xts
  my_labels <- my_mean %>% index %>% as.character
  
  my_df <- 
    data.frame(my_labels, my_mean) %>% 
    mutate(my_h_fcasts = 1:n())
  
  my_means[[k]] <- my_df
  
}

# joining forecasts and original data
my_fcasts_df <- 
  ldply(my_means, data.frame) %>%
  left_join(., my_ts_df)

# computing accuracy statistics
my_fcasting_acc_df <-
  my_fcasts_df %>% 
  dlply(., .(my_h_fcasts)) %>% 
  lapply(.,function(x) accuracy(ts(x$my_mean), 
                                ts(x$my_obs_ts))) %>% 
  ldply(., data.frame, .id = my_id)

# a look at accuracy statistics
my_fcasting_acc_df

