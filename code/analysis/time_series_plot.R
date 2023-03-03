#' @title Sketch the raw and processed time series data
#' The script provides figures for the time series data before our structural estimation

rm(list = ls())

#' Dependencies
source("./code/utility/utils.R")

#' @section Load data
path_data = "./data/cleaned_data/df.csv"
data <- read_csv(file = path_data)

#' Preprocessing
data <- data %>% 
    mutate(Date = as.Date(Date))

#' @title Figure 1
# Make multiple plots
fig_list <- create_multiple_time_series(data, "Date", 
                                        c("R", "Sent", "Permit_TW1", "Loan3", "hp_tw"))
fig <- make_multiple_plots(fig_list)

# Save figure
ggsave_default(fig, path = "./result/figure/fig_time_series_raw.pdf")



#' @title Figure 2

# We do not input the raw level data in the structural model
# instead, we feed processed data
data_new <- data %>% 
  # choose the raw level data that were used
  select(Date, R, Sent, Permit_TW1, Loan3, hp_tw) %>%
  # processing
  mutate(LPermit_TW1 = log(Permit_TW1),
         dLloan = c(rep(NA, 4), 100*diff(log(Loan3), 4)),
         dLhp = c(rep(NA,4), 100*diff(log(hp_tw), 4))) %>%
  select(Date,                   # Date for the quarterly data 
         R,                      # interest rate
         Sent,                   # sentiment index
         LPermit = LPermit_TW1,  # Log of number of house construction permits issued
         dLloan,                 # first difference of log of the total amount of housing loan
         dLhp                    # first difference of log of house price index
         )           

# Make multiple plots
fig_list <- create_multiple_time_series(data_new, "Date", 
                                        c("R", "Sent", "LPermit", "dLloan", "dLhp"))
fig <- make_multiple_plots(fig_list)

# Save figure
ggsave_default(fig, path = "./result/figure/fig_time_series_adjusted.pdf")

