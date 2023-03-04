#' @title Master Script

#' @section Pull data from several resources (hand collection)

#' @section constructing sentiment index

#' @section data cleaning




#' @section  estimation of SVAR
rm(list = ls())

# plot time series
source("./code/analysis/plot_time_series.R")

# estimate Reduced Form VAR model and find the A, B matrices
source("./code/analysis/svar.R")

# estimate IRF
source("./code/analysis/estimate_IRF.R")
source("./code/analysis/estimate_IRF_with_BS.R")

# plot IRF
source("./code/analysis/plot_IRF.R")
shock_sign <- 1
source("./code/analysis/plot_IRF_with_CI_shock_sign.R")
shock_sign <- -1
source("./code/analysis/plot_IRF_with_CI_shock_sign.R")

# estimate Variance Decomposition and export table
source("./code/analysis/estimate_VD.R")

# estimate Historical Decomposition
source("./code/analysis/estimate_HD.R")

# plot HD

