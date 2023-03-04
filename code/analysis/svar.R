#' @title Structural VAR Analysis
#' The script performs the estimation of a SVAR with recursive identification (World Ordering)
#' 

rm(list = ls())

#' Dependencies
source("./code/utility/utils.R")

# import VAR estimation functions
source("./code/utility/var_functions.R")

#' @section Preamble
inv_tol = 1e-20 #求反矩陣時做數值運算允許的最小誤差(避免singular matrix)
options(warn=-1)    # 關掉warning
options(scipen=999) #不要科學記號

#' @section Load data
path_data = "./data/cleaned_data/df.csv"
data <- read_csv(file = path_data)

#' Preprocessing
data <- data %>% 
    mutate(Date = as.Date(Date))

#' @section Prepare matrix form data of a 4-variable model
#' Adjust scales
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

By <- data_new %>% 
  select(R, Sent, LPermit, dLloan, dLhp) %>% 
  drop_na() %>%
  as.matrix()


#' @section Model setup
#' Parameters
#----- 模型設定 -----#
VAR.P = 7                       # 最大的落後項數
CONST = TRUE                    # 是否有常數項
Y     = VAR.Y(By, VAR.P)        # 設定 Y
X     = VAR.X(By, VAR.P)        # 設定 X
hrz = 19 # the length of response
shock_sign = -1 # control the positive/negative shock
num_var <- dim(By)[2]


#' @description Reduced Form VAR
###### 參數估計 ######
(Coef.OLS    = VAR.OLS(Y, X, CONST)                  )
(Sigma.OLS   = VAR.Sigma.OLS(Y, X, Coef.OLS, CONST)  )
(Sigma.MLE   = VAR.Sigma.MLE(Y, X, Coef.OLS, CONST))

#' @description Choosing Lag
# 依據AIC選擇
# VAR.P = 7

#' @description Identification Conditions

Amat = diag(num_var)
for (i in 2:num_var) {
  for (j in 1:(i-1) ) {
    Amat[i,j] <- NA
  }
}

#' it may be abstract to understand how we initialize A matrix
#' here is an example when num_var = 5
#' For a 5-variable model
# Amat = diag(5)
# Amat[2,1]  = NA; 
# Amat[3,1]  = NA; Amat[3,2]  = NA;
# Amat[4,1]  = NA; Amat[4,2]  = NA; Amat[4,3]  = NA;
# Amat[5,1]  = NA; Amat[5,2]  = NA; Amat[5,3]  = NA; Amat[5,4]  = NA;

Bmat = diag(5)
diag(Bmat) = NA


#' @description Estimate A, B matrix and implement Cholesky decomposition
C.Prime <- chol(Sigma.OLS)
C <- t(C.Prime)

# solving system of linear equation
B0 <- diag(diag(C), ncol = 5, nrow = 5)
A0 <- B0 %*% solve(C)

SVAR_AB_est <- list("A0.svar" = A0, "B0.svar" = B0)







#' #' @section Impulse Response Function (IRF) -- with C.I. (interval estimators)
#' source("./code/analysis/estimate_IRF_with_BS.R")
SVAR_AB_IRF <- VAR.svarirf.AB(By, VAR.P, Amat, Bmat, h = hrz, CONST, SVAR_AB_est = SVAR_AB_est)

#' @section Historical Decomposition
# source("./code/analysis/estimate_HD.R")

