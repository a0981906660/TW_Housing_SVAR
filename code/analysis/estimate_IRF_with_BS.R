#' @title Estimate IRF with Bootstrap Confidence Interval
# Load minimal data
source("./code/analysis/svar.R")
rm(list = setdiff(ls(), c("By", "VAR.P", "CONST", "Y", "X", "hrz", "shock_sign", "num_var", "inv_tol")))

source("./code/utility/utils.R")
source("./code/utility/var_functions.R")


lower = 0.025                                        # 控制成 95% CI
upper = 1-lower
ddY = VAR.ddY(By, VAR.P)
ddX = VAR.ddX(By, VAR.P)

# dim(ddY); dim(ddX)
# the shape of ddX is available length of the data (deduct lag length) by lag*num_var
# the shape of ddY is available length of the data (deduct lag length) by num_var

T   = nrow(ddY)
T.total= nrow(By)
Ik  = diag(rep(1, num_var))
# How many coefficients are there? 
# For example, we have a 5-variable model, lag = 7, and include intercept is True
# => (5 variables * 7 lags + 1 intercept)* 5 equations = (35+1)*5 = 180
Coef = t(VAR.EbyE(ddY, ddX, CONST)$ddA)              # Step 1 估計模型
# residuals
U    = VAR.EbyE(ddY, ddX, CONST)$ddU
BSigma.u = VAR.ddSigma.OLS(ddY, ddX, CONST)
if(CONST == TRUE){
  const = Coef[, ncol(Coef)]
  Coef.noc= Coef[,-ncol(Coef)]                      # 刪掉 const
}else{
  const = matrix(0, num_var, 1)
  Coef.noc = Coef
}

Theta.unit= VAR.Theta(Coef, hrz + 1, BSigma.u, CONST)$unit # 估算 Theta.unit
Theta.std = VAR.Theta(Coef, hrz + 1, BSigma.u, CONST)$std  # 估算 Theta.std

dm.U <- U

N = 2000 #重抽次數
Theta.unit.sim = vector("list", N)
Theta.std.sim  = vector("list", N)

# check dimension
cat(">>> check dimensionality\n")
assert_that(dim(ddX)[1] == dim(By)[1] - VAR.P)
assert_that(dim(ddX)[2] == num_var * VAR.P)
assert_that(dim(Coef.noc)[1] == num_var)
assert_that(dim(Coef.noc)[2] == num_var*VAR.P)
assert_that(dim(Coef.noc)[2] == num_var*VAR.P)
assert_that(dim(dm.U)[1] == dim(By)[1] - VAR.P)
assert_that(dim(dm.U)[2] == num_var)

# 存N次重抽的IRF
df_IRF.sim <- array(NA, c(hrz+1, num_var^2, N)) #dimensions are: Time Period, Number of shock interacts with variables, page (number of Bootstrap resamplings)
counter <- 1
while(TRUE){
  
  #cat("Now, there are ", counter-1, " sets of resamples.\n")
  Y.sim = matrix(0, nrow = T.total, ncol = num_var)          # Y.sim = 0 #pseudo time series
  Y.sim[c(1:VAR.P),] = By[c(1:VAR.P), ] #initial values
  
  boot.number = sample(c(1:T), replace = TRUE)      # Step 3 取出放回
  U.sim = dm.U[boot.number,]
  
  # predicted values given the above initial values
  last.y= c(t(By[VAR.P:1,]))
  for(ii in 1:T){
    last.y = last.y[1:(num_var*VAR.P)]
    Y.sim[ii+VAR.P, ] = Coef.noc %*% last.y + const + U.sim[ii,]      # Step 4 模擬資料
    last.y = c(Y.sim[ii+VAR.P,], last.y)
  }
  
  #   Y.sim[-c(1:VAR.P),] <- matrix(const, nrow = T.total-VAR.P, ncol = num_var, byrow = T) + ddX %*% t(Coef.noc) + U.sim
  
  
  #`Y.sim` is the pseudo time series
  # Step 5 重新估算SVAR
  
  ### SVAR.sim Start ###
  
  Y_pseudo     = VAR.Y(Y.sim, VAR.P)        # 設定 Y
  X_pseudo     = VAR.X(Y.sim, VAR.P)        # 設定 X
  Coef.OLS_pseudo    = VAR.OLS(Y_pseudo, X_pseudo, CONST)
  Sigma.OLS_pseudo   = VAR.Sigma.OLS(Y_pseudo, X_pseudo, Coef.OLS_pseudo, CONST)
  C.Prime_pseudo <- chol(Sigma.OLS_pseudo)
  C_pseudo <- t(C.Prime_pseudo)
  B0_pseudo <- diag(diag(C_pseudo), ncol = 5, nrow = 5)
  A0_pseudo <- B0_pseudo %*% solve(C_pseudo)
  SVAR_AB_est.sim <- list("A0.svar" = A0_pseudo, "B0.svar" = B0_pseudo)
  SVAR_AB_IRF.sim <- VAR.svarirf.AB(Y.sim, VAR.P, Amat, Bmat, h = hrz, CONST, SVAR_AB_est = SVAR_AB_est.sim)
  
  # 5*5個圖的time series
  df_IRF_plot.sim <- matrix(NA, hrz+1, num_var^2) #%>% as.tibble()
  # df_IRF.sim <- array(1:(120*25*N), c(120,25,N))
  # df_IRF.sim[2,1,1] # slicing
  
  h <- 0 # h表示第幾期的IRF
  for(period in SVAR_AB_IRF.sim){
    k <- 0 # k表示把5*5的矩陣攤平到25個col的df時，要攤到第幾個columns上
    h <- h+1 # h表示第幾期的IRF
    for(j in 1:num_var){
      for(i in 1:num_var){
        k <- k+1 # k表示把5*5的矩陣攤平到25個col的df時，要攤到第幾個columns上
        df_IRF_plot.sim[h,k] <- period[i,j]
      }
    }
  }
  # 把這一次重抽得到的IRF append進`df_IRF.sim`中
  df_IRF.sim[,,counter] <- df_IRF_plot.sim
  ### SVAR.sim Ends ###
  if(counter>=N){
    break
  }
  counter <- counter+1
}


# Save
saveRDS(df_IRF.sim, file = "./data/intermediate_result/df_IRF.sim_hrz20.rds")
