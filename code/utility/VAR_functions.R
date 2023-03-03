# library(plyr)
# # library(tidyverse)
# library(dplyr)
# library(tidyr)
# library(readr)
# library(ggplot2)
# require(matrixcalc)
# # library(Rmisc)
# library(lattice)
# library(cowplot)

Shift.Right = function(Data, n.shift = 1){
  n.row  = nrow(as.matrix(Data))
  n.col  = ncol(as.matrix(Data))
  NA.M   = matrix(NA, n.row, n.shift)  # 補上 NA 矩陣
  result = cbind(NA.M, Data[, 1:(n.col-n.shift)])
  return(result)
}


VAR.Y = function(Data, VAR.p = 1){
  Y = t(as.matrix(Data))
  Y = Y[, -1:-VAR.p]                   # 調整資料
  return(Y)
}


VAR.X = function(Data, VAR.p = 1){
  Y = t(as.matrix(Data)); X = Y        # VAR.p = 1
  if(VAR.p > 1){                       # VAR.p > 1
    for(i in 1:(VAR.p-1)){
      X.down = Shift.Right(Y, i)
      X      = rbind(X, X.down)
    }
    X = X[, -1:-(VAR.p-1)]            # 調整資料
  }
  X = X[, -ncol(X)]                    # 調整最後一行
  return(X)
}

VAR.ddY = function(Data, VAR.p = 1){
  Y = t(VAR.Y(Data, VAR.p))
  return(Y)
}

VAR.ddX = function(Data, VAR.p = 1){
  X = t(VAR.X(Data, VAR.p))
  return(X)
}

### Reduced Form Estimation

# OLS
VAR.OLS = function(BY, BX, Const = TRUE){
  if(Const == TRUE){
    n.col = ncol(BX)
    ones  = as.matrix(matrix(1, 1, n.col))
    BX    = rbind(BX, ones)   # 最後一行是常數項
  }
  CAL.A    = BY %*% t(BX) %*% solve(BX %*% t(BX), tol = inv_tol) #更改誤差，以避免singular
  rownames(CAL.A) = NULL
  colnames(CAL.A) = NULL
  return(CAL.A)
}

# MLE
VAR.MLE = function(BY, BX, Const = TRUE){
  if(Const == TRUE){
    n.col = ncol(BX)
    ones  = as.matrix(matrix(1, 1, n.col))
    BX    = rbind(BX, ones)   # 最後一行是常數項
  }
  CAL.A    = BY %*% t(BX) %*% solve(BX %*% t(BX), tol = inv_tol)
  rownames(CAL.A) = NULL
  colnames(CAL.A) = NULL
  return(CAL.A)
}


# 殘差的變異數
VAR.Sigma.OLS = function(BY, BX, BA.OLS, Const=TRUE){
  T  = ncol(BX)                # T
  kp = nrow(BX)                # k x p, 不含常數項
  n.C= 0                       # 常數項個數
  if(Const == TRUE){
    n.C   = 1
    n.col = ncol(BX)
    ones  = as.matrix(matrix(1, 1, n.col))
    BX    = rbind(BX, ones)   # 最後一行是常數項
  }
  BU     = BY- BA.OLS %*% BX
  BSigma = (BU%*%t(BU))/(T-kp-n.C)  # 變異數: OLS
  rownames(BSigma) = NULL
  colnames(BSigma) = NULL
  return(BSigma)
}

VAR.ddSigma.OLS = function(ddY, ddX, Const = TRUE){
   T   = nrow(ddX)              # T
   kp  = ncol(ddX)              # k x p, 不含常數項
   n.C = 0                      # 常數項個數
   if(Const == TRUE){n.C   = 1}
   ddBU   = VAR.EbyE(ddY, ddX, Const)$ddU
   BU     = t(ddBU)
   BSigma = (BU%*%t(BU))/(T-kp-n.C)  # 變異數: OLS
   rownames(BSigma) = NULL
   colnames(BSigma) = NULL
   return(BSigma)
 }

VAR.EbyE = function(ddY, ddX, Const = TRUE){
  if(Const == TRUE){
    T = nrow(ddY)
    ones  = as.matrix(matrix(1, T, 1))
    ddX   = cbind(ddX, ones)  # 最後一行是常數項
  }
  k     = ncol(ddY)
  m.ols = lm(ddY[,1]~ddX-1)    # 1st equation
  ddBA  = m.ols$coefficient    # t(A1 ... Ap)
  ddBU  = m.ols$residuals      # 殘差項
  if(k > 1){
    for(i.ind in 2:k){        # 2nd... equation
      m.ols = lm(ddY[,i.ind]~ddX-1)
      ddBA  = cbind(ddBA, m.ols$coefficient)
      ddBU  = cbind(ddBU, m.ols$residuals)
    }
  }
  rownames(ddBA) = NULL
  colnames(ddBA) = NULL
  result  = list("ddA" = ddBA, "ddU" = ddBU)
  return(result)
}

VAR.Sigma.MLE = function(BY, BX, BA.MLE, Const=TRUE){
  T  = ncol(BX)                # T
  n.C= 0                       # 常數項個數
  if(Const == TRUE){
    n.C   = 1
    n.col = ncol(BX)
    ones  = as.matrix(matrix(1, 1, n.col))
    BX    = rbind(BX, ones)   # 最後一行是常數項
  }
  BU     = BY- BA.MLE %*% BX
  BSigma = (BU%*%t(BU))/T      # 變異數: MLE
  rownames(BSigma) = NULL
  colnames(BSigma) = NULL
  return(BSigma)
}

# 選擇落後期數
VAR.IC = function(BSigma.MLE, A.Mat, T){
  gamma0 = length(A.Mat)
  eq.0   = which(as.vector(A.Mat) == 0)
  gamma0 = gamma0 - length(eq.0)
  AIC = log(det(BSigma.MLE))+ 2/T * gamma0
  SIC = log(det(BSigma.MLE))+ log(T)/T * gamma0
  HQ  = log(det(BSigma.MLE))+(2*log(log(T)))/T*gamma0
  result = list("AIC" = AIC, "SIC" = SIC, "HQ" = HQ)
  return(result)
}

VAR.Select= function(Data, Max.lag =1, Const = TRUE){
  IC.EST= matrix(NA, 3, Max.lag)
  rownames(IC.EST) = c("AIC", "SIC", "HQ")
  for(j in 1:Max.lag){
    VAR.p = j
    BY    = VAR.Y(Data, VAR.p)
    BX    = VAR.X(Data, VAR.p)
    k     = nrow(BY)
    T     = ncol(BY)
    if(Const == TRUE){# 只能估算參數不受限的情況
      A.Mat = matrix(1, k^2 * j + k , 1)
    }else{
      A.Mat = matrix(1, k^2 * j, 1)
    }
    Coef.MLE =VAR.MLE(BY, BX, Const)     # 參數不受限
    Sigma.MLE=VAR.Sigma.MLE(BY, BX, Coef.MLE, Const)
    IC.est  = VAR.IC(Sigma.MLE, A.Mat, T)
    IC.EST[1,j] = IC.est$AIC
    IC.EST[2,j] = IC.est$SIC
    IC.EST[3,j] = IC.est$HQ
  }
  return(IC.EST)
}


### SVAR Estimation ###
VAR.svarest.A = function(Data, VAR.p = 2, A0, Const = TRUE){
  Y    = VAR.Y(Data, VAR.p)
  X    = VAR.X(Data, VAR.p)
  k    = ncol(Data)                            # k
  T    = ncol(Y)                               # T
  Coef = VAR.OLS(Y, X, Const)                  # OLS
  BSigma.u = VAR.Sigma.OLS(Y, X, Coef, Const)  # BSigma.u
  params.NA = sum(is.na(A0))                   # NA 才估計
  start    = as.vector(rep(0.1, params.NA))    # 起始值
  param.Aidx = which(is.na(A0), arr.ind = TRUE)# A0 指標
  B0       = diag(k)                           # B = I_k
  loglike  = function(coef.est){               # log-likelihood
    A0[param.Aidx] = coef.est
    ln.likefn = -1 * (T*k/2) * log(2*pi) +
      T/2*log(det(A0)^2) -
      T/2*log(det(B0)^2) -
      T/2*sum(diag(t(A0)%*%solve(t(B0))%*%
                     solve(B0) %*% A0 %*% BSigma.u))
    return(-ln.likefn)                        # 極小化
  }
  optimun = optim(start, loglike, method = "BFGS", hessian = TRUE)
  A0[param.Aidx] = optimun$par                 # 還原回去
  inv.A0  = solve(A0)                          # inv(A0)
  if(any(diag(inv.A0 %*% B0) < 0)){            # 保證為正
    ind = which(diag(inv.A0 %*% B0) < 0)
    A0[ind, ind] = -1 * A0[ind, ind]
    #A0[, ind] = -1 * A0[, ind]               # R 錯誤設定:vars
  }
  A0std = matrix(0, nrow = k, ncol = k)        # A0 的標準差
  if(!(is.null(optimun$hessian))) {
    Sigma = sqrt(diag(solve(optimun$hessian)))# Information matrix inverse
    A0std[param.Aidx] = Sigma
  }
  Coef.svar = numeric(0)
  for(i in 1:VAR.p){                           # 計算 SVAR 中的 Ai
    Ai =  A0 %*% Coef[,1:k]
    Coef = Coef[, -(1:k)]                     # 刪掉前 k 行
    Coef.svar = cbind(Coef.svar, Ai)
  }
  if(Const == TRUE){                           # 計算 Constant Term
    Ai = A0 %*% Coef
    Coef.svar = cbind(Coef.svar, Ai)
  }
  result = list("A0.svar" = A0, "Ai.svar" = Coef.svar, "A0.Std" = A0std)
  return(result)
}




VAR.svarest.B = function(Data, VAR.p = 2, B0, Const = TRUE){
  Y    = VAR.Y(Data, VAR.p)
  X    = VAR.X(Data, VAR.p)
  k    = ncol(Data)                            # k
  T    = ncol(Y)                               # T
  Coef = VAR.OLS(Y, X, Const)                  # OLS
  BSigma.u = VAR.Sigma.OLS(Y, X, Coef, Const)  # BSigma.u
  params.NA = sum(is.na(B0))                   # NA 才估計
  start    = as.vector(rep(0.1, params.NA))    # 起始值
  param.Bidx = which(is.na(B0), arr.ind = TRUE)# B0 指標
  A0       = diag(k)                           # A = I_k
  loglike  = function(coef.est){               # log-likelihood
    B0[param.Bidx] = coef.est
    ln.likefn = -1 * (T*k/2) * log(2*pi) +
      T/2*log(det(A0)^2) -
      T/2*log(det(B0)^2) -
      T/2*sum(diag(t(A0)%*%solve(t(B0))%*%
                     solve(B0) %*% A0 %*% BSigma.u))
    return(-ln.likefn)                        # 極小化
  }
  optimun = optim(start, loglike, method = "BFGS", hessian = TRUE)
  B0[param.Bidx] = optimun$par                 # 還原回去
  inv.A0  = solve(A0)                          # inv(A0)
  if(any(diag(inv.A0 %*% B0) < 0)){            # 保證為正
    ind = which(diag(inv.A0 %*% B0) < 0)
    B0[ind, ind] = -1 * B0[ind, ind]
    #B0[, ind] = -1 * B0[, ind]               # R 錯誤設定:vars
  }
  B0std = matrix(0, nrow = k, ncol = k)        # B0 的標準差
  if(!(is.null(optimun$hessian))) {
    Sigma = sqrt(diag(solve(optimun$hessian)))# Information matrix inverse
    B0std[param.Bidx] = Sigma
  }
  Coef.svar = numeric(0)
  for(i in 1:VAR.p){                           # 計算 SVAR 中的 Ai
    Ai =  A0 %*% Coef[,1:k]
    Coef = Coef[, -(1:k)]                     # 刪掉前 k 行
    Coef.svar = cbind(Coef.svar, Ai)
  }
  if(Const == TRUE){                           # 計算 Constant Term
    Ai = A0 %*% Coef
    Coef.svar = cbind(Coef.svar, Ai)
  }
  result = list("B0.svar" = B0, "Ai.svar" = Coef.svar, "B0.Std" = B0std)
  return(result)
}

# 相當於 EViews中的S Matrix認定法

VAR.svarest.AB = function(Data, VAR.p = 2, A0, B0, Const = TRUE, start=NA){
  Y    = VAR.Y(Data, VAR.p)
  X    = VAR.X(Data, VAR.p)
  k    = ncol(Data)                            # k
  T    = ncol(Y)                               # T
  Coef = VAR.OLS(Y, X, Const)                  # OLS
  BSigma.u = VAR.Sigma.OLS(Y, X, Coef, Const)  # BSigma.u
  params.NAA = sum(is.na(A0))                  # NA 才估計
  params.NAB = sum(is.na(B0))                  # NA 才估計
  params.NA  = params.NAA + params.NAB
  
  # 如果沒有給定起使值的話
  if(is.na(start)){
    start = as.vector(rep(0.1, params.NA))    # 起始值
  }else{
    if(start=="normal"){
      start = as.vector(rep(rnorm(1), params.NA))    # 起始值
    }else{
      if(start=="uniform"){
        start = as.vector(rep(runif(1,-10,10), params.NA))    # 起始值
      }
    }
  }
  
  param.Aidx = which(is.na(A0), arr.ind = TRUE)# A0 指標
  param.Bidx = which(is.na(B0), arr.ind = TRUE)# B0 指標
  loglike  = function(coef.est){               # log-likelihood
    A0[param.Aidx] = coef.est[ c(1:nrow(param.Aidx))]
    coef.est1 = coef.est[-c(1:nrow(param.Aidx))]
    B0[param.Bidx] = coef.est[-c(1:nrow(param.Aidx))]
    ln.likefn = -1 * (T*k/2) * log(2*pi) +
      T/2*log(det(A0)^2) -
      T/2*log(det(B0)^2) -
      T/2*sum(diag(t(A0)%*%solve(t(B0))%*%
                     solve(B0) %*% A0 %*% BSigma.u))
    return(-ln.likefn)                        # 極小化
  }
  optimun = optim(start, loglike, method = "BFGS", hessian = TRUE)
  A0[param.Aidx] = head(optimun$par, nrow(param.Aidx)) # 還原回去
  B0[param.Bidx] = tail(optimun$par, nrow(param.Bidx))
  inv.A0  = solve(A0)                          # inv(A0)
  if(any(diag(A0) < 0)){                       # 保證為正; 跟 svarest.A, svarest.B 一樣
    ind = which(diag(A0)<0)
    A0[ind, ind] = -1 * A0[ind, ind]
  }
  if(any(diag(B0) < 0)){
    ind = which(diag(B0)<0)
    B0[ind, ind] = -1 * B0[ind, ind]
  }
  A0std = matrix(0, nrow = k, ncol = k)        # A0 的標準差
  B0std = matrix(0, nrow = k, ncol = k)        # B0 的標準差
  if(!(is.null(optimun$hessian))) {
    Sigma = sqrt(diag(solve(optimun$hessian)))# Information matrix inverse
    A0std[param.Aidx] = head(Sigma, nrow(param.Aidx))
    B0std[param.Bidx] = tail(Sigma, nrow(param.Bidx))
  }
  Coef.svar = numeric(0)
  for(i in 1:VAR.p){                           # 計算 SVAR 中的 Ai
    Ai =  A0 %*% Coef[,1:k]
    Coef = Coef[, -(1:k)]                     # 刪掉前 k 行
    Coef.svar = cbind(Coef.svar, Ai)
  }
  if(Const == TRUE){                           # 計算 Constant Term
    Ai = A0 %*% Coef
    Coef.svar = cbind(Coef.svar, Ai)
  }
  result = list("A0.svar" = A0,  "Ai.svar" = Coef.svar, "A0.Std" = A0std,
                "B0.svar" = B0,  "B0.Std" = B0std)
  return(result)
}      # 跟 EViews 有可能正負號不一樣, 但是這正常. 可能 A0 是負, B0 是正; 但 EViews 是 A0 是正, B0 是負
# B0 的正負號都跟 EViews 不一樣


# 估計完後的係數矩陣，轉換表達方式
VAR.F2Psi = function(Coef, h=10, Const = TRUE){
  k = nrow(Coef)
  if(Const == TRUE){
    Coef = Coef[,-ncol(Coef)] # 去掉常數項
  }
  VAR.p = ncol(Coef)/k
  Ik = diag(rep(1, k))
  BJ = cbind(Ik, matrix(0, k, (k*VAR.p-k)))
  Ai.list = NULL               # A: list
  for(i in 1:VAR.p){
    Ai.list[[i]] = Coef[,1:k]
    Coef = Coef[, -(1:k)]     # 刪掉前 k 行
  }
  CAL.F = VAR.ARp2AR1(Ai.list) # 處理 F
  Psi = NULL
  Psi[[1]] = Ik                # Psi0
  BF = diag(rep(1, ncol(CAL.F)))
  for(i in 2:(h+1)){
    BF = BF %*% CAL.F
    Psi[[i]] = BJ %*% BF %*% t(BJ)
  }
  Psi.names  = paste("Psi", 0:h, sep="")
  names(Psi) = Psi.names
  return(Psi)
}


VAR.Theta= function(Coef, h=10, BSigma.u, Const=TRUE){
   P = t(chol(BSigma.u))        # 下三角矩陣
   k = nrow(Coef); D = matrix(0, k, k)
   diag(D) = diag(P)
   inv.D   = solve(D)
   Psi     = VAR.F2Psi(Coef, h, Const)
   Theta.names = paste("Theta", 0:h, sep="");
   names(Psi)  = Theta.names
   Theta.unit  = Psi; Theta.std = Psi
   for(i in 1:(h+1)){
      Theta.unit[[i]] = Psi[[i]] %*% P %*% inv.D
      Theta.std[[i]]  = Psi[[i]] %*% P
   }
   result = list("unit"= Theta.unit, "std"= Theta.std)
   return(result)               # i = 0, 1, 2,
 }

VAR.ARp2AR1 = function(Ai.list){
  VAR.p = length(Ai.list)
  CAL.F = NULL
  for(i in 1:VAR.p){           # 計算 F 的上半截矩陣
    CAL.F = cbind(CAL.F, as.matrix(Ai.list[[i]]))
  }
  n.row  =nrow(CAL.F)          # 計算 F 的下半截矩陣
  F.down =(diag(rep(1, VAR.p))%x%diag(rep(1, n.row)))
  if( (nrow(F.down)-n.row) == 0){
    F.down = NULL
  }else{
    F.down = F.down[1:(nrow(F.down)-n.row),]
  }
  CAL.F = rbind(CAL.F, F.down) # 合併結果
  rownames(CAL.F) = NULL
  return(CAL.F)
}

# 設定完認定條件且估計完A,B Matrix後，估Impulse Response Function

VAR.svarirf.AB=function(Data, VAR.p = 2, AMat, BMat, h=10, Const = TRUE,
                        SVAR_AB_est = NA){
  Y    = VAR.Y(Data, VAR.p)
  X    = VAR.X(Data, VAR.p)
  Coef = VAR.OLS(Y, X, Const)
  Psi  = VAR.F2Psi(Coef, h, Const)
  
  # if(is.na(SVAR_AB_est)){
  #   A0   = VAR.svarest.AB(Data, VAR.p, AMat, BMat,Const, start="normal")$A0.svar # 不同之處
  #   A0.inv= solve(A0)
  #   B0   = VAR.svarest.AB(Data, VAR.p, AMat, BMat,Const, start="normal")$B0.svar # 不同之處
  # }else{
    # 用已經估完的（沒有問題的）A,B Matrix來找IRF
    A0 <- SVAR_AB_est$A0.svar
    A0.inv <- solve(A0)
    B0 <- SVAR_AB_est$B0.svar
  # }
  
  ddTheta.svar = list()
  for(i in 1:(h+1)){
    ddTheta.svar[[i]] = Psi[[i]] %*% A0.inv %*% B0            # 不同之處
  }
  Theta.names = paste("Theta", 0:h, sep="");
  names(ddTheta.svar)  = Theta.names
  return(ddTheta.svar)
}      # 跟 EViews 有可能正負號不一樣, 但是這正常. 可能 A0 是負, B0 是正; 但 EViews 是 A0 是正, B0 是負
# B0 的正負號都跟 EViews 不一樣



### Variance Decomposition
VAR.svardecomp.AB = function(m = 1, Data, VAR.p=2, AMat, BMat, h=4,
                             Const=TRUE, ddTheta = NA){
  Y    = VAR.Y(Data, VAR.p)
  X    = VAR.X(Data, VAR.p)
  k    = ncol(Data)
  T    = ncol(Y)
  
  #ddTheta=VAR.svarirf.AB(Data, VAR.p, AMat, BMat, h, Const)   # 不同之處
  
  Ik = diag(rep(1, k))                 # 以下部份是參考 VAR.decomp
  ell.m = Ik[, m]
  den = matrix(0, 1, h)                # 分母
  num = matrix(0, k, h)                # 分子
  var.decomp = matrix(0, k, h)         # 變異數分解
  i = 1                                # i = 0
  den[1,i]=t(ell.m)%*%ddTheta[[i]]%*%
    t(ddTheta[[i]])%*%ell.m
  num[,i] =(t(ell.m) %*% ddTheta[[i]])^2# 不用 ell.j
  var.decomp[,i] = num[,i]/den[1,i]
  for(i in 2:h){                       # i = 1...h-1
    den[1,i]=den[1,i-1]+t(ell.m)%*%ddTheta[[i]]%*%
      t(ddTheta[[i]])%*%ell.m
    num[,i] =num[,i-1]+(t(ell.m)%*%ddTheta[[i]])^2
    var.decomp[,i] = num[,i]/den[1,i]
  }
  var.decomp = t(var.decomp)
  decomp.names  = paste("h=", 1:h, sep="")
  rownames(var.decomp) = decomp.names
  return(var.decomp)
}

### Historical Decompostition
VAR.svarhist.AB = function(Data, VAR.p, AMat, BMat, Const){
  ddY = VAR.ddY(Data, VAR.p)
  ddX = VAR.ddX(Data, VAR.p)
  k   = ncol(ddY)
  T   = nrow(ddY)
  U   = VAR.EbyE(ddY, ddX, Const)$ddU                  # VAR 殘差
  
  
  # A   = VAR.svarest.AB(Data, VAR.p, AMat, BMat, Const)$A0.svar     # AB-Model 估計 A 不同之處
  # B   = VAR.svarest.AB(Data, VAR.p, AMat, BMat, Const)$B0.svar     # AB-Model 估計 B
  A <- SVAR_AB_est$A0.svar
  B <- SVAR_AB_est$B0.svar
  
  
  
  B.inv = solve(B)
  AB  = B.inv %*% A                                    # (A^{-1} B)^{-1}  不同之處
  ddEpsilon=numeric(0)
  for(i in 1:T){
    ddE = AB %*% (U[i,])                              # 還原成 epsilon   不同之處
    ddEpsilon = rbind(ddEpsilon, t(ddE))
  }
  Epsilon.list = list()
  for(i in 1:k){
    Epsilon.list[[i]]=matrix(0, T, k)                 # 其它干擾項=0, 所以是 X 0 0 矩陣
    Epsilon.list[[i]][,i] = ddEpsilon[,i]
  }
  
  ###
  ddTheta = SVAR_AB_IRF # 用已經拿到的IRF來做歷史分解 #當IRF的h大於過去資料的長度時才可以直接用
  #ddTheta=VAR.svarirf.AB(Data, VAR.p, AMat, BMat, h=T, Const)          # ddTheta 不同之處
  ###
  
  hist.list = list()                                   # 所有 historical decomp
  for(j in 1:k){
    hist.sum  = matrix(NA, VAR.p, k)
    for(t in 1:T){
      hist.j = matrix(0, 1, k)
      for(i in 1:t){
        hist.ind = ddTheta[[i]] %*% as.matrix(Epsilon.list[[j]][(t-i+1),])  # Theta_i Epsilon_{t-i}
        hist.j = hist.j + t(hist.ind)                # 累加 0 至 t-1
      }
      hist.sum = rbind(hist.sum, hist.j)
    }
    hist.list[[j]] = hist.sum
  }
  Hist.Decomp = matrix(NA, (T+VAR.p), (k*k) )          # 重新整理
  for(i in 1:k){
    ind = seq(i, k*k, k)
    Hist.Decomp[,ind] = hist.list[[i]]
  }
  return(Hist.Decomp)
}


VAR.baseproject = function(Data, VAR.p, Const){
  BaseProject = Data[1:VAR.p,]
  Y = VAR.Y(Data, VAR.p)
  X = VAR.X(Data, VAR.p)
  k = ncol(Data)
  T = ncol(Y)
  Coef = VAR.OLS(Y, X, Const)  # OLS 結果; 不論是 A-Model, B-Model 或是 AB-Model 均是一樣的程式
  if(Const == TRUE){
    const= Coef[, ncol(Coef)] # 常數項
    Coef = Coef[,-ncol(Coef)] # 去掉常數項
  }else{const = rep(0, k)}     # Const=FALSE, rep(0, k)
  Ai.list = NULL               # A: list
  for(i in 1:VAR.p){
    Ai.list[[i]] = Coef[,1:k]
    Coef = Coef[, -(1:k)]     # 刪掉前 k 行
  }
  CAL.F = VAR.ARp2AR1(Ai.list) # 處理 F
  Ik = diag(rep(1, k))
  BJ = cbind(Ik, matrix(0, k, (k*VAR.p-k)))
  n = VAR.p
  for(i in 1:T){
    base = BaseProject[n:(n-VAR.p+1), ]
    z.1  = vec(t(base))
    Bc = BJ %*% (CAL.F %*% z.1 + const)
    BaseProject = rbind(BaseProject, t(Bc))
    n = n + 1
  }
  colnames(BaseProject) = NULL
  return(BaseProject)
}


vec = function (x){     # 參考 matrixcalc
  if (!is.matrix(x)) {
    stop("argument x is not a matrix")
  }
  if (!is.numeric(x)) {
    stop("argument x is not a numeric matrix")
  }
  return(t(t(as.vector(x))))
}
