

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


VAR.Psi = function(Ai.list, n.BPsi = 1){
   VAR.p = length(Ai.list)
   k     = nrow(as.matrix(Ai.list[[1]]))
   n.BPsi= n.BPsi + 1
   j = VAR.p
   while(n.BPsi > j){           # 建構 Aj = 0, j > p
      Ai.list[[j+1]] = matrix(0, k, k)
      j = j + 1
   }
   BPsi = NULL
   BPsi[[1]] = diag(rep(1, k))
   if(n.BPsi > 1){
      for(i in 2:n.BPsi){       # 遞迴關係: i 從 2 開始
         sum.ji = matrix(0, k, k)
         for(j in 1:(i-1)){
            sum.ji = sum.ji+BPsi[[i-j]]%*%Ai.list[[j]]
         }
         BPsi[[i]] = sum.ji
      }
   }
   BPsi.names  = paste("Psi", 0:(n.BPsi-1), sep="")
   names(BPsi) = BPsi.names
   return(BPsi)
 }


VAR.ddGroup = function(Data, Set, VAR.p = 1){
   ddY  = VAR.ddY(Data, VAR.p)
   ddX  = VAR.ddX(Data, VAR.p)
   n.col= ncol(ddY)
   i = 1
   SetX = Set       # 計算所需資訊
   while(i < VAR.p){
      add = Set + (i * n.col)
      SetX = c(SetX, add)
      i = i + 1
   }
   ddY1 = as.matrix(ddY[,  Set ])
   ddY2 = as.matrix(ddY[, -Set ])
   ddX1 = as.matrix(ddX[,  SetX])
   ddX2 = as.matrix(ddX[, -SetX])
   result = list("ddY1" = ddY1, "ddY2" = ddY2,
                 "ddX1" = ddX1, "ddX2" = ddX2)
   return(result)
 }


VAR.OLS = function(BY, BX, Const = TRUE){
   if(Const == TRUE){
      n.col = ncol(BX)
      ones  = as.matrix(matrix(1, 1, n.col))
      BX    = rbind(BX, ones)   # 最後一行是常數項
   }
   CAL.A    = BY %*% t(BX) %*% solve(BX %*% t(BX))
   rownames(CAL.A) = NULL
   colnames(CAL.A) = NULL
   return(CAL.A)
 }


VAR.MLE = function(BY, BX, Const = TRUE){
   if(Const == TRUE){
      n.col = ncol(BX)
      ones  = as.matrix(matrix(1, 1, n.col))
      BX    = rbind(BX, ones)   # 最後一行是常數項
   }
   CAL.A    = BY %*% t(BX) %*% solve(BX %*% t(BX))
   rownames(CAL.A) = NULL
   colnames(CAL.A) = NULL
   return(CAL.A)
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


VAR.ddSigma.MLE = function(ddY, ddX, Const = TRUE){
   T   = nrow(ddX)              # T
   n.C = 0                      # 常數項個數
   if(Const == TRUE){n.C   = 1}
   ddBU   = VAR.EbyE(ddY, ddX, Const)$ddU
   BU     = t(ddBU)
   BSigma = (BU%*%t(BU))/T      # 變異數: MLE
   return(BSigma)
 }


VAR.loglike = function(BY, BX, BA.MLE, BSigma.MLE, Const = TRUE){
   T  = ncol(BX)                # T
   k  = nrow(BY)                # k
   if(Const == TRUE){
      n.col = ncol(BX)
      ones  = as.matrix(matrix(1, 1, n.col))
      BX    = rbind(BX, ones)   # 最後一行是常數項
   }
   BU = BY - BA.MLE %*% BX      # 殘差項
   tr = sum(diag(t(BU)%*%solve(BSigma.MLE)%*%BU))
   lnlike = -(T*k/2)*log(2*pi)-
            T/2*log(det(BSigma.MLE))-tr/2
   return(lnlike)
 }


VAR.ROLS = function(BY, BX, BR, Br, Const = TRUE){
   k = nrow(BY)
   Ik= diag(rep(1, k))          # Identity matrix
   vecY = as.matrix(as.vector((BY)))
   if(Const == TRUE){
      n.col = ncol(BX)
      ones  = as.matrix(matrix(1, 1, n.col))
      BX    = rbind(BX, ones)   # 最後一行是常數項
   }
   vecz = vecY-(t(BX) %x% Ik) %*% Br
   B1   = solve(t(BR) %*%( (BX%*%t(BX))%x%Ik ) %*% BR)
   B2   = t(BR) %*% (BX %x% Ik) %*% vecz
   Beta1= B1 %*% B2
   result = BR %*% Beta1 + Br
   return(result)
}


VAR.A2R = function(A.Mat){
   vecA.Mat = as.matrix(as.vector(A.Mat))
   n.row    = nrow(vecA.Mat)
   n.restr  = 0
   n.restr  = length(which(A.Mat == 0))
   BR = matrix(0, n.row, (n.row-n.restr))
   i = 1;  j = 1;  k = 1
   while(k <= n.row){
      if(vecA.Mat[i,1] == 1){
         BR[i,j] = 1
         j = j + 1
      }
      i = i + 1;  k = k + 1
   }
   return(BR)
 }


VAR.RFGLS=function(BY, BX, BR, Br, BSigma,Const=TRUE){
   inv.S  = solve(BSigma)
   vecY   = as.matrix(as.vector((BY)))
   if(Const == TRUE){
      n.col = ncol(BX)
      ones  = as.matrix(matrix(1, 1, n.col))
      BX    = rbind(BX, ones)   # 最後一行是常數項
   }
   vecz = vecY-(t(BX) %x% inv.S) %*% Br
   B1   = solve(t(BR) %*%((BX%*%t(BX))%x%inv.S)%*%BR)
   B2   = t(BR) %*% (BX %x% inv.S) %*% vecz
   Beta1= B1 %*% B2
   result = BR %*% Beta1 + Br
   return(result)
 }

VAR.RMLE=function(BY, BX, BR, Br, BSigma, Const=TRUE){
   inv.S  = solve(BSigma)
   vecY   = as.matrix(as.vector((BY)))
   if(Const == TRUE){
      n.col = ncol(BX)
      ones  = as.matrix(matrix(1, 1, n.col))
      BX    = rbind(BX, ones)   # 最後一行是常數項
   }
   vecz = vecY-(t(BX) %x% inv.S) %*% Br
   B1   = solve(t(BR) %*%((BX%*%t(BX))%x%inv.S)%*%BR)
   B2   = t(BR) %*% (BX %x% inv.S) %*% vecz
   Beta1= B1 %*% B2
   result = BR %*% Beta1 + Br
   return(result)
 }


VAR.Transform = function(A11, A12, Sigma11,
              Dy, D1, D2, M, d1 = 0, d2 = 0){
   inv.Sigma11 = solve(Sigma11)     # 逆矩陣
   Sigma21 = Dy %*% Sigma11
   Sigma12 = t(Sigma21)
   A21     = D1 + Sigma21 %*% inv.Sigma11 %*% A11
   A22     = D2 + Sigma21 %*% inv.Sigma11 %*% A12
   Sigma22 = M  + Sigma21 %*% inv.Sigma11 %*% Sigma12
   c2      = d2 + Sigma21 %*% inv.Sigma11 %*% d1
   result  = list("A11" = A11, "A12" = A12,
                 "A21" = A21, "A22" = A22,
                 "S11" = Sigma11, "S12" = Sigma12,
                 "S21" = Sigma21, "S22" = Sigma22,
                 "c1"  = d1, "c2" = c2)
   return(result)
 }


VAR.RTransform = function(A11, Sigma11,
              Dy, D1, D3, M, d1 = 0, d2 = 0){
   inv.Sigma11 = solve(Sigma11)     # 逆矩陣
   Sigma21 = Dy %*% Sigma11
   Sigma12 = t(Sigma21)
   A21     = D1 + Sigma21 %*% inv.Sigma11 %*% A11
   A22     = D3
   Sigma22 = M  + Sigma21 %*% inv.Sigma11 %*% Sigma12
   c2      = d2 + Sigma21 %*% inv.Sigma11 %*% d1
   result  = list("A11" = A11, "A12" = 0,
                 "A21" = A21, "A22" = A22,
                 "S11" = Sigma11, "S12" = Sigma12,
                 "S21" = Sigma21, "S22" = Sigma22,
                 "c1"  = d1, "c2" = c2)
   return(result)
 }


VAR.exog.MLE = function(Data, Set, VAR.p = 1,
              Const = TRUE){
   ddGroup = VAR.ddGroup(Data, Set, VAR.p)
   ddY1    = ddGroup$ddY1
   ddY2    = ddGroup$ddY2
   ddX1    = ddGroup$ddX1
   ddX2    = ddGroup$ddX2
   n.y     = ncol(ddY1)
   n.1     = ncol(ddX1)
   n.2     = ncol(ddX2)
   ddX     = cbind(ddX1, ddX2)          # 估算群組 1
   Group1  = VAR.EbyE(ddY1, ddX, Const)
   A       = t(Group1$ddA)
   Sigma11 = VAR.ddSigma.OLS(ddY1, ddX, Const)
   A11     = A[, 1:n.1]; zero1=matrix(0, nrow(A), 1)
   A12     = A[, (n.1+1):(n.1+n.2)]
   if(Const== TRUE){d1 = A[,ncol(A)]}else{d1=zero1}
   ddX     = cbind(ddY1, ddX1, ddX2)    # 估算群組 2
   Group2  = VAR.EbyE(ddY2, ddX, Const)
   A       = t(Group2$ddA)
   M       = VAR.ddSigma.OLS(ddY2, ddX, Const)
   Dy      = A[, 1:n.y]; zero2=matrix(0, nrow(A), 1)
   D1      = A[, (n.y+1):(n.y+n.1)]
   D2      = A[, (n.y+n.1+1):(n.y+n.1+n.2)]
   if(Const== TRUE){d2 = A[,ncol(A)]}else{d2=zero2}
   result  = VAR.Transform(A11, A12, Sigma11,
              Dy, D1, D2, M, d1, d2)
   return(result)
 }


VAR.exog.RMLE = function(Data, Set, VAR.p = 1,
              Const = TRUE){
   ddGroup = VAR.ddGroup(Data, Set, VAR.p)
   ddY1    = ddGroup$ddY1; n.y = ncol(ddY1)
   ddY2    = ddGroup$ddY2
   ddX1    = ddGroup$ddX1; n.1 = ncol(ddX1)
   ddX2    = ddGroup$ddX2; n.2 = ncol(ddX2)
   ddX     = cbind(ddX1)                # 估算群組 1
   Group1  = VAR.EbyE(ddY1, ddX, Const)
   A       = t(Group1$ddA)
   Sigma11 = VAR.ddSigma.OLS(ddY1, ddX, Const)
   A11     = A[, 1:n.1]; zero1=matrix(0, nrow(A), 1)
   if(Const== TRUE){d1 = A[,ncol(A)]}else{d1=zero1}
   ddX     = cbind(ddY1, ddX1, ddX2)    # 估算群組 2
   Group2  = VAR.EbyE(ddY2, ddX, Const)
   A       = t(Group2$ddA)
   M       = VAR.ddSigma.OLS(ddY2, ddX, Const)
   Dy      = A[, 1:n.y]; zero2=matrix(0, nrow(A), 1)
   D1      = A[, (n.y+1):(n.y+n.1)]
   D3      = A[, (n.y+n.1+1):(n.y+n.1+n.2)]
   if(Const== TRUE){d2 = A[,ncol(A)]}else{d2=zero2}
   result  = VAR.RTransform(A11, Sigma11,
              Dy, D1, D3, M, d1, d2)
 }


VAR.OLS.Std = function(BX, BSigma, Const = TRUE){
   if(Const == TRUE){
      n.col = ncol(BX)
      ones  = as.matrix(matrix(1, 1, n.col))
      BX    = rbind(BX, ones)   # 最後一行是常數項
   }
   XX       = BX %*% t(BX)
   inv.XX   = solve(XX)
   beta.std = sqrt(diag(inv.XX %x% BSigma))
   return(beta.std)
 }

VAR.MLE.Std = function(BX, BSigma, Const = TRUE){
   if(Const == TRUE){
      n.col = ncol(BX)
      ones  = as.matrix(matrix(1, 1, n.col))
      BX    = rbind(BX, ones)   # 最後一行是常數項
   }
   XX       = BX %*% t(BX)
   inv.XX   = solve(XX)
   beta.std = sqrt(diag(inv.XX %x% BSigma))
   return(beta.std)
 }


VAR.RFGLS.Std = function(BX, BSigma, BR, Const = TRUE){
   if(Const == TRUE){
      n.col = ncol(BX)
      ones  = as.matrix(matrix(1, 1, n.col))
      BX    = rbind(BX, ones)   # 最後一行是常數項
   }
   XX       = BX %*% t(BX)
   inv.Sigma= solve(BSigma)
   var.core = t(BR) %*% (XX %x% inv.Sigma) %*% BR
   beta.std = BR %*% solve(var.core) %*% t(BR)
   beta.std = sqrt(diag(beta.std))
   return(beta.std)
 }


VAR.tratio = function(Coef, Coef.Std){
   t.ratio = Coef / Coef.Std
   return(t.ratio)
}

VAR.Wald = function(BY, BX, BC, Bc, BSigma,
           Const = TRUE){
   if(Const == TRUE){
      n.col = ncol(BX)
      ones  = as.matrix(matrix(1, 1, n.col))
      BX    = rbind(BX, ones)   # 最後一行是常數項
   }
   Beta = VAR.OLS(BY, BX, FALSE)# 因為上式會用到 Const
   beta = as.vector(Beta)       # 所以一定是用 FALSE
   XX   = BX %*% t(BX)
   inv.XX = solve(XX)
   Wald.1 = BC %*% beta - Bc
   Wald.2 = BC %*% (inv.XX %x% BSigma) %*% t(BC)
   Wald   = t(Wald.1) %*% solve(Wald.2) %*% Wald.1
   F      = Wald / length(Bc)
   result = list("Wald" = Wald, "F" = F)
   return(result)
 }


VAR.A2C = function(A.Mat){
   vecA.Mat = as.matrix(as.vector(A.Mat))
   n.row    = nrow(vecA.Mat)
   eq.0     = which(vecA.Mat == 0)
   BC       = matrix(0, length(eq.0), n.row)
   for(i in 1:length(eq.0)){
      BC[i, eq.0[i]] = 1}
   return(BC)
 }

VAR.LR = function(BSigma.alt, Bsigma.null, T){
   LR.test = T*(log(det(Bsigma.null))
             -log(det(BSigma.alt)))
   return(LR.test)
 }


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


VAR.LRtestp = function(Data, ddp, Const = TRUE){
   VAR.p = ddp
   BY    = VAR.Y(Data, VAR.p)
   BX    = VAR.X(Data, VAR.p)
   T     = ncol(BY)
   if(Const == TRUE){
      n.col = ncol(BX)
      ones  = as.matrix(matrix(1, 1, n.col))
      BX    = rbind(BX, ones)   # 因為上式已用到 Const
   }                            # 所以以下用 FALSE
   Coef.MLE =VAR.MLE(BY, BX, FALSE)     # 參數不受限
   Sigma.MLE=VAR.Sigma.MLE(BY, BX, Coef.MLE, FALSE)
   k        = nrow(Coef.MLE)    # A.Mat
   A.Mat    = matrix(1, k, ncol(Coef.MLE))
   zero.Mat = matrix(0, k, k)
   one.Mat  = matrix(1, k, 1)
   A.Mat    = cbind(A.Mat, zero.Mat)
   if(Const == TRUE){
      A.Mat = A.Mat[,-(1:(k+1))]
      A.Mat = cbind(A.Mat, one.Mat)
   }else{
      A.Mat = A.Mat[, -(1:k)]
   }                            # 參數受限制
   vecA.Mat =as.matrix(as.vector(A.Mat))
   n.row = nrow(vecA.Mat)
   BR = VAR.A2R(A.Mat)
   Br = matrix(0, n.row, 1)
   Beta = VAR.RMLE(BY,BX, BR, Br, Sigma.MLE, FALSE)
   beta = as.vector(Beta)
   RCoef.RMLE= matrix(beta, k, (n.row/k))
   Sigma.RMLE= VAR.Sigma.MLE(BY,BX,RCoef.RMLE, FALSE)
   LR.test = VAR.LR(Sigma.MLE, Sigma.RMLE, T)
   return(LR.test)
 }


VAR.forecast = function(Data, Coef, h, Const = TRUE){
   k = ncol(Data)
   if(Const == TRUE){
      const= Coef[, ncol(Coef)] # 常數項
      Coef = Coef[,-ncol(Coef)] # 去掉常數項
   }else{const = matrix(0, k, 1)}
   VAR.p = ncol(Coef)/k
   Ik = diag(rep(1, k))
   BJ = cbind(Ik, matrix(0, k, (k*VAR.p-k)))
   Ai.list = NULL               # A: list
   for(i in 1:VAR.p){
      Ai.list[[i]] = Coef[,1:k]
      Coef = Coef[, -(1:k)]     # 刪掉前 k 行
   }
   CAL.F = VAR.ARp2AR1(Ai.list) # 處理 F

   X    = VAR.X(Data, VAR.p)      # 處理 z_T
   Y    = VAR.Y(Data, VAR.p)
   zT   = as.matrix(Y[, ncol(Y)])
   zT_1 = as.matrix(X[,ncol(X)])
   zT   = rbind(zT, zT_1)
   zT   = as.matrix(zT[1:(k*VAR.p), 1])
   result = matrix(NA, k, h)    # 結果呈現
   for(i in 1:h){
      one.step = const + BJ %*% CAL.F %*% zT
      result[,i] = one.step
      zT = rbind(one.step, zT)
      zT   = as.matrix(zT[1:(k*VAR.p), 1])
   }
  return(result)
 }


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


VAR.IRF=function(impulse= 1, response= 1, Coef, h=10,
        BSigma.u, Const=TRUE){
   Theta = VAR.Theta(Coef, h, BSigma.u, Const)
   k  = nrow(Coef)
   Ik = diag(rep(1, k))
   ell.j = Ik[, impulse]
   ell.i = Ik[, response]
   IRF.unit = NULL
   IRF.std  = NULL
   for(i in 1:(h+1)){
      IRF.unit[[i]]=t(ell.i)%*%Theta$unit[[i]]%*%ell.j
      IRF.std[[i]]=t(ell.i)%*%Theta$std[[i]]%*%ell.j
   }
   IRF.names  = paste("t = ", 0:h, sep="")
   names(IRF.unit) = IRF.names
   names(IRF.std) = IRF.names
   result = list("unit"= IRF.unit, "std"= IRF.std)
   return(result)               # i = 0, 1, 2,
 }



VAR.Gi = function(Coef, h=10, Const = TRUE){
   Coef.Psi = Coef
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
   Psi   = VAR.F2Psi(Coef.Psi, h, Const)
   F.list= NULL
   F.list[[1]] = diag(rep(1, nrow(CAL.F)))
   for(j in 2:h){
      F.list[[j]] = F.list[[j-1]] %*% t(CAL.F)
   }
   Gi = NULL
   for(i in 1:h){
      Gi.1 = NULL
      for(j in 0:(i-1)){
         Gi.1[[j+1]] = (BJ %*% F.list[[i-1-j + 1]]) %x% Psi[[j + 1]]
      }
      Gi.2 = matrix(0, nrow(Gi.1[[1]]), ncol(Gi.1[[1]]))
      for(j in 0:(i-1)){
         Gi.2 = Gi.2 + Gi.1[[j+1]]
      }
      Gi[[i]] = Gi.2
   }
   return(Gi)                   # i = 1, 2,
 }

VAR.Sigma.alpha = function(Data, Coef, BSigma.u, Const = TRUE){
   k = ncol(Data)
   if(Const == TRUE){
      const= Coef[, ncol(Coef)] # 常數項
      Coef = Coef[,-ncol(Coef)] # 去掉常數項
   }else{const = matrix(0, k, 1)}
   VAR.p  = ncol(Coef)/k
   X      = VAR.X(Data, VAR.p)
   const1 = rep(const, VAR.p)
   T = ncol(X)
   tilde.X = X                  # Lutkepohl (1993), p.85
   for(i in 1:T){
      tilde.X[,i] = X[,i] - const1
   }
   Sigma.alpha = solve((tilde.X %*% t(tilde.X))/T) %x% BSigma.u
   return(Sigma.alpha)
 }


VAR.Psi.Std = function(Data, Coef, BSigma.u, h = 10,
              Const= TRUE){
   k  = nrow(Coef)
   Gi = VAR.Gi(Coef, h, Const)
   S.alpha = VAR.Sigma.alpha(Data,Coef,BSigma.u,Const)
   Psi.Std = NULL
   for(i in 1:h){
      Psi.varcov = Gi[[i]] %*% S.alpha %*% t(Gi[[i]])
      Psi.Std[[i]]=matrix(sqrt(diag(Psi.varcov)),k,k)
   }
   Psi.Std.names  = paste("Std.Psi", 1:h, sep="")
   names(Psi.Std) = Psi.Std.names
   return(Psi.Std)
}


VAR.ddTheta.Std = function(Data, Coef, BSigma.u,
                  h= 10, T, Const= TRUE){
   k = nrow(Coef); P = t(chol(BSigma.u))
   D = D.matrix(k); L = L.matrix(k); K = K.matrix(k)
   Ik  = diag(rep(1, k))
   Ik2 = diag(rep(1, k^2))
   Psi = VAR.F2Psi(Coef,h,Const)# i = 0...h
   Gi  = VAR.Gi(Coef, h, Const) # i = 1...h
   S.alpha=VAR.Sigma.alpha(Data, Coef, BSigma.u,Const)
   S.1 =t(D)%*%(solve(BSigma.u)%x%solve(BSigma.u))%*%D
   S.sigma=2 * solve(S.1)
   H = t(L)%*%(solve(L %*%(Ik2+K)%*%(P%x%Ik)%*%t(L)))
   Ci = NULL; Ci.bar = NULL; ddTheta.Std = NULL
   for(i in 1:h){
      Ci[[i+1]] = (t(P) %x% Ik) %*% Gi[[i]]
      Ci.bar[[i+1]] = (Ik %x% Psi[[i+1]]) %*% H
   }
   Ci[[1]] = matrix(0, nrow(Ci[[2]]), ncol(Ci[[2]]))
   Ci.bar[[1]] = (Ik %x% Psi[[1]]) %*% H
   for(i in 1:(h+1)){
      ddTheta.varcov=(Ci[[i]]%*%S.alpha%*%t(Ci[[i]])
         +Ci.bar[[i]]%*%S.sigma %*% t(Ci.bar[[i]]))/T
      ddTheta.1 = sqrt(diag(ddTheta.varcov))
      ddTheta.Std[[i]]=matrix(ddTheta.1,k,k)
   }
   Theta.Std.names  = paste("Std.Theta", 0:h, sep="")
   names(ddTheta.Std) = Theta.Std.names
   return(ddTheta.Std)
 }


VAR.Theta.bootstrap = function(Data, VAR.p, h= 10, impulse= 1, response= 1, N=100, Const = TRUE){
   lower = 0.025                                        # 控制成 95% CI
   upper = 1-lower
   k = ncol(Data)
   ddY = VAR.ddY(Data, VAR.p)
   ddX = VAR.ddX(Data, VAR.p)
   T   = nrow(ddY)
   T.total= nrow(Data)
   Ik  = diag(rep(1, k))
   Coef = t(VAR.EbyE(ddY, ddX, Const)$ddA)              # Step 1 估計模型
   U    = VAR.EbyE(ddY, ddX, Const)$ddU
   BSigma.u = VAR.ddSigma.OLS(ddY, ddX, Const)
   if(Const == TRUE){
      const = Coef[, ncol(Coef)]
      Coef.noc= Coef[,-ncol(Coef)]                      # 刪掉 const
   }else{
      const = matrix(0, k, 1)
      Coef.noc = Coef
   }
   Theta.unit= VAR.Theta(Coef, h, BSigma.u, Const)$unit # 估算 Theta.unit
   Theta.std = VAR.Theta(Coef, h, BSigma.u, Const)$std  # 估算 Theta.std
   dm.U = scale(U, scale = FALSE)                       # Step 2 去除平均
   Theta.unit.sim = vector("list", N)
   Theta.std.sim  = vector("list", N)
   Y.sim = matrix(0, nrow = T.total, ncol = k)          # Y.sim = 0
   Y.sim[c(1:VAR.p),] = Data[c(1:VAR.p), ]
   for(j in 1:N){
      boot.number = sample(c(1:T), replace = TRUE)      # Step 3 取出放回
      U.sim = dm.U[boot.number,]
      last.y= c(t(Data[VAR.p:1,]))
      for(i in 1:T){
         last.y = last.y[1:(k*VAR.p)]
         Y.sim[i+VAR.p, ] = Coef.noc %*% last.y + const + U.sim[i,]      # Step 4 模擬資料
         last.y = c(Y.sim[i+VAR.p,], last.y)
      }
      ddY.sim = VAR.ddY(Y.sim, VAR.p)                   # Step 5 重新估算
      ddX.sim = VAR.ddX(Y.sim, VAR.p)
      Coef.sim = t(VAR.EbyE(ddY.sim, ddX.sim, Const)$ddA)
      BSigma.sim = VAR.ddSigma.OLS(ddY.sim, ddX.sim, Const)
      Theta.unit.sim[[j]] = VAR.Theta(Coef.sim, h, BSigma.sim, Const)$unit
      Theta.std.sim[[j]]  = VAR.Theta(Coef.sim, h, BSigma.sim, Const)$std
   }
   ell.j = Ik[, impulse]                                # 建立 Confidence Interval
   ell.i = Ik[, response]
   IRF.unit = matrix(0, nrow = N, ncol = (h+1))         # row = N
   IRF.std  = matrix(0, nrow = N, ncol = (h+1))         # row = N
   IRF.unit.sim = matrix(0, nrow = N, ncol = (h+1))     # row = N
   IRF.std.sim  = matrix(0, nrow = N, ncol = (h+1))     # row = N
   for(i in 1:(h+1)){
      IRF.unit[,i]=t(ell.i)%*%Theta.unit[[i]]%*%ell.j   # 估計的 theta
      IRF.std[,i] =t(ell.i)%*%Theta.std[[i]]%*%ell.j
   }
   for(j in 1:N){
      for(i in 1:(h+1)){                                # 模擬的 theta
         IRF.unit.sim[j,i]=t(ell.i)%*%Theta.unit.sim[[j]][[i]]%*%ell.j
         IRF.std.sim[j,i] =t(ell.i)%*%Theta.std.sim[[j]][[i]]%*%ell.j
      }
   }
   CI.S.unit = matrix(0, 2, (h+1))                      # 估算 CI.S
   CI.S.std  = matrix(0, 2, (h+1))
   CI.names = paste("h = ", 0:h, sep="");
   rownames(CI.S.unit) = c("lower", "upper")
   rownames(CI.S.std)  = c("lower", "upper")
   colnames(CI.S.unit) = CI.names
   colnames(CI.S.std)  = CI.names
   for(i in 1:(h+1)){
      CI.S.unit[1, i] = quantile(IRF.unit.sim[,i], lower, na.rm = TRUE)
      CI.S.unit[2, i] = quantile(IRF.unit.sim[,i], upper, na.rm = TRUE)
      CI.S.std[1, i]  = quantile(IRF.std.sim[,i],  lower, na.rm = TRUE)
      CI.S.std[2, i]  = quantile(IRF.std.sim[,i],  upper, na.rm = TRUE)
   }
   CI.H.unit = matrix(0, 2, (h+1))                      # 估算 CI.H
   CI.H.std  = matrix(0, 2, (h+1))
   rownames(CI.H.unit) = c("lower", "upper")
   rownames(CI.H.std)  = c("lower", "upper")
   colnames(CI.H.unit) = CI.names
   colnames(CI.H.std)  = CI.names
   IRF.unit.sim_IRF.unit = IRF.unit.sim - IRF.unit
   IRF.std.sim_IRF.std   = IRF.std.sim  - IRF.std
   for(i in 1:(h+1)){
      q.lower = quantile(IRF.unit.sim_IRF.unit[,i], lower, na.rm = TRUE)
      q.upper = quantile(IRF.unit.sim_IRF.unit[,i], upper, na.rm = TRUE)
      CI.H.unit[1, i] = IRF.unit[1,i] - q.upper
      CI.H.unit[2, i] = IRF.unit[1,i] - q.lower
      q.lower = quantile(IRF.std.sim_IRF.std[,i], lower, na.rm = TRUE)
      q.upper = quantile(IRF.std.sim_IRF.std[,i], upper, na.rm = TRUE)
      CI.H.std[1, i] = IRF.std[1,i] - q.upper
      CI.H.std[2, i] = IRF.std[1,i] - q.lower
   }
   CI.SH.unit = matrix(0, 2, (h+1))                     # 估算 CI.SH
   CI.SH.std  = matrix(0, 2, (h+1))                     # 有誤, 沒有 double bootstrap
   rownames(CI.SH.unit) = c("lower", "upper")
   rownames(CI.SH.std)  = c("lower", "upper")
   colnames(CI.SH.unit) = CI.names
   colnames(CI.SH.std)  = CI.names
   IRF.unit.sim.Std = sqrt(diag(var(IRF.unit.sim)))
   IRF.std.sim.Std  = sqrt(diag(var(IRF.std.sim)))
   IRF.unit.sim_studentized = IRF.unit.sim_IRF.unit
   IRF.std.sim_studentized  = IRF.std.sim_IRF.std
   for(i in 1:(h+1)){
      if(IRF.unit.sim.Std[i] == 0){IRF.unit.sim.Std[i] = 1}
      IRF.unit.sim_studentized[,i] = IRF.unit.sim_studentized[,i] / IRF.unit.sim.Std[i]
      if(IRF.std.sim.Std[i] == 0){IRF.std.sim.Std[i] = 1}
      IRF.std.sim_studentized[,i] = IRF.std.sim_studentized[,i] / IRF.std.sim.Std[i]
   }

   for(i in 1:(h+1)){
      q.lower = quantile(IRF.unit.sim_studentized[,i], lower, na.rm = TRUE)
      q.upper = quantile(IRF.unit.sim_studentized[,i], upper, na.rm = TRUE)
      CI.SH.unit[1, i] = IRF.unit[1,i] - q.upper * IRF.unit.sim.Std[i]
      CI.SH.unit[2, i] = IRF.unit[1,i] - q.lower * IRF.unit.sim.Std[i]
      q.lower = quantile(IRF.std.sim_studentized[,i], lower, na.rm = TRUE)
      q.upper = quantile(IRF.std.sim_studentized[,i], upper, na.rm = TRUE)
      CI.SH.std[1, i] = IRF.std[1,i] - q.upper * IRF.std.sim.Std[i]
      CI.SH.std[2, i] = IRF.std[1,i] - q.lower * IRF.std.sim.Std[i]
   }
   result = list("CI.S.unit" = CI.S.unit, "CI.S.std" = CI.S.std,
                 "CI.H.unit" = CI.H.unit, "CI.H.std" = CI.H.std,
                 "CI.SH.unit" = CI.SH.unit, "CI.SH.std" = CI.SH.std)
   return(result)
 }


VAR.GIRF.impulse = function(impulse = 1, Coef, h = 10,
                   BSigma.u, Const = TRUE){
   k  = nrow(Coef)
   Ik = diag(rep(1, k))
   ell.j = Ik[, impulse]
   Psi   = VAR.F2Psi(Coef, h, Const)
   GIRF.unit = NULL
   GIRF.std  = NULL
   for(i in 1:(h+1)){
      GIRF.unit[[i]]=Psi[[i]]%*%BSigma.u %*% ell.j %*%
                solve(t(ell.j) %*% BSigma.u %*% ell.j)
      GIRF.std[[i]]=Psi[[i]]%*% BSigma.u %*% ell.j %*%
          solve(sqrt(t(ell.j) %*% BSigma.u %*% ell.j))
   }
   GIRF.names  = paste("GIRF", 0:h, sep="")
   names(GIRF.unit) = GIRF.names
   names(GIRF.std)  = GIRF.names
   result = list("unit"= GIRF.unit, "std"= GIRF.std)
   return(result)
 }


VAR.GIRF = function(impulse = 1, response = 1, Coef,
                   h = 10, BSigma.u, Const = TRUE){
   k  = nrow(Coef)
   Ik = diag(rep(1, k))
   ell.j = Ik[, impulse]
   ell.i = Ik[, response]
   Psi   = VAR.F2Psi(Coef, h, Const)
   GIRF.unit = NULL
   GIRF.std  = NULL
   for(i in 1:(h+1)){
      GIRF.unit[[i]]=t(ell.i)%*%Psi[[i]]%*%BSigma.u%*%
         ell.j%*%solve(t(ell.j) %*% BSigma.u %*%ell.j)
      GIRF.std[[i]]=t(ell.i)%*%Psi[[i]]%*% BSigma.u%*%
      ell.j%*%solve(sqrt(t(ell.j)%*%BSigma.u%*%ell.j))
   }
   GIRF.names  = paste("t = ", 0:h, sep="")
   names(GIRF.unit) = GIRF.names
   names(GIRF.std)  = GIRF.names
   result = list("unit"= GIRF.unit, "std"= GIRF.std)
   return(result)
 }

VAR.decomp = function(m= 1, Coef, h= 10,
                BSigma.u, Const = TRUE){
   k  = nrow(Coef)
   Ik = diag(rep(1, k))
   ell.m = Ik[, m]
   ddTheta = VAR.Theta(Coef, h, BSigma.u, Const)$std
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


VAR.decomp.GIRF = function(m= 1, Coef, h= 10,
              BSigma.u, Const = Const){
   k=nrow(Coef); Ik=diag(rep(1, k)); ell.m=Ik[, m]
   Coef.Psi= Coef
   Psi     = VAR.F2Psi(Coef.Psi, h, Const)
   ddTheta = VAR.Theta(Coef, h, BSigma.u, Const)$std
   den = matrix(0, 1, h)                # 分母
   num = matrix(0, k, h)                # 分子
   var.decomp = matrix(0, k, h)         # 變異數分解
   inv.sigma2 = 1/diag(BSigma.u)
   i = 1                                # i = 0
   den[1,i]=t(ell.m)%*%ddTheta[[i]]%*%
            t(ddTheta[[i]])%*%ell.m     # 不用 ell.j
   num[,i] =(t(ell.m)%*%Psi[[1]]%*%BSigma.u)^2
   var.decomp[,i] = (num[,i]*inv.sigma2)/den[1,i]
   for(i in 2:h){                       # i = 1...h-1
      den[1,i]=den[1,i-1]+t(ell.m)%*%ddTheta[[i]]%*%
               t(ddTheta[[i]])%*%ell.m
      num[,i] =num[,i-1]+
               (t(ell.m)%*%Psi[[i]]%*%BSigma.u)^2
      var.decomp[,i] = (num[,i]*inv.sigma2)/den[1,i]
   }
   var.decomp = t(var.decomp)
   decomp.names  = paste("h=", 1:h, sep="")
   rownames(var.decomp) = decomp.names
   return(var.decomp)
 }


VAR.example.A = function(Data, VAR.p = 2, AMat,
        Const = TRUE){
   Y    = VAR.Y(Data, VAR.p)
   X    = VAR.X(Data, VAR.p)
   k    = ncol(Data)
   T    = ncol(Y)
   Coef = VAR.OLS(Y, X, Const)
   BSigma.u= VAR.Sigma.OLS(Y, X, Coef, Const)
   params.A = sum(is.na(AMat))
   start    = as.vector(rep(0.1, params.A)) # 起始值
   param.Aidx = which(is.na(AMat), arr.ind = TRUE)
   Bmat     = diag(k)                       # B = I_k
   loglike = function(coef){
      AMat[param.Aidx] = coef
      ln.likefn = -1 * (T*k/2) * log(2*pi) +
              T/2*log(det(AMat)^2) -
              T/2*log(det(Bmat)^2) -
              T/2*sum(diag(t(AMat)%*%solve(t(Bmat))%*%
                  solve(Bmat) %*% AMat %*% BSigma.u))
      return(-ln.likefn)                    # 極小化
   }
   optimun = optim(start, loglike,
                   method = "BFGS", hessian = TRUE)
   AMat[param.Aidx] = optimun$par
   if(any(diag(solve(AMat) %*% Bmat) < 0)){ # 保證為正
      ind = which(diag(solve(AMat) %*% Bmat) < 0)
      AMat[ind, ind] = -1 * AMat[ind, ind]
      #AMat[, ind] = -1 * AMat[, ind]  # 錯誤設定:vars
   }
   return(AMat)
 }



VAR.varest.BQ = function(Data, VAR.p = 2, Const = TRUE){
   Y    = VAR.Y(Data, VAR.p)
   X    = VAR.X(Data, VAR.p)
   k    = ncol(Data)
   T    = ncol(Y)
   CALA1= diag(k)               # I_k
   CALA2= diag(k)
   Coef = VAR.OLS(Y, X, Const)
   BSigma.u= VAR.Sigma.OLS(Y, X, Coef, Const)
   for(i in 1:VAR.p){
      CALA1 = CALA1 - Coef[,1:k]
      CALA2 = CALA2 - t(Coef[,1:k])
      Coef = Coef[, -(1:k)]     # 刪掉前 k 行
   }
   XiXi = solve(CALA1) %*% BSigma.u %*% solve(CALA2)
   Xi   = t(chol(XiXi))
   AB   = CALA1 %*% Xi
   result = list("Xi" = Xi, "AB" = AB)  #  AB = A.inv %*% B
   return(result)
 }


VAR.MLEest = function(Data, A.Mat, VAR.p, Const = TRUE){
   BY    = VAR.Y(Data, VAR.p)
   BX    = VAR.X(Data, VAR.p)
   k     = nrow(BY)
   if(Const == TRUE){
      n.col = ncol(BX)
      ones  = as.matrix(matrix(1, 1, n.col))
      BX    = rbind(BX, ones)   # 最後一行是常數項
      Const = FALSE             # 以下的 Const = FALSE
   }
   Coef.MLE =VAR.MLE(BY, BX, Const)     # 參數不受限
   Sigma.MLE=VAR.Sigma.MLE(BY,BX,Coef.MLE,Const)
   vecA.Mat =as.matrix(as.vector(A.Mat))# A.Mat
   eq.0     =which(vecA.Mat == 0)
   if(length(eq.0) > 0){                # 參數受限為 0
      n.row = nrow(vecA.Mat)
      BR = VAR.A2R(A.Mat)
      Br = matrix(0, n.row, 1)
      Beta = VAR.RMLE(BY,BX, BR, Br, Sigma.MLE, Const)
      beta = as.vector(Beta)
      RCoef.RMLE= matrix(beta, k, (n.row/k))
      Sigma.RMLE= VAR.Sigma.MLE(BY,BX,RCoef.RMLE,Const)
      Coef.MLE  = RCoef.RMLE
      Sigma.MLE = Sigma.RMLE
   }
   result = list("Coef" = Coef.MLE, "S" = Sigma.MLE)
 }




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



VAR.svarest.AB = function(Data, VAR.p = 2, A0, B0, Const = TRUE){
   Y    = VAR.Y(Data, VAR.p)
   X    = VAR.X(Data, VAR.p)
   k    = ncol(Data)                            # k
   T    = ncol(Y)                               # T
   Coef = VAR.OLS(Y, X, Const)                  # OLS
   BSigma.u = VAR.Sigma.OLS(Y, X, Coef, Const)  # BSigma.u
   params.NAA = sum(is.na(A0))                  # NA 才估計
   params.NAB = sum(is.na(B0))                  # NA 才估計
   params.NA  = params.NAA + params.NAB
   start    = as.vector(rep(0.1, params.NA))    # 起始值
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



VAR.example.A.IRF=function(Data, VAR.p = 2, AMat, h=10,
               Const = TRUE){
   Y    = VAR.Y(Data, VAR.p)
   X    = VAR.X(Data, VAR.p)
   Coef = VAR.OLS(Y, X, Const)
   Psi  = VAR.F2Psi(Coef, h, Const)
   A0   = VAR.example.A(Data, VAR.p, AMat, Const)
   A0.inv= solve(A0)
   ddTheta.svar = list()
   for(i in 1:(h+1)){
      ddTheta.svar[[i]] = Psi[[i]] %*% A0.inv
   }
   Theta.names = paste("Theta", 0:h, sep="");
   names(ddTheta.svar)  = Theta.names
   return(ddTheta.svar)
 }


VAR.example.A.decomp = function(m=1,
            Data, VAR.p=2, AMat, h=4, Const=TRUE){
   Y    = VAR.Y(Data, VAR.p)
   X    = VAR.X(Data, VAR.p)
   k    = ncol(Data)
   T    = ncol(Y)
   ddTheta=VAR.example.A.IRF(Data,VAR.p,AMat,h,Const)
   Ik = diag(rep(1, k)) # 以下部份是參考 VAR.decomp
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


VAR.example.A.hist=function(Data,VAR.p,AMat,Const){
   ddY = VAR.ddY(Data, VAR.p); ddX=VAR.ddX(Data,VAR.p)
   k   = ncol(ddY); T  = nrow(ddY)
   U   = VAR.EbyE(ddY, ddX, Const)$ddU # VAR 殘差
   A   = VAR.example.A(Data, VAR.p, AMat, Const)
   ddEpsilon=numeric(0)
   for(i in 1:T){
      ddE = A %*% (U[i,])       # 還原成 epsilon
      ddEpsilon = rbind(ddEpsilon, t(ddE))
   }
   Epsilon.list = list()
   for(i in 1:k){
      Epsilon.list[[i]]=matrix(0, T, k) # 其它干擾項=0
      Epsilon.list[[i]][,i] = ddEpsilon[,i]
   }
   ddTheta=VAR.example.A.IRF(Data,VAR.p,AMat,T,Const)
   hist.list = list()   # 所有 historical decomp
   for(j in 1:k){
      hist.sum  = matrix(NA, VAR.p, k)
      for(t in 1:T){
         hist.j = matrix(0, 1, k)
         for(i in 1:t){
           hist.ind = ddTheta[[i]] %*%
              as.matrix(Epsilon.list[[j]][(t-i+1),])
           hist.j = hist.j + t(hist.ind)
         }
         hist.sum = rbind(hist.sum, hist.j)
      }
      hist.list[[j]] = hist.sum
   }
   Hist.Decomp = matrix(NA, (T+VAR.p), (k*k) )
   for(i in 1:k){       # 重新整理
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



VAR.svarirf.A=function(Data, VAR.p = 2, AMat, h=10, Const = TRUE){
   Y    = VAR.Y(Data, VAR.p)
   X    = VAR.X(Data, VAR.p)
   Coef = VAR.OLS(Y, X, Const)
   Psi  = VAR.F2Psi(Coef, h, Const)
   A0   = VAR.svarest.A(Data, VAR.p, AMat, Const)$A0.svar       # 不同之處
   A0.inv= solve(A0)                                            # 不同之處
   ddTheta.svar = list()
   for(i in 1:(h+1)){
      ddTheta.svar[[i]] = Psi[[i]] %*% A0.inv                   # 不同之處
   }
   Theta.names = paste("Theta", 0:h, sep="");
   names(ddTheta.svar)  = Theta.names
   return(ddTheta.svar)
 }


VAR.svarirf.B=function(Data, VAR.p = 2, BMat, h=10, Const = TRUE){
   Y    = VAR.Y(Data, VAR.p)
   X    = VAR.X(Data, VAR.p)
   Coef = VAR.OLS(Y, X, Const)
   Psi  = VAR.F2Psi(Coef, h, Const)
   B0   = VAR.svarest.B(Data, VAR.p, BMat, Const)$B0.svar       # 不同之處
   ddTheta.svar = list()
   for(i in 1:(h+1)){
      ddTheta.svar[[i]] = Psi[[i]] %*% B0                       # 不同之處
   }
   Theta.names = paste("Theta", 0:h, sep="");
   names(ddTheta.svar)  = Theta.names
   return(ddTheta.svar)
 }

VAR.svarirf.AB=function(Data, VAR.p = 2, AMat, BMat, h=10, Const = TRUE){
   Y    = VAR.Y(Data, VAR.p)
   X    = VAR.X(Data, VAR.p)
   Coef = VAR.OLS(Y, X, Const)
   Psi  = VAR.F2Psi(Coef, h, Const)
   A0   = VAR.svarest.AB(Data, VAR.p, AMat, BMat,Const)$A0.svar # 不同之處
   A0.inv= solve(A0)
   B0   = VAR.svarest.AB(Data, VAR.p, AMat, BMat,Const)$B0.svar # 不同之處
   ddTheta.svar = list()
   for(i in 1:(h+1)){
      ddTheta.svar[[i]] = Psi[[i]] %*% A0.inv %*% B0            # 不同之處
   }
   Theta.names = paste("Theta", 0:h, sep="");
   names(ddTheta.svar)  = Theta.names
   return(ddTheta.svar)
 }      # 跟 EViews 有可能正負號不一樣, 但是這正常. 可能 A0 是負, B0 是正; 但 EViews 是 A0 是正, B0 是負
        # B0 的正負號都跟 EViews 不一樣


VAR.svarirf.BQ=function(Data, VAR.p = 2, h=10, Const = TRUE){   # 不同之處
   Y    = VAR.Y(Data, VAR.p)
   X    = VAR.X(Data, VAR.p)
   Coef = VAR.OLS(Y, X, Const)
   Psi  = VAR.F2Psi(Coef, h, Const)
   AB   = VAR.varest.BQ(Data, VAR.p, Const)$AB                  # 不同之處 AB = A.inv %*% B
   ddTheta.svar = list()
   for(i in 1:(h+1)){
      ddTheta.svar[[i]] = Psi[[i]] %*% AB                       # 不同之處
   }
   Theta.names = paste("Theta", 0:h, sep="");
   names(ddTheta.svar)  = Theta.names
   return(ddTheta.svar)
 }






VAR.svardecomp.A = function(m = 1, Data, VAR.p=2, AMat, h=4, Const=TRUE){
   Y    = VAR.Y(Data, VAR.p)
   X    = VAR.X(Data, VAR.p)
   k    = ncol(Data)
   T    = ncol(Y)
   ddTheta=VAR.svarirf.A(Data, VAR.p, AMat, h, Const)   # 不同之處
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

VAR.svardecomp.B = function(m = 1, Data, VAR.p=2, BMat, h=4, Const=TRUE){
   Y    = VAR.Y(Data, VAR.p)
   X    = VAR.X(Data, VAR.p)
   k    = ncol(Data)
   T    = ncol(Y)
   ddTheta=VAR.svarirf.B(Data, VAR.p, BMat, h, Const)   # 不同之處
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



VAR.svardecomp.AB = function(m = 1, Data, VAR.p=2, AMat, BMat, h=4, Const=TRUE){
   Y    = VAR.Y(Data, VAR.p)
   X    = VAR.X(Data, VAR.p)
   k    = ncol(Data)
   T    = ncol(Y)
   ddTheta=VAR.svarirf.AB(Data, VAR.p, AMat, BMat, h, Const)   # 不同之處
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


VAR.svardecomp.BQ = function(m = 1, Data, VAR.p=2, h=4, Const=TRUE){
   Y    = VAR.Y(Data, VAR.p)
   X    = VAR.X(Data, VAR.p)
   k    = ncol(Data)
   T    = ncol(Y)
   ddTheta=VAR.svarirf.BQ(Data, VAR.p, h, Const)   # 不同之處
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



VAR.svarhist.A = function(Data, VAR.p, AMat, Const){
   ddY = VAR.ddY(Data, VAR.p)
   ddX = VAR.ddX(Data, VAR.p)
   k   = ncol(ddY)
   T   = nrow(ddY)
   U   = VAR.EbyE(ddY, ddX, Const)$ddU                  # VAR 殘差
   A   = VAR.svarest.A(Data, VAR.p, AMat, Const)$A0.svar# A-Model 估計 A 不同之處
   ddEpsilon=numeric(0)
   for(i in 1:T){
      ddE = A %*% (U[i,])                               # 還原成 epsilon 不同之處
      ddEpsilon = rbind(ddEpsilon, t(ddE))
   }
   Epsilon.list = list()
   for(i in 1:k){
      Epsilon.list[[i]]=matrix(0, T, k)                 # 其它干擾項=0, 所以是 X 0 0 矩陣
      Epsilon.list[[i]][,i] = ddEpsilon[,i]
   }
   ddTheta=VAR.svarirf.A(Data, VAR.p, AMat, h=T, Const) # ddTheta 不同之處
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




VAR.svarhist.B = function(Data, VAR.p, BMat, Const){
   ddY = VAR.ddY(Data, VAR.p)
   ddX = VAR.ddX(Data, VAR.p)
   k   = ncol(ddY)
   T   = nrow(ddY)
   U   = VAR.EbyE(ddY, ddX, Const)$ddU                  # VAR 殘差
   B   = VAR.svarest.B(Data, VAR.p, BMat, Const)$B0.svar# B-Model 估計 B 不同之處
   B.inv = solve(B)
   ddEpsilon=numeric(0)
   for(i in 1:T){
      ddE = B.inv %*% (U[i,])                           # 還原成 epsilon 不同之處
      ddEpsilon = rbind(ddEpsilon, t(ddE))
   }
   Epsilon.list = list()
   for(i in 1:k){
      Epsilon.list[[i]]=matrix(0, T, k)                 # 其它干擾項=0, 所以是 X 0 0 矩陣
      Epsilon.list[[i]][,i] = ddEpsilon[,i]
   }
   ddTheta=VAR.svarirf.B(Data, VAR.p, BMat, h=T, Const) # ddTheta 不同之處
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



VAR.svarhist.AB = function(Data, VAR.p, AMat, BMat, Const){
   ddY = VAR.ddY(Data, VAR.p)
   ddX = VAR.ddX(Data, VAR.p)
   k   = ncol(ddY)
   T   = nrow(ddY)
   U   = VAR.EbyE(ddY, ddX, Const)$ddU                  # VAR 殘差
   A   = VAR.svarest.AB(Data, VAR.p, AMat, BMat, Const)$A0.svar     # AB-Model 估計 A 不同之處
   B   = VAR.svarest.AB(Data, VAR.p, AMat, BMat, Const)$B0.svar     # AB-Model 估計 B
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
   ddTheta=VAR.svarirf.AB(Data, VAR.p, AMat, BMat, h=T, Const)          # ddTheta 不同之處
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



VAR.svarhist.BQ = function(Data, VAR.p, Const){
   ddY = VAR.ddY(Data, VAR.p)
   ddX = VAR.ddX(Data, VAR.p)
   k   = ncol(ddY)
   T   = nrow(ddY)
   U   = VAR.EbyE(ddY, ddX, Const)$ddU                  # VAR 殘差
   AB  = VAR.varest.BQ(Data, VAR.p, Const)$AB           # AB-Model 估計 B  不同之處
   AB.inv = solve(AB)
   ddEpsilon=numeric(0)
   for(i in 1:T){
      ddE = AB.inv %*% (U[i,])                          # 還原成 epsilon   不同之處
      ddEpsilon = rbind(ddEpsilon, t(ddE))
   }
   Epsilon.list = list()
   for(i in 1:k){
      Epsilon.list[[i]]=matrix(0, T, k)                 # 其它干擾項=0, 所以是 X 0 0 矩陣
      Epsilon.list[[i]][,i] = ddEpsilon[,i]
   }
   ddTheta=VAR.svarirf.BQ(Data, VAR.p, h=T, Const)          # ddTheta 不同之處
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







L.matrix = function (n){        # 參考 matrixcalc
    if (missing(n))
        stop("argument n is missing")
    if (!is.numeric(n)) 
        stop("argument n is not numeric")
    if (n != trunc(n)) 
        stop("argument n is not an integer")
    if (n < 2) 
        stop("argument n is less than 2")
    u <- u.vectors(n)
    E <- E.matrices(n)
    k <- u$k
    I <- u$I
    p <- n * (n + 1)/2
    nsq <- n * n
    L <- matrix(0, nrow = p, ncol = nsq)
    for (j in 1:n) {
        for (i in j:n) {
            L <- L + I[, k[i, j]] %*% t(vec(E[[i]][[j]]))
        }
    }
    return(L)
}

u.vectors = function (n){       # 參考 matrixcalc
    if (n != trunc(n))
        stop("argument n is not an integer")
    if (n < 2) 
        stop("argument n is less than 2")
    p <- n * (n + 1)/2
    I <- diag(rep(1, p))
    k <- matrix(0, nrow = n, ncol = n)
    for (j in 1:n) {
        for (i in j:n) {
            k[i, j] <- (j - 1) * n + i - 0.5 * j * (j - 1)
        }
    }
    return(list(k = k, I = I))
}

E.matrices = function (n){      # 參考 matrixcalc
    if (missing(n))
        stop("argument n is missing")
    if (!is.numeric(n)) 
        stop("argument n is not numeric")
    if (n != trunc(n)) 
        stop("argument n is not an integer")
    if (n < 2) 
        stop("argument n is less than 2")
    I <- diag(rep(1, n))
    E <- list()
    for (i in 1:n) {
        E[[i]] <- list()
        for (j in 1:n) {
            E[[i]][[j]] <- I[i, ] %o% I[j, ]
        }
    }
    return(E)
}

K.matrix = function (r, c = r){
    if (missing(r))
        stop("argument r is missing")
    if (!is.numeric(r)) 
        stop("argument r is not numeric")
    if (r != trunc(r)) 
        stop("argument r is not an integer")
    if (r < 2) 
        stop("argument r is less than 2")
    if (!is.numeric(c)) 
        stop("argument c is not numeric")
    if (c != trunc(c)) 
        stop("argument c is not an integer")
    if (c < 2) 
        stop("argument c is less than 2")
    H <- H.matrices(r, c)
    p <- r * c
    K <- matrix(0, nrow = p, ncol = p)
    for (i in 1:r) {
        for (j in 1:c) {
            Hij <- H[[i]][[j]]
            K <- K + (Hij %x% t(Hij))
        }
    }
    return(K)
}


H.matrices = function (r, c = r){       # 參考 matrixcalc
    if (missing(r))
        stop("argument r is missing")
    if (!is.numeric(r)) 
        stop("argument r is not numeric")
    if (r != trunc(r)) 
        stop("argument r is not an integer")
    if (r < 2) 
        stop("argument r is less than 2")
    if (!is.numeric(c)) 
        stop("argument c is not numeric")
    if (c != trunc(c)) 
        stop("argument c is not an integer")
    if (c < 2) 
        stop("argument c is less than 2")
    Ir <- diag(rep(1, r))
    Ic <- diag(rep(1, c))
    H <- list()
    for (i in 1:r) {
        H[[i]] <- list()
        for (j in 1:c) {
            H[[i]][[j]] <- Ir[i, ] %o% Ic[j, ]
        }
    }
    return(H)
}


D.matrix = function (n){
    if (missing(n))
        stop("argument n is missing")
    if (!is.numeric(n)) 
        stop("argument n is not numeric")
    if (n != trunc(n)) 
        stop("argument n is not an integer")
    if (n < 2) 
        stop("argument n is less than 2")
    p <- n * (n + 1)/2
    nsq <- n * n
    Dt <- matrix(0, nrow = p, ncol = nsq)
    T <- T.matrices(n)
    u <- u.vectors(n)
    k <- u$k
    I <- u$I
    for (j in 1:n) {
        for (i in j:n) {
            Dt <- Dt + I[, k[i, j]] %*% t(vec(T[[i]][[j]]))
        }
    }
    return(t(Dt))
}

T.matrices = function (n){      # 參考 matrixcalc
    if (missing(n))
        stop("argument n is missing")
    if (!is.numeric(n)) 
        stop("argument n is not numeric")
    if (n != trunc(n)) 
        stop("argument n is not an integer")
    if (n < 2) 
        stop("argument n is less than 2")
    E <- E.matrices(n)
    T <- list()
    for (i in 1:n) {
        T[[i]] <- list()
        for (j in 1:n) {
            if (i == j) {
                T[[i]][[j]] <- E[[i]][[j]]
            }
            else {
                T[[i]][[j]] <- E[[i]][[j]] + E[[j]][[i]]
            }
        }
    }
    return(T)
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





