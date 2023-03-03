###### VAR/SVAR 模型相關程式
rm(list=ls())
Path = "c:\\VAR\\"              # 設定路徑
setwd(Path)
source("VARsource.R")           # 讀取 VARsource.R 檔

        ###### 讀取資料:第 1 章  ######
file = "TWData.csv"
data = read.csv(file = file, header = TRUE)
By   = as.matrix(data[,2:4])

        #----- 模型設定 -----#
VAR.P = 2                       # 最大的落後項數
CONST = TRUE                    # 是否有常數項
Y     = VAR.Y(By, VAR.P)        # 設定 Y
X     = VAR.X(By, VAR.P)        # 設定 X
ddY   = VAR.ddY(By, VAR.P)      # 設定 ddY
ddX   = VAR.ddX(By, VAR.P)      # 設定 ddX

        ###### 參數估計:第 2 章 ######
(Coef.OLS    = VAR.OLS(Y, X, CONST)                  )
(Coef.EbyE   = VAR.EbyE(ddY, ddX, CONST)             )
(Sigma.OLS   = VAR.Sigma.OLS(Y, X, Coef.OLS, CONST)  )
(ddSigma.OLS = VAR.ddSigma.OLS(ddY, ddX, CONST)      )
(Sigma.MLE   = VAR.Sigma.MLE(Y, X, Coef.OLS, CONST)  )
(ddSigma.MLE = VAR.ddSigma.MLE(ddY, ddX, CONST)      )

        ###### 受限制參數估計:第 3 章 ######
A.Mat  = matrix(1, 3, 7)        # 設定那些參數要受限制
A.Mat[2,3] = 0; A.Mat[2,6] = 0;
A.Mat[3,2] = 0; A.Mat[3,5] = 0;
R = VAR.A2R(A.Mat)              # 設定 R
r = matrix(0, 21, 1)            # 設定 r

        #----- 以 OLS 估計受限制參數 -----#
Beta = VAR.ROLS(Y, X, R, r, CONST)
beta = as.vector(Beta)
(RCoef.ROLS = matrix(beta, 3, 7)                     )

        #----- 以 FGLS 估計受限制參數 -----#
Sigma.OLS = VAR.Sigma.OLS(Y, X, Coef.OLS, CONST)
Beta = VAR.RFGLS(Y, X, R, r, Sigma.OLS, CONST)
beta = as.vector(Beta)
(RCoef.RFGLS = matrix(beta, 3, 7)                    )

        #----- 以 MLE 估計受限制參數 -----#
Sigma.MLE = VAR.Sigma.MLE(Y, X, Coef.OLS, CONST)
Beta = VAR.RMLE(Y, X, R, r, Sigma.MLE, CONST)
beta = as.vector(Beta)
(RCoef.RMLE = matrix(beta, 3, 7)                     )

        #----- MLE 估計群組外生特性的參數 -----#
(exog.MLE = VAR.exog.MLE(By, c(1, 3), VAR.P, CONST)  )

        #----- MLE 估計群組外生特性的受限參數 -----#
A1.Mat = matrix(1, 3, 7);       # 設定那些參數要受限制
A1.Mat[1,2] = 0; A1.Mat[1,5] = 0;
A1.Mat[3,2] = 0; A1.Mat[3,5] = 0
R1 = VAR.A2R(A1.Mat)            # 設定 R
r1 = matrix(0, 21, 1)           # 設定 r
Sigma.MLE = VAR.Sigma.MLE(Y, X, Coef.OLS, CONST)
Beta = VAR.RMLE(Y, X, R1, r1, Sigma.MLE, CONST)
beta = as.vector(Beta)
(RCoef1.RMLE = matrix(beta, 3, 7)                    )
(exog.RMLE = VAR.exog.RMLE(By, c(1, 3), VAR.P, CONST))

        ###### 假設檢定:第 4 章 ######
        #----- 參數估計:實例 1 -----#
beta.std = VAR.OLS.Std(X, Sigma.OLS, CONST)
(Coef.OLS.Std = matrix(beta.std, 3, 7)               )
(Sigma.RFGLS= VAR.Sigma.MLE(Y, X, RCoef.RFGLS, CONST))
beta.std = VAR.RFGLS.Std(X, Sigma.RFGLS, R, CONST)
(RCoef.RFGLS.Std = matrix(beta.std, 3, 7)            )

        #----- 參數估計:實例 2 -----#
Sigma1.RFGLS = VAR.Sigma.MLE(Y, X, RCoef1.RMLE, CONST)
beta.std   = VAR.RFGLS.Std(X, Sigma1.RFGLS, R1, CONST)
(RCoef1.RFGLS.Std = matrix(beta.std, 3, 7)           )

        #----- t-ratios -----#
(t.ratios = VAR.tratio(RCoef.RMLE, RCoef.RFGLS.Std)  )

        #----- Wald 檢定 -----#
C    = VAR.A2C(A.Mat)           # 設定虛無假設
c    = matrix(0, 4, 1)

        #----- Wald 檢定:實例 1 -----#
Wald = VAR.Wald(Y, X, C, c, Sigma.OLS, CONST)
(cat("F:", Wald$F, " p-value:", 1-pf(Wald$F, 4, 354)))
C1   = VAR.A2C(A1.Mat)
c1   = matrix(0, 4, 1)

        #----- Wald 檢定:實例 2 -----#
Wald1= VAR.Wald(Y, X, C1, c1, Sigma.OLS, CONST)
(cat("F:", Wald1$F," p-value:",1-pf(Wald1$F, 4, 354)))

        #----- LR 檢定 -----#
Sigma.RMLE = VAR.Sigma.MLE(Y, X, RCoef.RMLE, CONST)
T  = ncol(Y)
        #----- LR 檢定:實例 1 -----#
(LR = VAR.LR(Sigma.MLE, Sigma.RMLE, T)               )
Coef.MLE = Coef.OLS
Log.Alt = VAR.loglike(Y,X, Coef.MLE, Sigma.MLE, CONST)
Log.Null=VAR.loglike(Y,X,RCoef.RMLE,Sigma.RMLE, CONST)
(2*(Log.Alt - Log.Null)                              )

        #----- LR 檢定:實例 2 -----#
Sigma1.RMLE = VAR.Sigma.MLE(Y, X, RCoef1.RMLE, CONST)
(LR = VAR.LR(Sigma.MLE, Sigma1.RMLE, T)              )

        #----- 資訊準則 -----#
A0.Mat = matrix(1, 3, 7)
(SIC = VAR.IC(Sigma.MLE, A0.Mat, T)$SIC              )
(SIC = VAR.IC(Sigma.RMLE, A.Mat, T)$SIC              )
(IC = VAR.Select(By, Max.lag = 4, CONST)             )
apply(IC, 1, which.min)
(VAR.LRtestp(By, 2, CONST)                           )

        ###### 模型應用:第 5 章 ######
        #----- 預測 -----#
(forecast = VAR.forecast(By, Coef.OLS, 3, CONST)     )

        #----- Granger 因果檢定=Wald 檢定:實例 2 -----#
Wald1= VAR.Wald(Y, X, C1, c1, Sigma.OLS, CONST)
(cat("F:", Wald1$F," p-value:",1-pf(Wald1$F, 4, 354)))

        #----- 衝擊反應函數 -----#
IRF = VAR.IRF(impulse = 2, response = 1,
               Coef.OLS, h = 4,  Sigma.OLS, CONST)
(IRF$std                                             )
Theta = VAR.Theta(Coef.OLS, h = 2, Sigma.OLS, CONST)
(Theta$std                                           )

        #----- 衝擊反應函數樣本標準差 -----#
Psi.Std = VAR.Psi.Std(By,Coef.OLS,Sigma.OLS,h=4,CONST)
(Psi.Std[[2]]                                        )
(ddTheta.Std = VAR.ddTheta.Std(By, Coef.OLS,
               Sigma.OLS, h = 2, T = 125, CONST)     )

        #----- 以拔靴法估算衝擊反應函數 -----#
VAR.Theta.bootstrap(By, VAR.P, h = 2, impulse= 2,
               response= 1, N=100, CONST)

        #----- 一般化衝擊反應函數 -----#
GIRF= VAR.GIRF(2, 1, Coef.OLS, h= 4, Sigma.OLS, CONST)
(GIRF$std                                            )
        #----- 預測誤差變異數分解 -----#
(Dec= VAR.decomp(m=1,Coef.OLS, h=5, Sigma.OLS, CONST))
VAR.decomp.GIRF(m=1, Coef.OLS, h= 5, Sigma.OLS, CONST)

        ###### SVAR 估計:第 7 章 ######
        #----- A-Model 參數估算 -----#
Amat =diag(3)
diag(Amat) = NA;  Amat[2,1]  = NA;  Amat[3,1]  = NA
(A = VAR.example.A(By, VAR.P, Amat, CONST)           )

        #----- Blanchard-Quah 模型參數估算 -----#
BQ = VAR.varest.BQ(By, VAR.P, CONST)
(BQ$Xi                                               )
(BQ$AB                                               )

        ###### SVAR 應用:第 8 章 ######
        #----- A-Model 衝擊反應函數 -----#
(ddTheta.A=VAR.example.A.IRF(By,VAR.P,Amat,h=2,CONST))

        #----- Blanchard-Quah 模型衝擊反應函數 -----#
(ddTheta.BQ = VAR.svarirf.BQ(By, VAR.P, h = 2, CONST))

        #----- A-Model 預測誤差變異數分解 -----#
(Dec.A=VAR.example.A.decomp(1,By,VAR.P,Amat,3,CONST) )

        #----- Blanchard-Quah 模型預測誤差變異數分解 -----#
(Dec.BQ = VAR.svardecomp.BQ(m=1,By,VAR.P,h=3,CONST)  )

        #----- A-Model 歷史分解 -----#
Hist.Dec.A= VAR.example.A.hist(By, VAR.P, Amat, CONST)
head(Hist.Dec.A[,1:3])
head(Hist.Dec.A[,4:6])

        #----- Blanchard-Quah 模型歷史分解 -----#
Hist.BQ = VAR.svarhist.BQ(Data = By, VAR.P, CONST)
head(Hist.BQ[,1:3])

        #----- Base Project 估計 -----#
Hist.c0 = VAR.baseproject(By, VAR.P, CONST)
head(Hist.c0)






###### R 套件 vars
rm(list=ls())
Path = "c:\\VAR\\"              # 設定路徑
setwd(Path)
source("VARsource.R")           # 讀取 VARsource.R 檔

        ###### 讀取資料  ######
file = "TWData.csv"
data = read.csv(file = file, header = TRUE)
By   = as.matrix(data[,2:4])

        ###### 參數估計 ######
library(vars)                   # 讀取 vars 套件
varest = VAR(By, p = 2, type = c("const"))
summary(varest)

        ###### 受限制參數估計: OLS 估算 ######
A.Mat  = matrix(1, 3, 7)        # 設定那些參數要受限制
A.Mat[2,3] = 0; A.Mat[2,6] = 0;
A.Mat[3,2] = 0; A.Mat[3,5] = 0;
varest.rest = restrict(varest, method = "manual",
              resmat = A.Mat)
B(varest.rest)
Acoef(varest.rest)

        ###### 預測 ######
(forecast = predict(varest, n.ahead = 3)             )

        ###### Granger 因果 ######
(Granger = causality(varest, cause=c("y2"))          )

        ###### 衝擊反應函數 ######
(irf = irf(varest, n.ahead = 3)                      )

        ###### 預測誤差變異數分解 ######
(fevd = fevd(varest, n.ahead = 10)                   )

        ###### A-Model 參數估算 ######
Amat =diag(3)
diag(Amat) = NA;  Amat[2,1]  = NA;  Amat[3,1]  = NA
svar = SVAR(x = varest, estmethod = "direct", Amat,
     Bmat = NULL, hessian = TRUE, method="BFGS")
summary(svar)
svar$Sigma.U

        ###### Blanchard-Quah 模型參數估算 ######
BQ(varest)
