#' @section Impulse Response Function (IRF) -- without C.I. (point estimators)

source("./code/analysis/svar.R")

# SVAR_AB_IRF <- VAR.svarirf.AB(By, VAR.P, Amat, Bmat, h = hrz, CONST, SVAR_AB_est = SVAR_AB_est)

# 5*5個圖的time series
df_IRF_plot <- matrix(NA, hrz+1, num_var^2) 
h <- 0 # h表示第幾期的IRF
for(period in SVAR_AB_IRF){
  k <- 0 # k表示把5*5的矩陣攤平到25個col的df時，要攤到第幾個columns上
  h <- h + 1 # h表示第幾期的IRF
  for(j in (1:num_var) ){
    for(i in (1:num_var) ){
      k <- k + 1 # k表示把5*5的矩陣攤平到25個col的df時，要攤到第幾個columns上
      df_IRF_plot[h,k] <- period[i,j]
    }
  }
}
# the dimension of IRF dataframe would be horizon * num_var^2
df_IRF_plot <- df_IRF_plot %>% as_tibble()
saveRDS(df_IRF_plot, "./data/intermediate_result/df_IRF_plot.RDS")

# output entire table
IRF_TABLE <- df_IRF_plot[,c(5,10,15,20,25)] %>% 
  select(mp=1,exp=2,hs=3,hd=4,hp=5)
write_csv(IRF_TABLE, file = "./result/table/tab_IRF.csv")
