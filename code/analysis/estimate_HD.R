#' @section Historical Decomposition

rm(list = ls())

source("./code/analysis/svar.R")

# ddTheta's length determines the horizon of HD can be
# so if we want to trace back longer (have a greater length of HD horizon)
# we need to re-estimate ddTheta with full length
if(hrz < nrow(By)){
  SVAR_AB_IRF <- VAR.svarirf.AB(By, VAR.P, Amat, Bmat,    # pre-estimated A, B matrices
                                h = nrow(By),             # a longer horizon
                                CONST, 
                                SVAR_AB_est = SVAR_AB_est # estimated A, B matrices
                                )
}

# estimate historical decomposition
# it is a matrix that stores each shock in the past
# note that the dimension is (available horizon, num_var^2)

SVAR_AB_HistDecomp <- VAR.svarhist.AB(By, VAR.P, Amat, Bmat, CONST)
# check dimenson
# the column of the matrix should be num_var^2
# use assertthat
data_new %>% drop_na() %>% nrow() == dim(SVAR_AB_HistDecomp)[1]
num_var^2 == dim(SVAR_AB_HistDecomp)[2]

#' @example Explanation of the num_var^2 columns
#' We have 5-variable model which is [mp, exp, hs, hd, hp]'
# V1 is the historical mp shocks that affected the interest rate time series
# V5 is the historical mp shocks that affected the house price time series
# V21 is the historical hp shocks that affected the interest rate time series
# V25 is the historical hp shocks that affected the house price time series

#----- Base Project 估計 -----#
# Base projection is a counterfactural time series without any shock
SVAR_AB_Hist.c0 <- VAR.baseproject(By, VAR.P, CONST)
dim(SVAR_AB_Hist.c0) == dim(By)

#' @details 
#' The actual time series (what we observed): By
#' The counterfactual time series without any shocks (base projection): c0
#' The historical shocks: SVAR_AB_HistDecomp
#' The following should hold:
#' By = c0 + (aggregated historical shocks)

# 實際時間序列與基本預測時間序列之偏離值
# This amount of bias is contributed by all the shocks
# e.g. The historical hp time series is deviated from the base projected hp series
#      since there are 5 shocks contributed
#      so we could see how each shock contributes the most by compare the bias and the each shock
head(By-SVAR_AB_Hist.c0, 10)
# It is worth noting that since we chose the lag VAR.P (here is 7), 
# thus the base projection of 1:VAR.P would be exactly the same as the actual time series
# i.e. the bias of 1:VAR.P is 0


#' @section Bias when only turning on specific shocks
# 只有特定衝擊下的時間序列與基本預測時間序列之偏離值

## shock1: monetary policy shock
tail(SVAR_AB_HistDecomp[,c(1,6,11,16,21)], 10)
## 左：baseline上 hp 的變化；
## 右：shock1 對 R, sent, permit, loan, hp 的effects. And pick up the last one
tail(cbind((By-SVAR_AB_Hist.c0)[,5], 
           (SVAR_AB_HistDecomp[,c(1,6,11,16,21)])[,5]))




# Make preliminary plots
# HD for house price -> i = 5
df_HD_plot_list <- list() # store the df for plot
HD_plot_list <- list()    # store the plot itself
for ( j in 1:5) {
  df.temp <- make_HD_plot_dataframe(5, j)
  # prepare df
  df_HD_plot_list <- append(df_HD_plot_list,
                            list(df.temp)
                            )
  # make plot
  plot.temp <- ggplot(df.temp, aes(x = 1:nrow(df.temp))) + 
    geom_line(aes(y = Shock), col = "red", linetype = "dashed") +
    geom_line(aes(y = BaseLine), col = "royalblue") + 
    labs(x = "")
  HD_plot_list <- append(HD_plot_list, list(plot.temp))
}


# making table
# prepare time labels
t_label <- c()
year_label <- c()
season_label <- c()
year <- 1992
for(q in 1:115){
  if(q%%4==0){
    t_label <- c(t_label, paste0(year, "Q4", sep = ""))
    year_label <- c(year_label, year)
    season_label <- c(season_label, 4)
    year <- year+1
  }else{
    t_label <- c(t_label, paste0(year, "Q", q%%4, sep = ""))
    year_label <- c(year_label, year)
    season_label <- c(season_label, q%%4)
  }
}



# i: variable of interest
# j: shock of interest
i <- 5 
df_HD <- bind_cols(
  Time = t_label,
  Year = year_label,
  Season = season_label,
  BaseLine = (By-SVAR_AB_Hist.c0)[,i]
)

shock_labels <- c("mp", "exp", "hs", "hd", "hp")
for (j in 1:5) {
  df_HD <- bind_cols(
    df_HD,
    !!sym(shock_labels[j]) := SVAR_AB_HistDecomp[, num_var*(i-1) + j]
  ) 
}

saveRDS(df_HD, "./data/intermediate_result/df_HD.RDS")

df_HD.table <- df_HD %>% 
  # find "contribution" amount of each shock and normalized to percentage
  mutate(mp = mp/BaseLine*100,
         exp = exp/BaseLine*100,
         hs = hs/BaseLine*100,
         hd = hd/BaseLine*100,
         hp = hp/BaseLine*100) %>%
  select(-BaseLine) %>%
  drop_na()


# all samples
# 全樣本期間：1991Q1-2020Q3
HD_seq1 <- get_HD.table(df_HD.table, 1991, 1, 2020, 3)

# subsample 1: 1991Q1-2003Q3 (持平)
HD_seq2 <- get_HD.table(df_HD.table, 1991, 1, 2003, 3)

# subsample 2: 2003Q3-2006Q4 (上漲)
HD_seq3 <- get_HD.table(df_HD.table, 2003, 3, 2006, 4)

# subsample 3: 2006Q4-2009Q2 (下跌)
HD_seq4 <- get_HD.table(df_HD.table, 2006, 4, 2009, 2)

# subsample 4: 2009Q2-2013Q3 (上漲)
HD_seq5 <- get_HD.table(df_HD.table, 2009, 2, 2013, 3)

# subsample 5: 2013Q3-2016Q2 (下跌)
HD_seq6 <- get_HD.table(df_HD.table, 2013, 3, 2016, 2)

# subsample 6: 2016Q2-2020Q3 (上漲)
HD_seq7 <- get_HD.table(df_HD.table, 2016, 2, 2020, 3)


HD_seq <- bind_rows(HD_seq1, HD_seq2, HD_seq3, HD_seq4, HD_seq5, HD_seq6, HD_seq7)
HD.table <- bind_cols(c("All Samples (1993Q4-2020Q3)",
                        "Subsample 1 (1993Q4-2003Q3)",
                        "Subsample 2 (2003Q3-2006Q4)",
                        "Subsample 3 (2006Q4-2009Q2)",
                        "Subsample 4 (2009Q2-2013Q3)",
                        "Subsample 5 (2013Q3-2016Q2)",
                        "Subsample 6 (2016Q2-2020Q3)"), HD_seq)
colnames(HD.table) <- c("樣本期間", 
                        "Montary Policy Shock", 
                        "Expectation Shock",
                        "Demand Shock",
                        "Supply Shock",
                        "Residual Shock")

# export table
tab_HD <- xtable(HD.table, 
                 caption= "歷史分解下各衝擊的解釋力比率(%)", 
                 align=c("c","c","c","c","c","c", "c"))

print(tab_HD, include.rownames = FALSE,
      file = "result/table/tab_HD_subsamples.tex",
      append=T, table.placement = "h",
      caption.placement = "bottom", 
      hline.after = seq(from = -1,to = nrow(tab_HD), by = 1))

