rm(list = ls())

# Load graph setting
source("./code/utility/preamble.R")
source("./code/utility/utils.R")

# Load data
df_IRF_plot <- readRDS("./data/intermediate_result/df_IRF_plot.RDS")
df_IRF.sim <- read_rds("./data/intermediate_result/df_IRF.sim_hrz20.rds")

# 畫IRF & Bootstrap C.I.
df_IRF_plot.BS.L <- matrix(NA, nrow = hrz+1, ncol = 25)
df_IRF_plot.BS.U <- matrix(NA, nrow = hrz+1, ncol = 25)
df_IRF_plot.BS.Median <- matrix(NA, nrow = hrz+1, ncol = 25)
df_IRF_plot.BS.Mean <- matrix(NA, nrow = hrz+1, ncol = 25)
for(col in 1:25){
  for(row in 1:(hrz+1) ){
    df_IRF_plot.BS.L[row,col] <- quantile(df_IRF.sim[row,col,], probs = 0.025)
    df_IRF_plot.BS.U[row,col] <- quantile(df_IRF.sim[row,col,], probs = 0.975)
    df_IRF_plot.BS.Median[row,col] <- quantile(df_IRF.sim[row,col,], probs = 0.5)
    df_IRF_plot.BS.Mean[row,col] <- mean(df_IRF.sim[row,col,])
  }
}

df_IRF_plot.BS.L <- df_IRF_plot.BS.L %>% as_tibble()
df_IRF_plot.BS.U <- df_IRF_plot.BS.U %>% as_tibble()
df_IRF_plot.BS.Median <- df_IRF_plot.BS.Median %>% as_tibble()
df_IRF_plot.BS.Mean <- df_IRF_plot.BS.Mean %>% as_tibble()

# the combination of titles
title_list_response <- c("Interest Rate", 
                         "Sentiment", 
                         "LPermit", 
                         "dLloan", 
                         "Housing Price")
title_list_shock <- c("Monetary Policy", 
                      "Housing Expectation", 
                      "Housing Supply", 
                      "Housing Demand", 
                      "Residual")
title_list <- c(paste0("Reponse of ", title_list_response, " to ", title_list_shock[1], " Shock"),
                paste0("Reponse of ", title_list_response, " to ", title_list_shock[2], " Shock"),
                paste0("Reponse of ", title_list_response, " to ", title_list_shock[3], " Shock"),
                paste0("Reponse of ", title_list_response, " to ", title_list_shock[4], " Shock"),
                paste0("Reponse of ", title_list_response, " to ", title_list_shock[5], " Shock")
                )

# making figures
ind <- 0
for(i in 1:5){
  for(j in 1:5){
    ind <- ind+1
    nam <- paste("shock", j, "y", i, sep = '')
    assign(nam, bind_cols(df_IRF_plot.BS.L[ind], df_IRF_plot.BS.U[ind],
                          df_IRF_plot.BS.Median[ind], df_IRF_plot.BS.Mean[ind],
                          df_IRF_plot[ind]))
    # 改名
    evalStr <- paste0("colnames(", nam, ") <- c('Lower', 'Upper', 'Median', 'Mean', 'Actual')")
    eval(parse(text=evalStr))
    # 圖層
    evalStr <- paste0("p", ind, " <- ", "ggplot(",nam,")",
                      " + geom_hline(yintercept=0, color = 'grey')",
                      " + geom_line(aes(x = 1:nrow(", nam, "), y = Lower), linetype = 'dashed', col='red')",
                      " + geom_line(aes(x = 1:nrow(", nam, "), y = Upper), linetype = 'dashed', col='red')",
                      " + geom_line(aes(x = 1:nrow(", nam, "), y = Median), col = 'Blue')"
                      )
    eval(parse(text=evalStr))
    
    # assign x and y labels, and theme, title
    evalStr <- paste0("p", ind, " <- ", "p", ind, 
                      " + Text_Size_Theme",
                      " + labs(x = 'Time (Season)',
                               y = '',
                               title = title_list[", ind, "])"
                      )
    eval(parse(text=evalStr))
  }
}


## shock1: mp
fig_shock1 <- make_multiple_plots(list(p1,p2,p3,p4,p5), ncol = 2)

## shock2: exp
fig_shock2 <- make_multiple_plots(list(p6,p7,p8,p9,p10), ncol = 2)

## shock3: supply
fig_shock3 <- make_multiple_plots(list(p11,p12,p13,p14,p15), ncol = 2)

## shock4: demand
fig_shock4 <- make_multiple_plots(list(p16,p17,p18,p19,p20), ncol = 2)

## shock5: residual
fig_shock5 <- make_multiple_plots(list(p21,p22,p23,p24,p25), ncol = 2)

# save plot
ggsave_default(fig_shock1, "./result/figure/fig_IRF_shock1.pdf")
ggsave_default(fig_shock2, "./result/figure/fig_IRF_shock2.pdf")
ggsave_default(fig_shock3, "./result/figure/fig_IRF_shock3.pdf")
ggsave_default(fig_shock4, "./result/figure/fig_IRF_shock4.pdf")
ggsave_default(fig_shock5, "./result/figure/fig_IRF_shock5.pdf")


# With shock sign
cat(">>> shock sign is: ", shock_sign, "\n")

df_IRF_plot.BS.L_negative <- (df_IRF_plot.BS.L*shock_sign) %>% as_tibble()
df_IRF_plot.BS.U_negative <- (df_IRF_plot.BS.U*shock_sign) %>% as_tibble()
df_IRF_plot.BS.Median_negative <- (df_IRF_plot.BS.Median*shock_sign) %>% as_tibble()
df_IRF_plot.BS.Mean_negative <- (df_IRF_plot.BS.Mean*shock_sign) %>% as_tibble()
df_IRF_plot_negative <- (df_IRF_plot*shock_sign) %>% as_tibble()

