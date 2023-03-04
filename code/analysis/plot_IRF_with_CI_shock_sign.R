# rm(list = ls())
# 
# # Load graph setting
# source("./code/utility/preamble.R")
# source("./code/utility/utils.R")
# 
# # Load data
# df_IRF_plot <- readRDS("./data/intermediate_result/df_IRF_plot.RDS")
# df_IRF.sim <- read_rds("./data/intermediate_result/df_IRF.sim_hrz20.rds")
# 
# 
# 
# df_IRF_plot.BS.L_negative <- (df_IRF_plot.BS.L*shock_sign) %>% as_tibble()
# df_IRF_plot.BS.U_negative <- (df_IRF_plot.BS.U*shock_sign) %>% as_tibble()
# df_IRF_plot.BS.Median_negative <- (df_IRF_plot.BS.Median*shock_sign) %>% as_tibble()
# df_IRF_plot.BS.Mean_negative <- (df_IRF_plot.BS.Mean*shock_sign) %>% as_tibble()
# df_IRF_plot_negative <- (df_IRF_plot*shock_sign) %>% as_tibble()
# 
# ind <- 0
# for(i in 1:5){
#   for(j in 1:5){
#     ind <- ind+1
#     nam <- paste("shock", j, "y", i,"_negative", sep = '')
#     assign(nam, bind_cols(df_IRF_plot.BS.L_negative[ind], df_IRF_plot.BS.U_negative[ind],
#                           df_IRF_plot.BS.Median_negative[ind], df_IRF_plot.BS.Mean_negative[ind],
#                           df_IRF_plot_negative[ind]))
#     # 改名
#     evalStr <- paste0("colnames(", nam, ") <- c('Lower', 'Upper', 'Median', 'Mean', 'Actual')")
#     eval(parse(text=evalStr))
#     # 圖層
#     evalStr <- paste0("p", ind, " <- ", "ggplot(",nam,") +geom_hline(yintercept=0, color = 'grey')+ geom_line(aes(x = 1:nrow(", nam, "), y = Lower), linetype = 'dashed', col='red')+geom_line(aes(x = 1:nrow(", nam, "), y = Upper), linetype = 'dashed', col='red')+geom_line(aes(x = 1:nrow(", nam, "), y = Median), col = 'Blue')")
#     eval(parse(text=evalStr))
#   }
# }
# 
# 
# 
# 
# ## shock1: mp
# p1 <- p1+labs(x = 'Time (Season)',
#               y = '',
#               title = 'Response of Interest Rate to Monetary Policy Shock')+Text_Size_Theme
# p2 <- p2+labs(x = 'Time (Season)',
#               y = '',
#               title = 'Response of Sentiment to Monetary Policy Shock')+Text_Size_Theme
# p3 <- p3+labs(x = 'Time (Season)',
#               y = '',
#               title = 'Response of LPermit to Monetary Policy Shock')+Text_Size_Theme
# p4 <- p4+labs(x = 'Time (Season)',
#               y = '',
#               title = 'Response of dLloan to Monetary Policy Shock')+Text_Size_Theme
# p5 <- p5+labs(x = 'Time (Season)',
#               y = '',
#               title = 'Response of Housing Price to Monetary Policy Shock')+Text_Size_Theme
# 
# ## shock2: exp
# p6 <- p6+labs(x = 'Time (Season)',
#               y = '',
#               title = 'Response of Interest Rate to Housing Expectation Shock')+Text_Size_Theme
# p7 <- p7+labs(x = 'Time (Season)',
#               y = '',
#               title = 'Response of Sentiment to Housing Expectation Shock')+Text_Size_Theme
# p8 <- p8+labs(x = 'Time (Season)',
#               y = '',
#               title = 'Response of LPermit to Housing Expectation Shock')+Text_Size_Theme
# p9 <- p9+labs(x = 'Time (Season)',
#               y = '',
#               title = 'Response of dLloan to Housing Expectation Shock')+Text_Size_Theme
# p10 <- p10+labs(x = 'Time (Season)',
#                 y = '',
#                 title = 'Response of Housing Price to Housing Expectation Shock')+Text_Size_Theme
# 
# ## shock3: supply
# p11 <- p11+labs(x = 'Time (Season)',
#                 y = '',
#                 title = 'Response of Interest Rate to Housing Supply Shock')+Text_Size_Theme
# p12 <- p12+labs(x = 'Time (Season)',
#                 y = '',
#                 title = 'Response of Sentiment to Housing Supply Shock')+Text_Size_Theme
# p13 <- p13+labs(x = 'Time (Season)',
#                 y = '',
#                 title = 'Response of LPermit to Housing Supply Shock')+Text_Size_Theme
# p14 <- p14+labs(x = 'Time (Season)',
#                 y = '',
#                 title = 'Response of dLloan to Housing Supply Shock')+Text_Size_Theme
# p15 <- p15+labs(x = 'Time (Season)',
#                 y = '',
#                 title = 'Response of Housing Price to Housing Supply Shock')+Text_Size_Theme
# 
# 
# 
# 
# ## shock4: demand
# p16 <- p16+labs(x = 'Time (Season)',
#                 y = '',
#                 title = 'Response of Interest Rate to Housing Demand Shock')+Text_Size_Theme
# p17 <- p17+labs(x = 'Time (Season)',
#                 y = '',
#                 title = 'Response of Sentiment to Housing Demand Shock')+Text_Size_Theme
# p18 <- p18+labs(x = 'Time (Season)',
#                 y = '',
#                 title = 'Response of LPermit to Housing Demand Shock')+Text_Size_Theme
# p19 <- p19+labs(x = 'Time (Season)',
#                 y = '',
#                 title = 'Response of dLloan to Housing Demand Shock')+Text_Size_Theme
# p20 <- p20+labs(x = 'Time (Season)',
#                 y = '',
#                 title = 'Response of Housing Price to Housing Demand Shock')+Text_Size_Theme
# 
# 
# 
# ## shock5: sp
# p21 <- p21+labs(x = 'Time (Season)',
#                 y = '',
#                 title = 'Response of Interest Rate to Residual Shock')+Text_Size_Theme
# p22 <- p22+labs(x = 'Time (Season)',
#                 y = '',
#                 title = 'Response of Sentiment to Residual Shock')+Text_Size_Theme
# p23 <- p23+labs(x = 'Time (Season)',
#                 y = '',
#                 title = 'Response of LPermit to Residual Shock')+Text_Size_Theme
# p24 <- p24+labs(x = 'Time (Season)',
#                 y = '',
#                 title = 'Response of dLloan to Residual Shock')+Text_Size_Theme
# p25 <- p25+labs(x = 'Time (Season)',
#                 y = '',
#                 title = 'Response of Housing Price to Residual Shock')+Text_Size_Theme
# 
# 
# multiplot(p1,p2,p3,p4,p5,
#           p6,p7,p8,p9,p10,
#           p11,p12,p13,p14,p15,
#           p16,p17,p18,p19,p20,
#           p21,p22,p23,p24,p25,
#           cols = 5)
# 
# # For shock 1
# multiplot(p1,p2,p3,p4,p5,
#           cols = 2)
# # For shock 2
# multiplot(p6,p7,p8,p9,p10,
#           cols = 2)
# # For shock 3
# multiplot(p11,p12,p13,p14,p15,
#           cols = 2)
# # For shock 4
# multiplot(p16,p17,p18,p19,p20,
#           cols = 2)
# # For shock 5
# multiplot(p21,p22,p23,p24,p25,
#           cols = 2)
# 
# 
# 
# 
# # save plot
# # shock 1
# ggsave(filename = "result/figure/0219_m1_IRF_shock1_negative.png", 
#        plot = multiplot(p1,p2,p3,p4,p5, cols = 2),
#        width = 30, height = 20, units = "cm",
#        device = "png")
# 
# # shock 2
# ggsave(filename = "result/figure/0219_m1_IRF_shock2_negative.png", 
#        plot = multiplot(p6,p7,p8,p9,p10, cols = 2),
#        width = 30, height = 20, units = "cm",
#        device = "png")
# 
# # shock 3
# ggsave(filename = "result/figure/0219_m1_IRF_shock3_negative.png", 
#        plot = multiplot(p11,p12,p13,p14,p15, cols = 2),
#        width = 30, height = 20, units = "cm",
#        device = "png")
# 
# # shock 4
# ggsave(filename = "result/figure/0219_m1_IRF_shock4_negative.png",
#        plot = multiplot(p16,p17,p18,p19,p20, cols = 2),
#        width = 30, height = 20, units = "cm",
#        device = "png")
# 
# # shock 5
# ggsave(filename = "result/figure/0219_m1_IRF_shock5_negative.png",
#        plot = multiplot(p21,p22,p23,p24,p25, cols = 2),
#        width = 30, height = 20, units = "cm",
#        device = "png")
# 




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


# With shock sign
cat(">>> shock sign is: ", shock_sign, "\n")

df_IRF_plot.BS.L <- (df_IRF_plot.BS.L*shock_sign) %>% as_tibble()
df_IRF_plot.BS.U <- (df_IRF_plot.BS.U*shock_sign) %>% as_tibble()
df_IRF_plot.BS.Median <- (df_IRF_plot.BS.Median*shock_sign) %>% as_tibble()
df_IRF_plot.BS.Mean <- (df_IRF_plot.BS.Mean*shock_sign) %>% as_tibble()
df_IRF_plot <- (df_IRF_plot*shock_sign) %>% as_tibble()


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
title_list <- c(paste0("Reponse of ", title_list_response, " to Negative ", title_list_shock[1], " Shock"),
                paste0("Reponse of ", title_list_response, " to Negative ", title_list_shock[2], " Shock"),
                paste0("Reponse of ", title_list_response, " to Negative ", title_list_shock[3], " Shock"),
                paste0("Reponse of ", title_list_response, " to Negative ", title_list_shock[4], " Shock"),
                paste0("Reponse of ", title_list_response, " to Negative ", title_list_shock[5], " Shock")
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
ggsave_default(fig_shock1, "./result/figure/fig_IRF_shock1_neg.pdf")
ggsave_default(fig_shock2, "./result/figure/fig_IRF_shock2_neg.pdf")
ggsave_default(fig_shock3, "./result/figure/fig_IRF_shock3_neg.pdf")
ggsave_default(fig_shock4, "./result/figure/fig_IRF_shock4_neg.pdf")
ggsave_default(fig_shock5, "./result/figure/fig_IRF_shock5_neg.pdf")













