#' @title Create HD plots
rm(list = ls())

source("./code/utility/preamble.R")
source("./code/utility/utils.R")

df_HD <- readRDS("./data/intermediate_result/df_HD.RDS") %>% 
  mutate(Time = lubridate::yq(Time))

# Make and save plots
fig_HD_params_shocks <- c("mp", "exp", "hs", "hd", "hp")
fig_HD_params_legend_shocks <- c("Monetary Policy Shock", 
                                 "Housing Expectation Shock", 
                                 "Housing Supply Shock", 
                                 "Housing Demand Shock", 
                                 "Residual Shock"
                                 )
fig_HD_params_legend_baseline <- "dLHP Deviations from Base Projection"
fig_HD_params_title <- c("Historical Decomposition of dLhp: Monetary Policy Shock",
                         "Historical Decomposition of dLhp: Housing Expectation Shock",
                         "Historical Decomposition of dLhp: Housing Supply Shock",
                         "Historical Decomposition of dLhp: Housing Demand Shock",
                         "Historical Decomposition of dLhp: Residual Shock"
                         )
# apply the function to make HD plot for each shock
fig_list <- list()
for (i in 1:5) {
  fig <- make_HD_plot(df_HD, 
                      shock = fig_HD_params_shocks[i], 
                      legend_shock = fig_HD_params_legend_shocks[i],
                      legend_baseline = fig_HD_params_legend_baseline,
                      title_text = fig_HD_params_title[i])

  fig_list <- append(fig_list, list(fig))
  ggsave_default(fig, path = paste0("./result/figure/fig_HD_shock_", i, ".pdf"),
                 width = 15, height = 10)
}

# Save HD plots for all shocks once
fig <- make_multiple_plots(fig_list, ncol = 2)
ggsave_default(fig, "./result/figure/fig_HD_all_shocks.pdf",
               width = 30, height = 20)

# ALL shocks displayed at the same time
figure_HD_all_shocks <- df_HD %>%
  ggplot(aes(x = Time))+
  geom_line(aes(y = BaseLine, color = "dLHP Deviations from Base Projection"))+
  geom_line(aes(y = mp, color = "Monetary Policy Shock"), linetype = "dashed")+
  geom_line(aes(y = exp, color = "Housing Expectation Shock"), linetype = "dashed")+
  geom_line(aes(y = hs, color = "Housing Supply Shock"), linetype = "dashed")+
  geom_line(aes(y = hd, color = "Housing Demand Shock"), linetype = "dashed")+
  geom_line(aes(y = hp, color = "Residual Shock"), linetype = "dashed")+
  labs(x = '',
       y = '',
       title = 'Historical Decomposition of dLhp: All Shocks')+
  Text_Size_Theme+
  scale_color_manual(values=c('royalblue','red', 'green', 'yellow', 'lightgreen', 'lightblue'))+
  theme(legend.position="bottom", 
        legend.direction="vertical",
        legend.title = element_blank())
ggsave_default(figure_HD_all_shocks, "./result/figure/fig_HD_all_shocks_once.pdf")


# Only turn on specific shocks: mp, exp
figure_HD_spec_shocks <- df_HD %>%
  ggplot(aes(x = Time))+
  geom_line(aes(y = BaseLine, color = "dLHP Deviations from Base Projection"))+
  geom_line(aes(y = mp, color = "Monetary Policy Shock"), linetype = "dashed")+
  geom_line(aes(y = exp, color = "Housing Expectation Shock"), linetype = "dashed")+
  labs(x = '',
       y = '',
       title = 'Historical Decomposition of dLhp: Monetary Policy and Expectation Shocks')+
  Text_Size_Theme+
  scale_color_manual(values=c('royalblue', 'darkgreen', 'red'))+
  theme(legend.position="bottom", 
        legend.direction="vertical",
        legend.title = element_blank())
ggsave_default(figure_HD_spec_shocks, "result/figure/fig_HD_sepc_shocks.pdf")
