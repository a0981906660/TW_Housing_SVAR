#' @title Create HD plots
rm(list = ls())

source("./code/utility/preamble.R")

df_HD <- readRDS("./data/intermediate_result/df_HD.RDS") %>% 
  mutate(Time = lubridate::yq(Time))

# Make and save plots

make_HD_plot <- function(data,                # the dataframe of HD
                         shock,               # string; the shock of interest
                         legend_shock,        # string; the shock displayed 
                         legend_baseline,     # string; the bias displayed
                         title_text           # string
                         ) {
  fig <- data %>%
    ggplot()+
    geom_line(aes(x = Time, y = !!sym(shock), color = legend_shock), linetype = "dashed")+
    geom_line(aes(x = Time, y = BaseLine, color = legend_baseline))+
    labs(x = '',
         y = '',
         title = title_text)+
    Text_Size_Theme+
    scale_color_manual(values=c('royalblue','red'))+
    theme(legend.position="bottom", 
          legend.direction="vertical",
          legend.title = element_blank())
  return(fig)
}

make_HD_plot(df_HD, 
             shock = "mp", 
             legend_shock = "Monetary Policy Shock",
             legend_baseline = "dLHP Deviations from Base Projection",
             title_text = "Historical Decomposition of dLhp: Monetary Policy Shock")

# plot 1
figure_HD.1 <- df_HD %>%
  ggplot()+
  geom_line(aes(x = Time, y = mp, color = "Monetary Policy Shock"), linetype = "dashed")+
  geom_line(aes(x = Time, y = BaseLine, color = "dLHP Deviations from Base Projection"))+
  labs(x = '',
       y = '',
       title = 'Historical Decomposition of dLhp: Monetary Policy Shock')+
  Text_Size_Theme+
  scale_color_manual(values=c('royalblue','red'))+
  theme(legend.position="bottom", 
        legend.direction="vertical",
        legend.title = element_blank())

# plot 2
figure_HD.2 <- df_HD %>%
  ggplot()+
  geom_line(aes(x = xlab, y = exp, color = "Housing Expectation Shock"), linetype = "dashed")+
  geom_line(aes(x = xlab, y = BaseLine, color = "dLHP Deviations from Base Projection"))+
  labs(x = '',
       y = '',
       title = 'Historical Decomposition of dLhp: Housing Expectation Shock')+
  Text_Size_Theme+
  scale_color_manual(values=c('royalblue','red'))+
  theme(legend.position="bottom", 
        legend.direction="vertical",
        legend.title = element_blank())

# plot 3
figure_HD.3 <- df_HD %>%
  ggplot()+
  geom_line(aes(x = xlab, y = hs, color = "Housing Supply Shock"), linetype = "dashed")+
  geom_line(aes(x = xlab, y = BaseLine, color = "dLHP Deviations from Base Projection"))+
  labs(x = '',
       y = '',
       title = 'Historical Decomposition of dLhp: Housing Supply Shock')+
  Text_Size_Theme+
  scale_color_manual(values=c('royalblue','red'))+
  theme(legend.position="bottom", 
        legend.direction="vertical",
        legend.title = element_blank())

# plot 4
figure_HD.4 <- df_HD %>%
  ggplot()+
  geom_line(aes(x = xlab, y = hd, color = "Housing Demand Shock"), linetype = "dashed")+
  geom_line(aes(x = xlab, y = BaseLine, color = "dLHP Deviations from Base Projection"))+
  labs(x = '',
       y = '',
       title = 'Historical Decomposition of dLhp: Housing Demand Shock')+
  Text_Size_Theme+
  scale_color_manual(values=c('royalblue','red'))+
  theme(legend.position="bottom", 
        legend.direction="vertical",
        legend.title = element_blank())



# plot 5
figure_HD.5 <- df_HD %>%
  ggplot()+
  geom_line(aes(x = xlab, y = sp, color = "Residual Shock"), linetype = "dashed")+
  geom_line(aes(x = xlab, y = BaseLine, color = "dLHP Deviations from Base Projection"))+
  labs(x = '',
       y = '',
       title = 'Historical Decomposition of dLhp: Residual Shock')+
  Text_Size_Theme+
  scale_color_manual(values=c('royalblue','red'))+
  theme(legend.position="bottom", 
        legend.direction="vertical",
        legend.title = element_blank())


# save shock 1
ggsave(filename = "result/figure/0219_m1_HD_shock1.png", 
       plot = figure_HD.1,
       width = 15, height = 10, units = "cm",
       device = "png")

# save shock 2
ggsave(filename = "result/figure/0219_m1_HD_shock2.png", 
       plot = figure_HD.2,
       width = 15, height = 10, units = "cm",
       device = "png")

# save shock 3
ggsave(filename = "result/figure/0219_m1_HD_shock3.png", 
       plot = figure_HD.3,
       width = 15, height = 10, units = "cm",
       device = "png")

# save shock 4
ggsave(filename = "result/figure/0219_m1_HD_shock4.png", 
       plot = figure_HD.4,
       width = 15, height = 10, units = "cm",
       device = "png")
# save shock 5
ggsave(filename = "result/figure/0219_m1_HD_shock5.png", 
       plot = figure_HD.5,
       width = 15, height = 10, units = "cm",
       device = "png")



# For hp
multiplot(figure_HD.1,figure_HD.2,figure_HD.3,figure_HD.4,figure_HD.5,
          cols = 2)
ggsave(filename = "result/figure/HD.png", 
       plot = multiplot(figure_HD.1,
                        figure_HD.2,
                        figure_HD.3,
                        figure_HD.4,
                        figure_HD.5,
                        cols = 2),
       width = 15*2, height = 10*2, units = "cm",
       device = "png")



# ALL
figure_HD.6 <- df_HD %>%
  ggplot()+
  geom_line(aes(x = xlab, y = BaseLine, color = "dLHP Deviations from Base Projection"))+
  geom_line(aes(x = xlab, y = mp, color = "Monetary Policy Shock"), linetype = "dashed")+
  geom_line(aes(x = xlab, y = exp, color = "Housing Expectation Shock"), linetype = "dashed")+
  geom_line(aes(x = xlab, y = hs, color = "Housing Supply Shock"), linetype = "dashed")+
  geom_line(aes(x = xlab, y = hd, color = "Housing Demand Shock"), linetype = "dashed")+
  geom_line(aes(x = xlab, y = sp, color = "Residual Shock"), linetype = "dashed")+
  labs(x = '',
       y = '',
       title = 'Historical Decomposition of dLhp: All Shocks')+
  Text_Size_Theme+
  scale_color_manual(values=c('royalblue','red', 'green', 'yellow', 'lightgreen', 'lightblue'))+
  theme(legend.position="bottom", 
        legend.direction="vertical",
        legend.title = element_blank())
# save shock 6
ggsave(filename = "result/figure/0219_m1_HD_shock6.png", 
       plot = figure_HD.6,
       width = 15, height = 10, units = "cm",
       device = "png")
figure_HD.6



# ALL
figure_HD.6 <- df_HD %>%
  ggplot()+
  geom_line(aes(x = xlab, y = BaseLine, color = "dLHP Deviations from Base Projection"))+
  geom_line(aes(x = xlab, y = mp, color = "Monetary Policy Shock"), linetype = "dashed")+
  geom_line(aes(x = xlab, y = exp, color = "Housing Expectation Shock"), linetype = "dashed")+
  #     geom_line(aes(x = xlab, y = sp, color = "Residual Shock"), linetype = "dashed")+
  labs(x = '',
       y = '',
       title = 'Historical Decomposition of dLhp: Monetary Policy and Expectation Shocks')+
  Text_Size_Theme+
  scale_color_manual(values=c('royalblue', 'darkgreen', 'red'))+
  theme(legend.position="bottom", 
        legend.direction="vertical",
        legend.title = element_blank())
# save shock 6
ggsave(filename = "result/figure/0219_m1_HD_shock6.png", 
       plot = figure_HD.6,
       width = 20, height = 15, units = "cm",
       device = "png")
figure_HD.6