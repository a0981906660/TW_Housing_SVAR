#' @section Historical Decomposition

source("./code/analysis/svar.R")

if(hrz<nrow(By)){
  SVAR_AB_IRF <- VAR.svarirf.AB(By, VAR.P, Amat, Bmat, h = nrow(By), CONST, SVAR_AB_est = SVAR_AB_est)
}

SVAR_AB_HistDecomp <- VAR.svarhist.AB(By, VAR.P, Amat, Bmat, CONST)
dim(SVAR_AB_HistDecomp)

#----- Base Project 估計 -----#
SVAR_AB_Hist.c0 = VAR.baseproject(By, VAR.P, CONST)
head(SVAR_AB_Hist.c0)
dim(SVAR_AB_Hist.c0)
dim(By)

# 實際時間序列與基本預測時間序列之偏離值
head(By-SVAR_AB_Hist.c0, 10)
# 前幾個（VAR.P個，即lag幾期）會是0

# 只有特定衝擊下的時間序列與基本預測時間序列之偏離值
## shock1: monetary policy shock
head(SVAR_AB_HistDecomp[,c(1,6,11,16,21)], 10)


##左：baseline上 hp 的變化；右：shock1 對 hp 的衝擊
tail(cbind((By-SVAR_AB_Hist.c0)[,5], 
           (SVAR_AB_HistDecomp[,c(1,6,11,16,21)])[,5]))

# 對「房價」的歷史分解：monetary policy shock對房價的解釋力
df_HD_plot <- bind_cols((By-SVAR_AB_Hist.c0)[,5],
                        (SVAR_AB_HistDecomp[,c(1,6,11,16,21)])[,5])
colnames(df_HD_plot) <- c("BaseLine", "Shock1")
ggplot(df_HD_plot)+
  geom_line(aes(x = 1:nrow(df_HD_plot), y = Shock1), col = 'red', linetype = "dashed")+
  geom_line(aes(x = 1:nrow(df_HD_plot), y = BaseLine), col = 'royalblue')

# 對「房價」的歷史分解：expectation shock對房價的解釋力
df_HD_plot <- bind_cols((By-SVAR_AB_Hist.c0)[,5],
                        (SVAR_AB_HistDecomp[,c(2,7,12,17,22)])[,5])
colnames(df_HD_plot) <- c("BaseLine", "Shock2")
ggplot(df_HD_plot)+
  geom_line(aes(x = 1:nrow(df_HD_plot), y = Shock2), col = 'red', linetype = "dashed")+
  geom_line(aes(x = 1:nrow(df_HD_plot), y = BaseLine), col = 'royalblue')

# 對「房價」的歷史分解：supply shock對房價的解釋力
df_HD_plot <- bind_cols((By-SVAR_AB_Hist.c0)[,5],
                        (SVAR_AB_HistDecomp[,c(3,8,13,18,23)])[,5])
colnames(df_HD_plot) <- c("BaseLine", "Shock3")
ggplot(df_HD_plot)+
  geom_line(aes(x = 1:nrow(df_HD_plot), y = Shock3), col = 'red', linetype = "dashed")+
  geom_line(aes(x = 1:nrow(df_HD_plot), y = BaseLine), col = 'royalblue')


# 對「房價」的歷史分解：housing demand shock對房價的解釋力
df_HD_plot <- bind_cols((By-SVAR_AB_Hist.c0)[,5],
                        (SVAR_AB_HistDecomp[,c(4,9,14,19,24)])[,5])
colnames(df_HD_plot) <- c("BaseLine", "Shock4")
ggplot(df_HD_plot)+
  geom_line(aes(x = 1:nrow(df_HD_plot), y = Shock4), col = 'red', linetype = "dashed")+
  geom_line(aes(x = 1:nrow(df_HD_plot), y = BaseLine), col = 'royalblue')


# 對「房價」的歷史分解：Residual shock對房價的解釋力
df_HD_plot <- bind_cols((By-SVAR_AB_Hist.c0)[,5],
                        (SVAR_AB_HistDecomp[,c(5,10,15,20,25)])[,5])
colnames(df_HD_plot) <- c("BaseLine", "Shock4")
ggplot(df_HD_plot)+
  geom_line(aes(x = 1:nrow(df_HD_plot), y = Shock4), col = 'red', linetype = "dashed")+
  geom_line(aes(x = 1:nrow(df_HD_plot), y = BaseLine), col = 'royalblue')


# making table
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

df_HD <- bind_cols(t_label,
                   year_label,
                   season_label,
                   (By-SVAR_AB_Hist.c0)[,5],
                   SVAR_AB_HistDecomp[,21],
                   SVAR_AB_HistDecomp[,22],
                   SVAR_AB_HistDecomp[,23],
                   SVAR_AB_HistDecomp[,24],
                   SVAR_AB_HistDecomp[,25])
colnames(df_HD) <- c("Time", "Year", "Season", "BaseLine", "mp", "exp", "hs", "hd", "sp")
tail(df_HD)


df_HD.table <- df_HD %>% 
  summarise(Time = Time,
            Year = Year,
            Season = Season,
            mp = mp/BaseLine*100,
            exp = exp/BaseLine*100,
            hs = hs/BaseLine*100,
            hd = hd/BaseLine*100,
            sp = sp/BaseLine*100) %>%
  drop_na()



get_HD.table <- function(df_HD.table, 
                         year_start, season_start,
                         year_end, season_end){
  HD_seq.temp <- df_HD.table %>%
    filter( Year >= year_start & Year <= year_end) %>%
    filter( !(Year==year_start & Season < season_start) ) %>%
    filter( !(Year==year_end & Season > season_end) ) %>%
    summarise(mp = median(mp),
              exp = median(exp),
              hs = median(hs),
              hd = median(hd),
              sp = median(sp))
  return(HD_seq.temp)
}


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
HD.table


# 生出table
tab_HD <- xtable(HD.table, caption= "歷史分解下各衝擊的解釋力比率(%)", align=c("c","c","c","c","c","c", "c"))
print(tab_HD, include.rownames=FALSE)

print(tab_HD, include.rownames=FALSE,
      file="result/table/HD_0219_m1.tex",
      append=T, table.placement = "h",
      caption.placement="bottom", hline.after=seq(from=-1,to=nrow(tab_HD),by=1))


# Save Plots
xlab <- lubridate::yq(df_HD$Time)

# plot 1
figure_HD.1 <- df_HD %>%
  ggplot()+
  geom_line(aes(x = xlab, y = mp, color = "Monetary Policy Shock"), linetype = "dashed")+
  geom_line(aes(x = xlab, y = BaseLine, color = "dLHP Deviations from Base Projection"))+
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