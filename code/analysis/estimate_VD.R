#' @section Variance Decomposition

rm(list = ls())

# Prepare necessary matrices: By, Amat, Bmat and SVAR_AB_IRF
source("./code/analysis/svar.R")

# `ddTheta` 放已經估出來的IRF 
# m表示對於第幾個變數的變異數分解（如第五個是對房價的變異數分解）
SVAR_AB_VarDecomp <- VAR.svardecomp.AB(m = 5, By, VAR.P,
                                       AMat, BMat, h=(hrz+1),
                                       Const=TRUE, ddTheta = SVAR_AB_IRF)

# export the raw table
VD_TABLE <- (SVAR_AB_VarDecomp*100) %>% 
  as_tibble %>% 
  select(mp=1, exp=2, hs=3, hd=4, hp=5)
write_csv(VD_TABLE, file = "./result/table/tab_VD.csv")

# export LaTeX table
SVAR_VD <- (SVAR_AB_VarDecomp*100) %>% 
  as_tibble() %>% 
  # select different horizons
  filter(row_number(V1) %in% c(1,2,4,8,12,16,20)) %>%
  mutate(period = c(1,2,4,8,12,16,20)) %>%
  # rename columns and reordering
  select(period = period,
         mp = V1,
         exp = V2,
         hs = V3,
         hd = V4,
         sp = V5) %>%
  # round number
  mutate(period = as.character(period),
         mp = round(mp, digits = 2),
         exp = round(exp, digits = 2),
         hs = round(hs, digits = 2),
         hd = round(hd, digits = 2),
         sp = round(sp, digits = 2))

# convert to latex format
tab_VD <- xtable(SVAR_VD, 
                 caption= "房價指數的變異數分解", 
                 align=c("c","c","c","c","c","c","c"))

print(tab_VD, file="./result/table/tab_VD.tex",
      include.rownames=FALSE,
      append=T, table.placement = "h",
      caption.placement="bottom", 
      hline.after = seq(from = -1,to = nrow(tab_VD), by = 1))
