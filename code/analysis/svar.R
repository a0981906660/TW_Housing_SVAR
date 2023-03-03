#' @title Structural VAR Analysis
#' The script performs the estimation of a SVAR with recursive identification (World Ordering)
#' 

rm(list = ls())

#' Dependencies
source("./code/utility/utils.R")

# import VAR estimation functions
source("./code/utility/var_functions.R")

#' @section Preamble
inv_tol = 1e-20 #求反矩陣時做數值運算允許的最小誤差(避免singular matrix)
options(warn=-1)    # 關掉warning
options(scipen=999) #不要科學記號

#' @section Load data
path_data = "./data/cleaned_data/df.csv"
data <- read_csv(file = path_data)

#' Preprocessing
data <- data %>% 
    mutate(Date = as.Date(Date))

#' @section Prepare matrix form data of a 4-variable model
# By <- data %>% 
#   select(R, Sent, Permit_TW1, Loan3, hp_tw) %>% 
#   as.matrix

#' Prepare data.frame type data for further usage such as making plots
raw_level_R <- ggplot(data, aes(x = Date, y = R))+
  geom_line()
raw_level_Sent <- ggplot(data, aes(x = Date, y = Sent))+
  geom_line()
raw_level_Permit_TW1 <- ggplot(data, aes(x = Date, y = Permit_TW1))+
  geom_line()
raw_level_Loan3 <- ggplot(data, aes(x = Date, y = Loan3))+
  geom_line()
raw_level_hp_tw <- ggplot(data, aes(x = Date, y = hp_tw))+
  geom_line()

fig.temp <- cowplot::plot_grid(raw_level_R,
                               raw_level_Sent,
                               raw_level_Permit_TW1,
                               raw_level_Loan3,
                               raw_level_hp_tw,
                               ncol = 2)

ggsave(filename = "./result/figure/raw.pdf", 
       plot = fig.temp,
       width = 30, height = 20, units = "cm",
       device = "pdf")

ggsave(filename = "./result/figure/raw.png", 
       plot = fig.temp,
       width = 30, height = 20, units = "cm",
       device = "png")


#' @section Adjust scales
By <- data %>% select(R, Sent, Permit_TW1, Loan3, hp_tw) %>%
  mutate(LPermit_TW1 = log(Permit_TW1),
         dLloan = c(rep(NA, 4), 100*diff(log(Loan3), 4)),
         dLhp = c(rep(NA,4), 100*diff(log(hp_tw), 4))) %>%
  select(R, Sent, LPermit = LPermit_TW1, dLloan, dLhp) %>% 
  drop_na() %>%
  as.matrix


data_new = data %>% select(Date, R, Sent, Permit_TW1, Loan3, hp_tw) %>%
  mutate(LPermit_TW1 = log(Permit_TW1),
         dLloan = c(rep(NA, 4), 100*diff(log(Loan3), 4)),
         dLhp = c(rep(NA,4), 100*diff(log(hp_tw), 4))) %>%
  select(Date, R, Sent, LPermit = LPermit_TW1, dLloan, dLhp)

raw_level_R <- ggplot(data_new, aes(x = Date, y = R))+
  geom_line()

raw_level_Sent <- ggplot(data_new, aes(x = Date, y = Sent))+
  geom_line()

raw_level_LPermit <- ggplot(data_new, aes(x = Date, y = LPermit))+
  geom_line()

raw_level_dLloan <- ggplot(data_new, aes(x = Date, y = dLloan))+
  geom_line()

raw_level_dLhp <- ggplot(data_new, aes(x = Date, y = dLhp))+
  geom_line()

Rmisc::multiplot(raw_level_R, raw_level_Sent,
          raw_level_LPermit, raw_level_dLloan,
          raw_level_dLhp,
          cols = 2)



