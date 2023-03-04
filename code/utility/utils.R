#' This script provides self-defined functions and imports dependencies

#' Dependencies
library(plyr)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
require(matrixcalc)
# library(Rmisc)
library(lattice)
library(cowplot)
library(xtable)
library(Rmisc)

source("./code/utility/preamble.R")

#' @section Self-defined functions

#' @description create multiple time series plot by selecting columns from a dataframe
create_multiple_time_series <- function(data,         # a tibble
                                        x_axis,       # string; a column in `data` representing timestamps
                                        columns = ... # vector of string; time series data to plot
                                        ) {
  # a container to store tmep figures
  fig_lst <- list()
  for (column in columns) {
    fig_temp <- ggplot(data, aes(x = !!sym(x_axis), y = !!sym(column)))+
      geom_line()  
    fig_lst <- append(fig_lst, list(fig_temp))
  }
  return(fig_lst)
}

# # simple unit tests
# res <- create_multiple_time_series(data, "Date", c("R", "Sent"))
# res <- create_multiple_time_series(data, "Date", c("R"))
# res <- create_multiple_time_series(data, "Date", "R")
# print(res[[1]])
# print(res[[2]])
# fig_list <- create_multiple_time_series(data, "Date", 
#                                    c("R", "Sent", "Permit_TW1", "Loan3", "hp_tw"))
# print(fig_list[[5]])

#' @description make multiple plots
make_multiple_plots <- function(figure_list,
                                ncol = 2,
                                nrow = 3) {
  res <- cowplot::plot_grid(plotlist = figure_list, 
                            align = "vh",
                            axis = "l",
                            # rel_heights = c(1, 1),
                            byrow = T,
                            ncol = ncol,
                            nrow = nrow)
  return(res)
}

# # simple unit tests
# fig <- make_multiple_plots(fig_list)
# ggsave(filename = "./result/figure/raw.pdf",
#        plot = fig,
#        width = 30, height = 20, units = "cm",
#        device = "pdf")

#' @description Save plots with default settings
ggsave_default <- function(figure, path) {
  ggsave(filename = path,
         plot = figure,
         width = 30, height = 20, units = "cm",
         device = "pdf") 
}


#' @description Create multiplot for IRF
create_multiple_IRF_plot <- function(data,          # a tibble
                                     horizon,       # numeric
                                     columns = ..., # vector of string; time series data to plot
                                     xlab = "",     # label for x-axis
                                     ylab = ""      # label for y-axis
                                     ) {
  # a container to store tmep figures
  fig_lst <- list()
  for (column in columns) {
    fig_temp <- ggplot(data, aes(x = (1:horizon), y = !!sym(column)))+
      geom_line()+
      labs(x = xlab, y = ylab)+
      Text_Size_Theme
    fig_lst <- append(fig_lst, list(fig_temp))
  }
  return(fig_lst)
}

# # simple unit test
# fig_list <- create_multiple_IRF_plot(df_IRF_plot, 20, c("V1", "V2"))
# fig_list[[1]]
# fig_list[[2]]



