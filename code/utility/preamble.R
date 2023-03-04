library(ggplot2)

if (!exists("hrz")) { hrz <- 19 }
if (!exists("shock_sign")) { shock_sign <- -1 }

Text_Size_Theme <- theme(
  axis.title.x = element_text(size = 12),
  axis.text.x = element_text(size = 12),
  axis.title.y = element_text(size = 12),
  axis.title = element_text(size = 12),
  plot.title = element_text(size=12)
  )
