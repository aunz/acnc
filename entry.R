# set working dir to this script
setwd(getSrcDirectory(function () {}))

# libraries
sapply(setdiff(c(
  'data.table',
  'ggplot2',
  'plotly',
  'crosstalk',
  'dplyr'
), installed.packages()), install.packages)
# if (!'gganimate' %in% installed.packages()) devtools::install_github('thomasp85/gganimate')

library(data.table)
library(ggplot2)
library(plotly)
library(crosstalk)
library(dplyr)
# library(gganimate)

dt1 = fread('./data/data.csv')

