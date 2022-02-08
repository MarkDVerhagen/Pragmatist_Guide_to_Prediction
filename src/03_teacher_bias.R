### 02_data_teacher_bias.R
## Author: Mark Verhagen
##
## Description: Code to wrangle simulation results for the
## teacher bias example
##
## Data inputs:
## - data/teacher_bias/performance_simulation.dta
##
## Data outputs:
## - data/teacher_bias/teacher_bias_perf.rds
###

library(tidyverse)
source("src/functions.R")

results_df <- readRDS("data/teacher_bias/performance_simulation.rds")

perf <- delist_perf(results_df)

perf %>%
       saveRDS("./data/teacher_bias/teacher_bias_perf.rds")