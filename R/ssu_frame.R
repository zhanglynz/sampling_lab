# Created date: 10 August
# Description: This is to create ssu frame

library(tidyverse)

# read psu frame in -------------------------------------------------------
psu_frame <- readRDS(file = "./data/psu_frame.rds")

# create ssu frame --------------------------------------------------------
set.seed(20190810)

N <- 50000

ssu_frame <- 
  data.frame(ssu_ID = sprintf("I%05d", 1:N)) %>% 
  mutate(ssu_ID = as.character(ssu_ID)) %>% 
  mutate(psu_ID = sample(psu_frame$psu_ID, N, replace = TRUE),
         age_group = sample(c('1', '2', '3'), N, replace = TRUE))

# save to rds -------------------------------------------------------------
saveRDS(ssu_frame, file = "./data/ssu_frame.rds")

