# Created date: 9 August 2019
# Description: This is to create a psu frame

library(tidyverse)

# create a psu frame ------------------------------------------------------
set.seed(20190809)

psu_frame_0 <- 
  data.frame(psu_ID = sprintf("Z%04d", 1:1000)) %>% 
  mutate(psu_ID = as.character(psu_ID)) %>% 
  mutate(region = sample(LETTERS, 1000, replace = TRUE),
         UR_RU = sample(c('UR', 'RU'), 1000, replace = TRUE),
         L_H = sample(c('L', 'H'), 1000, replace = TRUE))

# save to rds file --------------------------------------------------------
saveRDS(psu_frame_0, "./data/psu_frame.rds")





