# Created date: 9 August 2019
# Description: This is to create a psu frame

library(tidyverse)

set.seed(20190809)

psu_frame_0 <- 
  data.frame(psu_ID = sprintf("Z%04d", 1:1000)) %>% 
  mutate(region = sample(LETTERS, 1000, replace = TRUE),
         UR_RU = sample(c('UR', 'RU'), 1000, replace = TRUE),
         L_H = sample(c('L', 'H'), 1000, replace = TRUE))

strata_0 <- 
  psu_frame_0 %>% 
  count(region, UR_RU, L_H) %>% 
  rename(stratum_size = n) %>% 
  mutate(stratum_ID = sprintf("S%04d", row_number())) %>%
  ungroup() %>% 
  select(region, UR_RU, L_H, stratum_ID, stratum_size)

no_of_strata <- dim(strata_0)[1]

strata_1 <- 
  strata_0 %>% 
  mutate(sampled_psu_size = round(stratum_size * runif(no_of_strata, 0)))

psu_frame <- 
  psu_frame_0 %>% 
  left_join(strata_1, by = c('region', 'UR_RU', 'L_H'))

ssu_frame <- 
  data.frame(ssu_ID = sprintf("I%05d", 1:10000)) %>% 
  mutate(psu_ID = sample(psu_frame$psu_ID, 10000, replace = TRUE),
         age_group = sample(c('1', '2', '3'), 10000, replace = TRUE))


