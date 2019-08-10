# Created date: 10 August 2019
# Description: This is to take a sample
#              The sampling scheme is "stratified multiple sampling"

library(tidyverse)

# read psu and ssu frame in -----------------------------------------------
psu_frame <- readRDS(file = "./data/psu_frame.rds")
ssu_frame <- readRDS(file = "./data/ssu_frame.rds")

# create strata -----------------------------------------------------------
strata_0 <- 
  psu_frame %>% 
  group_by(region, UR_RU, L_H) %>% 
  summarise(psu_in_stratum = list(psu_ID),
            stratum_size = n()) %>% 
  mutate(stratum_ID = sprintf("S%04d", row_number())) %>%
  mutate(stratum_ID = as.character(stratum_ID)) %>% 
  ungroup() 

temp_df <- 
  strata_0 %>%
  select(-psu_in_stratum)
  
psu_frame_with_stratum_ID <- 
  psu_frame %>% 
  left_join(temp_df, by = c('region', 'UR_RU', 'L_H'))


# take a sample of psus from each stratum ---------------------------------
no_of_strata <- dim(strata_0)[1]

sampled_psus <- 
  strata_0 %>% 
  mutate(sampled_psu_size = round(stratum_size * runif(no_of_strata, 0))) %>%
  mutate(sampled_psu_size = pmax(sampled_psu_size, 1)) %>% 
  select(stratum_ID, psu_in_stratum, stratum_size, sampled_psu_size) %>% 
  mutate(sampled_psus = map2(psu_in_stratum, 
                             sampled_psu_size, 
                             function(x, y) sample(x, y))) %>% 
  select(-psu_in_stratum)


# ssu information ---------------------------------------------------------
ssu_info_0 <- 
  ssu_frame %>% 
  count(psu_ID) %>% 
  rename(no_of_ssus = n) 

d <- dim(ssu_info_0)[1]

ssu_info_1 <- 
  ssu_info_0 %>% 
  mutate(sampled_ssu_size = round(no_of_ssus * runif(d), 0)) %>% 
  mutate(sampled_ssu_size = pmax(sampled_ssu_size, 1))

ssu_info_2 <- 
  ssu_frame %>% 
  count(psu_ID, age_group) %>% 
  rename(psu_age_grp_strat_size = n) %>% 
  mutate()

ssu_info_3 <- 
  ssu_info_2 %>% 
  left_join(ssu_info_1, by = "psu_ID") %>% 
  mutate(sampled_ssu_size = round(sampled_ssu_size * c(0.4, 0.4, 0.2), 0)) %>% 
  mutate(sampled_ssu_size = pmin(sampled_ssu_size, psu_age_grp_strat_size))

the_selected_psus <- 
  sampled_psus %>% 
  select(sampled_psus) %>% 
  unnest() %>% 
  left_join(ssu_info_3, by = c("sampled_psus" = "psu_ID")) 

ssu_info_4 <- 
  ssu_frame %>% 
  inner_join(the_selected_psus, by = c("psu_ID" = "sampled_psus",
                                       "age_group" = "age_group")) %>% 
  group_by(psu_ID, age_group) %>% 
  summarise(ssu_in_psu_age_grp = list(ssu_ID),
            sampled_ssu_size = mean(sampled_ssu_size)) %>% 
  select(psu_ID, age_group, ssu_in_psu_age_grp, sampled_ssu_size)

selected_ssus <- 
  ssu_info_4%>% 
  mutate(sampled_ssus = map2(ssu_in_psu_age_grp, 
                             sampled_ssu_size, 
                             function(x, y) sample(x, y))) %>% 
  select(sampled_ssus) %>% 
  unnest() %>% 
  ungroup() %>% 
  select(sampled_ssus)

the_final_sample <- 
  selected_ssus %>% 
  left_join(ssu_frame, by = c("sampled_ssus" = "ssu_ID")) %>% 
  left_join(the_selected_psus, by = c("psu_ID" = "sampled_psus",
                                      "age_group" = "age_group"))
  






