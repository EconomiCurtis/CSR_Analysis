library(readr)
library(ggplot2)
library(dplyr)
library(haven)


# go to CSR_Initial_Analysis.Rmd for pilot analysis. 

 

# Load stage game summary data ----
stage = read_csv('/home/cmk11/projects/CSR_Analysis/data/20170521_csr_030mpcr/csr_3_stageT (accessed 2017-05-21).csv')
stage = bind_rows(
  stage,
  read_csv('/home/cmk11/projects/CSR_Analysis/data/20170521_csr_075mpcr/csr_3_stageT (accessed 2017-05-21).csv'),
  read_csv('/home/cmk11/projects/CSR_Analysis/data/20170522_csr_030mpcr/csr_3_stageT (accessed 2017-05-22).csv')
) %>% ungroup()

stage = stage %>%
  distinct(participant.code, session.code, subsession.round_number, .keep_all = TRUE) %>%
  filter(session.code %in% c('2huaehm1', 'wsvf3qiy', 'wheg36z2')) %>%
  filter(subsession.round_number <11) %>%
  ungroup()

# 2huaehm1 first session 0.3 mpcr
# wsvf3qiy second session 0.75 mpcr
# wheg36z2 third session 0.3 mpcr



# Load RET Scores ----
ret = bind_rows(
  read_csv("/home/cmk11/projects/CSR_Analysis/data/20170521_csr_030mpcr/csr_0_realeffort (accessed 2017-05-21).csv"),
  read_csv("/home/cmk11/projects/CSR_Analysis/data/20170521_csr_075mpcr/csr_0_realeffort (accessed 2017-05-21).csv")
) %>%
  mutate(
    participant._round_number = as.integer(participant._round_number),
    participant.time_started = (participant.time_started),
    player.is_correct = as.integer(player.is_correct),
    player.ret_final_score = as.numeric(player.ret_final_score),
    player.round_payoff = as.numeric(player.round_payoff)
  ) %>% ungroup()
ret = bind_rows(
  ret, 
  read_csv("/home/cmk11/projects/CSR_Analysis/data/20170522_csr_030mpcr/csr_0_realeffort (accessed 2017-05-22).csv")
) %>% ungroup()
#cleanup ret
ret = ret %>%
  dplyr::filter(session.code %in% unique(stage$session.code)) %>%
  dplyr::filter(!is.na(player.user_text)) %>%
  distinct(participant.code, session.code, subsession.round_number, .keep_all = TRUE) %>%
  ungroup()

write.csv(
  ret, 
  "/home/cmk11/projects/CSR_Analysis/data/production_RET.csv")




#just getting to know the data
# {
#   table(ret$session.code)
#   }
  
  

VCM = bind_rows(
  read_csv("/home/cmk11/projects/CSR_Analysis/data/20170521_csr_030mpcr/csr_2_vcm (accessed 2017-05-21).csv"),
  read_csv("/home/cmk11/projects/CSR_Analysis/data/20170521_csr_075mpcr/csr_2_vcm (accessed 2017-05-21) (1).csv")
)  %>% ungroup()
VCM = bind_rows(
  VCM,
  read_csv("/home/cmk11/projects/CSR_Analysis/data/20170522_csr_030mpcr/csr_2_vcm (accessed 2017-05-22).csv")
)%>%
  dplyr::filter( !is.na(player.group_exchange)) %>%
  dplyr::filter(session.code %in% unique(stage$session.code)) %>%
  dplyr::distinct(.keep_all = T) %>%
  ungroup()


temp = VCM %>%
  group_by(session.code, participant.code) %>%
  summarise(
    vcm_mean_ge_percent = mean(player.group_exchange_percent)
  ) %>%
  ungroup() %>%
  select(
    participant.code, vcm_mean_ge_percent
  ) %>% ungroup()




stage = left_join(
  stage, temp
) %>% ungroup()
rm(temp)



write.csv(
  stage, 
  "/home/cmk11/projects/CSR_Analysis/data/productionDataStage.csv")

stage_dta = stage 
names(stage_dta) = gsub('\\.', '_',names(stage_dta))
stage_dta = stage_dta%>%
  select(
    -participant_exclude_from_data_analysis,
    -player_postStage_self_individual_exchange,
    -player_postStage_op_individual_exchange,
    -player_postStage_op_group_exchange
    ) %>% ungroup()

write_dta(
  stage_dta, 
  "/home/cmk11/projects/CSR_Analysis/data/productionDataStageSTATA.dta")

write.csv(
  VCM, 
  "/home/cmk11/projects/CSR_Analysis/data/productionDataVCM.csv")

VCM_dta = VCM
names(VCM_dta) = gsub('\\.', '_',names(VCM_dta))
VCM_dta = VCM_dta %>%
  select(
    -player_total_op_individual_exchange,
    -participant_exclude_from_data_analysis
  ) %>% ungroup()
write_dta(
  VCM_dta, 
  "/home/cmk11/projects/CSR_Analysis/data/productionData_VCM_STATA.dta")

write.csv(
  ret, 
  "/home/cmk11/projects/CSR_Analysis/data/production_RET.csv")


ret_dta = ret 
names(ret_dta) = gsub('\\.', '_',names(ret_dta))
ret_dta = ret_dta %>%
  select(
    -participant_exclude_from_data_analysis
  ) %>% ungroup()
write_dta(
  ret_dta, 
  "/home/cmk11/projects/CSR_Analysis/data/productionData_RET_STATA.dta")


  
