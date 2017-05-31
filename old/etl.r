library(readr);library(ggplot2)
library(dplyr)



# go to CSRPilotAnal.Rmd for pilot analysis. 


# Load stage game summary data ----
stage_cp_pilot = read_csv('/home/cmk11/projects/CSR_Analysis/data/csr_3_stageT (accessed 2017-02-16)_cleanup.csv')
stage_cp <- read_csv("/home/cmk11/projects/CSR_Analysis/20170326_CP_8Ss_nyuad/csr_3_stageT_coldPrickle (accessed 2017-03-26).csv")
stage_wg <- read_csv("/home/cmk11/projects/CSR_Analysis/20170326_WG_8Ss_nyuad/csr_3_stageT_warmGlow (accessed 2017-03-26).csv")

stage = bind_rows(
  stage_cp_pilot,
  stage_cp,
  stage_wg
) %>% 
  filter(
    participant._round_number == 1,
    !is.na(player.vcm_ge_percent)
  ) %>%
  mutate(
    player.role = ifelse(
      is.na(player.A_stage1),"F","A"
    )
  )

# Load RET Scores ----
ret = read_csv("/home/cmk11/projects/CSR_Analysis/20170326_WG_8Ss_nyuad/csr_0_realeffort (accessed 2017-03-26).csv") %>%
  mutate(
    participant._round_number = as.character(participant._round_number),
    participant.time_started = as.character(participant.time_started),
    player.is_correct = as.character(player.is_correct),
    player.ret_final_score = as.character(player.ret_final_score),
    player.round_payoff = as.character(player.round_payoff)
  )
ret_pilot = read_csv("/home/cmk11/projects/CSR_Analysis/data/csr_0_realeffort (accessed 2017-02-16).csv")

ret = bind_rows(
  ret_pilot,ret
)


#cleanup ret

ret = ret %>%
  dplyr::filter(session.code %in% unique(stage$session.code))
#just getting to know the data
# {
#   table(ret$session.code)
#   }

# VCM <- read_csv("/home/cmk11/projects/CSR_Analysis/data/csr_2_vcm_coldPrickle (accessed 2017-02-16).csv")
# VCM_cp <- read_csv("/home/cmk11/projects/CSR_Analysis/20170326_CP_8Ss_nyuad/csr_3_stageT_coldPrickle (accessed 2017-03-26).csv")
# VCM_wg <- read_csv("/home/cmk11/projects/CSR_Analysis/20170326_WG_8Ss_nyuad/csr_2_vcm_warmGlow (accessed 2017-03-26).csv")
VCM = bind_rows(
  read_csv("/home/cmk11/projects/CSR_Analysis/data/csr_2_vcm_coldPrickle (accessed 2017-02-16).csv") %>%
    dplyr::filter(session.code == "3ycq79md") %>%
    mutate(Treatment = "Cold Prickle"), 
  read_csv("/home/cmk11/projects/CSR_Analysis/20170326_CP_8Ss_nyuad/csr_2_vcm_coldPrickle (accessed 2017-03-26).csv") %>%
    mutate(Treatment = "Cold Prickle"),
  read_csv("/home/cmk11/projects/CSR_Analysis/20170326_WG_8Ss_nyuad/csr_2_vcm_warmGlow (accessed 2017-03-26).csv") %>%
    mutate(Treatment = "Warm Glow")
)  %>%
  dplyr::filter( !is.na(player.group_exchange))


temp = VCM %>%
  group_by(session.code, participant.code) %>%
  summarise(
    vcm_mean_ge_percent = mean(player.group_exchange_percent)
  ) %>%
  ungroup() %>%
  select(
    participant.code, vcm_mean_ge_percent
  )

stage = left_join(
  stage, temp
)
rm(temp)









