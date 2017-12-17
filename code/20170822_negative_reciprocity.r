
# we want to test for negative resiprocity
#' AJ "the idea is that we introduce a control variable for if the 
#' firm failed to contribute, i.e. if the terminal node was A3 or nature"
#' CK "Okay. Do you see my question? So the dependent is if A choices A3 
#' to go to N, but conditional in F having not picked F1 or F3 in previous 
#' repetition of stage game?"

source("code/etl_prod.r")
library(tidyr)


# Clean up data
# Use those to setup this logic: 
# in round t-1: firm choices F2 (don't contribute) & and A picks A3
# then in round t: F to F2, and A to A4.

# I think you might do: 
# group_by group.id_in_subsession, and session.code
# - Arrange by (session.code, group.id_in_subsession, subsession.round_number, player.player_role)
# - you want to arrange (or sort, reorder) so that you have a consistent structure 


Stage_neg_repc = stage %>%
  dplyr::arrange(session.code, group.id_in_subsession, player.stage_round_count, player.player_role) %>%
  dplyr::select(session.code,group.id_in_subsession,subsession.round_number,
                participant.code, participant.label,player.id_in_group, player.player_role,
                player.stage_round_count,player.A_stage1, player.F_stage2, player.A_stage3,
                player.Nature, player.terminal_choice)


Stage_neg_repc_F = Stage_neg_repc %>%
  dplyr::arrange(session.code, group.id_in_subsession, subsession.round_number, player.player_role) %>%
  dplyr::filter(
    is.na(player.A_stage1)
  ) %>%
  dplyr::select(session.code,group.id_in_subsession, subsession.round_number, player.stage_round_count,
                player.F_stage2)

Stage_neg_repc_A = Stage_neg_repc %>%
  dplyr::arrange(session.code, group.id_in_subsession, subsession.round_number, player.player_role) %>%
  dplyr::filter(
    is.na(player.F_stage2)
  ) %>%
  dplyr::select(session.code,group.id_in_subsession, subsession.round_number,
                player.A_stage1, player.A_stage3, player.Nature, player.terminal_choice)


Stage_neg_repc = dplyr::left_join(
  Stage_neg_repc_F,Stage_neg_repc_A,
  by = c('session.code','group.id_in_subsession', 'subsession.round_number')
  
) %>%
  dplyr::select(
    session.code, group.id_in_subsession, subsession.round_number,
    player.A_stage1,player.F_stage2,player.A_stage3, player.Nature, 
    player.terminal_choice 
  )

Stage_neg_repc = Stage_neg_repc %>%
  group_by(session.code, group.id_in_subsession) %>%
  dplyr::mutate(
    # did F pick F1 or F2 in previous round?
    F_prev_move = lag(player.F_stage2),
    # negrep_test = ifelse((F_prev_move == "F2" & player.terminal_choice %in% c("N1","N2")), yes = TRUE, no = FALSE)
    negrep_test = ifelse((F_prev_move == "F2"), yes = TRUE, no = FALSE)
    
  ) %>%
  dplyr::filter(subsession.round_number>1) %>%
  dplyr::mutate(
    negrep_test_avg = mean(negrep_test)
  ) %>% 
  dplyr::select(
    session.code, group.id_in_subsession,subsession.round_number, negrep_test, negrep_test_avg
  )

Stage_neg_repc %>%
  distinct(session.code, group.id_in_subsession, negrep_test_avg) %>%
  as.data.frame

# Okay, so the variable "negrep_test" is the percent of times that A-players met this condition. 
# The condition could be interpreted as "fuck you" to the F player. 
