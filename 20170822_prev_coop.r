

# Use those to setup this logic: 
# in round t-1: firm choices F2 (don't contribute) & and A picks A3
# then in round t: F to F2, and A to A4.

# I think you might do: 
# group_by group.id_in_subsession, and session.code
# - Arrange by (session.code, group.id_in_subsession, subsession.round_number, player.player_role)
# - you want to arrange (or sort, reorder) so that you have a consistent structure 

source("code/etl_prod.r")

Stage %>%
  arrange(session.code, group.id_in_subsession, subsession.round_number, player.player_role)
