# # funzione che attribuisce ranking a rispettivo giocatore e sistema dataset

df_match_rank <-function(df,df_match,rk) {
  
  df1 = df %>% 
    mutate(match_id = as.numeric(substr(match_id, nchar(match_id)-3, nchar(match_id)))) %>% 
    filter(match_id < 2000)
  
  df_match1 = df_match %>% 
    filter(match_num < 2000) %>% 
    left_join(rk_clean, by = join_by('player1' == 'player')) %>% 
    left_join(rk_clean, by = join_by('player2' == 'player')) %>% 
    rename('rankA' = 'Pos..x',
           'rankB' = 'Pos..y') %>% 
    select(year, slam, match_num, player1, rankA, player2, rankB)
    
  df_finale = df1 %>% 
    inner_join(df_match1, by = c('match_id' = 'match_num'))
  
  return(df_finale)
}  


