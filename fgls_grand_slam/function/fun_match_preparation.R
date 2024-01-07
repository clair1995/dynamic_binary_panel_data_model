# pulizia dataset e creazione variabili:
# yat - ybt = dicotomica, se il punto lo vince giocatore A o giocatore B
# gameA - gameB = cumulata, se il game lo vince giocatore A o giocatore B
# setA - setB = cumulata, se il set lo vince giocatore A o giocatore B
# rankA - rankB = calcolata come nell'articolo Magnus Klassen (log(rank, base = 2))

# infine, seguendo l'articolo, divido output in 2 dataset distinti, 
# 1 in cui per ogni partita batte il giocatore A, 
# 2 in cui per ogni partita batte giocatore B

preproc_df <- function(df) {
  
  df_clean = df %>% 
    mutate(pA1 = suppressWarnings(as.numeric(as.character(P1Score))),
           pB1 = suppressWarnings(as.numeric(as.character(P2Score)))) %>% 
    mutate(pA1 = ifelse(is.na(pA1), 50, pA1),
           pB1 = ifelse(is.na(pB1), 50, pB1),
           yat = ifelse(PointWinner == 1, 1, 0),
           ybt = ifelse(PointWinner == 2, 1, 0))

    
  df_game = df_clean %>% 
    group_by(match_id, SetNo, GameNo) %>% 
    summarize(somma_pti_A = sum(yat),
              somma_pti_B = sum(ybt)) %>% 
    mutate(gameA = somma_pti_A > somma_pti_B,
           gameB = somma_pti_A < somma_pti_B,
           game_A_cum = lag(cumsum(gameA), default = 0),
           game_B_cum = lag(cumsum(gameB), default = 0)) %>% 
    ungroup() %>% 
    select(match_id, SetNo, GameNo, somma_pti_A, somma_pti_B, game_A_cum, game_B_cum)
  
  df_set = df_game %>% 
    group_by(match_id, SetNo) %>% 
    filter(row_number()==n()) %>% 
    mutate(set_A = game_A_cum > game_B_cum,
           set_B = game_A_cum < game_B_cum,
           set_A  = ifelse((game_A_cum == 6 & game_B_cum == 6) & 
                            (somma_pti_A > somma_pti_B), T, set_A),
           set_B  = ifelse((game_A_cum == 6 & game_B_cum == 6) & 
                            (somma_pti_B > somma_pti_A), T, set_B)) %>%
    ungroup() %>% 
    group_by(match_id) %>% 
    mutate(set_A_cum = lag(cumsum(set_A), default = 0),
           set_B_cum = lag(cumsum(set_B), default = 0)) %>% 
    ungroup() %>% 
    select(match_id, SetNo, set_A_cum, set_B_cum)
  
  df_servizio = df_clean %>% 
    left_join(df_game) %>% 
    left_join(df_set)
  
  
  df_servizio_clean = df_servizio %>% 
    group_by(match_id, SetNo, GameNo) %>% 
    mutate(game_A_cum = lead(game_A_cum, default = max(game_A_cum, na.rm = T)),
           game_B_cum = lead(game_B_cum, default = max(game_B_cum, na.rm = T)),
           game_A = ifelse(row_number()==n() & yat == 1, max(game_A_cum, na.rm = T)+1, game_A_cum),
           game_B = ifelse(row_number()==n() & ybt == 1, max(game_B_cum, na.rm = T)+1, game_B_cum)) %>% 
    ungroup() %>%
    group_by(match_id, SetNo) %>% 
    mutate(set_A_cum  = lead(set_A_cum, default = max(set_A_cum, na.rm = T)),
           set_B_cum  = lead(set_B_cum, default = max(set_B_cum, na.rm = T)),
           set_A = ifelse(row_number()==n() & yat == 1, max(set_A_cum, na.rm = T)+1, set_A_cum),
           set_B = ifelse(row_number()==n() & ybt == 1, max(set_B_cum, na.rm = T)+1, set_B_cum)) %>% 
    ungroup() %>% 
    select(-ends_with('cum')) %>% 
    mutate(PointServer = ifelse(PointServer==1, 'p1', 'p2'),
           rankA = 8-log(rankA,base=2),
           rankB = 8-log(rankB,base=2))
    

  return( df_servizio_clean )
}
