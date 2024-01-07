# data preparation
# creazione del dataset definitivo che mi serve per costruire modello e analisi finali-----

# crea tutte le variabili utili per adattare poi il modello FGLS

# per ogni funzione creo la matrice di transizione con le diverse probabilita, poi le riempio
# con le prob di fare pto da servizio per giocatore A e B

# in seguito usero le markov chain per calcolarmi le diverse prob.

# il modo di giocare il tie-break al 5o set è diverso in ogni torneo, 
# quindi passo alla funzione la variabile torneo che mi dice che tipo 
# di torneo del Grande Slam sto considerando

prep_df_fin = function(df_serv, torneo = 'AO') {
  
  df0 = df_serv %>% 
    mutate(match_id = as.integer(factor(match_id))) %>%
    group_by(match_id, SetNo, GameNo) %>% 
    mutate(yat_1 = lag(yat, default = 0),
           ybt_1 = lag(ybt, default = 0)) %>% 
    ungroup() %>% 
    # identifica se siamo all inizio del set -> in qst casi ovviamente per y_t-1 non ha senso
    mutate(dat = ifelse(yat_1 == 0 & ybt_1 == 0 & PointServer == 'p1', 1, 0),
           dbt = ifelse(yat_1 == 0 & ybt_1 == 0 & PointServer == 'p2', 1, 0),
           P1Score = ifelse(P1Score == 'AD', 'Adv', P1Score),
           P2Score = ifelse(P2Score == 'AD', 'Adv', P2Score)) %>% 
    # modifiche per otttenere importanza nel 5o set del RG(non esite Tie-break)
    mutate(game_A_RG = case_when(
      SetNo == 5 & game_A > 6 & game_B > 6 & game_A == game_B ~ 6,
      SetNo == 5 & game_A > 6 & game_B > 6 & set_A == 3 ~ 7,
      SetNo == 5 & game_A > 6 & game_B > 6 & game_A > game_B ~ 6,
      SetNo == 5 & game_A > 6 & game_B > 6 & game_A < game_B ~ 5,
      TRUE ~ game_A
      ),
      game_B_RG = case_when(
        SetNo == 5 & game_A > 6 & game_B > 6 & game_A == game_B ~ 6,
        SetNo == 5 & game_A > 6 & game_B > 6 & set_B == 3  ~ 7,
        SetNo == 5 & game_A > 6 & game_B > 6 & game_A < game_B ~ 6,
        SetNo == 5 & game_A > 6 & game_B > 6 & game_A > game_B ~ 5,
        TRUE ~ game_B
      ),
      # a Wimbledon Tie-break al 5o set sul 12-12
      game_A_W = case_when(
        SetNo == 5 & game_A == 12 & game_B == 12 ~ 7,
        SetNo == 5 & game_A > 6 & game_B > 6 & game_A == game_B ~ 6,
        SetNo == 5 & game_A > 6 & game_B > 6 & set_A == 3 ~ 7,
        SetNo == 5 & game_A > 6 & game_B > 6 & game_A > game_B ~ 6,
        SetNo == 5 & game_A > 6 & game_B > 6 & game_A < game_B ~ 5,
        TRUE ~ game_A
      ),
      game_B_W = case_when(
        SetNo == 5 & game_A == 12 & game_B == 12 ~ 7,
        SetNo == 5 & game_A > 6 & game_B > 6 & game_A == game_B ~ 6,
        SetNo == 5 & game_A > 6 & game_B > 6 & set_B == 3  ~ 7,
        SetNo == 5 & game_A > 6 & game_B > 6 & game_A < game_B ~ 6,
        SetNo == 5 & game_A > 6 & game_B > 6 & game_A > game_B ~ 5,
        TRUE ~ game_B
      )) %>% 
    # creo stringa con tutte le combinazioni che ho trovato nel dataset per una determinata partita
    # verranno scritte nel formato ( ptiA-ptiB|gameA-gameB|setA-setB ) -> es. 15-30|1-4|2-1
    mutate(momento_partita = paste0(P1Score, '-',  P2Score,'|',
                                    game_A,  '-',  game_B, '|',
                                    set_A,   '-',  set_B), .after = match_id,
           mom_RG_to_join = paste0(P1Score, '-',  P2Score,'|',
                                      game_A_RG,  '-',  game_B_RG, '|',
                                      set_A,   '-',  set_B),
           mom_W_to_join = paste0(P1Score, '-',  P2Score,'|',
                                   game_A_W,  '-',  game_B_W, '|',
                                   set_A,   '-',  set_B)) %>% 
    select(-c(P1Score, P2Score, game_A, game_B, set_A, set_B, game_A_RG, game_B_RG, game_A_W, game_B_W))
           
  
  
  # prob per ogni match che A vinca punto in servizio-> hold service
  # uso serie di pti vinti o no DA SERVIZIO per il giocatore A
  prob_servA = prob_win_service(df0, on_service = 'p1')
  
  
  # prob per ogni match che B vinca punto in servizio-> hold service
  # uso serie di pti vinti o no DA SERVIZIO per il giocatore B
  prob_servB <- prob_win_service(df0, on_service = 'p2')
  
  # calcolo importanza del pti t-esimo per ognuno degli m match
  df_imp<-NULL  
  
  for (i in unique(df0$match_id)) {
    
    prob_A = prob_servA %>% 
      filter(match_id == i) %>% 
      pull(probs_per_match)
    
    prob_B = prob_servB %>% 
      filter(match_id == i) %>% 
      pull(probs_per_match)
    
    df_imp_pm = imp_pm(prob_A = prob_A, prob_B = prob_B) 
    
    if(torneo == 'RG') {
      
      df_imp_temp = df0 %>% 
        filter(match_id == i) %>% 
        left_join(df_imp_pm, by = c('mom_RG_to_join' = 'esiti'), 
                  relationship = "many-to-many") %>% 
        select(-mom_RG_to_join, -mom_W_to_join) %>% 
        distinct() %>% 
        mutate(imp = ifelse(is.na(imp),1,imp))
      
    } else if(torneo == 'W') {
      
      df_imp_temp = df0 %>% 
        filter(match_id == i) %>% 
        left_join(df_imp_pm, by = c('mom_W_to_join' = 'esiti'), 
                  relationship = "many-to-many") %>% 
        select(-mom_W_to_join, -mom_W_to_join) %>% 
        distinct() %>% 
        mutate(imp = ifelse(is.na(imp),1,imp))
      
      } else {
      
      df_imp_temp = df0 %>% 
        filter(match_id == i) %>% 
        left_join(df_imp_pm, by = c('momento_partita' = 'esiti'), 
                  relationship = "many-to-many") %>% 
        select(-mom_RG_to_join, -mom_W_to_join) %>% 
        distinct() %>% 
        mutate(imp = ifelse(is.na(imp),1,imp))
      
    }
      
    df_imp = rbind(df_imp, df_imp_temp)
    
    cat(glue('In elaborazione match: {i}'))
    cat("\n")

  }
  
  
  
  df_fin_A = df_imp %>% 
    filter(PointServer == 'p1') %>% 
    mutate(rel_qual = (rankA - rankB) - mean(rankA - rankB),
           ass_qual = (rankA + rankB) - mean(rankA + rankB),
           delta11  = rel_qual * yat_1,
           delta12  = ass_qual * yat_1,
           delta21  = rel_qual * dat,
           delta22  = ass_qual * dat,
           delta31  = rel_qual * imp,
           delta32  = ass_qual * imp,
           servizioA = player1) %>% 
    select(match_id, momento_partita, servizioA, yat, rel_qual, ass_qual,
           yat_1, delta11, delta12,
           dat, delta21, delta22,
           imp, delta31, delta32)
  
  
  df_fin_B = df_imp %>% 
    filter(PointServer == 'p2') %>% 
    mutate(rel_qual = (rankB - rankA) - mean(rankB - rankA),
           ass_qual = (rankA + rankB) - mean(rankB + rankA),
           delta11  = rel_qual * ybt_1,
           delta12  = ass_qual * ybt_1,
           delta21  = rel_qual * dbt,
           delta22  = ass_qual * dbt,
           delta31  = rel_qual * imp,
           delta32  = ass_qual * imp,
           servizioB = player2) %>% 
      select(match_id, momento_partita, servizioB, ybt, rel_qual, ass_qual,
             ybt_1, delta11, delta12,
             dbt, delta21, delta22,
             imp, delta31, delta32)
  
  return(list(df_prep_A = df_fin_A,
              df_prep_B = df_fin_B))

}
