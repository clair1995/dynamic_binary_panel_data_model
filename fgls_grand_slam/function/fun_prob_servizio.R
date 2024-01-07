# PROB CHE USER? PER CALCOLO IMPORTANZA

# questa funzione calcola la probabilità di vincere un punto al servizio (per A e B)
# per ogni match disputato nel torneo.

# vengono utilizzati la qualità relativa (rankA - rankB) e la qualità assoluta (rankA + rankB)
# come predittori della probabilità, per ottenere stime più precise, seguendo l'articolo di Magnus e Klaassen

# calcolata come suggerito nell articolo -> regredendo y su Xa=(1, Ra-Rb, Ra+Rb)
# stima di y e la prob di A o di B di fare pto da servizio

# ovviamente per questa funzione dovro usare y_da_servizio-> i pti segnati da A o B da servizio
# rA-> rank giocatore A rB-> rank giocatoreB


prob_win_service = function(df_service, on_service = 'p1'){
  
  df_serv = df_service %>% 
    filter(PointServer == on_service) %>% 
    select(match_id, yat, ybt, rankA, rankB, PointServer) %>% 
    mutate(qual_rel = ifelse(PointServer == 'p1',
                             (rankA - rankB) - mean(rankA - rankB),
                             (rankB - rankA) - mean(rankB - rankA)),
           qual_ass = (rankA + rankB) - mean(rankA + rankB))
  

  X = df_serv %>%
    select(qual_rel, qual_ass) %>% 
    model.matrix(~ qual_rel + qual_ass, .)
  
  if(on_service == 'p1'){ y = df_serv$yat 
  } else { y = df_serv$ybt }
   
  betahat = lm(y~X-1)$coefficients
  
  df_probs_tmp = data.frame(
    match_id        = df_serv$match_id,
    probs_per_match = X %*% betahat 
  )
  
  df_probs_serve = df_probs_tmp %>% 
    group_by(match_id) %>% 
    summarize(probs_per_match = mean(probs_per_match))
  
  return(df_probs_serve)
  
} 
  