# funzione che calcola le stime fgls a effetti casuali 
# con variabile risposta binaria(0 non fa punto, 1 fa punto)
# ogni passaggio è descritto nell'articolo di Magnus e Klaassen nella cartella documentazione
# passo betaA e deltaA -> set di parametri stimati a ogni passo per la convergenza
# allo stesso modo on set di parametri per giocatore B


fgls <- function(dfA,dfB, betaA = NULL, deltaA = NULL, betaB = NULL, deltaB = NULL, ...) {
  
  # calcolo errore per ogni match
  params_A = calc_params_fgls(dfA, est_beta = betaA, est_delta = deltaA, ...)
  params_B = calc_params_fgls(dfB, est_beta = betaB, est_delta = deltaB, ...)
  
  # stima gamma, una per tutti i match
  gamma.hat <- mean(params_A$u_t*params_B$u_t) # facendo la media tra tt i giocatori, averaging over players

  # stima tau2-> una per tutti i match
  tau2 <- (params_A$tau2+params_B$tau2)/2
  
  # sigma diverso per giocatore A e B (globale per tutti i match)
  sigma_a <- params_A$tau_sigma - tau2
  sigma_b <- params_B$tau_sigma - tau2
  
  colnames(dfA) = colnames(dfB) = c("match_id" 
                                   ,"momento_partita" 
                                   ,"servizio" 
                                   ,"yt" 
                                   ,"rel_qual" 
                                   ,"ass_qual"       
                                   ,"yt_1"
                                   ,"delta11"
                                   ,"delta12"
                                   ,"dt"
                                   ,"delta21"
                                   ,"delta22"
                                   ,"imp"
                                   ,"delta31"
                                   ,"delta32")
  
  df_AB = bind_rows(dfA %>% mutate(serve = 'A'), 
                    dfB %>% mutate(serve = 'B'))
  
  num_match = length(unique(dfA$match_id))
  
  df_fin = NULL
  
  for (i in 1:num_match) {
    
    # creo matrice covariate per entrambi i dataset per ogni match
    df_match = df_AB %>% 
      filter(match_id == i)

    X <- df_match %>% 
      select(rel_qual:delta32) %>% 
      model.matrix(~., data = .)
    
    Y <- df_match$yt
    
    Ta <- sum(df_match$serve=='A')
    Tb <- sum(df_match$serve=='B')
    
    O <- omega_match(Ta, Tb, sigma_a, sigma_b, gamma.hat, tau2)
    
    df_AB_fin = O%*%X %>% 
      bind_cols(df_match %>% select(match_id, serve, momento_partita)) %>% 
      mutate(yt = O%*%Y %>% as.vector())
    
    df_fin <- rbind(df_fin, df_AB_fin)
    
    #cat('\n')
    #cat(glue('match: {i}'))
    
  }
  
  Xa_fin <- df_fin %>% 
    filter(serve == 'A') %>% 
    select(-c(match_id, serve, momento_partita,yt)) %>% 
    as.matrix()
  
  Xb_fin <- df_fin %>% 
    filter(serve == 'B') %>% 
    select(-c(match_id, serve, momento_partita,yt)) %>% 
    as.matrix()
  
  Ya_fin <- df_fin %>% 
    filter(serve == 'A') %>% 
    .$yt %>% 
    as.matrix()
  
  Yb_fin <- df_fin %>% 
    filter(serve == 'B') %>% 
    .$yt %>% 
    as.matrix()
  
  beta_delta.a<- as.vector(solve(t(Xa_fin) %*% Xa_fin) %*% t(Xa_fin) %*% Ya_fin)
  
  beta_delta.b<- as.vector(solve(t(Xb_fin) %*% Xb_fin) %*% t(Xb_fin) %*% Yb_fin)
  
  df_fin = df_fin %>% 
    rename('Intercept' = `(Intercept)`) %>% 
    relocate(match_id, momento_partita, serve, yt)
    
  
  return( list ( "gamma"   = gamma.hat, 
                 "tau2"    = tau2, 
                 "sigma_A" = sigma_a, 
                 "sigma_B" = sigma_b, 
                 "beta_delta.a"= beta_delta.a, 
                 "beta_delta.b"= beta_delta.b, 
                 "df_finale"= df_fin ))
  
}
