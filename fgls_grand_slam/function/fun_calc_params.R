# calcolo i parametri per ogni match per il giocatore al servizio
# seguo formule articolo
# df è il dataframe dove batte solo A o solo B per ogni match
# mom_iter è un parametro xche gli passiamo che aiuta la funzione a capire in che 
# stato del while loop siamo
# solo alla prima iterazione calcola beta e delta dai dati iniziali usando variabili strumentali.
# dal secondo in avanti cerca di arrivare a convergenza ristimando i set di parametri
calc_params_fgls = function(df, mom_iter = NULL, est_beta = NULL, est_delta = NULL){
  
  colnames(df) = c("match_id" 
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
  
  
  # creo matrice qualità:
  # costante durante il match
  X_qual = df %>% 
    select(contains('qual')) %>% 
    model.matrix(~., data = .)
  
  # matrice che considera variabili dinamiche
  # che cambiano durante il match
  z_t = df %>% 
    select(yt_1:delta32) %>% 
    as.matrix()
  
  # y regressa
  Y_reg = df$yt - df$yt_1
  
  if(mom_iter == 1) {
    delta.hat = stima.iv(df)} else { delta.hat = est_delta }
  
  y_t.star <- round(df$yt - z_t %*% delta.hat)
  
  # avro un solo beta per tutti i match
  if(mom_iter == 1) {
    beta.hat <- lm(y_t.star~ X_qual - 1) %>% 
      .$coefficients %>% 
      as.vector()} else { beta.hat = est_beta }

  
  # calcolo errore medio di ogni match per il giocatore al servizio
  # un solo errore per tutti i match
  df_u.t <- df %>% 
    select(match_id, yt) %>% 
    mutate(u_t = yt - X_qual %*% beta.hat - z_t %*% delta.hat) 
  
  u_t.hat = df_u.t$u_t
  
  u_match = df_u.t %>% 
    group_by(match_id) %>% 
    summarize(ut_per_match = mean(u_t)) %>% 
    ungroup() %>% 
    pull(ut_per_match)
  
  # trovo stime di Tau -> diverse per ogni match
  tau_sigma <- mean((X_qual %*% beta.hat + z_t %*% delta.hat)*(1 - X_qual %*% beta.hat - z_t %*% delta.hat))-
    2*mean((z_t %*% delta.hat)*u_t.hat) 
  
  n = length(u_t.hat)
  
  tau2<- (mean(u_match^2) - (1/n)*tau_sigma)*(n/(n-1)) #1
  
  return(list(
    u_t = u_match,
    tau_sigma = tau_sigma,
    tau2 = tau2))
  
}

