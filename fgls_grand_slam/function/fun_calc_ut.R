# calcolo gli errori per ogni match per ilgiocatore al servizio

u_t = function(df){
  
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
  
  delta.hat = stima.iv(df)
  
  y_t.star <- round(df$yt - z_t %*% delta.hat)
  
  # avro un solo beta per tutti i match
  beta.hat <- lm(y_t.star~ X_qual - 1) %>% 
    .$coefficients %>% 
    as.vector()
  
  # calcolo errore medio di ogni match per il giocatore al servizio
  # un solo errore per tutti i match
  u_t.hat <- df %>% 
    select(match_id, yt) %>% 
    mutate(u_t = yt - X_qual %*% beta.hat - z_t %*% delta.hat) %>% 
    group_by(match_id) %>% 
    summarize(ut_per_match = mean(u_t)) %>% 
    ungroup() %>% 
    pull(ut_per_match)
  
  return(u_t.hat)
}

