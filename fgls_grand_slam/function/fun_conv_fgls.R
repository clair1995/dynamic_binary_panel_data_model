# funzione che itera fino i parametri usando i beta e delta stimati a passo 1 
# usando i dati originari, fino ad arrivare a convergenza -----------
# passo numero di iterazioni che voglio
# quando algoritmo raggiunge quel numero di iterazioni esce automaticamente dal loop

conv_fgls<-function(dfA, dfB, n.iter) {
  
  # while loop starts
  i = 1
  
  while(i <= n.iter){
  
    # la prima iterazione serve per trovarmi le prime stime di beta e delta
    if(i == 1) { 
      
      stime <- fgls(dfA,dfB, mom_iter = i) 
      
      par = c(0,0,0,0)
      
      } else {
        
      stime <- fgls(dfA, dfB, # dataframe di partenza 
                    mom_iter = i, # in che momento del loop sono (solo alla prima iter beta e delta vengono calcolati)
                    betaA  = betaA.hat, deltaA = deltaA.hat, # stime parametri per il giocatore A
                    betaB  = betaB.hat, deltaB = deltaB.hat) # stime parametri per il giocatore A
      
      par = par_loop
      
      }
        
      tau21    <- stime$tau2
      gamma21  <- stime$gamma
      sigma_a1 <- stime$sigma_A
      sigma_b1 <- stime$sigma_B
      
      par_loop <- round(c('tau2' = tau21, 'gamma' = gamma21, 
                          'sigma_A' = sigma_a1, 'sigma_B' = sigma_b1), digits = 5)
      
      stime.a <- stime$beta_delta.a
      stime.b <- stime$beta_delta.b
      
      # stime parametri A al servizio alla prima iterazione
      betaA.hat  <- stime.a[c(1:3)]
      deltaA.hat <- stime.a[c(4:12)]
      # stime parametri B al servizio alla prima iterazione
      betaB.hat  <- stime.b[c(1:3)]
      deltaB.hat <- stime.b[c(4:12)]
      
      # vauto se parametri sono arrivati a convergenza
      flag.stima = (sum(par == par_loop) == 4)
      
      print(rbind(par,par_loop))#,hat.a,hat.b))
      
      if(flag.stima == T | i == n.iter) {break}
      
      i = i+1
      
  } # while 
    
  df_finale_AB = stime$df_finale
  
  
  return(list( "df_finale" = df_finale_AB,
               "tau2" = tau21, 
               "gamma" = gamma21, 
               "sigmaA" = sigma_a1, 
               "sigmaB" = sigma_b1))
  
}  
