# importanza del punto nel match!!! --------
# moltiplico tra loro imp_pg*imp_gs*imp_sm

# avr?, date le prob di fare pto da servizio di A e B, l'importanza di tutti i diversi punti nel match in qualsiasi momento
# ho 18 possibili esiti per il punto nel game
# ho 39 possibili esiti per il game nel set ( compreso tie break )
# ho 9 possibili esiti  per il set nel match

# 18*39*9=6318 righe

# percio avro 6318 righe-> a ogni riga e legato il valore dell'importanza di quel punto nel match date probA e probB

# calcolo importnza pto nel game, game nel set e set nel match usando matrici
# e trasformazioni Markov Chain.

# l'importanza del pto nel match è data dalla moltiplicazione delle 3 misure di importanza


imp_pm<-function( prob_A, prob_B) { 
  
  # imp punto nel game
  imp.pg <- imp_pg(prob_A, prob_B)
  # importanza game nel set
  imp.gs <- imp_gs(prob_A,prob_B)
  # importanza set nel match
  imp.sm <- imp_sm(prob_A, prob_B)
  
  
  # imposto e salvo nelle vars tutti i possibili stati che possono verificarsi nel match
  
  st_point1 = imp.pg$imp_pg$mom_point
  st_point2 = imp.pg$imp_pg_tie$mom_point
  
  st_game = imp.gs$mom_game
  
  st_set = imp.sm$mom_set
  
  # creo griglie che definiscano tutti gli esiti possibili
  
  grid1 = expand.grid(punti = c(st_point1, st_point2), game = st_game)
  
  esiti_grid = expand_grid(grid1, set = st_set)
  
  # calcolo importanza come moltiplicazione tra imp punto*game*set
  df_imp = esiti_grid %>% 
    left_join(imp.pg$imp_pg,     by = c('punti' = 'mom_point')) %>% 
    left_join(imp.pg$imp_pg_tie, by = c('punti' = 'mom_point')) %>% 
    left_join(imp.gs, by = c('game' = 'mom_game')) %>% 
    left_join(imp.sm, by = c('set'  = 'mom_set')) %>% 
    mutate(imp_pg     = ifelse(game != '7-7', imp_pg, 1),
           imp_pg_tie = ifelse(game == '7-7', imp_pg_tie, 1)) %>% 
    mutate(esiti = paste(punti, game, set, sep = '|'),
           imp   = imp_pg * imp_pg_tie*imp_gs*imp_sm) %>% 
    na.omit() %>% 
    filter(imp > 0) %>% 
    select(esiti, imp)
  
  
  return(df_imp)
  
}



