library(markovchain)

# per ogni funzione creo la matrice di transizione con le diverse probabilità, poi le riempio
# con le prob di fare pto da servizio per giocatore A e B

df_A_serve<-function(df_A, df_B) {
  
  # restituisce numero di match-> ogni numero si ripete PER TUTTA la durata del match uso dataframe serve_A
  ID_match<-df_A$riga
  # restituisce numero di match-> ogni numero si ripete PER TUTTA la durata del match uso dataframe serve_B
  ID_m<-df_B$riga
  
  turno<-df_A$turno
  
  # punti quando batte A
  p1A<-df_A$puntiA
  p1B<-df_A$puntiB
  
  # punti quando batte B
  p2A<-df_B$puntiA
  p2B<-df_B$puntiB
  
  # game quando batte A
  gA<- df_A$gameA
  gB<- df_A$gameB
  
  # set quando batte A
  set_1A<- df_A$setA
  set_1B<- df_A$setB
  
  # ranking A e B quando batte A
  r1A<- df_A$rankA
  r1B<- df_A$rankB
  
  # punto a punto A e B
  yat<-df_A$vinceA
  ybt<-df_A$vinceB
  
  yat_1<-y_A_B(yat, ybt)[,2]
  
  # [,1]-> yA-> A fa punto o no (0,1)
  # [,2]-> yA_t-1<- A ha segnato al pto precedente
  # [,3]-> yB-> B fa punto o no (0,1)
  # [,4]-> yB_t-1<- B ha segnato al pto precedente
  
  
  # identifica se siamo all inizio del set -> in qst casi ovviamente per y_t-1 non ha senso
  dat<-d_at(p1A,p1B)
  
  # prob per ogni match che A vinca punto in servizio-> hold service
  # y_A_B(p1A, p1B)[,1] -> serie di pti vinti o no DA SERVIZIO per il giocatore A
  prob_servA<-prob_win_service_A(df_A)
  
  # prob per ogni match che A vinca punto in servizio-> hold service
  # y_A_B(p2A, p2B)[,3] -> serie di pti vinti o no DA SERVIZIO per il giocatore B
  prob_servB<-prob_win_service_B(df_B)
  
  # calcolo importanza del pti t-esimo per ognuno degli m match
  imp<-NULL  
  for (i in 1:length(unique(ID_match))) {
    imp<-c(imp,importanza_punto(df_A[which(ID_match==i),], #set B i-mo match
                                prob_servA[i],prob_servB[i])) #calcolate sopra-> prob di fare pto da servizio per A e B
    cat(i)
  }
  
  # in che momento della partita siamo: formato: [ punti | game | set ]-> [ 15-40|2-4|1-0 ]
  mom_partita<-partita_stringa(df_A)
  
  rel_qual<-round((r1A-r1B),digits = 4)
  
  ass_qual<-round((r1A+r1B),digits=4)
  
  imp<-round(imp,digits=6)
  
  # identifica se siamo all inizio del set -> in qst casi ovviamente per y_t-1(matrice[,2]) non ha senso-> lo pongo =0
  for (i in 1:length(p1A)) {
    
    if (dat[i]==1) yat_1[i]<-0
    else yat_1[i]<-yat_1[i]
    
  }
  
  # centro qualità relativa e assoluta-> vedi articolo
  rel_qual<- rel_qual- mean( rel_qual)
  ass_qual<- ass_qual- mean( ass_qual)
  
  #creo variabili delta_i -> come nell'articolo
  delta11<- rel_qual* yat_1
  delta12<- ass_qual* yat_1
  
  delta21<- rel_qual* dat
  delta22<- ass_qual* dat
  
  delta31<- rel_qual* imp
  delta32<- ass_qual* imp
  
  servizioA<-df_A$servizio
  return(as.data.frame(cbind(ID_match, mom_partita, turno, servizioA, yat, rel_qual, ass_qual,
                             yat_1, delta11, delta12,
                             dat, delta21, delta22,
                             imp, delta31, delta32)))
}


df_B_serve<-function(df_A, df_B) {
  
  # restituisce numero di match-> ogni numero si ripete PER TUTTA la durata del match uso dataframe serve_A
  ID_match<-df_B$riga
  # restituisce numero di match-> ogni numero si ripete PER TUTTA la durata del match uso dataframe serve_B
  ID_m<-df_A$riga
  
  turno<-df_B$turno
  
  # punti quando batte A
  p1A<-df_A$puntiA
  p1B<-df_A$puntiB
  
  # punti quando batte B
  p2A<-df_B$puntiA
  p2B<-df_B$puntiB
  
  # game quando batte A
  gA<- df_B$gameA
  gB<- df_B$gameB
  
  # set quando batte A
  set_2A<- df_B$setA
  set_2B<- df_B$setB
  
  # ranking A e B quando batte A
  r2A<- df_B$rankA
  r2B<- df_B$rankB
  
  # punto a punto A e B
  yat<-df_B$vinceA
  ybt<-df_B$vinceB
  
  ybt_1<-y_A_B(yat, ybt)[,4]
  
  # [,1]-> yA-> A fa punto o no (0,1)
  # [,2]-> yA_t-1<- A ha segnato al pto precedente
  # [,3]-> yB-> B fa punto o no (0,1)
  # [,4]-> yB_t-1<- B ha segnato al pto precedente
  
  
  # identifica se siamo all inizio del set -> in qst casi ovviamente per y_t-1 non ha senso
  dat<-d_at(p2A,p2B)
  
  # prob per ogni match che A vinca punto in servizio-> hold service
  # y_A_B(p1A, p1B)[,1] -> serie di pti vinti o no DA SERVIZIO per il giocatore A
  prob_servA<-prob_win_service_A(df_A)
  
  # prob per ogni match che A vinca punto in servizio-> hold service
  # y_A_B(p2A, p2B)[,3] -> serie di pti vinti o no DA SERVIZIO per il giocatore B
  prob_servB<-prob_win_service_B(df_B)
  
  # calcolo importanza del pti t-esimo per ognuno degli m match
  imp<-NULL  
  for (i in 1:length(unique(ID_match))) {
    imp<-c(imp,importanza_punto(df_B[which(ID_match==i),], 
                                prob_servA[i],prob_servB[i]))
    cat(i)
  }
  
  
  # in che momento della partita siamo: formato: [ punti | game | set ]-> [ 15-40|2-4|1-0 ]
  mom_partita<-partita_stringa(df_B)
  
  rel_qual<- round((r2B-r2A),digits = 4)
  
  ass_qual<- round((r2A+r2B),digits = 4)
  
  imp<-round(imp,digits=6)
  
  # identifica se siamo all inizio del set -> in qst casi ovviamente per y_t-1(matrice[,2]) non ha senso-> lo pongo =0
  for (i in 1:length(p2A)) {
    
    if (dat[i]==1) ybt_1[i]<-0
    else ybt_1[i]<-ybt_1[i]
    
  }
  
  # centro qualità relativa e assoluta-> vedi articolo
  rel_qual<- rel_qual- mean( rel_qual)
  ass_qual<- ass_qual- mean( ass_qual)
  
  # creo variabili delta_i -> come nell'articolo
  delta11<- rel_qual* ybt_1
  delta12<- ass_qual* ybt_1
  
  delta21<- rel_qual* dat
  delta22<- ass_qual* dat
  
  delta31<- rel_qual* imp
  delta32<- ass_qual* imp
  
  servizioB<-df_B$servizio
  
  return(as.data.frame(cbind(ID_match, mom_partita, turno, servizioB, ybt, rel_qual, ass_qual,
                             ybt_1, delta11, delta12,
                             dat, delta21, delta22,
                             imp, delta31, delta32)))
  
}



# in seguito userò le markov chain per calcolarmi le diverse probabilità di fare punto al servizio




# per la prob di vincere un GAME partendo da (0,0), 
# ppoint_server= prob di fare punto da servizio
prob_game<-function(ppoint_server) {
  ppoint_ret = 1 - ppoint_server
  STATES = c("HOLD", "BREAK","0-0","0-15","15-0","15-15",
             "30-0","0-30","40-0","30-15",
             "15-30","0-40","40-15","15-40",
             "30-30(DEUCE)","40-30(A-40)","30-40(40-A)" )
  tMat = matrix(0, nrow = 17, ncol = 17, byrow = TRUE)
  rownames(tMat) = STATES
  colnames(tMat) = STATES
  
  tMat["0-0","15-0"] <- ppoint_server
  tMat["15-0","30-0"] <- ppoint_server
  tMat["0-15","15-15"] <- ppoint_server
  tMat["30-0","40-0"] <- ppoint_server
  tMat["15-15","30-15"] <- ppoint_server
  tMat["0-30","15-30"] <- ppoint_server
  tMat["40-0","HOLD"] <- ppoint_server
  tMat["30-15","40-15"] <- ppoint_server
  tMat["40-15","HOLD"] <- ppoint_server
  tMat["30-30(DEUCE)","40-30(A-40)"] <- ppoint_server
  tMat["40-30(A-40)","HOLD"] <- ppoint_server
  tMat["0-40","15-40"] <- ppoint_server
  tMat["15-40","30-40(40-A)"] <- ppoint_server
  tMat["30-40(40-A)","30-30(DEUCE)"] <- ppoint_server
  tMat["15-30","30-30(DEUCE)"] <- ppoint_server
  
  tMat["0-0","0-15"] <- 1 - ppoint_server
  tMat["15-0","15-15"] <- 1 - ppoint_server
  tMat["0-15","0-30"] <- 1 - ppoint_server
  tMat["30-0","30-15"] <- 1 - ppoint_server
  tMat["15-15","15-30"] <- 1 - ppoint_server
  tMat["0-30","0-40"] <- 1 - ppoint_server
  tMat["40-0","40-15"] <- 1 - ppoint_server
  tMat["30-15","30-30(DEUCE)"] <- 1 - ppoint_server
  tMat["40-15","40-30(A-40)"] <- 1 - ppoint_server
  tMat["30-30(DEUCE)","30-40(40-A)"] <- 1 - ppoint_server
  tMat["40-30(A-40)","30-30(DEUCE)"] <- 1 - ppoint_server
  tMat["0-40","BREAK"] <- 1 - ppoint_server
  tMat["15-40","BREAK"] <- 1 - ppoint_server
  tMat["30-40(40-A)","BREAK"] <- 1 - ppoint_server
  tMat["15-30","15-40"] <- 1 - ppoint_server
  
  tMat["BREAK","BREAK"] <- 1
  tMat["HOLD","HOLD"] <- 1
  
  I<-diag(1,2,2)
  R<-tMat[3:17,1:2]
  Q<-tMat[3:17,3:17]
  O<-tMat[1:2,3:17]
  
  M_inf<-(solve(diag(1,15,15)-Q))%*%R
  return(M_inf[1,1])
}

# PROBABILITà di vincere un set partendo da (0,0) -----------
# pA e pB -> prob giocatore A e B di fare punto da servizio
prob_set<- function(pA, pB) {
  phold1<-prob_game(pA)
  phold2<-prob_game(pB)
  
  s0tb <- t(matrix(0, nrow = 54, ncol = 1))
  colnames(s0tb) <- c("0-0","0-1","1-0","1-1",
                      "2-0","0-2","3-0","2-1",
                      "1-2","0-3","4-0","3-1",
                      "2-2","1-3","0-4","5-0",
                      "4-1", "3-2","2-3","1-4",
                      "0-5","5-1","4-2","3-3",
                      "2-4","1-5","5-2","4-3","3-4",
                      "2-5","5-3","4-4","3-5","5-4",
                      "4-5", "5-5","6-5","5-6",
                      "6-6","SETv1","SETv2","6-0",
                      "6-1","6-2","6-3","6-4","4-6",
                      "3-6","2-6","1-6","0-6","7-7","7-6","6-7")
  s0tb[1,'0-0'] <- 1
  ptie1 = resTIE(phold1, phold2, s_tb = s0tb, graph = FALSE)[1, "SETv1"] 
  # restituisce prob di vincere tiebreak da qualsiasi pto della partita in cui mi trovi
  
  STATES = c("SETv1","SETv2","0-0","0-1","1-0","1-1",
             "2-0","0-2","3-0","2-1",
             "1-2","0-3","4-0","3-1",
             "2-2","1-3","0-4","5-0",
             "4-1", "3-2","2-3","1-4",
             "0-5","5-1","4-2","3-3",
             "2-4","1-5","5-2","4-3","3-4",
             "2-5","5-3","4-4","3-5","5-4",
             "4-5", "5-5","6-5","5-6",
             "6-6")
  
  
  
  tMat = matrix(0, nrow = 41, ncol = 41, byrow = TRUE)
  rownames(tMat) = STATES
  colnames(tMat) = STATES
  
  # somma game(a sx) pari -> batte giocatore A
  # ho la prob che il giocatore A tenga o perda il servizio
  tMat["0-0","1-0"] <- phold1
  tMat["2-0","3-0"] <- phold1
  tMat["1-1","2-1"] <- phold1
  tMat["0-2","1-2"] <- phold1
  tMat["4-0","5-0"]  <- phold1
  tMat["3-1","4-1"] <- phold1
  tMat["2-2","3-2"] <- phold1
  tMat["1-3","2-3"] <- phold1
  tMat["0-4","1-4"] <- phold1
  tMat["5-1","SETv1"] <- phold1
  tMat["4-2","5-2"] <- phold1
  tMat["3-3","4-3"] <- phold1
  tMat["2-4","3-4"] <- phold1
  tMat["1-5","2-5"]  <- phold1
  tMat["5-3","SETv1"] <- phold1
  tMat["4-4","5-4"] <- phold1
  tMat["3-5","4-5"] <- phold1
  tMat["5-5","6-5"] <- phold1
  
  
  tMat["0-0","0-1"] <- 1 - phold1
  tMat["2-0","2-1"] <- 1 - phold1
  tMat["1-1","1-2"] <- 1 - phold1
  tMat["0-2","0-3"] <- 1 - phold1
  tMat["4-0","4-1"]  <- 1 - phold1
  tMat["3-1","3-2"] <- 1 - phold1
  tMat["2-2","2-3"] <- 1 - phold1
  tMat["1-3","1-4"] <- 1 - phold1
  tMat["0-4","0-5"] <- 1 - phold1
  tMat["5-1","5-2"] <- 1 - phold1
  tMat["4-2","4-3"] <- 1 - phold1
  tMat["3-3","3-4"] <- 1 - phold1
  tMat["2-4","2-5"] <- 1 - phold1
  tMat["1-5","SETv2"]  <- 1 - phold1
  tMat["5-3","5-4"] <- 1 - phold1
  tMat["4-4","4-5"] <- 1 - phold1
  tMat["3-5","SETv2"] <- 1 - phold1
  tMat["5-5","5-6"] <- 1 - phold1
  
  
  
  # somma game(a sx) dispari -> batte giocatore B
  # ho la prob che il giocatore B tenga o perda il servizio
  tMat["1-0","1-1"] <- phold2
  tMat["0-1","0-2"] <- phold2
  tMat["3-0","3-1"] <- phold2
  tMat["2-1","2-2"] <- phold2
  tMat["1-2","1-3"]  <- phold2
  tMat["0-3","0-4"] <- phold2
  tMat["5-0","5-1"] <- phold2
  tMat["4-1","4-2"] <- phold2
  tMat["3-2","3-3"] <- phold2
  tMat["2-3","2-4"] <- phold2
  tMat["1-4","1-5"] <- phold2
  tMat["0-5","SETv2"] <- phold2
  tMat["5-2","5-3"] <- phold2
  tMat["4-3","4-4"]  <- phold2
  tMat["3-4","3-5"] <- phold2
  tMat["2-5","SETv2"] <- phold2
  tMat["5-4","5-5"] <- phold2
  tMat["4-5","SETv2"] <- phold2
  tMat["5-6","SETv2"] <- phold2
  tMat["6-5","6-6"] <- phold2
  
  
  
  
  tMat["1-0","2-0"] <- 1 - phold2
  tMat["0-1","1-1"] <- 1 - phold2
  tMat["3-0","4-0"] <- 1 - phold2
  tMat["2-1","3-1"] <- 1 - phold2
  tMat["1-2","2-2"]  <- 1 - phold2
  tMat["0-3","1-3"] <- 1 - phold2
  tMat["5-0","SETv1"] <- 1 - phold2
  tMat["4-1","5-1"] <- 1 - phold2
  tMat["3-2","4-2"] <- 1 - phold2
  tMat["2-3","3-3"] <- 1 - phold2
  tMat["1-4","2-4"] <- 1 - phold2
  tMat["0-5","1-5"] <- 1 - phold2
  tMat["5-2","SETv1"] <- 1 - phold2
  tMat["4-3","5-3"]  <- 1 - phold2
  tMat["3-4","4-4"] <- 1 - phold2
  tMat["2-5","3-5"] <- 1 - phold2
  tMat["5-4","SETv1"] <- 1 - phold2
  tMat["4-5","5-5"] <- 1 - phold2
  tMat["5-6","6-6"] <- 1 - phold2
  tMat["6-5","SETv1"] <- 1 - phold2
  
  
  
  tMat["SETv1","SETv1"] <- 1
  tMat["SETv2","SETv2"] <- 1
  
  # da calcolare al di fuori 
  tMat["6-6","SETv1"] <- ptie1
  tMat["6-6","SETv2"] <- 1 - ptie1
  
  tMat
  
  R1<-tMat[3:ncol(tMat),1:2]
  Q1<-tMat[3:ncol(tMat),3:ncol(tMat)]
  
 
  M_inf<-((solve(diag(1,ncol(tMat)-2,ncol(tMat)-2)-Q1))%*%R1)[,1]
  return(M_inf[[1]])
}


# IMPORTANZA DEL PUNTO NEL GAME ----------------

imp_pg<- function(ppoint_srv1, ppoint_srv2){
  ppoint_ret = 1 - ppoint_srv1
  STATES = c("HOLD", "BREAK","0-0","0-15","15-0","15-15",
             "30-0","0-30","40-0","30-15",
             "15-30","0-40","40-15","15-40",
             "30-30(DEUCE)","40-30(A-40)","30-40(40-A)" )
  
  STATES1 = c("SETv1","SETv2","0-0","0-1","1-0","1-1",
              "2-0","0-2","3-0","2-1",
              "1-2","0-3","4-0","3-1",
              "2-2","1-3","0-4","5-0",
              "4-1", "3-2","2-3","1-4",
              "0-5","5-1","4-2","3-3",
              "2-4","1-5","5-2","4-3","3-4",
              "2-5","5-3","4-4","3-5","5-4",
              "4-5", "5-5","6-5","5-6",
              "6-6","6-0",
              "6-1","6-2","6-3","6-4","4-6",
              "3-6","2-6","1-6","0-6","7-7","7-6","6-7")
  
  
  tMat = matrix(0, nrow = 17, ncol = 17, byrow = TRUE)
  rownames(tMat) = STATES
  colnames(tMat) = STATES
  
  tMat["0-0","15-0"] <- ppoint_srv1
  tMat["15-0","30-0"] <- ppoint_srv1
  tMat["0-15","15-15"] <- ppoint_srv1
  tMat["30-0","40-0"] <- ppoint_srv1
  tMat["15-15","30-15"] <- ppoint_srv1
  tMat["0-30","15-30"] <- ppoint_srv1
  tMat["40-0","HOLD"] <- ppoint_srv1
  tMat["30-15","40-15"] <- ppoint_srv1
  tMat["40-15","HOLD"] <- ppoint_srv1
  tMat["30-30(DEUCE)","40-30(A-40)"] <- ppoint_srv1
  tMat["40-30(A-40)","HOLD"] <- ppoint_srv1
  tMat["0-40","15-40"] <- ppoint_srv1
  tMat["15-40","30-40(40-A)"] <- ppoint_srv1
  tMat["30-40(40-A)","30-30(DEUCE)"] <- ppoint_srv1
  tMat["15-30","30-30(DEUCE)"] <- ppoint_srv1
  
  tMat["0-0","0-15"] <- 1 - ppoint_srv1
  tMat["15-0","15-15"] <- 1 - ppoint_srv1
  tMat["0-15","0-30"] <- 1 - ppoint_srv1
  tMat["30-0","30-15"] <- 1 - ppoint_srv1
  tMat["15-15","15-30"] <- 1 - ppoint_srv1
  tMat["0-30","0-40"] <- 1 - ppoint_srv1
  tMat["40-0","40-15"] <- 1 - ppoint_srv1
  tMat["30-15","30-30(DEUCE)"] <- 1 - ppoint_srv1
  tMat["40-15","40-30(A-40)"] <- 1 - ppoint_srv1
  tMat["30-30(DEUCE)","30-40(40-A)"] <- 1 - ppoint_srv1
  tMat["40-30(A-40)","30-30(DEUCE)"] <- 1 - ppoint_srv1
  tMat["0-40","BREAK"] <- 1 - ppoint_srv1
  tMat["15-40","BREAK"] <- 1 - ppoint_srv1
  tMat["30-40(40-A)","BREAK"] <- 1 - ppoint_srv1
  tMat["15-30","15-40"] <- 1 - ppoint_srv1
  
  tMat["BREAK","BREAK"] <- 1
  tMat["HOLD","HOLD"] <- 1
  
  tMat1 = matrix(0, nrow = 54, ncol = 54, byrow = TRUE)
  rownames(tMat1) = STATES1
  colnames(tMat1) = STATES1
  
  tMat1["0-0","1-0"] <- ppoint_srv1
  tMat1["3-0","4-0"] <- ppoint_srv1
  tMat1["2-1","3-1"] <- ppoint_srv1
  tMat1["1-2","2-2"] <- ppoint_srv1
  tMat1["0-3","1-3"]  <- ppoint_srv1
  tMat1["4-0","5-0"] <- ppoint_srv1
  tMat1["3-1","4-1"] <- ppoint_srv1
  tMat1["2-2","3-2"] <- ppoint_srv1
  tMat1["1-3","2-3"] <- ppoint_srv1
  tMat1["0-4","1-4"] <- ppoint_srv1
  tMat1["6-1","SETv1"] <- ppoint_srv1
  tMat1["5-2","6-2"] <- ppoint_srv1
  tMat1["4-3","5-3"] <- ppoint_srv1
  tMat1["3-4","4-4"]  <- ppoint_srv1
  tMat1["2-5","3-5"] <- ppoint_srv1
  tMat1["1-6","2-6"] <- ppoint_srv1
  tMat1["6-2","SETv1"] <- ppoint_srv1
  tMat1["5-3","6-3"] <- ppoint_srv1
  tMat1["4-4","5-4"] <- ppoint_srv1
  tMat1["3-5","4-5"]  <- ppoint_srv1
  tMat1["2-6","3-6"] <- ppoint_srv1
  tMat1["6-5","SETv1"] <- ppoint_srv1
  tMat1["5-6","6-6"] <- ppoint_srv1
  
  
  
  
  tMat1["0-0","0-1"] <- 1 - ppoint_srv1
  tMat1["3-0","3-1"] <- 1 - ppoint_srv1
  tMat1["2-1","2-2"] <- 1 - ppoint_srv1
  tMat1["1-2","1-3"] <- 1 - ppoint_srv1
  tMat1["0-3","0-4"]  <- 1 - ppoint_srv1
  tMat1["4-0","4-1"] <- 1 - ppoint_srv1
  tMat1["3-1","3-2"] <- 1 - ppoint_srv1
  tMat1["2-2","2-3"] <- 1 - ppoint_srv1
  tMat1["1-3","1-4"] <- 1 - ppoint_srv1
  tMat1["0-4","0-5"] <- 1 - ppoint_srv1
  tMat1["6-1","6-2"] <- 1 - ppoint_srv1
  tMat1["5-2","5-3"] <- 1 - ppoint_srv1
  tMat1["4-3","4-4"] <- 1 - ppoint_srv1
  tMat1["3-4","3-5"]  <- 1 - ppoint_srv1
  tMat1["2-5","2-6"] <- 1 - ppoint_srv1
  tMat1["1-6","SETv2"] <- 1 - ppoint_srv1
  tMat1["6-2","6-3"] <- 1 - ppoint_srv1
  tMat1["5-3","5-4"] <- 1 - ppoint_srv1
  tMat1["4-4","4-5"] <- 1 - ppoint_srv1
  tMat1["3-5","3-6"]  <- 1 - ppoint_srv1
  tMat1["2-6","SETv2"] <- 1 - ppoint_srv1
  tMat1["6-5","6-6"] <- 1 - ppoint_srv1
  tMat1["5-6","SETv2"] <- 1 - ppoint_srv1
  
  
  tMat1["1-0","1-1"] <- ppoint_srv2
  tMat1["0-1","0-2"] <- ppoint_srv2
  tMat1["2-0","2-1"] <- ppoint_srv2
  tMat1["1-1","1-2"] <- ppoint_srv2
  tMat1["0-2","0-3"]  <- ppoint_srv2
  tMat1["5-0","5-1"] <- ppoint_srv2
  tMat1["4-1","4-2"] <- ppoint_srv2
  tMat1["3-2","3-3"] <- ppoint_srv2
  tMat1["2-3","2-4"] <- ppoint_srv2
  tMat1["1-4","1-5"] <- ppoint_srv2
  tMat1["0-5","0-6"] <- ppoint_srv2
  tMat1["6-0","6-1"] <- ppoint_srv2
  tMat1["5-1","5-2"]  <- ppoint_srv2
  tMat1["4-2","4-3"] <- ppoint_srv2
  tMat1["3-3","3-4"] <- ppoint_srv2
  tMat1["2-4","2-5"] <- ppoint_srv2
  tMat1["1-5","1-6"] <- ppoint_srv2
  tMat1["0-6","SETv2"] <- ppoint_srv2
  tMat1["6-3","6-4"] <- ppoint_srv2
  tMat1["5-4","5-5"] <- ppoint_srv2
  tMat1["4-5","4-6"] <- ppoint_srv2
  tMat1["3-6","SETv2"] <- ppoint_srv2
  tMat1["6-4","6-5"] <- ppoint_srv2
  tMat1["5-5","5-6"] <- ppoint_srv2
  tMat1["4-6","SETv2"] <- ppoint_srv2
  tMat1["6-7","SETv2"] <- ppoint_srv2
  tMat1["7-6","7-7"] <- ppoint_srv2
  tMat1["7-7","5-6"] <- ppoint_srv2
  
  
  
  
  tMat1["1-0","2-0"] <- 1 - ppoint_srv2
  tMat1["0-1","1-1"] <- 1 - ppoint_srv2
  tMat1["2-0","3-0"] <- 1 - ppoint_srv2
  tMat1["1-1","2-1"] <- 1 - ppoint_srv2
  tMat1["0-2","1-2"]  <- 1 - ppoint_srv2
  tMat1["5-0","6-0"] <- 1 - ppoint_srv2
  tMat1["4-1","5-1"] <- 1 - ppoint_srv2
  tMat1["3-2","4-2"] <- 1 - ppoint_srv2
  tMat1["2-3","3-3"] <- 1 - ppoint_srv2
  tMat1["1-4","2-4"] <- 1 - ppoint_srv2
  tMat1["0-5","1-5"] <- 1 - ppoint_srv2
  tMat1["6-0","SETv1"] <- 1 - ppoint_srv2
  tMat1["5-1","6-1"]  <- 1 - ppoint_srv2
  tMat1["4-2","5-2"] <- 1 - ppoint_srv2
  tMat1["3-3","4-3"] <- 1 - ppoint_srv2
  tMat1["2-4","3-4"] <- 1 - ppoint_srv2
  tMat1["1-5","2-5"] <- 1 - ppoint_srv2
  tMat1["0-6","1-6"] <- 1 - ppoint_srv2
  tMat1["6-3","SETv1"] <- 1 - ppoint_srv2
  tMat1["5-4","6-4"] <- 1 - ppoint_srv2
  tMat1["4-5","5-5"] <- 1 - ppoint_srv2
  tMat1["3-6","4-6"] <- 1 - ppoint_srv2
  tMat1["6-4","SETv1"] <- 1 - ppoint_srv2
  tMat1["5-5","6-5"] <- 1 - ppoint_srv2
  tMat1["4-6","5-6"] <- 1 - ppoint_srv2
  tMat1["6-7","7-7"] <- 1 - ppoint_srv2
  tMat1["7-6","SETv1"] <- 1 - ppoint_srv2
  tMat1["7-7","6-5"] <- 1 - ppoint_srv2
  
  
  
  
  # IN CASO DI TIE BREAK
  p1_66 <- (((ppoint_srv1)*(1-ppoint_srv2))/((ppoint_srv1)*(1-ppoint_srv2) + (ppoint_srv2)*(1-ppoint_srv1)))
  tMat1["6-6","SETv1"] <- p1_66
  tMat1["6-6","SETv2"] <- 1 - p1_66
  
  
  
  
  tMat1["SETv1","SETv1"] <- 1
  tMat1["SETv2","SETv2"] <- 1
  
  
  I1<-diag(1,2,2)
  R1<-tMat1[3:54,1:2]
  Q1<-tMat1[3:54,3:54]
  O1<-tMat1[1:2,3:54]
  
  
  I<-diag(1,2,2)
  R<-tMat[3:17,1:2]
  Q<-tMat[3:17,3:17]
  O<-tMat[1:2,3:17]
  
  M_inf<-(solve(diag(1,15,15)-Q))%*%R
  
  M_inf1<-((solve(diag(1,ncol(tMat1)-2,ncol(tMat1)-2)-Q1))%*%R1)[,1]
  
  colrig = c("0", "15","30","40","Adv")
  
  colrig1= c("0","1","2","3","4","5","6","7")
  
  mat_imp = matrix(0, nrow = 5, ncol = 5, byrow = TRUE)
  rownames(mat_imp) = colrig
  colnames(mat_imp) = colrig
  
  mat_imp1 = matrix(0, nrow = 8, ncol = 8, byrow = TRUE)
  rownames(mat_imp1) = colrig1
  colnames(mat_imp1) = colrig1
  
  # gi(a + 1, b), the probability that the server wins the game given that he or she wins the point;
  # gi(a, b + 1), the probability that the server wins the game given that he or she loses the point
  
  mat_imp["0","0"]<-M_inf[3,1]-M_inf[2,1] #prob(15,0)-prob(0,15) e cosi via
  mat_imp["15","0"]<-M_inf[5,1]-M_inf[4,1]
  mat_imp["0","15"]<-M_inf[4,1]-M_inf[6,1]
  mat_imp["15","15"]<-M_inf[8,1]-M_inf[9,1]
  mat_imp["30","0"]<-M_inf[7,1]-M_inf[8,1]
  mat_imp["40","0"]<-1-M_inf[11,1]
  mat_imp["0","30"]<-M_inf[9,1]-M_inf[10,1]
  mat_imp["0","40"]<-M_inf[12,1]
  mat_imp["30","15"]<-M_inf[11,1]-M_inf[13,1]
  mat_imp["15","30"]<-M_inf[13,1]-M_inf[12,1]
  mat_imp["40","15"]<-1-M_inf[14,1]
  mat_imp["15","40"]<-M_inf[15,1]
  mat_imp["30","30"]<-M_inf[14,1]-M_inf[15,1]
  mat_imp["40","40"]<-M_inf[14,1]-M_inf[15,1]
  mat_imp["30","40"]<-M_inf[13,1]
  mat_imp["40","30"]<-1-M_inf[13,1]
  mat_imp["Adv","40"]<-1-M_inf[13,1]
  mat_imp["40","Adv"]<-M_inf[13,1] # prob(40,40)-prob(vinceB)
  
  # tie- break
  mat_imp1["0","0"]<-M_inf1["1-0"]-M_inf1["0-1"] #prob(1,0)-prob(0,1) e cosi via
  mat_imp1["1","0"]<-M_inf1["2-0"]-M_inf1["1-1"]
  mat_imp1["0","1"]<-M_inf1["1-1"]-M_inf1["0-2"]
  mat_imp1["1","1"]<-M_inf1["2-1"]-M_inf1["1-2"]
  mat_imp1["2","0"]<-M_inf1["3-0"]-M_inf1["2-1"]
  mat_imp1["3","0"]<-M_inf1["4-0"]-M_inf1["3-1"]
  mat_imp1["0","2"]<-M_inf1["1-2"]-M_inf1["0-3"]
  mat_imp1["0","3"]<-M_inf1["1-3"]-M_inf1["0-4"]
  mat_imp1["2","1"]<-M_inf1["3-1"]-M_inf1["2-2"]
  mat_imp1["1","2"]<-M_inf1["2-2"]-M_inf1["1-3"]
  mat_imp1["3","1"]<-M_inf1["4-1"]-M_inf1["3-2"]
  mat_imp1["1","3"]<-M_inf1["2-3"]-M_inf1["1-4"]
  mat_imp1["2","2"]<-M_inf1["3-2"]-M_inf1["2-3"]
  mat_imp1["3","3"]<-M_inf1["4-3"]-M_inf1["3-4"]
  mat_imp1["3","2"]<-M_inf1["4-2"]-M_inf1["3-3"]
  mat_imp1["2","3"]<-M_inf1["3-3"]-M_inf1["2-4"]
  mat_imp1["3","3"]<-M_inf1["4-3"]-M_inf1["3-4"]
  mat_imp1["0","4"]<-M_inf1["1-4"]-M_inf1["0-5"]
  mat_imp1["4","0"]<-M_inf1["5-0"]-M_inf1["4-1"]
  mat_imp1["4","1"]<-M_inf1["5-1"]-M_inf1["4-2"]
  mat_imp1["1","4"]<-M_inf1["2-4"]-M_inf1["1-5"]
  mat_imp1["4","2"]<-M_inf1["5-2"]-M_inf1["4-3"]
  mat_imp1["2","4"]<-M_inf1["3-4"]-M_inf1["2-5"]
  mat_imp1["4","3"]<-M_inf1["5-3"]-M_inf1["4-4"]
  mat_imp1["3","4"]<-M_inf1["4-4"]-M_inf1["3-5"]
  mat_imp1["4","4"]<-M_inf1["5-4"]-M_inf1["4-5"]
  mat_imp1["5","0"]<-M_inf1["6-0"]-M_inf1["5-1"]
  mat_imp1["5","1"]<-M_inf1["6-1"]-M_inf1["5-2"]
  mat_imp1["5","2"]<-M_inf1["6-2"]-M_inf1["5-3"]
  mat_imp1["5","3"]<-M_inf1["6-3"]-M_inf1["5-4"]
  mat_imp1["5","4"]<-M_inf1["6-4"]-M_inf1["5-5"]
  mat_imp1["5","5"]<-M_inf1["6-5"]-M_inf1["5-6"]
  mat_imp1["0","5"]<-M_inf1["1-5"]-M_inf1["0-6"]
  mat_imp1["1","5"]<-M_inf1["2-5"]-M_inf1["1-6"]
  mat_imp1["2","5"]<-M_inf1["3-5"]-M_inf1["2-6"]
  mat_imp1["3","5"]<-M_inf1["4-5"]-M_inf1["3-6"]
  mat_imp1["4","5"]<-M_inf1["5-5"]-M_inf1["4-6"]
  mat_imp1["6","0"]<-1-M_inf1["6-1"]
  mat_imp1["6","1"]<-1-M_inf1["6-2"]
  mat_imp1["6","2"]<-1-M_inf1["6-3"]
  mat_imp1["6","3"]<-1-M_inf1["6-4"]
  mat_imp1["6","4"]<-1-M_inf1["6-5"]
  mat_imp1["6","5"]<-1-M_inf1["6-6"]
  mat_imp1["0","6"]<-M_inf1["1-6"]
  mat_imp1["1","6"]<-M_inf1["2-6"]
  mat_imp1["2","6"]<-M_inf1["3-6"]
  mat_imp1["3","6"]<-M_inf1["4-6"]
  mat_imp1["4","6"]<-M_inf1["5-6"]
  mat_imp1["5","6"]<-M_inf1["6-6"]
  mat_imp1["6","6"]<-M_inf1["7-6"]-M_inf1["6-7"]
  mat_imp1["7","6"]<-1-M_inf1["7-7"]
  mat_imp1["6","7"]<-M_inf1["7-7"]
  mat_imp1["7","0"]<-1-M_inf1["6-1"]
  mat_imp1["7","1"]<-1-M_inf1["6-2"]
  mat_imp1["7","2"]<-1-M_inf1["6-3"]
  mat_imp1["7","3"]<-1-M_inf1["6-4"]
  mat_imp1["7","4"]<-1-M_inf1["6-5"]
  mat_imp1["7","5"]<-1-M_inf1["6-6"]
  mat_imp1["0","7"]<-M_inf1["1-6"]
  mat_imp1["1","7"]<-M_inf1["2-6"]
  mat_imp1["2","7"]<-M_inf1["3-6"]
  mat_imp1["3","7"]<-M_inf1["4-6"]
  mat_imp1["4","7"]<-M_inf1["5-6"]
  mat_imp1["5","7"]<-M_inf1["6-6"]
  
  
  
  vett<-NULL
  for (i in 1:length(colrig)) {
    for (j in 1:length(colrig)){
      vett<-c(vett,paste(colrig[j],colrig[i], sep="-"))
    }
  }
  
  vett1<-NULL
  for (i in 1:length(colrig1)) {
    for (j in 1:length(colrig1)){
      vett1<-c(vett1,paste(colrig1[j],colrig1[i], sep="-"))
    }
  }
  imp2<-as.matrix(as.vector(round(mat_imp1,digits=3)))
  imp1<-as.matrix(as.vector(round(mat_imp,digits=3)))
  #imp<-rbind(imp1,imp2)
  rownames(imp1)<-vett
  imp1<-imp1[-which(imp1[,]==0),]
  imp1<-as.matrix(imp1,nrow(imp1),1)
  colnames(imp1)<- "imp_pg"
  
  
  
  rownames(imp2)<-vett1
  imp2<-imp2[-which(imp2[,]==0),]
  imp2<-as.matrix(imp2,nrow(imp2),1)
  colnames(imp2)<- "imp_pg"
  return(list(imp1,imp2))
}


# funzione per calcolare le diverse prob per A e B nel tiebreak-----

MCtb2 <- function(ppoint_srv1, ppoint_srv2) {
  #this fuction omits the possibility of infinit deuces : useful to compute only win probs
  
  
  STATES = c("0-0","0-1","1-0","1-1",
             "2-0","0-2","3-0","2-1",
             "1-2","0-3","4-0","3-1",
             "2-2","1-3","0-4","5-0",
             "4-1", "3-2","2-3","1-4",
             "0-5","5-1","4-2","3-3",
             "2-4","1-5","5-2","4-3","3-4",
             "2-5","5-3","4-4","3-5","5-4",
             "4-5", "5-5","6-5","5-6",
             "6-6","SETv1","SETv2","6-0",
             "6-1","6-2","6-3","6-4","4-6",
             "3-6","2-6","1-6","0-6","7-7","7-6","6-7")
  
  
  
  tMat = matrix(0, nrow = 54, ncol = 54, byrow = TRUE)
  rownames(tMat) = STATES
  colnames(tMat) = STATES
  
  tMat["0-0","1-0"] <- ppoint_srv1
  tMat["3-0","4-0"] <- ppoint_srv1
  tMat["2-1","3-1"] <- ppoint_srv1
  tMat["1-2","2-2"] <- ppoint_srv1
  tMat["0-3","1-3"]  <- ppoint_srv1
  tMat["4-0","5-0"] <- ppoint_srv1
  tMat["3-1","4-1"] <- ppoint_srv1
  tMat["2-2","3-2"] <- ppoint_srv1
  tMat["1-3","2-3"] <- ppoint_srv1
  tMat["0-4","1-4"] <- ppoint_srv1
  tMat["6-1","SETv1"] <- ppoint_srv1
  tMat["5-2","6-2"] <- ppoint_srv1
  tMat["4-3","5-3"] <- ppoint_srv1
  tMat["3-4","4-4"]  <- ppoint_srv1
  tMat["2-5","3-5"] <- ppoint_srv1
  tMat["1-6","2-6"] <- ppoint_srv1
  tMat["6-2","SETv1"] <- ppoint_srv1
  tMat["5-3","6-3"] <- ppoint_srv1
  tMat["4-4","5-4"] <- ppoint_srv1
  tMat["3-5","4-5"]  <- ppoint_srv1
  tMat["2-6","3-6"] <- ppoint_srv1
  tMat["6-5","SETv1"] <- ppoint_srv1
  tMat["5-6","6-6"] <- ppoint_srv1
  
  
  
  
  tMat["0-0","0-1"] <- 1 - ppoint_srv1
  tMat["3-0","3-1"] <- 1 - ppoint_srv1
  tMat["2-1","2-2"] <- 1 - ppoint_srv1
  tMat["1-2","1-3"] <- 1 - ppoint_srv1
  tMat["0-3","0-4"]  <- 1 - ppoint_srv1
  tMat["4-0","4-1"] <- 1 - ppoint_srv1
  tMat["3-1","3-2"] <- 1 - ppoint_srv1
  tMat["2-2","2-3"] <- 1 - ppoint_srv1
  tMat["1-3","1-4"] <- 1 - ppoint_srv1
  tMat["0-4","0-5"] <- 1 - ppoint_srv1
  tMat["6-1","6-2"] <- 1 - ppoint_srv1
  tMat["5-2","5-3"] <- 1 - ppoint_srv1
  tMat["4-3","4-4"] <- 1 - ppoint_srv1
  tMat["3-4","3-5"]  <- 1 - ppoint_srv1
  tMat["2-5","2-6"] <- 1 - ppoint_srv1
  tMat["1-6","SETv2"] <- 1 - ppoint_srv1
  tMat["6-2","6-3"] <- 1 - ppoint_srv1
  tMat["5-3","5-4"] <- 1 - ppoint_srv1
  tMat["4-4","4-5"] <- 1 - ppoint_srv1
  tMat["3-5","3-6"]  <- 1 - ppoint_srv1
  tMat["2-6","SETv2"] <- 1 - ppoint_srv1
  tMat["6-5","6-6"] <- 1 - ppoint_srv1
  tMat["5-6","SETv2"] <- 1 - ppoint_srv1
  
  
  tMat["1-0","1-1"] <- ppoint_srv2
  tMat["0-1","0-2"] <- ppoint_srv2
  tMat["2-0","2-1"] <- ppoint_srv2
  tMat["1-1","1-2"] <- ppoint_srv2
  tMat["0-2","0-3"]  <- ppoint_srv2
  tMat["5-0","5-1"] <- ppoint_srv2
  tMat["4-1","4-2"] <- ppoint_srv2
  tMat["3-2","3-3"] <- ppoint_srv2
  tMat["2-3","2-4"] <- ppoint_srv2
  tMat["1-4","1-5"] <- ppoint_srv2
  tMat["0-5","0-6"] <- ppoint_srv2
  tMat["6-0","6-1"] <- ppoint_srv2
  tMat["5-1","5-2"]  <- ppoint_srv2
  tMat["4-2","4-3"] <- ppoint_srv2
  tMat["3-3","3-4"] <- ppoint_srv2
  tMat["2-4","2-5"] <- ppoint_srv2
  tMat["1-5","1-6"] <- ppoint_srv2
  tMat["0-6","SETv2"] <- ppoint_srv2
  tMat["6-3","6-4"] <- ppoint_srv2
  tMat["5-4","5-5"] <- ppoint_srv2
  tMat["4-5","4-6"] <- ppoint_srv2
  tMat["3-6","SETv2"] <- ppoint_srv2
  tMat["6-4","6-5"] <- ppoint_srv2
  tMat["5-5","5-6"] <- ppoint_srv2
  tMat["4-6","SETv2"] <- ppoint_srv2
  tMat["6-7","SETv2"] <- ppoint_srv2
  tMat["7-6","7-7"] <- ppoint_srv2
  tMat["7-7","5-6"] <- ppoint_srv2
  
  
  
  
  tMat["1-0","2-0"] <- 1 - ppoint_srv2
  tMat["0-1","1-1"] <- 1 - ppoint_srv2
  tMat["2-0","3-0"] <- 1 - ppoint_srv2
  tMat["1-1","2-1"] <- 1 - ppoint_srv2
  tMat["0-2","1-2"]  <- 1 - ppoint_srv2
  tMat["5-0","6-0"] <- 1 - ppoint_srv2
  tMat["4-1","5-1"] <- 1 - ppoint_srv2
  tMat["3-2","4-2"] <- 1 - ppoint_srv2
  tMat["2-3","3-3"] <- 1 - ppoint_srv2
  tMat["1-4","2-4"] <- 1 - ppoint_srv2
  tMat["0-5","1-5"] <- 1 - ppoint_srv2
  tMat["6-0","SETv1"] <- 1 - ppoint_srv2
  tMat["5-1","6-1"]  <- 1 - ppoint_srv2
  tMat["4-2","5-2"] <- 1 - ppoint_srv2
  tMat["3-3","4-3"] <- 1 - ppoint_srv2
  tMat["2-4","3-4"] <- 1 - ppoint_srv2
  tMat["1-5","2-5"] <- 1 - ppoint_srv2
  tMat["0-6","1-6"] <- 1 - ppoint_srv2
  tMat["6-3","SETv1"] <- 1 - ppoint_srv2
  tMat["5-4","6-4"] <- 1 - ppoint_srv2
  tMat["4-5","5-5"] <- 1 - ppoint_srv2
  tMat["3-6","4-6"] <- 1 - ppoint_srv2
  tMat["6-4","SETv1"] <- 1 - ppoint_srv2
  tMat["5-5","6-5"] <- 1 - ppoint_srv2
  tMat["4-6","5-6"] <- 1 - ppoint_srv2
  tMat["6-7","7-7"] <- 1 - ppoint_srv2
  tMat["7-6","SETv1"] <- 1 - ppoint_srv2
  tMat["7-7","6-5"] <- 1 - ppoint_srv2
  
  
  
  
  
  p1_66 <- (((ppoint_srv1)*(1-ppoint_srv2))/((ppoint_srv1)*(1-ppoint_srv2) + (ppoint_srv2)*(1-ppoint_srv1)))
  tMat["6-6","SETv1"] <- p1_66
  tMat["6-6","SETv2"] <- 1 - p1_66
  
  
  
  
  tMat["SETv1","SETv1"] <- 1
  tMat["SETv2","SETv2"] <- 1
  
  tMat
  
  
  MC_set <- new("markovchain", states = STATES,
                transitionMatrix = tMat, name ="MCset" )
  return(MC_set)
  
}



resTIE <- function (ppoint_srv1, ppoint_srv2, s_tb, graph = FALSE){
  # This function computes outcomes probabilities for a tie-break
  # ppoint_srv1 : probability that the first player wins a point on his serve. Between 0 and 1
  # ppoint_srv2 : probability that the second player wins a point on his serve. Between 0 and 1
  # s_tb : state of the tie-break in a Markov sense. If you want to start from 0-0 just take s0tb
  # Otherwise just put 1 in the score you want to start from, e.g. if I want to compute probabilities from 2-3
  # I just need to set this : s_tb <- s0tb;s_tb[1,'0-0'] <- 0; s_tb[1, '2-3'] <- 1
  # graph : should we display the Markov Chain representation ? boolean
  #
  # Output : Markov matrix containing stable states (SETv1 or SETv2) probabilities
  MC_tb <- MCtb2(ppoint_srv1, ppoint_srv2)
  if(graph){plot(MC_tb, edge.arrow.size=0.5, vertex.size = 15, main = "Tie-break model")}
  resTIE <- s_tb*(MC_tb ^ 1000)
  #print(resTIE)
  return(resTIE)
}

# imp_gs mi restituisce prob condizionate....
# io devo moltiplicare output per la probabilità che l'nesimo game sia presente nel dataset
# x es: l(7) = (num di volte che viene giocato game 7)/ (tot game giocati)


# calcolo della probabilità che nel dataset ci sia il game numero n! 
prob_n_occurs<-function(gameA,gameB) {
  
  somma_game<- gameA+gameB
  
  vett<-NULL
  for (i in 2:length(somma_game)) {
    if (somma_game[i-1]!=somma_game[i]) {
      vett[i]=somma_game[i] 
    }
    else {vett[i]=0}
  }
  vett<-vett[-1] 
  vett<-vett[-which(vett==0)]
  
  l_n<-NULL
  
  for (i in 1:13) {
    
    l_n[i]<- length(vett[which(vett==i)])/length(vett[which(vett==1)])
    
  }
  
  return(l_n)
  
}


# importanza del game nel set ------------------
imp_gs<-function(probA,probB) { #, gameA, gameB)  # probA e probB sono le probabilità di far punto a servizio
  phold1<-prob_game(probA)
  phold2<-prob_game(probB)
  
  s0tb <- t(matrix(0, nrow = 54, ncol = 1))
  colnames(s0tb) <- c("0-0","0-1","1-0","1-1",
                      "2-0","0-2","3-0","2-1",
                      "1-2","0-3","4-0","3-1",
                      "2-2","1-3","0-4","5-0",
                      "4-1", "3-2","2-3","1-4",
                      "0-5","5-1","4-2","3-3",
                      "2-4","1-5","5-2","4-3","3-4",
                      "2-5","5-3","4-4","3-5","5-4",
                      "4-5", "5-5","6-5","5-6",
                      "6-6","SETv1","SETv2","6-0",
                      "6-1","6-2","6-3","6-4","4-6",
                      "3-6","2-6","1-6","0-6","7-7","7-6","6-7")
  s0tb[1,'0-0'] <- 1
  ptie1 = resTIE(phold1, phold2, s_tb = s0tb,graph = FALSE)[1, "SETv1"]
  
  STATES = c("SETv1","SETv2","0-0","0-1","1-0","1-1",
             "2-0","0-2","3-0","2-1",
             "1-2","0-3","4-0","3-1",
             "2-2","1-3","0-4","5-0",
             "4-1", "3-2","2-3","1-4",
             "0-5","5-1","4-2","3-3",
             "2-4","1-5","5-2","4-3","3-4",
             "2-5","5-3","4-4","3-5","5-4",
             "4-5", "5-5","6-5","5-6",
             "6-6")
  
  
  
  tMat = matrix(0, nrow = 41, ncol = 41, byrow = TRUE)
  rownames(tMat) = STATES
  colnames(tMat) = STATES
  
  # somma game(a sx) pari -> batte giocatore A
  # ho la prob che il giocatore A tenga o perda il servizio
  tMat["0-0","1-0"] <- phold1
  tMat["2-0","3-0"] <- phold1
  tMat["1-1","2-1"] <- phold1
  tMat["0-2","1-2"] <- phold1
  tMat["4-0","5-0"]  <- phold1
  tMat["3-1","4-1"] <- phold1
  tMat["2-2","3-2"] <- phold1
  tMat["1-3","2-3"] <- phold1
  tMat["0-4","1-4"] <- phold1
  tMat["5-1","SETv1"] <- phold1
  tMat["4-2","5-2"] <- phold1
  tMat["3-3","4-3"] <- phold1
  tMat["2-4","3-4"] <- phold1
  tMat["1-5","2-5"]  <- phold1
  tMat["5-3","SETv1"] <- phold1
  tMat["4-4","5-4"] <- phold1
  tMat["3-5","4-5"] <- phold1
  tMat["5-5","6-5"] <- phold1
  
  
  tMat["0-0","0-1"] <- 1 - phold1
  tMat["2-0","2-1"] <- 1 - phold1
  tMat["1-1","1-2"] <- 1 - phold1
  tMat["0-2","0-3"] <- 1 - phold1
  tMat["4-0","4-1"]  <- 1 - phold1
  tMat["3-1","3-2"] <- 1 - phold1
  tMat["2-2","2-3"] <- 1 - phold1
  tMat["1-3","1-4"] <- 1 - phold1
  tMat["0-4","0-5"] <- 1 - phold1
  tMat["5-1","5-2"] <- 1 - phold1
  tMat["4-2","4-3"] <- 1 - phold1
  tMat["3-3","3-4"] <- 1 - phold1
  tMat["2-4","2-5"] <- 1 - phold1
  tMat["1-5","SETv2"]  <- 1 - phold1
  tMat["5-3","5-4"] <- 1 - phold1
  tMat["4-4","4-5"] <- 1 - phold1
  tMat["3-5","SETv2"] <- 1 - phold1
  tMat["5-5","5-6"] <- 1 - phold1
  
  
  
  # somma game(a sx) dispari -> batte giocatore B
  # ho la prob che il giocatore B tenga o perda il servizio
  tMat["1-0","1-1"] <- phold2
  tMat["0-1","0-2"] <- phold2
  tMat["3-0","3-1"] <- phold2
  tMat["2-1","2-2"] <- phold2
  tMat["1-2","1-3"]  <- phold2
  tMat["0-3","0-4"] <- phold2
  tMat["5-0","5-1"] <- phold2
  tMat["4-1","4-2"] <- phold2
  tMat["3-2","3-3"] <- phold2
  tMat["2-3","2-4"] <- phold2
  tMat["1-4","1-5"] <- phold2
  tMat["0-5","SETv2"] <- phold2
  tMat["5-2","5-3"] <- phold2
  tMat["4-3","4-4"]  <- phold2
  tMat["3-4","3-5"] <- phold2
  tMat["2-5","SETv2"] <- phold2
  tMat["5-4","5-5"] <- phold2
  tMat["4-5","SETv2"] <- phold2
  tMat["5-6","SETv2"] <- phold2
  tMat["6-5","6-6"] <- phold2
  
  
  
  
  tMat["1-0","2-0"] <- 1 - phold2
  tMat["0-1","1-1"] <- 1 - phold2
  tMat["3-0","4-0"] <- 1 - phold2
  tMat["2-1","3-1"] <- 1 - phold2
  tMat["1-2","2-2"]  <- 1 - phold2
  tMat["0-3","1-3"] <- 1 - phold2
  tMat["5-0","SETv1"] <- 1 - phold2
  tMat["4-1","5-1"] <- 1 - phold2
  tMat["3-2","4-2"] <- 1 - phold2
  tMat["2-3","3-3"] <- 1 - phold2
  tMat["1-4","2-4"] <- 1 - phold2
  tMat["0-5","1-5"] <- 1 - phold2
  tMat["5-2","SETv1"] <- 1 - phold2
  tMat["4-3","5-3"]  <- 1 - phold2
  tMat["3-4","4-4"] <- 1 - phold2
  tMat["2-5","3-5"] <- 1 - phold2
  tMat["5-4","SETv1"] <- 1 - phold2
  tMat["4-5","5-5"] <- 1 - phold2
  tMat["5-6","6-6"] <- 1 - phold2
  tMat["6-5","SETv1"] <- 1 - phold2
  
  
  
  tMat["SETv1","SETv1"] <- 1
  tMat["SETv2","SETv2"] <- 1
  
  # da calcolare al di fuori 
  tMat["6-6","SETv1"] <- ptie1
  tMat["6-6","SETv2"] <- 1 - ptie1
  
  tMat
  
  # per calcolare M_inf-> tMat^Inf -> articolo Analyze Wimbledon
  R1<-tMat[3:ncol(tMat),1:2]
  Q1<-tMat[3:ncol(tMat),3:ncol(tMat)]
  
  # si(a + 1, b), the probability that the server wins the set given that he or she wins the game;
  # si(a, b + 1), the probability that the server wins the set given that he or she loses the game
  
  M_inf<-((solve(diag(1,ncol(tMat)-2,ncol(tMat)-2)-Q1))%*%R1)[,1]
  
  colrig = c("0", "1","2" ,"3", "4" , "5" , "6")
  
  mat_imp = matrix(0, nrow = 7, ncol = 7, byrow = TRUE)
  rownames(mat_imp) = colrig
  colnames(mat_imp) = colrig
  
  mat_imp["0","0"]<-M_inf["1-0"]-M_inf["0-1"] #prob(1,0)-prob(0,1) e cosi via
  mat_imp["1","0"]<-M_inf["2-0"]-M_inf["1-1"]
  mat_imp["0","1"]<-M_inf["1-1"]-M_inf["0-2"]
  mat_imp["1","1"]<-M_inf["2-1"]-M_inf["1-2"]
  mat_imp["2","0"]<-M_inf["3-0"]-M_inf["2-1"]
  mat_imp["3","0"]<-M_inf["4-0"]-M_inf["3-1"]
  mat_imp["0","2"]<-M_inf["1-2"]-M_inf["0-3"]
  mat_imp["0","3"]<-M_inf["1-3"]-M_inf["0-4"]
  mat_imp["2","1"]<-M_inf["3-1"]-M_inf["2-2"]
  mat_imp["1","2"]<-M_inf["2-2"]-M_inf["1-3"]
  mat_imp["3","1"]<-M_inf["4-1"]-M_inf["3-2"]
  mat_imp["1","3"]<-M_inf["2-3"]-M_inf["1-4"]
  mat_imp["2","2"]<-M_inf["3-2"]-M_inf["2-3"]
  mat_imp["3","3"]<-M_inf["4-3"]-M_inf["3-4"]
  mat_imp["3","2"]<-M_inf["4-2"]-M_inf["3-3"]
  mat_imp["2","3"]<-M_inf["3-3"]-M_inf["2-4"]
  mat_imp["3","3"]<-M_inf["4-3"]-M_inf["3-4"]
  mat_imp["0","4"]<-M_inf["1-4"]-M_inf["0-5"]
  mat_imp["4","0"]<-M_inf["5-0"]-M_inf["4-1"]
  mat_imp["4","1"]<-M_inf["5-1"]-M_inf["4-2"]
  mat_imp["1","4"]<-M_inf["2-4"]-M_inf["1-5"]
  mat_imp["4","2"]<-M_inf["5-2"]-M_inf["4-3"]
  mat_imp["2","4"]<-M_inf["3-4"]-M_inf["2-5"]
  mat_imp["4","3"]<-M_inf["5-3"]-M_inf["4-4"]
  mat_imp["3","4"]<-M_inf["4-4"]-M_inf["3-5"]
  mat_imp["4","4"]<-M_inf["5-4"]-M_inf["4-5"]
  mat_imp["5","0"]<-1-M_inf["5-1"]
  mat_imp["5","1"]<-1-M_inf["5-2"]
  mat_imp["5","2"]<-1-M_inf["5-3"]
  mat_imp["5","3"]<-1-M_inf["5-4"]
  mat_imp["5","4"]<-1-M_inf["5-5"]
  mat_imp["5","5"]<-M_inf["6-5"]-M_inf["5-6"]
  mat_imp["0","5"]<-M_inf["1-5"]
  mat_imp["1","5"]<-M_inf["2-5"]
  mat_imp["2","5"]<-M_inf["3-5"]
  mat_imp["3","5"]<-M_inf["4-5"]
  mat_imp["4","5"]<-M_inf["5-5"]
  mat_imp["6","5"]<-1-M_inf["6-6"]
  mat_imp["5","6"]<-M_inf["6-6"]
  mat_imp["6","6"]<-1
  

  vett<-NULL
  for (i in 1:length(colrig)) {
    for (j in 1:length(colrig)){
      vett<-c(vett,paste(colrig[j],colrig[i], sep="-"))
    }
  }

  imp<-as.matrix(as.vector(round(mat_imp,digits=3)))
  rownames(imp)<-vett
  colnames(imp)<- "imp_gs"
  imp<-as.matrix(imp[-c(7,14,21,28,35,43:47),])
  return(imp)

}



# IMPORTANZA DEL SET NEL MATCH ------------

imp_sm<- function(probA,probB) {
  pset_v1<-prob_set(probA,probB)
  pset_v2 = 1 - pset_v1
  STATES = c("V1","V2","0-0","0-1","1-0","1-1","2-0","0-2","2-1","1-2","2-2")
  tMat = matrix(0, nrow = 11, ncol = 11, byrow = TRUE)
  rownames(tMat) = STATES
  colnames(tMat) = STATES

  tMat["0-0","1-0"] <- pset_v1
  tMat["1-0","2-0"] <- pset_v1
  tMat["1-1","2-1"] <- pset_v1
  tMat["0-1", "1-1"] <- pset_v1
  tMat["2-2","V1"] <- pset_v1
  tMat["0-2","1-2"] <- pset_v1
  tMat["1-2","2-2"] <- pset_v1
  tMat["2-0","V1"] <- pset_v1
  tMat["2-1","V1"] <- pset_v1
  
  
  tMat["0-0","0-1"] <- pset_v2
  tMat["0-1","0-2"] <- pset_v2
  tMat["1-0","1-1"] <- pset_v2
  tMat["1-1","1-2"] <- pset_v2
  tMat["2-2","V2"] <- pset_v2
  tMat["2-0","2-1"] <- pset_v2
  tMat["2-1","2-2"] <- pset_v2
  tMat["0-2","V2"] <- pset_v2
  tMat["1-2","V2"] <- pset_v2

  
  tMat["V1","V1"] <- 1
  tMat["V2","V2"] <- 1
  
  tMat
  
  R2<-tMat[3:ncol(tMat),1:2]
  Q2<-tMat[3:ncol(tMat),3:ncol(tMat)]
  
  M_inf<-((solve(diag(1,ncol(tMat)-2,ncol(tMat)-2)-Q2))%*%R2)[,1]
  
  colrig = c("0", "1", "2")
  
  mat_imp = matrix(0, nrow = 3, ncol = 3, byrow = TRUE)
  rownames(mat_imp) = colrig
  colnames(mat_imp) = colrig
  
  mat_imp["0","0"]<-M_inf["1-0"]-M_inf["0-1"] #prob(1,0)-prob(0,1) e cosi via
  mat_imp["1","0"]<-M_inf["2-0"]-M_inf["1-1"]
  mat_imp["0","1"]<-M_inf["1-1"]-M_inf["0-2"]
  mat_imp["1","1"]<-M_inf["2-1"]-M_inf["1-2"]
  mat_imp["2","1"]<-1-M_inf["2-2"]
  mat_imp["1","2"]<-M_inf["2-2"]
  mat_imp["2","0"]<-1-M_inf["2-1"]
  mat_imp["0","2"]<-M_inf["1-2"]
  mat_imp["2","2"]<-1
  
  # creo vettore per nomi di riga della matrice
  vett<-NULL
  for (i in 1:length(colrig)) {
    for (j in 1:length(colrig)){
      vett<-c(vett,paste(colrig[j],colrig[i], sep="-"))
    }
  }
  
  imp<-as.matrix(as.vector(round(mat_imp,digits=3)))
  rownames(imp)<-vett
  colnames(imp)<- "imp_sm"
  
  return(imp)
}




# importanza del punto nel match!!! --------
# moltiplico tra loro imp_pg*imp_gs*imp_sm

# avrò, date le prob di fare pto da servizio di A e B, l'importanza di tutti i diversi punti nel match in qualsiasi momento
# ho 18 possibili esiti per il punto nel game
# ho 39 possibili esiti per il game nel set ( compreso tie break )
# ho 9 possibili esiti  per il set nel match

# 18*39*9=6318 righe

# perciò avrò 6318 righe-> a ogni riga è legato il valore dell'importanza di quel punto nel match date probA e probB
# 
imp_pm<-function( prob_A, prob_B) { 
  #, game_A, game_B) {
  st_point1 = rownames(imp_pg(prob_A, prob_B)[[1]])
  st_point2 = rownames(imp_pg(prob_A, prob_B)[[2]])
  
  st_game = rownames(imp_gs(prob_A , prob_B)) #, game_A, game_B))
  
  st_set = rownames(imp_sm(prob_A , prob_B))
  
  
  imp.pg<-imp_pg(prob_A, prob_B)
  imp.gs<-imp_gs(prob_A,prob_B)
  imp.sm<-imp_sm(prob_A, prob_B)
  # creo stringa che comprende tutti gli esiti possibili che possono occorrere in una partita di tennis
  esiti<-NULL
  
  for (k in 1:length(st_set)){
    for( i in 1:length(st_game)) {
      
      
      
      if (st_game[i]!= "6-6") {
        for(j in 1:length(st_point1)) {
          esiti<-c(esiti,paste(st_point1[j],st_game[i], st_set[k], sep="|"))
        }
      }
      
      if (st_game[i]== "6-6") {
        for(g in 1:length(st_point2)) {
            esiti<-c(esiti,paste(st_point2[g],st_game[i], st_set[k], sep="|"))
          }
      }
      
    }
  }
  
  
  point_game<-NULL
  
  for (k in 1:length(st_set)){
    
    for( i in 1:length(st_game)) {
      
      if (st_game[i]!= "6-6") {
        
        point_game<-c(point_game,as.vector(imp.pg[[1]]*imp.gs[i]*imp.sm[k]))
        
      }
      
      if (st_game[i]== "6-6") {

          point_game<-c(point_game, as.vector(imp.pg[[2]]*imp.gs[i]*imp.sm[k]))

      }
      
    }
  }
  #point_game<-as.matrix(point_game)
  point_match<- as.matrix(point_game)
  rownames(point_match)<-esiti
  
  
  return(point_match)
  
}


# creo stringa con tutte le combinazioni che ho trovato nel dataset per una determinata partita ----------------
# verranno scritte nel formato ( ptiA-ptiB|gameA-gameB|setA-setB ) -> es. 15-30|1-4|2-1

partita_stringa<-function(dataframe) {
  
  
  pA<-dataframe$P1Score
  pB<-dataframe$P2Score
  game_A<-dataframe$gameA
  game_B<-dataframe$gameB
  set_A<-dataframe$setA
  set_B<-dataframe$setB
  
  point<-NULL
  game<-NULL
  set<-NULL
  evento<-NULL
  
  for (i in 1:length(pA)) {
    
    if(pA[i]==50) pA[i]="Adv"
    if(pB[i]==50) pB[i]="Adv"
    
    point<- c(point,paste(pA[i],pB[i],sep="-"))
    game<- c(game, paste(game_A[i],game_B[i],sep="-"))
    set<- c(set, paste(set_A[i],set_B[i],sep="-"))
    
    
    evento<-c(evento, paste(point[i],game[i], set[i], sep="|"))
    
  }
  
  return(evento) 
}


# creo colonna relativa all'importanza del punto nella partita -------------

importanza_punto<-function(df, pA, pB) {
  

  stringa<-partita_stringa(df)
  impor<-imp_pm(pA,pB) #,game_A,game_B)
  
  imp_finale<-NULL
  
  # se ho un match tra la stringa che mi dice l'andamento della partita e la riga di "impor" che contiene tutti i possibili esiti
  # allora prendi quella riga e quel valore dell importanza e mettilo dentro la variabile imp_finale
  for( i in 1:length(stringa)) {
    for( j in 1:nrow(impor))  {
      
      if ( stringa[i]==rownames(impor)[j] ) {
        
        imp_finale[i]<-impor[j]
        break
      }
      
      else {
        imp_finale[i]<-NA
      }
      
    }
    
  }
  #imp_finale<-na.omit(imp_finale)
  imp_finale<-as.matrix(imp_finale)
  rownames(imp_finale)<-stringa
  colnames(imp_finale)<- "importanza"
  return(imp_finale)
  
}



##################################################
# FUNZIONE PER CREARE VARIABILE MATCH : ------------
#################################################à

## funzione che ripete il numero del match tante volte quante il numero di pti in quel match
# mi serve per identificare che match è

rep_match<-function(setA,setB) {
  
  che_match<-NULL
  set<-setA+setB
  lunghezza<-NULL
  pos_fin<-NULL
  
  for (j in 2:length(set)) {
    if (set[j]<set[j-1]) lunghezza<-c(lunghezza,j-1)
  }
  lunghezza[length(lunghezza)+1]<-length(set)
  
  if (length(lunghezza)>1) {
  for (i in 2:(length(lunghezza))) {
    pos_fin[1]<-lunghezza[1]
    pos_fin[i] <- lunghezza[i]- lunghezza[i-1] 
  }  
  
      for(i in 1:length(pos_fin)){
    
    che_match<-append(che_match,rep(i,pos_fin[i]))
    
    }
  }
  
  else if (length(lunghezza)==1) {
    che_match<-rep(1, length(set))
  }
  return(che_match)
  
}

##################################
# creo var risposta, A o B fanno punto o no al servizio t-esimo -> ottengo vettore di solo 0 o 1---------
# creo anche variabili yA-1 e yB-1 relative al fatto se A o B hanno vinto il precedente punto
#################################

# punti_A-> pti fatti da A nella partita ( in formato originale: 1,2,3,4 ), idem per B
# cioè uso colonna punti_A e punti_B del dataset tennis
# in output ho una matrice con 4 colonne:
# mat[,1]-> giocatore A ha fatto punto (1) o no (0) nel punto corrente
# mat[,2]-> giocatore A ha vinto o no il punto precedente
# mat[,3]-> giocatore B ha fatto punto (1) o no (0) nel punto corrente
# mat[,4]-> giocatore B ha vinto o no il punto precedente

y_A_B<- function(vinceA, vinceB) {

  yA_1<-NULL
  yB_1<-NULL
  yA<-vinceA
  yA_1[1]<-0
  yB<-vinceB
  yB_1[1]<-0
  
  for (i in 2:length(vinceA)) {
    
    yA_1[i]<- yA[i-1]
    yB_1[i]<- yB[i-1]
    
  }
  
  return(cbind(yA,yA_1,yB,yB_1))
}


# matrice con 4 colonne, prendo quelle relative a A o B




############################
# creo dat-> variabile che mi dice se siamo al primo punto del game o meno ---
#############################

# uguale a quello dell'articolo

d_at<-function(puntiA,puntiB) {
  dat<-NULL
  dat[1]<-1
  
  punti<-puntiA+puntiB
  
  for ( i in 2:length(punti)) {
    if(punti[i]==1) dat[i]=1
    if(punti[i]!=1) dat[i]=0
  }
  return(dat)
}


###############################à
# PROB CHE USERò PER CALCOLO IMPORTANZA
# probabilità che A vinca servizio --------
#################################
# calcolata come suggerito nell articolo-> regredendo y su Xa=(1, Ra-Rb, Ra+Rb)
# stima di y è la prob di A o di B di fare pto da servizio

# per questa funzione dovrò usare y_da_servizio-> i pti segnati da A o B da servizio
# rA-> rank giocatore A rB-> rank giocatoreB
# ID_match-> colonna creata usando funz rep_match
prob_win_service_A<-function(dataframe) { # uso serve_A
  
  p_win<-NULL
  
  rA<-dataframe$rankA
  rB<-dataframe$rankB
  row<-length(unique(dataframe$riga))
  vinceA<-dataframe$vinceA
  vinceB<-dataframe$vinceB
  yA<- y_A_B(vinceA, vinceB)[,1]
  
  a<-(rA-rB)-mean(rA-rB)
  b<-(rA+rB)-mean(rA+rB)
  
  Xa<-model.matrix(~a+b)
  
  # 
  #   a<-(rB-rA)-mean(rB-rA)
  #   b<-(rA+rB)-mean(rA+rB)
  
  betahat<-as.vector((lm(yA~Xa-1))$coefficients)
  
  prob<-NULL
  
  for ( i in 1:row) {
    
    y<-yA[which(dataframe$riga==i)]
    X<-Xa[which(dataframe$riga==i),]
    
    prob<-mean(X%*%betahat)          
    p_win<- c(p_win, prob)
    
  }
  
  
  return(round(p_win,digits=4))
}



prob_win_service_B<-function(dataframe) { # uso serve_A
  
  p_win<-NULL
  
  rA<-dataframe$rankA
  rB<-dataframe$rankB
  row<-length(unique(dataframe$riga))
  vinceA<-dataframe$vinceA
  vinceB<-dataframe$vinceB
  yB<- y_A_B(vinceA, vinceB)[,3]
  
  
  a<-(rB-rA)-mean(rB-rA)
  b<-(rA+rB)-mean(rA+rB)
  
  Xb<-model.matrix(~a+b)
  
  
  betahat<-as.vector((lm(yB~Xb-1))$coefficients)
  
  prob<-NULL
  
  for ( i in 1:row) {
    
    y<-yB[which(dataframe$riga==i)]
    X<-Xb[which(dataframe$riga==i),]
    
    prob<-mean(X%*%betahat)          
    p_win<- c(p_win, prob)
    
  }
  
  return(round(p_win,digits=4))
}






