library(markovchain)



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
  
  # centro qualit? relativa e assoluta-> vedi articolo
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
  
  # centro qualit? relativa e assoluta-> vedi articolo
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



# IMPORTANZA DEL PUNTO NEL GAME imp(p,g) -------------
# ppoint_server= prob che giocatore faccia pto da servizio
# imp_pg<- function(ppoint_server){
#   ppoint_ret = 1 - ppoint_server
#   STATES = c("HOLD", "BREAK","0-0","0-15","15-0","15-15",
#              "30-0","0-30","40-0","30-15",
#              "15-30","0-40","40-15","15-40",
#              "30-30(DEUCE)","40-30(A-40)","30-40(40-A)" )
#   tMat = matrix(0, nrow = 17, ncol = 17, byrow = TRUE)
#   rownames(tMat) = STATES
#   colnames(tMat) = STATES
#   
#   tMat["0-0","15-0"] <- ppoint_server
#   tMat["15-0","30-0"] <- ppoint_server
#   tMat["0-15","15-15"] <- ppoint_server
#   tMat["30-0","40-0"] <- ppoint_server
#   tMat["15-15","30-15"] <- ppoint_server
#   tMat["0-30","15-30"] <- ppoint_server
#   tMat["40-0","HOLD"] <- ppoint_server
#   tMat["30-15","40-15"] <- ppoint_server
#   tMat["40-15","HOLD"] <- ppoint_server
#   tMat["30-30(DEUCE)","40-30(A-40)"] <- ppoint_server
#   tMat["40-30(A-40)","HOLD"] <- ppoint_server
#   tMat["0-40","15-40"] <- ppoint_server
#   tMat["15-40","30-40(40-A)"] <- ppoint_server
#   tMat["30-40(40-A)","30-30(DEUCE)"] <- ppoint_server
#   tMat["15-30","30-30(DEUCE)"] <- ppoint_server
#   
#   tMat["0-0","0-15"] <- 1 - ppoint_server
#   tMat["15-0","15-15"] <- 1 - ppoint_server
#   tMat["0-15","0-30"] <- 1 - ppoint_server
#   tMat["30-0","30-15"] <- 1 - ppoint_server
#   tMat["15-15","15-30"] <- 1 - ppoint_server
#   tMat["0-30","0-40"] <- 1 - ppoint_server
#   tMat["40-0","40-15"] <- 1 - ppoint_server
#   tMat["30-15","30-30(DEUCE)"] <- 1 - ppoint_server
#   tMat["40-15","40-30(A-40)"] <- 1 - ppoint_server
#   tMat["30-30(DEUCE)","30-40(40-A)"] <- 1 - ppoint_server
#   tMat["40-30(A-40)","30-30(DEUCE)"] <- 1 - ppoint_server
#   tMat["0-40","BREAK"] <- 1 - ppoint_server
#   tMat["15-40","BREAK"] <- 1 - ppoint_server
#   tMat["30-40(40-A)","BREAK"] <- 1 - ppoint_server
#   tMat["15-30","15-40"] <- 1 - ppoint_server
#   
#   tMat["BREAK","BREAK"] <- 1
#   tMat["HOLD","HOLD"] <- 1
#   
#   I<-diag(1,2,2)
#   R<-tMat[3:17,1:2]
#   Q<-tMat[3:17,3:17]
#   O<-tMat[1:2,3:17]
#   
#   M_inf<-(solve(diag(1,15,15)-Q))%*%R
# 
#   colrig = c("0", "15","30","40","Adv")
#   
#   mat_imp = matrix(0, nrow = 5, ncol = 5, byrow = TRUE)
#   rownames(mat_imp) = colrig
#   colnames(mat_imp) = colrig
#   
#   # gi(a + 1, b), the probability that the server wins the game given that he or she wins the point;
#   # gi(a, b + 1), the probability that the server wins the game given that he or she loses the point
#   
#   mat_imp["0","0"]<-M_inf[3,1]-M_inf[2,1] #prob(15,0)-prob(0,15) e cosi via
#   mat_imp["15","0"]<-M_inf[5,1]-M_inf[4,1]
#   mat_imp["0","15"]<-M_inf[4,1]-M_inf[6,1]
#   mat_imp["15","15"]<-M_inf[8,1]-M_inf[9,1]
#   mat_imp["30","0"]<-M_inf[7,1]-M_inf[8,1]
#   mat_imp["40","0"]<-1-M_inf[11,1]
#   mat_imp["0","30"]<-M_inf[9,1]-M_inf[10,1]
#   mat_imp["0","40"]<-M_inf[12,1]
#   mat_imp["30","15"]<-M_inf[11,1]-M_inf[13,1]
#   mat_imp["15","30"]<-M_inf[13,1]-M_inf[12,1]
#   mat_imp["40","15"]<-1-M_inf[14,1]
#   mat_imp["15","40"]<-M_inf[15,1]
#   mat_imp["30","30"]<-M_inf[14,1]-M_inf[15,1]
#   mat_imp["40","40"]<-M_inf[14,1]-M_inf[15,1]
#   mat_imp["30","40"]<-M_inf[13,1]
#   mat_imp["40","30"]<-1-M_inf[13,1]
#   mat_imp["Adv","40"]<-1-M_inf[13,1]
#   mat_imp["40","Adv"]<-M_inf[13,1] # prob(40,40)-prob(vinceB)
#   
#   vett<-NULL
#   for (i in 1:length(colrig)) {
#     for (j in 1:length(colrig)){
#       vett<-c(vett,paste(colrig[j],colrig[i], sep="-"))
#     }
#   }
#   imp<-as.matrix(as.vector(round(mat_imp,digits=3)))
#   rownames(imp)<-vett
#   imp<-imp[-which(imp[,]==0),]
#   imp<-as.matrix(imp,18,1)
#   colnames(imp)<- "imp_pg"
#   return(imp)
# }


# s_tb ? lo stato in cui mi trovo  in quel momento ( x es. sullo (0,0), o sul (2,1) )

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
# io devo moltiplicare output per la probabilit? che l'nesimo game sia presente nel dataset
# x es: l(7) = (num di volte che viene giocato game 7)/ (tot game giocati)


# calcolo della probabilit? che nel dataset ci sia il game numero n! 
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




##################################################
# FUNZIONE PER CREARE VARIABILE MATCH : ------------
#################################################?

## funzione che ripete il numero del match tante volte quante il numero di pti in quel match
# mi serve per identificare che match ?

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





