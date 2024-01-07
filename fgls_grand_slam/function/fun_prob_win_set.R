# PROBABILITA di vincere un set partendo sullo (0,0) 
# pA e pB -> prob giocatore A e B di fare pto da servizio

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
  ptie1 = resTIE(phold1, phold2, s_tb = s0tb)[1, "SETv1"] 
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
  
  # si(a + 1, b), the probability that the server wins the set given that he or she wins the game;
  # si(a, b + 1), the probability that the server wins the set given that he or she loses the game
  
  M_inf<-((solve(diag(1,ncol(tMat)-2,ncol(tMat)-2)-Q1))%*%R1)[,1]
  return(M_inf[[1]])
  
}
