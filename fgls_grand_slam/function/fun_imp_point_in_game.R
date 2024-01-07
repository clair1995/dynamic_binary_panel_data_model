# calcolo importanza del punto nel game partendo dalla probabilità di far punto al servizio usando matrici Markov Chain

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
  tMat1["0-3","0-4"] <- ppoint_srv2
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
  tMat1["0-3","1-3"] <- 1 - ppoint_srv2
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
  # imp1<-imp1[-which(imp1[,]==0),]
  # imp1<-as.matrix(imp1,nrow(imp1),1)
  # colnames(imp1)<- "imp_pg"
  
  df_imp1 = data.frame(mom_point = vett, 
                       imp_pg    = imp1)
  
  
  df_imp2 = data.frame(mom_point     = vett1, 
                       imp_pg_tie    = imp2)
  
  # rownames(imp2)<-vett1
  # imp2<-imp2[-which(imp2[,]==0),]
  # imp2<-as.matrix(imp2,nrow(imp2),1)
  # colnames(imp2)<- "imp_pg"
  
  return(list(imp_pg     = df_imp1,
              imp_pg_tie = df_imp2))
}
