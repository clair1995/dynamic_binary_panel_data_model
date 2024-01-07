


std<-function(x){(x-mean(x))/sqrt(var(x))}


fgls<-function(dfA,dfB) {
  
  y_at<-dfA$yat
  y_bt<-dfB$ybt
  
  
  ID_matchA<- dfA$ID_match
  ID_matchB<- dfB$ID_match
  
  n<-length(unique(ID_matchA))
  
  
  # standardizzo l'importanza -> se no valori troppo grandi dei coeff
  XA1<-NULL
  XB1<-NULL
  
  for (i in 1:n) {
    
    XA<-as.matrix(dfA[which(ID_matchA==i),c(4:14)])
    XB<-as.matrix(dfB[which(ID_matchB==i),c(4:14)])
    
    # for (j in 9:11) {
    #   XA[,j]<-std(XA[,j])
    #   XB[,j]<-std(XB[,j])
    # }
    
    XA1<-rbind(XA1, XA)
    XB1<-rbind(XB1, XB)
  }
  # per tutti i giocatori e tutti i match
  
  Xa<-model.matrix(~XA1[,c(1,2)]) # qualita giocatore a
  Xb<-model.matrix(~XB1[,c(1,2)]) # qualita giocatore b
  z_at<-XA1[,c(3:11)] # dinamica giocatore a
  z_bt<-XB1[,c(3:11)] # dinamica giocatore b
  Ya<-y_at-XA1[,3] # y(t)-y(t-1)
  Yb<-y_bt-XB1[,3]
  
  
  delta.hat<-stima.iv(dfA,XA1)
  deltab.hat<-stima.iv(dfB,XB1)
  
  y_at.star<- round(y_at - z_at%*%delta.hat)
  y_bt.star<- round(y_bt - z_bt%*%deltab.hat)
  
  # avro un solo beta per tutti i match
  beta.hat<-as.vector((lm(y_at.star~Xa-1))$coefficients)
  betab.hat<-as.vector((lm(y_bt.star~Xb-1))$coefficients)
  
  # un solo errore per tutti i match
  u_at.hat<- (y_at- Xa%*%beta.hat - z_at %*% delta.hat)
  u_bt.hat<- (y_bt- Xb%*%betab.hat - z_bt %*% deltab.hat)
  
  ua<-NULL
  ub<-NULL
  
  for ( i in 1:n){
    
    ua[i]<-mean(u_at.hat[which(ID_matchA==i)]) #errore medio di ogni match quando batte A
    ub[i]<-mean(u_bt.hat[which(ID_matchB==i)]) # errore medio di ogni match quando batte B  
    
  }
  
  
  # stima gamma, una per tutti i match
  gamma.hat<-mean(ua*ub) # facendo la media tra tt i giocatori, averaging over players
  
  
  # trovo stime dei parametri-> diverse per ogni match
  
  tau_sigma_a<-mean((Xa%*%beta.hat+z_at%*%delta.hat)*(1-Xa %*%beta.hat-z_at %*%delta.hat))-
    2*mean((z_at %*%delta.hat)*u_at.hat) 
  
  
  tau_sigma_b<-mean((Xb%*%betab.hat + z_bt%*%deltab.hat)*(1-Xb%*%betab.hat - z_bt%*%deltab.hat))-
    2*mean((z_bt %*% deltab.hat)*u_bt.hat) 
  
  T1<-length(u_at.hat)
  T2<-length(u_bt.hat)
  tau2_a<- (mean(ua^2) - (1/T1)*tau_sigma_a)*(T1/(T1-1)) #1
  tau2_b<- (mean(ub^2) - (1/T2)*tau_sigma_b)*(T2/(T2-1)) #1
  
  
  
  # stima tau2-> una per tutti i match
  
  tau2<-mean(tau2_a,tau2_b)
  
  sigma_a<-tau_sigma_a-tau2
  sigma_b<-tau_sigma_b-tau2
  
  X_fin<-NULL
  Y_fin<-NULL
  ab<-NULL
  for ( i in 1:n) {
    
    
    X1<-XA1[which(ID_matchA==i),]
    X2<-XB1[which(ID_matchB==i),]
    X<-rbind(X1, X2) 
    X<-model.matrix(~X)
    Y<-c(y_at[which(ID_matchA==i)],y_bt[which(ID_matchB==i)])
    Ta<-nrow(X1)
    Tb<-nrow(X2)
    O<-omega_match(Ta, Tb, sigma_a, sigma_b, gamma.hat, tau2)
    
    ab<-c(ab,rep(1,Ta), rep(2,Tb))
    
    X_fin<- rbind( X_fin, O%*%X )
    Y_fin<- rbind( Y_fin, O%*%Y )
  }
  
  Xa_fin<-X_fin[which(ab==1),]
  Xb_fin<-X_fin[which(ab==2),]
  Ya_fin<-Y_fin[which(ab==1),]
  Yb_fin<-Y_fin[which(ab==2),]
  
  beta_delta.a<- as.vector(solve(t(Xa_fin) %*% Xa_fin) %*% t(Xa_fin) %*% Ya_fin)
  
  beta_delta.b<- as.vector(solve(t(Xb_fin) %*% Xb_fin) %*% t(Xb_fin) %*% Yb_fin)
  
  
  return( list ( "gamma" = gamma.hat, "tau2" = tau2, 
                 "sigma_A" = sigma_a, "sigma_B" = sigma_b, "beta_delta.a"=beta_delta.a, "beta_delta.b"= beta_delta.b, "df_finale"=cbind(Y_fin,X_fin)) )
  
}



# CONVERGENZA -----------

conv_fgls<-function(dfA,dfB, n.iter) {
  
  st<-fgls(dfA,dfB)
  tau21<-st$tau2
  gamma21<-st$gamma
  sigma_a1<- st$sigma_A
  sigma_b1<-st$sigma_B
  
  parametri<-c(tau21, gamma21, sigma_a1, sigma_b1)
  
  
  y_at<-dfA$yat
  y_bt<-dfB$ybt
  
  
  ID_matchA<- dfA$ID_match
  ID_matchB<- dfB$ID_match
  
  n<-length(unique(ID_matchA))
  
  
  # standardizzo l'importanza -> se no valori troppo grandi dei coeff
  XA1<-NULL
  XB1<-NULL
  
  for (i in 1:n) {
    
    XA<-as.matrix(dfA[which(ID_matchA==i),c(4:14)])
    XB<-as.matrix(dfB[which(ID_matchB==i),c(4:14)])
    
    # for (j in 9:11) {
    #   XA[,j]<-std(XA[,j])
    #   XB[,j]<-std(XB[,j])
    # }
    
    XA1<-rbind(XA1, XA)
    XB1<-rbind(XB1, XB)
  }
  # per tutti i giocatori e tutti i match
  
  Xa<-model.matrix(~XA1[,c(1,2)]) # qualita giocatore a
  Xb<-model.matrix(~XB1[,c(1,2)]) # qualita giocatore b
  z_at<-XA1[,c(3:11)] # dinamica giocatore a
  z_bt<-XB1[,c(3:11)] # dinamica giocatore b
  Ya<-y_at-XA1[,3] # y(t)-y(t-1)
  Yb<-y_bt-XB1[,3]
  
  stime.a<-st$beta_delta.a
  stime.b<-st$beta_delta.b
  
  beta.hat<-stime.a[c(1:3)]
  delta.hat<-stime.a[c(4:12)]
  betab.hat<-stime.b[c(1:3)]
  deltab.hat<-stime.b[c(4:12)]
  
  u_at.hat<- (y_at- Xa%*%beta.hat - z_at %*% delta.hat)
  u_bt.hat<- (y_bt- Xb%*%betab.hat - z_bt %*% deltab.hat)
  
  ua<-NULL
  ub<-NULL
  
  for ( i in 1:n){
    
    ua[i]<-mean(u_at.hat[which(ID_matchA==i)]) #errore medio di ogni match quando batte A
    ub[i]<-mean(u_bt.hat[which(ID_matchB==i)]) # errore medio di ogni match quando batte B  
    
  }
  
  
  # stima gamma, una per tutti i match
  gamma.hat<-mean(ua*ub) # facendo la media tra tt i giocatori, averaging over players
  
  
  # trovo stime dei parametri-> diverse per ogni match
  
  tau_sigma_a<-mean((Xa%*%beta.hat+z_at%*%delta.hat)*(1-Xa %*%beta.hat-z_at %*%delta.hat))-
    2*mean((z_at %*%delta.hat)*u_at.hat) 
  
  
  tau_sigma_b<-mean((Xb%*%betab.hat + z_bt%*%deltab.hat)*(1-Xb%*%betab.hat - z_bt%*%deltab.hat))-
    2*mean((z_bt %*% deltab.hat)*u_bt.hat) 
  
  T1<-length(u_at.hat)
  T2<-length(u_bt.hat)
  tau2_a<- (mean(ua^2) - (1/T1)*tau_sigma_a)*(T1/(T1-1)) #1
  tau2_b<- (mean(ub^2) - (1/T2)*tau_sigma_b)*(T2/(T2-1)) #1
  
  
  
  # stima tau2-> una per tutti i match
  
  tau2<-mean(tau2_a,tau2_b)
  
  sigma_a<-tau_sigma_a-tau2
  sigma_b<-tau_sigma_b-tau2
  
  X_fin<-NULL
  Y_fin<-NULL
  ab<-NULL
  for ( i in 1:n) {
    
    
    X1<-XA1[which(ID_matchA==i),]
    X2<-XB1[which(ID_matchB==i),]
    X<-rbind(X1, X2) 
    X<-model.matrix(~X)
    Y<-c(y_at[which(ID_matchA==i)],y_bt[which(ID_matchB==i)])
    Ta<-nrow(X1)
    Tb<-nrow(X2)
    O<-omega_match(Ta, Tb, sigma_a, sigma_b, gamma.hat, tau2)
    
    ab<-c(ab,rep(1,Ta), rep(2,Tb))
    
    X_fin<- rbind( X_fin, O%*%X )
    Y_fin<- rbind( Y_fin, O%*%Y )
    
  }
  
  Xa_fin<-X_fin[which(ab==1),]
  Xb_fin<-X_fin[which(ab==2),]
  Ya_fin<-Y_fin[which(ab==1),]
  Yb_fin<-Y_fin[which(ab==2),]
  
  hat.a<- as.vector(solve(t(Xa_fin) %*% Xa_fin) %*% t(Xa_fin) %*% Ya_fin)
  
  hat.b<- as.vector(solve(t(Xb_fin) %*% Xb_fin) %*% t(Xb_fin) %*% Yb_fin)
  
  
  
  #dbhat<-round(haz, digits = 5)
  #db.hat1<-list(stime, haz)
  #print(hat)
  par1<-parametri
  par2<- c(tau2,gamma.hat, sigma_a, sigma_b)
  i=1
  flag.stima = (sum(par1==par2)==4) 
  
  while (flag.stima==F) {
    
    par1<-c(tau2,gamma.hat, sigma_a, sigma_b)
    par1<-round(par1, digits=5)
    beta.hat<-hat.a[c(1:3)]
    delta.hat<-hat.a[c(4:12)]
    betab.hat<-hat.b[c(1:3)]
    deltab.hat<-hat.b[c(4:12)]
    
    u_at.hat<- (y_at- Xa%*%beta.hat - z_at %*% delta.hat)
    u_bt.hat<- (y_bt- Xb%*%betab.hat - z_bt %*% deltab.hat)
    
    ua<-NULL
    ub<-NULL
    
    for ( i in 1:n){
      
      ua[i]<-mean(u_at.hat[which(ID_matchA==i)]) #errore medio di ogni match quando batte A
      ub[i]<-mean(u_bt.hat[which(ID_matchB==i)]) # errore medio di ogni match quando batte B  
      
    }
    
    
    # stima gamma, una per tutti i match
    gamma.hat<-mean(ua*ub) # facendo la media tra tt i giocatori, averaging over players
    
    
    # trovo stime dei parametri-> diverse per ogni match
    
    tau_sigma_a<-mean((Xa%*%beta.hat+z_at%*%delta.hat)*(1-Xa %*%beta.hat-z_at %*%delta.hat))-
      2*mean((z_at %*%delta.hat)*u_at.hat) 
    
    
    tau_sigma_b<-mean((Xb%*%betab.hat + z_bt%*%deltab.hat)*(1-Xb%*%betab.hat - z_bt%*%deltab.hat))-
      2*mean((z_bt %*% deltab.hat)*u_bt.hat) 
    
    T1<-length(u_at.hat)
    T2<-length(u_bt.hat)
    tau2_a<- (mean(ua^2) - (1/T1)*tau_sigma_a)*(T1/(T1-1)) #1
    tau2_b<- (mean(ub^2) - (1/T2)*tau_sigma_b)*(T2/(T2-1)) #1
    
    
    
    # stima tau2-> una per tutti i match
    
    tau2<-mean(tau2_a,tau2_b)
    
    sigma_a<-tau_sigma_a-tau2
    sigma_b<-tau_sigma_b-tau2
    
    X_fin<-NULL
    Y_fin<-NULL
    ab<-NULL
    for ( i in 1:n) {
      
      
      X1<-XA1[which(ID_matchA==i),]
      X2<-XB1[which(ID_matchB==i),]
      X<-rbind(X1, X2) 
      X<-model.matrix(~X)
      Y<-c(y_at[which(ID_matchA==i)],y_bt[which(ID_matchB==i)])
      Ta<-nrow(X1)
      Tb<-nrow(X2)
      O<-omega_match(Ta, Tb, sigma_a, sigma_b, gamma.hat, tau2)
      
      ab<-c(ab,rep(1,Ta), rep(2,Tb))
      
      X_fin<- rbind( X_fin, O%*%X )
      Y_fin<- rbind( Y_fin, O%*%Y )
      
    }
    
    Xa_fin<-X_fin[which(ab==1),]
    Xb_fin<-X_fin[which(ab==2),]
    Ya_fin<-Y_fin[which(ab==1),]
    Yb_fin<-Y_fin[which(ab==2),]
    
    hat.a<- as.vector(solve(t(Xa_fin) %*% Xa_fin) %*% t(Xa_fin) %*% Ya_fin)
    
    hat.b<- as.vector(solve(t(Xb_fin) %*% Xb_fin) %*% t(Xb_fin) %*% Yb_fin)
    
    par2<- c(tau2,gamma.hat, sigma_a, sigma_b)
    par2<-round(par2, digits =5)
    print(rbind(par1,par2))#,hat.a,hat.b))
    i=i+1
    flag.stima = (sum(par1==par2)==4) 
    
    if (i==n.iter) {flag.stima==T}
    
  }
  
  finale.a<-as.data.frame(cbind(Ya_fin, Xa_fin))
  finale.b<-as.data.frame(cbind(Yb_fin, Xb_fin))
  
  
  colnames(finale.a)<-c("yt","intercetta", "qual_rel", "qual_ass", "yt_1", "delta11", "delta12", "dt" , "delta21", "delta22", "imp", "delta31", "delta32")
  colnames(finale.b)<-c("yt","intercetta", "qual_rel", "qual_ass", "yt_1", "delta11", "delta12", "dt" , "delta21", "delta22", "imp", "delta31", "delta32")
  
  
  return(list( finale.a, finale.b,
              "tau2"=tau2, "gamma"=gamma.hat, "sigmaA"=sigma_a, "sigmaB"=sigma_b))
  
}  
