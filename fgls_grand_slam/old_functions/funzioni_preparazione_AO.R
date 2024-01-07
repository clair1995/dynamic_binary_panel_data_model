rk.f<-function(df, df_rank) {
  
  giocA<-NULL
  giocB<-NULL
  for (i in 1:nrow(df))
  {
    x1 <- as.vector(df$Giocatore.1[i])
    x1.2=type.convert(x1,as.is=TRUE)
    x1.3=unlist(strsplit(x1.2, split=" "))
    xa=unlist(strsplit(x1.3, split="()"))
    
    x2<-as.vector(df$Giocatore.2[i])
    x2.2=type.convert(x2,as.is=TRUE)
    x2.3=unlist(strsplit(x2.2, split=" "))
    xb=unlist(strsplit(x2.3, split="()"))
    
    if ( xa[1]!="(" )  giocA[i]<-x1
    if ( xa[1]=="(" & (is.na(x1.3[4]))==T )  giocA[i]<-paste(x1.3[2], x1.3[3], collapse = " ")
    if ( xa[1]=="(" & (is.na(x1.3[4]))==F ) giocA[i]<-paste(x1.3[2], x1.3[3], x1.3[4], collapse = " ")
    
    
    if ( xb[1]!="(" )  giocB[i]<-x2
    if ( xb[1]=="(" & (is.na(x2.3[4]))==T ) giocB[i]<-paste(x2.3[2], x2.3[3], collapse = " ")
    if ( xb[1]=="(" & (is.na(x2.3[4]))==F ) giocB[i]<-paste(x2.3[2], x2.3[3], x2.3[4], collapse = " ")
  }
  
  df$Giocatore.1<-giocA
  df$Giocatore.2<-giocB
  
  rankA<-NULL
  rankB<-NULL
  for (j in 1:nrow(df_rank)){
    for(i in 1:nrow(df)) {
      
      if(df$Giocatore.1[i]==df_rank$Giocatore[j]) rankA[i]<-df_rank$Pos.[j]
      if(df$Giocatore.2[i]==df_rank$Giocatore[j]) rankB[i]<-df_rank$Pos.[j]
      
    }
  }
  
  
  df$rankA<-rankA
  df$rankB<-rankB
  
  return(df)
  
}

pulisci2=function(y)
{
  punti.oc=c("[0-0*]","[*0-0]","[15-15*]","[*15-15]","[30-30*]","[*30-30]",
             "[15-40*]","[*15-40]", "[40-15*]","[*40-15]","[0-30*]","[*0-30]",
             "[30-0*]","[*30-0]", "[*40-40]", "[40-40*]","[15-0*]","[*15-0]",
             "[0-15*]","[*0-15]","[15-30*]","[*15-30]", "[30-15*]","[*30-15]",
             "[A-40*]","[*A-40]", "[40-A*]","[*40-A]", "[40-30*]","[*40-30]",
             "[30-40*]","[*30-40]", "[40-0*]","[*40-0]","[0-40*]","[*0-40]",
             "[*1-0]", "[1-0*]", "[*0-1]", "[0-1*]", "[*1-1]","[1-1*]",
             "[*2-1]", "[2-1*]", "[*1-2]", "[1-2*]", "[*2-2]","[2-2*]",
             "[*3-2]", "[3-2*]", "[*2-3]", "[2-3*]", "[*3-3]","[3-3*]",
             "[*4-3]", "[4-3*]", "[*3-4]", "[3-4*]", "[*4-4]","[4-4*]",
             "[*5-4]", "[5-4*]", "[*4-5]", "[4-5*]", "[*5-5]","[5-5*]",
             "[*6-5]", "[6-5*]", "[*5-6]", "[5-6*]", "[*6-6]","[6-6*]",
             "[*0-2]", "[0-2*]", "[*2-0]", "[2-0*]", "[*3-0]","[3-0*]", "[*0-3]","[0-3*]",
             "[*0-4]", "[0-4*]", "[*4-0]", "[4-0*]", "[*5-0]","[5-0*]", "[*0-5]","[0-5*]",
             "[*0-6]", "[0-6*]", "[*6-0]", "[6-0*]",
             "[*1-3]", "[1-3*]", "[*3-1]", "[3-1*]", "[*4-1]","[4-1*]", "[*5-1]","[5-1*]",
             "[*6-1]", "[6-1*]", "[*1-4]", "[1-4*]", "[*1-5]","[1-5*]", "[*1-6]","[1-6*]",
             "[*2-4]", "[2-4*]", "[*2-5]", "[2-5*]", "[*2-6]","[2-6*]", "[*4-2]","[4-2*]",
             "[*5-2]", "[5-2*]", "[*6-2]", "[6-2*]",
             "[*3-5]", "[3-5*]", "[*3-6]", "[3-6*]", "[*5-3]","[5-3*]", "[*6-3]","[6-3*]",
             "[*4-6]", "[4-6*]", "[*6-4]", "[6-4*]","[*7-6]" , "[7-6*]", "[*6-7]", "[6-7*]",
             "[*7-7]" , "[7-7*]", "[*7-8]", "[7-8*]", "[*8-7]" , "[8-7*]",
             "[*8-8]" , "[8-8*]", "[*8-9]", "[8-9*]", "[*9-8]" , "[9-8*]",
             "[*9-9]" , "[9-9*]", "[*9-10]", "[9-10*]", "[*10-9]" , "[10-9*]",
             "[*10-10]" , "[10-10*]", "[*11-10]", "[11-10*]", "[*10-11]" , "[10-11*]")
  
  punti.ok=c("00-00*","*00-00","15-15*","*15-15","30-30*","*30-30",
             "15-40*","*15-40", "40-15*","*40-15","00-30*","*00-30",
             "30-00*","*30-00", "*40-40", "40-40*","15-00*","*15-00",
             "00-15*","*00-15","15-30*","*15-30", "30-15*","*30-15",
             "50-40*","*50-40", "40-50*","*40-50", "40-30*","*40-30",
             "30-40*","*30-40", "40-00*","*40-00","00-40*","*00-40",
             "*01-00", "01-00*", "*00-01", "00-01*", "*01-01","01-01*",
             "*02-01", "02-01*", "*01-02", "01-02*", "*02-02","02-02*",
             "*03-02", "03-02*", "*02-03", "02-03*", "*03-03","03-03*",
             "*04-03", "04-03*", "*03-04", "03-04*", "*04-04","04-04*",
             "*05-04", "05-04*", "*04-05", "04-05*", "*05-05","05-05*",
             "*06-05", "06-05*", "*05-06", "05-06*", "*06-06","06-06*",
             "*00-02", "00-02*", "*02-00", "02-00*", "*03-00","03-00*", "*00-03","00-03*",
             "*00-04", "00-04*", "*04-00", "04-00*", "*05-00","05-00*", "*00-05","00-05*",
             "*00-06", "00-06*", "*06-00", "06-00*",
             "*01-03", "01-03*", "*03-01", "03-01*", "*04-01","04-01*", "*05-01","05-01*",
             "*06-01", "06-01*", "*01-04", "01-04*", "*01-05","01-05*", "*01-06","01-06*",
             "*02-04", "02-04*", "*02-05", "02-05*", "*02-06","02-06*", "*04-02","04-02*",
             "*05-02", "05-02*", "*06-02", "06-02*",
             "*03-05", "03-05*", "*03-06", "03-06*", "*05-03","05-03*", "*06-03","06-03*",
             "*04-06", "04-06*", "*06-04", "06-04*", "*07-06" , "07-06*", "*06-07", "06-07*",
             "*07-07" , "07-07*", "*07-08", "07-08*", "*08-07" , "08-07*",
             "*08-08" , "08-08*", "*08-09", "08-09*", "*09-08" , "09-08*",
             "*09-09" , "09-09*", "*09-10", "09-10*", "*10-09" , "10-09*",
             "*10-10" , "10-10*", "*11-10", "11-10*", "*10-11" , "10-11*")
             
  n=length(y)
  yok=c()
  for (i in 1:n)
  {if (sum(y[i]==punti.oc)==1) yok=c(yok,y[i])
  #cat(i, y[i], substr(y[i],1,1), (substr(y[i],1,1)=="["), "\n")
  }
  
  n=length(yok)
  m=length(punti.oc)
  yokk=rep(0,length(yok))
  for (i in 1:m)
  {
    ind=which(yok==punti.oc[i])
    yokk[ind]=punti.ok[i]
  }
  ind=which(yokk!=0)
  yokkk=yokk[ind]
  return(yokkk)
}


# trasforma da stringa di valori a dataframe con andamento dei punti in tutti i match
puntiA_B<-function(x00) {
  
  righe=nrow(x00)
  punti.fin<-NULL
  
  for (riga in 1:righe)
  {
    giocatore1<-as.character(x00$Giocatore.1[riga])
    giocatore2<-as.character(x00$Giocatore.2[riga])
    turno<-as.character(x00$Turno[riga])
    rankA<-x00$rankA[riga]
    rankB<-x00$rankB[riga]
    x0.1=x00$X.1[riga]
    x0.2=as.vector(x0.1)
    x0.3=type.convert(x0.2,as.is=TRUE)
    x0=unlist(strsplit(x0.3, split=" "))
    x=pulisci2(y=x0)
    
    servizio<-NULL
    
    n=length(x)
    batteA=rep(0,n-1)
    batteB=rep(0,n-1)
    vinceA=rep(0,n-1)
    vinceB=rep(0,n-1)
    
    p0A<-NULL
    p0B<-NULL
    p1A<-NULL
    p1B<-NULL
    
    for (i in 1:(n-1)){
    if (substr(x[i],1,1)=="*") 
    {
    batteA[i]=1 
    batteB[i]=0
    y0=substr(x[i],2,6)
    } else
    {
      batteA[i]=0 
      batteB[i]=1
      y0=substr(x[i],1,5)
    }
      
    
    # mi preparo y1 cio? il punto successivo a y0
    if (substr(x[i+1],1,1)=="*") 
    {
    y1=substr(x[i+1],2,6)
    } else 
    {
    y1=substr(x[i+1],1,5)
    }
    
    ### stabilisco chi ha fatto il punto
    
    punto0A=as.numeric(substr(y0,1,2))
    punto0B=as.numeric(substr(y0,4,5))
    punto1A=as.numeric(substr(y1,1,2))
    punto1B=as.numeric(substr(y1,4,5))
    if (punto1A-punto0A>0) vinceA[i]=1
    #if (punto1B-punto0B>0) vinceB[i]=1
    #if ( (punto1B==40) & (punto1A<punto0A) ) vinceB[i]=1
    if ( (punto1A==40) & (punto1B<punto0B) ) vinceA[i]=1
    if ( ((punto1A+punto1B)==0) & (punto0A>punto0B)) vinceA[i]=1
    #if ( ((punto1A+punto1B)==0) & (punto0A<punto0B)) vinceB[i]=1
    # if (((punto0A+punto0B)==0)) vinceA[i]=NA
    # if (((punto0A+punto0B)==0)) vinceB[i]=NA
    
    p0A<-c(p0A,punto0A)
    p0B<-c(p0B,punto0B)
    p1A<-c(p1A,punto1A)
    p1B<-c(p1B,punto1B)
    
    vinceB[i]<-1-vinceA[i]
    
    
    }
    

        for(k in 1:length(batteA)){
          if (batteA[k]==1) servizio[k]<-giocatore1
          if (batteA[k]==0) servizio[k]<-giocatore2
        }
    punti<-cbind(vinceA,vinceB,p0A,p0B,p1A,p1B, batteA, batteB,riga,rankA, rankB, turno, servizio)
    punti.fin<-rbind(punti.fin,punti)
  }
  

  punti.fin<-as.data.frame(punti.fin)
  
  for (j in c(1:11)) {
    
    punti.fin[,j]<-as.numeric(as.character(punti.fin[,j]))
    
  }
  
  
return(punti.fin)
  
}

# trasforma nel dataset come voglio io: puntiA,B-gameA,B- setA,B
game_set<-function(vinceA, vinceB ,uomini) {
  
  if (uomini==1) nset=3
  if (uomini==0) nset=2
  gA<-c()
  gB<-c()
  sA<-c()
  sB<-c()
  set_A=0
  set_B=0
  point_A=c()
  point_B=c()
  n<-length(vinceA)
  r<-NULL
  i=1
  
  flag.set=((set_A>=nset) || (set_B>=nset))
  while (flag.set==F) {
    quinto.set<-((set_A==2 & set_B==2))
    
    if(quinto.set==T) {
      
      game_A=0
      game_B=0
      flag.game=((game_A>5 & game_B<game_A-1) || (game_B>5 & game_A<game_B-1))
      
      while (flag.game==F)
      {
        
        flag.tie=((game_A==6) & (game_B==6)) 
        
        if (flag.tie==T) {
          
          punt_A=0
          punt_B=0
          
          
          flag.punti2=((punt_A==10) || (punt_B==10)) # || (punt_A==7) || (punt_B==7) )
          
          while (flag.punti2==F)
          {
            
            if (vinceA[i]==1) punt_A=punt_A+1
            if (vinceB[i]==1) punt_B=punt_B+1
            if (vinceA[i]==0) punt_A=punt_A
            if (vinceB[i]==0) punt_B=punt_B
            
            point_A<-c(point_A, punt_A)
            point_B<-c(point_B, punt_B)
            #print(i)
            gB<-c(gB,game_B)
            gA<-c(gA,game_A)
            sA<-c(sA, set_A)
            sB<-c(sB, set_B)
            
            if (i == n) break
            
            flag.punti2=((punt_A==10) || (punt_B==10))# || (punt_A==7) || (punt_B==7))            
            
            i=i+1
          }
          flag.game=T
        }
        
        if (flag.tie==F) {
          
          punt_A=0
          punt_B=0
          
          
          flag.punti=((punt_A>3 & punt_B<punt_A-1) || (punt_B>3 & punt_A<punt_B-1))
          
          while (flag.punti==F)
          {
            
            #print(i)
            
            if (vinceA[i]==1) punt_A=punt_A+1
            if (vinceB[i]==1) punt_B=punt_B+1
            if (vinceA[i]==0) punt_A=punt_A
            if (vinceB[i]==0) punt_B=punt_B
            
            r<-c(r,i)
            
            point_A<-c(point_A, punt_A)
            point_B<-c(point_B, punt_B)
            
            
            gB<-c(gB,game_B)
            gA<-c(gA,game_A)
            sA<-c(sA, set_A)
            sB<-c(sB, set_B)
            
            if (i == n) break
            
            
            flag.punti=((punt_A>3 & punt_B<punt_A-1) || (punt_B>3 & punt_A<punt_B-1))
            i=i+1
          }
          
          if (i == n) break
          
          
          if (punt_A>punt_B) {game_A=game_A+1}
          
          if (punt_A<punt_B) {game_B=game_B+1}
          
          
          flag.game=((game_A>5 & game_B<game_A-1) || (game_B>5 & game_A<game_B-1))
          
        }
        
        
        
      }
      
      if (i == n) break
      
      
      if (game_A>game_B) { 
        set_A=set_A+1
      }
      
      if (game_A<game_B) { 
        set_B=set_B+1
      }
      if (game_A==6 & game_B==6) {
        if(punt_A>punt_B) set_A=set_A+1
        if(punt_A<punt_B) set_B=set_B+1
      }
      
      flag.set=((set_A>=nset) || (set_B>=nset))
      
    }
    
    if(quinto.set==F) {
      
      game_A=0
      game_B=0
      flag.game=((game_A>5 & game_B<game_A-1) || (game_B>5 & game_A<game_B-1))
      
      while (flag.game==F)
      {
        
        flag.tie=((game_A==6) & (game_B==6)) 
        
        if (flag.tie==T) {
          
          punt_A=0
          punt_B=0
          
          
          flag.punti1=((punt_A>6 & punt_B<punt_A-1) || (punt_B>6 & punt_A<punt_B-1)) # || (punt_A==7) || (punt_B==7) )
          
          while (flag.punti1==F)
          {
            
            if (vinceA[i]==1) punt_A=punt_A+1
            if (vinceB[i]==1) punt_B=punt_B+1
            if (vinceA[i]==0) punt_A=punt_A
            if (vinceB[i]==0) punt_B=punt_B
            
            point_A<-c(point_A, punt_A)
            point_B<-c(point_B, punt_B)
            #print(i)
            gB<-c(gB,game_B)
            gA<-c(gA,game_A)
            sA<-c(sA, set_A)
            sB<-c(sB, set_B)
            
            if (i == n) break
            
            flag.punti1=((punt_A>6 & punt_B<punt_A-1) || (punt_B>6 & punt_A<punt_B-1))# || (punt_A==7) || (punt_B==7))            
            
            i=i+1
          }
          flag.game=T
        }
        
        if (flag.tie==F) {
          
          punt_A=0
          punt_B=0
          
          
          flag.punti=((punt_A>3 & punt_B<punt_A-1) || (punt_B>3 & punt_A<punt_B-1))
          
          while (flag.punti==F)
          {
            
            #print(i)
            
            if (vinceA[i]==1) punt_A=punt_A+1
            if (vinceB[i]==1) punt_B=punt_B+1
            if (vinceA[i]==0) punt_A=punt_A
            if (vinceB[i]==0) punt_B=punt_B
            
            r<-c(r,i)
            
            point_A<-c(point_A, punt_A)
            point_B<-c(point_B, punt_B)
            
            
            gB<-c(gB,game_B)
            gA<-c(gA,game_A)
            sA<-c(sA, set_A)
            sB<-c(sB, set_B)
            
            if (i == n) break
            
            
            flag.punti=((punt_A>3 & punt_B<punt_A-1) || (punt_B>3 & punt_A<punt_B-1))
            i=i+1
          }
          
          if (i == n) break
          
          
          if (punt_A>punt_B) {game_A=game_A+1}
          
          if (punt_A<punt_B) {game_B=game_B+1}
          
          
          flag.game=((game_A>5 & game_B<game_A-1) || (game_B>5 & game_A<game_B-1))
          
        }
        
        
        
      }
      
      if (i == n) break
      
      
      if (game_A>game_B) { 
        set_A=set_A+1
      }
      
      if (game_A<game_B) { 
        set_B=set_B+1
      }
      if (game_A==6 & game_B==6) {
        if(punt_A>punt_B) set_A=set_A+1
        if(punt_A<punt_B) set_B=set_B+1
      }
      
      flag.set=((set_A>=nset) || (set_B>=nset))
      
    }
  }
  
  
  return(cbind(point_A,point_B,gA,gB,sA,sB))
  
}  

match_finale<- function(x00, uomini=1) {

  finale<-puntiA_B(x00)
  g_s<-NULL
  for (i in 1:length(unique(finale$riga))) {

    ptA<-finale$vinceA[which(finale$riga==i)]
    ptB<-finale$vinceB[which(finale$riga==i)]
    g_s <- rbind(g_s, game_set(ptA, ptB, uomini=1) )
    print(i)

  }

  finale$puntiA<-g_s[,1]
  finale$puntiB<-g_s[,2]
  finale$gameA<-g_s[,3]
  finale$gameB<-g_s[,4]
  finale$setA<-g_s[,5]
  finale$setB<-g_s[,6]

  return (finale)
}


match_finale1<- function(df, uomini=1) {
  
  #finale<-puntiA_B(x00)
  id<-unique(df$match_id)
  g_s<-NULL
  for (i in 1:length(unique(df$match_id))) {
    
    ptA<-df$vinceA[which(df$match_id==id[i])]
    ptB<-df$vinceB[which(df$match_id==id[i])]
    g_s <- rbind(g_s, game_set(ptA, ptB, uomini=1) )
    print(i)
    
  }
  
  df$puntiA<-g_s[,1]
  df$puntiB<-g_s[,2]
  df$gameA<-g_s[,3]
  df$gameB<-g_s[,4]
  df$setA<-g_s[,5]
  df$setB<-g_s[,6]
  
  return (df)
}

# tolgo i numeri del ranking dai giocatori-----
# ranking messo a mano su excel dato che molti giocatori, la maggior parte ne ? sprovvista




# FUNZIONI PER TROVARE DOVE SONO GLI ERRORI NEI DIVERSI MATCH -----------


trova_errori<-function(finale, n) {
  
  check<-NULL
  x1<-NULL
  for (i in 1:length(unique(finale$riga))) {
    X=finale[which(finale$riga==i),]
    ptA<-X$vinceA
    ptB<-X$vinceB
    check[i]<-(nrow(X)==nrow(game_set(ptA,ptB,1)))
    x1<-rbind(x1,c(i,check[i]))
    
  }
  
  x1[which(x1[,2]==0),]
  
  x<-NULL
  for (i in 1:n) {
    
    x[i]<- nrow(finale[which(finale$riga==i),])
    
  }
  b<-NULL
  g_s<-NULL
  for (i in 1:length(unique(finale$riga))) {
    
    ptA<-finale$vinceA[which(finale$riga==i)]
    ptB<-finale$vinceB[which(finale$riga==i)]
    b<-cbind(game_set(ptA, ptB, uomini=1),i)
    g_s <- rbind(g_s, b )
    
  }
  y<-NULL
  for (i in 1:n) {
    
    y[i]<- nrow(g_s[which(g_s[,7]==i),])
    
  }
  sum(y==x) #i false sono errori nei match
  
  #-> A contiene match errati nel computo totale dei punti
  #x-> il numero di punti che dovrebbero esserci
  #y-> il numero di punti che la funzione calcola-> se mancano alcuni punti ma si arriva a 3 set  ci sono punti che non vengono considerati
  
  A<-cbind(x,y)
  A<-matrix(A[which(x!=y),], ncol= 2)
  rownames(A)<-x1[which(x1[,2]==0),1]
  
  return(A)
}


errori<-function(m,n) {
  
  p0A<-m$p0A
  p1A<-m$p1A
  p0B<-m$p0B
  p1B<-m$p1B
  err<-NULL
  for (i in 1:nrow(m)) {
    
    if (p0A[i]==0 & p0B[i]==0){
      
      if((p1A[i]==15 & p1B[i]==0) || (p1A[i]==0 & p1B[i]==15)) err[i]<-0
      
    }
    
    if (p0A[i]==15 & p0B[i]==0){
      
      if((p1A[i]==15 & p1B[i]==15) || (p1A[i]==30 & p1B[i]==0)) err[i]<-0
      
    }
    
    if (p0A[i]==0 & p0B[i]==15){
      
      if((p1A[i]==15 & p1B[i]==15) || (p1A[i]==0 & p1B[i]==30)) err[i]<-0
      
    }
    
    if (p0A[i]==30 & p0B[i]==0){
      
      if((p1A[i]==40 & p1B[i]==0) || (p1A[i]==30 & p1B[i]==15)) err[i]<-0
      
    }
    
    if (p0A[i]==40 & p0B[i]==0){
      
      if((p1A[i]==0 & p1B[i]==0) || (p1A[i]==40 & p1B[i]==15)) err[i]<-0
      
    }
    
    if (p0A[i]==40 & p0B[i]==15){
      
      if((p1A[i]==0 & p1B[i]==0) || (p1A[i]==40 & p1B[i]==30)) err[i]<-0
      
    }
    
    if (p0A[i]==0 & p0B[i]==30){
      
      if((p1A[i]==0 & p1B[i]==40) || (p1A[i]==15 & p1B[i]==30)) err[i]<-0
      
    }
    
    if (p0A[i]==15 & p0B[i]==40){
      
      if((p1A[i]==0 & p1B[i]==0) || (p1A[i]==30 & p1B[i]==40)) err[i]<-0
      
    }
    
    if (p0A[i]==0 & p0B[i]==40){
      
      if((p1A[i]==0 & p1B[i]==0) || (p1A[i]==15 & p1B[i]==40)) err[i]<-0
      
    }
    
    if (p0A[i]==15 & p0B[i]==15){
      
      if((p1A[i]==30 & p1B[i]==15) || (p1A[i]==15 & p1B[i]==30)) err[i]<-0
      
    }
    
    if (p0A[i]==30 & p0B[i]==15){
      
      if((p1A[i]==40 & p1B[i]==15) || (p1A[i]==30 & p1B[i]==30)) err[i]<-0
      
    }
    
    if (p0A[i]==15 & p0B[i]==30){
      
      if((p1A[i]==30 & p1B[i]==30) || (p1A[i]==15 & p1B[i]==40)) err[i]<-0
      
    }
    
    if (p0A[i]==30 & p0B[i]==30){
      
      if((p1A[i]==40 & p1B[i]==30) || (p1A[i]==30 & p1B[i]==40)) err[i]<-0
      
    }
    
    if (p0A[i]==40 & p0B[i]==30){
      
      if((p1A[i]==0 & p1B[i]==0) || (p1A[i]==40 & p1B[i]==40)) err[i]<-0
      
    }
    
    if (p0A[i]==30 & p0B[i]==40){
      
      if((p1A[i]==0 & p1B[i]==0) || (p1A[i]==40 & p1B[i]==40)) err[i]<-0
      
    }
    
    if (p0A[i]==40 & p0B[i]==40){
      
      if((p1A[i]==50 & p1B[i]==40) || (p1A[i]==40 & p1B[i]==50)) err[i]<-0
      
    }
    
    if (p0A[i]==50 & p0B[i]==40){
      
      if((p1A[i]==0 & p1B[i]==0) || (p1A[i]==40 & p1B[i]==40)) err[i]<-0
      
    }
    
    if (p0A[i]==40 & p0B[i]==50){
      
      if((p1A[i]==0 & p1B[i]==0) || (p1A[i]==40 & p1B[i]==40)) err[i]<-0
      
    }
    
    if ( (p1A[i] %in% c(1:14) & p1B[i] %in% c(0:14)) || (p1A[i] %in% c(0:14) & p1B[i] %in% c(1:14)) ) {err[i]<-0}
    
  }
  
  if (length(err)!=nrow(m)) {err<-c(err,0) }
  conta<-c(1:nrow(m))
  return(cbind(err,conta/9, nrow(m)/9))
}



trova_errori_2<-function(df, finale) {
  
  err<-NULL
  for (i in 1:n) {
    
    m<-puntiA_B(df[i,])
    e<-errori(m)
    err<-rbind(err,e)
    
  }
  e<-cbind( finale$p0A, finale$p0B, finale$p1A, finale$p1B, err)
  rownames(e)<- finale$riga
  e<-e[is.na(e[,"err"]),]
  return(e)
}

match_uomini<-function(df,df_match,rk) {
  
  str<-as.character(df$match_id)
  check<-NULL
  
  for (i in 1:nrow(df)){
    x1<-as.vector(str[i])
    x1.2=type.convert(x1,as.is=TRUE)
    x1.3=unlist(strsplit(x1.2, split="-"))
    xa=as.integer(x1.3[3])
    if(xa<2000) check[i]=1
    if(xa>2000) check[i]=NA
    
  }
  
  num<-df_match$match_num 
  
  ck<-NULL
  
  for (i in 1:nrow(df_match)){
    
    if(num[i]<2000) ck[i]=1
    if(num[i]>2000) ck[i]=NA
    
  }
  
  df$check<-check
  df_match$ck<-ck
  
  df1<-na.omit(df)
  df_match<-na.omit(df_match)
  
  a1<-as.character(df_match$player1)
  a2<-as.character(df_match$player2)
  
  gioc1<-NULL
  gioc2<-NULL
  
  for(i in 1:nrow(df_match)){
    xx<-as.vector(a1[i])
    x2=type.convert(xx,as.is=TRUE)
    x3=unlist(strsplit(x2, split=" "))
    x=x3[2]
    yy<-as.vector(a2[i])
    y2=type.convert(yy,as.is=TRUE)
    y3=unlist(strsplit(y2, split=" "))
    y=y3[2]
    gioc1<-c(gioc1,x)
    gioc2<-c(gioc2,y)
  }
  
  rk_g<-as.character(rk$Giocatore)
  giocatore<-NULL
  for(i in 1:500){
    x1<-as.vector(rk_g[i])
    x1.2=type.convert(x1,as.is=TRUE)
    x1.3=unlist(strsplit(x1.2, split=" "))
    xa=x1.3[length(x1.3)]
    giocatore<-c(giocatore,xa)
  }
  
  
  
  rankA<-NULL
  rankB<-NULL
  
  for (j in 1:500){
    for(i in 1:nrow(df_match)) {
      
      if (gioc1[i]==giocatore[j]) rankA[i]<-j 
      if (gioc2[i]==giocatore[j]) rankB[i]<-j 
    }
  }
  
  df_match$rankA<-rankA
  df_match$rankB<-rankB
  
  df_finale<-giocatore(df1,df_match)
  
  return(df_finale)
}  




