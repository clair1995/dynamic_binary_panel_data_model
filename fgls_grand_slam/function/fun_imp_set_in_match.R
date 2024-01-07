# IMPORTANZA DEL SET NEL MATCH ------------

imp_sm <- function(probA,probB) {
  
  pset_v1 <- prob_set(probA,probB)
  pset_v2 = 1 - pset_v1
  
  STATES = c("V1","V2","0-0","0-1","1-0","1-1","2-0","0-2","2-1","1-2","2-2")
  
  tMat <- matrix(0, nrow = length(STATES), ncol = length(STATES), byrow = TRUE)
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
  
  mat_imp <- matrix(0, nrow = length(colrig), ncol = length(colrig), byrow = TRUE)
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
  vett <- as.vector(outer(colrig, colrig, paste, sep = "-"))
  imp <- as.matrix(as.vector(round(mat_imp, digits = 3)))
  
  df_imp = data.frame(mom_set = vett, 
                      imp_sm  = imp)
  # rownames(imp) <- vett
  # colnames(imp) <- "imp_sm"
  
  return(df_imp)
}
