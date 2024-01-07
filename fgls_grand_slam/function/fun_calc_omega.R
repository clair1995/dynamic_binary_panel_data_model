# calcolo matrice Omega come nell'articolo

omega_match <- function(Ta, Tb, sigma_A, sigma_B, gamma, tau2){
  
  Ita<-diag(1,Ta,Ta)
  Itb<-diag(1,Tb,Tb)
  
  ia<-rep(1, Ta)
  ib<-rep(1,Tb)
  
  var_a<-sigma_A*Ita+tau2*ia%*%t(ia)
  var_b<-sigma_B*Itb+tau2*ib%*%t(ib)
  cov_ab<-gamma*ia%*%t(ib)
  cov_ba<-gamma*ib%*%t(ia)
  
  A<-cbind(var_a,cov_ab)
  B<-cbind(cov_ba, var_b)
  
  omega<-rbind(A,B)
  
  return(solve(omega))
}
