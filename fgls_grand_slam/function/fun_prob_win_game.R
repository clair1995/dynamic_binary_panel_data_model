# per la prob di vincere un GAME partendo da (0,0), 
# ppoint_server= prob di fare punto da servizio

prob_game<-function(ppoint_server) {
  
  ppoint_ret <- 1 - ppoint_server
  
  STATES <- c("HOLD", "BREAK", "0-0", "0-15", "15-0", "15-15",
              "30-0", "0-30", "40-0", "30-15", "15-30", "0-40",
              "40-15", "15-40", "30-30(DEUCE)", "40-30(A-40)", "30-40(40-A)")
  
  tMat <- matrix(0, nrow = 17, ncol = 17, byrow = TRUE)
  rownames(tMat) <- STATES
  colnames(tMat) <- STATES
  
  set_transition <- function(from, to, prob) {
    tMat[from, to] <<- prob
  }
  
  set_transition("0-0", "15-0", ppoint_server)
  set_transition("15-0", "30-0", ppoint_server)
  set_transition("0-15", "15-15", ppoint_server)
  set_transition("30-0", "40-0", ppoint_server)
  set_transition("15-15", "30-15", ppoint_server)
  set_transition("0-30", "15-30", ppoint_server)
  set_transition("40-0", "HOLD", ppoint_server)
  set_transition("30-15", "40-15", ppoint_server)
  set_transition("40-15", "HOLD", ppoint_server)
  set_transition("30-30(DEUCE)", "40-30(A-40)", ppoint_server)
  set_transition("40-30(A-40)", "HOLD", ppoint_server)
  set_transition("0-40", "15-40", ppoint_server)
  set_transition("15-40", "30-40(40-A)", ppoint_server)
  set_transition("30-40(40-A)", "30-30(DEUCE)", ppoint_server)
  set_transition("15-30", "30-30(DEUCE)", ppoint_server)
  
  set_transition("0-0", "0-15", ppoint_ret)
  set_transition("15-0", "15-15", ppoint_ret)
  set_transition("0-15", "0-30", ppoint_ret)
  set_transition("30-0", "30-15", ppoint_ret)
  set_transition("15-15", "15-30", ppoint_ret)
  set_transition("0-30", "0-40", ppoint_ret)
  set_transition("40-0", "40-15", ppoint_ret)
  set_transition("30-15", "30-30(DEUCE)", ppoint_ret)
  set_transition("40-15", "40-30(A-40)", ppoint_ret)
  set_transition("30-30(DEUCE)", "30-40(40-A)", ppoint_ret)
  set_transition("40-30(A-40)", "30-30(DEUCE)", ppoint_ret)
  set_transition("0-40", "BREAK", ppoint_ret)
  set_transition("15-40", "BREAK", ppoint_ret)
  set_transition("30-40(40-A)", "BREAK", ppoint_ret)
  set_transition("15-30", "15-40", ppoint_ret)
  
  set_transition("BREAK", "BREAK", 1)
  set_transition("HOLD", "HOLD", 1)
  
  I <- diag(1, 2, 2)
  R <- tMat[3:17, 1:2]
  Q <- tMat[3:17, 3:17]
  O <- tMat[1:2, 3:17]
  
  M_inf <- solve(diag(1, 15, 15) - Q) %*% R
  return(M_inf[1, 1])
}
