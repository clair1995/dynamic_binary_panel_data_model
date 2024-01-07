# This function computes outcomes probabilities for a tie-break
# ppoint_srv1 : probability that the first player wins a point on his serve. Between 0 and 1
# ppoint_srv2 : probability that the second player wins a point on his serve. Between 0 and 1
# s_tb : state of the tie-break in a Markov sense. If you want to start from 0-0 just take s0tb
# Otherwise just put 1 in the score you want to start from, e.g. if I want to compute probabilities from 2-3
# I just need to set this : s_tb <- s0tb;s_tb[1,'0-0'] <- 0; s_tb[1, '2-3'] <- 1
# graph : should we display the Markov Chain representation ? boolean
#
# Output : Markov matrix containing stable states (SETv1 or SETv2) probabilities

resTIE <- function (ppoint_srv1, ppoint_srv2, s_tb){

  MC_tb <- MCtb2(ppoint_srv1, ppoint_srv2)
  resTIE <- s_tb*(MC_tb ^ 1000)
  
  return(resTIE)
}


# funzione per calcolare le diverse prob per A e B nel tiebreak-----
#this fuction omits the possibility of infinit deuces : useful to compute only win probs

MCtb2 <- function(ppoint_srv1, ppoint_srv2) {
  
  
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
  tMat["0-3","0-4"] <- ppoint_srv2
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
  tMat["0-3","1-3"] <- 1 - ppoint_srv2
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

