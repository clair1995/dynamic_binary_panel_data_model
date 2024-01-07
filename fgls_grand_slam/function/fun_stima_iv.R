# funzio ne che calcola una stima per le variabili strumentali
# come nell'articolo

stima.iv <- function(df) {
  
  z_t = df %>% 
    select(yt_1:delta32) %>% 
    as.matrix()
  
  # regredisco varts dinamiche e y
  z_t_1 = df %>% 
    select(match_id, yt_1:delta32) %>% 
    group_by(match_id) %>% 
    mutate_at(vars(-group_cols()), lag) %>% 
    ungroup() %>% 
    replace(is.na(.), 0) %>% 
    select(-match_id) %>% 
    as.matrix()

  Y = df$yt - df$yt_1 %>% 
    as.vector()
  
  Z = z_t - z_t_1 %>% 
    as.matrix()
  
  mod.iv <- AER::ivreg(Y~Z-1|z_t_1)
  
  # summary(mod.iv)
  stima <- as.vector(mod.iv$coefficients)
  return(stima)
}
