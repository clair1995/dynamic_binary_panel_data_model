# 01 - Creazione dataset per modellistica Australian Open ----------------------

# Inizializzazione variabili d'ambiente -----------------------------------

suppressWarnings(suppressMessages(source('/home/clair/Documents/fgls_grand_slam/script/00_init_fgls.R')))

# carico file punto a punto
df<-read.csv(file_id$input_csv$W_points, header=T, sep=",") %>% 
  mutate(PointNumber = as.numeric(PointNumber))

# carico caratteristiche match
df_match<-read.csv(file_id$input_csv$W_match, header=T, sep=",") %>% 
  mutate(player1 = paste0(substr(player1,1,1), ". ", word(player1,2,-1)),
         player2 = paste0(substr(player2,1,1), ". ", word(player2,2,-1)))

# carico file per il ranking scaricato da On Court
rk<-read.csv(file_id$input_csv$W_ranking,  header=T, sep=";")

rk_clean = rk %>%
  mutate(player = paste0(substr(Giocatore,1,1), ". ", word(Giocatore,2,-1))) %>% 
  mutate(player = gsub('J. Antoni Munar Clar', 'J. Munar', player),
         player = gsub('P. Carreno-Busta', 'P. Carreno Busta', player),
         player = gsub('T. Harry Fritz', 'T. Fritz', player),
         player = gsub('J. Murray Kubler', 'J. Kubler', player),
         player = gsub('D. Sebastian Schwartzman', 'D. Schwartzman', player),
         player = gsub('T. Moura Monteiro', 'T. Monteiro', player),
         player = gsub('P. Martinez Portero', 'P. Martinez', player),
         player = gsub('J. Martin Del Potro', 'J. Del Potro', player),
         player = gsub('A. Ramos-Vinolas', 'A. Ramos Vinolas', player),
         player = gsub('J. Tsonga', 'J. Wilfried Tsonga', player),
         player = gsub('J. Londero', 'J. Ignacio Londero', player),
         player = gsub('J. Struff', 'J. Lennard Struff', player),
         player = gsub('P. Herbert', 'P. Hugues Herbert', player),
         player = gsub('J. Struff', 'J. Lennard Struff', player),
         player = gsub('C. Stebe', 'C. Marcel Stebe', player),
         player = gsub('G. Garcia-Lopez', 'G. Garcia Lopez',  player),
         player = gsub('R. Strombachs', 'Z. Svajda',  player)
  )


# tengo solo variabili che mi interessano
df<-df %>% 
  select(match_id,
         SetNo,
         GameNo,
         PointNumber,
         PointWinner,
         PointServer,
         P1Score,
         P2Score) %>% 
  na.omit()


df_match <- df_match %>% 
  select(match_id,
         year,
         slam,
         match_num,
         player1,
         player2)

# TROVO DF FINALE PER ROLAND GARROS  -----------

# funzione che attribuisce ranking a rispettivo giocatore e sistema dataset
finale <- df_match_rank(df,df_match,rk)

# se son presenti na -> giocatori non matchano
# finale %>%
#   select(rankA, player1, rankB, player2) %>%
#   filter(is.na(rankA) | is.na(rankB)) %>%
#   distinct() %>%
#   View

# PROSEGUO SISTEMANDO IL DF, PER POI CALCOLARE FUNZIONI DI IMPORTANZA-----------

# pulisco dataset con tutti i match e lo preparo per la funzione finale dove mi calcolerò anche l'importanza 

df_servizio_clean = preproc_df(finale)


# creazione del dataset definitivo che mi serve per costruire modello e analisi finali-----

# specifichiamo il tipo di torneo del GS,
# poichè ogni torneo ha un modo diverso di goicare il Tie break a  5o set

list_df = prep_df_fin(df_servizio_clean, torneo = 'W')

df_serveA <- list_df$df_prep_A
df_serveB <- list_df$df_prep_B

# df_serveA e df_serveB sono i 2 dataset con le informazioni utili e su cui adattero successivamente il modello

# serie storica punti giocatore A negli N match
saveRDS(df_serveA, file = file_id$file_temp_service$W_service_A)
saveRDS(df_serveB, file = file_id$file_temp_service$W_service_B)

# Pulizia Ambiente --------------------------------------------------------

setwd(paths$project)
rm(list = ls())
gc()







