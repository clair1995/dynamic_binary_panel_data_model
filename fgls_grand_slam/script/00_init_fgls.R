
# 00 - Initialize Environment ---------------------------------------------

# Global Parameters -------------------------------------------------------

library(glue)

# Percorsi ----------------------------------------------------------------

prj_dir = '/home/clair/Documents/fgls_grand_slam/'

setwd(prj_dir)

paths = list(
  project          = prj_dir,
  script           = glue('{prj_dir}script/'),
  fun              = glue('{prj_dir}function/'),
  input            = glue('{prj_dir}data_input/'),
  AO               = glue('{prj_dir}data_input/AO/'),
  RG               = glue('{prj_dir}data_input/RG/'),
  US               = glue('{prj_dir}data_input/US/'),
  W                = glue('{prj_dir}data_input/W/'),
  output           = glue('{prj_dir}data_output/'),
  temp_data        = glue('{prj_dir}data_temp/')
  )


# Caricamento Funzionalita' esterne ---------------------------------------

# Caricamento Librerie
lib_file = list.files(path       = paths$script,
                      pattern    = 'libraries',
                      full.names = TRUE)
suppressMessages(suppressWarnings(source(lib_file)))

# Caricamento Funzioni
fun_file = list.files(path       = paths$fun,
                      full.names = TRUE, 
                      recursive  = TRUE,
                      pattern    ='.R')
for (f in fun_file) {suppressMessages(suppressWarnings(source(f)))}


# Parametri d'Ambiente ----------------------------------------------------


# Data Corrente -----------------------------------------------------------

current_date = Sys.Date()


# Inizializzazione sorgenti dati (In/Out) ---------------------------------

file_id = list(
  input_csv = list(
    AO_match   = glue('{in_dir}2019-ausopen-matches.csv', in_dir = paths$AO),
    AO_points  = glue('{in_dir}2019-ausopen-points.csv', in_dir = paths$AO),
    AO_ranking = glue('{in_dir}ranking_07012019.csv', in_dir = paths$AO),
    RG_match   = glue('{in_dir}2019-frenchopen-matches.csv', in_dir = paths$RG),
    RG_points  = glue('{in_dir}2019-frenchopen-points.csv', in_dir = paths$RG),
    RG_ranking = glue('{in_dir}ranking_20052019.csv', in_dir = paths$RG),
    US_match   = glue('{in_dir}2019-usopen-matches.csv', in_dir = paths$US),
    US_points  = glue('{in_dir}2019-usopen-points.csv', in_dir = paths$US),
    US_ranking = glue('{in_dir}ranking_280819.csv', in_dir = paths$US),
    W_match    = glue('{in_dir}2019-wimbledon-matches.csv', in_dir = paths$W),
    W_points   = glue('{in_dir}2019-wimbledon-points.csv', in_dir = paths$W),
    W_ranking  = glue('{in_dir}ranking_24062019.csv', in_dir = paths$W)
  ),
  file_temp_service = list(
    AO_service_A        = glue('{in_dir}Australian_Open_A.rds', in_dir = paths$temp_data),
    AO_service_B        = glue('{in_dir}Australian_Open_B.rds', in_dir = paths$temp_data),
    RG_service_A        = glue('{in_dir}Roland_Garros_A.rds', in_dir = paths$temp_data),
    RG_service_B        = glue('{in_dir}Roland_Garros_B.rds', in_dir = paths$temp_data),
    US_service_A        = glue('{in_dir}US_open_A.rds', in_dir = paths$temp_data),
    US_service_B        = glue('{in_dir}US_open_B.rds', in_dir = paths$temp_data),
    W_service_A         = glue('{in_dir}Wimbledon_A.rds', in_dir = paths$temp_data),
    W_service_B         = glue('{in_dir}Wimbledon_B.rds', in_dir = paths$temp_data)
  ),
  output_df = list(
    out_df_AO = glue('{in_dir}/AO_2019/df_AO_2019_finale.rds', in_dir = paths$output),
    out_df_RG = glue('{in_dir}/RG_2019/df_RG_2019_finale.rds', in_dir = paths$output),
    out_df_US = glue('{in_dir}US_2019/df_US_2019_finale.rds', in_dir = paths$output),
    out_df_W  = glue('{in_dir}W_2019/df_W_2019_finale.rds', in_dir = paths$output)
  ),
  output_mod = list(
    out_mod_AO = glue('{in_dir}/AO_2019/mod_AO_2019.rds', in_dir = paths$output),
    out_mod_RG = glue('{in_dir}/RG_2019/mod_RG_2019.rds', in_dir = paths$output),
    out_mod_US = glue('{in_dir}US_2019/mod_US_2019.rds', in_dir = paths$output),
    out_mod_W  = glue('{in_dir}W_2019/mod_W_2019.rds', in_dir = paths$output)
  )
)


# Parametri Date ----------------------------------------------------------

date_params = list(
  today_date        = current_date,
  last_date         = current_date - day(current_date),
  train_model_date  = current_date - months(12),
  estimate_date     = current_date,
  extraction_start  = as.character((current_date - days(day(current_date)-1)) %m-% months(1)),
  extraction_end    = as.character(current_date),
  last_purch_filter = ((current_date - day(current_date) + 1) %m+% months(1))  - years(2) - years(1)
)



