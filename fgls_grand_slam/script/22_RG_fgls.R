# 2 - calcolo stime per tutti i match ------

# Inizializzazione variabili d'ambiente -----------------------------------

suppressWarnings(suppressMessages(source('/home/clair/Documents/fgls_grand_slam/script/00_init_fgls.R')))

# CALCOLO STIME MATCH GENERATO ----------

# carico file sistemati con tutte le covariate che mi servono da repository data_temp
dfA<-readRDS(file_id$file_temp_service$RG_service_A) 
dfB<-readRDS(file_id$file_temp_service$RG_service_B) 

# itero procedimento fgls fino a convergenza. 
# Imposto il numewro massimo di iterazioni con n.iter
conv <- conv_fgls(dfA, dfB, n.iter = 10)

# salvo df finale sui cui andrò ad adattare il modello lm
saveRDS(conv$df_finale, file = file_id$output_df$out_df_RG)

ds <- conv$df_finale %>% 
  select(-c(match_id, momento_partita, serve))

# applico come passaggio finale il modello lineare per ottenere le stime finali del modello
full.model <- lm(yt ~.-1, data = ds)
summary(full.model)

# uso procedura stepwise forward per eliminare iterazioni tra variabili
# che potrebbero sporcare le stime finali

fit0 = lm(yt~-1, data=ds)
covariates = paste('~',paste(names(ds)[-1], collapse='+'))
step.model = step(fit0, covariates, direction='forward')
summary(step.model)

# MODELLO FINALE DOPO STEPWISE
mod.rid<-lm(yt~.-1, data= ds %>% select(yt, names(step.model$coefficients)))
summary(mod.rid)

# salvo  modello ridotto finale
saveRDS(mod.rid, file = file_id$output_mod$out_mod_RG)


# confronto finale della varianza totale tra i dati
# modello ridotto vs modello completo
anova(mod.rid,full.model, test="F")



