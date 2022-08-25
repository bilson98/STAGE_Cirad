install.packages('RPostgreSQL')
install.packages('devtools')
install.packages('remotes')
#remotes::install_github("r-dbi/RPostgres")
install.packages('RPostgres')
install.packages("odbc")
install.packages('RPostgreSQL')


library(odbc)
library(DBI)
library(RPostgreSQL)
library(RODBC)
library(dplyr)
library(plyr)



dsn_database = "daphne_dev"   
dsn_hostname = "localhost"  # Specify host name e.g.:"aws-us-east-1-portal.4.dblayer.com"
dsn_port = "5432" # Specify your port number. e.g. 98939
dsn_uid = "postgres"  # Specify your username. e.g. "admin"
dsn_pwd = "azerty"


drv <- DBI::dbDriver("PostgreSQL")



connec <- dbConnect(#RPostgreSQL::PostgreSQL(),
                   drv,
                  dbname = dsn_database,
                     host = dsn_hostname,
                     port = dsn_port,
                     user = dsn_uid,
                     password = dsn_pwd)

# dbListTables(connec)


#### INPUTUSMS

######
sql_soils = "select * from soil_type"
df_soils <- dbGetQuery(connec, sql_soils )
write.csv(df_soils,"Data_AEGIS/soil_type.csv", row.names = FALSE)

######
sql_weather = "select * from weather_day"
df_weather <- dbGetQuery(connec, sql_weather )
write.csv(df_weather,"Data_AEGIS/weather.csv", row.names = FALSE)

######
sql_trial = "select * from trial"
df_trial <- dbGetQuery(connec, sql_trial )
write.csv(df_trial,"Data_AEGIS/trial.csv", row.names = FALSE)

######
sql_trial_project = "select * from trial_project"
df_trial_project <- dbGetQuery(connec, sql_trial_project )
write.csv(df_trial,"Data_AEGIS/trial_project.csv", row.names = FALSE)

######
sql_ws = "select * from ws"
df_ws <- dbGetQuery(connec, sql_ws )
write.csv(df_ws,"Data_AEGIS/ws.csv", row.names = FALSE)

######
sql_ws_trial = "select * from ws_trial"
df_ws_trial <- dbGetQuery(connec, sql_ws_trial )
write.csv(df_ws_trial,"Data_AEGIS/ws_trial.csv", row.names = FALSE)


######
sql_sample = "select * from sample"
df_sample <- dbGetQuery(connec, sql_sample )
write.csv(df_sample,"Data_AEGIS/sample.csv", row.names = FALSE)

######
sql_variable = "select * from variable"
df_variable <- dbGetQuery(connec, sql_variable )
write.csv(df_variable,"Data_AEGIS/variable.csv", row.names = FALSE)


######
sql_variable_ontology = "select * from variable_ontology"
df_variable_ontology <- dbGetQuery(connec, sql_variable_ontology )
write.csv(df_variable_ontology,"Data_AEGIS/variable_ontology.csv", row.names = FALSE)



######
sql_factor = "select * from factor"
df_factor <- dbGetQuery(connec, sql_factor )
write.csv(df_factor,"Data_AEGIS/factor.csv", row.names = FALSE)

######
sql_factor_level = "select * from factor_level"
df_factor_level <- dbGetQuery(connec, sql_factor_level )
write.csv(df_factor_level,"Data_AEGIS/factor_level.csv", row.names = FALSE)


######
sql_lot = "select * from lot"
df_lot <- dbGetQuery(connec, sql_lot )
write.csv(df_lot,"Data_AEGIS/lot.csv", row.names = FALSE)


######
sql_lot_unit = "select * from lot_unit"
df_lot_unit <- dbGetQuery(connec, sql_lot_unit )
write.csv(df_lot_unit,"Data_AEGIS/lot_unit.csv", row.names = FALSE)

######
sql_obs_equipment = "select * from obs_equipment"
df_obs_equipment <- dbGetQuery(connec, sql_obs_equipment )
write.csv(df_obs_equipment,"Data_AEGIS/obs_equipment.csv", row.names = FALSE)


######
sql_obs_operator = "select * from obs_operator"
df_obs_operator <- dbGetQuery(connec, sql_obs_operator )
write.csv(df_obs_operator,"Data_AEGIS/obs_operator.csv", row.names = FALSE)

######
sql_obs_unit = "select * from obs_unit"
df_obs_unit <- dbGetQuery(connec, sql_obs_unit )
write.csv(df_obs_unit,"Data_AEGIS/obs_unit.csv", row.names = FALSE)


######
sql_itk = "select * from itk"
df_itk <- dbGetQuery(connec, sql_itk )
write.csv(df_itk,"Data_AEGIS/itk.csv", row.names = FALSE)

######
sql_exp_unit = "select * from exp_unit, factor_unit  where exp_unit.exp_unit_id = factor_unit.exp_unit_i and trial_code = 'ICRIR0F1' and factor_level_id = 4 "
df_exp_unit <- dbGetQuery(connec, sql_exp_unit )
write.csv(df_exp_unit,"Data_AEGIS/exp_unit.csv", row.names = FALSE)


# sql = "select * 
#           from exp_unit, factor_unit 
#           left join itk on factor_unit.exp_unit_id = itk.exp_unit_id 
#           left join factor_level on factor_level.factor_level_id = factor_unit.factor_level_id
#           where
#             factor_unit.exp_unit_id = exp_unit.exp_unit_id
#             and
#             trial_code = 'ICRIR0F1' and factor_level_id = 4
#         "





#, factor_level
                  # and 
                  # factor_level.factor_level_id = factor_unit.factor_level_id "
                  # and trial_code = 'ICRIR0F1' 
                  # and factor_level_id = 4 "





######
sql_factor_unit = "select * from factor_unit"
df_factor_unit <- dbGetQuery(connec, sql_factor_unit )
write.csv(df_factor_unit,"Data_AEGIS/factor_unit.csv", row.names = FALSE)



sql = "select * from daphne_dev where factor_level_id = 4"








################### 3 PRINCIPALES TABLES


#### DESIGN

sql = "select *
          from exp_unit, factor_unit, factor_level, trial
            where trial.trial_code = exp_unit.trial_code
              and
                factor_level.factor_level_id = factor_unit.factor_level_id
              and
                factor_unit.exp_unit_id = exp_unit.exp_unit_id
              and
                trial.trial_code = 'DEL1'
              and
                factor_unit.factor_level_id = 4 "


# sql = "select distinct *
#           from exp_unit
#             full join factor_unit on factor_unit.exp_unit_id = exp_unit.exp_unit_id
#             full join trial on trial.trial_code = exp_unit.trial_code
#             full join factor_level on factor_level.factor_level_id = factor_unit.factor_level_id
#               where
#                   exp_unit.trial_code = 'DEL1'
#                 and
#                   factor_level.factor_level_id = 4  "



design <- dbGetQuery(connec, sql )

design = select(design,-c(17,21,25))  # Suppression des colonnes en doublon

write.csv(design,"Data_AEGIS/design.csv", row.names = FALSE)




#### METEO

sql = "select * 
          from ws, ws_trial, weather_day 
            where weather_day.wscode = ws_trial.wscode
              and
                ws_trial.wscode = ws.wscode
              and
                ws_trial.trialcode = 'DEL1' 
               "

meteo <- dbGetQuery(connec, sql )

meteo = select(meteo,-c(10,14))

write.csv(meteo,"Data_AEGIS/meteo.csv", row.names = FALSE)   

#write.csv(meteo,"Data_AEGIS/meteo_test.csv", row.names = FALSE)
#### OBSERVATION

sql = "select * 
          from obs_unit 
            where obs_unit.unit_id in (11517,11557,11537,11577) "   # filtre sur les exp_unit_id du Design (design.exp_unit_id)

observation <- dbGetQuery(connec, sql )

observation = rename(observation, c("unit_id" = "exp_unit_id") )

write.csv(observation,"Data_AEGIS/observation.csv", row.names = FALSE)


#### ITK

# sql = "select *
#           from exp_unit, factor_unit, factor_level, trial, itk
#             where trial.trial_code = exp_unit.trial_code
#               and
#                 factor_level.factor_level_id = factor_unit.factor_level_id
#               and
#                 factor_unit.exp_unit_id = exp_unit.exp_unit_id
#               and
#                 itk.exp_unit_id = exp_unit.exp_unit_id
#               and
#                 trial.trial_code = 'DEL1'
#               and
#                 factor_unit.factor_level_id = 4
#                 "   
# 
# itk <- dbGetQuery(connec, sql )


itk = merge(design,df_itk,all = TRUE)

write.csv(itk,"Data_AEGIS/itk.csv", row.names = FALSE)
