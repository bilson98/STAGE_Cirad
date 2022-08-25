### packages
#install.packages('rJava', .libPaths()[1])
#install.packages('XLConnect')
#options(java.parameters = "-Xmx1024m")
#library(XLConnect)

#### Selection of trials to be used ###

# list of available plot in ECOFI database
MATPLOTTREAT <- read.csv2(file = 'DATA/ECOFI_DATAVERSE/ecofi_plot.csv',header = T,sep =";")
head(MATPLOTTREAT)

# list of plot that we will keep (all of them at the moment)
PLOTCODE <- as.vector(MATPLOTTREAT$plotcode)

# list of available cycle within the selected plots 
MATPLOTCYCLE <- read.csv2(file = 'DATA/ECOFI_DATAVERSE/ecofi_plotcycle.csv',header = T,sep =";")
MATPLOTCYCLE <- MATPLOTCYCLE[which(MATPLOTCYCLE$plotcode %in% PLOTCODE),]
head(MATPLOTCYCLE)

# list of cycle that we will keep (all of them at the moment)
PLOTCODECYCLE <- as.vector(MATPLOTCYCLE$plotcyclecode)

# details of cycle, cultivar, starting and endding dates within the selected cycle
MATVARCYCLE <- read.csv2(file='DATA/ECOFI_DATAVERSE/ecofi_varcycle.csv',header=T,sep=';')
MATVARCYCLE <- MATVARCYCLE[which(MATVARCYCLE$varcyclecode %in% PLOTCODECYCLE),]
head(MATVARCYCLE)

# List of available cultivar
LISTVAR <- levels(MATVARCYCLE$varcode)

# choice of one or more cultivar
VAR <- 'R570'
VAR <- c('R579','R579-reb')
MATVARCYCLE <- MATVARCYCLE[which(MATVARCYCLE$varcode %in% VAR),]
head(MATVARCYCLE)



###########################################
#### Création input trait
source('R_function_stics_input.R')





# Warning: Tec_ini.csv has to be in STICS_MAILING_INPUT
#INPUTTECIRRIGV10( IDCODE='ALL_R579')


###### mon exemple
INPUTUSMS(paste(unique(design$trialcode),unique(design$factor_level),sep = '_'),SETOUT='STICS_MAILING_INPUT/',IDCODE='DELL1_exple')
INPUTSOIL(IDCODE='DELL1_exple')
INPUTINI(IDCODE='DELL1_exple')
INPUTSTA(IDCODE='DELL1_exple')
INPUTWEATHER(IDCODE='DELL1_exple')
INPUTTEC( IDCODE='DELL1_exple')
INPUTOBS(IDCODE='DELL1_exple')
######



source('R_function_mailing_adapted.R')
library(STICSmailing)

# set input_dir and output dir paths
input_dir = "STICS_MAILING_INPUT"
excel_file = "Inputs_STICS_mailing_R579_IRRIG.xlsx"
output_dir = "STICS_USMs"

STICS_mailing_USMs2(stwF = input_dir,
                    Excel_file = "USMs_DELL1_exple.csv",
                    sheetIndex = "USMs",
                    stwR = output_dir)



STICS_mailing_Ini2(stwF = input_dir,
                   Excel_file = "INI_DELL1_exple.csv",
                   sheetIndex = "Ini",
                   stwR = output_dir)

STICS_mailing_Obs2(stwF = input_dir,
                   Excel_file = "OBS_DELL1_exple.csv",
                   sheetIndex = "Obs",
                   stwR = output_dir)

STICS_mailing_Sols2(stwF = input_dir,
                    Excel_file = "SOILS_DELL1_exple.csv",
                    sheetIndex = "Soils",
                    stwR = output_dir)


STICS_mailing_Tec2(stwF = input_dir,
                   Excel_file = "TEC_DELL1_exple.csv",
                   sheetIndex = "Tec",
                   stwR = output_dir)


STICS_mailing_Sta2(stwF = input_dir,
                   Excel_file = "STATION_DELL1_exple.csv",
                   sheetIndex = "Station",
                   stwR = output_dir)


STICS_mailing_Weather2(stwF = input_dir,
                       Excel_file1 = "USMs_DELL1_exple.csv",
                       Excel_file2 = "WEATHER_DELL1_exple.csv",
                       sheetIndex_USMs = "USMs",
                       sheetIndex_Weather = "Weather",
                       stwR = output_dir)










