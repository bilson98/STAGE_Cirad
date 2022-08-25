

######### Fait

INPUTUSMS <- function(TREATCODE,SETOUT='STICS_MAILING_INPUT/',IDCODE=''){
  
  # TREATCODE <- sort(as.vector(TREATCODE)) #exple : O.Fontaine_2018_factorLevel
  
  # MATSOIL <-
  #   read.csv(
  #     file = 'Data_AEGIS/soil_type.csv',  #' ECOFI_DATAVERSE/ecofi_genesoil.csv',
  #     header = T,
  #     #sep = ";",
  #     stringsAsFactors = F
  #   )
  
  # MATPC <-
  #   read.csv(
  #     file = '',  #' ECOFI_DATAVERSE/ecofi_plotcycle.csv',
  #     header = T,
  #     #sep = ";",
  #     stringsAsFactors = F
  #   )
  # 
  # MATVC <-
  #   read.csv(
  #     file = '',   #' ECOFI_DATAVERSE/ecofi_varcycle.csv',
  #     header = T,
  #     #sep = ";",
  #     stringsAsFactors = F
  #   )
  # 
  # MATP <-
  #   read.csv(
  #     file = '', #' ECOFI_DATAVERSE/ecofi_plot.csv',
  #     header = T,
  #     #sep = ";",
  #     stringsAsFactors = F
  #   )
  # 
  # MATT <-
  #   read.csv(
  #     file = 'Data_AEGIS/trial.csv',  #' ECOFI_DATAVERSE/ecofi_trial.csv',
  #     header = T,
  #     #sep = ";",
  #     stringsAsFactors = F
  #   )
  # 
  # MATTOT <- merge(MATPC, MATVC, by = 'plotcyclecode', all = T)
  # 
  # MATTOT <- merge(MATP, MATTOT, by = 'plotcode', all = T)
  # 
  # MATTOT <- merge(MATT, MATTOT, by = 'trialcode', all = T)
  # 
  # MATTOT <- MATTOT[which(MATTOT$varcyclecode %in% TREATCODE), ]
  # 
  # head(MATTOT)
  # 
  # MATWT <-
  #   read.csv(
  #     file = 'Data_AEGIS/ws_trial.csv',   #' ECOFI_DATAVERSE/ecofi_wstrial.csv', 
  #     header = T, 
  #     #sep =";"
  #     )
  # 
  # MATTOT <- merge(MATTOT,
  #                 MATWT,
  #                 by = 'trialcode',
  #                 all.x = T,
  #                 all.y = F)
  # head(MATWT)
  
  MATTOT <-
    read.csv(
      file = 'Data_AEGIS/design.csv',   #' ECOFI_DATAVERSE/ecofi_wstrial.csv', 
      header = T, 
      #sep =";"
    )
  
  meteo <-
    read.csv(
      file = 'Data_AEGIS/meteo.csv',   #' ECOFI_DATAVERSE/ecofi_wstrial.csv', 
      header = T, 
      #sep =";"
    )
  
  #MATTOT = merge(MATTOT,meteo, all = TRUE)
  
  # on prend un nouveau TREATCODE ordonné comme dans la data.frame
  TREATCODE <- as.vector(paste(unique(MATTOT$trial_code),unique(MATTOT$factor_level),sep = '_')) #MATTOT$varcyclecode
  
  usm_nom <-
    sapply(1:length(TREATCODE), function(i)
      paste('USM', TREATCODE[i], sep = '_'))
  
  finit <-
    sapply(1:length(TREATCODE), function(i)
      paste(usm_nom[i], '_ini.xml', sep = ''))
  
  nomsol <-
    sapply(1:length(TREATCODE), function(i)
      paste(usm_nom[i], '_soil', sep = ''))
    # sapply(1:length(TREATCODE), function(i)
    #   MATTOT$soilcode[which(MATTOT$varcyclecode == TREATCODE[i])])
  
  fstat <-
    sapply(1:length(TREATCODE), function(i)
      meteo$wscode[which(unique(meteo$trialcode) %in% unique(MATTOT$trial_code)[i])]) #MATTOT$varcyclecode == TREATCODE[i])
  
  fstation <-
    sapply(1:length(TREATCODE), function(i)
      paste(fstat[i], '_sta.xml', sep = ''))
  
  ###### format(tmp, "%j") -> conversion de la date tmp en julian day ######
  
  # DateIni <- sapply(1:length(TREATCODE), function(i) substr(MATTOT$cyclestartingdate[i],1,10))
  # DateEnd <- sapply(1:length(TREATCODE), function(i) substr(MATTOT$cyclendingdate[i],1,10))
  YearIni <-
    as.numeric(sapply(1:length(TREATCODE), function(i)
      substr(unique(MATTOT$starting_date[i]), 1, 4)))
  
  YearEnd <-
    as.numeric(sapply(1:length(TREATCODE), function(i)
      substr(unique(MATTOT$ending_date[i]), 1, 4)))
  
  DateIni <- as.Date(unique(MATTOT$starting_date), format = '%Y-%m-%d')
  
  DateEnd <- as.Date(unique(MATTOT$ending_date), format = '%Y-%m-%d')
  
  DateRef <-
    sapply(1:length(TREATCODE), function(i)
      paste(YearIni[i], '-01-01', sep = ''))
  
  DateRef <- as.Date(DateRef, format = '%Y-%m-%d')
  
  datedebut <- as.numeric(DateIni - DateRef) + 1
  
  datefin <-
    as.numeric(DateEnd - DateRef) + 1 + 1 ## Warning for STICS v10 add +1 because if observation were made the last day, the model haverst at the beginning of the day
  
  ID <- which(datefin > 730)
  
  datedebut[ID] <- 1#datedebut[ID]
  
  datefin[ID] <- datefin[ID] - 365
  
  YearIni <-
    as.numeric(sapply(1:length(TREATCODE), function(i)
      substr(unique(MATTOT$starting_date[i]), 1, 4)))
  
  YearEnd <-
    as.numeric(sapply(1:length(TREATCODE), function(i)
      substr(unique(MATTOT$ending_date[i]), 1, 4)))
  
  YearIni[ID] <- YearIni[ID] + 1
  
  YearEnd[ID] <- YearEnd[ID]
  
  fclim1 <-
    sapply(1:length(TREATCODE), function(i)
      paste(fstat[i], '.', YearIni[i], sep = ''))
  
  fclim2 <-
    sapply(1:length(TREATCODE), function(i)
      paste(fstat[i], '.', YearEnd[i], sep = ''))
  
  #data.frame(DateIni[ID] , DateEnd[ID], usm_nom[ID])
  
  culturean <-
    rep(2, length(TREATCODE))
  
  ID <- which(YearIni == YearEnd)
  
  culturean[ID] <- 1
  
  nbplantes <- rep(1, length(TREATCODE))
  
  codesimul <- rep(0, length(TREATCODE))
  
  fplt_1 <- rep('Sugarcane_CM2_plt.xml', length(TREATCODE))
  
  ftec_1 <-
    sapply(1:length(TREATCODE), function(i)
      paste('USM_', TREATCODE[i], '_tec.xml', sep = ''))
  
  flai_1 <- rep('NULL', length(TREATCODE))
  
  fplt_2 <- rep('NULL', length(TREATCODE))
  
  ftec_2 <- rep('NULL', length(TREATCODE))
  
  flai_2 <- rep('NULL', length(TREATCODE))
  
  MATOUT <-
    data.frame(
      usm_nom = usm_nom,
      datedebut = datedebut,
      datefin = datefin,
      finit = finit,
      nomsol = nomsol,
      fstation = fstation,
      fclim1 = fclim1,
      fclim2 = fclim2,
      culturean = culturean,
      nbplantes = nbplantes,
      codesimul = codesimul,
      fplt_1 = fplt_1,
      ftec_1 = ftec_1,
      flai_1 = flai_1,
      fplt_2 = fplt_2,
      ftec_2 = ftec_2,
      flai_2 = flai_2
    )
  
  write.csv2(
    MATOUT,
    file = paste('STICS_MAILING_INPUT/USMs_', IDCODE, '.csv', sep = ''),
    quote = F,
    row.names = F,
    col.names = T,
    dec = '.',
    sep = ';'
  )
  
  print(paste(dim(MATOUT)[1], ' USMs created'))
  #
  
}



########## Fait
  
INPUTINI <- function(IDCODE){
  library(stringr)
  MATUSMS <-
    read.csv2(
      paste('STICS_MAILING_INPUT/USMs_', IDCODE, '.csv', sep = ''),
      header = T,
      sep = ';'
    )
  MATTEMP <- data.frame(plotcyclecode = substring(MATUSMS$usm_nom, 5))
  # MATPC <-
  #   read.csv(
  #     file = '', #' ECOFI_DATAVERSE/ecofi_plotcycle.csv',
  #     header = T,
  #     #sep = ";",
  #     stringsAsFactors = F
  #   )
  # MATTEMP <- merge(MATTEMP, MATPC[, c(1, 3)], by = 'plotcyclecode')
  # CYCLE <- MATTEMP$cycle
  
  LEN <- dim(MATUSMS)[1]

  CYCLE=1
  
  Ini_name <- MATUSMS$finit
  nbplantes <- rep(1, LEN)
  stade0_Crop1 <- rep('plt', LEN)
  stade0_Crop1 <- replace(stade0_Crop1, CYCLE > 0, 'lev')
  lai0_Crop1 <- rep(0, LEN)
  lai0_Crop1 <- replace(lai0_Crop1, CYCLE > 0, 0.001)
  masec0_Crop1	 <- rep(0, LEN)
  masec0_Crop1 <- replace(masec0_Crop1, CYCLE > 0, 0.01)
  QNplante0_Crop1	 <- rep(0, LEN)
  QNplante0_Crop1 <- replace(QNplante0_Crop1, CYCLE > 0, 0.001)
  magrain0_Crop1	 <- rep(0, LEN)
  zrac0_Crop1	 <- rep(0, LEN)
  zrac0_Crop1 <- replace(zrac0_Crop1, CYCLE > 0, 50)
  resperenne0_Crop1 <- rep(0, LEN)
  resperenne0_Crop1 <- replace(resperenne0_Crop1, CYCLE > 0, 1)
  densinitial_1_Crop1	 <- rep(0, LEN)
  densinitial_1_Crop1 <- replace(densinitial_1_Crop1, CYCLE > 0, 0.5)
  densinitial_2_Crop1	 <- rep(0, LEN)
  densinitial_2_Crop1 <- replace(densinitial_2_Crop1, CYCLE > 0, 0.5)
  densinitial_3_Crop1	 <- rep(0, LEN)
  densinitial_3_Crop1 <- replace(densinitial_3_Crop1, CYCLE > 0, 0.2)
  densinitial_4_Crop1	 <- rep(0, LEN)
  densinitial_4_Crop1 <- replace(densinitial_4_Crop1, CYCLE > 0, 0.05)
  densinitial_5_Crop1	 <- rep(0, LEN)
  stade0_Crop2	 <- rep(0, LEN)
  lai0_Crop2	 <- rep(0, LEN)
  masec0_Crop2	 <- rep(0, LEN)
  QNplante0_Crop2	 <- rep(0, LEN)
  magrain0_Crop2	 <- rep(0, LEN)
  zrac0_Crop2	 <- rep(0, LEN)
  resperenne0_Crop2	 <- rep(0, LEN)
  densinitial_1_Crop2	 <- rep(0, LEN)
  densinitial_2_Crop2	 <- rep(0, LEN)
  densinitial_3_Crop2	 <- rep(0, LEN)
  densinitial_4_Crop2	 <- rep(0, LEN)
  densinitial_5_Crop2	 <- rep(0, LEN)
  hinit_1	 <- rep(0, LEN)
  hinit_2	 <- rep(0, LEN)
  hinit_3	 <- rep(0, LEN)
  hinit_4	 <- rep(0, LEN)
  hinit_5	 <- rep(0, LEN)
  NO3init_1	 <- rep(32, LEN)
  NO3init_2	 <- rep(12, LEN)
  NO3init_3	 <- rep(9, LEN)
  NO3init_4	 <- rep(9, LEN)
  NO3init_5	 <- rep(9, LEN)
  NH4init_1	 <- rep(0, LEN)
  NH4init_2	 <- rep(0, LEN)
  NH4init_3	 <- rep(0, LEN)
  NH4init_4	 <- rep(0, LEN)
  NH4init_5 <- rep(0, LEN)
  
  MATOUT <-
    data.frame(
      Ini_name,
      nbplantes,
      stade0_Crop1,
      lai0_Crop1,
      masec0_Crop1,
      QNplante0_Crop1,
      magrain0_Crop1,
      zrac0_Crop1,
      resperenne0_Crop1,
      densinitial_1_Crop1,
      densinitial_2_Crop1,
      densinitial_3_Crop1,
      densinitial_4_Crop1,
      densinitial_5_Crop1,
      stade0_Crop2,
      lai0_Crop2,
      masec0_Crop2,
      QNplante0_Crop2,
      magrain0_Crop2,
      zrac0_Crop2,
      resperenne0_Crop2,
      densinitial_1_Crop2,
      densinitial_2_Crop2,
      densinitial_3_Crop2,
      densinitial_4_Crop2,
      densinitial_5_Crop2,
      hinit_1,
      hinit_2,
      hinit_3,
      hinit_4,
      hinit_5,
      NO3init_1,
      NO3init_2,
      NO3init_3,
      NO3init_4,
      NO3init_5,
      NH4init_1,
      NH4init_2,
      NH4init_3,
      NH4init_4,
      NH4init_5
    )
  
  write.csv2(
    MATOUT,
    file = paste('STICS_MAILING_INPUT/INI_', IDCODE, '.csv', sep = ''),
    quote = F,
    row.names = F,
    col.names = T,
    dec = '.',
    sep = ';'
  )
  print(paste('Initial conditions created'))
  
}






######## Fait

INPUTSOIL <- function(IDCODE){
  
  MATUSMS <-
    read.csv2(
      paste('STICS_MAILING_INPUT/USMs_', IDCODE, '.csv', sep = ''),
      header = T,
      sep = ';'
    )
  
  TREATSOIL <- MATUSMS$nomsol
  
  # MATSOIL <-
  #   read.csv(
  #     file = 'Data_AEGIS/soil_type.csv',  #' ECOFI_DATAVERSE//ecofi_genesoil.csv',
  #     header = T,
  #     #sep = ";",
  #     stringsAsFactors = F
  #   )
  # MATLAYSOIL <-
  #   read.csv(
  #     file = '',   #' ECOFI_DATAVERSE/ecofi_layersoil.csv',
  #     header = T,
  #     dec = '.',
  #     #sep = ";",
  #     stringsAsFactors = F
  #   )
  
  # MATSOIL <- MATSOIL[which(MATSOIL$soilcode %in% TREATSOIL), ]
  # LEN <- dim(MATSOIL)[1]
  # 
  Soil_name <- TREATSOIL
  
  epc_1 <- c()
  epc_2 <- c()
  epc_3 <- c()
  epc_4 <- c()
  epc_5 <- c()
  HCCF_1 <- c()
  HCCF_2 <- c()
  HCCF_3 <- c()
  HCCF_4 <- c()
  HCCF_5 <- c()
  HMINF_1 <-c()
  HMINF_2 <- c()
  HMINF_3 <- c()
  HMINF_4 <- c()
  HMINF_5 <- c()
  DAF_1 <- c()
  DAF_2 <- c()
  DAF_3 <- c()
  DAF_4 <- c()
  DAF_5 <- c()
  
  for (i in 1:length(Soil_name)) {
    
    MATTEMP <- NULL#MATLAYSOIL[which(MATLAYSOIL$soilcode == Soil_name[i]), ]
    NBLAY <- 0#dim(MATTEMP)[1] / 5
    
    if (NBLAY %in% c(0, 1)) {
      IDSOIL <- Soil_name[i]
      #MATSOIL[which(MATSOIL$soilcode == Soil_name[i]), ]
      epc <- c(20,20,20,40,40)
        # rep(round(MATSOIL[which(MATSOIL$soilcode == Soil_name[i]), ]$depthsoil /
        #             5, 0), 5)
      HCCF <- rep(40, 5)
        #rep((300 + MATSOIL[which(MATSOIL$soilcode == Soil_name[i]), ]$ru) / 10, 5)
      HMINF <- rep(27, 5)
      DAF <- rep(1.1, 5)
    }
    
    if (NBLAY > 1) {
      epc <- c()
      HCCF <- c()
      HMINF <- c()
      DAF <- c()
      
      for (j in 1:min(5, NBLAY)) {
        epc <-
          c(epc, MATTEMP$layerval[MATTEMP$layerlib == 'epaiss'][MATTEMP$layernum[MATTEMP$layerlib ==
                                                                                   'epaiss'] == j] * 1)
        HCCF <-
          c(HCCF, MATTEMP$layerval[MATTEMP$layerlib == 'hcc'][MATTEMP$layernum[MATTEMP$layerlib ==
                                                                                 'hcc'] == j] * 100)
        HMINF <-
          c(HMINF, MATTEMP$layerval[MATTEMP$layerlib == 'hpf'][MATTEMP$layernum[MATTEMP$layerlib ==
                                                                                  'hpf'] == j] * 100)
        DAF <-
          c(DAF, MATTEMP$layerval[MATTEMP$layerlib == 'bdens'][MATTEMP$layernum[MATTEMP$layerlib ==
                                                                                  'bdens'] == j] * 1)
      }
      
      if (NBLAY < 5) {
        epc <- c(epc, rep(epc[NBLAY], 5 - NBLAY))
        HCCF <- c(HCCF, rep(HCCF[NBLAY], 5 - NBLAY))
        HMINF <- c(HMINF, rep(HMINF[NBLAY], 5 - NBLAY))
        DAF <- c(DAF, rep(DAF[NBLAY], 5 - NBLAY))
      }
      
    }
    
    epc_1 <-
      c(epc_1, epc[1])
    epc_2 <-
      c(epc_2, epc[2])
    epc_3 <-
      c(epc_3, epc[3])
    epc_4 <- c(epc_4, epc[4])
    epc_5 <- c(epc_5, epc[5])
    HCCF_1 <-
      c(HCCF_1, HCCF[1])
    HCCF_2 <-
      c(HCCF_2, HCCF[2])
    HCCF_3 <-
      c(HCCF_3, HCCF[3])
    HCCF_4 <- c(HCCF_4, HCCF[4])
    HCCF_5 <- c(HCCF_5, HCCF[5])
    HMINF_1 <-
      c(HMINF_1, HMINF[1])
    HMINF_2 <-
      c(HMINF_2, HMINF[2])
    HMINF_3 <-
      c(HMINF_3, HMINF[3])
    HMINF_4 <- c(HMINF_4, HMINF[4])
    HMINF_5 <- c(HMINF_5, HMINF[5])
    DAF_1 <-
      c(DAF_1, DAF[1])
    DAF_2 <-
      c(DAF_2, DAF[2])
    DAF_3 <-
      c(DAF_3, DAF[3])
    DAF_4 <- c(DAF_4, DAF[4])
    DAF_5 <- c(DAF_5, DAF[5])
  }
  
  epc_5 <- epc_5 + 40
  argi <- rep(17, LEN)
  norg <- rep(0.12, LEN)
  profhum <- rep(35, LEN)
  calc <- rep(1, LEN)
  pH <- rep(6, LEN)
  concseuil <- rep(0, LEN)
  albedo <- rep(0.2, LEN)
  q0 <- rep(0, LEN)
  ruisolnu <- rep(0, LEN)
  obstarac <- rep(150,LEN)
  pluiebat <- rep(50, LEN)
  mulchbat <- rep(0.5, LEN)
  zesx <- rep(60, LEN)
  cfes <- rep(5, LEN)
  z0solnu <- rep(0.01, LEN)
  CsurNsol <- rep(13, LEN)
  penterui <- rep(0.33, LEN)
  codecailloux <- rep(2, LEN)
  codemacropor <- rep(2, LEN)
  codefente <- rep(2, LEN)
  codrainage <- rep(2, LEN)
  profimper <- rep(0, LEN)
  ecartdrain <- rep(0, LEN)
  ksol <- rep(0, LEN)
  profdrain <- rep(0, LEN)
  coderemontcap <- rep(2, LEN)
  capiljour <- rep(0, LEN)
  humcapil <- rep(0, LEN)
  codenitrif <- rep(2, LEN)
  codedenit <- rep(2, LEN)
  profdenit <- rep(20, LEN)
  vpotdenit <- rep(2.6, LEN)
  cailloux_1 <- rep(0, LEN)
  typecailloux_1 <- rep(1, LEN)
  infil_1 <- rep(50, LEN)
  epd_1 <- rep(10, LEN)
  cailloux_2 <- rep(0, LEN)
  typecailloux_2 <- rep(1, LEN)
  infil_2 <- rep(50, LEN)
  epd_2 <- rep(10, LEN)
  cailloux_3 <- rep(0, LEN)
  typecailloux_3 <- rep(1, LEN)
  infil_3 <- rep(50, LEN)
  epd_3 <- rep(10, LEN)
  cailloux_4 <- rep(0, LEN)
  typecailloux_4 <- rep(1, LEN)
  infil_4 <- rep(0, LEN)
  epd_4 <- rep(0, LEN)
  cailloux_5 <- rep(0, LEN)
  typecailloux_5 <- rep(1, LEN)
  infil_5 <- rep(0, LEN)
  epd_5 <- rep(0, LEN)
  
  MATOUT <-
    data.frame(
      Soil_name,
      argi,
      norg,
      profhum,
      calc,
      pH,
      concseuil,
      albedo,
      q0,
      ruisolnu,
      obstarac,
      pluiebat,
      mulchbat,
      zesx,
      cfes,
      z0solnu,
      CsurNsol,
      penterui,
      codecailloux,
      codemacropor,
      codefente,
      codrainage,
      profimper,
      ecartdrain,
      ksol,
      profdrain,
      coderemontcap,
      capiljour,
      humcapil,
      codenitrif,
      codedenit,
      profdenit,
      vpotdenit,
      epc_1,
      HCCF_1,
      HMINF_1,
      DAF_1,
      cailloux_1,
      typecailloux_1,
      infil_1,
      epd_1,
      epc_2,
      HCCF_2,
      HMINF_2,
      DAF_2,
      cailloux_2,
      typecailloux_2,
      infil_2,
      epd_2,
      epc_3,
      HCCF_3,
      HMINF_3,
      DAF_3,
      cailloux_3,
      typecailloux_3,
      infil_3,
      epd_3,
      epc_4,
      HCCF_4,
      HMINF_4,
      DAF_4,
      cailloux_4,
      typecailloux_4,
      infil_4,
      epd_4,
      epc_5,
      HCCF_5,
      HMINF_5,
      DAF_5,
      cailloux_5,
      typecailloux_5,
      infil_5,
      epd_5
    )
  
  write.csv2(
    MATOUT,
    file = paste('STICS_MAILING_INPUT/SOILS_', IDCODE, '.csv', sep = ''),
    quote = F,
    row.names = F,
    col.names = T,
    dec = '.',
    sep = ';'
  )
  
  print(paste('Soils created'))

}






########## Fait

INPUTTEC <- function(IDCODE){
  MATUSMS <-
    read.csv2(
      paste('STICS_MAILING_INPUT/USMs_', IDCODE, '.csv', sep = ''),
      header = T,
      sep = ';',
      stringsAsFactors = F
    )
  require(stringr)
  TREATCODE <-
    sapply(1:dim(MATUSMS)[1], function(i)
      str_sub(MATUSMS$usm_nom[i], start = 5))
  
  TREATTEC <- MATUSMS$ftec_1
  LEN <- length(TREATTEC)
  
  MATINI <-
    read.csv2('STICS_MAILING_INPUT/Tec_ini.csv',
              header = T,
              sep = ';')
  MATINI[, 1:10]
  VALINI <- MATINI[which(MATINI$ITK_name == 'USM_DEFAULT'), ]
  MATINIout <-
    VALINI
#  for (i in 1:(length(TREATTEC) - 1)) {
#    MATINIout <- rbind(MATINIout, VALINI)
#  }
  
  MATINIout$ITK_name <- TREATTEC
  MATINIout$iplt0 <- MATUSMS$datedebut
  MATINIout$profsem <- rep(10, LEN)
  MATINIout$densitesem <- rep(8, LEN)
  if (IDCODE == 'ALL_R570') {
    MATINIout$variete <- rep(3, LEN)
  }
  #MATINIout$codetradtec <- rep(2,LEN)
  MATINIout$irec <- MATUSMS$datefin
  MATINIout$engrais <- rep(3, LEN)
  MATINIout$julapN_or_sum_upvt_1 <- MATUSMS$datedebut
  MATINIout$Amount_N_Ferti_1 <- rep(220, LEN)
  MATINIout$codrecolte <- rep(1, LEN)
  
  
  # MATPC <-
  #   read.csv(file = 'Data_AEGIS/itk.csv',   #' ECOFI_DATAVERSE/ecofi_plotcycle.csv', 
  #             header = T, 
  #             #sep =";"
  #             )
  # MATVC <-
  #   read.csv(file ='',   #' ECOFI_DATAVERSE/ecofi_varcycle.csv', 
  #             header = T, 
  #             #sep =";"
  #             )
  MATMERGE <-
    read.csv(file = 'Data_AEGIS/itk.csv',    
             header = T, 
             #sep =";"
    )
  MATMERGE <- MATMERGE[which(paste(unique(MATMERGE$trial_code),unique(MATMERGE$factor_level),sep = '_') %in% TREATCODE), ] #MATMERGE$varcyclecode
  # IRRCODE <-
  #   as.vector(sapply(1:length(TREATCODE), function(i)
  #     MATMERGE$irricode[which(paste(unique(MATMERGE$trial_code),unique(MATMERGE$factor_level),sep = '_') == TREATCODE[i])]))
  # 
  # MATIRRDATA <-
  #   read.csv(
  #     file = '',   #' ECOFI_DATAVERSE/ecofi_irridata.csv',
  #     header = T,
  #     #sep = ";",
  #     stringsAsFactors = F
  #   )
  # head(MATIRRDATA)
  IRRCODE= 'RAINFED'  
  if (unique(MATMERGE$irrigated=='Y')|is.na(unique(MATMERGE$irrigated))){
    IRRCODE= 'NC'
  }
  
  MATINIout[, 'codecalirrig'] <-
    replace(MATINIout[, 'codecalirrig'], IRRCODE == 'NC', 1)
  MATINIout[, 'ratiol'] <-
    replace(MATINIout[, 'ratiol'], IRRCODE == 'NC', 0.9)
  MATINIout[, 'dosimx'] <-
    replace(MATINIout[, 'dosimx'], IRRCODE == 'NC', 25)
  
  MATINIout[, 'codecalirrig'] <-
    replace(MATINIout[, 'codecalirrig'], IRRCODE != 'RAINFED', 1)
  MATINIout[, 'ratiol'] <-
    replace(MATINIout[, 'ratiol'], IRRCODE != 'RAINFED', 0.9)
  MATINIout[, 'dosimx'] <-
    replace(MATINIout[, 'dosimx'], IRRCODE != 'RAINFED', 5)
  MATOUT <- MATINIout
  #  DIMIRR <- c()
  #  for (i in 1:length(IRRCODE)){
  #    if ((IRRCODE[i] != 'RAINFED')&(IRRCODE[i] != 'NC')){
  #      MATTEMP <- MATIRRDATA[which(MATIRRDATA$irricode==IRRCODE[i]),]
  #      DIMIRR <- c(DIMIRR,dim(MATTEMP)[1])
  #    }}
  #  MAXIRR <- max(DIMIRR)
  #  MATOUT <- MATINIout
  #  for (j in 21:(MAXIRR+10)){
  #    NAME1 <- paste('julapI_or_sum_upvt_',j,sep="")
  #    NAME2 <- paste('Amount_Irrig_',j,sep="")
  #    MATOUT <- cbind(MATOUT,rep(999,length(TREATTEC)));
  #    names(MATOUT)[294+(j-21)+(j-20)] <- NAME1
  #    MATOUT <- cbind(MATOUT,rep(0,length(TREATTEC)));
  #    names(MATOUT)[294+(j-19)+(j-21)] <- NAME2
  #  }
  
  #  for (i in 1:length(TREATTEC)){
  #    if ((IRRCODE[i] != 'RAINFED')&(IRRCODE[i] != 'NC')){
  #      MATTEMP <- MATIRRDATA[which(MATIRRDATA$irricode==IRRCODE[i]),]
  #      INIYEAR <- substr(MATTEMP$irridate[1],7,10) ; INIDAT <- as.Date(paste(INIYEAR,'-01-01',sep=""),format="%Y-%m-%d")
  #      IRRDAT <- MATTEMP$irridate ; IRRDAT <- as.Date(IRRDAT,format="%d/%m/%Y")
  #      for (j in 1:dim(MATTEMP)[1]){
  #        NAME1 <- paste('julapI_or_sum_upvt_',j,sep="")
  #        NAME2 <- paste('Amount_Irrig_',j,sep="")
  #        MATOUT[,NAME1][i] <- as.numeric(IRRDAT[j]-INIDAT)+1
  #        MATOUT[,NAME2][i] <- MATTEMP$irridose[j]
  #      }
  #      }
  
  #  }
  
  
  write.csv2(
    MATOUT,
    file = paste('STICS_MAILING_INPUT/TEC_', IDCODE, '.csv', sep = ''),
    quote = F,
    row.names = F,
    col.names = T,
    dec = '.',
    sep = ';'
  )
  print(paste('Tec created'))
  
}








##########

 INPUTTECV10 <- function(IDCODE){
  MATUSMS <-
    read.csv2(
      paste('STICS_MAILING_INPUT/USMs_', IDCODE, '.csv', sep = ''),
      header = T,
      sep = ';',
      stringsAsFactors = F
    )
  require(stringr)
  TREATCODE <-
    sapply(1:dim(MATUSMS)[1], function(i)
      str_sub(MATUSMS$usm_nom[i], start = 5))
  
  TREATTEC <- MATUSMS$ftec_1
  LEN <- length(TREATTEC)
  
  MATINI <-
    read.csv2('STICS_MAILING_INPUT/Tec_ini.csv',
              header = T,
              sep = ';')
  MATINI[, 1:10]
  VALINI <- MATINI[which(MATINI$ITK_name == 'USM_DEFAULT'), ]
  MATINIout <-
    VALINI
  for (i in 1:(length(TREATTEC) - 1)) {
    MATINIout <- rbind(MATINIout, VALINI)
  }
  
  MATINIout$ITK_name <- TREATTEC
  MATINIout$iplt0 <- MATUSMS$datedebut
  MATINIout$profsem <- rep(10, LEN)
  MATINIout$densitesem <- rep(8, LEN)
  if (IDCODE == 'ALL_R570') {
    MATINIout$variete <- rep(3, LEN)
  }
  #MATINIout$codetradtec <- rep(2,LEN)
  MATINIout$codestade <- rep(2, LEN)
  MATINIout$codetaille <- rep(2, LEN)
  MATINIout$jultaille <- MATUSMS$datefin
  MATINIout$irec <- rep(999, LEN)
  MATINIout$engrais <- rep(3, LEN)
  MATINIout$julapN_or_sum_upvt_1 <- MATUSMS$datedebut
  MATINIout$Amount_N_Ferti_1 <- rep(220, LEN)
  MATINIout$codrecolte <- rep(1, LEN)
  
  
  MATPC <-
    read.csv(file = '',   #' ECOFI_DATAVERSE/ecofi_plotcycle.csv', 
              header = T, 
              #sep =";"
             )
  MATVC <-
    read.csv(file = '',    #' ECOFI_DATAVERSE/ecofi_varcycle.csv', 
              header = T, 
              #sep =";"
             )
  MATMERGE <-
    merge(MATPC, MATVC, all.y = T)
  MATMERGE <- MATMERGE[which(MATMERGE$varcyclecode %in% TREATCODE), ]
  IRRCODE <-
    as.vector(sapply(1:length(TREATCODE), function(i)
      MATMERGE$irricode[which(MATMERGE$varcyclecode == TREATCODE[i])]))
  
  MATIRRDATA <-
    read.csv(
      file = '', #'ECOFI_DATAVERSE/ecofi_irridata.csv',
      header = T,
      #sep = ";",
      stringsAsFactors = F
    )
  head(MATIRRDATA)
  
  MATINIout[, 'codecalirrig'] <-
    replace(MATINIout[, 'codecalirrig'], IRRCODE == 'NC', 1)
  MATINIout[, 'ratiol'] <-
    replace(MATINIout[, 'ratiol'], IRRCODE == 'NC', 0.9)
  MATINIout[, 'dosimx'] <-
    replace(MATINIout[, 'dosimx'], IRRCODE == 'NC', 25)
  
  MATINIout[, 'codecalirrig'] <-
    replace(MATINIout[, 'codecalirrig'], IRRCODE != 'RAINFED', 1)
  MATINIout[, 'ratiol'] <-
    replace(MATINIout[, 'ratiol'], IRRCODE != 'RAINFED', 0.9)
  MATINIout[, 'dosimx'] <-
    replace(MATINIout[, 'dosimx'], IRRCODE != 'RAINFED', 5)
  MATOUT <- MATINIout
  #  DIMIRR <- c()
  #  for (i in 1:length(IRRCODE)){
  #    if ((IRRCODE[i] != 'RAINFED')&(IRRCODE[i] != 'NC')){
  #      MATTEMP <- MATIRRDATA[which(MATIRRDATA$irricode==IRRCODE[i]),]
  #      DIMIRR <- c(DIMIRR,dim(MATTEMP)[1])
  #    }}
  #  MAXIRR <- max(DIMIRR)
  #  MATOUT <- MATINIout
  #  for (j in 21:(MAXIRR+10)){
  #    NAME1 <- paste('julapI_or_sum_upvt_',j,sep="")
  #    NAME2 <- paste('Amount_Irrig_',j,sep="")
  #    MATOUT <- cbind(MATOUT,rep(999,length(TREATTEC)));
  #    names(MATOUT)[294+(j-21)+(j-20)] <- NAME1
  #    MATOUT <- cbind(MATOUT,rep(0,length(TREATTEC)));
  #    names(MATOUT)[294+(j-19)+(j-21)] <- NAME2
  #  }
  
  #  for (i in 1:length(TREATTEC)){
  #    if ((IRRCODE[i] != 'RAINFED')&(IRRCODE[i] != 'NC')){
  #      MATTEMP <- MATIRRDATA[which(MATIRRDATA$irricode==IRRCODE[i]),]
  #      INIYEAR <- substr(MATTEMP$irridate[1],7,10) ; INIDAT <- as.Date(paste(INIYEAR,'-01-01',sep=""),format="%Y-%m-%d")
  #      IRRDAT <- MATTEMP$irridate ; IRRDAT <- as.Date(IRRDAT,format="%d/%m/%Y")
  #      for (j in 1:dim(MATTEMP)[1]){
  #        NAME1 <- paste('julapI_or_sum_upvt_',j,sep="")
  #        NAME2 <- paste('Amount_Irrig_',j,sep="")
  #        MATOUT[,NAME1][i] <- as.numeric(IRRDAT[j]-INIDAT)+1
  #        MATOUT[,NAME2][i] <- MATTEMP$irridose[j]
  #      }
  #      }
  
  #  }
  
  
  write.csv2(
    MATOUT,
    file = paste('STICS_MAILING_INPUT/TEC_', IDCODE, '.csv', sep = ''),
    quote = F,
    row.names = F,
    col.names = T,
    dec = '.',
    sep = ';'
  )
  print(paste('Tec created'))
  
  
}








 INPUTTECIRRIG <- function(IDCODE){
  MATUSMS <-
    read.csv2(
      paste('STICS_MAILING_INPUT/USMs_', IDCODE, '.csv', sep = ''),
      header = T,
      sep = ';',
      stringsAsFactors = F
    )
  require(stringr)
  TREATCODE <-
    sapply(1:dim(MATUSMS)[1], function(i)
      str_sub(MATUSMS$usm_nom[i], start = 5))
  
  TREATTEC <- MATUSMS$ftec_1
  LEN <- length(TREATTEC)
  
  MATINI <-
    read.csv2('STICS_MAILING_INPUT/Tec_ini.csv',
              header = T,
              sep = ';')
  MATINI[, 1:10]
  VALINI <- MATINI[which(MATINI$ITK_name == 'USM_DEFAULT'), ]
  MATINIout <-
    VALINI
  for (i in 1:(length(TREATTEC) - 1)) {
    MATINIout <- rbind(MATINIout, VALINI)
  }
  
  MATINIout$ITK_name <- TREATTEC
  MATINIout$iplt0 <- MATUSMS$datedebut
  MATINIout$profsem <- rep(10, LEN)
  MATINIout$densitesem <- rep(8, LEN)
  if (IDCODE == 'ALL_R570') {
    MATINIout$variete <- rep(3, LEN)
  }
  #MATINIout$codetradtec <- rep(2,LEN)
  MATINIout$irec <- MATUSMS$datefin
  MATINIout$engrais <- rep(3, LEN)
  MATINIout$julapN_or_sum_upvt_1 <- MATUSMS$datedebut
  MATINIout$Amount_N_Ferti_1 <- rep(220, LEN)
  MATINIout$codrecolte <- rep(1, LEN)
  
  
  MATPC <-
    read.csv(file ='',    #' ECOFI_DATAVERSE/ecofi_plotcycle.csv', 
              header = T, 
              #sep =";"
             )
  MATVC <-
    read.csv(file ='',     #' ECOFI_DATAVERSE/ecofi_varcycle.csv', 
              header = T, 
              #sep =";"
              )
  MATMERGE <-
    merge(MATPC, MATVC, all.y = T)
  MATMERGE <- MATMERGE[which(MATMERGE$varcyclecode %in% TREATCODE), ]
  IRRCODE <-
    as.vector(sapply(1:length(TREATCODE), function(i)
      MATMERGE$irricode[which(MATMERGE$varcyclecode == TREATCODE[i])]))
  
  MATIRRDATA <-
    read.csv(
      file = '',    #'ECOFI_DATAVERSE/ecofi_irridata.csv',
      header = T,
      #sep = ";",
      stringsAsFactors = F
    )
  head(MATIRRDATA)
  
  # when NC means that there is irrgation but not indicated
  MATINIout[, 'codecalirrig'] <-
    replace(MATINIout[, 'codecalirrig'], IRRCODE == 'NC', 1)
  MATINIout[, 'ratiol'] <-
    replace(MATINIout[, 'ratiol'], IRRCODE == 'NC', 0.9)
  MATINIout[, 'dosimx'] <-
    replace(MATINIout[, 'dosimx'], IRRCODE == 'NC', 25)
  
  MATOUT <- MATINIout
  DIMIRR <- c()
  for (i in 1:length(IRRCODE)) {
    if ((IRRCODE[i] != 'RAINFED') & (IRRCODE[i] != 'NC')) {
      MATTEMP <- MATIRRDATA[which(MATIRRDATA$irricode == IRRCODE[i]), ]
      DIMIRR <- c(DIMIRR, dim(MATTEMP)[1])
    }
  }
  MAXIRR <- max(DIMIRR)
  MATOUT <- MATINIout
  for (j in 21:(MAXIRR + 10)) {
    NAME1 <- paste('julapI_or_sum_upvt_', j, sep = "")
    NAME2 <- paste('Amount_Irrig_', j, sep = "")
    MATOUT <- cbind(MATOUT, rep(999, length(TREATTEC)))
    
    names(MATOUT)[294 + (j - 21) + (j - 20)] <- NAME1
    MATOUT <- cbind(MATOUT, rep(0, length(TREATTEC)))
    
    names(MATOUT)[294 + (j - 19) + (j - 21)] <- NAME2
  }
  
  for (i in 1:length(TREATTEC)) {
    if ((IRRCODE[i] != 'RAINFED') & (IRRCODE[i] != 'NC')) {
      MATTEMP <- MATIRRDATA[which(MATIRRDATA$irricode == IRRCODE[i]), ]
      INIYEAR <-
        substr(MATTEMP$irridate[1], 7, 10)
      INIDAT <- as.Date(paste(INIYEAR, '-01-01', sep = ""), format = "%Y-%m-%d")
      IRRDAT <-
        MATTEMP$irridate
      IRRDAT <- as.Date(IRRDAT, format = "%d/%m/%Y")
      for (j in 1:dim(MATTEMP)[1]) {
        NAME1 <- paste('julapI_or_sum_upvt_', j, sep = "")
        NAME2 <- paste('Amount_Irrig_', j, sep = "")
        MATOUT[, NAME1][i] <- as.numeric(IRRDAT[j] - INIDAT) + 1
        MATOUT[, NAME2][i] <- MATTEMP$irridose[j]
      }
    }
    
  }
  
  
  write.csv2(
    MATOUT,
    file = paste('STICS_MAILING_INPUT/TEC_', IDCODE, '.csv', sep = ''),
    quote = F,
    row.names = F,
    col.names = T,
    dec = '.',
    sep = ';'
  )
  print(paste('Tec created'))
  
  
}








INPUTTECIRRIGV10 <- function(IDCODE){
  MATUSMS <-
    read.csv2(
      paste('STICS_MAILING_INPUT/USMs_', IDCODE, '.csv', sep = ''),
      header = T,
      sep = ';',
      stringsAsFactors = F
    )
  require(stringr)
  TREATCODE <-
    sapply(1:dim(MATUSMS)[1], function(i)
      str_sub(MATUSMS$usm_nom[i], start = 5))
  
  TREATTEC <- MATUSMS$ftec_1
  LEN <- length(TREATTEC)
  
  MATINI <-
    read.csv2('STICS_MAILING_INPUT/Tec_ini.csv',
              header = T,
              sep = ';')
  MATINI[, 1:10]
  VALINI <- MATINI[which(MATINI$ITK_name == 'USM_DEFAULT'), ]
  MATINIout <-
    VALINI
  for (i in 1:(length(TREATTEC) - 1)) {
    MATINIout <- rbind(MATINIout, VALINI)
  }
  
  MATINIout$ITK_name <- TREATTEC
  MATINIout$iplt0 <- MATUSMS$datedebut
  MATINIout$profsem <- rep(10, LEN)
  MATINIout$densitesem <- rep(8, LEN)
  MATINIout$variete <- rep(3, LEN)
  #MATINIout$codetradtec <- rep(2,LEN)
  MATINIout$codestade <- rep(2, LEN)
  MATINIout$codetaille <- rep(2, LEN)
  MATINIout$jultaille <- MATUSMS$datefin
  MATINIout$irec <- rep(999, LEN)
  MATINIout$engrais <- rep(3, LEN)
  MATINIout$julapN_or_sum_upvt_1 <- MATUSMS$datedebut
  MATINIout$Amount_N_Ferti_1 <- rep(220, LEN)
  MATINIout$codrecolte <- rep(1, LEN)
  
  
  MATPC <-
    read.csv(file ='',    #' ECOFI_DATAVERSE/ecofi_plotcycle.csv', 
              header = T, 
             #sep =";"
             )
  MATVC <-
    read.csv(file ='',   #' ECOFI_DATAVERSE/ecofi_varcycle.csv', 
              header = T, 
             #sep =";"
             )
  MATMERGE <-
    merge(MATPC, MATVC, all.y = T)
  MATMERGE <- MATMERGE[which(MATMERGE$varcyclecode %in% TREATCODE), ]
  IRRCODE <-
    as.vector(sapply(1:length(TREATCODE), function(i)
      MATMERGE$irricode[which(MATMERGE$varcyclecode == TREATCODE[i])]))
  
  MATIRRDATA <-
    read.csv(
      file = '',  #' ECOFI_DATAVERSE/ecofi_irridata.csv',
      header = T,
      #sep = ";",
      stringsAsFactors = F
    )
  head(MATIRRDATA)
  
  MATINIout[, 'codecalirrig'] <-
    replace(MATINIout[, 'codecalirrig'], IRRCODE == 'NC', 1)
  MATINIout[, 'ratiol'] <-
    replace(MATINIout[, 'ratiol'], IRRCODE == 'NC', 0.9)
  MATINIout[, 'dosimx'] <-
    replace(MATINIout[, 'dosimx'], IRRCODE == 'NC', 25)
  
  #  MATINIout[,'codecalirrig'] <- replace(MATINIout[,'codecalirrig'],IRRCODE !='RAINFED',1)
  #  MATINIout[,'ratiol'] <- replace(MATINIout[,'ratiol'],IRRCODE !='RAINFED',0.9)
  #  MATINIout[,'dosimx'] <- replace(MATINIout[,'dosimx'],IRRCODE !='RAINFED',5)
  MATOUT <- MATINIout
  DIMIRR <- c()
  for (i in 1:length(IRRCODE)) {
    if ((IRRCODE[i] != 'RAINFED') & (IRRCODE[i] != 'NC')) {
      MATTEMP <- MATIRRDATA[which(MATIRRDATA$irricode == IRRCODE[i]), ]
      DIMIRR <- c(DIMIRR, dim(MATTEMP)[1])
    }
  }
  MAXIRR <- max(DIMIRR)
  MATOUT <- MATINIout
  for (j in 21:(MAXIRR + 10)) {
    NAME1 <- paste('julapI_or_sum_upvt_', j, sep = "")
    NAME2 <- paste('Amount_Irrig_', j, sep = "")
    MATOUT <- cbind(MATOUT, rep(999, length(TREATTEC)))
    
    names(MATOUT)[294 + (j - 21) + (j - 20)] <- NAME1
    MATOUT <- cbind(MATOUT, rep(0, length(TREATTEC)))
    
    names(MATOUT)[294 + (j - 19) + (j - 21)] <- NAME2
  }
  
  for (i in 1:length(TREATTEC)) {
    if ((IRRCODE[i] != 'RAINFED') & (IRRCODE[i] != 'NC')) {
      DATINI <-
        MATMERGE$cyclestartingdate[MATMERGE$plotcyclecode == TREATCODE[i]]
      MATTEMP <-
        MATIRRDATA[which(MATIRRDATA$irricode == IRRCODE[i]), ]
      if (dim(MATTEMP)[1] > 0) {
        INIYEAR <- substr(DATINI, 7, 10)
        
        INIDAT <-
          as.Date(paste(INIYEAR, '-01-01', sep = ""), format = "%Y-%m-%d")
        IRRDAT <- MATTEMP$irridate
        
        IRRDAT <- as.Date(IRRDAT, format = "%d/%m/%Y")
        for (j in 1:dim(MATTEMP)[1]) {
          NAME1 <- paste('julapI_or_sum_upvt_', j, sep = "")
          NAME2 <- paste('Amount_Irrig_', j, sep = "")
          MATOUT[, NAME1][i] <- as.numeric(IRRDAT[j] - INIDAT) + 1
          MATOUT[, NAME2][i] <- MATTEMP$irridose[j]
        }
      }
      if (dim(MATTEMP)[1] == 0) {
        MATOUT[i, 'codecalirrig'] <- 1
        MATOUT[i, 'ratiol'] <- 0.9
        MATOUT[i, 'dosimx'] <- 25
      }
    }
    
  }
  
  
  write.csv2(
    MATOUT,
    file = paste(' STICS_MAILING_INPUT/TEC_', IDCODE, '.csv', sep = ''),
    quote = F,
    row.names = F,
    col.names = T,
    dec = '.',
    sep = ';'
  )
  print(paste('Tec created'))
  
  
}








######## Fait
INPUTSTA <- function(IDCODE){ #
  
  # MATUSMS <-
  #   read.csv2(
  #     paste(' STICS_MAILING_INPUT/USMs_', IDCODE, '.csv', sep = ''),
  #     header = T,
  #     sep = ';'
  #   )
  
  MATWS <-
    read.csv(   #read.csv2 ?
      file = 'Data_AEGIS/meteo.csv',    #' ECOFI_DATAVERSE/ecofi_ws.csv',
      header = T,
      sep = ",",
      stringsAsFactors = F
    )
  head(MATWS)
  
  
  
  require(stringr)
  WSTACODE <- unique(MATWS$wscode)
  LEN <- length(WSTACODE)
  #WSCODE <- str_sub(WSTACODE, end = -9)
  
  Sta_name <- paste(WSTACODE,'sta.xml',sep='_')
  zr <- rep(3.5, LEN)
  NH3ref <- rep(0, LEN)
  latitude <-
    sapply(1:LEN, function(i)
      MATWS$wslat[MATWS$wscode == WSTACODE][i])
  patm <- rep(1000, LEN)
  aclim <- rep(20, LEN)
  codeetp <- rep(1, LEN)
  alphapt <- rep(1.26, LEN)
  codeclichange <- rep(1, LEN)
  codaltitude <- rep(1, LEN)
  altistation <-
    sapply(1:LEN, function(i)
      MATWS$wsalt[MATWS$wscode == WSTACODE][i])
  altisimul <-
    sapply(1:LEN, function(i)
      MATWS$wsalt[MATWS$wscode == WSTACODE][i])
  gradtn <- rep(-0.5, LEN)
  gradtx <- rep(-0.55, LEN)
  altinversion <- rep(500, LEN)
  gradtninv <- rep(1.3, LEN)
  cielclair <- rep(0.8, LEN)
  codadret <- rep(1, LEN)
  ombragetx <- rep(-1.4, LEN)
  ra <- rep(50, LEN)
  albveg <- rep(0.23, LEN)
  aangst <- rep(0.18, LEN)
  bangst <- rep(0.62, LEN)
  corecTrosee <- rep(1, LEN)
  codecaltemp <- rep(2, LEN)
  codernet <- rep(2, LEN)
  coefdevil <- rep(0.7, LEN)
  aks <- rep(6, LEN)
  bks <- rep(0.5, LEN)
  cvent <- rep(0.16, LEN)
  phiv0 <- rep(0.004, LEN)
  coefrnet <- rep(0.59, LEN)
  
  MATOUT <-
    data.frame(
      Sta_name,
      zr,
      NH3ref,
      latitude,
      patm,
      aclim,
      codeetp,
      alphapt,
      codeclichange,
      codaltitude,
      altistation,
      altisimul,
      gradtn,
      gradtx,
      altinversion,
      gradtninv,
      cielclair,
      codadret,
      ombragetx,
      ra,
      albveg,
      aangst,
      bangst,
      corecTrosee,
      codecaltemp,
      codernet,
      coefdevil,
      aks,
      bks,
      cvent,
      phiv0,
      coefrnet
    )
  
  write.csv2(
    MATOUT,
    file = paste('STICS_MAILING_INPUT/STATION_', IDCODE, '.csv', sep = ''),
    quote = F,
    row.names = F,
    col.names = T,
    dec = '.',
    sep = ';'
  )
  
  print(paste('Station created'))
  
}







######### Fait

INPUTWEATHER <- function(IDCODE){
  
  MATSTA <-
    read.csv2(
      paste('STICS_MAILING_INPUT/STATION_', IDCODE, '.csv', sep = ''),
      header = T,
      sep = ';'
    )
  
  TREATSTA <- unique(MATSTA$Sta_name)
  require(stringr)
  TREATSTA <- str_sub(TREATSTA, end = -9) # -11 ????????
  
  MATWDATA <-
    read.csv(
      file ='Data_AEGIS/meteo.csv',    #' ECOFI_DATAVERSE/ecofi_wdataday.csv',
      header = T,
      #sep = ";",
      stringsAsFactors = F
    )
  head(MATWDATA)
  
  ######
  MATTEMP <- MATWDATA[which(MATWDATA$wscode %in% TREATSTA), ] ## ??????????
  head(MATTEMP)
  ######
  
  #tmean <- MATTEMP$weather_value[which(MATTEMP$weather_variable == "tmean")]
  tmin <-
    MATTEMP$weather_value[which(MATTEMP$weather_variable == "tmin")] #as.numeric(MATTEMP$tmin)

  dates_tmin <- MATTEMP$weatherdate[which(MATTEMP$weather_variable == "tmin")]
  
  tmax <-
    MATTEMP$weather_value[which(MATTEMP$weather_variable == "tmax")] #as.numeric(MATTEMP$tmax)

  dates_tmax <- MATTEMP$weatherdate[which(MATTEMP$weather_variable == "tmax")]
  
  srad <-
    MATTEMP$weather_value[which(MATTEMP$weather_variable == "grad")] #as.numeric(MATTEMP$grad)

  dates_srad <- MATTEMP$weatherdate[which(MATTEMP$weather_variable == "grad")]
  
  ETP <-
    MATTEMP$weather_value[which(MATTEMP$weather_variable == "eto")] #as.numeric(MATTEMP$eto)

  dates_ETP <- MATTEMP$weatherdate[which(MATTEMP$weather_variable == "eto")]
  
  rain <-
    MATTEMP$weather_value[which(MATTEMP$weather_variable == "rainfall")] #as.numeric(MATTEMP$rainfall)

  dates_rain <- MATTEMP$weatherdate[which(MATTEMP$weather_variable == "rainfall")]
  
  
  wind <- MATTEMP$weather_value[which(MATTEMP$weather_variable == "windtot")] #MATTEMP$windtot

  dates_wind <- MATTEMP$weatherdate[which(MATTEMP$weather_variable == "windtot")]
  
  ########
  
  Tab_tmin = data.frame(tmin,date = dates_tmin)
  Tab_tmax = data.frame(tmax,date = dates_tmax)
  Tab_MATOUT = merge(Tab_tmin,Tab_tmax)
  Tab_srad = data.frame(srad,date = dates_srad)
  Tab_MATOUT = merge(Tab_MATOUT,Tab_srad)
  Tab_ETP = data.frame(ETP,date = dates_ETP)
  Tab_MATOUT = merge(Tab_MATOUT,Tab_ETP)
  Tab_rain = data.frame(rain,date = dates_rain)
  Tab_MATOUT = merge(Tab_MATOUT,Tab_rain)
  Tab_wind = data.frame(wind,date = dates_wind)
  Tab_MATOUT = merge(Tab_MATOUT,Tab_wind, all = TRUE) # full join
  
  len_MATOUT = dim(Tab_MATOUT)[1]
  #
  
  vapor_pressure <- rep(999,len_MATOUT ) # 
  CO2 <- rep(405, len_MATOUT) # 
  
  
  tmin = Tab_MATOUT$tmin
  tmin <- replace(tmin, is.na(tmin), mean(tmin, na.rm = T))
  tmax = Tab_MATOUT$tmax
  tmax <- replace(tmax, is.na(tmax), mean(tmax, na.rm = T))
  srad = Tab_MATOUT$srad
  srad <- replace(srad, is.na(srad), mean(srad, na.rm = T))
  ETP = Tab_MATOUT$ETP
  ETP <- replace(ETP, is.na(ETP), mean(ETP, na.rm = T))
  rain = Tab_MATOUT$rain
  rain <- replace(rain, is.na(rain), mean(rain, na.rm = T))
  wind = Tab_MATOUT$wind
  wind <- replace(wind, is.na(wind), 999)
  
  Weather_name <-MATTEMP$wscode[1:len_MATOUT]
  ian <- str_sub(Tab_MATOUT$date, start = 1,end=4) #MATTEMP$weatherdate
  
  mo <- as.numeric(str_sub(Tab_MATOUT$date, start = 6, end = 7)) #MATTEMP$weatherdate
  
  jo <- as.numeric(str_sub(Tab_MATOUT$date, start = 9, end = 10)) #MATTEMP$weatherdate
  
  YearIni <-
    ian
  DateIni <-
    sapply(1:length(ian), function(i)
      paste(YearIni[i], '-01-01', sep = ''))
  DateIni <- as.Date(DateIni, format = '%Y-%m-%d')
  DATE <- as.Date(str_sub(Tab_MATOUT$date,start=1,end=10), format = '%Y-%m-%d')
  jul <-
    sapply(1:length(ian), function(i)
      as.numeric(DATE[i] - DateIni[i]) + 1)
  
  MATOUT <-
    data.frame(Weather_name,
               ian,
               mo,
               jo,
               jul,
               tmin,
               tmax,
               srad,
               ETP,
               rain,
               wind,
               vapor_pressure,
               CO2)
  
  write.csv2(
    MATOUT,
    file = paste('STICS_MAILING_INPUT/WEATHER_', IDCODE, '.csv', sep = ''),
    quote = F,
    row.names = F,
    col.names = T,
    dec = '.',
    sep = ';'
  )
  print(paste('Weather created'))
}


############################################




######## Fait


INPUTOBS <- function(IDCODE){
  
  MATUSMS <-
    read.csv2(
      paste('STICS_MAILING_INPUT/USMs_', IDCODE, '.csv', sep = ''),
      header = T,
      sep = ';'
    )
  USMS <- (MATUSMS$usm_nom)
  require(stringr)
  TREATCODE <-
    sapply(1:dim(MATUSMS)[1], function(i)
      str_sub(MATUSMS$usm_nom[i], start = 5))
  
  MATOBSP <-
    read.csv(
      file = 'Data_AEGIS/observation.csv',  #' ECOFI_DATAVERSE/ecofi_obsplant.csv',
      header = T,
      #sep = ";",
      stringsAsFactors = F
    )
  head(MATOBSP)
  
  #VARCYCLE <- str_sub(USMS, start = 5)
  #MATOBSP <- MATOBSP[which(paste(unique(MATTOT$trial_code),unique(MATTOT$factor_level),sep = '_') %in% VARCYCLE), ] # MATOBSP$varcyclecode
  
  VAROBS <- levels(as.factor(MATOBSP$obs_variable))
  VAROBSINT <- c('ei',
                 'LAI',
                 'mfa',
                 'mflv',
                 'mftu',
                 'msa',
                 'msfm',
                 'mslv',
                 'mstu',
                 'mssucrtu',
                 'sucrt')
  STICSVAR <- c(
    'fapar',
    'lai(n)',
    'mafrais',
    'mafeuil',
    'STALKFRESH',
    'masec(n)',
    'mafeuiljaune',
    'mafeuilverte',
    'STALKDRY',
    'sucre',
    'sucre_percent'
  )
  
  MATOBSTICS <-
    data.frame(matrix(rep(NA, 6 + length(STICSVAR)), nrow = 1))
  names(MATOBSTICS) <-
    c('usm_name', 'ian', 'mo', 'jo', 'jul', 'date', STICSVAR)
  
#  for (i in 1:length(TREATCODE)) {
    MATTEMP <- MATOBSP#[which(MATOBS$varcyclecode == VARCYCLE[i]), ]
    if (dim(MATTEMP)[1] > 0) {
      DATE <-
        MATTEMP$obs_date
      DATE <- as.Date(DATE)#, format = '%d/%m/%Y')
      OBSDATE <- sort(unique(DATE))
      usm_name <- rep(TREATCODE, length(OBSDATE))
      ian <- str_sub(OBSDATE, end = 4)
      mo <- as.numeric(str_sub(OBSDATE, start = 6, end = 7))
      jo <- as.numeric(str_sub(OBSDATE, start = 9))
      INIDATE <-
        sapply(1:length(OBSDATE), function(i)
          paste(ian[i], '-01-01', sep = ''))
      jul <-
        as.numeric(as.Date(OBSDATE, format = '%Y-%m-%d') - as.Date(INIDATE, format =
                                                                     '%Y-%m-%d')) + 1
      date <-
        format(as.Date(OBSDATE, format = '%Y-%m-%d'), format = '%d/%m/%Y')
      
      MATOBSTEMP <-
        matrix(
          c(usm_name, ian, mo, jo, jul, date, rep(rep(
#          c(rep(rep(
            NA, length(OBSDATE)
          ), length(STICSVAR))),
          nrow = length(OBSDATE),
          byrow = F
        )
      
      for (j in 1:length(OBSDATE)) {
        VAL <- c()
        for (k in 1:length(VAROBSINT)) {
          val <-
            as.numeric(MATTEMP$obs_value[(MATTEMP$obs_date == OBSDATE[j])&
                                           (MATTEMP$obs_variable== VAROBSINT[k])])
  
        if (length(val) == 0) {
            VAL <- c(VAL, NA)
          }
          if (length(val) > 0) {
            VAL <- c(VAL, mean(val))
          }
        }
        MATOBSTEMP[j, 7:(6 + length(STICSVAR))] <- VAL
      }
      
      MATOBSTEMP <- data.frame(MATOBSTEMP)
      names(MATOBSTEMP) <- names(MATOBSTICS)
      MATOBSTICS <- rbind(MATOBSTICS, MATOBSTEMP)
      
    }
    
#  }
  
  MATOBSTICS <- MATOBSTICS[2:dim(MATOBSTICS)[1], ]
  
  write.csv2(
    MATOBSTICS,
    file = paste('STICS_MAILING_INPUT/OBS_', IDCODE, '.csv', sep = ''),
    quote = F,
    row.names = F,
    col.names = T,
    dec = '.',
    sep = ';'
  )
  print(paste('Obs created'))
}
    


