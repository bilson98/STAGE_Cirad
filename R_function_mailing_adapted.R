library(xfun)

STICS_mailing_Ini2 <- function (stwF, Excel_file, sheetIndex, stwR) {
  isunix <- is_unix()
#  Ini_data <- as.data.frame(read_excel(file = file.path(stwF, Excel_file), sheet = sheetIndex, col_types = "text"))
  Ini_data <- as.data.frame(read.csv2(file = file.path(stwF, Excel_file), header = T,sep =";"))
  Ini_data_default <- as.data.frame(read_excel(path = file.path(system.file("extdata/data", 
                                                                            package = "STICSmailing"), "Inputs_STICS_mailing.xlsx"), 
                                               sheet = "Ini"))
  if (is_missing_df(Ini_data, Ini_data_default)) {
    return()
  }
  if (dir.exists(path.expand(stwR)) == FALSE) {
    dir.create(stwR)
  }
  if (!isunix) {
    Progress_bar_n = 0
    Progress_bar_max = length(Ini_data$Ini_name)
    Progress_bar = winProgressBar(title = "Progress bar", 
                                  min = 0, max = Progress_bar_max, width = 500)
  }
  else {
    print("Creating initialization files...")
  }
  nb_ini = dim(Ini_data)[1]
  if (length(unique(Ini_data$Ini_name)) != nb_ini) {
    warning("Duplicated file names in Ini_name, aborting !")
    return((invisible()))
  }
  for (ki in 1:nb_ini) {
    if (!isunix) {
      Progress_bar_n = Progress_bar_n + 1
      setWinProgressBar(Progress_bar, value = Progress_bar_n, 
                        title = paste(round(Progress_bar_n/Progress_bar_max * 
                                              100, 0), "% done - Creating initialization files", 
                                      sep = ""))
    }
    ini_name <- Ini_data$Ini_name[ki]
    file_path <- file.path(stwR, ini_name)
    if (file.exists(file_path)) {
      file.remove(file_path)
    }
    sink(file_path)
    cat(noquote(paste("<?xml version=\"1.0\" encoding=\"UTF-8\" ?>", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("<initialisations>", sep = "")))
    cat("\n")
    cat(noquote(paste("<nbplantes>", as.numeric(as.character(Ini_data$nbplantes[ki])), 
                      "</nbplantes>", sep = "")))
    cat("\n")
    cat(noquote(paste("<plante dominance=\"1\">", sep = "")))
    cat("\n")
    cat(noquote(paste("<stade0>", as.character(Ini_data$stade0_Crop1[ki]), 
                      "</stade0>", sep = "")))
    cat("\n")
    cat(noquote(paste("<lai0>", as.numeric(as.character(Ini_data$lai0_Crop1[ki])), 
                      "</lai0>", sep = "")))
    cat("\n")
    cat(noquote(paste("<masec0>", as.numeric(as.character(Ini_data$masec0_Crop1[ki])), 
                      "</masec0>", sep = "")))
    cat("\n")
    cat(noquote(paste("<QNplante0>", as.numeric(as.character(Ini_data$QNplante0_Crop1[ki])), 
                      "</QNplante0>", sep = "")))
    cat("\n")
    cat(noquote(paste("<magrain0>", as.numeric(as.character(Ini_data$magrain0_Crop1[ki])), 
                      "</magrain0>", sep = "")))
    cat("\n")
    cat(noquote(paste("<zrac0>", as.numeric(as.character(Ini_data$zrac0_Crop1[ki])), 
                      "</zrac0>", sep = "")))
    cat("\n")
    cat(noquote(paste("<resperenne0>", as.numeric(as.character(Ini_data$resperenne0_Crop1[ki])), 
                      "</resperenne0>", sep = "")))
    cat("\n")
    cat(noquote(paste("<densinitial>", sep = "")))
    cat("\n")
    cat(noquote(paste("<horizon nh=\"1\">", as.numeric(as.character(Ini_data$densinitial_1_Crop1[ki])), 
                      "</horizon>", sep = "")))
    cat("\n")
    cat(noquote(paste("<horizon nh=\"2\">", as.numeric(as.character(Ini_data$densinitial_2_Crop1[ki])), 
                      "</horizon>", sep = "")))
    cat("\n")
    cat(noquote(paste("<horizon nh=\"3\">", as.numeric(as.character(Ini_data$densinitial_3_Crop1[ki])), 
                      "</horizon>", sep = "")))
    cat("\n")
    cat(noquote(paste("<horizon nh=\"4\">", as.numeric(as.character(Ini_data$densinitial_4_Crop1[ki])), 
                      "</horizon>", sep = "")))
    cat("\n")
    cat(noquote(paste("<horizon nh=\"5\">", as.numeric(as.character(Ini_data$densinitial_5_Crop1[ki])), 
                      "</horizon>", sep = "")))
    cat("\n")
    cat(noquote(paste("</densinitial>", sep = "")))
    cat("\n")
    cat(noquote(paste("</plante>", sep = "")))
    cat("\n")
    cat(noquote(paste("<plante dominance=\"2\">", sep = "")))
    cat("\n")
    cat(noquote(paste("<stade0>", as.character(Ini_data$stade0_Crop2[ki]), 
                      "</stade0>", sep = "")))
    cat("\n")
    cat(noquote(paste("<lai0>", as.numeric(as.character(Ini_data$lai0_Crop2[ki])), 
                      "</lai0>", sep = "")))
    cat("\n")
    cat(noquote(paste("<masec0>", as.numeric(as.character(Ini_data$masec0_Crop2[ki])), 
                      "</masec0>", sep = "")))
    cat("\n")
    cat(noquote(paste("<QNplante0>", as.numeric(as.character(Ini_data$QNplante0_Crop2[ki])), 
                      "</QNplante0>", sep = "")))
    cat("\n")
    cat(noquote(paste("<magrain0>", as.numeric(as.character(Ini_data$magrain0_Crop2[ki])), 
                      "</magrain0>", sep = "")))
    cat("\n")
    cat(noquote(paste("<zrac0>", as.numeric(as.character(Ini_data$zrac0_Crop2[ki])), 
                      "</zrac0>", sep = "")))
    cat("\n")
    cat(noquote(paste("<resperenne0>", as.numeric(as.character(Ini_data$resperenne0_Crop2[ki])), 
                      "</resperenne0>", sep = "")))
    cat("\n")
    cat(noquote(paste("<densinitial>", sep = "")))
    cat("\n")
    cat(noquote(paste("<horizon nh=\"1\">", as.numeric(as.character(Ini_data$densinitial_1_Crop2[ki])), 
                      "</horizon>", sep = "")))
    cat("\n")
    cat(noquote(paste("<horizon nh=\"2\">", as.numeric(as.character(Ini_data$densinitial_2_Crop2[ki])), 
                      "</horizon>", sep = "")))
    cat("\n")
    cat(noquote(paste("<horizon nh=\"3\">", as.numeric(as.character(Ini_data$densinitial_3_Crop2[ki])), 
                      "</horizon>", sep = "")))
    cat("\n")
    cat(noquote(paste("<horizon nh=\"4\">", as.numeric(as.character(Ini_data$densinitial_4_Crop2[ki])), 
                      "</horizon>", sep = "")))
    cat("\n")
    cat(noquote(paste("<horizon nh=\"5\">", as.numeric(as.character(Ini_data$densinitial_5_Crop2[ki])), 
                      "</horizon>", sep = "")))
    cat("\n")
    cat(noquote(paste("</densinitial>", sep = "")))
    cat("\n")
    cat(noquote(paste("</plante>", sep = "")))
    cat("\n")
    cat(noquote(paste("<sol>", sep = "")))
    cat("\n")
    cat(noquote(paste("<hinit>", sep = "")))
    cat("\n")
    cat(noquote(paste("<horizon nh=\"1\">", Ini_data$hinit_1[ki], 
                      "</horizon>", sep = "")))
    cat("\n")
    cat(noquote(paste("<horizon nh=\"2\">", Ini_data$hinit_2[ki], 
                      "</horizon>", sep = "")))
    cat("\n")
    cat(noquote(paste("<horizon nh=\"3\">", Ini_data$hinit_3[ki], 
                      "</horizon>", sep = "")))
    cat("\n")
    cat(noquote(paste("<horizon nh=\"4\">", Ini_data$hinit_4[ki], 
                      "</horizon>", sep = "")))
    cat("\n")
    cat(noquote(paste("<horizon nh=\"5\">", Ini_data$hinit_5[ki], 
                      "</horizon>", sep = "")))
    cat("\n")
    cat(noquote(paste("</hinit>", sep = "")))
    cat("\n")
    cat(noquote(paste("<NO3init>", sep = "")))
    cat("\n")
    cat(noquote(paste("<horizon nh=\"1\">", Ini_data$NO3init_1[ki], 
                      "</horizon>", sep = "")))
    cat("\n")
    cat(noquote(paste("<horizon nh=\"2\">", Ini_data$NO3init_2[ki], 
                      "</horizon>", sep = "")))
    cat("\n")
    cat(noquote(paste("<horizon nh=\"3\">", Ini_data$NO3init_3[ki], 
                      "</horizon>", sep = "")))
    cat("\n")
    cat(noquote(paste("<horizon nh=\"4\">", Ini_data$NO3init_4[ki], 
                      "</horizon>", sep = "")))
    cat("\n")
    cat(noquote(paste("<horizon nh=\"5\">", Ini_data$NO3init_5[ki], 
                      "</horizon>", sep = "")))
    cat("\n")
    cat(noquote(paste("</NO3init>", sep = "")))
    cat("\n")
    cat(noquote(paste("<NH4init>", sep = "")))
    cat("\n")
    cat(noquote(paste("<horizon nh=\"1\">", Ini_data$NH4init_1[ki], 
                      "</horizon>", sep = "")))
    cat("\n")
    cat(noquote(paste("<horizon nh=\"2\">", Ini_data$NH4init_2[ki], 
                      "</horizon>", sep = "")))
    cat("\n")
    cat(noquote(paste("<horizon nh=\"3\">", Ini_data$NH4init_3[ki], 
                      "</horizon>", sep = "")))
    cat("\n")
    cat(noquote(paste("<horizon nh=\"4\">", Ini_data$NH4init_4[ki], 
                      "</horizon>", sep = "")))
    cat("\n")
    cat(noquote(paste("<horizon nh=\"5\">", Ini_data$NH4init_5[ki], 
                      "</horizon>", sep = "")))
    cat("\n")
    cat(noquote(paste("</NH4init>", sep = "")))
    cat("\n")
    cat(noquote(paste("</sol>", sep = "")))
    cat("\n")
    cat(noquote(paste("</initialisations>", sep = "")))
    sink()
  }
  if (!isunix) {
    close(Progress_bar)
  }
  return("Tout s'est bien deroule !!! Super :-)")
}

STICS_mailing_Obs2 <- function (stwF, Excel_file, sheetIndex, stwR) {
  isunix <- is_unix()
  #Obs_data <- as.data.frame(read_excel(path = file.path(stwF, 
  #                                                      Excel_file), sheet = sheetIndex, col_types = "text"))
  Obs_data <- as.data.frame(read.csv2(file = file.path(stwF, Excel_file), header = T,sep =";"))
  id_cols <- c("usm_name", "ian", "mo", "jo", "jul")
  if (!all(colnames(Obs_data)[1:5] == id_cols)) {
    return("Les 5 premieres colonnes doivent etre dans l'ordre suivant : `usm_name`, `ian`, `mo`, `jo`, `jul`")
  }
  if (dim(Obs_data)[2] < 6) {
    return("Pas de colonnes pour les variables !")
  }
  Obs_data[is.na(Obs_data)] <- -999.99
  if (dir.exists(path.expand(stwR)) == FALSE) {
    dir.create(stwR)
  }
  if (!isunix) {
    Progress_bar_n = 0
    Progress_bar_max = length(unique(Obs_data$usm_name))
    Progress_bar = winProgressBar(title = "Progress bar", 
                                  min = 0, max = Progress_bar_max, width = 500)
  }
  else {
    print("Creating .obs files...")
  }
  usm_names <- unique(Obs_data$usm_name)
  for (usm in usm_names) {
    if (!isunix) {
      Progress_bar_n = Progress_bar_n + 1
      setWinProgressBar(Progress_bar, value = Progress_bar_n, 
                        title = paste(round(Progress_bar_n/Progress_bar_max * 
                                              100, 0), "% done - Creating .obs files", sep = ""))
    }
    Fich_obs <- Obs_data[Obs_data$usm_name == usm, !colnames(Obs_data) %in% 
                           "usm_name"]
    file_path <- file.path(stwR, paste0(usm, ".obs"))
    if (file.exists(file_path)) {
      file.remove(file_path)
    }
    write.table(x = Fich_obs, file = file_path, sep = ";", 
                row.names = FALSE, col.names = TRUE)
  }
  if (!isunix) {
    close(Progress_bar)
  }
  return("Tout s'est bien deroule !!! Super :-)")
}

STICS_mailing_Sols2 <- function (stwF, Excel_file, sheetIndex, stwR) {
  isunix <- is_unix()
#  Sol_data <- as.data.frame(read_excel(path = file.path(stwF, 
#                                                        Excel_file), sheet = sheetIndex, col_types = "text"))
  Sol_data <- as.data.frame(read.csv2(file = file.path(stwF, Excel_file), header = T,sep =";"))
  
  Sol_data_default <- as.data.frame(read_excel(path = file.path(system.file("extdata/data", 
                                                                            package = "STICSmailing"), "Inputs_STICS_mailing.xlsx"), 
                                               sheet = "Soils"))
  if (is_missing_df(Sol_data, Sol_data_default)) {
    return()
  }
  if (dir.exists(path.expand(stwR)) == FALSE) {
    dir.create(stwR)
  }
  if (!isunix) {
    Progress_bar_n = 0
    Progress_bar_max = length(Sol_data$Soil_name)
    Progress_bar = winProgressBar(title = "Progress bar", 
                                  min = 0, max = Progress_bar_max, width = 500)
  }
  else {
    print("Creating sols.xml file...")
  }
  file_path <- file.path(stwR, "sols.xml")
  if (file.exists(file_path)) {
    file.remove(file_path)
  }
  sink(file_path)
  cat(noquote(paste("<?xml version=", "1.0", " encoding=", 
                    "UTF-8", "?>", sep = "\"")))
  cat("\n")
  cat(noquote("<sols>"))
  nb_sols <- dim(Sol_data)[1]
  if (length(unique(Sol_data$Soil_name)) != nb_sols) {
    warning("Duplicated file names in Soil_name, aborting !")
    return((invisible()))
  }
  for (ks in 1:nb_sols) {
    if (!isunix) {
      Progress_bar_n = Progress_bar_n + 1
      setWinProgressBar(Progress_bar, value = Progress_bar_n, 
                        title = paste(round(Progress_bar_n/Progress_bar_max * 
                                              100, 0), "% done - Creating sols.xml file", 
                                      sep = ""))
    }
    cat("\n")
    cat(noquote(paste("<sol nom=", as.character(Sol_data$Soil_name[ks]), 
                      ">", sep = "\"")))
    cat("\n")
    cat(noquote(paste("<param format=\"real\" max=\"60.0\" min=\"0.0\" nom=\"argi\">", 
                      as.numeric(as.character(Sol_data$argi[ks])), "</param>", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("<param format=\"real\" max=\"0.5\" min=\"0.05\" nom=\"norg\">", 
                      as.numeric(as.character(Sol_data$norg[ks])), "</param>", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("<param format=\"real\" max=\"60.0\" min=\"10.0\" nom=\"profhum\">", 
                      as.numeric(as.character(Sol_data$profhum[ks])), "</param>", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("<param format=\"real\" max=\"100.0\" min=\"0.0\" nom=\"calc\">", 
                      as.numeric(as.character(Sol_data$calc[ks])), "</param>", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("<param format=\"real\" max=\"9.0\" min=\"4.0\" nom=\"pH\">", 
                      as.numeric(as.character(Sol_data$pH[ks])), "</param>", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("<param format=\"real\" max=\"0.5\" min=\"0.0\" nom=\"concseuil\">", 
                      as.numeric(as.character(Sol_data$concseuil[ks])), 
                      "</param>", sep = "")))
    cat("\n")
    cat(noquote(paste("<param format=\"real\" max=\"0.6\" min=\"0.05\" nom=\"albedo\">", 
                      as.numeric(as.character(Sol_data$albedo[ks])), "</param>", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("<param format=\"real\" max=\"50.0\" min=\"0.0\" nom=\"q0\">", 
                      as.numeric(as.character(Sol_data$q0[ks])), "</param>", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("<param format=\"real\" max=\"1.0\" min=\"0.0\" nom=\"ruisolnu\">", 
                      as.numeric(as.character(Sol_data$ruisolnu[ks])), 
                      "</param>", sep = "")))
    cat("\n")
    cat(noquote(paste("<param format=\"real\" max=\"1000.0\" min=\"10.0\" nom=\"obstarac\">", 
                      as.numeric(as.character(Sol_data$obstarac[ks])), 
                      "</param>", sep = "")))
    cat("\n")
    cat(noquote(paste("<param format=\"real\" max=\"100.0\" min=\"5.0\" nom=\"pluiebat\">", 
                      as.numeric(as.character(Sol_data$pluiebat[ks])), 
                      "</param>", sep = "")))
    cat("\n")
    cat(noquote(paste("<param format=\"real\" max=\"2.0\" min=\"0.0\" nom=\"mulchbat\">", 
                      as.numeric(as.character(Sol_data$mulchbat[ks])), 
                      "</param>", sep = "")))
    cat("\n")
    cat(noquote(paste("<param format=\"real\" max=\"150.0\" min=\"10.0\" nom=\"zesx\">", 
                      as.numeric(as.character(Sol_data$zesx[ks])), "</param>", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("<param format=\"real\" max=\"10.0\" min=\"-10.0\" nom=\"cfes\">", 
                      as.numeric(as.character(Sol_data$cfes[ks])), "</param>", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("<param format=\"real\" max=\"0.2\" min=\"0.01\" nom=\"z0solnu\">", 
                      as.numeric(as.character(Sol_data$z0solnu[ks])), "</param>", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("<param format=\"real\" max=\"20.0\" min=\"8.0\" nom=\"CsurNsol\">", 
                      as.numeric(as.character(Sol_data$CsurNsol[ks])), 
                      "</param>", sep = "")))
    cat("\n")
    cat(noquote(paste("<param format=\"real\" max=\"5.0\" min=\"0.0\" nom=\"penterui\">", 
                      as.numeric(as.character(Sol_data$penterui[ks])), 
                      "</param>", sep = "")))
    cat("\n")
    cat(noquote(paste("<option choix=\"", as.numeric(as.character(Sol_data$codecailloux[ks])), 
                      "\" nom=\"pebbles\" nomParam=\"codecailloux\">", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("<choix code=\"1\" nom=\"yes\" />", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("<choix code=\"2\" nom=\"no\" />", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("</option>", sep = "")))
    cat("\n")
    cat(noquote(paste("<option choix=\"", as.numeric(as.character(Sol_data$codemacropor[ks])), 
                      "\" nom=\"macroporosity\" nomParam=\"codemacropor\">", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("<choix code=\"1\" nom=\"yes\" />", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("<choix code=\"2\" nom=\"no\" />", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("</option>", sep = "")))
    cat("\n")
    cat(noquote(paste("<option choix=\"", as.numeric(as.character(Sol_data$codefente[ks])), 
                      "\" nom=\"cracks (case of swelling clay soils)\" nomParam=\"codefente\">", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("<choix code=\"1\" nom=\"yes\" />", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("<choix code=\"2\" nom=\"no\" />", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("</option>", sep = "")))
    cat("\n")
    cat(noquote(paste("<option choix=\"", as.numeric(as.character(Sol_data$codrainage[ks])), 
                      "\" nom=\"artificial drainage\" nomParam=\"codrainage\">", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("<choix code=\"1\" nom=\"yes\">", sep = "")))
    cat("\n")
    cat(noquote(paste("<param nom=\"profimper\">", as.numeric(as.character(Sol_data$profimper[ks])), 
                      "</param>", sep = "")))
    cat("\n")
    cat(noquote(paste("<param nom=\"ecartdrain\">", as.numeric(as.character(Sol_data$ecartdrain[ks])), 
                      "</param>", sep = "")))
    cat("\n")
    cat(noquote(paste("<param nom=\"ksol\">", as.numeric(as.character(Sol_data$ksol[ks])), 
                      "</param>", sep = "")))
    cat("\n")
    cat(noquote(paste("<param nom=\"profdrain\">", as.numeric(as.character(Sol_data$profdrain[ks])), 
                      "</param>", sep = "")))
    cat("\n")
    cat(noquote(paste("</choix>", sep = "")))
    cat("\n")
    cat(noquote(paste("<choix code=\"2\" nom=\"no\" />", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("</option>", sep = "")))
    cat("\n")
    cat(noquote(paste("<option choix=\"", as.numeric(as.character(Sol_data$coderemontcap[ks])), 
                      "\" nom=\"capillary rise\" nomParam=\"coderemontcap\">", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("<choix code=\"1\" nom=\"yes\">", sep = "")))
    cat("\n")
    cat(noquote(paste("<param nom=\"capiljour\">", as.numeric(as.character(Sol_data$capiljour[ks])), 
                      "</param>", sep = "")))
    cat("\n")
    cat(noquote(paste("<param nom=\"humcapil\">", as.numeric(as.character(Sol_data$humcapil[ks])), 
                      "</param>", sep = "")))
    cat("\n")
    cat(noquote(paste("</choix>", sep = "")))
    cat("\n")
    cat(noquote(paste("<choix code=\"2\" nom=\"no\" />", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("</option>", sep = "")))
    cat("\n")
    cat(noquote(paste("<option choix=\"", as.numeric(as.character(Sol_data$codenitrif[ks])), 
                      "\" nom=\"nitrification\" nomParam=\"codenitrif\">", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("<choix code=\"1\" nom=\"yes\" />", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("<choix code=\"2\" nom=\"no\" />", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("</option>", sep = "")))
    cat("\n")
    cat(noquote(paste("<option choix=\"", as.numeric(as.character(Sol_data$codedenit[ks])), 
                      "\" nom=\"denitrification\" nomParam=\"codedenit\">", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("<choix code=\"1\" nom=\"yes\">", sep = "")))
    cat("\n")
    cat(noquote(paste("<param nom=\"profdenit\">", as.numeric(as.character(Sol_data$profdenit[ks])), 
                      "</param>", sep = "")))
    cat("\n")
    cat(noquote(paste("<param nom=\"vpotdenit\">", as.numeric(as.character(Sol_data$vpotdenit[ks])), 
                      "</param>", sep = "")))
    cat("\n")
    cat(noquote(paste("</choix>", sep = "")))
    cat("\n")
    cat(noquote(paste("<choix code=\"2\" nom=\"no\" />", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("</option>", sep = "")))
    cat("\n")
    cat(noquote(paste("<tableau_entete nb_colonnes=\"8\">", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("<colonne nom=\"epc\" />", sep = "")))
    cat("\n")
    cat(noquote(paste("<colonne nom=\"HCCF\" />", sep = "")))
    cat("\n")
    cat(noquote(paste("<colonne nom=\"HMINF\" />", sep = "")))
    cat("\n")
    cat(noquote(paste("<colonne nom=\"DAF\" />", sep = "")))
    cat("\n")
    cat(noquote(paste("<colonne nom=\"cailloux\" />", sep = "")))
    cat("\n")
    cat(noquote(paste("<colonne nom=\"typecailloux\" />", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("<colonne nom=\"infil\" />", sep = "")))
    cat("\n")
    cat(noquote(paste("<colonne nom=\"epd\" />", sep = "")))
    cat("\n")
    cat(noquote(paste("</tableau_entete>", sep = "")))
    cat("\n")
    cat(noquote(paste("<tableau nb_colonnes=\"8\" nom=\"layer 1\">", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("<colonne nom=\"epc\">", as.numeric(as.character(Sol_data$epc_1[ks])), 
                      "</colonne>", sep = "")))
    cat("\n")
    cat(noquote(paste("<colonne nom=\"HCCF\">", as.numeric(as.character(Sol_data$HCCF_1[ks])), 
                      "</colonne>", sep = "")))
    cat("\n")
    cat(noquote(paste("<colonne nom=\"HMINF\">", as.numeric(as.character(Sol_data$HMINF_1[ks])), 
                      "</colonne>", sep = "")))
    cat("\n")
    cat(noquote(paste("<colonne nom=\"DAF\">", as.numeric(as.character(Sol_data$DAF_1[ks])), 
                      "</colonne>", sep = "")))
    cat("\n")
    cat(noquote(paste("<colonne nom=\"cailloux\">", as.numeric(as.character(Sol_data$cailloux_1[ks])), 
                      "</colonne>", sep = "")))
    cat("\n")
    cat(noquote(paste("<colonne nom=\"typecailloux\">", as.numeric(as.character(Sol_data$typecailloux_1[ks])), 
                      "</colonne>", sep = "")))
    cat("\n")
    cat(noquote(paste("<colonne nom=\"infil\">", as.numeric(as.character(Sol_data$infil_1[ks])), 
                      "</colonne>", sep = "")))
    cat("\n")
    cat(noquote(paste("<colonne nom=\"epd\">", as.numeric(as.character(Sol_data$epd_1[ks])), 
                      "</colonne>", sep = "")))
    cat("\n")
    cat(noquote(paste("</tableau>", sep = "")))
    cat("\n")
    cat(noquote(paste("<tableau nb_colonnes=\"8\" nom=\"layer 2\">", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("<colonne nom=\"epc\">", as.numeric(as.character(Sol_data$epc_2[ks])), 
                      "</colonne>", sep = "")))
    cat("\n")
    cat(noquote(paste("<colonne nom=\"HCCF\">", as.numeric(as.character(Sol_data$HCCF_2[ks])), 
                      "</colonne>", sep = "")))
    cat("\n")
    cat(noquote(paste("<colonne nom=\"HMINF\">", as.numeric(as.character(Sol_data$HMINF_2[ks])), 
                      "</colonne>", sep = "")))
    cat("\n")
    cat(noquote(paste("<colonne nom=\"DAF\">", as.numeric(as.character(Sol_data$DAF_2[ks])), 
                      "</colonne>", sep = "")))
    cat("\n")
    cat(noquote(paste("<colonne nom=\"cailloux\">", as.numeric(as.character(Sol_data$cailloux_2[ks])), 
                      "</colonne>", sep = "")))
    cat("\n")
    cat(noquote(paste("<colonne nom=\"typecailloux\">", as.numeric(as.character(Sol_data$typecailloux_2[ks])), 
                      "</colonne>", sep = "")))
    cat("\n")
    cat(noquote(paste("<colonne nom=\"infil\">", as.numeric(as.character(Sol_data$infil_2[ks])), 
                      "</colonne>", sep = "")))
    cat("\n")
    cat(noquote(paste("<colonne nom=\"epd\">", as.numeric(as.character(Sol_data$epd_2[ks])), 
                      "</colonne>", sep = "")))
    cat("\n")
    cat(noquote(paste("</tableau>", sep = "")))
    cat("\n")
    cat(noquote(paste("<tableau nb_colonnes=\"8\" nom=\"layer 3\">", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("<colonne nom=\"epc\">", as.numeric(as.character(Sol_data$epc_3[ks])), 
                      "</colonne>", sep = "")))
    cat("\n")
    cat(noquote(paste("<colonne nom=\"HCCF\">", as.numeric(as.character(Sol_data$HCCF_3[ks])), 
                      "</colonne>", sep = "")))
    cat("\n")
    cat(noquote(paste("<colonne nom=\"HMINF\">", as.numeric(as.character(Sol_data$HMINF_3[ks])), 
                      "</colonne>", sep = "")))
    cat("\n")
    cat(noquote(paste("<colonne nom=\"DAF\">", as.numeric(as.character(Sol_data$DAF_3[ks])), 
                      "</colonne>", sep = "")))
    cat("\n")
    cat(noquote(paste("<colonne nom=\"cailloux\">", as.numeric(as.character(Sol_data$cailloux_3[ks])), 
                      "</colonne>", sep = "")))
    cat("\n")
    cat(noquote(paste("<colonne nom=\"typecailloux\">", as.numeric(as.character(Sol_data$typecailloux_3[ks])), 
                      "</colonne>", sep = "")))
    cat("\n")
    cat(noquote(paste("<colonne nom=\"infil\">", as.numeric(as.character(Sol_data$infil_3[ks])), 
                      "</colonne>", sep = "")))
    cat("\n")
    cat(noquote(paste("<colonne nom=\"epd\">", as.numeric(as.character(Sol_data$epd_3[ks])), 
                      "</colonne>", sep = "")))
    cat("\n")
    cat(noquote(paste("</tableau>", sep = "")))
    cat("\n")
    cat(noquote(paste("<tableau nb_colonnes=\"8\" nom=\"layer 4\">", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("<colonne nom=\"epc\">", as.numeric(as.character(Sol_data$epc_4[ks])), 
                      "</colonne>", sep = "")))
    cat("\n")
    cat(noquote(paste("<colonne nom=\"HCCF\">", as.numeric(as.character(Sol_data$HCCF_4[ks])), 
                      "</colonne>", sep = "")))
    cat("\n")
    cat(noquote(paste("<colonne nom=\"HMINF\">", as.numeric(as.character(Sol_data$HMINF_4[ks])), 
                      "</colonne>", sep = "")))
    cat("\n")
    cat(noquote(paste("<colonne nom=\"DAF\">", as.numeric(as.character(Sol_data$DAF_4[ks])), 
                      "</colonne>", sep = "")))
    cat("\n")
    cat(noquote(paste("<colonne nom=\"cailloux\">", as.numeric(as.character(Sol_data$cailloux_4[ks])), 
                      "</colonne>", sep = "")))
    cat("\n")
    cat(noquote(paste("<colonne nom=\"typecailloux\">", as.numeric(as.character(Sol_data$typecailloux_4[ks])), 
                      "</colonne>", sep = "")))
    cat("\n")
    cat(noquote(paste("<colonne nom=\"infil\">", as.numeric(as.character(Sol_data$infil_4[ks])), 
                      "</colonne>", sep = "")))
    cat("\n")
    cat(noquote(paste("<colonne nom=\"epd\">", as.numeric(as.character(Sol_data$epd_4[ks])), 
                      "</colonne>", sep = "")))
    cat("\n")
    cat(noquote(paste("</tableau>", sep = "")))
    cat("\n")
    cat(noquote(paste("<tableau nb_colonnes=\"8\" nom=\"layer 5\">", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("<colonne nom=\"epc\">", as.numeric(as.character(Sol_data$epc_5[ks])), 
                      "</colonne>", sep = "")))
    cat("\n")
    cat(noquote(paste("<colonne nom=\"HCCF\">", as.numeric(as.character(Sol_data$HCCF_5[ks])), 
                      "</colonne>", sep = "")))
    cat("\n")
    cat(noquote(paste("<colonne nom=\"HMINF\">", as.numeric(as.character(Sol_data$HMINF_5[ks])), 
                      "</colonne>", sep = "")))
    cat("\n")
    cat(noquote(paste("<colonne nom=\"DAF\">", as.numeric(as.character(Sol_data$DAF_5[ks])), 
                      "</colonne>", sep = "")))
    cat("\n")
    cat(noquote(paste("<colonne nom=\"cailloux\">", as.numeric(as.character(Sol_data$cailloux_5[ks])), 
                      "</colonne>", sep = "")))
    cat("\n")
    cat(noquote(paste("<colonne nom=\"typecailloux\">", as.numeric(as.character(Sol_data$typecailloux_5[ks])), 
                      "</colonne>", sep = "")))
    cat("\n")
    cat(noquote(paste("<colonne nom=\"infil\">", as.numeric(as.character(Sol_data$infil_5[ks])), 
                      "</colonne>", sep = "")))
    cat("\n")
    cat(noquote(paste("<colonne nom=\"epd\">", as.numeric(as.character(Sol_data$epd_5[ks])), 
                      "</colonne>", sep = "")))
    cat("\n")
    cat(noquote(paste("</tableau>", sep = "")))
    cat("\n")
    cat(noquote(paste("</sol>", sep = "")))
  }
  cat("\n")
  cat(noquote("</sols>"))
  sink()
  if (!isunix) {
    close(Progress_bar)
  }
  return("Tout s'est bien deroule !!! Super :-)")
}

STICS_mailing_Sta2 <- function (stwF, Excel_file, sheetIndex, stwR) {
  isunix <- is_unix()
  #Sta_data <- as.data.frame(read_excel(path = file.path(stwF, 
  #                                                      Excel_file), sheet = sheetIndex, col_types = "text"))
  Sta_data <- as.data.frame(read.csv2(file = file.path(stwF, Excel_file), header = T,sep =";"))
  
  Sta_data_default <- as.data.frame(read_excel(path = file.path(system.file("extdata/data", 
                                                                            package = "STICSmailing"), "Inputs_STICS_mailing.xlsx"), 
                                               sheet = "Station"))
  if (is_missing_df(Sta_data, Sta_data_default)) {
    return()
  }
  if (dir.exists(path.expand(stwR)) == FALSE) {
    dir.create(stwR)
  }
  if (!isunix) {
    Progress_bar_n = 0
    Progress_bar_max = length(Sta_data$Sta_name)
    Progress_bar = winProgressBar(title = "Progress bar", 
                                  min = 0, max = Progress_bar_max, width = 500)
  }
  else {
    print("Creating station files...")
  }
  nb_sta = dim(Sta_data)[1]
  if (length(unique(Sta_data$Sta_name)) != nb_sta) {
    warning("Duplicated file names in Sta_name, aborting !")
    return((invisible()))
  }
  for (ks in 1:nb_sta) {
    if (!isunix) {
      Progress_bar_n = Progress_bar_n + 1
      setWinProgressBar(Progress_bar, value = Progress_bar_n, 
                        title = paste(round(Progress_bar_n/Progress_bar_max * 
                                              100, 0), "% done - Creating station files", 
                                      sep = ""))
    }
    sta_name <- Sta_data$Sta_name[ks]
    file_path <- file.path(stwR, sta_name)
    if (file.exists(file_path)) {
      file.remove(file_path)
    }
    sink(file_path)
    cat(noquote(paste("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>", 
                      sep = "")))
    cat("\n")
    cat(noquote("<fichiersta>"))
    cat("\n")
    cat(noquote(paste("<formalisme nom=\"Weather station\">", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("<param format=\"real\" max=\"10.0\" min=\"2.0\" nom=\"zr\">", 
                      as.numeric(as.character(Sta_data$zr[ks])), "</param>", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("<param format=\"real\" max=\"10.0\" min=\"0.0\" nom=\"NH3ref\">", 
                      as.numeric(as.character(Sta_data$NH3ref[ks])), "</param>", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("<param format=\"real\" max=\"90.0\" min=\"-90.0\" nom=\"latitude\">", 
                      as.numeric(as.character(Sta_data$latitude[ks])), 
                      "</param>", sep = "")))
    cat("\n")
    cat(noquote(paste("<param format=\"real\" max=\"1200.0\" min=\"800.0\" nom=\"patm\">", 
                      as.numeric(as.character(Sta_data$patm[ks])), "</param>", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("<param format=\"real\" max=\"25.0\" min=\"4.0\" nom=\"aclim\">", 
                      as.numeric(as.character(Sta_data$aclim[ks])), "</param>", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("</formalisme>", sep = "")))
    cat("\n")
    cat(noquote(paste("<formalisme nom=\"climate\">", sep = "")))
    cat("\n")
    cat(noquote(paste("<option choix=\"", as.numeric(as.character(Sta_data$codeetp[ks])), 
                      "\" nom=\"reading OR calculation of PET\" nomParam=\"codeetp\">", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("<choix code=\"1\" nom=\"PET-Penman_reading\"/>", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("<choix code=\"2\" nom=\"PET-Penman_calculation\"/>", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("<choix code=\"3\" nom=\"PET-Shuttleworth-Wallace_calculation\"/>", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("<choix code=\"4\" nom=\"PET-Priestley-Taylor_calculation\">", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("<param format=\"real\" max=\"2.0\" min=\"1.0\" nom=\"alphapt\">", 
                      as.numeric(as.character(Sta_data$alphapt[ks])), "</param>", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("</choix>", sep = "")))
    cat("\n")
    cat(noquote(paste("</option>", sep = "")))
    cat("\n")
    cat(noquote(paste("<option choix=\"", as.numeric(as.character(Sta_data$codeclichange[ks])), 
                      "\" nom=\"climate change\" nomParam=\"codeclichange\">", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("<choix code=\"1\" nom=\"no\"/>", sep = "")))
    cat("\n")
    cat(noquote(paste("<choix code=\"2\" nom=\"yes\"/>", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("</option>", sep = "")))
    cat("\n")
    cat(noquote(paste("<option choix=\"", as.numeric(as.character(Sta_data$codaltitude[ks])), 
                      "\" nom=\"climate in altitude\" nomParam=\"codaltitude\">", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("<choix code=\"1\" nom=\"no\"/>", sep = "")))
    cat("\n")
    cat(noquote(paste("<choix code=\"2\" nom=\"yes\">", sep = "")))
    cat("\n")
    cat(noquote(paste("<param format=\"real\" max=\"2000.0\" min=\"0.0\" nom=\"altistation\">", 
                      as.numeric(as.character(Sta_data$altistation[ks])), 
                      "</param>", sep = "")))
    cat("\n")
    cat(noquote(paste("<param format=\"real\" max=\"2000.0\" min=\"0.0\" nom=\"altisimul\">", 
                      as.numeric(as.character(Sta_data$altisimul[ks])), 
                      "</param>", sep = "")))
    cat("\n")
    cat(noquote(paste("<param format=\"real\" max=\"3.0\" min=\"0.1\" nom=\"gradtn\">", 
                      as.numeric(as.character(Sta_data$gradtn[ks])), "</param>", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("<param format=\"real\" max=\"3.0\" min=\"0.1\" nom=\"gradtx\">", 
                      as.numeric(as.character(Sta_data$gradtx[ks])), "</param>", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("<param format=\"real\" max=\"2000.0\" min=\"0.0\" nom=\"altinversion\">", 
                      as.numeric(as.character(Sta_data$altinversion[ks])), 
                      "</param>", sep = "")))
    cat("\n")
    cat(noquote(paste("<param format=\"real\" max=\"3.0\" min=\"0.1\" nom=\"gradtninv\">", 
                      as.numeric(as.character(Sta_data$gradtninv[ks])), 
                      "</param>", sep = "")))
    cat("\n")
    cat(noquote(paste("<param format=\"real\" max=\"1.0\" min=\"0.0\" nom=\"cielclair\">", 
                      as.numeric(as.character(Sta_data$cielclair[ks])), 
                      "</param>", sep = "")))
    cat("\n")
    cat(noquote(paste("<option choix=\"", as.numeric(as.character(Sta_data$codadret[ks])), 
                      "\" nom=\"option.adret.or.ubac\" nomParam=\"codadret\">", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("<choix code=\"1\" nom=\"adret(south)\"/>", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("<choix code=\"2\" nom=\"ubac(north)\">", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("<param format=\"real\" max=\"5.0\" min=\"-5.0\" nom=\"ombragetx\">", 
                      as.numeric(as.character(Sta_data$ombragetx[ks])), 
                      "</param>", sep = "")))
    cat("\n")
    cat(noquote(paste("</choix>", sep = "")))
    cat("\n")
    cat(noquote(paste("</option>", sep = "")))
    cat("\n")
    cat(noquote(paste("</choix>", sep = "")))
    cat("\n")
    cat(noquote(paste("</option>", sep = "")))
    cat("\n")
    cat(noquote(paste("</formalisme>", sep = "")))
    cat("\n")
    cat(noquote(paste("<formalisme nom=\"Microclimate\">", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("<param format=\"real\" max=\"70.0\" min=\"10.0\" nom=\"ra\">", 
                      as.numeric(as.character(Sta_data$ra[ks])), "</param>", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("<param format=\"real\" max=\"0.3\" min=\"0.05\" nom=\"albveg\">", 
                      as.numeric(as.character(Sta_data$albveg[ks])), "</param>", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("<param nom=\"aangst\">", as.numeric(as.character(Sta_data$aangst[ks])), 
                      "</param>", sep = "")))
    cat("\n")
    cat(noquote(paste("<param format=\"real\" max=\"1.0\" min=\"0.0\" nom=\"bangst\">", 
                      as.numeric(as.character(Sta_data$bangst[ks])), "</param>", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("<param format=\"real\" max=\"5.0\" min=\"-5.0\" nom=\"corecTrosee\">", 
                      as.numeric(as.character(Sta_data$corecTrosee[ks])), 
                      "</param>", sep = "")))
    cat("\n")
    cat(noquote(paste("<option choix=\"", as.numeric(as.character(Sta_data$codecaltemp[ks])), 
                      "\" nom=\"calculation of crop temperature\" nomParam=\"codecaltemp\">", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("<choix code=\"1\" nom=\"empirical relation\"/>", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("<choix code=\"2\" nom=\"energy balance\"/>", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("</option>", sep = "")))
    cat("\n")
    cat(noquote(paste("<option choix=\"", as.numeric(as.character(Sta_data$codernet[ks])), 
                      "\" nom=\"calculation of net radiation\" nomParam=\"codernet\">", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("<choix code=\"1\" nom=\"Brunt\"/>", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("<choix code=\"2\" nom=\"Brutsaert\"/>", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("</option>", sep = "")))
    cat("\n")
    cat(noquote(paste("</formalisme>", sep = "")))
    cat("\n")
    cat(noquote(paste("<formalisme nom=\"climate under a shelter\">", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("<param format=\"real\" max=\"1.2\" min=\"0.3\" nom=\"coefdevil\">", 
                      as.numeric(as.character(Sta_data$coefdevil[ks])), 
                      "</param>", sep = "")))
    cat("\n")
    cat(noquote(paste("<param format=\"real\" max=\"100.0\" min=\"0.01\" nom=\"aks\">", 
                      as.numeric(as.character(Sta_data$aks[ks])), "</param>", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("<param format=\"real\" max=\"100.0\" min=\"0.0\" nom=\"bks\">", 
                      as.numeric(as.character(Sta_data$bks[ks])), "</param>", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("<param format=\"real\" max=\"1.0\" min=\"0.0010\" nom=\"cvent\">", 
                      as.numeric(as.character(Sta_data$cvent[ks])), "</param>", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("<param format=\"real\" max=\"0.01\" min=\"0.0\" nom=\"phiv0\">", 
                      as.numeric(as.character(Sta_data$phiv0[ks])), "</param>", 
                      sep = "")))
    cat("\n")
    cat(noquote(paste("<param format=\"real\" max=\"1.0\" min=\"0.0010\" nom=\"coefrnet\">", 
                      as.numeric(as.character(Sta_data$coefrnet[ks])), 
                      "</param>", sep = "")))
    cat("\n")
    cat(noquote(paste("</formalisme>", sep = "")))
    cat("\n")
    cat(noquote(paste("</fichiersta>", sep = "")))
    sink()
  }
  if (!isunix) {
    close(Progress_bar)
  }
  return("Tout s'est bien deroule !!! Super :-)")
}

STICS_mailing_Tec2 <- function (stwF, Excel_file, sheetIndex, stwR,
                               option_engrais_multiple = 2, codetauxexportfauche = 2, codejourdes = 2) {
  
  ## On charge la library suivante pour permettre de lire une feuille d'un fichier Excel
  
  # requireNamespace("readxl")
  # library("readxl")
  
  isunix <- is_unix()
  
  ## On se place dans l'environnement de travail ou se trouve le fichier Excel avec les donnees necessaire a la creation des fichiers tecs
  
  #.Internal(setwd(stwF))
  
#  Tec_data <- as.data.frame(read_excel(path = file.path(stwF, Excel_file), sheet = sheetIndex, col_types = "text"))
  Tec_data <- as.data.frame(read.csv2(file = file.path(stwF, Excel_file), header = T,sep =";"))
  
  
  ## On verifie que les options ont une valeurs attendues
  
  if (option_engrais_multiple != 1 & option_engrais_multiple != 2) { return ("`option_engrais_multiple` doit prendre soit la valeur 1, soit la valeur 2")}
  if (codetauxexportfauche != 1 & codetauxexportfauche != 2) { return ("`codetauxexportfauche` doit prendre soit la valeur 1, soit la valeur 2")}
  if (codejourdes != 1 & codejourdes != 2) { return ("`codejourdes` doit prendre soit la valeur 1, soit la valeur 2")}
  
  ## On verifie que toutes les colonnes attendues sont bien presentes
  
  # On charge les colonnes par defaut :
  
  Tec_data_default <- as.data.frame(read_excel(path = file.path(system.file("extdata/data", package = "STICSmailing"),"Inputs_STICS_mailing.xlsx"),
                                               sheet = "Tec"))
  
  # Mis dans une fonction generique + avec le message + status retour pour sortir
  # de STICS_mailing_Ini
  # absence de donnees dans le tableau -> donne des NA
  # donc le check est
  if ( is_missing_df(Tec_data, Tec_data_default) ) {
    return()
  }
  
  # Lack_data <- NULL
  #
  # if (is.null(Tec_data$ITK_name)) { Lack_data <- c(Lack_data, "ITK_name") }
  #
  # for (kt in 1:5) {
  #
  #   julres =  paste("julres", kt, sep = "_")
  #   coderes =  paste("coderes", kt, sep = "_")
  #   qres =  paste("qres", kt, sep = "_")
  #   Crespc =  paste("Crespc", kt, sep = "_")
  #   CsurNres =  paste("CsurNres", kt, sep = "_")
  #   Nminres =  paste("Nminres", kt, sep = "_")
  #   eaures =  paste("eaures", kt, sep = "_")
  #
  #   if (is.null(Tec_data[[julres]])) { Lack_data <- c(Lack_data, julres) }
  #   if (is.null(Tec_data[[coderes]])) { Lack_data <- c(Lack_data, coderes) }
  #   if (is.null(Tec_data[[qres]])) { Lack_data <- c(Lack_data, qres) }
  #   if (is.null(Tec_data[[Crespc]])) { Lack_data <- c(Lack_data, Crespc) }
  #   if (is.null(Tec_data[[CsurNres]])) { Lack_data <- c(Lack_data, CsurNres) }
  #   if (is.null(Tec_data[[Nminres]])) { Lack_data <- c(Lack_data, Nminres) }
  #   if (is.null(Tec_data[[eaures]])) { Lack_data <- c(Lack_data, eaures) }
  #
  # }
  #
  # for (kt in 1:8) {
  #
  #   jultrav =  paste("jultrav", kt, sep = "_")
  #   profres =  paste("profres", kt, sep = "_")
  #   proftrav =  paste("proftrav", kt, sep = "_")
  #
  #   if (is.null(Tec_data[[jultrav]])) { Lack_data <- c(Lack_data, jultrav) }
  #   if (is.null(Tec_data[[profres]])) { Lack_data <- c(Lack_data, profres) }
  #   if (is.null(Tec_data[[proftrav]])) { Lack_data <- c(Lack_data, proftrav) }
  #
  # }
  #
  # if (is.null(Tec_data$iplt0)) { Lack_data <- c(Lack_data, "iplt0") }
  # if (is.null(Tec_data$profsem)) { Lack_data <- c(Lack_data, "profsem") }
  # if (is.null(Tec_data$densitesem)) { Lack_data <- c(Lack_data, "densitesem") }
  # if (is.null(Tec_data$variete)) { Lack_data <- c(Lack_data, "variete") }
  # if (is.null(Tec_data$codetradtec)) { Lack_data <- c(Lack_data, "codetradtec") }
  # if (is.null(Tec_data$interrang)) { Lack_data <- c(Lack_data, "interrang") }
  # if (is.null(Tec_data$orientrang)) { Lack_data <- c(Lack_data, "orientrang") }
  # if (is.null(Tec_data$codedecisemis)) { Lack_data <- c(Lack_data, "codedecisemis") }
  # if (is.null(Tec_data$nbjmaxapressemis)) { Lack_data <- c(Lack_data, "nbjmaxapressemis") }
  # if (is.null(Tec_data$nbjseuiltempref)) { Lack_data <- c(Lack_data, "nbjseuiltempref") }
  #
  # if (is.null(Tec_data$codestade)) { Lack_data <- c(Lack_data, "codestade") }
  # if (is.null(Tec_data$ilev)) { Lack_data <- c(Lack_data, "ilev") }
  # if (is.null(Tec_data$iamf)) { Lack_data <- c(Lack_data, "iamf") }
  # if (is.null(Tec_data$ilax)) { Lack_data <- c(Lack_data, "ilax") }
  # if (is.null(Tec_data$isen)) { Lack_data <- c(Lack_data, "isen") }
  # if (is.null(Tec_data$ilan)) { Lack_data <- c(Lack_data, "ilan") }
  # if (is.null(Tec_data$iflo)) { Lack_data <- c(Lack_data, "iflo") }
  # if (is.null(Tec_data$idrp)) { Lack_data <- c(Lack_data, "idrp") }
  # if (is.null(Tec_data$imat)) { Lack_data <- c(Lack_data, "imat") }
  # if (is.null(Tec_data$irec)) { Lack_data <- c(Lack_data, "irec") }
  # if (is.null(Tec_data$irecbutoir)) { Lack_data <- c(Lack_data, "irecbutoir") }
  # if (codejourdes == 1) {
  #   if (is.null(Tec_data$juldes)) { Lack_data <- c(Lack_data, "juldes") }
  # }
  #
  # if (is.null(Tec_data$effirr)) { Lack_data <- c(Lack_data, "effirr") }
  # if (is.null(Tec_data$codecalirrig)) { Lack_data <- c(Lack_data, "codecalirrig") }
  # if (is.null(Tec_data$ratiol)) { Lack_data <- c(Lack_data, "ratiol") }
  # if (is.null(Tec_data$dosimx)) { Lack_data <- c(Lack_data, "dosimx") }
  # if (is.null(Tec_data$doseirrigmin)) { Lack_data <- c(Lack_data, "doseirrigmin") }
  # if (is.null(Tec_data$codedateappH2O)) { Lack_data <- c(Lack_data, "codedateappH2O") }
  # if (is.null(Tec_data$codlocirrig)) { Lack_data <- c(Lack_data, "codlocirrig") }
  # if (is.null(Tec_data$locirrig)) { Lack_data <- c(Lack_data, "locirrig") }
  # if (is.null(Tec_data$profmes)) { Lack_data <- c(Lack_data, "profmes") }
  #
  # for (kt in 1:20) {
  #
  #   julapI_or_sum_upvt =  paste("julapI_or_sum_upvt", kt, sep = "_")
  #   Amount_Irrig =  paste("Amount_Irrig", kt, sep = "_")
  #
  #   if (is.null(Tec_data[[julapI_or_sum_upvt]])) { Lack_data <- c(Lack_data, julapI_or_sum_upvt) }
  #   if (is.null(Tec_data[[Amount_Irrig]])) { Lack_data <- c(Lack_data, Amount_Irrig) }
  #
  # }
  #
  # if (is.null(Tec_data$engrais)) { Lack_data <- c(Lack_data, "engrais") }
  # if (is.null(Tec_data$concirr)) { Lack_data <- c(Lack_data, "concirr") }
  # if (is.null(Tec_data$codedateappN)) { Lack_data <- c(Lack_data, "codedateappN") }
  # if (is.null(Tec_data$codefracappN)) { Lack_data <- c(Lack_data, "codefracappN") }
  # if (is.null(Tec_data$Qtot_N)) { Lack_data <- c(Lack_data, "Qtot_N") }
  #
  # for (kt in 1:8) {
  #
  #   julapN_or_sum_upvt =  paste("julapN_or_sum_upvt", kt, sep = "_")
  #   Amount_N_Ferti =  paste("Amount_N_Ferti", kt, sep = "_")
  #   if (option_engrais_multiple == 1) { engrais =  paste("engrais", kt, sep = "_") }
  #
  #   if (is.null(Tec_data[[julapN_or_sum_upvt]])) { Lack_data <- c(Lack_data, julapN_or_sum_upvt) }
  #   if (is.null(Tec_data[[Amount_N_Ferti]])) { Lack_data <- c(Lack_data, Amount_N_Ferti) }
  #   if (option_engrais_multiple == 1) { if (is.null(Tec_data[[engrais]])) { Lack_data <- c(Lack_data, engrais) } }
  #
  # }
  #
  # if (is.null(Tec_data$codlocferti)) { Lack_data <- c(Lack_data, "codlocferti") }
  # if (is.null(Tec_data$locferti)) { Lack_data <- c(Lack_data, "locferti") }
  # if (is.null(Tec_data$ressuite)) { Lack_data <- c(Lack_data, "ressuite") }
  #
  # if (is.null(Tec_data$codcueille)) { Lack_data <- c(Lack_data, "codcueille") }
  # if (is.null(Tec_data$nbcueille)) { Lack_data <- c(Lack_data, "nbcueille") }
  # if (is.null(Tec_data$cadencerec)) { Lack_data <- c(Lack_data, "cadencerec") }
  # if (is.null(Tec_data$codrecolte)) { Lack_data <- c(Lack_data, "codrecolte") }
  # if (is.null(Tec_data$codeaumin)) { Lack_data <- c(Lack_data, "codeaumin") }
  # if (is.null(Tec_data$h2ograinmin)) { Lack_data <- c(Lack_data, "h2ograinmin") }
  # if (is.null(Tec_data$h2ograinmax)) { Lack_data <- c(Lack_data, "h2ograinmax") }
  # if (is.null(Tec_data$sucrerec)) { Lack_data <- c(Lack_data, "sucrerec") }
  # if (is.null(Tec_data$CNgrainrec)) { Lack_data <- c(Lack_data, "CNgrainrec") }
  # if (is.null(Tec_data$huilerec)) { Lack_data <- c(Lack_data, "huilerec") }
  # if (is.null(Tec_data$coderecolteassoc)) { Lack_data <- c(Lack_data, "coderecolteassoc") }
  # if (is.null(Tec_data$codedecirecolte)) { Lack_data <- c(Lack_data, "codedecirecolte") }
  # if (is.null(Tec_data$nbjmaxapresrecolte)) { Lack_data <- c(Lack_data, "nbjmaxapresrecolte") }
  #
  # if (is.null(Tec_data$codefauche)) { Lack_data <- c(Lack_data, "codefauche") }
  # if (is.null(Tec_data$mscoupemini)) { Lack_data <- c(Lack_data, "mscoupemini") }
  # if (is.null(Tec_data$codemodfauche)) { Lack_data <- c(Lack_data, "codemodfauche") }
  # if (is.null(Tec_data$hautcoupedefaut)) { Lack_data <- c(Lack_data, "hautcoupedefaut") }
  # if (is.null(Tec_data$stadecoupedf)) { Lack_data <- c(Lack_data, "stadecoupedf") }
  #
  #
  # for (kt in 1:10) {
  #
  #   julfauche =  paste("julfauche", kt, sep = "_")
  #   tempfauche =  paste("tempfauche", kt, sep = "_")
  #   hautcoupe =  paste("hautcoupe", kt, sep = "_")
  #   lairesiduel =  paste("lairesiduel", kt, sep = "_")
  #   msresiduel =  paste("msresiduel", kt, sep = "_")
  #   anitcoupe =  paste("anitcoupe", kt, sep = "_")
  #   if (codetauxexportfauche == 1) { tauxexportfauche =  paste("tauxexportfauche", kt, sep = "_") }
  #
  #   if (is.null(Tec_data[[julfauche]])) { Lack_data <- c(Lack_data, julfauche) }
  #   if (is.null(Tec_data[[tempfauche]])) { Lack_data <- c(Lack_data, tempfauche) }
  #   if (is.null(Tec_data[[hautcoupe]])) { Lack_data <- c(Lack_data, hautcoupe) }
  #   if (is.null(Tec_data[[lairesiduel]])) { Lack_data <- c(Lack_data, lairesiduel) }
  #   if (is.null(Tec_data[[msresiduel]])) { Lack_data <- c(Lack_data, msresiduel) }
  #   if (is.null(Tec_data[[anitcoupe]])) { Lack_data <- c(Lack_data, anitcoupe) }
  #   if (codetauxexportfauche == 1) { if (is.null(Tec_data[[tauxexportfauche]])) { Lack_data <- c(Lack_data, tauxexportfauche) } }
  #
  # }
  #
  # if (is.null(Tec_data$codepaillage)) { Lack_data <- c(Lack_data, "codepaillage") }
  # if (is.null(Tec_data$couvermulchplastique)) { Lack_data <- c(Lack_data, "couvermulchplastique") }
  # if (is.null(Tec_data$albedomulchplastique)) { Lack_data <- c(Lack_data, "albedomulchplastique") }
  # if (is.null(Tec_data$codrognage)) { Lack_data <- c(Lack_data, "codrognage") }
  # if (is.null(Tec_data$largrogne)) { Lack_data <- c(Lack_data, "largrogne") }
  # if (is.null(Tec_data$hautrogne)) { Lack_data <- c(Lack_data, "hautrogne") }
  # if (is.null(Tec_data$biorognem)) { Lack_data <- c(Lack_data, "biorognem") }
  # if (is.null(Tec_data$codcalrogne)) { Lack_data <- c(Lack_data, "codcalrogne") }
  # if (is.null(Tec_data$julrogne)) { Lack_data <- c(Lack_data, "julrogne") }
  # if (is.null(Tec_data$margerogne)) { Lack_data <- c(Lack_data, "margerogne") }
  # if (is.null(Tec_data$codeclaircie)) { Lack_data <- c(Lack_data, "codeclaircie") }
  # if (is.null(Tec_data$juleclair)) { Lack_data <- c(Lack_data, "juleclair") }
  # if (is.null(Tec_data$nbinfloecl)) { Lack_data <- c(Lack_data, "nbinfloecl") }
  # if (is.null(Tec_data$codeffeuil)) { Lack_data <- c(Lack_data, "codeffeuil") }
  # if (is.null(Tec_data$codhauteff)) { Lack_data <- c(Lack_data, "codhauteff") }
  # if (is.null(Tec_data$codcaleffeuil)) { Lack_data <- c(Lack_data, "codcaleffeuil") }
  # if (is.null(Tec_data$laidebeff)) { Lack_data <- c(Lack_data, "laidebeff") }
  # if (is.null(Tec_data$effeuil)) { Lack_data <- c(Lack_data, "effeuil") }
  # if (is.null(Tec_data$juleffeuil)) { Lack_data <- c(Lack_data, "juleffeuil") }
  # if (is.null(Tec_data$laieffeuil)) { Lack_data <- c(Lack_data, "laieffeuil") }
  # if (is.null(Tec_data$codetaille)) { Lack_data <- c(Lack_data, "codetaille") }
  # if (is.null(Tec_data$jultaille)) { Lack_data <- c(Lack_data, "jultaille") }
  # if (is.null(Tec_data$codepalissage)) { Lack_data <- c(Lack_data, "codepalissage") }
  # if (is.null(Tec_data$hautmaxtec)) { Lack_data <- c(Lack_data, "hautmaxtec") }
  # if (is.null(Tec_data$largtec)) { Lack_data <- c(Lack_data, "largtec") }
  # if (is.null(Tec_data$codabri)) { Lack_data <- c(Lack_data, "codabri") }
  # if (is.null(Tec_data$transplastic)) { Lack_data <- c(Lack_data, "transplastic") }
  # if (is.null(Tec_data$surfouvre1)) { Lack_data <- c(Lack_data, "surfouvre1") }
  # if (is.null(Tec_data$julouvre2)) { Lack_data <- c(Lack_data, "julouvre2") }
  # if (is.null(Tec_data$surfouvre2)) { Lack_data <- c(Lack_data, "surfouvre2") }
  # if (is.null(Tec_data$julouvre3)) { Lack_data <- c(Lack_data, "julouvre3") }
  # if (is.null(Tec_data$surfouvre3)) { Lack_data <- c(Lack_data, "surfouvre3") }
  # if (is.null(Tec_data$codeDST)) { Lack_data <- c(Lack_data, "codeDST") }
  # if (is.null(Tec_data$dachisel)) { Lack_data <- c(Lack_data, "dachisel") }
  # if (is.null(Tec_data$dalabour)) { Lack_data <- c(Lack_data, "dalabour") }
  # if (is.null(Tec_data$rugochisel)) { Lack_data <- c(Lack_data, "rugochisel") }
  # if (is.null(Tec_data$rugolabour)) { Lack_data <- c(Lack_data, "rugolabour") }
  # if (is.null(Tec_data$codeDSTtass)) { Lack_data <- c(Lack_data, "codeDSTtass") }
  # if (is.null(Tec_data$profhumsemoir)) { Lack_data <- c(Lack_data, "profhumsemoir") }
  # if (is.null(Tec_data$dasemis)) { Lack_data <- c(Lack_data, "dasemis") }
  # if (is.null(Tec_data$profhumrecolteuse)) { Lack_data <- c(Lack_data, "profhumrecolteuse") }
  # if (is.null(Tec_data$darecolte)) { Lack_data <- c(Lack_data, "darecolte") }
  # if (is.null(Tec_data$codeDSTnbcouche)) { Lack_data <- c(Lack_data, "codeDSTnbcouche") }
  #
  # if (!is.null(Lack_data)) { return(paste("Il manque la colonne suivante : ", Lack_data, sep = "")) }
  
  ## Une fois le fichier Excel lu
  ## On cree un dossier de sauvegarde pour les fichiers tecs
  
  if (dir.exists(path.expand(stwR)) == FALSE) {
    
    dir.create(stwR)
    
  }
  
  ## On se place dans le dosier de travail adequat
  
  #.Internal(setwd(stwR))
  
  ## On utilise une barre de progression pour suivre l'evolution de l'execution
  
  
  if (! isunix ) {
    Progress_bar_n = 0
    Progress_bar_max = length(Tec_data$ITK_name)
    
    Progress_bar = winProgressBar(title = "Progress bar", min = 0, max = Progress_bar_max, width = 500)
    
  } else {
    print("Creating crop management files...")
  }
  
  
  nb_tec = dim(Tec_data)[1]
  # Checking if any duplicates
  if (length(unique(Tec_data$ITK_name)) != nb_tec) {
    warning("Duplicated file names in ITK_name, aborting !")
    return((invisible()))
  }
  
  # on va donc desormais creer les divers fichier en les placant dans les dossiers adaptes
  
  for(kt in 1:length(Tec_data$ITK_name)) {
    
    if (! isunix ) {
      Progress_bar_n = Progress_bar_n + 1
      setWinProgressBar(Progress_bar, value = Progress_bar_n, title = paste(round(Progress_bar_n / Progress_bar_max * 100, 0), "% done - Creating crop management files", sep = ""))
    }
    
    # if (file.exists(paste(Tec_data$ITK_name[kt]))) {
    #
    #   file.remove(paste(Tec_data$ITK_name[kt]))
    #
    # }
    
    tec_name <- Tec_data$ITK_name[kt]
    file_path <- file.path(stwR,tec_name)
    
    if (file.exists(file_path)) {
      
      file.remove(file_path)
      
    }
    
    
    sink(file_path)
    
    cat(noquote(paste('<?xml version="1.0" encoding="UTF-8" standalone="no"?>', sep = "")))
    cat("\n")
    cat(noquote(paste('<fichiertec>', sep = "")))
    cat("\n")
    cat(noquote(paste('<formalisme nom="supply of organic residus">', sep = "")))
    cat("\n")
    cat(noquote(paste('<ta nb_interventions="5" nom="interventions">', sep = "")))
    cat("\n")
    cat(noquote(paste('<ta_entete nb_colonnes="7">', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="julres"/>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="coderes"/>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="qres"/>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="Crespc"/>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="CsurNres"/>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="Nminres"/>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="eaures"/>', sep = "")))
    cat("\n")
    cat(noquote(paste('</ta_entete>', sep = "")))
    cat("\n")
    cat(noquote(paste('<intervention nb_colonnes="7">', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="julres">', as.numeric(as.character(Tec_data$julres_1[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="coderes">', as.numeric(as.character(Tec_data$coderes_1[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="qres">', as.numeric(as.character(Tec_data$qres_1[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="Crespc">', as.numeric(as.character(Tec_data$Crespc_1[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="CsurNres">', as.numeric(as.character(Tec_data$CsurNres_1[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="Nminres">', as.numeric(as.character(Tec_data$Nminres_1[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="eaures">', as.numeric(as.character(Tec_data$eaures_1[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('</intervention>', sep = "")))
    cat("\n")
    cat(noquote(paste('<intervention nb_colonnes="7">', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="julres">', as.numeric(as.character(Tec_data$julres_2[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="coderes">', as.numeric(as.character(Tec_data$coderes_2[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="qres">', as.numeric(as.character(Tec_data$qres_2[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="Crespc">', as.numeric(as.character(Tec_data$Crespc_2[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="CsurNres">', as.numeric(as.character(Tec_data$CsurNres_2[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="Nminres">', as.numeric(as.character(Tec_data$Nminres_2[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="eaures">', as.numeric(as.character(Tec_data$eaures_2[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('</intervention>', sep = "")))
    cat("\n")
    cat(noquote(paste('<intervention nb_colonnes="7">', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="julres">', as.numeric(as.character(Tec_data$julres_3[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="coderes">', as.numeric(as.character(Tec_data$coderes_3[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="qres">', as.numeric(as.character(Tec_data$qres_3[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="Crespc">', as.numeric(as.character(Tec_data$Crespc_3[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="CsurNres">', as.numeric(as.character(Tec_data$CsurNres_3[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="Nminres">', as.numeric(as.character(Tec_data$Nminres_3[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="eaures">', as.numeric(as.character(Tec_data$eaures_3[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('</intervention>', sep = "")))
    cat("\n")
    cat(noquote(paste('<intervention nb_colonnes="7">', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="julres">', as.numeric(as.character(Tec_data$julres_4[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="coderes">', as.numeric(as.character(Tec_data$coderes_4[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="qres">', as.numeric(as.character(Tec_data$qres_4[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="Crespc">', as.numeric(as.character(Tec_data$Crespc_4[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="CsurNres">', as.numeric(as.character(Tec_data$CsurNres_4[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="Nminres">', as.numeric(as.character(Tec_data$Nminres_4[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="eaures">', as.numeric(as.character(Tec_data$eaures_4[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('</intervention>', sep = "")))
    cat("\n")
    cat(noquote(paste('<intervention nb_colonnes="7">', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="julres">', as.numeric(as.character(Tec_data$julres_5[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="coderes">', as.numeric(as.character(Tec_data$coderes_5[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="qres">', as.numeric(as.character(Tec_data$qres_5[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="Crespc">', as.numeric(as.character(Tec_data$Crespc_5[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="CsurNres">', as.numeric(as.character(Tec_data$CsurNres_5[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="Nminres">', as.numeric(as.character(Tec_data$Nminres_5[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="eaures">', as.numeric(as.character(Tec_data$eaures_5[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('</intervention>', sep = "")))
    cat("\n")
    cat(noquote(paste('</ta>', sep = "")))
    cat("\n")
    cat(noquote(paste('</formalisme>', sep = "")))
    cat("\n")
    cat(noquote(paste('<formalisme nom="soil tillage">', sep = "")))
    cat("\n")
    cat(noquote(paste('<ta nb_interventions="8" nom="interventions">', sep = "")))
    cat("\n")
    cat(noquote(paste('<ta_entete nb_colonnes="3">', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="jultrav"/>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="profres"/>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="proftrav"/>', sep = "")))
    cat("\n")
    cat(noquote(paste('</ta_entete>', sep = "")))
    cat("\n")
    cat(noquote(paste('<intervention nb_colonnes="3">', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="jultrav">', as.numeric(as.character(Tec_data$jultrav_1[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="profres">', as.numeric(as.character(Tec_data$profres_1[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="proftrav">', as.numeric(as.character(Tec_data$proftrav_1[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('</intervention>', sep = "")))
    cat("\n")
    cat(noquote(paste('<intervention nb_colonnes="3">', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="jultrav">', as.numeric(as.character(Tec_data$jultrav_2[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="profres">', as.numeric(as.character(Tec_data$profres_2[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="proftrav">', as.numeric(as.character(Tec_data$proftrav_2[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('</intervention>', sep = "")))
    cat("\n")
    cat(noquote(paste('<intervention nb_colonnes="3">', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="jultrav">', as.numeric(as.character(Tec_data$jultrav_3[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="profres">', as.numeric(as.character(Tec_data$profres_3[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="proftrav">', as.numeric(as.character(Tec_data$proftrav_3[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('</intervention>', sep = "")))
    cat("\n")
    cat(noquote(paste('<intervention nb_colonnes="3">', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="jultrav">', as.numeric(as.character(Tec_data$jultrav_4[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="profres">', as.numeric(as.character(Tec_data$profres_4[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="proftrav">', as.numeric(as.character(Tec_data$proftrav_4[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('</intervention>', sep = "")))
    cat("\n")
    cat(noquote(paste('<intervention nb_colonnes="3">', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="jultrav">', as.numeric(as.character(Tec_data$jultrav_5[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="profres">', as.numeric(as.character(Tec_data$profres_5[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="proftrav">', as.numeric(as.character(Tec_data$proftrav_5[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('</intervention>', sep = "")))
    cat("\n")
    cat(noquote(paste('<intervention nb_colonnes="3">', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="jultrav">', as.numeric(as.character(Tec_data$jultrav_6[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="profres">', as.numeric(as.character(Tec_data$profres_6[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="proftrav">', as.numeric(as.character(Tec_data$proftrav_6[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('</intervention>', sep = "")))
    cat("\n")
    cat(noquote(paste('<intervention nb_colonnes="3">', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="jultrav">', as.numeric(as.character(Tec_data$jultrav_7[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="profres">', as.numeric(as.character(Tec_data$profres_7[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="proftrav">', as.numeric(as.character(Tec_data$proftrav_7[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('</intervention>', sep = "")))
    cat("\n")
    cat(noquote(paste('<intervention nb_colonnes="3">', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="jultrav">', as.numeric(as.character(Tec_data$jultrav_8[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="profres">', as.numeric(as.character(Tec_data$profres_8[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="proftrav">', as.numeric(as.character(Tec_data$proftrav_8[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('</intervention>', sep = "")))
    cat("\n")
    cat(noquote(paste('</ta>', sep = "")))
    cat("\n")
    cat(noquote(paste('</formalisme>', sep = "")))
    cat("\n")
    cat(noquote(paste('<formalisme nom="sowing">', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="integer" max="731" min="1" nom="iplt0">', as.numeric(as.character(Tec_data$iplt0[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="real" max="10.0" min="0.0" nom="profsem">', as.numeric(as.character(Tec_data$profsem[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="real" max="2000.0" min="0.05" nom="densitesem">', as.numeric(as.character(Tec_data$densitesem[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="integer" max="200" min="1" nom="variete">', as.numeric(as.character(Tec_data$variete[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('<option choix="', as.numeric(as.character(Tec_data$codetradtec[kt])), '" nom="planting structure (if radiative transfer)" nomParam="codetradtec">', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="1" nom="yes">', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="real" max="10.0" min="0.0" nom="interrang">', as.numeric(as.character(Tec_data$interrang[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="real" max="6.28" min="0.0" nom="orientrang">', as.numeric(as.character(Tec_data$orientrang[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('</choix>', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="2" nom="no"/>', sep = "")))
    cat("\n")
    cat(noquote(paste('</option>', sep = "")))
    cat("\n")
    cat(noquote(paste('<option choix="', as.numeric(as.character(Tec_data$codedecisemis[kt])), '" nom="rules to prescribe the sowing date" nomParam="codedecisemis">', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="1" nom="yes">', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="integer" max="90" min="0" nom="nbjmaxapressemis">', as.numeric(as.character(Tec_data$nbjmaxapressemis[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="integer" max="60" min="1" nom="nbjseuiltempref">', as.numeric(as.character(Tec_data$nbjseuiltempref[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('</choix>', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="2" nom="no"/>', sep = "")))
    cat("\n")
    cat(noquote(paste('</option>', sep = "")))
    cat("\n")
    cat(noquote(paste('</formalisme>', sep = "")))
    cat("\n")
    cat(noquote(paste('<formalisme nom="phenological stages">', sep = "")))
    cat("\n")
    cat(noquote(paste('<option choix="', as.numeric(as.character(Tec_data$codestade[kt])), '" nom="forcing" nomParam="codestade">', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="1" nom="yes">', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="integer" max="999" min="1" nom="ilev">', as.numeric(as.character(Tec_data$ilev[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="integer" max="999" min="1" nom="iamf">', as.numeric(as.character(Tec_data$iamf[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="integer" max="999" min="1" nom="ilax">', as.numeric(as.character(Tec_data$ilax[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="integer" max="999" min="1" nom="isen">', as.numeric(as.character(Tec_data$isen[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="integer" max="999" min="1" nom="ilan">', as.numeric(as.character(Tec_data$ilan[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="integer" max="999" min="1" nom="iflo">', as.numeric(as.character(Tec_data$iflo[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="integer" max="999" min="1" nom="idrp">', as.numeric(as.character(Tec_data$idrp[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="integer" max="999" min="1" nom="imat">', as.numeric(as.character(Tec_data$imat[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="integer" max="999" min="1" nom="irec">', as.numeric(as.character(Tec_data$irec[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('</choix>', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="2" nom="no"/>', sep = "")))
    cat("\n")
    cat(noquote(paste('</option>', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="integer" max="999" min="1" nom="irecbutoir">', as.numeric(as.character(Tec_data$irecbutoir[kt])), '</param>', sep = "")))
    
    if (codejourdes == 1) {
      
      cat("\n")
      cat(noquote(paste('<param format="integer" max="999" min="1" nom="juldes">', as.numeric(as.character(Tec_data$juldes[kt])), '</param>', sep = "")))
      
    }
    
    cat("\n")
    cat(noquote(paste('</formalisme>', sep = "")))
    cat("\n")
    cat(noquote(paste('<formalisme nom="irrigation">', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="real" max="1.0" min="0.2" nom="effirr">', as.numeric(as.character(Tec_data$effirr[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('<option choix="', as.numeric(as.character(Tec_data$codecalirrig[kt])), '" nom="automatic calculation of irrigations" nomParam="codecalirrig">', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="1" nom="yes">', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="real" max="1.0" min="0.2" nom="ratiol">', as.numeric(as.character(Tec_data$ratiol[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="real" max="500.0" min="10.0" nom="dosimx">', as.numeric(as.character(Tec_data$dosimx[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="real" max="500.0" min="0.0" nom="doseirrigmin">', as.numeric(as.character(Tec_data$doseirrigmin[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('</choix>', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="2" nom="no">', sep = "")))
    cat("\n")
    cat(noquote(paste('<option choix="', as.numeric(as.character(Tec_data$codedateappH2O[kt])), '" nom="date of irrigation" nomParam="codedateappH2O">', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="1" nom="in sum of upvt"/>', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="2" nom="in julian days"/>', sep = "")))
    cat("\n")
    cat(noquote(paste('</option>', sep = "")))
    cat("\n")
    
    # getting irrigation number
    irr_str <- grep(pattern = "julapI_or_sum_upvt_[0-9]*$",names(Tec_data), value = T)
    irr_num <- as.numeric(gsub(pattern = "julapI_or_sum_upvt_","", irr_str))
    nb_irr <- length(irr_num)
    
    cat(noquote(paste("<ta nb_interventions=\"",as.character(nb_irr),"\" nom=\"water inputs\">", sep = "")))
    cat("\n")
    cat(noquote(paste('<ta_entete nb_colonnes="2">', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="julapI_or_sum_upvt"/>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="amount"/>', sep = "")))
    cat("\n")
    cat(noquote(paste('</ta_entete>', sep = "")))
    
    # writing irrigations blocs
    for (irr in sort(irr_num)) {
      #print(irr)
      irr_date <- eval(parse(text=paste0("Tec_data$julapI_or_sum_upvt_",as.character(irr),"[kt]")))
      irr_amount <- eval(parse(text=paste0("Tec_data$Amount_Irrig_",as.character(irr),"[kt]")))
      cat("\n")
      cat(noquote(paste('<intervention nb_colonnes="2">', sep = "")))
      cat("\n")
      cat(noquote(paste('<colonne nom="julapI_or_sum_upvt">', irr_date, '</colonne>', sep = "")))
      cat("\n")
      cat(noquote(paste('<colonne nom="amount">', irr_amount, '</colonne>', sep = "")))
      cat("\n")
      cat(noquote(paste('</intervention>', sep = "")))
    }
    
    
    
    
    
    # cat("\n")
    # cat(noquote(paste('<intervention nb_colonnes="2">', sep = "")))
    # cat("\n")
    # cat(noquote(paste('<colonne nom="julapI_or_sum_upvt">', Tec_data$julapI_or_sum_upvt_1[kt], '</colonne>', sep = "")))
    # cat("\n")
    # cat(noquote(paste('<colonne nom="amount">', Tec_data$Amount_Irrig_1[kt], '</colonne>', sep = "")))
    # cat("\n")
    # cat(noquote(paste('</intervention>', sep = "")))
    # cat("\n")
    # cat(noquote(paste('<intervention nb_colonnes="2">', sep = "")))
    # cat("\n")
    # cat(noquote(paste('<colonne nom="julapI_or_sum_upvt">', Tec_data$julapI_or_sum_upvt_2[kt], '</colonne>', sep = "")))
    # cat("\n")
    # cat(noquote(paste('<colonne nom="amount">', Tec_data$Amount_Irrig_2[kt], '</colonne>', sep = "")))
    # cat("\n")
    # cat(noquote(paste('</intervention>', sep = "")))
    # cat("\n")
    # cat(noquote(paste('<intervention nb_colonnes="2">', sep = "")))
    # cat("\n")
    # cat(noquote(paste('<colonne nom="julapI_or_sum_upvt">', Tec_data$julapI_or_sum_upvt_3[kt], '</colonne>', sep = "")))
    # cat("\n")
    # cat(noquote(paste('<colonne nom="amount">', Tec_data$Amount_Irrig_3[kt], '</colonne>', sep = "")))
    # cat("\n")
    # cat(noquote(paste('</intervention>', sep = "")))
    # cat("\n")
    # cat(noquote(paste('<intervention nb_colonnes="2">', sep = "")))
    # cat("\n")
    # cat(noquote(paste('<colonne nom="julapI_or_sum_upvt">', Tec_data$julapI_or_sum_upvt_4[kt], '</colonne>', sep = "")))
    # cat("\n")
    # cat(noquote(paste('<colonne nom="amount">', Tec_data$Amount_Irrig_4[kt], '</colonne>', sep = "")))
    # cat("\n")
    # cat(noquote(paste('</intervention>', sep = "")))
    # cat("\n")
    # cat(noquote(paste('<intervention nb_colonnes="2">', sep = "")))
    # cat("\n")
    # cat(noquote(paste('<colonne nom="julapI_or_sum_upvt">', Tec_data$julapI_or_sum_upvt_5[kt], '</colonne>', sep = "")))
    # cat("\n")
    # cat(noquote(paste('<colonne nom="amount">', Tec_data$Amount_Irrig_5[kt], '</colonne>', sep = "")))
    # cat("\n")
    # cat(noquote(paste('</intervention>', sep = "")))
    # cat("\n")
    # cat(noquote(paste('<intervention nb_colonnes="2">', sep = "")))
    # cat("\n")
    # cat(noquote(paste('<colonne nom="julapI_or_sum_upvt">', Tec_data$julapI_or_sum_upvt_6[kt], '</colonne>', sep = "")))
    # cat("\n")
    # cat(noquote(paste('<colonne nom="amount">', Tec_data$Amount_Irrig_6[kt], '</colonne>', sep = "")))
    # cat("\n")
    # cat(noquote(paste('</intervention>', sep = "")))
    # cat("\n")
    # cat(noquote(paste('<intervention nb_colonnes="2">', sep = "")))
    # cat("\n")
    # cat(noquote(paste('<colonne nom="julapI_or_sum_upvt">', Tec_data$julapI_or_sum_upvt_7[kt], '</colonne>', sep = "")))
    # cat("\n")
    # cat(noquote(paste('<colonne nom="amount">', Tec_data$Amount_Irrig_7[kt], '</colonne>', sep = "")))
    # cat("\n")
    # cat(noquote(paste('</intervention>', sep = "")))
    # cat("\n")
    # cat(noquote(paste('<intervention nb_colonnes="2">', sep = "")))
    # cat("\n")
    # cat(noquote(paste('<colonne nom="julapI_or_sum_upvt">', Tec_data$julapI_or_sum_upvt_8[kt], '</colonne>', sep = "")))
    # cat("\n")
    # cat(noquote(paste('<colonne nom="amount">', Tec_data$Amount_Irrig_8[kt], '</colonne>', sep = "")))
    # cat("\n")
    # cat(noquote(paste('</intervention>', sep = "")))
    # cat("\n")
    # cat(noquote(paste('<intervention nb_colonnes="2">', sep = "")))
    # cat("\n")
    # cat(noquote(paste('<colonne nom="julapI_or_sum_upvt">', Tec_data$julapI_or_sum_upvt_9[kt], '</colonne>', sep = "")))
    # cat("\n")
    # cat(noquote(paste('<colonne nom="amount">', Tec_data$Amount_Irrig_9[kt], '</colonne>', sep = "")))
    # cat("\n")
    # cat(noquote(paste('</intervention>', sep = "")))
    # cat("\n")
    # cat(noquote(paste('<intervention nb_colonnes="2">', sep = "")))
    # cat("\n")
    # cat(noquote(paste('<colonne nom="julapI_or_sum_upvt">', Tec_data$julapI_or_sum_upvt_10[kt], '</colonne>', sep = "")))
    # cat("\n")
    # cat(noquote(paste('<colonne nom="amount">', Tec_data$Amount_Irrig_10[kt], '</colonne>', sep = "")))
    # cat("\n")
    # cat(noquote(paste('</intervention>', sep = "")))
    # cat("\n")
    # cat(noquote(paste('<intervention nb_colonnes="2">', sep = "")))
    # cat("\n")
    # cat(noquote(paste('<colonne nom="julapI_or_sum_upvt">', Tec_data$julapI_or_sum_upvt_11[kt], '</colonne>', sep = "")))
    # cat("\n")
    # cat(noquote(paste('<colonne nom="amount">', Tec_data$Amount_Irrig_11[kt], '</colonne>', sep = "")))
    # cat("\n")
    # cat(noquote(paste('</intervention>', sep = "")))
    # cat("\n")
    # cat(noquote(paste('<intervention nb_colonnes="2">', sep = "")))
    # cat("\n")
    # cat(noquote(paste('<colonne nom="julapI_or_sum_upvt">', Tec_data$julapI_or_sum_upvt_12[kt], '</colonne>', sep = "")))
    # cat("\n")
    # cat(noquote(paste('<colonne nom="amount">', Tec_data$Amount_Irrig_12[kt], '</colonne>', sep = "")))
    # cat("\n")
    # cat(noquote(paste('</intervention>', sep = "")))
    # cat("\n")
    # cat(noquote(paste('<intervention nb_colonnes="2">', sep = "")))
    # cat("\n")
    # cat(noquote(paste('<colonne nom="julapI_or_sum_upvt">', Tec_data$julapI_or_sum_upvt_13[kt], '</colonne>', sep = "")))
    # cat("\n")
    # cat(noquote(paste('<colonne nom="amount">', Tec_data$Amount_Irrig_13[kt], '</colonne>', sep = "")))
    # cat("\n")
    # cat(noquote(paste('</intervention>', sep = "")))
    # cat("\n")
    # cat(noquote(paste('<intervention nb_colonnes="2">', sep = "")))
    # cat("\n")
    # cat(noquote(paste('<colonne nom="julapI_or_sum_upvt">', Tec_data$julapI_or_sum_upvt_14[kt], '</colonne>', sep = "")))
    # cat("\n")
    # cat(noquote(paste('<colonne nom="amount">', Tec_data$Amount_Irrig_14[kt], '</colonne>', sep = "")))
    # cat("\n")
    # cat(noquote(paste('</intervention>', sep = "")))
    # cat("\n")
    # cat(noquote(paste('<intervention nb_colonnes="2">', sep = "")))
    # cat("\n")
    # cat(noquote(paste('<colonne nom="julapI_or_sum_upvt">', Tec_data$julapI_or_sum_upvt_15[kt], '</colonne>', sep = "")))
    # cat("\n")
    # cat(noquote(paste('<colonne nom="amount">', Tec_data$Amount_Irrig_15[kt], '</colonne>', sep = "")))
    # cat("\n")
    # cat(noquote(paste('</intervention>', sep = "")))
    # cat("\n")
    # cat(noquote(paste('<intervention nb_colonnes="2">', sep = "")))
    # cat("\n")
    # cat(noquote(paste('<colonne nom="julapI_or_sum_upvt">', Tec_data$julapI_or_sum_upvt_16[kt], '</colonne>', sep = "")))
    # cat("\n")
    # cat(noquote(paste('<colonne nom="amount">', Tec_data$Amount_Irrig_16[kt], '</colonne>', sep = "")))
    # cat("\n")
    # cat(noquote(paste('</intervention>', sep = "")))
    # cat("\n")
    # cat(noquote(paste('<intervention nb_colonnes="2">', sep = "")))
    # cat("\n")
    # cat(noquote(paste('<colonne nom="julapI_or_sum_upvt">', Tec_data$julapI_or_sum_upvt_17[kt], '</colonne>', sep = "")))
    # cat("\n")
    # cat(noquote(paste('<colonne nom="amount">', Tec_data$Amount_Irrig_17[kt], '</colonne>', sep = "")))
    # cat("\n")
    # cat(noquote(paste('</intervention>', sep = "")))
    # cat("\n")
    # cat(noquote(paste('<intervention nb_colonnes="2">', sep = "")))
    # cat("\n")
    # cat(noquote(paste('<colonne nom="julapI_or_sum_upvt">', Tec_data$julapI_or_sum_upvt_18[kt], '</colonne>', sep = "")))
    # cat("\n")
    # cat(noquote(paste('<colonne nom="amount">', Tec_data$Amount_Irrig_18[kt], '</colonne>', sep = "")))
    # cat("\n")
    # cat(noquote(paste('</intervention>', sep = "")))
    # cat("\n")
    # cat(noquote(paste('<intervention nb_colonnes="2">', sep = "")))
    # cat("\n")
    # cat(noquote(paste('<colonne nom="julapI_or_sum_upvt">', Tec_data$julapI_or_sum_upvt_19[kt], '</colonne>', sep = "")))
    # cat("\n")
    # cat(noquote(paste('<colonne nom="amount">', Tec_data$Amount_Irrig_19[kt], '</colonne>', sep = "")))
    # cat("\n")
    # cat(noquote(paste('</intervention>', sep = "")))
    # cat("\n")
    # cat(noquote(paste('<intervention nb_colonnes="2">', sep = "")))
    # cat("\n")
    # cat(noquote(paste('<colonne nom="julapI_or_sum_upvt">', Tec_data$julapI_or_sum_upvt_20[kt], '</colonne>', sep = "")))
    # cat("\n")
    # cat(noquote(paste('<colonne nom="amount">', Tec_data$Amount_Irrig_20[kt], '</colonne>', sep = "")))
    # cat("\n")
    #cat(noquote(paste('</intervention>', sep = "")))
    cat("\n")
    cat(noquote(paste('</ta>', sep = "")))
    cat("\n")
    cat(noquote(paste('</choix>', sep = "")))
    cat("\n")
    cat(noquote(paste('</option>', sep = "")))
    cat("\n")
    cat(noquote(paste('<option choix="', as.numeric(as.character(Tec_data$codlocirrig[kt])), '" nom="location of irrigation" nomParam="codlocirrig">', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="1" nom="above foliage"/>', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="2" nom="under foliage"/>', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="3" nom="in the soil">', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="integer" max="30" min="0" nom="locirrig">', as.numeric(as.character(Tec_data$locirrig[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('</choix>', sep = "")))
    cat("\n")
    cat(noquote(paste('</option>', sep = "")))
    
    
    
    cat("\n")
    cat(noquote(paste('<param format="real" max="1000.0" min="10.0" nom="profmes">', as.numeric(as.character(Tec_data$profmes[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('</formalisme>', sep = "")))
    cat("\n")
    cat(noquote(paste('<formalisme nom="fertilisation">', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="integer" max="8" min="0" nom="engrais">', as.numeric(as.character(Tec_data$engrais[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="real" max="0.2" min="0.0" nom="concirr">', as.numeric(as.character(Tec_data$concirr[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('<option choix="', as.numeric(as.character(Tec_data$codedateappN[kt])), '" nom="date of fertilisation" nomParam="codedateappN">', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="1" nom="sum of upvt"/>', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="2" nom="julian days"/>', sep = "")))
    cat("\n")
    cat(noquote(paste('</option>', sep = "")))
    cat("\n")
    cat(noquote(paste('<option choix="', as.numeric(as.character(Tec_data$codefracappN[kt])), '" nom="splitting fertilisation" nomParam="codefracappN">', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="1" nom="amounts in absolute value"/>', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="2" nom="amounts in % of the total value">', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="integer" max="200" min="0" nom="Qtot_N">', as.numeric(as.character(Tec_data$Qtot_N[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('</choix>', sep = "")))
    cat("\n")
    cat(noquote(paste('</option>', sep = "")))
    cat("\n")
    cat(noquote(paste('<ta nb_interventions="8" nom="mineral nitrogen inputs">', sep = "")))
    cat("\n")
    if (option_engrais_multiple == 1) {
      cat(noquote(paste('<ta_entete nb_colonnes="3">', sep = "")))
    } else {
      cat(noquote(paste('<ta_entete nb_colonnes="2">', sep = "")))
    }
    cat("\n")
    cat(noquote(paste('<colonne nom="julapN_or_sum_upvt"/>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="absolute_value/%"/>', sep = "")))
    if (option_engrais_multiple == 1) {
      cat("\n")
      cat(noquote(paste('<colonne nom="engrais"/>', sep = "")))
    }
    cat("\n")
    cat(noquote(paste('</ta_entete>', sep = "")))
    
    cat("\n")
    if (option_engrais_multiple == 1) {
      cat(noquote(paste('<intervention nb_colonnes="3">', sep = "")))
    } else {
      cat(noquote(paste('<intervention nb_colonnes="2">', sep = "")))
    }
    cat("\n")
    cat(noquote(paste('<colonne nom="julapN_or_sum_upvt">', as.numeric(as.character(Tec_data$julapN_or_sum_upvt_1[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="absolute_value/%">', as.numeric(as.character(Tec_data$Amount_N_Ferti_1[kt])), '</colonne>', sep = "")))
    if (option_engrais_multiple == 1) {
      cat("\n")
      cat(noquote(paste('<colonne nom="engrais">', as.numeric(as.character(Tec_data$engrais_1[kt])), '</colonne>', sep = "")))
    }
    cat("\n")
    cat(noquote(paste('</intervention>', sep = "")))
    
    cat("\n")
    if (option_engrais_multiple == 1) {
      cat(noquote(paste('<intervention nb_colonnes="3">', sep = "")))
    } else {
      cat(noquote(paste('<intervention nb_colonnes="2">', sep = "")))
    }
    cat("\n")
    cat(noquote(paste('<colonne nom="julapN_or_sum_upvt">', as.numeric(as.character(Tec_data$julapN_or_sum_upvt_2[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="absolute_value/%">', as.numeric(as.character(Tec_data$Amount_N_Ferti_2[kt])), '</colonne>', sep = "")))
    if (option_engrais_multiple == 1) {
      cat("\n")
      cat(noquote(paste('<colonne nom="engrais">', as.numeric(as.character(Tec_data$engrais_2[kt])), '</colonne>', sep = "")))
    }
    cat("\n")
    cat(noquote(paste('</intervention>', sep = "")))
    
    cat("\n")
    if (option_engrais_multiple == 1) {
      cat(noquote(paste('<intervention nb_colonnes="3">', sep = "")))
    } else {
      cat(noquote(paste('<intervention nb_colonnes="2">', sep = "")))
    }
    cat("\n")
    cat(noquote(paste('<colonne nom="julapN_or_sum_upvt">', as.numeric(as.character(Tec_data$julapN_or_sum_upvt_3[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="absolute_value/%">', as.numeric(as.character(Tec_data$Amount_N_Ferti_3[kt])), '</colonne>', sep = "")))
    if (option_engrais_multiple == 1) {
      cat("\n")
      cat(noquote(paste('<colonne nom="engrais">', as.numeric(as.character(Tec_data$engrais_3[kt])), '</colonne>', sep = "")))
    }
    cat("\n")
    cat(noquote(paste('</intervention>', sep = "")))
    
    cat("\n")
    if (option_engrais_multiple == 1) {
      cat(noquote(paste('<intervention nb_colonnes="3">', sep = "")))
    } else {
      cat(noquote(paste('<intervention nb_colonnes="2">', sep = "")))
    }
    cat("\n")
    cat(noquote(paste('<colonne nom="julapN_or_sum_upvt">', as.numeric(as.character(Tec_data$julapN_or_sum_upvt_4[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="absolute_value/%">', as.numeric(as.character(Tec_data$Amount_N_Ferti_4[kt])), '</colonne>', sep = "")))
    if (option_engrais_multiple == 1) {
      cat("\n")
      cat(noquote(paste('<colonne nom="engrais">', as.numeric(as.character(Tec_data$engrais_4[kt])), '</colonne>', sep = "")))
    }
    cat("\n")
    cat(noquote(paste('</intervention>', sep = "")))
    
    cat("\n")
    if (option_engrais_multiple == 1) {
      cat(noquote(paste('<intervention nb_colonnes="3">', sep = "")))
    } else {
      cat(noquote(paste('<intervention nb_colonnes="2">', sep = "")))
    }
    cat("\n")
    cat(noquote(paste('<colonne nom="julapN_or_sum_upvt">', as.numeric(as.character(Tec_data$julapN_or_sum_upvt_5[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="absolute_value/%">', as.numeric(as.character(Tec_data$Amount_N_Ferti_5[kt])), '</colonne>', sep = "")))
    if (option_engrais_multiple == 1) {
      cat("\n")
      cat(noquote(paste('<colonne nom="engrais">', as.numeric(as.character(Tec_data$engrais_5[kt])), '</colonne>', sep = "")))
    }
    cat("\n")
    cat(noquote(paste('</intervention>', sep = "")))
    
    cat("\n")
    if (option_engrais_multiple == 1) {
      cat(noquote(paste('<intervention nb_colonnes="3">', sep = "")))
    } else {
      cat(noquote(paste('<intervention nb_colonnes="2">', sep = "")))
    }
    cat("\n")
    cat(noquote(paste('<colonne nom="julapN_or_sum_upvt">', as.numeric(as.character(Tec_data$julapN_or_sum_upvt_6[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="absolute_value/%">', as.numeric(as.character(Tec_data$Amount_N_Ferti_6[kt])), '</colonne>', sep = "")))
    if (option_engrais_multiple == 1) {
      cat("\n")
      cat(noquote(paste('<colonne nom="engrais">', as.numeric(as.character(Tec_data$engrais_6[kt])), '</colonne>', sep = "")))
    }
    cat("\n")
    cat(noquote(paste('</intervention>', sep = "")))
    
    cat("\n")
    if (option_engrais_multiple == 1) {
      cat(noquote(paste('<intervention nb_colonnes="3">', sep = "")))
    } else {
      cat(noquote(paste('<intervention nb_colonnes="2">', sep = "")))
    }
    cat("\n")
    cat(noquote(paste('<colonne nom="julapN_or_sum_upvt">', as.numeric(as.character(Tec_data$julapN_or_sum_upvt_7[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="absolute_value/%">', as.numeric(as.character(Tec_data$Amount_N_Ferti_7[kt])), '</colonne>', sep = "")))
    if (option_engrais_multiple == 1) {
      cat("\n")
      cat(noquote(paste('<colonne nom="engrais">', as.numeric(as.character(Tec_data$engrais_7[kt])), '</colonne>', sep = "")))
    }
    cat("\n")
    cat(noquote(paste('</intervention>', sep = "")))
    
    cat("\n")
    if (option_engrais_multiple == 1) {
      cat(noquote(paste('<intervention nb_colonnes="3">', sep = "")))
    } else {
      cat(noquote(paste('<intervention nb_colonnes="2">', sep = "")))
    }
    cat("\n")
    cat(noquote(paste('<colonne nom="julapN_or_sum_upvt">', as.numeric(as.character(Tec_data$julapN_or_sum_upvt_8[kt])), '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="absolute_value/%">', as.numeric(as.character(Tec_data$Amount_N_Ferti_8[kt])), '</colonne>', sep = "")))
    if (option_engrais_multiple == 1) {
      cat("\n")
      cat(noquote(paste('<colonne nom="engrais">', as.numeric(as.character(Tec_data$engrais_8[kt])), '</colonne>', sep = "")))
    }
    cat("\n")
    cat(noquote(paste('</intervention>', sep = "")))
    cat("\n")
    cat(noquote(paste('</ta>', sep = "")))
    cat("\n")
    cat(noquote(paste('<option choix="', as.numeric(as.character(Tec_data$codlocferti[kt])), '" nom="location of mineral nitrogen inputs" nomParam="codlocferti">', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="1" nom="at soil surface"/>', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="2" nom="below soil surface">', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="integer" max="30" min="0" nom="locferti">', as.numeric(as.character(Tec_data$locferti[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('</choix>', sep = "")))
    cat("\n")
    cat(noquote(paste('</option>', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="character" nom="ressuite">', as.character(Tec_data$ressuite[kt]), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('</formalisme>', sep = "")))
    cat("\n")
    cat(noquote(paste('<formalisme nom="harvest">', sep = "")))
    cat("\n")
    cat(noquote(paste('<option choix="', as.numeric(as.character(Tec_data$codcueille[kt])), '" nom="method of harvest" nomParam="codcueille">', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="1" nom="cutting"/>', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="2" nom="picking">', sep = "")))
    cat("\n")
    cat(noquote(paste('<option choix="', as.numeric(as.character(Tec_data$nbcueille[kt])), '" nom="number of pickings" nomParam="nbcueille">', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="1" nom="one at the end"/>', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="2" nom="many during the cycle">', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="integer" max="30" min="1" nom="cadencerec">', as.numeric(as.character(Tec_data$cadencerec[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('</choix>', sep = "")))
    cat("\n")
    cat(noquote(paste('</option>', sep = "")))
    cat("\n")
    cat(noquote(paste('</choix>', sep = "")))
    cat("\n")
    cat(noquote(paste('</option>', sep = "")))
    cat("\n")
    cat(noquote(paste('<option choix="', as.numeric(as.character(Tec_data$codrecolte[kt])), '" nom="harvest decision" nomParam="codrecolte">', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="1" nom="physiological maturity"/>', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="2" nom="water content">', sep = "")))
    cat("\n")
    cat(noquote(paste('<option choix="', as.numeric(as.character(Tec_data$codeaumin[kt])), '" nom="minimum.or.maximum" nomParam="codeaumin">', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="1" nom="minimum">', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="real" max="1.0" min="0.05" nom="h2ograinmin">', as.numeric(as.character(Tec_data$h2ograinmin[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('</choix>', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="2" nom="maximum">', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="real" max="1.0" min="0.05" nom="h2ograinmax">', as.numeric(as.character(Tec_data$h2ograinmax[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('</choix>', sep = "")))
    cat("\n")
    cat(noquote(paste('</option>', sep = "")))
    cat("\n")
    cat(noquote(paste('</choix>', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="3" nom="sugar content">', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="real" max="0.5" min="1.0E-4" nom="sucrerec">', as.numeric(as.character(Tec_data$sucrerec[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('</choix>', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="4" nom="nitrogen content">', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="real" max="1.0" min="0.0" nom="CNgrainrec">', as.numeric(as.character(Tec_data$CNgrainrec[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('</choix>', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="5" nom="oil content">', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="real" max="1.0" min="0.1" nom="huilerec">', as.numeric(as.character(Tec_data$huilerec[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('</choix>', sep = "")))
    cat("\n")
    cat(noquote(paste('</option>', sep = "")))
    cat("\n")
    cat(noquote(paste('<option choix="', as.numeric(as.character(Tec_data$coderecolteassoc[kt])), '" nom="Decision of harvest for associated crops" nomParam="coderecolteassoc">', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="1" nom="Maturity of the earliest"/>', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="2" nom="Maturity of the both (2 dates)"/>', sep = "")))
    cat("\n")
    cat(noquote(paste('</option>', sep = "")))
    cat("\n")
    cat(noquote(paste('<option choix="', as.numeric(as.character(Tec_data$codedecirecolte[kt])), '" nom="rules of harvest/moisture status of the soil" nomParam="codedecirecolte">', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="1" nom="yes">', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="integer" max="90" min="0" nom="nbjmaxapresrecolte">', as.numeric(as.character(Tec_data$nbjmaxapresrecolte[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('</choix>', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="2" nom="no"/>', sep = "")))
    cat("\n")
    cat(noquote(paste('</option>', sep = "")))
    cat("\n")
    cat(noquote(paste('</formalisme>', sep = "")))
    cat("\n")
    cat(noquote(paste('<formalisme nom="special techniques">', sep = "")))
    cat("\n")
    cat(noquote(paste('<option choix="', as.numeric(as.character(Tec_data$codefauche[kt])), '" nom="cut crop" nomParam="codefauche">', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="1" nom="yes">', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="real" max="10.0" min="1.0" nom="mscoupemini">', as.numeric(as.character(Tec_data$mscoupemini[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('<option choix="', as.numeric(as.character(Tec_data$codemodfauche[kt])), '" nom="Method.of.cutting" nomParam="codemodfauche">', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="1" nom="automatic">', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="real" max="0.5" min="0.01" nom="hautcoupedefaut">', as.numeric(as.character(Tec_data$hautcoupedefaut[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="character" nom="stadecoupedf">', as.character(Tec_data$stadecoupedf[kt]), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('</choix>', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="2" nom="calendar in days">', sep = "")))
    cat("\n")
    cat(noquote(paste('<ta nb_interventions="10" nom="cutting management">', sep = "")))
    cat("\n")
    if (codetauxexportfauche == 1) {
      cat(noquote(paste('<ta_entete nb_colonnes="6">', sep = "")))
    } else {
      cat(noquote(paste('<ta_entete nb_colonnes="5">', sep = "")))
    }
    cat("\n")
    cat(noquote(paste('<colonne nom="julfauche"/>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="hautcoupe"/>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="lairesiduel"/>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="msresiduel"/>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="anitcoupe"/>', sep = "")))
    if (codetauxexportfauche == 1) {
      cat("\n")
      cat(noquote(paste('<colonne nom="tauxexportfauche"/>', sep = "")))
    }
    cat("\n")
    cat(noquote(paste('</ta_entete>', sep = "")))
    
    cat("\n")
    if (codetauxexportfauche == 1) {
      cat(noquote(paste('<intervention nb_colonnes="6">', sep = "")))
    } else {
      cat(noquote(paste('<intervention nb_colonnes="5">', sep = "")))
    }
    cat("\n")
    cat(noquote(paste('<colonne nom="julfauche">', Tec_data$julfauche_1[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="hautcoupe">', Tec_data$hautcoupe_1[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="lairesiduel">', Tec_data$lairesiduel_1[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="msresiduel">', Tec_data$msresiduel_1[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="anitcoupe">', Tec_data$anitcoupe_1[kt], '</colonne>', sep = "")))
    if (codetauxexportfauche == 1) {
      cat("\n")
      cat(noquote(paste('<colonne nom="tauxexportfauche">', Tec_data$tauxexportfauche_1[kt], '</colonne>', sep = "")))
    }
    cat("\n")
    cat(noquote(paste('</intervention>', sep = "")))
    
    cat("\n")
    if (codetauxexportfauche == 1) {
      cat(noquote(paste('<intervention nb_colonnes="6">', sep = "")))
    } else {
      cat(noquote(paste('<intervention nb_colonnes="5">', sep = "")))
    }
    cat("\n")
    cat(noquote(paste('<colonne nom="julfauche">', Tec_data$julfauche_2[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="hautcoupe">', Tec_data$hautcoupe_2[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="lairesiduel">', Tec_data$lairesiduel_2[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="msresiduel">', Tec_data$msresiduel_2[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="anitcoupe">', Tec_data$anitcoupe_2[kt], '</colonne>', sep = "")))
    if (codetauxexportfauche == 1) {
      cat("\n")
      cat(noquote(paste('<colonne nom="tauxexportfauche">', Tec_data$tauxexportfauche_2[kt], '</colonne>', sep = "")))
    }
    cat("\n")
    cat(noquote(paste('</intervention>', sep = "")))
    
    cat("\n")
    if (codetauxexportfauche == 1) {
      cat(noquote(paste('<intervention nb_colonnes="6">', sep = "")))
    } else {
      cat(noquote(paste('<intervention nb_colonnes="5">', sep = "")))
    }
    cat("\n")
    cat(noquote(paste('<colonne nom="julfauche">', Tec_data$julfauche_3[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="hautcoupe">', Tec_data$hautcoupe_3[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="lairesiduel">', Tec_data$lairesiduel_3[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="msresiduel">', Tec_data$msresiduel_3[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="anitcoupe">', Tec_data$anitcoupe_3[kt], '</colonne>', sep = "")))
    if (codetauxexportfauche == 1) {
      cat("\n")
      cat(noquote(paste('<colonne nom="tauxexportfauche">', Tec_data$tauxexportfauche_3[kt], '</colonne>', sep = "")))
    }
    cat("\n")
    cat(noquote(paste('</intervention>', sep = "")))
    
    cat("\n")
    if (codetauxexportfauche == 1) {
      cat(noquote(paste('<intervention nb_colonnes="6">', sep = "")))
    } else {
      cat(noquote(paste('<intervention nb_colonnes="5">', sep = "")))
    }
    cat("\n")
    cat(noquote(paste('<colonne nom="julfauche">', Tec_data$julfauche_4[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="hautcoupe">', Tec_data$hautcoupe_4[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="lairesiduel">', Tec_data$lairesiduel_4[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="msresiduel">', Tec_data$msresiduel_4[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="anitcoupe">', Tec_data$anitcoupe_4[kt], '</colonne>', sep = "")))
    if (codetauxexportfauche == 1) {
      cat("\n")
      cat(noquote(paste('<colonne nom="tauxexportfauche">', Tec_data$tauxexportfauche_4[kt], '</colonne>', sep = "")))
    }
    cat("\n")
    cat(noquote(paste('</intervention>', sep = "")))
    
    cat("\n")
    if (codetauxexportfauche == 1) {
      cat(noquote(paste('<intervention nb_colonnes="6">', sep = "")))
    } else {
      cat(noquote(paste('<intervention nb_colonnes="5">', sep = "")))
    }
    cat("\n")
    cat(noquote(paste('<colonne nom="julfauche">', Tec_data$julfauche_5[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="hautcoupe">', Tec_data$hautcoupe_5[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="lairesiduel">', Tec_data$lairesiduel_5[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="msresiduel">', Tec_data$msresiduel_5[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="anitcoupe">', Tec_data$anitcoupe_5[kt], '</colonne>', sep = "")))
    if (codetauxexportfauche == 1) {
      cat("\n")
      cat(noquote(paste('<colonne nom="tauxexportfauche">', Tec_data$tauxexportfauche_5[kt], '</colonne>', sep = "")))
    }
    cat("\n")
    cat(noquote(paste('</intervention>', sep = "")))
    
    cat("\n")
    if (codetauxexportfauche == 1) {
      cat(noquote(paste('<intervention nb_colonnes="6">', sep = "")))
    } else {
      cat(noquote(paste('<intervention nb_colonnes="5">', sep = "")))
    }
    cat("\n")
    cat(noquote(paste('<colonne nom="julfauche">', Tec_data$julfauche_6[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="hautcoupe">', Tec_data$hautcoupe_6[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="lairesiduel">', Tec_data$lairesiduel_6[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="msresiduel">', Tec_data$msresiduel_6[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="anitcoupe">', Tec_data$anitcoupe_6[kt], '</colonne>', sep = "")))
    if (codetauxexportfauche == 1) {
      cat("\n")
      cat(noquote(paste('<colonne nom="tauxexportfauche">', Tec_data$tauxexportfauche_6[kt], '</colonne>', sep = "")))
    }
    cat("\n")
    cat(noquote(paste('</intervention>', sep = "")))
    
    cat("\n")
    if (codetauxexportfauche == 1) {
      cat(noquote(paste('<intervention nb_colonnes="6">', sep = "")))
    } else {
      cat(noquote(paste('<intervention nb_colonnes="5">', sep = "")))
    }
    cat("\n")
    cat(noquote(paste('<colonne nom="julfauche">', Tec_data$julfauche_7[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="hautcoupe">', Tec_data$hautcoupe_7[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="lairesiduel">', Tec_data$lairesiduel_7[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="msresiduel">', Tec_data$msresiduel_7[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="anitcoupe">', Tec_data$anitcoupe_7[kt], '</colonne>', sep = "")))
    if (codetauxexportfauche == 1) {
      cat("\n")
      cat(noquote(paste('<colonne nom="tauxexportfauche">', Tec_data$tauxexportfauche_7[kt], '</colonne>', sep = "")))
    }
    cat("\n")
    cat(noquote(paste('</intervention>', sep = "")))
    
    cat("\n")
    if (codetauxexportfauche == 1) {
      cat(noquote(paste('<intervention nb_colonnes="6">', sep = "")))
    } else {
      cat(noquote(paste('<intervention nb_colonnes="5">', sep = "")))
    }
    cat("\n")
    cat(noquote(paste('<colonne nom="julfauche">', Tec_data$julfauche_8[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="hautcoupe">', Tec_data$hautcoupe_8[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="lairesiduel">', Tec_data$lairesiduel_8[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="msresiduel">', Tec_data$msresiduel_8[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="anitcoupe">', Tec_data$anitcoupe_8[kt], '</colonne>', sep = "")))
    if (codetauxexportfauche == 1) {
      cat("\n")
      cat(noquote(paste('<colonne nom="tauxexportfauche">', Tec_data$tauxexportfauche_8[kt], '</colonne>', sep = "")))
    }
    cat("\n")
    cat(noquote(paste('</intervention>', sep = "")))
    
    cat("\n")
    if (codetauxexportfauche == 1) {
      cat(noquote(paste('<intervention nb_colonnes="6">', sep = "")))
    } else {
      cat(noquote(paste('<intervention nb_colonnes="5">', sep = "")))
    }
    cat("\n")
    cat(noquote(paste('<colonne nom="julfauche">', Tec_data$julfauche_9[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="hautcoupe">', Tec_data$hautcoupe_9[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="lairesiduel">', Tec_data$lairesiduel_9[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="msresiduel">', Tec_data$msresiduel_9[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="anitcoupe">', Tec_data$anitcoupe_9[kt], '</colonne>', sep = "")))
    if (codetauxexportfauche == 1) {
      cat("\n")
      cat(noquote(paste('<colonne nom="tauxexportfauche">', Tec_data$tauxexportfauche_9[kt], '</colonne>', sep = "")))
    }
    cat("\n")
    cat(noquote(paste('</intervention>', sep = "")))
    
    cat("\n")
    if (codetauxexportfauche == 1) {
      cat(noquote(paste('<intervention nb_colonnes="6">', sep = "")))
    } else {
      cat(noquote(paste('<intervention nb_colonnes="5">', sep = "")))
    }
    cat("\n")
    cat(noquote(paste('<colonne nom="julfauche">', Tec_data$julfauche_10[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="hautcoupe">', Tec_data$hautcoupe_10[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="lairesiduel">', Tec_data$lairesiduel_10[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="msresiduel">', Tec_data$msresiduel_10[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="anitcoupe">', Tec_data$anitcoupe_10[kt], '</colonne>', sep = "")))
    if (codetauxexportfauche == 1) {
      cat("\n")
      cat(noquote(paste('<colonne nom="tauxexportfauche">', Tec_data$tauxexportfauche_10[kt], '</colonne>', sep = "")))
    }
    cat("\n")
    cat(noquote(paste('</intervention>', sep = "")))
    
    cat("\n")
    cat(noquote(paste('</ta>', sep = "")))
    cat("\n")
    cat(noquote(paste('</choix>', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="3" nom="calendar in degree days">', sep = "")))
    cat("\n")
    cat(noquote(paste('<ta nb_interventions="10" nom="cutting management">', sep = "")))
    cat("\n")
    if (codetauxexportfauche == 1) {
      cat(noquote(paste('<ta_entete nb_colonnes="6">', sep = "")))
    } else {
      cat(noquote(paste('<ta_entete nb_colonnes="5">', sep = "")))
    }
    cat("\n")
    cat(noquote(paste('<colonne nom="tempfauche"/>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="hautcoupe"/>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="lairesiduel"/>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="msresiduel"/>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="anitcoupe"/>', sep = "")))
    if (codetauxexportfauche == 1) {
      cat("\n")
      cat(noquote(paste('<colonne nom="tauxexportfauche"/>', sep = "")))
    }
    cat("\n")
    cat(noquote(paste('</ta_entete>', sep = "")))
    
    cat("\n")
    if (codetauxexportfauche == 1) {
      cat(noquote(paste('<intervention nb_colonnes="6">', sep = "")))
    } else {
      cat(noquote(paste('<intervention nb_colonnes="5">', sep = "")))
    }
    cat("\n")
    cat(noquote(paste('<colonne nom="tempfauche">', Tec_data$tempfauche_1[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="hautcoupe">', Tec_data$hautcoupe_1[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="lairesiduel">', Tec_data$lairesiduel_1[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="msresiduel">', Tec_data$msresiduel_1[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="anitcoupe">', Tec_data$anitcoupe_1[kt], '</colonne>', sep = "")))
    if (codetauxexportfauche == 1) {
      cat("\n")
      cat(noquote(paste('<colonne nom="tauxexportfauche">', Tec_data$tauxexportfauche_1[kt], '</colonne>', sep = "")))
    }
    cat("\n")
    cat(noquote(paste('</intervention>', sep = "")))
    
    cat("\n")
    if (codetauxexportfauche == 1) {
      cat(noquote(paste('<intervention nb_colonnes="6">', sep = "")))
    } else {
      cat(noquote(paste('<intervention nb_colonnes="5">', sep = "")))
    }
    cat("\n")
    cat(noquote(paste('<colonne nom="tempfauche">', Tec_data$tempfauche_2[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="hautcoupe">', Tec_data$hautcoupe_2[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="lairesiduel">', Tec_data$lairesiduel_2[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="msresiduel">', Tec_data$msresiduel_2[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="anitcoupe">', Tec_data$anitcoupe_2[kt], '</colonne>', sep = "")))
    if (codetauxexportfauche == 1) {
      cat("\n")
      cat(noquote(paste('<colonne nom="tauxexportfauche">', Tec_data$tauxexportfauche_2[kt], '</colonne>', sep = "")))
    }
    cat("\n")
    cat(noquote(paste('</intervention>', sep = "")))
    
    cat("\n")
    if (codetauxexportfauche == 1) {
      cat(noquote(paste('<intervention nb_colonnes="6">', sep = "")))
    } else {
      cat(noquote(paste('<intervention nb_colonnes="5">', sep = "")))
    }
    cat("\n")
    cat(noquote(paste('<colonne nom="tempfauche">', Tec_data$tempfauche_3[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="hautcoupe">', Tec_data$hautcoupe_3[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="lairesiduel">', Tec_data$lairesiduel_3[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="msresiduel">', Tec_data$msresiduel_3[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="anitcoupe">', Tec_data$anitcoupe_3[kt], '</colonne>', sep = "")))
    if (codetauxexportfauche == 1) {
      cat("\n")
      cat(noquote(paste('<colonne nom="tauxexportfauche">', Tec_data$tauxexportfauche_3[kt], '</colonne>', sep = "")))
    }
    cat("\n")
    cat(noquote(paste('</intervention>', sep = "")))
    
    cat("\n")
    if (codetauxexportfauche == 1) {
      cat(noquote(paste('<intervention nb_colonnes="6">', sep = "")))
    } else {
      cat(noquote(paste('<intervention nb_colonnes="5">', sep = "")))
    }
    cat("\n")
    cat(noquote(paste('<colonne nom="tempfauche">', Tec_data$tempfauche_4[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="hautcoupe">', Tec_data$hautcoupe_4[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="lairesiduel">', Tec_data$lairesiduel_4[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="msresiduel">', Tec_data$msresiduel_4[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="anitcoupe">', Tec_data$anitcoupe_4[kt], '</colonne>', sep = "")))
    if (codetauxexportfauche == 1) {
      cat("\n")
      cat(noquote(paste('<colonne nom="tauxexportfauche">', Tec_data$tauxexportfauche_4[kt], '</colonne>', sep = "")))
    }
    cat("\n")
    cat(noquote(paste('</intervention>', sep = "")))
    
    cat("\n")
    if (codetauxexportfauche == 1) {
      cat(noquote(paste('<intervention nb_colonnes="6">', sep = "")))
    } else {
      cat(noquote(paste('<intervention nb_colonnes="5">', sep = "")))
    }
    cat("\n")
    cat(noquote(paste('<colonne nom="tempfauche">', Tec_data$tempfauche_5[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="hautcoupe">', Tec_data$hautcoupe_5[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="lairesiduel">', Tec_data$lairesiduel_5[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="msresiduel">', Tec_data$msresiduel_5[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="anitcoupe">', Tec_data$anitcoupe_5[kt], '</colonne>', sep = "")))
    if (codetauxexportfauche == 1) {
      cat("\n")
      cat(noquote(paste('<colonne nom="tauxexportfauche">', Tec_data$tauxexportfauche_5[kt], '</colonne>', sep = "")))
    }
    cat("\n")
    cat(noquote(paste('</intervention>', sep = "")))
    
    cat("\n")
    if (codetauxexportfauche == 1) {
      cat(noquote(paste('<intervention nb_colonnes="6">', sep = "")))
    } else {
      cat(noquote(paste('<intervention nb_colonnes="5">', sep = "")))
    }
    cat("\n")
    cat(noquote(paste('<colonne nom="tempfauche">', Tec_data$tempfauche_6[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="hautcoupe">', Tec_data$hautcoupe_6[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="lairesiduel">', Tec_data$lairesiduel_6[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="msresiduel">', Tec_data$msresiduel_6[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="anitcoupe">', Tec_data$anitcoupe_6[kt], '</colonne>', sep = "")))
    if (codetauxexportfauche == 1) {
      cat("\n")
      cat(noquote(paste('<colonne nom="tauxexportfauche">', Tec_data$tauxexportfauche_6[kt], '</colonne>', sep = "")))
    }
    cat("\n")
    cat(noquote(paste('</intervention>', sep = "")))
    
    cat("\n")
    if (codetauxexportfauche == 1) {
      cat(noquote(paste('<intervention nb_colonnes="6">', sep = "")))
    } else {
      cat(noquote(paste('<intervention nb_colonnes="5">', sep = "")))
    }
    cat("\n")
    cat(noquote(paste('<colonne nom="tempfauche">', Tec_data$tempfauche_7[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="hautcoupe">', Tec_data$hautcoupe_7[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="lairesiduel">', Tec_data$lairesiduel_7[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="msresiduel">', Tec_data$msresiduel_7[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="anitcoupe">', Tec_data$anitcoupe_7[kt], '</colonne>', sep = "")))
    if (codetauxexportfauche == 1) {
      cat("\n")
      cat(noquote(paste('<colonne nom="tauxexportfauche">', Tec_data$tauxexportfauche_7[kt], '</colonne>', sep = "")))
    }
    cat("\n")
    cat(noquote(paste('</intervention>', sep = "")))
    
    cat("\n")
    if (codetauxexportfauche == 1) {
      cat(noquote(paste('<intervention nb_colonnes="6">', sep = "")))
    } else {
      cat(noquote(paste('<intervention nb_colonnes="5">', sep = "")))
    }
    cat("\n")
    cat(noquote(paste('<colonne nom="tempfauche">', Tec_data$tempfauche_8[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="hautcoupe">', Tec_data$hautcoupe_8[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="lairesiduel">', Tec_data$lairesiduel_8[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="msresiduel">', Tec_data$msresiduel_8[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="anitcoupe">', Tec_data$anitcoupe_8[kt], '</colonne>', sep = "")))
    if (codetauxexportfauche == 1) {
      cat("\n")
      cat(noquote(paste('<colonne nom="tauxexportfauche">', Tec_data$tauxexportfauche_8[kt], '</colonne>', sep = "")))
    }
    cat("\n")
    cat(noquote(paste('</intervention>', sep = "")))
    
    cat("\n")
    if (codetauxexportfauche == 1) {
      cat(noquote(paste('<intervention nb_colonnes="6">', sep = "")))
    } else {
      cat(noquote(paste('<intervention nb_colonnes="5">', sep = "")))
    }
    cat("\n")
    cat(noquote(paste('<colonne nom="tempfauche">', Tec_data$tempfauche_9[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="hautcoupe">', Tec_data$hautcoupe_9[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="lairesiduel">', Tec_data$lairesiduel_9[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="msresiduel">', Tec_data$msresiduel_9[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="anitcoupe">', Tec_data$anitcoupe_9[kt], '</colonne>', sep = "")))
    if (codetauxexportfauche == 1) {
      cat("\n")
      cat(noquote(paste('<colonne nom="tauxexportfauche">', Tec_data$tauxexportfauche_9[kt], '</colonne>', sep = "")))
    }
    cat("\n")
    cat(noquote(paste('</intervention>', sep = "")))
    
    cat("\n")
    if (codetauxexportfauche == 1) {
      cat(noquote(paste('<intervention nb_colonnes="6">', sep = "")))
    } else {
      cat(noquote(paste('<intervention nb_colonnes="5">', sep = "")))
    }
    cat("\n")
    cat(noquote(paste('<colonne nom="tempfauche">', Tec_data$tempfauche_10[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="hautcoupe">', Tec_data$hautcoupe_10[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="lairesiduel">', Tec_data$lairesiduel_10[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="msresiduel">', Tec_data$msresiduel_10[kt], '</colonne>', sep = "")))
    cat("\n")
    cat(noquote(paste('<colonne nom="anitcoupe">', Tec_data$anitcoupe_10[kt], '</colonne>', sep = "")))
    if (codetauxexportfauche == 1) {
      cat("\n")
      cat(noquote(paste('<colonne nom="tauxexportfauche">', Tec_data$tauxexportfauche_10[kt], '</colonne>', sep = "")))
    }
    cat("\n")
    cat(noquote(paste('</intervention>', sep = "")))
    
    cat(noquote(paste('</ta>', sep = "")))
    cat("\n")
    cat(noquote(paste('</choix>', sep = "")))
    cat("\n")
    cat(noquote(paste('</option>', sep = "")))
    cat("\n")
    cat(noquote(paste('</choix>', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="2" nom="no"/>', sep = "")))
    cat("\n")
    cat(noquote(paste('</option>', sep = "")))
    cat("\n")
    cat(noquote(paste('<option choix="', as.numeric(as.character(Tec_data$codepaillage[kt])), '" nom="mulch" nomParam="codepaillage">', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="1" nom="no"/>', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="2" nom="plastic mulch">', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="real" max="1.0" min="0.0" nom="couvermulchplastique">', as.numeric(as.character(Tec_data$couvermulchplastique[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="real" max="0.8" min="0.05" nom="albedomulchplastique">', as.numeric(as.character(Tec_data$albedomulchplastique[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('</choix>', sep = "")))
    cat("\n")
    cat(noquote(paste('</option>', sep = "")))
    cat("\n")
    cat(noquote(paste('<option choix="', as.numeric(as.character(Tec_data$codrognage[kt])), '" nom="topping" nomParam="codrognage">', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="1" nom="no"/>', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="2" nom="yes">', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="real" max="2.0" min="0.1" nom="largrogne">', as.numeric(as.character(Tec_data$largrogne[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="real" max="2.0" min="0.2" nom="hautrogne">', as.numeric(as.character(Tec_data$hautrogne[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="real" max="10.0" min="0.0" nom="biorognem">', as.numeric(as.character(Tec_data$biorognem[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('<option choix="', as.numeric(as.character(Tec_data$codcalrogne[kt])), '" nom="topping calendar" nomParam="codcalrogne">', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="1" nom="fixed date">', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="integer" max="731" min="1" nom="julrogne">', as.numeric(as.character(Tec_data$julrogne[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('</choix>', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="2" nom="automatic calculation">', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="real" max="1.0" min="0.01" nom="margerogne">', as.numeric(as.character(Tec_data$margerogne[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('</choix>', sep = "")))
    cat("\n")
    cat(noquote(paste('</option>', sep = "")))
    cat("\n")
    cat(noquote(paste('</choix>', sep = "")))
    cat("\n")
    cat(noquote(paste('</option>', sep = "")))
    cat("\n")
    cat(noquote(paste('<option choix="', as.numeric(as.character(Tec_data$codeclaircie[kt])), '" nom="thinning" nomParam="codeclaircie">', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="1" nom="no"/>', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="2" nom="yes">', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="integer" max="731" min="1" nom="juleclair">', as.numeric(as.character(Tec_data$juleclair[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="real" max="10.0" min="0.0" nom="nbinfloecl">', as.numeric(as.character(Tec_data$nbinfloecl[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('</choix>', sep = "")))
    cat("\n")
    cat(noquote(paste('</option>', sep = "")))
    cat("\n")
    cat(noquote(paste('<option choix="', as.numeric(as.character(Tec_data$codeffeuil[kt])), '" nom="leaf removal" nomParam="codeffeuil">', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="1" nom="no"/>', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="2" nom="yes">', sep = "")))
    cat("\n")
    cat(noquote(paste('<option choix="', as.numeric(as.character(Tec_data$codhauteff[kt])), '" nom="location of leaf removal" nomParam="codhauteff">', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="1" nom="bottom of the canopy"/>', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="2" nom="top of the canopy"/>', sep = "")))
    cat("\n")
    cat(noquote(paste('</option>', sep = "")))
    cat("\n")
    cat(noquote(paste('<option choix="', as.numeric(as.character(Tec_data$codcaleffeuil[kt])), '" nom="leaf.removal.calculation" nomParam="codcaleffeuil">', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="1" nom="automatic calculation">', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="real" max="10.0" min="1.0" nom="laidebeff">', as.numeric(as.character(Tec_data$laidebeff[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="real" max="1.0" min="0.0" nom="effeuil">', as.numeric(as.character(Tec_data$effeuil[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('</choix>', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="2" nom="fixed date">', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="integer" max="731" min="1" nom="juleffeuil">', as.numeric(as.character(Tec_data$juleffeuil[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="real" max="10.0" min="0.05" nom="laieffeuil">', as.numeric(as.character(Tec_data$laieffeuil[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('</choix>', sep = "")))
    cat("\n")
    cat(noquote(paste('</option>', sep = "")))
    cat("\n")
    cat(noquote(paste('</choix>', sep = "")))
    cat("\n")
    cat(noquote(paste('</option>', sep = "")))
    cat("\n")
    cat(noquote(paste('<option choix="', as.numeric(as.character(Tec_data$codetaille[kt])), '" nom="pruning" nomParam="codetaille">', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="1" nom="no"/>', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="2" nom="yes">', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="integer" max="731" min="1" nom="jultaille">', as.numeric(as.character(Tec_data$jultaille[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('</choix>', sep = "")))
    cat("\n")
    cat(noquote(paste('</option>', sep = "")))
    cat("\n")
    cat(noquote(paste('<option choix="', as.numeric(as.character(Tec_data$codepalissage[kt])), '" nom="trellis system" nomParam="codepalissage">', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="1" nom="no"/>', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="2" nom="yes">', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="real" max="3.0" min="0.5" nom="hautmaxtec">', as.numeric(as.character(Tec_data$hautmaxtec[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="real" max="2.0" min="0.1" nom="largtec">', as.numeric(as.character(Tec_data$largtec[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('</choix>', sep = "")))
    cat("\n")
    cat(noquote(paste('</option>', sep = "")))
    cat("\n")
    cat(noquote(paste('<option choix="', as.numeric(as.character(Tec_data$codabri[kt])), '" nom="greenhouse crop" nomParam="codabri">', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="1" nom="no"/>', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="2" nom="yes">', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="real" max="0.9" min="0.3" nom="transplastic">', as.numeric(as.character(Tec_data$transplastic[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="real" max="0.8" min="0.1" nom="surfouvre1">', as.numeric(as.character(Tec_data$surfouvre1[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="integer" max="731" min="1" nom="julouvre2">', as.numeric(as.character(Tec_data$julouvre2[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="real" max="0.8" min="0.1" nom="surfouvre2">', as.numeric(as.character(Tec_data$surfouvre2[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="integer" max="731" min="1" nom="julouvre3">', as.numeric(as.character(Tec_data$julouvre3[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="real" max="0.8" min="0.1" nom="surfouvre3">', as.numeric(as.character(Tec_data$surfouvre3[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('</choix>', sep = "")))
    cat("\n")
    cat(noquote(paste('</option>', sep = "")))
    cat("\n")
    cat(noquote(paste('</formalisme>', sep = "")))
    cat("\n")
    cat(noquote(paste('<formalisme nom="soil modification by techniques (compaction-fragmentation)">', sep = "")))
    cat("\n")
    cat(noquote(paste('<option choix="', as.numeric(as.character(Tec_data$codeDST[kt])), '" nom="activation fragmentation" nomParam="codeDST">', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="1" nom="yes">', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="real" max="2.0" min="0.8" nom="dachisel">', as.numeric(as.character(Tec_data$dachisel[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="real" max="2.0" min="0.8" nom="dalabour">', as.numeric(as.character(Tec_data$dalabour[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="real" max="0.2" min="0.01" nom="rugochisel">', as.numeric(as.character(Tec_data$rugochisel[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="real" max="0.2" min="0.01" nom="rugolabour">', as.numeric(as.character(Tec_data$rugolabour[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('</choix>', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="2" nom="no"/>', sep = "")))
    cat("\n")
    cat(noquote(paste('</option>', sep = "")))
    cat("\n")
    cat(noquote(paste('<option choix="', as.numeric(as.character(Tec_data$codeDSTtass[kt])), '" nom="activation compaction sowing/harvest" nomParam="codeDSTtass">', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="1" nom="yes">', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="real" max="100.0" min="0.0" nom="profhumsemoir">', as.numeric(as.character(Tec_data$profhumsemoir[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="real" max="2.0" min="0.8" nom="dasemis">', as.numeric(as.character(Tec_data$dasemis[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="real" max="100.0" min="0.0" nom="profhumrecolteuse">', as.numeric(as.character(Tec_data$profhumrecolteuse[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('<param format="real" max="2.0" min="0.8" nom="darecolte">', as.numeric(as.character(Tec_data$darecolte[kt])), '</param>', sep = "")))
    cat("\n")
    cat(noquote(paste('</choix>', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="2" nom="no"/>', sep = "")))
    cat("\n")
    cat(noquote(paste('</option>', sep = "")))
    cat("\n")
    cat(noquote(paste('<option choix="', as.numeric(as.character(Tec_data$codeDSTnbcouche[kt])), '" nom="nb of layers affected by the compaction" nomParam="codeDSTnbcouche">', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="1" nom="1"/>', sep = "")))
    cat("\n")
    cat(noquote(paste('<choix code="2" nom="2"/>', sep = "")))
    cat("\n")
    cat(noquote(paste('</option>', sep = "")))
    cat("\n")
    cat(noquote(paste('</formalisme>', sep = "")))
    cat("\n")
    cat(noquote(paste('</fichiertec>', sep = "")))
    
    
    ## On enregistre le fichier ITK ainsi cree
    
    sink()
    
  } ## Fin du for kt
  
  if ( ! isunix ) {
    close(Progress_bar)
  }
  
  return("Tout s'est bien deroule !!! Super :-)")
  
} ## Fin de la fonction STICS_mailing_Tec

STICS_mailing_USMs2 <- function (stwF, Excel_file, sheetIndex, stwR) {
  isunix <- is_unix()
#  USMs_data <- read_excel(path = file.path(stwF, Excel_file), 
#                          sheet = sheetIndex, col_types = "text")
  USMs_data <- as.data.frame(read.csv2(file = file.path(stwF, Excel_file), header = T,sep =";"))
  
  USMs_data_default <- as.data.frame(read_excel(path = file.path(system.file("extdata/data", 
                                                                             package = "STICSmailing"), "Inputs_STICS_mailing.xlsx"), 
                                                sheet = "USMs"))
  if (is_missing_df(USMs_data, USMs_data_default)) {
    return()
  }
  if (dir.exists(path.expand(stwR)) == FALSE) {
    dir.create(stwR)
  }
  if (!isunix) {
    Progress_bar_n = 0
    Progress_bar_max = length(USMs_data$usm_nom)
    Progress_bar = winProgressBar(title = "Progress bar", 
                                  min = 0, max = Progress_bar_max, width = 500)
  }
  else {
    print("Creating usms.xml file...")
  }
  file_path = file.path(stwR, "usms.xml")
  if (file.exists(file_path)) {
    file.remove(file_path)
  }
  sink(file_path)
  cat(noquote(paste("<?xml version=", "1.0", " encoding=", 
                    "UTF-8", "?>", sep = "\"")))
  cat("\n")
  cat(noquote("<usms>"))
  nb_usms = dim(USMs_data)[1]
  if (length(unique(USMs_data$usm_nom)) != nb_usms) {
    warning("Duplicated file names in usm_nom, aborting !")
  }
  for (ku in 1:nb_usms) {
    if (!isunix) {
      Progress_bar_n = Progress_bar_n + 1
      setWinProgressBar(Progress_bar, value = Progress_bar_n, 
                        title = paste(round(Progress_bar_n/Progress_bar_max * 
                                              100, 0), "% done - Creating usms.xml file", 
                                      sep = ""))
    }
    cat("\n")
    cat(noquote(paste("<usm nom=", as.character(USMs_data$usm_nom[ku]), 
                      ">", sep = "\"")))
    cat("\n")
    cat(noquote(paste("<datedebut>", as.numeric(as.character(USMs_data$datedebut[ku])), 
                      "</datedebut>", sep = "")))
    cat("\n")
    cat(noquote(paste("<datefin>", as.numeric(as.character(USMs_data$datefin[ku])), 
                      "</datefin>", sep = "")))
    cat("\n")
    cat(noquote(paste("<finit>", as.character(USMs_data$finit[ku]), 
                      "</finit>", sep = "")))
    cat("\n")
    cat(noquote(paste("<nomsol>", as.character(USMs_data$nomsol[ku]), 
                      "</nomsol>", sep = "")))
    cat("\n")
    cat(noquote(paste("<fstation>", as.character(USMs_data$fstation[ku]), 
                      "</fstation>", sep = "")))
    cat("\n")
    cat(noquote(paste("<fclim1>", as.character(USMs_data$fclim1[ku]), 
                      "</fclim1>", sep = "")))
    cat("\n")
    cat(noquote(paste("<fclim2>", as.character(USMs_data$fclim2[ku]), 
                      "</fclim2>", sep = "")))
    cat("\n")
    cat(noquote(paste("<culturean>", as.numeric(as.character(USMs_data$culturean[ku])), 
                      "</culturean>", sep = "")))
    cat("\n")
    cat(noquote(paste("<nbplantes>", as.numeric(as.character(USMs_data$nbplantes[ku])), 
                      "</nbplantes>", sep = "")))
    cat("\n")
    cat(noquote(paste("<codesimul>", as.numeric(as.character(USMs_data$codesimul[ku])), 
                      "</codesimul>", sep = "")))
    cat("\n")
    cat(noquote(paste("<plante dominance=", 1, ">", sep = "\"")))
    cat("\n")
    cat(noquote(paste("<fplt>", as.character(USMs_data$fplt_1[ku]), 
                      "</fplt>", sep = "")))
    cat("\n")
    cat(noquote(paste("<ftec>", as.character(USMs_data$ftec_1[ku]), 
                      "</ftec>", sep = "")))
    cat("\n")
    cat(noquote(paste("<flai>", as.character(USMs_data$flai_1[ku]), 
                      "</flai>", sep = "")))
    cat("\n")
    cat(noquote(paste("</plante>", sep = "")))
    cat("\n")
    cat(noquote(paste("<plante dominance=", 2, ">", sep = "\"")))
    cat("\n")
    cat(noquote(paste("<fplt>", as.character(USMs_data$fplt_2[ku]), 
                      "</fplt>", sep = "")))
    cat("\n")
    cat(noquote(paste("<ftec>", as.character(USMs_data$ftec_2[ku]), 
                      "</ftec>", sep = "")))
    cat("\n")
    cat(noquote(paste("<flai>", as.character(USMs_data$flai_2[ku]), 
                      "</flai>", sep = "")))
    cat("\n")
    cat(noquote(paste("</plante>", sep = "")))
    cat("\n")
    cat(noquote(paste("</usm>", sep = "")))
  }
  cat("\n")
  cat(noquote("</usms>"))
  sink()
  if (!isunix) {
    close(Progress_bar)
  }
  return("Tout s'est bien deroule !!! Super :-)")
}

STICS_mailing_Weather2 <- function (stwF, Excel_file1, sheetIndex_USMs, sheetIndex_Weather, stwR, Excel_file2) {
  isunix <- is_unix()
#  USMs_data <- read_excel(path = file.path(stwF, Excel_file), 
#                          sheet = sheetIndex_USMs, col_types = "text")
  USMs_data <- as.data.frame(read.csv2(file = file.path(stwF, Excel_file1), header = T,sep =";"))
  
  USMs_data_default <- as.data.frame(read_excel(path = file.path(system.file("extdata/data", 
                                                                             package = "STICSmailing"), "Inputs_STICS_mailing.xlsx"), 
                                                sheet = "USMs"))
  if (is_missing_df(USMs_data, USMs_data_default)) {
    return()
  }
#  Weather_data <- read_excel(path = file.path(stwF, Excel_file), 
#                             sheet = sheetIndex_Weather)
  Weather_data <- as.data.frame(read.csv2(file = file.path(stwF, Excel_file2), header = T,sep =";"))
  Weather_data <- Weather_data[, 1:13]
  Weather_data_default <- as.data.frame(read_excel(path = file.path(system.file("extdata/data", 
                                                                                package = "STICSmailing"), "Inputs_STICS_mailing.xlsx"), 
                                                   sheet = "Weather"))
  Weather_data_default <- Weather_data_default[, 1:13]
  if (is_missing_df(Weather_data, Weather_data_default)) {
    return()
  }
  if (dir.exists(path.expand(stwR)) == FALSE) {
    dir.create(stwR)
  }
  if (!isunix) {
    Progress_bar_n = 0
    Progress_bar_max = length(USMs_data$usm_nom)
    Progress_bar = winProgressBar(title = "Progress bar", 
                                  min = 0, max = Progress_bar_max, width = 500)
  }
  else {
    print("Creating wheather files...")
  }
  climate_files <- unique(c(as.vector(USMs_data$fclim1), as.vector(USMs_data$fclim2)))
  nb_files <- length(climate_files)
  clim_year_list <- strsplit(climate_files, split = "[.]")
  for (kw in 1:nb_files) {
    if (!isunix) {
      Progress_bar_n = Progress_bar_n + 1
      setWinProgressBar(Progress_bar, value = Progress_bar_n, 
                        title = paste(round(Progress_bar_n/Progress_bar_max * 
                                              100, 0), "% done - Creating wheather files", 
                                      sep = ""))
    }
    site <- clim_year_list[[kw]][1]
    year <- clim_year_list[[kw]][2]
    weather_xtract <- Weather_data[Weather_data$Weather_name == 
                                     site & Weather_data$ian == year, ]
    file_path <- file.path(stwR, climate_files[kw])
    if (file.exists(file_path)) {
      file.remove(file_path)
    }
    write.table(weather_xtract, file = file_path, quote = FALSE, 
                sep = "\t", row.names = FALSE, col.names = FALSE)
  }
  if (!isunix) {
    close(Progress_bar)
  }
  return("Tout s'est bien deroule !!! Super :-)")
}




