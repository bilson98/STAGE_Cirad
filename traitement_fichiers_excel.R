# install.packages("xlsx") 
# library(xlsx)
library(openxlsx)

#flie <- 
A.Crescence_design = read.xlsx("A.Crescence_design.xlsx", 1)  # Lire la prémière feuille

data = A.Crescence_design

sp_row = tail(data, n =125)


sp_row$unit_code = gsub("8","9",sp_row$unit_code)
sp_row$assigned_to = gsub("8","9",sp_row$assigned_to)

data = rbind(data,sp_row )


write.xlsx(data, "DesignR.xlsx")




######## O.Fontaine

ir = rep("IR", 36)
r = rep("R", 36)
cane = rep("Cane", 36)
weed = rep("weed", 36)
weed_sp = rep("weed_sp", 36)
cc = rep("cc", 36)



O.Fontaine_2016_design = read.xlsx("O.Fontaine_2016_design.xlsx", 1)  # Lire la prémière feuille

data = O.Fontaine_2016_design

data_exp_n1 = tail(data, n =36) #niveau 1
data_exp_n1$level_label= "plot"

# niveau 2

data_exp_n2 = data_exp_n1 
data_exp_n2$num_level = data_exp_n1$num_level +1
data_exp_n2$assigned_to = data_exp_n1$unit_code

##inter-rang
data_exp_n2$unit_code = paste(data_exp_n1$unit_code, ir, sep = '_')
data_exp_n2$level_label= "inter_rank"
data = rbind(data,data_exp_n2)

##rang
data_exp_n2$unit_code = paste(data_exp_n1$unit_code, r, sep = '_')
data_exp_n2$level_label= "rank"
data = rbind(data,data_exp_n2)

##cane
data_exp_n2$unit_code = paste(data_exp_n1$unit_code, cane, sep = '_')
data_exp_n2$level_label= "subplot"
data = rbind(data,data_exp_n2)

##weed
data_exp_n2$unit_code = paste(data_exp_n1$unit_code, weed, sep = '_')
data_exp_n2$level_label= "subplot"
data = rbind(data,data_exp_n2)

##weed_sp
data_exp_n2$unit_code = paste(data_exp_n1$unit_code, weed_sp, sep = '_')
data_exp_n2$level_label= "subplot"
data = rbind(data,data_exp_n2)

#niveau 3

data_exp_n2 = data[37:72,] ## pour ir

data_exp_n3 = data_exp_n2 
data_exp_n3$num_level = data_exp_n2$num_level +1
data_exp_n3$assigned_to = data_exp_n2$unit_code

##inter-rang
data_exp_n3$unit_code = paste(data_exp_n2$unit_code, cc, sep = '_')
data_exp_n3$level_label= "plant"
data = rbind(data,data_exp_n3)
data_exp_n3$unit_code = paste(data_exp_n2$unit_code, weed, sep = '_') #adv
data_exp_n3$level_label= "plant"
data = rbind(data,data_exp_n3)

##rang
data_exp_n2 = data[73:108,] ## pour r
data_exp_n3$assigned_to = data_exp_n2$unit_code
data_exp_n3$unit_code = paste(data_exp_n2$unit_code, cc, sep = '_')
data_exp_n3$level_label= "plant"
data = rbind(data,data_exp_n3)
data_exp_n3$unit_code = paste(data_exp_n2$unit_code, weed, sep = '_') #adv
data_exp_n3$level_label= "plant"
data = rbind(data,data_exp_n3)

## Gestion des espèces d'adventices
Liste_espece_adv = read.csv("Donnees_canne_PDS_plantes_de_service/Liste_espece_adv.csv", sep = ";")
Liste_espece_adv = Liste_espece_adv$Species
Liste_adv = data[181:216,]$unit_code

data_exp_sp = tail(data, n =125)
data_exp_sp$unit_code = Liste_espece_adv
data_exp_sp$assigned_to = Liste_adv[1]
data_exp_sp$unit_code = paste(data_exp_sp$assigned_to, data_exp_sp$unit_code, sep = '_')

data = rbind(data,data_exp_sp)

for(i in 2:length(Liste_adv)){
  data_exp_sp = tail(data, n =125)
  data_exp_sp$unit_code = Liste_espece_adv
  data_exp_sp$assigned_to = Liste_adv[i]
  data_exp_sp$unit_code = paste(data_exp_sp$assigned_to, data_exp_sp$unit_code, sep = '_')
  data = rbind(data,data_exp_sp)
}

# test = full_join(data,data_exp_sp)

write.xlsx(data, "O.Fontaine_2016_designR.xlsx")




####################### Treatment_level et Treatment_unit
library(openxlsx)

ressource = read.csv("Donnees_canne_PDS_plantes_de_service/MAT_FACTEUR3.csv", sep = ";")
facteurs = unique(ressource$Facteur)
n_fact1 = unique(ressource$Niveaufacteur_1)
n_fact2 = unique(ressource$Niveau.facteur.2)
n_fact3 = unique(ressource$Niveau.facteur.3)

## Treatment_Level

Tableau = data.frame(2,2)
Tableau = Tableau[-1,]
colnames(Tableau) = c("factor","factor_level")

essai = subset(ressource, essai == "O.Fontaine" & YEAR == 2016)
facteurs_es = unique(essai$Facteur)
n_fact1_es = unique(essai$Niveaufacteur_1)
n_fact2_es = unique(essai$Niveau.facteur.2)
n_fact3_es = unique(essai$Niveau.facteur.3)
par_es = unique(essai$parcelle)


###############

TP1 = cbind(c("TP"),c("TP","TPextra","Tbordure"))
TP21 = cbind(c("TP"),c("TP","TPextra"))
TP22 = cbind(c("TP"),c("TP","Tbordure"))
TP3 = cbind(c("TP"),c("TP"))
    
TE = cbind(c("TE"),c("TE"))  

ZE1 = cbind(c("ZE"),c("ZEP","ZESP"))
ZE21 = cbind(c("ZE"),c("ZEP"))
ZE22 = cbind(c("ZE"),c("ZESP"))

Des_c1 = cbind(c("Désherbage"),c("Chimique en plein","Enherbement contrôlé"))
Des_c21 = cbind(c("Désherbage"),c("Chimique en plein"))
Des_c22 = cbind(c("Désherbage"),c("Enherbement contrôlé"))

Des_meca = n_fact1[7:16]
Des_meca = Des_meca[-4]


if("TP" %in% facteurs_es){
  if(("TP bis" %in% n_fact1_es | "TPextra" %in% n_fact1_es) & "Tbordure" %in% n_fact1_es ){
    Tableau = rbind(Tableau,TP1)
  }
  else if(("TP bis" %in% n_fact1_es | "TPextra" %in% n_fact1_es)  ){
    Tableau = rbind(Tableau,TP21)
  }
  else if("Tbordure" %in% n_fact1_es){
    Tableau = rbind(Tableau,TP22)
  }
  else{
    Tableau = rbind(Tableau,TP3)
  }
  Tableau = rbind(Tableau,Des_c21)
  
}

if("TE" %in% facteurs_es){
  Tableau = rbind(Tableau,TE)
  Tableau = rbind(Tableau,Des_c22)
}

if("ZE" %in% facteurs_es){
  if("ZEP" %in% n_fact1_es & "ZESP" %in% n_fact1_es){
    Tableau = rbind(Tableau,ZE1)
  }
  else if("ZEP" %in% n_fact1_es){
    Tableau = rbind(Tableau,ZE21)
  }
  else{
    Tableau = rbind(Tableau,ZE22)
  }
}

if("PDS" %in% facteurs_es){
  PDS = cbind("PDS", na.omit(n_fact2_es))
  Tableau = rbind(Tableau,PDS)
}

if("Désherbage" %in% facteurs_es){
  Des_meca_es = intersect(n_fact1_es,Des_meca)
  Des = cbind("Désherbage", Des_meca_es)
  Tableau = rbind(Tableau,Des)
}

colnames(Tableau) = c("factor","factor_level")
library(openxlsx)
write.xlsx(Tableau, "essai_TL.xlsx")

#####

# Tableau_es = cbind(par_es[1], Tableau)
# colnames(Tableau_es) = c("unit_code","factor","factor_level")
# for(i in 2:length(par_es)){
#   Tableau_par = cbind(par_es[i], Tableau)
#   colnames(Tableau_par) = c("unit_code","factor","factor_level")
#   Tableau_es = rbind(Tableau_es,Tableau_par)
# }
# library(openxlsx)
# write.xlsx(Tableau_es, "essai_TU.xlsx")
#colnames(Tableau_es) = c("unit_code","factor","factor_level","from date","to date")
    