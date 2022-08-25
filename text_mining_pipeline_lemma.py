import os
os.chdir("C:\\Users\\Billy Ngaba\Dropbox\Mon PC (DESKTOP-U2JDFK5)\Desktop\STAGE_M2_SSD")
from sklearn.metrics.pairwise import cosine_similarity
import numpy as np
import spacy
from spacy.lang.en import STOP_WORDS
import numpy as np
import math
import pandas as pd
from nltk.stem import WordNetLemmatizer
import re
import nltk
from nltk.corpus import wordnet
from sklearn.feature_extraction.text import TfidfVectorizer

lmtzr = nltk.WordNetLemmatizer().lemmatize # Je déclare la fonction lemmatiser, qui me servira derrière pour la lemmatisation


def get_wordnet_pos(treebank_tag): # Renvoit la nature du mot (verbe, adjectif, nom ou adverbe), il s'agit du type (balise POS) d'un mot
    if treebank_tag.startswith('J'):
        return wordnet.ADJ
    elif treebank_tag.startswith('V'):
        return wordnet.VERB
    elif treebank_tag.startswith('N'):
        return wordnet.NOUN
    elif treebank_tag.startswith('R'):
        return wordnet.ADV
    else:
        return wordnet.NOUN



def lemmalize_text(text):
    word_pos = nltk.pos_tag(nltk.word_tokenize(text)) # Tokanise le texte et renvoit chaque mot et sa balise POS(part of speech) ou type (exple: la balise POS d'un verbe est 'VBZ')
    lemm_words = [lmtzr(sw[0].lower(), get_wordnet_pos(sw[1])) for sw in word_pos] # Je réalise l'opération Mot + Type (balise POS) -> Mot lemmatisé pour tous les mots du texte après tokénisation

    return [x for x in lemm_words] # renvoie tous les mots du texte pris en entrée sous forme de mots lemmatisés


def levenshtein(mot1,mot2):  # fonction de calcul de la méthode Levenshtein

    maximun = max(len(mot1), len(mot2)) # pour ramener la distance lev. entre 0 et 1
    # ligne_i est un tableau tel que tout au long de l'algorithme,
    # ligne_i[k] contienne la distance de levenshtein entre les k premières lettres de mot1
    # et les i premières lettres de mot2
    # Au début, i=0, et la distance entre les k premières lettres de mot1 et la chaîne vide
    # vaut bien sûr k. (il faut faire k suppressions pour passer des k premières lettres de mot1
    # à la chaîne vide)
    ligne_i = [ k for k in range(len(mot1)+1) ]
    # i va ensuite varier de 1 à len(mot2)
    for i in range(1, len(mot2) + 1):
        # i vient d'être incrémenté. On stocke dans ligne_prec la valeur de la ligne numéro i-1
        ligne_prec = ligne_i
        # On crée la nouvelle ligne, dont le premier élément (l'élement numéro 0) doit être
        # la distance de levenshtein entre la chaîne vide ("") et les i premières lettres de mot2, soit i
        # (il faut faire i additions pour passer de la chaîne vide aux i premières lettres de mot2)
        ligne_i = [i]*(len(mot1)+1)
        # On va ensuite remplir le reste de la ligne i, c'est-à-dire calculer ligne_i[k] pour k allant de 1 à len(mot1)
        for k in range(1,len(ligne_i)):
            # La variable cout vaut 0 si la kième lettre de mot1 est la même que la ième lettre de mot2, et 1 sinon
            #La kième lettre de mot1 s'obtient avec mot1[k-1], les indices commencent à 0
            cout = int(mot1[k-1] != mot2[i-1])
            #Voilà enfin le sel de l'algorithme, le calcul de ligne_i[k] pour i et k quelconques,
            # connaissant ligne_prec[k-1], ligne_prec[k] et ligne_i[k-1]
            ligne_i[k] = min(ligne_i[k-1] + 1, ligne_prec[k] + 1, ligne_prec[k-1] + cout)
    # Lorsque l'on sort de la boucle, i vaut len(mot2)
    #Ce que l'on cherche est la distance de levenshtein entre les len(mot1) premières lettres de mot1
    # et les len(mot2) premières lettres de mot2, qui est stockée dans ligne_i[len(mot1)]

    return (1-(ligne_i[len(mot1)]/maximun)) # je ramène la distance de Levenshtein dans l’espace [0,1].




# Je regroupe toutes les données textuelles dans des fichiers .txt:
# Tous les noms de variables sources (des chercheurs) et de variables candidates (d'AEGIS) respectivement dans les fichiers
## variables_sources.txt et variables_candidates.txt

## chaque ligne du fichier variables_sources.txt est représentée par un nom de variable source 
## idem pour variables_candidates.txt

# Toutes les descriptions de variables sources (des chercheurs) et de variables candidates (d'AEGIS) respectivement dans les fichiers
## descriptions_sources.txt et descriptions_candidates.txt

## chaque ligne du fichier descriptions_sources.txt est représentée par une description de variable source 
## idem pour descriptions_candidates.txt

# Donc toute variable source numéro i est représentée par son nom qui est à la ligne numéro i de variables_sources.txt et 
## sa description qui est à la ligne numéro i de descriptions_sources.txt
# idem pour toute variable candidate

#############################

# Dans cette partie, je lis les fichiers .txt décrits ci-dessus et je les stocke dans les listes python

with open("variables_sources.txt", "r",encoding='utf-8') as file:
    lines_var_src=file.read().splitlines()
    file.close()

lines_var_src1 = []
for i in range(len(lines_var_src)):
    lines_var_src1.append(re.sub('_',' ', lines_var_src[i])) # remplacement des '_' par des espaces

with open('descriptions_sources.txt', "r",encoding='utf-8') as file:
    lines_des_src = file.read().splitlines()
    file.close()


with open('variables_candidates.txt', "r",encoding='utf-8') as file:
    lines_var_cand = file.read().splitlines()
    file.close()

lines_var_cand1 = []
for i in range(len(lines_var_cand)):
    lines_var_cand1.append(re.sub('_',' ', lines_var_cand[i])) # remplacement des '_' par des espaces



with open('descriptions_candidates.txt', "r",encoding='utf-8') as file:
    lines_des_cand = file.read().splitlines()
    file.close()

#################################

# Vocabulaire pré-traité
vocab_pre = lines_des_cand + lines_var_cand1 #+ lines_des_src
vocab_sans_ponctuations = []
for line in vocab_pre:
    line = re.sub("(),?!","", line) ## Suppression des parenthèses et ponctuations
    vocab_sans_ponctuations.append(line) ## je retire les ponctuations de mon corpus

# Vocabulaire lemmatisé
vocab_lemmatized = []
for texte in vocab_sans_ponctuations:
    vocab_lemmatized.append(lemmalize_text(texte))
vocab = [item for sublist in vocab_lemmatized for item in sublist] # corpus constitué de mots lemmatisés

# Je convertis vocabulaire (ou corpus) un espace vectoriel appliquant la méthode tf-idf (décrite dans le rapport) pour affecter un poids à chacun des termes de ce vocabulaire
vectorizer = TfidfVectorizer(stop_words='english') 
esp_vec = vectorizer.fit(vocab)

# Je transforme toutes les variables (sources et candidates) en vecteurs dans l'espace vectoriel ci-dessus pour pouvoir appliquer la méthode de calcul de cosinus
var_src_vect = esp_vec.transform(lines_var_src)
des_src_vect = esp_vec.transform(lines_des_src)
var_cand_vect = esp_vec.transform(lines_var_cand)
des_cand_vect = esp_vec.transform(lines_des_cand)


######################
# Cette fonction permet, pour une variable source donnée (choisi via son numéro), de regarder en détails
# les résultats de calcul des méthodes Levenshtein, cosinus et la combinaison des 2 avec toutes variables candidates
# Elle prend aussi en entrée les listes des noms de variables (avec ou sans '_')
def recap(numero, lines_var_src, lines_var_cand ):
    i = numero
    cosinus1 = [] # avec les des. cand
    lev = []
    combi = []
    x = 0.5  # x dans [0,1]
    for k in range(len(lines_var_cand)):
        cosinus1.append(cosine_similarity(des_src_vect[i],des_cand_vect[k])[0][0])
        lev.append(levenshtein(lines_var_src[i], lines_var_cand[k]))
        combi.append(x*cosine_similarity(des_src_vect[i],des_cand_vect[k])[0][0] + (1-x)*levenshtein(lines_var_src[i], lines_var_cand[k]))
    recap = pd.DataFrame({'var_src':[lines_var_src[i]]*len(lines_var_cand),'var_cand': lines_var_cand,'lev': lev,'des_src':[lines_des_src[i]]*len(lines_var_cand),'des_cand': lines_des_cand, 'cos des cand': cosinus1,  'combi': combi })
    return recap


# sans les '_'
# df_recap2 = pd.DataFrame( columns=['var_src','var_cand','lev','des_src','des_cand','cos des cand','combi'])
# df_recap2 = pd.concat([df_recap2,recap(0,lines_var_src1, lines_var_cand1)], ignore_index=True)


##########################

## Evaluation
def evaluation(numero,x, rang,lines_var_src, lines_var_cand ):
    i = numero
    cosinus1 = [] # avec les des. cand
    lev = []
    combi = []
    # x dans [0,1]
    for k in range(len(lines_var_cand)):
        cosinus1.append(cosine_similarity(des_src_vect[i],des_cand_vect[k])[0][0])
        lev.append(levenshtein(lines_var_src[i], lines_var_cand[k]))
        combi.append(x*cosine_similarity(des_src_vect[i],des_cand_vect[k])[0][0] + (1-x)*levenshtein(lines_var_src[i], lines_var_cand[k]))
    ##
    df_lev = pd.DataFrame({'var_src':[lines_var_src[i]]*len(lines_var_cand),'var. cand. avec les meilleurs scores sur Levenshtein': lines_var_cand,'lev': lev })
    df_lev = df_lev.sort_values(by='lev', ascending=False)
    df_lev.reset_index(drop=True, inplace=True) 
    ##
    df_cos = pd.DataFrame({'var_src':[lines_var_src[i]]*len(lines_var_cand),'var. cand. avec les meilleurs scores sur cosinus': lines_var_cand, 'cos des cand': cosinus1})
    df_cos = df_cos.sort_values(by = 'cos des cand', ascending=False)
    df_cos.reset_index(drop=True, inplace=True)
    ##
    df_combi = pd.DataFrame({'var_src':[lines_var_src[i]]*len(lines_var_cand),'var. cand. avec les meilleurs scores sur la combinaison': lines_var_cand,'combi': combi})
    df_combi = df_combi.sort_values(by = 'combi', ascending=False)
    df_combi.reset_index(drop=True, inplace=True)
    ##
    df_eval = pd.concat([df_lev,df_cos,df_combi], axis = 1)
    return df_eval.head(rang)


####################### Tableaux des évaluations

correspondances = pd.read_excel("Correspondances.xlsx")
varSrcList = correspondances["Variable source"]

## Lev
TableauGlob = pd.DataFrame({'Variable source':varSrcList,'rang 1': [0]*len(lines_var_src),'rang 3': [0]*len(lines_var_src),'rang 5': [0]*len(lines_var_src),'rang 10': [0]*len(lines_var_src) })
liste_varScr_precision_inf_10_lev = []
liste_varScr_precision_inf_5_lev = []
liste_varScr_precision_inf_3_lev = []

num_var_src = 0
for i in varSrcList: 
    df_eval10 = evaluation(num_var_src,0, 10,lines_var_src1, lines_var_cand1 )
    df_eval5 = evaluation(num_var_src,0, 5,lines_var_src1, lines_var_cand1 )
    df_eval3 = evaluation(num_var_src,0,3,lines_var_src1, lines_var_cand1 )
    df_eval1 = evaluation(num_var_src,0, 1,lines_var_src1, lines_var_cand1 )
    Valeur = correspondances.loc[correspondances['Variable source'] == i]['Variable correspondante'].values[0]
    if(Valeur in df_eval1["var. cand. avec les meilleurs scores sur Levenshtein"].values):
        TableauGlob.loc[TableauGlob['Variable source'] == i,'rang 10'] = 1
        TableauGlob.loc[TableauGlob['Variable source'] == i,'rang 5'] = 1
        TableauGlob.loc[TableauGlob['Variable source'] == i,'rang 3'] = 1
        TableauGlob.loc[TableauGlob['Variable source'] == i,'rang 1'] = 1
    else:
        if(Valeur in df_eval3["var. cand. avec les meilleurs scores sur Levenshtein"].values):
            TableauGlob.loc[TableauGlob['Variable source'] == i,'rang 3'] = 1
            TableauGlob.loc[TableauGlob['Variable source'] == i,'rang 5'] = 1
            TableauGlob.loc[TableauGlob['Variable source'] == i,'rang 10'] = 1
        else:
           liste_varScr_precision_inf_3_lev.append(i)
           if(Valeur in df_eval5["var. cand. avec les meilleurs scores sur Levenshtein"].values):
               TableauGlob.loc[TableauGlob['Variable source'] == i,'rang 5'] = 1
               TableauGlob.loc[TableauGlob['Variable source'] == i,'rang 10'] = 1
           else:
              liste_varScr_precision_inf_5_lev.append(i)
              if(Valeur in df_eval10["var. cand. avec les meilleurs scores sur Levenshtein"].values): 
                  TableauGlob.loc[TableauGlob['Variable source'] == i,'rang 10'] = 1
              else:
                liste_varScr_precision_inf_10_lev.append(i)
    num_var_src = num_var_src + 1

## cosinus
TableauGlobCos = pd.DataFrame({'Variable source':varSrcList,'rang 1': [0]*len(lines_var_src),'rang 3': [0]*len(lines_var_src),'rang 5': [0]*len(lines_var_src),'rang 10': [0]*len(lines_var_src) })
liste_varScr_precision_inf_10_cos = []
liste_varScr_precision_inf_5_cos = []
liste_varScr_precision_inf_3_cos = []

num_var_src = 0
for i in varSrcList:
    df_eval10 = evaluation(num_var_src,1, 10,lines_var_src1, lines_var_cand1 ) 
    df_eval5 = evaluation(num_var_src,1, 5,lines_var_src1, lines_var_cand1 )
    df_eval3 = evaluation(num_var_src,1,3,lines_var_src1, lines_var_cand1 )
    df_eval1 = evaluation(num_var_src,1, 1,lines_var_src1, lines_var_cand1 )
    Valeur = correspondances.loc[correspondances['Variable source'] == i]['Variable correspondante'].values[0]
    if(Valeur in df_eval1["var. cand. avec les meilleurs scores sur cosinus"].values):
        TableauGlobCos.loc[TableauGlobCos['Variable source'] == i,'rang 1'] = 1
        TableauGlobCos.loc[TableauGlobCos['Variable source'] == i,'rang 3'] = 1
        TableauGlobCos.loc[TableauGlobCos['Variable source'] == i,'rang 5'] = 1
        TableauGlobCos.loc[TableauGlobCos['Variable source'] == i,'rang 10'] = 1
    else:
        if(Valeur in df_eval3["var. cand. avec les meilleurs scores sur cosinus"].values):
            TableauGlobCos.loc[TableauGlobCos['Variable source'] == i,'rang 3'] = 1
            TableauGlobCos.loc[TableauGlobCos['Variable source'] == i,'rang 5'] = 1
            TableauGlobCos.loc[TableauGlobCos['Variable source'] == i,'rang 10'] = 1
        else:
           liste_varScr_precision_inf_3_cos.append(i)
           if(Valeur in df_eval5["var. cand. avec les meilleurs scores sur cosinus"].values):
               TableauGlobCos.loc[TableauGlobCos['Variable source'] == i,'rang 5'] = 1
               TableauGlobCos.loc[TableauGlobCos['Variable source'] == i,'rang 10'] = 1
           else:
              liste_varScr_precision_inf_5_cos.append(i)
              if(Valeur in df_eval10["var. cand. avec les meilleurs scores sur cosinus"].values):
                   TableauGlobCos.loc[TableauGlobCos['Variable source'] == i,'rang 10'] = 1
              else:
                liste_varScr_precision_inf_10_cos.append(i)
    num_var_src = num_var_src + 1


###
columns = [ ["Variable source","Levenshtein","Levenshtein","Levenshtein","Levenshtein","cosinus","cosinus","cosinus","cosinus"],
            ["","précision au rang 1","Rang 3","Rang 5","Rang 10","précision au rang 1","Rang 3","Rang 5","Rang 10"]
            ]

tuples = list(zip(*columns))

index = pd.MultiIndex.from_tuples(tuples)
# tableau d'évaluation de Levenshtein et du cosinus
Tableau_sans_lemma = pd.DataFrame( columns=index)

Tableau_sans_lemma["Variable source"] = varSrcList
Tableau_sans_lemma["Levenshtein","précision au rang 1"] = TableauGlob["rang 1"]
Tableau_sans_lemma["Levenshtein","Rang 3"] = TableauGlob["rang 3"]
Tableau_sans_lemma["Levenshtein","Rang 5"] = TableauGlob["rang 5"]
Tableau_sans_lemma["Levenshtein","Rang 10"] = TableauGlob["rang 10"]
Tableau_sans_lemma["cosinus","précision au rang 1"] = TableauGlobCos["rang 1"]
Tableau_sans_lemma["cosinus","Rang 3"] = TableauGlobCos["rang 3"]
Tableau_sans_lemma["cosinus","Rang 5"] = TableauGlobCos["rang 5"]
Tableau_sans_lemma["cosinus","Rang 10"] = TableauGlobCos["rang 10"]

Tableau_sans_lemma.to_csv('Eval_Lev&Cos_avec_Lemma.csv')

html = Tableau_sans_lemma.to_html()
html_TG_Lev_cos = open('Eval_Lev&Cos_avec_Lemma.html', "w")
html_TG_Lev_cos.write(html)
html_TG_Lev_cos.close()

###### combi 
for x in [0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9]:
    TableauGlob = pd.DataFrame({'Variable source':varSrcList,'rang 1': [0]*len(lines_var_src),'rang 3': [0]*len(lines_var_src),'rang 5': [0]*len(lines_var_src),'rang 10': [0]*len(lines_var_src) })
    num_var_src = 0
    for i in varSrcList: 
        df_eval10 = evaluation(num_var_src,x, 10,lines_var_src1, lines_var_cand1 )
        df_eval5 = evaluation(num_var_src,x, 5,lines_var_src1, lines_var_cand1 )
        df_eval3 = evaluation(num_var_src,x,3,lines_var_src1, lines_var_cand1 )
        df_eval1 = evaluation(num_var_src,x, 1,lines_var_src1, lines_var_cand1 )
        Valeur = correspondances.loc[correspondances['Variable source'] == i]['Variable correspondante'].values[0]
        if(Valeur in df_eval1["var. cand. avec les meilleurs scores sur la combinaison"].values):
            TableauGlob.loc[TableauGlob['Variable source'] == i,'rang 10'] = 1
            TableauGlob.loc[TableauGlob['Variable source'] == i,'rang 5'] = 1
            TableauGlob.loc[TableauGlob['Variable source'] == i,'rang 3'] = 1
            TableauGlob.loc[TableauGlob['Variable source'] == i,'rang 1'] = 1
        else:
            if(Valeur in df_eval3["var. cand. avec les meilleurs scores sur la combinaison"].values):
                TableauGlob.loc[TableauGlob['Variable source'] == i,'rang 3'] = 1
                TableauGlob.loc[TableauGlob['Variable source'] == i,'rang 5'] = 1
                TableauGlob.loc[TableauGlob['Variable source'] == i,'rang 10'] = 1
            else:
                if(Valeur in df_eval5["var. cand. avec les meilleurs scores sur la combinaison"].values):
                    TableauGlob.loc[TableauGlob['Variable source'] == i,'rang 5'] = 1
                    TableauGlob.loc[TableauGlob['Variable source'] == i,'rang 10'] = 1
                else:
                    if(Valeur in df_eval10["var. cand. avec les meilleurs scores sur la combinaison"].values): 
                        TableauGlob.loc[TableauGlob['Variable source'] == i,'rang 10'] = 1
        num_var_src = num_var_src + 1
    y = x*10 # Utile juste pour le nom du tableau 
    TableauGlob.to_csv(f'TableauCosLem{y}.csv')

# Je remplis le Tableau d'évoluation global sur la combinaison de Lev et de cos en fonction du poids x que je donne à cos dans la combinaison x*cos+(1-x)*Lev
df1 = pd.read_csv('TableauCosLem1.0.csv')
df2 = pd.read_csv('TableauCosLem2.0.csv')
df3 = pd.read_csv('TableauCosLem3.0.csv')
df4 = pd.read_csv('TableauCosLem4.0.csv')
df5 = pd.read_csv('TableauCosLem5.0.csv')
df6 = pd.read_csv('TableauCosLem6.0.csv')
df7 = pd.read_csv('TableauCosLem7.0.csv')
df8 = pd.read_csv('TableauCosLem8.0.csv')
df9 = pd.read_csv('TableauCosLem9.0.csv')

columns = [ ["Variable source","cosinus","cosinus","cosinus","cosinus","cosinus","cosinus","cosinus","cosinus","cosinus",
			 "cosinus","cosinus","cosinus","cosinus","cosinus","cosinus","cosinus","cosinus","cosinus",
			 "cosinus","cosinus","cosinus","cosinus","cosinus","cosinus","cosinus","cosinus","cosinus",
             "cosinus","cosinus","cosinus","cosinus","cosinus","cosinus","cosinus","cosinus","cosinus"],
			["","poids 0.1","poids 0.1","poids 0.1","poids 0.1","poids 0.2","poids 0.2","poids 0.2","poids 0.2","poids 0.3","poids 0.3","poids 0.3","poids 0.3",
			 "poids 0.4","poids 0.4","poids 0.4","poids 0.4","poids 0.5","poids 0.5","poids 0.5","poids 0.5","poids 0.6","poids 0.6","poids 0.6","poids 0.6",
			 "poids 0.7","poids 0.7","poids 0.7","poids 0.7","poids 0.8","poids 0.8","poids 0.8","poids 0.8","poids 0.9","poids 0.9","poids 0.9","poids 0.9"],
			["","précision au rang 1","Rang 3","Rang 5","Rang 10","précision au rang 1","Rang 3","Rang 5","Rang 10","précision au rang 1","Rang 3","Rang 5","Rang 10",
			 "précision au rang 1","Rang 3","Rang 5","Rang 10","précision au rang 1","Rang 3","Rang 5","Rang 10","précision au rang 1","Rang 3","Rang 5","Rang 10",
			 "précision au rang 1","Rang 3","Rang 5","Rang 10","précision au rang 1","Rang 3","Rang 5","Rang 10","précision au rang 1","Rang 3","Rang 5","Rang 10"]
		]

tuples = list(zip(*columns))

index = pd.MultiIndex.from_tuples(tuples)

Tableaux_combis = pd.DataFrame( columns=index)

Tableaux_combis["Variable source"] = varSrcList
Tableaux_combis["cosinus","poids 0.1","précision au rang 1"] = df1["rang 1"]
Tableaux_combis["cosinus","poids 0.1","Rang 3"] = df1["rang 3"]
Tableaux_combis["cosinus","poids 0.1","Rang 5"] = df1["rang 5"]
Tableaux_combis["cosinus","poids 0.1","Rang 10"] = df1["rang 10"]

Tableaux_combis["cosinus","poids 0.2","précision au rang 1"] = df2["rang 1"]
Tableaux_combis["cosinus","poids 0.2","Rang 3"] = df2["rang 3"]
Tableaux_combis["cosinus","poids 0.2","Rang 5"] = df2["rang 5"]
Tableaux_combis["cosinus","poids 0.2","Rang 10"] = df2["rang 10"]

Tableaux_combis["cosinus","poids 0.3","précision au rang 1"] = df3["rang 1"]
Tableaux_combis["cosinus","poids 0.3","Rang 3"] = df3["rang 3"]
Tableaux_combis["cosinus","poids 0.3","Rang 5"] = df3["rang 5"]
Tableaux_combis["cosinus","poids 0.3","Rang 10"] = df3["rang 10"]

Tableaux_combis["cosinus","poids 0.4","précision au rang 1"] = df4["rang 1"]
Tableaux_combis["cosinus","poids 0.4","Rang 3"] = df4["rang 3"]
Tableaux_combis["cosinus","poids 0.4","Rang 5"] = df4["rang 5"]
Tableaux_combis["cosinus","poids 0.4","Rang 10"] = df4["rang 10"]

Tableaux_combis["cosinus","poids 0.5","précision au rang 1"] = df5["rang 1"]
Tableaux_combis["cosinus","poids 0.5","Rang 3"] = df5["rang 3"]
Tableaux_combis["cosinus","poids 0.5","Rang 5"] = df5["rang 5"]
Tableaux_combis["cosinus","poids 0.5","Rang 10"] = df5["rang 10"]

Tableaux_combis["cosinus","poids 0.6","précision au rang 1"] = df6["rang 1"]
Tableaux_combis["cosinus","poids 0.6","Rang 3"] = df6["rang 3"]
Tableaux_combis["cosinus","poids 0.6","Rang 5"] = df6["rang 5"]
Tableaux_combis["cosinus","poids 0.6","Rang 10"] = df6["rang 10"]

Tableaux_combis["cosinus","poids 0.7","précision au rang 1"] = df7["rang 1"]
Tableaux_combis["cosinus","poids 0.7","Rang 3"] = df7["rang 3"]
Tableaux_combis["cosinus","poids 0.7","Rang 5"] = df7["rang 5"]
Tableaux_combis["cosinus","poids 0.7","Rang 10"] = df7["rang 10"]

Tableaux_combis["cosinus","poids 0.8","précision au rang 1"] = df8["rang 1"]
Tableaux_combis["cosinus","poids 0.8","Rang 3"] = df8["rang 3"]
Tableaux_combis["cosinus","poids 0.8","Rang 5"] = df8["rang 5"]
Tableaux_combis["cosinus","poids 0.8","Rang 10"] = df8["rang 10"]

Tableaux_combis["cosinus","poids 0.9","précision au rang 1"] = df9["rang 1"]
Tableaux_combis["cosinus","poids 0.9","Rang 3"] = df9["rang 3"]
Tableaux_combis["cosinus","poids 0.9","Rang 5"] = df9["rang 5"]
Tableaux_combis["cosinus","poids 0.9","Rang 10"] = df9["rang 10"]

Tableaux_combis.to_csv('Eval_combi_avec_Lemma.csv')

html = Tableaux_combis.to_html()
html_TG_combi = open('Eval_combi_avec_Lemma.html', "w")
html_TG_combi.write(html)
html_TG_combi.close()


############ Récapitatif des résultats
nbre_varSrc = len(varSrcList)
###
columns = [ ["Nombre de bons résultats après lemmatisation (sur les 84 variables sources)","Nombre de bons résultats après lemmatisation (sur les 84 variables sources)","Nombre de bons résultats après lemmatisation (sur les 84 variables sources)","Nombre de bons résultats après lemmatisation (sur les 84 variables sources)","Nombre de bons résultats après lemmatisation (sur les 84 variables sources)","Nombre de bons résultats après lemmatisation (sur les 84 variables sources)","Nombre de bons résultats après lemmatisation (sur les 84 variables sources)","Nombre de bons résultats après lemmatisation (sur les 84 variables sources)"],
            ["Levenshtein","Levenshtein","Levenshtein","Levenshtein","cosinus","cosinus","cosinus","cosinus"],
            ["précision au rang 1","Rang 3","Rang 5","Rang 10","précision au rang 1","Rang 3","Rang 5","Rang 10"]
            ]

tuples = list(zip(*columns))

index = pd.MultiIndex.from_tuples(tuples)
# tableau d'évaluation de Levenshtein et du cosinus
Recap_sans_lemma = pd.DataFrame( columns=index)

resultat = np.sum(Tableau_sans_lemma["Levenshtein","précision au rang 1"])/nbre_varSrc
Recap_sans_lemma["Nombre de bons résultats après lemmatisation (sur les 84 variables sources)","Levenshtein","précision au rang 1"] = ["{:.2%}".format(resultat)]

resultat = np.sum(Tableau_sans_lemma["Levenshtein","Rang 3"])/nbre_varSrc
Recap_sans_lemma["Nombre de bons résultats après lemmatisation (sur les 84 variables sources)","Levenshtein","Rang 3"] = ["{:.2%}".format(resultat)]

resultat = np.sum(Tableau_sans_lemma["Levenshtein","Rang 5"])/nbre_varSrc
Recap_sans_lemma["Nombre de bons résultats après lemmatisation (sur les 84 variables sources)","Levenshtein","Rang 5"] = ["{:.2%}".format(resultat)]

resultat = np.sum(Tableau_sans_lemma["Levenshtein","Rang 10"])/nbre_varSrc
Recap_sans_lemma["Nombre de bons résultats après lemmatisation (sur les 84 variables sources)","Levenshtein","Rang 10"] = ["{:.2%}".format(resultat)]

resultat = np.sum(Tableau_sans_lemma["cosinus","précision au rang 1"])/nbre_varSrc
Recap_sans_lemma["Nombre de bons résultats après lemmatisation (sur les 84 variables sources)","cosinus","précision au rang 1"] = ["{:.2%}".format(resultat)]

resultat = np.sum(Tableau_sans_lemma["cosinus","Rang 3"])/nbre_varSrc
Recap_sans_lemma["Nombre de bons résultats après lemmatisation (sur les 84 variables sources)","cosinus","Rang 3"] = ["{:.2%}".format(resultat)]

resultat = np.sum(Tableau_sans_lemma["cosinus","Rang 5"])/nbre_varSrc
Recap_sans_lemma["Nombre de bons résultats après lemmatisation (sur les 84 variables sources)","cosinus","Rang 5"] = ["{:.2%}".format(resultat)]

resultat = np.sum(Tableau_sans_lemma["cosinus","Rang 10"])/nbre_varSrc
Recap_sans_lemma["Nombre de bons résultats après lemmatisation (sur les 84 variables sources)","cosinus","Rang 10"] = ["{:.2%}".format(resultat)]

html = Recap_sans_lemma.to_html()
html_TG_Lev_cos = open('Recap_Eval_Lev&Cos_avec_Lemma.html', "w")
html_TG_Lev_cos.write(html)
html_TG_Lev_cos.close()


####
columns = [ ["Nombre de bons résultats après lemmatisation (sur les 84 variables sources)","Nombre de bons résultats après lemmatisation (sur les 84 variables sources)","Nombre de bons résultats après lemmatisation (sur les 84 variables sources)","Nombre de bons résultats après lemmatisation (sur les 84 variables sources)","Nombre de bons résultats après lemmatisation (sur les 84 variables sources)","Nombre de bons résultats après lemmatisation (sur les 84 variables sources)","Nombre de bons résultats après lemmatisation (sur les 84 variables sources)","Nombre de bons résultats après lemmatisation (sur les 84 variables sources)","Nombre de bons résultats après lemmatisation (sur les 84 variables sources)",
			 "Nombre de bons résultats après lemmatisation (sur les 84 variables sources)","Nombre de bons résultats après lemmatisation (sur les 84 variables sources)","Nombre de bons résultats après lemmatisation (sur les 84 variables sources)","Nombre de bons résultats après lemmatisation (sur les 84 variables sources)","Nombre de bons résultats après lemmatisation (sur les 84 variables sources)","Nombre de bons résultats après lemmatisation (sur les 84 variables sources)","Nombre de bons résultats après lemmatisation (sur les 84 variables sources)","Nombre de bons résultats après lemmatisation (sur les 84 variables sources)","Nombre de bons résultats après lemmatisation (sur les 84 variables sources)",
			 "Nombre de bons résultats après lemmatisation (sur les 84 variables sources)","Nombre de bons résultats après lemmatisation (sur les 84 variables sources)","Nombre de bons résultats après lemmatisation (sur les 84 variables sources)","Nombre de bons résultats après lemmatisation (sur les 84 variables sources)","Nombre de bons résultats après lemmatisation (sur les 84 variables sources)","Nombre de bons résultats après lemmatisation (sur les 84 variables sources)","Nombre de bons résultats après lemmatisation (sur les 84 variables sources)","Nombre de bons résultats après lemmatisation (sur les 84 variables sources)","Nombre de bons résultats après lemmatisation (sur les 84 variables sources)",
             "Nombre de bons résultats après lemmatisation (sur les 84 variables sources)","Nombre de bons résultats après lemmatisation (sur les 84 variables sources)","Nombre de bons résultats après lemmatisation (sur les 84 variables sources)","Nombre de bons résultats après lemmatisation (sur les 84 variables sources)","Nombre de bons résultats après lemmatisation (sur les 84 variables sources)","Nombre de bons résultats après lemmatisation (sur les 84 variables sources)","Nombre de bons résultats après lemmatisation (sur les 84 variables sources)","Nombre de bons résultats après lemmatisation (sur les 84 variables sources)","Nombre de bons résultats après lemmatisation (sur les 84 variables sources)"],
            ["cosinus","cosinus","cosinus","cosinus","cosinus","cosinus","cosinus","cosinus","cosinus",
			 "cosinus","cosinus","cosinus","cosinus","cosinus","cosinus","cosinus","cosinus","cosinus",
			 "cosinus","cosinus","cosinus","cosinus","cosinus","cosinus","cosinus","cosinus","cosinus",
             "cosinus","cosinus","cosinus","cosinus","cosinus","cosinus","cosinus","cosinus","cosinus"],
			["poids 0.1","poids 0.1","poids 0.1","poids 0.1","poids 0.2","poids 0.2","poids 0.2","poids 0.2","poids 0.3","poids 0.3","poids 0.3","poids 0.3",
			 "poids 0.4","poids 0.4","poids 0.4","poids 0.4","poids 0.5","poids 0.5","poids 0.5","poids 0.5","poids 0.6","poids 0.6","poids 0.6","poids 0.6",
			 "poids 0.7","poids 0.7","poids 0.7","poids 0.7","poids 0.8","poids 0.8","poids 0.8","poids 0.8","poids 0.9","poids 0.9","poids 0.9","poids 0.9"],
			["précision au rang 1","Rang 3","Rang 5","Rang 10","précision au rang 1","Rang 3","Rang 5","Rang 10","précision au rang 1","Rang 3","Rang 5","Rang 10",
			 "précision au rang 1","Rang 3","Rang 5","Rang 10","précision au rang 1","Rang 3","Rang 5","Rang 10","précision au rang 1","Rang 3","Rang 5","Rang 10",
			 "précision au rang 1","Rang 3","Rang 5","Rang 10","précision au rang 1","Rang 3","Rang 5","Rang 10","précision au rang 1","Rang 3","Rang 5","Rang 10"]
		]

tuples = list(zip(*columns))

index = pd.MultiIndex.from_tuples(tuples)

Recap_combis = pd.DataFrame( columns=index)

for i in [1,2, 3, 4, 5, 6, 7, 8, 9]:
    resultat = np.sum(Tableaux_combis["cosinus",f"poids 0.{i}","précision au rang 1"])/nbre_varSrc
    Recap_combis["Nombre de bons résultats après lemmatisation (sur les 84 variables sources)","cosinus",f"poids 0.{i}","précision au rang 1"] = ["{:.2%}".format(resultat)]
    for j in [3,5,10]:
        resultat = np.sum(Tableaux_combis["cosinus",f"poids 0.{i}",f"Rang {j}"])/nbre_varSrc
        Recap_combis["Nombre de bons résultats après lemmatisation (sur les 84 variables sources)","cosinus",f"poids 0.{i}",f"Rang {j}"] = ["{:.2%}".format(resultat)]

html = Recap_combis.to_html()
html_TG_combi = open('Recap_Eval_combi_avec_Lemma.html', "w")
html_TG_combi.write(html)
html_TG_combi.close()


######## Je répertorie toutes les variables sources dont les vrais correspondancesn'ont pas une précision de rang 10

### Cos
def var_src_avec_mauvaise_precision_cos(rang):
    liste_sol_precision_inf_cos = []
    liste_score_cos = []
    liste_varScr_precision_inf_cos = []
    liste_desScr_precision_inf_cos = []
    liste_desVal_precision_inf_cos = []
    if rang == 10:
        for varSrc in liste_varScr_precision_inf_10_cos:
            Valeur = correspondances.loc[correspondances['Variable source'] == varSrc]['Variable correspondante'].values[0]
            liste_sol_precision_inf_cos.append(Valeur)
            ind_des_src = lines_var_src1.index(varSrc)
            liste_desScr_precision_inf_cos.append(lines_des_src[ind_des_src])
            ind_des_valeur = lines_var_cand1.index(Valeur)
            liste_desVal_precision_inf_cos.append(lines_des_cand[ind_des_valeur])
            liste_score_cos.append(cosine_similarity(des_src_vect[ind_des_src],des_cand_vect[ind_des_valeur])[0][0])
        liste_varScr_precision_inf_cos = liste_varScr_precision_inf_10_cos
    else:
        if rang == 5:
            for varSrc in liste_varScr_precision_inf_5_cos:
                Valeur = correspondances.loc[correspondances['Variable source'] == varSrc]['Variable correspondante'].values[0]
                liste_sol_precision_inf_cos.append(Valeur)
                ind_des_src = lines_var_src1.index(varSrc)
                liste_desScr_precision_inf_cos.append(lines_des_src[ind_des_src])
                ind_des_valeur = lines_var_cand1.index(Valeur)
                liste_desVal_precision_inf_cos.append(lines_des_cand[ind_des_valeur])
                liste_score_cos.append(cosine_similarity(des_src_vect[ind_des_src],des_cand_vect[ind_des_valeur])[0][0])
            liste_varScr_precision_inf_cos = liste_varScr_precision_inf_5_cos
        else:
            if rang == 3:
                for varSrc in liste_varScr_precision_inf_3_cos:
                    Valeur = correspondances.loc[correspondances['Variable source'] == varSrc]['Variable correspondante'].values[0]
                    liste_sol_precision_inf_cos.append(Valeur)
                    ind_des_src = lines_var_src1.index(varSrc)
                    liste_desScr_precision_inf_cos.append(lines_des_src[ind_des_src])
                    ind_des_valeur = lines_var_cand1.index(Valeur)
                    liste_desVal_precision_inf_cos.append(lines_des_cand[ind_des_valeur])
                    liste_score_cos.append(cosine_similarity(des_src_vect[ind_des_src],des_cand_vect[ind_des_valeur])[0][0])
                liste_varScr_precision_inf_cos = liste_varScr_precision_inf_3_cos
    df = pd.DataFrame({f'Variable avec une précision inférieure à {rang}': liste_varScr_precision_inf_cos,'Description variable source': liste_desScr_precision_inf_cos  ,'Solution pertinente': liste_sol_precision_inf_cos, 'Description solution pertinente': liste_desVal_precision_inf_cos ,'score sur cosinus': liste_score_cos })
    return df

rang = 10
html = var_src_avec_mauvaise_precision_cos(rang).to_html()
html_mauvaise_precision_cos = open(f'Details_varSrc_precision_{rang}_sans_Lemma.html', "w")
html_mauvaise_precision_cos.write(html)
html_mauvaise_precision_cos.close()






###### Tableau avec tous les résultats
# num_var_src = 0
# html = df_recap2.to_html()
# html_recap = open(f'df_recapBis{num_var_src}.html', "w")
# html_recap.write(html)
# html_recap.close()

###### Tableau avec les 5 plus proches

# html = evaluation(0, 0.5, 5,lines_var_src1, lines_var_cand1 ).to_html()
# html_eval = open('df_evalBis0.html', "w")
# html_eval.write(html)
# html_eval.close()