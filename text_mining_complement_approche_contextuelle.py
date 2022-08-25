import os
os.chdir("C:\\Users\\Billy Ngaba\Dropbox\Mon PC (DESKTOP-U2JDFK5)\Desktop\STAGE_M2_SSD\Doc_Billy")
#import aspose.words as aw
import re
from pathlib import Path
#import glob
from collections import Counter
import nltk
from nltk.corpus import stopwords


def extract_surround_words(text, keyword, n, m):
    '''
    text : texte (de l'article) en entrée
    keyword : mot ciblé
    n : nombre de mots autour du mot ciblé
    m : nombre de mots sélectionnés à la fin pour constituer le corpus autour du mot ciblé
    '''
    #Extraction de tous les mots du texte
    words = words = re.findall(r'\w+', text)
    
    liste = [] # De tous les mots autour du mot-clé
    # Parcours de tous les mots
    for index, word in enumerate(words):

        #vérifier si le mot-clé de recherche correspond
        if word == keyword:
            #récupérer les mots du côté gauche
            left_side_words = words[index-n : index]
            liste.append(left_side_words)

            #récupérer les mots du côté droit
            right_side_words = words[index+1 : index + n + 1]
            liste.append(right_side_words)

    liste = [item for sublist in liste for item in sublist]
    
    # Je retire les 'stopwords' de la liste
    liste = [word for word in liste if word not in stopwords.words('english')]

    # Je filtre la liste avec les m mots les plus fréquents
    liste = Counter(liste).most_common() # cette fonction donne le mot avec le nombre de répétitions de celui-ci dans l'ordre décroissant

    liste = [i[0] for i in liste] # Je garde le mot sans son nombre de repétition

    liste = liste[0:m] # Je garde les m premiers mots
    
    return liste


# Descriptions sources à contextualiser
# descr_1 = "full weed coverage"
# descr_2 = "Aboveground fresh mass"
# descr_3 = "Cover crop coverage in percentage"
# descr_4 = "Total weight of cane harvested on the elementary plot"
# descr_5 = "Sucrose content in harvested cane"
# descr_6 = "Cane yield (in fresh machinable stem)"
# descr_7 = "Herbicide Application Frequency Index"

keywords_list = ['weed','coverage','aboveground','fresh','mass','crop','percentage',
                    'weight','cane','harvested','plot', 'yield', 'fresh',
                    'machinable','stem','herbicide','application','frequency','index',
                    'cane_yield','cover_crop','cover_crops','elementary_plot','fertiliser','ligule','plant','Height','stalks']


# Je convertis les fichiers pdf en txt "à la main" sur des sites dédiés




text1 = Path('Docs_convertis\Akemo-Wortman-2.txt',encoding='utf-8').read_text()
text2 = Path('Docs_convertis\Aude_merged.txt').read_text(encoding='utf-8')
text3 = Path('Docs_convertis\Baraibar-Gfeller-10.txt').read_text(encoding='utf-8')
text4 = Path('Docs_convertis\Beillouin_merged.txt').read_text(encoding='utf-8')
text5 = Path('Docs_convertis\Berry2009_merged.txt').read_text(encoding='utf-8')
text6 = Path('Docs_convertis\Cajas2019_These_merged.txt').read_text(encoding='utf-8')
text7 = Path('Docs_convertis\Christina2021.txt').read_text(encoding='utf-8')
text8 = Path('Docs_convertis\Damien_merged.txt').read_text(encoding='utf-8')
text9 = Path('Docs_convertis\Damour_merged.txt').read_text(encoding='utf-8')
text10 = Path('Docs_convertis\Hajjar-Malezieux10.txt').read_text(encoding='utf-8')
text11 = Path('Docs_convertis\muhammad-Tribouillois-10.txt',encoding='utf-8').read_text()
text12 = Path('Docs_convertis\Rakotomanga_ASD_merged.txt').read_text(encoding='utf-8')
text13 = Path('Docs_convertis\Ratnadass_2020_merged.txt').read_text(encoding='utf-8')
text14 = Path('Docs_convertis\Sobia_merged.txt').read_text(encoding='utf-8')
text15 = Path('Docs_convertis\Weeds of tropical rainfed cropping systems.txt').read_text(encoding='utf-8')


# Je regroupe tous les articles 'nettoyés' en un seul texte
text="\n\n\n".join([text1,text2,text3,text4,text5,text6,text7,text8,text9,text10,text11,text12,text13,text14,text15])

# Je nettoie le texte et je remplace certains termes de deux mots en un mot car ma fonction  
# se focalise autour d'un mot-cible
text = text.replace('\n', ' ')
#text = text.replace('Created with an evaluation copy of Aspose.Words. To discover the full versions of our APIs please visit: https://products.aspose.com/words/', ' ')
text = text.replace('cane yield', 'cane_yield')
text = text.replace('cover crop', 'cover_crop')
text = text.replace('cover crops', 'cover_crops')
text = text.replace('elementary plot', 'elementary_plot')
# A la fin du processus, il est IMPORTANT D'ENLEVER les '_' dans les fichiers obtenus 
# car cane_yield par exemple n'a pas de sens car n'est pas un mot




# Je construis un premier corpus avec chacun des mots clés
n = 5
m = 20   
for keyword in keywords_list:
    liste = extract_surround_words(text, keyword, n, m)   
    with open('complement_approche_contextuelle.txt', 'a+', encoding='utf-8') as fp: # j'écris tous les mots autour des mots cibles dans le fichier compliment_approche_contextuelle.txt
        # l'option 'a+' permet de re-écrire dans le fichier txt sans écrire si on veut changer les variables n et m, 
        # il faut soit choisir l'option 'w' soit supprimer le fichier créé précédemment
        for item in liste:
            # J'écris les mots les eux après les autres dans le fichier
            fp.write("%s " % item)
        fp.write("\n")

  





#########

#########

# Je passe du .pdf au .txt
# doc = aw.Document("Article_scientifique\Association_PDS-Controle_MH\muhammad - Tribouillois 10.pdf")
# doc.save("Article_scientifique\Association_PDS-Controle_MH\muhammad - Tribouillois 10.txt")

# liste des fichiers transformés et à traiter
# chemin = "Docs_convertis"
# fichiers_a_traiter = os.listdir(chemin)
# test = Path('Docs_convertis\fichiers_a_traiter[1]',encoding='utf-8').read_text()
# # Je convertit le document .txt en chaines de caractères python
