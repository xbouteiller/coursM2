#----------------------------------------------------------------------------------------------------------
#### Fiche XXX : L'Analyse Factorielle des Correspondances (AFC)


##Importation et observation des données

#Importation
Ois <- read.table("Oiseaux.csv", sep =";", h=T)
str(Ois)#Pour connaître la structure du jeu de données
head(Ois)#Pour afficher les 6 premières lignes

#Création de la table de contingence
tableOis <- table(Ois$site, Ois$espece)
tableOis 

#Visualiser la table de contingence
#install.packages("ade4",dependencies=T) # Installer le package ade4, décommenter si besoin
library(ade4) #Chargement de la librairie ade4 qui sera utilsiée pour l'AFC
table.value(tableOis, 
col.labels = colnames(tableOis), #Définir les espèces en nom de colonne
csize =0.75, clegend = 0.75) #paramètres pour gérer la taille d'affichage

#Pour sauvegarder la figure dans le dossier de travail
tiff("tablevalue.tif")
table.value(tableOis, 
col.labels = colnames(tableOis) , #Mettre les noms des oiseaux en nom de colonne
csize =0.75, clegend = 0.75) #paramètres pour gérer la taille d'affichage
dev.off()

##Commandes R et résultats

#Réaliser l'AFC
afc1 <- dudi.coa(as.data.frame.matrix(tableOis)) #2 axes sélectionnés
#Alternative
#dudi.coa(as.data.frame.matrix(table.ois), scannf = FALSE, nf =2)->afc1 # Ne aps afficher le diagramme bâton des valeurs propres et conserver les 2 premiers axes

#Inertie Conservée
inertia.dudi(afc1)#Afficher la variance conservée dans chaque axe(table.ois))->afc1 
inertia.dudi(afc1, col.inertia =TRUE)#Afficher la variance conservée dans chaque axe(table.ois))->afc1 
inertia.dudi(afc1, row.inertia =TRUE)
#Afficher le plan factoriel
scatter(afc1,
posieig = "bottomleft", #Gérer la positon du diagramme des valeurs propres
clab.row = 1,clab.col = 0.75)#Gérer la taille des étiquettes

#Pour sauvegarder le graphique
tiff("afc.tif")
scatter(afc1, posieig = "bottomleft", clab.row = 2, clab.col = 1)
dev.off()

#----------------------------------------------------------------------------------------------------------
#### 
