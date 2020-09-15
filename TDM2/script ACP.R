### L'Analyse en Composantes Principales

## Charger la librairie
#install.packages("ade4") # Pour installer le package si besoin
library(ade4)

#Cahrger le jeu de données et l'observer
data(olympic)
str(olympic)
head(olympic)
?decathlon

# on transforme les données pour faciliter l'interprétation 
head(olympic$tab)
olympic$tab[-c(1,5,6,10)]->ol1 #enlever les courses
head(ol1)
olympic$tab[c(1,5,6,10)]->ol2 # garder uniquement  les courses
head(ol2)
head(-ol2)
cbind(ol1, -ol2) ->ol3 #le -ol2 permet de prendre l'opposé des résultats aux courses


#Réalisation de l'ACP
pcaOl<-dudi.pca(ol3)#garder les 2 premiers axes
?dudi.pca
tiff("vp.tif", points = 20)
screeplot(pcaOl, main ="")
dev.off()
inertia.dudi(pcaOl)

#Affichage du plan factoriel
tiff("acp1.tif", points = 20)
scatter(pcaOl, posieig = "none", xax =1, yax =2) # axe 1 bon en sprint : attention aux unités plus est bon en sprint moins on va vite !
dev.off()


?scatter
#tracer le cercle des corrélations
tiff("ccor.tif", point = 20)
s.corcircle(pcaOl$co)
dev.off()
inertia.dudi(pcaOl2, col.inertia = TRUE)

#position des individus et carrés proportionnels au score
tiff("acp_ind.tif", points = 20)
s.value(pcaOl$li, scale(olympic$score)) 
dev.off()

#tracer le plan factoriel
tiff("acp2.tif", points = 20)
scatter(pcaOl, posieig = "bottomright")
dev.off()



